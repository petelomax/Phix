--
-- pfileioN.e  (Phix compatible 0.6.4)
-- =========
--

--DEV need saving/restoring over opInterp: fdtbl/fdmax/freelist/(finit) [use bang labels!]
--DEV wrap/scroll/text_rows/bk_color/text_color/clear_screen/free_console/position

--include builtins\VM\pUnassigned.e -- :%pRTErn (DEV/temp)
--
--  Phix implementation of fast buffered and thread safe file i/o, namely the 
--  routines open/puts/getc/gets/seek/where/flush/close, and also the related
--  get_textn/wrap/scroll/text_rows/bk_color/text_color/position/free_console/
--  clear_screen.
--
--  This is an auto-include file; there is no need to manually include it, unless 
--  for some reason you want a namespace.
--
--DEV:
--  NB: This file is part of the optable: while you can test any modifications
--      using "p -c test" (p -d -run test?), you will need to run p -cp before
--      changes here will have any effect on "p test".
--
--/*
This will not work on RDS Eu/OpenEuphoria!!
--*/
--without debug -- (barely measurable savings, but [DEV] will want for release?)

--
--  <glossary>
--      When I say "the asm", I tend to mean the older closed source backend,
--      whereas when I mean the #ilASM in here, I intend to say "the ilASM".
--  </glossary>
--
--  Originally, file i/o was coded in (closed source) assembly; this is a port
--  of that to (an eclectic mix of) (open source) hll (normal Phix code) and 
--  ilASM (inline assembly). It may not be very pretty, but is designed to:
--
--      * provide the best possible performance (especially getc)
--      * hide grubby details away, and keep day-to-day code clean
--      * be thread-safe (the asm version was most definitely not)
--      * fully support file i/o above 4GB (asm version was partial)    [DEV proper testing rqd]
--      * cope with redirected stdin/stdout (asm version did not)       [""]
--
--  Technically, of course, this is part of the Phix Virtual Machine, the mapping
--  between the simple "fn = open(name,mode)" and the raw hardware. It should be
--  considered part of the compiler, and thus no different to writing PE headers,
--  RVA entries, raw machine code, and other such nasties. If you think this is a 
--  bit difficult to read, trust me, it was much more difficult to write, but was
--  worth the effort because it improves the performance of almost every program.
--  And, believe it or not, this is /miles/ clearer than the stuff it replaced!
--
--  An obvious benefit is that you can safely take a copy of this and fiddle with 
--  or hack it to your hearts content (rename all the globals, obviously), unlike 
--  the asm, which was all-or-nothing, updates involving thousands of tiny little 
--  steps, any that broke anything had to be immediately undone, and an alternate 
--  and ever more circuitous route devised.
--
--  Experiments with the hll version to get redirection working took maybe around
--  two hours, maybe less, in contrast to several failed attempts on the original
--  asm version, spread out over more months than I care to admit.
--
--  The now-commented-out n_diag routine was trivial to write; the equivalent in 
--  the original asm backend code would certainly be far more difficult.
--

-- Performance considerations
-- ==========================
--  The original hll implementation showed that, in particular for getc(), an
--  opFrame/opRetf overhead was unacceptable. This lead to the introduction and
--  use of global labels (":%" in file-level #ilASM). An unplanned benefit of
--  said introduction is that it promises a means of implementing opFrame/Retf
--  in hll/ilASM, as obviously they could never be implemented via themselves.
--
--      <aside>
--          I still firmly maintain that opFrame/opRetf are perfectly fast
--          enough; just because I found a case where //billions// of calls
--          could be done noticeably faster, it does not mean I would ever 
--          consider completely throwing away all ability to debug things.
--      </aside>
--
--  The original hll implementation (pfileio.e) is included in the distribution 
--  since it may prove easier to debug or test some enhancement.
--
--  While some ilASM is used to improve performance, the majority of such code 
--  is probably more likely to have been added to make things thread-safe and 
--  re-entrant, since that is generally speaking the one thing that file-level 
--  code was never meant to be. Typically that means allocating all work space 
--  on the system stack.
--
--  Further performance improvements are no doubt possible, so far I have only
--  focused on the worst offenders (according to with profile[_time]).
--
--DEV fixme (figure out how to invoke hll from ilASM, and dealloc as rqd) [DONE, see :%opOpen]
--  One known issue is puts(1[or 2],<dword_sequence>). The console (correctly)
--  has no buffer, so this resorts to displaying one-character-at-a-time, which
--  could get quite slow. The solution is to code something like:
--      if not string(s) then s = toString(s) end if
--      puts(1,s)
--  (you may have to copy the toString in here as it is not global.) Obviously
--  you do not need to bother with this when outputting to a file.
--
--with profile_time

-- Technical notes
-- ===============
--  As we open each file, we allocate(FDSIZE32/64), a reusable block of memory which
--  contains handles, flags, indexes, a 64-bit position, and a buffer. Location of a 
--  block is stored in sequence fdtbl. A file handle is in fact an index to fdtbl; 
--  given 0/1/2 are stdin/stdout/stderr, 3 corresponds to fdtbl[1], 4 to [2], etc. 
--  Keeping all the necesary information about a file in such blocks both aids in
--  performance and minimises multithreading issues, covered in more depth below. 
--  Now, we could store the result from allocate(FDSIZE32/64) as an atom in fdtbl[i], 
--  but that would probably require code such as:
--
--      #ilASM{ mov edi,[fdtbl]
--              shl edi,2               -- (ref->raw addr)
--              mov esi,[edi+eax*4]     -- ("this:=fdtbl[fn-2]")
--              ...
--              cmp esi,h4
--              jl @f
--                  sub esp,8
--                  fld qword[ebx+esi*4]
--                  fistp qword[esp]
--                  pop esi
--                  add esp,4
--            @@:
--              mov edx,[esi+MODE]
--              ... }
--
--  Alternatively we can recognise that the result from allocate() is always a
--  multiple of 4, and divide by 4 so that fdtbl entries are always integer,
--  in which case we can instead (of between the ...) use (eg):
--
--      #ilASM{ ...
--              mov edx,[ebx+esi*4+MODE]
--              ... }
--
--  A more complete example: The hll variable "iThis" is often used as follows:
--
--      integer iThis
--      ...
--          iThis = floor(allocate(FDSIZE32)/4)     -- ("open()")
--          fdtbl = append(fdtbl,iThis)
--      else
--          iThis = fdtbl[i]                        -- ("getc()" etc)
--      ...
--      #ilASM{ mov esi,[iThis]
--              shl esi,2
--              ...
--              mov edx,[esi+MODE]
--              ... }
--
--  This (final) step gained the final 10% in performance to match the original
--  assembly code. (At least for the getc() tests I was running.)
--

-- Multithreading issues
-- =====================
--  It should be no surprise to anyone that multiple threads acting on the same
--  file, without locking, are not going to be thread safe. Generally speaking,
--  each file handle should be owned exclusively by a single thread, or covered
--  by appropriate critical section handling (ie enter_cs).
--
--  Clearly close(-9) and flush(-9) could wreak havoc in a multithreaded program.
--  The main thread executes a close(-9) on termination; it is entirely up to the 
--  programmer to ensure any and all child threads (that use file i/o) properly 
--  terminate before exiting(/letting that close(-9) transpire). [DEV write a demo]
--
--  Obviously there should be no problem with one thread creating/processing a 
--  file and then "handing it over" to another thread, but sharing may require 
--  extensive locking at the application level. That applies equally to stdin, 
--  stdout, and stderr, not that slightly garbled error messages once or twice 
--  a year justifies panic. The rest of this section discusses a (minor) issue
--  with (eg) Thread A operating on file 3 while Thread B opens file 4.
--
--  Locking is needed to cover fdtbl/freelist updates in open/close, otherwise 
--  these routines are thread safe, except for the following race condition:
--      Thread A (many):        this = fdtbl[fidx]
--      Thread B (open):        fdtbl = append(fdtbl,this)
--  The line in Thread A would succeed if performed either wholly before or 
--  after the line in Thread B, however if Thread B reallocates fdtbl *AND* 
--  either it or something else manages to re-use the just-freed old memory, 
--  during the (very brief) moment that Thread A is still referencing it, we 
--  might have a problem. While I am happy to lock open/close, adding such
--  to getc (etc) would (without any doubt whatsoever) cripple performance. 
--  However, given that the pseudo-code for append (nb *NOT* prepend) is:
--      if <out of space> then
--          allocate a larger memory block
--          copy the contents (rep movsd, without refcounting etc)  [3]
--          replace the ref (ie [fdtbl], as per edi below)          [4]
--          free the previous memory
--      end if
--  and critically that [3][4] occur in that order, then a simple retry loop 
--  in all such Thread A code solves the issue, ie/eg change:
--
--          mov edx,[index]
--          mov edi,[fdtbl]
--          mov esi,[edi+edx] (esi:=fdtbl[fidx], shifting/scaling omitted)
--
--      to:
--
--          mov edx,[index]
--        ::retry
--          mov edi,[fdtbl]                                         [X]
--          mov esi,[edi+edx]                                       [Y]
--          cmp edi,[fdtbl]                                         [Z]
--          jne :retry
--
--  This effectively and efficiently makes the ([X][Y]) pair atomic, and is the
--  reason why fdtbl is always (except when locked) subscripted using #ilASM{}.
--  Of course any non-h4 value in [fdtbl] will always point to valid memory, even 
--  if that memory has been reused, and transiently loading complete and utter 
--  garbage into esi is no problem, as long as we do not use it/correct it soon.
--  (I suppose that technically this makes the ABA problem highly unlikely rather 
--   than impossible: *lots* would have to happen /both/ between X and Y, /and/ 
--   between Y and Z, for fdtbl to be replaced twice and esi left invalid. An ABA 
--   can occur /either/ side of Y without problem, as long as either XY or YZ 
--   behave atomically, and as the 3 are consecutive, one pair probably will.
--   If the highly improbable happens, then you will either have to add locking
--   in hll, or create a separate thread to handle all an application's file io.
--   It is of course unreasonable to deliberately slow everyone down for the one
--   in a million billion trillion chance that will quite probably never happen.)
--  (Update: if you can, get rid of the AGI stall on [Y], and focus on forcing
--   YZ to pair, which they should, that is if you have a problem.)
--  
--  A similar situation exists for length(fdtbl); so instead we use fdmax (ie the
--  latter is always in a fixed location and is always "good enough": it will cover
--  all files the current thread could possibly know about but may exclude 1 or 2
--  which have recently been created in other threads, that you could not legally  
--  want to access anyway, whereas length(fdtbl) moves during the append and hence 
--  could theoretically yield garbage, if the freed memory got reused quickly).
--
--  Also note that builtins\database.e has not been assessed for thread safety;
--  it is assumed that all database operations will occur in the same thread.
--  (It may already be perfectly thread safe, though to be honest I would be
--   more than a little gobsmacked, but I haven't even looked.)
--

constant
    INVALID_HANDLE_VALUE     = -1,
    STD_INPUT_HANDLE         = -10,
    STD_OUTPUT_HANDLE        = -11,
    STD_ERROR_HANDLE         = -12,

--DEV?? (this /is/ defined as such in psym.e)
--  C_PTR = C_POINTER,

--  FDSIZE = 28,--8192,         -- an FD block is ... [DEV]
--DEV newsize (different for 32/64 bit)
--< [9]       8736[#00002220]       8704[#00002200]=8+8696      x(8,663)        s[1,082]            so, FDSIZE64=8688
--  [9]       9248[#00002420]       9216[#00002400]=8+9208      x(9,175)        s[1,146]            so, FDSIZE64=9200
-- [10]      10260[#00002814]      10240[#00002800]=4+10236         x(10,219)           s[2,554]    so, FDSIZE32=10232
--SUG:
--  FDSIZE = iff(machine_bits()=32,10232,8688)
--  FDSIZE = iff(machine_bits()=32,10232,9200)
    FDSIZE32 = 8192,            -- an FD block is ... [DEV]
    BUFFERSIZE32 = FDSIZE32-24, -- 24 bytes for header fields

    FDSIZE64 = 8192,            -- an FD block is ... [DEV]
--  FDSIZE64 = 44,          -- an FD block is ... [DEV]
--  FDSIZE64 = 120,         -- an FD block is ... [DEV]
    BUFFERSIZE64 = FDSIZE64-40  -- 40 bytes for header fields

---- COORD structure (pDEST):
--constant
----    C_SIZEX = 0,
----    C_SIZEY = 2,
--  sizeof_COORD = 4

-- CONSOLE_SCREEN_BUFFER_INFO structure:
constant
    CSBI_SIZEX  = 0,    --  COORD      dwSize
    CSBI_SIZEY  = 2,
    CSBI_CPOSX  = 4,    --  COORD      dwCursorPosition
    CSBI_CPOSY  = 6,
    CSBI_ATTR   = 8,    --  WORD       wAttributes
--  CSBI_WINX1  = 10,   --  SMALL_RECT srWindow
--  CSBI_WINY1  = 12,
--  CSBI_WINX2  = 14,
--  CSBI_WINY2  = 16,
--  CSBI_MAXX   = 18,   --  COORD      dwMaximumWindowSize
--  CSBI_MAXY   = 20,
--  sizeof_CSBI = 22
    sizeof_CSBI = 24,   -- (rounded up to whole dwords)
    sizeof_CSBI64 = 24  -- (rounded up to whole qwords/keeps code looking right)


-- BY_HANDLE_FILE_INFORMATION structure
constant
--  BHFI_ATTR   = 0,    --  DWORD    dwFileAttributes
--  BHFI_CTIME  = 4,    --  FILETIME ftCreationTime
--  BHFI_ATIME  = 12,   --  FILETIME ftLastAccessTime
--  BHFI_UTIME  = 20,   --  FILETIME ftLastWriteTime
--  BHFI_SNUMB  = 28,   --  DWORD    dwVolumeSerialNumber
    BHFI_FSHI   = 32,   --  DWORD    nFileSizeHigh
    BHFI_FSLO   = 36,   --  DWORD    nFileSizeLow
    BHFI_NLINK  = 40,   --  DWORD    nNumberOfLinks
--  BHFI_FIHI   = 44,   --  DWORD    nFileIndexHigh
--  BHFI_FILO   = 48,   --  DWORD    nFileIndexLow
    sizeof_BHFI = 52,
    sizeof_BHFI64 = 56  -- (rounded up to whole quadwords) [64 might be even better]

-- OVERLAPPED structure (simplified)
constant
--  OV_INT      = 0,    --  ULONG_PTR Internal
--  OV_IHI      = 4,    --  ULONG_PTR InternalHigh
    OV_OFFSET   = 8,    --  DWORD Offset
    OV_OFFHI    = 12,   --  DWORD OffsetHigh
    OV_EVENT    = 16,   --  HANDLE  hEvent
    sizeof_OVERLAPPED = 20,
    sizeof_OVERLAPPED64 = 24 -- (rounded up to whole quadwords) [32 might be even better]
                             -- (actually, I think HANDLE is 8 bytes on 64-bit anyway)

--/*
BHFI:   BYHANDLEFILEINFO structure
;   BHFIdwFileAttributes     dd ?   ; DWORD    dwFileAttributes;        (offset 0)
;   BHFIftCreationTime       dq ?   ; FILETIME ftCreationTime;          (offset 4)
;   BHFIftLastAccessTime     dq ?   ; FILETIME ftLastAccessTime;        (offset 12)
;   BHFIftLastWriteTime      dq ?   ; FILETIME ftLastWriteTime;         (offset 20)
;   BHFIdwVolumeSerialNumber dd ?   ; DWORD    dwVolumeSerialNumber;    (offset 28)
    rb 32
    BHFInFileSizeHigh        dd ?   ; DWORD    nFileSizeHigh;           (offset 32)
    BHFInFileSizeLow         dd ?   ; DWORD    nFileSizeLow;            (offset 36)
;   BHFInNumberOfLinks       dd ?   ; DWORD    nNumberOfLinks;          (offset 40)
;   BHFInFileIndexHigh       dd ?   ; DWORD    nFileIndexHigh;          (offset 44)
;   BHFInFileIndexLow        dd ?   ; DWORD    nFileIndexLow;           (offset 48)
    rb 12
--*/

---- SMALL_RECT structure (pSMALLRECT):
--constant
--  SR_Left = 0,
--  SR_Top = 2,
--  SR_Right = 4,
--  SR_Bottom = 6,
--  sizeof_SMALL_RECT = 8,
--  sizeof_SMALL_RECT64 = 8 -- (rounded for stack alignment/keeps code looking right)

-- CHAR_INFO structure (pCHARINFO):
--constant
--  CI_UnicodeChar = 0,     -- Unicode or ANSI character
--  CI_Attributes = 2,      -- text and background colors
--  sizeof_CHAR_INFO = 4,
--  sizeof_CHAR_INFO64 = 4  -- (rounded up for stack alignment)


--DEV include kernel32.e
--atom kernel32,
--  xGetStdHandle,
--  xCreateFile,
--  xReadFile,
--  xWriteFile,
--  xSetFilePointer,
--  xGetFileInformationByHandle,
--  xLockFile,
--  xUnlockFile,
--  xGetFileSize,
--  xCloseHandle,
--  xAllocConsole,
--  xFreeConsole,
--  xSetConsoleMode,
--  xReadConsole,
--  xWriteConsole,
--  xGetConsoleMode,
--  xGetConsoleScreenBufferInfo,
--  xSetConsoleScreenBufferSize,
--  xScrollConsoleScreenBuffer,
--  xSetConsoleTextAttribute,
--  xGetLastError
--  xCSBI,
--DEV not thread-safe: (first pass completed)
--  pDword,         -- general purpose pointer to dword (bytes read/written/etc) [should be write-only] [DEV 26/05/2013 is now "safe"]
--  pSMALLRECT,
--  pCHARINFO
--  LF,
--  CRLF
--  back3,
--  pHigh

constant -- Windows
    CREATE_NEW               = 1,
--  CREATE_ALWAYS            = 2,
    OPEN_EXISTING            = 3,
    OPEN_ALWAYS              = 4,
    TRUNCATE_EXISTING        = 5,
--  GENERIC_READ             = #80000000,   -- Too big for forward init!
--  GENERIC_WRITE            = #40000000,   -- ""
--DEV newsize
    GENERIC_READo4           = #20000000,   -- (#80000000 really | stored /4)
    GENERIC_WRITEo4          = #10000000,   -- (#40000000 really | stored /4)
    FILE_SHARE_READ          = 1,
    FILE_SHARE_WRITE         = 2,
    FILE_ATTRIBUTE_NORMAL    = #80,
--  FILE_FLAG_RANDOM_ACCESS  = #10000000,
-- SetFilePointer methods
    FILE_BEGIN               = 0,
--  FILE_CURRENT             = 1,
    FILE_END                 = 2,
    NO_ERROR                 = 0,

    ENABLE_PROCESSED_INPUT   = 1,
--  ENABLE_LINE_INPUT        = 2,
--  ENABLE_ECHO_INPUT        = 4,
--  ENABLE_WINDOW_INPUT      = 8,
--  ENABLE_MOUSE_INPUT       = #10,
--  ENABLE_PROCESSED_OUTPUT  = 1,
    ENABLE_WRAP_AT_EOL_OUTPUT = 2

    --DEV (suppress unused warnings, should prolly be using this in h_puts)
--  if ENABLE_PROCESSED_OUTPUT then end if

--DEV newsize
--atom GENERIC_READ,        -- = #80000000,   -- Too big for forward init!
--   GENERIC_WRITE      -- = #40000000,   -- ""

constant -- Linux
    -- <aside> there is no valid mode of 5 (=TRUNCATE_EXISTING) </aside>
    O_RDONLY    = 0,    -- Open the file in read-only mode.
    O_WRONLY    = 1,    -- Open the file in write-only mode.
    O_RDWR      = 2,    -- Open the file in read-write mode.
    O_CREAT     = #40,  -- If the file does not exist it will be created.
--  O_EXCL      = #80,  -- (must be used with O_CREAT) Fail if file already exists.
--  O_NOCTTY    = #100,
    O_TRUNC     = #200, -- truncate to length 0 if write mode
    O_APPEND    = #400, -- open in append mode
--  O_NONBLOCK  = #800,
--  O_NONDELAY  = O_NONBLOCK,
--  O_SYNC      = #1000,
--  O_ASYNC     = #2000,
--  O_DIRECT    = #4000,
--  O_LARGEFILE = #8000,
--  O_DIRECTORY = #10000,   -- Fail if not a directory
--  O_NOFOLLOW  = #20000,   -- Fail if file is a symbolic link

--  S_ISUID     = 0o4000,   -- set user ID on execution
--  S_ISGID     = 0o2000,   -- set group ID on execution
--  S_ISVTX     = 0o1000,   -- on directories, restricted deletion flag
    S_IRWXU     = 0o0700,   -- owner has read, write and execute permission 
--  S_IRW_U     = 0o0600,   -- owner has read and write permission 
--  S_IRUSR     = 0o0400,   -- owner has read permission 
--  S_IWUSR     = 0o0200,   -- owner has write permission 
--  S_IXUSR     = 0o0100,   -- owner has execute permission
    S_IRWXG     = 0o0070,   -- group has read, write and execute permission 
--  S_IRW_G     = 0o0060,   -- group has read and write permission 
--  S_IRGRP     = 0o0040,   -- group has read permission 
--  S_IWGRP     = 0o0020,   -- group has write permission 
--  S_IXGRP     = 0o0010,   -- group has execute permission 
--  S_IRWXO     = 0o0007,   -- others have read, write and execute permission 
--  S_IRW_O     = 0o0006,   -- others have read and write permission 
    S_IRX_O     = 0o0005,   -- others have read and execute permission 
--  S_IROTH     = 0o0004,   -- others have read permission 
--  S_IWOTH     = 0o0002,   -- others have write permission 
--  S_IXOTH     = 0o0001,   -- others have execute permission

    $

include VM\pHeap.e          -- init_cs etc
include VM\pStack.e

atom fdcs -- critical section for locking fdtbl and freelist (in open/close only)

--DEV need saving/restoring over opInterp...
sequence fdtbl      -- memory blocks, allocate(FDSIZE32/64)/4, as above
                    -- Note that stdin/stdout/stderr (0/1/2) operate directly
                    --  on real file handles without any special buffering.
                    -- Hence (eg) getc(3) operates on fdtbl[1], and so on.
integer fdmax = 0   -- length(fdtbl), but thread-safe.
integer freelist = 0    -- free list pointer for fdtbl (lock before using/modifying)

--sequence filenames

-- verify fdtbl is a sequence of integer:
--!/**/ #isginfo{fdtbl,0b0100,MIN,MAX,integer,-2}

--Layout of the FDSIZE32 memory blocks:
--==================================
--  +0  handle, also used for freelist
--  +4  fmode, see F_XXX flags below
--  +8  posn, read/write position, index to buffer
--  +12 fend, used part, index to buffer
--  +16 frealposn, 64 bit qword, see notes below
--DEV newsize (it will be fine like this but we may as well use pHeap sizes)
--  +24 buffer, 8192-24 = 8168 bytes

constant HNDL = 0,
         MODE = 4,  -- (could be a byte if that helps)
         POSN = 8,  -- (could be a word if that helps)
         FEND = 12, -- (could be a word if that helps)
         POSL = 16, -- (real pos lodword)
         POSH = 20, -- (real pos hidword)
         BUFF = 24,
        
         HNDL64 = 0,
         MODE64 = 8,    -- (could be a byte if that helps)
         POSN64 = 16,   -- (could be a word if that helps)
         FEND64 = 24,   -- (could be a word if that helps)
         RPOS64 = 32,
         BUFF64 = 40

-- bit settings for fmode (same on all platforms):
constant F_CLOSED   = #00,  -- file is closed/available for re-use (see flist)
         F_READ     = #01,  -- file has read permission
         F_WRITE    = #02,  -- file has write permission
         F_BINARY   = #04,  -- binary mode
         F_DIRTY    = #08   -- write cache flag

-- If fmode has the F_DIRTY bit set, ie we have modified buffer but not written it out
--  to file, then frealposn corresponds to the start of the buffer, otherwise, ie we
--  have read something into the buffer but not altered it, frealposn corresponds to 
--  the fend of the buffer. Naturally, if we modify a clean buffer we must reposition
--  (via SetFilePointer) at the point we set F_DIRTY, for a subsequent write, but only
--  if there is something worth keeping in the (clean) buffer.
-- The use of atom to hold/modify frealposn (instead of a true 64-bit integer) limits
--  file sizes on 32-bit to 9,007,199,254,740,992 (not 18,446,744,073,709,551,616). 
--  In 2013 the largest drive available is 12TB, so you would need to plug together 
--  683 of them (somehow) to breach that. There is no such artificial limit on 64-bit,
--  where the limit is as expected(/predicted) 18 million TB, aka 4GB squared.
-- A posn of 1 and fend of 0 is the standard "empty" state.

--DEV [ftbl]!=h4
integer finit
        finit = 0

integer safemode = false -- (aka 0)

procedure initF()
--/*
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
--  xReadFile   = define_c_func(kernel32,"ReadFile",
--      {C_PTR,     --  HANDLE  hFile,  // handle of file to read
--       C_PTR,     --  LPVOID  lpBuffer,   // address of buffer that receives data
--       C_LONG,    --  DWORD  nNumberOfBytesToRead,    // number of bytes to read
--       C_PTR,     --  LPDWORD  lpNumberOfBytesRead,   // address of number of bytes read
--       C_PTR},    --  LPOVERLAPPED  lpOverlapped  // address of structure for data
--      C_INT)      -- BOOL
--  xWriteFile  = define_c_func(kernel32,"WriteFile",
--      {C_PTR,     --  HANDLE  hFile,  // handle of file to write to
--       C_PTR,     --  LPCVOID  lpBuffer,  // address of data to write to file
--       C_LONG,    --  DWORD  nNumberOfBytesToWrite,   // number of bytes to write
--       C_PTR,     --  LPDWORD  lpNumberOfBytesWritten,    // address of number of bytes written
--       C_PTR},    --  LPOVERLAPPED  lpOverlapped  // addr. of structure needed for overlapped I/O
--      C_INT)      -- BOOL
--  pHigh = allocate(4) -- for SetFilePointer's lpDistanceToMoveHigh
    xSetFilePointer = define_c_func(kernel32,"SetFilePointer",
        {C_PTR,     --  HANDLE  hFile,  // handle of file
         C_LONG,    --  LONG  lDistanceToMove,  // number of bytes to move file pointer
         C_PTR,     --  PLONG  lpDistanceToMoveHigh,    // address of high-order word of distance to move
         C_LONG},   --  DWORD  dwMoveMethod     // how to move
        C_LONG)     -- DWORD
    xGetFileInformationByHandle = define_c_func(kernel32,"GetFileInformationByHandle",
        {C_PTR,     --  HANDLE hFile
         C_PTR},    --  LPBY_HANDLE_FILE_INFORMATION lpFileInformation
        C_INT)      -- BOOL
    xLockFile = define_c_func(kernel32,"LockFile",
        {C_PTR,     --  HANDLE hFile
         C_LONG,    --  DWORD dwFileOffsetLow
         C_LONG,    --  DWORD dwFileOffsetHigh,
         C_LONG,    --  DWORD nNumberOfBytesToLockLow,
         C_LONG},   --  DWORD nNumberOfBytesToLockHigh
        C_INT)      -- BOOL
    xUnlockFile = define_c_func(kernel32,"UnlockFile",
        {C_PTR,     --  HANDLE hFile
         C_LONG,    --  DWORD dwFileOffsetLow
         C_LONG,    --  DWORD dwFileOffsetHigh,
         C_LONG,    --  DWORD nNumberOfBytesToUnlockLow,
         C_LONG},   --  DWORD nNumberOfBytesToUnlockHigh
        C_INT)      -- BOOL
--  xGetFileSize = define_c_func(kernel32,"GetFileSize",
--      {C_PTR,     --  HANDLE  hFile,  // handle of file to get size of
--       C_PTR},    --  LPDWORD  lpFileSizeHigh,    // address of high-order word for file size
--      C_LONG)     -- DWORD
    xCloseHandle = define_c_func(kernel32,"CloseHandle",
        {C_PTR},    --  HANDLE  hObject     // handle of object to close
        C_INT)      -- BOOL
    xAllocConsole = define_c_func(kernel32,"AllocConsole",
        {},         --  no parameters
        C_INT)      -- BOOL
    xFreeConsole = define_c_func(kernel32,"FreeConsole",
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
        {C_PTR,     --  HANDLE  hConsoleOutput, // handle of a console screen buffer
         C_PTR,     --  CONST VOID  *lpvBuffer, // address of buffer to write from
         C_LONG,    --  DWORD  cchToWrite,      // number of characters to write
         C_PTR,     --  LPDWORD  lpcchWritten,  // address of number of characters written
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
    xSetConsoleTextAttribute = define_c_func(kernel32,"SetConsoleTextAttribute",
        {C_PTR,     --  HANDLE  hConsoleOutput, // handle of console screen buffer
         C_INT},    --  WORD wAttributes
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
--DEV newsize
--  GENERIC_READ             = #80000000    -- Too big for forward init!
--  GENERIC_WRITE            = #40000000    -- ""
--*/
--  fdtbl = {}
    fdtbl = repeat(0,0)
--  filenames = {}
    fdcs = init_cs()
--  pDword = allocate(4)    --DEV /4?
--  LF = allocate(1)
--  CRLF = allocate(2)  --DEV /4? or inline this?
--  poke(CRLF,"\r\n")
--  LF = CRLF+1
--  back3 = allocate(3)
--  poke(back3,{8,' ',8})

    finit = 1
end procedure


--constant e57ifn = 57  -- "invalid file name"
--constant e65sfics = 65    -- "sequence found in character string"

--DEV remove this...? (see eg seek, open) [test as you go and get the era right!]
-- (NB: We may yet want MORE use of fatalN instead of the #ilASM replacements: 
--  when I replaced global function open() with :%opOpen, the era shifted from 
--  the return address to the called from address, due to use of jmp $_il etc. 
--  The #ilASM replacements pre-date the introduction of :%opOpen etc.)
--procedure iofatal(integer errcode, integer ep1=0)
--  #ilASM{
--      [32]
--          mov eax,[errcode]
--          mov edi,[ep1]
--          xor esi,esi     -- ep2 unused
--          mov ebp,[ebp+20]    -- ebp_prev --(?is this just to counteract iofatal?)
--          call :%pRTErn       -- fatal error
--      [64]
--          mov rax,[errcode]
--          mov rdi,[ep1]
--          xor rsi,rsi     -- ep2 unused
--          mov rbp,[rbp+40]    -- ebp_prev
--          call :%pRTErn       -- fatal error
--      []
--        }
--  ?9/0
--end procedure

constant e58ifn     = 58,       -- "invalid file number"
         e59wfmfao  = 59,       -- "wrong file mode for attempted operation"
         e83atpmbi  = 83,       -- "arguments to position() must be integer"
         e108pe     = 108       -- "position error [%s]"

procedure fatalN(integer level, integer errcode, integer ep1=0, integer ep2=0)
--DEV reword, or just refer to pcfunc.e (note pcfunc hasn't had the open->:%opOpen stuff done yet)
-- level is the number of frames to pop to obtain an era (must be >=2).
-- we report errors on (eg) the c_func call, not in c_func below, so
-- obviously c_func itself calls fatalN(2..), whereas if c_func calls
-- toString, that must then call fatalN(3..), and when open_dll calls
-- OpenOneDLL, which calls toString which finally calls this, it must 
-- do so with call fatalN(4..). There are no fatalN(1..) calls since
-- this is local and that would report an error in pfileioN.e itself,
-- which is the very thing the level parameter is supposed to avoid!
    #ilASM{
        [32]
            mov ecx,[level]
            mov eax,[errcode]
            mov edi,[ep1]
            mov esi,[ep2]
          @@:
            mov edx,[ebp+12]    -- era (called from address)
            mov ebp,[ebp+20]    -- prev_ebp (nb no local vars after this!)
            sub ecx,1
            jg @b
            sub edx,1
            jmp :!iDiag         -- fatal error (see pdiagN.e)
--          jmp :!fatalN        -- fatal error (see pdiagN.e)
            int3
        [64]
            mov rcx,[level]
            mov rax,[errcode]
            mov rdi,[ep1]
            mov rsi,[ep2]
          @@:
            mov rdx,[rbp+24]    -- era (called from address)
            mov rbp,[rbp+40]    -- prev_ebp (nb no local vars after this!)
            sub rcx,1
            jg @b
            sub rdx,1
            jmp :!iDiag         -- fatal error (see pdiagN.e)
--          jmp :!fatalN        -- fatal error (see pdiagN.e)
            int3
        []
          }
    ?9/0
end procedure


--/*
--DEV ,atom era?
old function toString(sequence name, integer errno)
-- explicitly convert a dword-sequence to an 8-bit string.
-- errno should be one of the constants just defined (e57ifn..e65sfics)
string res
integer nlen
object ch
    nlen = length(name)
    res = repeat(' ',nlen)
    for i=1 to nlen do
        ch = name[i]
        if not atom(ch) then
            iofatal(errno)
        end if
        ch = and_bits(ch,#FF)
        res[i] = ch
    end for
--DEV (alternative to below)
    name = ""
    return res
end function
--DEV compiler thingy... (xType=0 on symtab[1548] (name)) (methinks because routine is being optimised away but still analysed)
--{} = toString("123",57)
--*/

function toStringN(sequence name)
-- explicitly convert a dword-sequence to an 8-bit string.
-- returns {flag,res}; if flag is 0 a non-char/subsequence was found (and res is partial).
integer nlen
object ch
string res
integer flag = 1
    nlen = length(name)
    res = repeat(' ',nlen)
    for i=1 to nlen do
        ch = name[i]
        if not atom(ch) then
            flag = 0
            exit
        end if
        ch = and_bits(ch,#FF)
        res[i] = ch
    end for
    return {flag,res}
end function

--DEV newEmit (global)
--global function open(sequence filepath, object openmode)
function fopen(sequence filepath, object openmode) -- (see :%opOpen)
--
-- Open a file or device, to get the file number. -1 is returned if the open fails.
-- Single character modes are used for text handling: "r"ead, "w"rite, "u"pdate, "a"ppend.
-- Output to text files will have carriage-return characters (\r) automatically added before
-- linefeed characters (\n). On input, these carriage-return characters are removed.
-- A control-Z character (ASCII 26) will signal an immediate end of file. [DEV]
-- Binary mode uses the same one-character mode as above plus a "b", ie "rb","wb","ub","ab".
--
-- Note: open(xxx,'a') is perfectly valid and the same as open(xxx,"a"), but on Phix only.
--
integer res, imode, fmode
integer accessmode, 
        createmode,     -- (Windows only; on Linux O_CREAT (or not) goes in accessmode)
        sharemode
atom fhandle
integer iThis
--atom frealposn

--DEV not working as expected: p -safe -test allows t39rndio, t58rtxt, whereas 41/42 (ilASM) /were/ caught & excluded...
--DOH! I didn't put !SetSafeMode in the optable!!
    if safemode then safemode=0 ?9/0 end if
    if not finit then initF() end if
    fmode = 0                                   -- assume text mode
    if sequence(openmode) then                  -- allow open(<file>,'r') as well as open(<file>,"r")
        if length(openmode)=2 then
            if find(openmode[2],"bB") then
                fmode = F_BINARY
                openmode = openmode[1]          -- make openmode a char
             end if
        elsif length(openmode)=1 then
            openmode = openmode[1]              -- make openmode a char
--      else invalid length, triggers invalid open mode next
        end if
    end if

    if not string(filepath) then
--      filepath = toString(filepath,e57ifn)
        {res,filepath} = toStringN(filepath)
        if res=0 then
--          iofatal(e57ifn)
            #ilASM{
                [32]
                    mov edx,[ebp+12]                    -- "called from" address
                    mov ebp,[ebp+20]                    -- prev_ebp
                    mov al,57                           -- e57ifn: "invalid file name"
--                  xor edi,edi                         -- ep1 unused
--                  xor esi,esi                         -- ep2 unused
                    sub edx,1
                [64]
                    mov rdx,[rbp+24]                    -- "called from" address
                    mov rbp,[rbp+40]                    -- prev_ebp
                    mov al,57                           -- e57ifn: "invalid file name"
--                  xor rdi,rdi                         -- ep1 unused
--                  xor rsi,rsi                         -- ep2 unused
                    sub rdx,1
                []
                    jmp :!iDiag
                  }
        end if
    end if
    if openmode<'a' then
        openmode += 'a'-'A'
    end if
    if openmode='r' then        -- read
        if platform()=WINDOWS then
            accessmode = GENERIC_READo4
            createmode = OPEN_EXISTING
            sharemode = FILE_SHARE_READ
        elsif platform()=LINUX then
            accessmode = O_RDONLY   -- (and not O_CREAT)
            sharemode = 0           -- (ignored w/o "")
        else
            ?9/0
        end if
        imode = F_READ
    elsif openmode='w' then     -- write
        if platform()=WINDOWS then
            accessmode = GENERIC_WRITEo4
            createmode = TRUNCATE_EXISTING
--DEV test:
--          sharemode = FILE_SHARE_WRITE
            if fmode=F_BINARY then
                sharemode = FILE_SHARE_READ+FILE_SHARE_WRITE
            else
                sharemode = FILE_SHARE_WRITE
            end if
        elsif platform()=LINUX then
            if fmode=F_BINARY then
                accessmode = O_RDWR+O_CREAT+O_TRUNC
            else
                accessmode = O_WRONLY+O_CREAT+O_TRUNC
            end if
            sharemode = S_IRWXU+S_IRWXG+S_IRX_O
        else
            ?9/0
        end if
        imode = F_WRITE
    elsif openmode='u' then     -- update
        if platform()=WINDOWS then
            accessmode = GENERIC_READo4+GENERIC_WRITEo4
            createmode = OPEN_EXISTING
            sharemode = FILE_SHARE_READ+FILE_SHARE_WRITE
        elsif platform()=LINUX then
            accessmode = O_RDWR     -- (and not O_CREAT)
            sharemode = 0           -- (ignored w/o "")
        else
            ?9/0
        end if
        imode = F_READ+F_WRITE
    elsif openmode='a' then     -- append
        if platform()=WINDOWS then
            accessmode = GENERIC_WRITEo4
            createmode = OPEN_ALWAYS
--DEV test:
--          sharemode = FILE_SHARE_WRITE
            if fmode=F_BINARY then
                sharemode = FILE_SHARE_READ+FILE_SHARE_WRITE
            else
                sharemode = FILE_SHARE_WRITE
            end if
        elsif platform()=LINUX then
            if fmode=F_BINARY then
                accessmode = O_RDWR+O_CREAT+O_APPEND
            else
                accessmode = O_WRONLY+O_CREAT+O_APPEND
            end if
            sharemode = S_IRWXU+S_IRWXG+S_IRX_O
        else
            ?9/0
        end if
        imode = F_WRITE
    else
--      iofatal(61) -- "invalid open mode"
        #ilASM{
            [32]
                mov edx,[ebp+12]                    -- "called from" address
                mov ebp,[ebp+20]                    -- prev_ebp
                mov al,61                           -- e61iom: "invalid open mode"
                sub edx,1
            [64]
                mov rdx,[rbp+24]                    -- "called from" address
                mov rbp,[rbp+40]                    -- prev_ebp
                mov al,61                           -- e61iom: "invalid open mode"
                sub rdx,1
            []
                jmp :!iDiag
              }
    end if

    --
    -- If opening for write and the file does not exist, TRUNCATE_EXISTING will
    -- fail, so in that one case retry with CREATE_NEW.
    --
    while 1 do  -- (max 2 iterations, platform()=WINDOWS only)
-->
--      fhandle = c_func(xCreateFile,{filepath,accessmode,sharemode,0,createmode,FILE_ATTRIBUTE_NORMAL,0})
--DEV newsize
--  make accessmode an integer, stored /4 to avoid int/float conversions
--  make sharemode and createmode integers too
--!/*
        #ilASM{
            [PE32]
                mov eax,[accessmode]            -- (/4)
                mov esi,[filepath]
                shl eax,2
                shl esi,2
                push ebx                        -- hTemplateFile (NULL)
                push FILE_ATTRIBUTE_NORMAL      -- dwFlagsAndAttributes
                push [createmode]               -- dwCreationDisposition
                push ebx                        -- lpSecurityAttributes (NULL)
                push [sharemode]                -- dwShareMode
                push eax                        -- dwDesiredAccess
                push esi                        -- lpFileName
                call "kernel32.dll","CreateFileA"
--              mov [fhandle],eax
                lea edi,[fhandle]
                call :%pStoreMint               -- [edi]:=eax as 31-bit int or float if needed
            [PE64]
                mov rcx,rsp -- put 2 copies of rsp onto the stack...
                push rsp
                push rcx
                or rsp,8    -- [rsp] is now 1st or 2nd copy:
                            -- if on entry rsp was xxx8: both copies remain on the stack
                            -- if on entry rsp was xxx0: or rsp,8 effectively pops one of them (+8)
                            -- obviously rsp is now xxx8, whichever alignment we started with

                mov rdx,[accessmode]            -- (/4)
                mov rcx,[filepath]
                shl rdx,2
                shl rcx,2
                mov r8,[sharemode]
                mov r9,rbx
                mov rax,[createmode]
                sub rsp,8*7                         -- minimum 4 param shadow space, and align(none here)
                mov r10,FILE_ATTRIBUTE_NORMAL
                mov [rsp+48],rbx                    -- hTemplateFile (NULL)
                mov [rsp+40],r10                    -- dwFlagsAndAttributes
                mov [rsp+32],rax                    -- dwCreationDisposition
--              (r9)                                -- lpSecurityAttributes (NULL)
--              (r8)                                -- dwShareMode
--              (rdx)                               -- dwDesiredAccess
--              (rcx)                               -- lpFileName
                call "kernel32.dll","CreateFileA"
--              add rsp,8*7
--              pop rsp
                mov rsp,[rsp+8*7]   -- equivalent to the add/pop
--              mov [fhandle],eax
                lea rdi,[fhandle]
                call :%pStoreMint               -- [rdi]:=rax as 63-bit int or float if needed
            [ELF32]
--sys_write     = 0x4 
--sys_open    = 0x5 
--sys_close    = 0x6 
--io_bytes dd ? 
--filesize   dd ? 
--filename db  "XYZ.XYZ",0 
--buffer     rb 10000h ; This must hold the readed bytes  

--                                  eax     ebx                     ecx                     edx                     esi                     edi
--5     sys_open                    0x05    const char *filename    int flags               int mode                -                       -               fs/open.c:900
                mov eax,5   -- sys_open
                mov ebx,[filepath]
                mov ecx,[accessmode]
                shl ebx,2
                mov edx,[sharemode]
                int 0x80
                cmp eax, -4069 
                jbe @f
                    mov eax,INVALID_HANDLE_VALUE
              @@:
                lea edi,[fhandle]
                xor ebx,ebx                     -- (common requirement after int 0x80)
                call :%pStoreMint               -- [edi]:=eax as 31-bit int or float if needed
--      mov eax,6   -- sys_close
--      int 0x80
----        cmp eax, -4069 
----        ja .error
            [ELF64]
--%rax  System call             %rdi                    %rsi                            %rdx                    %rcx                    %r8                     %r9
--2     sys_open                const char *filename    int flags                       int mode
                mov rax,2   -- sys_open
                mov rdi,[filepath]
                mov rsi,[accessmode]
                shl rdi,2
                mov rdx,[sharemode]
                syscall
                or rax,rax 
                jns @f
                    mov rax,INVALID_HANDLE_VALUE
              @@:
                lea rdi,[fhandle]
                call :%pStoreMint               -- [rdi]:=rax as 63-bit int or float if needed
--              pop al
            []
              }
--!*/
        if fhandle!=INVALID_HANDLE_VALUE then exit end if                   -- success!

        if platform()=WINDOWS then
            -- loop once if 'w'/TRUNCATE_EXISTING, retry as CREATE_NEW:
            if createmode!=TRUNCATE_EXISTING then return -1 end if              -- failure!
            createmode = CREATE_NEW
        elsif platform()=LINUX then
            -- (no retry needed on lnx)
            return -1
        end if
    end while

    enter_cs(fdcs)
    if freelist=0 then
        if machine_bits()=32 then
            iThis = floor(allocate(FDSIZE32)/4)
        else
            iThis = floor(allocate(FDSIZE64)/4)
        end if
        fdtbl = append(fdtbl,iThis)
        fdmax = length(fdtbl)
        res = fdmax
--      filenames = append(filenames,filepath)
    else
        res = freelist
        iThis = fdtbl[freelist]
        freelist = peek4u(iThis*4)
        if peek4u(iThis*4+MODE)!=0 then ?9/0 end if
--      if length(filenames[res]) then ?9/0 end if
--      filenames[res] = filepath
    end if
    leave_cs(fdcs)

    fmode += imode
--DEV/SUG: (is it easier to load from [iThis*4+HDNL] than cmp [fhandle],h4 etc?)
--  poke4(iThis*4+HNDL,handle)

--DEV: (doh)
--  if openmode='a' then
--      if fseek(res,-1)!=SEEK_OK then ?9/0 end if
--or
--      #ilASM{
--          [32]
--              mov esi,[iThis]
--              mov edx,[ebp+12]
--              call :%n_seek_esi_end
--          [64]
--              mov rsi,[iThis]
--              mov rdx,[rbp+24]
--              call :%n_seek_rsi_end
--            }
--  end if
--DEV newsize [PE32]
    #ilASM{
        [32]
            mov eax,[fhandle]
            mov ecx,ebx                         -- rposn loword (set to 0)
            mov edx,ebx                         -- rposn hiword (set to 0)
            cmp eax,h4  --DEV :%pLoadMint
            jl @f
                sub esp,8
                fld qword[ebx+eax*4]
                fistp qword[esp]
                pop eax
                add esp,4
          @@:
            mov edi,[openmode]
            cmp edi,'a'
            jne :nota
                push eax        -- save
        [PE32]
                push ebx        --[1] DistanceToMoveHigh (0), and rposn hiword (edx)
                mov edi,esp
                push FILE_END                   -- dwMoveMethod
                push edi                        -- lpDistanceToMoveHigh
                push ebx                        -- lDistanceToMove (0)
                push eax                        -- hFile
                call "kernel32.dll","SetFilePointer"
                mov ecx,eax     -- rposn loword
                pop edx         --[1] rposn hiword
                cmp eax,-1      -- INVALID_SET_FILE_POINTER (may be a valid loword)
                jne @f
                    call "kernel32.dll","GetLastError"
                    cmp eax,NO_ERROR
                    je @f
-- warning, untested code: [done, opOpen03 (p7 only)]
--                      mov al,64               -- e64sfooa: "seek fail on open append"
--                      xor edi,edi -- ep1 unused
--                      xor esi,esi -- ep2 unused
--                      call :%pRTErn
--better?
                        mov al,64               -- e64sfooa: "seek fail on open append"
                        mov edx,[ebp+12]        -- "called from" address
                        mov ebp,[ebp+20]        -- prev_ebp
                        sub edx,1
                        jmp :!iDiag
                        int3
              @@:
        [ELF32]
                -- (actual repositioning done via O_CREAT, just get the size)
--                                  eax     ebx                     ecx                     edx                     esi                     edi
--197   sys_fstat64                 0xc5    unsigned long fd        struct stat64 *statbuf  -                       -                       -                       fs/stat.c:380
--struc stat64
--{
--.st_dev      rq 1     0
--.__pad0      rb 4     8
--.__st_ino    rd 1     12
--.st_mode     rd 1     16
--.st_nlink    rd 1     20
--.st_uid      rd 1     24
--.st_gid      rd 1     28
--.st_rdev     rq 1     32
--.__pad3      rb 4     40
-- <4 bytes more padding here>
--.st_size     rq 1     48
--.st_blksize  rd 1     56
--.st_blocks   rq 1     60
--.st_atime    rd 1     68
--.st_atime_nsec rd 1   72
--.st_mtime    rd 1     76
--.st_mtime_nsec rd 1   80
--.st_ctime    rd 1     84
--.st_ctime_nsec rd 1   88
--.st_ino      rq 1     96
--}                 -- 104
                sub esp,104 -- sizeof(stat64)
                mov ecx,esp
                mov ebx,eax
                mov eax,197 -- sys_fstat64
                int 0x80
                xor ebx,ebx                 -- (common requirement after int 0x80)
                cmp eax, -4069 
                jbe @f
                    mov al,64               -- e64sfooa: "seek fail on open append"
                    mov edx,[ebp+12]        -- "called from" address
                    mov ebp,[ebp+20]        -- prev_ebp
                    sub edx,1
                    jmp :!iDiag
                    int3
              @@:
                mov ecx,[esp+44]            -- low dword
--              mov ecx,[esp+48]            -- low dword
                mov edx,[esp+48]            -- high dword
--              mov edx,[esp+52]            -- high dword
                add esp,104
        [32]
                pop eax     -- restore
          ::nota
            mov esi,[iThis]
            shl esi,2
            mov edi,[fmode]
            mov [esi+HNDL],eax
            mov [esi+MODE],edi
            mov [esi+POSN],dword 1
            mov [esi+FEND],ebx --(0)
            mov [esi+POSL],ecx
            mov [esi+POSH],edx
        [64]
            mov rax,[fhandle]
            mov rcx,rbx                         -- rposn loword (set to 0)
            mov rdx,rbx                         -- rposn hiword (set to 0)
            mov r15,h4
            cmp rax,r15
            jl @f
--DEV %pLoadMint
                sub rsp,8
                fld tbyte[rbx+rax*4]
                fistp qword[rsp]
                pop rax
          @@:
            mov rdi,[openmode]
            cmp rdi,'a'
            jne :nota
        [PE64]
            mov rcx,rsp -- put 2 copies of rsp onto the stack...
            push rsp
            push rcx
            or rsp,8    -- [rsp] is now 1st or 2nd copy:
                        -- if on entry rsp was xxx8: both copies remain on the stack
                        -- if on entry rsp was xxx0: or rsp,8 effectively pops one of them (+8)
                        -- obviously rsp is now xxx8, whichever alignment we started with

            sub rsp,8*7                         -- minimum 4 param shadow space, newFilePointer, save rax, and align
            mov [rsp+40],rax                    -- save
            mov r9,FILE_END                     -- dwMoveMethod
            lea r8,[rsp+32]                     -- lpNewFilePointer
            mov rdx,rbx                         -- liDistanceToMove (0)
            mov rcx,rax                         -- hFile
            call "kernel32.dll","SetFilePointerEx"
            cmp rax,0
            jne @f
                mov al,64                       -- e64sfooa: "seek fail on open append"
                mov rdx,[rbp+24]                -- "called from" address
                mov rbp,[rbp+40]                -- prev_ebp
                sub rdx,1
                jmp :!iDiag
                int3
          @@:
            mov rcx,[rsp+32]
            mov rax,[rsp+40]
--          add rsp,8*7
--          pop rsp
            mov rsp,[rsp+8*7]   -- equivalent to the add/pop
        [ELF64]
                -- (actual repositioning done via O_CREAT, just get the size)
--%rax  System call             %rdi                    %rsi                            %rdx                    %rcx                    %r8                     %r9
--5     sys_fstat               unsigned int fd         struct stat *statbuf
--struc stat64
--{                         offset
--.st_dev           rq 1    0
--.__pad0           rb 4    8
--.__st_ino         rd 1    12
--.st_mode          rd 1    16
--.st_nlink         rd 1    20
--.st_uid           rd 1    24
--.st_gid           rd 1    28
--.st_rdev          rq 1    32
--.__pad3           rb 4    40
--.st_size          rq 1    44 = (hmm, found 48 elsewhere... matches cffi.e)
--.st_blksize       rd 1    52
--.st_blocks        rq 1    56
--.st_atime         rd 1    64
--.st_atime_nsec    rd 1    68
--.st_mtime         rd 1    72
--.st_mtime_nsec    rd 1    76
--.st_ctime         rd 1    80
--.st_ctime_nsec    rd 1    84
--.st_ino           rq 1    88
--                          96 (144 found elsewhere, 128 according to cffi.e)
--}
            push rax
            -- (untested)
            -- the values of 144 and 48 were taken from (best I could find)
            -- https://rosettacode.org/wiki/Execute_Brain****/x86_Assembly
            -- somewhere else I found "#define sizeof.stat 120"...
            -- I also found 48 confirmed in a couple of other places
            sub rsp,144 -- sizeof(stat64)
            mov rsi,rsp
            mov rdi,rax
            mov rax,5 -- sys_fstat
            syscall
            or rax,rax 
            jns @f
                                                --DEV actually this is fstat fail...
                mov al,64                       -- e64sfooa: "seek fail on open append"
                mov rdx,[rbp+24]                -- "called from" address
                mov rbp,[rbp+40]                -- prev_ebp
                sub rdx,1
                jmp :!iDiag
                int3
          @@:
--          cmp rax,-1
--          je fs_error
            mov rcx,[rsp+48]
            add rsp,144
            pop rax         -- restore
        [64]
          ::nota
            mov rsi,[iThis]
            shl rsi,2
            mov rdi,[fmode]
--          mov [rsi+HNDL64],eax
            mov [rsi+HNDL64],rax
            mov [rsi+MODE64],rdi
            mov [rsi+POSN64],qword 1
--          mov [rsi+FEND64],ebx --(0)
            mov [rsi+FEND64],rbx --(0)
            mov [rsi+RPOS64],rcx
        []
          }
    res += 2
    return res
end function

integer stdin=0,
        stdout=0,
        stderr

integer cinit = 0
integer stdin_redirected = 0

procedure initConsole()
    if not finit then initF() end if
--DEV call :%n_initC??
-->
--  {} = c_func(xAllocConsole,{})
--  stdin = c_func(xGetStdHandle,{STD_INPUT_HANDLE})
--  stdout = c_func(xGetStdHandle,{STD_OUTPUT_HANDLE})
--  stderr = c_func(xGetStdHandle,{STD_ERROR_HANDLE})
--  -- nb following is not ENABLE_LINE_INPUT and not ENABLE_ECHO_INPUT
--  if not c_func(xSetConsoleMode,{stdin,ENABLE_PROCESSED_INPUT}) then
--      stdin_redirected = 1
--  end if
    #ilASM { call :%n_initC }
--DEV newsize - make these local/use the stack
--  pSMALLRECT = allocate(sizeof_SMALL_RECT)
--  pCHARINFO = allocate(sizeof_CHAR_INFO)
--DEV?
--  -- set initial foreground and background colours
--  getConsoleScreenBufferInfo()
--  fg_colour = peek2u(xCSBI+CSBI_ATTR)
--  bg_colour = and_bits(fg_colour,#F0)/#10
--  fg_colour = and_bits(fg_colour,#F)
    cinit = 1
end procedure

--DEV use elsewhere (I have tagged several places with get_this()?)
function get_this(integer fn)
integer iThis = 0
integer fidx = fn-2
integer fmode = 0
    if fidx>=1 and fidx<=fdmax then
        iThis = 1   --DEV fix this!
        #ilASM{
            [32]
                mov edx,[fidx]
                shl edx,2
              @@:
                mov edi,[fdtbl]
                mov esi,[edi*4+edx-4]   -- esi:=fdtbl[fidx] (nb fidx=fn-2)
                cmp edi,[fdtbl]
                jne @b
                mov [iThis],esi
            [64] --and next... [and fix "that bug" otherwise fmode will be treated as 0 next!]
                mov rdx,[fidx]
                shl rdx,3
              @@:
                mov rdi,[fdtbl]
                mov rsi,[rdi*4+rdx-8]   -- rsi:=fdtbl[fidx(=fn-2)]
                cmp rdi,[fdtbl]
                jne @b
                mov [iThis],rsi
              }
        if machine_bits()=32 then
            fmode = peek4u(iThis*4+MODE)
        else
--          ?9/0
            fmode = peek8u(iThis*4+MODE64)
        end if
    end if
    if fmode=0 then
--DEV use fatalN()
--      iofatal(58,fn)  -- "invalid file number (%d)"
        #ilASM{
            [32]
                mov edi,[fn]                        -- ep1:=fn
                mov edx,[ebp+12]                    -- "called from" address
                mov ebp,[ebp+20]                    -- prev_ebp
                mov al,58                           -- e58ifn: "invalid file number"
                xor esi,esi                         -- ep2 unused
                sub edx,1
            [64]
                mov rdi,[fn]                        -- ep1:=fn
                mov rdx,[rbp+24]                    -- "called from" address
                mov rbp,[rbp+40]                    -- prev_ebp
                mov al,58                           -- e58ifn: "invalid file number"
                xor rsi,rsi                         -- ep2 unused
                sub rdx,1
            []
                jmp :!iDiag
              }
    end if
    return iThis
--DEV/SUG:
--- return {iThis,fmode,fhandle}
end function

--
-- Internal routine flushfidx.
--
--DEV newsize [PE32]
--DEV (11/9/16)
--  #ilASM{ jmp :fin
    #ilASM{ jmp :!opCallOnceYeNot
--/*
global procedure :%n_flush_esiedi(:%)
end procedure -- (for Edita/CtrlQ)
--*/
        [32]
          :%n_flush_esiedi
            -- On entry, esi is fdtbl[fn-2] shl 2, and
            --           edi is fmode ([esi+MODE], adjusted)
            --           resets edi (to [esi_BUFF])
            --           preserves esi, everything else gets trashed
            mov [esi+MODE],edi
          :%n_flush_esi2
            mov edx,[esi+FEND]
            test edx,edx
            jz @f
--/*
            fild qword[esi+POSL]
            fild dword[esi+FEND]
            faddp st1,st0
            fistp qword[esi+POSL]
--*/
            mov eax,[esi+POSL]
            mov ecx,[esi+POSH]
--16/2/17: (nope)
--          mov edx,[esi+POSN]
            add eax,edx -- frealposn += fend
--          add eax,edx -- frealposn += posn
            adc ecx,ebx
--          mov edx,[esi+FEND]
            mov [esi+POSL],eax
            mov [esi+POSH],ecx
            lea edi,[esi+BUFF]
        [PE32]
            push ebx                                    -- lpOverlapped (NULL)
            push esp                                    -- lpNumberOfBytesWritten
            push edx                                    -- nNumberOfBytesToWrite (fend)
            push edi                                    -- lpBuffer
            push dword[esi]                             -- hFile
            call "kernel32.dll","WriteFile"
            test eax,eax
            jnz @f
                call "kernel32.dll","GetLastError"
        [ELF32]
--#     Name                        Registers                                                                                                               Definition
--                                  eax     ebx                     ecx                     edx                     esi                     edi
--4     sys_write                   0x04    unsigned int fd         const char *buf         size_t count            -                       -               fs/read_write.c:408
            push edi            -- save
            push esi            -- save
            mov eax,4           -- sys_write
            mov ebx,[esi]       -- fd
            mov ecx,edi         -- buffer
--          mov edx,[esi+FEND]  -- (already set [and not 0])
            int 0x80
            xor ebx,ebx         -- (common requirement after int 0x80)
            pop esi             -- restore
            pop edi             -- restore
            cmp eax, -4069 
            jbe @f
        [32]
                mov edi,eax                             -- ep1 [DEV testme]
--              xor esi,esi                             -- ep2 unused
--              jmp :%pRTErn                            -- fatal error
                pop edx -- era
                mov al,98                               -- e98fiofe -- flush error [ep1]
                sub edx,1
                jmp :!iDiag
                int3
          @@:
--DEV 16/2/17:
--          if posn!=fend+1 then
--              call :%n_seek_esi_rpos
--          end if
            ret
--/*
global procedure :%n_flush_rsirdi(:%)
end procedure -- (for Edita/CtrlQ)
--*/
        [64]
          :%n_flush_rsirdi
            -- On entry, rsi is fdtbl[fn-2] shl 2, and
            --           rdi is fmode ([rsi+MODE64], adjusted)
            --           resets rdi (to [RSI+BUFF64])
            --           preserves rsi, everything else gets trashed
            mov [rsi+MODE64],rdi
          :%n_flush_rsi2
            mov r8,[rsi+FEND64]
            test r8,r8
--29/12/15:
--          jz @f
            jz :flushret
            mov rax,[rsi+RPOS64]
--16/2/17: (nope)
            add rax,r8 -- frealposn += fend
--          mov r9,[rsi+POSN64]
--          add rax,r9 -- frealposn += posn
            mov [rsi+RPOS64],rax
--          lea rdi,[rsi+BUFF64]
        [PE64]
            mov rcx,rsp -- put 2 copies of rsp onto the stack...
            push rsp
            push rcx
            or rsp,8    -- [rsp] is now 1st or 2nd copy:
                        -- if on entry rsp was xxx8: both copies remain on the stack
                        -- if on entry rsp was xxx0: or rsp,8 effectively pops one of them (+8)
                        -- obviously rsp is now xxx8, whichever alignment we started with
            sub rsp,8*5     -- minimum 4 param shadow space, lpOverlapped, and align (none in this case)
            mov [rsp+32],rbx                            -- lpOverlapped (NULL)
--          mov r9,rbx                                  -- lpNumberOfBytesWritten (NULL)    -- NO: can only be NULL when lpOverlappend is non-NULL!!
            lea r9,[rsp+32]                             -- lpNumberOfBytesWritten
--          (r8 already set)                            -- nNumberOfBytesToWrite (fend)
            lea rdx,[rsi+BUFF64]                        -- lpBuffer
            mov rcx,[rsi+HNDL64]                        -- hFile
            call "kernel32.dll","WriteFile"
            test rax,rax
            jnz @f
                call "kernel32.dll","GetLastError"
                mov rdi,rax                             -- ep1
                mov rdx,[rbp+24]                        -- "called from" address
                mov rbp,[rbp+40]                        -- prev_ebp
                mov al,98                               -- e98fiofe -- flush error [ep1]
--              xor rsi,rsi                             -- ep2 unused
--              jmp :%pRTErn                            -- fatal error
                sub rdx,1
                jmp :!iDiag
                int3
          @@:
--          add rsp,8*5
--          pop rsp
--mov r11,rsp
            mov rsp,[rsp+8*5]   -- equivalent to the add/pop
        [ELF64]
--%rax  System call             %rdi                    %rsi                            %rdx                    %rcx                    %r8                     %r9
--1     sys_write               unsigned int fd         const char *buf                 size_t count
            push rsi                -- save
            mov rax,1               -- sys_write
            mov rdi,[rsi+HNDL64]    -- fd
            lea rsi,[rsi+BUFF64]    -- buffer
            mov rdx,r8              -- count
            syscall
            pop rsi                 -- restore
            or rax,rax 
            jns @f
                mov rdi,rax                             -- ep1
                mov rdx,[rbp+24]                        -- "called from" address
                mov rbp,[rbp+40]                        -- prev_ebp
                mov al,98                               -- e98fiofe -- flush error [ep1]
                sub rdx,1
                jmp :!iDiag
                int3
          @@:
        [64]
          ::flushret
--added|resurrected 5/4/16 (spotted in passing)
            lea rdi,[rsi+BUFF64]
            ret
        []
--        ::fin
          }

--global procedure flush(integer fn)
procedure fflush(integer fn)
-- write any output still in a buffer to disk.
-- this is performed automatically by close.
-- flush(1) may sometimes be needed (eg when i/o is redirected) before 
-- a getc or gets(0), to force display before input.
-- flush(-1) flushes all open files (but not the console).
integer fidx
--atom fhandle

--DEV -9?
    if fn=-1 then
        for i=1 to fdmax do
            flush(i+2)  --DEV better!
--/*
            #ilASM{
                [32]
                    mov edx,[i]
                    shl edx,2   -- edx:=i*4
                  @@:
                    mov edi,[fdtbl]
                    mov esi,[edi*4+edx-4] -- edi:=fdtbl[i]
                    cmp edi,[fdtbl]
                    jne @b
                    mov edi,[ebx+esi*4+MODE]
                    test edi,F_DIRTY
                    jz @f
                        shl esi,2
                        sub edi,F_DIRTY
                        call :%n_flush_esiedi
                  @@:
                [64]
                    pop al
                []
                    }
--*/
        end for
        return
    end if
    if fn>2 then
        fidx = fn-2
        if fidx<=fdmax then
--DEV newsize [PE32], get_this()?
--          {iThis,fidx,...} = get_this(fn,F_DIRTY)
            #ilASM{
                [32]
                    mov edx,[fidx]
                    shl edx,2               -- edx:=(fidx*4) (nb fidx=fn-2)
                  @@:
                    mov edi,[fdtbl]
                    mov esi,[edi*4+edx-4]   -- esi:=fdtbl[fidx]
                    cmp edi,[fdtbl]
                    jne @b
                    push dword[ebx+esi*4]           -- hFile (param for FlushFileBuffers)
                    mov edi,[ebx+esi*4+MODE]
--                  test edi,edi
--                  jnz @f
--                      -- e62fnnino:"file number %d is not open"
--                      mov al,e62
--                      mov edi,[fn]        -- ep1:=fn
--                      xor esi,esi         -- ep2 unused
--                      call :%pRTErn       -- fatal error
--               @@:
                    test edi,F_DIRTY
                    jz @f
                        shl esi,2
                        sub edi,F_DIRTY
                        call :%n_flush_esiedi
                  @@:
                [PE32]
                    call "kernel32.dll","FlushFileBuffers"
--              [ELF32]
--                  pop al
                [64]
                    mov rdx,[fidx]
                    shl rdx,3               -- rdx:=(fidx*8) (nb fidx=fn-2)
                  @@:
                    mov rdi,[fdtbl]
                    mov rsi,[rdi*4+rdx-8]   -- rsi:=fdtbl[fidx]
                    cmp rdi,[fdtbl]
                    jne @b
                    mov rdi,[rbx+rsi*4+MODE64]
                    test rdi,F_DIRTY
                    jz @f
                        shl rsi,2
                        sub rdi,F_DIRTY
                        call :%n_flush_rsirdi       -- (rsi is preserved)
                  @@:
                [PE64]
                    mov rcx,rsp -- put 2 copies of rsp onto the stack...
                    push rsp
                    push rcx
                    or rsp,8    -- [rsp] is now 1st or 2nd copy:
                                -- if on entry rsp was xxx8: both copies remain on the stack
                                -- if on entry rsp was xxx0: or rsp,8 effectively pops one of them (+8)
                                -- obviously rsp is now xxx8, whichever alignment we started with

                    sub rsp,8*5
                    mov rcx,[rbx+rsi*4]                 -- hFile
                    call "kernel32.dll","FlushFileBuffers"
--                  add rsp,8*5
--                  pop rsp
                    mov rsp,[rsp+8*5]   -- equivalent to the add/pop
--              [ELF64]
--                  pop al
                []
                  }
        end if
    elsif fn<=0 then
        -- flush(0) [stdin] is clearly a programming error
--      iofatal(e59wfmfao)
        #ilASM{
            [32]
                mov edx,[ebp+12]                    -- "called from" address
                mov ebp,[ebp+20]                    -- prev_ebp
                mov al,59                           -- e59wfmfao: "wrong file mode for attempted operation"
                sub edx,1
            [64]
                mov rdx,[rbp+24]                    -- "called from" address
                mov rbp,[rbp+40]                    -- prev_ebp
                mov al,59                           -- e59wfmfao: "wrong file mode for attempted operation"
                sub rdx,1
            []
                jmp :!iDiag
              }
    else -- fn=1 (stdout) or fn=2 (stderr)
--      if not cinit then initConsole() end if
--DEV newsize [PE32]
        #ilASM{
            [32]
                mov eax,[stdout]
                test eax,eax
                jnz @f
                    call :%n_initC -- (preserves eax)
--                  mov eax,[stdout]
              @@:
                cmp [fn],1
                je @f
--                  jl :e59wfmfao   -- flush(0[=stdin]) => "wrong file mode for attempted operation"
                    mov eax,[stderr]
              @@:
            [PE32]
                push eax                            -- hFile
                call "kernel32.dll","FlushFileBuffers"
--          [ELF32]
--              pop al
            [64]
                mov rax,[stdout]
                test rax,rax
                jnz @f
                    call :%n_initC -- (preserves rax)
--                  mov rax,[stdout]
              @@:
                cmp [fn],1
                je @f
--                  jl :e59wfmfao   -- flush(0[=stdin]) => "wrong file mode for attempted operation"
                    mov rax,[stderr]
              @@:
            [PE64]
                mov rcx,rsp -- put 2 copies of rsp onto the stack...
                push rsp
                push rcx
                or rsp,8    -- [rsp] is now 1st or 2nd copy:
                            -- if on entry rsp was xxx8: both copies remain on the stack
                            -- if on entry rsp was xxx0: or rsp,8 effectively pops one of them (+8)
                            -- obviously rsp is now xxx8, whichever alignment we started with
                sub rsp,8*5
                mov rcx,rax                         -- hFile
                call "kernel32.dll","FlushFileBuffers"
--              add rsp,8*5
--              pop rsp
                mov rsp,[rsp+8*5]   -- equivalent to the add/pop
--          [ELF64]
--              pop al
            []
              }
    end if
end procedure

--global procedure close(integer fn)
procedure fclose(integer fn)
--
-- Close a file or device and flush out any still-buffered characters. 
-- It is not particularly an error to close the same file twice, the
-- second and subsequent calls will simply have no effect.
-- close(-9) closes all open files. Used as part of the normal shutdown process. 
--  Be warned however this will cause catastrophic failure on a multithreaded 
--  program if any still-running threads attempt any further file access.
--
integer fidx
--integer fmode
--DEV fmode=0?
--atom fhandle
--atom this
integer iThis

    if fn=-9 then
        if finit then
            for i=1 to fdmax do -- (file nos 3 and up)
                --DEV/SUG (add locking?)
--              if length(filenames[i]) then
--                  printf(1,"warning: close(-9) automatically closing %s\n",{filenames[i]})
                fclose(i+2)
--                  filenames[i] = ""
--              end if
            end for
            -- (DEV) may want a few free() here...
--(eg)
            enter_cs(fdcs)
            for i=1 to fdmax do -- (file nos 3 and up)
                iThis = fdtbl[i]
--              fdtbl[i] = 0
                free(iThis*4)           
            end for
            fdtbl = {}
            freelist = 0
            fdmax = 0
            leave_cs(fdcs)
            delete_cs(fdcs)
            finit = 0
        end if
    elsif fn>2 then
--      {iThis,fidx,..} = get_this(fn,F_DIRTY)
        if not finit then initF() end if
        fidx = fn-2
        if fidx<=fdmax then
--DEV newsize [PE32,ELF32], get_this()?
            #ilASM{
                [32]
                    mov edx,[fidx]
                    shl edx,2
                  @@:
                    mov edi,[fdtbl]
                    mov esi,[edx+edi*4-4]   -- esi:=fdtbl[fidx]
                    cmp edi,[fdtbl]
                    jne @b
                    mov eax,[ebx+esi*4+MODE]
                    mov [iThis],esi
--                  mov [fmode],eax
--DEV move down? (/make all (including E/LCS, which are in p -imports) of this asm)
--          fhandle = peek4u(iThis*4+HNDL)
--          fmode = peek4u(iThis*4+MODE)
--          if fmode!=0 then
                    test eax,eax
                    jz :alreadyclosed
--              if and_bits(fmode,F_DIRTY) then
                    test eax,F_DIRTY
                    jz @f
--                      mov esi,[iThis]
                        push esi
                        xor edi,edi         -- mov edi,0 (F_CLOSED)
                        shl esi,2
                        call :%n_flush_esiedi
                        pop esi
--              end if
                   @@:
--              poke4(iThis*4+MODE,F_CLOSED)
                    mov dword[ebx+esi*4+MODE],F_CLOSED
--              if c_func(xCloseHandle,{fhandle}) then end if
                [PE32]
                    push dword[ebx+esi*4+HNDL]      -- hObject
                    call "kernel32.dll","CloseHandle"
                [ELF32]
--#     Name                        Registers                                                                                                               Definition
--                                  eax     ebx                     ecx                     edx                     esi                     edi
--6     sys_close                   0x06    unsigned int fd         -                       -                       -                       -               fs/open.c:969
                    mov eax,6   -- sys_close
                    mov ebx,[ebx+esi*4+HNDL]
                    int 0x80
                    xor ebx,ebx             -- (common requirement after int 0x80)
                [64]
                    mov rdx,[fidx]
                    shl rdx,3
                  @@:
                    mov rdi,[fdtbl]
                    mov rsi,[rdx+rdi*4-8]   -- rsi:=fdtbl[fidx]
                    cmp rdi,[fdtbl]
                    jne @b
                    mov rax,[rbx+rsi*4+MODE64]
                    mov [iThis],rsi
                    test rax,rax
                    jz :alreadyclosed
                    test rax,F_DIRTY
                    jz @f
                        push rsi
                        xor rdi,rdi         -- mov rdi,0 (F_CLOSED)
                        shl rsi,2
                        call :%n_flush_rsirdi   -- (preserves rsi, ..., but we damaged it)
                        pop rsi
                   @@:
                    mov dword[rbx+rsi*4+MODE64],F_CLOSED
                [PE64]
                    mov rcx,rsp -- put 2 copies of rsp onto the stack...
                    push rsp
                    push rcx
                    or rsp,8    -- [rsp] is now 1st or 2nd copy:
                                -- if on entry rsp was xxx8: both copies remain on the stack
                                -- if on entry rsp was xxx0: or rsp,8 effectively pops one of them (+8)
                                -- obviously rsp is now xxx8, whichever alignment we started with
                    sub rsp,8*5
                    mov rcx,[rbx+rsi*4+HNDL64]      -- hObject
                    call "kernel32.dll","CloseHandle"
--                  add rsp,8*5
--                  pop rsp
                    mov rsp,[rsp+8*5]   -- equivalent to the add/pop
                [ELF64]
--%rax  System call             %rdi                    %rsi                            %rdx                    %rcx                    %r8                     %r9
--3     sys_close               unsigned int fd
                    mov rax,3   -- sys_close
                    mov rdi,[rbx+rsi*4+HNDL64]
                    syscall
                []
                  }
            -- and add to freelist
            enter_cs(fdcs)
            poke4(iThis*4,freelist)
            freelist = fidx
            leave_cs(fdcs)
--          end if
            #ilASM { ::alreadyclosed }
        end if
    end if
end procedure
--DEV set callback here... (for opClosem9)


--DEV builtin? [DONE]
--/*
old function peek8u(atom addr)
-- (Use of atoms limits accuracy to 2^53 or 9,007,199,254,740,992
--                  instead of 2^64 or 18,446,744,073,709,551,616.)
atom res
--DEV newsize [PE32],:%pStoreFlt
    #ilASM{
        [32]
            mov eax,[addr]
            lea edi,[res]
            cmp eax,h4
            jl @f
                sub esp,8
                fld qword[ebx+eax*4]
                fistp qword[esp]
                pop eax
                add esp,4
          @@:
            fild qword[eax]
            call :%pStoreFlt    -- ([edi]:=ST0)
        [64]
            mov r15,h4
            mov rax,[addr]
            lea rdi,[res]
            cmp rax,r15
            jl @f
                sub rsp,8
                fld tbyte[rbx+rax*4]
                fistp qword[rsp]
                pop rax
          @@:
            fild qword[rax]
            call :%pStoreFlt    -- ([rdi]:=ST0)
          }
    return res
end function
--*/

--DEV... allow overrides?
--/*
old procedure poke8(atom addr, atom a)
-- (Use of atoms limits accuracy to 2^53 or 9,007,199,254,740,992
--   on 32-bit Phix instead of 2^64 or 18,446,744,073,709,551,616.)
    #ilASM{
        [32]
            mov eax,[addr]
            mov ecx,[a]
            cmp eax,h4
            jl @f
                sub esp,8
                fld qword[ebx+eax*4]
                fistp qword[esp]
                pop eax
                add esp,4
          @@:
            push ebx -- (0)
            push ecx
            cmp ecx,h4
            jl @f
                fld qword[ebx+ecx*4]
                fistp qword[esp]
          @@:
            fild qword[esp]
            add esp,8
            fistp qword[eax]
        [64]
            mov r15,h4
            mov rax,[addr]
            sub rsp,8
            mov rcx,[a]
            cmp rax,r15
            jl @f
                fld tbyte[rbx+rax*4]
                fistp qword[rsp]
                mov rax,[rsp]
          @@:
            cmp rcx,r15
            jl @f
                fld tbyte[rbx+rcx*4]
                fistp qword[rsp]
                mov rcx,[rsp]
          @@:
            add rsp,8
            mov [rax],rcx
          }
end procedure
--*/

--DEV/SUG: call this directly from within this file, rather than seek() aka :%opSeek.
--global function seek(integer fn, atom pos)
function fseek(integer fn, atom pos)
--
-- usage:
--          if seek(fn,pos)!=SEEK_OK then
--              ?9/0 -- some error
--          end if
--
-- If you find any code that does not use SEEK_OK (defined as 0 in psym.e)
--  either a) change it to use SEEK_OK or b) be advised that seek returns 0 
--  if the seek was successful, 1 if it was not (somewhat counter-intuitive,
--  but not worth changing given that it will break mountains of legacy code).
--
integer fidx
integer fmode
integer fend
--atom fhandle
atom workpos    -- (may exceed integer during tests)
atom frealposn
integer iThis

    if not finit then initF() end if
    fidx = fn-2
    fmode = 0
    if fidx>=1 and fidx<=fdmax then
--DEV/SUG:
--      {iThis,fidx,..} = get_this(fn,F_DIRTY)
--      fmode = peek4u(iThis*4+MODE)
        fmode = 1   --DEV should really fix this... (compiler issue)
        #ilASM{
            [32]
                mov edx,[fidx]
                shl edx,2
              @@:
                mov edi,[fdtbl]
                mov esi,[edi*4+edx-4]   -- esi:=fdtbl[fidx]
                cmp edi,[fdtbl]
                jne @b
                mov eax,[ebx+esi*4+MODE]
                mov [iThis],esi
                mov [fmode],eax
            [64]
                mov rdx,[fidx]
                shl rdx,3
              @@:
                mov rdi,[fdtbl]
                mov rsi,[rdi*4+rdx-8]   -- rsi:=fdtbl[fidx]
                cmp rdi,[fdtbl]
                jne @b
                mov rax,[rbx+rsi*4+MODE64]
                mov [iThis],rsi
                mov [fmode],rax
              }
    end if
    if fmode=0 then
--      iofatal(58,fn)  -- "invalid file number (%d)"
        #ilASM{
            [32]
                mov edi,[fn]                        -- ep1:=fn
                mov edx,[ebp+12]                    -- "called from" address
                mov ebp,[ebp+20]                    -- prev_ebp
                mov al,58                           -- e58bfn: "bad file number"
                xor esi,esi                         -- ep2 unused
                sub edx,1
            [64]
                mov rdi,[fn]                        -- ep1:=fn
                mov rdx,[rbp+24]                    -- "called from" address
                mov rbp,[rbp+40]                    -- prev_ebp
                mov al,58                           -- e58bfn: "bad file number"
                xor rsi,rsi                         -- ep2 unused
                sub rdx,1
            []
                jmp :!iDiag
              }
    end if
    if pos=-1 then -- eof
        if and_bits(fmode,F_DIRTY) then
            #ilASM{
                [32]
                    mov esi,[iThis]
                    mov edi,[fmode]
                    shl esi,2
                    sub edi,F_DIRTY
                    call :%n_flush_esiedi
                [64]
                    mov rsi,[iThis]
                    mov rdi,[fmode]
                    shl rsi,2
                    sub rdi,F_DIRTY
                    call :%n_flush_rsirdi
                  }
        end if
        #ilASM{
            [32]
                mov esi,[iThis]
            [PE32]
                push ebx        -- DistanceToMoveHigh (0), and rposn hiword (edx)
                shl esi,2
                mov edi,esp
                push esi        -- save [DEV better:restore iThis b4 cmp eax,-1 and use ebx+esi*4?] (spotted in passing)
                push FILE_END                       -- dwMoveMethod
                push edi                            -- lpDistanceToMoveHigh
                push ebx                            -- lDistanceToMove (0)
                push dword[esi]                     -- hFile
                call "kernel32.dll","SetFilePointer"
                mov ecx,eax     -- rposn loword
                cmp eax,-1
                jne @f
                    call "kernel32.dll","GetLastError"
                    cmp eax,NO_ERROR
                    jne @f
                        -- return 1
                        mov eax,1
                        jmp :%opRetf
              @@:
                pop esi     -- restore
                pop edx     -- rposn hiword
            [ELF32]
--#     Name                        Registers                                                                                                               Definition
--                                  eax     ebx                     ecx                     edx                     esi                     edi
--140   sys_llseek                  0x8c    unsigned int fd         unsigned long offset_high   unsigned long offset_low    loff_t *result  unsigned int origin     fs/read_write.c:191
-- (I assume loff_t is a "long long" aka int64)
--SEEK_SET              =       0
--SEEK_CUR              =       1
--SEEK_END              =       2
                shl esi,2
                mov eax,140         -- sys_llseek
                push esi            -- save
                sub esp,8           -- space for loff_t result
                mov ebx,[esi]       -- fd
                xor ecx,ecx         -- offset_high
                xor edx,edx         -- offset_low
                mov esi,esp         -- *result
                mov edi,2           -- SEEK_END
                int 0x80
                xor ebx,ebx         -- (common requirement after int 0x80)
                cmp eax, -4069 
                jbe @f
--DEV stack imbalance...
                    -- return 1
                    mov eax,1
                    jmp :%opRetf
              @@:
                pop ecx             -- offset low
                pop edx             -- offset high
                pop esi             -- restore
            [32]
                mov [esi+POSN],dword 1
                mov [esi+FEND],ebx --(0)
                mov [esi+POSL],ecx
                mov [esi+POSH],edx
            [PE64]
                mov rcx,rsp -- put 2 copies of rsp onto the stack...
                push rsp
                push rcx
                or rsp,8    -- [rsp] is now 1st or 2nd copy:
                            -- if on entry rsp was xxx8: both copies remain on the stack
                            -- if on entry rsp was xxx0: or rsp,8 effectively pops one of them (+8)
                            -- obviously rsp is now xxx8, whichever alignment we started with
                sub rsp,8*5                         -- minimum 4 param shadow space, newFilePointer, save rax, and align
                mov rsi,[iThis]
                mov r9,FILE_END                     -- dwMoveMethod
                lea r8,[rsp+32]                     -- lpNewFilePointer
                mov rdx,rbx                         -- liDistanceToMove (0)
                mov rcx,[rbx+rsi*4+HNDL64]          -- hFile
                call "kernel32.dll","SetFilePointerEx"
                cmp rax,0
                jne @f
                    -- return 1
                    mov rax,1
                    jmp :%opRetf
              @@:
                mov rcx,[rsp+32]
--              add rsp,8*5
--              pop rsp
                mov rsp,[rsp+8*5]   -- equivalent to the add/pop
                mov [rbx+rsi*4+POSN64],qword 1
                mov [rbx+rsi*4+FEND64],rbx --(0)
                mov [rbx+rsi*4+RPOS64],rcx
            [ELF64]
--%rax  System call             %rdi                    %rsi                            %rdx                    %rcx                    %r8                     %r9
--8     sys_lseek               unsigned int fd         off_t offset                    unsigned int origin
                mov r12,[iThis]
                mov rax,8                           -- sys_lseek
                xor rsi,rsi
                mov rdx,2                           -- SEEK_END
                mov rdi,[rbx+r12*4+HNDL64]          -- hFile
                syscall
                cmp rax, -1
                jne @f
--DEV stack imbalance...
                    -- return 1
                    mov rax,1
                    jmp :%opRetf
              @@:
                mov [rbx+r12*4+POSN64],qword 1
                mov [rbx+r12*4+FEND64],rbx --(0)
                mov [rbx+r12*4+RPOS64],rax
              }
        return 0                            -- success
    end if

    if machine_bits()=32 then
--      frealposn = peek8u(iThis*4+POSL)
        frealposn = peek4u(iThis*4+POSL)+(peek4u(iThis*4+POSH)*#100000000)
        fend = peek4u(iThis*4+FEND)
    else -- machine_bits()=64
        frealposn = peek8u(iThis*4+RPOS64)
        fend = peek8u(iThis*4+FEND64)
    end if
    if and_bits(fmode,F_DIRTY) then
        -- realposn corresponds to the \\start\\ of the buffer,
        -- and we can extend/write beyond fend upto BUFFERSIZE.
        -- (however, avoid any buffer-gap-posn type nonsense)
        workpos = pos-frealposn+1
        if machine_bits()=32 then
            if workpos>=1 and workpos<=BUFFERSIZE32 and workpos<=fend+1 then
                poke4(iThis*4+POSN,workpos)
                return 0                            -- success
            end if
        else -- machine_bits()=64
            if workpos>=1 and workpos<=BUFFERSIZE64 and workpos<=fend+1 then
--DEV (compiler crashes)
--              poke8(iThis*4+POSN64,workpos)
                poke4(iThis*4+POSN64,and_bits(workpos,#FFFFFFFF))
                poke4(iThis*4+POSN64+4,floor(workpos/#100000000))
                return 0                            -- success
            end if
        end if
        #ilASM{
            [32]
                mov esi,[iThis]
                mov edi,[fmode]
                shl esi,2
                sub edi,F_DIRTY
                call :%n_flush_esiedi
            [64]
                mov rsi,[iThis]
                mov rdi,[fmode]
                shl rsi,2
                sub rdi,F_DIRTY
                call :%n_flush_rsirdi
              }
    else
        -- realposn corresponds to the \\end\\ of the buffer, but
        -- in this case fend is the hard limit of readable bytes.
        workpos = pos-(frealposn-fend)+1
        if workpos>=1 and workpos<=fend then
            if machine_bits()=32 then
                poke4(iThis*4+POSN,workpos)
            else --64
--DEV as above...
--              poke8(iThis*4+POSN64,workpos)
                poke4(iThis*4+POSN64,and_bits(workpos,#FFFFFFFF))
                poke4(iThis*4+POSN64+4,floor(workpos/#100000000))
            end if
            return 0                            -- success
        end if
    end if
--DEV newsize [PE32,ELF32]
--DEV sug:
--/*
    #ilASM{
        [32]
            mov esi,[iThis]
--          mov ecx,[pos]
            mov eax,[pos]
        [64]    
            mov rsi,[iThis]
--          mov rdx,[pos]
            mov rax,[pos]
        []
            call :%pLoadMint    -- eax:=(int32)eax; edx:=hi_dword
...
            call :%n_seek_esi_eax
        }
--*/
    #ilASM{
        [32]
            mov esi,[iThis]
            push ebx                                -- DistanceToMoveHigh (0), and rposn hiword (edx)
            shl esi,2
            mov ecx,[pos]
            mov edi,esp                             -- (lpDistanceToMoveHigh)
            cmp ecx,h4  --DEV :%pLoadMint
            jl @f
                push ebx
--              fld qword[ebx+eax*4]
                fld qword[ebx+ecx*4]
                fistp qword[esp]                    -- nb: [esp+4]=[edi]=*lpDistanceToMoveHigh just got set
                pop ecx                             -- DistanceToMove (loword)
          @@:
        [PE32]
            push esi                                -- save
            push FILE_BEGIN                         -- dwMoveMethod
            push edi                                -- lpDistanceToMoveHigh
            push ecx                                -- lDistanceToMove
            push dword[esi+HNDL]                    -- hFile
            call "kernel32.dll","SetFilePointer"
            mov ecx,eax     -- rposn loword
            cmp eax,-1
            jne @f
                call "kernel32.dll","GetLastError"
                cmp eax,NO_ERROR
                jne @f
                    add esp,8
                    -- return 1 (failure)
                    mov eax,1
                    jmp :%opRetf
          @@:
            pop esi                                 -- restore
            pop edx                                 -- rposn hiword
        [ELF32]
--#     Name                        Registers                                                                                                               Definition
--                                  eax     ebx                     ecx                     edx                     esi                     edi
--140   sys_llseek                  0x8c    unsigned int fd         unsigned long offset_high   unsigned long offset_low    loff_t *result  unsigned int origin     fs/read_write.c:191
            mov edx,ecx         -- offset_low
            pop ecx             -- offset_high
            mov eax,140         -- sys_llseek
            push esi            -- save
            sub esp,8           -- space for loff_t result
            mov ebx,[esi+HNDL]  -- fd
            mov esi,esp         -- *result
            mov edi,0           -- SEEK_SET
            int 0x80
            xor ebx,ebx         -- (common requirement after int 0x80)
            cmp eax, -4069 
            jbe @f
                -- return 1
                mov eax,1
                jmp :%opRetf
          @@:
            pop ecx             -- offset low
            pop edx             -- offset high
            pop esi             -- restore
        [32]
            mov [esi+POSN],dword 1
            mov [esi+FEND],ebx -- (0)
            mov [esi+POSL],ecx
            mov [esi+POSH],edx
        [64]
            mov rsi,[iThis]
            mov rdx,[pos]
            mov r15,h4
            cmp rdx,r15
            jl @f
--DEV %pLoadMint
                push rbx
                fld tbyte[rbx+rdx*4]
                fistp qword[rsp]
                pop rdx
          @@:
        [PE64]
            mov rcx,rsp -- put 2 copies of rsp onto the stack...
            push rsp
            push rcx
            or rsp,8    -- [rsp] is now 1st or 2nd copy:
                        -- if on entry rsp was xxx8: both copies remain on the stack
                        -- if on entry rsp was xxx0: or rsp,8 effectively pops one of them (+8)
                        -- obviously rsp is now xxx8, whichever alignment we started with
            sub rsp,8*5                         -- minimum 4 param shadow space, newFilePointer, and align(0)
            mov r9,FILE_BEGIN                   -- dwMoveMethod
            lea r8,[rsp+32]                     -- lpNewFilePointer
--          (rdx)                               -- liDistanceToMove
            mov rcx,[rbx+rsi*4+HNDL64]          -- hFile
            call "kernel32.dll","SetFilePointerEx"
            cmp rax,0
            jne @f
                -- return 1 (failure)
                mov rax,1
                jmp :%opRetf
          @@:
            mov rcx,[rsp+32]
--          add rsp,8*5
--          pop rsp
            mov rsp,[rsp+8*5]   -- equivalent to the add/pop
            mov [rbx+rsi*4+POSN64],qword 1
            mov [rbx+rsi*4+FEND64],rbx --(0)
            mov [rbx+rsi*4+RPOS64],rcx
        [ELF64]
--%rax  System call             %rdi                    %rsi                            %rdx                    %rcx                    %r8                     %r9
--8     sys_lseek               unsigned int fd         off_t offset                    unsigned int origin
--          mov r12,[iThis]
            mov r12,rsi
            mov rdi,[rbx+rsi*4+HNDL64]          -- hFile
            mov rax,8                           -- sys_lseek
--16/2/17 (spotted in passing!)
--          xor rsi,rsi
            mov rsi,rdx
--14/2/17:
--          mov rdx,2                           -- SEEK_END
            mov rdx,0                           -- SEEK_SET
--          mov rdi,[rbx+r12*4+HNDL64]          -- hFile
            syscall
            cmp rax, -1
            jne @f
                -- return 1
                mov rax,1
                jmp :%opRetf
          @@:
            mov [rbx+r12*4+POSN64],qword 1
            mov [rbx+r12*4+FEND64],rbx --(0)
            mov [rbx+r12*4+RPOS64],rax
          }
    return 0                                -- success
end function

--global function where(integer fn)
function fwhere(integer fn)
integer fidx
--integer fmode
--integer fend
--integer fposn
atom frealposn
--atom this
--integer iThis

--  {iThis,fidx,fmode,..} = get_this(fn,0)
    if not finit then initF() end if
    fidx = fn-2
    if fidx<1 or fidx>fdmax then
--      iofatal(58,fn)  -- "invalid file number (%d)"
        fatalN(2, e58ifn, fn)
--/*
        #ilASM{
            [32]
                mov edx,[ebp+12]                    -- "called from" address
                mov ebp,[ebp+20]                    -- prev_ebp
                mov al,57                           -- e57ifn: "invalid file name"
                sub edx,1
            [64]
                mov rdx,[rbp+24]                    -- "called from" address
                mov rbp,[rbp+40]                    -- prev_ebp
                mov al,57                           -- e57ifn: "invalid file name"
                sub rdx,1
            []
                jmp :!iDiag
              }
--*/
    end if
--DEV newsize [PE32,ELF32], get_this()?
    #ilASM{
        [32]
            mov edx,[fidx]
            shl edx,2
          @@:
            mov edi,[fdtbl]
            mov esi,[edx+edi*4-4]   -- esi:=fdtbl[fidx]
            cmp edi,[fdtbl]
            jne @b
            mov eax,[ebx+esi*4+MODE]
--          mov ecx,[ebx+esi*4+POSN]
--          mov [iThis],esi
--          mov [fmode],eax
--          mov [fposn],ecx
            fild qword[ebx+esi*4+POSL]
            fild dword[ebx+esi*4+POSN]
            faddp st1,st0
            test eax,F_DIRTY
            jnz @f
                fild dword[ebx+esi*4+FEND]
                fsubp st1,st0
          @@:
            lea edi,[frealposn]
            call :%pStoreFlt
        [64]
            mov rdx,[fidx]
--          shl rdx,2
            shl rdx,3
          @@:
            mov rdi,[fdtbl]
            mov rsi,[rdx+rdi*4-8]   -- rsi:=fdtbl[fidx]
            cmp rdi,[fdtbl]
            jne @b
            mov rax,[rbx+rsi*4+MODE64]
            fild qword[rbx+rsi*4+RPOS64]
            fild qword[rbx+rsi*4+POSN64]
            faddp st1,st0
            test rax,F_DIRTY
            jnz @f
                fild qword[rbx+rsi*4+FEND64]
                fsubp st1,st0
          @@:
            lea rdi,[frealposn]
            call :%pStoreFlt
          }
--DEV newsize [32,64]/machine_bits()
--  frealposn = peek8u(iThis*4+POSL)
--  fposn = peek4u(iThis*4+POSN)
--  frealposn += fposn-1
--  fmode = peek4u(iThis*4+MODE)
--  if not and_bits(fmode,F_DIRTY) then
--      fend = peek4u(iThis*4+FEND)
--      frealposn -= fend
--  end if
    frealposn -= 1
    return frealposn
end function

--[DEV] epic fail: (DOH, was not setting up eax/stdout!!) [btw: there's another commented-out puts1seq below]
--procedure puts1seq(sequence s)
--? if not cinit then initConsole() end if
--  if not string(s) then
--      s = toString(s,e65sfics)
--  end if
--DEV newsize [PE32]
--  #ilASM{
--      [32]
--          cmp [stdout],0
--          jne @f
--              call :%n_initC -- (preserves eax)
--        @@:
--          mov edx,[s]
--          mov ecx,[ebx+edx*4-12]      -- length
--          shl edx,2
--          push ebx                    -- lpOverlapped (NULL)
--          push esp                    -- lpNumberOfBytesWritten
--          push ecx                    -- nNumberOfBytesToWrite
--          push edx                    -- lpBuffer
--          push [stdout]               -- hFile,
--          call "kernel32.dll","WriteFile"
--      [64]
--          pop al
--      [ELF32]
--          pop al
--      [PE64]
--          pop al
--      [ELF64]
--          pop al
--        }
--end procedure
--
--puts1seq({'a','b','\n'})

--DEV newsize [PE32]
#ilASM{ jmp :fin
--/*
global function getc(integer fn, :%opGetc)
end function
--*/
  :%opGetc
----------
    [32]
        -- fn in eax
        -- dest in edi
        -- if dest is integer, result in ecx, range -1..255, done by opGetc in pilx86.e
        push dword[edi]
        push edi
        lea ecx,[ebx+eax*4]                     -- ecx:=fn*4 [fn->0-based byte idx+12]
        mov edi,[fdtbl]
        sub eax,3 -- [fn->0-based idx]
        jb :getc0
        cmp eax,[fdmax]
        jb @f
      ::getce58bfn
            add esp,8
      ::getce58bfnNP
            lea edi,[eax+3]                     -- ep1:=fn
            mov al,58                           -- e58bfn: "bad file number"
            pop edx
            xor esi,esi                         -- ep2 unused
--          call :%pRTErn                       -- fatal error
            sub edx,1
            jmp :!iDiag
            int3
      ::retryx
          mov edi,[fdtbl]
      @@:
        mov esi,[edi*4+ecx-12]                  -- esi:=fdtbl[fn-2]
        cmp edi,[fdtbl]
        jne :retryx
        shl esi,2
        mov edi,[esi+MODE]
        mov eax,[esi+POSN]
        test edi,F_READ
--      jz :e59wfmfao
        jnz @f
            mov edx,[esp+8]
            mov al,59   -- e59wfmfao: "wrong file mode for attempted operation"
--          add esp,12
            sub edx,1
            jmp :!iDiag         -- fatal error (see pdiagN.e)
            int3
      @@:
        mov ecx,ebx                             -- ecx:=0
      ::looptop
        cmp eax,[esi+FEND]
        jle :notfull                            -- jl @f    [?] [DEV]
            push esi
            mov edi,[esi+MODE]
            test edi,F_DIRTY
            jz @f
                sub edi,F_DIRTY
                call :%n_flush_esiedi
                mov esi,[esp]
          @@:
            lea ecx,[esi+FEND]
            lea edi,[esi+BUFF]
        [PE32]
            push ebx                                        -- lpOverlapped (NULL)
            push ecx                                        -- lpNumberOfBytesRead (fend)
            push BUFFERSIZE32                               -- nNumberOfBytesToRead
            push edi                                        -- lpBuffer
            push [esi+HNDL]                                 -- hFile
            call "kernel32.dll","ReadFile"
            pop esi
            test eax,eax
            jz :retm1
            cmp dword[esi+FEND],0
            je :retm1
        [ELF32]
--#     Name                        Registers                                                                                                               Definition
--                                  eax     ebx                     ecx                     edx                     esi                     edi
--3     sys_read                    0x03    unsigned int fd         char *buf               size_t count            -                       -               fs/read_write.c:391
            mov eax,3   -- sys_read
            mov ebx,[esi+HNDL]
            mov ecx,edi
            mov edx,BUFFERSIZE32
            int 0x80
            xor ebx,ebx             -- (common requirement after int 0x80)
--          cmp eax, -4069 
--          jbe @f
--              mov eax,INVALID_HANDLE_VALUE
--        @@:
            pop esi
            test eax,eax
            jle :retm1
            mov dword[esi+FEND],eax
        [32]
--DEV replace with proper 64-bit maths:
            fild qword[esi+POSL]
            fild dword[esi+FEND]
            mov ecx,ebx -- ecx:=0
            faddp
            mov eax,1
            fistp qword[esi+POSL]
--/*
            mov eax,[esi+POSL]
            mov ecx,[esi+POSH]
--          add eax,edx -- frealposn += fend
            add eax,[esi+FEND] -- frealposn += fend
            adc ecx,ebx
            mov edi,1
            mov [esi+POSL],eax
            mov [esi+POSH],ecx
            mov [esi+POSN],edi
--*/
--          mov [esi+POSN],eax (unnecessary?)
      ::notfull
        mov cl,byte[esi+eax+BUFF-1]
        add eax,1                                           -- fposn+=1
        cmp cl,'\r'
        jne @f
        test dword[esi+MODE],F_BINARY
        jz :looptop
      @@:
        mov [esi+POSN],eax
      ::opGetcStoreEcx
        pop edi         -- (result addr)
        pop edx         -- (prev content)
        cmp edx,h4
        mov [edi],ecx
        jle @f
            sub dword[ebx+edx*4-8],1
            je :%pDealloc
      @@:
        ret

      ::retm1
        mov ecx,-1
        jmp :opGetcStoreEcx

    [64]
        -- fn in rax
        -- dest in rdi
        -- if dest is integer, result in rcx, range -1..255, done by opGetc in pilx86.e
        push qword[rdi]
        push rdi
        lea rcx,[rbx+rax*8]                     -- rcx:=fn*8 [fn->0-based byte idx+24]
        mov rdi,[fdtbl]
        sub rax,3 -- [fn->0-based idx]
        jb :getc0
        cmp rax,[fdmax]
        jb @f
      ::getce58bfn
            add rsp,16
      ::getce58bfnNP
            lea rdi,[rax+3]                     -- ep1:=fn
            mov al,58                           -- e58bfn: "bad file number"
            pop rdx
            xor rsi,rsi                         -- ep2 unused
--          call :%pRTErn                       -- fatal error
            sub rdx,1
            jmp :!iDiag
            int3
      ::retryx
          mov rdi,[fdtbl]
      @@:
        mov rsi,[rdi*4+rcx-24]                  -- rsi:=fdtbl[fn-2]
        cmp rdi,[fdtbl]
        jne :retryx
        shl rsi,2
        mov rdi,[rsi+MODE64]
        mov rax,[rsi+POSN64]
        test rdi,F_READ
--      jz :e59wfmfao
        jnz @f
            mov rdx,[rsp+16]
            mov al,59   -- e59wfmfao: "wrong file mode for attempted operation"
--          add rsp,24
            sub rdx,1
            jmp :!iDiag         -- fatal error (see pdiagN.e)
            int3
      @@:
        mov rcx,rbx                             -- rcx:=0
      ::looptop
        cmp rax,[rsi+FEND64]
        jle :notfull                            -- jl @f    [?] [DEV]
            mov rdi,[rsi+MODE64]
            test rdi,F_DIRTY
            jz @f
                sub rdi,F_DIRTY
                call :%n_flush_rsirdi           -- (rsi is preserved)
          @@:
        [PE64]
            mov rcx,rsp -- put 2 copies of rsp onto the stack...
            push rsp
            push rcx
            or rsp,8    -- [rsp] is now 1st or 2nd copy:
                        -- if on entry rsp was xxx8: both copies remain on the stack
                        -- if on entry rsp was xxx0: or rsp,8 effectively pops one of them (+8)
                        -- obviously rsp is now xxx8, whichever alignment we started with
            sub rsp,8*5                         -- minimum 4 param shadow space, lpOverlapped, and align(none here)
            mov [rsp+32],rbx                    -- lpOverlapped (NULL)
            lea r9,[rsi+FEND64]                 -- lpNumberOfBytesRead (fend) [DWORD, but should be fine]
            mov r8,BUFFERSIZE64                 -- nNumberOfBytesToRead
            lea rdx,[rsi+BUFF64]                -- lpBuffer
            mov rcx,[rsi+HNDL64]                -- hFile
            call "kernel32.dll","ReadFile"
--          add rsp,8*5
--          pop rsp
            mov rsp,[rsp+8*5]   -- equivalent to the add/pop
            test rax,rax
            jz :retm1
        [ELF64]
--%rax  System call             %rdi                    %rsi                            %rdx                    %rcx                    %r8                     %r9
--0     sys_read                unsigned int fd         char *buf                       size_t count
push rsi
            mov rax,0               -- sys_read
            mov rdi,[rsi+HNDL64]    -- fd
            lea rsi,[rsi+BUFF64]
            mov rdx,BUFFERSIZE64
            syscall
pop rsi
            test rax,rax
            jle :retm1
            mov [rsi+FEND64],rax
        [64]
            mov rax,[rsi+RPOS64]
            mov rcx,[rsi+FEND64]
            cmp rcx,0
            je :retm1
            add rax,rcx -- frealposn += fend
            mov rcx,rbx -- rcx:=0
            mov [rsi+RPOS64],rax
            mov rax,1
--          mov [rsi+POSN64],rax
      ::notfull
        mov cl,byte[rsi+rax+BUFF64-1]
        add rax,1                                           -- fposn+=1
        cmp cl,'\r'
        jne @f
        test qword[rsi+MODE64],F_BINARY
        jz :looptop
      @@:
        mov [rsi+POSN64],rax
      ::opGetcStoreRcx
        pop rdi
        pop rdx
        mov r15,h4
        cmp rdx,r15
        mov [rdi],rcx
        jle @f
            sub qword[rbx+rdx*4-16],1
            jz :%pDealloc
      @@:
        ret

  ::retm1
---------
        mov rcx,-1
        jmp :opGetcStoreRcx
    []

--/*
        ch = 255    -- bugfix (without this the compiler assumes ch is -1..-1 (from below), and 
                    --          hence getc always returns -1; naturally this makes it -1..255.)
                ch = -1
--*/
--  ::e59wfmfao
---------------
--      -- e59wfmfao: "wrong file mode for attempted operation"
--      mov al,59
--  [32]
--      xor edi,edi                         -- ep1 unused
--      xor esi,esi                         -- ep2 unused
--  [64]
--      xor rdi,rdi                         -- ep1 unused
--      xor rsi,rsi                         -- ep2 unused
--  []
----DEV
--      call :%pRTErn                       -- fatal error

--/*
global procedure :%n_initC(:%)
end procedure -- (for Edita/CtrlQ)
--*/
  :%n_initC
-----------
    [PE32]
        push eax    -- save
        call "kernel32.dll","AllocConsole"
        push STD_OUTPUT_HANDLE              -- nStdHandle
        call "kernel32.dll","GetStdHandle"
        mov [stdout],eax
        push STD_ERROR_HANDLE               -- nStdHandle
        call "kernel32.dll","GetStdHandle"
        mov [stderr],eax
        push STD_INPUT_HANDLE               -- nStdHandle
        call "kernel32.dll","GetStdHandle"
        mov [stdin],eax
        push ENABLE_PROCESSED_INPUT         -- fdwMode
        push eax                            -- hConsole
        call "kernel32.dll","SetConsoleMode"
        test eax,eax
        jnz @f
            mov [stdin_redirected],1
      @@:
--3/4/17: (messed up trace - DEV)
--      push 65001                          -- wCodePageID (CP_UTF8)
--      call "kernel32.dll","SetConsoleOutputCP"
        pop eax     -- restore
        ret
    [ELF32,ELF64]
        mov [stdout],1
        mov [stderr],2
        mov [stdin],0
        ret
    [PE64]
        push rax    -- save [DEV use [rsp+32]?]
        mov rcx,rsp -- put 2 copies of rsp onto the stack...
        push rsp
        push rcx
        or rsp,8    -- [rsp] is now 1st or 2nd copy:
                    -- if on entry rsp was xxx8: both copies remain on the stack
                    -- if on entry rsp was xxx0: or rsp,8 effectively pops one of them (+8)
                    -- obviously rsp is now xxx8, whichever alignment we started with
        sub rsp,8*5
        call "kernel32.dll","AllocConsole"
        mov rcx,STD_OUTPUT_HANDLE           -- nStdHandle
        call "kernel32.dll","GetStdHandle"
        mov [stdout],rax
        mov rcx,STD_ERROR_HANDLE            -- nStdHandle
        call "kernel32.dll","GetStdHandle"
        mov [stderr],rax
        mov rcx,STD_INPUT_HANDLE            -- nStdHandle
        call "kernel32.dll","GetStdHandle"
        mov [stdin],rax
        mov rdx,ENABLE_PROCESSED_INPUT      -- fdwMode
        mov rcx,rax                         -- hConsole
        call "kernel32.dll","SetConsoleMode"
        test rax,rax
        jnz @f
            mov [stdin_redirected],1
      @@:
--3/4/17: (messed up trace - DEV)
--      mov rcx,65001                       -- wCodePageID (CP_UTF8)
--      call "kernel32.dll","SetConsoleOutputCP"
--      add rsp,8*5
--      pop rsp
        mov rsp,[rsp+8*5]   -- equivalent to the add/pop
        pop rax     -- restore
        ret
--  [ELF64]
--      pop al
    []

--/*
global procedure ::getc0(::)
end procedure -- (for Edita/CtrlQ)
--*/
  ::getc0
---------
    [32]
        cmp [stdout],0
        jne @f
            call :%n_initC -- (preserves eax)
      @@:
        add eax,3 -- (undo sub3 above and test)
--      jnz :e59wfmfao
        jz @f
            mov edx,[esp+8]
            mov al,59   -- e59wfmfao: "wrong file mode for attempted operation"
--          add esp,12
            sub edx,1
            jmp :!iDiag         -- fatal error (see pdiagN.e)
            int3
      @@:

--DEV clear_debug
--      #ilASM{ call :%opClrDbg }
    [PE32]
        push ebx            -- reserve space for buffer (1 byte really)
        mov esi,esp
        push ebx            -- reserve space for NumberOfBytesRead
        mov edi,esp
        push ebx                                        -- lpOverlapped
        push edi                                        -- lpNumberOfBytesRead
        push dword 1                                    -- nNumberOfBytesToRead (1)
        push esi                                        -- lbBuffer
        push [stdin]                                    -- hFile
        call "kernel32.dll","ReadFile"
        pop edx         -- NumberOfBytesRead
        pop ecx         -- buffer (1 byte)
        test eax,eax
        jz :retm1
        test edx,edx
        jz :retm1
    [ELF32]
--#     Name                        Registers                                                                                                               Definition
--                                  eax     ebx                     ecx                     edx                     esi                     edi
--3     sys_read                    0x03    unsigned int fd         char *buf               size_t count            -                       -               fs/read_write.c:391
        push ebx            -- reserve space for buffer (1 byte really)
        mov eax,3   -- sys_read
        mov ebx,[stdin]
        mov ecx,esp
        mov edx,1
        int 0x80
        xor ebx,ebx     -- (common requirement after int 0x80)
        pop ecx         -- buffer (1 byte)
        test eax,eax
        jle :retm1
    [32]
--      mov eax,ecx
--/*
--DEV still outstanding...::
--DEV as per gets....
--DEV 0 & 9 loop, 26 -> -1
--          if ch!='\r' then exit end if                -- skip cr...
--          -- (file 0 is always text mode)
--      end while
--*/
        jmp :opGetcStoreEcx
    [64]
        cmp [stdout],0
        jne @f
            call :%n_initC -- (preserves rax)
      @@:
        add rax,3 -- (undo sub3 above and test)
--      jnz :e59wfmfao
        jz @f
            mov rdx,[rsp+16]
            mov al,59   -- e59wfmfao: "wrong file mode for attempted operation"
--          add rsp,24
            sub rdx,1
            jmp :!iDiag         -- fatal error (see pdiagN.e)
            int3
      @@:
--DEV clear_debug
--      #ilASM{ call :%opClrDbg }
    [PE64]
        mov rcx,rsp -- put 2 copies of rsp onto the stack...
        push rsp
        push rcx
        or rsp,8    -- [rsp] is now 1st or 2nd copy:
                    -- if on entry rsp was xxx8: both copies remain on the stack
                    -- if on entry rsp was xxx0: or rsp,8 effectively pops one of them (+8)
                    -- obviously rsp is now xxx8, whichever alignment we started with
        sub rsp,8*7
        mov [rsp+48],rbx    -- NumberOfBytesRead
        mov [rsp+40],rbx    -- buffer (1 byte really)
        mov [rsp+32],rbx                                -- lpOverlapped (NULL)
        lea r9,[rsp+48]                                 -- lpNumberOfBytesRead
        mov r8,1                                        -- nNumberOfBytesToRead (1)
        lea rdx,[rsp+40]                                -- lbBuffer
        mov rcx,[stdin]                                 -- hFile
        call "kernel32.dll","ReadFile"
--      pop rdx         -- NumberOfBytesRead
--      pop rcx         -- buffer (1 byte)
--      add rsp,8*5
        mov rdx,[rsp+48]    -- NumberOfBytesRead
        mov rcx,[rsp+40]    -- buffer (1 byte)
--      add rsp,8*7
--      pop rsp
        mov rsp,[rsp+8*7]   -- equivalent to the add/pop
        test rax,rax
        jz :retm1
        test rdx,rdx
        jz :retm1
    [ELF64]
--%rax  System call             %rdi                    %rsi                            %rdx                    %rcx                    %r8                     %r9
--0     sys_read                unsigned int fd         char *buf                       size_t count
        push rbx
        mov rax,0           -- sys_read
        mov rdi,[stdin]     -- fd
        mov rsi,rsp         -- buffer (1 byte)
        mov rdx,1
        syscall
        pop rcx
        test rax,rax
        jle :retm1
    [64]
--      mov eax,ecx
--/*
--DEV still outstanding...::
--DEV as per gets....
--DEV 0 & 9 loop, 26 -> -1
--          if ch!='\r' then exit end if                -- skip cr...
--          -- (file 0 is always text mode)
--      end while
--*/
        jmp :opGetcStoreRcx
    []

--/*
global function gets(integer fn, :%opGets)
end function
--*/
  :%opGets
----------
    [32]
--DEV proper calling convention...
        -- fn in eax
        -- dest in edi
        push dword[edi]
        push edi
        lea ecx,[ebx+eax*4]                     -- ecx:=fn*4 [fn->0-based byte idx+12]
        mov edi,[fdtbl]
--      cmp eax,3 -- NO!
        sub eax,3 -- [fn->0-based idx, for comparison against fdmax]
        jb :gets0
        cmp eax,[fdmax]
        jae :getce58bfn
      @@:
        mov esi,[edi*4+ecx-12]                  -- esi:=fdtbl[fn-2]
        cmp edi,[fdtbl]
        je @f
            mov edi,[fdtbl]
            jmp @b
      @@:
        test dword[ebx+esi*4+MODE],F_READ
--      jz :e59wfmfao
        jnz @f
            mov edx,[esp+8]
            mov al,59   -- e59wfmfao: "wrong file mode for attempted operation"
--          add esp,12
            sub edx,1
            jmp :!iDiag         -- fatal error (see pdiagN.e)
            int3
      @@:
        shl esi,2
        push -1                                 -- partial result @ [esp+4]
        push esi                                -- this @ [esp]
      ::looptop2
            mov edi,[esi+POSN]
          ::looptop7
            mov edx,[esi+FEND]
            cmp edi,edx
            jle :notfull2
                mov edi,[esi+MODE]
                test edi,F_DIRTY
                jz @f
                    sub edi,F_DIRTY
                    call :%n_flush_esiedi
                    mov esi,[esp]
              @@:
                lea edx,[esi+FEND]
                lea edi,[esi+BUFF]
            [PE32]
                push ebx                        -- lpOverlapped (NULL)
                push edx                        -- lpNumberOfBytesRead (==addr fend)
                push BUFFERSIZE32               -- nNumberOfBytesToRead
                push edi                        -- lpBuffer
                push dword[esi+HNDL]            -- hFile
                call "kernel32.dll","ReadFile"
                test eax,eax
                jz :exitwhile
                mov edx,[esi+FEND]
                cmp edx,0
--              cmp dword[esi+FEND],0   -- no!!
                je :exitwhile
            [ELF32]
--#     Name                        Registers                                                                                                               Definition
--                                  eax     ebx                     ecx                     edx                     esi                     edi
--3     sys_read                    0x03    unsigned int fd         char *buf               size_t count            -                       -               fs/read_write.c:391
                mov eax,3   -- sys_read
                mov ebx,[esi+HNDL]
                mov ecx,edi
                mov edx,BUFFERSIZE32
                int 0x80
                xor ebx,ebx             -- (common requirement after int 0x80)
                test eax,eax
                jle :exitwhile
                mov dword[esi+FEND],eax
                mov edx,eax
            [32]
--DEV proper way to do 64-bit math:
--/*
; x86 assembly, Intel syntax
; adds ebx:edx to ecx:eax
add eax, edx
adc ecx, ebx
--*/
                mov eax,[esi+POSL]
                mov ecx,[esi+POSH]
                add eax,edx -- frealposn += fend
                adc ecx,ebx
                mov edi,1
                mov [esi+POSL],eax
                mov [esi+POSH],ecx
                mov [esi+POSN],edi
--/*
                fild qword[esi+POSL]
                fild dword[esi+FEND]
                mov edi,1
                faddp   -- frealposn += fend
                mov [esi+POSN],edi
                fistp qword[esi+POSL]
--*/
          ::notfull2
            --
            -- reminder:
            --  esi is this (=[esp], becomes [esp+4] rsn)
            --  edx is FEND
            --  edi is POSN
            --  partial result in [esp+4], becomes [esp+8] rsn (may still be -1)
            --
            lea esi,[esi+edi+BUFF-1]
            mov ecx,ebx -- ecx:=0 (bytes to copy)
            push esi
            sub edx,edi -- <bytes left - 1>
          @@:
            lodsb   -- al:=[esi++]
            cmp al,26
            jb :checkterm
          ::nextch
            add ecx,1
            cmp ecx,edx
            jle @b
            -- (at this point (ie fallthrough/end of buffer) ecx!=0 and al!='\n')
          ::copybytes
            --
            -- reminder:
            --  if al=='\n' then exit (after copy) else unused/goto looptop2
            --  ecx is bytes to copy (shd not be 0 here)
            --  [esp] is buffer[fposn] from when we started scanning
            --  [esp+4] is this (ie fdtbl[fn-2]*4)
            --  [esp+8] is partial result or -1
            --  edi is POSN
            --
            mov edx,[esp+4] -- iThis
            add edi,ecx
            mov [edx+POSN],edi
            push eax    -- save ch
            -- not ecx,ebx,
            mov edi,[esp+12]    -- partial result
            cmp edi,-1
            je :nopartialyet
                push ecx
                mov edx,[ebx+edi*4-12]  -- length(partial)
                mov esi,[ebx+edi*4-16]  -- maxlen(partial)
                sub esi,16              -- less headers
                sub esi,edx             -- less already used
                cmp esi,ecx             -- does it fit?
                jle @f
                    add ecx,edx
                    mov [ebx+edi*4-12],ecx -- set new length
                    lea edi,[edx+edi*4]
                    pop ecx
                    jmp :copyinsitu
              @@:
                lea esi,[ebx+edi*4]     -- partial[1], for rep movsd
                add ecx,edx
                call :%pAllocStr    -- damages eax only [?]
                lea edi,[ebx+eax*4]
                mov ecx,edx
                rep movsb
                mov edx,[esp+16]    -- partial (for deallocate)
                push edi
                push eax
                push dword[esp+36]
                call :%pDealloc0
                pop eax
                pop edi
                pop ecx
                jmp :copyremainder
          ::nopartialyet
--              mov edx,ecx
                call :%pAllocStr    -- damages eax only
                lea edi,[ebx+eax*4]
          ::copyremainder
            mov [esp+12],eax    -- save new partial
          ::copyinsitu
            pop eax
            pop esi
            rep movsb
            mov byte[edi],0
            cmp al,'\n'
            je :exitwhile
            mov esi,[esp]
            jmp :looptop2

          ::checkterm
            jne :getsnotcz
            -- (al is 26 (Ctrl Z) here)
            mov al,'\n'
            add edi,1
            test ecx,ecx
            jnz :copybytes
            mov edx,[esp+4] -- iThis
            mov [edx+POSN],edi
            jmp :exitwhile

          ::getsnotcz
            cmp al,'\r'
            jne :notcr5
                -- (not ecx, edx, ebx, esi, edi: -> must use eax)
                mov eax,[esp+4] -- iThis
                test byte[eax+MODE],F_BINARY
                jnz :nextch
                test ecx,ecx
                jnz @f
                    mov esi,eax
                    add esp,4 -- discard that saved buffer[posn]
                    add edi,1
                    jmp :looptop7
              @@:
                mov al,'\r'
                add edi,1
                jmp :copybytes
          ::notcr5
            cmp al,'\n'
            jne :nextch
            add ecx,1
            jmp :copybytes
      ::exitwhile
        add esp,4
        pop eax
      ::opGetsStoreEax
        pop edi
        pop edx
        cmp edx,h4
        mov [edi],eax
        jle @f
            sub dword[ebx+edx*4-8],1
            je :%pDealloc
      @@:
        ret
    [64]
--DEV calling convention:
        -- fn in rax
        -- dest in rdi
        push qword[rdi]
        push rdi
        lea rcx,[rbx+rax*8]                     -- rcx:=fn*8 [fn->0-based byte idx+24]
        mov rdi,[fdtbl]
--      cmp rax,3 -- NO!
        sub rax,3 -- [fn->0-based idx, for comparison against fdmax]
        jb :gets0
        cmp rax,[fdmax]
        jae :getce58bfn
      @@:
        mov rsi,[rdi*4+rcx-24]                  -- rsi:=fdtbl[fn-2]
        cmp rdi,[fdtbl]
        je @f
            mov rdi,[fdtbl]
            jmp @b
      @@:
        test qword[rbx+rsi*4+MODE64],F_READ
--      jz :e59wfmfao
        jnz @f
            mov rdx,[rsp+16]
            mov al,59   -- e59wfmfao: "wrong file mode for attempted operation"
--          add rsp,24
            sub rdx,1
            jmp :!iDiag         -- fatal error (see pdiagN.e)
            int3
      @@:
        shl rsi,2
        push -1                                 -- partial result @ [rsp+8]
        push rsi                                -- this @ [rsp]
      ::looptop2
            mov rdi,[rsi+POSN64]
          ::looptop7
            mov rdx,[rsi+FEND64]
            cmp rdi,rdx
            jle :notfull2
                mov rdi,[rsi+MODE64]
                test rdi,F_DIRTY
                jz @f
                    sub rdi,F_DIRTY
                    call :%n_flush_rsirdi       -- (preserves rsi)
              @@:
            [PE64]
                mov rcx,rsp -- put 2 copies of rsp onto the stack...
                push rsp
                push rcx
                or rsp,8    -- [rsp] is now 1st or 2nd copy:
                            -- if on entry rsp was xxx8: both copies remain on the stack
                            -- if on entry rsp was xxx0: or rsp,8 effectively pops one of them (+8)
                            -- obviously rsp is now xxx8, whichever alignment we started with

                sub rsp,8*5                     -- minimum 4 param shadow space, lpOverlapped, and align(none here)
                mov [rsp+32],rbx                -- lpOverlapped (NULL)
                lea r9,[rsi+FEND64]             -- lpNumberOfBytesRead (fend) [DWORD, but should be fine]
                mov r8,BUFFERSIZE64             -- nNumberOfBytesToRead
                lea rdx,[rsi+BUFF64]            -- lpBuffer
                mov rcx,[rsi+HNDL64]            -- hFile
                call "kernel32.dll","ReadFile"
--              add rsp,8*5
--              pop rsp
                mov rsp,[rsp+8*5]   -- equivalent to the add/pop
                test rax,rax
                jz :exitwhile
                mov rdx,[rsi+FEND64]
                cmp rdx,0
                je :exitwhile
            [ELF64]
--%rax  System call             %rdi                    %rsi                            %rdx                    %rcx                    %r8                     %r9
--0     sys_read                unsigned int fd         char *buf                       size_t count
                mov rax,0               -- sys_read
                mov rdi,[rsi+HNDL64]    -- fd
                lea rsi,[rsi+BUFF64]
                mov rdx,BUFFERSIZE64
                syscall
--15/4/17
                mov rsi,[rsp]
                test rax,rax
                jle :exitwhile
                mov [rsi+FEND64],rax
                mov rdx,rax
            [64]
                mov rax,[rsi+RPOS64]
                add rax,rdx -- frealposn += fend
                mov rdi,1
                mov [rsi+RPOS64],rax
                mov [rsi+POSN64],rdi
          ::notfull2
            --
            -- reminder:
            --  rsi is this (=[rsp], becomes [rsp+8] rsn)
            --  rdx is FEND
            --  rdi is POSN
            --  partial result in [rsp+8], becomes [rsp+16] rsn (may still be -1)
            --
            lea rsi,[rsi+rdi+BUFF64-1]
            mov rcx,rbx -- rcx:=0 (bytes to copy)
            push rsi
            sub rdx,rdi -- <bytes left - 1>
          @@:
            lodsb   -- al:=[rsi++]
            cmp al,26
            jb :checkterm
          ::nextch
            add rcx,1
            cmp rcx,rdx
            jle @b
            -- (at this point (ie fallthrough/end of buffer) rcx!=0 and al!='\n')
          ::copybytes
            --
            -- reminder:
            --  if al=='\n' then exit (after copy) else unused/goto looptop2
            --  rcx is bytes to copy (shd not be 0 here)
            --  [rsp] is buffer[fposn] from when we started scanning
            --  [rsp+8] is iThis (ie fdtbl[fn-2]) shl 2
            --  [rsp+16] is partial result or -1
            --  rdi is POSN
            --
            mov rdx,[rsp+8] -- iThis shl 2
            add rdi,rcx
            mov [rdx+POSN64],rdi
            push rax    -- save ch
            -- not rcx,ebx,
            mov rdi,[rsp+24]    -- partial result
            cmp rdi,-1
            je :nopartialyet
                push rcx
                mov rdx,[rbx+rdi*4-24]  -- length(partial)
                mov rsi,[rbx+rdi*4-32]  -- maxlen(partial)
                sub rsi,32              -- less headers
                sub rsi,rdx             -- less already used
                cmp rsi,rcx             -- does it fit?
                jle @f
                    add rcx,rdx
                    mov [rbx+rdi*4-24],rcx -- set new length
                    lea rdi,[rdx+rdi*4]
                    pop rcx
                    jmp :copyinsitu
              @@:
                lea rsi,[rbx+rdi*4]     -- partial[1], for rep movsd
                add rcx,rdx
                call :%pAllocStr    -- damages eax only
                lea rdi,[rbx+rax*4]
                mov rcx,rdx
                rep movsb
                mov rdx,[rsp+32]    -- partial (for deallocate)
                push rdi
                push rax
                push qword[rsp+9*8]
                call :%pDealloc0
                pop rax
                pop rdi
                pop rcx
                jmp :copyremainder
          ::nopartialyet
--              mov rdx,rcx
                call :%pAllocStr    -- damages eax only [?]
                lea rdi,[rbx+rax*4]
          ::copyremainder
            mov [rsp+24],rax    -- save new partial
          ::copyinsitu
            pop rax
            pop rsi
            rep movsb
            mov byte[rdi],0
            cmp al,'\n'
            je :exitwhile
            mov rsi,[rsp]
            jmp :looptop2

          ::checkterm
            jne :getsnotcz
            -- (al is 26 (Ctrl Z) here)
            mov al,'\n'
            add rdi,1
            test rcx,rcx
            jnz :copybytes
            mov rdx,[rsp+8] -- iThis
            mov [rdx+POSN64],rdi
            jmp :exitwhile

          ::getsnotcz
            cmp al,'\r'
            jne :notcr5
                -- (not ecx, edx, ebx, esi, edi: -> must use eax)
                mov rax,[rsp+8] -- iThis
                test qword[rax+MODE64],F_BINARY
                jnz :nextch
                test rcx,rcx
                jnz @f
                    mov rsi,rax
                    add rsp,8 -- discard that saved buffer[posn]
                    add rdi,1
                    jmp :looptop7
              @@:
                mov al,'\r'
                add rdi,1
                jmp :copybytes
          ::notcr5
            cmp al,'\n'
            jne :nextch
            add rcx,1
            jmp :copybytes
      ::exitwhile
        add rsp,8
        pop rax
      ::opGetsStoreRax
        pop rdi
        pop rdx
        mov r15,h4
        cmp rdx,r15
        mov [rdi],rax
        jle @f
            sub qword[rbx+rdx*4-16],1
            jz :%pDealloc
      @@:
        ret
    []

--/*
        ch = 255    -- bugfix (without this the compiler assumes ch is -1..-1 (from below), and 
                    --          hence getc always returns -1; naturally this makes it -1..255.)
                ch = -1
--*/
--/*
global procedure ::gets0(::)
end procedure -- (for Edita/CtrlQ)
--*/
  ::gets0           -- fn (eax)<3
---------
    [32]
        -- [esp]    edi
        -- [esp+4]  prev [edi]
        -- [esp+8]  <return address>
        add eax,3 -- (undo sub3 above and test) [fn]
--      jnz :e59wfmfao      -- only gets(0) valid
        jz @f
            mov edx,[esp+8]
            mov al,59   -- e59wfmfao: "wrong file mode for attempted operation"
--          add esp,12
            sub edx,1
            jmp :!iDiag         -- fatal error (see pdiagN.e)
            int3
      @@:
        cmp [stdout],0
        jne @f
            call :%n_initC -- (preserves eax, not that we need it anymore)
      @@:
--      #ilASM{ call :%opClrDbg }
--      r = ""
        xor ecx,ecx -- length
        call :%pAllocStr        -- damages eax only
        push eax
--/*
        mov eax,[esp]
        mov ecx,[ebx+eax*4-12]      -- length
        mov edi,[ebx+eax*4-16]      -- maxlen
--      mov [esp+4],ecx
        sub edi,16
--      mov [esp+8],edi
--*/
--/*
--      to add ch (in cl [maybe with ch=0]):
        mov eax,[esp]
        mov edx,[ebx+eax*4-12]      -- length
        mov edi,[ebx+eax*4-16]      -- maxlen
        add edx, 1
        sub edi,16
        cmp edx,edi                 -- cmp length,maxlen
        jl @f
--          push whatever we need to save
            push ecx                -- save
            push edx                -- save
            call :%pAllocStr        -- damages eax only
            lea esi,[ebx+?*4]
            lea edi,[ebx+eax*4]
            mov ecx,edx
            rep movsb
--          swap refs
            pop edx
            push eax
            call %pDealloc
--          pop whatever we need to restore
            pop edx                 -- restore
            pop ecx                 -- restore
            mov eax,[esp]
      @@:
        mov word[edx+eax*4],cx  -- (or leave planting the final \0 to the end)
--*/
      ::looptop3
--      while 1 do
        [PE32]
            push ebx        -- reserve space for buffer (1 byte really)
            mov edi,esp
            push ebx        -- reserve space for NumberOfBytesRead
            mov edx,esp
            push ebx                        -- lpOverlapped (NULL)
            push edx                        -- lpNumberOfBytesRead
            push 1                          -- nNumberOfBytesToRead (1)
            push edi                        -- lbBuffer
            push [stdin]                    -- hFile
            call "kernel32.dll","ReadFile"
            pop edx         -- NumberOfBytesRead
            pop ecx         -- buffer
            test eax,eax    -- (0 = fail)
--          setz dl
            jz :set26
            test edx,edx
            jnz @f
          ::set26
                mov cl,26 -- (Ctrl Z)
          @@:
        [ELF32]
--#     Name                        Registers                                                                                                               Definition
--                                  eax     ebx                     ecx                     edx                     esi                     edi
--3     sys_read                    0x03    unsigned int fd         char *buf               size_t count            -                       -               fs/read_write.c:391
            push ebx            -- reserve space for buffer (1 byte really)
            mov eax,3   -- sys_read
            mov ebx,[stdin]
            mov ecx,esp
            mov edx,1
            int 0x80
            xor ebx,ebx     -- (common requirement after int 0x80)
            pop ecx         -- buffer (1 byte)
            test eax,eax
            jg @f
                mov cl,26 -- (Ctrl Z)
          @@:
            cmp cl,10
            je :addlf
        [32]
--          mov [ch],ecx
--DEV what if ch='\n'? (test with various redirected files)
--          if ch='\r' then         -- CR
            cmp cl,'\r'
            jne :notcr
                cmp [stdin_redirected],0
                je :addlf
--              push ecx        -- save
              [PE32]
                push ebx        --[1] reserve space for buffer (1 byte really)
                mov edi,esp
                push ebx                        -- lpOverlapped (NULL)
                push esp                        -- lpNumberOfBytesRead
                push 1                          -- nNumberOfBytesToRead (1)
                push edi                        -- lbBuffer
                push [stdin]                    -- hFile
                call  "kernel32.dll","ReadFile"
                pop eax         --[1] discard
--              pop ecx         -- restore
--            @@:
              [ELF32]
--#     Name                        Registers                                                                                                               Definition
--                                  eax     ebx                     ecx                     edx                     esi                     edi
--3     sys_read                    0x03    unsigned int fd         char *buf               size_t count            -                       -               fs/read_write.c:391
                -- (this may not be required on Lnx)
                push ebx            -- reserve space for buffer (1 byte really)
                mov eax,3   -- sys_read
                mov ebx,[stdin]
                mov ecx,esp
                mov edx,1
                int 0x80
                xor ebx,ebx     -- (common requirement after int 0x80)
                pop ecx         -- buffer (1 byte, discard)
              [32]
                jmp :addlf
          ::notcr
            [32]
--          elsif ch=26 then        -- Ctrl Z
            cmp cl,26   -- CtrlZ
            jne :notcz
--              if length(r)=0 then
--                  return -1
--              else
--                  return r
--              end if
                pop eax                             -- newly allocated string
                mov edx,[ebx+eax*4-12]              -- length("")
                test edx,edx
                jnz :opGetsStoreEax
                mov edx,eax
                push dword[esp+8]
                call :%pDealloc0                    -- discard
                mov eax,-1                          -- return -1
                jmp :opGetsStoreEax
          ::notcz
--          elsif ch=8 then         -- backspace
            cmp cl,8    -- backspace
            jne :notbs
--              if length(r) then
                mov eax,[esp]                       -- newly allocated string
                mov edx,[ebx+eax*4-12]              -- length("")
                sub edx,1
                jl :looptop3                        -- already empty
--              r = r[1..-2]
                mov [ebx+eax*4-12],edx              -- length -= 1
                mov [edx+eax*4],bl                  -- plant new terminator
--                  if not stdin_redirected then
                cmp [stdin_redirected],0
                jne :looptop3
                push 0x21082008 -- back,space,back (buffer) [with a '!' (#21) that should not be seen/used]
            [PE32]
                mov esi,esp
                push ebx                            -- lpvReserved (NULL)
                push esp                            -- lpcchWritten
                push dword 3                        -- cchToWrite (3)
                push esi                            -- lpvBuffer
                push [stdout]                       -- hConsoleOutput
                call "kernel32.dll","WriteConsoleA"
            [ELF32]
--#     Name                        Registers                                                                                                               Definition
--                                  eax     ebx                     ecx                     edx                     esi                     edi
--4     sys_write                   0x04    unsigned int fd         const char *buf         size_t count            -                       -               fs/read_write.c:408
                mov eax,4           -- sys_write
                mov ebx,[stdout]    -- fd
                mov ecx,esp         -- buffer
                mov edx,3           -- count
                int 0x80
                xor ebx,ebx         -- (common requirement after int 0x80)
            [32]
                pop eax         -- (discard buffer [0x21082008])
--                  end if
--              end if
                jmp :looptop3
          ::notbs
--          else
--              r &= ch
                mov eax,[esp]                       -- newly allocated string
                mov edx,[ebx+eax*4-12]              -- length
                mov edi,[ebx+eax*4-16]              -- maxlen
                add edx, 1
                sub edi,16
                cmp edx,edi                         -- cmp length,maxlen
                jl @f
                    lea esi,[ebx+eax*4]
--                  push whatever we need to save
                    push ecx                        -- save (char)
                    push edx                        -- save (new length)
--                  push eax                        -- save for deallocation
                    mov ecx,edx
                    call :%pAllocStr                -- damages eax only
                    lea edi,[ebx+eax*4]
                    rep movsb
--                  swap refs
--                  pop edx
--                  push eax
                    mov edx,[esp+8]                 -- prev allocated string
                    mov [esp+8],eax                 -- update with replacement
                    push dword[esp+5*4]             -- era
                    call :%pDealloc0
                    pop edx                         -- restore (new length)
                    pop ecx                         -- restore (char)
                    mov eax,[esp]
              @@:
                mov word[edx+eax*4-1],cx    -- (or leave planting the final \0 to the end)
                mov [ebx+eax*4-12],edx      -- length
--              if not stdin_redirected then
                cmp [stdin_redirected],0
                jnz :looptop3
--              mov eax,[ch]
--              push eax
                push ecx
            [PE32]
                mov eax,esp
                push ebx                        -- lpvReserved
                push esp                        -- lpcchWritten
                push 1                          -- cchToWrite (1)
                push eax                        -- lpvBuffer
                push [stdout]                   -- hConsoleOutput
                call "kernel32.dll","WriteConsoleA"
            [ELF32]
--#     Name                        Registers                                                                                                               Definition
--                                  eax     ebx                     ecx                     edx                     esi                     edi
--4     sys_write                   0x04    unsigned int fd         const char *buf         size_t count            -                       -               fs/read_write.c:408
                mov eax,4           -- sys_write
                mov ebx,[stdout]    -- fd
                mov ecx,esp         -- buffer
                mov edx,1           -- count
                int 0x80
                xor ebx,ebx         -- (common requirement after int 0x80)
            [32]
                pop eax -- (discard buffer)
--              end if
--          end if
            jmp :looptop3
--      end while
      ::addlf
--      r &= '\n'
        mov eax,[esp]                           -- newly allocated string
        mov edx,[ebx+eax*4-12]                  -- length
        mov edi,[ebx+eax*4-16]                  -- maxlen
        add edx,1
        sub edi,16
        cmp edx,edi                             -- cmp length,maxlen
        jl @f
--          push whatever we need to save
            lea esi,[ebx+eax*4]
            push ecx                            -- save (char)
            push edx                            -- save (new length)
            mov ecx,edx
            call :%pAllocStr                    -- damages eax only
            lea edi,[ebx+eax*4]
            rep movsb
--          swap refs
--          pop edx
--          push eax
            mov edx,[esp+8]                     -- prev allocated string
            mov [esp+8],eax                     -- update with replacement
            push dword[esp+5*4]                 -- era
            call :%pDealloc0
            pop edx                             -- restore (new length)
            pop ecx                             -- restore
            mov eax,[esp]                       -- newly allocated string
      @@:
        mov [ebx+eax*4-12],edx                  -- update length
        mov word[edx+eax*4-1],0x000A            -- "\n\0"
--      return r
        pop eax
        jmp :opGetsStoreEax
    [64]
--DEV calling convention
        -- [rsp]    rdi
        -- [rsp+8]  prev [rdi]
        -- [rsp+12] <return address>
        add rax,3 -- (undo sub3 above and test)
--      jnz :e59wfmfao      -- only gets(0) valid
        jz @f
            mov rdx,[rsp+16]
            mov al,59   -- e59wfmfao: "wrong file mode for attempted operation"
--          add rsp,24
            sub rdx,1
            jmp :!iDiag         -- fatal error (see pdiagN.e)
            int3
      @@:
        cmp [stdout],0
        jne @f
            call :%n_initC -- (preserves rax, not that we need it)
      @@:
--      #ilASM{ call :%opClrDbg }
--      r = ""
        xor rcx,rcx -- length
        call :%pAllocStr        -- damages eax only
        push rax
      ::looptop3
--      while 1 do
        [PE64]
            mov rcx,rsp -- put 2 copies of rsp onto the stack...
            push rsp
            push rcx
            or rsp,8    -- [rsp] is now 1st or 2nd copy:
                        -- if on entry rsp was xxx8: both copies remain on the stack
                        -- if on entry rsp was xxx0: or rsp,8 effectively pops one of them (+8)
                        -- obviously rsp is now xxx8, whichever alignment we started with
            sub rsp,8*7
            mov [rsp+48],rbx                -- NumberOfBytesRead
            mov [rsp+40],rbx                -- buffer (1 byte really)
            mov [rsp+32],rbx                -- lpOverlapped (NULL)
            lea r9,[rsp+48]                 -- lpNumberOfBytesRead
            mov r8,1                        -- nNumberOfBytesToRead (1)
            lea rdx,[rsp+40]                -- lbBuffer
            mov rcx,[stdin]                 -- hFile
            call "kernel32.dll","ReadFile"
--          pop rdx             -- NumberOfBytesRead
--          pop rcx             -- buffer (1 byte)
--          add rsp,8*5
            mov rdx,[rsp+48]    -- NumberOfBytesRead
            mov rcx,[rsp+40]    -- buffer (1 byte)
--          add rsp,8*7
--          pop rsp
            mov rsp,[rsp+8*7]   -- equivalent to the add/pop
            test rax,rax
--          setz dl
            jz :set26
            test rdx,rdx
            jnz @f
          ::set26
                mov cl,26 -- (Ctrl Z)
          @@:
        [ELF64]
--%rax  System call             %rdi                    %rsi                            %rdx                    %rcx                    %r8                     %r9
--0     sys_read                unsigned int fd         char *buf                       size_t count
            push rbx                -- buffer (1 byte really)
            mov rax,0               -- sys_read
            mov rdi,[stdin]         -- fd
            mov rsi,rsp             -- buffer
            mov rdx,1               -- count
            syscall
            pop rcx
            test rax,rax
            jg @f
                mov cl,26 -- (Ctrl Z)
          @@:
            cmp cl,10
            je :addlf
        [64]
--          mov [ch],ecx
--DEV what if ch='\n'? (test with various redirected files)
--          if ch='\r' then         -- CR
            cmp cl,'\r'
            jne :notcr
                cmp [stdin_redirected],0
                je :addlf
                --DEV might be better to mov [skipstdinlf],1...
            [PE64]
                mov rcx,rsp -- put 2 copies of rsp onto the stack...
                push rsp
                push rcx
                or rsp,8    -- [rsp] is now 1st or 2nd copy:
                            -- if on entry rsp was xxx8: both copies remain on the stack
                            -- if on entry rsp was xxx0: or rsp,8 effectively pops one of them (+8)
                            -- obviously rsp is now xxx8, whichever alignment we started with
                sub rsp,8*7
                mov [rsp+48],rbx    -- NumberOfBytesRead
                mov [rsp+40],rbx    -- buffer (1 byte really)
                mov [rsp+32],rbx                                -- lpOverlapped (NULL)
                lea r9,[rsp+48]                                 -- lpNumberOfBytesRead
                mov r8,1                                        -- nNumberOfBytesToRead (1)
                lea rdx,[rsp+40]                                -- lbBuffer
                mov rcx,[stdin]                                 -- hFile
                call "kernel32.dll","ReadFile"
--              add rsp,8*7
--              pop rsp
                mov rsp,[rsp+8*7]   -- equivalent to the add/pop
            [ELF64]
--%rax  System call             %rdi                    %rsi                            %rdx                    %rcx                    %r8                     %r9
--0     sys_read                unsigned int fd         char *buf                       size_t count
                -- (might not be necessary, **might be horribly wrong!**)
                push rbx                -- buffer
                mov rax,0               -- sys_read
                mov rdi,[stdin]         -- fd
                mov rsi,rsp             -- buffer
                mov rdx,1               -- count
                syscall
                pop rax                 -- discard
            [64]
                jmp :addlf
          ::notcr
--          elsif ch=26 then        -- Ctrl Z
            cmp cl,26   -- CtrlZ
            jne :notcz
--              if length(r)=0 then
--                  return -1
--              else
--                  return r
--              end if
                pop rax
                mov rdx,[rbx+rax*4-24]
                test rdx,rdx
                jnz :opGetsStoreRax
                mov rdx,rax
                push qword[rsp+24]      -- era
                call :%pDealloc0
                mov rax,-1
                jmp :opGetsStoreRax
          ::notcz
--          elsif ch=8 then         -- backspace
            cmp cl,8    -- backspace
            jne :notbs
--              if length(r) then
                mov rax,[rsp]
                mov rdx,[rbx+rax*4-24]
                sub rdx,1
                jl :looptop3
--              r = r[1..-2]
                mov [rbx+rax*4-24],rdx
                mov [rdx+rax*4],bl
--                  if not stdin_redirected then
                cmp [stdin_redirected],0
                jne :looptop3
            [PE64]
                mov rcx,rsp -- put 2 copies of rsp onto the stack...
                push rsp
                push rcx
                or rsp,8    -- [rsp] is now 1st or 2nd copy:
                            -- if on entry rsp was xxx8: both copies remain on the stack
                            -- if on entry rsp was xxx0: or rsp,8 effectively pops one of them (+8)
                            -- obviously rsp is now xxx8, whichever alignment we started with
--              push 0x21082008 -- back,space,back (buffer) [with a '!' (#21) that should not be seen/used]
--              sub rsp,8*6
                sub rsp,8*7
                mov dword[rsp+48],0x21082008            -- back,space,back (buffer) [with a '!' (#21) that should not be seen/used]
                mov [rsp+32],rbx                        -- lpvReserved (NULL)
                lea r9,[rsp+40]                         -- lpcchWritten
                mov r8,3                                -- cchToWrite (3)
                lea rdx,[rsp+48]                        -- lpvBuffer
                mov rcx,[stdout]                        -- hConsoleOutput
                call "kernel32.dll","WriteConsoleA"
--              add rsp,8*7
--              pop rsp
                mov rsp,[rsp+8*7]   -- equivalent to the add/pop
            [ELF64]
--%rax  System call             %rdi                    %rsi                            %rdx                    %rcx                    %r8                     %r9
--1     sys_write               unsigned int fd         const char *buf                 size_t count
                push 0x21082008 -- back,space,back (buffer) [with a '!' (#21) that should not be seen/used]
                mov rax,1               -- sys_write
                mov rdi,[stdout]        -- fd
                mov rcx,rsp             -- buffer
                mov rdx,3               -- count
                syscall
                pop rax                 -- discard buffer
            [64]
--                  end if
--              end if
                jmp :looptop3
          ::notbs
--          else
--              r &= ch
                mov rax,[rsp]
                mov rdx,[rbx+rax*4-24]      -- length
                mov rdi,[rbx+rax*4-32]      -- maxlen
                add rdx,1
                sub rdi,32
                cmp rdx,rdi                 -- cmp length,maxlen
                jl @f
                    lea rsi,[rbx+rax*4]
--                  push whatever we need to save
                    push rcx                -- save
                    push rdx                -- save
--                  push rax                -- save for deallocation
                    mov rcx,rdx
                    call :%pAllocStr        -- damages eax only
                    lea rdi,[rbx+rax*4]
                    rep movsb
--                  swap refs
--                  pop edx
--                  push eax
                    mov rdx,[rsp+16]
                    mov [rsp+16],rax
                    push qword[rsp+5*8]     -- era
                    call :%pDealloc0
                    pop rdx                 -- restore
                    pop rcx                 -- restore
                    mov rax,[rsp]
              @@:
                mov word[rdx+rax*4-1],cx    -- (or leave planting the final \0 to the end)
                mov [rbx+rax*4-24],rdx      -- length
--              if not stdin_redirected then
                cmp [stdin_redirected],0
                jnz :looptop3
            [PE64]
                mov rdx,rsp -- put 2 copies of rsp onto the stack...
                push rsp
                push rdx
                or rsp,8    -- [rsp] is now 1st or 2nd copy:
                            -- if on entry rsp was xxx8: both copies remain on the stack
                            -- if on entry rsp was xxx0: or rsp,8 effectively pops one of them (+8)
                            -- obviously rsp is now xxx8, whichever alignment we started with
--              push rcx
--              sub rsp,8*6
                sub rsp,8*7
                mov [rsp+48],rcx
                mov [rsp+32],rbx                        -- lpvReserved (NULL)
                lea r9,[rsp+40]                         -- lpcchWritten
                mov r8,1                                -- cchToWrite (1)
                lea rdx,[rsp+48]                        -- lpvBuffer (the stored rcx)
                mov rcx,[stdout]                        -- hConsoleOutput
                call "kernel32.dll","WriteConsoleA"
--              add rsp,8*7
--              pop rsp
                mov rsp,[rsp+8*7]   -- equivalent to the add/pop
            [ELF64]
--%rax  System call             %rdi                    %rsi                            %rdx                    %rcx                    %r8                     %r9
--1     sys_write               unsigned int fd         const char *buf                 size_t count
                push rcx
                mov rax,1               -- sys_write
                mov rdi,[stdout]        -- fd
                mov rcx,rsp             -- buffer
                mov rdx,1               -- count
                syscall
                pop rcx                 -- discard
            [64]
--              end if
--          end if
            jmp :looptop3
--      end while
      ::addlf
--      r &= '\n'
        mov rax,[rsp]               -- (the AllocStr result)
        mov rdx,[rbx+rax*4-24]      -- length
        mov rdi,[rbx+rax*4-32]      -- maxlen
        add rdx,1
        sub rdi,32
        cmp rdx,rdi                 -- cmp length,maxlen
        jl @f
--          push whatever we need to save
            lea rsi,[rbx+rax*4]
            push rcx                -- save
            push rdx                -- save
            mov rcx,rdx
            call :%pAllocStr        -- damages eax only
            lea rdi,[rbx+rax*4]
            rep movsb
--          swap refs
--          pop rdx
--          push rax
            mov rdx,[rsp+16]
            mov [rsp+16],rax
            push qword[rsp+5*8]     -- era
            call :%pDealloc0
--          pop whatever we need to restore
            pop rdx                 -- restore
            pop rcx                 -- restore
            mov rax,[rsp]
      @@:
        mov [rbx+rax*4-24],rdx      -- length
        mov word[rdx+rax*4-1],0x000A    -- "\n\0"
--      return r
        pop rax
        jmp :opGetsStoreRax
    []
--/*
--DEV still outstanding...::
--DEV as per gets....
--DEV 0 & 9 loop, 26 -> -1
--          if ch!='\r' then exit end if                -- skip cr...
--          -- (file 0 is always text mode)
--      end while
        ret
--*/

--/*
global procedure puts(integer fn, object x, :%opPuts)
end procedure
--*/
  :%opPuts
----------
    [32]
        -- fn in eax
        -- object to print in edx
        lea ecx,[ebx+eax*4]                     -- ecx:=fn*4 [fn->0-based byte idx+12]
        mov edi,[fdtbl]
--      cmp eax,3 -- NO!
        sub eax,3 -- [fn->0-based idx, for comparison against fdmax]
        jb :puts1
        cmp eax,[fdmax]
        jae :getce58bfnNP
      @@:
        mov esi,[edi*4+ecx-12]                  -- esi:=fdtbl[fn-2]
        cmp edi,[fdtbl]
        je @f
            mov edi,[fdtbl]
            jmp @b
      @@:
        mov edi,[ebx+esi*4+MODE]
        shl esi,2
        push edx
        test edi,F_WRITE
--      jz :e59wfmfao   -- (may need a pop?)
        jnz @f
            mov edx,[esp+4]
            mov al,59   -- e59wfmfao: "wrong file mode for attempted operation"
            sub edx,1
            jmp :!iDiag         -- fatal error (see pdiagN.e)
            int3
      @@:
        test edi,F_DIRTY
        jnz @f
            or edi,F_DIRTY
            mov ecx,[esi+POSN]
            mov eax,[esi+FEND]
            mov [esi+MODE],edi
            cmp ecx,eax
            jg :clearbuff
                fild qword[esi+POSL]
                fild dword[esi+FEND]
                fsubp st1,st0
                fistp qword[esi+POSL] -- (realpos -= fend) [also sets [esi+POSH]
                push edx -- save
            [PE32]
                lea ecx,[esi+POSH]
                push FILE_BEGIN                     -- dwMoveMethod
                push ecx                            -- lpDistanceToMoveHigh
                push dword[esi+POSL]                -- lDistanceToMove (0)
                push dword[esi+HNDL]                -- hFile
                call "kernel32.dll","SetFilePointer"
            [ELF32]
--#     Name                        Registers                                                                                                               Definition
--                                  eax     ebx                     ecx                     edx                     esi                     edi
--140   sys_llseek                  0x8c    unsigned int fd         unsigned long offset_high   unsigned long offset_low    loff_t *result  unsigned int origin     fs/read_write.c:191
-- (I assume loff_t is a "long long" aka int64)
--SEEK_SET              =       0
--SEEK_CUR              =       1
--SEEK_END              =       2
                mov eax,140         -- sys_llseek
--2/2/18:
push edi
                push esi            -- save
                mov ebx,[esi+HNDL]  -- fd
                mov ecx,[esi+POSH]  -- offset_high
                mov edx,[esi+POSL]  -- offset_low
                lea esi,[esi+POSL]  -- *result (also targets [esi+POSH])
                mov edi,0           -- SEEK_SET
                int 0x80
                xor ebx,ebx         -- (common requirement after int 0x80)
                pop esi             -- restore
pop edi
            [32]
                pop edx -- restore
                jmp @f
          ::clearbuff
                mov [esi+POSN],dword 1
                mov [esi+FEND],ebx
      @@:
        cmp edx,h4
        jl @f
            -- nb: pilx86.e is expected to opUnassigned edx (and eax)
            cmp byte[ebx+edx*4-1],#12
            jne :putstr                 -- (may jump to putsq, after getting the length)
            fld qword[ebx+edx*4]
            fistp dword[esp]
            mov edx,[esp]
      @@:
        mov ecx,1
        mov eax,[esi+POSN]
        cmp dl,'\n'
        jne @f
            test edi,F_BINARY
            jnz @f
--DEV ELF32?
                mov dword[esp],#00000A0D  --(\r\n)
--DEV this should really jump to string handling...?
-- (however seek in non-binary mode is deeply suspect anyway)
-- (moot point if n_flush_esi2 takes care of things)
                mov ecx,2
      @@:
        --
        -- reminder:
        --  esi is this (fdtbl[fn-2]*4)
        --  edi is mode (not used again)
        --  ecx is length (1 or 2)
        --  buffer is [esp] ("")
        --  eax is posn
        --
--DEV lea edi here seems a better idea!
        lea edx,[esi+eax+BUFF-1]
        add eax,ecx
--      cmp eax,BUFFERSIZE32
        cmp eax,BUFFERSIZE32+1
        jle @f
            push edx
            push ecx
            push esi
            call :%n_flush_esi2
            pop esi
            pop ecx
            pop edx
--          mov [esi+POSN],dword 1
            mov eax,ecx
            mov [esi+FEND],ebx
            add eax,1
--Ditto (edi)
            lea edx,[esi+BUFF]
      @@:
--      sub eax,ecx
--      lea edi,[esi+eax+BUFF-1]
        mov edi,edx
        mov edx,esi
        mov esi,esp
        rep movsb       -- (just one byte, or 2 in the \n -> \r\n case)
        mov edi,[edx+FEND]
        mov [edx+POSN],eax
        sub eax,1
        add esp,4
        cmp eax,edi
        jle @f
--          add eax,1
            mov [edx+FEND],eax
      @@:
        ret
    [64]
        -- fn in rax
        -- object to print in rdx
        lea rcx,[rbx+rax*8]                     -- rcx:=fn*8 [fn->0-based byte idx+24]
        mov rdi,[fdtbl]
--      cmp eax,3 -- NO!
        sub rax,3 -- [fn->0-based idx, for comparison against fdmax]
        jb :puts1
        cmp rax,[fdmax]
        jae :getce58bfnNP
      @@:
        mov rsi,[rdi*4+rcx-24]                  -- rsi:=fdtbl[fn-2]
        cmp rdi,[fdtbl]
        je @f
            mov rdi,[fdtbl]
            jmp @b
      @@:
        mov rdi,[rbx+rsi*4+MODE64]
        mov r15,h4
        shl rsi,2
        push rdx
        test rdi,F_WRITE
--      jz :e59wfmfao   -- (may need a pop?)
        jnz @f
            mov rdx,[rsp+8]
            mov al,59   -- e59wfmfao: "wrong file mode for attempted operation"
            sub rdx,1
            jmp :!iDiag         -- fatal error (see pdiagN.e)
            int3
      @@:
        test rdi,F_DIRTY
        jnz @f
            or rdi,F_DIRTY
            mov rcx,[rsi+POSN64]
            mov rax,[rsi+FEND64]
            mov [rsi+MODE64],rdi
            cmp rcx,rax
            jg :clearbuff
                sub [rsi+RPOS64],rax    -- (realpos -= fend)
            [PE64]
                mov rcx,rsp -- put 2 copies of rsp onto the stack...
                push rsp
                push rcx
                or rsp,8    -- [rsp] is now 1st or 2nd copy:
                            -- if on entry rsp was xxx8: both copies remain on the stack
                            -- if on entry rsp was xxx0: or rsp,8 effectively pops one of them (+8)
                            -- obviously rsp is now xxx8, whichever alignment we started with
                sub rsp,8*5 -- minimum 4 param shadow space, save rdx, and align(0)
                mov [rsp+32],rdx    -- save
                mov r9,FILE_BEGIN                       -- dwMoveMethod
                mov r8,rbx                              -- lpNewFilePointer (NULL)
                mov rdx,[rsi+RPOS64]                    -- liDistanceToMove
                mov rcx,[rsi+HNDL64]                    -- hFile
                call "kernel32.dll","SetFilePointerEx"
--              cmp rax,0
--              jne ??
                mov rdx,[rsp+32] -- restore
--              add rsp,8*5
--              pop rsp
                mov rsp,[rsp+8*5]   -- equivalent to the add/pop
            [ELF64]
--%rax  System call             %rdi                    %rsi                            %rdx                    %rcx                    %r8                     %r9
--8     sys_lseek               unsigned int fd         off_t offset                    unsigned int origin
                mov r12,rsi
--2/2/18:
mov r13,rdi
                mov rax,8                           -- sys_lseek
                mov rsi,[rsi+RPOS64]                -- offset
                mov rdx,0                           -- SEEK_SET
                mov rdi,[r12+HNDL64]                -- hFile
                syscall
--13/2/17 no (and no @@:!!)
--              cmp rax, -1
--              jne no@f
--                  -- return 1
--                  mov rax,1
--                  jmp :%opRetf
--            no@@:
--18/2/17 (spotted in passing)
--              mov [rbx+r12*4+POSN64],qword 1
--              mov [rbx+r12*4+FEND64],rbx --(0)
--              mov [rbx+r12*4+RPOS64],rax
                mov rsi,r12
mov rdi,r13
            [64]
                jmp @f
          ::clearbuff
                mov [rsi+POSN64],qword 1
                mov [rsi+FEND64],rbx
      @@:
        cmp rdx,r15
        jl @f
            -- nb: pilx86.e is expected to opUnassigned rdx (and rax)
            cmp byte[rbx+rdx*4-1],#12
            jne :putstr                 -- (may jump to putsq, after getting the length)
            fld tbyte[rbx+rdx*4]
            fistp qword[rsp]
            mov rdx,[rsp]
      @@:
        mov rcx,1
        mov rax,[rsi+POSN64]
        cmp dl,'\n'
        jne @f
            test rdi,F_BINARY
            jnz @f
--DEV ELF64?
                mov qword[rsp],#00000A0D  --(\r\n)
                mov rcx,2
      @@:
        --
        -- reminder:
        --  rsi is this (fdtbl[fn-2]*4)
        --  rdi is mode (not used again)
        --  rcx is length (1 or 2)
        --  buffer is [rsp] ("")
        --  rax is posn
        --
        lea rdi,[rsi+rax+BUFF64-1]
        add rax,rcx
        cmp rax,BUFFERSIZE64+1
        jle @f
--          push rdx
            push rcx
            push rsi
            call :%n_flush_rsi2
            pop rsi
            pop rcx
--          pop rdx
            mov rax,rcx
            mov [rsi+FEND64],rbx
            add rax,1
--          lea rdi,[rsi+BUFF64]    -- already done by :%n_flush_rsi2
      @@:
        mov rdx,rsi
        mov rsi,rsp
        rep movsb       -- (just one byte, or 2 in the \n -> \r\n case)
        mov rdi,[rdx+FEND64]
        mov [rdx+POSN64],rax
        sub rax,1
        add rsp,8
        cmp rax,rdi
        jle @f
            mov [rdx+FEND64],rax
      @@:
        ret
    []

--/*
global procedure ::putstr(::)
end procedure -- (for Edita/CtrlQ)
--*/
  ::putstr
----------
    [32]
        add esp,4
        mov ecx,[ebx+edx*4-12]      -- length
        mov eax,[esi+POSN]
        test ecx,ecx
        jz @b
        cmp byte[ebx+edx*4-1],#82
        jne :putsq
        --
        -- reminder:
        --  esi is this (fdtbl[fn-2]*4)
        --  edi is mode
        --  ecx is length (non-zero, in case that matters)
        --  edx is string to print (ref)
        --  eax is fposn
        --
        test edi,F_BINARY
        jz :putstrtxt
        add eax,ecx
        cmp eax,BUFFERSIZE32+1
        jle @f
            cmp dword[esi+FEND],0
            je :putstrnoflush
                push edx
                push ecx
                -- (remove dirty flag in case we writefile direct) [DEV we didn't test it was dirty!?]
                sub edi,F_DIRTY
                call :%n_flush_esiedi
                pop ecx
                mov eax,1
                pop edx
                mov [esi+POSN],eax
                mov [esi+FEND],ebx
                add eax,ecx
          ::putstrnoflush
            cmp ecx,BUFFERSIZE32
            jle :putstrinbuffer
                mov eax,[esi+POSL]
                mov edi,[esi+POSH]
                add eax,ecx -- frealposn += len
                adc edi,ebx
                mov [esi+POSL],eax
                mov [esi+POSH],edi
                mov eax,[esi+HNDL]
                shl edx,2
            [PE32]
                push ebx                        -- lpOverlapped (NULL)
                push esp                        -- lpNumberOfBytesWritten
                push ecx                        -- nNumberOfBytesToWrite
                push edx                        -- lpBuffer
                push eax                        -- hFile,
                call "kernel32.dll","WriteFile"
            [ELF32]
--#     Name                        Registers                                                                                                               Definition
--                                  eax     ebx                     ecx                     edx                     esi                     edi
--4     sys_write                   0x04    unsigned int fd         const char *buf         size_t count            -                       -               fs/read_write.c:408
                mov ebx,eax         -- fd
                mov eax,4           -- sys_write
                xchg ecx,edx        -- buffer<->count
                int 0x80
                xor ebx,ebx         -- (common requirement after int 0x80)
            [32]
                ret
          ::putstrinbuffer
            --
            -- reminder:
            --  esi is this
            --  ecx is len
            --  edx is string to print (ref)
            --  eax is posn+len
            --
            or dword[esi+MODE],F_DIRTY
      @@:
        mov edi,[esi+POSN]
        push esi
        mov [esi+POSN],eax
        sub eax,1
        lea edi,[esi+edi+BUFF-1]
        lea esi,[ebx+edx*4]
        pop edx
        rep movsb
        cmp eax,[edx+FEND]
        jle @f
            mov [edx+FEND],eax
      @@:
        ret
    [64]
        add rsp,8
        mov rcx,[rbx+rdx*4-24]      -- length
        mov rax,[rsi+POSN64]
        test rcx,rcx
        jz @b
        cmp byte[rbx+rdx*4-1],#82
        jne :putsq
        --
        -- reminder:
        --  rsi is this (fdtbl[fn-2]*4)
        --  rdi is mode
        --  rcx is length (non-zero, in case that matters)
        --  rdx is string to print (ref)
        --  rax is fposn
        --
        test rdi,F_BINARY
        jz :putstrtxt
        add rax,rcx
        cmp rax,BUFFERSIZE64+1
        jle @f
            cmp qword[rsi+FEND64],0
            je :putstrnoflush
                push rdx
                push rcx
                -- (remove dirty flag in case we writefile direct) [DEV we didn't test it was dirty!?]
                sub rdi,F_DIRTY
                call :%n_flush_rsirdi
                pop rcx
                mov rax,1
                pop rdx
                mov [rsi+POSN64],rax
                mov [rsi+FEND64],rbx
                add rax,rcx
          ::putstrnoflush
            cmp rcx,BUFFERSIZE64
            jle :putstrinbuffer
                mov rax,[rsi+RPOS64]
                add rax,rcx -- frealposn += len
                mov [rsi+RPOS64],rax
                mov rax,[rsi+HNDL64]
                shl rdx,2
            [PE64]
                mov rax,rsp -- put 2 copies of rsp onto the stack...
                push rsp
                push rax
                or rsp,8    -- [rsp] is now 1st or 2nd copy:
                            -- if on entry rsp was xxx8: both copies remain on the stack
                            -- if on entry rsp was xxx0: or rsp,8 effectively pops one of them (+8)
                            -- obviously rsp is now xxx8, whichever alignment we started with
                sub rsp,8*5     -- minimum 4 param shadow space, lpOverlapped, and align (none in this case)
                mov [rsp+32],rbx                            -- lpOverlapped (NULL)
--              mov r9,rbx                                  -- lpNumberOfBytesWritten (NULL)    -- NO!
                lea r9,[rsp+32]                             -- lpNumberOfBytesWritten
                mov r8,rcx                                  -- nNumberOfBytesToWrite (fend)
--30/12/15: no! rdx already set.
--              lea rdx,[rsi+BUFF64]                        -- lpBuffer
                mov rcx,[rsi+HNDL64]                        -- hFile
                call "kernel32.dll","WriteFile"
--/* (DEV: we should probably do this, but the 32-bit version don't, and we'd need a new error code)
                test rax,rax
                jnz @f
                    call "kernel32.dll","GetLastError"
                    mov rdi,rax                             -- ep1
                    mov al,98                               -- e98fiofe -- flush error [ep1]
                    xor rsi,rsi                             -- ep2 unused
--                  jmp :%pRTErn                            -- fatal error
              @@:
--*/
--              add rsp,8*5
--              pop rsp
                mov rsp,[rsp+8*5]   -- equivalent to the add/pop
            [ELF64]
--%rax  System call             %rdi                    %rsi                            %rdx                    %rcx                    %r8                     %r9
--1     sys_write               unsigned int fd         const char *buf                 size_t count
                mov rax,1               -- sys_write
                mov rdi,[rsi+HNDL64]    -- fd
                mov rsi,rdx             -- buffer
                mov rdx,rcx             -- count
                syscall
--          or rax,rax 
--          jns @f
--              mov rdi,rax                             -- ep1
--              mov rdx,[rbp+24]                        -- "called from" address
--              mov rbp,[rbp+40]                        -- prev_ebp
--              mov al,98                               -- e98fiofe -- flush error [ep1]
--              sub rdx,1
--              jmp :!iDiag
--              int3
--        @@:
            [64]
                ret
          ::putstrinbuffer
            --
            -- reminder:
            --  rsi is this
            --  rcx is len
            --  rdx is string to print (ref)
            --  rax is posn+len
            --
            or qword[rsi+MODE64],F_DIRTY
      @@:
        mov rdi,[rsi+POSN64]
        push rsi
        mov [rsi+POSN64],rax
        sub rax,1
        lea rdi,[rsi+rdi+BUFF64-1]
        lea rsi,[rbx+rdx*4]
        pop rdx
        rep movsb
        cmp rax,[rdx+FEND64]
        jle @f
            mov [rdx+FEND64],rax
      @@:
        ret
    []

--/*
global procedure ::putstrtxt(::)
end procedure -- (for Edita/CtrlQ)
--*/
  ::putstrtxt
-------------
        -- write string, inserting carriage-return characters before linefeeds
    [32]
        push esi
        lea edi,[esi+eax+BUFF-1]
        lea esi,[ebx+edx*4]
--      mov edx,[esi+POSN]
        mov edx,eax -- posn

      ::putstrtxtlooptop
        cmp edx,BUFFERSIZE32
        jle @f
--DEV common code with below
            push esi
            mov esi,[esp+4]
            push ecx
            mov dword[esi+FEND],BUFFERSIZE32
            call :%n_flush_esi2
            pop ecx
            mov [esi+FEND],ebx
            pop esi
            mov edx,1
      @@:
        lodsb
        cmp al,'\n'
        jne @f
            mov byte[edi],'\r'
            add edx,1
            add edi,1
            cmp edx,BUFFERSIZE32
            jle @f
--DEV common code with above (up to mov al,'\n')
                push esi
                mov esi,[esp+4]
                push ecx
                mov dword[esi+FEND],BUFFERSIZE32
                call :%n_flush_esi2
                pop ecx
                mov [esi+FEND],ebx
                pop esi
                mov edx,1
                mov al,'\n'
      @@:
        stosb
        add edx,1
        sub ecx,1
        jg :putstrtxtlooptop
        pop esi
        mov [esi+POSN],edx
        sub edx,1
        cmp [esi+FEND],edx
        jge @f
            mov [esi+FEND],edx
      @@:
        ret
    [64]
        push rsi
        lea rdi,[rsi+rax+BUFF64-1]
        lea rsi,[rbx+rdx*4]
--      mov rdx,[rsi+POSN64]
        mov rdx,rax -- posn

      ::putstrtxtlooptop
        cmp rdx,BUFFERSIZE64
        jle @f
--DEV common code with below
            push rsi
            mov rsi,[rsp+8]
            push rcx
            mov qword[rsi+FEND64],BUFFERSIZE64
            call :%n_flush_rsi2
            pop rcx
            mov [rsi+FEND64],rbx
            pop rsi
            mov rdx,1
      @@:
        lodsb
        cmp al,'\n'
        jne @f
            mov byte[rdi],'\r'
            add rdx,1
            add rdi,1
            cmp rdx,BUFFERSIZE64
            jle @f
--DEV common code with above (up to mov al,'\n')
                push rsi
                mov rsi,[rsp+8]
                push rcx
                mov qword[rsi+FEND64],BUFFERSIZE64
                call :%n_flush_rsi2
                pop rcx
                mov [rsi+FEND64],rbx
                pop rsi
                mov rdx,1
                mov al,'\n'
      @@:
        stosb
        add rdx,1
        sub rcx,1
        jg :putstrtxtlooptop
        pop rsi
        mov [rsi+POSN64],rdx
        sub rdx,1
        cmp [rsi+FEND64],rdx
        jge @f
            mov [rsi+FEND64],rdx
      @@:
        ret
    []

--/*
global procedure ::putsq(::)
end procedure -- (for Edita/CtrlQ)
--*/
  ::putsq
---------
        -- write a dword-sequence, which must not contain any subsequences,
        -- as 1-byte chars, and if in binary mode then insert cr before lf.
    [32]
        push esi
--6/5/15:
--      lea edi,[esi+BUFF]
        lea edi,[esi+eax+BUFF-1]
        lea esi,[ebx+edx*4]
--      mov edx,[esi+POSN]
        mov edx,eax -- posn

      ::putsqlooptop
        cmp edx,BUFFERSIZE32
        jle @f
            push esi
            mov esi,[esp+4]
            push ecx
            mov dword[esi+FEND],BUFFERSIZE32
            call :%n_flush_esi2
            pop ecx
            mov [esi+FEND],ebx
            pop esi
            mov edx,1
      @@:
        lodsd
        cmp eax,h4
        jl @f
            push ebx
            cmp byte[ebx+eax*4-1],#12
            jne :putsqe65sfics
            fld qword[ebx+eax*4]
            fistp dword[esp]
            pop eax
      @@:
        cmp al,'\n'
        jne @f
            mov eax,[esp]
            test dword[eax+MODE],F_BINARY
--12/5/15:
--          jz :putsqresetal
            jnz :putsqresetal
            mov byte[edi],'\r'
            add edx,1
            add edi,1
            cmp edx,BUFFERSIZE32
--12/5/15:
--          jle @f
            jle :putsqresetal
                push esi
                mov esi,[esp+4]
                push ecx
                mov dword[esi+FEND],BUFFERSIZE32
                call :%n_flush_esi2
                pop ecx
                mov [esi+FEND],ebx
                pop esi
                mov edx,1
          ::putsqresetal
                mov al,'\n'
      @@:
--DEV stosb? (6/5/15)
--      movsb
        stosb
        add edx,1
        sub ecx,1
        jg :putsqlooptop
        pop esi
        mov [esi+POSN],edx
        sub edx,1
        cmp [esi+FEND],edx
        jge @f
            mov [esi+FEND],edx
      @@:
        ret
    [64]
        push rsi
--6/5/15:
--      lea rdi,[rsi+BUFF64]
        lea rdi,[rsi+rax+BUFF64-1]
        lea rsi,[rbx+rdx*4]
--      mov rdx,[rsi+POSN64]
        mov rdx,rax -- posn
        mov r15,h4

      ::putsqlooptop
        cmp rdx,BUFFERSIZE64
        jle @f
            push rsi
            mov rsi,[rsp+8]
            push rcx
            mov qword[rsi+FEND64],BUFFERSIZE64
            call :%n_flush_rsi2
            pop rcx
            mov [rsi+FEND64],rbx
            pop rsi
            mov rdx,1
      @@:
        lodsq
        cmp rax,r15
        jl @f
            push rbx
            cmp byte[rbx+rax*4-1],#12
            jne :putsqe65sfics
            fld tbyte[rbx+rax*4]
            fistp qword[rsp]
            pop rax
      @@:
        cmp al,'\n'
        jne @f
            mov rax,[rsp]
            test qword[rax+MODE64],F_BINARY
-- 12/5/15:
--          jz :putsqresetal
            jnz :putsqresetal
            mov byte[rdi],'\r'
            add rdx,1
            add rdi,1
            cmp rdx,BUFFERSIZE64
-- 12/5/15:
--          jle @f
            jle :putsqresetal
                push rsi
                mov rsi,[rsp+8]
                push rcx
                mov qword[rsi+FEND64],BUFFERSIZE64
                call :%n_flush_rsi2
                pop rcx
                mov [rsi+FEND64],rbx
                pop rsi
                mov rdx,1
          ::putsqresetal
                mov al,'\n'
      @@:
--DEV stosb? (6/5/15)
--      movsb
        stosb
        add rdx,1
        sub rcx,1
        jg :putsqlooptop
        pop rsi
        mov [rsi+POSN64],rdx
        sub rdx,1
        cmp [rsi+FEND64],rdx
        jge @f
            mov [rsi+FEND64],rdx
      @@:
        ret
    []

--/*
global procedure ::puts1(::)
end procedure -- (for Edita/CtrlQ)
--*/
  ::puts1           -- fn (eax)<3
---------
    [32]
        add eax,3 -- (undo sub3 above and test)
--      jle :e59wfmfao      -- only puts(1|2) valid
        jg @f
            pop edx
            mov al,59   -- e59wfmfao: "wrong file mode for attempted operation"
            sub edx,1
            jmp :!iDiag         -- fatal error (see pdiagN.e)
            int3
      @@:
        push edx
        cmp [stdout],0
        jne @f
            call :%n_initC -- (preserves eax)
            mov edx,[esp]
      @@:
        cmp eax,1
        je :putstdout
            mov eax,[stderr]
            jmp @f
      ::putstdout
            mov eax,[stdout]
      @@:
--      #ilASM{ call :%opClrDbg }
        cmp edx,h4
        jl @f
            -- nb: pilx86.e is expected to opUnassigned edx (and eax)
            cmp byte[ebx+edx*4-1],#12
            jne :puts1str
            fld qword[ebx+edx*4]
            fistp dword[esp]
      @@:
    [PE32]
        mov edx,esp
        push ebx                    -- lpOverlapped (NULL)
        push esp                    -- lpNumberOfBytesWritten
        push dword 1                -- nNumberOfBytesToWrite (1)
        push edx                    -- lpBuffer
        push eax                    -- hFile,
        call "kernel32.dll","WriteFile"
    [ELF32]
--#     Name                        Registers                                                                                                               Definition
--                                  eax     ebx                     ecx                     edx                     esi                     edi
--4     sys_write                   0x04    unsigned int fd         const char *buf         size_t count            -                       -               fs/read_write.c:408
        mov ebx,eax         -- fd
        mov eax,4           -- sys_write
        mov ecx,esp         -- buffer
        mov edx,1           -- count
        int 0x80
        xor ebx,ebx         -- (common requirement after int 0x80)
    [32]
        pop edx     -- discard (one byte) buffer
        ret
    [64]
        add rax,3 -- (undo sub3 above and test)
--      jle :e59wfmfao      -- only puts(1|2) valid
        jg @f
            pop rdx
            mov al,59   -- e59wfmfao: "wrong file mode for attempted operation"
            sub rdx,1
            jmp :!iDiag         -- fatal error (see pdiagN.e)
            int3
      @@:
        push rdx
        mov r15,h4
        cmp [stdout],0
        jne @f
            call :%n_initC -- (preserves rax)
            mov rdx,[rsp]
      @@:
        cmp rax,1
        je :putstdout
            mov rax,[stderr]
            jmp @f
      ::putstdout
            mov rax,[stdout]
      @@:
--      #ilASM{ call :%opClrDbg }
        cmp rdx,r15
        jl @f
            -- nb: pilx86.e is expected to opUnassigned rdx (and rax)
            cmp byte[rbx+rdx*4-1],#12
            jne :puts1str
            fld tbyte[rbx+rdx*4]
            fistp qword[rsp]
      @@:
        pop rdx
    [PE64]
        mov rcx,rsp -- put 2 copies of rsp onto the stack...
        push rsp
        push rcx
        or rsp,8    -- [rsp] is now 1st or 2nd copy:
                    -- if on entry rsp was xxx8: both copies remain on the stack
                    -- if on entry rsp was xxx0: or rsp,8 effectively pops one of them (+8)
                    -- obviously rsp is now xxx8, whichever alignment we started with
        sub rsp,8*7     -- minimum 4 param shadow space, lpOverlapped, align, and buffer(/rdx already pushed)
        mov [rsp+48],rdx
        mov [rsp+32],rbx                -- lpOverlapped (NULL)
--      mov r9,rbx                      -- lpNumberOfBytesWritten (NULL)    -- NO!
        lea r9,[rsp+32]                 -- lpNumberOfBytesWritten
        mov r8,1                        -- nNumberOfBytesToWrite
        lea rdx,[rsp+48]                -- lpBuffer
        mov rcx,rax                     -- hFile
        call "kernel32.dll","WriteFile"
--      add rsp,8*7
--      pop rsp
        mov rsp,[rsp+8*7]   -- equivalent to the add/pop
    [ELF64]
--%rax  System call             %rdi                    %rsi                            %rdx                    %rcx                    %r8                     %r9
--1     sys_write               unsigned int fd         const char *buf                 size_t count
        push rdx
        mov rdi,rax             -- fd
        mov rax,1               -- sys_write
        mov rsi,rsp             -- buffer
        mov rdx,1               -- count
        syscall
        pop rdx                 -- discard
    [64]
        ret
    []

--/*
global procedure ::puts1str(::)
end procedure -- (for Edita/CtrlQ)
--*/
  ::puts1str
------------
        -- write a string to the console
    [32]
        add esp,4 -- (discard the edx we pushed above)
        mov ecx,[ebx+edx*4-12]      -- length
        cmp byte[ebx+edx*4-1],#82
        jne :puts1sq
--      jne :%n_opPuts1Sq
        lea edx,[ebx+edx*4]
        --
        -- Bugfix 23/01/2014: was getting ERROR_NOT_ENOUGH_MEMORY when displaying strings 
        --  over 62,600 bytes in length, not something I recommend, but a "?s" should not 
        --  just display nowt because of some internal limit being exceeded; -> 8k blocks.
        --
      ::p1blockloop
        cmp ecx,8192    -- (reasonable for both 32 and 64-bit, btw)
        jle :p1oneblock
        push ecx
        push edx
        push eax
        mov ecx,8192
        call :p1oneblock
        pop eax
        pop edx
        pop ecx
        add edx,8192
        sub ecx,8192
        jmp :p1blockloop
      ::p1oneblock
    [PE32]
        push ebx                    -- lpOverlapped (NULL)
        push esp                    -- lpNumberOfBytesWritten
        push ecx                    -- nNumberOfBytesToWrite
        push edx                    -- lpBuffer
        push eax                    -- hFile,
        call "kernel32.dll","WriteFile"
--      test eax,eax
--      jz :puts1err
    [ELF32]
--#     Name                        Registers                                                                                                               Definition
--                                  eax     ebx                     ecx                     edx                     esi                     edi
--4     sys_write                   0x04    unsigned int fd         const char *buf         size_t count            -                       -               fs/read_write.c:408
        mov ebx,eax         -- fd
        mov eax,4           -- sys_write
        xchg ecx,edx        -- buffer<->count
        int 0x80
        xor ebx,ebx         -- (common requirement after int 0x80)
    [64]
        add rsp,8 -- (discard the rdx we pushed above)
        mov rcx,[rbx+rdx*4-24]      -- length
        cmp byte[rbx+rdx*4-1],#82
        jne :puts1sq
--      jne :%n_opPuts1Sq
        lea rdx,[rbx+rdx*4]
      ::p1blockloop
        cmp rcx,8192    -- (reasonable for both 32 and 64-bit, btw)
        jle :p1oneblock
        push rcx
        push rdx
        push rax
        mov rcx,8192
        call :p1oneblock
        pop rax
        pop rdx
        pop rcx
        add rdx,8192
        sub rcx,8192
        jmp :p1blockloop
      ::p1oneblock
    [PE64]
        mov rsi,rsp -- put 2 copies of rsp onto the stack...
        push rsp
        push rsi    -- (rax/rcx/rdx are all in use)
        or rsp,8    -- [rsp] is now 1st or 2nd copy:
                    -- if on entry rsp was xxx8: both copies remain on the stack
                    -- if on entry rsp was xxx0: or rsp,8 effectively pops one of them (+8)
                    -- obviously rsp is now xxx8, whichever alignment we started with
        sub rsp,8*5     -- minimum 4 param shadow space, lpOverlapped, and align (none in this case)
        mov [rsp+32],rbx                -- lpOverlapped (NULL)
--      mov r9,rbx                      -- lpNumberOfBytesWritten (NULL)    -- NO!
        lea r9,[rsp+32]                 -- lpNumberOfBytesWritten
        mov r8,rcx                      -- nNumberOfBytesToWrite
--      (rdx already set)               -- lpBuffer
        mov rcx,rax                     -- hFile
        call "kernel32.dll","WriteFile"
--      add rsp,8*5
--      pop rsp
        mov rsp,[rsp+8*5]   -- equivalent to the add/pop
--      test rax,rax
--      jz :puts1err
    [ELF64]
--%rax  System call             %rdi                    %rsi                            %rdx                    %rcx                    %r8                     %r9
--1     sys_write               unsigned int fd         const char *buf                 size_t count
        mov rdi,rax     -- fd
        mov rax,1       -- sys_write
--      xchg rcx,rdx    -- buffer<->count
        mov rsi,rdx
        mov rdx,rcx
        syscall
    []
      @@:
        ret
--    ::puts1err
--      call "kernel32.dll","GetLastError"
--      mov edi,eax
----DEV temp:
--      mov al,58
--      xor esi,esi     -- ep2 unused
--      jmp %pRTErn     -- fatal error


--/*
global procedure ::puts1sq(::)
end procedure -- (for Edita/CtrlQ)
--*/
-- old (working but slower) code
  ::puts1sq
-----------
        -- Note: puts(1,{'a','b','c'}) is going to be quite slow.
        --       For redirected output, we should [DEV] allocate
        --       a (byte-)buffer and WriteFile just the once.
    [32]
        lea esi,[ebx+edx*4]
        mov edi,eax
      ::puts1sqlooptop
        sub ecx,1
        jl @b
        lodsd           -- eax:=[esi++]
        push edi        -- save
        push esi        -- save
        push ecx        -- save
        push eax        -- Buffer (of one byte)
        mov edx,esp
        cmp eax,h4
        jl @f
            cmp byte[ebx+eax*4-1],#12
            jne :puts1sqe65sfics
            fld qword[ebx+eax*4]
            fistp dword[esp]
      @@:
    [PE32]
        push ebx                    -- lpOverlapped (NULL)
        push esp                    -- lpNumberOfBytesWritten
        push 1                      -- nNumberOfBytesToWrite (1)
        push edx                    -- lpBuffer
        push edi                    -- hFile,
        call "kernel32.dll","WriteFile"
    [ELF32]
--#     Name                        Registers                                                                                                               Definition
--                                  eax     ebx                     ecx                     edx                     esi                     edi
--4     sys_write                   0x04    unsigned int fd         const char *buf         size_t count            -                       -               fs/read_write.c:408
        mov eax,4       -- sys_write
        mov ebx,edi     -- fd
        mov ecx,edx     -- buffer
        mov edx,1
        int 0x80
        xor ebx,ebx     -- (common requirement after int 0x80)
    [32]
        pop eax         -- discard buffer
        pop ecx         -- restore
        pop esi         -- restore
        pop edi         -- restore
        jmp :puts1sqlooptop

      ::puts1sqe65sfics
        add esp,8       -- (locates era properly)
      ::putsqe65sfics
        add esp,8       -- (locates era properly)
        mov al,65       -- "sequence found in character string"
        pop edx
        xor edi,edi     -- ep1 unused
        xor esi,esi     -- ep2 unused
--      jmp :%pRTErn    -- fatal error
        sub edx,1
        jmp :!iDiag
        int3

    [64]
        lea rsi,[rbx+rdx*4]
        mov rdi,rax     -- hFile
      ::puts1sqlooptop
        sub rcx,1
        jl @b
        lodsq           -- rax:=[rsi++]
        push rdi        -- save
        push rsi        -- save
        push rcx        -- save
        push rax        -- Buffer (of one byte)
        mov rdx,rsp
        cmp rax,r15
        jl @f
            cmp byte[rbx+rax*4-1],#12
            jne :puts1sqe65sfics
            fld tbyte[rbx+rax*4]
            fistp qword[rsp]
--          mov rax,[rsp]   -- maybe, see below
      @@:
    [PE64]
        mov rcx,rsp -- put 2 copies of rsp onto the stack...
        push rsp
        push rcx
        or rsp,8    -- [rsp] is now 1st or 2nd copy:
                    -- if on entry rsp was xxx8: both copies remain on the stack
                    -- if on entry rsp was xxx0: or rsp,8 effectively pops one of them (+8)
                    -- obviously rsp is now xxx8, whichever alignment we started with
--      sub rsp,8*5     -- minimum 4 param shadow space, lpOverlapped, and align (none in this case)
        sub rsp,8*7     -- minimum 4 param shadow space, lpOverlapped, and align (none in this case)
--DEV unnecessary, or last fistp needs that mov rax,[rsp] (spotted in passing)
        mov [rsp+40],rax                -- char/buffer
        mov [rsp+32],rbx                -- lpOverlapped (NULL)
--      mov r9,rbx                      -- lpNumberOfBytesWritten (NULL)    -- NO!
        lea r9,[rsp+32]                 -- lpNumberOfBytesWritten
        mov r8,1                        -- nNumberOfBytesToWrite (1)
--      (rdx already set)               -- lpBuffer
        mov rcx,rdi                     -- hFile
        call "kernel32.dll","WriteFile"
--      add rsp,8*5
--      pop rsp
--      mov rsp,[rsp+8*5]   -- equivalent to the add/pop
        mov rsp,[rsp+8*7]   -- equivalent to the add/pop
    [ELF64]
--%rax  System call             %rdi                    %rsi                            %rdx                    %rcx                    %r8                     %r9
--1     sys_write               unsigned int fd         const char *buf                 size_t count
        mov rax,1               -- sys_write
--      rdi already set         -- fd
--31/10/17 (thanks to tom for finding this)
--      mov rcx,rsp             -- buffer (==rdx btw)
        mov rsi,rsp             -- buffer (==rdx btw)
        mov rdx,1               -- count
        syscall
    [64]
        pop rax         -- discard buffer
        pop rcx         -- restore
        pop rsi         -- restore
        pop rdi         -- restore
        jmp :puts1sqlooptop

      ::puts1sqe65sfics
        add rsp,16      -- (locates era properly)
      ::putsqe65sfics
        add rsp,16      -- (locates era properly)
        mov al,65       -- "sequence found in character string"
        pop rdx
        xor rdi,rdi     -- ep1 unused
        xor rsi,rsi     -- ep2 unused
--      jmp :%pRTErn    -- fatal error
        sub rdx,1
        jmp :!iDiag
        int3
    []

--/*
global function get_key(, :%opGetKey)
-- Return next key that was pressed by the user, without waiting.
-- Return -1 if no key was pressed.
end function
--*/
    :%opGetKey                     -- [edi]=get_key()
--------------
    [32]
        -- calling convention:
        --  lea edi,[p1]    -- result location
        --  call :%opGetKey
        push edi
        cmp [stdout],0
        jne @f
            call :%n_initC -- (preserves eax)
      @@:
--      call clear_debug [DEV]
    [PE32]
        sub esp,20 -- sizeof(INPUT_RECORD[/KEY_EVENT_RECORD])
        mov edi,esp -- (preserved over api calls)
        push ebx    -- DWORD NumberOfEventsRead(:=0)
      @@:
        push esp                                -- lpNumberOfEventsRead
        push 1                                  -- nLength
        push edi                                -- lpBuffer
        push [stdin]                            -- hConsoleInput
        call "kernel32.dll","PeekConsoleInputA"
        mov eax,-1
        cmp dword[esp],0
        je @f
            push esp                                -- lpNumberOfEventsRead
            push 1                                  -- nLength
            push edi                                -- lpBuffer
            push [stdin]                            -- hConsoleInput
            call "kernel32.dll","ReadConsoleInputA"
            cmp word[edi],0x0001                    -- lpBuffer.EventType=KEY_EVENT?
            jne @b
            cmp dword[edi+4],0x00000001             -- lpBuffer.keyDown(ignore key up events)
            jne @b
            xor eax,eax
            mov al,[edi+14]                         -- lpBuffer.keyChar
            or ax,ax
            jne @f
                mov ax,256
                add ax,[edi+12]                     -- lpBuffer.keyScan
                --
                -- Skip control, alt, shift and caps lock keys.
                -- Note that shift, ctrl, alt, and caps lock keys will repeat 
                -- while they are held down, so a routine which wants to detect
                -- them pressed on their own cannot accept eg <Ctrl A>, or even
                -- <Shift 1>, aka '!'. If you _do_ want to detect them on their 
                -- own, just make a copy of this routine minus these lines.
                -- Note values determined by experimentation, not documentation!
                --
                cmp ax,298  -- shift key
                je @b
                cmp ax,285  -- ctrl key
                je @b
                cmp ax,312  -- alt key
                je @b
-- actually, RDS lets this through...
--              cmp ax,314  -- caps lock
--              je @b
      @@:
        add esp,24  -- sizeof(INPUT_RECORD[/KEY_EVENT_RECORD] and DWORD NumberOfEventsRead)
--      jmp @f
    [ELF32]
--#     Name                        Registers                                                                                                               Definition
--                                  eax     ebx                     ecx                     edx                     esi                     edi
--3     sys_read                    0x03    unsigned int fd         char *buf               size_t count            -                       -               fs/read_write.c:391
--? sys_poll?
        -- (this may block...) [DEV testme, ie that get_key() yields -1 and carries on]
--/*
        push ebx            -- reserve space for buffer (1 byte really)
        mov eax,3           -- sys_read
        mov ebx,[stdin]     -- fd
        mov ecx,esp         -- buffer
        mov edx,1           -- count
        int 0x80
        xor ebx,ebx         -- (common requirement after int 0x80)
        test eax,eax
        pop eax
        jg @f
            mov eax,-1
      @@:
--*/
--/!*
        xor eax,eax
      ::xGetOrWait
        push eax
        sub esp,36          -- termios struct
        xor ebx,ebx         -- stdin (0)
        mov ecx,0x5401      -- TCGETS 
        mov edx,esp
        mov eax,54          -- sys_ioctl 
        int 0x80 
        and dword[esp+12],#FFFFFFF5     -- not(ICANON or ECHO)   ;turn off echo 
        mov eax,[esp+36]
--DEV... broken/may be wrong offsets/still echoes (pretty sure 23 is TIME)
        mov byte[esp+22],al             -- VMIN                  ;turn off canonical mode 
        mov byte[esp+23],al             -- VTIME     ;we dont want to wait for keystrokes 
        xor ebx,ebx         -- stdin (0)
        mov ecx,0x5402      -- TCSETS
        mov edx,esp
        mov eax,54          -- sys_ioctl 
        int 0x80 
        xor ebx,ebx         -- (common requirement after int 0x80)

        push ebx            -- reserve space for buffer (1 byte really)
        mov eax,3           -- sys_read
        mov ebx,[stdin]     -- fd
        mov ecx,esp         -- buffer
        mov edx,1           -- count
        int 0x80
        xor ebx,ebx         -- (common requirement after int 0x80)
        test eax,eax
        pop eax
        jg @f
            mov eax,-1
      @@:

        or dword[esp+12],#0000000A  -- (ICANON or ECHO)  ;turn on echo 
        mov byte[esp+22],1          -- VMIN              ;turn on canonical mode 
--(to mirror OE:[?])
--      mov byte[esp+23],1          -- VTIME             ;wait for keystrokes 
        xor ebx,ebx         -- stdin (0)
        mov ecx,0x5402      -- TCSETS
        mov edx,esp
        push eax
        mov eax,54          -- sys_ioctl 
        int 0x80 
        xor ebx,ebx         -- (common requirement after int 0x80)
        pop eax
        add esp,40
--*!/
--/*
see also: http://stackoverflow.com/questions/24419077/reading-input-from-keyboard-with-x64-linux-syscalls-assembly/24422643#24422643
typedef unsigned char   cc_t;
typedef unsigned int    speed_t;
typedef unsigned int    tcflag_t;

#define NCCS 19
struct termios {
        tcflag_t c_iflag;               /* input mode flags */
        tcflag_t c_oflag;               /* output mode flags */
        tcflag_t c_cflag;               /* control mode flags */
        tcflag_t c_lflag;               /* local mode flags */
        cc_t c_line;                    /* line discipline */
        cc_t c_cc[NCCS];                /* control characters */
};

termios:        times 36 db 0
stdin:          equ 0
ICANON:         equ 1<<1
ECHO:           equ 1<<3

canonical_off:
        call read_stdin_termios

        ; clear canonical bit in local mode flags
        push rax
        mov eax, ICANON
        not eax
        and [termios+12], eax   -- c_lflag
        pop rax

        call write_stdin_termios
        ret

echo_off:
        call read_stdin_termios

        ; clear echo bit in local mode flags
        push rax
        mov eax, ECHO
        not eax
        and [termios+12], eax
        pop rax

        call write_stdin_termios
        ret

canonical_on:
        call read_stdin_termios

        ; set canonical bit in local mode flags
        or dword [termios+12], ICANON

        call write_stdin_termios
        ret

echo_on:
        call read_stdin_termios

        ; set echo bit in local mode flags
        or dword [termios+12], ECHO

        call write_stdin_termios
        ret

read_stdin_termios:
        push rax
        push rbx
        push rcx
        push rdx

(#36 = 54)
        mov eax, 36h
        mov eax, 54     -- sys_ioctl
        mov ebx, stdin
        mov ecx, 5401h
        mov edx, termios
        int 80h

        pop rdx
        pop rcx
        pop rbx
        pop rax
        ret

write_stdin_termios:
        push rax
        push rbx
        push rcx
        push rdx

        mov eax, 36h
        mov ebx, stdin
        mov ecx, 5402h
        mov edx, termios
        int 80h

        pop rdx
        pop rcx
        pop rbx
        pop rax
        ret

(#define VMIN 6)
(#define ICANON 0000002)
(#define ECHO   0000010)    -- octal 8!!
/* tcsetattr uses these */
((#define TCSANOW       0))
from be_execute.c:
void echo_wait()
// sets tty to wait for input and echo keystrokes
{
        newtty.c_lflag |= ICANON;
        newtty.c_lflag |= ECHO;   // turns on echo
        newtty.c_cc[VMIN] = 1;    // wait for input
        tcsetattr(STDIN_FILENO, TCSANOW, &newtty);
}

void noecho(int wait)
// sets tty to not echo keystrokes, and either wait for input or not
{
        newtty.c_lflag &= ~ICANON;
        newtty.c_lflag &= ~ECHO;   // turns off echo
        newtty.c_cc[VMIN] = wait;  // wait for input, if wait = 1
        tcsetattr(STDIN_FILENO, TCSANOW, &newtty);
}
#endif

void InitGraphics()
/* initialize for graphics */
{
#ifdef EUNIX
        // save initial tty state
        tcgetattr(STDIN_FILENO, &savetty);

        // set up tty for no echo
        newtty = savetty;
        newtty.c_cc[VTIME] = 0;
        //noecho(0); // necessary?
#endif

        NewConfig(FALSE);

}

and something from fasmboard:


;this proggy mainly shows the next char in 1 second after you typed 
;i hate to see characters "being typed" while im not touching the keyboard 
;so i added a buffer flush code 
;i hope it would help someone 

;equates are stolen from asmutils include files (os_linux.inc) 
;some of them also can be found at www.lxhp.in-berlin.de/lhpioctl.html 
;but they are well hidden so try searching the document for "termios" 

;i came across this info after examining "snake" proggie in asmutils. 
;here the keyword is "non-canonical". after googling for "canonical tty" 
;i found this addr http://ou800doc.caldera.com/en/SDK_sysprog/_TTY_in_Canonical_Mode.html 
;check out the link topright says "terminal device control" you should see 
;a lot of good docs here 

;also check out http://ou800doc.caldera.com/en/man/html.7/termio.7.html 
;for better explanations for termios 

;hold down the key to see what happens. ESC to quit. 

;i apologize for my poor programming skills but i mainly tried to 
;write more understandable code for newbies like me 


format ELF executable 

entry start 

TCGETS    equ 0x5401 
TCSETS    equ 0x5402 
TCFLSH    equ 0x540b 
          TCIOFLUSH equ 2          ;flush both input output 
          TCOFLUSH  equ 1          ;flush output 
          TCIFLUSH  equ 0          ;flush input 
ICANON    equ 2 
ECHO      equ 8 
STDIN     equ 0 
STDOUT    equ 1 
SYS_NANOSLEEP equ 162 
SYS_IOCTL equ 54 
SYS_READ  equ 3 
SYS_WRITE equ 4 
SYS_EXIT  equ 1 

section readable writeable 
start: 

call begingetkey         ;enter non-canonical mode 
                         ;close local echo 
mainloop: 
mov ebx,timer            ;wait a little to fill keyboard buffer 
mov ecx,timer2           ;simulate a time consuming process 
mov eax,SYS_NANOSLEEP 
int 0x80 

mov ebx,STDIN 
mov ecx,key 
mov edx,1 
mov eax,SYS_READ 
int 0x80 
push eax                 ;number of read bytes 

mov ebx,STDIN            ;flush extra chars written to the 
mov ecx,TCFLSH           ;buffer while we hold down the key 
mov edx,TCIOFLUSH        ;in fact i dont know how it really effects 
mov eax,SYS_IOCTL        ;but i prefer to flush both buffers 
int 0x80                 ;it seems ok for this code 

pop eax 
test eax,eax             ;read anything? 
jz mainloop 

cmp byte [key],0x1b      ;ESC key quit 
jz quitloop 
inc byte [key]           ;just to see WE are echoing 
                         ;show next char 
mov ebx,STDOUT 
mov ecx,key 
mov edx,1 
mov eax,SYS_WRITE 
int 0x80 

jmp mainloop 

quitloop: 

call endgetkey           ;restore previous state and quit 

mov eax,ebx 
mov eax,SYS_EXIT 
int 0x80 


begingetkey: 
        mov ebx,STDIN             ;get old state here 
        mov ecx,TCGETS 
        mov edx,old_termios 
        mov eax,SYS_IOCTL 
        int 0x80 

        mov ecx,19                ;and make a copy 
        mov esi,old_termios 
        mov edi,new_termios 
        rep movsb 

        and [new_termios.c_lflag],dword not(ICANON or ECHO)   ;turn off echo 
        mov byte [new_termios.VMIN],0                    ;turn off canonical mode 
        mov byte [new_termios.VTIME],0                   ;we dont want to wait for keystrokes 

        mov ebx,STDIN 
        mov ecx,TCSETS 
        mov edx,new_termios 
        mov eax,SYS_IOCTL 
        int 0x80 
        ret 

endgetkey:                                                 ;clean up 
        mov ebx,STDIN 
        mov ecx,TCSETS 
        mov edx,old_termios 
        mov eax,SYS_IOCTL 
        int 0x80 
        ret 

timer:               ;timespec structure 
        dd 1         ;seconds 
        dd 0         ;nanoseconds 
timer2: 
        dd 1 
        dd 0 

old_termios:         ;termios structure NOT termio 
.c_iflag: 
          rd 1      ;0
.c_oflag: 
          rd 1      ;4
.c_cflag: 
          rd 1      ;8
.c_lflag: 
          rd 1      ;12
;.c_cc 
       rb 4 
       .VMIN: 
               rb 1
       .VTIME: 
               rb 1
       rb 13 
new_termios: 
.c_iflag: 
          rd 1 
.c_oflag: 
          rd 1 
.c_cflag: 
          rd 1 
.c_lflag: 
          rd 1      ;12
;.c_cc 
       .VINTR: 
               rb 1 ;16
       .VQUIT: 
               rb 1 ;17
       .VERASE: 
               rb 1 ;18
       .VKILL: 
               rb 1 ;19
       .VEOF: 
               rb 1 ;20
       .VMIN:          ;in canonical mode VEOL 
               rb 1 ;21
       .VTIME:         ;in canonical mode VEOL2 
               rb 1 ;22
       .VSWTCH: 
               rb 1 
       .VSTRT: 
               rb 1 
       .VSTOP: 
               rb 1 
       .VSUSP: 
               rb 1 
       .VDSUSP: 
               rb 1 
       .VREPRINT: 
               rb 1 
       .VDISCARD: 
               rb 1 
       .VWERASE: 
               rb 1 
       .VLNEXT: 
               rb 1 
               rb 3       ;reserved 
key: 
           rb 1 

--*/
    [32]
        pop edi
        mov edx,[edi]
        mov [edi],eax
        cmp edx,h4
        jle @f                          -- this jump almost always taken
            sub dword[ebx+edx*4-8],1    -- <any object> := wait_key() case [rare, tgt is usually an int]
            jz :%pDealloc
      @@:
        ret
    [64]
        -- calling convention:
        --  lea rdi,[p1]    -- result location
        --  call :%opGetKey
        push rdi
        cmp [stdout],0
        jne @f
            call :%n_initC -- (preserves rax)
      @@:
--      call clear_debug [DEV]
    [PE64]
        mov rcx,rsp -- put 2 copies of rsp onto the stack...
        push rsp
        push rcx
        or rsp,8    -- [rsp] is now 1st or 2nd copy:
                    -- if on entry rsp was xxx8: both copies remain on the stack
                    -- if on entry rsp was xxx0: or rsp,8 effectively pops one of them (+8)
                    -- obviously rsp is now xxx8, whichever alignment we started with
        sub rsp,24 -- sizeof(INPUT_RECORD[/KEY_EVENT_RECORD]) (rounded up to qwords)
        mov rdi,rsp -- (preserved over api calls)
--      push rbx    -- DWORD NumberOfEventsRead(:=0) (rounded up to a whole qword)

        sub rsp,8*6 -- shadow space for 4 params and NumberOfEventsRead, plus align
      @@:
        mov [rsp+32],rbx
        lea r9,[rsp+32]                         -- lpNumberOfEventsRead
        mov r8,1                                -- nLength
        mov rdx,rdi                             -- lpBuffer
        mov rcx,[stdin]                         -- hConsoleInput
        call "kernel32.dll","PeekConsoleInputA"
        mov rax,-1
        cmp dword[rsp+32],0
        je @f
            lea r9,[rsp+32]                         -- lpNumberOfEventsRead
            mov r8,1                                -- nLength
            mov rdx,rdi                             -- lpBuffer
            mov rcx,[stdin]                         -- hConsoleInput
            call "kernel32.dll","ReadConsoleInputA"
            cmp word[rdi],0x0001                    -- lpBuffer.EventType=KEY_EVENT?
            jne @b
            cmp dword[rdi+4],0x00000001             -- lpBuffer.keyDown(ignore key up events)
            jne @b
            xor rax,rax
            mov al,[rdi+14]                         -- lpBuffer.keyChar
            or ax,ax
            jne @f
                mov ax,256
                add ax,[rdi+12]                     -- lpBuffer.keyScan
                --
                -- Skip control, alt, shift and caps lock keys.
                -- Note that shift, ctrl, alt, and caps lock keys will repeat 
                -- while they are held down, so a routine which wants to detect
                -- them pressed on their own cannot accept eg <Ctrl A>, or even
                -- <Shift 1>, aka '!'. If you _do_ want to detect them on their 
                -- own, just make a copy of this routine minus these lines.
                -- Note values determined by experimentation, not documentation!
                --
                cmp ax,298  -- shift key
                je @b
                cmp ax,285  -- ctrl key
                je @b
                cmp ax,312  -- alt key
                je @b
-- actually, RDS lets this through...
--              cmp ax,314  -- caps lock
--              je @b
      @@:
--      add rsp,24+8*6  -- sizeof(INPUT_RECORD[/KEY_EVENT_RECORD] and shadowspace/NumberOfEventsRead)
--      pop rsp
--      mov rsp,[rsp+24+8*6]    -- equivalent to the add/pop
        mov rsp,[rsp+24+48] -- equivalent to the add/pop
    [ELF64]
--%rax  System call             %rdi                    %rsi                            %rdx                    %rcx                    %r8                     %r9
--0     sys_read                unsigned int fd         char *buf                       size_t count
--DEV see [ELF32]
--(16   sys_ioctl               unsigned int fd         unsigned int cmd                unsigned long arg)
--/*
        push rbx                -- buffer
        mov rax,0               -- sys_read
        mov rdi,[stdin]         -- fd
        mov rsi,rsp             -- buffer
        mov rdx,1               -- count
        syscall
        test rax,rax
        pop rax
        jg @f
            mov rax,-1
      @@:
#       Name                        Registers                                                                                                               Definition
                                    eax     ebx                     ecx                     edx                     esi                     edi
54      sys_ioctl                   0x36    unsigned int fd         unsigned int cmd        unsigned long arg       -                       -               fs/ioctl.c:613
==>
%rax    System call             %rdi                    %rsi                            %rdx                    %rcx                    %r8                     %r9
16      sys_ioctl               unsigned int fd         unsigned int cmd                unsigned long arg
--*/
--/!*
        xor rax,rax
      ::xGetOrWait
        push rax
        sub rsp,40          -- termios struct (+4 bytes padding)
        xor rdi,rdi         -- stdin (0)
        mov rsi,0x5401      -- TCGETS 
        mov rdx,rsp
        mov rax,16          -- sys_ioctl 
        syscall
        and dword[rsp+12],#FFFFFFF5     -- not(ICANON or ECHO)   ;turn off echo 
        mov rax,[rsp+40]
--DEV... broken/may be wrong offsets/still echoes (pretty sure 23 is TIME)
        mov byte[rsp+22],al             -- VMIN                  ;turn off canonical mode 
        mov byte[rsp+23],al             -- VTIME     ;we dont want to wait for keystrokes 
        xor rdi,rdi         -- stdin (0)
        mov rsi,0x5402      -- TCSETS
        mov rdx,rsp
        mov rax,16          -- sys_ioctl 
        syscall
        xor rbx,rbx         -- (common requirement after int 0x80)

        push rbx            -- reserve space for buffer (1 byte really)
        mov rax,0           -- sys_read
        mov rdi,[stdin]     -- fd
        mov rsi,rsp         -- buffer
        mov rdx,1           -- count
        syscall
        xor rbx,rbx         -- (common requirement after int 0x80)
        test rax,rax
        pop rax
        jg @f
            mov rax,-1
      @@:

        or dword[rsp+12],#0000000A  -- (ICANON or ECHO)  ;turn on echo 
        mov byte[rsp+22],1          -- VMIN              ;turn on canonical mode 
--(to mirror OE:[?])
--      mov byte[rsp+23],1          -- VTIME             ;wait for keystrokes 
        xor rdi,rdi         -- stdin (0)
        mov rsi,0x5402      -- TCSETS
        mov rdx,rsp
        push rax
        mov rax,16          -- sys_ioctl 
        syscall
        xor rbx,rbx         -- (common requirement after int 0x80)
        pop rax
        add rsp,48
--*!/

    [64]
--      jmp @f
        pop rdi
        mov r15,h4
        mov rdx,[rdi]
        mov [rdi],rax
        cmp rdx,r15
        jle @f                          -- this jump almost always taken
            sub qword[rbx+rdx*4-16],1   -- <any object> := wait_key() case [rare, tgt is usually an int]
            jz :%pDealloc
      @@:
        ret
    []

--/*
BOOL WINAPI PeekConsoleInput(
  _In_   HANDLE ,
  _Out_  PINPUT_RECORD lpBuffer,
  _In_   DWORD nLength,
  _Out_  LPDWORD lpNumberOfEventsRead
);
;typedef struct _INPUT_RECORD { // ir
;   WORD EventType;
;   union {
;       KEY_EVENT_RECORD KeyEvent;
;       MOUSE_EVENT_RECORD MouseEvent;
;       WINDOW_BUFFER_SIZE_RECORD WindowBufferSizeEvent;
;       MENU_EVENT_RECORD MenuEvent;
;       FOCUS_EVENT_RECORD FocusEvent;
;   } Event;
;} INPUT_RECORD;
;typedef struct _KEY_EVENT_RECORD { // ker
;   BOOL bKeyDown;
;   WORD wRepeatCount;
;   WORD wVirtualKeyCode;
;   WORD wVirtualScanCode;
;   union {
;       WCHAR UnicodeChar;
;       CHAR  AsciiChar;
;   } uChar;
;   DWORD dwControlKeyState;
;} KEY_EVENT_RECORD;

lpBuffer:       ; INPUT_RECORD, KEY_EVENT_RECORD flavour
  .EventType dw ?   (0)
        .crud     dw ?      ; (uH? probably it must dword-align before the union)
    .keyDown    dd ? (4)
    .keyRepeat  dw ? (8)
    .keyVirt    dw ? (10)
    .keyScan    dw ? (12)
    .keyChar    db ? (14)
                db ?
    .keyState   dd ? (16)
--*/

--/*
global function wait_key(, :%opWaitKey)
-- Get the next key pressed by the user.
-- Wait until a key is pressed.
end function
--*/

  :%opWaitKey                      -- [edi]=wait_key()
-------------
    [32]
        -- calling convention:
        --  lea edi,[p1]    -- result location
        --  call :%opWaitKey
        push edi
        cmp [stdout],0
        jne @f
            call :%n_initC -- (preserves eax)
      @@:
--      call clear_debug [DEV]
    [PE32]
        sub esp,20 -- sizeof(INPUT_RECORD[/KEY_EVENT_RECORD])
        mov edi,esp -- (preserved over api calls)
        push ebx    -- DWORD NumberOfEventsRead(:=0)
      @@:
        push esp                                -- lpNumberOfEventsRead
        push 1                                  -- nLength
        push edi                                -- lpBuffer
        push [stdin]                            -- hConsoleInput
        call "kernel32.dll","ReadConsoleInputA"
        cmp word[edi],0x0001                    -- lpBuffer.EventType=KEY_EVENT?
        jne @b
        cmp dword[edi+4],0x00000001             -- lpBuffer.keyDown(ignore key up events)
        jne @b
        xor eax,eax
        mov al,[edi+14]                         -- lpBuffer.keyChar
        or ax,ax
        jne @f
            mov ax,256
            add ax,[edi+12]                     -- lpBuffer.keyScan
            --
            -- Skip control, alt, shift and caps lock keys.
            -- Note that shift, ctrl, alt, and caps lock keys will repeat 
            -- while they are held down, so a routine which wants to detect
            -- them pressed on their own cannot accept eg <Ctrl A>, or even
            -- <Shift 1>, aka '!'. If you _do_ want to detect them on their 
            -- own, just make a copy of this routine minus these lines.
            -- Note values determined by experimentation, not documentation!
            --
            cmp ax,298  -- shift key
            je @b
            cmp ax,285  -- ctrl key
            je @b
            cmp ax,312  -- alt key
            je @b
-- actually, RDS lets this through...
--              cmp ax,314  -- caps lock
--              je @b
--          xor ah,ah
      @@:
        add esp,24  -- sizeof(INPUT_RECORD[/KEY_EVENT_RECORD] and DWORD NumberOfEventsRead)
    [ELF32]
--#     Name                        Registers                                                                                                               Definition
--                                  eax     ebx                     ecx                     edx                     esi                     edi
--3     sys_read                    0x03    unsigned int fd         char *buf               size_t count            -                       -               fs/read_write.c:391
        -- DEV either this or get_key() must be wrong... [test this does not return -1!]
--/*
        push ebx            -- reserve space for buffer (1 byte really)
        mov eax,3           -- sys_read
        mov ebx,[stdin]     -- fd
        mov ecx,esp         -- buffer
        mov edx,1           -- count
        int 0x80
        xor ebx,ebx         -- (common requirement after int 0x80)
        test eax,eax
        pop eax
        jg @f
            mov eax,-1
      @@:
--*/
        mov eax,1
        jmp :xGetOrWait
    [32]
        pop edi
        mov edx,[edi]
        mov [edi],eax
        cmp edx,h4
        jle @f                          -- this jump almost always taken
            sub dword[ebx+edx*4-8],1    -- <any object> := wait_key() case [rare, tgt is usually an int]
            jz :%pDealloc
      @@:       
        ret
    [64]
        -- calling convention:
        --  lea rdi,[p1]    -- result location
        --  call :%opWaitKey
        push rdi
        cmp [stdout],0
        jne @f
            call :%n_initC -- (preserves eax)
      @@:
--      call clear_debug [DEV]
    [PE64]
        mov rcx,rsp -- put 2 copies of rsp onto the stack...
        push rsp
        push rcx
        or rsp,8    -- [rsp] is now 1st or 2nd copy:
                    -- if on entry rsp was xxx8: both copies remain on the stack
                    -- if on entry rsp was xxx0: or rsp,8 effectively pops one of them (+8)
                    -- obviously rsp is now xxx8, whichever alignment we started with
        sub rsp,24 -- sizeof(INPUT_RECORD[/KEY_EVENT_RECORD])   (rounded up to whole qwords)
        mov rdi,rsp -- (preserved over api calls)
        sub rsp,8*6
      @@:
        mov [rsp+32],rbx
        lea r9,[rsp+32]                         -- lpNumberOfEventsRead
        mov r8,1                                -- nLength
        mov rdx,rdi                             -- lpBuffer
        mov rcx,[stdin]                         -- hConsoleInput
        call "kernel32.dll","ReadConsoleInputA"
        cmp word[rdi],0x0001                    -- lpBuffer.EventType=KEY_EVENT?
        jne @b
        cmp dword[rdi+4],0x00000001             -- lpBuffer.keyDown(ignore key up events)
        jne @b
        xor rax,rax
        mov al,[rdi+14]                         -- lpBuffer.keyChar
        or ax,ax
        jne @f
            mov ax,256
            add ax,[rdi+12]                     -- lpBuffer.keyScan
            --
            -- Skip control, alt, shift and caps lock keys.
            -- Note that shift, ctrl, alt, and caps lock keys will repeat 
            -- while they are held down, so a routine which wants to detect
            -- them pressed on their own cannot accept eg <Ctrl A>, or even
            -- <Shift 1>, aka '!'. If you _do_ want to detect them on their 
            -- own, just make a copy of this routine minus these lines.
            -- Note values determined by experimentation, not documentation!
            --
            cmp ax,298  -- shift key
            je @b
            cmp ax,285  -- ctrl key
            je @b
            cmp ax,312  -- alt key
            je @b
-- actually, RDS lets this through...
--              cmp ax,314  -- caps lock
--              je @b
--          xor ah,ah
      @@:
--      add rsp,24+8*6  -- sizeof(INPUT_RECORD[/KEY_EVENT_RECORD] and shadowspace/DWORD NumberOfEventsRead)
--      add rsp,24+48   -- (19/8/15 oops, above became add rsp,160 ie 32*5)
--      pop rsp
        mov rsp,[rsp+24+48] -- equivalent to the add/pop
    [ELF64]
--%rax  System call             %rdi                    %rsi                            %rdx                    %rcx                    %r8                     %r9
--0     sys_read                unsigned int fd         char *buf                       size_t count
--/*
        push rbx                -- buffer
        mov rax,0               -- sys_read
        mov rdi,[stdin]         -- fd
        mov rsi,rsp             -- buffer
        mov rdx,1               -- count
        syscall
        test rax,rax
        pop rax
        jg @f
            mov rax,-1
      @@:
--*/
        mov rax,1
        jmp :xGetOrWait
    [64]
        pop rdi
        mov r15,h4
        mov rdx,[rdi]
        mov [rdi],rax
        cmp rdx,r15
        jle @f                          -- this jump almost always taken
            sub qword[rbx+rdx*4-16],1   -- <any object> := wait_key() case [rare, tgt is usually an int]
            jz :%pDealloc
      @@:       
        ret
    []

--    ::wait_key
------------------
--    @@:
--      call clear_debug
--      invoke  ReadConsoleInput,[stdin],lpBuffer,1,lpEventsRead
--      cmp [lpBuffer.EventType],0x0001 --KEY_EVENT
--      jne @b
--              cmp [lpBuffer.keyDown],0x00000001 --ignore key up events
--              jne @b

--              xor eax,eax
--              mov al,[lpBuffer.keyChar]
--              or ax,ax
--              jne @f
--              mov ax,256
--              add ax,[lpBuffer.keyScan]
--
-- Skip control, alt, shift and caps lock keys.
-- Note that shift, ctrl, alt, and caps lock keys will repeat 
-- while they are held down, so a routine which wants to detect
-- them pressed on their own cannot accept eg <Ctrl A>, or even
-- <Shift 1>, aka '!'. If you _do_ want to detect them on their 
-- own, just make a copy of this routine minus these lines.
-- Note values determined by experimentation, not documentation!
--
--              cmp ax,298  -- shift key
--              je @b
--              cmp ax,285  -- ctrl key
--              je @b
--              cmp ax,312  -- alt key
--              je @b
-- actually, RDS lets this through...
--       cmp ax,314  -- caps lock
--       je @b
--              ret
--        @@:
--       xor eax,eax
--       mov al,[lpBuffer.keyChar]
-- instead I've just used the byte from keyState, again, maybe
-- my definition of lbBuffer is not quite right, but it works.
--              xor ah,ah
--      ret
--endp

      ::fin
      }
--test with: (and later make pilx86.e emit)
--#ilASM{ mov eax,[fn]
--      call :%opGetc
--      mov [ch],eax }

--/*
--[DEV] untested (search for opPuts1Sq [in this source]):
--procedure puts1seq()--sequence s)
--sequence s
--  #ilASM{
--      [32]
--          pop esi
--          mov [s],esi     -- (incref'd on the push in :%n_opPuts1Sq)
--      [64]
--          pop rsi
--          mov [s],rsi     -- (incref'd on the push in :%n_opPuts1Sq)
--        }
----DEV or use mov edx,routine_id("toStringN") etc
--  s = toString(s,e65sfics)
----DEV newsize [PE32] (or, use something from above...)
--  #ilASM{
--      [PE32]
--          mov edx,[s]
--          mov ecx,[ebx+edx*4-12]      -- length
--          shl edx,2
--          pop eax
--          push ebx                    -- lpOverlapped (NULL)
--          push esp                    -- lpNumberOfBytesWritten
--          push ecx                    -- nNumberOfBytesToWrite
--          push edx                    -- lpBuffer
--          push eax                    -- hFile,
--          call "kernel32.dll","WriteFile"
--      [ELF32]
--          pop al
--      [PE64]
--          pop al
--      [ELF64]
--          pop al
--        }
--end procedure
--*/
--/*
#ilASM{
  :%n_opPuts1Sq
    [32]
        push eax    -- stdout/stderr (DEV from where? update:it is [stdout|stderr] by the time we get here)
        add dword[ebx+edx*4-8],1    -- incref
        push edx    -- the dword sequence to be displayed
    [64]
        pop al
      }
--better: use the mov edx,routine_id() thing...
puts1seq()
#ilASM{ ret }
--*/

--DEV we might want to allow forward types after all...
-- for now, just make them local...
--global 
type lock_type(integer t)
    return t = LOCK_SHARED or t = LOCK_EXCLUSIVE
end type

--global 
type byte_range(sequence r)
    if length(r) = 0 then
        return 1
    elsif length(r) = 2 and r[1] <= r[2] then
        return 1
    else
        return 0
    end if
end type

--/*
function process_byterange(atom fhandle, sequence byterange)
atom offsetLO,offsetHI,bytesLO,bytesHI
    if length(byterange)=0 then
        if c_func(xGetFileInformationByHandle, {fhandle,pBHFI})=0 then ?9/0 end if
        offsetLO = 0
        offsetHI = 0
        bytesLO = peek4u(pBHFI+BHFI_FSLO)
        bytesHI = peek4u(pBHFI+BHFI_FSHI)
    else
        offsetLO = byterange[1]
        bytesLO = byterange[2]-offsetLO
        offsetHI = floor(offsetLO/#100000000)
        offsetLO = and_bits(offsetLO,#FFFFFFFF)
        bytesHI = floor(bytesLO/#100000000)
        bytesLO = and_bits(bytesLO,#FFFFFFFF)
    end if
    return {fhandle,offsetLO,offsetHI,bytesLO,bytesHI}
end function
--*/

constant UNLOCK = 0
function flock(integer fn, integer locktype, byte_range byterange)
--function flock(integer fn, lock_type locktype, byte_range byterange)
-- locktype is UNLOCK/LOCK_SHARED/LOCK_EXCLUSIVE (0/1/2) (NB: not a lock_type)
integer iThis
atom offset, bytes
integer res

    if not finit then initF() end if
    iThis = get_this(fn)
if platform()=WINDOWS then
    if length(byterange)=0 then
        offset = 0
        #ilASM{
            -- bytes:=file_size(fn)
            [PE32]
                mov esi,[iThis]
                sub esp,sizeof_BHFI
                push esp                                    -- lpFileInformation
                push dword[ebx+esi*4+HNDL]                  -- hFile
                call "kernel32.dll","GetFileInformationByHandle"
--              test eax,eax
--              jz ??? [DEV]
--              fld qword[esp+BHFI_FSHI]    --DEV are these the wrong way round?
                mov edx,[esp+BHFI_FSHI]
                lea edi,[bytes]
                mov [esp+BHFI_NLINK],edx
                fild qword[esp+BHFI_FSLO]
                add esp,sizeof_BHFI
                call :%pStoreFlt
--          [ELF32]
-- ugh, we don't use this anyway...
--              sub esp,104 -- sizeof(stat64)
--              mov ecx,esp
--              mov ebx,eax
--              mov eax,197 -- sys_fstat64
--              int 0x80
--              xor ebx,ebx             -- (common requirement after int 0x80)
----                cmp eax, -4069 
----                jbe @f
----                    mov al,64               -- e64sfooa: "seek fail on open append"
----                    mov edx,[ebp+12]        -- "called from" address
----                    mov ebp,[ebp+20]        -- prev_ebp
----                    sub edx,1
----                    jmp :!iDiag
----                    int3
----              @@:
--              fild qword[esp+44] ??48
--              add esp,104
--              lea edi,[bytes]
--              call :%pStoreFlt
            [PE64]
                mov rsi,[iThis]
                sub rsp,sizeof_BHFI64
                mov rdx,rsp
--              mov rcx,rsp -- put 2 copies of rsp onto the stack...
                push rsp
                push rdx
                or rsp,8    -- [rsp] is now 1st or 2nd copy:
                            -- if on entry rsp was xxx8: both copies remain on the stack
                            -- if on entry rsp was xxx0: or rsp,8 effectively pops one of them (+8)
                            -- obviously rsp is now xxx8, whichever alignment we started with
                sub rsp,8*5
--              (rdx already set)                               -- lpFileInformation
                mov rcx,[rbx+rsi*4+HNDL64]                      -- hFile
                call "kernel32.dll","GetFileInformationByHandle"
--              add rsp,8*5
--              pop rsp
                mov rsp,[rsp+8*5]   -- equivalent to the add/pop
--              test rax,rax
--              jz ??? [DEV]
--              fld qword[rsp+BHFI_FSHI]    --DEV are these the wrong way round?
                mov edx,[rsp+BHFI_FSHI]
                lea rdi,[bytes]
                mov [rsp+BHFI_NLINK],edx
                fild qword[rsp+BHFI_FSLO]
                add rsp,sizeof_BHFI64
                call :%pStoreFlt
--          [ELF64]
--              sub rsp,144 -- sizeof(stat64)
--              mov rsi,rsp
--              mov rdi,rax
--              mov rax,5 -- sys_fstat
--              syscall
----                or rax,rax 
----                jns @f
----                                                    --DEV actually this is fstat fail...
----                    mov al,64                       -- e64sfooa: "seek fail on open append"
----                    mov rdx,[rbp+24]                -- "called from" address
----                    mov rbp,[rbp+40]                -- prev_ebp
----                    sub rdx,1
----                    jmp :!iDiag
----                    int3
----              @@:
----                cmp rax,-1
----                je fs_error
--              fild qword[rsp+48]
--              add rsp,144
--              lea edi,[bytes]
--              call :%pStoreFlt
            []
              }
    else
        offset = byterange[1]
        bytes = byterange[2]-offset
    end if
else
    -- UNLOCK/LOCK_SHARED/LOCK_EXCLUSIVE (0/1/2) -> LOCK_UN/LOCK_SH+LOCK_NB/LOCK_EX+LOCK_NB (8/5/6)
--LOCK_SH = 1;
--LOCK_EX = 2;
--LOCK_NB = 4;
--LOCK_UN = 8;
    locktype = iff(locktype=0?8:locktype+4)
end if
    #ilASM{
        [PE32]
            sub esp,sizeof_OVERLAPPED
            mov edx,esp
            mov eax,[offset]
            mov ecx,[bytes]
            push ebx                        -- bytesHi
            push ecx                        -- bytesLo
            mov [edx+OV_OFFHI],ebx          -- OVERLAPPED.offsetHi
            mov [edx+OV_OFFSET],eax         -- OVERLAPPED.offsetLo
            cmp eax,h4
            jl @f
                sub esp,8
                fld qword[ebx+eax*4]
                fistp qword[esp]
                pop dword[edx+OV_OFFSET]    -- OVERLAPPED.offsetLo
                pop dword[edx+OV_OFFHI]     -- OVERLAPPED.offsetHi
          @@:
            cmp ecx,h4
            jl @f
                fld qword[ebx+ecx*4]
                fistp qword[esp]
          @@:
            mov edi,[locktype]
            pop ecx                         -- nNumberOfBytesTo(Un)LockLow
            pop eax                         -- nNumberOfBytesTo(Un)LockHigh
            mov esi,[iThis]
            mov [edx+OV_EVENT],ebx          -- OVERLAPPED.hEvent:=0
            push edx                        -- lpOverlapped
            push eax                        -- nNumberOfBytesToLockHigh
            push ecx                        -- nNumberOfBytesToLockLow
            push ebx                        -- dwReserved
            cmp edi,UNLOCK
            je :unlock
            and edi,2                       -- LOCKFILE_EXCLUSIVE_LOCK (keep)
            or edi,1                        -- LOCKFILE_FAIL_IMMEDIATELY (add)

--          push edx                        -- lpOverlapped
--          push eax                        -- nNumberOfBytesToLockHigh
--          push ecx                        -- nNumberOfBytesToLockLow
--          push ebx                        -- dwReserved
            push edi                        -- dwFlags
            push dword[ebx+esi*4+HNDL]      -- hFile
            call "kernel32.dll","LockFileEx"
            jmp @f
          ::unlock
--          push edx                        -- lpOverlapped
--          push eax                        -- nNumberOfBytesToUnlockHigh
--          push ecx                        -- nNumberOfBytesToUnlockLow
--          push ebx                        -- dwReserved
            push dword[ebx+esi*4+HNDL]      -- hFile
            call "kernel32.dll","UnlockFileEx"
          @@:
            add esp,sizeof_OVERLAPPED
            mov [res],eax
        [ELF32]
--#     Name                        Registers                                                                                                               Definition
--                                  eax     ebx                     ecx                     edx                     esi                     edi
--143   sys_flock                   0x8f    unsigned int fd         unsigned int cmd        -                       -                       -                       fs/locks.c:1569
--LOCK_SH = 1;
--LOCK_EX = 2;
--LOCK_NB = 4;
--LOCK_UN = 8;
            mov esi,[iThis]
            mov eax,143                 -- sys_flock
            mov ecx,[locktype]          -- (as mapped above)
            mov ebx,[ebx+esi*4+HNDL]    -- fd
            int 0x80
            -- 0=success, -1=error => 1,0
            add eax,1
            xor ebx,ebx                 -- (common requirement after int 0x80)
            mov [res],eax
        [PE64]
            sub rsp,sizeof_OVERLAPPED64
            mov r15,h4
            mov rdx,rsp
            mov rax,[offset]
            mov rcx,[bytes]
            mov qword[rdx+OV_OFFSET],rax    -- OVERLAPPED.offsetLo (and OV_OFFHI)
            push rcx
            cmp rax,r15
            jl @f
                fld tbyte[rbx+rax*4]
                fistp qword[rdx+OV_OFFSET]  -- OVERLAPPED.offsetLo (and OV_OFFHI)
          @@:
            cmp rcx,r15
            jl @f
                fld tbyte[rbx+rcx*4]
                fistp qword[rsp]
          @@:
            mov rdi,[locktype]
            mov r10,rbx
            mov rax,rbx
--DEV??
            mov r10d,[rsp]                      -- (DWORD) nNumberOfBytesTo(Un)LockLow
            mov eax,[rsp+4]                     -- (DWORD) nNumberOfBytesTo(Un)LockHigh
            pop rcx
            mov rsi,[iThis]
            mov rcx,rsp -- put 2 copies of rsp onto the stack...
            push rsp
            push rcx
            or rsp,8    -- [rsp] is now 1st or 2nd copy:
                        -- if on entry rsp was xxx8: both copies remain on the stack
                        -- if on entry rsp was xxx0: or rsp,8 effectively pops one of them (+8)
                        -- obviously rsp is now xxx8, whichever alignment we started with
--          sub rsp,8*6
            sub rsp,8*7
            mov rcx,[rbx+rsi*4+HNDL64]
            mov [rdx+OV_EVENT],rbx              -- OVERLAPPED.hEvent:=0
            cmp rdi,0
            je :unlock
            and rdi,2                           -- LOCKFILE_EXCLUSIVE_LOCK (keep)
            mov [rsp+40],rdx                    -- lpOverlapped
            or rdi,1                            -- LOCKFILE_FAIL_IMMEDIATELY (add)
            mov [rsp+32],rax                    -- nNumberOfBytesToLockHigh
            mov r9,r10                          -- nNumberOfBytesToLockLow
            mov r8,rbx                          -- dwReserved
            mov rdx,rdi                         -- dwFlags
--          (rcx already set)                   -- hFile
            call "kernel32.dll","LockFileEx"
            jmp @f
          ::unlock
            mov [rsp+32],rdx                    -- lpOverlapped
            mov r9,rax                          -- nNumberOfBytesToUnlockHigh
            mov r8,r10                          -- nNumberOfBytesToUnlockLow
            mov rdx,rbx                         -- dwReserved
--          (rcx already set)                   -- hFile
            call "kernel32.dll","UnlockFileEx"
          @@:
--          add rsp,8*7 -- (includes the push rcx from 41 lines ago)
--          add rsp,8*7
--          pop rsp
            mov rsp,[rsp+8*7]   -- equivalent to the add/pop
            add rsp,sizeof_OVERLAPPED64
            mov [res],rax
        [ELF64]
--%rax  System call             %rdi                    %rsi                            %rdx                    %rcx                    %r8                     %r9
--73    sys_flock               unsigned int fd         unsigned int cmd
--LOCK_SH = 1;
--LOCK_EX = 2;
--LOCK_NB = 4;
--LOCK_UN = 8;
            mov rdi,[iThis]
            mov eax,73                  -- sys_flock
            mov rsi,[locktype]          -- (as mapped above)
            mov rdi,[rbx+rdi*4+HNDL]    -- fd
            syscall
            -- 0=success, -1=error => 1,0
            add rax,1
            mov [res],rax
        []
          }
    return res
end function

--global function lock_file(integer fn, lock_type locktype, byte_range byterange)
function flock_file(integer fn, lock_type locktype, byte_range byterange)
-- note this has a typecheck on locktype whereas flock() (which is used for both lock and unlock) does not (and should not).
    return flock(fn,locktype,byterange)
end function

--/*
flock:
    mov eax,[edi]
    cmp eax,h4
    jge e60fninai8      -- file number is not an integer
    cmp eax,2
    jle e58bfneax8                  -- invalid file number (%d)
    mov edi,[ftend]
  @@:
    or edi,edi
    jz e58bfneax8                   -- invalid file number (%d)
    cmp [edi+f_no],eax
    je @f
    mov edi,[edi+f_prev]
    jmp @b
  @@:
    mov eax,[byterange]
    mov esi,edi
    cmp eax,h4
    jle e67bre
    shl eax,2
    test byte[eax-1],0x80
    jz e67bre
    mov edx,[eax-12]
    or edx,edx
    jne @f
--   if equal(byterange,{}) then
    mov eax,[esi]   -- handle
    invoke GetFileInformationByHandle, eax, BHFI
    mov eax,[esi]   -- handle
    xor ecx,ecx     -- high-order word of lock region (0)
    mov edi,ecx     -- low-order word of lock region (0)
                    -- number of bytes to lock are in BHFIfileSize.
    ret

  @@:
    cmp edx,2
    jne e67bre                -- byterange error (length not 0 or 2)
    cmp byte[eax-1],0x80
    je @f
    cmp byte[eax-1],0x82
    jne e67bre
    xor ecx,ecx
    mov ax,[eax]
    mov cl,ah
    and eax,0xFF
    mov edi,eax
    jmp byterangeLoaded
  @@:
    lea edi,[eax+4]
    mov ecx,[edi]
    cmp ecx,h4
    jl @f
        shl ecx,2
        mov [emapTo],edi
        call emapecx8
        jne e67bre     -- ecx = byterange[2]
  @@:
    mov edi,[eax]
    cmp edi,h4
    jl @f
        shl edi,2
        mov [emapTo],eax
      flocke92b:    -- exception here mapped to e92vhnbaavEmapToEsp8feh
        cmp byte[edi-1],0x12
        jne e67bre     -- edi = byterange[1]
        fld qword[edi]
        mov edi,FltWrk
        fldcw [down53]
        fistp qword[edi]
        fldcw [near53]
        mov edi,[edi]
  @@:
  byterangeLoaded:
    sub ecx,edi        -- dev 32-bit maths!?
    mov [BHFInFileSizeLow],ecx -- number of bytes to lock (DEV max 4G only)
    xor ecx,ecx        -- high-order word of lock region [must be 0 on 95/98 anyway]
    mov [BHFInFileSizeHigh],ecx -- high-order word of number of bytes to lock=0 [DEV]
    mov eax,[esi]    -- handle
    ret

opLock:                        -- [edx]=lock_file(edi,ecx,esi)
--------
    trc Lock
-- calling convention:
--  mov esi,p4  -- addr byterange
--  mov ecx,p3  -- addr locktype
--  mov edi,p2  -- addr fileno
--  mov edx,p1  -- result location
--  call opLock

    mov ecx,[locktype]
    mov esi,[byterange]

    mov eax,ecx
    and eax,0x03
    cmp eax,ecx
    jne e66ilt                      -- invalid lock type
    mov [byterange],esi
    push edx
    call flock

    invoke LockFile, eax,edi,ecx,[BHFInFileSizeLow],[BHFInFileSizeHigh]
    mov [res],eax
    ret

opUnLock:                      -- unlock(edi,esi)
----------
    trc UnLock
-- calling convention:
--  mov esi,p2  -- addr byterange
--  mov edi,p1  -- addr fileno
--  call opUnLockR

    mov esi,[esi]               -- byterange
push edx    -- to match opLock, [era]-wise
    mov [byterange],esi
    call flock

    invoke UnlockFile, eax,edi,ecx,[BHFInFileSizeLow],[BHFInFileSizeHigh]
pop edi     -- ditto
    ret
--*/

-- replaced by :%opUnLock
--global procedure unlock_file(integer fn, byte_range byterange)
--  {} = flock(fn,UNLOCK,byterange)
----integer iThis
----atom fhandle
----sequence params
----integer res
----    if not finit then initF() end if
----    iThis = get_this(fn)
----    fhandle = peek4u(iThis*4+HNDL)
----    params = process_byterange(fhandle,byterange)
----    res = c_func(xUnlockFile,params)
--end procedure

--/* -- now in pfile.e:
function addline(sequence res, integer i, integer start, integer option, integer filesize, string src)
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
    if option=GT_LF_STRIPPED then
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
    return res
end function
--*/

function fget_text(integer fn, integer option)
integer fidx, ch
integer fmode
--atom fhandle
--integer i
object res
--integer start
integer filesize
string src
--atom frealposn
--atom this
integer iThis

    if safemode then ?9/0 end if
--DEV/temp:
bool from_hll = false
if option>8 then
    from_hll = true
    option -= 10        -- (temp) [-2/-1/0/1 <--> 8/9/10/11]
end if

--  if string(fn) then
--      integer fn2 = open(fn,"rb")
--      src = fget_text(fn2,option)
--      close(fn2)
--      return src
--  end if
    fidx = fn-2
    fmode = 0
    if finit and fidx>=1 and fidx<=fdmax then
--DEV newsize [PE32], get_this()?
        fmode = 1   --DEV should really fix this... (compiler issue)
        #ilASM{
            [32]
                mov edx,[fidx]
                shl edx,2
              @@:
                mov edi,[fdtbl]
                mov esi,[edx+edi*4-4]   -- esi:=fdtbl[fidx]
                cmp edi,[fdtbl]
                jne @b
                mov eax,[ebx+esi*4+MODE]
                mov [iThis],esi
                mov [fmode],eax
                mov dword[ebx+esi*4+POSN],1
                mov [ebx+esi*4+FEND],ebx
                mov [ebx+esi*4+POSL],ebx
                mov [ebx+esi*4+POSH],ebx
            [64]
                mov rdx,[fidx]
                shl rdx,3
              @@:
                mov rdi,[fdtbl]
                mov rsi,[rdx+rdi*4-8]   -- rsi:=fdtbl[fidx]
                cmp rdi,[fdtbl]
                jne @b
                mov rax,[rbx+rsi*4+MODE64]
                mov [iThis],rsi
                mov [fmode],rax
                mov qword[rbx+rsi*4+POSN64],1
                mov [rbx+rsi*4+FEND64],rbx
                mov [rbx+rsi*4+RPOS64],rbx
              }
--      fmode = peek4u(this+MODE)
    end if
    if fmode=0 or fn<=2 then
--      iofatal(58,fn)  -- "invalid file number (%d)"
--      fatalN(2,e59wfmfao,fn)
        fatalN(2,e58ifn,fn)
    end if
    if and_bits(fmode,or_bits(F_READ,F_WRITE))!=F_READ then
--      iofatal(59) -- "wrong file mode for attempted operation"
        fatalN(2,e59wfmfao,fn)
    end if
--DEV
    if and_bits(fmode,F_DIRTY) then
        ?9/0 
    end if

--  if option<-2 or option>1 then return -1 end if
--  if option<-2 or option>1 then ?9/0 end if
    if option<0 or option>8 then ?9/0 end if

--DEV newsize [PE32] machine_bits()
--  if machine_bits()=32 then
--      poke4(iThis*4+POSN,1)   -- unnecessary?
--      poke4(iThis*4+FEND,0)
--      poke8(iThis*4+POSL,0)
----DEV we don't need this:
--      fhandle = peek4u(iThis*4+HNDL)
--  else
--      poke8(iThis*4+POSN64,1) -- unnecessary?
--      poke8(iThis*4+FEND64,0)
--      poke8(iThis*4+RPOS64,0)
----DEV we don't need this:
--      fhandle = peek8u(iThis*4+HNDL64)
--  end if

--DEV better?:
    if seek(fn,0)!=SEEK_OK then ?9/0 end if
--  frealposn = 0
--  frealposn = c_func(xSetFilePointer,{fhandle,0,NULL,FILE_BEGIN})

--DEV newsize [PE32]
    #ilASM{
        [32]
            mov esi,[iThis]
        [PE32]
            push ebx                            -- reserve space for FileSizeHigh
            push esp                            -- lpFileSizeHigh
            push dword[ebx+esi*4+HNDL]          -- hFile
            call "kernel32.dll","GetFileSize"
            pop ecx
        [ELF32]
            sub esp,104                     -- sizeof(stat64)
            mov eax,197                     -- sys_fstat64
            mov ebx,[ebx+esi*4+HNDL]        -- fd
            mov ecx,esp                     -- stat64
            int 0x80
            xor ebx,ebx                     -- (common requirement after int 0x80)
--              cmp eax, -4069 
--              jbe @f
--                  mov al,?64              -- ?
--                  mov edx,[ebp+12]        -- "called from" address
--                  mov ebp,[ebp+20]        -- prev_ebp
--                  sub edx,1
--                  jmp :!iDiag
--                  int3
--            @@:
            mov ecx,[esp+48]                -- high dword
--          mov ecx,[esp+52]
            mov eax,[esp+44]                -- low dword
--          mov eax,[esp+48]
            add esp,104
        [32]
            test ecx,ecx
            jnz :highnotzero
            cmp eax,h4
            jb :lowOK
          ::highnotzero
--              -- e78atgtgt1gbf: "attempt to get_text() >1GB file"
--              mov al,78
--              xor edi,edi     -- ep1 unused
--              xor esi,esi     -- ep2 unused
----DEV
--              call :%pRTErn   -- fatal error
                mov al,78               -- e78atgtgt1gbf: "attempt to get_text() >1GB file"
                mov edx,[ebp+12]        -- "called from" address
                mov ebp,[ebp+20]        -- prev_ebp
                sub edx,1
                jmp :!iDiag
                int3
          ::lowOK
            mov [filesize],eax
        [64]
            mov rsi,[iThis]
        [PE64]
            mov rcx,rsp -- put 2 copies of rsp onto the stack...
            push rsp
            push rcx
            or rsp,8    -- [rsp] is now 1st or 2nd copy:
                        -- if on entry rsp was xxx8: both copies remain on the stack
                        -- if on entry rsp was xxx0: or rsp,8 effectively pops one of them (+8)
                        -- obviously rsp is now xxx8, whichever alignment we started with
            sub rsp,8*7
            lea rdx,[rsp+32]                    -- lpFileSize
            mov rcx,[rbx+rsi*4+HNDL64]          -- hFile
            call "kernel32.dll","GetFileSizeEx"
            mov rcx,[rsp+32]
--          add rsp,8*7
--          pop rsp
            mov rsp,[rsp+8*7]   -- equivalent to the add/pop
        [ELF64]
--%rax  System call             %rdi                    %rsi                            %rdx                    %rcx                    %r8                     %r9
--5     sys_fstat               unsigned int fd         struct stat *statbuf
            sub rsp,144 -- sizeof(stat64)
            mov rax,5                           -- sys_fstat
            mov rdi,[rbx+rsi*4+HNDL64]          -- fd
            mov rsi,rsp                         -- stat64
            syscall
--          or rax,rax 
--          jns @f
--                                              --DEV actually this is fstat fail...
--              mov al,64                       -- e64sfooa: "seek fail on open append"
--              mov rdx,[rbp+24]                -- "called from" address
--              mov rbp,[rbp+40]                -- prev_ebp
--              sub rdx,1
--              jmp :!iDiag
--              int3
--        @@:
--          cmp rax,-1
--          je fs_error
            mov rcx,[rsp+48]
            add rsp,144
        [64]
            mov [filesize],rcx
          }
    if filesize=0 then
        if option=GT_WHOLE_FILE then
            if and_bits(fmode,F_BINARY) then
                return ""
            else
                return "\n"
            end if
        else
            return {}
        end if
    end if
--DEV or binary mode?? (spotted in passing)
--  if option=0 then    -- GT_LF_LEFT
    if option=GT_LF_LEFT
    or (option=GT_WHOLE_FILE and and_bits(fmode,F_BINARY)) then
        src = repeat('\n',filesize)
        option = GT_LF_LEFT -- (avoid adjust below, in the bin/whole case)
    else
        src = repeat('\n',filesize+1)       -- add/plant "safety lf"
    end if
    res = -1
--DEV newsize [PE32]
    #ilASM{
        [32]
            mov edi,[src]
            mov esi,[iThis]
            shl edi,2
        [PE32]
            push ebx                    -- reserve space for NumberOfBytesRead
            mov ecx,[filesize]
            mov edx,esp
            push ebx                                        -- lpOverlapped (NULL)
            push edx                                        -- lpNumberOfBytesRead
            push ecx                                        -- nNumberOfBytesToRead
            push edi                                        -- lpBuffer
            push dword[ebx+esi*4+HNDL]                      -- hFile
            call "kernel32.dll","ReadFile"
            pop ecx
            test eax,eax
            jz :retZ2       -- ReadFile failed (see GetLastError)
            test ecx,ecx
            jz :retZ2       -- ReadFile read 0 bytes
        [ELF32]
--#     Name                        Registers                                                                                                               Definition
--                                  eax     ebx                     ecx                     edx                     esi                     edi
--3     sys_read                    0x03    unsigned int fd         char *buf               size_t count            -                       -               fs/read_write.c:391
            mov eax,3                   -- sys_read
            mov ebx,[ebx+esi*4+HNDL]    -- fd
            mov ecx,edi                 -- buffer
            mov edx,[filesize]          -- count
            int 0x80
--          cmp eax, -4069 
--          jbe @f
--              mov eax,INVALID_HANDLE_VALUE
--        @@:
            xor ebx,ebx                 -- (common requirement after int 0x80)
            test eax,eax
            jle :retZ2
        [64]
            mov rdx,[src]
            mov rsi,[iThis]
            shl rdx,2
        [PE64]
            mov rcx,rsp -- put 2 copies of rsp onto the stack...
            push rsp
            push rcx
            or rsp,8    -- [rsp] is now 1st or 2nd copy:
                        -- if on entry rsp was xxx8: both copies remain on the stack
                        -- if on entry rsp was xxx0: or rsp,8 effectively pops one of them (+8)
                        -- obviously rsp is now xxx8, whichever alignment we started with
            sub rsp,8*7
            mov [rsp+40],rbx
            mov [rsp+32],rbx                                -- lpOverlapped (NULL)
            lea r9,[rsp+40]                                 -- lpNumberOfBytesRead
            mov r8,[filesize]                               -- nNumberOfBytesToRead
--          (rdx already set)                               -- lpBuffer
            mov rcx,[rbx+rsi*4+HNDL]                        -- hFile
            call "kernel32.dll","ReadFile"
            mov rcx,[rsp+40]
--          add rsp,8*7
--          pop rsp
            mov rsp,[rsp+8*7]   -- equivalent to the add/pop
            test rax,rax
            jz :retZ2
            test rcx,rcx
            jz :retZ2
        [ELF64]
--%rax  System call             %rdi                    %rsi                            %rdx                    %rcx                    %r8                     %r9
--0     sys_read                unsigned int fd         char *buf                       size_t count
            mov rax,0                   -- sys_read
            mov rdi,[rbx+rsi*4+HNDL64]  -- fd
            mov rsi,rdx                 -- buffer
            mov rdx,[filesize]          -- count
            syscall
            test rax,rax
--14/2/17:
--          jle :retZ2
            jl :retZ2
          }

--DEV as above.. (why bother?)
--  frealposn = c_func(xSetFilePointer,{fhandle,0,NULL,FILE_BEGIN})

--  if option!=0 then   -- not GT_LF_LEFT
    if option!=GT_LF_LEFT then -- (nor whole/bin)
        -- remove that safety \n when it is not being helpful
        ch = src[filesize]
        if ch='\n' then
            -- "...\n<safety\n>" ==> "...\n"
            src = src[1..-2]                -- remove that "safety \n"
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

--DEV temp, until the rename is finished...
--  if option=-2 then   -- GT_WHOLE_FILE
--  if option=-2 or from_hll then   -- GT_WHOLE_FILE
--  if option=-2 or option=0 or from_hll then   -- GT_WHOLE_FILE
        return src
--  end if

--/*
    res = {}
    i = 1
    start = 1
    while i<=filesize do
        ch = src[i]
        if ch='\r' or ch='\n' then
            res = addline(res,i,start,option,filesize,src)
--DEV tryme
--          res = append(res,addline(i,start,option,filesize,src))
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
        res = addline(res,i,start,option,filesize,src)
    end if
--*/
 #ilASM{ ::retZ2 }
    return res
end function


--
--integer flag
--sequence expected
--procedure test(sequence src)
--sequence res
--  res = get_text(src,flag)
--  if res!=expected then ?9/0 end if
--end procedure
--if 01 then
----    (GT_WHOLE_FILE) get whole file as one long string, plus final '\n' if missing.
----         (tests: "a\nb\n" from both "a\nb" and "a\nb\n")
--  flag = GT_WHOLE_FILE
--  expected = "a\nb\n"
--  test("a\nb")
--  test("a\nb\n")
--
----    (GT_LF_STRIPPED) cr-stripped lines
----         (eg: {"a","b"} from both "a\nb" and "a\nb\n")
----         (tests: {"a","b"} from "a\nb", "a\nb\n", "a\r\nb", "a\r\nb\r\n", "a\n\rb\n\r", "a\rb", "a\rb\r")
--  flag = GT_LF_STRIPPED
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
----     (GT_LF_LEFT) '\n' left on lines,
----         (tests: {"a\n","b"} from "a\nb" but {"a\n","b\n"} from "a\nb\n")
----        (it should be clear from these tests that being fussy about the
----         last line probably just complicates things unnecessarily.)
--  flag = GT_LF_LEFT
--  expected = {"a\n","b"}
--  test("a\nb")
--  test("a\r\nb")
--  test("a\n\rb")
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
----    (GT_LF_LAST) '\n' left on lines, and put on last line if missing.
----         (tests: {"a\n","b\n"} from both "a\nb" and "a\nb\n")
--  flag = GT_LF_LAST
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

--/*
constant ModeFlagSet = {{0,         "closed"},
                        {F_READ,    "read"  },
                        {F_WRITE,   "write" },
                        {F_BINARY,  "binary"},
                        {F_DIRTY,   "dirty" }}

--:%opIOdiag?? (I somehow think this has passed it's use by date)
global procedure fiodiag(integer fn)
integer fidx
--atom this
integer iThis
atom fhandle
integer fmode
string smode
integer fposn
integer fend
atom frealposn
    fidx = fn-2
--DEV use get_this()?
    #ilASM{
        [32]
            mov edx,[fidx]
            shl edx,2
          @@:
            mov edi,[fdtbl]
            mov esi,[edx+edi*4-4]   -- esi:=fdtbl[fidx]
            cmp edi,[fdtbl]
            jne @b
            mov eax,[ebx+esi*4+MODE]
            mov [iThis],esi
            mov [fmode],eax
            lea edi,[fhandle]
            fild dword[ebx+esi*4+HNDL]
            call :%pStoreFlt
            lea edi,[fposn]
            fild dword[ebx+esi*4+POSN]
            call :%pStoreFlt
            lea edi,[fend]
            fild dword[ebx+esi*4+FEND]
            call :%pStoreFlt
            lea edi,[frealposn]
            fild qword[ebx+esi*4+POSL]
            call :%pStoreFlt
        [64]
            mov rdx,[fidx]
            shl rdx,3
          @@:
            mov rdi,[fdtbl]
            mov rsi,[rdx+rdi*4-8]   -- esi:=fdtbl[fidx]
            cmp rdi,[fdtbl]
            jne @b
            mov rax,[rbx+rsi*4+MODE64]
            mov [iThis],rsi
            mov [fmode],rax
            lea edi,[fhandle]
            fild qword[rbx+rsi*4+HNDL64]
            call :%pStoreFlt
            lea edi,[fposn]
            fild qword[rbx+rsi*4+POSN64]
            call :%pStoreFlt
            lea edi,[fend]
            fild qword[rbx+rsi*4+FEND64]
            call :%pStoreFlt
            lea edi,[frealposn]
            fild qword[rbx+rsi*4+RPOS64]
            call :%pStoreFlt
          }
--  if machine_bits()=32 then
--      fhandle = peek4u(iThis*4+HNDL)
----        fmode = peek4u(iThis*4+MODE)
--      fposn = peek4u(iThis*4+POSN)
--      fend = peek4u(iThis*4+FEND)
--      frealposn = peek8u(iThis*4+POSL)
--  else
--      ?9/0
--  end if
    smode = decode_flags(ModeFlagSet,fmode)
    printf(1,"Handle:#%08x, Mode:#%02x(%s), Posn:%d, Fend:%d, realposn:%d\n",{fhandle,fmode,smode,fposn,fend,frealposn})
    if machine_bits()=32 then
        ?peek({iThis*4+BUFF,10})
    else
        ?peek({iThis*4+BUFF64,10})
    end if
end procedure
--*/

--global function get_position()
--  :%opGetPos
function fget_position()
integer posX,posY
    if not cinit then initConsole() end if
--  #ilASM{ call :%pClearDbg }
    #ilASM{
        [PE32]
            sub esp,sizeof_CSBI
            mov eax,[stdout]
            mov edi,esp
            push edi                                        -- lpConsoleScreenBufferInfo
            push eax                                        -- hConsoleOutput
            call "kernel32.dll","GetConsoleScreenBufferInfo"
--          test eax,eax
--          jz @f
            xor eax,eax
            xor ecx,ecx
            mov ax,[edi+CSBI_CPOSX]
            mov cx,[edi+CSBI_CPOSY]
--        @@:
            mov [posX],eax
            mov [posY],ecx
            add esp,sizeof_CSBI
        [ELF32]
            --DEV OpenEuphoria maintains screen_line/col..
            mov [posX],ebx
            mov [posY],ebx
        [PE64]
            sub rsp,sizeof_CSBI64
            mov rdi,rsp
--          mov rcx,rsp -- put 2 copies of rsp onto the stack...
            push rsp
            push rdi
            or rsp,8    -- [rsp] is now 1st or 2nd copy:
                        -- if on entry rsp was xxx8: both copies remain on the stack
                        -- if on entry rsp was xxx0: or rsp,8 effectively pops one of them (+8)
                        -- obviously rsp is now xxx8, whichever alignment we started with
            sub rsp,8*5
            mov rdx,rdi                                     -- lpConsoleScreenBufferInfo
            mov rcx,[stdout]                                -- hConsoleOutput
            call "kernel32.dll","GetConsoleScreenBufferInfo"
--          add rsp,8*5
--          pop rsp
            mov rsp,[rsp+8*5]   -- equivalent to the add/pop
--          test rax,rax
--          jz @f
            xor rax,rax
            xor rcx,rcx
            mov ax,[rdi+CSBI_CPOSX]
            mov cx,[rdi+CSBI_CPOSY]
--        @@:
            mov [posX],rax
            mov [posY],rcx
            add rsp,sizeof_CSBI64
        [ELF64]
            --DEV ditto
            mov [posX],rbx
            mov [posY],rbx
        []
          }
--  if platform()=WINDOWS then
--      if posX=0 then
--          #ilASM{
--                  call "kernel32.dll","GetLastError"
--              [32]
--                  mov [posX],eax
--              [64]
--                  mov [posX],rax
--                }
--          fatalN(2,e108pe,posX)
--      end if
--  end if
    return {posY+1,posX+1}
end function

--  :%opWrap
--global procedure wrap(integer flag)
procedure fwrap(integer flag)
--atom pMode = allocate(4) [and I never free()'d that!]
--integer Mode, blSet

    if not cinit then initConsole() end if
--> if not c_func(xGetConsoleMode,{stdout,pMode}) then ?9/0 end if
--  Mode = peek4u(pMode)
--  blSet = (and_bits(Mode,ENABLE_WRAP_AT_EOL_OUTPUT)!=0)
----    if blSet!=flag then
--  if blSet=(not flag) then -- (as "", but treats all flag!=0 as true)
--      if flag=0 then
--          Mode -= ENABLE_WRAP_AT_EOL_OUTPUT
--      else
--          Mode += ENABLE_WRAP_AT_EOL_OUTPUT
--      end if
--      if not c_func(xSetConsoleMode,{stdout,Mode}) then ?9/0 end if
--  end if
    #ilASM{
        [PE32]
            push ebx    -- pMode
            push esp                        -- lpMode
            push [stdout]                   -- hConsoleHandle
            call "kernel32.dll","GetConsoleMode"
            mov eax,[esp]
            xor edx,edx
            and eax,ENABLE_WRAP_AT_EOL_OUTPUT
            cmp [flag],0
            je @f
                mov edx,ENABLE_WRAP_AT_EOL_OUTPUT
          @@:
            -- edx is now "desired", eax is "actual" (one bit only)
            cmp eax,edx
            pop eax
            je @f
                xor eax,ENABLE_WRAP_AT_EOL_OUTPUT   -- toggle
                push eax                            -- dwMode
                push [stdout]                       -- hConsoleHandle
                call "kernel32.dll","SetConsoleMode"
          @@:
        [PE64]
--          push ebx    -- pMode
            mov rcx,rsp -- put 2 copies of rsp onto the stack...
            push rsp
            push rcx
            or rsp,8    -- [rsp] is now 1st or 2nd copy:
                        -- if on entry rsp was xxx8: both copies remain on the stack
                        -- if on entry rsp was xxx0: or rsp,8 effectively pops one of them (+8)
                        -- obviously rsp is now xxx8, whichever alignment we started with
            sub rsp,8*5
            mov [rsp+32],rbx
            lea rdx,[rsp+32]                    -- lpMode
            mov rcx,[stdout]                    -- hConsoleHandle
            call "kernel32.dll","GetConsoleMode"
            mov rax,[rsp+32]
            xor rdx,rdx
            and rax,ENABLE_WRAP_AT_EOL_OUTPUT
            cmp [flag],0
            je @f
                mov rdx,ENABLE_WRAP_AT_EOL_OUTPUT
          @@:
            -- rdx is now "desired", rax is "actual" (one bit only)
            cmp rax,rdx
            mov rax,[rsp+32]
            je @f
                xor rax,ENABLE_WRAP_AT_EOL_OUTPUT   -- toggle
                mov rdx,rax                         -- dwMode
                mov rcx,[stdout]                    -- hConsoleHandle
                call "kernel32.dll","SetConsoleMode"
          @@:
--          add rsp,8*5
--          pop rsp
            mov rsp,[rsp+8*5]   -- equivalent to the add/pop
        [ELF32]
            --DEV OpenEuphoria has a flag, and uses it manually...
--          pop al
        [ELF64]
--          pop al
        []
          }
end procedure

----DEV not quite right... (now in builtins\pScrollN.e)
----    :%opScroll
----global procedure scroll(integer amount, integer top, integer bottom)
--procedure fscroll(integer amount, integer top, integer bottom)
--integer right
--integer attributes
--
--  if not cinit then initConsole() end if
--  #ilASM{
--      [PE32]
--          sub esp,sizeof_CSBI
--          mov eax,[stdout]
--          mov edi,esp
--          push edi                                        -- lpConsoleScreenBufferInfo
--          push eax                                        -- hConsoleOutput
--          call "kernel32.dll","GetConsoleScreenBufferInfo"
----            test eax,eax
----            jz ??? [DEV]
--          xor eax,eax
--          xor ecx,ecx
--          mov ax,[edi+CSBI_SIZEX]
--          mov cx,[edi+CSBI_ATTR]
--          sub eax,1
--          mov [attributes],ecx
--          mov [right],eax
--          add esp,sizeof_CSBI
--      [ELF32]
--          pop al
--      [PE64]
--          sub rsp,sizeof_CSBI64
--          mov rdi,rsp
--          mov rcx,rsp -- put 2 copies of rsp onto the stack...
--          push rsp
--          push rcx
--          or rsp,8    -- [rsp] is now 1st or 2nd copy:
--                      -- if on entry rsp was xxx8: both copies remain on the stack
--                      -- if on entry rsp was xxx0: or rsp,8 effectively pops one of them (+8)
--                      -- obviously rsp is now xxx8, whichever alignment we started with
--          sub rsp,8*5
--          mov rdx,rdi                                     -- lpConsoleScreenBufferInfo
--          mov rcx,[stdout]                                -- hConsoleOutput
--          call "kernel32.dll","GetConsoleScreenBufferInfo"
--          add rsp,8*5         
--          pop rsp
--          mov rsp,[rsp+8*5]   -- equivalent to the add/pop
----            test rax,rax
----            jz ??? [DEV]
--          xor rax,rax
--          xor rcx,rcx
--          mov ax,[rdi+CSBI_SIZEX]
--          mov cx,[rdi+CSBI_ATTR]
--          sub rax,1
--          mov [attributes],rcx
--          mov [right],rax
--          add rsp,sizeof_CSBI64
--      [ELF64]
--          pop al
--      []
--        }
--  if abs(amount)>abs(bottom-top) then
--      for i=top-1 to bottom-1 do
--          #ilASM{
--              [PE32]
--                  push ebx    -- space for NumberOf(Attrs|Chars)Written
--                  mov esi,esp
--                  mov eax,[i]
--                  -- push params for FillConsoleOutputAttribute first (before regs get damaged)
--                  push esi                                    -- lpNumberOfAttrsWritten
--                  shl eax,16                                  -- a COORD of {0,i}
--                  mov ecx,[right]
--                  mov edx,[attributes]
--                  mov edi,[stdout]
--                  push eax                                    -- dwWriteCoord
--                  push ecx                                    -- nLength
--                  push edx                                    -- wAttribute
--                  push edi                                    -- hConsoleOutput
--                  -- now params for FillConsoleOutputCharacter
--                  push esi                                    -- lpNumberOfCharsWritten
--                  push eax                                    -- dwWriteCoord
--                  push ecx                                    -- nLength
--                  push ' '                                    -- cCharacter
--                  push edi                                    -- hConsoleOutput
--                  call "kernel32.dll","FillConsoleOutputCharacterA"
--                  call "kernel32.dll","FillConsoleOutputAttribute"
--                  pop eax     -- discard NumberOf(Attrs|Chars)Written
--              [ELF32]
--                  pop al
--              [PE64]
--                  mov rcx,rsp -- put 2 copies of rsp onto the stack...
--                  push rsp
--                  push rcx
--                  or rsp,8    -- [rsp] is now 1st or 2nd copy:
--                              -- if on entry rsp was xxx8: both copies remain on the stack
--                              -- if on entry rsp was xxx0: or rsp,8 effectively pops one of them (+8)
--                              -- obviously rsp is now xxx8, whichever alignment we started with
--                  sub rsp,8*7
--                  mov rsi,[i]
--                  xor rdx,rdx
--                  lea rdi,[rsp+40]                            -- (repeserved over api calls)
--                  shl rsi,16                                  -- a COORD of {0,i} (preserved over api calls)
--                  mov [rsp+32],rdi                                    -- lpNumberOfCharsWritten
--                  mov r9,rsi                                          -- dwWriteCoord
--                  mov r8,[right]                                      -- nLength
----DEV check this in list.asm:
----                    mov rdx,dword' '                                    -- cCharacter
--                  mov dl,' '
--                  mov rcx,[stdout]                                    -- hConsoleOutput
--                  call "kernel32.dll","FillConsoleOutputCharacterA"
--                  mov [rsp+32],rdi                                    -- lpNumberOfAttrsWritten
--                  mov r9,rsi                                          -- dwWriteCoord
--                  mov r8,[right]                                      -- nLength
--                  mov edx,[attributes]                                -- wAttribute
--                  mov rcx,[stdout]                                    -- hConsoleOutput
--                  call "kernel32.dll","FillConsoleOutputAttribute"
---                 add rsp,8*7
---                 pop rsp
--                  mov rsp,[rsp+8*7]   -- equivalent to the add/pop
--              [ELF64]
--                  pop al
--              []
--                }
--      end for
--  else
----        poke2(pSMALLRECT,{0,top-1,right,bottom-1})
----        poke2(pCHARINFO,{' ',attributes})
----        dest = (top-1-amount)*#10000    -- a COORD of {0,top-1-amount}
---->   if not c_func(xScrollConsoleScreenBuffer,{stdout,pSMALLRECT,NULL,dest,pCHARINFO}) then ?9/0 end if
--      #ilASM{
--          [PE32]
--              sub esp,sizeof_SMALL_RECT
--              mov edi,esp
--              sub esp,sizeof_CHAR_INFO
--              mov esi,esp
--              mov eax,[top]
--              mov ecx,[right]
--              sub eax,1
--              mov edx,[attributes]
--              mov [edi+SR_Left],bx    -- 0
--              mov [edi+SR_Top],ax     -- top-1
--              mov [edi+SR_Right],cx   -- right
----DEV not yet supported..
----                sub eax,[amount]
--              mov ecx,[amount]
--              sub eax,ecx
--              mov ecx,[bottom]
----DEV... (same op)
----                sal eax,16              -- a COORD of {0,top-1-amount}
--              shl eax,16              -- a COORD of {0,top-1-amount}
--              mov [esi+CI_UnicodeChar],word' '
--              sub ecx,1
--              mov [esi+CI_Attributes],dx
--              mov [edi+SR_Bottom],cx  -- bottom-1
--              push esi                                    -- lpFill
--              push eax                                    -- dwDestinationOrigin
--              push ebx                                    -- lpClipRectangle(NULL)
--              push edi                                    -- lpScrollRectangle
--              push dword[stdout]                          -- hConsoleOutput
--              call "kernel32.dll","ScrollConsoleScreenBufferA"
--              add esp,sizeof_CHAR_INFO+sizeof_SMALL_RECT
--          [ELF32]
--              pop al
--          [PE64]
--              sub rsp,sizeof_SMALL_RECT64
--              mov rdi,rsp
--              sub rsp,sizeof_CHAR_INFO64
--              mov rsi,rsp
--              mov rax,[top]
--              mov rcx,[right]
--              sub rax,1
--              mov rdx,[attributes]
--              mov [rdi+SR_Left],bx    -- 0
--              mov [rdi+SR_Top],ax     -- top-1
--              mov [rdi+SR_Right],cx   -- right
----DEV not yet supported..
----                sub rax,[amount]
--              mov rcx,[amount]
--              sub rax,rcx
--              mov rcx,[bottom]
----DEV... (same op)
----                sal rax,16              -- a COORD of {0,top-1-amount}
--              shl rax,16              -- a COORD of {0,top-1-amount}
--              mov [rsi+CI_UnicodeChar],word' '
--              sub rcx,1
--              mov [rsi+CI_Attributes],dx
--              mov [rdi+SR_Bottom],cx  -- bottom-1
--              mov rcx,rsp -- put 2 copies of rsp onto the stack...
--              push rsp
--              push rcx
--              or rsp,8    -- [rsp] is now 1st or 2nd copy:
--                          -- if on entry rsp was xxx8: both copies remain on the stack
--                          -- if on entry rsp was xxx0: or rsp,8 effectively pops one of them (+8)
--                          -- obviously rsp is now xxx8, whichever alignment we started with
--              sub rsp,8*5
--              mov [rsp+32],rsi                            -- lpFill
--              mov r9,rax                                  -- dwDestinationOrigin
--              mov r8,rbx                                  -- lpClipRectangle(NULL)
--              mov rdx,rdi                                 -- lpScrollRectangle
--              mov rcx,[stdout]                            -- hConsoleOutput
--              call "kernel32.dll","ScrollConsoleScreenBufferA"
---             add rsp,8*5
---             pop rsp
--              mov rsp,[rsp+8*5]   -- equivalent to the add/pop
--              add rsp,sizeof_CHAR_INFO+sizeof_SMALL_RECT
--          [ELF64]
--              pop al
--          []
--            }
--  end if
--end procedure

--  :%opTextRows
--global function text_rows(integer newrows)
function ftext_rows(integer newrows)
integer res
    if newrows<1 then ?9/0 end if
    if not cinit then initConsole() end if
    #ilASM{
        [PE32]
            sub esp,sizeof_CSBI
            mov esi,[stdout]
            mov edi,esp
            push edi                                            -- lpConsoleScreenBufferInfo
            push esi                                            -- hConsoleOutput
            call "kernel32.dll","GetConsoleScreenBufferInfo"
--          test eax,eax
--          jz ??? [DEV]
            mov edx,[newrows]
            shl edx,16
            mov dx,[edi+CSBI_SIZEX] -- a COORD of {X,newrows}
            push edx                                            -- dwSize
            push esi                                            -- hConsoleOutput
            call "kernel32.dll","SetConsoleScreenBufferSize"
            -- (ignore error, instead just return how things ended up)
            push edi                                            -- lpConsoleScreenBufferInfo
            push esi                                            -- hConsoleOutput
            call "kernel32.dll","GetConsoleScreenBufferInfo"
--          test eax,eax
--          jz ??? [DEV]
            xor eax,eax
            mov ax,[edi+CSBI_SIZEY]
            add esp,sizeof_CSBI
            mov [res],eax
        [PE64]
            sub rsp,sizeof_CSBI64
            mov rdi,rsp
--          mov rcx,rsp -- put 2 copies of rsp onto the stack...
            push rsp
            push rdi
            or rsp,8    -- [rsp] is now 1st or 2nd copy:
                        -- if on entry rsp was xxx8: both copies remain on the stack
                        -- if on entry rsp was xxx0: or rsp,8 effectively pops one of them (+8)
                        -- obviously rsp is now xxx8, whichever alignment we started with
            sub rsp,8*5
            mov rdx,rdi                                     -- lpConsoleScreenBufferInfo
            mov rcx,[stdout]                                -- hConsoleOutput
            call "kernel32.dll","GetConsoleScreenBufferInfo"
--          test rax,rax
--          jz ??? [DEV]
            mov rdx,[newrows]
            shl rdx,16
            mov dx,[rdi+CSBI_SIZEX]                         -- dwSize (a COORD of {X,newrows})
            mov rcx,[stdout]                                -- hConsoleOutput
            call "kernel32.dll","SetConsoleScreenBufferSize"
            -- (ignore error, instead just return how things ended up)
            mov rdx,rdi                                     -- lpConsoleScreenBufferInfo
            mov rcx,[stdout]                                -- hConsoleOutput
            call "kernel32.dll","GetConsoleScreenBufferInfo"
--          test rax,rax
--          jz ??? [DEV]
            xor rax,rax
            mov ax,[rdi+CSBI_SIZEY]
--          add rsp,8*5
--          pop rsp
            mov rsp,[rsp+8*5]   -- equivalent to the add/pop
            mov [res],rax
            add rsp,sizeof_CSBI64
        [ELF32]
            -- this routine is documented as having no effect on Lnx
            mov [res],ebx
        [ELF64]
            -- this routine is documented as having no effect on Lnx
            mov [res],rbx
          }
    return res
end function


constant BACKGROUNDCOLOR=0, TEXTCOLOR=1

procedure set_console_color(integer color, integer cmode)
    if not cinit then initConsole() end if
    #ilASM{
        [PE32]
            sub esp,sizeof_CSBI
            mov esi,[stdout]
            mov edi,esp
--          push eax                                        -- lpConsoleScreenBufferInfo
            push edi                                        -- lpConsoleScreenBufferInfo
            push esi                                        -- hConsoleOutput
            call "kernel32.dll","GetConsoleScreenBufferInfo"
--          test eax,eax
--          jz ??? [DEV]
            xor eax,eax
            mov ecx,[color]
            mov ax,[edi+CSBI_ATTR]
            and cl,0x0F
--          and ecx,0x0F
            cmp [cmode],0
            je :bk_clr
                and ax,0xF0
--              and eax,0xF0
--              mov al,cl
                jmp @f
          ::bk_clr
                and ax,0x0F
--              and eax,0x0F
                shl cl,4
--              mov ah,cl
          @@:
            or al,cl
            push eax                                        -- wAttributes
            push esi                                        -- hConsoleOutput
            call "kernel32.dll","SetConsoleTextAttribute"
--          test eax,eax
--          jz ??? [DEV]
            add esp,sizeof_CSBI
        [PE64]
            sub rsp,sizeof_CSBI64
            mov rdi,rsp
--          mov rcx,rsp -- put 2 copies of rsp onto the stack...
            push rsp
            push rdi
            or rsp,8    -- [rsp] is now 1st or 2nd copy:
                        -- if on entry rsp was xxx8: both copies remain on the stack
                        -- if on entry rsp was xxx0: or rsp,8 effectively pops one of them (+8)
                        -- obviously rsp is now xxx8, whichever alignment we started with
            sub rsp,8*5
            mov rdx,rdi                                     -- lpConsoleScreenBufferInfo
            mov rcx,[stdout]                                -- hConsoleOutput
            call "kernel32.dll","GetConsoleScreenBufferInfo"
--          test rax,rax
--          jz ??? [DEV]
            xor rax,rax
            mov rcx,[color]
            mov ax,[rdi+CSBI_ATTR]
            and cl,0x0F
            cmp [cmode],0
            je :bk_clr
                and ax,0xF0
--              mov al,cl
                jmp @f
          ::bk_clr
                and ax,0x0F
                shl cl,4
--              mov ah,cl
          @@:
            or al,cl
            mov rdx,rax                                     -- wAttributes
            mov rcx,[stdout]                                -- hConsoleOutput
            call "kernel32.dll","SetConsoleTextAttribute"
--          add rsp,8*5
--          pop rsp
            mov rsp,[rsp+8*5]   -- equivalent to the add/pop
--          test rax,rax
--          jz ??? [DEV]
            add rsp,sizeof_CSBI64
        [ELF32]
            --DEV not attempted...
--          pop al
--object SetTColor(object x)
--#ifdef EUNIX
--      current_fg_color = c & 15;
--      if (current_fg_color > 7) {
--              bold = 1; // BOLD ON (BRIGHT)
--              c = 30 + (current_fg_color & 7);
--      }
--      else {
--              bold = 22; // BOLD OFF
--              c = 30 + current_fg_color;
--      }
--      snprintf(buff, STC_buflen, "\E[%d;%dm", bold, c);
--      iputs(buff, stdout);
--      iflush(stdout);
--#endif
--
--object SetBColor(object x)
--#ifdef EUNIX
--      current_bg_color = c & 15;
--      if (current_bg_color > 7) {
--              c = 100 + (current_bg_color & 7);
--      }
--      else {
--              c = 40 + current_bg_color;
--      }
--      snprintf(buff, SBC_buflen, "\E[%dm", c);
--      iputs(buff, stdout);
--      iflush(stdout);
--#endif
        [ELF64]
--          pop al
        []
          }
    if platform()=LINUX then
        string txt
        color = and_bits(color,#0F)
        if cmode=BACKGROUNDCOLOR then
            if color>7 then
                color = 100+and_bits(color,7)
            else
                color = 40+color
            end if
            txt = sprintf("\E[%dm", {color})
        else -- TEXTCOLOR
            integer bold
            if color>7 then
                bold = 1    -- bold on (bright)
                color = 30+and_bits(color,7)
            else
                bold = 22   -- bold off
                color = 30+color
            end if
            txt = sprintf("\E[%d;%dm", {bold,color})
        end if
        puts(1,txt)
    end if
end procedure

--  opName("opBkClr",opBkClr,2)
--global procedure bk_color(integer color)
--  set_console_color(color, BACKGROUNDCOLOR)
--end procedure

--  opName("opTxtClr",opTxtClr,2)
--global procedure text_color(integer color)
--  set_console_color(color, TEXTCOLOR)
--end procedure

--  opName("opPosition",opPosition,3)
--global procedure position(integer line, integer col)
procedure fposition(integer line, integer col)
    if line<1 or line>=#4000 or col<1 or col>=#4000 then
        fatalN(2, e83atpmbi)
    end if
--  #ilASM{ call :%pClearDbg }
    if platform()=WINDOWS then
        if not cinit then initConsole() end if
        integer coord = and_bits(line-1,#FFFF)*#10000 + and_bits(col-1,#FFFF)
        #ilASM{
            [PE32]
                push dword[coord]                           -- dwCursorPosition
                push [stdout]                               -- hConsoleOutput
                call "kernel32.dll","SetConsoleCursorPosition"
                test eax,eax
                jne @f
                    mov [coord],-1
              @@:
            [PE64]
                mov rcx,rsp -- put 2 copies of rsp onto the stack...
                push rsp
                push rcx
                or rsp,8    -- [rsp] is now 1st or 2nd copy:
                            -- if on entry rsp was xxx8: both copies remain on the stack
                            -- if on entry rsp was xxx0: or rsp,8 effectively pops one of them (+8)
                            -- obviously rsp is now xxx8, whichever alignment we started with
                sub rsp,8*5
                mov rdx,[coord]                                 -- dwCursorPosition
                mov rcx,[stdout]                                -- hConsoleOutput
                call "kernel32.dll","SetConsoleCursorPosition"
--              add rsp,8*5
--              pop rsp
                mov rsp,[rsp+8*5]   -- equivalent to the add/pop
                test rax,rax
                jne @f
                    mov [coord],-1
              @@:
              }
--DEV does not appear to be setting emitON=0 as I expected...
--  if platform()=WINDOWS then
        if coord=-1 then
            #ilASM{
                [PE32,PE64]
                    call "kernel32.dll","GetLastError"
                [32]
                    mov [coord],eax
                [64]
                    mov [coord],rax
                  }
            fatalN(2,e108pe,coord)
        end if
    elsif platform()=LINUX then
        printf(1,"\E[%d;%dH", {line, col})
    end if
end procedure

--  opName("opClrScrn",opClrScrn,1)
--global procedure clear_screen()
procedure fclear_screen()
    if platform()=WINDOWS then
        if not cinit then initConsole() end if
        #ilASM{
            [PE32]
                sub esp,sizeof_CSBI
                mov eax,[stdout]
                mov edi,esp
                push edi            -- lpConsoleScreenBufferInfo
                push eax            -- hConsoleOutput
                call "kernel32.dll","GetConsoleScreenBufferInfo"
--              test eax,eax
--              jz ??? [DEV]
                xor eax,eax
                xor ecx,ecx
                mov ax,[edi+CSBI_SIZEX]
                mov cx,[edi+CSBI_SIZEY]
                xor edx,edx
                imul ecx
                mov dx,[edi+CSBI_ATTR]
                add esp,sizeof_CSBI
                push ebx    -- space for NumberOf(Attrs|Chars)Written
                mov esi,esp
                -- push params for FillConsoleOutputAttribute first (before regs get damaged)
                push esi                                        -- lpNumberOfAttrsWritten
                mov edi,[stdout]
                push ebx                                        -- dwWriteCoord
                push eax                                        -- nLength
                push edx                                        -- wAttribute
                push edi                                        -- hConsoleOutput
                -- now params for FillConsoleOutputCharacter
                push esi                                        -- lpNumberOfCharsWritten
                push ebx                                        -- dwWriteCoord
                push eax                                        -- nLength
                push ' '                                        -- cCharacter
                push edi                                        -- hConsoleOutput
                call "kernel32.dll","FillConsoleOutputCharacterA"
                call "kernel32.dll","FillConsoleOutputAttribute"
--              pop eax     -- discard NumberOf(Attrs|Chars)Written
--              push ebx                                        -- dwCursorPosition
                mov [esp],ebx                                   -- dwCursorPosition
                push [stdout]                                   -- hConsoleOutput
                call "kernel32.dll","SetConsoleCursorPosition"
            [PE64]
                sub rsp,sizeof_CSBI64
                mov rdi,rsp
--              mov rcx,rsp -- put 2 copies of rsp onto the stack...
                push rsp
                push rdi
                or rsp,8    -- [rsp] is now 1st or 2nd copy:
                            -- if on entry rsp was xxx8: both copies remain on the stack
                            -- if on entry rsp was xxx0: or rsp,8 effectively pops one of them (+8)
                            -- obviously rsp is now xxx8, whichever alignment we started with
                sub rsp,8*7
                mov rdx,rdi                                     -- lpConsoleScreenBufferInfo
                mov rcx,[stdout]                                -- hConsoleOutput
                call "kernel32.dll","GetConsoleScreenBufferInfo"
--              test rax,rax
--              jz ??? [DEV]
                xor rax,rax
                xor rcx,rcx
                mov ax,[rdi+CSBI_SIZEX]
                mov cx,[rdi+CSBI_SIZEY]
--              xor edx,edx
                imul rcx
                lea r14,[rsp+40]    -- (as r14 is preserved over api calls)
                mov r12,rax         -- (as r12 is preserved over api calls)
                xor rdx,rdx
                mov [rsp+32],r14                                -- lpNumberOfCharsWritten
                mov r9,rbx                                      -- dwWriteCoord ({0,0})
                mov r8,r12                                      -- nLength
--              mov rdx,' '                                     -- cCharacter
                mov dl,' '                                      -- cCharacter
                mov rcx,[stdout]                                -- hConsoleOutput
                call "kernel32.dll","FillConsoleOutputCharacterA"
                xor rdx,rdx
                mov [rsp+32],r14                                -- lpNumberOfAttrsWritten
                mov r9,rbx                                      -- dwWriteCoord ({0,0})
                mov r8,r12                                      -- nLength
                mov dx,[rdi+CSBI_ATTR]                          -- wAttribute
                mov rcx,[stdout]                                -- hConsoleOutput
                call "kernel32.dll","FillConsoleOutputAttribute"
                mov rdx,rbx                                     -- dwCursorPosition ({0,0})
                mov rcx,[stdout]                                -- hConsoleOutput
                call "kernel32.dll","SetConsoleCursorPosition"
--              add rsp,8*7
--              pop rsp
                mov rsp,[rsp+8*7]   -- equivalent to the add/pop
                add rsp,sizeof_CSBI64
              }
    elsif platform()=LINUX then
        puts(1,"\E[2J") -- clear screen
        fposition(1, 1)
    end if
end procedure

--  opName("opFreeCons",opFreeCons,1)
--global procedure free_console()
procedure ffree_console()
--?{{{{{cinit}}}}}
--  if cinit then
-->     {} = c_func(xFreeConsole,{})
        #ilASM{
            [PE32]
                call "kernel32.dll","FreeConsole"
            [PE64]
                mov rcx,rsp -- put 2 copies of rsp onto the stack...
                push rsp
                push rcx
                or rsp,8    -- [rsp] is now 1st or 2nd copy:
                            -- if on entry rsp was xxx8: both copies remain on the stack
                            -- if on entry rsp was xxx0: or rsp,8 effectively pops one of them (+8)
                            -- obviously rsp is now xxx8, whichever alignment we started with
                sub rsp,8*5
                call "kernel32.dll","FreeConsole"
--              add rsp,8*5
--              pop rsp
                mov rsp,[rsp+8*5]   -- equivalent to the add/pop
            [ELF32]
                --DEV not attempted yet
--              pop al
--  (seems to be just "have_console = FALSE;" and the effects that later has elsewhere)
            [ELF64]
--              pop al
            []
              }
        stdin = 0
        stdout = 0
        stderr = 0
--      free(pBHFI)
--      free(pSMALLRECT)
--      free(pCHARINFO)
        cinit = 0
--  end if
end procedure

#ilASM{ jmp :fin
--/*
procedure :!SetSafeMode(:%)
end procedure -- (for Edita/CtrlQ)
--*/
    :!SetSafeMode
        -- called from processCommandLine()
        [32]
            mov eax,[esp]
            mov [safemode],1 -- (aka true)
            jmp eax
        [64]
            mov rax,[rsp]
            mov [safemode],1
            jmp rax
        []

--/*
procedure :%opOpen(:%)
end procedure -- (for Edita/CtrlQ)
--*/
    :%opOpen
------------
        --
        --  This is the "glue" needed to allow open() to be put in the optable.
        --  Sure, I could rewrite it as pure #ilASM, but it was much easier to
        --  write (and test) as a (global) hll routine; plus writing something 
        --  like x=s[i] in assembler gets real tedious real fast, trust me.
        --
        [32]
            -- calling convention
            --  lea edi,[res]       -- result location
            --  mov eax,[filepath]  -- (opUnassigned)
            --  mov ecx,[openmode]  -- (opUnassigned)
            --  call :%opOpen       -- [edi]:=open(eax,ecx)
            cmp eax,h4
            jl @f
                add dword[ebx+eax*4-8],1
          @@:
            cmp ecx,h4
            jl @f
                add dword[ebx+ecx*4-8],1
          @@:
            push edi                            --[1] addr res
            push ecx                            --[2] openmode
            push eax                            --[3] filepath
            mov edx,routine_id(fopen)           -- mov edx,imm32 (sets K_ridt)
            mov ecx,$_Ltot                      -- mov ecx,imm32 (=symtab[open][S_Ltot])
            call :%opFrame
            mov edx,[esp+12]
            pop dword[ebp]                      --[3] filepath
            pop dword[ebp-4]                    --[2] openmode
--EXCEPT
--X         mov dword[ebp+16],:openret          -- return address
            mov dword[ebp+28],:openret          -- return address
            mov dword[ebp+12],edx               -- called from address
            jmp $_il                            -- jmp code:fopen
          ::openret     
            pop edi                             --[1] addr res
            mov edx,[edi]
            mov [edi],eax
            cmp edx,h4
            jle @f
                sub dword[ebx+edx*4-8],1
                jz :%pDealloc
        [64]
            -- calling convention
            --  lea rdi,[res]       -- result location
            --  mov rax,[filepath]  -- (opUnassigned)
            --  mov rcx,[openmode]  -- (opUnassigned)
            --  call :%opOpen       -- [rdi]:=open(rax,rcx)
            mov r15,h4
            cmp rax,r15
            jl @f
                add qword[rbx+rax*4-16],1
          @@:
            cmp rcx,r15
            jl @f
                add qword[rbx+rcx*4-16],1
          @@:
            push rdi                            --[1] addr res
            push rcx                            --[2] openmode
            push rax                            --[3] filepath
            mov rdx,routine_id(fopen)           -- mov rdx,imm32 (sets K_ridt)
            mov rcx,$_Ltot                      -- mov rcx,imm32 (=symtab[open][S_Ltot])
            call :%opFrame
            mov rdx,[rsp+24]
            pop qword[rbp]                      --[3] filepath
            pop qword[rbp-8]                    --[2] openmode
--EXCEPT
--X         mov qword[rbp+32],:openret          -- return address
            mov qword[rbp+56],:openret          -- return address
            mov qword[rbp+24],rdx               -- called from address
            jmp $_il                            -- jmp code:fopen
          ::openret     
            pop rdi                             --[1] addr res
            mov rdx,[rdi]
            mov [rdi],rax
            cmp rdx,r15
            jle @f
                sub qword[rbx+rdx*4-16],1
                jz :%pDealloc
        []
          @@:
            ret

--/*
procedure :%opFlush(:%)
end procedure -- (for Edita/CtrlQ)
--*/
    :%opFlush
-------------
        --  The "glue" needed to allow flush() to be put in the optable.
        [32]
            -- calling convention
            --  mov eax,[fn]        -- (opUnassigned, should be integer)
            --  call :%opFlush      -- flush(eax)
            cmp eax,h4
            jl @f
                int3    --DEV (better error msg)
          @@:
            push eax                            --[1] fn
            mov edx,routine_id(fflush)          -- mov edx,imm32 (sets K_ridt)
            mov ecx,$_Ltot                      -- mov ecx,imm32 (=symtab[open][S_Ltot])
            call :%opFrame
--          mov edx,[esp+4]
            pop dword[ebp]                      --[1] fn
            pop edx
--EXCEPT
--X         mov dword[ebp+16],:flushret2        -- return address
--          mov dword[ebp+28],:flushret2        -- return address
            mov dword[ebp+28],edx               -- return address
            mov dword[ebp+12],edx               -- called from address
            jmp $_il                            -- jmp code:fflush
        [64]
            -- calling convention
            --  mov rax,[fn]        -- (opUnassigned)
            --  call :%opFlush      -- flush(rax)
            mov r15,h4
            cmp rax,r15
            jl @f
                int3
          @@:
            push rax                            --[1] fn
            mov rdx,routine_id(fflush)          -- mov rdx,imm32 (sets K_ridt)
            mov rcx,$_Ltot                      -- mov rcx,imm32 (=symtab[open][S_Ltot])
            call :%opFrame
--          mov rdx,[rsp+8]
            pop qword[rbp]                      --[1] fn
            pop rdx
--EXCEPT
--X         mov qword[rbp+32],:flushret2        -- return address
--          mov qword[rbp+56],:flushret2        -- return address
            mov qword[rbp+56],rdx               -- return address
            mov qword[rbp+24],rdx               -- called from address
            jmp $_il                            -- jmp code:fflush
        []
--        ::flushret2
            ret

    :!opClosem9
---------------
        [32]
            push dword[esp]
            mov eax,-9
        [64]
            push qword[rsp]
            mov rax,-9
        []
            cmp [finit],0
            jne @f
                ret
          @@:
--/*
procedure :%opClose(:%)
end procedure -- (for Edita/CtrlQ)
--*/
    :%opClose
-------------
        --  The "glue" needed to allow close() to be put in the optable.
        [32]
            -- calling convention
            --  mov eax,[fn]        -- (opUnassigned, integer)
            --  call :%opClose      -- close(eax)
            cmp eax,h4
            jl @f
                int3    --DEV
          @@:
            push eax                            --[1] fn
            mov edx,routine_id(fclose)          -- mov edx,imm32 (sets K_ridt)
            mov ecx,$_Ltot                      -- mov ecx,imm32 (=symtab[open][S_Ltot])
            call :%opFrame
            pop dword[ebp]                      --[1] fn
--EXCEPT
--X         mov dword[ebp+16],:closeret
--          mov dword[ebp+28],:closeret
            pop dword[ebp+28]
            jmp $_il                            -- jmp code:fclose
        [64]
            -- calling convention
            --  mov rax,[fn]        -- (opUnassigned, integer)
            --  call :%opClose      -- close(rax)
            mov r15,h4
            cmp rax,r15
            jl @f
                int3
          @@:
            push rax                            --[1] fn
            mov rdx,routine_id(fclose)      -- mov rdx,imm32 (sets K_ridt)
            mov rcx,$_Ltot                      -- mov rcx,imm32 (=symtab[open][S_Ltot])
            call :%opFrame
            pop qword[rbp]                      --[1] fn
--EXCEPT
--X         mov qword[rbp+32],:closeret
--          mov qword[rbp+56],:closeret
            pop qword[rbp+56]
            jmp $_il                            -- jmp code:fclose
        []
--        ::closeret    
--          ret

--/*
procedure :%opSeek(:%)
end procedure -- (for Edita/CtrlQ)
--*/
    :%opSeek
------------
        --  The "glue" needed to allow seek() to be put in the optable.
        [32]
            -- calling convention
            --  lea edi,[res]       -- result location
            --  mov eax,[fn]        -- file number (opUnassigned, integer)
            --  mov ecx,[pos]       -- position (opUnassigned)
            --  call :%opSeek       -- [edi]:=seek(eax,ecx)

--          (don't bother, will crash soon enough if needed)
            cmp eax,h4
            jl @f
--              add dword[ebx+eax*4-8],1
                int3    --DEV
          @@:
            cmp ecx,h4
            jl @f
                add dword[ebx+ecx*4-8],1
          @@:
            push dword[edi]                     --[1] prev res (popped into edx)
            push edi                            --[2] addr res
            push ecx                            --[3] position
            push eax                            --[4] file number
            mov edx,routine_id(fseek)           -- mov edx,imm32 (sets K_ridt)
            mov ecx,$_Ltot                      -- mov ecx,imm32 (=symtab[open][S_Ltot])
            call :%opFrame
--          mov edx,[esp+24]
            mov edx,[esp+16]                    -- return address (of the call :%opSeek)
            pop dword[ebp]                      --[4] file number
            pop dword[ebp-4]                    --[3] position
--EXCEPT
--X         mov dword[ebp+16],:seekret          -- return address
            mov dword[ebp+28],:seekret          -- return address
            mov dword[ebp+12],edx               -- called from address (for errors)
            jmp $_il                            -- jmp code:fseek
          ::seekret     
            pop edi                             --[2] addr res
            pop edx                             --[1] prev (equiv to mov edx,[edi])
            mov [edi],eax
            cmp edx,h4
            jle @f
                sub dword[ebx+edx*4-8],1
                jz :%pDealloc
        [64]
            -- calling convention
            --  lea rdi,[res]       -- result location
            --  mov rax,[fn]        -- file number (opUnassigned, integer)
            --  mov rcx,[pos]       -- position (opUnassigned)
            --  call :%opSeek       -- [rdi]:=seek(rax,rcx)

            mov r15,h4
--          (don't bother, will crash soon enough if needed)
            cmp rax,r15
            jl @f
--              add qword[rbx+rax*4-16],1
                int3
          @@:
            cmp rcx,r15
            jl @f
                add qword[rbx+rcx*4-16],1
          @@:
            push qword[rdi]                     --[1] prev res (popped into rdx)
            push rdi                            --[2] addr res
            push rcx                            --[3] position
            push rax                            --[4] file number
            mov rdx,routine_id(fseek)           -- mov rdx,imm32 (sets K_ridt)
            mov rcx,$_Ltot                      -- mov rcx,imm32 (=symtab[open][S_Ltot])
            call :%opFrame
            mov rdx,[rsp+32]                    -- return address (of the call :%opSeek)
            pop qword[rbp]                      --[4] file number
            pop qword[rbp-8]                    --[3] position
--EXCEPT
--X         mov qword[rbp+32],:seekret          -- return address
            mov qword[rbp+56],:seekret          -- return address
            mov qword[rbp+24],rdx               -- called from address (for errors)
            jmp $_il                            -- jmp code:fseek
          ::seekret     
            pop rdi                             --[2] addr res
            pop rdx                             --[1] prev (equiv to mov rdx,[rdi])
            mov [rdi],rax
            cmp rdx,r15
            jle @f
                sub qword[rbx+rdx*4-16],1
                jz :%pDealloc
        []
          @@:
            ret

--/*
procedure :%opWhere(:%)
end procedure -- (for Edita/CtrlQ)
--*/
    :%opWhere
------------
        --  The "glue" needed to allow where() to be put in the optable.
        [32]
            -- calling convention
            --  lea edi,[res]       -- result location
            --  mov eax,[fn]        -- file number (opUnassigned, integer)
            --  call :%opWhere      -- [edi]:=where(eax)

--          (don't bother, will crash soon enough if needed)
            cmp eax,h4
            jl @f
--              add dword[ebx+eax*4-8],1
                int3
          @@:
            push dword[edi]                     --[1] prev res (popped into edx)
            push edi                            --[2] addr res
            push eax                            --[3] file number
            mov edx,routine_id(fwhere)          -- mov edx,imm32 (sets K_ridt)
            mov ecx,$_Ltot                      -- mov ecx,imm32 (=symtab[open][S_Ltot])
            call :%opFrame
            mov edx,[esp+12]                    -- return address (of the call :%opWhere)
            pop dword[ebp]                      --[3] file number
--EXCEPT
--X         mov dword[ebp+16],:whereret         -- return address
            mov dword[ebp+28],:whereret         -- return address
            mov dword[ebp+12],edx               -- called from address (for errors)
            jmp $_il                            -- jmp code:fwhere
          ::whereret    
            pop edi                             --[2] addr res
            pop edx                             --[1] prev (equiv to mov edx,[edi])
            mov [edi],eax
            cmp edx,h4
            jle @f
                sub dword[ebx+edx*4-8],1
                jz :%pDealloc
        [64]
            -- calling convention
            --  lea rdi,[res]       -- result location
            --  mov rax,[fn]        -- file number (opUnassigned, integer)
            --  call :%opWhere      -- [rdi]:=where(rax)

            mov r15,h4
--          (don't bother, will crash soon enough if needed)
            cmp rax,r15
            jl @f
--              add qword[rbx+rax*4-16],1
                int3
          @@:
            push qword[rdi]                     --[1] prev res (popped into rdx)
            push rdi                            --[2] addr res
            push rax                            --[3] file number
            mov rdx,routine_id(fwhere)          -- mov rdx,imm32 (sets K_ridt)
            mov rcx,$_Ltot                      -- mov rcx,imm32 (=symtab[open][S_Ltot])
            call :%opFrame
            mov rdx,[rsp+24]                    -- return address (of the call :%opSeek)
            pop qword[rbp]                      --[3] file number
--EXCEPT
--X         mov qword[rbp+32],:whereret         -- return address
            mov qword[rbp+56],:whereret         -- return address
            mov qword[rbp+24],rdx               -- called from address (for errors)
            jmp $_il                            -- jmp code:fwhere
          ::whereret    
            pop rdi                             --[2] addr res
            pop rdx                             --[1] prev (equiv to mov rdx,[rdi])
            mov [rdi],rax
            cmp rdx,r15
            jle @f
                sub qword[rbx+rdx*4-16],1
                jz :%pDealloc
        []
          @@:
            ret

--DEV replaced with :%opLock
--global function lock_file(integer fn, lock_type locktype, byte_range byterange)
--  return flock(fn,locktype,byterange)
--end function

--/*
procedure :%opLock(:%)
end procedure -- (for Edita/CtrlQ)
--*/
    :%opLock
------------
        --  The "glue" needed to allow lock_file() to be put in the optable.
        [32]
            -- calling convention
            --  lea edi,[res]       -- result location
            --  mov eax,[fn]        -- file number (opUnassigned, integer)
            --  mov ecx,[locktype]  -- lock type (opUnassigned, integer)
            --  mov esi,[byterange] -- byte range (opUnassigned, sequence)
            --  call :%opLock       -- [edi]:=lock_file(eax,ecx,esi)
            cmp eax,h4
            jl @f
--              add dword[ebx+eax*4-8],1
                int3
          @@:
            cmp ecx,h4
            jl @f
--              add dword[ebx+ecx*4-8],1
                int3
          @@:
            cmp esi,h4
            jl @f
                add dword[ebx+esi*4-8],1
          @@:
            push dword[edi]                     --[1] prev res (popped into edx)
            push edi                            --[2] addr res
            push eax                            --[3] file number
            push ecx                            --[4] lock type
            push esi                            --[5] byte range
            mov edx,routine_id(flock_file)      -- mov edx,imm32 (sets K_ridt)
            mov ecx,$_Ltot                      -- mov ecx,imm32 (=symtab[open][S_Ltot])
            call :%opFrame
            mov edx,[esp+20]                    -- return address (of the call :%opLock)
            pop dword[ebp-8]                    --[5] byte range
            pop dword[ebp-4]                    --[4] lock type
            pop dword[ebp]                      --[3] file number
--EXCEPT
--X         mov dword[ebp+16],:lockret          -- return address
            mov dword[ebp+28],:lockret          -- return address
            mov dword[ebp+12],edx               -- called from address (for errors)
            jmp $_il                            -- jmp code:flock_file
          ::lockret
            pop edi                             --[2] addr res
            pop edx                             --[1] prev (equiv to mov edx,[edi])
            mov [edi],eax
            cmp edx,h4
            jle @f
                sub dword[ebx+edx*4-8],1
                jz :%pDealloc
        [64]
            -- calling convention
            --  lea rdi,[res]       -- result location
            --  mov rax,[fn]        -- file number (opUnassigned, integer)
            --  mov rcx,[locktype]  -- lock type (opUnassigned, integer)
            --  mov rsi,[byterange] -- byte range (opUnassigned, sequence)
            --  call :%opLock       -- [rdi]:=lock_file(rax,rcx,rsi)
            mov r15,h4
            cmp rax,r15
            jl @f
--              add qword[rbx+rax*4-16],1
                int3
          @@:
            cmp rcx,r15
            jl @f
--              add qword[rbx+rcx*4-16],1
                int3
          @@:
            cmp rsi,r15
            jl @f
                add qword[rbx+rsi*4-16],1
          @@:
            push qword[rdi]                     --[1] prev res (popped into rdx)
            push rdi                            --[2] addr res
            push rax                            --[3] file number
            push rcx                            --[4] lock type
            push rsi                            --[5] byte range
            mov rdx,routine_id(flock_file)      -- mov rdx,imm32 (sets K_ridt)
            mov rcx,$_Ltot                      -- mov rcx,imm32 (=symtab[open][S_Ltot])
            call :%opFrame
            mov rdx,[rsp+40]                    -- return address (of the call :%opLock)
            pop qword[rbp-16]                   --[5] byte range
            pop qword[rbp-8]                    --[4] lock type
            pop qword[rbp]                      --[3] file number
--EXCEPT
--X         mov qword[rbp+32],:lockret          -- return address
            mov qword[rbp+56],:lockret          -- return address
            mov qword[rbp+24],rdx               -- called from address (for errors)
            jmp $_il                            -- jmp code:flock_file
          ::lockret
            pop rdi                             --[2] addr res
            pop rdx                             --[1] prev (equiv to mov rdx,[rdi])
            mov [rdi],rax
            cmp rdx,r15
            jle @f
                sub qword[rbx+rdx*4-16],1
                jz :%pDealloc
        []
          @@:
            ret

--/*
procedure :%opUnLock(:%)
end procedure -- (for Edita/CtrlQ)
--*/
    :%opUnLock
--------------
        --  The "glue" needed to allow unlock_file() to be put in the optable.
        [32]
            -- calling convention
            --  mov eax,[fn]        -- file number (opUnassigned, integer)
            --  mov esi,[byterange] -- byte range (opUnassigned, sequence)
            --  call :%opUnLock     -- unlock_file(eax,esi)
            cmp eax,h4
            jl @f
--              add dword[ebx+eax*4-8],1
                int3
          @@:
            cmp esi,h4
            jl @f
                add dword[ebx+esi*4-8],1
          @@:
            push eax                            --[1] file number
            push esi                            --[2] byte range
            mov edx,routine_id(flock)           -- mov edx,imm32 (sets K_ridt)
            mov ecx,$_Ltot                      -- mov ecx,imm32 (=symtab[open][S_Ltot])
            call :%opFrame
--          mov edx,[esp+8]                     -- return address (of the call :%opUnLock)
            pop dword[ebp-8]                    --[2] byte range
            mov dword[ebp-4],UNLOCK             --    lock type
            pop dword[ebp]                      --[1] file number
            pop edx
--EXCEPT
--X         mov dword[ebp+16],:unlockret        -- return address
--          mov dword[ebp+28],:unlockret        -- return address
            mov dword[ebp+28],edx               -- return address
            mov dword[ebp+12],edx               -- called from address (for errors)
            jmp $_il                            -- jmp code:flock
--        ::unlockret
        [64]
            -- calling convention
            --  mov rax,[fn]        -- file number (opUnassigned, integer)
            --  mov rsi,[byterange] -- byte range (opUnassigned, sequence)
            --  call :%opUnLock     -- [rdi]:=unlock_file(rax,rsi)
            mov r15,h4
            cmp rax,r15
            jl @f
--              add qword[rbx+rax*4-16],1
                int3
          @@:
            cmp rsi,r15
            jl @f
                add qword[rbx+rsi*4-16],1
          @@:
            push rax                            --[1] file number
            push rsi                            --[2] byte range
            mov rdx,routine_id(flock)           -- mov rdx,imm32 (sets K_ridt)
            mov rcx,$_Ltot                      -- mov rcx,imm32 (=symtab[open][S_Ltot])
            call :%opFrame
--          mov rdx,[rsp+16]                    -- return address (of the call :%opLock)
            pop qword[rbp-16]                   --[2] byte range
            mov qword[rbp-8],UNLOCK             --    lock type
            pop qword[rbp]                      --[1] file number
            pop rdx
--EXCEPT
--X         mov qword[rbp+32],:unlockret        -- return address
--          mov qword[rbp+56],:unlockret        -- return address
            mov qword[rbp+56],rdx               -- return address
            mov qword[rbp+24],rdx               -- called from address (for errors)
            jmp $_il                            -- jmp code:flock
--        ::unlockret
        []
--          ret

--global function get_text(integer fn, integer option=GT_WHOLE_FILE)
--function fget_text(integer fn, integer option) (option gets a default of -2 in pmain.e)
--/*
procedure :%opGetText(:%)
end procedure -- (for Edita/CtrlQ)
--*/
    :%opGetText
--------------
        --  The "glue" needed to allow get_textn() to be put in the optable.
        [32]
            -- calling convention
            --  lea edi,[res]       -- result location
            --  mov eax,[fn]        -- file number (opUnassigned, integer)
            --  mov ecx,[option]    -- option (opUnassigned, integer, =-2 in pmain.e)
            --  call :%opGetText    -- [edi]:=get_textn(eax,ecx)
            cmp eax,h4
            jl @f
--              add dword[ebx+eax*4-8],1
                int3
          @@:
            cmp ecx,h4
            jl @f
--              add dword[ebx+ecx*4-8],1
                int3
          @@:
            push dword[edi]                     --[1] prev res (popped into edx)
            push edi                            --[2] addr res
            push eax                            --[3] fn
            push ecx                            --[4] option
            mov edx,routine_id(fget_text)       -- mov edx,imm32 (sets K_ridt)
            mov ecx,$_Ltot                      -- mov ecx,imm32 (=symtab[open][S_Ltot])
            call :%opFrame
            mov edx,[esp+16]                    -- return address (of the call :%opGetText)
            pop dword[ebp-4]                    --[4] option
            pop dword[ebp]                      --[3] fn
--EXCEPT
--X         mov dword[ebp+16],:gtret            -- return address
            mov dword[ebp+28],:gtret            -- return address
            mov dword[ebp+12],edx               -- called from address (for errors)
            jmp $_il                            -- jmp code:fget_text
          ::gtret
            pop edi                             --[2] addr res
            pop edx                             --[1] prev (equiv to mov edx,[edi])
            mov [edi],eax
            cmp edx,h4
            jle @f
                sub dword[ebx+edx*4-8],1
                jz :%pDealloc
        [64]
            -- calling convention
            --  lea rdi,[res]       -- result location
            --  mov rax,[fn]        -- file number (opUnassigned, integer)
            --  mov rcx,[option]    -- option (opUnassigned, integer, =-2 in pmain.e)
            --  call :%opGetText    -- [rdi]:=get_textn(rax,rcx)
            mov r15,h4
            cmp rax,r15
            jl @f
--              add qword[rbx+rax*4-16],1
                int3
          @@:
            cmp rcx,r15
            jl @f
--              add qword[rbx+rcx*4-16],1
                int3
          @@:

            push qword[rdi]                     --[1] prev res (popped into rdx)
            push rdi                            --[2] addr res
            push rax                            --[3] fn
            push rcx                            --[4] option
            mov rdx,routine_id(fget_text)       -- mov rdx,imm32 (sets K_ridt)
            mov rcx,$_Ltot                      -- mov rcx,imm32 (=symtab[open][S_Ltot])
            call :%opFrame
            mov rdx,[rsp+32]                    -- return address (of the call :%opGetText)
            pop qword[rbp-8]                    --[4] option
            pop qword[rbp]                      --[3] fn
--EXCEPT
--X         mov qword[rbp+32],:gtret            -- return address
            mov qword[rbp+56],:gtret            -- return address
            mov qword[rbp+24],rdx               -- called from address (for errors)
            jmp $_il                            -- jmp code:fget_text
          ::gtret
            pop rdi                             --[2] addr res
            pop rdx                             --[1] prev (equiv to mov rdx,[rdi])
            mov [rdi],rax
            cmp rdx,r15
            jle @f
                sub qword[rbx+rdx*4-16],1
                jz :%pDealloc
        []
          @@:
            ret

--/*
procedure :%opGetPos(:%)
end procedure -- (for Edita/CtrlQ)
--*/
    :%opGetPos
--------------
        --  The "glue" needed to allow get_position() to be put in the optable.
        [32]
            -- calling convention
            --  lea edi,[res]       -- result location
            --  call :%opGetPos -- [edi]:=get_position()
            push dword[edi]                     --[1] prev res (popped into edx)
            push edi                            --[2] addr res
            mov edx,routine_id(fget_position)   -- mov edx,imm32 (sets K_ridt)
            mov ecx,$_Ltot                      -- mov ecx,imm32 (=symtab[open][S_Ltot])
            call :%opFrame
            mov edx,[esp+8]                     -- return address (of the call :%opGetPos)
--EXCEPT
--X         mov dword[ebp+16],:getposret        -- return address
            mov dword[ebp+28],:getposret        -- return address
            mov dword[ebp+12],edx               -- called from address (for errors)
            jmp $_il                            -- jmp code:fget_position
          ::getposret
            pop edi                             --[2] addr res
            pop edx                             --[1] prev (equiv to mov edx,[edi])
            mov [edi],eax
            cmp edx,h4
            jle @f
                sub dword[ebx+edx*4-8],1
                jz :%pDealloc
        [64]
            -- calling convention
            --  lea rdi,[res]       -- result location
            --  call :%opGetPos     -- [rdi]:=get_position()
            push qword[rdi]                     --[1] prev res (popped into rdx)
            push rdi                            --[2] addr res
            mov rdx,routine_id(fget_position)   -- mov rdx,imm32 (sets K_ridt)
            mov rcx,$_Ltot                      -- mov rcx,imm32 (=symtab[open][S_Ltot])
            call :%opFrame
            mov rdx,[rsp+16]                    -- return address (of the call :%opGetPos)
--EXCEPT
--X         mov qword[rbp+32],:getposret        -- return address
            mov qword[rbp+56],:getposret        -- return address
            mov qword[rbp+24],rdx               -- called from address (for errors)
            jmp $_il                            -- jmp code:fget_position
          ::getposret
            pop rdi                             --[2] addr res
            pop rdx                             --[1] prev (equiv to mov rdx,[rdi])
            mov [rdi],rax
            cmp rdx,r15
            jle @f
                sub qword[rbx+rdx*4-16],1
                jz :%pDealloc
        []
          @@:
            ret

--/*
procedure :%opWrap(:%)
end procedure -- (for Edita/CtrlQ)
--*/
    :%opWrap
------------
        --  The "glue" needed to allow get_text() to be put in the optable.
        [32]
            -- calling convention
            --  mov eax,[flag]      -- flag (opUnassigned, integer 0|1)
            --  call :%opWrap       -- wrap(eax)
            cmp eax,h4
            jl @f
--              add dword[ebx+eax*4-8],1
                int3
          @@:
            push eax                            --[1] flag
            mov edx,routine_id(fwrap)           -- mov edx,imm32 (sets K_ridt)
            mov ecx,$_Ltot                      -- mov ecx,imm32 (=symtab[open][S_Ltot])
            call :%opFrame
--          mov edx,[esp+4]                     -- return address (of the call :%opWrap)
            pop dword[ebp]                      --[1] flag
            pop edx
--EXCEPT
--X         mov dword[ebp+16],:wrapret          -- return address
--          mov dword[ebp+28],:wrapret          -- return address
            mov dword[ebp+28],edx               -- return address
            mov dword[ebp+12],edx               -- called from address (for errors)
            jmp $_il                            -- jmp code:fwrap
--        ::wrapret
        [64]
            -- calling convention
            --  mov rax,[flag]      -- flag (opUnassigned, integer 0|1)
            --  call :%opWrap       -- wrap(rax)
            mov r15,h4
            cmp rax,r15
            jl @f
--              add qword[rbx+rax*4-16],1
                int3
          @@:
            push rax                            --[1] flag
            mov rdx,routine_id(fwrap)           -- mov rdx,imm32 (sets K_ridt)
            mov rcx,$_Ltot                      -- mov rcx,imm32 (=symtab[open][S_Ltot])
            call :%opFrame
--          mov rdx,[rsp+8]                     -- return address (of the call :%opWrap)
            pop qword[rbp]                      --[1] flag
            pop rdx
--EXCEPT
--X         mov qword[rbp+32],:wrapret          -- return address
--          mov qword[rbp+56],:wrapret          -- return address
            mov qword[rbp+56],rdx               -- return address
            mov qword[rbp+24],rdx               -- called from address (for errors)
            jmp $_il                            -- jmp code:fwrap
--        ::wrapret
        []
--        @@:
--          ret

--/*
procedure :%opTextRows(:%)
end procedure -- (for Edita/CtrlQ)
--*/
    :%opTextRows
--------------
        --  The "glue" needed to allow text_rows() to be put in the optable.
        [32]
            -- calling convention
            --  lea edi,[res]       -- result location
            --  mov eax,[lines]     -- lines (opUnassigned, integer)
            --  call :%opTextRows   -- [edi]:=text_rows(eax)
            cmp eax,h4
            jl @f
--              add dword[ebx+eax*4-8],1
                int3
          @@:
            push dword[edi]                     --[1] prev res (popped into edx)
            push edi                            --[2] addr res
            push eax                            --[3] lines
            mov edx,routine_id(ftext_rows)      -- mov edx,imm32 (sets K_ridt)
            mov ecx,$_Ltot                      -- mov ecx,imm32 (=symtab[open][S_Ltot])
            call :%opFrame
            mov edx,[esp+12]                    -- return address (of the call :%opTextRows)
            pop dword[ebp]                      --[3] lines
--EXCEPT
--X         mov dword[ebp+16],:textrowsret      -- return address
            mov dword[ebp+28],:textrowsret      -- return address
            mov dword[ebp+12],edx               -- called from address (for errors)
            jmp $_il                            -- jmp code:ftext_rows
          ::textrowsret
            pop edi                             --[2] addr res
            pop edx                             --[1] prev (equiv to mov edx,[edi])
            mov [edi],eax
            cmp edx,h4
            jle @f
                sub dword[ebx+edx*4-8],1
                jz :%pDealloc
        [64]
            -- calling convention
            --  lea rdi,[res]       -- result location
            --  mov rax,[lines]     -- lines (opUnassigned, integer)
            --  call :%opTextRows   -- [rdi]:=text_rows(rax)
            mov r15,h4
            cmp rax,r15
            jl @f
--              add qword[rbx+rax*4-16],1
                int3
          @@:
            push qword[rdi]                     --[1] prev res (popped into rdx)
            push rdi                            --[2] addr res
            push rax                            --[3] lines
            mov rdx,routine_id(ftext_rows)      -- mov rdx,imm32 (sets K_ridt)
            mov rcx,$_Ltot                      -- mov rcx,imm32 (=symtab[open][S_Ltot])
            call :%opFrame
            mov rdx,[rsp+24]                    -- return address (of the call :%opTextRows)
            pop qword[rbp]                      --[3] lines
--EXCEPT
--X         mov qword[rbp+32],:textrowsret      -- return address
            mov qword[rbp+56],:textrowsret      -- return address
            mov qword[rbp+24],rdx               -- called from address (for errors)
            jmp $_il                            -- jmp code:ftext_rows
          ::textrowsret
            pop rdi                             --[2] addr res
            pop rdx                             --[1] prev (equiv to mov rdx,[rdi])
            mov [rdi],rax
            cmp rdx,r15
            jle @f
                sub qword[rbx+rdx*4-16],1
                jz :%pDealloc
        []
          @@:
            ret

--/*
procedure :%opBkClr(:%)
end procedure -- (for Edita/CtrlQ)
--*/
    :%opBkClr
------------
        --  The "glue" needed to allow bk_color() to be put in the optable.
        [32]
            -- calling convention
            --  mov eax,[color]     -- color (opUnassigned, integer)
            --  call :%opBkClr      -- bk_color(eax)
            cmp eax,h4
            jl @f
--              add dword[ebx+eax*4-8],1
                int3
          @@:
            push eax                                --[1] color
            mov edx,routine_id(set_console_color)   -- mov edx,imm32 (sets K_ridt)
            mov ecx,$_Ltot                          -- mov ecx,imm32 (=symtab[open][S_Ltot])
            call :%opFrame
--          mov edx,[esp+4]                         -- return address (of the call :%opBkClr)
            mov dword[ebp-4],BACKGROUNDCOLOR
            pop dword[ebp]                          --[1] color
            pop edx
--EXCEPT
--X         mov dword[ebp+16],:bkclrret             -- return address
--          mov dword[ebp+28],:bkclrret             -- return address
            mov dword[ebp+28],edx                   -- return address
            mov dword[ebp+12],edx                   -- called from address (for errors)
            jmp $_il                                -- jmp code:set_console_color
--        ::bkclrret
        [64]
            -- calling convention
            --  mov rax,[color]     -- color (opUnassigned, integer)
            --  call :%opBkClr      -- bk_color(rax)
            mov r15,h4
            cmp rax,r15
            jl @f
--              add qword[rbx+rax*4-16],1
                int3
          @@:
            push rax                                --[1] color
            mov rdx,routine_id(set_console_color)   -- mov rdx,imm32 (sets K_ridt)
            mov rcx,$_Ltot                          -- mov rcx,imm32 (=symtab[open][S_Ltot])
            call :%opFrame
--          mov rdx,[rsp+8]                         -- return address (of the call :%opBkClr)
            mov qword[rbp-8],BACKGROUNDCOLOR
            pop qword[rbp]                          --[1] color
            pop rdx
--EXCEPT
--X         mov qword[rbp+32],:bkclrret             -- return address
--          mov qword[rbp+56],:bkclrret             -- return address
            mov qword[rbp+56],rdx                   -- return address
            mov qword[rbp+24],rdx                   -- called from address (for errors)
            jmp $_il                                -- jmp code:set_console_color
--        ::bkclrret
        []
--        @@:
--          ret

--/*
procedure :%opTxtClr(:%)
end procedure -- (for Edita/CtrlQ)
--*/
    :%opTxtClr
------------
        --  The "glue" needed to allow text_color() to be put in the optable.
        [32]
            -- calling convention
            --  mov eax,[color]     -- color (opUnassigned, integer)
            --  call :%opTxtClr     -- text_color(eax)
            cmp eax,h4
            jl @f
--              add dword[ebx+eax*4-8],1
                int3
          @@:
            push eax                                --[1] color
            mov edx,routine_id(set_console_color)   -- mov edx,imm32 (sets K_ridt)
            mov ecx,$_Ltot                          -- mov ecx,imm32 (=symtab[open][S_Ltot])
            call :%opFrame
--          mov edx,[esp+4]                         -- return address (of the call :%opTxtClr)
            mov dword[ebp-4],TEXTCOLOR
            pop dword[ebp]                          --[1] color
            pop edx
--EXCEPT
--X         mov dword[ebp+16],:txclrret             -- return address
--          mov dword[ebp+28],:txclrret             -- return address
            mov dword[ebp+28],edx                   -- return address
            mov dword[ebp+12],edx                   -- called from address (for errors)
            jmp $_il                                -- jmp code:set_console_color
--        ::txclrret
        [64]
            -- calling convention
            --  mov rax,[color]     -- color (opUnassigned, integer)
            --  call :%opTxtClr     -- text_color(rax)
            mov r15,h4
            cmp rax,r15
            jl @f
--              add qword[rbx+rax*4-16],1
                int3
          @@:
            push rax                                --[1] color
            mov rdx,routine_id(set_console_color)   -- mov rdx,imm32 (sets K_ridt)
            mov rcx,$_Ltot                          -- mov rcx,imm32 (=symtab[open][S_Ltot])
            call :%opFrame
--          mov rdx,[rsp+8]                         -- return address (of the call :%opTxtClr)
            mov qword[rbp-8],TEXTCOLOR
            pop qword[rbp]                          --[1] color
            pop rdx
--EXCEPT
--X         mov qword[rbp+32],:txclrret             -- return address
--          mov qword[rbp+56],:txclrret             -- return address
            mov qword[rbp+56],rdx                   -- return address
            mov qword[rbp+24],rdx                   -- called from address (for errors)
            jmp $_il                                -- jmp code:set_console_color
--        ::txclrret
        []
--        @@:
--          ret

--/*
procedure :%opClrScrn(:%)
end procedure -- (for Edita/CtrlQ)
--*/
    :%opClrScrn
---------------
        --  The "glue" needed to allow clear_screen() to be put in the optable.
        [32]
            -- calling convention
            --  call :%opClrScrn -- clear_screen()
            mov edx,routine_id(fclear_screen)   -- mov edx,imm32 (sets K_ridt)
            mov ecx,$_Ltot                      -- mov ecx,imm32 (=symtab[open][S_Ltot])
            call :%opFrame
--          mov edx,[esp]                       -- return address (of the call :%opClrScrn)
            pop edx
--EXCEPT
--X         mov dword[ebp+16],:clsret           -- return address
--          mov dword[ebp+28],:clsret           -- return address
            mov dword[ebp+28],edx               -- return address
            mov dword[ebp+12],edx               -- called from address (for errors)
            jmp $_il                            -- jmp code:fclear_screen
--        ::clsret
        [64]
            -- calling convention
            --  call :%opClrScrn -- clear_screen()
            mov rdx,routine_id(fclear_screen)   -- mov rdx,imm32 (sets K_ridt)
            mov rcx,$_Ltot                      -- mov rcx,imm32 (=symtab[open][S_Ltot])
            call :%opFrame
--          mov rdx,[rsp]                       -- return address (of the call :%opClrScrn)
            pop rdx
--EXCEPT
--X         mov qword[rbp+32],:clsret           -- return address
--          mov qword[rbp+56],:clsret           -- return address
            mov qword[rbp+56],rdx               -- return address
            mov qword[rbp+24],rdx               -- called from address (for errors)
            jmp $_il                            -- jmp code:fclear_screen
--        ::clsret
        []
--        @@:
--          ret

--/*
procedure :%opFreeCons(:%)
end procedure -- (for Edita/CtrlQ)
--*/
    :%opFreeCons
----------------
        --  The "glue" needed to allow get_text() to be put in the optable.
        [32]
            -- calling convention
            --  call :%opFreeCons -- free_console()
            mov edx,routine_id(ffree_console)   -- mov edx,imm32 (sets K_ridt)
            mov ecx,$_Ltot                      -- mov ecx,imm32 (=symtab[open][S_Ltot])
            call :%opFrame
--          mov edx,[esp]                       -- return address (of the call :%opFreeCons)
            pop edx
--EXCEPT
--X         mov dword[ebp+16],:fcret            -- return address
--          mov dword[ebp+28],:fcret            -- return address
            mov dword[ebp+28],edx               -- return address
            mov dword[ebp+12],edx               -- called from address (for errors)
            jmp $_il                            -- jmp code:ffree_console
--        ::fcret
        [64]
            -- calling convention
            --  call :%opFreeCons -- free_console()
            mov rdx,routine_id(ffree_console)   -- mov rdx,imm32 (sets K_ridt)
            mov rcx,$_Ltot                      -- mov rcx,imm32 (=symtab[open][S_Ltot])
            call :%opFrame
--          mov rdx,[rsp]                       -- return address (of the call :%opFreeCons)
            pop rdx
--EXCEPT
--X         mov qword[rbp+32],:fcret            -- return address
--          mov qword[rbp+56],:fcret            -- return address
            mov qword[rbp+56],rdx               -- return address
            mov qword[rbp+24],rdx               -- called from address (for errors)
            jmp $_il                            -- jmp code:ffree_console
--        ::fcret
        []
--        @@:
--          ret

--/*
procedure :%opPosition(:%)
end procedure -- (for Edita/CtrlQ)
--*/
    :%opPosition
------------
        --  The "glue" needed to allow position() to be put in the optable.
        [32]
            -- calling convention
            --  mov eax,[line]      -- line (opUnassigned, integer)
            --  mov ecx,[col]       -- col (opUnassigned, integer)
            --  call :%opPosition   -- position(eax,ecx)
            cmp eax,h4
            jl @f
--              add dword[ebx+eax*4-8],1
                int3
          @@:
            cmp ecx,h4
            jl @f
--              add dword[ebx+ecx*4-8],1
                int3
          @@:
            push eax                            --[1] line
            push ecx                            --[2] col
            mov edx,routine_id(fposition)       -- mov edx,imm32 (sets K_ridt)
            mov ecx,$_Ltot                      -- mov ecx,imm32 (=symtab[open][S_Ltot])
            call :%opFrame
--          mov edx,[esp+8]                     -- return address (of the call :%opPosition)
            pop dword[ebp-4]                    --[2] col
            pop dword[ebp]                      --[1] line
            pop edx
--EXCEPT
--X         mov dword[ebp+16],:posnret          -- return address
--          mov dword[ebp+28],:posnret          -- return address
            mov dword[ebp+28],edx               -- return address
            mov dword[ebp+12],edx               -- called from address (for errors)
            jmp $_il                            -- jmp code:fposition
--        ::posnret
        [64]
            -- calling convention
            --  mov rax,[line]      -- line (opUnassigned, integer)
            --  mov rcx,[col]       -- col (opUnassigned, integer)
            --  call :%opPosition   -- position(rax,rcx)
            mov r15,h4
            cmp rax,r15
            jl @f
--              add qword[rbx+rax*4-16],1
                int3
          @@:
            cmp rcx,r15
            jl @f
--              add dword[rbx+rcx*4-16],1
                int3
          @@:
            push rax                            --[1] line
            push rcx                            --[2] col
            mov rdx,routine_id(fposition)       -- mov rdx,imm32 (sets K_ridt)
            mov rcx,$_Ltot                      -- mov rcx,imm32 (=symtab[open][S_Ltot])
            call :%opFrame
--          mov rdx,[rsp+16]                    -- return address (of the call :%opPosition)
            pop qword[rbp-8]                    --[2] col
            pop qword[rbp]                      --[1] line
            pop rdx
--EXCEPT
--X         mov qword[rbp+32],:posnret          -- return address
--          mov qword[rbp+56],:posnret          -- return address
            mov qword[rbp+56],rdx               -- return address
            mov qword[rbp+24],rdx               -- called from address (for errors)
            jmp $_il                            -- jmp code:fposition
--        ::posnret
        []
--        @@:
--          ret

          ::fin
      }

