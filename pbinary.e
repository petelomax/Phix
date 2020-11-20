--
-- pbinary.e
-- =========
--
constant datab4code = 01 --DEV (could/should be a format option?)

--  Implements the messy details of actually creating 32/64-bit PE/ELF executable files.
--  Executable files contain mountains of obscure and often historical/obsolete details. This
--  program implements all that technical complexity in the simplest and clearest way I know,
--  and I am more than reasonably confident you will fail to find anything simpler than this.
--  In any case, it certainly helps to sequester this lot away from the rest of the system.
--  The demo program filedump.exw can and should be used to diagnose problems with the output,
--  and I strongly recommend you to familiarise yourself with the operation of that before you
--  even think about reading any of the code below.
--
--  Interface: see CreateExecutable() below.
--
--  DLL files could also (theoretically) be created, but I have not finalised the export
--  settings required (DLLMain, etc?), and in fact it might not need any changes to the 
--  compiler at all, just a set of rules of the form "you must have this, that, etc".
--
-- work in progress:
-- MZ (done)
-- PE/RVA (done)
-- sections (done)
-- imports (done)
-- exports (done, triggered false positive)
-- relocations (done, cleared that FP)
-- resources (done)
-- data (sort of)
-- code (sort of)
-- thunk references need to be a linked list in code.... (done, ish)
--
-- Cross platform operation:
--  On Windows, you can create a windows PE format executable or a Linux ELF format binary
--  simply by changing(/inserting) a format directive in the program source. However, on
--  Linux, unless you run the Windows version of Phix on Wine, PE format executables may
--  have incorrect timestamps and checksums, because various system routines (GetSystemTime
--  and SystemTimeToFileTime in kernel32.dll, and CheckSumMappedFile in imagehlp.dll) are
--  not available. They may work just fine, they may trigger false positives in your virus
--  scanner, and given both the freely available workaround, plus the need to perform such
--  virus scanning, and in most cases build a Windows Installer, I do not plan on expending
--  any further effort trying to improve upon that situation.
-- TRY2:
-- Note about cross platform operation:
--  While the Windows versions of Phix can create both PE and ELF binaries (and running said 
--  on Wine is the suggested workaround to this), the native Linux versions will struggle to
--  set correct timestamps and checksums if asked to create PE binaries, since that is done
--  using routines in kernel32.dll and imagehlp.dll. Otherwise you should be able to use any
--  version to create any other version, eg a 32bit PE p.exe can create 64bit ELF binaries.
--

constant M32 = 32,
         M64 = 64

constant BYTE = 1,
         WORD = 2*BYTE,     -- (suppresses unused warning)
         DWORD = 4, 
         QWORD = 8

function stringify(sequence s)
string res
integer ch
    if string(s) then
        res = s
    else
        res = repeat(' ',length(s))
        for i=1 to length(s) do
            ch = s[i]
            res[i] = ch
        end for
    end if
    return res
end function

--DEV:
--function SetField(string res, integer offset, integer dsize, atom v)
function SetField(sequence res, integer offset, integer dsize, atom v)
--
-- Breakup v into dsize bytes (little endian) in res at offset.
-- Note that offset as passed is 0-based, adjusted here(+1) to index res.
-- dsize is BYTE(1), WORD(2), DWORD(4), or QWORD(8)
-- This routine, btw, does not distinguish between -1 and +4294967295 in v.
-- Also note this relies on PBR semantics for performance, ie res=SetField(res,..),
-- where res is a local sequence, as opposed to a file-level or global sequence.
--
--DEV fixme
--!/**/ #isginfo{res,0b1000,MIN,MAX,integer,-2} -- verify this is a string
    for i=1 to dsize do
        res[offset+i] = and_bits(v,#FF)
        v = floor(v/#100)
    end for
    return res
end function

constant mul = {#1,
                #100,
                #10000,
                #1000000,
                #100000000,
                #10000000000,
                #1000000000000,
                #100000000000000}

--DEV
--function GetField(string res, integer offset, integer dsize)
function GetField(sequence res, integer offset, integer dsize)
--
-- Reassemble dsize bytes in res at offset to an unsigned int/atom result.
-- Note that offset as passed is 0-based, adjusted here(+1) to index res.
-- dsize is BYTE(1), WORD(2), DWORD(4), or QWORD(8)
--
atom v = 0
    for i=1 to dsize do
        v += res[offset+i]*mul[i]
    end for
    return v
end function

--DEV integer userip, rather then integer machine?
--function fixup(string res, sequence relocations, integer machine, integer userip, integer Base)
function fixup(sequence res, sequence relocations, integer machine, integer userip, integer Base, integer traceit=0) --DEV
--
-- res can be code or data (or possibly something else)
-- relocations contains a list of addresses of dwords (even on M64) that contain
--  (just) an offset from the start of the target section; now that we know the
--  virtual address where that section is going to be, update them appropriately.
--  Note that all entries in relocations (as passed here) refer to the same target 
--  section (as well, of course, as all being somewhere in res).
-- machine should be M32 or M64
-- rip should be 0 for all M32 use and 1 for M64/code. [DEV?]
-- Note that for M32, Base is the virtual address (start) of whatever section the
--  relocations target (which can be the same section we are currently patching), 
--  whereas for M64 it is either (data-code) or (code-data), depending on whether 
--  we are relocating data references in the code section or vice versa (and it
--  makes no sense to relocate references to the same section with RIP). [DEV or 0?]
-- Also note this routine is used for both PE and ELF formats.
--
integer addr
atom v, vhi
integer lres = length(res)  -- temp/debug
--DEV fixme
--!/**/ #isginfo{res,0b1000,MIN,MAX,integer,-2} -- verify this is a string
    for i=1 to length(relocations) do
        addr = relocations[i]
        v = GetField(res,addr,DWORD)
        if machine=M64 then
--DEV 5/12/14... (maybe we only need this for data section?) [DONE, 6/12/14, and undone immediately!]
            vhi = GetField(res,addr+4,DWORD)
--          vhi = 0
if traceit then
--  printf(1,"fixup %08x: %08x, %08x [%d, %d, %d]\n",{addr,v,vhi,vhi=#80000000,vhi,#80000000})
end if
        end if
--      if machine=M32 then
        if userip=0 then
            v += Base
        else -- M64, RIP addressing
--      if userip then
            v += Base-(addr+4)
--      else
--          v += Base
        end if
--DEV not DLL... (must be done after loading)
        if machine=M32 then
            if and_bits(v,#80000000) then
                v = floor(v/4)+#20000000
            end if
        else -- machine=64
            -- (assumes <=4GB executables)
            -- DEV not convinced this will be right for -ve offsets, btw
--DEV
--          if vhi=#80000000 then
            if vhi/#100=#800000 then
--              res = SetField(res,addr+4,DWORD,#40000000)
                res = SetField(res,addr+4,DWORD,vhi/2)
                v = floor(v/4)
--DEV (3/1/16) try: (no help)
--          elsif and_bits(vhi,#80000000) then
--              ?9/0
--          elsif and_bits(v,#80000000) then
--              res = SetField(res,addr+4,DWORD,#40000000)
--              v = floor(v/4)+#20000000
            end if
        end if
        res = SetField(res,addr,DWORD,v)
    end for
    return res
end function

--DEV/erm
--/*
    for i=1 to length(relocations) do
        addr = relocations[i]
        v = GetField(res,addr,DWORD)
        if machine=M32 then
            v += Base
            if and_bits(v,#80000000) then
                v = floor(v/4)+#20000000
            end if
        else -- M64, RIP addressing
--      if userip then
            if and_bits(v,#80000000) then
                if v!=#80000000 then ?9/0 end if
--              v = floor(v/4)+#20000000
                v = #40000000
                res = SetField(res,addr,DWORD,v)
            else
                if v!=0 then ?9/0 end if
            end if
            addr += 4
            v = GetField(res,addr,DWORD)
            v += Base-(addr+4)
--      else
--          v += Base
        end if
--DEV not DLL... (must be done after loading)
        res = SetField(res,addr,DWORD,v)
    end for
--*/

-- PE routines
-- ===========

constant GUI = 2,
         CUI = 3

--DEV not linux friendly...
include builtins\timestamp.ew   -- SYSTEMTIMEtoDateTimeStamp

atom TimeDateStamp

constant FileAlignment = #200,
         SectionAlignment = #1000

--function RoundToDwordAlignment(integer v)
--  return floor((v+3)/4)*4
--end function

function RoundToFileAlignment(integer v)
    return floor((v+FileAlignment-1)/FileAlignment)*FileAlignment
end function

function RoundToSectionAlignment(integer v)
    return floor((v+SectionAlignment-1)/SectionAlignment)*SectionAlignment
end function

function pad(string s, integer wordsize)
integer padding = and_bits(length(s),wordsize-1)
    if padding then
        s &= stringify(repeat('\0',wordsize-padding))
    end if
    return s
end function

constant
          IMAGE_FILE_RELOCS_STRIPPED         = 0x0001,
          IMAGE_FILE_EXECUTABLE_IMAGE        = 0x0002,
--        IMAGE_FILE_LINE_NUMS_STRIPPED      = 0x0004,      -- (deprecated)
--        IMAGE_FILE_LOCAL_SYMS_STRIPPED     = 0x0008,      -- (deprecated)
--        IMAGE_FILE_AGGRESIVE_WS_TRIM       = 0x0010,      -- (obsolete)
          IMAGE_FILE_LARGE_ADDRESS_AWARE     = 0x0020,
--                                             0x0040,      -- (reserved for future use)
--        IMAGE_FILE_BYTES_REVERSED_LO       = 0x0080,      -- (deprecated)
          IMAGE_FILE_32BIT_MACHINE           = 0x0100,
          IMAGE_FILE_DEBUG_STRIPPED          = 0x0200,
--        IMAGE_FILE_REMOVABLE_RUN_FROM_SWAP = 0x0400,
--        IMAGE_FILE_NET_RUN_FROM_SWAP       = 0x0800,
--        IMAGE_FILE_SYSTEM                  = 0x1000,
          IMAGE_FILE_DLL                     = 0x2000,
--        IMAGE_FILE_UP_SYSTEM_ONLY          = 0x4000,
--        IMAGE_FILE_BYTES_REVERSED_HI       = 0x8000,      -- (deprecated)
$

constant -- (excludes OBJ-only settings)
--        IMAGE_SCN_SCALE_INDEX             = 0x00000001,
          IMAGE_SCN_CNT_CODE                = 0x00000020,   -- Section contains code.
          IMAGE_SCN_CNT_INITIALIZED_DATA    = 0x00000040,   -- Section contains initialized data.
--        IMAGE_SCN_CNT_UNINITIALIZED_DATA  = 0x00000080,   -- Section contains uninitialized data.
--        IMAGE_SCN_LNK_OTHER               = 0x00000100,
--        IMAGE_SCN_NO_DEFER_SPEC_EXC       = 0x00004000,
--        IMAGE_SCN_GPREL                   = 0x00008000,
--        IMAGE_SCN_MEM_FARDATA             = 0x00008000,
--        IMAGE_SCN_MEM_LOCKED              = 0x00040000,
--        IMAGE_SCN_MEM_PRELOAD             = 0x00080000,
--        IMAGE_SCN_LNK_NRELOC_OVFL         = 0x01000000,
          IMAGE_SCN_MEM_DISCARDABLE         = 0x02000000,
--        IMAGE_SCN_MEM_NOT_CACHED          = 0x04000000,
--        IMAGE_SCN_MEM_NOT_PAGED           = 0x08000000,
--        IMAGE_SCN_MEM_SHARED              = 0x10000000,
          IMAGE_SCN_MEM_EXECUTE             = 0x20000000,
          IMAGE_SCN_MEM_READ                = 0x40000000,
          IMAGE_SCN_MEM_WRITE               = 0x80000000,
$

constant 
--                                                            0x0001,   -- ? process attachment notify?     \
--                                                            0x0002,   -- ? thread detachment notify?       } Reserved, must be zero
--                                                            0x0004,   -- ? thread attachment notify?      /
--                                                            0x0008,   -- ? process detachment notify?    /
          IMAGE_DLLCHARACTERISTICS_DYNAMIC_BASE             = 0x0040,
--        IMAGE_DLLCHARACTERISTICS_FORCE_INTEGRITY          = 0x0080,
--        IMAGE_DLLCHARACTERISTICS_NX_COMPAT                = 0x0100,
--        IMAGE_DLLCHARACTERISTICS_NO_ISOLATION             = 0x0200,
--        IMAGE_DLLCHARACTERISTICS_NO_SEH                   = 0x0400,
--        IMAGE_DLLCHARACTERISTICS_NO_BIND                  = 0x0800,
--        IMAGE_DLLCHARACTERISTICS_WDM_DRIVER               = 0x2000,
--        IMAGE_DLLCHARACTERISTICS_TERMINAL_SERVER_AWARE    = 0x8000,
$

sequence sectionNames = {},
         sectionCharacteristics = {}

procedure DefineSectionCharacteristics(string name, atom flags)
    sectionNames = append(sectionNames,name)
    sectionCharacteristics = append(sectionCharacteristics,flags)
end procedure

--  DefineSectionCharacteristics(".idata",IMAGE_SCN_MEM_READ+IMAGE_SCN_CNT_INITIALIZED_DATA) -- +write?
    DefineSectionCharacteristics(".idata",IMAGE_SCN_MEM_READ+IMAGE_SCN_MEM_WRITE+IMAGE_SCN_CNT_INITIALIZED_DATA)
    DefineSectionCharacteristics(".edata",IMAGE_SCN_MEM_READ+IMAGE_SCN_CNT_INITIALIZED_DATA)
    DefineSectionCharacteristics(".reloc",IMAGE_SCN_MEM_READ+IMAGE_SCN_CNT_INITIALIZED_DATA+IMAGE_SCN_MEM_DISCARDABLE)
    DefineSectionCharacteristics(".pdata",IMAGE_SCN_MEM_READ+IMAGE_SCN_CNT_INITIALIZED_DATA)
    DefineSectionCharacteristics(".data",IMAGE_SCN_MEM_READ+IMAGE_SCN_MEM_WRITE+IMAGE_SCN_CNT_INITIALIZED_DATA)
--DEV lets have .code rather than .text:
    DefineSectionCharacteristics(".text",IMAGE_SCN_CNT_CODE+IMAGE_SCN_MEM_EXECUTE+IMAGE_SCN_MEM_READ)
--  DefineSectionCharacteristics(".code",IMAGE_SCN_CNT_CODE+IMAGE_SCN_MEM_EXECUTE+IMAGE_SCN_MEM_READ)
    DefineSectionCharacteristics(".rsrc",IMAGE_SCN_MEM_READ+IMAGE_SCN_CNT_INITIALIZED_DATA)


function mzHeader()
-- (same for 32 and 64 bit PE executables)
sequence res
--string res

--puts(1,"warning: e_magic of ZZ line 312, pbinary.e\n")

    res = stringify(    --  offset    name     size value       desc
          {'M','Z',     -- 00000000,e_magic,    x2, MZ,     signature (should be "MZ")
--        {'Z','Z',     -- 00000000,e_magic,    x2, MZ,     signature (should be "MZ")
           #80,#00,     -- 00000002,e_cblp,     h2, 0080h,  Bytes in last block of file
           1,0,         -- 00000004,e_cp,       2,  1,      Blocks in file
           0,0,         -- 00000006,e_crlc,     2,  0,      Relocations
           4,0,         -- 00000008,e_cparhdr,  2,  4,      Size of header in paragraphs
           #10,#00,     -- 0000000A,e_minalloc, h2, 0010h,  Minimum extra paragraphs needed
           #FF,#FF,     -- 0000000C,e_maxalloc, h2, FFFFh,  Maximum extra paragraphs needed
           0,0,         -- 0000000E,e_ss,       h2, 0000h,  Initial (relative) SS value [ignore]
           #40,#01,     -- 00000010,e_sp,       h2, 0140h,  Initial SP value [ignore]
           0,0,         -- 00000012,e_csum,     h2, 0000h,  Checksum
           0,0,         -- 00000014,e_ip,       h2, 0000h,  Initial IP value [ignore]
           0,0,         -- 00000016,e_cs,       h2, 0000h,  Initial (relative) CS value [ignore]
           #40,#00,     -- 00000018,e_lfarlc,   h2, 0040h,  File address of relocation table
           0,0,         -- 0000001A,e_ovno,     h2, 0000h,  Overlay number
           0,0,0,0,     -- 0000001C,e_res,      h8, 0h,     Reserved words
           0,0,0,0,
           0,0,         -- 00000024,e_oemid,    h2, 0000h,  OEM identifier (for e_oeminfo)
           0,0,         -- 00000026,e_oeminfo,  h2, 0000h,  OEM information; e_oemid specific
           0,0,0,0,     -- 00000028,e_res2,     h20,0h,     Reserved words
           0,0,0,0,
           0,0,0,0,
           0,0,0,0,
           0,0,0,0,
           #80,0,0,0,   -- 0000003C,e_lfanew,   h4, 00000080h,File address of new exe header
           #0E,         -- 00000040,push cs,h1,0Eh,
           #1F,         -- 00000041,pop ds,h1,1Fh,(ie data segment:=code segment)
           #BA,#0E,#00, -- 00000042,mov dx,0x000E,h3,BA0E00h,(ie ds:dx is text string below)
           #B4,#09,     -- 00000045,mov ah,0x09,h2,B409h,(write to stdout)
           #CD,#21,     -- 00000047,int 0x21,h2,CD21h,
           #B8,#01,#4C, -- 00000049,mov ax,0x4C01,h3,B8014Ch,(exit)
           #CD,#21,     -- 0000004C,int 0x21,h2,CD21h,
           'T','h','i','s',' ','p','r','o','g','r','a','m',' ','c','a','n','n','o','t',' ',
           'b','e',' ','r','u','n',' ','i','n',' ','D','O','S',' ','m','o','d','e','.',#0D,#0A,'$',
           0,0,0,0,
           0,0,0,0})
    return res
end function

sequence inames
function by_importname(integer i, integer j)
    return compare(inames[i],inames[j])
end function

sequence enames
function by_exportname(integer i, integer j)
    return compare(enames[i],enames[j])
end function

constant HighLow = #3000

integer actual_reloc_size = 0

function relocate(sequence relocations, sequence ridx, sequence Bases)
integer Base
integer first
integer n
integer PageRVA
integer BlockSize
string block
integer bidx
integer TypeOffset
string res = ""
integer lastRVA = 0

    if Bases[1]>Bases[2] then
        relocations = reverse(relocations)
        ridx = reverse(ridx)
        Bases = reverse(Bases)
--  elsif length(relocations)=3 then
--      relocations = {relocations[3],relocations[1],relocations[2]}
--      ridx = {ridx[3],ridx[1],ridx[2]}
--      Bases = {Bases[3],Bases[1],Bases[2]}
    end if

    for i=1 to length(ridx) do
        Base = Bases[i]
        for j=1 to length(ridx[i]) do
            {first,n,PageRVA} = ridx[i][j]
            PageRVA = PageRVA*#1000+Base
--          if PageRVA<=lastRVA then ?9/0 end if
            if PageRVA<=lastRVA then ?"pbinary.e: 9/0 line 420\n" end if
            lastRVA = PageRVA
            BlockSize = 8+n*2
            actual_reloc_size += BlockSize
--?{actual_reloc_size,n,BlockSize}
            block = repeat(' ',BlockSize)
            if and_bits(n,1) then
                BlockSize += 2
            end if
            block = SetField(block,#0,DWORD,PageRVA)
            block = SetField(block,#4,DWORD,BlockSize)
            bidx = 8
            for k=first to first+n-1 do
                integer rik = and_bits(relocations[i][k],#0FFF)
--              if rik<0 or rik>#0FFF then ?9/0 end if
                TypeOffset = rik+HighLow
                block = SetField(block,bidx,WORD,TypeOffset)
                bidx += 2
            end for
            res &= block
            if and_bits(n,1) then
                res &= repeat('\0',2)
                actual_reloc_size += 2
            end if
        end for
    end for
    return res
end function

--DEV (for listing)
--25/4/16 (pHeap using mmap)
--global integer BaseOfData2, SizeOfData2
--global integer BaseOfCode2, SizeOfCode2
global atom BaseOfData2, SizeOfData2
global atom BaseOfCode2, SizeOfCode2
--global integer IB
global integer ImageBase2
--with trace
function peHeader(integer machine, integer subsystem, integer subvers, sequence imports, sequence exports, sequence relocations, integer datalen, integer codelen, integer resourcelen)
integer BaseOfData
integer BaseOfCode
--integer BaseOfImports
integer resourceBase
integer SizeOfImage
integer importRVA
integer importBase
--
-- machine should be M32 or M64
-- subsystem should be CUI or GUI
-- imports should be eg {{"kernel32.dll",{"GetLastError","FormatMessageA","LocalFree"},{#17,#4C,#58}},
--                       {"user32.dll",{"MessageBoxA"},{#69}}},
--          or (rarely) {} if no imports whatsoever are required
--                          Routine names get automatically [tag]sorted per dll, as required.
--                          The {#17,#4C,#58} and {#69} are link chain starts, not used here.
-- exports should be eg {"errormsg.dll",{"ShowErrorMessage","ShowLastError"},{#0000000C,#0000004E}}
--                      (where #C,#4E are the corresponding offsets into the code section)
--                      (the names and offsets are automatically [tag]sorted, as required)
--                   or {} if not a dll
--datab4code: (and this description is out-of-date/incomplete anyway)
-- relocations should be {} or {{code_relocations},{data_relocations}}
-- relocations should be {} or {{data_relocations},{code_relocations}}
--                              where each entry is a HighLow offset into the
--                              appropriate section. Should only be required
--                              for 32-bit DLLs, not exes, not 64-bit DLLs.
--                              Also, cmiiw, the resource section does not
--                              need or get relocations, even if logically
--                              the DataRVA therein ought to be adjusted.
-- For data, code, and resources, this routine only needs to know the lengths (pre-padding).
--
-- Returns: A sequence containing the MZ and PE headers, RVA (Relative Virtual Address) table,
--          the completed section table, and any required imports, exports, and relocations,
--          suitable for writing directly to a file (previously opened in binary mode).
--          Also sets BaseOfData, BaseOfCode, and resourceBase, which will likely need to be
--          applied to the revelant block(s). [Hopefully without changing the size of them!]
--          Calling routine is responsible for writing data, code, and resources, with
--          appropriate padding (which should /not/ be included in the lengths passed).
--
--DEV:
--sequence res
string res
sequence sections   -- {{RVAdx,name,VirtualSize,VirtualAddress}}
integer RVAaddr
--sequence names
integer thunks,
        thunkstart,
        isize = 0,  -- imports
        ibase,
        esize = 0,  -- exports
        elen,
        ebase,
        xsize = 0,  -- exceptions (PE64 only)
        xbase,
        rsize = 0,  -- relocs/resources
        rbase,
        prevbase,
        newbase,
--      dsize,
--      csize,
        RVAtable,
        SizeOfData,
        SizeOfCode,
        padding
sequence thunk
sequence ridx
integer RelativeVirtualAddress,
        VirtualSize,
        VirtualAddress,
        k
integer HeaderCharacteristics
sequence itags
sequence ithunks
sequence etags
integer ImageBase

--DEV:
        sequence section
        string name
        integer ln
        integer SizeOfRawData
atom v
--integer symidx
--object tmp
integer expected_reloc_size

--puts(1,"peHeader: entry\n")
    ImageBase = #00400000 -- DEV what about DLLs?
ImageBase2 = ImageBase

    HeaderCharacteristics = IMAGE_FILE_EXECUTABLE_IMAGE +
--                          IMAGE_FILE_LINE_NUMS_STRIPPED +
--                          IMAGE_FILE_LOCAL_SYMS_STRIPPED +
                            IMAGE_FILE_DEBUG_STRIPPED
    sections = {}
    RVAaddr = #1000
    SizeOfImage = 0 -- (+length(MZ/PE/etc) done last)
    if length(imports) then
        thunks = 0
        isize = (length(imports)+1)*20
        itags = repeat(0,length(imports))
        ithunks = repeat(0,length(imports))
        for i=1 to length(imports) do
            isize += length(imports[i][1])+1
-- 5/10/14:
if and_bits(isize,1) then isize += 1 end if
            inames = imports[i][2]
            ithunks[i] = tagset(length(inames)) -- (replaced with thunk addrs later)
            itags[i] = custom_sort(routine_id("by_importname"),ithunks[i])
            thunks += length(inames)+1
            for j=1 to length(inames) do
--              isize += 2+length(inames[itags[i][j]])+1 -- (not necessary)
                isize += 2+length(inames[j])+1
-- 5/10/14:
if and_bits(isize,1) then isize += 1 end if
            end for
        end for
        if machine=M32 then
            isize += thunks*4
        else --machine=M64
            isize += thunks*8
        end if
        ibase = RVAaddr
        RVAaddr += RoundToSectionAlignment(isize)
        sections = append(sections,{2,".idata",isize,ibase})
--      BaseOfImports = RVAaddr (untried)
--      BaseOfImports = ibase
        SizeOfImage += RoundToFileAlignment(isize)
    else
        ithunks = {}
    end if
    if length(exports) then
        enames = exports[2]
        etags = custom_sort(routine_id("by_exportname"),tagset(length(exports[2])))
        elen = length(exports[2])
        esize = #28+elen*10+length(exports[1])+1
-- 5/10/14:
if and_bits(esize,1) then esize += 1 end if
        for i=1 to elen do
            esize += length(exports[2][i])+1
-- 5/10/14:
if and_bits(esize,1) then esize += 1 end if
        end for
        ebase = RVAaddr
        RVAaddr += RoundToSectionAlignment(esize)
        sections = append(sections,{1,".edata",esize,ebase})
        SizeOfImage += RoundToFileAlignment(esize)
        HeaderCharacteristics += IMAGE_FILE_DLL
        if machine=M64 then
            HeaderCharacteristics += IMAGE_FILE_RELOCS_STRIPPED
        end if
    else
        HeaderCharacteristics += IMAGE_FILE_RELOCS_STRIPPED
    end if
    if gexch!=0 then    -- global exception handler (PE64 only)
--      ?9/0
        xsize = #0C
        xbase = RVAaddr
        RVAaddr += RoundToSectionAlignment(xsize)
        sections = append(sections,{4,".pdata",xsize,xbase})
        SizeOfImage += RoundToFileAlignment(xsize)
    end if
    if length(relocations) then
        -- (only needed for 32-bit dlls)
        if equal(relocations,{{},{}}) then ?9/0 end if
        rsize = 10
        ridx = {{},{}}
        for i=1 to length(relocations) do
            if length(relocations[i]) then
                prevbase = floor(relocations[i][1]/#1000)
                ridx[i] = append(ridx[i],{1,1,prevbase})
                for j=2 to length(relocations[i]) do
                    newbase = floor(relocations[i][j]/#1000)
                    if newbase=prevbase then
                        ridx[i][$][2] += 1
                        rsize += 2
                    else
                        if and_bits(ridx[i][$][2],1) then
                            rsize += 2
                        end if
--?{rsize}
                        ridx[i] = append(ridx[i],{j,1,newbase})
                        rsize += 10
                        prevbase = newbase
                    end if
                end for
                if and_bits(ridx[i][$][2],1) then
                    rsize += 2
                end if
--?rsize
            end if
----??rsize+=8??
            if i=1 then rsize+=10 end if
        end for
        rbase = RVAaddr
        RVAaddr += RoundToSectionAlignment(rsize)
        sections = append(sections,{6,".reloc",rsize,rbase})
        expected_reloc_size = rsize
        SizeOfImage += RoundToFileAlignment(rsize)
    end if
if datab4code then
    sections = append(sections,{0,".data",datalen,RVAaddr})
    BaseOfData = RVAaddr
BaseOfData2 = BaseOfData
SizeOfData2 = datalen
    RVAaddr += RoundToSectionAlignment(datalen)
    SizeOfImage += RoundToFileAlignment(datalen)
--  sections = append(sections,{0,".code",codelen,RVAaddr})
    sections = append(sections,{0,".text",codelen,RVAaddr})
    BaseOfCode = RVAaddr
BaseOfCode2 = BaseOfCode
SizeOfCode2 = codelen
    RVAaddr += RoundToSectionAlignment(codelen)
    SizeOfImage += RoundToFileAlignment(codelen)
else
    sections = append(sections,{0,".text",codelen,RVAaddr})
    BaseOfCode = RVAaddr
BaseOfCode2 = BaseOfCode
SizeOfCode2 = codelen
    RVAaddr += RoundToSectionAlignment(codelen)
    SizeOfImage += RoundToFileAlignment(codelen)
    sections = append(sections,{0,".data",datalen,RVAaddr})
    BaseOfData = RVAaddr
BaseOfData2 = BaseOfData
SizeOfData2 = datalen
    RVAaddr += RoundToSectionAlignment(datalen)
    SizeOfImage += RoundToFileAlignment(datalen)
end if

    resourceBase = RVAaddr
    if resourcelen!=0 then
        rsize = resourcelen
        -- aside: we are reusing rsize; in SizeOfData calculations 
        --        below, we want this (rsize/resources) and /not/ 
        --        relocations (which/as that is "discardable").
        sections = append(sections,{3,".rsrc",rsize,RVAaddr})
        RVAaddr += RoundToSectionAlignment(rsize)
        SizeOfImage += RoundToFileAlignment(rsize)
    end if
--  SizeOfImage = RVAaddr
--?SizeOfImage
integer SizeOfImage2 = RVAaddr

--?res
    res = mzHeader()
--?res

    -- Note: The following offsets assume e_lfanew is #80. If you are writing a program which reads
    ---      exe/dll files, then (for example) Subsystem is at (e_lfanew)+#5C rather than #DC.

    if machine=M32 then
        res &= stringify(               --  offset    name                     size value       desc
               {'P','E',0,0,            -- 00000080,signature,                  x4, PE\0\0,     should be "PE\0\0"
                #4C,#01,                -- 00000084,machine,                    h2, 014Ch,      i386
                0,0,                    -- 00000086,sections,                   2,  3,          Number of sections
                0,0,0,0,                -- 00000088,DateTimeStamp,              h4, 52EFE35Fh,  03 February 2014, 18:43:43 (ish)
                0,0,0,0,                -- 0000008C,PointerToSymbolTable,       h4, 00000000h,
                0,0,0,0,                -- 00000090,NumberOfSymbols,            h4, 00000000h,
                #E0,#00,                -- 00000094,SizeOfOptionalHeader,       h2, 00E0h,
                #0F,#01,                -- 00000096,Characteristics,            h2, 010Fh,      32BIT+EXEC-RELOCS-LINES-LOCALS
                #0B,#01,                -- 00000098,Magic,                      h2, 010Bh,      32bit
                #01,                    -- 0000009A,MajorLinkerVersion,         h1, 01h,        1.70
                #46,                    -- 0000009B,MinorLinkerVersion,         h1, 46h,
                0,0,0,0,                -- 0000009C,SizeOfCode,                 h4, 00000200h,
                0,0,0,0,                -- 000000A0,SizeOfInitializedData,      h4, 00000400h,
                0,0,0,0,                -- 000000A4,SizeOfUninitializedData,    h4, 00000000h,
                0,0,0,0,                -- 000000A8,AddressOfEntryPoint,        h4, 00002000h,
                0,0,0,0,                -- 000000AC,BaseOfCode,                 h4, 00002000h,
                0,0,0,0,                -- 000000B0,BaseOfData,                 h4, 00001000h,
                0,0,#40,0,              -- 000000B4,ImageBase,                  h4, 00400000h,
                0,#10,0,0,              -- 000000B8,SectionAlignment,           h4, 00001000h,
                0,#02,0,0,              -- 000000BC,FileAlignment,              h4, 00000200h,
                1,0,                    -- 000000C0,MajorOperatingSystemVersion,h2, 0001h,      1.0
                0,0,                    -- 000000C2,MinorOperatingSystemVersion,h2, 0000h,
                0,0,                    -- 000000C4,MajorImageVersion,          h2, 0000h,      0.0
                0,0,                    -- 000000C6,MinorImageVersion,          h2, 0000h,
                4,0,                    -- 000000C8,MajorSubsystemVersion,      h2, 0004h,      4.0
                0,0,                    -- 000000CA,MinorSubsystemVersion,      h2, 0000h,
                0,0,0,0,                -- 000000CC,Win32VersionValue,          h4, 00000000h,
                0,0,0,0,                -- 000000D0,SizeOfImage,                h4, 00004000h,
                0,2,0,0,                -- 000000D4,SizeOfHeaders,              h4, 00000200h,
                0,0,0,0,                -- 000000D8,CheckSum,                   h4, 000025F4h,
                2,0,                    -- 000000DC,Subsystem,                  h2, 0002h,      2=GUI,3=CUI
                0,0,                    -- 000000DE,DllCharacteristics,         h2, 0000h,
                0,#10,0,0,              -- 000000E0,SizeOfStackReserve,         h4, 00001000h,
                0,#10,0,0,              -- 000000E4,SizeOfStackCommit,          h4, 00001000h,
                0,0,#01,0,              -- 000000E8,SizeOfHeapReserve,          h4, 00010000h,
                0,0,0,0,                -- 000000EC,SizeOfHeapCommit,           h4, 00000000h,
                0,0,0,0,                -- 000000F0,LoaderFlags,                h4, 00000000h,
                16,0,0,0,               -- 000000F4,NumberOfRvaAndSizes,        4,  16,

                0,0,0,0,                -- 000000F8,RelativeVirtualAddress[1],  h4, 00000000h,  export
                0,0,0,0,                -- 000000FC,RVASize[1],                 h4, 00000000h,
                0,0,0,0,                -- 00000100,RelativeVirtualAddress[2],  h4, 00003000h,  import
                0,0,0,0,                -- 00000104,RVASize[2],                 h4, 000000B0h,
                0,0,0,0,                -- 00000108,RelativeVirtualAddress[3],  h4, 00000000h,  resource
                0,0,0,0,                -- 0000010C,RVASize[3],                 h4, 00000000h,
                0,0,0,0,                -- 00000110,RelativeVirtualAddress[4],  h4, 00000000h,  exception
                0,0,0,0,                -- 00000114,RVASize[4],                 h4, 00000000h,
                0,0,0,0,                -- 00000118,RelativeVirtualAddress[5],  h4, 00000000h,  security
                0,0,0,0,                -- 0000011C,RVASize[5],                 h4, 00000000h,
                0,0,0,0,                -- 00000120,RelativeVirtualAddress[6],  h4, 00000000h,  basereloc
                0,0,0,0,                -- 00000124,RVASize[6],                 h4, 00000000h,
                0,0,0,0,                -- 00000128,RelativeVirtualAddress[7],  h4, 00000000h,  debug
                0,0,0,0,                -- 0000012C,RVASize[7],                 h4, 00000000h,
                0,0,0,0,                -- 00000130,RelativeVirtualAddress[8],  h4, 00000000h,  architecture
                0,0,0,0,                -- 00000134,RVASize[8],                 h4, 00000000h,
                0,0,0,0,                -- 00000138,RelativeVirtualAddress[9],  h4, 00000000h,  globalptr
                0,0,0,0,                -- 0000013C,RVASize[9],                 h4, 00000000h,
                0,0,0,0,                -- 00000140,RelativeVirtualAddress[10], h4, 00000000h,  TLS table
                0,0,0,0,                -- 00000144,RVASize[10],                h4, 00000000h,
                0,0,0,0,                -- 00000148,RelativeVirtualAddress[11], h4, 00000000h,  loadconfig
                0,0,0,0,                -- 0000014C,RVASize[11],                h4, 00000000h,
                0,0,0,0,                -- 00000150,RelativeVirtualAddress[12], h4, 00000000h,  boundimport
                0,0,0,0,                -- 00000154,RVASize[12],                h4, 00000000h,
                0,0,0,0,                -- 00000158,RelativeVirtualAddress[13], h4, 00000000h,  IAT
                0,0,0,0,                -- 0000015C,RVASize[13],                h4, 00000000h,
                0,0,0,0,                -- 00000160,RelativeVirtualAddress[14], h4, 00000000h,  delayimport
                0,0,0,0,                -- 00000164,RVASize[14],                h4, 00000000h,
                0,0,0,0,                -- 00000168,RelativeVirtualAddress[15], h4, 00000000h,  comdescriptor
                0,0,0,0,                -- 0000016C,RVASize[15],                h4, 00000000h,
                0,0,0,0,                -- 00000170,RelativeVirtualAddress[16], h4, 00000000h,  IDD 16
                0,0,0,0})               -- 00000174,RVASize[16],                h4, 00000000h,
        if length(res)!=#178 then ?9/0 end if
        if GetField(res,#B4,DWORD)!=ImageBase then ?9/0 end if
        RVAtable = #F8
        res = SetField(res,#B0,DWORD,BaseOfData)
--DEV/SUG:
--      if and_bits(HeaderCharacteristics,IMAGE_FILE_DLL)!=0 then
--          res = SetField(res,#34,DWORD,#100000000)
--      end if
        HeaderCharacteristics += IMAGE_FILE_32BIT_MACHINE

    elsif machine=M64 then
        res &= stringify(               --  offset    name                     size value       desc
               {'P','E',0,0,            -- 00000080,signature,                  x4, PE\0\0,     should be "PE\0\0"
                #64,#86,                -- 00000084,machine,                    h2, 8664h,      amd64
                3,0,                    -- 00000086,sections,                   2,  3,          Number of sections
                0,0,0,0,                -- 00000088,DateTimeStamp,              h4, 52EFE35Fh,  03 February 2014, 18:43:43 (ish)
                0,0,0,0,                -- 0000008C,PointerToSymbolTable,       h4, 00000000h,
                0,0,0,0,                -- 00000090,NumberOfSymbols,            h4, 00000000h,
                #F0,#00,                -- 00000094,SizeOfOptionalHeader,       h2, 00F0h,
                #2F,#00,                -- 00000096,Characteristics,            h2, 002Fh,      EXEC-RELOCS-LINES-LOCALS+LARGE_ADDR
                #0B,#02,                -- 00000098,Magic,                      h2, 020Bh,      64bit
                #01,                    -- 0000009A,MajorLinkerVersion,         h1, 01h,        1.70
                #46,                    -- 0000009B,MinorLinkerVersion,         h1, 46h,
                0,0,0,0,                -- 0000009C,SizeOfCode,                 h4, 00000200h,
                0,0,0,0,                -- 000000A0,SizeOfInitializedData,      h4, 00000400h,
                0,0,0,0,                -- 000000A4,SizeOfUninitializedData,    h4, 00000000h,
                0,0,0,0,                -- 000000A8,AddressOfEntryPoint,        h4, 00001000h,
                0,0,0,0,                -- 000000AC,BaseOfCode,                 h4, 00001000h,
                0,0,#40,0,0,0,0,0,      -- 000000B0,ImageBase,                  h0, 0000000000400000h,
                0,#10,0,0,              -- 000000B8,SectionAlignment,           h4, 00001000h,
                0,#02,0,0,              -- 000000BC,FileAlignment,              h4, 00000200h,
                1,0,                    -- 000000C0,MajorOperatingSystemVersion,h2, 0001h,      1.0
                0,0,                    -- 000000C2,MinorOperatingSystemVersion,h2, 0000h,
                0,0,                    -- 000000C4,MajorImageVersion,          h2, 0000h,      0.0
                0,0,                    -- 000000C6,MinorImageVersion,          h2, 0000h,
                5,0,                    -- 000000C8,MajorSubsystemVersion,      h2, 0005h,      5.0
                0,0,                    -- 000000CA,MinorSubsystemVersion,      h2, 0000h,
                0,0,0,0,                -- 000000CC,Win32VersionValue,          h4, 00000000h,
                0,0,0,0,                -- 000000D0,SizeOfImage,                h4, 00004000h,
                0,2,0,0,                -- 000000D4,SizeOfHeaders,              h4, 00000200h,
                0,0,0,0,                -- 000000D8,CheckSum,                   h4, 000025F4h,
                2,0,                    -- 000000DC,Subsystem,                  h2, 0002h,      2=GUI,3=CUI
                0,0,                    -- 000000DE,DllCharacteristics,         h2, 0000h,
                0,#10,0,0,0,0,0,0,      -- 000000E0,SizeOfStackReserve,         h8, 0000000000001000h,
                0,#10,0,0,0,0,0,0,      -- 000000E8,SizeOfStackCommit,          h8, 0000000000001000h,
                0,0,#01,0,0,0,0,0,      -- 000000F0,SizeOfHeapReserve,          h8, 0000000000010000h,
                0,0,0,0,0,0,0,0,        -- 000000F8,SizeOfHeapCommit,           h8, 0000000000000000h,
                0,0,0,0,                -- 00000100,LoaderFlags,                h4, 00000000h,
                16,0,0,0,               -- 00000104,NumberOfRvaAndSizes,        4,  16,
                0,0,0,0,                -- 00000108,RelativeVirtualAddress[1],  h4, 00000000h,  export
                0,0,0,0,                -- 0000010C,RVASize[1],                 h4, 00000000h,
                0,0,0,0,                -- 00000110,RelativeVirtualAddress[2],  h4, 00003000h,  import
                0,0,0,0,                -- 00000114,RVASize[2],                 h4, 00000189h,
                0,0,0,0,                -- 00000118,RelativeVirtualAddress[3],  h4, 00000000h,  resource
                0,0,0,0,                -- 0000011C,RVASize[3],                 h4, 00000000h,
                0,0,0,0,                -- 00000120,RelativeVirtualAddress[4],  h4, 00000000h,  exception
                0,0,0,0,                -- 00000124,RVASize[4],                 h4, 00000000h,
                0,0,0,0,                -- 00000128,RelativeVirtualAddress[5],  h4, 00000000h,  security
                0,0,0,0,                -- 0000012C,RVASize[5],                 h4, 00000000h,
                0,0,0,0,                -- 00000130,RelativeVirtualAddress[6],  h4, 00000000h,  basereloc
                0,0,0,0,                -- 00000134,RVASize[6],                 h4, 00000000h,
                0,0,0,0,                -- 00000138,RelativeVirtualAddress[7],  h4, 00000000h,  debug
                0,0,0,0,                -- 0000013C,RVASize[7],                 h4, 00000000h,
                0,0,0,0,                -- 00000140,RelativeVirtualAddress[8],  h4, 00000000h,  architecture
                0,0,0,0,                -- 00000144,RVASize[8],                 h4, 00000000h,
                0,0,0,0,                -- 00000148,RelativeVirtualAddress[9],  h4, 00000000h,  globalptr
                0,0,0,0,                -- 0000014C,RVASize[9],                 h4, 00000000h,
                0,0,0,0,                -- 00000150,RelativeVirtualAddress[10], h4, 00000000h,  TLS table
                0,0,0,0,                -- 00000154,RVASize[10],                h4, 00000000h,
                0,0,0,0,                -- 00000158,RelativeVirtualAddress[11], h4, 00000000h,  loadconfig
                0,0,0,0,                -- 0000015C,RVASize[11],                h4, 00000000h,
                0,0,0,0,                -- 00000160,RelativeVirtualAddress[12], h4, 00000000h,  boundimport
                0,0,0,0,                -- 00000164,RVASize[12],                h4, 00000000h,
                0,0,0,0,                -- 00000168,RelativeVirtualAddress[13], h4, 00000000h,  IAT
                0,0,0,0,                -- 0000016C,RVASize[13],                h4, 00000000h,
                0,0,0,0,                -- 00000170,RelativeVirtualAddress[14], h4, 00000000h,  delayimport
                0,0,0,0,                -- 00000174,RVASize[14],                h4, 00000000h,
                0,0,0,0,                -- 00000178,RelativeVirtualAddress[15], h4, 00000000h,  comdescriptor
                0,0,0,0,                -- 0000017C,RVASize[15],                h4, 00000000h,
                0,0,0,0,                -- 00000180,RelativeVirtualAddress[16], h4, 00000000h,  IDD 16
                0,0,0,0})               -- 00000184,RVASize[16],                h4, 00000000h,
        if length(res)!=#188 then ?9/0 end if
        if GetField(res,#B0,QWORD)!=ImageBase then ?9/0 end if
        RVAtable = #108
--DEV/SUG:
--      if and_bits(HeaderCharacteristics,IMAGE_FILE_DLL)!=0 then
--          res = SetField(res,#30,QWORD,#100000000)
--      end if
        HeaderCharacteristics += IMAGE_FILE_LARGE_ADDRESS_AWARE
    else
        ?9/0
    end if
--?res
    for i=1 to length(sections) do
        k = sections[i][1]
        if k then
            RelativeVirtualAddress = sections[i][4]
            VirtualSize = sections[i][3]
            res = SetField(res,RVAtable+(k-1)*8,DWORD,RelativeVirtualAddress)
            res = SetField(res,RVAtable+(k-1)*8+4,DWORD,VirtualSize)
        end if
    end for
    SizeOfData = RoundToFileAlignment(datalen) +    -- data
                 RoundToFileAlignment(isize) +      -- import section
                 RoundToFileAlignment(esize)        -- export section
    if resourcelen then
        SizeOfData += RoundToFileAlignment(rsize)   -- resources [/not/ relocations]
    end if
    SizeOfCode = RoundToFileAlignment(codelen)
    res = SetField(res,#86,WORD,length(sections))
    if GetField(res,#B8,DWORD)!=SectionAlignment then ?9/0 end if
    res = SetField(res,#88,DWORD,TimeDateStamp)
    res = SetField(res,#96,WORD,HeaderCharacteristics)
    res = SetField(res,#9C,DWORD,SizeOfCode)
    res = SetField(res,#A0,DWORD,SizeOfData)
    res = SetField(res,#A8,DWORD,BaseOfCode) -- (EntryPoint is always the same as BaseOfCode)
    res = SetField(res,#AC,DWORD,BaseOfCode)
    if GetField(res,#B8,DWORD)!=SectionAlignment then ?9/0 end if
    if GetField(res,#BC,DWORD)!=FileAlignment then ?9/0 end if
    res = SetField(res,#C8,DWORD,subvers)
    res = SetField(res,#D0,DWORD,SizeOfImage2)
    res = SetField(res,#DC,WORD,subsystem)
    if and_bits(HeaderCharacteristics,IMAGE_FILE_DLL)!=0 then
        res = SetField(res,#DE,WORD,IMAGE_DLLCHARACTERISTICS_DYNAMIC_BASE)
    end if

    -- The section table starts at #178 (32-bit) or #188 (64-bit), each entry is #28 bytes long

    integer PointerToRawData = RoundToFileAlignment(length(res)+length(sections)*#28)
--  importBase = PointerToRawData
    importBase = #1000
    if length(imports) then
        if machine=M32 then
            importRVA = #1000-PointerToRawData
        else
            importRVA = 0-PointerToRawData
        end if
    end if
--puts(1,"13\n")
--?res
    for i=1 to length(sections) do
--DEV:
--      sequence section
--      string name
--      integer ln
--      integer SizeOfRawData

        section = stringify(            --  offset    name                 size value       desc
                  {0,0,0,0,0,0,0,0,     -- 00000000,Name[1],                x8,.data\0\0\0,
                   0,0,0,0,             -- 00000008,VirtualSize,h4,0000001Dh,(not really used)
                   0,0,0,0,             -- 0000000C,VirtualAddress,h4,00001000h,
                   0,0,0,0,             -- 00000010,SizeOfRawData,h4,00000200h,
                   0,0,0,0,             -- 00000014,PointerToRawData,h4,00000200h,
                   0,0,0,0,             -- 00000018,PointerToRelocations,h4,00000000h,
                   0,0,0,0,             -- 0000001C,PointerToLinenumbers,h4,00000000h,
                   0,0,                 -- 00000020,NumberOfRelocations,h2,0000h,
                   0,0,                 -- 00000022,NumberOfLinenumbers,h2,0000h,
                   0,0,0,0})            -- 00000024,Characteristics,h4,C0000040h,read+write+init
        {?,name,VirtualSize,VirtualAddress} = sections[i]
        ln = length(name)
        if ln>8 then ?9/0 end if
        section[1..ln] = name
        if name=".pdata" then
            VirtualSize = #18
        end if
        section = SetField(section,#08,DWORD,VirtualSize)
        section = SetField(section,#0C,DWORD,VirtualAddress)
        SizeOfRawData = RoundToFileAlignment(VirtualSize)
        section = SetField(section,#10,DWORD,SizeOfRawData)
        section = SetField(section,#14,DWORD,PointerToRawData)
        v = sectionCharacteristics[find(name,sectionNames)]
        section = SetField(section,#24,DWORD,v)
        PointerToRawData += SizeOfRawData
        res &= section
    end for
    padding = RoundToFileAlignment(length(res))-length(res)
    if padding then
        res &= stringify(repeat(0,padding))
    end if
--  SizeOfImage += RoundToFileAlignment(length(res))
    SizeOfImage += length(res) -- (+length(MZ/PE/etc), as promised. This is now the final value.)
--?SizeOfImage

    if length(imports) then
        thunkstart = (length(imports)+1)*20
        isize = thunkstart
        if machine=M32 then
            isize += thunks*4
        else --machine=M64
            isize += thunks*8
        end if
        for i=1 to length(imports)+1 do
--          sequence section
            section = stringify(        --  offset    name                     size value       desc
                      {0,0,0,0,         -- 00000000,OriginalFirstThunk,         h4, 00000000h,  Hint Name Array
                       0,0,0,0,         -- 00000004,TimeDateStamp,              h4, 00000000h,
                       0,0,0,0,         -- 00000008,ForwarderChain,             h4, 00000000h,
                       0,0,0,0,         -- 0000000C,Name,                       h4, 0000303Ch,  [0x0000063C:KERNEL32.DLL]
                       0,0,0,0})        -- 00000010,FirstThunk,                 h4, 00003064h,  Hint Name Array, see 0x00000664
            if i<=length(imports) then -- (not the terminating null block)
                section = SetField(section,#0C,DWORD,isize+ibase)
                isize += length(imports[i][1])+1
-- 5/10/14:
if and_bits(isize,1) then isize += 1 end if
                section = SetField(section,#10,DWORD,thunkstart+ibase)
                if machine=M32 then
                    thunkstart += (length(imports[i][2])+1)*4
                else
                    thunkstart += (length(imports[i][2])+1)*8
                end if
            end if
            res &= section
        end for
        -- thunks
        for i=1 to length(imports) do
            for j=1 to length(imports[i][2])+1 do
                if machine=M32 then
                    thunk = stringify({0,0,0,0})
                else
                    thunk = stringify({0,0,0,0,0,0,0,0})
                end if
                if j<=length(imports[i][2]) then
--                  ithunks[i][j] = length(res)+importRVA -- (nb do not apply the tag sort)
                    ithunks[i][itags[i][j]] = length(res)+importRVA -- (nb do not apply the tag sort)
                    if machine=M32 then
                        thunk = SetField(thunk,0,DWORD,isize+ibase)
                    else
                        thunk = SetField(thunk,0,QWORD,isize+ibase)
                    end if
--                  isize += 2+length(imports[i][2][j])+1
                    isize += 2+length(imports[i][2][itags[i][j]])+1
-- 5/10/14:
if and_bits(isize,1) then isize += 1 end if
                end if
                res &= thunk
            end for
        end for
        -- dll names
        for i=1 to length(imports) do
            res &= imports[i][1]
            res &= 0
-- 5/10/14:
if and_bits(length(res),1) then res &= 0 end if
        end for
        -- hints and function names
        for i=1 to length(imports) do
            for j=1 to length(imports[i][2]) do
                res &= stringify({0,0}) -- hint
--              res &= imports[i][2][j]
                res &= imports[i][2][itags[i][j]]
                res &= 0
-- 5/10/14:
if and_bits(length(res),1) then res &= 0 end if
            end for
        end for
        padding = RoundToFileAlignment(length(res))-length(res)
        if padding then
            res &= stringify(repeat(0,padding))
        end if
    end if

    if length(exports) then
        -- exports is eg  {"errormsg.dll",{"ShowErrorMessage","ShowLastError"},{#0C,#4E}}
--      sequence section

        section = stringify(        --  offset    name                 size value       desc
                  { 0,0,0,0,        --00000000, ExportCharacteristics,  h4,00000000h,   (should be 0)
                    0,0,0,0,        --00000004, TimeDateStamp,          h4,52EEFA97h,   03 February 2014, 02:10:31 (ish)
                    0,0,            --00000008, MajorVersion,           h2,0000h,       0.0
                    0,0,            --0000000A, MinorVersion,           h2,0000h,
                    0,0,0,0,        --0000000C, Name,                   h4,0000403Ch,   [0x00000A3C:errormsg.dll]
                    1,0,0,0,        --00000010, OrdinalBase,            h4,00000001h,   (usually 1)
                    0,0,0,0,        --00000014, NumberOfFunctions,      h4,00000002h,
                    0,0,0,0,        --00000018, NumberOfNames,          h4,00000002h,
                    0,0,0,0,        --0000001C, AddressOfFunctions,     h4,00004028h,   see 0x00000A28
                    0,0,0,0,        --00000020, AddressOfNames,         h4,00004030h,   see 0x00000A30
                    0,0,0,0})       --00000024, AddressOfNameOrdinals,  h4,00004038h,   see 0x00000A38
        section = SetField(section,#04,DWORD,TimeDateStamp)
--      elen = length(exports[2]) -- already set
        section = SetField(section,#14,DWORD,elen)
        section = SetField(section,#18,DWORD,elen)
        RVAaddr = length(section)+ebase
        section = SetField(section,#1C,DWORD,RVAaddr)
        RVAaddr += elen*4
        section = SetField(section,#20,DWORD,RVAaddr)
        RVAaddr += elen*4
        section = SetField(section,#24,DWORD,RVAaddr)
        RVAaddr += elen*2
        section = SetField(section,#0C,DWORD,RVAaddr)
        RVAaddr += length(exports[1])+1
-- 5/10/14:
if and_bits(RVAaddr,1) then RVAaddr += 1 end if
        -- ExportRVAs
        for i=1 to elen do
            thunk = stringify({0,0,0,0})
--          thunk = SetField(thunk,#0,DWORD,exports[3][i]+BaseOfCode)
            thunk = SetField(thunk,#0,DWORD,exports[3][etags[i]]+BaseOfCode)
            section &= thunk
        end for
        -- NameRVAs
        for i=1 to elen do
--          string name
--          name = exports[2][i]
            name = exports[2][etags[i]]
            thunk = stringify({0,0,0,0})
            thunk = SetField(thunk,#0,DWORD,RVAaddr)
            section &= thunk
            RVAaddr += length(name)+1
-- 5/10/14:
if and_bits(RVAaddr,1) then RVAaddr += 1 end if
        end for
        -- Ordinals
        for i=1 to elen do
            thunk = stringify({0,0})
            thunk = SetField(thunk,#0,2,i-1)
            section &= thunk
        end for
        -- Name
        section &= exports[1]
        section &= 0
-- 5/10/14:
if and_bits(length(section),1) then section &= 0 end if
        -- Names
        for i=1 to elen do
--          string name
--          name = exports[2][i]
            name = exports[2][etags[i]]
            section &= name
            section &= 0
-- 5/10/14:
if and_bits(length(section),1) then section &= 0 end if
        end for
        res &= section
        padding = RoundToFileAlignment(length(res))-length(res)
        if padding then
            res &= stringify(repeat(0,padding))
        end if
    end if
    if gexch!=0 then    -- global exception handler (PE64 only)
--;   DWORD BeginAddress; 
--;   DWORD EndAddress; 
--;   DWORD UnwindData; 
--
--;   UBYTE Version       : 3;      \ PL: so that be one byte!
--;   UBYTE Flags         : 5;      / #19 = 0b10001001, 1 plus flags 2 and #10
--;   UBYTE SizeOfProlog; 
--;   UBYTE CountOfCodes; 
--;   UBYTE FrameRegister : 4; 
--;   UBYTE FrameOffset   : 4; 
--;   UNWIND_CODE UnwindCode[1]; 
--; /*  UNWIND_CODE MoreUnwindCode[((CountOfCodes + 1) & ~1) - 1]; 
--; *   union { 
--; *     OPTIONAL ULONG ExceptionHandler; 
--; *     OPTIONAL ULONG FunctionEntry; 
--; *   }; 
--; *   OPTIONAL ULONG ExceptionData[]; */ 
        section = stringify(        --  offset    name                 size value       desc
                  { 0,0,0,0,        --00000000, BeginAddress,           h4,00001000h,   (should be RVA of code section)
                    0,0,0,0,        --00000004, EndAddress,             h4,000010??h,   (end of code section)
                    0,0,0,0,        --00000008, UnwindData,             h4,000030??h,   (RVA of next byte)
                    0x19,           --0000000C, VersionFlags,           h1,19h,         {1,0b00011}
                    0,              --0000000D, SizeOfProlog,           h1,00h,         (should be 0)
                    0,              --0000000E, CountOfCodes,           h1,00h,         (should be 0)
                    0,              --0000000F, FrameRegister/Offset,   h1,00h,         (should be 0:0)
                    0,0,0,0,        --00000010, ExceptionHandler,       h4,000010??h,   address of exception handler
                    0,0,0,0})       --00000014, ExceptionData,          h4,00000000h,   (should be 0)
        section = SetField(section,#0,DWORD,BaseOfCode)
        section = SetField(section,#4,DWORD,BaseOfCode+codelen)
        section = SetField(section,#08,DWORD,xbase+#0C)
--object dbg = glboffset[gexch]
--dbg = glblabel[gexch]
--dbg = symtab[dbg]
--      section = SetField(section,#10,DWORD,BaseOfCode+glboffset[gexch]+symtab[glblabel[gexch]][S_il])
        section = SetField(section,#10,DWORD,BaseOfCode+glboffset[gexch])
--      k = glblabel[gexch]
--      if bind and mapsymtab then
--          symidx = symtabmap[k]
--      else
--          symidx = k
--      end if
----        section = SetField(section,#10,DWORD,BaseOfCode+glboffset[gexch]+symtab[symidx][S_il])
--      section = SetField(section,#10,DWORD,BaseOfCode+glboffset[gexch]+flatsym2[symidx][S_il])
        res &= section
        padding = RoundToFileAlignment(length(res))-length(res)
        if padding then
            res &= stringify(repeat(0,padding))
        end if
    end if
    if length(relocations) then
        -- (only needed for 32-bit dlls) [DEV you sure about that?]
        res &= relocate(relocations,ridx,{BaseOfCode,BaseOfData})
--      res &= relocate(relocations,ridx,{BaseOfCode,BaseOfData,BaseOfImports})
        if actual_reloc_size!=expected_reloc_size then ?{actual_reloc_size,"!=",expected_reloc_size} end if
--datab4code:
--      res &= relocate(relocations,ridx,{BaseOfData,BaseOfCode})
-- or better yet:
--      sequence bases = {0,0}  -- (avoid assuming DATA=1, CODE=2, or v.v.)
--      bases[DATA] = BaseOfData
--      bases[CODE] = BaseOfCode
--      res &= relocate(relocations,ridx,bases)
        padding = RoundToFileAlignment(length(res))-length(res)
        if padding then
            res &= stringify(repeat(0,padding))
        end if
    end if
--puts(1,"peHeader: exit\n")
    return {res,importBase,BaseOfData,BaseOfCode,resourceBase,SizeOfImage,ithunks,ImageBase}
end function

-- resource section routines:

function unicode(string s)
sequence res
    res = repeat(0,length(s)*2+2)
    for i=1 to length(s) do
        res[i*2-1] = s[i]
    end for
    return res
end function

function decode_version_number(string n)
integer res = 0
integer ch
    for i=1 to length(n) do
        ch = n[i]
        if ch<'0' or ch>'9' then exit end if
        res = res*10+ch-'0'
    end for
    if res>#FFFF then ?9/0 end if
    return res
end function

function decode_version_string(string v)
sequence res = {}
integer k
    while 1 do
        k = find('.',v)
        if k=0 then exit end if
        res = append(res, decode_version_number(v[1..k-1]))
        v = v[k+1..$]
    end while
    res = append(res, decode_version_number(v))
    return res
end function


-- VS_FIXEDFILEINFO.dw(File|Product)Version(MS|LS)s are a little confusing. Version 1.2.3.4 appears as:
--           VS_FIXEDFILEINFO.dwFileVersionMS,h4,00010002h,         1.2.3.4
--           VS_FIXEDFILEINFO.dwFileVersionLS,h4,00030004h,
-- which in a plain byte-wise hex dump is #0200010004000300 (when little endian, that is)
constant vwbeetle = {10,8,14,12}    -- ie  ^8  ^10 ^12 ^14  (VS_FIXEDFILEINFO offsets, after dwSignature and dwStructVers)
-- Obviously install etc must compare them as a pair of dwords, and /not/ (except for equality) four words, one quadword, or an 8-byte string.
-- (I named it vwbeetle for a bit of fun, as any more obvious but duller name probably does not help comprehension anyway, and perhaps it even
--  helps avoid some automatic but incorrect assumptions about the way these things work. vw=version,word, beetle=offset index, or something.)


--DEV/SUG: (nah)
--include rsLabels.e

sequence rsLabels,          -- names (strings)
         rsLblRefs,         -- to_do (offsets)
         rsLblAddrs         -- -1 or actual location

function rsLabelRef(sequence res, integer offset, string LabelName)
-- Obviously, labels can be referenced before or after they have been defined.
-- Either add offset to the to-do list (rsLblRefs[k]), or fixup immediately.
integer k, LabelAddr
    if offset+3>length(res) then ?9/0 end if -- sanity check
    k = find(LabelName,rsLabels)
    if k=0 then
        rsLabels = append(rsLabels,LabelName)
        rsLblRefs = append(rsLblRefs,{offset})
        rsLblAddrs = append(rsLblAddrs,-1)
    else
        LabelAddr = rsLblAddrs[k]
        if LabelAddr=-1 then
            rsLblRefs[k] = append(rsLblRefs[k],offset)
        else
            -- res[offset] += LabelAddr:
--          res = SetField(res,offset,DWORD,GetField(res,offset,DWORD)+LabelAddr)
            LabelAddr += GetField(res,offset,DWORD)
            res = SetField(res,offset,DWORD,LabelAddr)
        end if
    end if
    return res
end function

--function rsLabel(string res, string LabelName)
function rsLabel(sequence res, string LabelName)
-- Obviously, a label can be defined before the first reference, otherwise fixup any
-- prior references right now, and in either case update the table(s) for later use.
integer LabelAddr = length(res)
atom newAddr
integer k = find(LabelName,rsLabels)
integer offset
    if k=0 then
        rsLabels = append(rsLabels,LabelName)
        rsLblRefs = append(rsLblRefs,{})
        rsLblAddrs = append(rsLblAddrs,LabelAddr)
    else
        if rsLblAddrs[k]!=-1 then ?9/0 end if -- already defined?
        rsLblAddrs[k] = LabelAddr
        for i=1 to length(rsLblRefs[k]) do
            offset = rsLblRefs[k][i]
            -- res[offset] += LabelAddr:
--          res = SetField(res,offset,DWORD,GetField(res,offset,DWORD)+LabelAddr)
--NO!
--          LabelAddr += GetField(res,offset,DWORD)
--          res = SetField(res,offset,DWORD,LabelAddr)
--          newAddr = LabelAddr + GetField(res,offset,DWORD)
            newAddr = GetField(res,offset,DWORD) + LabelAddr
            res = SetField(res,offset,DWORD,newAddr)
        end for
        rsLblRefs[k] = {} -- (for final check)
    end if
    return res
end function

function rsDirectory(sequence d)
--
-- The Top Level Directory (start of resource section), containing the resource Types
--
--DEV
sequence res
--string res
sequence ResourceRVA -- (not really used here except init to {} and keeps things consistent)
sequence chunk
--string chunk
integer Type, PrevType
string LabelName

    ResourceRVA = {}
    res = stringify(            --  offset    name                 size value       desc
          { 0,0,0,0,            -- 00000000,ResourceCharacteristics,h4,00000000h,
            0,0,0,0,            -- 00000004,TimeDateStamp,          h4,52E1FC3Ch,   24 January 2014, 05:38:04 (ish)
            0,0,                -- 00000008,MajorVersion,           h2,0000h,       0.0
            0,0,                -- 0000000A,MinorVersion,           h2,0000h,
            0,0,                -- 0000000C,NumberOfNamedEntries,   h2,0000h,
            0,0 })              -- 0000000E,NumberOfIdEntries,      h2,0004h,}
    res = SetField(res,#04,DWORD,TimeDateStamp)
    res = SetField(res,#0E,WORD,length(d))
    for i=1 to length(d) do
        {Type,LabelName} = d[i]
        if i>1 and Type<=PrevType then ?9/0 end if
        PrevType = Type
        chunk = stringify(      --  offset    name                 size value       desc
                {3,0,0,0,       -- 00000010,ID,                     h4,00000003h,   RT_ICON
                 0,0,0,#80})    -- 00000014,DataEntryRVA,           h4,80000030h,   see 0x00000030 (subdirectory)
        chunk = SetField(chunk,#0,DWORD,Type)
        res &= chunk
        res = rsLabelRef(res,length(res)-4,LabelName)
    end for
    return {res,ResourceRVA}
end function

function rsResource(sequence res, string LabelName, sequence d)
--
-- Two levels of subdirectory, containing the IDs and languages.
--
-- note this currently only supports one language per id, not that I
--  imagine it would be terribly difficult to change that.
--  (obviously the first NumberOfIdEntries would have to be less 
--   than length(d), and it would be best to assume that if eg
--      res = rsResource(res,"versions", {{1,LANG_NEUTRAL,"version"}})
--      res = rsVersionInfo(res, "version",...)
--   is changed to have say 1,LANG_NEUTRAL and 2,SERBO_CROATIAN, then
--   demand/check dirLabelNames ("version") are unique, rather than try
--   to suffix instance numbers here and (somehow!?) in rsVersionInfo.
--   On that last point, we don't /have/ to use rsLabelRef/rsLabel and
--   (...dirLabelName&".directory"),, oh, well I just did that anyway.)
--   (I also quickly banged in seenNames, to show what I meant by check
--    for uniqueness, which might crash if there are shared subnodes..)
--
sequence ResourceRVA -- (not really used here, but keeps things consistent)
--DEV
sequence chunk
--string chunk
integer ID, PrevID, offset, lang
string dirLabelName
sequence seenNames

    {res,ResourceRVA} = res
    res = rsLabel(res,LabelName)
    chunk = stringify(          --  offset    name                 size value       desc
            { 0,0,0,0,          -- 00000000,ResourceCharacteristics,h4,00000000h,
              0,0,0,0,          -- 00000004,TimeDateStamp,          h4,52E1FC3Ch,   24 January 2014, 05:38:04 (ish)
              0,0,              -- 00000008,MajorVersion,           h2,0000h,       0.0
              0,0,              -- 0000000A,MinorVersion,           h2,0000h,
              0,0,              -- 0000000C,NumberOfNamedEntries,   h2,0000h,
              0,0 })            -- 0000000E,NumberOfIdEntries,      h2,0004h,}
    chunk = SetField(chunk,#04,DWORD,TimeDateStamp)
    chunk = SetField(chunk,#0E,WORD,length(d))
    res &= chunk
    seenNames = {}
    for i=1 to length(d) do
        {ID,lang,dirLabelName} = d[i]
        if find(dirLabelName,seenNames) then ?9/0 end if
        seenNames = append(seenNames,dirLabelName)
        if i>1 and ID<=PrevID then ?9/0 end if
        PrevID = ID
        chunk = stringify(      --  offset    name                 size value       desc
                {0,0,0,0,       -- 00000010,ID,                     h4,00000003h,   (RT_ICON,)2
                 0,0,0,#80})    -- 00000014,DataEntryRVA,           h4,80000030h,   see 0x00000030 (subdirectory)
        chunk = SetField(chunk,#00,DWORD,ID)
        res &= chunk
--      res = rsLabelRef(res,length(res)-4,dirLabelName&".directory")
        d[i][1] = length(res)-4
    end for
    for i=1 to length(d) do
--      {ID,lang,dirLabelName} = d[i]
--      res = rsLabel(res,dirLabelName&".directory")
        {offset,lang,dirLabelName} = d[i]
        res = SetField(res,offset,DWORD,GetField(res,offset,DWORD)+length(res))
        chunk = stringify(          --  offset    name                 size value       desc
                { 0,0,0,0,          -- 00000000,ResourceCharacteristics,h4,00000000h,
                  0,0,0,0,          -- 00000004,TimeDateStamp,          h4,52E1FC3Ch,   24 January 2014, 05:38:04 (ish)
                  0,0,              -- 00000008,MajorVersion,           h2,0000h,       0.0
                  0,0,              -- 0000000A,MinorVersion,           h2,0000h,
                  0,0,              -- 0000000C,NumberOfNamedEntries,   h2,0000h,
                  1,0,              -- 0000000E,NumberOfIdEntries,      h2,0001h,
                  0,0,0,0,          -- 00000010,ID,                     h4,00000000h,   (RT_ICON,2,)LANG_NEUTRAL
                  0,0,0,0 })        -- 00000014,DataEntryRVA,           h4,000001E8h,   see 0x000001E8
        chunk = SetField(chunk,#04,DWORD,TimeDateStamp)
        chunk = SetField(chunk,#10,DWORD,lang)
        res &= chunk
        res = rsLabelRef(res,length(res)-4,dirLabelName)
    end for
    return {res,ResourceRVA}
end function

--function rsMultiIcon(sequence resRVA, string LabelName, string iconfile, sequence d)
function rsMultiIcon(sequence res, string LabelName, string iconfile, sequence d)
--
-- output the RT_ICON and RT_GROUP_ICON data
--
-- You can change the default icons in a format statement, eg
--  format PE32 
--  icons {"ok.ico",{0,1}}
--  [version, manifest if present]
--
-- The above details coincide with the default.
--
sequence ResourceRVA
--DEV
--string res
sequence chunk
--string chunk
integer offset, padding
integer fn, size, pos
--sequence icon_image   -- (raw binary)
string icon_image   -- (raw binary)
string dirLabelName
integer icon

    {res,ResourceRVA} = res
--13/5/15:
--  fn = open(iconfile,"rb")
    fn = open(rootpath&iconfile,"rb")
    if fn=-1 then ?9/0 end if
    if seek(fn,-1)!=SEEK_OK then ?9/0 end if
    size = where(fn)
    if seek(fn,0)!=SEEK_OK then ?9/0 end if
    icon_image = repeat(' ',size)
    for i=1 to size do
        icon_image[i] = getc(fn)
    end for
    close(fn)

    for i=1 to length(d) do
        {dirLabelName,icon} = d[i]
        res = rsLabel(res,dirLabelName)
        offset = length(res)
        ResourceRVA = append(ResourceRVA,offset)
        res &= stringify(           --  offset    name                 size value       desc
               {0,0,0,0,            -- 000000B0,DataRVA,                h4,0000D0C0h,   see 0x0000D0C0
                0,0,0,0,            -- 000000B4,Size,                   h4,00000128h,
                0,0,0,0,            -- 000000B8,Codepage,               h4,00000000h,   (RT_ICON,1,LANG_NEUTRAL)
                0,0,0,0})           -- 000000BC,Reserved,               h4,00000000h,
        size = GetField(icon_image,#0E+icon*#10,DWORD)
        pos = GetField(icon_image,#12+icon*#10,DWORD)
        res = SetField(res,offset+#04,DWORD,size)
        res = SetField(res,offset,DWORD,length(res))
        res &= icon_image[pos+1..pos+size]
    end for
    padding = and_bits(length(res),#03)
    if padding then
        res &= stringify(repeat(0,4-padding))
    end if
    res = rsLabel(res,LabelName)
    offset = length(res)
    ResourceRVA = append(ResourceRVA,offset)
    res &= stringify(           --  offset    name                 size value       desc
           {0,0,0,0,            -- 000000B0,DataRVA,                h4,0000D0C0h,   see 0x0000D0C0
            0,0,0,0,            -- 000000B4,Size,                   h4,00000128h,
            0,0,0,0,            -- 000000B8,Codepage,               h4,00000000h,   (RT_ICON,1,LANG_NEUTRAL)
            0,0,0,0})           -- 000000BC,Reserved,               h4,00000000h,
    res = SetField(res,offset,DWORD,length(res))
    size = 6+length(d)*14
    res = SetField(res,offset+#04,DWORD,size)
    offset = length(res)
    res &= stringify(           --  offset    name                size value        desc
           {0,0,                -- 00000EF0,idReserved,             2,0,            (must be 0) [RT_GROUP_ICON]
            1,0,                -- 00000EF2,idType,                 2,1,            icon
            0,0})               -- 00000EF4,idCount,                2,2,
    res = SetField(res,offset+#04,WORD,length(d))
    for i=1 to length(d) do
        icon = d[i][2]
        pos = 6+icon*16
        res &= icon_image[pos+1..pos+12]
        chunk = stringify({0,0})
        chunk = SetField(chunk,#0,WORD,i)
        res &= chunk
    end for
    padding = and_bits(length(res),#03)
    if padding then
        res &= stringify(repeat(#90,4-padding))
    end if
    return {res,ResourceRVA}
end function


--function rsVersionInfo(sequence resRVA, string LabelName, integer fileos, integer filetype, integer filesubtype, integer lang, integer cp, sequence d)
function rsVersionInfo(sequence res, string LabelName, integer fileos, integer filetype, integer filesubtype, integer lang, integer cp, sequence d)
--
-- output the RT_VERSION data.
--
-- You can specify version details in a format statement, eg
--  format PE32 
--  [icons, if present]
--  version { "FileDescription","Phix Programming Language",
--            "LegalCopyright","Pete Lomax",
--            "FileVersion","0.6.3",
--            "ProductName","Phix Programming Language",
--            "ProductVersion","0.6.3",
--            "OriginalFilename","pw.exe" }
--  [manifest, if present]
--
--  If no version details are specified the RT_VERSION directory is completely omitted.
--
sequence ResourceRVA
--DEV
--string res
sequence chunk
--string chunk
integer versionstart
sequence verwords
integer ChildrenLengthOffset, 
        StringTableLengthOffset,
        StringLengthOffset
integer padding
integer k, size

    {res,ResourceRVA} = res
    res = rsLabel(res,LabelName)
    versionstart = length(res)
    ResourceRVA = append(ResourceRVA,versionstart)
    chunk = stringify(          --  offset    name                 size value       desc
            {0,0,0,0,           -- 00000544,DataRVA,                h4,0000D554h,   see 0x00000554
             0,0,0,0,           -- 00000548,Size,                   h4,0000025Ch,
             0,0,0,0,           -- 0000054C,Codepage,               h4,00000000h,   (RT_VERSION,1,LANG_NEUTRAL)
             0,0,0,0})          -- 00000550,Reserved,               h4,00000000h,
    res &= chunk
    res = SetField(res,versionstart,DWORD,length(res))
    -- note that Size and VSVERSIONINFO.wLength are set the same (last thing below, using versionstart+4|16)
    res &= stringify(           --  offset    name                    size value    desc
           {0,0,                -- 00000554,VSVERSIONINFO.wLength,      2, 604,
            52,0,               -- 00000556,VSVERSIONINFO.wValueLength, 2, 52,      (a VS_FIXEDFILEINFO structure)
            0,0})               -- 00000558,VSVERSIONINFO.wType,        2, 0,       binary
    res &= stringify(unicode("VS_VERSION_INFO")&{0,0})
    chunk = stringify(          --  offset    name                                     size value       desc
            {#BD,#04,#EF,#FE,   -- 0000057C[00],VS_FIXEDFILEINFO.dwSignature,           h4,FEEF04BDh,   (should be #FEEF04BD)
             1,0,               -- 00000580[04],VS_FIXEDFILEINFO.dwStrucVersHi,         h4,0001h,       1.0
             0,0,               -- 00000582[06],VS_FIXEDFILEINFO.dwStrucVersLo,         h4,0000h,
             0,0,0,0,           -- 00000584[08],VS_FIXEDFILEINFO.dwFileVersionMS,       h4,00000006h,   0.6.3.0
             0,0,0,0,           -- 00000588[0C],VS_FIXEDFILEINFO.dwFileVersionLS,       h4,00030000h,
             0,0,0,0,           -- 0000058C[10],VS_FIXEDFILEINFO.dwProductVersionMS,    h4,00000006h,   0.6.3.0
             0,0,0,0,           -- 00000590[14],VS_FIXEDFILEINFO.dwProductVersionLS,    h4,00030000h,
             0,0,0,0,           -- 00000594[18],VS_FIXEDFILEINFO.dwFileFlagsMask,       h4,00000000h,
             0,0,0,0,           -- 00000598[1C],VS_FIXEDFILEINFO.dwFileFlags,           h4,00000000h,
             4,0,0,0,           -- 0000059C[20],VS_FIXEDFILEINFO.dwFileOS,              h4,00000004h,   VOS__WINDOWS32
             1,0,0,0,           -- 000005A0[24],VS_FIXEDFILEINFO.dwFileType,            h4,00000001h,   VFT_APP
             0,0,0,0,           -- 000005A4[28],VS_FIXEDFILEINFO.dwFileSubtype,         h4,00000000h,
             0,0,0,0,           -- 000005A8[2C],VS_FIXEDFILEINFO.dwFileDateMS,          h4,00000000h,
             0,0,0,0})          -- 000005AC[30],VS_FIXEDFILEINFO.dwFileDateLS,          h4,00000000h,
    if length(chunk)!=52 then ?9/0 end if -- sanity check
    k = find("FileVersion",d)
    if k then
        if and_bits(k,#01)!=1 then ?9/0 end if
        verwords = decode_version_string(d[k+1])
        if length(verwords)>4 then ?9/0 end if
        for i=1 to length(verwords) do
            chunk = SetField(chunk,vwbeetle[i],WORD,verwords[i])
        end for
    end if
    k = find("ProductVersion",d)
    if k then
        if and_bits(k,#01)!=1 then ?9/0 end if
        verwords = decode_version_string(d[k+1])
        if length(verwords)>4 then ?9/0 end if
        for i=1 to length(verwords) do
            chunk = SetField(chunk,vwbeetle[i]+8,WORD,verwords[i])
        end for
    end if
    chunk = SetField(chunk,#20,WORD,fileos)
    chunk = SetField(chunk,#24,WORD,filetype)
    chunk = SetField(chunk,#28,WORD,filesubtype)
    res &= chunk
    ChildrenLengthOffset = length(res)
    res &= stringify(           --  offset    name                    size value desc
           {0,0,                -- 000005B0,Children.wLength,           2,  444 (444=#1BC, +#5B0=#76C)
            0,0,                -- 000005B2,Children.wValueLength,      2,  0,  (should be 0)
            1,0})               -- 000005B4,Children.wType,             2,  1,  text
    res &= stringify(unicode("StringFileInfo"))
    StringTableLengthOffset = length(res)
    res &= stringify(           --  offset    name                    size value desc
           {0,0,                -- 000005D4,StringTable.wLength,        2,  408 (408=#198, +#5D4=#76C)
            0,0,                -- 000005D6,StringTable.wValueLength,   2,  0,  (should be 0)
            1,0})               -- 000005D8,StringTable.wType,          2,  1,  text
    res &= stringify(unicode("040904E4"))
    for i=1 to length(d) by 2 do
        padding = and_bits(length(res),#03)
        if padding then
            res &= stringify(repeat(#90,4-padding))
        end if
        StringLengthOffset = length(res)
        res &= stringify(       --  offset    name                    size value desc
               {0,0,            -- 000005EC,String.wLength,             2,  92, (in bytes)
                0,0,            -- 000005EE,String.wValueLength,        2,  26, (in words)
                1,0})           -- 000005F0,String.wType,               2,  1,  text
        res &= stringify(unicode(d[i]))
        padding = and_bits(length(res),#03)
        if padding then
            res &= stringify(repeat(#90,4-padding))
        end if
        res &= stringify(unicode(d[i+1]))
        res = SetField(res,StringLengthOffset+2,WORD,length(d[i+1])+1)
        res = SetField(res,StringLengthOffset,WORD,length(res)-StringLengthOffset)
    end for
    padding = and_bits(length(res),#03)
    if padding then
        res &= stringify(repeat(#90,4-padding))
    end if
    size = length(res)-StringTableLengthOffset
    res = SetField(res,StringTableLengthOffset,WORD,size)
    size = length(res)-ChildrenLengthOffset
    res = SetField(res,ChildrenLengthOffset,WORD,size)

    ChildrenLengthOffset = length(res)
    res &= stringify(           --  offset    name                size value desc
           {0,0,                -- 0000076C,Children.wLength,       2,  68,
            0,0,                -- 0000076E,Children.wValueLength,  2,  0,  (should be 0)
            1,0})               -- 00000770,Children.wType,         2,  1,  text
    res &= stringify(unicode("VarFileInfo"))
    padding = and_bits(length(res),#03)
    if padding then
        res &= stringify(repeat(0,4-padding))
    end if
    StringLengthOffset = length(res)
    res &= stringify(           --  offset    name                size value desc
           {36,0,               -- 0000078C,wLength,                2,  36, (in bytes)
            4,0,                -- 0000078E,wValueLength,           2,  4,  (in bytes)
            0,0})               -- 00000790,wType,                  2,  0,  binary
    res &= stringify(unicode("Translation"))
    padding = and_bits(length(res),#03)
    if padding then
        res &= stringify(repeat(0,4-padding))
    end if
    chunk = stringify(
            {0,0,               -- lang
             0,0})              -- cp
    chunk = SetField(chunk,#0,WORD,lang)
    chunk = SetField(chunk,#2,WORD,cp)
    res &= chunk
    res = SetField(res,StringLengthOffset,WORD,length(res)-StringLengthOffset)
    res = SetField(res,ChildrenLengthOffset,WORD,length(res)-ChildrenLengthOffset)
    size = length(res)-(versionstart+16)
    res = SetField(res,versionstart+4,DWORD,size)
    res = SetField(res,versionstart+16,WORD,size) -- VSVERSIONINFO.wLength
    return {res,ResourceRVA}
end function

--DEV builtin/autoinclude?
--/*
function substitute_all(string text, string s, string r)
-- replace all instances of s in text with r
integer k = 1, 
        l = length(s),
        startidx = 1
sequence chunks = {}
    while 1 do
        k = match(s,text,k)
        if k=0 then exit end if
--      text[k..k+l-1] = r
        chunks = append(chunks,text[startidx..k-1])
        k += l
        startidx = k
    end while
    if length(chunks) then
        chunks = append(chunks,text[startidx..$])
        text = chunks[1]
        for i=2 to length(chunks) do
            text &= r
            text &= chunks[i]
        end for
    end if
    return text
end function
--*/

--function rsManifest(sequence resRVA, string LabelName, string manifest, integer machine)
function rsManifest(sequence res, string LabelName, string manifest)--, integer machine)
--
-- output the RT_MANIFEST data.
--
-- machine should be M32 or M64. Note that when machine=M64, all instances of "x86" in the
-- manifest are automatically replaced with "amd64" (nb **including** the quotes).
--
-- You can specify a different manifest in a format statement, eg
--  format PE32 
--  [icons, version if present]
--  manifest `
--  <?xml version="1.0" encoding="UTF-8" standalone="yes"?>
--  <assembly xmlns="urn:schemas-microsoft-com:asm.v1" manifestVersion="1.0">
--   <assemblyIdentity name="x.x.x" processorArchitecture="x86" version="5.1.0.0" type="win32"/>
--   <description>no</description>
--   <dependency>
--    <dependentAssembly>
--     <assemblyIdentity type="win32" name="Microsoft.Windows.Common-Controls" version="6.0.0.0" 
--                       processorArchitecture="x86" publicKeyToken="6595b64144ccf1df" language="*" />
--    </dependentAssembly>
--   </dependency>
--  </assembly>`
--
--  The above is the standard manifest used if nothing else is specified.
--  
sequence ResourceRVA
--string res
integer manifestRVA
integer padding
    {res,ResourceRVA} = res
    res = rsLabel(res,LabelName)
    manifestRVA = length(res)
    ResourceRVA = append(ResourceRVA,manifestRVA)
    res &= stringify(       --  offset    name                 size value       desc
           {0,0,0,0,        -- 000007E0,DataRVA,                h4,0000D7F0h,   see 0x000007F0
            0,0,0,0,        -- 000007E4,Size,                   h4,000001FFh,
            0,0,0,0,        -- 000007E8,Codepage,               h4,00000000h,   (RT_MANIFEST,1,LANG_NEUTRAL)
            0,0,0,0})       -- 000007EC,Reserved,               h4,00000000h,
    res = SetField(res,manifestRVA,DWORD,length(res))
--  if machine=M64 then
--DEV see euforum post/todo/format.htm.
--      manifest = substitute_all(manifest,"\"x86\"","\"amd64\"")
--  end if
    res = SetField(res,manifestRVA+4,DWORD,length(manifest))
    res &= manifest 
    padding = and_bits(length(res),#03)
    if padding then
        res &= stringify(repeat(0,4-padding))
    end if
    return {res,ResourceRVA}
end function

constant RT_ICON        = 3,
         RT_GROUP_ICON  = 14,
         RT_VERSION     = 16,
         RT_MANIFEST    = 24,
         LANG_NEUTRAL   = 0,
         LANG_ENGLISH   = #09,
         SUBLANG_DEFAULT = #0400,
         VOS__WINDOWS32 = 4,
         VFT_APP        = 1,
         VFT2_UNKNOWN   = 0

--function resourceSection(sequence resources, integer machine)
function resourceSection(sequence resources)
--
-- Create a resource section containing icons, optionally version details, and a manifset.
-- If you have previous experience with resource sections (/compilers) please bear in mind
-- the above trio is it - there are no menus, dialogs, etc. and multiple language support
-- is (currently) just english and nothing else. Generally speaking, in Phix, the resource
-- section is there for the Operating System, rather than the Application.
--
-- Technical note: The strings below ("icons", "group_icons", etc) are effectively labels
--                  to addresses within the resource section, see rsLabelRef()/rsLabel().
--
sequence res -- {binary,ResourceRVA}
string iconfile -- a filename
sequence icons  -- integer ID/idx of icons (in "") to use
sequence versions -- pairs of strings
string manifest -- a single string
sequence params -- scratch var

--DEV rsLabelInit() (if rsLabels.e created)
    rsLabels = {}
    rsLblRefs = {}
    rsLblAddrs = {}

    {{iconfile, icons},versions,manifest} = resources
    params = {{RT_ICON, "icons"},{RT_GROUP_ICON, "group_icons"}}
    if length(versions) then
        params = append(params,{RT_VERSION, "versions"})
    end if
    params = append(params,{RT_MANIFEST, "manifest"})
    res = rsDirectory(params)
    params = {}
    for i=1 to length(icons) do
        params = append(params,{i,LANG_NEUTRAL,sprintf("icon_data%d",i)})
    end for
    res = rsResource(res,"icons",params)
    res = rsResource(res,"group_icons", {{10, LANG_NEUTRAL, "main_icon"}})
    params = {}
    for i=1 to length(icons) do
        params = append(params,{sprintf("icon_data%d",i),icons[i]})
    end for
    res = rsMultiIcon(res,"main_icon", iconfile, params)
    if length(versions) then
        res = rsResource(res,"versions", {{1,LANG_NEUTRAL,"version"}})
        -- Naturally, if you fancy extending the format statement and passing a few more of these as parameters, feel free.
        res = rsVersionInfo(res, "version",VOS__WINDOWS32,VFT_APP,VFT2_UNKNOWN,LANG_ENGLISH+SUBLANG_DEFAULT,0,versions)
    end if
    res = rsResource(res,"manifest",{{1,LANG_NEUTRAL,"man"}})
--  res = rsManifest(res,"man",manifest,machine)
    res = rsManifest(res,"man",manifest)

    -- finally check that all labels were properly defined and resolved:
    for i=1 to length(rsLabels) do
        if rsLblAddrs[i]=-1 then ?9/0 end if
        if length(rsLblRefs[i]) then ?9/0 end if
    end for
    return res
end function

--DEV (split temporarily)
procedure SetCheckSum(string img, integer SizeOfImage)
atom imagehlp = 0, 
     kernel32,
     xCheckSumMappedFile,
     xGetLastError,
     pChecksums,
     pPE

    imagehlp = open_dll("imagehlp.dll")
    xCheckSumMappedFile = define_c_func(imagehlp,"CheckSumMappedFile",
        {C_PTR,     --  PVOID BaseAddress,
         C_LONG,    --  DWORD FileLength,
         C_PTR,     --  PDWORD HeaderSum,
         C_PTR},    --  PDWORD CheckSum
        C_PTR)      -- PIMAGE_NT_HEADERS
    kernel32 = open_dll("kernel32.dll")
    xGetLastError = define_c_func(kernel32, "GetLastError",
        {},
        C_INT)      -- DWORD
    pChecksums = allocate(8) -- {PE, Calc}
    poke4(pChecksums,{0,0})
    pPE = c_func(xCheckSumMappedFile,{img, SizeOfImage, pChecksums, pChecksums+4})
    if pPE=NULL then pPE=c_func(xGetLastError,{}) ?9/0 end if
    if peek4u(pPE+#58)!=peek4u(pChecksums) then ?9/0 end if
    poke(pPE+#58,peek4u(pChecksums+4))
    free(pChecksums)
end procedure

--procedure SetCheckSum(atom img, integer SizeOfImage)
----DEV (temp)
----if peek(img)='M' then
--  SetCheckSumO(img, SizeOfImage)
----else
----    poke(img,'M')
----    SetCheckSumO(img, SizeOfImage)
----    poke(img,'Z')
----end if
--end procedure

--procedure SetCheckSum2(string img, integer SizeOfImage)
--  SetCheckSumO(img, SizeOfImage)
--end procedure


--DEV (for listing)
global sequence fixedupdata, fixedupcode
--DEV fixme!
--!/**/ #isginfo{fixedupdata,0b1000,MIN,MAX,integer,-2} -- verify this is a string

--DEV string code?
procedure CreatePE(integer fn, integer machine, integer subsystem, integer subvers, sequence imports, sequence exports, sequence relocations, string data, string  code, sequence resources)
--
-- fn should be an open file handle, created with "wb", closed on exit
-- machine should be M32 or M64
-- subsystem should be CUI or GUI
-- subvers should be as per pglobals.e
-- imports should be eg {{"kernel32.dll",{"FormatMessageA","GetLastError","LocalFree"},{#4C,#17,#58}},
--                       {"user32.dll",{"MessageBoxA"},{#69}}},
--          or (rarely) {} if no imports whatsoever are required.
--                          Routine names get automatically [tag]sorted per dll, as required.
--                          The {#4C,#17,#58} and {#69} are link chain starts, after creating the MZ/PE
--                          headers these get patched with the thunk addresses.
-- exports should be eg {"errormsg.dll",{"ShowErrorMessage","ShowLastError"},{#0000000C,#0000004E}}
--                      (where #C,#4E are the corresponding offsets into the code section)
--                      (the names and offsets are automatically [tag]sorted, as required)
--                   or {} if not a dll
--DEV
-- relocations should be {} or {{{code_relocations},{data_relocations}},
--                               {code_relocations},{data_relocations}}}
--                              where each entry is a HighLow offset into the
--                              appropriate section. Should only be required
--                              for 32-bit DLLs, not exes, not 64-bit DLLs. [DEV BLUFF!]
--                              Also, cmiiw, the resource section does not
--                              need or get relocations, even if logically
--                              the DataRVA therein ought to be adjusted.
--                              The first pair of sequences contain code and
--                              data references in the code section, whereas
--                              the second pair are data section references.
--                              References get codeBase or dataBase added.
--                              There is never a relocation section for M64, 
--                              since all addressing is RIP-relative, and 
--                              only one for M32 DLLs (exports!={}).
-- relocations should consist of two pairs of two pairs of sequences ({{{r1},{r2}},{{r3},{r4}}}):
--                  relocations[DATA][DATA],
--                  relocations[DATA][CODE],
--                  relocations[CODE][CODE], and 
--                  relocations[CODE][DATA]
--                  where each rN is a list of relocations in the appropriate section relative
--                  to either BaseOfCode or BaseOfData. For example a mov reg,[gvar] statement
--                  would create an entry in relocations[CODE][DATA]. Note that these lists are
--                  merged ({sort(r1&r2), sort(r3&r4)}) before being passed to peHeader(), for
--                  the .reloc section, but applied indidividually to code/data here.
--                      are relocations in the data section
--                      and relocations[x][DATA] are relocations relative to BaseOfData,
--                      and obviously and likewise CODE/code section/BaseOfCode. Note
--                      these are merged before being passed to peHeader().
-- code and data are raw binary, subject to final fixups once codeBase and dataBase are known.
-- resources is {icons,versions,manifest}, where
--     icons is {filename,{ids}},
--  versions is (an even number of|(name,value)) strings
--  manifest is a single string (xml)
--  for more details/examples, see pglobals.e (default_rs_icon) and/or pe32.fmt
--
integer blocklen
integer codelen
integer datalen
sequence ResourceRVA
integer resourcelen
sequence mergedrelocations
sequence res -- scratch
--sequence block
string block
integer importBase
integer codeBase
integer dataBase
integer resourceBase
integer SizeOfImage
sequence ithunks
--integer padding
integer offset
integer imgidx
--DEV (temp)
--integer thunk, thunkaddr, next
--integer thunk, thunkaddr
atom thunk, thunkaddr
atom next
integer ImageBase
integer newRVA

if use_pini_time then
--  TimeDateStamp = GetLastAccessTime("p.ini")
    TimeDateStamp = GetFileTime("p.ini",LAST_ACCESS_TIME)
else
    TimeDateStamp = SYSTEMTIMEtoDateTimeStamp()
end if
--/*
                pFileTime = allocate(sizeofFILETIME)
                poke4(pFileTime,reverse(nTst))
                nTst = FILETIMEtoDateTimeStamp(pFileTime)
                free(pFileTime)
--*/
--/*
// GetLastWriteTime - Retrieves the last-write time and converts
//                    the time to a string
//
// Return value - TRUE if successful, FALSE otherwise
// hFile      - Valid file handle
// lpszString - Pointer to buffer to receive string

--function GetLastAccessTime(string filename)
--atom hFile
--atom pAccessTime
--atom res
--  hFile = c_func(xCreateFile,{"p.ini", GENERIC_READ, FILE_SHARE_READ, NULL, OPEN_EXISTING, 0, NULL})
--  if hFile == INVALID_HANDLE_VALUE then
--      printf(1,"CreateFile(p.ini) failed\n")-- with %d\n", c_func(xGetLastError,{}))
--      return 0
--  end if
--  pAccessTime = allocate(sizeofFILETIME)
--  if c_func(xGetFileTime,{hFile, NULL, pAccessTime, NULL})=0 then
--      res = printf(1,"GetFileTime(p.ini) failed\n")
--      res = 0
--  else
--      res = FILETIMEtoDateTimeStamp(pAccessTime)
--  end if
--  if c_func(xCloseHandle,{hFile}) then end if
--  free(pAccessTime)
--  return res
--end function

--*/
--??FILETIMEtoDateTimeStamp(atom pFileTime)

--puts(1,"warning, data:=\"\0\0\0\0\", line 1877 pbinary.e\n")
--data = "\0\0\0\0"
--relocations[DATA] = {{},{}}
--relocations[CODE][DATA] = {}
    datalen = length(data)
    codelen = length(code)
--?codelen
--?code [-8..-1]
    resourcelen = length(resources)
    if resourcelen then
--      {resources,ResourceRVA} = resourceSection(resources,machine)
        {resources,ResourceRVA} = resourceSection(resources)
        -- (resourceBase needs to be added to the points in ResourceRVA, once we know what it is)
        resourcelen = length(resources)
    end if
--trace(1)
--  block = peHeader(machine,subsystem,subvers,imports,exports,relocations,datalen,codelen,resourcelen)
--  {block,dataBase,codeBase,resourceBase,SizeOfImage,ithunks} = peHeader(M32,GUI,subvers,imports,exports,relocations,datalen,codelen,resourcelen)
--DEV we are going to need data relocations for refs/symtab etc...
--  if machine=M64 or length(exports)=0 then
    if length(exports)=0 then
        mergedrelocations = {}
    elsif machine=M64 then
--      mergedrelocations = {sort(relocations[1][2]),
--datab4code:
        mergedrelocations = {{},
                             sort(relocations[2][1]&relocations[2][2])}
    else
--      mergedrelocations = {sort(relocations[1][1]&relocations[1][2]),
--                           sort(relocations[2][1]&relocations[2][2])}
        mergedrelocations = {sort(relocations[CODE][CODE]&relocations[CODE][DATA]&relocations[CODE][IMPORTS]),
                             sort(relocations[DATA][CODE]&relocations[DATA][DATA])} -- nb no REFS
--      if length(relocations[2])=3 then
--          mergedrelocations = append(mergedrelocations,relocations[3][2])
--      end if
--      if length(relocations[2][3])!=0 then
--          mergedrelocations = append(mergedrelocations,relocations[2][3][$])
--      end if
    end if
--puts(1,"CreatePE: calling peHeader\n")
    res = peHeader(machine,subsystem,subvers,imports,exports,mergedrelocations,datalen,codelen,resourcelen)
--puts(1,"CreatePE: returned from peHeader\n")
    {block,importBase,dataBase,codeBase,resourceBase,SizeOfImage,ithunks,ImageBase} = res
--DEV (temp, for listing)
    DSvaddr = dataBase
    CSvaddr = codeBase
--DEV fixup thunk (and data?) references?
    -- note: all thunk references are assumed to be in the code section.
    for i=1 to length(imports) do
--DEV temp:
        sequence ii3 = imports[i][3]
        for j=1 to length(imports[i][2]) do
            thunk = ithunks[i][j]
if newEmit and listing then
--printf(1,"%s:%08x (%08x,%08x)\n",{imports[i][2][j],thunk+ImageBase,ImageBase,importBase})
if X64 then
            knownAddr = append(knownAddr,thunk+ImageBase+importBase)
else
            knownAddr = append(knownAddr,thunk+ImageBase)
end if
            knownNames = append(knownNames,imports[i][2][j])
end if
--          thunkaddr = imports[i][3][j]
            thunkaddr = ii3[j]
            while thunkaddr!=0 do
                next = GetField(code,thunkaddr,DWORD)
                if machine=M32 then
                    code = SetField(code,thunkaddr,DWORD,thunk+ImageBase)
                else -- M64, RIP addressing
                    code = SetField(code,thunkaddr,DWORD,thunk-(codeBase-importBase)-(thunkaddr+4))
--                  code = SetField(code,thunkaddr,DWORD,thunk-(thunkaddr+4))
                end if
                thunkaddr = next
            end while
        end for
    end for
--?length(relocations)
    if length(relocations) then
-- 6/12/14 (*2) (all code fixups are dword, all data fixups are qword)
        code = fixup(code,relocations[CODE][CODE],machine,0,codeBase+ImageBase)
--      code = fixup(code,relocations[CODE][CODE],M32,0,codeBase+ImageBase)         -- eg return addresses
        if machine=M32 then
--printf(1,"dATaBase:%08x, codeBase:%08x, d-c:%08x, c-d:%08x\n",{dataBase,codeBase,dataBase-codeBase,codeBase-dataBase})
            code = fixup(code,relocations[CODE][DATA],machine,0,dataBase+ImageBase) -- eg var references
        else -- M64, RIP addressing
--printf(1,"dataBase:%08x, codeBase:%08x, d-c:%08x, c-d:%08x\n",{dataBase,codeBase,dataBase-codeBase,codeBase-dataBase})
            code = fixup(code,relocations[CODE][DATA],machine,1,dataBase-codeBase)
--          code = fixup(code,relocations[CODE][DATA],M32,1,dataBase-codeBase)      -- eg var references
        end if
        data = fixup(data,relocations[DATA][CODE],machine,0,codeBase+ImageBase)     -- (eg symtab[N][S_il])
        data = fixup(data,relocations[DATA][DATA],machine,0,dataBase+ImageBase,1)   -- (eg [nested] constants)
        if DLL then
            data = fixup(data,relocations[DATA][REFS],machine,0,dataBase+ImageBase,1)
        end if
    end if
--if 0 then -- checksum [DEV] (might still want this for linux)
--  puts(fn,block)
--if datab4code then
--  puts(fn,data)
--  padding = RoundToFileAlignment(datalen)-datalen
--  if padding then
----        puts(fn,repeat(0,padding))
--      puts(fn,stringify(repeat(0,padding)))
--  end if
--  puts(fn,code)
--  padding = RoundToFileAlignment(codelen)-codelen
--  if padding then
----        puts(fn,repeat(0,padding))
--      puts(fn,stringify(repeat(0,padding)))
--  end if
--else
--  puts(fn,code)
--  padding = RoundToFileAlignment(codelen)-codelen
--  if padding then
----        puts(fn,repeat(0,padding))
--      puts(fn,stringify(repeat(0,padding)))
--  end if
--  puts(fn,data)
--  padding = RoundToFileAlignment(datalen)-datalen
--  if padding then
----        puts(fn,repeat(0,padding))
--      puts(fn,stringify(repeat(0,padding)))
--  end if
--end if
--  if resourcelen then
--      -- add resourceBase to the points where it is needed
--      for i=1 to length(ResourceRVA) do
--          offset = ResourceRVA[i]
----            resources = SetField(resources,offset,DWORD,GetField(resources,offset,DWORD)+resourceBase)
--          newRVA = GetField(resources,offset,DWORD)+resourceBase
--          resources = SetField(resources,offset,DWORD,newRVA)
--      end for
--      puts(fn,resources)
--      padding = RoundToFileAlignment(resourcelen)-resourcelen
--      if padding then
----            puts(fn,repeat(0,padding))
--          puts(fn,stringify(repeat(0,padding)))
--      end if
--  end if
--  close(fn)
--else
--DEV tryme:
--!/*
--if 01 then

--DEV fixme... ("repeatch" and/or "= resources" messes it up...)    [FIXED 15/1/16, see pilx86/GetSrc()]
--  string img = repeat(' ',SizeOfImage)
--  for i=1 to SizeOfImage do
--      img[i] = '\0'
--  end for
    string img = repeatch('\0',SizeOfImage)
    blocklen = length(block)
    imgidx = blocklen
    img[1..imgidx] = block
    if datab4code then
        img[imgidx+1..imgidx+datalen] = data
        imgidx += RoundToFileAlignment(datalen)
        img[imgidx+1..imgidx+codelen] = code
        imgidx += RoundToFileAlignment(codelen)
    else
        img[imgidx+1..imgidx+codelen] = code
        imgidx += RoundToFileAlignment(codelen)
        img[imgidx+1..imgidx+datalen] = data
        imgidx += RoundToFileAlignment(datalen)
    end if
--  if gexch!=0 then    -- global exception handler (PE64 only)
--      ?9/0
-->
--  end if
    if resourcelen then
        for i=1 to length(ResourceRVA) do
            offset = ResourceRVA[i]
            newRVA = GetField(resources,offset,DWORD)+resourceBase
            resources = SetField(resources,offset,DWORD,newRVA)
        end for
        img[imgidx+1..imgidx+resourcelen] = resources
--      for i=1 to resourcelen do
--          img[imgidx+i] = resources[i]
--      end for
        imgidx += RoundToFileAlignment(resourcelen)
    end if
    if imgidx!=SizeOfImage then ?9/0 end if
    SetCheckSum(img,SizeOfImage)
    puts(fn,img)
    close(fn)
--!*/
--?SizeOfImage
--DEV repeat('\0',SizeOfImage,string)/repeatch()/block&pad(data)&pad(code)&pad(resources)/SetCheckSumS(img)
--else
--  atom img = allocate(SizeOfImage)
----printf(1,"SizeofImage:#%08x\n",SizeOfImage)
--  mem_set(img,0,SizeOfImage)
----    mem_set(img,#90,SizeOfImage)    -- no help :-(
--  poke(img,block)
--  blocklen = length(block)
--  imgidx = length(block)
--  if datab4code then
--      poke(img+imgidx,data)
--      imgidx += RoundToFileAlignment(datalen)
--      poke(img+imgidx,code)
--      imgidx += RoundToFileAlignment(codelen)
--  else
--      poke(img+imgidx,code)
--      imgidx += RoundToFileAlignment(codelen)
--      poke(img+imgidx,data)
----printf(1,"data:#%08x, len:#%08x, datalen:#%08x, rounded:#%08x, ",{imgidx,length(data),datalen,RoundToFileAlignment(datalen)})
--      imgidx += RoundToFileAlignment(datalen)
----printf(1,"-> #%08x\n",{imgidx})
--  end if
--  if resourcelen then
--      for i=1 to length(ResourceRVA) do
--          offset = ResourceRVA[i]
----            resources = SetField(resources,offset,DWORD,GetField(resources,offset,DWORD)+resourceBase)
--          newRVA = GetField(resources,offset,DWORD)+resourceBase
--          resources = SetField(resources,offset,DWORD,newRVA)
--      end for
--      poke(img+imgidx,resources)
--      imgidx += RoundToFileAlignment(resourcelen)
----?imgidx
--  end if
----?{imgidx,SizeOfImage}
--  if imgidx!=SizeOfImage then ?9/0 end if
--  SetCheckSum(img,SizeOfImage)
--  puts(fn,peek({img,SizeOfImage}))
--  close(fn)
----end if
--end if

--DEV
    fixedupdata = data
--!/**/ #isginfo{data,0b1000,MIN,MAX,integer,-2} -- verify this is a string
    fixedupcode = code

end procedure

-- ELF routines

--constant S_OFFSET=1, S_SIZE=2, S_FLAGS=3
constant Read = 4, Write= 2, Execute = 1 -- (for S_FLAGS)

--constant interpreter = "/lib/ld-linux.so.2\0\0",  -- (manually padded to whole dwords/DEV)
string interpreter = "/lib/ld-linux.so.2\0"
string interpreter64 = "/lib64/ld-linux-x86-64.so.2\0"


constant PT_LOAD = 1,
         PT_DYNAMIC = 2,
         PT_INTERP = 3

constant DT_NULL            = 0,
         DT_NEEDED          = 1,
--       DT_PLTRELSZ        = 2,
--       DT_PLTGOT          = 3,
         DT_HASH            = 4,
         DT_STRTAB          = 5,
         DT_SYMTAB          = 6,
         DT_RELA            = 7,
         DT_RELASZ          = 8,
         DT_RELAENT         = 9,
         DT_STRSZ           = 10,
         DT_SYMENT          = 11,
--       DT_INIT            = 12,
--       DT_FINI            = 13,
--       DT_SONAME          = 14,
--       DT_RPATH           = 15,
--       DT_SYMBOLIC        = 16,
         DT_REL             = 17,
         DT_RELSZ           = 18,
         DT_RELENT          = 19,
--       DT_PLTREL          = 20,
--       DT_DEBUG           = 21,
--       DT_TEXTREL         = 22,
--       DT_JMPREL          = 23,
--       DT_BIND_NOW        = 24,
--       DT_INIT_ARRAY      = 25,
--       DT_FINI_ARRAY      = 26,
--       DT_INIT_ARRAYSZ    = 27,
--       DT_FINI_ARRAYSZ    = 28,
--       DT_RUNPATH         = 29,
         $

constant R_386_32 = 1

function elfHeader(integer machine, atom base, sequence imports, integer datalen, integer codelen)
--
-- machine should be M32 or M64
-- imports is eg {{"libdl.so.2",{"dlopen","dlsym"},{28,62}},
--                {"libc.so.6",{"printf"},{91}}}
--              ie a list of names and fixup chains for each DT_NEEDED library.
-- segments should be { {offset,size,flags}, ... } [DEV that there comment is just a wee bit out of date...]
--  Obviously you need to know quite a bit about what the rest of the file
--  contents need to be (especially the segment sizes), before you call this.
--
-- Returns: a sequence containing an elf header and the requested program header table entries,
--          suitable for writing directly to a file previously opened in binary mode, plus
--          codeBase and dataBase values.
--
--DEV string
sequence res, shdr
integer phnum
atom vaddr
atom offset, shtype, size, flags
integer e_entry
integer e_phentsize
integer p_type
integer p_offset
integer p_vaddr
integer p_paddr
integer p_filesz
integer p_memsz
integer p_flags
integer elfHeaderSize
sequence segments
integer dataBase
integer codeBase
integer start
--sequence stringaddrs = imports
string dt = "", dtentry, st, stentry, ht, rt = "", rtentry
integer rtidx = 0
integer dynalen
string dynstrings = "\0"
integer padding
sequence dynfixups = {}
integer interplen
integer datapad

--DEV
ImageBase2 = base
    if machine=M32 then                 
        vaddr = #08048000
        dtentry = stringify(        --  offset    name         size value       desc
                  { 1,0,0,0,        -- 00000000,d_tag           h4, 1           DT_NEEDED
                    0,0,0,0 })      -- 00000004,d_val            4, 1           libc.so.6
        stentry = stringify(        --  offset    name         size value       desc
                  { 0,0,0,0,        -- 00000000,st_name         h4, 0           (eg dlopen)
                    0,0,0,0,        -- 00000004,st_value        h4, 0
                    0,0,0,0,        -- 00000008,st_size          4, 0
                    0,              -- 0000000C,st_info         h1, 0           (12h=STB_GLOBAL,STT_FUNC)
                    0,              -- 0000000D,st_other        h1, 0
                    0,0 })          -- 0000000E,st_shndx        h2, 0
        rtentry = stringify(        --  offset    name         size value       desc
                  { 0,0,0,0,        -- 00000000,r_offset        h4, 0           relocation offset
                    1,0,0,0 })      -- 00000004,r_info          h4, 0           eg 0201h = R_386_32,symtab[2]
        st = stentry
        stentry = SetField(stentry,#C,BYTE,#12) -- STB_GLOBAL, STT_FUNC
        for i=1 to length(imports) do
            dtentry = SetField(dtentry,0,DWORD,DT_NEEDED)
            dtentry = SetField(dtentry,4,DWORD,length(dynstrings))
            dynstrings &= imports[i][1]&'\0'
            dt &= dtentry
            for j=1 to length(imports[i][2]) do
                stentry = SetField(stentry,0,DWORD,length(dynstrings))
                dynstrings &= imports[i][2][j]&'\0'
                st &= stentry                       
                rtidx += 1
                -- (r_offset filled in later)
--              rtentry = SetField(rtentry,5,WORD,rtidx)
                rtentry = SetField(rtentry,4,DWORD,rtidx*#100+R_386_32) -- might be better
                rt &= rtentry
            end for
        end for
        -- pad dynstrings to whole dwords:
        padding = and_bits(length(dynstrings),#03)
        if padding then
            dynstrings &= stringify(repeat('\0',4-padding))
        end if
        
--      dt &= 9/0 -- we've only done DT_NEEDED!

        ht = stringify(repeat('\0',16+rtidx*4))
        ht = SetField(ht,0,DWORD,1)         -- nbucket
        ht = SetField(ht,4,DWORD,rtidx+1)   -- nchain
        for i=1 to rtidx do
            -- chain[0] = 1
            -- ...
            -- chain[n-1] = n
            -- chain[n] = 0
            ht = SetField(ht,(i+2)*4,DWORD,i)
        end for
        
--      rt &= stringify(repeat('\0',rtidx*4))
        dynalen = length(st)+length(ht)+length(dynstrings)+length(rt)+rtidx*4
--      reloc = stringify({0,0,0,0}) --??
                
        dtentry = SetField(dtentry,4,DWORD,0)
--      interpreter = pad(interpreter,iff(machine=32?DWORD:QWORD))
        interpreter = pad(interpreter,DWORD)
        interplen = length(interpreter)
--DEV I think this should be if rtidx=0?? (do we care?)
        if dynalen=0 then
            segments = {{PT_INTERP,interplen,Read}}
        else
            segments = {{PT_INTERP,interplen,Read},
                        {PT_DYNAMIC,length(dt)+9*8,Read},
                        {PT_LOAD,dynalen,Read+Write}}
            interplen += length(dt)+9*8+dynalen
        end if
        elfHeaderSize = #34
        e_phentsize = 32
if datab4code then
        interplen += datalen + (length(segments)+2)*e_phentsize + elfHeaderSize
        datapad = RoundToSectionAlignment(interplen)-interplen
        datalen += datapad
        segments &= {{PT_LOAD,datalen,Read+Write},
                     {PT_LOAD,codelen,Read+Execute}}
else
        ?9/0 --(double padding rqd)
        segments &= {{PT_LOAD,codelen,Read+Execute},
                     {PT_LOAD,datalen,Read+Write}}
end if
        phnum = length(segments)
        res = stringify(                --  offset    name         size value       desc
              { #7F, 'E', 'L', 'F',     -- 00000000,ei_magic,       x4, 0x7F&"ELF", ELF signature
                1,                      -- 00000004,ei_class,       1,  1,          32 bit
                1,                      -- 00000005,ei_data,        1,  1,          little endian
                1,                      -- 00000006,ei_version,     1,  1,          current
                0,                      -- 00000007,ei_osabi,       1,  0,
                0,                      -- 00000008,ei_abiversion,  1,  0,
                0,0,0,0,0,0,            -- 00000009,ei_pad,         h6, 0h,
                0,                      -- 0000000F,ei_size,        h1, 10h
                2,0,                    -- 00000010,e_type,         2,  2,          Executable file (3=Shared object file)
                3,0,                    -- 00000012,e_machine,      2,  3,          Intel 386
                1,0,0,0,                -- 00000014,e_version,      4,  1,          current
                #80,#80,#04,#08,        -- 00000018,e_entry,        h4, 08048080h,  entry point                         -- (set below)
                #34,0,0,0,              -- 0000001C,e_phoff,        h4, 00000034h,  program header table
                0,0,0,0,                -- 00000020,e_shoff,        h4, 00000000h,  section header table
                0,0,0,0,                -- 00000024,e_flags,        h4, 00000000h,
                #34,0,                  -- 00000028,e_ehsize,       h2, 0034h,      ELF header size (32 bit)
                32,0,                   -- 0000002A,e_phentsize,    2,  32,         program header table entry size
                phnum,0,                -- 0000002C,e_phnum,        2,  4,          number of program header entries (segments)
                40,0,                   -- 0000002E,e_shentsize,    2,  40,         section header entry size
                0,0,                    -- 00000030,e_shnum,        2,  0,          number of section header entries
                0,0 })                  -- 00000032,e_shstrndx,     2,  0,          section name string table index
        if length(res)!=elfHeaderSize then ?9/0 end if
        e_entry = #00000018
        e_phentsize = #0000002A
        if GetField(res,e_entry,DWORD)!=#08048080 then ?9/0 end if  -- (sanity check, overwritten below)
--DEV
--      res = SetField(res,e_entry,DWORD,entry_point)
        if GetField(res,e_phentsize,WORD)!=32 then ?9/0 end if
        e_phentsize = 32

        shdr = stringify(               --  offset    name         size value       desc
               {1,0,0,0,                -- 00000000,p_type,         4,  1,          loadable                        -- (set below)
                #80,0,0,0,              -- 00000004,p_offset,       h4, 00000080h,  file offset                     -- (set below)
                #80,#80,#48,#08,        -- 00000008,p_vaddr,        h4, 08048080h,  virtual address                 -- (set below)
                0,0,0,0,                -- 0000000C,p_paddr,        h4, 00000000h,  physical addressing(ignored)
                #24,0,0,0,              -- 00000010,p_filesz,       h4, 00000024h,  bytes in file image             -- (set below)
                #24,0,0,0,              -- 00000014,p_memsz,        h4, 00000024h,  bytes in memory image           -- (set below)
                #05,0,0,0,              -- 00000018,p_flags,        h4, 00000005h,  Execute+Read                    -- (set below)
                #00,#10,0,0})           -- 0000001C,p_align,        h4, 00001000h,
        if length(shdr)!=e_phentsize then ?9/0 end if
        p_type   = #00000000
        p_offset = #00000004
        p_vaddr  = #00000008
        p_paddr  = #0000000C
        p_filesz = #00000010
        p_memsz  = #00000014
        p_flags  = #00000018

        offset = length(res)+phnum*e_phentsize
        for i=1 to phnum do
--  segments = {{#00000080,#00000024,Read+Execute},
--              {#000000A4,#0000000E,Read+Write}
--             }
--          {offset,size,flags} = segments[i]
--          segments[i][1] = offset
            {shtype,size,flags} = segments[i]
            segments[i][1] = vaddr+offset
            shdr = SetField(shdr,p_type,DWORD,shtype)
            shdr = SetField(shdr,p_offset,DWORD,offset)
            shdr = SetField(shdr,p_vaddr,DWORD,vaddr+offset)
            shdr = SetField(shdr,p_paddr,DWORD,vaddr+offset)
            shdr = SetField(shdr,p_filesz,DWORD,size)
            shdr = SetField(shdr,p_memsz,DWORD,size)
            shdr = SetField(shdr,p_flags,DWORD,flags)
            res &= shdr
            offset += size
        end for
--      offset = length(res)
--      size = codelen
--      flags = Read+Execute
--      codeBase = length(res)
        res &= interpreter

        dynalen = vaddr+length(res)+length(dt)+9*8
        dtentry = SetField(dtentry,0,DWORD,DT_SYMTAB)
        dtentry = SetField(dtentry,4,DWORD,dynalen)
        dt &= dtentry
        dtentry = SetField(dtentry,0,DWORD,DT_SYMENT)
        dtentry = SetField(dtentry,4,DWORD,16)
        dt &= dtentry
        dynalen += length(st)
        dtentry = SetField(dtentry,0,DWORD,DT_HASH)
        dtentry = SetField(dtentry,4,DWORD,dynalen)
        dt &= dtentry
        dynalen += length(ht)
        dtentry = SetField(dtentry,0,DWORD,DT_STRTAB)
        dtentry = SetField(dtentry,4,DWORD,dynalen)
        dt &= dtentry
        dtentry = SetField(dtentry,0,DWORD,DT_STRSZ)
        dtentry = SetField(dtentry,4,DWORD,length(dynstrings))
        dt &= dtentry
        dynalen += length(dynstrings)
        dtentry = SetField(dtentry,0,DWORD,DT_REL)
        dtentry = SetField(dtentry,4,DWORD,dynalen)
        dt &= dtentry
        dtentry = SetField(dtentry,0,DWORD,DT_RELSZ)
        dtentry = SetField(dtentry,4,DWORD,8*rtidx)
        dt &= dtentry
        dtentry = SetField(dtentry,0,DWORD,DT_RELENT)
        dtentry = SetField(dtentry,4,DWORD,8)
        dt &= dtentry
        dtentry = SetField(dtentry,0,DWORD,DT_NULL)
        dtentry = SetField(dtentry,4,DWORD,0)
        dt &= dtentry

        dt &= st
        dt &= ht
        dt &= dynstrings

        dynalen = vaddr+length(res)+length(dt)+length(rt)
        for i=1 to rtidx do
            dynfixups = append(dynfixups,dynalen)
            rt = SetField(rt,(i-1)*8,DWORD,dynalen) -- r_offset
            rt &= stringify(repeat('\0',4))
            dynalen += 4
        end for

        dt &= rt
--DEV
--      if vaddr+length(res)+length(dt)!=dynalen then ?9/0 end if
        res &= dt
        start = base+length(res)
        if datab4code then
            start += datalen
        end if
        res = SetField(res,e_entry,DWORD,start)

    elsif machine=M64 then
        vaddr = #00400000
        dtentry = stringify(            --  offset    name         size value       desc
                  { 1,0,0,0,0,0,0,0,    -- 00000000,d_tag           h8, 1           DT_NEEDED
                    0,0,0,0,0,0,0,0 })  -- 00000008,d_val            8, 1           libc.so.6
        stentry = stringify(            --  offset    name         size value       desc
                  { 0,0,0,0,            -- 00000000,st_name         h4, 0           (eg dlopen)
                    0,                  -- 00000004,st_info         h1, 0           (12h=STB_GLOBAL,STT_FUNC)
                    0,                  -- 00000005,st_other        h1, 0           (should be 0)
                    0,0,                -- 0000000E,st_shndx        h2, 0
                    0,0,0,0,0,0,0,0,    -- 00000004,st_value        h8, 0
                    0,0,0,0,0,0,0,0})   -- 00000008,st_size          8, 0
        rtentry = stringify(            --  offset    name         size value       desc
                  { 0,0,0,0,0,0,0,0,    -- 00000000,r_offset        h8, 0           relocation offset
                    0,0,0,0,0,0,0,0,    -- 00000008,r_info          h8, 0           eg 0200000001h = R_X86_64_64,symtab[2]
                    0,0,0,0,0,0,0,0 })  -- 00000008,r_addend        h8, 0
        st = stentry
        stentry = SetField(stentry,#4,BYTE,#12) -- STB_GLOBAL, STT_FUNC
        for i=1 to length(imports) do
            dtentry = SetField(dtentry,0,QWORD,DT_NEEDED)
            dtentry = SetField(dtentry,8,QWORD,length(dynstrings))
            dynstrings &= imports[i][1]&'\0'
            dt &= dtentry
            for j=1 to length(imports[i][2]) do
                stentry = SetField(stentry,0,DWORD,length(dynstrings))
                dynstrings &= imports[i][2][j]&'\0'
                st &= stentry                       
                rtidx += 1
                -- (r_offset filled in later)
                rtentry = SetField(rtentry,8,QWORD,rtidx*#100000000+R_386_32)
                rt &= rtentry
            end for
        end for
        -- pad dynstrings to whole qwords:
        padding = and_bits(length(dynstrings),#07)
        if padding then
            dynstrings &= stringify(repeat('\0',8-padding))
        end if

        ht = stringify(repeat('\0',16+rtidx*4))
        ht = SetField(ht,0,DWORD,1)         -- nbucket
        ht = SetField(ht,4,DWORD,rtidx+1)   -- nchain
        for i=1 to rtidx do
            -- chain[0] = 1
            -- ...
            -- chain[n-1] = n
            -- chain[n] = 0
            ht = SetField(ht,(i+2)*4,DWORD,i)
        end for
        
        dynalen = length(st)+length(ht)+length(dynstrings)+length(rt)+rtidx*8
                
        dtentry = SetField(dtentry,8,QWORD,0)
        interpreter64 = pad(interpreter64,QWORD)
        interplen = length(interpreter64)
        if dynalen=0 then
            segments = {{PT_INTERP,interplen,Read}}
        else
            segments = {{PT_INTERP,interplen,Read},
                        {PT_DYNAMIC,length(dt)+9*16,Read},
                        {PT_LOAD,dynalen,Read+Write}}
            interplen += length(dt)+9*16+dynalen
        end if
        elfHeaderSize = #00000040
        e_phentsize = 56
if datab4code then
        interplen += datalen + (length(segments)+2)*e_phentsize + elfHeaderSize
        datapad = RoundToSectionAlignment(interplen)-interplen
        datalen += datapad
        segments &= {{PT_LOAD,datalen,Read+Write},
                     {PT_LOAD,codelen,Read+Execute}}
else
        ?9/0
        segments &= {{PT_LOAD,codelen,Read+Execute},
                     {PT_LOAD,datalen,Read+Write}}
end if
        phnum = length(segments)

        res = stringify(                --  offset    name         size value       desc
              { #7F, 'E', 'L', 'F',     -- 00000000,ei_magic,       x4, 0x7F&"ELF", ELF signature
                2,                      -- 00000004,ei_class,       1,  2,          64 bit
                1,                      -- 00000005,ei_data,        1,  1,          little endian
                1,                      -- 00000006,ei_version,     1,  1,          current
                0,                      -- 00000007,ei_osabi,       1,  0
                0,                      -- 00000008,ei_abiversion,  1,  0
                0,0,0,0,0,0,            -- 00000009,ei_pad,         h6, 0h
                0,                      -- 0000000F,ei_size,        h1, 0h
                2,0,                    -- 00000010,e_type,         2,  2,          Executable file (3=Shared object file)
                62,0,                   -- 00000012,e_machine,      2,  62,         x86-64 (AMD Opteron)
                1,0,0,0,                -- 00000014,e_version,      4,  1,          current
                #B0,#00,#40,0,0,0,0,0,  -- 00000018,e_entry,        h8, 4000B0h,    entry point                         -- (set below)
                #40,0,0,0,0,0,0,0,      -- 00000020,e_phoff,        h8, 40h,        program header table
                0,0,0,0,0,0,0,0,        -- 00000028,e_shoff,        h8, 0h,         section header table
                0,0,0,0,                -- 00000030,e_flags,        h4, 00000000h,
                #40,0,                  -- 00000034,e_ehsize,       h2, 0040h,      ELF header size (64 bit)
                56,0,                   -- 00000036,e_phentsize,    2,  56,         program header table entry size
                phnum,0,                -- 00000038,e_phnum,        2,  4,          number of program header entries (segments)
                64,0,                   -- 0000003A,e_shentsize,    2,  64,         section header entry size
                0,0,                    -- 0000003C,e_shnum,        2,  0,          number of section header entries
                0,0 })                  -- 0000003E,e_shstrndx,     2,  0,          section name string table index
        if length(res)!=elfHeaderSize then ?9/0 end if
        e_entry = #00000018
        e_phentsize = #00000036

        if GetField(res,e_entry,QWORD)!=#4000B0 then ?9/0 end if    -- (sanity check, overwritten below)
--      res = SetField(res,e_entry,QWORD,entry_point)
        if GetField(res,e_phentsize,WORD)!=56 then ?9/0 end if
        e_phentsize = 56

        shdr = stringify(               --  offset    name         size value       desc
               {1,0,0,0,                -- 00000000,p_type,         4,  1,          loadable                        -- (set below)
                #05,0,0,0,              -- 00000004,p_flags,        h4, 00000005h,  Execute+Read                    -- (set below)
                #B0,0,0,0,0,0,0,0,      -- 00000008,p_offset,       h8, B0h,        file offset                     -- (set below)
                #B0,#00,#40,0,0,0,0,0,  -- 00000010,p_vaddr,        h8, 4000B0h,    virtual address                 -- (set below)
--              #B0,#00,#40,0,0,0,0,0,  -- 00000018,p_paddr,        h8, 4000B0h,    physical addressing(ignored)
                0,0,0,0,0,0,0,0,        -- 00000018,p_paddr,        h8, 0,          physical addressing(ignored)
                #21,0,0,0,0,0,0,0,      -- 00000020,p_filesz,       h8, 21h,        bytes in file image             -- (set below)
                #21,0,0,0,0,0,0,0,      -- 00000028,p_memsz,        h8, 21h,        bytes in memory image           -- (set below)
                #00,#10,0,0,0,0,0,0})   -- 00000030,p_align,        h8, 1000h,
        if length(shdr)!=e_phentsize then ?9/0 end if
        p_type   = #00000000
        p_offset = #00000008
        p_vaddr  = #00000010
        p_paddr  = #00000018
        p_filesz = #00000020
        p_memsz  = #00000028
        p_flags  = #00000004

        if length(segments)!=phnum then ?9/0 end if
        offset = length(res)+phnum*e_phentsize
        for i=1 to phnum do
--          {offset,size,flags} = segments[i]
            {shtype,size,flags} = segments[i]
--          offset = length(res)
--          segments[i][1] = offset
            segments[i][1] = vaddr+offset
            shdr = SetField(shdr,p_type,QWORD,shtype)
            shdr = SetField(shdr,p_offset,QWORD,offset)
            shdr = SetField(shdr,p_vaddr,QWORD,vaddr+offset)
            shdr = SetField(shdr,p_paddr,QWORD,vaddr+offset)
            shdr = SetField(shdr,p_filesz,QWORD,size)
            shdr = SetField(shdr,p_memsz,QWORD,size)
            shdr = SetField(shdr,p_flags,DWORD,flags)
            res &= shdr
            --DEV not entirely sure we need to do this... (or whether we should do the same for 32-bit)
--          vaddr += size+#1000-1
--          vaddr = and_bits(vaddr,#FFFFF000)
            offset += size
        end for

        res &= interpreter64

        vaddr = #00400000
        dynalen = vaddr+length(res)+length(dt)+9*16
        dtentry = SetField(dtentry,0,QWORD,DT_SYMTAB)
        dtentry = SetField(dtentry,8,QWORD,dynalen)
        dt &= dtentry
        dtentry = SetField(dtentry,0,QWORD,DT_SYMENT)
        dtentry = SetField(dtentry,8,QWORD,16)
        dt &= dtentry
        dynalen += length(st)
        dtentry = SetField(dtentry,0,QWORD,DT_HASH)
        dtentry = SetField(dtentry,8,QWORD,dynalen)
        dt &= dtentry
        dynalen += length(ht)
        dtentry = SetField(dtentry,0,QWORD,DT_STRTAB)
        dtentry = SetField(dtentry,8,QWORD,dynalen)
        dt &= dtentry
        dtentry = SetField(dtentry,0,QWORD,DT_STRSZ)
        dtentry = SetField(dtentry,8,QWORD,length(dynstrings))
        dt &= dtentry
        dynalen += length(dynstrings)
        dtentry = SetField(dtentry,0,QWORD,DT_RELA)
        dtentry = SetField(dtentry,8,QWORD,dynalen)
        dt &= dtentry
        dtentry = SetField(dtentry,0,QWORD,DT_RELASZ)
        dtentry = SetField(dtentry,8,QWORD,24*rtidx)
        dt &= dtentry
        dtentry = SetField(dtentry,0,QWORD,DT_RELAENT)
        dtentry = SetField(dtentry,8,QWORD,24)
        dt &= dtentry
        dtentry = SetField(dtentry,0,QWORD,DT_NULL)
        dtentry = SetField(dtentry,8,QWORD,0)
        dt &= dtentry

        dt &= st
        dt &= ht
        dt &= dynstrings

        dynalen = vaddr+length(res)+length(dt)+length(rt)
        for i=1 to rtidx do
            dynfixups = append(dynfixups,dynalen)
            rt = SetField(rt,(i-1)*24,QWORD,dynalen) -- r_offset
            rt &= stringify(repeat('\0',8))
            dynalen += 8
        end for

        dt &= rt
--DEV
--      if vaddr+length(res)+length(dt)!=dynalen then ?9/0 end if
        res &= dt
        start = base+length(res)
        if datab4code then
            start += datalen
        end if
        res = SetField(res,e_entry,QWORD,start)
    else
        ?9/0
    end if

--DEV
--  if length(res)!=elfHeaderSize+phnum*e_phentsize then ?9/0 end if

--  return res          
--  return {res,segments}
--  dataBase = codeBase+codelen
if datab4code then
--  dataBase = segments[1][1]
--  codeBase = segments[2][1]
    dataBase = segments[-2][1]
    codeBase = segments[-1][1]
else
--  codeBase = segments[1][1]
--  dataBase = segments[2][1]
    codeBase = segments[-2][1]
    dataBase = segments[-1][1]
end if
--DEV
BaseOfData2 = dataBase-ImageBase2
SizeOfData2 = datalen
BaseOfCode2 = codeBase-ImageBase2
SizeOfCode2 = codelen
    return {res,codeBase,dataBase,dynfixups,datapad}
end function

procedure CreateELF(integer fn, integer machine, integer base, sequence imports, sequence exports, sequence relocations, string data, string code)
--
-- fn should be an open file handle, created with "wb", closed on exit
-- machine should be M32 or M64
-- base is typically #08048000 for 32-bit and #00400000 for 64-bit, afaict both quite arbitrary.
-- imports is eg {{"libdl.so.2",{"dlopen","dlsym"},{28,62}},
--                {"libc.so.6",{"printf"},{91}}}
--              ie a list of names and fixup chains for each DT_NEEDED library.
--              No attempt is made here to catch attempts to link kernel32.dll from an ELF file, or libc.so.6 from a PE file; instead
--              such checking is [only] performed in the non-bind and non-norun case in pEmit2.e.
-- exports must be {}, for now. [DEV: in much the same way that imports are specified via eg #ilASM{call "kernel32","LoadLibrary"},
--                                      exports are likely to be via "#ilASM{export :name}", rather than "global function name()"]
--DEV (and datab4code)
--! relocations should be {} or {{{code_relocations},{data_relocations}},
--!                              {code_relocations},{data_relocations}}}
-- relocations should be {} or {{{},{data_relocations}},
--                               {code_relocations},{}}}
--                              where each entry is a HighLow offset into the
--                              appropriate section. Should only be required
--                              for 32-bit DLLs, not exes, not 64-bit DLLs. [DEV BLUFF!]
--                              Also, cmiiw, the resource section does not
--                              need or get relocations, even if logically
--                              the DataRVA therein ought to be adjusted.
--                              The first pair of sequences contain code and
--                              data references in the code section, whereas
--                              the second pair are data section references.
--                              References get codeBase or dataBase added.
--                              There is never a relocation section for M64, 
--                              since all addressing is RIP-relative, and 
--                              only one for M32 DLLs (exports!={}).
--
-- code and data are raw binary, subject to final fixups once codeBase and dataBase are known.
--
-- See cwgtLinux.txt (from mpeforth) for an excellent introduction to low-level elf.
--
integer datalen
integer codelen
sequence block
integer dataBase
integer codeBase
sequence dynfixups
integer datapad
integer didx = 1
atom thunk
integer thunkaddr, next

--  if exports!={} then ?9/0 end if -- (temp)
    if exports!={} then
        -- (temp)
        puts(1,"pbinary.e/CreateELF line 2807: exports is\n")
        ?exports
    end if
    datalen = length(data)
    codelen = length(code)
    {block,codeBase,dataBase,dynfixups,datapad} = elfHeader(machine, base, imports, datalen, codelen)
    if datapad!=0 then
        data &= stringify(repeat('\0',datapad))
    end if

--DEV apply dynfixups::
-- (the following is copied from CreatePE, something similar rqd, however I believe dynfixups is "flattened")
--?imports  -- {{"libdl.so.2",{"dlopen","dlsym"},{28,62}},{"libc.so.6",{"printf"},{91}}}
--?dynfixups    -- {134513120,134513124,134513128}
--?dynfixups    -- {#80481E0,#80481E4,#80481E8}
--DEV fixup thunk (and data?) references?
    -- note: all thunk references are assumed to be in the code section.
    for i=1 to length(imports) do
--DEV temp:
        sequence ii3 = imports[i][3]
        for j=1 to length(imports[i][2]) do
            thunk = dynfixups[didx]
if newEmit and listing then
--printf(1,"%s:%08x (%08x,%08x)\n",{imports[i][2][j],thunk+ImageBase,ImageBase,importBase})
if X64 then
--DEV? (check litsings carefully)
            knownAddr = append(knownAddr,thunk)--+ImageBase+importBase)
else
--          knownAddr = append(knownAddr,thunk+ImageBase)
            knownAddr = append(knownAddr,thunk)
end if
            knownNames = append(knownNames,imports[i][2][j])
end if
--          thunkaddr = imports[i][3][j]
            thunkaddr = ii3[j]
            while thunkaddr!=0 do
                next = GetField(code,thunkaddr,DWORD)
                if machine=M32 then
--                  code = SetField(code,thunkaddr,DWORD,thunk+ImageBase)
                    code = SetField(code,thunkaddr,DWORD,thunk)
                else -- M64, RIP addressing
--DEV? (ditto)
--                  code = SetField(code,thunkaddr,DWORD,thunk-(codeBase-importBase)-(thunkaddr+4))
                    code = SetField(code,thunkaddr,DWORD,thunk-(codeBase-0)-(thunkaddr+4))
--                  code = SetField(code,thunkaddr,DWORD,thunk-(thunkaddr+4))
                end if
                thunkaddr = next
            end while
            didx += 1
        end for
    end for

--printf(1,"codeBase=#%08x, dataBase=#%08x\n",{codeBase,dataBase})
--if getc(0) then end if
    puts(fn,block)
--  if length(relocations[CODE][CODE]) then ?9/0 end if
--  if length(relocations[DATA][DATA]) then ?"9/0" end if
--datab4code?
--6/12/14 (*2)
    code = fixup(code,relocations[CODE][CODE],machine,0,codeBase)
--  code = fixup(code,relocations[CODE][CODE],M32,0,codeBase)
    if machine=M32 then
        code = fixup(code,relocations[CODE][DATA],machine,0,dataBase)
    else -- M64, RIP addressing
        code = fixup(code,relocations[CODE][DATA],machine,1,dataBase-codeBase)
--      code = fixup(code,relocations[CODE][DATA],M32,1,dataBase-codeBase)
    end if
    data = fixup(data,relocations[DATA][CODE],machine,0,codeBase)
    data = fixup(data,relocations[DATA][DATA],machine,0,dataBase)
if datab4code then
    puts(fn,data)
    puts(fn,code)
else
    puts(fn,code)
    puts(fn,data)
end if
    close(fn)
--DEV
    fixedupdata = data
--!/**/ #isginfo{data,0b1000,MIN,MAX,integer,-2} -- verify this is a string
    fixedupcode = code
end procedure

function CleanUpImports(sequence imports)
--
-- Remove any entries with a NULL link chain start.
--   eg pfileio3.e defines scroll(), which uses "kernel32","FillConsoleOutputCharacterA";
--      so FillConsoleOutputCharacterA /will/ get added to imports, but if we don't call
--      scroll, no code is emitted, so FillConsoleOutputCharacterA is not actually used,
--      so obviously there is no point including it in the final import table of the exe.
-- imports is eg {{"kernel32.dll",{"FillConsoleOutputCharacterA","GetLastError"},{#4C,#17}},
--                {"user32.dll",{"MessageBoxA"},{#69}}},
--      obviously any 0 in imports[i][3] can go, along with the name of that entry. To
--      continue with our example, if FillConsoleOutputCharacterA were 0 not #4C, drop
--      it, and if GetLastError goes too, drop the whole kernel32.dll entry.
--
integer k
    for i=length(imports) to 1 by -1 do -- (backwards so delete does not skip anything)
        while 1 do
            k = find(0,imports[i][3])
            if k=0 then exit end if
            imports[i][2][k..k] = {}
            imports[i][3][k..k] = {}
        end while
        if length(imports[i][2])=0 then
            imports[i..i] = {}
        end if
    end for
    return imports
end function

global procedure CreateExecutable(integer fn, sequence imports, sequence exports, sequence relocations, string data, string code)
--
-- fn should be an open file handle, created with "wb", which is closed on exit.
-- globals X64/PE/DLL/OptConsole/subvers (from pglobals.e) should be set (see pmain.e/DoFormat)
-- imports and exports should both be {} for ELF [DEV?], otherwise:
-- imports should be eg {{"kernel32.dll",{"FormatMessageA","GetLastError","LocalFree"},{#4C,#17,#58}},
--                       {"user32.dll",{"MessageBoxA"},{#69}}},
--          or (rarely) {} if no imports whatsoever are required.
--                         Routine names get automatically [tag]sorted per dll, as required.
--                         The {#4C,#17,#58} and {#69} are link chain starts; after creating 
--                         the MZ/PE headers these get patched with the rqd thunk addresses.
--                         Any null link chain starts etc are automatically excluded for you.
-- exports should be eg {"errormsg.dll",{"ShowErrorMessage","ShowLastError"},{#0000000C,#0000004E}}
--                      where #0C,#4E are the corresponding offsets into the code section.
--                      The names and offsets are automatically [tag]sorted, as required.
--                   or {} if not a dll
--datab4code:
-- relocations should be {} or {{{code_relocations},{data_relocations}},
--                               {code_relocations},{data_relocations}}}
--                              The first pair of sequences contain code and data references in the
--                              code section respectively, whereas the second pair are data section 
--                              references. Each entry is a HighLow offset into the appropriate 
--                              section. References get codeBase or dataBase added, once we know
--                              what those are. For example {{{},{#0D,#14}},{{},{}}} indicates that
--                              the code section contains two data references. (It might get big!)
--                              Of course for normal executables these just represent a quick fixup
--                              once we know where everything is, but for 32-bit DLLS (64-bit should
--                              get by with RIP-relative addressing) the information is also used to
--                              build a pukka relocation section (ie .reloc/IMAGE_DATA_DIRECTORY[6]).
--                              Also, cmiiw, the resource section does not need or get relocations, 
--                              even if logically the DataRVA therein ought to be adjusted.
-- data and code are raw binary, subject to said final fixups once their base addresses are known.
--
--global integer DLL = 0 --DEV ([from pglobals.e] unused as yet)
integer machine
integer base
sequence resources
integer subsystem
    machine = M32
    base = #08048000
    if X64 then
        machine = M64
        base = #00400000
    end if
    if PE then
        subsystem = GUI
        if OptConsole then
            subsystem = CUI
        end if
        imports = CleanUpImports(imports)
--DEV (temp, evo-gen false positive goes away when I do this...)
        resources = {rs_icon,rs_version,rs_manifest}
--      resources = {rs_icon,{},rs_manifest}
--      resources = {}
        CreatePE(fn, machine, subsystem, subvers, imports, exports, relocations, data, code, resources)
--DEV (temp)
--puts(1,"\n")
--for i=1 to length(glboffset) do
--  printf(1,"%s: %08x, ",{glblname[i],glboffset[i]})
--end for
--puts(1,"\n")
--printf(1,"BaseOfCode2: %08x\n",{BaseOfCode2})
--printf(1,"BaseOfData2: %08x\n",{BaseOfData2})
--printf(1,"ImageBase2: %08x\n",{ImageBase2})
    else -- elf
--?{imports, exports, relocations}
--C:\Program Files (x86)\Phix>p -d -nodiag e03
--{{{"libdl.so.2",{"dlopen","dlsym"},{28,62}},      -- THERE SHE BLOWS!
--  {"libc.so.6",{"printf"},{91}}},
-- {},
-- {{{3828,4027,4161,4232,4283,4422,4473,4670,4833,4932,5053,8524,8969,9218,9298,9363,9427},
--   {16,40,51,70,81,97,106,2836,2846,2856,2866,2876,2883,2890,2897,2904,2911,2918,4537,5143,   -- CODE,DATA
--    5199,5205,5211,5218,5223,5347,5470,5935,5958,5964,6010,6146,6202,6384,7844,7853,7864,
--    7922,8500,8509,8595,8606,8631,8656,8681,8706,8731,8756,8792,8799,8809,8979,9091,9256,
--    9413,9437,9450,9572,9583,9600}},
--  {{2132,2136,2140,2144,2148,2152,2156,2212,2216,2220,2224,2228,2232,2236,2240,2244,2248,
--    2252,2256,2260,2264,2268,2272,2276,2280,2284,2288,2292,2296,2300,2304,2308,2312,2316,
--    2320,2324,2328,2332,2336,2340,2344,2348,2352,2356,2360,2364,2388,2392,2396,2400,2404,
--    2408,2412,2416,2420,2424,2428,2432,2436,2440,2444,2448,2452,2508,2512,2516,2520,2524,
--    2528,2532,2536,2540,2552,2556,2560,2564,2568,2572,2576,2580,2644,2648,2652,2656,2680,
--    2852,3032,3500,3688,3928,4684,6012,6156,6592,7428,8380,9048,9344,9624,10152,10324,11252,11784},
--   {8,44,192,40,148,48,52,56,68,60,64,36,152,184,1044,1048,1052,1324,1240,1376,1244,1432,     -- DATA,DATA
--    1248,1484,1252,1536,1256,1596,1260,1648,1264,1704,1268,1760,1272,1816,1276,1872,1280,
--    1924,1284,1980,1288,2032,1292,2084,1296,2856,3036,3504,3692,3932,4688,6016,6160,6596,
--    7432,8384,9052,9348,9628,10156,10328,11256,11788,304,276,280,292,296,320,324,328,332,
--    336,340,344,348,352,356,360,364,368,372,376,380,384,388,392,396,400,404,408,412,416,
--    420,424,428,432,436,440,444,448,452,456,460,464,468,472,476,480,484,488,492,496,500,
--    504,508,512,516,520,524,528,532,536,540,544,548,552,556,560,564,568,572,576,580,584,
--    588,592,596,600,604,608,612,616,620,624,628,632,636,7412,6576,8364,11768,11236,10308,
--    10136,9608,9328,9032,6140,5996,4668,3912,3672,3484,3016,2836,8840,2936,7052,6884,6996,
--    7220,7164,7108,6940,7276,7388,9776,9944,6440,6496,6272,6328,6384,4196,4252,4308,4084,
--    4364,4532,4420,6772,6828,6716,4476,6552,8340,4588,10228,11156,7668,6660,8952,10056,
--    7948,7892,11688,9720,9888,8784,8060,8004,8448,8896,8728,8616,7612,8172,8504,7556,7836,
--    7780,8116,4140,8672,7724,9832,10000}}}}
--#1C = 28
        CreateELF(fn, machine, base, imports, exports, relocations, data, code)
    end if
end procedure

