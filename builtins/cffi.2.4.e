--
--  cffi.2.4.e
--  ==========
--   (Version I got working on RDS Eu 2.4 before deciding it wasn't a great idea)
--
--      Eliminates most of the byte-counting needed when interfacing to C routines.
--      The motivation behind this was that while I had many of the windows API
--      structures laboriously translated to offsets for 32-bit from many years
--      ago, they would all have otherwise needed re-doing for 64-bit.
--
--      I must stress that any savings are limited to converting C header (.h) files,
--      or more accruately fragments copied verbatim from said, which would otherwise 
--      have to be done line-by-line manually. There is no new fancy slick syntax to 
--      help with the code that follows, or anything remotely like that.
--
--      Structures:
--      ===========
--          id = define_struct(s) parses C struct definitions into sizes/offsets etc.
--          pStruct = allocate_struct(id)   -- (invoke free(pStruct) when done)
--          set_struct_field(id,pStruct,"name",value)
--          value = get_struct_field(id,pStruct,"name")
--          {offset,size,sign} = get_field_details(id,"name")
--          size = get_struct_size(id)
--
--      Routine deinition:
--      ==================
--          integer rid = define_cfunc(object lib, string cdef)
--          integer rid = define_cproc(object lib, string cdef)
--
--
--      Example 1 (structures):
--          constant tRECT = """
--                              typedef struct _RECT {
--                                LONG left;
--                                LONG top;
--                                LONG right;
--                                LONG bottom;
--                              } RECT, *PRECT;""",
--                   tPS = """
--                              typedef struct tagPAINTSTRUCT {
--                                HDC  hdc;
--                                BOOL fErase;
--                                RECT rcPaint;
--                                BOOL fRestore;
--                                BOOL fIncUpdate;
--                                BYTE rgbReserved[32];
--                              } PAINTSTRUCT, *PPAINTSTRUCT;"""
--          idRECT = define_struct(tRECT)
--          idPS = define_struct(tPS)
--          pPS = allocate_struct(idPS)
--          ...
--          {pRECT,size,sign} = get_field_details(idPS,"rcPaint.left")
--          pokeN(pRECT,{1,2,3,4},size)
--          set_struct_field(idPS,"rcPaint.right",450)
--          ?peekNS({pRECT,4},size,sign)    -- displays {1,2,450,4}
--          ?get_struct_field(idRECT,pRECT,"right") -- displays 450
--          ?get_struct_field(idPS,pPS,"rcPaint.right") -- displays 450
--
--      Notes:  tPS can use "RECT", but only after tRECT has been processed.
--              set/get_struct_field only deal with atoms. As per the comments
--              at the start of parse_c_struct(), any sequence/string handling
--              is the domain of the calling code, using address of element 1.
--              there is no "rcPaint", instead we have "rcPaint.left" etc.
--              I could equally have used:
--              {pRECT} = get_field_details(idPS,"rcPaint.left")
--              poke4(pRECT,{1,2,3,4})
--              ?peek4s({pRECT,4})
--              assuming I am confident that rcPaint.left etc are dwords.
--              allocate_struct() zero-fills the allocated memory.
--              get_struct_size() might be useful if you want to allocate
--              several structures contiguously, without creating a new
--              C-style struct to represent that.
--              typos in field names and the like cause fatal errors in these
--              routines, rather than returning some kind of error code. It 
--              may be possible to replicate the builtins/VM/pcfuncN.e/fatalN
--              mechanism in here to get errors on more appropriate lines. [DEV]
--
--
--      Example2:
--          constant func3="""
--          int WINAPI MessageBox(
--            _In_opt_  HWND hWnd,
--            _In_opt_  LPCTSTR lpText,
--            _In_opt_  LPCTSTR lpCaption,
--            _In_      UINT uType
--          );
--          """
--          set_unicode(0)
--          constant xMessageBox = define_cfunc("user32.dll",func3)
--          ?c_func(xMessageBox,{0,"text","caption",0})
--
--      Notes:  set_unicode() is needed before any auto "A/W" handling.
--              0 (ansi) is recommended: it is more about what your code is
--              going to do than whether you're on 32/64 bit or somesuch.
--              a proc is expected to begin with a "void" return type.
--              lib can be a string or the previous result of open_dll.
--              pass by value (eg/ie RECT not RECT*) is not supported.
--              define_c_func etc is rather carefree regarding type: if
--              is a 4 byte dword (or an 8 byte qword), it does not care 
--              whether it is signed, a BOOL, an INT, a PTR, etc.
--
--
-- The following are not supported:
--  #pragma pack(). This only works for standard alignment.
--  __attribute__((packed));    ditto
--  #if             Preprocessing directives must be manually edited away or commented out.
--  int flags:3;    Bitfields (well, bitfield alignments) are not supported.
--                  (see http://www.catb.org/esr/structure-packing/#_structure_alignment_and_padding 
--                      [bitfields are covered in the second half of point 5])
--  long double.    Size and alignment are compiler- and platform- dependent, details below.
--  int_leastN_t,   \
--  int_fastN_t.     These all imply that the actual size is unknown.
--  intmax_t.       /
--  char (*pc)[10]  this is a single ptr; char *pc[10] is ten pointers.
--                  for now (you'll get an error), rewrite it as char *pc
--  "raw" unions    They must be wrapped in (part of) a struct, for now.
--  (UNICODE)       I mean the #define thing: either invoke set_unicode(0|1)
--                  prior to using either of the following, or replace as:
--                      TBYTE -> WCHAR,CHAR
--                      TCHAR -> WCHAR,CHAR
--                  likewise set_unicode() to auto_select eg MessageBoxA/W.
--   forward
--  references      sub-structures must be previously defined
--  "...",va_list   Beyond my ken, pure gobbledegook to me.
--  static          Obviously I've never really used C...
--  volatile        Ditto
--
--DEV/SUG/?:
--  Note that "int(*)(int, int)" is a C function pointer type, whereas "int(int, int)" is a
--   C function type. Either can be specified to ffi.callback() and the result is the same.
--

with trace
--/* -- OpenEuphoria/RDS Eu compatability:
type string(object s)
    return sequence(s)
end type
procedure pokeN(atom addr, object v, integer size)
    if size=1 then
        poke(addr,v)
    elsif size=2 then   
        poke(addr, v) -- note: only the lower 8 bits are stored
        poke(addr+1, floor(v/#100))
    elsif size=4 then
        poke4(addr,v)
    elsif size=8 then
        poke4(addr,v)
        poke4(addr+4,floor(v/#100000000))
    else
        ?9/0
    end if
end procedure
function peekNS(object addr, integer size, integer signed)
object res
integer len
    if atom(addr) then
        if signed then
            if size=1 then
                res = peek(addr)
                if res>#7F then
                    res -= #100
                end if
            elsif size=2 then   
                res = peek(addr) + peek(addr+1)*#100
                if res>#7FFF then
                    res -= #10000
                end if
            elsif size=4 then
                res = peek4s(addr)
            elsif size=8 then
    --          res = peek8s(addr)
                res = peek4u(addr) + peek4u(addr+4)*#100000000
                if res>#7FFFFFFFFFFF then
                    res -= #10000000000000000
                end if
            else
                ?9/0
            end if
        else
            if size=1 then
                res = peek(addr)
            elsif size=2 then   
                res = peek(addr) + peek(addr+1)*#100
            elsif size=4 then
                res = peek4u(addr)
            elsif size=8 then
    --          res = peek8u(addr)
                res = peek4u(addr) + peek4u(addr+4)*#100000000
            else
                ?9/0
            end if
        end if
    else
        if length(addr)!=2 then ?9/0 end if
        len = addr[2]
        addr = addr[1]
        if not atom(addr) then ?9/0 end if
        res = repeat(0,len)
        for i=1 to len do
            res[i] = peekNS(addr,size,signed)
            addr += size
        end for
    end if
    return res
end function
include misc.e      -- LINUX
include dll.e       -- C_INT
constant C_DWORD = C_INT
constant C_PTR = C_POINTER
include get.e       -- wait_key
include machine.e   -- allocate
--include pcolumn.e -- columnize    ** NB Phix version **
function machine_bits()         -- **NB comment out the ifdef for RDS Eu **
--  ifdef BITS64 then  
--      return 64
--  elsedef  
        return 32
--  end ifdef 
end function
--*/

-- RDS Eu 2.4 compatible (no param defaults) version of the one in pcolumn.e:
function columnize24(sequence source, object columns, object defval)
--
-- Convert a set of sequences into a set of columns.
--
-- Any atoms found in source are treated as if they were a 1-element sequence.
-- The optional columns parameter can be a specific column number or an ordered set.
-- The default value is used when some elements of source have different lengths.
--
-- Examples
--  ?columnize24({{1, 2}, {3, 4}, {5, 6}})          -- {{1,3,5}, {2,4,6}}
--  ?columnize24({{1, 2}, {3, 4}, {5, 6, 7}})       -- {{1,3,5}, {2,4,6}, {0,0,7}}
--  ?columnize24({{1}, {2}, {3, 4}},defval:=-999)   -- {{1,2,3}, {-999,-999,4}}
--  ?columnize24({{1, 2}, {3, 4}, {5, 6, 7}}, 2)        -- {{2,4,6}}
--  ?columnize24({{1, 2}, {3, 4}, {5, 6, 7}}, {2,1})    -- {{2,4,6}, {1,3,5}}
--  ?columnize24({"abc", "def", "ghi"},defval:=' ') -- {"adg", "beh", "cfi" }
--
--  (Specifying a default value of <SPACE> on the last example causes it to
--   return a sequence of strings, rather than dword-sequences, which might
--   display as {{97,100,103},{98,101,104},{99,102,105}}, or cause unwanted
--   type checks if the result is later processed using a string variable.)
--
sequence result
integer ncolumns, col
object sj
integer k

    if not sequence(columns) then
        columns = {columns}
    end if

    ncolumns = length(columns)
    if ncolumns=0 then
        ncolumns = 0
        for j=1 to length(source) do
            sj = source[j]
            if atom(sj) then
                if ncolumns=0 then
                    ncolumns = 1
                end if
            else
                k = length(sj)
                if ncolumns<k then ncolumns = k end if
            end if
        end for
        for i=1 to ncolumns do
            columns &= i
        end for
    end if

    result = repeat(repeat(defval,length(source)), ncolumns)
    for i=1 to ncolumns do
        col = columns[i]
        for j=1 to length(source) do
            sj = source[j]
            if atom(sj) then
                if col=1 then
                    result[i][j] = sj
                end if
            elsif col<=length(sj) then
                result[i][j] = sj[col]
            end if
        end for
    end for

    return result
end function

procedure err(string msg)
    puts(1,msg)
    ?9/0
end procedure

string s
integer ch
integer sidx

--procedure nch(object msg="eof")
procedure nch(object msg)
    sidx += 1
    if sidx>length(s) then
        if string(msg) then
            err(msg)
        else
            ch = -1
        end if
    else
        ch = s[sidx]
    end if
end procedure

--procedure skipspaces(object msg="eof")
procedure skipspaces(object msg)
integer k
    while 1 do
        while find(ch," \t\r\n")!=0 do nch(msg) end while
        if ch!='/' then exit end if
--      if match("/*",s,sidx)=sidx then
        if sidx<length(s) and s[sidx+1]='*' then
--          k = match("*/",s,sidx+2)
            k = 0
            for i=sidx+2 to length(s)-1 do
                if s[i]='*' and s[i+1]='/' then
                    k = i
                    exit
                end if
            end for
            if k=0 then err("missing closing comment") end if
            sidx = k+1
            nch(msg)
        else
--          if match("//",s,sidx)!=sidx then exit end if
            if sidx>=length(s) or s[sidx+1]!='/' then exit end if
            while find(ch,"\r\n")=0 do nch(msg) end while
        end if
    end while
end procedure

function stoken()
integer tokstart, tokend
    if ch=-1 then err("eof") end if
--  skipspaces("eof")
    tokstart = sidx
    if find(ch,"{;*}[]:,()")!=0 then
        tokend = sidx
        nch(0)
    else
        while 1 do
            nch("eof")
            if find(ch," \t\r\n{;*}[]:,()")!=0 then exit end if
        end while
        tokend = sidx-1
    end if
    skipspaces(0)
    return s[tokstart..tokend]
end function

global constant S32=1, S64=2

-- a "long" is 32 bits on Windows64, but 64 bits on Linux64 (!!)
integer L
    L = 4
if platform()=LINUX then
    L = 8
end if

-- entries are {name, signed, 32-bit size, 64-bit size}:
--  (this table is my own choice of the minimal/core types)
--constant {SizeNames,SizeSigns,Sizes} = columnize24({
sequence SizeNames,SizeSigns,Sizes,Stemp
--  {SizeNames,SizeSigns,Sizes} = columnize24({
    Stemp = columnize24({
                                                  {"char",      1,{1,1}},
                                                  {"uchar",     0,{1,1}},
                                                  {"short",     1,{2,2}},
                                                  {"ushort",    0,{2,2}},
                                                  {"int",       1,{4,4}},
                                                  {"uint",      0,{4,4}},
                                                  {"long",      1,{4,L}},
                                                  {"ulong",     0,{4,L}},
                                                  {"float",     1,{4,4}},
                                                  {"double",    1,{8,8}},       -- (4-byte aligned on 32-bit linux, unless -malign-double specified)
                                                  {"int64",     1,{8,8}},       -- aka long long (windows only?)
                                                  {"uint64",    0,{8,8}},
--                                                {"longdouble",1,{8,8}},       -- ambiguous!!!, see below
--                                                {"ptr",       0,{4,8}},       -- (no point having ptr/uptr that I can think of)
--                                                $})
                                                  {"ptr",       0,{4,8}}},{},0) -- (no point having ptr/uptr that I can think of)
    SizeNames = Stemp[1]
    SizeSigns = Stemp[2]
    Sizes     = Stemp[3]


--The wikipedia definition of long double ( https://en.wikipedia.org/wiki/Data_structure_alignment ):
-- on 32 bit:
--A long double (ten bytes with C++Builder and DMC, eight bytes with Visual C++, twelve bytes with GCC) will be 
--  8-byte aligned with C++Builder, 2-byte aligned with DMC, 8-byte aligned with Visual C++ and 4-byte aligned with GCC.
-- on 64 bit:
--A long double (eight bytes with Visual C++, sixteen bytes with GCC) will be 8-byte aligned with Visual C++ and 16-byte aligned with GCC.
-- (PL: Note that Windows is an LLP64 data model, not LP64 which the above article seems to be writing about.)

constant as_char = find("char",SizeNames)
constant as_uchar = find("uchar",SizeNames)
constant as_int = find("int",SizeNames)
constant as_uint = find("uint",SizeNames)
constant as_ptr = find("ptr",SizeNames)
constant as_short = find("short",SizeNames)
constant as_ushort = find("ushort",SizeNames)
constant as_long = find("long",SizeNames)
constant as_ulong = find("ulong",SizeNames)
constant as_float = find("float",SizeNames)
constant as_int64 = find("int64",SizeNames)
constant as_uint64 = find("uint64",SizeNames)

-- From MSDN. Suspect items are commented out, please check results carefully if you uncomment them.
--              (not that there is any warranty that the others are all perfect!)
--sequence {AltNames,AltSize} = columnize24({
sequence AltNames,AltSize,AltTemp
--  {AltNames,AltSize} = columnize24({
    AltTemp = columnize24({
                                         {"PWORD",          as_ptr},
                                         {"PDWORD",         as_ptr},
--m                                      {"LPWORD",         as_ptr},    -- now covered by match("LP",mtype)=1
--m                                      {"LPBYTE",         as_ptr},
                                         {"PBYTE",          as_ptr},
                                         {"PBOOLEAN",       as_ptr},
                                         {"HANDLE",         as_ptr},
                                         {"HACCEL",         as_ptr},
                                         {"HBITMAP",        as_ptr},
                                         {"HBRUSH",         as_ptr},
                                         {"HCOLORSPACE",    as_ptr},
                                         {"HCONV",          as_ptr},
                                         {"HCONVLIST",      as_ptr},
                                         {"HDC",            as_ptr},
                                         {"HDDEDATA",       as_ptr},
                                         {"HDESK",          as_ptr},
                                         {"HDROP",          as_ptr},
                                         {"HDWP",           as_ptr},
                                         {"HENHMETAFILE",   as_ptr},
                                         {"HFONT",          as_ptr},
                                         {"HGDIOBJ",        as_ptr},
                                         {"HGLOBAL",        as_ptr},
                                         {"HHOOK",          as_ptr},
                                         {"HICON",          as_ptr},
                                         {"HCURSOR",        as_ptr},
                                         {"HINSTANCE",      as_ptr},
                                         {"HKEY",           as_ptr},
                                         {"HKL",            as_ptr},
                                         {"HLOCAL",         as_ptr},
                                         {"HMENU",          as_ptr},
                                         {"HMETAFILE",      as_ptr},
                                         {"HMODULE",        as_ptr},
                                         {"HMONITOR",       as_ptr},
                                         {"HPALETTE",       as_ptr},
                                         {"HPEN",           as_ptr},
                                         {"HRGN",           as_ptr},
                                         {"HRSRC",          as_ptr},
                                         {"HSZ",            as_ptr},
                                         {"HWINSTA",        as_ptr},
                                         {"HWND",           as_ptr},
--m                                      {"LPBOOL",         as_ptr},
--m                                      {"LPCOLORREF",     as_ptr},
--m                                      {"LPCSTR",         as_ptr},
--m                                      {"LPCTSTR",        as_ptr},
--m                                      {"LPCVOID",        as_ptr},
--m                                      {"LPCWSTR",        as_ptr},
--m                                      {"LPDWORD",        as_ptr},
--m                                      {"LPHANDLE",       as_ptr},
--m                                      {"LPINT",          as_ptr},
--m                                      {"LPLONG",         as_ptr},
--m                                      {"LPSTR",          as_ptr},
--m                                      {"LPVOID",         as_ptr},
--m                                      {"LPWSTR",         as_ptr},
                                         {"PBOOL",          as_ptr},
                                         {"PCHAR",          as_ptr},
                                         {"PCSTR",          as_ptr},
                                         {"PCTSTR",         as_ptr},
                                         {"PCWSTR",         as_ptr},
                                         {"PDWORDLONG",     as_ptr},
                                         {"PDWORD_PTR",     as_ptr},
                                         {"PDWORD32",       as_ptr},
                                         {"PDWORD64",       as_ptr},
                                         {"PFLOAT",         as_ptr},
--                                       {"PHALF_PTR",      as_ptr},
                                         {"PHANDLE",        as_ptr},
                                         {"PHKEY",          as_ptr},
                                         {"PINT",           as_ptr},
                                         {"PINT_PTR",       as_ptr},
                                         {"PINT8",          as_ptr},
                                         {"PINT16",         as_ptr},
                                         {"PINT32",         as_ptr},
                                         {"PINT64",         as_ptr},
                                         {"PLCID",          as_ptr},
                                         {"PLONG",          as_ptr},
                                         {"PLONGLONG",      as_ptr},
                                         {"PLONG_PTR",      as_ptr},
                                         {"PLONG32",        as_ptr},
                                         {"PLONG64",        as_ptr},
                                         {"PSHORT",         as_ptr},
--                                       {"PPSIZE_T",       as_ptr},
--                                       {"PSSIZE_T",       as_ptr},
                                         {"PSTR",           as_ptr},
                                         {"PTBYTE",         as_ptr},
                                         {"PTCHAR",         as_ptr},
                                         {"PTSTR",          as_ptr},
                                         {"PUCHAR",         as_ptr},
--                                       {"PUHALF_PTR",     as_ptr},
                                         {"PUINT",          as_ptr},
                                         {"PUINT_PTR",      as_ptr},
                                         {"PUINT8",         as_ptr},
                                         {"PUINT16",        as_ptr},
                                         {"PUINT32",        as_ptr},
                                         {"PUINT64",        as_ptr},
                                         {"PULONG",         as_ptr},
                                         {"PULONGLONG",     as_ptr},
                                         {"PULONG_PTR",     as_ptr},
                                         {"PULONG32",       as_ptr},
                                         {"PULONG64",       as_ptr},
                                         {"PUSHORT",        as_ptr},
                                         {"PVOID",          as_ptr},
                                         {"PWCHAR",         as_ptr},
                                         {"PWSTR",          as_ptr},
--                                       {"SC_HANDLE",      as_ptr},
--                                       {"SC_LOCK",        as_ptr},
--                                       {"SERVICE_STATUS_HANDLE",as_ptr},
--                                       {"UHALF_PTR",      as_ptr},
                                         {"UINT_PTR",       as_ptr},
                                         {"WPARAM",         as_ptr},
                                         {"LONG_PTR",       as_ptr},
--                                       {"LPARAM",         as_ptr},
                                         {"LRESULT",        as_ptr},
--                                       {"SSIZE_T",        as_ptr},
--                                       {"SIZE_T",         as_ptr},
                                         {"DWORD_PTR",      as_ptr},
                                         {"ULONG_PTR",      as_ptr},
                                         {"INT_PTR",        as_ptr},
--                                       {"POINTER_SIGNED", as_ptr},
--                                       {"POINTER_UNSIGNED",as_ptr},
--                                       {"HALF_PTR",       as_ptr?},
--                                       {"POINTER_32",     as_ptr?},
--                                       {"POINTER_64",     as_ptr?},
                                         {"WNDPROC",        as_ptr},
                                         {"BYTE",           as_char},
                                         {"BOOLEAN",        as_char},
                                         {"CCHAR",          as_char},
                                         {"CHAR",           as_char},
                                         {"INT8",           as_char},
--                                       {"int8_t",         as_[u]char},
                                         {"UCHAR",          as_uchar},
                                         {"UINT8",          as_uchar},
                                         {"WORD",           as_short},
                                         {"ATOM",           as_short},
                                         {"LANGID",         as_short},
                                         {"INT16",          as_short},
                                         {"SHORT",          as_short},
--                                       {"int16_t",        as_[u]short},
                                         {"UINT16",         as_ushort},
                                         {"USHORT",         as_ushort},
                                         {"WCHAR",          as_ushort},
                                         {"BOOL",           as_int},
                                         {"HFILE",          as_int},
                                         {"INT",            as_int},
                                         {"INT32",          as_int},
                                         {"LONG32",         as_int},
--                                       {"int32_t",        as_[u]int},
                                         {"REGSAM",         as_int},        -- assumes C enums are signed ints
                                         {"DWORD32",        as_uint},
                                         {"UINT",           as_uint},
                                         {"UINT32",         as_uint},
                                         {"ULONG32",        as_uint},
                                         {"LONG",           as_long},
                                         {"HRESULT",        as_long},
                                         {"DWORD",          as_ulong},
                                         {"COLORREF",       as_ulong},
                                         {"LCID",           as_ulong},
                                         {"LCTYPE",         as_ulong},
                                         {"LGRPID",         as_ulong},
                                         {"ULONG",          as_ulong},
                                         {"FLOAT",          as_float},
                                         {"INT64",          as_int64},
                                         {"LONGLONG",       as_int64},
                                         {"LONG64",         as_int64},
--                                       {"USN",            as_int64},
--                                       {"int64_t",        as_[u]int64},
                                         {"DWORDLONG",      as_uint64},
                                         {"DWORD64",        as_uint64},
                                         {"QWORD",          as_uint64},
                                         {"UINT64",         as_uint64},
                                         {"ULONGLONG",      as_uint64},
--                                       {"ULONG64",        as_uint64},
--                                       $})
                                         {"ULONG64",        as_uint64}},{},0)
    AltNames = AltTemp[1]
    AltSize  = AltTemp[2]

integer unicode
        unicode = -1    -- -1 = unknown
                        --  0 = ansi
                        -- +1 = unicode

-- (this ended up a bit overkill, nevermind)
--constant {UnicodeNames,Uon,Uoff} = columnize24({
sequence UnicodeNames,Uon,Uoff,Utemp
--  {UnicodeNames,Uon,Uoff} = columnize24({
    Utemp = columnize24({
                                              {"TBYTE",as_ushort,as_char},
--                                            {"TCHAR",as_ushort,as_char},
--                                            $})
                                              {"TCHAR",as_ushort,as_char}},{},0)
    UnicodeNames = Utemp[1]
    Uon          = Utemp[2]
    Uoff         = Utemp[3]

constant AW = "AW" -- index with[unicode+1] (errors if set_unicode not called)

integer convert_types
        convert_types = 0   -- 1 = convert eg "LONG" to "long", ie reduce
                            --      the no of types from five dozen or so
                            --      to a bakers dozen. However, I suspect
                            --      doing so may just increase confusion.

--/**/global procedure set_unicode(integer flag, integer ct=convert_types)
--/*
global procedure set_unicode(integer flag, integer ct)
--*/
    if not find(flag,{0,1}) then ?9/0 end if
    if not find(ct,{0,1}) then ?9/0 end if
    unicode = flag
    convert_types = ct
end procedure

function toInt(sequence txt)
-- convert string to integer.
-- " is treated as 0
integer ch, n
--  if txt="KNOWN_CONSTANT" then    -- (something like this to handle MAX_PATH etc)
    if equal(txt,"KNOWN_CONSTANT") then -- (something like this to handle MAX_PATH etc)
        n = 9/0
    else
        n = 0
        for i=1 to length(txt) do
            ch = txt[i]
            if ch<'0' or ch>'9' then err("number expected") end if
            n = n*10+ch-'0'
        end for
    end if
    return n
end function

--sequence structs = {},
--       stsizes = {},
--       saligns = {},
--       smembers = {}
sequence structs, stsizes, saligns, smembers
         structs = {}
         stsizes = {}
         saligns = {}
         smembers = {}

function add_struct(sequence res)
-- internal routine
-- res is eg {"RECT",16,4,{{"left","top","right","bottom"},{{"long",4,0,1},{"long",4,4,1},{"long",4,8,1},{"long",4,12,1}}}}
-- returns an integer id --?, 0 if the structure has no name
string name
integer sizeofS, widest
sequence members, ptrnames
integer id --?= 0
--  {name,sizeofS,widest,members,ptrnames} = res
    name     = res[1]
    sizeofS  = res[2]
    widest   = res[3]
    members  = res[4]
    ptrnames = res[5]
    if length(name) then
        structs = append(structs,name)
        id = length(structs)
        stsizes = append(stsizes,sizeofS)
        saligns = append(saligns,widest)
        smembers = append(smembers,members)
        for i=1 to length(ptrnames) do
            AltNames = append(AltNames,ptrnames[i])
            AltSize = append(AltSize,as_ptr)
        end for
    end if
    return id
end function

function do_type(string mtype, integer machine)
--
-- internal routine
--  processes "RECT", "RECT*", "long", "long long int", "long long int*" etc
--  returns a bunch of stuff for parse_c_struct() that define_c() can quietly ignore.
--
--integer substruct = 0, k, size, align, signed = 1
integer substruct, k, size, align, signed
string mname

    substruct = 0
    signed = 1
    if equal(mtype,"const") then
        mtype = stoken()
    end if
    k = find(mtype,structs)
    if k then
        size = stsizes[k]
        align = saligns[k]
        substruct = k
        mname = stoken()
    else
        k = find(mtype,AltNames)
        if k then
            k = AltSize[k]
            mname = stoken()
        elsif match("LP",mtype)=1
           or match("CALLBACK",mtype)=length(mtype)-7 then
            k = as_ptr
            mname = stoken()
        else
            k = find(mtype,UnicodeNames)
            if k then
                if unicode=-1 then err(mtype&": set_unicode() has not been called") end if
                if unicode then
                    k = Uon[k]
                else
                    k = Uoff[k]
                end if
                mname = stoken()
            else
--              if mtype="signed" then
                if equal(mtype,"signed") then
                    mtype = stoken()
--              elsif mtype="unsigned" then
                elsif equal(mtype,"unsigned") then
                    signed = 0
                    mtype = "u"&stoken()
                end if
                k = find(mtype,SizeNames)
                if k=0 then
                    err("unknown size "&mtype)
                end if
                mname = stoken()
--              if mtype="long" then
                if equal(mtype,"long") then
                    if equal(mname,"int") then          -- "long int" -> "long"
                        mname = stoken()
                    elsif equal(mname,"long") then      -- "long long" -> "int64"
                        mtype = "int64"
                        k = as_int64
                        mname = stoken()
                        if equal(mname,"int") then      -- "long long int" -> int64"
                            mname = stoken()
                        end if
                    elsif equal(mname,"double") then
                        err("not supported (size=10/8/12/16?, align=8/2/4/16?!! !! !!)")
                    end if
                elsif equal(mtype,"short") then
                    if equal(mname,"int") then          -- "short int" -> "short"
                        mname = stoken()
                    end if
                end if
            end if
        end if
        if convert_types then       -- (can be set via set_unicode)
            mtype = SizeNames[k]    -- (replaces eg "LONG" with "long")
        end if
        size = Sizes[k][machine]
        align = size
        signed = SizeSigns[k]
--      mname = stoken()
    end if
--  if ch='*' then
--  if mname="*" then
    if equal(mname,"*") then    -- "&"? (would need '&' adding twice in stoken())
        mname = stoken()
--DEV untested... (still just a ptr, innit!)
--      if ch='*' then err("** not (yet) supported") end if
--      if mname="*" then
        if equal(mname,"*") then
            mname = stoken()
        end if
        mtype = "ptr"
        size = Sizes[as_ptr][machine]
        align = size
        substruct = 0
    end if
    return {mname,substruct,mtype,size,align,signed}
end function

function parse_c_struct(integer struct, integer machine, integer base)
--
-- internal routine:
--  struct is 1 for struct, 0 for union
--  machine is S32 or S64
--  base is 0 from top level, non-zero for nested structs/unions.
-- returns:
--  {name,size,align,members,ptrnames}
-- where members is:
--  {{names},{{type,size,offset,sign}}}
-- eg:
--    {"RECT",16,4,{{"left","top","right","bottom"},
--                  {{"LONG",4,0,1},{"LONG",4,4,1},
--                   {"LONG",4,8,1},{"LONG",4,12,1}}}}
--  ptrnames may be something like {"LPRECT"}.
--
--  So a RECT is 16 bytes and needs 4 byte alignment, and
--  RECT.bottom is a long, 4 bytes, at offset 12, and is signed.
--  Note that members.type (eg "LONG") is only for debugging/sanity and 
--   is not intended or suitable for any practical use. You can set the
--   convert_types flag via set_unicode() to convert any AltNames and
--   UnicodeNames to their SizeNames equivalent, which would reduce the
--   number of types to be dealt with around five-fold, if you like,
--   but I suspect in most cases that would just add more confusion.
--  Also note that members.size is for a single element: a char[32] has
--   a size of 1 not a size of 32. It would be difficult to ascertain 
--   the number of elements(32) from the return value, particularly if
--   nested structs and unions are involved, instead it is assumed that
--   the calling code is happy enough just to know where the first is.
--
string name, token, mtype, mname, subname, void
integer signed, k, mult, size, subsize, align, substruct, offset
--integer sizeofS = 0, widest = 0, isstruct
integer sizeofS, widest, isstruct
--sequence members = {}, ptrnames = {}
sequence members, ptrnames
sequence submembers, sm2i, submembernames
sequence res
    sizeofS = 0
    widest = 0
    members = {}
    ptrnames = {}
    if ch!='{' then
        name = stoken()
    end if
    if ch!='{' then err("{ expected") end if
--  {} = stoken()
    void = stoken()
    while 1 do
        signed = 1
        mtype = stoken()
        isstruct = equal(mtype,"struct")
        if isstruct or equal(mtype,"union") then
            res = parse_c_struct(isstruct,machine,base+sizeofS)
--?res
--          {subname,subsize,align,submembers} = res
            subname    = res[1]
            subsize    = res[2]
            align      = res[3]
            submembers = res[4]

--DEV pad to align?
            k = remainder(sizeofS,align)
            if k then ?9/0 end if
--              padding = align-k
--              sizeofS += padding
--          end if
            for i=1 to length(submembers[1]) do
                mname = submembers[1][i]
                if length(subname) then
                    mname = subname&"."&mname
                end if
                sm2i = submembers[2][i]
--              if padding then
--  --              {mtype,size,offset,signed} = sm2i
--  --              offset += padding
--  --              sm2i = {mtype,size,offset,signed}
--                  sm2i[3] += padding
--              end if
                members = append(members,{mname,sm2i})
--              members = append(members,{mname,{mtype,size,offset,signed}})
            end for
            widest = subsize
            sizeofS += subsize
        else

--          {mname,substruct,mtype,size,align,signed} = do_type(mtype,machine)
            res = do_type(mtype,machine)
            mname     = res[1]
            substruct = res[2]
            mtype     = res[3]
            size      = res[4]
            align     = res[5]
            signed    = res[6]

            if equal(mname,";") then err("member name expected") end if
            mult = 1
            if ch='[' then
                nch("eof")
                if ch=']' then
                    if not equal(mtype,"ptr") then
                        mult = 0
                    end if
                else
--                  if mtype="ptr" then
--                      {} = stoken()
--                  else
                        mult = toInt(stoken())
--                  end if
                end if
                if ch!=']' then err("] expected") end if
                nch("eof")
                if ch='[' then err("multi-dimensional arrays are not (yet) supported") end if
            elsif ch=':' then
                err("bitfields are not (yet) supported")
            end if
            token = stoken()
            if not equal(token,";") then err("; expected") end if
            if size>widest then
                widest = size
            end if
            if struct then
--              k = remainder(sizeofS,size)
                k = remainder(sizeofS,align)
                if k then
--                  sizeofS += size-k
                    sizeofS += align-k
                end if
            end if
            if substruct then
--              {submembernames,submembers} = smembers[substruct]
                submembernames = smembers[substruct][1]
                submembers     = smembers[substruct][2]
                for i=1 to length(submembernames) do
                    subname = mname&"."&submembernames[i]
--                  {mtype,subsize,offset,signed} = submembers[i]
                    mtype   = submembers[i][1]
                    subsize = submembers[i][2]
                    offset  = submembers[i][3]
                    signed  = submembers[i][4]
                    members = append(members,{subname,{mtype,subsize,offset+base+sizeofS,signed}})
                end for
            else
                members = append(members,{mname,{mtype,size,base+sizeofS,signed}})
            end if
            if struct then
                sizeofS += size*mult
            end if
        end if

        if ch='}' then exit end if
    end while
    k = remainder(sizeofS,widest)
    if k then
        sizeofS += widest-k
    end if
--  {} = stoken()   -- discard '}'
    void = stoken() -- discard '}'
    if ch!=-1 then
        if ch!=';' then
            if ch='*' then err("name of *") end if
            name = stoken()
            while ch=',' do
--              {} = stoken()   -- discard ','
                void = stoken() -- discard ','
                if ch='*' then
--                  {} = stoken()
                    void = stoken()
                    token = stoken()
                    ptrnames = append(ptrnames,token)
                else
                    token = stoken()
                end if
            end while
        end if
        if ch!=-1 then
            if ch!=';' then err("; expected") end if
--          {} = stoken()
            void = stoken()
        end if
    end if
    res = {name,sizeofS,widest,columnize24(members,{},0),ptrnames}
    return res
end function

--/**/global function define_struct(string struct_str, integer machine=0, integer add=1)
--/*
global function define_struct(string struct_str, integer machine, integer add)
--*/
--
-- The struct_str parameter is eg "typedef struct{LONG left; .. } RECT;"
--  - note that without a "typedef", nothing gets stored permanantly.
-- The machine parameter can be set to S32 or S64, or (left) 0 for auto.
-- The add parameter is set to 0 for testing (override/ignore "typedef")
--
-- If add is 1 and struct_str contains "typedef", the return value is a
--  small integer id that can be used in calls to get/set_struct_value.
--
-- Otherwise full details of the structure are returned, which you can
--  display, use to write a little help file, or perhaps even directly 
--  use the sizes and offsets etc. For detyails see parse_c_struct().
--
string token
--integer typedef = 0
integer typedef
sequence res
    typedef = 0
    if machine=0 then
        if machine_bits()=32 then
            machine = S32
        else
            machine = S64
        end if
    end if
    s = struct_str
    sidx = 1
    ch = s[1]
    skipspaces("eof")
    token = stoken()
    if equal(token,"typedef") then
        typedef = 1
        token = stoken()
    end if
    if not equal(token,"struct") then err("struct expected") end if
    res = parse_c_struct(1,machine,0)
    if add and typedef then
        return add_struct(res)
    end if
    return res
end function

global function get_struct_size(integer id)
    return stsizes[id]
end function

global function allocate_struct(integer id)
-- remember to free() the result once done.
--integer size = stsizes[id]
--atom res = allocate(size)
integer size
atom res
    size = stsizes[id]
    res = allocate(size)
    mem_set(res,0,size)
    return res
end function

global procedure set_struct_field(integer id, atom pStruct, string fieldname, atom v)
--sequence {membernames,details} = smembers[id]
sequence membernames,details
--integer k = find(fieldname,membernames)
integer k
integer size, offset
--  {membernames,details} = smembers[id]
    membernames = smembers[id][1]
    details     = smembers[id][2]
    k = find(fieldname,membernames)
--  {{},size,offset} = details[k]
--  {?,size,offset} = details[k]
    size   = details[k][2]
    offset = details[k][3]
    pokeN(pStruct+offset,v,size)
end procedure

global function get_struct_field(integer id, atom pStruct, string fieldname)
--sequence {membernames,details} = smembers[id]
sequence membernames,details
integer k
integer size, offset, signed
--  {membernames,details} = smembers[id]
    membernames = smembers[id][1]
    details     = smembers[id][2]
    k = find(fieldname,membernames)
--  {{},size,offset,signed} = details[k]
--  {?,size,offset,signed} = details[k]
    size   = details[k][2]
    offset = details[k][3]
    signed = details[k][4]
    return peekNS(pStruct+offset,size,signed)
end function

global function get_field_details(integer id, string fieldname)
--sequence {membernames,details} = smembers[id]
sequence membernames,details
integer k
integer size, offset, sign
--  {membernames,details} = smembers[id]
    membernames = smembers[id][1]
    details     = smembers[id][2]
    k = find(fieldname,membernames)
--  {{},size,offset,sign} = details[k]
--  {?,size,offset,sign} = details[k]
    size   = details[k][2]
    offset = details[k][3]
    sign   = details[k][4]
    return {offset,size,sign}
end function

--sequence dll_names = {},
--       dll_addrs = {}
sequence dll_names, dll_addrs
         dll_names = {}
         dll_addrs = {}

function open_lib(object lib)
-- internal caching wrapper to open_dll()
integer k
    if string(lib) then
        k = find(lib,dll_names)
        if k=0 then
            dll_names = append(dll_names,lib)
            lib = open_dll(lib)
            dll_addrs = append(dll_addrs,lib)
        else
            lib = dll_addrs[k]
        end if
    end if
    if lib=0 then ?9/0 end if
    return lib
end function

--constant {C_SIZES,C_CONSTS} = columnize24({
sequence C_SIZES,C_CONSTS,C_TEMP
--  {C_SIZES,C_CONSTS} = columnize24({
    C_TEMP = columnize24({
                                         {{4,0},C_DWORD},   -- (=== C_PTR, C_HWND, etc)
                                         {{4,1},C_INT},
--                                       {{8,0},C_POINTER},
--                                       $})
                                         {{8,0},C_POINTER}},{},0)
    C_SIZES  = C_TEMP[1]
    C_CONSTS = C_TEMP[2]
--/* Some of these may need adding:
--  initialConstant("C_CHAR",       #01000001)
--  initialConstant("C_BYTE",       #01000001)
--  initialConstant("C_UCHAR",      #02000001)
--  initialConstant("C_UBYTE",      #02000001)
--  initialConstant("C_SHORT",      #01000002)
--  initialConstant("C_WORD",       #01000002)
--  initialConstant("C_USHORT",     #02000002)
--  initialConstant("C_INT64",      #01000008)  -- (a 64 bit signed integer)
--  initialConstant("C_QWORD",      #02000008)  -- (a 64 bit unsigned integer)
--  initialConstant("C_FLOAT",      #03000004)
--*/

function define_c(object lib, string cdef, integer func, integer machine)
--
-- internal wrapper to define_c_func/define_c_proc.
--
--  eg define_cfunc("kernel32.dll","DWORD WINAPI GetLastError(void);")
--  firstly performs lib=open_dll("kernel32.dll") and then returns
--  (non-0) result from define_c_func(lib,"GetLastError",{},C_INT).
--
--  lib can be a string or the non-0 atom result from open_dll().
--  cdef is usually just copied from MSDN or similar.
--  func is 0 or 1, from define_cfunc/define_cproc below.
--  machine can be set to S32 or S64, or (left) 0 for auto.
--
-- NOTE: if there are ansi and unicode versions of the routine, you
--  can edit the C definition to eg MessageBoxA or MessageBoxW, or
--  if set_unicode() has been called this routine will retry once.
-- This routine deliberately fails immediately with a fatal error if
--  any problem is detected, rather than return an error code.
--
string name, mtype, void
sequence args, res
integer substruct, size, align, signed, return_type, ptype
integer rid

    args = {}
    if machine=0 then
        if machine_bits()=32 then
            machine = S32
        else
            machine = S64
        end if
    end if
    lib = open_lib(lib)
    s = cdef
    sidx = 1
    ch = s[1]
    skipspaces("eof")
    mtype = stoken()

    if func then
--      {name,substruct,mtype,size,align,signed} = do_type(mtype,machine)
        res = do_type(mtype,machine)
        name      = res[1]
        substruct = res[2]
        mtype     = res[3]
        size      = res[4]
        align     = res[5]
        signed    = res[6]
        if substruct then ?9/0 end if
        return_type = C_CONSTS[find({size,signed},C_SIZES)]
    else
        if not equal(mtype,"void") then ?9/0 end if
        name = stoken()
    end if
    if equal(name,"WINAPI") then
        name = stoken()
    end if
    if ch!='(' then err("( expected") end if
--  {} = stoken()
    void = stoken()
    while ch!=')' do
        mtype = stoken()
        if match("_",mtype)=1 then  --  "_In_", "_Inout_opt_", "_Out_" etc
            mtype = stoken()
        elsif equal(mtype,"void") then
            if ch!=')' then ?9/0 end if
            if length(args) then ?9/0 end if
            exit
        end if
--      {{},substruct,mtype,size,align,signed} = do_type(mtype,machine)
--      {?,substruct,mtype,size,align,signed} = do_type(mtype,machine)
        res = do_type(mtype,machine)
        substruct = res[2]
        mtype     = res[3]
        size      = res[4]
        align     = res[5]
        signed    = res[6]
        if substruct then ?9/0 end if
        ptype = C_CONSTS[find({size,signed},C_SIZES)]
        args = append(args,ptype)
        if ch=')' then exit end if
        if ch!=',' then err(", expected") end if
--      {} = stoken()
        void = stoken()
        if ch=')' then ?9/0 end if
    end while
    --
    -- Note: define_c_func/proc are rather carefree regarding type: if
    --       a parameter is a 4 byte dword (or an 8 byte qword), it does 
    --       not care whether it is signed, a BOOL, an INT, a PTR, etc.
    --       Typically size/sign matters far more on the return type than 
    --       it does on the parameters, at least in my experience.
    --
    if func then
        rid = define_c_func(lib,name,args,return_type)
    else
        rid = define_c_proc(lib,name,args)
    end if
    if rid=-1 then
        name &= AW[unicode+1] -- (errors out if unicode still -1)
        if func then
            rid = define_c_func(lib,name,args,return_type)
        else
            rid = define_c_proc(lib,name,args)
        end if
        if rid=-1 then ?9/0 end if
    end if
    return rid
end function

--/**/global function define_cfunc(object lib, string cdef, integer machine=0)
--/*
global function define_cfunc(object lib, string cdef, integer machine)
--*/
--atom ol=open_dll(lib)
--integer rid = define_c_func(ol,"MessageBoxA",{C_PTR,C_PTR,C_PTR,C_PTR},C_INT)
atom ol
integer rid
    ol=open_dll(lib)
    rid = define_c_func(ol,"MessageBoxA",{C_PTR,C_PTR,C_PTR,C_PTR},C_INT)
    return define_c(lib,cdef,1,machine)
end function

--/**/global function define_cproc(object lib, string cdef, integer machine=0)
--/*
global function define_cproc(object lib, string cdef, integer machine)
--*/
    return define_c(lib,cdef,0,machine)
end function

--DEV <cffi.e ends>

--constant func1="""
--DWORD WINAPI GetLastError(void);
--"""
constant func1=
"DWORD WINAPI GetLastError(void);"
?define_cfunc("kernel32.dll",func1,0)

--constant func2="""
--void WINAPI SetLastError(
--  _In_    DWORD dwErrCode
--);
--"""
constant func2=
"void WINAPI SetLastError("&
"  _In_ DWORD dwErrCode"&
");"
?define_cproc("kernel32.dll",func2,0)

--constant func3="""
--int WINAPI MessageBox(
--  _In_opt_    HWND hWnd,
--  _In_opt_    LPCTSTR lpText,
--  _In_opt_    LPCTSTR lpCaption,
--  _In_        UINT uType
--);
--"""
constant func3=
"int WINAPI MessageBox("&
"  _In_opt_ HWND hWnd,"&
"  _In_opt_ LPCTSTR lpText,"&
"  _In_opt_ LPCTSTR lpCaption,"&
"  _In_     UINT uType"&
");"

--set_unicode(0)
set_unicode(0,0)
constant xMessageBox = define_cfunc("user32.dll",func3,0)
--?c_func(xMessageBox,{0,"text","caption",0})
--atom pText= allocate_string("text"),
--   pCaption =allocate_string("caption")
atom pText,pCaption
     pText= allocate_string("text")
     pCaption =allocate_string("caption")
if 01 then
?c_func(xMessageBox,{0,pText,pCaption,0})
end if

--constant MBP="""
--typedef struct {
--  UINT             cbSize;
--  HWND             hwndOwner;
--  HINSTANCE    hInstance;
--  LPCTSTR      lpszText;
--  LPCTSTR      lpszCaption;
--  DWORD        dwStyle;
--  LPCTSTR      lpszIcon;
--  DWORD_PTR    dwContextHelpId;
--  MSGBOXCALLBACK lpfnMsgBoxCallback;
--  DWORD        dwLanguageId;
--} MSGBOXPARAMS, *PMSGBOXPARAMS;
--"""
--constant MBI = """
--int WINAPI MessageBoxIndirect(
--  _In_ const LPMSGBOXPARAMS lpMsgBoxParams
--);
--"""
constant MBP=
"typedef struct {"&
"  UINT          cbSize;"&
"  HWND          hwndOwner;"&
"  HINSTANCE     hInstance;"&
"  LPCTSTR       lpszText;"&
"  LPCTSTR       lpszCaption;"&
"  DWORD         dwStyle;"&
"  LPCTSTR       lpszIcon;"&
"  DWORD_PTR     dwContextHelpId;"&
"  MSGBOXCALLBACK lpfnMsgBoxCallback;"&
"  DWORD         dwLanguageId;"&
"} MSGBOXPARAMS, *PMSGBOXPARAMS;"
constant MBI =
"int WINAPI MessageBoxIndirect("&
"  _In_ const LPMSGBOXPARAMS lpMsgBoxParams"&
");"

--integer idMBP = define_struct(MBP)
--integer idMBP = define_struct(MBP,0,1)
--integer xMBI = define_cfunc("user32.dll",MBI,0)
integer idMBP, xMBI
        idMBP = define_struct(MBP,0,1)
        xMBI = define_cfunc("user32.dll",MBI,0)

--atom pMBP = allocate_struct(idMBP)
atom pMBP
     pMBP = allocate_struct(idMBP)
    set_struct_field(idMBP,pMBP,"cbSize",get_struct_size(idMBP))
    set_struct_field(idMBP,pMBP,"lpszText",pText)
    set_struct_field(idMBP,pMBP,"lpszCaption",pCaption)
-- Note: this will not work on eui64 if you leave the ifdef BIT64 commented out (DOH!!).
    ?c_func(xMBI,{pMBP})

--tests:

--procedure check(string struct_str, sequence expected, sequence expected64=expected)
procedure check(string struct_str, sequence expected, object expected64)
--sequence actual = define_struct(struct_str, S32,0)
--integer le = length(expected)
sequence actual
integer le
    actual = define_struct(struct_str, S32,0)
    le = length(expected)
    if not equal(actual[1..le],expected) then ?9/0 end if
    actual = define_struct(struct_str,S64,0)
    if atom(expected64) then expected64=expected end if
    if not equal(actual[1..le],expected64) then ?9/0 end if
end procedure

-- from http://c-faq.com/struct/align.esr.html
--(1) struct {char *; long;} has no trailing padding and is (4 + 4) = 8 bytes long, or (8 + 8) = 16 with no padding on a 64-bit architecture. 
check("struct x{char *ps; long l;} ",{"x", 8,4,{{"ps","l"},{{"ptr",4,0,1},{"long",4,4,1}}}},
                                     {"x",16,8,{{"ps","l"},{{"ptr",8,0,1},{"long",4,8,1}}}})
--(2) struct {char *; short;} will also be two machine words long, even though a short is only a half-word, because the leading char * would force word alignment on a following instance. 
check("struct x{char *ps; short s;} ",{"x", 8,4,{{"ps","s"},{{"ptr",4,0,1},{"short",2,4,1}}}},
                                      {"x",16,8,{{"ps","s"},{{"ptr",8,0,1},{"short",2,8,1}}}})
--(3) struct {char *; short; short;} will be the same two-word size as example 2; the final short simply takes up what would have been padding.
check("struct x{char *ps; short s; short s2;}",{"x", 8,4,{{"ps","s","s2"},{{"ptr",4,0,1},{"short",2,4,1},{"short",2, 6,1}}}},
                                               {"x",16,8,{{"ps","s","s2"},{{"ptr",8,0,1},{"short",2,8,1},{"short",2,10,1}}}})
--(4) struct {char *; short; long;} will have a half-word of padding after the short because long has to be word-aligned, and is thus the same size as struct {char *; short; short; long;}. 
check("struct x{char *ps; short s; long l;}",{"x",12,4,{{"ps","s","l"},{{"ptr",4,0,1},{"short",2,4,1},{"long",4, 8,1}}}},
                                             {"x",16,8,{{"ps","s","l"},{{"ptr",8,0,1},{"short",2,8,1},{"long",4,12,1}}}})
check("struct x{char *ps; short s; short s2; long l;}",{"x",12,4,{{"ps","s","s2","l"},{{"ptr",4,0,1},{"short",2,4,1},{"short",2, 6,1},{"long", 4,8,1}}}},
                                                       {"x",16,8,{{"ps","s","s2","l"},{{"ptr",8,0,1},{"short",2,8,1},{"short",2,10,1},{"long",4,12,1}}}})
--(5) struct {short; char;} would have one byte of trailing padding;
check("struct x{short s; char c;}",{"x",4,2,{{"s","c"},{{"short",2,0,1},{"char",1,2,1}}}},0)
--(6) struct {short; char; char;} would be the same size as example 5, but with no padding. 
check("struct x{short s; char c; char c2;}",{"x",4,2,{{"s","c","c2"},{{"short",2,0,1},{"char",1,2,1},{"char",1,3,1}}}},0)
--(7) struct {long; char;} would have 3 bytes of trailing padding, 
check("struct x{long l; char c;}",{"x",8,4,{{"l","c"},{{"long",4,0,1},{"char",1,4,1}}}},0)
--(8) struct {long; char; short} would have 1 byte of padding after the char, be the same size as example 7, and have no trailing padding.
check("struct x{long l; char c; short s;}",{"x",8,4,{{"l","c","s"},{{"long",4,0,1},{"char",1,4,1},{"short",2,6,1}}}},0)

-- from http://www.catb.org/esr/structure-packing/
check("struct x{char *pc; char c; short s;}",{"x", 8,4,{{"pc","c","s"},{{"ptr",4,0,1},{"char",1,4,1},{"short",2, 6,1}}}},
                                             {"x",16,8,{{"pc","c","s"},{{"ptr",8,0,1},{"char",1,8,1},{"short",2,10,1}}}})
check("struct x{char *pc; char c; long l;}",{"x",12,4,{{"pc","c","l"},{{"ptr",4,0,1},{"char",1,4,1},{"long",4, 8,1}}}},
                                            {"x",16,8,{{"pc","c","l"},{{"ptr",8,0,1},{"char",1,8,1},{"long",4,12,1}}}})

--from https://en.wikipedia.org/wiki/Data_structure_alignment
--constant test1="""
--struct MyData
--{
--  short Data1;
--  short Data2;
--  short Data3;
--};
--"""
constant test1=
"struct MyData"&
"{"&
"   short Data1;"&
"   short Data2;"&
"   short Data3;"&
"};"


check(test1,{"MyData",6,2,{{"Data1","Data2","Data3"},{{"short",2,0,1},{"short",2,2,1},{"short",2,4,1}}}},0)

--constant test2="""
--struct MixedData
--{
--  char Data1;
--  short Data2;
--  int Data3;
--  char Data4;
--};
--"""
constant test2=
"struct MixedData"&
"{"&
"   char Data1;"&
"   short Data2;"&
"   int Data3;"&
"   char Data4;"&
"};"
check(test2,{"MixedData",12,4,{{"Data1","Data2","Data3","Data4"},{{"char",1,0,1},{"short",2,2,1},{"int",4,4,1},{"char",1,8,1}}}},0)

-- (after compiling on 32-bit:)
--constant test2_32ac="""
--struct MixedData  /* After compilation in 32-bit x86 machine */
--{
--  char Data1; /* 1 byte */
--  char Padding1[1]; /* 1 byte for the following 'short' to be aligned on a 2 byte boundary 
--assuming that the address where structure begins is an even number */
--  short Data2; /* 2 bytes */
--  int Data3;  /* 4 bytes - largest structure member */
--  char Data4; /* 1 byte */
--  char Padding2[3]; /* 3 bytes to make total size of the structure 12 bytes */
--};
--"""
constant test2_32ac=
"struct MixedData  /* After compilation in 32-bit x86 machine */"&
"{"&
"   char Data1; /* 1 byte */"&
"   char Padding1[1]; /* 1 byte for the following 'short' to be aligned on a 2 byte boundary "&
"assuming that the address where structure begins is an even number */"&
"   short Data2; /* 2 bytes */"&
"   int Data3;  /* 4 bytes - largest structure member */"&
"   char Data4; /* 1 byte */"&
"   char Padding2[3]; /* 3 bytes to make total size of the structure 12 bytes */"&
"};"
check(test2_32ac,{"MixedData",12,4,{{"Data1","Padding1","Data2","Data3","Data4","Padding2"},{{"char",1,0,1},{"char",1,1,1},{"short",2,2,1},{"int",4,4,1},{"char",1,8,1},{"char",1,9,1}}}},0)

--constant test3="""
--struct FinalPad {
--  float x;
--  char n[1];
--};
--"""
constant test3=
"struct FinalPad {"&
"  float x;"&
"  char n[1];"&
"};"
check(test3,{"FinalPad",8,4,{{"x","n"},{{"float",4,0,1},{"char",1,4,1}}}},0)

--constant test4="""
--struct FinalPadShort {
--  short s;
--  char n[3];                                                                              
--};
--"""
constant test4=
"struct FinalPadShort {"&
"  short s;"&
"  char n[3];"&                                                                             
"};"
check(test4,{"FinalPadShort",6,2,{{"s","n"},{{"short",2,0,1},{"char",1,2,1}}}},0)

--constant test5="""
--struct MixedData  /* after reordering */
--{
--  char Data1;
--  char Data4;   /* reordered */
--  short Data2;
--  int Data3;  
--};
--"""
constant test5=
"struct MixedData  /* after reordering */"&
"{"&
"  char Data1;"&
"  char Data4;   /* reordered */"&
"  short Data2;"&
"  int Data3;"&
"};"
check(test5,{"MixedData",8,4,{{"Data1","Data4","Data2","Data3"},{{"char",1,0,1},{"char",1,1,1},{"short",2,2,1},{"int",4,4,1}}}},0)

--constant test6="""
--struct s6{
--              char a[3];
--              short b;
--              long c;
--              char d[3];
--              };
--"""
constant test6=
"struct s6{"&
"  char a[3];"&
"  short b;"&
"  long c;"&
"  char d[3];"&
"  };"
check(test6,{"s6",16,4,{{"a","b","c","d"},{{"char",1,0,1},{"short",2,4,1},{"long",4,8,1},{"char",1,12,1}}}},0)

--constant test7="""
--struct smth
--{
--  char a;
--  int b[];
--};
--"""
constant test7=
"struct smth"&
"{"&
"  char a;"&
"  int b[];"&
"};"
check(test7,{"smth",4,4,{{"a","b"},{{"char",1,0,1},{"int",4,4,1}}}},0)

--constant test8="""
--struct T {
--      char c;
--      int *p;
--      short s;
--      } t;
--"""
constant test8=
"struct T {"&
"  char c;"&
"  int *p;"&
"  short s;"&
"  } t;"
check(test8,{"t",12,4,{{"c","p","s"},{{"char",1,0,1},{"ptr",4,4,1},{"short",2, 8,1}}}},
            {"t",24,8,{{"c","p","s"},{{"char",1,0,1},{"ptr",8,8,1},{"short",2,16,1}}}})

--constant test9="""
--struct _TEST {
--  union
--  {
--      int i;
--      float f;
--      struct
--      {
--          unsigned int u;
--          double d;
--      } s;
--  } u;
--} TEST, *PTEST;
--"""
constant test9=
"struct _TEST {"&
"  union"&
"  {"&
"    int i;"&
"    float f;"&
"    struct"&
"    {"&
"      unsigned int u;"&
"      double d;"&
"    } s;"&
"  } u;"&
"} TEST, *PTEST;"
check(test9,{"TEST",16,16,{{"u.i","u.f","u.s.u","u.s.d"},{{"int",4,0,1},{"float",4,0,1},{"uint",4,0,0},{"double",8,8,1}}}},0)

--constant structx = """
--typedef struct x {
--  int member_a;
--  int member_b;
--} x;
--"""
--constant structx = 
--"typedef struct x {"&
--"  int member_a;"&
--"  int member_b;"&
--"} x;"

--constant tRECT = """
--typedef struct _RECT {
--  LONG left;
--  LONG top;
--  LONG right;
--  LONG bottom;
--} RECT, *PRECT;
--"""
constant tRECT = 
"typedef struct _RECT {"&
"  LONG left;"&
"  LONG top;"&
"  LONG right;"&
"  LONG bottom;"&
"} RECT, *PRECT;"

--constant tPS ="""
--typedef struct tagPAINTSTRUCT {
--  HDC  hdc;
--  BOOL fErase;
--  RECT rcPaint;
--  BOOL fRestore;
--  BOOL fIncUpdate;
--  BYTE rgbReserved[32];
--} PAINTSTRUCT, *PPAINTSTRUCT;
--"""
constant tPS =
"typedef struct tagPAINTSTRUCT {"&
"  HDC  hdc;"&
"  BOOL fErase;"&
"  RECT rcPaint;"&
"  BOOL fRestore;"&
"  BOOL fIncUpdate;"&
"  BYTE rgbReserved[32];"&
"} PAINTSTRUCT, *PPAINTSTRUCT;"
procedure testPS()
integer idRECT, idPS, size, sign
atom pPS, pRECT
sequence res
    idRECT = define_struct(tRECT,0,1)
    idPS = define_struct(tPS,S32,1)
    pPS = allocate_struct(idPS)

--  {pRECT,size,sign} = get_field_details(idPS,"rcPaint.left")
    res = get_field_details(idPS,"rcPaint.left")
    pRECT = res[1]+pPS
    size  = res[2]
    sign  = res[3]
    pokeN(pRECT,{1,2,3,4},size)
    set_struct_field(idPS,pPS,"rcPaint.right",450)
    ?peekNS({pRECT,4},size,sign)    -- displays {1,2,450,4}
    ?get_struct_field(idRECT,pRECT,"right") -- displays 450
    ?get_struct_field(idPS,pPS,"rcPaint.right") -- displays 450
--?{structs,stsizes,saligns,smembers}
end procedure

if 01 then
    testPS()
end if

--constant testROK="""
--LONG WINAPI RegOpenKeyEx(
--  _In_          HKEY hKey,
--  _In_opt_      LPCTSTR lpSubKey,
--  _Reserved_  DWORD ulOptions,
--  _In_          REGSAM samDesired,
--  _Out_     PHKEY phkResult
--);
--"""		   
constant testROK=
"LONG WINAPI RegOpenKeyEx("&
"  _In_        HKEY hKey,"&
"  _In_opt_    LPCTSTR lpSubKey,"&
"  _Reserved_  DWORD ulOptions,"&
"  _In_        REGSAM samDesired,"&
"  _Out_       PHKEY phkResult"&
");"
if 01 then
?define_cfunc("Advapi32.dll",testROK,S32)
end if

--constant test = "struct x{char *ps; short s;} "
--?define_struct(test,S32,0)
--?define_struct(test,S64,0)

--?define_struct(structx,S32,0)
--?define_struct(structx,S64,0)

--constant bad1 = "struct {char *; long;} "     -- error
--constant bad1 = "struct x{char *; long;} "    -- error
--constant bad1 = "struct x{char *ps; long;} "  -- error
--constant bad1="""
--struct foo5 {
--  short s;
--  char c;
--  int flip:1;
--  int nybble:4;
--  int septet:7;
--};"""
--?define_struct(bad1,S32,0)

--DEV (temp)
--set_unicode(0,0)
--set_unicode(1,0)
--if sequence(SizeSigns) then end if

--{} = wait_key()
--if wait_key() then end if
--abort(0)

