--
--  cffi.e
--  ======
--
--      Eliminates most of the byte-counting needed when interfacing to C routines.
--      The motivation behind this was that while I had many of the windows API
--      structures laboriously translated to offsets for 32-bit from many years
--      ago, they would all have otherwise needed re-doing for 64-bit.
--
--      I must stress that any savings are limited to converting C header (.h) files,
--      or more accurately fragments copied verbatim from said, which would otherwise 
--      have to be done line-by-line manually. There is no new fancy slick syntax to 
--      help with the code that follows, or anything remotely like that. Although it
--      knows the names, offsets and sizes of fields, that does not mean that you no 
--      longer have to fill them in! But there is less chance of being out-by-2 or
--      whetever on the 4th field and therefore all the rest.
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
--      Routine definition:
--      ===================
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
--          constant tMB="""
--          int WINAPI MessageBox(
--            _In_opt_  HWND hWnd,
--            _In_opt_  LPCTSTR lpText,
--            _In_opt_  LPCTSTR lpCaption,
--            _In_      UINT uType
--          );
--          """
--          set_unicode(0)
--          constant xMessageBox = define_cfunc("user32.dll",tMB)
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
--/* -- OpenEuphoria compatability (4.1.0+; for RDS Eu 2.4 see cffi.2.4.e):
type string(object s)
    return sequence(s)
end type
procedure pokeN(atom addr, object v, integer size)
    if size=1 then
        poke(addr,v)
    elsif size=2 then   
        poke2(addr, v)
    elsif size=4 then
        poke4(addr,v)
    elsif size=8 then
        poke8(addr,v)
    else
        ?9/0
    end if
end procedure
function peekNS(object addr, integer size, integer signed)
object res
    if signed then
        if size=1 then
            res = peeks(addr)
        elsif size=2 then   
            res = peek2s(addr)
        elsif size=4 then
            res = peek4s(addr)
        elsif size=8 then
            res = peek8s(addr)
        else
            ?9/0
        end if
    else
        if size=1 then
            res = peek(addr)
        elsif size=2 then   
            res = peek2u(addr)
        elsif size=4 then
            res = peek4u(addr)
        elsif size=8 then
            res = peek8u(addr)
        else
            ?9/0
        end if
    end if
    return res
end function
include misc.e      -- LINUX
include dll.e       -- C_INT
constant C_DWORD = C_INT
constant C_PTR = C_POINTER
include get.e       -- wait_key
include machine.e   -- allocate
include pcolumn.e   -- columnize    ** NB Phix version **
function machine_bits()
    ifdef BITS64 then  
        return 64
    elsedef  
        return 32
    end ifdef 
end function
--*/

procedure err(string msg)
    puts(1,msg)
    ?9/0
end procedure

string s
integer ch
integer sidx

procedure nch(object msg="eof")
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

procedure skipspaces(object msg="eof")
integer k
    while 1 do
        while find(ch," \t\r\n")!=0 do nch(msg) end while
        if ch!='/' then exit end if
        if match("/*",s,sidx)=sidx then
            k = match("*/",s,sidx+2)
            if k=0 then err("missing closing comment") end if
            sidx = k+1
            nch(msg)
        else
            if match("//",s,sidx)!=sidx then exit end if
            while find(ch,"\r\n")=0 do nch(msg) end while
        end if
    end while
end procedure

function stoken()
integer tokstart, tokend
    if ch=-1 then err("eof") end if
--  skipspaces()
    tokstart = sidx
    if find(ch,"{;*}[]:,()")!=0 then
        tokend = sidx
        nch(0)
    else
        while 1 do
            nch()
            if find(ch," \t\r\n{;*}[]:,()")!=0 then exit end if
        end while
        tokend = sidx-1
    end if
    skipspaces(0)
    return s[tokstart..tokend]
end function

global constant S32=1, S64=2

-- a "long" is 32 bits on Windows64, but 64 bits on Linux64 (!!)
integer L = 4
if platform()=LINUX then
    L = 8
end if

-- entries are {name, signed, 32-bit size, 64-bit size}:
--  (this table is my own choice of the minimal/core types)
--constant {SizeNames,SizeSigns,Sizes} = columnize({
sequence SizeNames,SizeSigns,Sizes
    {SizeNames,SizeSigns,Sizes} = columnize({
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
--                                                {"flt80",     1,{10,10}},     -- maybe?
                                                  {"ptr",       0,{4,8}},       -- (no point having ptr/uptr that I can think of)
                                                  $})


--A partial definition of long double from wikipedia( https://en.wikipedia.org/wiki/Data_structure_alignment ):
-- on 32 bit:
--A long double (ten bytes with C++Builder and DMC, eight bytes with Visual C++, twelve bytes with GCC) will be 
--  8-byte aligned with C++Builder, 2-byte aligned with DMC, 8-byte aligned with Visual C++ and 4-byte aligned with GCC.
-- on 64 bit:
--A long double (eight bytes with Visual C++, sixteen bytes with GCC) will be 8-byte aligned with Visual C++ and 16-byte aligned with GCC.
-- (PL: Note that Windows64 is an LLP64 data model, not LP64 which the above article seems to be writing about.)
--
--And here's another gratuitous link: https://en.wikipedia.org/wiki/Long_double
--
--If you ask me, life would be simpler if float/double/longdouble were instead more explicitly named say flt32/flt64/flt80.
--
--Might I suggest trying flt80 for any long doubles you happen to run into, not that I have any evidence that will work.
--

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
--sequence {AltNames,AltSize} = columnize({
sequence AltNames,AltSize
    {AltNames,AltSize} = columnize({
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
--                                       {"float",          as_float},
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
                                         {"ULONG64",        as_uint64},
                                         $})

integer unicode = -1    -- -1 = unknown
                        --  0 = ansi
                        -- +1 = unicode

-- (this ended up a bit overkill, nevermind)
sequence UnicodeNames,UnicodeAs
    {UnicodeNames,UnicodeAs} = columnize({
                                              {"TBYTE",{as_char,as_ushort}},
                                              {"TCHAR",{as_char,as_ushort}},
                                              $})

constant AW = "AW" -- index with[unicode+1] (errors if set_unicode not called)

integer convert_types = 0   -- 1 = convert eg "LONG" to "long", ie reduce
                            --      the no of types from five dozen or so
                            --      to a bakers dozen. However, I suspect
                            --      doing so may just increase confusion.

global procedure set_unicode(integer flag, integer ct=convert_types)
    if not find(flag,{0,1}) then ?9/0 end if
    if not find(ct,{0,1}) then ?9/0 end if
    unicode = flag
    convert_types = ct
end procedure

function toInt(sequence txt)
-- convert string to integer.
-- " is treated as 0
integer ch, n
    if equal(txt,"KNOWN_CONSTANT") then -- (something like this to handle MAX_PATH etc)
        n = 9/0                         -- (or maybe parse C constants/enums/#defines?)
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

sequence structs = {},
         stsizes = {},
         saligns = {},
         smembers = {}

function add_struct(sequence res)
-- internal routine
-- res is eg {"RECT",16,4,{{"left","top","right","bottom"},{{"long",4,0,1},{"long",4,4,1},{"long",4,8,1},{"long",4,12,1}}}}
-- returns an integer id
string name
integer sizeofS, widest
sequence members, ptrnames
integer id
    {name,sizeofS,widest,members,ptrnames} = res
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
integer substruct = 0, k, size, align, signed = 1
string mname

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
           or match("CALLBACK",mtype)=length(mtype)-7
           or match("Proc",mtype)=length(mtype)-3 then
            k = as_ptr
            mname = stoken()
        else
            k = find(mtype,UnicodeNames)
            if k then
                if unicode=-1 then err(mtype&": set_unicode() has not been called") end if
                k = UnicodeAs[unicode+1][k]
                mname = stoken()
            else
                if equal(mtype,"signed") then
                    mtype = stoken()
                elsif equal(mtype,"unsigned") then
                    signed = 0
                    mtype = "u"&stoken()
                end if
                k = find(mtype,SizeNames)
                if k=0 then
                    err("unknown size "&mtype)
                end if
                mname = stoken()
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
    end if
    if equal(mname,"*") then    -- "&"? (would need '&' adding twice in stoken())
        mname = stoken()
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
string name, token, mtype, mname, subname
integer signed, k, mult, size, subsize, align, substruct, offset
integer sizeofS = 0, widest = 0, isstruct
sequence members = {}, ptrnames = {}
sequence submembers, sm2i, submembernames
sequence res
    if ch!='{' then
        name = stoken()
    end if
    if ch!='{' then err("{ expected") end if
    {} = stoken()
    while 1 do
        signed = 1
        mtype = stoken()
        isstruct = equal(mtype,"struct")
        if isstruct or equal(mtype,"union") then
            res = parse_c_struct(isstruct,machine,base+sizeofS)
            {subname,subsize,align,submembers} = res

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

            {mname,substruct,mtype,size,align,signed} = do_type(mtype,machine)

            if equal(mname,";") then err("member name expected") end if
            mult = 1
            if ch='[' then
                nch()
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
                nch()
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
                k = remainder(sizeofS,align)
                if k then
                    sizeofS += align-k
                end if
            end if
            if substruct then
                {submembernames,submembers} = smembers[substruct]
                for i=1 to length(submembernames) do
                    subname = mname&"."&submembernames[i]
                    {mtype,subsize,offset,signed} = submembers[i]
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
    {} = stoken()   -- discard '}'
    if ch!=-1 then
        if ch!=';' then
            if ch='*' then err("name of *") end if
            name = stoken()
            while ch=',' do
                {} = stoken()   -- discard ','
                if ch='*' then
                    {} = stoken()
                    token = stoken()
                    ptrnames = append(ptrnames,token)
                else
                    token = stoken()
                end if
            end while
        end if
        if ch!=-1 then
            if ch!=';' then err("; expected") end if
            {} = stoken()
        end if
    end if
    res = {name,sizeofS,widest,columnize(members),ptrnames}
    return res
end function

global function define_struct(string struct_str, integer machine=0, integer add=1)
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
--  use the sizes and offsets etc. For details see parse_c_struct().
--
string token
integer typedef = 0
sequence res
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
    skipspaces()
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
integer size = stsizes[id]
atom res = allocate(size)
    mem_set(res,0,size)
    return res
end function

global procedure set_struct_field(integer id, atom pStruct, string fieldname, atom v)
--sequence {membernames,details} = smembers[id]
sequence membernames,details
--integer k = find(fieldname,membernames)
integer k
integer size, offset
    {membernames,details} = smembers[id]
    k = find(fieldname,membernames)
    {?,size,offset} = details[k]
    pokeN(pStruct+offset,v,size)
end procedure

global function get_struct_field(integer id, atom pStruct, string fieldname)
--sequence {membernames,details} = smembers[id]
sequence membernames,details
integer k
integer size, offset, signed
    {membernames,details} = smembers[id]
    k = find(fieldname,membernames)
    {?,size,offset,signed} = details[k]
    return peekNS(pStruct+offset,size,signed)
end function

global function get_field_details(integer id, string fieldname)
--sequence {membernames,details} = smembers[id]
sequence membernames,details
integer k
integer size, offset, sign
    {membernames,details} = smembers[id]
    k = find(fieldname,membernames)
    {?,size,offset,sign} = details[k]
    return {offset,size,sign}
end function

sequence dll_names = {},
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

--constant {C_SIZES,C_CONSTS} = columnize({
sequence C_SIZES,C_CONSTS
    {C_SIZES,C_CONSTS} = columnize({
                                         {{4,0},C_DWORD},   -- (=== C_PTR, C_HWND, etc)
                                         {{4,1},C_INT},
                                         {{8,0},C_POINTER},
                                         $})
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
string name, mtype
sequence args
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
    skipspaces()
    mtype = stoken()

    if func then
        {name,substruct,mtype,size,align,signed} = do_type(mtype,machine)
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
    {} = stoken()
    while ch!=')' do
        mtype = stoken()
        if match("_",mtype)=1 then  --  "_In_", "_Inout_opt_", "_Out_" etc
            mtype = stoken()
        elsif equal(mtype,"void") then
            if ch!=')' then ?9/0 end if
            if length(args) then ?9/0 end if
            exit
        end if
        {?,substruct,mtype,size,align,signed} = do_type(mtype,machine)
        if substruct then ?9/0 end if
        ptype = C_CONSTS[find({size,signed},C_SIZES)]
        args = append(args,ptype)
        if ch=')' then exit end if
        if ch!=',' then err(", expected") end if
        {} = stoken()
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

global function define_cfunc(object lib, string cdef, integer machine=0)
    return define_c(lib,cdef,1,machine)
end function

global function define_cproc(object lib, string cdef, integer machine=0)
    return define_c(lib,cdef,0,machine)
end function


