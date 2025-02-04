﻿--
--  builtins\cffi.e
--  ===============
--
--      Parse C struct and function definitions, and use them.
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
--      whatever on the 4th field and therefore all the rest.
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
--          integer rid = define_cffi_func(object lib, string cdef)
--          integer rid = define_cffi_proc(object lib, string cdef)
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
--          constant idRECT = define_struct(tRECT),
--                   idPS = define_struct(tPS)
--                   pPS = allocate_struct(idPS)
--          ...
--          {oRECT,size,sign} = get_field_details(idPS,"rcPaint.left")
--          pokeN(pPS+oRECT,{1,2,3,4},size)
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
--              may be possible to replicate the builtins\VM\pcfunc.e\fatalN
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
--          constant xMessageBox = define_cffi_func("user32.dll",tMB)
--          ?c_func(xMessageBox,{0,"text","caption",0})
--
--      Notes:  set_unicode() is needed before any auto "A/W" handling.
--              0 (ansi) is recommended: it is more about what your code is
--              going to do than whether you're on 32/64 bit or somesuch.
--              a proc is expected to begin with a "void" return type.
--              lib can be a string or the previous result of open_dll.
--              pass by value (eg/ie RECT not RECT*) is not supported.
--              define_c_func etc is rather carefree regarding type: if it
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

--DEV I just rediscovered the following fragment from python's cffi:
--      from cffi import FFI
--
--      ffi.cdef("""
--          typedef void DIR;
--          typedef long ino_t;
--          typedef long off_t;
--
--          struct dirent {
--              ino_t          d_ino;       /* inode number */
--              off_t          d_off;       /* offset to the next dirent */
--              unsigned short d_reclen;    /* length of this record */
--              unsigned char  d_type;      /* type of file; not supported
--                                             by all file system types */
--              char           d_name[256]; /* filename */
--          };
--
--          DIR *opendir(const char *name);
--          int readdir_r(DIR *dirp, struct dirent *entry, struct dirent **result);
--          int closedir(DIR *dirp);
--      """)
-- Opps, seems I completely missed the "typedef void DIR;" requirement: while I suspect it 
--  would be relatively trivial to add, I'll not bother until I actually need/can test it.

--with trace

--/*
--
-- OpenEuphoria compatability (4.1.0+; for RDS Eu 2.4 see cffi.2.4.e):
-- ==================================================================
--
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
include builtins\ptypes.e   -- atom_string

procedure cffi_error(string msg)
--19/2/21:
--  puts(1,msg)
--  ?9/0
    crash(msg)
end procedure

string s
integer ch
integer sidx

procedure nch(object msg="eof")
    sidx += 1
    if sidx>length(s) then
        if string(msg) then
            cffi_error(msg)
        else
            ch = -1
        end if
    else
        ch = s[sidx]
    end if
end procedure

procedure skipspaces(object msg="eof")
--integer k
    while true do
--      while find(ch," \t\r\n")!=0 do nch(msg) end while
        while find(ch," \r\n"&9)!=0 do nch(msg) end while
        if ch!='/' then exit end if
--16/2/23 (reduce .asm listing files showing data as comments)
--      if match("/!*",s,sidx)=sidx then
        if s[sidx+1]='*' then
--          k = match("*!/",s,sidx+2)
            integer k = 0
            for i=sidx+2 to length(s)-1 do
                if s[i]='*' and s[i+1]='/' then
                    k = i
                    exit
                end if
            end for
            if k=0 then cffi_error("missing closing comment") end if
            sidx = k+1
            nch(msg)
        else
--          if match("/!/",s,sidx)!=sidx then exit end if
--          if s[sidx+1]!=sidx then exit end if
            if s[sidx+1]!='/' then exit end if
            while find(ch,"\r\n")=0 do nch(msg) end while
        end if
    end while
end procedure

function stoken()
    if ch=-1 then cffi_error("eof") end if
--  skipspaces()
    integer tokstart = sidx, tokend
    if find(ch,"{;*}[]:,()")!=0 then
        tokend = sidx
        nch(0)
    else
        while 1 do
            nch()
--          if find(ch," \t\r\n{;*}[]:,()")!=0 then exit end if
            if find(ch," \r\n{;*}[]:,()"&9)!=0 then exit end if
        end while
        tokend = sidx-1
    end if
    skipspaces(0)
--?{"stoken",s[tokstart..tokend]}
    return s[tokstart..tokend]
end function

--<global constant S32=1, S64=2

-- a "long" is 32 bits on Windows64, but 64 bits on Linux64 (!!)
--DEV
--constant L = iff(platform()=WINDOWS?4:8)
integer L = 4
--if platform()=LINUX then  -- [moved to init_cffi()]
--  L = 8
--end if

-- entries are {name, signed, 32-bit size, 64-bit size}:
--  (this table is my own choice of the minimal/core types)
--constant {SizeNames,SizeSigns,Sizes} = columnize({
sequence SizeNames,SizeSigns,Sizes

integer as_char,  -- = find("char",SizeNames)
        as_uchar, -- = find("uchar",SizeNames)
        as_int,   -- = find("int",SizeNames)
        as_uint,  -- = find("uint",SizeNames)
        as_ptr,   -- = find("ptr",SizeNames)
        as_short, -- = find("short",SizeNames)
        as_ushort,-- = find("ushort",SizeNames)
        as_long,  -- = find("long",SizeNames)
        as_ulong, -- = find("ulong",SizeNames)
        as_float, -- = find("float",SizeNames)
        as_double,-- = find("double",SizeNames)
        as_int64, -- = find("int64",SizeNames)
        as_uint64 -- = find("uint64",SizeNames)

sequence AltNames,AltSize

integer unicode = -1    -- -1 = unknown
                        --  0 = ansi
                        -- +1 = unicode

-- (this ended up a bit overkill, nevermind)
sequence UnicodeNames,UnicodeAs

--constant AW = "AW" -- index with[unicode+1] (errors if set_unicode not called)

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
-- "" is treated as 0
    integer n = 0

    -- (add more cases for MAX_PATH etc as needed)
    -- (or maybe parse C constants/enums/#defines?)
    if equal(txt,"LF_FACESIZE") then
        n = 32
    elsif equal(txt,"MAX_PATH") then
        n = 260
    elsif equal(txt,"WSADESCRIPTION_LEN+1") then
        n = 256+1
    elsif equal(txt,"WSASYS_STATUS_LEN+1") then
        n = 128+1
    else
        for ch in txt do
            if ch<'0' or ch>'9' then cffi_error("number expected") end if
            n = n*10 + (ch-'0')
        end for
    end if
    return n
end function

-- Note: The sequence structs currently holds only one name per struct, whereas AltNames can hold several pointer aliases
--       [as they are just pointers, not quite as useful as it sounds], perhaps there could be a struct_aliases sequence?

sequence structs,       -- struct names
         stsizes,
         saligns,
         smembers       

function add_struct(sequence res)
-- internal routine
-- res is eg {"RECT",16,4,{{"left","top","right","bottom"},{{"long",4,0,1},{"long",4,4,1},{"long",4,8,1},{"long",4,12,1}}},{}}
-- returns an integer id
    string name
    sequence members, ptrnames
    integer sizeofS, widest, id
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

function endswith(string s, string mtype)
    return length(mtype)>length(s) and mtype[-length(s)..-1]=s
end function

function do_type(string mtype, integer machine)
--
-- internal routine
--  processes "RECT", "RECT*", "long", "long long int", "long long int*" etc
--  returns a bunch of stuff for parse_c_struct() that define_c() can quietly ignore.
--
integer substruct = 0, size, align, signed = 1
string mname
bool bFunc = false

--?{"do_type",mtype}
--11/5/19:
--  if equal(mtype,"const") then
--  if find(mtype,{"const","struct"}) then
    while find(mtype,{"const","struct"}) do
        mtype = stoken()
--  end if
    end while
    integer k = find(mtype,structs)
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
--         or match("CALLBACK",mtype)=length(mtype)-7
--         or match("Proc",mtype)=length(mtype)-3 then
           or endswith("CALLBACK",mtype)
           or endswith("Proc",mtype) then
            k = as_ptr
            mname = stoken()
        else
            k = find(mtype,UnicodeNames)
            if k then
                if unicode=-1 then
                    cffi_error(mtype&": set_unicode() has not been called")
                end if
--23/6/19!!
--              k = UnicodeAs[unicode+1][k]
                k = UnicodeAs[k][unicode+1]
                mname = stoken()
            else
                if equal(mtype,"signed") then
                    mtype = stoken()
                elsif equal(mtype,"unsigned") then
                    signed = 0
                    mtype = "u"&stoken()
                end if
                k = find(mtype,SizeNames)
                mname = stoken()
                if k=0 then
--10/5/21:
                    if mname="(" then
                        mname = stoken()
                        bFunc = true
                    end if
--                      while true do
--                          string ftok = stoken()
--?{ftok,ch}
--              if ftok=")" then
--                  if ch!='(' then exit end if
--              end if
--          end while

                    if mname!="*" then
                        cffi_error("unknown size "&mtype)
                    end if
                else
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
                            cffi_error("not supported (size=10/8/12/16?, align=8/2/4/16?!! !! !!)")
                        end if
                    elsif equal(mtype,"short") then
                        if equal(mname,"int") then          -- "short int" -> "short"
                            mname = stoken()
                        end if
--did not work..
--                  elsif equal(mtype,"FAR") then
--                      mname = stoken()
--                      if mname!="*" then
--                          cffi_error("* expected, not "&mtype)
--                      end if
                    end if
                end if
            end if
        end if
        if k!=0 then
            if convert_types then       -- (can be set via set_unicode)
                mtype = SizeNames[k]    -- (replaces eg "LONG" with "long")
            end if
--<         size = Sizes[k][machine]
            size = Sizes[k][machine/32]
            align = size
            signed = SizeSigns[k]
        end if
    end if
--20/9/19:
    if equal(mname,"FAR") then
        mname = stoken()
        if mname!="*" then
            cffi_error("* expected, not "&mtype)
        end if
    end if
    if equal(mname,"*") then    -- "&"? (would need '&' adding twice in stoken())
        mname = stoken()
--17/12/17:
        if equal(mname,"const") then
            mname = stoken()
        end if
        if equal(mname,"*") then
            mname = stoken()
        end if
--10/5/21: (handle "void *(*bzalloc)(void *,int,int);")
--             and "void (*bzfree)(void *,void *);" (via earlier bFunc)
        if mname="(" then
            mname = stoken()
            if mname!="*" then cffi_error("* expected") end if
            mname = stoken()
            if bFunc then ?9/0 end if
            bFunc = true
        end if
        if bFunc then
            while true do
                string ftok = stoken()
--?{ftok,ch}
                if ftok=")" then
                    if ch!='(' then exit end if
                end if
            end while
--trace(1)
        end if
        mtype = "ptr"
--<     size = Sizes[as_ptr][machine]
        size = Sizes[as_ptr][machine/32]
        align = size
        substruct = 0
--28/12/16:
        signed = 1
    end if
    return {mname,substruct,mtype,size,align,signed}
end function

--26/9/23
--function parse_c_struct(bool bStruct, integer machine, base)
function parse_c_struct(bool bStruct, integer machine, base, bool pack1=false)
--
-- internal routine:
--  bStruct is 1 for struct, 0 for union
--< machine is S32 or S64
--  machine is 32 or 64
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
string name="?", token, mtype, mname, subname
integer signed, k, mult, size, subsize, align, substruct, offset
integer sizeofS = 0, widest = 0, isstruct
sequence members = {}, ptrnames = {}
sequence submembers, sm2i, submembernames
sequence res
    if ch!='{' then
        name = stoken()
    end if
    if ch!='{' then cffi_error("{ expected") end if
    {} = stoken()
    while 1 do
        signed = 1
        mtype = stoken()
--11/5/19:
--      isstruct = equal(mtype,"struct")
        isstruct = (equal(mtype,"struct") and ch='{')
        if isstruct or equal(mtype,"union") then
--          res = parse_c_struct(isstruct,machine,base+sizeofS)
            res = parse_c_struct(isstruct,machine,base+sizeofS,pack1)
            {subname,subsize,align,submembers} = res

--DEV pad to align?
            if not pack1 then
                k = remainder(sizeofS,align)
                if k then ?9/0 end if
--                  padding = align-k
--                  sizeofS += padding
--              end if
            end if
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
--if mname="FAR" then ?9/0 end if
                members = append(members,{mname,sm2i})
--              members = append(members,{mname,{mtype,size,offset,signed}})
            end for
            widest = subsize
            sizeofS += subsize
        else
--9/5/18 support eg "int x,y;" (not just/vs only "int x;")
            while 1 do
                {mname,substruct,mtype,size,align,signed} = do_type(mtype,machine)

--if mname="FAR" then ?9/0 end if
                if equal(mname,";") then
                    cffi_error("member name expected")
                end if
                mult = 1
                if ch='[' then
                    nch()
                    if ch=']' then
                        if not equal(mtype,"ptr") then
                            mult = 0
                        end if
                    else
--                      if mtype="ptr" then
--                          {} = stoken()
--                      else
                            mult = toInt(stoken())
--                      end if
                    end if
                    if ch!=']' then cffi_error("] expected") end if
                    nch()
                    if ch='[' then
                        cffi_error("multi-dimensional arrays are not (yet) supported")
                    end if
                elsif ch=':' then
                    cffi_error("bitfields are not (yet) supported")
                end if
                token = stoken()
--              if not equal(token,";") then cffi_error("; expected") end if
                if size>widest then
                    widest = size
                end if
--              if bStruct then
                if bStruct and not pack1 then
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
--if mname="FAR" then ?9/0 end if
                    members = append(members,{mname,{mtype,size,base+sizeofS,signed}})
                end if
                if bStruct then
                    sizeofS += size*mult
                end if
                if not equal(token,",") then exit end if
            end while
            if not equal(token,";") then cffi_error("; expected") end if
        end if
        if ch='}' then exit end if
    end while
    if not pack1 then
        k = remainder(sizeofS,widest)
        if k then
            sizeofS += widest-k
        end if
    end if
    {} = stoken()   -- discard '}'
    if ch!=-1 then
        if ch!=';' then
            if ch='*' then cffi_error("name of *") end if
            name = stoken()
            while ch=',' do
                {} = stoken()   -- discard ','
                if ch='*' then
                    {} = stoken()
                    token = stoken()
                    ptrnames = append(ptrnames,token)
                else
                    token = stoken()
--21/9/19:
                    if token="FAR" then token = stoken() end if
                    if token="*" then token = stoken() end if
                end if
            end while
        end if
        if ch!=-1 then
            if ch!=';' then cffi_error("; expected") end if
            {} = stoken()
        end if
    end if
    res = {name,sizeofS,widest,columnize(members),ptrnames}
    return res
end function

sequence dll_names,
         dll_addrs

sequence C_SIZES,C_CONSTS

bool cffi_init = false

procedure init_cffi()

    if platform()=LINUX then
        L = 8
    end if

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
-- made signed 28/12/16 (*2):
                                                  {"ptr",       1,{4,8}},       -- (no point having ptr/uptr that I can think of)
                                                  {"size_t",    1,{4,8}},
                                                  $})


--A partial definition of long double from wikipedia( https://en.wikipedia.org/wiki/Data_structure_alignment ):
-- on 32 bit:
--A long double (ten bytes with C++Builder and DMC, eight bytes with Visual C++, twelve bytes with GCC) will be 
--  8-byte aligned with C++Builder, 2-byte aligned with DMC, 8-byte aligned with Visual C++ and 4-byte aligned with GCC.
-- on 64 bit:
--A long double (eight bytes with Visual C++, sixteen bytes with GCC) will be 8-byte aligned with Visual C++ and 16-byte aligned with GCC.
-- (PL: Note that Windows64 is an LLP64 data model, not LP64 which the above article seems to be writing about.)
--
--For more information, be sure to read "calling_conventions.pdf" by Agner Fog, especially table 1 on page 6 ("Data Sizes").
--
--And here's another gratuitous link: https://en.wikipedia.org/wiki/Long_double
--
--If you ask me, life would be simpler if float/double/longdouble were instead more explicitly named say flt32/flt64/flt80.
--
--Might I suggest trying flt80 for any long doubles you happen to run into, not that I have any evidence that will work.
--

    as_char = find("char",SizeNames)
    as_uchar = find("uchar",SizeNames)
    as_int = find("int",SizeNames)
    as_uint = find("uint",SizeNames)
    as_ptr = find("ptr",SizeNames)
    as_short = find("short",SizeNames)
    as_ushort = find("ushort",SizeNames)
    as_long = find("long",SizeNames)
    as_ulong = find("ulong",SizeNames)
    as_float = find("float",SizeNames)
    as_double = find("double",SizeNames)
    as_int64 = find("int64",SizeNames)
    as_uint64 = find("uint64",SizeNames)

-- From MSDN. Suspect items are commented out, please check results carefully if you uncomment them.
--              (not that there is any warranty that the others are all perfect!)
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
                                         {"HTREEITEM",      as_ptr},
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
                                         {"gint8",          as_char},
--                                       {"int8_t",         as_[u]char},
                                         {"UCHAR",          as_uchar},
                                         {"UINT8",          as_uchar},
                                         {"u_char",         as_uchar},
                                         {"WORD",           as_short},
                                         {"ATOM",           as_short},
                                         {"LANGID",         as_short},
                                         {"INT16",          as_short},
                                         {"SHORT",          as_short},
--                                       {"int16_t",        as_[u]short},
                                         {"UINT16",         as_ushort},
                                         {"guint16",        as_ushort},
                                         {"USHORT",         as_ushort},
                                         {"WCHAR",          as_ushort},
                                         {"u_short",        as_ushort},
                                         {"BOOL",           as_int},
                                         {"HFILE",          as_int},        -- (obsolete)
                                         {"INT",            as_int},
                                         {"INT32",          as_int},
                                         {"LONG32",         as_int},
                                         {"gint",           as_int},
--                                       {"gint",           as_long}, -- no help!
--                                       {"int32_t",        as_[u]int},
                                         {"REGSAM",         as_int},        -- assumes C enums are signed ints
                                         {"DWORD32",        as_uint},
                                         {"UINT",           as_uint},
                                         {"UINT32",         as_uint},
                                         {"guint32",        as_uint},
                                         {"ULONG32",        as_uint},
                                         {"guint",          as_uint},
                                         {"LONG",           as_long},
                                         {"HRESULT",        as_long},
                                         {"DWORD",          as_ulong},
                                         {"COLORREF",       as_ulong},
                                         {"LCID",           as_ulong},
                                         {"LCTYPE",         as_ulong},
                                         {"LGRPID",         as_ulong},
                                         {"SOCKET",         as_ulong},
                                         {"ULONG",          as_ulong},
                                         {"u_long",         as_ulong},
--                                       {"float",          as_float},
                                         {"FLOAT",          as_float},
                                         {"gdouble",        as_double},
--                                       {"gdouble",        as_int64},
                                         {"INT64",          as_int64},
                                         {"LONGLONG",       as_int64},
                                         {"LONG64",         as_int64},
--                                       {"USN",            as_int64},
--                                       {"int64_t",        as_[u]int64},
                                         {"DWORDLONG",      as_uint64},
                                         {"DWORD64",        as_uint64},
                                         {"QWORD",          as_uint64},
                                         {"UINT64",         as_uint64},
                                         {"uint64_t",       as_uint64},
                                         {"ULONGLONG",      as_uint64},
                                         {"ULONG64",        as_uint64},
                                         {"GdkEventType",   as_long},
                                         {"GdkCrossingMode",as_int},
                                         {"GdkNotifyType",  as_int},
                                         {"gboolean",       as_int},
--                                       {"gint8",          as_char},
--                                       {"guint32",        as_uint},
--                                       {"guint",          as_uint},
--                                       {"gint",           as_int},
                                         {"gint16",         as_short},
--                                       {"guint16",        as_ushort},
--                                       {"guint8",         as_uchar},
                                         {"byte",           as_char},
                                         {"ubyte",          as_uchar},
                                         {"ModifierType",   as_int},
                                         $})

    {UnicodeNames,UnicodeAs} = columnize({
                                              {"TBYTE",{as_char,as_ushort}},
                                              {"TCHAR",{as_char,as_ushort}},
                                              $})

    structs = {}
    stsizes = {}
    saligns = {}
    smembers = {}

    dll_names = {}
    dll_addrs = {}
    {C_SIZES,C_CONSTS} = columnize({
                                         {{1,1},C_BYTE},    -- (=== C_CHAR)
                                         {{1,0},C_UBYTE},   -- (=== C_UCHAR)
                                         {{2,1},C_WORD},    -- (=== C_SHORT)
                                         {{2,0},C_USHORT},
                                         {{4,1},C_INT},
                                         {{4,0},C_DWORD},   -- (=== C_PTR, C_HWND, etc)
                                         {{8,0},C_PTR},
--DEV temp (do_type should probably zero signed on all pointers)
                                         {{8,1},C_PTR},
                                         $})
--/* Some of these may need adding:
--X initialConstant("C_CHAR",       #01000001)
--X initialConstant("C_BYTE",       #01000001)
--X initialConstant("C_UCHAR",      #02000001)
--X initialConstant("C_UBYTE",      #02000001)
--X initialConstant("C_SHORT",      #01000002)
--X initialConstant("C_WORD",       #01000002)
--X initialConstant("C_USHORT",     #02000002)
--  initialConstant("C_INT64",      #01000008)  -- (a 64 bit signed integer)
--  initialConstant("C_QWORD",      #02000008)  -- (a 64 bit unsigned integer)
--  initialConstant("C_FLOAT",      #03000004)
--*/

--/* bitfields: in the following, fBinary..fDummy2 occupy one dword, ie 4 bytes or 32 bits...
                 // and fBinary can only be 0/1, fDtrControl 0..3, ie 0..power(2,b)-1
// each field has size,offset,signed: if size&signed -ve it is a bitmap field??
typedef struct _DCB {
  DWORD DCBlength;
  DWORD BaudRate;
  DWORD fBinary : 1;
  DWORD fParity : 1;
  DWORD fOutxCtsFlow : 1;
  DWORD fOutxDsrFlow : 1;
  DWORD fDtrControl : 2;
  DWORD fDsrSensitivity : 1;
  DWORD fTXContinueOnXoff : 1;
  DWORD fOutX : 1;
  DWORD fInX : 1;
  DWORD fErrorChar : 1;
  DWORD fNull : 1;
  DWORD fRtsControl : 2;
  DWORD fAbortOnError : 1;
  DWORD fDummy2 : 17;
  WORD  wReserved;
  WORD  XonLim;
  WORD  XoffLim;
  BYTE  ByteSize;
  BYTE  Parity;
  BYTE  StopBits;
  char  XonChar;
  char  XoffChar;
  char  ErrorChar;
  char  EofChar;
  char  EvtChar;
  WORD  wReserved1;
} DCB, *LPDCB;

That's actually a pretty decent example, with fBinary..fDummy2 occupying all 32 bits of a single dword
--*/
    cffi_init = true
end procedure

--<?
--with trace
--global function define_struct(string struct_str, integer machine=0, integer add=1)
global function define_struct(string struct_str, integer machine=machine_bits(), bAdd=true)
--
-- The struct_str parameter is eg "typedef struct{LONG left; .. } RECT;"
--  - note that without a "typedef", nothing gets stored permanantly.
-- The machine parameter can be set to 32 or 64, for testing purposes.
-- The add parameter is set to 0 for testing (override/ignore "typedef")
--
-- If add is 1 and struct_str contains "typedef", the return value is a
--  small integer id that can be used in calls to get/set_struct_value.
--
-- Otherwise full details of the structure are returned, which you can
--  display, use to write a little help file, or perhaps even directly 
--  use the sizes and offsets etc. For details see parse_c_struct().
--
    if not cffi_init then init_cffi() end if
--  if machine=0 then
----<       if machine_bits()=32 then
----            machine = S32
----        else
----            machine = S64
----        end if
--      machine = machine_bits()
--  end if
--trace(1)
    s = struct_str
    sidx = 1
    ch = s[1]
    skipspaces()
--26/9/23:
    bool pack1 = false
    if ch='#' then
        string pp1 = "#pragma pack(1)"
        assert(s[sidx..sidx+length(pp1)-1]==pp1)
        sidx += length(pp1)
        ch = s[sidx]
        skipspaces()
        if machine=32 then -- as per docs
            pack1 = true
        end if
    end if
    integer typedef = 0
    string token = stoken()
    if equal(token,"typedef") then
        typedef = 1
        token = stoken()
    end if
    if not equal(token,"struct") then
        cffi_error("struct expected")
    end if
--?"pcs"
-- 19/2/21
--  sequence res = parse_c_struct(1,machine,0)
    sequence res = parse_c_struct(1,machine,0,pack1)
--/*
    try
        res = parse_c_struct(1,machine,0)
    catch e
--      crash(e[E_USER],{},2)
        throw(e[E_USER])
    end try
--*/
--?"pcs ret"
    if bAdd and typedef then
        return add_struct(res)
    end if
    return res
end function

global function get_struct_size(integer id)
    if not cffi_init or id<0 then ?9/0 end if
    return stsizes[id]
end function

global function allocate_struct(integer id, bool cleanup=true)
-- if cleanup is false, remember to free() the result once done.
    if not cffi_init or id<0 then ?9/0 end if
    integer size = stsizes[id]
    atom pMem = allocate(size,cleanup)
    mem_set(pMem,0,size)
    return pMem
end function

--global procedure clear_struct(integer id, atom pMem)
--  if not cffi_init or pMem=0 or id<0 then ?9/0 end if
--  integer size = stsizes[id]
--  mem_set(pMem,0,size)
--end procedure

function get_smembers(integer id)
    if not cffi_init or id<0 then ?9/0 end if
    return smembers[id]
end function

--global procedure set_struct_field(integer id, atom pStruct, string fieldname, atom v)
--global procedure set_struct_field(integer id, atom pStruct, atom_string field, atom_string v)
global procedure set_struct_field(integer id, atom pStruct, atom_string field, object v)
    if not cffi_init or pStruct=0 or id<0 then ?9/0 end if
    sequence {membernames,details} = get_smembers(id)
    integer k = iff(string(field)?find(field,membernames):field)
    integer {?,size,offset} = details[k]
--  integer {?,size,offset,signed} = details[k]
    if atom(v) then
        pokeN(pStruct+offset,v,size)
    else
        if not string(v) then ?9/0 end if
        -- (the following should never trigger, since something similar
        --  when defining the TCHAR[] should have already have happened.)
--?details[k]
        if unicode=-1 then ?9/0 end if
        if unicode=0 then -- ansi
            poke(pStruct+offset,v)
        else
            poke2(pStruct+offset,utf8_to_utf16(v))
        end if
    end if
end procedure

--DEV 17/4/23 (!!) bAsFlt is a total fudge, this should already know all about that, maybe just use -ve sizes?.
--                  If you do fix this, may I suggest you first test xpGUI.e is getting bAsFlt correct, before ripping it out (from both).
--global function get_struct_field(integer id, atom pStruct, string fieldname)
global function get_struct_field(integer id, atom pStruct, atom_string field, bool bAsFlt=false)
    if not cffi_init or pStruct=0 or id<0 then ?9/0 end if
    sequence {membernames,details} = get_smembers(id)
--sequence membernames,details
--integer k
--integer size, offset, signed
--  if not cffi_init then ?9/0 end if
--  {membernames,details} = smembers[id]
--  k = find(fieldname,membernames)
    integer k = iff(string(field)?find(field,membernames):field)
    integer {?,size,offset,signed} = details[k]
    if bAsFlt then
        sequence f4or8 = peek({pStruct+offset,size})
        return iff(size=4?float32_to_atom(f4or8):float64_to_atom(f4or8))
    end if
    return peekNS(pStruct+offset,size,signed)
end function

global function get_struct_string(integer id, atom pStruct, string field, integer len)
    if not cffi_init or pStruct=0 or id<0 then ?9/0 end if
    sequence {membernames,details} = get_smembers(id)
    integer k = find(field,membernames)
    integer {?,?,offset} = details[k]
    if unicode=1 then -- widestring
        return utf16_to_utf8(peek2u({pStruct+offset,len}))
    end if
    return peek({pStruct+offset,len})
end function

--global function get_field_details(integer id, string fieldname)
global function get_field_details(integer id, atom_string field)
    if not cffi_init or id<0 then ?9/0 end if
--  sequence {membernames,details} = smembers[id]
    sequence {membernames,details} = get_smembers(id)
    integer k = iff(string(field)?find(field,membernames):field)
--  integer {?,size,offset,sgn} = details[k]
    string mtype
    integer size, offset, signed
    {mtype,size,offset,signed} = details[k]
--  return {offset,size,signed}
    return {offset,size,signed,mtype}
--  return details[k]
end function

function open_lib(object lib)
-- internal caching wrapper to open_dll()
    if string(lib) then
        integer k = find(lib,dll_names)
        if k=0 then
            dll_names = append(dll_names,lib)
            lib = open_dll(lib)
            dll_addrs = append(dll_addrs,lib)
        else
            lib = dll_addrs[k]
        end if
    end if
    if lib<=0 then ?9/0 end if
    return lib
end function

function define_c(object lib, string cdef, integer func, integer machine)
--
-- internal wrapper to define_c_func/define_c_proc.
--
--  eg define_cffi_func("kernel32.dll","DWORD WINAPI GetLastError(void);")
--  firstly performs lib=open_dll("kernel32.dll") and then returns
--  (non-0) result from define_c_func(lib,"GetLastError",{},C_INT).
--
--  lib can be a string or the non-0 atom result from open_dll().
--  cdef is usually just copied from MSDN or similar.
--  func is 0 or 1, from define_cffi_func/define_cffi_proc below.
--< machine can be set to S32 or S64, or (left) 0 for auto.
--  machine can be set to 32 or 64.
--
-- NOTE: if there are ansi and unicode versions of the routine, you
--  can edit the C definition to eg MessageBoxA or MessageBoxW, or
--  if set_unicode() has been called this routine will retry once.
-- This routine deliberately fails immediately with a fatal error if
--  any problem is detected, rather than return an error code.
--
string name, mtype
integer substruct, size, align, signed, return_type, ptype
integer rid

    if not cffi_init then init_cffi() end if
    sequence args = {}
--  if machine=0 then
----<       if machine_bits()=32 then
----            machine = S32
----        else
----            machine = S64
----        end if
--      machine = machine_bits()
--  end if
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
--  if equal(name,"WINAPI") then
    if find(name,{"WINAPI","WSAAPI"}) then
        name = stoken()
    end if
    if ch!='(' then cffi_error("( expected") end if
    {} = stoken()
    while ch!=')' do
        mtype = stoken()
        if match("_",mtype)=1 then  --  "_In_", "_Inout_opt_", "_Out_" etc
            mtype = stoken()
        elsif equal(mtype,"[") then  -- "[in]", "[in, optional]", etc
            while not equal(mtype,"]") do
                mtype = stoken()
            end while
            mtype = stoken()
        elsif equal(mtype,"void") then
            if ch!=')' then ?9/0 end if
            if length(args) then ?9/0 end if
            exit
        end if
        {?,substruct,mtype,size,align,signed} = do_type(mtype,machine)
        if substruct then ?9/0 end if
--printf(1,"%d\n",size)
--?size
--?signed
--?C_SIZES
--?{{size,signed},C_SIZES}
        ptype = C_CONSTS[find({size,signed},C_SIZES)]
        args = append(args,ptype)
        if ch=')' then exit end if
        if ch!=',' then cffi_error(", expected") end if
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
        rid = define_c_func(lib,name,args,return_type,false)
    else
        rid = define_c_proc(lib,name,args,false)
    end if
    if rid=-1 then
        if unicode=-1 then crash(`"%s" not found (unicode still -1)`,{name},3) end if
--      if unicode=-1 then throw(`"%s" not found (unicode still -1)`,{name}) end if
--      name &= AW[unicode+1] -- (errors out if unicode still -1)
        name &= "AW"[unicode+1] -- (errors out if unicode still -1)
        if func then
            rid = define_c_func(lib,name,args,return_type,false)
        else
            rid = define_c_proc(lib,name,args,false)
        end if
--      if rid=-1 then ?9/0 end if
        if rid=-1 then crash(`"%s" not found`,{name[1..-2]},3) end if
--      if rid=-1 then throw(`"%s" not found`,{name[1..-2]}) end if
    end if
    return rid
end function

--<global function define_cffi_func(object lib, string cdef, integer machine=0)
global function define_cffi_func(object lib, string cdef, integer machine=machine_bits())
    return define_c(lib,cdef,1,machine)
end function

--<global function define_cffi_proc(object lib, string cdef, integer machine=0)
global function define_cffi_proc(object lib, string cdef, integer machine=machine_bits())
    return define_c(lib,cdef,0,machine)
end function


