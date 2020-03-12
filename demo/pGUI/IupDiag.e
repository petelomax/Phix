--
-- possible beginnings of an IupDiag routine.
-- I started this when struggling to find SHRINK, did not really help at that time.
-- I clearly got something wrong with pEntries, getting {0,0,0,0,0,0}..??
--

sequence binary
atom address

constant escapes = "\0\t\r\n"
constant escaped = "0trn"

function toString(integer size)
integer ch
integer escidx
string res = ""
integer instring = 0
integer nostrip = 0
integer i

    i = 1
    while 1 do -- until (size=-1 and ch = '\0') or (size!=-1 and i>size)
        ch = binary[address+i]
        if size=-1 then
            if ch='\0' then exit end if
        end if
        escidx = find(ch,escapes)
        if escidx!=0
        or (ch>=' ' and ch<='~') then
            if not instring then
                if length(res) then
                    res &= "&"
                    nostrip = 1
                end if
                res &= `"`
                instring = 1
            end if
            if escidx!=0 then
                res &= `\`&escaped[escidx]
            else
                res &= ch
            end if
        else
            if instring then
                res &= `"`
                instring = 0
            end if
            if length(res) then
                res &= "&"
                nostrip = 1
            end if
            res &= sprintf("0x%02x",ch)
        end if
        i += 1
        if size!=-1 then
            if i>size then exit end if
        end if
    end while
    if instring then
        if nostrip then
            res &= `"`
        else
            res = res[2..$] -- (remove opening \")
        end if
    end if
    return res
end function

constant atom h8a = allocate(8),
         integer h8i = floor(h8a/4)     -- (easier for ilASM)

function reconstitute_int64(sequence s8)
-- While 64-bit apps use 80-bit floats, which have 64 bits of precision,
--  32-bit apps use 64-bit floats, which only have 53 bits of precision.
-- This retrieves "normal" numbers as "normal" values (eg 0,1,-1,237),
--  but eg #8000000000000001 (or more accurately {1,0,0,0,0,0,0,#80}),
--  as {#80000000,1}, ie {hi_dword,lo_dword}, rather than minus the 1.
-- (If invoked on 64-bit, does a pointless peek and returns an atom.)   [DEV, try below (once this has been ported to pGUI)]
object res
    poke(h8a,s8)
    #ilASM{ 
-- untested on 64 bit:
        [32]
            mov eax,[h8i]
            lea edi,[res]
            fild qword[ebx+eax*4]
--          call %opMovbi               -- call StoreFlt ([edi]:=ST0)
            call :%pStoreFlt        -- ([edi]:=ST0)
--          [PE32,ELF32]
            mov eax,[res]
            mov edi,[h8i]
            cmp eax,h4
--          jl %opRetf
            jl :%opRetf
            fld qword[ebx+eax*4]
            fistp qword[ebx+edi*4]
        [64]
            mov rax,[h8i]
            lea rdi,[res]
            fild qword[rbx+rax*4]       -- (assume signed)
--          call %opMovbi               -- call StoreFlt ([rdi]:=ST0)
            call :%pStoreFlt        -- ([edi]:=ST0)
--          mov rax,[res]
--          mov rdi,[h8i]
--          cmp rax,h4
--          jl %opRetf
--          fld tbyte[rbx+rax*4]
--          fistp tbyte[rbx+rdi*4]
        []
          }
    if peek({h8a,8})=s8 then
        return res
    end if
    poke(h8a,s8)
    return reverse(peek4u({h8a,2}))
end function

integer asHex = 0 -- Normally (with asHex=0) we want the real number, so #01,#00 -> #0001, not #0100 (=256).
                  -- For disassembly: code, and numbers embedded in it, should be shown as-is, little-endian.
                  -- eg mov dx,1 is "BA 01 00", whereas "00 01 BA" would just be completely misleading drivel.
                  -- That is, asHex can be set to 1 before calling decode(), and reset once we're finished.
                  -- Incidentally, xN/toString behave "as asHex=1" but of course we do not want our mov dx,1
                  -- shown as "0xBA&0x01&0x00" or worse mix in some character literals in the middle.

constant mul = {#1,#100,#10000,#1000000,#100000000,#10000000000,#1000000000000,#100000000000000}

integer signed
--integer xlen

--include builtins\pfloat.e -- float80_to_atom

function decode(string description, object size, string notes="")
object val
sequence line
integer byte
atom mi
    line = {address,description,size,0,notes}
    if string(size)
    and size[1]='h' then
        if size="h" then
--          size = xlen
            ?9/0
        elsif size="h1" then
            size = 1
        elsif size="h2" then
            size = 2
        elsif size="h3" then
            size = 3
        elsif size="h4" then
            size = 4
        elsif size!="h8" then
            ?9/0
        end if
    end if
    if integer(size) then
        signed = 0
        val = binary[address+1]
        for i=2 to size do
            byte = binary[address+i]
            if asHex then
                val = val*#100+byte
            else
                if byte!=0 then
                    if i<=length(mul) then
                        mi = mul[i]
                    else
                        mi = power(2,(i-1)*8)
                    end if
                    if i=size and size>4 and and_bits(byte,#80) then
                        byte -= #80
                        signed = 1
                        if byte!=0 then
                            val += byte*mi
                        end if
                    else
                        val += byte*mi
                    end if
                end if
            end if
        end for
    elsif size="x" then
--      val = toString(xlen)
--      size = xlen
        ?9/0
    elsif size="x2" then
        val = toString(2)
        size = 2
    elsif size="x3" then
        val = toString(3)
        size = 3
    elsif size="x4" then
        val = toString(4)
        size = 4
    elsif size="x8" then
        val = toString(8)
        size = 8
    elsif size="f8" then
        val = float64_to_atom(binary[address+1..address+8])
        size = 8
    elsif size="f10" then
        val = float80_to_atom(binary[address+1..address+10])
        size = 10
    elsif size="h8" then
        val = reconstitute_int64(binary[address+1..address+8])
        size = 8
    else
        val = "unknown size"
    end if
    line[4] = val
--  res[tabidx] = append(res[tabidx],line)
    address += size
--  return val
    return line
end function

sequence line
procedure xdecode(string desc, object size)
    line = decode(desc,size)
    ?line
end procedure

constant W = machine_word()
constant P = sprintf("h%d",W)

procedure IupDiag(atom ih)
atom pAttr
sequence pEntries
integer nEnt
--/*
struct Ihandle_
{
  char sig[4];           /**< IUP Signature, initialized with "IUP", cleared on destroy */
  Iclass* iclass;        /**< Ihandle Class */
  Itable* attrib;        /**< attributes table */
  int serial;            /**< serial number used for controls that need a numeric id, initialized with -1 */
  InativeHandle* handle; /**< native handle. initialized when mapped. InativeHandle definition is system dependent. */
  int expand;            /**< expand configuration, a combination of \ref Iexpand, for containers is a combination of the children expand's */
  int flags;             /**< flags configuration, a combination of \ref Iflags */
  int x, y;              /**< upper-left corner relative to the native parent. always 0 for the dialog. */
  int userwidth,    userheight; /**< user defined size for the control using SIZE or RASTERSIZE */
  int naturalwidth, naturalheight; /**< the calculated size based in the control contents and the user size */
  int currentwidth, currentheight; /**< actual size of the control in pixels (window size, including decorations and margins). */
  Ihandle* parent;       /**< previous control in the hierarchy tree */
  Ihandle* firstchild;   /**< first child control in the hierarchy tree */
  Ihandle* brother;      /**< next control inside parent */
  IcontrolData* data;    /**< private control data. automatically freed if not NULL in destroy */
};
--*/
    binary = peek({ih,4+18*W})
    address = 0
--  printf(1,"sig: %s\n",{toString(4)})
    xdecode("sig","x4")
    xdecode("iclass",P)
    xdecode("attrib",P)
    pAttr = line[4]
    xdecode("serial",W)
    xdecode("handle",P)
    xdecode("expand",W)
    xdecode("flags",W)
    xdecode("x",W)
    xdecode("y",W)
    xdecode("userwidth",W)
    xdecode("userheight",W)
    xdecode("naturalwidth",W)
    xdecode("naturalheight",W)
    xdecode("currentwidth",W)
    xdecode("currentheight",W)
    xdecode("parent",P)
    xdecode("firstchild",P)
    xdecode("brother",P)
    xdecode("data",P)
    ?"======================="
--/*
struct _Itable
{
  unsigned int         entriesSize;
  unsigned int         numberOfEntries;
  unsigned int         tableSizeIndex;  /* index into itable_hashTableSize array */
  Itable_IndexTypes    indexType;  /* type of the index: string or pointer. */
  ItableEntry          *entries;
  ItableContext        context;
};
typedef enum _Itable_IndexTypes
{
  IUPTABLE_POINTERINDEXED = 10, /**< a pointer address is used as key. */
  IUPTABLE_STRINGINDEXED        /**< a string as key */
} Itable_IndexTypes;
typedef struct _ItableEntry
{
  unsigned int  nextItemIndex;
  unsigned int  itemsSize;
  ItableItem*   items;
} ItableEntry;
typedef struct _ItableItem
{
  Itable_Types  itemType;
  ItableKey     key;
  void*         value;
} ItableItem;
typedef enum _Itable_Types
{
  IUPTABLE_POINTER,     /**< regular pointer for strings and other pointers */
  IUPTABLE_STRING,      /**< string duplicated internally */
  IUPTABLE_FUNCPOINTER  /**< function pointer */
} Itable_Types;
typedef struct _ItableKey
{
  unsigned long keyIndex;  /* the secondary hash number */
  const char   *keyStr;
} ItableKey;
typedef struct _ItableContext
{
  unsigned int entryIndex;  /* index at the Itable::entries array */
  unsigned int itemIndex;   /* index at the ItableEntry::items array */
} ItableContext;
--*/
    binary = peek({pAttr,7*W})
    address = 0
    xdecode("entriesSize",W)
    xdecode("numberOfEntries",W)    -- 6, looking good...
    nEnt = line[4]
    xdecode("tableSizeIndex",W)
    xdecode("indexType",W)
    if line[4]!=11 then ?9/0 end if -- must be IUPTABLE_STRINGINDEXED
    xdecode("entries",P)
--?line[4]
--  pEntries = peekNS({line[4],nEnt},W,0)
    pEntries = {peekNS(line[4],W,0)}
?pEntries -- fails (all zeroes)
    xdecode("context.entryIndex",W)
    xdecode("context.itemIndex",W)

end procedure

--/*
-- small:
6
0
"\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0"
{}
={0,"sig","x4","IUP\\0",""}
{4,"iclass","h4",35672720,""}
{8,"attrib","h4",35680296,""}
={12,"serial",4,101,""}
{16,"handle","h4",14158356,""}
={20,"expand",4,12,""}
={24,"flags",4,0,""}
={28,"x",4,38,""}
={32,"y",4,2,""}
={36,"userwidth",4,0,""}
={40,"userheight",4,0,""}
={44,"naturalwidth",4,61,""}
={48,"naturalheight",4,21,""}
{52,"currentwidth",4,77,""}
={56,"currentheight",4,21,""}
{60,"parent","h4",36014864,""}
={64,"firstchild","h4",0,""}
{68,"brother","h4",36015480,""}
{72,"data","h4",7151400,""}
big:
6
0
"\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0"
{}
={0,"sig","x4","IUP\\0",""}
{4,"iclass","h4",36336624,""}
{8,"attrib","h4",36463504,""}
={12,"serial",4,101,""}
{16,"handle","h4",40830516,""}
={20,"expand",4,12,""}
={24,"flags",4,0,""}
={28,"x",4,38,""}
={32,"y",4,2,""}
={36,"userwidth",4,0,""}
={40,"userheight",4,0,""}
={44,"naturalwidth",4,61,""}
={48,"naturalheight",4,21,""}
{52,"currentwidth",4,1127,""}
={56,"currentheight",4,21,""}
{60,"parent","h4",36789840,""}
={64,"firstchild","h4",0,""}
{68,"brother","h4",36789136,""}
{72,"data","h4",7151232,""}

--*/

