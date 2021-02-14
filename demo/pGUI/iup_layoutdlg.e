--with trace
--
-- iuplayoutdlg.e
-- ==============
--
-- This is a straight translation of iup_layoutdlg.c (3.17 or 3.23 ish?) to Phix.
--
--  The original c version is included in the distribution for the express purpose
--  of comparing against any new releases of IUP and applying changes manually. [DEV no point...]
--  If it turns out that C file is regularly and heavily updated, maybe I'll write 
--  something to automate the conversion; if it is only rarely and lightly updated, 
--  then I'll be glad that I didn't bother. Also, tedious as it may be, doing this
--  manually is still a valuable learning/documentation exercise for me. Besides,
--  with a bit of luck this might prove extendable in ways I cannot yet imagine.
--
-- UPDATE: this was just a bit of fun and nowhere near as useful as I first thought.
--  The dialog is fine, but the interface is all wrong. Yes, you could run the thing
--  and suck out of it, but it would be much better to pass a full definition over
--  and return that, modified. 
--  How about expanding on the paranormalised idea? The "gui" (ie the window painter)
--  simply ignores anything else.

--
-- Author Pete Lomax, May 2016
-- Original Copyright Notice (duplicated from "iup.h" as requested):
/******************************************************************************
* Copyright (C) 1994-2015 Tecgraf/PUC-Rio.
*
* Permission is hereby granted, free of charge, to any person obtaining
* a copy of this software and associated documentation files (the
* "Software"), to deal in the Software without restriction, including
* without limitation the rights to use, copy, modify, merge, publish,
* distribute, sublicense, and/or sell copies of the Software, and to
* permit persons to whom the Software is furnished to do so, subject to
* the following conditions:
*
* The above copyright notice and this permission notice shall be
* included in all copies or substantial portions of the Software.
*
* THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
* EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
* MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
* IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
* CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
* TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
* SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
******************************************************************************/

--DOC: Note this does not invoke the official shared library version of the same name,
--      instead ~2750 lines of iup_layoutdlg.c have been manually translated to Phix,
--      primarily to facilitate output in Phix (vs C/lua/LED), and hence new versions 
--      of IUP may require substantial work to apply the matching changes to Phix.
--      (If and when that becomes an issue, I'll try automating it, but not before.)
-- The following essentially internal routines are used in iup_layoutdlg.e, but are
--  otherwise not documented or offically supported:

--SUG: provide as input the Phix source, and as a first step validate that can be
--      recreated perfectly from the running IupDialog. Output to a temp file and
--      use plan9 (in Edix) to apply changes? Or perhaps a stub such as:
--      global constant src=<layout_file>.e; global string result=""
--      include pGUI.e; include iup_layoutdlg.e; include <layout_file>.e; 
--      Ihandle layout = IupLayoutDialog(<dialog_name>)
--      IupShowXY(layout, IUP_CURRENT, IUP_CURRENT)
--      if length(result) then write_file("temp",result); ping(Edix) end if

include pGUI.e  -- (Ihandle etc)

constant -- (copied from pGUI, rather than made global)
--       D  = C_DOUBLE, 
         F  = C_FLOAT,      -- NB: VM/pcfunc.e may not be up to this.. [edited 25/2/16]
         I  = C_INT,
--       L  = C_LONG,
         P  = C_POINTER, 
--       U  = C_UINT,
--       UC = C_UCHAR,
--       UL = C_ULONG,
         $

atom xiupAttribGet = NULL,
     xiupAttribGetInherit,
     xiupAttribGetInt,
     xiupAttribGetLocal,
--   xiupAttribSetInt,
     xiupClassRegisterGetAttribute,
     xiupDlgListVisibleCount,
     xiupDlgListCount,
     xiupDlgListFirst,
     xiupDlgListNext,
     xiupdrvDrawCreateCanvas,
     xiupdrvDrawRectangle,
     xiupdrvDrawLine,
     xiupdrvDrawSetClipRect,
     xiupdrvDrawImage,
     xiupdrvDrawGetSize,
     xiupdrvDrawText,
     xiupdrvDrawResetClip,
     xiupdrvDrawSelectRect,
     xiupdrvDrawFlush,
     xiupdrvDrawKillCanvas,
     xiupdrvGetScreenDpi,
     xiupRegisterFindClass,
     xiupObjectCheck,
     xiupFocusCanAccept

--DEV/doc (NO: these should be moved to iuplayoutdlg.e [test is in tee.exw])
--/*
char* iupAttribGet(Ihandle *ih, const char *name)     
Returns the attribute from the hash table only. 
NO inheritance, NO control implementation, NO defalt value here. 
--*/
function iupAttribGet(Ihandle ih, string name)
    atom ptr = c_func(xiupAttribGet, {ih, name})
    if ptr=NULL then return "" end if
    return peek_string(ptr)
end function

--/*
char* iupAttribGetInherit(Ihandle *ih, const char *name)
Returns the attribute from the hash table only, but if not defined then checks in its parent tree. 
NO control implementation, NO defalt value here. 
Used for EXPAND and internal attributes inside a dialog. 
--*/
function iupAttribGetInherit(Ihandle ih, string name)
    atom ptr = c_func(xiupAttribGetInherit, {ih, name})
    if ptr=NULL then return "" end if
    return peek_string(ptr)
end function

--/* unused
function iupAttribGetInheritInt(Ihandle ih, string name)
    atom ptr = c_func(xiupAttribGetInherit, {ih, name})
    return ptr
end function
--*/

--/*
int iupAttribGetInt(Ihandle *ih, const char *name)
Same as iupAttribGetStr but returns an integer number. Checks also for boolean values. 
--*/
function iupAttribGetInt(Ihandle ih, string name)
    atom res = c_func(xiupAttribGetInt, {ih, name})
    return res
end function

--/*
char* iupAttribGetLocal(Ihandle *ih, const char *name)    
Returns the attribute from the hash table as a string, but if not defined then checks in the control implementation, if still not defined then returns the registered default value if any. 
NO inheritance here. Used only in the IupLayoutDialog. 
--*/
function iupAttribGetLocal(Ihandle ih, string name)
    atom ptr = c_func(xiupAttribGetLocal, {ih, name})
    if ptr=NULL then return "" end if
    return peek_string(ptr)
end function

--procedure iupAttribSetInt(Ihandle ih, string name, integer i)
--  c_proc(xiupAttribSetInt, {ih, name, i})
--end procedure

function iupClassGetAttribNameInfo(atom iclass, string name)
atom pMem = allocate(8,1)
object def_value
atom flags
    c_proc(xiupClassRegisterGetAttribute,{iclass,name,NULL,NULL,pMem,NULL,pMem+4})
    def_value = peek4u(pMem)
    if def_value!=NULL then
        def_value = peek_string(def_value)
    end if
    flags = peek4u(pMem+4)
    return {def_value,flags}
end function

function iupDlgListVisibleCount()
integer res = c_func(xiupDlgListVisibleCount,{})
    return res
end function

function iupDlgListCount()
integer res = c_func(xiupDlgListCount,{})
    return res
end function

function iupDlgListFirst()
Ihandle ih = c_func(xiupDlgListFirst,{})
    return ih
end function

function iupDlgListNext()
Ihandln ih = c_func(xiupDlgListNext,{})
    return ih
end function

function iupdrvDrawCreateCanvas(Ihandle ih)
atom dc = c_func(xiupdrvDrawCreateCanvas,{ih})
    return dc
end function

--function iupStrToRGB(string s)
--sequence rbg
--atom pMem = allocate(machine_word()*3)
--  if c_func(x
--end function

procedure iupdrvDrawRectangle(atom dc, integer x1, x2, y1, y2, r, g, b, style)
    c_proc(xiupdrvDrawRectangle,{dc, x1, x2, y1, y2, r, g, b, style})
end procedure

procedure iupdrvDrawLine(atom dc, integer x1, x2, y1, y2, r, g, b, style)
    c_proc(xiupdrvDrawLine,{dc, x1, x2, y1, y2, r, g, b, style})
end procedure

procedure iupdrvDrawSetClipRect(atom dc, integer x1, x2, y1, y2)
    c_proc(xiupdrvDrawSetClipRect,{dc, x1, x2, y1, y2})
end procedure

--<void iupDrawImage(IdrawCanvas* dc, const char* name, int make_inactive, int x, int y, int *img_w, int *img_h)
--  function iupdrvDrawImage(atom dc, string name, integer make_inactive, x, y)
--  integer w,h
--  atom pMem = allocate(machine_word()*2)
--      c_proc(xiupdrvDrawImage, {dc,name,make_inactive,x,y,pMem,pMem+machine_word()})
--      {w,h} = peekNS({pMem,2},machine_word(),1)
--      free(pMem)
--      return {w,h}
--  end function
-->void iupdrvDrawImage(IdrawCanvas* dc, const char* name, int make_inactive, int x, int y)
procedure iupdrvDrawImage(atom dc, string name, integer make_inactive, x, y)
    c_proc(xiupdrvDrawImage, {dc,name,make_inactive,x,y})
end procedure

function iupdrvDrawGetSize(atom dc)
integer w,h
atom pMem = allocate(machine_word()*2)
    c_proc(xiupdrvDrawGetSize, {dc,pMem,pMem+machine_word()})
    {w,h} = peekNS({pMem,2},machine_word(),1)
    free(pMem)
    return {w,h}
end function

--void iupdrvDrawText(IdrawCanvas* dc, const char* text, int len, int x, int y, unsigned char r, unsigned char g, unsigned char b, const char* font);
procedure iupdrvDrawText(atom dc, string text, integer len, x, y, r, g, b, string font)
    c_proc(xiupdrvDrawText,{dc, text, len, x, y, r, g, b, font})
end procedure

procedure iupdrvDrawResetClip(atom dc)
    c_proc(xiupdrvDrawResetClip,{dc})
end procedure

procedure iupdrvDrawSelectRect(atom dc, integer x, y, w, h)
    c_proc(xiupdrvDrawSelectRect,{dc, x, y, w, h})
end procedure

procedure iupdrvDrawFlush(atom dc)
    c_proc(xiupdrvDrawFlush,{dc})
end procedure

procedure iupdrvDrawKillCanvas(atom dc)
    c_proc(xiupdrvDrawKillCanvas,{dc})
end procedure

function iupRegisterFindClass(string name)
atom iclass = c_func(xiupRegisterFindClass,{name})
    return iclass
end function

function iupObjectCheck(Ihandle ih)
integer res = c_func(xiupObjectCheck,{ih})
    return res
end function

function iupFocusCanAccept(Ihandle ih)
integer res = c_func(xiupFocusCanAccept,{ih})
    return res
end function

-- (struct _iLayoutDialog made static and extended:)
integer layoutdlg_destroy = 0   -- destroy the selected dialog, when the layout dialog is destroyed
integer layoutdlg_changed
Ihandln layoutdlg_dialog    -- the selected dialog
Ihandle layoutdlg_dlg -- (this is the one with the tree and the canvas mimic of layoutdlg_dialog)
Ihandle layoutdlg_tree
Ihandle layoutdlg_status
Ihandln layoutdlg_timer
Ihandln layoutdlg_properties=NULL
Ihandle layoutdlg_canvas
Ihandln layoutdlg_copy
Ihandle layoutdlg_list1
Ihandle layoutdlg_list2
Ihandle layoutdlg_list3
Ihandln layoutdlg_mark=NULL
Ihandle layoutdlg_propelem
--Ihandle layoutcontextelement
Ihandln layoutcontextelement

--typedef enum _IattribFlags{
constant 
--  IUPAF_DEFAULT=0,            /**< inheritable, can have a default value, is a string, can call the set/get functions only if mapped, no ID */
  IUPAF_NO_INHERIT=1,       /**< is not inheritable */
--  IUPAF_NO_DEFAULTVALUE=2,    /**< can not have a default value */
  IUPAF_NO_STRING=4,        /**< is not a string */
--  IUPAF_NOT_MAPPED=8,     /**< will call the set/get functions also when not mapped */
  IUPAF_HAS_ID=16,          /**< can have an ID at the end of the name, automatically set by \ref iupClassRegisterAttributeId */
  IUPAF_READONLY=32,        /**< is read-only, can not be changed */
  IUPAF_WRITEONLY=64,       /**< is write-only, usually an action */
  IUPAF_HAS_ID2=128,        /**< can have two IDs at the end of the name, automatically set by \ref iupClassRegisterAttributeId2 */
--  IUPAF_CALLBACK=256,     /**< is a callback, not an attribute */
  IUPAF_NO_SAVE=512,        /**< can NOT be directly saved, should have at least manual processing */
  IUPAF_NOT_SUPPORTED=1024, /**< not supported in that driver */
  IUPAF_IHANDLENAME=2048    /**< is an Ihandle* name, associated with IupSetHandle */
--  IUPAF_IHANDLE=4096      /**< is an Ihandle* */
--} IattribFlags;



function iLayoutGetTitle(Ihandle ih)
string str = sprintf("[%s]", {IupGetClassName(ih)})
--string title = IupGetAttribute(ih, "TITLE")
string title = iupAttribGetLocal(ih, "TITLE")
string name = IupGetName(ih);
  if length(title) then
      str &= sprintf(" %.50s", {title});
  end if
  if name!="" then
      str &= sprintf(` "%.50s"`, {name})
  end if
  return str
end function

function iLayoutRemoveExt(string title, string ext)
integer lext = length(ext)
    if length(title)>lext
    and title[-1-lext]='.'
    and upper(title[-lext..-1])=upper(ext) then
        title = title[1..-2-lext]
    end if
    return title
end function

function iLayoutHasDigit(string name)
integer ch
  for i=1 to length(name) do
    ch = name[i]
    if ch>='0' and ch<='9' then
      return 1
    end if
  end for
  return 0
end function

function iupStrHasSpace(string str)
  for i=1 to length(str) do
    if str[i]=' ' then return 1 end if
  end for
  return 0
end function

function iupStrConvertToC(string s)
    return substitute_all(s,{"\n","\r","\t"},{`\n`,`\r`,`\t`})
end function

function iupStrFileGetTitle(string file_name)
    for i=length(file_name) to 1 by -1 do
        if find(file_name[i],`\/`) then
            file_name = file_name[i+1..$]
            exit
        end if
    end for
    return file_name
end function

function iupStrFileNameSplit(string file_name, string path, string title)
  /* Look for last folder separator and split title from path */
  for i=length(file_name) to 1 by -1 do
    if find(file_name[i],`\/`) then
      path = file_name[1..i]
      title = file_name[i+1..$]
      exit
    end if
  end for
  return {path,title}
end function

function iupStrBoolean(string str)
  if str="" then return 0 end if
  if str="1" then return 1 end if
  if find(upper(str),{"YES","ON"}) then return 1 end if
  return 0
end function

function iupATTRIB_ISINTERNAL(string name)
    return length(name)>4 and name[1..4]="_IUP"
end function

constant 
--enum Iflags {
  IUP_FLOATING         = 0x01,  /**< is a floating element. FLOATING=Yes */
--  IUP_FLOATING_IGNORE  = 0x02,    /**< is a floating element. FLOATING=Ignore. Do not compute layout. */
--  IUP_MAXSIZE        = 0x04,  /**< has the MAXSIZE attribute set */
--  IUP_MINSIZE        = 0x08,  /**< has the MAXSIZE attribute set */
  IUP_INTERNAL         = 0x10   /**< it is an internal element of the container */
--};

--DEV should probably use cffi for this!
--/*
--DEV see/test with IupDiag.e, esp 64-bit
struct Ihandle_         offset(32)  (64)
{
  char sig[4];               0       0  /**< IUP Signature, initialized with "IUP", cleared on destroy */
  Iclass* iclass;            4       4  /**< Ihandle Class */
  Itable* attrib;            8      12  /**< attributes table */
  int serial;               12      20  /**< serial number used for controls that need a numeric id, initialized with -1 */
  InativeHandle* handle;    16     ?24  /**< native handle. initialized when mapped. InativeHandle definition is system dependent. */
  int expand;               20      32  /**< expand configuration, a combination of \ref Iexpand, for containers is a combination of the children expand's */
  int flags;                24      36  /**< flags configuration, a combination of \ref Iflags */
  int x;                    28      40  /**< upper-left corner relative to the native parent. always 0 for the dialog. */
  int y;                    32      44
  int userwidth;            36      48  /**< user defined size for the control using SIZE or RASTERSIZE */
  int userheight;           40
  int naturalwidth;         44      64  /**< the calculated size based in the control contents and the user size */
  int naturalheight;        48
  int currentwidth;         52      72  /**< actual size of the control in pixels (window size, including decorations and margins). */
  int currentheight;        56
  Ihandle* parent;          60      80  /**< previous control in the hierarchy tree */
  Ihandle* firstchild;      64      88  /**< first child control in the hierarchy tree */
  Ihandle* brother;         68      96  /**< next control inside parent */
  IcontrolData* data;       72     104  /**< private control data. automatically freed if not NULL in destroy */
};
--*/

function ih_handle(Ihandle ih)
atom handle = peekNS(ih+iff(machine_bits()=32?16:24),machine_word(),0)
    return handle
end function

function ih_flags(Ihandle ih)
    integer flags = peek4s(ih+iff(machine_bits()=32?24:36))
    return flags
end function

function ih_x(Ihandle ih)
    integer x = peek4s(ih+iff(machine_bits()=32?28:40))
    return x
end function

function ih_y(Ihandle ih)
    integer y = peek4s(ih+iff(machine_bits()=32?32:44))
    return y
end function

function ih_userwidth(Ihandle ih)
    integer userwidth = peek4s(ih+iff(machine_bits()=32?36:48))
    return userwidth
end function

function ih_userheight(Ihandle ih)
    integer userheight = peek4s(ih+iff(machine_bits()=32?40:52))
    return userheight
end function

function ih_naturalwidth(Ihandle ih)
    integer naturalwidth = peek4s(ih+iff(machine_bits()=32?44:64))
    return naturalwidth
end function

function ih_naturalheight(Ihandle ih)
    integer naturalheight = peek4s(ih+iff(machine_bits()=32?48:48))
    return naturalheight
end function

function ih_currentwidth(Ihandle ih)
    integer currentwidth = peek4s(ih+iff(machine_bits()=32?52:72))
    return currentwidth
end function

function ih_currentheight(Ihandle ih)
    integer currentheight = peek4s(ih+iff(machine_bits()=32?56:76))
    return currentheight
end function

function ih_parent(Ihandle ih)
    Ihandln parent = peekNS(ih+iff(machine_bits()=32?60:80),machine_word(),0)
    return parent
end function

function ih_firstchild(Ihandle ih)
    Ihandln firstchild = peekNS(ih+iff(machine_bits()=32?64:88),machine_word(),0)
    return firstchild
end function

function ih_brother(Ihandle ih)
    Ihandln brother = peekNS(ih+iff(machine_bits()=32?68:96),machine_word(),0)
    return brother
end function

--/*
struct Iclass_
{
  /* Class configuration parameters. */
  char* name;     /**< class name. No default, must be initialized. */
  char* format;   /**< Creation parameters format of the class. \n
                   * Used only for LED parsing. \n
                   * It can have none (NULL), one or more of the following.
                   * - "b" = (unsigned char) - byte
                   * - "c" = (unsigned char*) - array of byte
                   * - "i" = (int) - integer
                   * - "j" = (int*) - array of integer
                   * - "f" = (float) - real
                   * - "s" = (char*) - string 
                   * - "a" = (char*) - name of the ACTION callback
                   * - "h" = (Ihandle*) - element handle
                   * - "g" = (Ihandle**) - array of element handle */
  InativeType nativetype; /**< native type. Default is IUP_TYPEVOID. */
  int childtype;   /**< children count enum: none, many, or n, as described in \ref IchildType. Default is IUP_CHILDNONE. \n
                    * This identifies a container that can be manipulated with IupReparent, IupAppend and IupInsert. \n
                    * Used to control the allowed number of children and define its behavior in the layout processing. \n
                    * The element can still have hidden children even if this is none. */
  int is_interactive; /**< keyboard interactive boolean, 
                       * true if the class can have the keyboard input focus. Default is false. */
  int has_attrib_id;  /**< indicate if any attribute is numbered. Default is not. Can be 1 or 2. */
 <SNIP>
--*/

constant 
--typedef enum _IchildType {
  IUP_CHILDNONE=0  /**< can not add children using Append/Insert */
--  IUP_CHILDMANY=1   /**< can add any number of children. /n
--                     IUP_CHILDMANY+n can add n children. */
--} IchildType;

--DEV document enum!
enum 
--typedef enum _InativeType {
  IUP_TYPEVOID=0,   /**< No native representation - HBOX, VBOX, ZBOX, FILL, RADIO (handle==(void*)-1 always) */
  IUP_TYPECONTROL,  /**< Native controls - BUTTON, LABEL, TOGGLE, LIST, TEXT, MULTILINE, FRAME, others */
  IUP_TYPECANVAS    /**< Drawing canvas, also used as a base control for custom controls. */ 
--  IUP_TYPEDIALOG, /**< DIALOG */
--  IUP_TYPEIMAGE,  /**< IMAGE */
--  IUP_TYPEMENU        /**< MENU, SUBMENU, ITEM, SEPARATOR */
--} InativeType;

--enum{IUP_DRAW_FILL, IUP_DRAW_STROKE, IUP_DRAW_STROKE_DASH};
enum IUP_DRAW_FILL=0, IUP_DRAW_STROKE, IUP_DRAW_STROKE_DASH

function ih_iclass(Ihandle ih)
atom iclass = peekNS(ih+4,machine_word(),0)
    return iclass
end function

--for testing:
function ih_iclass_name(Ihandle ih)
atom iclass = ih_iclass(ih)
atom pName = peek4s(iclass)
    return peek_string(pName)
end function

function ih_iclass_format(Ihandle ih)
atom iclass = ih_iclass(ih)
atom pFormat = peekNS(iclass+machine_word(),machine_word(),0)
string fmt = ""
    if pFormat!=NULL then
        fmt = peek_string(pFormat)
    end if
    return fmt
end function

--Hi Antonio, [ERM, we'd need a dozen or more...]
--  I've hit a bit of a stumbling block trying to get (a read-only copy of) ih->iclass->childtype using the pre-built shared libraries.
--  (What I am actually doing is trying to incorporate a translated/heavily modify copy of IupDialogLayout into my editor/IDE.)
--  As a short-term hack, I have hard-coded some compiler/platform-dependent offsets, but it would be great if iup.dll/.so exported a
--  new function iupGetClassChildType and that way saved me the trouble of worrying about 32/64/win/lnx/etc correctness issues.
--  Using a test that performs the same kind of low-level hack to retrieve the class name, and verifies that against the result from 
--  IupGetClassName(), I should get a pretty good early warning of anything amiss, so this is a low priority request.

function ih_iclass_nativetype(Ihandle ih)
string a = IupGetClassName(ih)
string b = ih_iclass_name(ih)
if a!=b then ?9/0 end if
atom iclass = ih_iclass(ih)
atom nativetype = peek4s(iclass+8)  --DEV 32/64/win/lnx...
    return nativetype
end function

function iclass_nativetype(atom iclass)
integer nativetype = peek4s(iclass+8)   --DEV 32/64/win/lnx...
    return nativetype
end function

function ih_iclass_childtype(Ihandle ih)
string a = IupGetClassName(ih)
string b = ih_iclass_name(ih)
if a!=b then ?9/0 end if
atom iclass = ih_iclass(ih)
atom childtype = peek4s(iclass+12)  --DEV 32/64/win/lnx...
    return childtype
end function

function ih_iclass_has_attrib_id(Ihandle ih)
string a = IupGetClassName(ih)
string b = ih_iclass_name(ih)
if a!=b then ?9/0 end if
atom iclass = ih_iclass(ih)
integer has_attrib_id = peek4s(iclass+20)   --DEV 32/64/win/lnx...
    return has_attrib_id
end function

function iupChildTreeIsChild(Ihandle ih, Ihandle child)
  Ihandle parent;

  if ih == child then
    return 1
  end if
  parent = ih_parent(child)
  while parent!=NULL do
    if parent == ih then
      return 1
    end if
    parent = ih_parent(parent)
  end while

  return 0;
end function


function iupBaseNoSaveCheck(Ihandle ih, string name)
-- (returns 0 to save, 1 to not save)
object tmp
  if find(name,{"BGCOLOR","VISIBLE","SIZE"}) then
--  return iupAttribGet(ih, name)=NULL  /* save if stored at the hash table */
--  return iupAttribGet(ih, name)=""    /* save if stored at the hash table */
    tmp = iupAttribGet(ih, name)
--  ?{name,tmp}
    return tmp=""   /* save if stored at the hash table */
  elsif name="RASTERSIZE" then
--  if iupAttribGet(ih, "SIZE")=NULL    /* save if SIZE is not set, and user size is set */
--  and (ih_userwidth(ih)!=0 or ih_userheight(ih)!=0)
--    return 0;
--  else
--    return 1;
    /* save(0) if SIZE is not set, and user size is set */
--  return iupAttribGet(ih, "SIZE")!=NULL
    return iupAttribGet(ih, "SIZE")!=""
        or (ih_userwidth(ih)=0 and ih_userheight(ih)=0)
  elsif name="POSITION" then
    if and_bits(ih_flags(ih),IUP_FLOATING)  /* save only if floating is set */
    and (ih_x(ih)!=0 or ih_y(ih)!=0) then
      return 0
    end if
  end if
  return 1 /* default is NOT to save */
end function

function iupStrFalse(string str)
  if length(str)
  and (str="0" or find(upper(str),{"NO","OFF"})) then
      return 1
  end if
  return 0
end function

-- Hi Antonio,
-- I had a go at translating iup_layoutdlg.c to phix (my language) and it went fairly well, 
--  however I don't think I managed to get iLayoutAttributeChanged right. Can I ask you for
--  a few words about making something like that available as a standard routine, ideally a
--  new routine that only needs (ih,name) and manages val/def_val/flags internally, ie as a
--  new entry point that will be available in future versions of pre-compiled shared libs.
--  (This isn't time-sensitive, I could easily wait several months for a new release.)
--
--  One thing I have noticed is that my version gets a def_value of "DLGFGCOLOR" for "FGCOLOR",
--  and I cannot see how/why the c version does not export that. Likewise "DEFAULTFONT" for
--  "FONT" - while I could easily add specific kludges for both, I wonder what I am missing.
--  It is of course very likely I introduced several bugs while porting 2750 lines of code.
--
--  In case you are wondering, the idea behind translating iup_layoutdlg.c was partly as a
--  learning exercise, and partly so that I could add an export(phix) menu entry. With the
--  benefit of hindsight, it would probably be just as easy to process the C output, or
--  even reinvent the thing from scratch. All of this is strictly hobby/spare time stuff.
--
function iLayoutAttributeChanged(Ihandle ih, string name, string val, nullable_string def_value, integer flags)
  if (and_bits(flags,IUPAF_NO_STRING) or /* not a string */
      and_bits(flags,IUPAF_HAS_ID) or  /* has id */
      and_bits(flags,or_bits(IUPAF_READONLY,IUPAF_WRITEONLY)))  /* can only read or only write */
  or (val="" or iupATTRIB_ISINTERNAL(val))
  or (and_bits(flags,IUPAF_NO_SAVE) and iupBaseNoSaveCheck(ih, name))  /* can not be saved */
  or (string(def_value) and length(def_value) and upper(def_value)=upper(val))  /* equal to the default value */
  or (find(def_value,{NULL,""}) and iupStrFalse(val)) then /* default=NULL and value=NO */
    return 0
  elsif (and_bits(flags,IUPAF_NO_INHERIT)=0 and ih_parent(ih)!=NULL) then 
    /* if inherit, check if the same value is defined at parent */
    string parent_value = iupAttribGetInherit(ih_parent(ih), name)
    if length(parent_value) and upper(val)=upper(parent_value) then
      return 0
    end if
  end if
  return 1
end function

function iLayoutGetName(Ihandle ih)
string name = IupGetName(ih)
  if length(name) and iupATTRIB_ISINTERNAL(name) then
    name = ""
  end if
  if name="" and IupGetClassType(ih)="dialog" then
    name = iupAttribGet(ih, "_IUP_DIALOG_NAME")
  end if
  return name
end function


/***************************************************************************
                          Tree Utilities
 ***************************************************************************/


procedure iLayoutTreeSetNodeColor(Ihandle tree, integer id, Ihandle ih)
  if ih_handle(ih)!=NULL then
    IupSetAttributeId(tree, "COLOR", id, "0 0 0");
  else
    IupSetAttributeId(tree, "COLOR", id, "128 128 128");
  end if
end procedure

procedure iLayoutTreeSetNodeInfo(Ihandle tree, integer id, Ihandle ih)
    IupSetAttributeId(tree, "TITLE", id, iLayoutGetTitle(ih))
    iLayoutTreeSetNodeColor(tree, id, ih)
    IupTreeSetUserId(tree, id, ih)
end procedure

function iLayoutTreeAddNode(Ihandle tree, integer id, Ihandle ih)
  if ih_iclass_childtype(ih)=IUP_CHILDNONE then
    if ih_parent(ih)=NULL 
    or ih == ih_firstchild(ih_parent(ih)) then
      IupSetAttributeId(tree, "ADDLEAF", id, "")
      id +=1
    else
      IupSetAttributeId(tree, "INSERTLEAF", id, "");
      id = IupGetInt(tree, "LASTADDNODE");
    end if
  else
    if ih_parent(ih)=NULL
    or ih == ih_firstchild(ih_parent(ih)) then
      IupSetAttributeId(tree, "ADDBRANCH", id, "");
      id +=1
    else
      IupSetAttributeId(tree, "INSERTBRANCH", id, "");
      id = IupGetInt(tree, "LASTADDNODE");
    end if
  end if

  iLayoutTreeSetNodeInfo(tree, id, ih)
  return id
end function

procedure iLayoutTreeAddChildren(Ihandle tree, integer parent_id, Ihandle parent)
  Ihandln child = ih_firstchild(parent)
  integer last_child_id = parent_id

  while child!=NULL do
    last_child_id = iLayoutTreeAddNode(tree, last_child_id, child);
    if ih_iclass_childtype(child)!=IUP_CHILDNONE then
      iLayoutTreeAddChildren(tree, last_child_id, child)
    end if
    child = ih_brother(child)
  end while
end procedure

procedure iLayoutTreeRebuild()
Ihandle tree = layoutdlg_tree
  IupSetAttribute(tree, "DELNODE0", "CHILDREN")

  layoutdlg_changed = 0
  layoutdlg_copy = NULL

  IupRefresh(layoutdlg_dialog)

  iLayoutTreeSetNodeInfo(tree, 0, layoutdlg_dialog)
  iLayoutTreeAddChildren(tree, 0, layoutdlg_dialog)

  IupUpdate(layoutdlg_canvas)
end procedure


/***************************************************************************
                         Layout Export
 ***************************************************************************/

--DEV: (duh, would need to be an array[?])
--integer iup_container_idx = 0

function iLayoutExportCountContainersRec(Ihandle ih, integer index)
  Ihandln child = ih_firstchild(ih)
  while child!=NULL do
    if ih_iclass_childtype(child)!= IUP_CHILDNONE then
      if and_bits(ih_flags(child),IUP_INTERNAL)=0 then
        index += 1
--      iupAttribSetInt(child, "_IUP_CONTAINER_INDEX", index)
        IupSetInt(child, "_IUP_CONTAINER_INDEX", index)
--      iup_container_idx = index
        index = iLayoutExportCountContainersRec(child, index)
      end if
    end if
    child = ih_brother(child)
  end while
  return index
end function

function iLayoutExportCountContainers(Ihandle dialog)
integer index = 0
--  iupAttribSetInt(dialog, "_IUP_CONTAINER_INDEX", index)
  IupSetInt(dialog, "_IUP_CONTAINER_INDEX", index)
--  iup_container_idx = index
  index = iLayoutExportCountContainersRec(dialog, index)
  return index+1
end function

procedure iLayoutExportWriteAttrib(integer fn, string name, string val, string indent, integer typ)
  if typ==1 then    /* Lua */
    name = lower(name)
    if iLayoutHasDigit(name) then
      printf(fn, "%s[\"%s\"] = \"%s\",\n", {indent, name, val})
    else
      printf(fn, "%s%s = \"%s\",\n", {indent, name, val})
    end if
  elsif typ==-1 then /* LED */
    name = upper(name);
    if iupStrHasSpace(val) then
      printf(fn, "%s%s = \"%s\",\n", {indent, name, val})
    else
      printf(fn, "%s%s = %s,\n", {indent, name, val})
    end if
  else
    printf(fn, "%s\"%s\", \"%s\",\n", {indent, name, val});
  end if
end procedure

function iLayoutExportElementAttribs(integer fn, Ihandle ih, string indent, integer typ)
integer i, wcount = 0, has_attrib_id = ih_iclass_has_attrib_id(ih), start_id = 0
string class_name = IupGetClassName(ih)
sequence attr_names = IupGetClassAttributes(class_name)

  if IupClassMatch(ih, "tree")       /* tree can only set id attributes after map, so they can not be saved */
  or IupClassMatch(ih, "cells") then /* cells do not have any savable id attributes */
    has_attrib_id = 0;  
  end if
  if IupClassMatch(ih, "list") then
    start_id = 1
  end if
  for i=1 to length(attr_names) do
    string name = attr_names[i];
if not find(name,{"HFONT","WID","HWND"}) then   -- PL (18/5/16) skip native handles
if name="FONT" then trace(1) end if
    string val = iupAttribGetLocal(ih, name)
--  string val = IupGetAttribute(ih, name)
    nullable_string def_value;
    integer flags;

    {def_value,flags} = iupClassGetAttribNameInfo(ih_iclass(ih), name)

    if iLayoutAttributeChanged(ih, name, val, def_value, flags) then

      string str = iupStrConvertToC(val);

      iLayoutExportWriteAttrib(fn, name, str, indent, typ)

      wcount +=1
    end if

    if has_attrib_id and and_bits(flags,IUPAF_HAS_ID) then
      flags -= IUPAF_HAS_ID; /* clear flag so the next function call can work */
--    if iLayoutAttributeChanged(ih, name, "X", NULL, flags) then
      if iLayoutAttributeChanged(ih, name, "X", "", flags) then
        if name="IDVALUE" then
          name = ""
        end if
        if and_bits(flags,IUPAF_HAS_ID2) then
          integer lin, col, 
              numcol = IupGetInt(ih, "NUMCOL"),
              numlin = IupGetInt(ih, "NUMLIN")
          for lin=0 to numlin do
            for col=0 to numcol do
              val = IupGetAttributeId2(ih, name, lin, col)
              if val!="" and not iupATTRIB_ISINTERNAL(val) then
                string str = sprintf("%s%d:%d", {name, lin, col})
                iLayoutExportWriteAttrib(fn, str, val, indent, typ)
                wcount += 1
              end if
            end for
          end for
        else
          integer id, count = IupGetInt(ih, "COUNT");
          for id=start_id to count+start_id-1 do
            val = IupGetAttributeId(ih, name, id);
            if val!="" and not iupATTRIB_ISINTERNAL(val) then
              string str = sprintf("%s%d", {name, id})
              iLayoutExportWriteAttrib(fn, str, val, indent, typ);
              wcount += 1
            end if
          end for
        end if
      end if
    end if
end if
  end for

  if typ!=0 then    /* LED or C */
    attr_names = IupGetClassCallbacks(IupGetClassName(ih))
    for i=1 to length(attr_names) do
--DEV
      string cb_name = ""--iupGetCallbackName(ih, attr_names[i]);
      if length(cb_name) and not iupATTRIB_ISINTERNAL(cb_name) then
        iLayoutExportWriteAttrib(fn, attr_names[i], cb_name, indent, typ)
        wcount += 1
      end if
    end for
  end if

  if typ==-1 then /* LED */
    /* remove last comma ',' and new line */
    /* if wcount==0, it will remove '[' and new line */
--DEV[?]
--  fseek(fn, -2, SEEK_CUR);
  end if

  return wcount
end function

procedure iLayoutExportElementC(integer fn, Ihandle ih)
string name = IupGetName(ih);
string classname = IupGetClassName(ih)
string indent = "    ";
integer idx
  if ih_iclass_childtype(ih)==IUP_CHILDNONE then
    indent = "        ";
  end if
  if name!="" and iupATTRIB_ISINTERNAL(name) then
    name = ""
  end if
  name = iff(name=""?"NULL":`"`&name&`"`)
  if ih_iclass_childtype(ih)==IUP_CHILDNONE then
    printf(fn, "      IupSetAtt(%s, IupCreate(\"%s\"), \n", {name,classname})
  else
--  if iup_container_idx!=iupAttribGet(ih, "_IUP_CONTAINER_INDEX") then ?9/0 end if
--  printf(fn, "  containers[%s] = IupSetAtt(%s, IupCreatep(\"%s\", \n", {iupAttribGet(ih, "_IUP_CONTAINER_INDEX"), name, classname});
--  printf(fn, "  containers[%s] = IupSetAtt(%s, IupCreatep(\"%s\", \n", {iup_container_idx, name, classname});
    idx = IupGetInt(ih, "_IUP_CONTAINER_INDEX")
    printf(fn, "  containers[%d] = IupSetAtt(%s, IupCreatep(\"%s\", \n", {idx, name, classname});
    Ihandln child = ih_firstchild(ih)
    while child!=NULL do
      if not and_bits(ih_flags(child),IUP_INTERNAL) then
        if ih_iclass_childtype(child)==IUP_CHILDNONE then
          iLayoutExportElementC(fn, child);  /* only one level of recursion */
        else
--        if iup_container_idx!=iupAttribGet(ih, "_IUP_CONTAINER_INDEX") then ?9/0 end if
--        printf(fn, "      containers[%s],\n", {iupAttribGet(child, "_IUP_CONTAINER_INDEX")});
--        printf(fn, "      containers[%s],\n", {iup_container_idx});
          idx = IupGetInt(child, "_IUP_CONTAINER_INDEX")
          printf(fn, "      containers[%d],\n", {idx});
          IupSetAttribute(child, "_IUP_CONTAINER_INDEX", NULL);  /* clear when last used */
--        IupSetAttributePtr(child, "_IUP_CONTAINER_INDEX", NULL);  /* clear when last used */
        end if
      end if
      child = ih_brother(child)
    end while

    printf(fn, "      NULL),\n"); /* end of IupCreatep */
  end if

  {} = iLayoutExportElementAttribs(fn, ih, indent, 0);  /* C */

  /* end of IupSetAtt */
  if ih_iclass_childtype(ih)!=IUP_CHILDNONE then
    printf(fn, "    NULL);\n\n");
  else
    printf(fn, "        NULL),\n");
  end if
end procedure

procedure iLayoutExportContainerC(integer fn, Ihandle ih)
Ihandln child = ih_firstchild(ih)
  /* export children first */
  while child!=NULL do
    if not and_bits(ih_flags(child),IUP_INTERNAL) 
    and ih_iclass_childtype(child)!=IUP_CHILDNONE then
      iLayoutExportContainerC(fn, child);
    end if
    child = ih_brother(child)
  end while

  iLayoutExportElementC(fn, ih);
end procedure

procedure iLayoutExportDialogC(integer fn, Ihandle dialog, string filename)
integer count = iLayoutExportCountContainers(dialog);
string title = iupStrFileGetTitle(filename);
  title = iLayoutRemoveExt(title, "c");

  printf(fn, "/*   Generated by IupLayoutDialog export to C.   */\n\n");
  printf(fn, "#include <stdlib.h>\n");
  printf(fn, "#include <iup.h>\n\n");
  printf(fn, "Ihandle* create_dialog_%s(void)\n", {title});
  printf(fn, "{\n");
  printf(fn, "  Ihandle* containers[%d];\n\n", {count});

  iLayoutExportContainerC(fn, dialog);
  IupSetAttribute(dialog, "_IUP_CONTAINER_INDEX", NULL);  /* clear when last used */
--  IupSetAttributePtr(dialog, "_IUP_CONTAINER_INDEX", NULL);  /* clear when last used */

  printf(fn, "  return containers[0];\n");
  printf(fn, "}\n");
end procedure

procedure iLayoutExportElementLua(integer fn, Ihandle ih)
string indent = "    "
string classname = IupGetClassName(ih)
integer idx

  if ih_iclass_childtype(ih)==IUP_CHILDNONE then
    indent = "      ";
    printf(fn, "    iup.%s{\n", {classname})
  else
--  printf(fn, "  containers[%d] = iup.%s{\n", {iupAttribGetInt(ih, "_IUP_CONTAINER_INDEX")+1, classname});
    idx = IupGetInt(ih, "_IUP_CONTAINER_INDEX")+1
    printf(fn, "  containers[%d] = iup.%s{\n", {idx, classname});
    Ihandln child = ih_firstchild(ih)
    while child!=NULL do
      if not and_bits(ih_flags(child),IUP_INTERNAL) then
        if ih_iclass_childtype(child)==IUP_CHILDNONE then
          iLayoutExportElementLua(fn, child);  /* only one level of recursion */
        else
--        printf(fn, "    containers[%d],\n", {iupAttribGetInt(child, "_IUP_CONTAINER_INDEX")+1});
          idx = IupGetInt(child, "_IUP_CONTAINER_INDEX")+1
          printf(fn, "    containers[%d],\n", {idx});
          IupSetAttribute(child, "_IUP_CONTAINER_INDEX", NULL);  /* clear when last used */
--        IupSetAttributePtr(child, "_IUP_CONTAINER_INDEX", NULL);  /* clear when last used */
        end if
      end if
      child = ih_brother(child)
    end while
  end if

  {} = iLayoutExportElementAttribs(fn, ih, indent, 1);  /* Lua */

  if ih_iclass_childtype(ih)!=IUP_CHILDNONE then
    printf(fn, "  }\n\n");
  else
    printf(fn, "    },\n");
  end if
end procedure

procedure iLayoutExportContainerLua(integer fn, Ihandle ih)
  /* export children first */
  Ihandln child = ih_firstchild(ih)
  while child!=NULL do
    if not and_bits(ih_flags(child),IUP_INTERNAL) 
    and ih_iclass_childtype(child)!=IUP_CHILDNONE then
      iLayoutExportContainerLua(fn, child)
    end if
    child = ih_brother(child)
  end while

  iLayoutExportElementLua(fn, ih)
end procedure

procedure iLayoutExportDialogLua(integer fn, Ihandle dialog, string filename)
string title = iupStrFileGetTitle(filename);
  title = iLayoutRemoveExt(title, "lua");
  {} = iLayoutExportCountContainers(dialog);

  printf(fn, "--   Generated by IupLayoutDialog export to Lua.\n\n");
  printf(fn, "function create_dialog_%s()\n", {title});
  printf(fn, "  local containers = {}\n\n");

  iLayoutExportContainerLua(fn, dialog);
  IupSetAttribute(dialog, "_IUP_CONTAINER_INDEX", NULL);  /* clear when last used */
--  IupSetAttributePtr(dialog, "_IUP_CONTAINER_INDEX", NULL);  /* clear when last used */

  printf(fn, "  return containers[1]\n");
  printf(fn, "end\n");
end procedure

procedure iLayoutExportElementLED(integer fn, Ihandle ih, nullable_string name, integer indent_level)
integer i, indent_count = 0;
string fmt = ih_iclass_format(ih)
string classname = upper(IupGetClassName(ih))

  /* constructor identation */
  if indent_level then
    indent_count = indent_level*4;
  end if

  if name!=NULL then
    printf(fn, "%s = %s[\n", {name, classname})  /* start of attributes */
  else
    printf(fn, "%s%s[\n", {repeat(' ',indent_count), classname})
  end if

  /* attributes identation */
  indent_count += 2;

  if iLayoutExportElementAttribs(fn, ih, repeat(' ',indent_count), -1)!=0 then  /* LED */
    printf(fn, "]"); /* end of attributes (no new line) */
  end if
  if fmt="" then
    printf(fn, "()");
  else
    if find(fmt[1],"hg") then

      printf(fn, "(\n")

      /* children identation */
      indent_count += 2;

      Ihandln child = ih_firstchild(ih)
      while child!=NULL do
        if not and_bits(ih_flags(child),IUP_INTERNAL) then
          string childname = iLayoutGetName(child);
          if childname="" then
            iLayoutExportElementLED(fn, child, NULL, indent_level+1);   /* here process the ones that does NOT have names */
          else
            printf(fn, "%s%s", {repeat(' ',indent_count), childname})
          end if

          if ih_brother(child)!=NULL then
            printf(fn, ",\n")
          end if
        end if
        child = ih_brother(child)
      end while

      printf(fn, ")")
    else
      printf(fn, "(");

      for i=1 to length(fmt) do
        if fmt[i]=='s' then
          printf(fn, `""`);  /* empty string, let the job to the attributes */
        elsif fmt[i]=='a' then
--        string cb_name = ""--iupGetCallbackName(ih, "ACTION");
--        if (!cb_name) then
--          cb_name = ""--iupGetCallbackName(ih, "ACTION_CB");
--        end if
--        if (cb_name && !iupATTRIB_ISINTERNAL(cb_name)) then
--          printf(fn, "%s", {cb_name})
--        else
            printf(fn, "do_nothing");  /* dummy name */
--        end if
        end if
        if i<length(fmt) then
          printf(fn, ", ")
        end if
      end for

      printf(fn, ")");
    end if
  end if

  if name!=NULL then
    printf(fn, "\n\n")
  end if
end procedure

procedure iLayoutExportChildrenLED(integer fn, Ihandle ih)
string name;

  /* export children first */
  Ihandln child = ih_firstchild(ih)
  while child!=NULL do
    if not and_bits(ih_flags(child),IUP_INTERNAL) then
      iLayoutExportChildrenLED(fn, child)
    end if
    child = ih_brother(child)
  end while

  name = iLayoutGetName(ih);
  if name!="" then /* here process only the ones that have names */
    iLayoutExportElementLED(fn, ih, name, 0)
  end if
end procedure

procedure iLayoutExportDialogLED(integer fn, Ihandle dialog, string filename)
string title
string name = IupGetName(dialog)
  if name="" then
    title = iupStrFileGetTitle(filename);
    title = iLayoutRemoveExt(title, "led");
    IupSetAttribute(dialog, "_IUP_DIALOG_NAME", title);
  end if

  printf(fn, "#   Generated by IupLayoutDialog export to LED.\n\n");

  iLayoutExportChildrenLED(fn, dialog);

end procedure

procedure iLayoutExportDialog(Ihandle dialog, string filename, string fmt)
integer fn = open(filename, "wb");
  if fn!=-1 then
      if fmt="LED" then
        iLayoutExportDialogLED(fn, dialog, filename);
      elsif fmt="Lua" then
        iLayoutExportDialogLua(fn, dialog, filename);
      elsif fmt="C" then
        iLayoutExportDialogC(fn, dialog, filename);
      end if
      close(fn)
    end if
end procedure

string fdir = "" -- persist from one call to another if not defined
function iLayoutGetExportFile(Ihandle parent, string filename)
    Ihandln file_dlg = IupFileDlg()
    string filt = ""
    {fdir, filt} = iupStrFileNameSplit(filename, fdir, filt)

    IupSetAttribute(file_dlg, "FILTER", filt);
    IupSetAttribute(file_dlg, "DIRECTORY", fdir);
    IupSetAttribute(file_dlg, "DIALOGTYPE", "SAVE");
    IupSetAttribute(file_dlg, "ALLOWNEW", "YES");
    IupSetAttribute(file_dlg, "NOCHANGEDIR", "YES");
    IupSetAttributeHandle(file_dlg, "PARENTDIALOG", parent);
    nullable_string icon = IupGetGlobal("ICON")
    if string(icon) then
        IupSetAttribute(file_dlg, "ICON", icon);
--  else
--    IupSetAttributePtr(file_dlg, "ICON", NULL);
    end if

    IupPopup(file_dlg, IUP_CENTERPARENT, IUP_CENTERPARENT);

    integer ret = IupGetInt(file_dlg, "STATUS");
    if ret!=-1 then
        string val = IupGetAttribute(file_dlg, "VALUE");
        if val!="" then
            filename = val
            {fdir} = iupStrFileNameSplit(val, fdir, "");
        end if
    end if

    file_dlg = IupDestroy(file_dlg);

    return {ret,filename}
end function


/***************************************************************************
                             Layout Dialog Menus
 ***************************************************************************/


function iLayoutMenuNew_CB(Ihandle /*ih*/)
  if layoutdlg_destroy then
    layoutdlg_dialog = IupDestroy(layoutdlg_dialog)
  end if
  layoutdlg_dialog = IupDialog(NULL)
  layoutdlg_destroy = 1
  iLayoutTreeRebuild()
  return IUP_DEFAULT
end function

function iLayoutMenuReload_CB(Ihandle /*ih*/)
  iLayoutTreeRebuild()
  return IUP_DEFAULT
end function

function iLayoutMenuExportLED_CB(Ihandle ih)
Ihandle dlg = IupGetDialog(ih);
integer res
string filename
  {res,filename} = iLayoutGetExportFile(dlg, "*.led")
  if res!=-1 then
    iLayoutExportDialog(layoutdlg_dialog, filename, "LED")
  end if
  return IUP_DEFAULT
end function

function iLayoutMenuExportLua_CB(Ihandle ih)
Ihandle dlg = IupGetDialog(ih);
integer res
string filename
  {res,filename} = iLayoutGetExportFile(dlg, "*.lua")
  if res!=-1 then
    iLayoutExportDialog(layoutdlg_dialog, filename, "Lua")
  end if
  return IUP_DEFAULT
end function

function iLayoutMenuExportC_CB(Ihandle ih)
Ihandle dlg = IupGetDialog(ih);
integer res
string filename
  {res,filename} = iLayoutGetExportFile(dlg, "*.c")
  if res!=-1 then
    iLayoutExportDialog(layoutdlg_dialog, filename, "C")
  end if
  return IUP_DEFAULT
end function

function iLayoutMenuClose_CB(Ihandle ih)
  Ihandln dlg = IupGetDialog(ih)
  if IupGetInt(dlg, "DESTROYWHENCLOSED") then
    dlg = IupDestroy(dlg)
    return IUP_IGNORE
  end if
  IupHide(dlg)
  return IUP_DEFAULT
end function

function iLayoutMenuTree_CB(Ihandle ih)
Ihandle dlg = IupGetDialog(ih);
Ihandle splitter = IupGetChild(IupGetChild(dlg, 0), 0);
  if IupGetInt(splitter, "VALUE") then
    IupSetAttribute(splitter, "VALUE", "0");
  else
    IupSetAttribute(splitter, "VALUE", "300");
  end if
  return IUP_DEFAULT;
end function

function iLayoutMenuRefresh_CB(Ihandle /*ih*/)
  IupRefresh(layoutdlg_dialog)
  IupUpdate(layoutdlg_canvas)
  return IUP_DEFAULT
end function

function iLayoutTimerAutoUpdate_CB(Ihandle /*ih*/)
  IupUpdate(layoutdlg_canvas)
  return IUP_DEFAULT
end function

function iLayoutMenuShowHidden_CB(Ihandle ih)
Ihandle dlg = IupGetDialog(ih)
--if dlg=layoutdlg_dlg then ?9/0 end if --DEV...
--if dlg!=layoutdlg_dialog then ?9/0 end if -- first make sure this never triggers...
if dlg!=layoutdlg_dlg then ?9/0 end if  -- first make sure this never triggers...
  if IupGetInt(dlg, "SHOWHIDDEN") then
    IupSetAttribute(dlg, "SHOWHIDDEN", "No")
  else
    IupSetAttribute(dlg, "SHOWHIDDEN", "Yes")
  end if
  IupUpdate(layoutdlg_canvas)
  return IUP_DEFAULT
end function

function iLayoutMenuAutoUpdate_CB(Ihandle ih)
Ihandle dlg = IupGetDialog(ih);
  if IupGetInt(layoutdlg_timer, "RUN") then
    IupSetAttribute(layoutdlg_timer, "RUN", "No");
  else
    IupSetAttribute(layoutdlg_timer, "RUN", "Yes");
  end if
  return IUP_DEFAULT;
end function

function iLayoutMenuUpdate_CB(Ihandle /*ih*/)
  IupUpdate(layoutdlg_canvas)
  return IUP_DEFAULT
end function

function iLayoutMenuRedraw_CB(Ihandle /*ih*/)
  IupRedraw(layoutdlg_dialog, 1);
  return IUP_DEFAULT
end function

function iLayoutGetParamOpacity_CB(Ihandle dialog, integer param_index, atom user_data)
  if param_index==0 then
    Ihandle dlg = user_data;
--DEV (doc, possibly add IupSetAttributePtr, make IupSetAttribute only accept string [NAH?])
    Ihandle param = IupGetAttributePtr(dialog, "PARAM0");
--  Ihandle param = IupGetInt(dialog, "PARAM0");
--  Ihandln param = IupGetInt(dialog, "PARAM0");
--  Ihandle param = IupGetAttributeHandle(dialog, "PARAM0");
    integer opacity = IupGetInt(param, "VALUE");
    IupSetInt(dlg,"OPACITY", opacity);
  end if
  return 1;
end function

function iLayoutMenuOpacity_CB(Ihandle ih)
  Ihandle dlg = IupGetDialog(ih);
  integer status
  integer opacity = IupGetInt(dlg, "OPACITY")
  if opacity==0 then
    opacity = 255
  end if
  {opacity,status} = IupGetParam("Dialog Layout", Icallback("iLayoutGetParamOpacity_CB"), dlg,
                                 "Opacity: %i[0,255]\n", {opacity})

  if opacity == 0 or opacity == 255 then
    IupSetAttribute(dlg, "OPACITY", NULL)
--  IupSetAttributePtr(dlg, "OPACITY", NULL)
  else
    IupSetInt(dlg,"OPACITY", opacity)
  end if
  return IUP_DEFAULT;
end function

function iLayoutMenuShow_CB(Ihandle /*ih*/)
  IupShow(layoutdlg_dialog)
  return IUP_DEFAULT
end function

function iLayoutMenuHide_CB(Ihandle /*ih*/)
  IupHide(layoutdlg_dialog)
  return IUP_DEFAULT
end function

procedure iLayoutDialogLoad(Ihandle parent_dlg, integer only_visible)
integer ret, count, i;  
sequence dlg_list, dlg_list_str;
Ihandln dlg;
Ihandln old_parent_dlg

  if only_visible then
    count = iupDlgListVisibleCount();
  else
    count = iupDlgListCount();
  end if
  dlg_list = repeat(0,count)
  dlg_list_str = repeat("",count)

  dlg = iupDlgListFirst()
  for i=1 to count do
    if dlg=NULL then 
        count := i-1
        dlg_list = dlg_list[1..count]
        dlg_list_str = dlg_list_str[1..count]
        exit 
    end if
    if not only_visible 
    or (ih_handle(dlg)!=NULL and IupGetInt(dlg, "VISIBLE")) then
      dlg_list[i] = dlg;
      dlg_list_str[i] = iLayoutGetTitle(dlg)
    end if
    dlg = iupDlgListNext()
  end for

--  iupASSERT(i == count);
--  if (i != count)
--  count = i;

--  old_parent_dlg = IupGetGlobal("PARENTDIALOG")
  old_parent_dlg = IupGetInt(NULL,"PARENTDIALOG")
  IupSetAttributeHandle(NULL, "PARENTDIALOG", parent_dlg);

  ret = IupListDialog(1,"Dialogs",dlg_list_str,1,15,iff(count<15?count+1:15))

  IupStoreGlobal("PARENTDIALOG", old_parent_dlg);

--  if ret!=-1 then
  if ret>0 then
    if layoutdlg_destroy then
      layoutdlg_dialog = IupDestroy(layoutdlg_dialog);
    end if
    layoutdlg_dialog = dlg_list[ret];
    layoutdlg_destroy = 0;

    integer {w,h} = IupGetIntInt(layoutdlg_dialog, "CLIENTSIZE")
    if w!=0 and h!=0 then
--DEV? layoutdlg_dialog? or layoutdlg_dlg?
--    int prop = IupGetInt(IupGetParent(layoutdlg_tree), "VALUE");
--    integer prop = IupGetInt(layoutdlg_dialog, "VALUE");
      integer prop = IupGetInt(layoutdlg_dlg, "VALUE");
--DEV..
--    int status = IupGetInt2(IupGetBrother(IupGetParent(layoutdlg_tree)), "RASTERSIZE");
      integer status = IupGetInt2(layoutdlg_canvas, "RASTERSIZE");
      w = floor((w*1000)/(1000-prop))
      integer {pw,ph} = IupGetIntInt(parent_dlg, "CLIENTSIZE")
      ph -= status;
      if w>pw or h>ph then
        IupSetStrAttribute(parent_dlg, "CLIENTSIZE", "%dx%d", {w+10, h+status+10});
        IupShow(parent_dlg);
      end if
    end if
  end if
end procedure

function iLayoutMenuLoad_CB(Ihandle ih)
Ihandle dlg = IupGetDialog(ih);
  iLayoutDialogLoad(dlg, 0)
  iLayoutTreeRebuild()
  return IUP_DEFAULT
end function

function iLayoutMenuLoadVisible_CB(Ihandle ih)
Ihandle dlg = IupGetDialog(ih);
  iLayoutDialogLoad(dlg, 1);
  iLayoutTreeRebuild()
  return IUP_DEFAULT
end function


/***************************************************************************
                               Canvas Drawing
 ***************************************************************************/


type IdrawCanvas(object dc)
    return atom(dc) and dc>NULL and dc=floor(dc)
end type

--DEV I could redo this using the Phix scanf(), 
--  (I was thinking of getting #RRGGBB as one big atom then and_bits/floor to r/g/b)
--  but for now I have just wrapped the existing C function.
--  -- nah, it just as messy, left in ?9/0 for now.
--/*
int iupStrToRGB(const char *str, unsigned char *r, unsigned char *g, unsigned char *b)
{
  unsigned int ri = 0, gi = 0, bi = 0;
  if (!str) return 0;
  if (str[0]=='#')
  {
    str++;
    if (sscanf(str, "%2X%2X%2X", &ri, &gi, &bi) != 3) return 0;
  }
  else
  {
    if (sscanf(str, "%u %u %u", &ri, &gi, &bi) != 3) return 0;
  }
  if (ri > 255 || gi > 255 || bi > 255) return 0;
  *r = (unsigned char)ri;
  *g = (unsigned char)gi;
  *b = (unsigned char)bi;
  return 1;
}
--*/
function iupStrToRGB(string s, sequence defval)
integer {r,g,b} = defval
    if length(s) then
        if s[1]='#' then
            ?s
            ?9/0
            ?defval
        else
            sequence paramsets = scanf(s,"%d %d %d")
            if length(paramsets)>0 then
                {r,g,b} = paramsets[1]
            end if
        end if
    end if
    return {r,g,b}
end function

--/*
function iupdrvGetScreenDpi()
    atom res = c_func(xiupdrvGetScreenDpi,{})
    return res
end function

--static Ihandle* iImageGetImageFromName(const char* name)
function iImageGetImageFromName(string name)
  Ihandln ih = IupGetHandle(name)
  if ih!=NULL then
    integer bpp = IupGetInt(ih, "BPP")
    string autoscale = iupAttribGet(ih, "AUTOSCALE")
    if autoscale="" then autoscale = IupGetGlobal("IMAGEAUTOSCALE") end if
    if autoscale!="" and bpp > 8 and iupAttribGet(ih, "SCALED")="" then
      atom scale = 0
      if upper(autoscale)="DPI" then
        integer dpi = floor(iupdrvGetScreenDpi() + 0.6)
        integer images_dpi = IupGetInt(NULL, "IMAGESDPI")
        if images_dpi == 0 then images_dpi = 96 end if
        if dpi <= 96 then
          dpi = 96
        elsif dpi <= 144 then
          dpi = 144
        elsif dpi <= 192 then
          dpi = 192
        else
          dpi = 288
        end if
        scale = dpi / images_dpi
      else
--      iupStrToFloat(autoscale, &scale);
        scale = to_number(autoscale)
      end if
      if scale > 0 and (scale < 0.99 or scale > 1.01) then
        string hotspot = iupAttribGet(ih, "HOTSPOT");

--      integer new_width = iupRound(scale*ih_currentwidth(ih));
--      integer new_height = iupRound(scale*ih_currentwidth(ih));
        integer new_width = round(scale*ih_currentwidth(ih))
        integer new_height = round(scale*ih_currentwidth(ih))

--      iupImageResize(ih, new_width, new_height);
        ?9/0
--      iupAttribSet(ih, "SCALED", "1");
        IupSetAttribute(ih, "SCALED", "1")

        if hotspot!="" then
--        integer x = 0, y = 0;
--        iupStrToIntInt(hotspot, &x, &y, ':');
          integer {x,y} = IupGetIniInt(id,"HOTSPOT")

--        x = iupRound(scale*x);
--        y = iupRound(scale*y);
          x = round(scale*x);
          y = round(scale*y);

--        iupAttribSetStrf(ih, "HOTSPOT", "%d:%d", x, y);
          IupSetStrAttribute(ih, "HOTSPOT", "%d:%d", {x, y})
        end if
      end if
    end if
  end if

  return ih;
end function

--void iupImageGetInfo(const char* name, int *w, int *h, int *bpp)
function iupImageGetInfo(string name)--, int *w, int *h, int *bpp)
  void* handle;
  Ihandln ih

--  if (!name)
--  return;
  if name="" then return {0,0,0} end if

  ih = iImageGetImageFromName(name)
  if ih=NULL then
    const char* native_name = NULL;

    /* Check in the system resources. */
>   handle = iupdrvImageLoad(name, IUPIMAGE_IMAGE);
    if (handle)
    {
      iupdrvImageGetInfo(handle, w, h, bpp);
      return;
    }

    /* Check in the stock images. */
    iImageStockGet(name, &ih, &native_name);
    if (native_name) 
    {
>     handle = iupdrvImageLoad(native_name, IUPIMAGE_IMAGE);
      if (handle) 
      {
--      iupdrvImageGetInfo(handle, w, h, bpp);
--      return;
        return iupdrvImageGetInfo(handle)
      }
    }

    if ih=NULL then return {0,0,0} end if
  end if

--  if (w) *w = ih->currentwidth;
--  if (h) *h = ih->currentheight;
--  if (bpp) *bpp = IupGetInt(ih, "BPP");
    return {ih->currentwidth, ih->currentheight, IupGetInt(ih, "BPP")}
end function
--*/

procedure iLayoutDrawElement(IdrawCanvas dc, Ihandle ih, integer marked, integer native_parent_x, integer native_parent_y)
integer x, y, w, h
string bgcolor
integer r, g, b;
integer 
--br, bg, bb, 
--      fr, fg, fb,
--      fmr, fmg, fmb,
--      fvr, fvg, fvb;

  br = 255, bg = 255, bb = 255,  /* background color */
  fr = 0, fg = 0, fb = 0,        /* foreground color */
  fvr = 164, fvg = 164, fvb = 164,  /* foreground color for void elements */
  fmr = 255, fmg = 0, fmb = 0;      /* foreground color for elements that are maximizing parent size */

  x = ih_x(ih)+native_parent_x
  y = ih_y(ih)+native_parent_y
  w = ih_currentwidth(ih)
  h = ih_currentheight(ih)
  if w<=0 then w=1 end if
  if h<=0 then h=1 end if

  bgcolor = IupGetAttribute(ih, "BGCOLOR");
  if bgcolor!="" and ih_iclass_nativetype(ih)!=IUP_TYPEVOID then
--  r = br, g = bg, b = bb;
    {r,g,b} = iupStrToRGB(bgcolor,{br,bg,bb})
    iupdrvDrawRectangle(dc, x, y, x+w-1, y+h-1, r, g, b, IUP_DRAW_FILL);
  end if

  if ih_iclass_nativetype(ih)==IUP_TYPEVOID then
    iupdrvDrawRectangle(dc, x, y, x+w-1, y+h-1, fvr, fvg, fvb, IUP_DRAW_STROKE_DASH);
  else
    iupdrvDrawRectangle(dc, x, y, x+w-1, y+h-1, fr, fg, fb, IUP_DRAW_STROKE);
  end if

  iupdrvDrawSetClipRect(dc, x, y, x+w-1, y+h-1);

  if ih_iclass_childtype(ih)==IUP_CHILDNONE then
    integer {pw, ph} = IupGetIntInt(ih_parent(ih), "CLIENTSIZE")

    if ih_currentwidth(ih) == pw and ih_currentwidth(ih) == ih_naturalwidth(ih) then
      iupdrvDrawLine(dc, x+1, y+1, x+w-2, y+1, fmr, fmg, fmb, IUP_DRAW_STROKE);
      iupdrvDrawLine(dc, x+1, y+h-2, x+w-2, y+h-2, fmr, fmg, fmb, IUP_DRAW_STROKE);
    end if

    if ih_currentheight(ih) == ph and ih_currentheight(ih) == ih_naturalheight(ih) then
      iupdrvDrawLine(dc, x+1, y+1, x+1, y+h-2, fmr, fmg, fmb, IUP_DRAW_STROKE);
      iupdrvDrawLine(dc, x+w-2, y+1, x+w-2, y+h-2, fmr, fmg, fmb, IUP_DRAW_STROKE);
    end if
  elsif ih_iclass_nativetype(ih)!=IUP_TYPEVOID then
    /* if ih is a Tabs, position the title accordingly */
    if IupClassMatch(ih, "tabs") then
      /* TABORIENTATION is ignored */
      string tabtype = IupGetAttribute(ih, "TABTYPE");
      if upper(tabtype)="BOTTOM" then
        integer {cw,ch} = IupGetIntInt(ih, "CLIENTSIZE")
        y += ch;  /* position after the client area */
      elsif upper(tabtype)="RIGHT" then
        integer {cw, ch} = IupGetIntInt(ih, "CLIENTSIZE")
        x += cw;  /* position after the client area */
      end if
    end if
  end if

  /* always draw the image first */
  if ih_iclass_nativetype(ih)!=IUP_TYPEVOID then
    string title, image

    image = IupGetAttribute(ih, "IMAGE0");  /* Tree root node title */
    if image="" then
      image = IupGetAttribute(ih, "TABIMAGE0");  /* Tabs first tab image */
    end if
    if image!="" then
      /* returns the image of the active tab */
      integer pos = IupGetInt(ih, "VALUEPOS");
      image = IupGetAttributeId(ih, "TABIMAGE", pos);
    end if
    if image="" then
      image = IupGetAttribute(ih, "IMAGE");
    end if
    if image!="" then
--?9/0
--<   iupImageGetInfo(image, &img_w, &img_h, NULL);
      integer {img_w, img_h} = IupDrawGetImageInfo(image)
-->!      integer {img_w, img_h} = iupdrvDrawImage(dc, image, 0, x+1, y+1)
      iupdrvDrawImage(dc, image, 0, x+1, y+1)

      string pos = IupGetAttribute(ih, "IMAGEPOSITION");    /* used only for buttons */
      if pos!=""
      and find(upper(pos),{"BOTTOM","TOP"}) then
        y += img_h;
      else
        x += img_w;  /* position text usually at right */
      end if
    end if

    title = IupGetAttribute(ih, "0:0");  /* Matrix title cell */
    if title="" then 
      title = IupGetAttribute(ih, "1");  /* List first item */
      if title="" then
        title = IupGetAttribute(ih, "TITLE0");  /* Tree root node title */
        if title="" then
          title = IupGetAttribute(ih, "TABTITLE0");  /* Tabs first tab title */
          if title!="" then
            /* returns the title of the active tab */
            integer pos = IupGetInt(ih, "VALUEPOS");
            title = IupGetAttributeId(ih, "TABTITLE", pos);
          end if
          if title="" then
            title = iupAttribGetLocal(ih, "TITLE");
          end if
        end if
      end if
    end if
    if title!="" then
      /* get the size of the first line */
      integer len = 0
      for i=1 to length(title) do
        if find(title[i],"\r\n") then exit end if
        len += 1
      end for
--    r = fr, g = fg, b = fb;
      {r,g,b} = iupStrToRGB(IupGetAttribute(ih, "FGCOLOR"), {fr,fg,fb})
      iupdrvDrawText(dc, title, len, x+1, y+1, r, g, b, IupGetAttribute(ih, "FONT"));
    end if

    if ih_iclass_childtype(ih)==IUP_CHILDNONE 
    and title="" and image="" then
      if IupClassMatch(ih, "progressbar") then
        atom vmin = IupGetDouble(ih, "MIN");
        atom vmax = IupGetDouble(ih, "MAX");
        atom val = IupGetDouble(ih, "VALUE");
--      r = fr, g = fg, b = fb;
        {r,g,b} = iupStrToRGB(IupGetAttribute(ih, "FGCOLOR"), {fr,fg,fb})
        if upper(IupGetAttribute(ih, "ORIENTATION"))="VERTICAL" then
          integer ph = floor(((vmax-val)*(h-5))/(vmax-vmin))
          iupdrvDrawRectangle(dc, x+2, y+2, x+w-3, y+ph, r, g, b, IUP_DRAW_FILL);
        else
          integer pw = floor(((val-vmin)*(w-5))/(vmax-vmin))
          iupdrvDrawRectangle(dc, x+2, y+2, x+pw, y+h-3, r, g, b, IUP_DRAW_FILL);
        end if
      elsif IupClassMatch(ih, "val") then
        atom vmin = IupGetDouble(ih, "MIN");
        atom vmax = IupGetDouble(ih, "MAX");
        atom val = IupGetDouble(ih, "VALUE");
--      r = fr, g = fg, b = fb;
        {r,g,b} = iupStrToRGB(IupGetAttribute(ih, "FGCOLOR"), {fr,fg,fb})
        if upper(IupGetAttribute(ih, "ORIENTATION"))="VERTICAL" then
          integer ph = floor(((vmax-val)*(h-5))/(vmax-vmin))
          iupdrvDrawRectangle(dc, x+2, y+ph-1, x+w-3, y+ph+1, r, g, b, IUP_DRAW_FILL);
        else
          integer pw = floor(((val-vmin)*(w-5))/(vmax-vmin))
          iupdrvDrawRectangle(dc, x+pw-1, y+2, x+pw+1, y+h-3, r, g, b, IUP_DRAW_FILL);
        end if
      end if
    end if
  end if

  iupdrvDrawResetClip(dc);

  if marked then
    x = ih_x(ih)+native_parent_x
    y = ih_y(ih)+native_parent_y
    w = ih_currentwidth(ih)
    h = ih_currentheight(ih)
    if w<=0 then w=1 end if
    if h<=0 then h=1 end if

    iupdrvDrawSelectRect(dc, x, y, x+w, y+h);
  end if
end procedure



function iLayoutElementIsVisible(Ihandle ih, integer dlgvisible)
  if dlgvisible then
    return iupStrBoolean(iupAttribGetLocal(ih, "VISIBLE"));
  end if
  /* can not check at native implementation because it will be always not visible */
  string val = iupAttribGet(ih, "VISIBLE");
  if val="" then
    return 1; /* default is visible */
  end if
  return iupStrBoolean(val) 
end function

procedure iLayoutDrawElementTree(IdrawCanvas dc, integer showhidden, integer dlgvisible, integer shownotmapped, Ihandln mark, Ihandle ih, integer native_parent_x, integer native_parent_y)
Ihandln child
integer dx, dy

  if (showhidden or iLayoutElementIsVisible(ih, dlgvisible)) 
  and (shownotmapped or ih_handle(ih)!=NULL) then
    /* draw the element */
    iLayoutDrawElement(dc, ih, ih==mark, native_parent_x, native_parent_y);

    if ih_iclass_childtype(ih)!=IUP_CHILDNONE then
      -- if ih is a native parent, then update the offset
      if ih_iclass_nativetype(ih)!=IUP_TYPEVOID then
        {dx,dy} = IupGetIntInt(ih, "CLIENTOFFSET")
        native_parent_x += ih_x(ih)+dx;
        native_parent_y += ih_y(ih)+dy;

        -- if ih is a Tabs, then draw only the active child
        if IupClassMatch(ih, "tabs") then
          child = IupGetInt(ih, "VALUE_HANDLE");
          if child!=NULL then
            iLayoutDrawElementTree(dc, showhidden, dlgvisible, shownotmapped, mark, child, native_parent_x, native_parent_y)
          end if
          return
        end if
      end if
    end if

    /* draw its children */
    child = ih_firstchild(ih)
    while child!=NULL do
      iLayoutDrawElementTree(dc, showhidden, dlgvisible, shownotmapped, mark, child, native_parent_x, native_parent_y);
      child = ih_brother(child)
    end while
  end if
end procedure

procedure iLayoutDrawDialog(integer showhidden, IdrawCanvas dc, Ihandln mark)
integer {w, h} = iupdrvDrawGetSize(dc)
  iupdrvDrawRectangle(dc, 0, 0, w-1, h-1, 255, 255, 255, IUP_DRAW_FILL);

  /* draw the dialog */
  {w, h} = IupGetIntInt(layoutdlg_dialog, "CLIENTSIZE")
  iupdrvDrawRectangle(dc, 0, 0, w-1, h-1, 0, 0, 0, IUP_DRAW_STROKE);

  if ih_firstchild(layoutdlg_dialog)!=NULL then
    integer shownotmapped = (ih_handle(layoutdlg_dialog)==NULL) /* only show not mapped if dialog is also not mapped */
    integer dlgvisible = IupGetInt(layoutdlg_dialog, "VISIBLE");
    integer {native_parent_x, native_parent_y} = IupGetIntInt(layoutdlg_dialog, "CLIENTOFFSET")
    iLayoutDrawElementTree(dc, showhidden, dlgvisible, shownotmapped, mark, ih_firstchild(layoutdlg_dialog), native_parent_x, native_parent_y);
  end if
end procedure

function iLayoutCanvas_CB(Ihandle canvas)
  Ihandle dlg = IupGetDialog(canvas);
  IdrawCanvas dc = iupdrvDrawCreateCanvas(canvas);
  integer showhidden = IupGetInt(dlg, "SHOWHIDDEN");
  iLayoutDrawDialog(showhidden, dc, layoutdlg_mark);
  iupdrvDrawFlush(dc);
  iupdrvDrawKillCanvas(dc);
  return IUP_DEFAULT;
end function


/***************************************************************************
                        Element Properties Dialog
 ***************************************************************************/


procedure iLayoutPropertiesUpdate(Ihandle properties, Ihandle ih)
  integer i, j
  string class_name = IupGetClassName(ih)
  sequence attr_names = IupGetClassAttributes(class_name)
  sequence callback_names = IupGetClassCallbacks(class_name)
  sequence all_names = IupGetAllAttributes(ih)

  Ihandle list1 = layoutdlg_list1
  Ihandle list2 = layoutdlg_list2
  Ihandle list3 = layoutdlg_list3

  /* Clear everything */
  IupSetAttribute(list1, "REMOVEITEM", NULL);
  IupSetAttribute(list2, "REMOVEITEM", NULL);
  IupSetAttribute(list3, "REMOVEITEM", NULL);
  IupSetAttribute(IupGetDialogChild(properties, "VALUE1A"), "VALUE", "");
  IupSetAttribute(IupGetDialogChild(properties, "VALUE1B"), "TITLE", "");
  IupSetAttribute(IupGetDialogChild(properties, "VALUE1C"), "TITLE", "");
  IupSetAttribute(IupGetDialogChild(properties, "VALUE2"), "VALUE", "");
  IupSetAttribute(IupGetDialogChild(properties, "VALUE3"), "TITLE", "");
  IupSetAttribute(IupGetDialogChild(properties, "SETBUT"), "ACTIVE", "No");
  IupSetAttribute(IupGetDialogChild(properties, "SETCOLORBUT"), "VISIBLE", "No");
  IupSetAttribute(IupGetDialogChild(properties, "SETFONTBUT"), "VISIBLE", "No");
  IupSetAttribute(IupGetDialogChild(properties, "SHOWIDLIST"), "VISIBLE", "No");

  for i=1 to length(attr_names) do
    IupSetAttributeId(list1,"",i, attr_names[i])
  end for

  for i=1 to length(callback_names) do
    IupSetAttributeId(list3,"",i, callback_names[i])
  end for

  j = 1
  for i=1 to length(all_names) do
-- bit of a punt this one:
--  if (!iupClassAttribIsRegistered(ih->iclass, all_names[i])) then
    if not find(all_names[i],attr_names) then
      IupSetAttributeId(list2,"",j, all_names[i]);
      j += 1
    end if
  end for

  layoutdlg_propelem = ih

  IupStoreAttribute(IupGetDialogChild(properties, "ELEMTITLE"), "TITLE", iLayoutGetTitle(ih));

end procedure

function iLayoutPropertiesClose_CB(Ihandle ih)
  IupHide(IupGetDialog(ih));
  return IUP_DEFAULT;
end function

function iLayoutPropertiesShowId_CB(Ihandle showidlist, atom id, integer /*item*/, integer state)
--  (void)item;
  if state then
    string def_value;
    integer flags;
    Ihandle elem = layoutdlg_propelem
    Ihandle txt1 = IupGetDialogChild(showidlist, "VALUE1A");
    Ihandle list1 = layoutdlg_list1
    string itemlist1 = IupGetAttribute(list1, "VALUE");
    if itemlist1="" then
      return IUP_DEFAULT
    end if

    string name = IupGetAttribute(list1, itemlist1)

    {def_value, flags} = iupClassGetAttribNameInfo(ih_iclass(elem), name)

    name &= peek_string(id)

    string val = IupGetAttribute(elem, name);
    if val!="" then
      if and_bits(flags,IUPAF_NO_STRING) then
        IupSetStrAttribute(txt1, "VALUE", "%p", val);
      else
        IupStoreAttribute(txt1, "VALUE", val);
      end if
    else
      IupSetAttribute(txt1, "VALUE", "NULL");
    end if
    if match("COLOR",name)!=0 then
      Ihandle colorbut = IupGetDialogChild(showidlist, "SETCOLORBUT");
      IupStoreAttribute(colorbut, "BGCOLOR", val);
    end if
  end if
  return IUP_DEFAULT;
end function

function iLayoutPropertiesSet_CB(Ihandle button)
  Ihandle list1 = layoutdlg_list1
  string item = IupGetAttribute(list1, "VALUE");
  if item!="" then
    Ihandle elem = layoutdlg_propelem
    Ihandle txt1 = IupGetDialogChild(button, "VALUE1A");
    string val = IupGetAttribute(txt1, "VALUE");
    string name = IupGetAttribute(list1, item);
    Ihandle showidlist = IupGetDialogChild(button, "SHOWIDLIST");

    if IupGetInt(showidlist, "VISIBLE") then
      item = IupGetAttribute(showidlist, "VALUE")
      if length(item) then
        string id = IupGetAttribute(showidlist, item)
        string nameid = sprintf("%s%s", {name, id})

        if val="" or val="NULL" then
          IupSetAttribute(elem, nameid, NULL)
        else
          IupStoreAttribute(elem, nameid, val)
        end if
      end if
    else
      if val="" or val="NULL" then
        IupSetAttribute(elem, name, NULL)
      else
        IupStoreAttribute(elem, name, val)
      end if
    end if

    if match("COLOR",name)!=0 then
      Ihandle colorbut = IupGetDialogChild(button, "SETCOLORBUT");
      IupStoreAttribute(colorbut, "BGCOLOR", val);
    end if

--DEV PL...
--  if (layoutdlg) then
      layoutdlg_changed = 1

      IupUpdate(layoutdlg_canvas)
--  end if
  end if
  return IUP_DEFAULT
end function

function iLayoutPropertiesSetColor_CB(Ihandle colorbut)
  Ihandln color_dlg = IupColorDlg();
  IupSetAttributeHandle(color_dlg, "PARENTDIALOG", IupGetDialog(colorbut));
  IupSetAttribute(color_dlg, "TITLE", "Choose Color");
  IupStoreAttribute(color_dlg, "VALUE", IupGetAttribute(colorbut, "BGCOLOR"));

  IupPopup(color_dlg, IUP_CENTER, IUP_CENTER);

  if IupGetInt(color_dlg, "STATUS")==1 then
    Ihandle elem = layoutdlg_propelem
    Ihandle list1 = layoutdlg_list1
    Ihandle txt1 = IupGetDialogChild(colorbut, "VALUE1A");
    string val = IupGetAttribute(color_dlg, "VALUE");
    string name = IupGetAttribute(list1, IupGetAttribute(list1, "VALUE"));
    Ihandle showidlist = IupGetDialogChild(colorbut, "SHOWIDLIST");

    IupStoreAttribute(txt1, "VALUE", val);
    IupStoreAttribute(colorbut, "BGCOLOR", val);

    if IupGetInt(showidlist, "VISIBLE") then
      string item = IupGetAttribute(showidlist, "VALUE");
      if item!="" then
        string id = IupGetAttribute(showidlist, item);
        string nameid = sprintf("%s%s", {name, id});
        IupSetStrAttribute(elem, nameid, val);
      end if
    else
      IupStoreAttribute(elem, name, val);
    end if
--DEV PL...
--  if (layoutdlg) then
      layoutdlg_changed = 1
      IupUpdate(layoutdlg_canvas)
--  end if
  end if

  color_dlg = IupDestroy(color_dlg);

  return IUP_DEFAULT;
end function

function iLayoutPropertiesSetFont_CB(Ihandle fontbut)
  Ihandln font_dlg = IupFontDlg();
  Ihandle txt1 = IupGetDialogChild(fontbut, "VALUE1A");
  IupSetAttributeHandle(font_dlg, "PARENTDIALOG", IupGetDialog(fontbut));
  IupSetAttribute(font_dlg, "TITLE", "Choose Font");
  IupStoreAttribute(font_dlg, "VALUE", IupGetAttribute(txt1, "VALUE"));

  IupPopup(font_dlg, IUP_CENTER, IUP_CENTER);

  if IupGetInt(font_dlg, "STATUS")==1 then
    Ihandle elem = layoutdlg_propelem
    string val = IupGetAttribute(font_dlg, "VALUE");

    IupStoreAttribute(txt1, "VALUE", val);
    IupStoreAttribute(elem, "FONT", val);

--  if (layoutdlg) then
      layoutdlg_changed = 1;
      IupUpdate(layoutdlg_canvas);
--  end if
  end if

  font_dlg = IupDestroy(font_dlg)

  return IUP_DEFAULT
end function

procedure iLayoutPropertiesUpdateIdList(Ihandle showidlist, Ihandle ih, integer has_id2)
  IupSetAttribute(showidlist, "REMOVEITEM", NULL);
  IupSetAttribute(showidlist, "VALUE", NULL);

  if has_id2 then
    integer numcol = IupGetInt(ih, "NUMCOL"),
            numlin = IupGetInt(ih, "NUMLIN")
    for lin=0 to numlin do
      for col=0 to numcol do
--      IupSetStrAttributeId(showidlist, "", lin*numcol+col+1, "%d:%d", {lin, col});
        IupSetStrAttributeId(showidlist, "", lin*numcol+col+1, sprintf("%d:%d", {lin, col}));
      end for
    end for
  else
    integer id, start_id = 0,
            count = IupGetInt(ih, "COUNT");

    if IupClassMatch(ih, "list") then
      start_id = 1
    end if

    for id=start_id to count+start_id-1 do
      IupSetIntId(showidlist, "", id-start_id+1, id)
    end for
  end if
end procedure

function iLayoutPropertiesList1_CB(Ihandle list1, atom pName, integer /*item*/, integer state)
  if state then
    string name = peek_string(pName)
    nullable_string def_value;
    integer flags;
    Ihandle elem = layoutdlg_propelem
    string val = IupGetAttribute(elem, name);
    Ihandle txt1 = IupGetDialogChild(list1, "VALUE1A");
    Ihandle lbl2 = IupGetDialogChild(list1, "VALUE1B");
    Ihandle lbl3 = IupGetDialogChild(list1, "VALUE1C");
    Ihandle setbut = IupGetDialogChild(list1, "SETBUT");
    Ihandle colorbut = IupGetDialogChild(list1, "SETCOLORBUT");
    Ihandle fontbut = IupGetDialogChild(list1, "SETFONTBUT");
    Ihandle showidlist = IupGetDialogChild(list1, "SHOWIDLIST");

    {def_value, flags} = iupClassGetAttribNameInfo(ih_iclass(elem), name)

--  if val!=NULL and length(val)!=0 then
    if val!="" and length(val)!=0 then
--DEV oh dear...
      if and_bits(flags,IUPAF_NO_STRING) then
        IupSetStrAttribute(txt1, "VALUE", "%p", {val})
      else
        IupStoreAttribute(txt1, "VALUE", val)
      end if
    else
      IupSetAttribute(txt1, "VALUE", "NULL")
    end if

    if def_value!="" then
      IupStoreAttribute(lbl2, "TITLE", def_value);
    else
      IupSetAttribute(lbl2, "TITLE", "NULL")
    end if

    IupSetStrAttribute(lbl3, "TITLE", "%s\n%s%s%s%s%s",
--                     {iff(and_bits(flags,or_bits(IUPAF_NO_INHERIT,IUPAF_NO_STRING))!=0? "Inheritable": "NON Inheritable"), 
                       {iff(and_bits(flags,or_bits(IUPAF_NO_INHERIT,IUPAF_NO_STRING))!=0? "NON Inheritable": "Inheritable"), 
                        iff(and_bits(flags,IUPAF_NO_STRING)? "NOT a String\n": ""), 
                        iff(and_bits(flags,IUPAF_HAS_ID)? "Has ID\n":""), 
                        iff(and_bits(flags,IUPAF_READONLY)? "Read-Only\n": iff(and_bits(flags,IUPAF_WRITEONLY)? "Write-Only\n": "")),
                        iff(and_bits(flags,IUPAF_IHANDLENAME)? "Ihandle* name\n": ""),
                        iff(and_bits(flags,IUPAF_NOT_SUPPORTED)? "NOT SUPPORTED in this driver": "")})

    if iLayoutAttributeChanged(elem, name, val, def_value, flags) then
      IupSetAttribute(txt1, "FGCOLOR", "255 0 0");
    else
      IupSetAttribute(txt1, "FGCOLOR", "0 0 0")
    end if

    if and_bits(flags,IUPAF_READONLY)=0
    and and_bits(flags,IUPAF_NO_STRING)=0 then
      IupSetAttribute(setbut, "ACTIVE", "Yes");
      IupSetAttribute(txt1, "READONLY", "No");

      if match("COLOR",name)!=0 then
        IupStoreAttribute(colorbut, "BGCOLOR", val)
        IupSetAttribute(colorbut, "VISIBLE", "Yes")
      else
        IupSetAttribute(colorbut, "VISIBLE", "No")
      end if
      if name="FONT" then
        IupSetAttribute(fontbut, "VISIBLE", "Yes")
      else
        IupSetAttribute(fontbut, "VISIBLE", "No")
      end if
    else
      IupSetAttribute(txt1, "READONLY", "Yes");
      IupSetAttribute(setbut, "ACTIVE", "No");
      IupSetAttribute(colorbut, "VISIBLE", "No");
      IupSetAttribute(fontbut, "VISIBLE", "No");
    end if

    if and_bits(flags,IUPAF_HAS_ID) then
      IupSetAttribute(showidlist, "VISIBLE", "Yes");
      iLayoutPropertiesUpdateIdList(showidlist, elem, and_bits(flags,IUPAF_HAS_ID2));
    else
      IupSetAttribute(showidlist, "VISIBLE", "No")
    end if
  end if

  return IUP_DEFAULT
end function

function iLayoutPropertiesList2_CB(Ihandle list2, atom pName, integer /*item*/, integer state)
--  (void)item;
  if state then
    string name = peek_string(pName)
    Ihandle elem = layoutdlg_propelem
    atom val = iupAttribGetInt(elem, name);
    Ihandle lbl = IupGetDialogChild(list2, "VALUE2");
    if val then
      IupSetStrAttribute(lbl, "VALUE", "%p", {val});
    else
      IupSetAttribute(lbl, "VALUE", "NULL")
    end if
  end if
  return IUP_DEFAULT
end function

function iLayoutPropertiesGetAsString_CB(Ihandle button)
  Ihandle elem = layoutdlg_propelem
  Ihandle list2 = layoutdlg_list2
  string item = IupGetAttribute(list2, "VALUE");
  if item!="" then
    string val = iupAttribGet(elem, IupGetAttribute(list2, item));
    Ihandle lbl = IupGetDialogChild(button, "VALUE2");
    if val!="" then
      IupStoreAttribute(lbl, "VALUE", val);
    else
      IupSetAttribute(lbl, "VALUE", "NULL")
    end if
  end if
  return IUP_DEFAULT
end function

function iLayoutPropertiesSetStr_CB(Ihandle button)
  Ihandle elem = layoutdlg_propelem
  string name = IupGetAttribute(IupGetDialogChild(button, "NAME22"), "VALUE");
  string val = IupGetAttribute(IupGetDialogChild(button, "VALUE22"), "VALUE");
  if val="" or val="NULL" then
    IupSetAttribute(elem, name, NULL)
  else
    IupStoreAttribute(elem, name, val)
  end if

  iLayoutPropertiesUpdate(IupGetDialog(button), elem);

  return IUP_DEFAULT
end function

function iLayoutPropertiesList3_CB(Ihandle list3, atom pText, integer /*item*/, integer state)
--  (void)item;
  if state then
    string name = peek_string(pText)
    Ihandle elem = layoutdlg_propelem
    atom cb = IupGetCallback(elem, name);
    Ihandle lbl = IupGetDialogChild(list3, "VALUE3");
    if cb!=NULL then
--    char* name = ""--iupGetCallbackName(elem, text);
--    if (name)
--      IupSetfAttribute(lbl, "TITLE", "%p\n\"%s\"", cb, name);
--    else
--      IupSetStrAttribute(lbl, "TITLE", "%p", {cb});
        IupSetStrAttribute(lbl, "TITLE", "#%08x", {cb});
    else
      IupSetAttribute(lbl, "TITLE", "NULL")
    end if
  end if
  return IUP_DEFAULT
end function

function iLayoutPropertiesTabChangePos_CB(Ihandle ih, integer new_pos, integer /*old_pos*/)
--  (void)old_pos;
  if new_pos=0 then
    IupSetAttribute(ih, "TIP", "All attributes that are known by the element.");
  elsif new_pos=1 then
    IupSetAttribute(ih, "TIP", "Custom attributes set by the application.");
  elsif new_pos=2 then
    IupSetAttribute(ih, "TIP", "All callbacks that are known by the element.");
  end if

  /* In GTK the TIP appears for all children */
  /* TODO: move this code to iupgtk_tabs.c */
  if upper(IupGetGlobal("DRIVER"))="GTK" then
    string tabtype = IupGetAttribute(ih, "TABTYPE");
    integer x = 0
    integer y = 0
    integer w = ih_currentwidth(ih)
    integer h = ih_currentheight(ih)
    integer {cw, ch} = IupGetIntInt(ih, "CLIENTSIZE")

    /* TABORIENTATION is ignored */
    if upper(tabtype)="BOTTOM" then
      y += ch;  /* position after the client area */
      h -= ch;
    elsif upper(tabtype)="RIGHT" then
      x += cw;  /* position after the client area */
      w -= cw;
    elsif upper(tabtype)="LEFT" then
      w -= cw;
    else  /* TOP */
      h -= ch;
    end if
    IupSetStrAttribute(ih, "TIPRECT", "%d %d %d %d", {x, y, x+w, y+h});
  end if

  IupSetAttribute(ih, "TIPVISIBLE", "YES");
  return IUP_DEFAULT
end function

function iLayoutPropertiesCreateDialog(Ihandle parent)
  Ihandle closebtn, dlg, dlg_box, button_box, colorbut, fontbut,
          tabs, box1, box11, box2, box22, box3, box33, set, showidlist;
--  nullable_string padding = IupGetGlobal("DEFAULTBUTTONPADDING")
  string padding = IupGetGlobal("DEFAULTBUTTONPADDING")

  closebtn = IupButton("Close", NULL);
--  if string(padding) then
    IupSetStrAttribute(closebtn, "PADDING", padding);
--  end if
  IupSetCallback(closebtn, "ACTION", Icallback("iLayoutPropertiesClose_CB"));

  button_box = IupHbox({IupFill(), closebtn}, "MARGIN=0x0");

  layoutdlg_list1 = IupList("ACTION", Icallback("iLayoutPropertiesList1_CB"), "VISIBLELINES=15, VISIBLECOLUMNS=11, SORT=Yes, EXPAND=VERTICAL")
  layoutdlg_list2 = IupList("ACTION", Icallback("iLayoutPropertiesList2_CB"), "VISIBLELINES=15, VISIBLECOLUMNS=11, SORT=Yes, EXPAND=VERTICAL")
  layoutdlg_list3 = IupList("ACTION", Icallback("iLayoutPropertiesList3_CB"), "VISIBLELINES=15, VISIBLECOLUMNS=14, SORT=Yes, EXPAND=VERTICAL")

  set = IupButton("Set", NULL);
  IupSetCallback(set, "ACTION", Icallback("iLayoutPropertiesSet_CB"));
  IupSetStrAttribute(set, "PADDING", padding);
  IupSetAttribute(set, "NAME", "SETBUT");

  showidlist = IupList(NULL);
  IupSetCallback(showidlist, "ACTION", Icallback("iLayoutPropertiesShowId_CB"));
  IupSetAttribute(showidlist, "DROPDOWN", "Yes");
  IupSetAttribute(showidlist, "VISIBLECOLUMNS", "5");
  IupSetAttribute(showidlist, "NAME", "SHOWIDLIST");

  colorbut = IupButton(NULL, NULL);
  IupSetAttribute(colorbut, "SIZE", "20x10");
  IupStoreAttribute(colorbut, "BGCOLOR", "0 0 0");
  IupSetCallback(colorbut, "ACTION", Icallback("iLayoutPropertiesSetColor_CB"));
  IupSetAttribute(colorbut, "NAME", "SETCOLORBUT");
  IupSetAttribute(colorbut, "VISIBLE", "NO");

  fontbut = IupButton("F", NULL);
  IupSetAttribute(fontbut, "SIZE", "20x10");
  IupStoreAttribute(fontbut, "FONT", "Times, Bold Italic 12");
  IupSetCallback(fontbut, "ACTION", Icallback("iLayoutPropertiesSetFont_CB"));
  IupSetAttribute(fontbut, "NAME", "SETFONTBUT");
  IupSetAttribute(fontbut, "VISIBLE", "NO");

  box11 = IupVbox({
            IupLabel("Value:"),
            IupHbox({IupText("MULTILINE=Yes, ALIGNMENT=ALEFT:ATOP, EXPAND=YES, NAME=VALUE1A"), IupVbox({set, colorbut, fontbut, showidlist})}),
--          IupSetAttributesf(IupFill(), "RASTERSIZE=10"), 
            IupFill("RASTERSIZE=10"), 
            IupLabel("Default Value:"),
            IupFrame(IupLabel(NULL, "ALIGNMENT=ALEFT:ATOP, EXPAND=HORIZONTAL, NAME=VALUE1B")),
--          IupSetAttributesf(IupFill(), "RASTERSIZE=10"), 
            IupFill("RASTERSIZE=10"), 
            IupLabel("Other Info:"),
            IupFrame(IupLabel(NULL, "SIZE=90x48, ALIGNMENT=ALEFT:ATOP, NAME=VALUE1C"))})
  IupSetAttribute(box11,"MARGIN","0x0");
  IupSetAttribute(box11,"GAP","0");

  box22 = IupVbox({
            IupLabel("Value:"),
            IupText("MULTILINE=Yes, ALIGNMENT=ALEFT:ATOP, EXPAND=YES, NAME=VALUE2, READONLY=Yes"),
--          IupSetAttributesf(IupFill(), "RASTERSIZE=10"), 
            IupFill("RASTERSIZE=10"), 
            IupButton("Get as String", "ACTION", Icallback("iLayoutPropertiesGetAsString_CB"), "PADDING=3x3"),
            IupLabel("IMPORTANT: if the attribute is not a string\nthis can crash the application."),
--          IupSetAttributesf(IupFill(), "SIZE=60")})
            IupFill("SIZE=60")})
  IupSetAttribute(box22,"MARGIN","0x0");
  IupSetAttribute(box22,"GAP","0");

  box33 = IupVbox({
            IupLabel("Value:"),
            IupFrame(IupLabel("", "SIZE=x20, ALIGNMENT=ALEFT:ATOP, EXPAND=HORIZONTAL, NAME=VALUE3"))})
  IupSetAttribute(box33,"MARGIN","0x0");
  IupSetAttribute(box33,"GAP","0");

  box1 = IupHbox({IupVbox({IupLabel("Name:"), layoutdlg_list1}, "MARGIN=0x0, GAP=0"), box11});
  box2 = IupHbox({IupVbox({IupLabel("Name:"), layoutdlg_list2}, "MARGIN=0x0, GAP=0"), box22});
  box3 = IupHbox({IupVbox({IupLabel("Name:"), layoutdlg_list3}, "MARGIN=0x0, GAP=0"), box33});

  box2 = IupVbox({box2, 
                  IupHbox({IupVbox({IupLabel("Name:"), IupText("VISIBLECOLUMNS=9, NAME=NAME22")}, "GAP=0"), 
                           IupVbox({IupLabel("Value:"), IupText("EXPAND=HORIZONTAL, NAME=VALUE22")}, "GAP=0"), 
                           IupButton("Set", "ACTION", Icallback("iLayoutPropertiesSetStr_CB"), "PADDING=3x0")},
                          "ALIGNMENT=ABOTTOM")},
                "MARGIN=0x0")

  tabs = IupTabs({box1, box2, box3});
  IupSetAttribute(tabs, "TABTITLE0", "Registered Attributes");
  IupSetAttribute(tabs, "TABTITLE1", "Hash Table Only");
  IupSetAttribute(tabs, "TABTITLE2", "Callbacks");
  IupSetCallback(tabs, "TABCHANGEPOS_CB", Icallback("iLayoutPropertiesTabChangePos_CB"));
  {} = iLayoutPropertiesTabChangePos_CB(tabs, 0, 0);

  dlg_box = IupVbox({IupLabel("", "EXPAND=HORIZONTAL, NAME=ELEMTITLE"),
                     tabs,
                     button_box})

  IupSetAttribute(dlg_box,"MARGIN","10x10");
  IupSetAttribute(dlg_box,"GAP","10");

  dlg = IupDialog(dlg_box);
  IupSetAttribute(dlg,"TITLE", "Element Properties");
  IupSetAttribute(dlg,"MINBOX","NO");
  IupSetAttribute(dlg,"MAXBOX","NO");
  IupSetAttributeHandle(dlg,"DEFAULTENTER", closebtn);
  IupSetAttributeHandle(dlg,"DEFAULTESC", closebtn);
  IupSetAttributeHandle(dlg,"PARENTDIALOG", parent);
  nullable_string icon = IupGetGlobal("ICON")
  if string(icon) then
    IupSetAttribute(dlg,"ICON", icon);
  end if

  return dlg
end function

--/*
function IupElementPropertiesDialog(Ihandle elem)
  Ihandle dlg = iLayoutPropertiesCreateDialog(NULL)
  iLayoutPropertiesUpdate(dlg, elem)
  return dlg
end function
--*/

/***************************************************************************
                          Context Menu
 ***************************************************************************/


function iLayoutContextMenuProperties_CB(Ihandle /*menu*/)
  if layoutdlg_properties=NULL then
    layoutdlg_properties = iLayoutPropertiesCreateDialog(layoutdlg_dlg)
  end if
  iLayoutPropertiesUpdate(layoutdlg_properties, layoutcontextelement);
  IupShow(layoutdlg_properties)
  return IUP_DEFAULT
end function

function iLayoutContextMenuAdd_CB(Ihandle menu)
  Ihandle ref_elem = layoutcontextelement
  integer ret, count, i;    
  sequence class_list = sort(IupGetAllClasses())
  Ihandln old_parent_dlg

  /* filter the list of classes */
  count = 0
  for i=1 to length(class_list) do
--  Iclass* iclass = iupRegisterFindClass(class_list[i])
    atom iclass = iupRegisterFindClass(class_list[i])
    if find(iclass_nativetype(iclass),{IUP_TYPEVOID,IUP_TYPECONTROL,IUP_TYPECANVAS}) then
        count += 1
        class_list[count] = class_list[i]
    end if
  end for
  class_list = class_list[1..count]

--  old_parent_dlg = IupGetGlobal("PARENTDIALOG")
--  old_parent_dlg = IupGetInt(NULL,"PARENTDIALOG")
--DEV... better:
  old_parent_dlg = IupGetAttributePtr(NULL,"PARENTDIALOG")
  IupSetAttributeHandle(NULL, "PARENTDIALOG", layoutdlg_dlg);

  ret = IupListDialog(1,"Available Classes",class_list,1,10,iff(count<15?count+1:15))

  IupStoreGlobal("PARENTDIALOG", old_parent_dlg)

--  if ret!=-1 then
  if ret>0 then
--  Ihandle ret_ih = NULL;
    integer add_child = IupGetInt(menu, "_IUP_ADDCHILD");
    Ihandle new_ih = IupCreate(class_list[ret]);
    integer ref_id = IupTreeGetId(layoutdlg_tree, ref_elem);

    if add_child then
      Ihandln child = ih_firstchild(ref_elem)
      if child!=NULL and and_bits(ih_flags(child),IUP_INTERNAL) then
        /* the first child is internal, so add after it */
        if ih_brother(child)!=NULL then
--        ret_ih = IupInsert(ref_elem, ih_brother(child), new_ih);
          IupInsert(ref_elem, ih_brother(child), new_ih);
        else
--        ret_ih = IupAppend(ref_elem, new_ih)
          IupAppend(ref_elem, new_ih)
        end if
      else
        /* add as first child */
--      ret_ih = 
        IupInsert(ref_elem, NULL, new_ih);
      end if
    else
      if ih_parent(ref_elem)=NULL then
        IupMessage("Error", "Can NOT add here as brother.");
        return IUP_DEFAULT;
      end if

      /* add as brother after reference */
      if ih_brother(ref_elem)!=NULL then
        /* add before the brother, so it will be the brother */
--      ret_ih = 
        IupInsert(ih_parent(ref_elem), ih_brother(ref_elem), new_ih);
      else
--      ret_ih = 
        IupAppend(ih_parent(ref_elem), new_ih)
      end if
    end if

--  if ret_ih=NULL then
--    IupMessage("Error", "Add failed. Invalid operation for this node.");
--    return IUP_DEFAULT;
--  end if

    layoutdlg_changed = 1;

    {} = iLayoutTreeAddNode(layoutdlg_tree, ref_id, new_ih);

    IupRefresh(layoutdlg_dialog);

    IupUpdate(layoutdlg_canvas);
  end if

  return IUP_DEFAULT;
end function

procedure iLayoutUpdateColors(Ihandle tree, Ihandle ih)
  iLayoutTreeSetNodeColor(tree, IupTreeGetId(tree, ih), ih);

  if ih_iclass_childtype(ih)!=IUP_CHILDNONE then
    Ihandln child = ih_firstchild(ih)
    while child!=NULL do
      iLayoutUpdateColors(tree, child);
      child = ih_brother(child)
    end while
  end if
end procedure

function iLayoutContextMenuMap_CB(Ihandle /*menu*/)
  IupMap(layoutcontextelement)
  iLayoutUpdateColors(layoutdlg_tree, layoutcontextelement);
  IupRefresh(layoutdlg_dialog);
  IupUpdate(layoutdlg_canvas);
  return IUP_DEFAULT;
end function

function iLayoutContextMenuRefreshChildren_CB(Ihandle /*menu*/)
  IupRefreshChildren(layoutdlg_dialog);
  IupUpdate(layoutdlg_canvas);
  return IUP_DEFAULT
end function

procedure iLayoutSaveAttributes(Ihandle ih)
  IupSaveClassAttributes(ih);

  if ih_iclass_childtype(ih)!=IUP_CHILDNONE then
    Ihandln child = ih_firstchild(ih)
    while child!=NULL do
      iLayoutSaveAttributes(child)
      child = ih_brother(child)
    end while
  end if
end procedure

function iLayoutContextMenuUnmap_CB(Ihandle /*menu*/)
  iLayoutSaveAttributes(layoutcontextelement);
  IupUnmap(layoutcontextelement);
  iLayoutUpdateColors(layoutdlg_tree, layoutcontextelement)
  IupRefresh(layoutdlg_dialog)
  IupUpdate(layoutdlg_canvas)
  return IUP_DEFAULT
end function

function iLayoutContextMenuSetFocus_CB(Ihandle /*menu*/)
  IupSetFocus(layoutcontextelement);
  return IUP_DEFAULT;
end function

forward procedure iLayoutBlink(Ihandle ih)

function iLayoutContextMenuBlink_CB(Ihandle /*menu*/)
  iLayoutBlink(layoutcontextelement)
  return IUP_DEFAULT
end function

function iLayoutContextMenuRemove_CB(Ihandle /*menu*/)
  Ihandle msg_dlg;
  Ihandln elem = layoutcontextelement
  if elem=NULL then  /* can be called from a key press */
    elem = IupTreeGetUserId(layoutdlg_tree, IupGetInt(layoutdlg_tree, "VALUE"))
  end if
  if elem=NULL then
    return IUP_DEFAULT
  end if
  if and_bits(ih_flags(elem),IUP_INTERNAL) then
    IupMessage("Error", "Can NOT remove this child. It is an internal element of the container.");
    return IUP_DEFAULT;
  end if

  msg_dlg = IupMessageDlg();
  IupSetAttribute(msg_dlg,"DIALOGTYPE", "QUESTION");
  IupSetAttribute(msg_dlg,"BUTTONS", "OKCANCEL");
  IupSetAttribute(msg_dlg,"TITLE", "Element Remove");
  IupSetAttribute(msg_dlg,"VALUE", "Remove the selected element?");

  IupPopup(msg_dlg, IUP_MOUSEPOS, IUP_MOUSEPOS);

  if IupGetInt(msg_dlg, "BUTTONRESPONSE")==1 then
    integer id = IupTreeGetId(layoutdlg_tree, elem);

    layoutdlg_changed = 1;

    /* remove from the tree */
    IupSetAttributeId(layoutdlg_tree, "DELNODE", id, "SELECTED");

    /* update properties if necessary */
    if layoutdlg_properties!=NULL
    and IupGetInt(layoutdlg_properties, "VISIBLE") then
      Ihandle propelem = layoutdlg_propelem
      if iupChildTreeIsChild(elem, propelem) then
        /* if current element will be removed, then use the previous element on the tree |*/
        iLayoutPropertiesUpdate(layoutdlg_properties, IupTreeGetUserId(layoutdlg_tree, id-1));
      end if
    end if

    if elem=layoutcontextelement then   -- \
        layoutcontextelement = NULL     --  } PL added 19/5/16 (on a whim)
    end if                              -- /
    elem = IupDestroy(elem);
    IupRefresh(layoutdlg_dialog);
    IupUpdate(layoutdlg_canvas);
  end if
  return IUP_DEFAULT
end function

function iLayoutContextMenuCopy_CB(Ihandle /*menu*/)
  layoutdlg_copy = layoutcontextelement
  return IUP_DEFAULT;
end function

function iLayoutContextMenuPaste_CB(Ihandle menu)
  Ihandle new_ih
--  Ihandln ret_ih = NULL
  Ihandle ref_elem = layoutcontextelement
  integer paste_child = IupGetInt(menu, "_IUP_PASTECHILD");
  integer ref_id = IupTreeGetId(layoutdlg_tree, ref_elem);
  if not iupObjectCheck(layoutdlg_copy) then
    return IUP_DEFAULT
  end if
  new_ih = IupCreate(IupGetClassName(layoutdlg_copy))
  IupCopyClassAttributes(layoutdlg_copy, new_ih);

  if paste_child then
    /* add as first child */
--  ret_ih = 
    IupInsert(ref_elem, NULL, new_ih);
  else
    if ih_parent(ref_elem)=NULL then
      IupMessage("Error", "Can NOT paste here as brother.");
      return IUP_DEFAULT;
    end if

    /* add as brother after reference */
    if ih_brother(ref_elem)!=NULL then
      /* add before the brother, so it will be the brother */
--    ret_ih = 
      IupInsert(ih_parent(ref_elem), ih_brother(ref_elem), new_ih); 
    else
--    ret_ih = 
      IupAppend(ih_parent(ref_elem), new_ih)
    end if
  end if

--  if ret_ih=NULL then
--  IupMessage("Error", "Paste failed. Invalid operation for this node.");
--  return IUP_DEFAULT;
--  end if

  layoutdlg_changed = 1;

  /* add to the tree */
  {} = iLayoutTreeAddNode(layoutdlg_tree, ref_id, new_ih);

  /* make sure the dialog layout is updated */
  IupRefresh(layoutdlg_dialog);

  /* since we are only moving existing nodes, 
     title, map state, and user data was not changed.
     there is no need to update the node info */

  IupUpdate(layoutdlg_canvas)

  return IUP_DEFAULT;
end function

function YN(integer flag) return iff(flag?"Yes":"No") end function

procedure iLayoutContextMenu(Ihandle ih)
  Ihandle menu;
  integer is_container = (ih_iclass_childtype(ih)!=IUP_CHILDNONE)
  integer can_copy = ((not is_container) or ih_firstchild(ih) == NULL)
  integer can_paste = (layoutdlg_copy!=NULL)
  integer can_map = (ih_handle(ih)==NULL) and (ih_parent(ih)==NULL or ih_handle(ih_parent(ih))!=NULL)
  integer can_unmap = (ih_handle(ih)!=NULL)
  integer can_blink = (ih_iclass_nativetype(ih)!=IUP_TYPEVOID and IupGetInt(ih, "VISIBLE"))
  integer can_focus = iupFocusCanAccept(ih)

  menu = IupMenu({
    IupMenuItem("Properties...",    "ACTION", Icallback("iLayoutContextMenuProperties_CB")),
    IupMenuItem("Map",              "ACTION", Icallback("iLayoutContextMenuMap_CB"),            "ACTIVE=%s", {YN(can_map)}),
    IupMenuItem("Unmap",            "ACTION", Icallback("iLayoutContextMenuUnmap_CB"),          "ACTIVE=%s", {YN(can_unmap)}),
    IupMenuItem("Refresh Children", "ACTION", Icallback("iLayoutContextMenuRefreshChildren_CB")),
    IupSeparator(),
    IupMenuItem("Blink",            "ACTION", Icallback("iLayoutContextMenuBlink_CB"),          "ACTIVE=%s", {YN(can_blink)}),
    IupMenuItem("Set Focus",        "ACTION", Icallback("iLayoutContextMenuSetFocus_CB"),       "ACTIVE=%s", {YN(can_focus)}),
    IupSeparator(),
    IupMenuItem("Copy",             "ACTION", Icallback("iLayoutContextMenuCopy_CB"),           "ACTIVE=%s", {YN(can_copy)}),
    IupMenuItem("Paste Child",      "ACTION", Icallback("iLayoutContextMenuPaste_CB"),          "ACTIVE=%s, _IUP_PASTECHILD=1", {YN(can_paste and is_container)}),
    IupMenuItem("Paste Brother",    "ACTION", Icallback("iLayoutContextMenuPaste_CB"),          "ACTIVE=%s", {YN(can_paste)}),
    IupSeparator(),
    IupMenuItem("Add Child...",     "ACTION", Icallback("iLayoutContextMenuAdd_CB"),            "ACTIVE=%s, _IUP_ADDCHILD=1", {YN(is_container)}),
    IupMenuItem("Add Brother...",   "ACTION", Icallback("iLayoutContextMenuAdd_CB"),            "_IUP_ADDCHILD=0"),
    IupMenuItem("Remove...\tDel",   "ACTION", Icallback("iLayoutContextMenuRemove_CB"))})

  layoutcontextelement = ih
--DEV/temp: (seems fine)
--  if layoutdlg_dlg!=dlg then ?9/0 end if

  IupPopup(menu, IUP_MOUSEPOS, IUP_MOUSEPOS)
end procedure


/***************************************************************************
                       Layout Canvas Interaction
 ***************************************************************************/


procedure iLayoutBlink(Ihandle ih)
  if ih_iclass_nativetype(ih)!=IUP_TYPEVOID and IupGetInt(ih, "VISIBLE") then
    for i=1 to 3 do
      IupSetAttribute(ih, "VISIBLE", "NO");
      IupFlush();
--    iupdrvSleep(100);
      sleep(0.1)
      IupSetAttribute(ih, "VISIBLE", "Yes");
      IupFlush();
--    iupdrvSleep(100);
      sleep(0.1)
    end for
  end if
end procedure

procedure iLayoutUpdateMark(Ihandle ih, integer id)
  IupSetStrAttribute(layoutdlg_status, "TITLE", "[SZ] User:%4d,%4d | Natural:%4d,%4d | Current:%4d,%4d", 
                     {ih_userwidth(ih),    ih_userheight(ih), 
                      ih_naturalwidth(ih), ih_naturalheight(ih), 
                      ih_currentwidth(ih), ih_currentheight(ih)})

  if ih_handle(ih)=NULL then
    IupSetAttributeId(layoutdlg_tree, "COLOR", id, "128 0 0")
  else
    IupSetAttributeId(layoutdlg_tree, "COLOR", id, "255 0 0")
  end if
  
  layoutdlg_mark = ih
  IupUpdate(layoutdlg_canvas);

  if layoutdlg_properties!=NULL
  and IupGetInt(layoutdlg_properties, "VISIBLE") then
    iLayoutPropertiesUpdate(layoutdlg_properties, ih)
  end if
end procedure

function iLayoutFindElementByPos(Ihandle ih, integer native_parent_x, integer native_parent_y, integer x, integer y, integer dlgvisible, integer shownotmapped)
  Ihandln child
  Ihandln elem
  integer dx, dy;

  /* can only click in elements that are visible, 
     independent from showhidden */
  if (iLayoutElementIsVisible(ih, dlgvisible)
  and (shownotmapped or ih_handle(ih)!=NULL)) then
    /* check the element */
    if x >= ih_x(ih)+native_parent_x
    and y >= ih_y(ih)+native_parent_y
    and x < ih_x(ih)+native_parent_x+ih_currentwidth(ih)
    and y < ih_y(ih)+native_parent_y+ih_currentheight(ih) then
      if ih_iclass_childtype(ih)!= IUP_CHILDNONE then
        -- if ih is a native parent, then update the offset
        if ih_iclass_nativetype(ih)!=IUP_TYPEVOID then
          {dx,dy} = IupGetIntInt(ih, "CLIENTOFFSET")
          native_parent_x += ih_x(ih)+dx;
          native_parent_y += ih_y(ih)+dy;

          -- if ih is a Tabs, then find only the active child
          if IupClassMatch(ih, "tabs") then
            child = IupGetInt(ih, "VALUE_HANDLE");
            if child!=NULL then
              elem = iLayoutFindElementByPos(child, native_parent_x, native_parent_y, x, y, dlgvisible, shownotmapped);
              if elem!=NULL then
                return elem
              end if
            end if
            return ih
          end if
        end if
      end if

      /* check its children */
      child = ih_firstchild(ih)
      while child!=NULL do
        elem = iLayoutFindElementByPos(child, native_parent_x, native_parent_y, x, y, dlgvisible, shownotmapped);
        if elem!=NULL then 
            return elem
        end if
        child = ih_brother(child)
      end while

      return ih;
    end if
  end if
  return NULL
end function

-- layoutdlg as static (seems OK)
function iLayoutFindDialogElementByPos(integer x, integer y)

  /* check the dialog */
  integer {w, h} = IupGetIntInt(layoutdlg_dialog, "CLIENTSIZE")
  if ih_firstchild(layoutdlg_dialog)!=NULL 
  and x>=0 and y>=0
  and x<w and y<h then
    Ihandln elem
    integer shownotmapped = (ih_handle(layoutdlg_dialog)==NULL)  /* only check not mapped if dialog is also not mapped */
    integer dlgvisible = IupGetInt(layoutdlg_dialog, "VISIBLE");
    integer {native_parent_x, native_parent_y} = IupGetIntInt(layoutdlg_dialog, "CLIENTOFFSET")
    elem = iLayoutFindElementByPos(ih_firstchild(layoutdlg_dialog), native_parent_x, native_parent_y, x, y, dlgvisible, shownotmapped);
    if elem!=NULL then
      return elem
    end if
    return layoutdlg_dialog
  end if
  return NULL
end function

function iLayoutCanvasButton_CB(Ihandle canvas, integer but, integer pressed, integer x, integer y, atom status)
  if but==IUP_BUTTON1 and pressed then
    Ihandle dlg = IupGetDialog(canvas)
    Ihandln elem = iLayoutFindDialogElementByPos(x, y)
    if elem!=NULL then
      if iup_isdouble(status) then
        iLayoutBlink(elem);
        IupUpdate(canvas);
      else
        integer id = IupTreeGetId(layoutdlg_tree, elem);
        integer old_id = IupGetInt(layoutdlg_tree, "VALUE");
        Ihandle old_elem = IupTreeGetUserId(layoutdlg_tree, old_id);
        iLayoutTreeSetNodeColor(layoutdlg_tree, old_id, old_elem);
        IupSetInt(layoutdlg_tree, "VALUE", id);
        iLayoutUpdateMark(elem, id);
      end if
    elsif layoutdlg_mark!=NULL then
      layoutdlg_mark=NULL
      IupUpdate(canvas)
    end if
  elsif but==IUP_BUTTON3 and pressed then
    Ihandln elem = iLayoutFindDialogElementByPos(x, y)
    if elem!=NULL and elem!=layoutdlg_dialog then
      iLayoutContextMenu(elem)
    end if
  end if
  return IUP_DEFAULT
end function


/***************************************************************************
                              Layout Tree
 ***************************************************************************/


function iLayoutTreeExecuteLeaf_CB(Ihandle tree, integer id)
  Ihandle elem = IupTreeGetUserId(tree, id);
  iLayoutBlink(elem);
  return IUP_DEFAULT;
end function

function iLayoutTreeRightClick_CB(Ihandle tree, integer id)
  Ihandle elem = IupTreeGetUserId(tree, id);
  iLayoutContextMenu(elem)
  return IUP_DEFAULT;
end function

function iLayoutTreeDragDrop_CB(Ihandle tree, integer drag_id, integer drop_id, integer /*isshift*/, integer iscontrol)
  Ihandle dlg = IupGetDialog(tree);
  Ihandle drag_elem = IupTreeGetUserId(tree, drag_id);
  Ihandle drop_elem = IupTreeGetUserId(tree, drop_id);
  integer error;

  /* no support for copy */
  if iscontrol then
    IupMessage("Error", "Copy not supported for drag&drop.");
    return IUP_IGNORE;
  end if

  if and_bits(ih_flags(drag_elem),IUP_INTERNAL) then
    IupMessage("Error", "Can NOT drag an internal element. This element exists only inside this container.");
    return IUP_IGNORE;
  end if

  if upper(IupGetAttributeId(tree, "KIND", drop_id))="BRANCH"
  and upper(IupGetAttributeId(tree, "STATE", drop_id))="EXPANDED" then
    Ihandln ref_child = ih_firstchild(drop_elem)   /* the first child as reference */

    /* if first element is internal, use the next one. */
    if ref_child!=NULL and and_bits(ih_flags(ref_child),IUP_INTERNAL) then
      /* the first child is internal, so use brother as reference */
      if ih_brother(ref_child)!=NULL then
        ref_child = ih_brother(ref_child)
      end if
    end if

    if drop_elem==ih_parent(drag_elem) and ref_child==drag_elem then
       /* dropped at the same place, just ignore it */
       return IUP_IGNORE;
    end if

    /* If the drop node is a branch and it is expanded, */
    /* add as first child */
    error = IupReparent(drag_elem, drop_elem, ref_child);  /* add before the reference */
  else
    if ih_parent(drop_elem)=NULL then
      IupMessage("Error", "Can NOT drop here as brother.");
      return IUP_IGNORE;
    end if

    if ih_parent(drop_elem)=ih_parent(drag_elem) and ih_brother(drop_elem)=drag_elem then
      /* dropped at the same place, just ignore it */
      return IUP_IGNORE;
    end if

    /* If the branch is not expanded or the drop node is a leaf, */
    /* add as brother after reference */
    /* ih_brother(drop_elem) can be NULL here */
    error = IupReparent(drag_elem, ih_parent(drop_elem), ih_brother(drop_elem));    
  end if

    if error == IUP_ERROR then
        IupMessage("Error", "Drop failed. Invalid operation for this node.");
        return IUP_IGNORE;
    end if

  layoutdlg_changed = 1;

  /* make sure the dialog layout is updated */
  IupRefresh(layoutdlg_dialog);

  /* since we are only moving existing nodes, 
     title, map state, and user data was not changed.
     there is no need to update the node info */

  /* redraw canvas */
  IupUpdate(IupGetBrother(tree));

--  (void)isshift;
  return IUP_CONTINUE;  /* the nodes of the tree will be automatically moved */
end function

function iLayoutTreeSelection_CB(Ihandle tree, integer id, integer status)
  Ihandle elem = IupTreeGetUserId(tree, id);
  if status == 1 then
    iLayoutUpdateMark(elem, id);
  else
    iLayoutTreeSetNodeColor(tree, id, elem)
  end if
  return IUP_DEFAULT;
end function


/***************************************************************************
                            Layout Dialog Callbacks
 ***************************************************************************/


function iLayoutDialogKAny_CB(Ihandle dlg, atom key)
  switch key
  case K_DEL:
    return iLayoutContextMenuRemove_CB(dlg);
  case K_F5:
    return iLayoutMenuUpdate_CB(dlg);
  case K_ESC:
    return iLayoutMenuClose_CB(dlg);
  case K_cO:
    return iLayoutMenuLoad_CB(dlg);
  case K_cF5:
    return iLayoutMenuRefresh_CB(dlg);
  case K_cMinus:
  case K_cPlus:
  case K_cEqual:    -- PL (19/5/16) treat Ctrl= as Ctrl+
      integer opacity = IupGetInt(dlg, "OPACITY");
      if opacity == 0 then
        opacity = 255
      end if
      if key == K_cPlus
      or key == K_cEqual then
        opacity += 1
      else
        opacity -= 1
      end if
      if opacity == 0 or opacity == 255 then
        IupSetAttribute(dlg, "OPACITY", NULL);
      else
        IupSetInt(dlg,"OPACITY", opacity)
      end if
  end switch

  return IUP_DEFAULT;
end function

function iLayoutDialogShow_CB(Ihandle dlg, integer state)
  if state == IUP_SHOW then
    IupSetAttribute(dlg, "RASTERSIZE", NULL);
  end if
  return IUP_DEFAULT;
end function

function iLayoutDialogClose_CB(Ihandln dlg)
  if IupGetInt(dlg, "DESTROYWHENCLOSED") then
    dlg = IupDestroy(dlg)
    return IUP_IGNORE
  end if
  return IUP_DEFAULT
end function

function iLayoutDialogDestroy_CB(Ihandle /*dlg*/)
  layoutdlg_timer = IupDestroy(layoutdlg_timer);
  if layoutdlg_properties!=NULL and iupObjectCheck(layoutdlg_properties) then
    layoutdlg_properties = IupDestroy(layoutdlg_properties)
  end if
  if layoutdlg_destroy!=NULL and iupObjectCheck(layoutdlg_dialog) then
    layoutdlg_dialog = IupDestroy(layoutdlg_dialog)
  end if
  return IUP_DEFAULT;
end function

global function IupLayoutDialog2(Ihandle dialog)
Ihandle tree, canvas, dlg, menu, status, splitter;

    if xiupAttribGet=NULL then
     xiupAttribGet                  = iup_c_func(iup, "iupAttribGet", {P,P}, P)
     xiupAttribGetInherit           = iup_c_func(iup, "iupAttribGetInherit", {P,P}, P)
     xiupAttribGetInt               = iup_c_func(iup, "iupAttribGetInt", {P,P}, I)
     xiupAttribGetLocal             = iup_c_func(iup, "iupAttribGetLocal", {P,P}, P)
--   xiupAttribSetInt               = iup_c_proc(iup, "iupAttribSetInt", {P,P,I})
     xiupClassRegisterGetAttribute  = iup_c_proc(iup, "iupClassRegisterGetAttribute", {P,P,P,P,P,P,P})
     xiupDlgListVisibleCount        = iup_c_func(iup, "iupDlgListVisibleCount", {}, I)
     xiupDlgListCount               = iup_c_func(iup, "iupDlgListCount", {}, I)
     xiupDlgListFirst               = iup_c_func(iup, "iupDlgListFirst", {}, P)
     xiupDlgListNext                = iup_c_func(iup, "iupDlgListNext", {}, P)
     xiupdrvDrawCreateCanvas        = iup_c_func(iup, "iupdrvDrawCreateCanvas", {P}, P)
     xiupdrvDrawRectangle           = iup_c_proc(iup, "iupdrvDrawRectangle", {P,I,I,I,I,I,I,I,I})
     xiupdrvDrawLine                = iup_c_proc(iup, "iupdrvDrawLine", {P,I,I,I,I,I,I,I,I})
     xiupdrvDrawSetClipRect         = iup_c_proc(iup, "iupdrvDrawSetClipRect", {P,I,I,I,I})
--   xiupdrvDrawImage               = iup_c_proc(iup, "iupdrvDrawImage", {P,P,I,I,I,P,P})
     xiupdrvDrawImage               = iup_c_proc(iup, "iupdrvDrawImage", {P,P,I,I,I})
     xiupdrvDrawGetSize             = iup_c_proc(iup, "iupdrvDrawGetSize", {P,P,P})
     xiupdrvDrawText                = iup_c_proc(iup, "iupdrvDrawText", {P,P,I,I,I,I,I,I,P})
     xiupdrvDrawResetClip           = iup_c_proc(iup, "iupdrvDrawResetClip", {P})
     xiupdrvDrawSelectRect          = iup_c_proc(iup, "iupdrvDrawSelectRect", {P,I,I,I,I})
     xiupdrvDrawFlush               = iup_c_proc(iup, "iupdrvDrawFlush", {P})
     xiupdrvDrawKillCanvas          = iup_c_proc(iup, "iupdrvDrawKillCanvas", {P})
     xiupdrvGetScreenDpi            = iup_c_func(iup, "iupdrvGetScreenDpi", {}, F)
     xiupRegisterFindClass          = iup_c_func(iup, "iupRegisterFindClass", {P}, P)
     xiupObjectCheck                = iup_c_func(iup, "iupObjectCheck", {P}, I)
     xiupFocusCanAccept             = iup_c_func(iup, "iupFocusCanAccept", {P}, I)
    end if
  if dialog!=NULL then
    layoutdlg_dialog = dialog;
  else
    layoutdlg_dialog = IupDialog(NULL);
    layoutdlg_destroy = 1;
  end if

  layoutdlg_timer = IupTimer();
  IupSetCallback(layoutdlg_timer, "ACTION_CB", Icallback("iLayoutTimerAutoUpdate_CB"));
  IupSetAttribute(layoutdlg_timer, "TIME", "300");

  canvas = IupCanvas(NULL)
  layoutdlg_canvas = canvas
  IupSetCallback(canvas, "ACTION", Icallback("iLayoutCanvas_CB"));
  IupSetCallback(canvas, "BUTTON_CB", Icallback("iLayoutCanvasButton_CB"));

  tree = IupTree();
  layoutdlg_tree = tree;
  IupSetAttribute(tree, "RASTERSIZE", NULL);
  IupSetAttribute(tree, "SHOWDRAGDROP", "Yes");
--PL:
  IupSetAttribute(tree, "DROPEQUALDRAG", "YES");
  IupSetCallback(tree, "SELECTION_CB", Icallback("iLayoutTreeSelection_CB"));
  IupSetCallback(tree, "EXECUTELEAF_CB", Icallback("iLayoutTreeExecuteLeaf_CB"));
  IupSetCallback(tree, "RIGHTCLICK_CB", Icallback("iLayoutTreeRightClick_CB"));
  IupSetCallback(tree, "DRAGDROP_CB", Icallback("iLayoutTreeDragDrop_CB"));
  
  status = IupLabel(NULL);
  IupSetAttribute(status, "EXPAND", "HORIZONTAL");
  IupSetAttribute(status, "FONT", "Courier, 11");
  IupSetAttribute(status, "SIZE", "x12");
  layoutdlg_status = status;

  splitter = IupSplit(tree, canvas);
  IupSetAttribute(splitter, "VALUE", "300");

  menu = IupMenu({
    IupSubmenu("&Dialog", IupMenu({
      IupMenuItem("New", "ACTION", Icallback("iLayoutMenuNew_CB")),
      IupMenuItem("Load...\tCtrl+O", "ACTION", Icallback("iLayoutMenuLoad_CB")),
      IupMenuItem("Load Visible...", "ACTION", Icallback("iLayoutMenuLoadVisible_CB")),
      IupMenuItem("Reload", "ACTION", Icallback("iLayoutMenuReload_CB")),
      IupSubmenu("&Export", IupMenu({
        IupMenuItem("C...", "ACTION", Icallback("iLayoutMenuExportC_CB")),
        IupMenuItem("LED...", "ACTION", Icallback("iLayoutMenuExportLED_CB")),
        IupMenuItem("Lua...", "ACTION", Icallback("iLayoutMenuExportLua_CB"))})),
      IupSeparator(),
      IupMenuItem("Refresh\tCtrl+F5", "ACTION", Icallback("iLayoutMenuRefresh_CB")),
      IupMenuItem("Redraw", "ACTION", Icallback("iLayoutMenuRedraw_CB")),
      IupMenuItem("Show", "ACTION", Icallback("iLayoutMenuShow_CB")),
      IupMenuItem("Hide", "ACTION", Icallback("iLayoutMenuHide_CB")),
      IupSeparator(),
      IupMenuItem("&Close\tEsc", "ACTION", Icallback("iLayoutMenuClose_CB"))})),
    IupSubmenu("&Layout", IupMenu({
      IupMenuItem("&Hierarchy", "ACTION", Icallback("iLayoutMenuTree_CB"), "AUTOTOGGLE=YES, VALUE=ON"),
      IupSeparator(),
      IupMenuItem("Update\tF5", "ACTION", Icallback("iLayoutMenuUpdate_CB")),
      IupMenuItem("Auto Update", "ACTION", Icallback("iLayoutMenuAutoUpdate_CB"), "AUTOTOGGLE=YES, VALUE=OFF"),
      IupMenuItem("Show Hidden", "ACTION", Icallback("iLayoutMenuShowHidden_CB"), "AUTOTOGGLE=YES, VALUE=OFF"),
      IupSeparator(),
      IupMenuItem("Opacity\tCtrl+/Ctrl-", "ACTION", Icallback("iLayoutMenuOpacity_CB"))}))})

  dlg = IupDialog(IupVbox({splitter, status}))
  layoutdlg_dlg = dlg
  IupSetAttribute(dlg,"TITLE", "Dialog Layout");
  IupSetAttribute(dlg,"PARENTDIALOG", IupGetGlobal("PARENTDIALOG"));
  IupSetAttribute(dlg,"ICON", IupGetGlobal("ICON"));
  IupSetCallback(dlg, "DESTROY_CB", Icallback("iLayoutDialogDestroy_CB"));
  IupSetCallback(dlg, "SHOW_CB", Icallback("iLayoutDialogShow_CB"));
  IupSetCallback(dlg, "K_ANY", Icallback("iLayoutDialogKAny_CB"));
  IupSetCallback(dlg, "CLOSE_CB", Icallback("iLayoutDialogClose_CB"));
  IupSetAttributeHandle(dlg, "MENU", menu);
  IupSetAttribute(dlg, "OPACITY", "255");

  IupSetAttribute(dlg,"DESTROYWHENCLOSED", "Yes");

    integer {w,h} = IupGetIntInt(layoutdlg_dialog, "RASTERSIZE")
    if w and h then
      IupSetStrAttribute(dlg, "RASTERSIZE", "%dx%d", {w*1.3, h})
    else
      IupSetAttribute(dlg, "SIZE", "HALFxHALF")
    end if

  IupMap(dlg)

  iLayoutTreeRebuild()

  return dlg
end function

