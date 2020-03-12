--
-- LiteZip.e
--
--  Simple wrapper to https://www.codeproject.com/Articles/13370/LiteZip-and-LiteUnzip
--
--  Allows the creation of and extraction from standard zip archives.
--
--  While you may be able to create compatible archives with other tools, the emphasis
--  is really on extraction from archives created with this mechanism, rather than the
--  ability to extract from any archive, and targeted at software distribution.
--
--  Requires LiteUnZip.dll and/or LiteZip.dll to be installed/distributed, but either
--  may be absent, as long as only a subset of these routines are actually invoked.
--
--  NB: This is 32-bit Windows-only. While I have no objections to revisiting this if 
--  someone compiles the source (see link above) into a 64-bit dll or 32/64.so, it is
--  recommended to use eg system("unzip filename.zip -d dirname") or similar on linux.
--  You might want to try http://aminet.net/package/disk/misc/adfchk_v022_src as that
--  appears to contain a 32-bit libliteunzip.so - untested, also the exact same files 
--  can be found via http://adfchk.sourceforge.net/ or so it seems.
--  Personally I only ship a 32-bit phix installer anyway...
--  Likewise only the ansi/utf-8 routines are supported, but should suffice.
--  I have also deliberately dropped any support for passwords.
--
--  The compression functions are really only provided as a fail-safe, since unzip
--  is perhaps more likely to work on things the same library zipped. But, so far,
--  it works fine on 7-zip files, which are not surprisingly ~3.3% smaller (or it
--  is perhaps surprising that this manages to be only 3.3% larger.)
--  For my second test (source files not dlls), the difference was only 1.7%, so 
--  the zip was 23% of the original, as opposed to 22.5% achieved by 7-zip.
--
--  Not [yet] attempted:
--
--   UnzipItemToBuffer
--   UnzipItemToHandle
--   UnzipOpenBuffer[Raw]
--   UnzipOpenFileRaw
--   UnzipOpenHandle[Raw]
--
--   ZipAddBuffer[Raw]
--   ZipAddFileRaw
--   ZipAddHandle[Raw]
--   ZipAddPipe[Raw]
--   ZipCreateHandle
--   ZipCreateBuffer
--   ZipGetMemory
--   ZipResetMemory
--   ZipOptions (options are TZIP_OPTION_GZIP and TZIP_OPTION_ABORT only anyway)
--
--  Note this wrapper uses ZR_MESSAGES instead of (Unz|Z)ipFormatMessage().
--

global constant
    -- These are the return codes from Unzip functions
    ZR_OK           = 0,    -- Success
    -- The following come from general system stuff (e.g. files not openable)
    ZR_NOFILE       = 1,    -- Cannot create/open the file
    ZR_NOALLOC      = 2,    -- Failed to allocate memory
    ZR_WRITE        = 3,    -- A general error writing to the file
    ZR_NOTFOUND     = 4,    -- Cannot find the specified file in the zip
    ZR_MORE         = 5,    -- There is still more data to be unzipped
    ZR_CORRUPT      = 6,    -- The zipfile is corrupt or not a zipfile
    ZR_READ         = 7,    -- An error reading the file
    ZR_NOTSUPPORTED = 8,    -- The entry is in a format that can't be decompressed by this Unzip add-on
    -- The following come from mistakes on the part of the caller
    ZR_ARGS         = 9,    -- Bad arguments passed
    ZR_NOTMMAP      = 10,   -- Tried to ZipGetMemory, but that only works on mmap zipfiles, which yours wasn't
    ZR_MEMSIZE      = 11,   -- The memory-buffer size is too small
    ZR_FAILED       = 12,   -- Already failed when you called this function
    ZR_ENDED        = 13,   -- The zip creation has already been closed
    ZR_MISSIZE      = 14,   -- The source file size turned out mistaken
    ZR_ZMODE        = 15,   -- Tried to mix creating/opening a zip 
    -- The following come from bugs within the zip library itself
    ZR_SEEK         = 16,   -- trying to seek in an unseekable file
    ZR_NOCHANGE     = 17,   -- changed its mind on storage, but not allowed
    ZR_FLATE        = 18,   -- An error in the de/inflation code
    ZR_PASSWORD     = 19    -- Password is incorrect

constant ZR_MESSAGES = {
--  "ZR_OK           = 0   -- Success",
    "ZR_NOFILE       = 1   -- Cannot create/open the file",
    "ZR_NOALLOC      = 2   -- Failed to allocate memory",
    "ZR_WRITE        = 3   -- A general error writing to the file",
    "ZR_NOTFOUND     = 4   -- Cannot find the specified file in the zip",
    "ZR_MORE         = 5   -- There is still more data to be unzipped",
    "ZR_CORRUPT      = 6   -- The zipfile is corrupt or not a zipfile",
    "ZR_READ         = 7   -- An error reading the file",
    "ZR_NOTSUPPORTED = 8   -- The entry is in a format that can't be decompressed by this Unzip add-on",
    "ZR_ARGS         = 9   -- Bad arguments passed",
    "ZR_NOTMMAP      = 10  -- Tried to ZipGetMemory, but that only works on mmap zipfiles, which yours wasn't",
    "ZR_MEMSIZE      = 11  -- The memory-buffer size is too small",
    "ZR_FAILED       = 12  -- Already failed when you called this function",
    "ZR_ENDED        = 13  -- The zip creation has already been closed",
    "ZR_MISSIZE      = 14  -- The source file size turned out mistaken",
    "ZR_ZMODE        = 15  -- Tried to mix creating/opening a zip",
    "ZR_SEEK         = 16  -- trying to seek in an unseekable file",
    "ZR_NOCHANGE     = 17  -- changed its mind on storage, but not allowed",
    "ZR_FLATE        = 18  -- An error in the de/inflation code",
    "ZR_PASSWORD     = 19  -- Password is incorrect"}

atom zip = NULL,
     unzip = NULL

function find_dll(string dll_name)
    if platform()!=WINDOWS then ?9/0 end if
    if machine_bits()!=32 then ?9/0 end if
    atom res = open_dll(dll_name)
    if res=NULL then
        -- try to load from \builtins
        sequence s = include_paths()
        for i=1 to length(s) do
            if match("builtins",s[i]) then
                res = open_dll(join_path({s[i],dll_name}))
                if res!=NULL then exit end if
            end if
        end for
        if res=NULL then ?9/0 end if
    end if
    return res
end function

procedure zip_init()
    if zip!=NULL then ?9/0 end if -- sanity check
    zip = find_dll("LiteZip.dll")
end procedure

procedure unzip_init()
    if unzip!=NULL then ?9/0 end if -- sanity check
    unzip = find_dll("LiteUnZip.dll")
end procedure

function link_c_func(atom dll, sequence name, sequence args, atom result)
    if dll=NULL then ?9/0 end if
    integer rid = define_c_func(dll, name, args, result)
    if rid<1 then ?9/0 end if
    return rid
end function


atom xUnzipOpenFile = NULL
global function UnzipOpenFile(string filename)--, nullable_string password)
-- returns an atom handle or a string error message
    if xUnzipOpenFile=NULL then
        if unzip=NULL then unzip_init() end if
        xUnzipOpenFile = link_c_func(unzip,"UnzipOpenFileA",{C_PTR,C_PTR,C_PTR},C_INT)
    end if
    atom pHZIP = allocate(machine_word(),true)
    integer res = c_func(xUnzipOpenFile,{pHZIP,filename,NULL})
    if res!=ZR_OK then
        return ZR_MESSAGES[res]
    end if
    atom hzip = peekNS(pHZIP,machine_word(),false)
    return hzip
end function

atom xUnzipSetBaseDir = NULL
global procedure UnzipSetBaseDir(atom hzip, string basedir)
-- terminates in error on failure
    if xUnzipSetBaseDir=NULL then
        xUnzipSetBaseDir = link_c_func(unzip,"UnzipSetBaseDirA",{C_PTR,C_PTR},C_INT)
    end if
    integer res = c_func(xUnzipSetBaseDir,{hzip,basedir})
    if res!=ZR_OK then ?9/0 end if
end procedure

include builtins\cffi.e

constant tFT = """
typedef struct _FILETIME {
  DWORD dwLowDateTime;
  DWORD dwHighDateTime;
} FILETIME, *PFILETIME;
""",
idFT = define_struct(tFT)

-- Struct used to retrieve info about an entry in an archive
--DEV cfii does not handle this!:
--  FILETIME        AccessTime, CreateTime, ModifyTime; // access, create, modify filetimes

constant tZE = """
typedef struct
{
    DWORD           Index;
    DWORD           Attributes;
    FILETIME        AccessTime;
    FILETIME        CreateTime;
    FILETIME        ModifyTime;
    unsigned long   CompressedSize;
    unsigned long   UncompressedSize;
    char            Name[MAX_PATH];
} ZIPENTRY;
""",
--global constant integer 
idZE = define_struct(tZE)
--DEV... [fixed 6/3/18, once a new version gets shipped]
--constant integer {name_offset} = get_field_details(idZE, "Name") 
constant {name_offset} = get_field_details(idZE, "Name") 

global type ZIPENTRY(object o)
    return atom(o) and o!=NULL
end type

global function new_ZIPENTRY()
-- result must be free()d manually
    ZIPENTRY ze = allocate_struct(idZE)
    return ze
end function

atom xUnzipGetItem = NULL
global function UnzipGetItem(atom hzip, ZIPENTRY ze, integer idx)
-- returns ZR_OK or a string error message
    if xUnzipGetItem=NULL then
        xUnzipGetItem = link_c_func(unzip,"UnzipGetItemA",{C_PTR,C_PTR},C_INT)
    end if
    set_struct_field(idZE,ze,"Index",idx)
    integer res = c_func(xUnzipGetItem,{hzip,ze})
    if res!=ZR_OK then
        return ZR_MESSAGES[res]
    end if
    return ZR_OK
end function

global function UnzipGetItems(atom hzip, ZIPENTRY ze)
-- returns the integer count of entries or a string error message
    object res = UnzipGetItem(hzip, ze, -1)
    if res=ZR_OK then
        res = get_struct_field(idZE,ze,"Index")
    end if
    return res
end function

global function UnzipGetFileName(ZIPENTRY ze)
    string filename = peek_string(ze+name_offset)
    return filename
end function

constant MAX_PATH = 260

atom xUnzipFindItem = NULL
global function UnzipFindItem(atom hzip, ZIPENTRY ze, string name, bool case_insensitive=false)
-- returns ZR_OK (and pZIPENTRY.Index etc) if found, or a string error message
-- (can be ZR_ARGS, ZR_NOTFOUND, ZR_NOTSUPPORTED, ZR_NOALLOC, ZR_CORRUPT, ...)
    if xUnzipFindItem=NULL then
        xUnzipFindItem = link_c_func(unzip,"UnzipFindItemA",{C_PTR,C_PTR,C_INT},C_INT)
    end if
    if length(name)>=MAX_PATH then ?9/0 end if
    poke(ze+name_offset,name&'\0')
    integer res = c_func(xUnzipFindItem,{hzip,ze,case_insensitive})
    if res!=ZR_OK then
        return ZR_MESSAGES[res]
    end if
    return res
end function

atom xUnzipItemToFile = NULL
global function UnzipItemToFile(atom hzip, ZIPENTRY ze, string filename="")
-- returns ZR_OK or a string error message
    if xUnzipItemToFile=NULL then
        xUnzipItemToFile = link_c_func(unzip,"UnzipItemToFileA",{C_PTR,C_PTR,C_PTR},C_INT)
    end if
    object pfn = iff(length(filename)?filename:ze+name_offset) -- (string/atom)
    integer res = c_func(xUnzipItemToFile,{hzip,pfn,ze})
    if res!=ZR_OK then
        return ZR_MESSAGES[res]
    end if
    return ZR_OK
end function

atom xUnzipClose = NULL
global procedure UnzipClose(atom hzip)
-- terminates in error on failure
    if xUnzipClose=NULL then
        xUnzipClose = link_c_func(unzip,"UnzipClose",{C_PTR},C_INT)
    end if
    integer res = c_func(xUnzipClose,{hzip})
    if res!=ZR_OK then ?9/0 end if
end procedure


atom xZipCreateFile = NULL
global function ZipCreateFile(string filename)--, nullable_string password)
-- returns an atom handle or a string error message
-- (you may want to delete_file() first)
    if xZipCreateFile=NULL then
        if zip=NULL then zip_init() end if
        xZipCreateFile = link_c_func(zip,"ZipCreateFileA",{C_PTR,C_PTR,C_PTR},C_INT)
    end if
    atom pHZIP = allocate(machine_word(),true)
    integer res = c_func(xZipCreateFile,{pHZIP,filename,NULL})
    if res!=ZR_OK then
        return ZR_MESSAGES[res]
    end if
    atom hzip = peekNS(pHZIP,machine_word(),false)
    return hzip
end function

atom xZipAddFile = NULL
global function ZipAddFile(atom hzip, string dest, string src=dest)
-- dest is the (relative) path the zip should contain, src is the (full) path to the file
--  eg ZipAddFile(hz,`builtins\base64.e`,`C:\Program Files (x86)\Phix\builtins\base64.e`).
-- returns ZR_OK or a string error message
    if xZipAddFile=NULL then
        xZipAddFile = link_c_func(zip,"ZipAddFileA",{C_PTR,C_PTR,C_PTR},C_INT)
    end if
    integer res = c_func(xZipAddFile,{hzip,dest,src})
    if res!=ZR_OK then
        return ZR_MESSAGES[res]
    end if
    return ZR_OK
end function

atom xZipAddDir = NULL
global function ZipAddDir(atom hzip, string destname, integer offset=0)
-- offset specifes that part of destname that should be omitted from the zip, eg
--  ZipAddDir(hz,`test\LiteZip`,5) stores the files in a "LiteZip" subdir, so
--  in that specific case it will effectively omit destname[1..5], ie `test\`.
--  Of course destname can/may need to be a full rather than relative pathname.
-- returns ZR_OK or a string error message
    if xZipAddDir=NULL then
        xZipAddDir = link_c_func(zip,"ZipAddDirA",{C_PTR,C_PTR,C_INT},C_INT)
    end if
    -- we may as well verify things best we can:
    if get_file_type(destname)!=FILETYPE_DIRECTORY then ?9/0 end if
    if offset>0 and offset<length(destname) then
        integer ch = destname[offset]
        if not find(ch,`\/`) then ?9/0 end if
    end if
    integer res = c_func(xZipAddDir,{hzip,destname,offset})
    if res!=ZR_OK then
        return ZR_MESSAGES[res]
    end if
    return ZR_OK
end function

atom xZipAddFolder = NULL
global function ZipAddFolder(atom hzip, string zipname)
-- Adds an empty folder. There is no need for the directory to exist, so
--  you only need to provide the name that you want it to be stored as.
-- returns ZR_OK or a string error message
    if xZipAddFolder=NULL then
        xZipAddFolder = link_c_func(zip,"ZipAddFolderA",{C_PTR,C_PTR},C_INT)
    end if
    integer res = c_func(xZipAddFolder,{hzip,zipname})
    if res!=ZR_OK then
        return ZR_MESSAGES[res]
    end if
    return ZR_OK
end function

atom xZipClose = NULL
global procedure ZipClose(object hzip)
-- terminates in error on failure
    if sequence(hzip) then
        for i=1 to length(hzip) do
            atom hzi = hzip[i]  -- (ensure non-nested)
            ZipClose(hzi)
        end for
    else
        if xZipClose=NULL then
            xZipClose = link_c_func(zip,"ZipClose",{C_PTR},C_INT)
        end if
        integer res = c_func(xZipClose,{hzip})
        if res!=ZR_OK then ?9/0 end if
    end if
end procedure


