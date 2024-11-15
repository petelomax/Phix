--
-- builtins\pfile.e (an autoinclude)
-- ================
--
--  Ripped from std/filesys.e, and a few other places
--
without debug
include file_utils.e

--#withtype bool

--atom finit = 0, W, xGetFileAttributes, xMoveFile, xDeleteFile, xCopyFile, 
atom finit = 0, W, xMoveFile, xDeleteFile, xCopyFile, 
     xCreateDirectory, xRemoveDirectory, xGetLogicalDriveStrings, xGetDriveType, 
     xCreateFileA, xSetFilePointer, xSetEndOfFile, xSetFileTime, xGetSystemTime,
     xSystemTimeToFileTime, xCloseHandle, xGetLastError,
     last_file_error -- (debug aid)
--,x40

--Windows only constants for MoveFileEx:
constant MOVEFILE_REPLACE_EXISTING  = #01,  -- (Not allowed for directories)
         MOVEFILE_COPY_ALLOWED      = #02,  -- (Simulate move to another volume)
         MOVEFILE_WRITE_THROUGH     = #08   -- (Only return once actually moved)

procedure initf()
    W = (platform()=WINDOWS)
--  SLASH = iff(W?'\\':'/')
--  atom lib = open_dll(iff(W?"kernel32":""))   -- libc.so? libc.dylib?
    enter_cs()
    atom lib = open_dll(iff(W?"kernel32":"libc.so"),false) -- DEV/take the safe option
--  xGetFileAttributes      = iff(W?define_c_func(lib, "GetFileAttributesA", {C_PTR}, C_INT)
----    xGetFileAttributes      = iff(W?define_c_func(lib, "GetFileAttributesExA", {C_PTR,C_INT,C_PTR}, C_INT)
--                                 :define_c_func(lib, "access", {C_PTR, C_INT}, C_INT))
    xMoveFile               = iff(W?define_c_func(lib, "MoveFileExA", {C_PTR, C_PTR, C_INT}, C_BOOL)
                                   :define_c_func(lib, "rename", {C_PTR, C_PTR}, C_INT))
    xDeleteFile             = iff(W?define_c_func(lib, "DeleteFileA", {C_PTR}, C_BOOL)
                                   :define_c_func(lib, "unlink", {C_PTR}, C_INT))
    xCopyFile               = iff(W?define_c_func(lib, "CopyFileA", {C_PTR, C_PTR, C_BOOL}, C_BOOL)
                                   :-1)             -- (done manually)
    xCreateDirectory        = iff(W?define_c_func(lib, "CreateDirectoryA", {C_PTR, C_PTR}, C_BOOL)
                                   :define_c_func(lib, "mkdir", {C_PTR, C_INT}, C_INT))
    xRemoveDirectory        = iff(W?define_c_func(lib, "RemoveDirectoryA", {C_PTR}, C_BOOL)
                                   :define_c_func(lib, "rmdir", {C_PTR}, C_INT))
    xGetLogicalDriveStrings = iff(W?define_c_func(lib, "GetLogicalDriveStringsA", {C_UINT,C_PTR}, C_UINT)
                                   :-1)             -- (just yield "\")
    xGetDriveType           = iff(W?define_c_func(lib, "GetDriveTypeA", {C_PTR}, C_UINT)
                                   :-1)             -- (just yield "\",DRIVE_FIXED)
    xCreateFileA            = iff(W?define_c_func(lib, "CreateFileA", {C_PTR,C_INT,C_INT,C_PTR,C_INT,C_INT,C_PTR}, C_PTR)
                                   :-1)             -- (all done by truncate, next)
    xSetFilePointer         = iff(W?define_c_func(lib, "SetFilePointer", {C_PTR, C_LONG, C_PTR, C_INT}, C_BOOL)
                                   :-1)             -- (all done by truncate, next)
    xSetEndOfFile           = iff(W?define_c_func(lib, "SetEndOfFile", {C_PTR}, C_BOOL)
                                   :define_c_func(lib, "truncate", {C_PTR, C_LONG}, C_BOOL))
    xGetSystemTime          = iff(W?define_c_proc(lib, "GetSystemTime", {C_PTR})
--                                 :define_c_func(lib, "time", {C_PTR}, C_LONG)
                                   :-1)
    xSystemTimeToFileTime   = iff(W?define_c_func(lib, "SystemTimeToFileTime", {C_PTR, C_PTR}, C_INT)
                                   :-1)
    xSetFileTime            = iff(W?define_c_func(lib, "SetFileTime",{C_PTR, C_PTR, C_PTR, C_PTR},C_INT)
                                   :define_c_func(lib, "utime", {C_PTR, C_PTR}, C_INT))
    xCloseHandle            = iff(W?define_c_func(lib, "CloseHandle", {C_PTR}, C_BOOL)
                                   :-1)             -- (not used)
    xGetLastError           = iff(W?define_c_func(lib, "GetLastError", {}, C_INT)
                                   :-1)             -- (windows only)
--x40 = allocate(40)
    leave_cs()
    finit = 1
end procedure

global function file_exists(string name)
-- (if you want to know that is is a file and not a directory, use 
--  file_exists(name) and get_file_type(name)=FILETYPE_FILE, or
--  just get_file_type(name)=FILETYPE_FILE)
--reverted to older method 24/3/21 (/pagefile.sys reported as does not exist - ???!!)
--24/3/22: (don't bother reading directory contents or retrieving any file details, return -1 or {} only)
--  object d = dir(name)
    object d = dir(name,NULL)
    return sequence(d)
/*
    if not finit then initf() end if
-- temp/debug
    integer res = iff(W?c_func(xGetFileAttributes, {name})
--  integer res = iff(W?c_func(xGetFileAttributes, {name,1,x40})
                       :c_func(xGetFileAttributes, {name, 0}))
?res
    return iff(W?res>0
                :res=0)
--  return iff(W?c_func(xGetFileAttributes, {name})>0
--              :c_func(xGetFileAttributes, {name, 0})=0)
*/
end function

--/* Now defined in psym.e:
global enum
    FILETYPE_UNDEFINED = -1,
    FILETYPE_NOT_FOUND =  0,
    FILETYPE_FILE      =  1,
    FILETYPE_DIRECTORY =  2
--*/

--object lastd = 0 (diag)

global function get_file_type(string filename)
    if find('*', filename) 
    or find('?', filename) then 
        return FILETYPE_UNDEFINED 
    end if

    if length(filename)=2 and filename[2]=':' then
        filename &= `\`
--3/10/21: (no help)
--  elsif find(filename[$],`\/`) then
--?filename
--      filename = filename[1..$-1]
    end if

    object d = dir(filename)
--if d!=lastd then
--  ?filename
--  ?shorten(d)
--  lastd = d
--end if
    if sequence(d)
    and length(d)>0 then
        --
        -- Note that a directory will get the full list (ugh),
        -- and the following tests the "." entry, or for top
        -- level directories, which have neither "." nor "..",
        -- at least on windows, that is.
        --
--10/6/24 (dir entries not in order...)
--      if find('d', d[1][2])
        if length(d)>1
        or (length(filename)=3 and filename[2]=':') then
            return FILETYPE_DIRECTORY
        else
            return FILETYPE_FILE
        end if
    else
        return FILETYPE_NOT_FOUND
    end if
end function

--global function get_file_size(string filename, bool asStringK=false, integer width=sw)
global function get_file_size(string filename, bool asStringK=false, integer width=1)
    -- (aside: directories get a length(d) of >=2, 
    --         ie "." and ".." and whatever else,
    --         and in that way this yields -1.)
    object d = dir(filename)
    if atom(d) or length(d)!=1 then return -1 end if
    atom size = d[1][D_SIZE]
    if asStringK then return file_size_k(size,width) end if
    return size
end function

global function set_file_size(string filename, atom size)

    object res = true
    if not finit then initf() end if
       
    if get_file_type(filename)!=FILETYPE_FILE then return "not found" end if

    if platform()=WINDOWS then

        atom GENERIC_WRITE = #40000000,
             INVALID_HANDLE_VALUE = #FFFFFFFF
        integer OPEN_EXISTING = 3,
                FILE_BEGIN = 0
        atom fh = c_func(xCreateFileA,{filename, GENERIC_WRITE, 0, NULL, OPEN_EXISTING, 0, NULL})
        if (fh == INVALID_HANDLE_VALUE) then return "cannot open" end if
        if c_func(xSetFilePointer,{fh, size, NULL, FILE_BEGIN})=0
        or c_func(xSetEndOfFile,{fh})=0 then res = "cannot set size" end if
        {} = c_func(xCloseHandle,{fh})

    elsif platform()=LINUX then

        res = not c_func(xSetEndOfFile,{filename, size})

    else

        ?9/0    -- unknown platform

    end if

    return res
end function

global function get_file_date(string filename, integer date_type=D_MODIFICATION)
-- returns last modification date, in [DT_YEAR..DT_SECOND] format, 
--          or -1 if the file details could not be retrieved.
    object d = dir(filename,date_type)
    if atom(d) or length(d)!=1 then return -1 end if
    sequence res = d[1][D_YEAR..D_SECOND]
    return res
end function

global function set_file_date(string filename, integer date_type=D_MODIFICATION)
-- sets the specified date_type to the current date and time.
    object res = true
    if not finit then initf() end if
    if get_file_type(filename)!=FILETYPE_FILE then return "not found" end if

    if platform()=WINDOWS then

        atom GENERIC_WRITE = #40000000,
             INVALID_HANDLE_VALUE = #FFFFFFFF
        integer FILE_SHARE_READ = 1,
                OPEN_EXISTING = 3,
                sizeofFILETIME = 8,
                sizeofSYSTEMTIME = 16

        atom fh = c_func(xCreateFileA,{filename, GENERIC_WRITE, FILE_SHARE_READ, NULL, OPEN_EXISTING, 0, NULL})
        if (fh == INVALID_HANDLE_VALUE) then
            res = "cannot open"
        else
            atom pFileTime = allocate(sizeofFILETIME),
                 pSystemTime = allocate(sizeofSYSTEMTIME)
            c_proc(xGetSystemTime,{pSystemTime})
            if c_func(xSystemTimeToFileTime,{pSystemTime,pFileTime})=0 then
                res = "SystemTimeToFileTime failed"
            else
                sequence args = {fh,NULL,NULL,NULL}
                args[date_type+1] = pFileTime
                if c_func(xSetFileTime,args)=0 then
                    res = "SetFileTime failed"
                end if
            end if
            {} = c_func(xCloseHandle,{fh})
            free(pFileTime)
            free(pSystemTime)
        end if

    elsif platform()=LINUX then

        --note: this sets access and modification times to the current time.
--?9/0
--      if date_type=D_CREATION then ?9/0 end if    -- not supported
--      atom pTimeBuf = allocate(16),
----             pTime = pTimeBuf+iff(date_type=D_LASTACCESS?0:8)
--           pTime = pTimeBuf+{{},0,8}[date_type]
--      {} = c_func(xGetSystemTime,{pTime}) -- time()
----        res = not c_func(xSetEndOfFile,{filename, size})
--      free(pTimeBuf)
        res = not c_func(xSetFileTime,{filename,NULL})
        -- (obviously, errors for non-existing/read-only/no-write-permission files)

    else

        ?9/0    -- unknown platform

    end if
    return res
end function

--/* Now defined in psym.e:
global constant DRIVE_UNKNOWN       = 0,    -- The drive type cannot be determined.
                DRIVE_NO_ROOT_DIR   = 1,    -- The root path is invalid; for example, there is no volume mounted at the specified path.
                DRIVE_REMOVABLE     = 2,    -- The drive has removable media; for example, a floppy drive, thumb drive, or flash card reader.
                DRIVE_FIXED         = 3,    -- The drive has fixed media; for example, a hard disk drive or flash drive.
                DRIVE_REMOTE        = 4,    -- The drive is a remote (network) drive.
                DRIVE_CDROM         = 5,    -- The drive is a CD-ROM drive.
                DRIVE_RAMDISK       = 6     -- The drive is a RAM disk.
--*/ 

global function get_logical_drives(integer drive_type=-1)
--(suggestions for enhancements to this routine are welcome)
    sequence res
    if platform()=WINDOWS then
        if not finit then initf() end if
        res = {}
        integer buflen = c_func(xGetLogicalDriveStrings,{0,NULL})
        atom buffer = allocate(buflen), pbuf = buffer
        {} = c_func(xGetLogicalDriveStrings,{buflen,buffer})
        while 1 do
            string onedrive = peek_string(pbuf)
            buflen = length(onedrive)
            if buflen=0 then exit end if
            integer drivetype = c_func(xGetDriveType,{onedrive})
            if drive_type=-1 or drivetype=drive_type then
                res = append(res,{onedrive,drivetype})
            end if
            pbuf += buflen+1  -- skip trailing/separating nulls
        end while
        free(buffer)
    else
        res = {{"/",DRIVE_FIXED}}
    end if
    return res
end function

global function rename_file(string src, string dest, bool overwrite=false)
    atom ret
    if not finit then initf() end if
    if not overwrite then
        if file_exists(dest) then
            return 0
        end if
    end if

--  src = get_proper_path(src)
    -- Remove any trailing slash.
    if src[$]=SLASH then
        src = src[1..$-1]
    end if

--  dest = get_proper_path(dest)
    -- Remove any trailing slash.
    if dest[$]=SLASH then
        dest = dest[1..$-1]
    end if

    if platform()=WINDOWS then

        integer flags = or_bits(MOVEFILE_COPY_ALLOWED,MOVEFILE_WRITE_THROUGH)
        if get_file_type(src)!=FILETYPE_DIRECTORY then
            flags = or_bits(flags,MOVEFILE_REPLACE_EXISTING)
        end if
        ret = c_func(xMoveFile, {src, dest, flags})

    elsif platform()=LINUX then

        ret = not c_func(xMoveFile, {src, dest})

    else

        ?9/0 -- unknown platform

    end if

    return ret
end function

--11/9/19:
--global function copy_file(string src, dest, bool overwrite=false)
local function copy_one_file(string src, dest, bool overwrite=false)
    integer success
--  if not finit then initf() end if

    if get_file_type(src)!=FILETYPE_FILE then return false end if

    if length(dest)=0 then ?9/0 end if

    if file_type(dest)=FILETYPE_DIRECTORY then
        if dest[$]!=SLASH then
            dest &= SLASH
        end if
        dest &= get_file_name(src)
    end if

    if platform()=WINDOWS then
        success = c_func(xCopyFile, {src, dest, not overwrite})
--      if success=0 then ?{"copy_one_file",c_func(xGetLastError,{})} end if
        if success=0 then last_file_error = c_func(xGetLastError,{}) end if
    else
        success = 0
        if file_exists(src) then
            if overwrite or not file_exists(dest) then
                integer infd = open(src, "rb")
                if infd!=-1 then
                    integer outfd = open(dest, "wb")
                    if outfd!=-1 then
                        while 1 do
                            integer byte = getc(infd)
                            if byte=-1 then exit end if
                            puts(outfd, byte)
                        end while
                        success = 1
                        close(outfd)
                    end if
                    close(infd)
                end if
            end if
        end if
    end if
    return success
end function

global function move_file(string src, dest, bool overwrite=false)
    atom ret

    if not file_exists(src)
    or (not overwrite and file_exists(dest)) then
        return false
    end if

    if platform()=WINDOWS then

        integer flags = or_bits(MOVEFILE_COPY_ALLOWED,MOVEFILE_WRITE_THROUGH)
        if get_file_type(src)!=FILETYPE_DIRECTORY then
            flags = or_bits(flags,MOVEFILE_REPLACE_EXISTING)
        end if
        ret = c_func(xMoveFile, {src, dest, flags})

--if ret=0 then
--  if platform()=WINDOWS then
--      ?{"error (ret=0)",c_func(xGetLastError,{})}
--  end if
--end if

    elsif platform()=LINUX then

        if not equal(get_proper_dir(src),get_proper_dir(dest)) then
--          ret = copy_file(src, dest, overwrite)
            ret = copy_one_file(src, dest, overwrite)
            if ret then
                ret = delete_file(src)
            end if
            return (not ret)
        end if
        ret = not c_func(xMoveFile, {src, dest})

    else

        ?9/0    -- unknown platform

    end if

    return ret
end function

global function delete_file(string filename)

    if not finit then initf() end if
    integer res = c_func(xDeleteFile, {filename})
    if platform()=LINUX then
        res = not res
    end if
    return res
end function

global function create_directory(string name, integer mode=0o700, bool make_parent=true)

    if not finit then initf() end if

    assert(length(name)!=0)
    name = get_proper_path(name)

    -- Remove any trailing slash.
    if name[$]=SLASH then
        name = name[1..$-1]
    end if

    if make_parent then
        integer pos = rfind(SLASH, name)
        if pos!=0 then
            string parent = name[1..pos-1]
            if file_exists(parent) then
                assert(file_type(parent)==FILETYPE_DIRECTORY)
            else
                if not create_directory(parent, mode, make_parent) then
                    return 0
                end if
            end if
        end if
    end if

    bool ret
    if platform()=LINUX then

        ret = not c_func(xCreateDirectory, {name, mode})

    elsif platform()=WINDOWS then

        ret = c_func(xCreateDirectory, {name, 0})

    end if

    return ret
end function

global function clear_directory(string path, bool recurse=true)

    if not finit then initf() end if

    if length(path)>0
    and path[$]=SLASH then
        path = path[1..$-1]
    end if

    if length(path)=0 then
        return 0 -- Nothing specified to clear. Not safe to assume anything.
                         -- (btw, not allowed to clear root directory)
    end if

    if platform()=WINDOWS then
        if length(path)=2
        and path[2]=':' then
            return 0 -- nothing specified to delete
        end if
    end if

    object d = dir(path)
    if atom(d) then
        return 0
    end if

    if platform()=WINDOWS then
        if length(d)<3
        or not equal(d[1][D_NAME], ".")
        or not find('d', d[1][D_ATTRIBUTES]) then
            return 0 -- Supplied name was not a directory
        end if
    else      -- LINUX
        if length(d)<2 then
            return 0 -- not a directory
        end if
    end if

    path &= SLASH

    for i=1 to length(d) do
        string name = d[i][D_NAME]
--24/3/22
--      if platform()=WINDOWS then
--          if find(name,{".",".."}) then
--              continue
--          end if
--      elsif platform()=LINUX then
--          if name[1]='.' then
--              continue
--          end if
--      end if
        bool bClear = iff((platform()=WINDOWS) ? find(name,{".",".."})=0
                                     /*LINUX*/ : name[1]!='.')
        if bClear then
            if find('d', d[i][D_ATTRIBUTES]) then
                if recurse then
                    if clear_directory(path&name, recurse)=0 then
                        return 0
                    end if
                end if
            else
                if delete_file(path&name)=0 then
                    return 0
                end if
            end if
        end if
    end for
    return 1
end function

--global function copy_directory(string src, dest, bool overwrite=false)
global function copy_file(string src, dest, bool overwrite=false)
    if not finit then initf() end if
    integer st = get_file_type(src),
            dt = get_file_type(dest)
    if st=FILETYPE_FILE then return copy_one_file(src,dest,overwrite) end if
    if st!=FILETYPE_DIRECTORY then return false end if
    if dt==FILETYPE_FILE then return false end if
    if dt!=FILETYPE_DIRECTORY then
        -- note: FILETYPE_UNDEFINED is deliberately barred
        if dt!=FILETYPE_NOT_FOUND then return false end if
        if not create_directory(dest) then return false end if
    end if
    sequence d = dir(src)
    for i=1 to length(d) do
        sequence di = d[i]
        string name = di[D_NAME]
        if not find(name,{".",".."}) then
            string src_path = join_path({src,name}),
                   dest_path = join_path({dest,name})
            if find('d',di[D_ATTRIBUTES]) then
--              if not copy_directory(src_path,dest_path) then return false end if
                if not copy_file(src_path,dest_path,overwrite) then return false end if
            else
--              if not copy_file(src_path,dest_path) then return false end if
                if not copy_one_file(src_path,dest_path,overwrite) then return false end if
            end if
        end if
    end for
    return true
end function

global function copy_directory(string src, dest, bool overwrite=false)
    if not finit then initf() end if
    if get_file_type(src)!=FILETYPE_DIRECTORY then return false end if
    return copy_file(src,dest,overwrite)
end function

global function remove_directory(string dir_name, bool force=false)
    atom ret
    object files

    if not finit then initf() end if

    if length(dir_name)>0
    and dir_name[$]=SLASH then
        dir_name = dir_name[1..$-1]
    end if

    if length(dir_name)=0 then
        return 0    -- nothing specified to delete.
                    -- (not allowed to delete root directory btw)
    end if

    if platform()=WINDOWS then
        if length(dir_name)=2 then
            if dir_name[2]=':' then
                return 0 -- nothing specified to delete
            end if
        end if
    end if

    files = dir(dir_name)
    if atom(files)
    or not equal(files[1][D_NAME], ".") then
        return 0
    end if

    dir_name &= SLASH

    for i=1 to length(files) do
        if not find(files[i][D_NAME], {".", ".."}) then
            if not force then
                return 0
            else
                if find('d', files[i][D_ATTRIBUTES]) then
                    ret = remove_directory(dir_name & files[i][D_NAME] & SLASH, force)
                else
                    ret = delete_file(dir_name & files[i][D_NAME])
                end if
                if not ret then
                    return 0
                end if
            end if
        end if
    end for

    ret = c_func(xRemoveDirectory, {dir_name})
    if platform()=LINUX then
        ret = not ret
    end if
    return ret
end function

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
    integer lend = i-1
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

global function get_text(object file, integer options=GT_WHOLE_FILE)
--18/12/20: (final \n)
--  integer fn = iff(string(file)?open(file,"rb"):file)
--27/12/20: (GT_BINARY option)
--  integer fn = iff(string(file)?open(file,"r"):file)
    string mode = iff(and_bits(options,GT_BINARY)?"rb":"r")
    options = and_bits(options,#0F) -- (kill any GT_BINARY flag, string file or otherwise)
    integer fn = iff(string(file)?open(file,mode):file)
    if fn=-1 then return -1 end if
--(may not need to do this...)
--  bool keep_bom = false
--  if options>6 then
--  if and_bits(options,GT_KEEP_BOM) then
--      keep_bom = true
--      options -= GT_KEEP_BOM
--  end if
--  sequence res = get_text(fn,options)
--  sequence res = get_text(fn,and_bits(options,0b111))
--  sequence res = get_textn(fn,options)
    --
    -- note that get_textn() is responsible for any final \n placement (pre-split),
    --      aka anything and everything concerning GT_LF_LEFT vs. GT_LF_LAST, plus
    --      GT_WHOLE_FILE handling when the file arg was pre-opened in text mode.
    --
    sequence res = get_textn(fn,options+10) -- (temp)
--  sequence res = get_text(fn,options+10)  -- (temp)
    if string(file) then close(fn) end if
--... [DEV] and move what we can from pfileioN.e to here...
    if options<8    -- ie not +GT_KEEP_BOM
--  if not keep_bom
--  if not and_bits(options,GT_KEEP_BOM)
    and length(res)>=3
    and res[1..3] = x"EFBBBF" then
        res = res[4..$]
    end if
    if options!=GT_WHOLE_FILE 
    and string(res) then
        string src = res
        res = {}
        integer i = 1,
                start = 1,
                filesize = length(src)
        while i<=filesize do
            integer ch = src[i]
            if ch='\r' or ch='\n' then
                res = addline(res,i,start,options,filesize,src)
--DEV tryme
--              res = append(res,addline(i,start,option,filesize,src))
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
            res = addline(res,i,start,options,filesize,src)
        end if
    end if

    return res
end function

global function temp_file(string location = "", prefix = "", extn = "tmp", open_mode="")
    if not finit then initf() end if
    if length(location)=0 then
        object envtmp = getenv("TEMP")
        if atom(envtmp) then
            envtmp = getenv("TMP")
            if atom(envtmp) then
                envtmp = iff(W?`C:\temp\`:"/tmp/")
            end if
        end if
        location = envtmp
    else
        location = get_proper_path(location)
    end if
    if not file_exists(location)
    and not create_directory(location) then
        crash("could not create temp directory %s",{location})
    end if
    object res          -- string or {integer,string}
    while true do
        string randname = join_path({location, sprintf("%s%06d.%s",{prefix,rand(1_000_000)-1, extn})})
        if not file_exists(randname) then
            if length(open_mode)=0 then
                res = randname
                exit
            end if
            if not find(open_mode,{"w","wb","a","ab"}) then crash("invalid open mode") end if
            integer fn = open(randname,open_mode)
            if fn!=-1 then
                res = {fn,randname}
                exit
            end if
        end if
    end while
    return res
end function

--/*
--SUG: (replaces/removes read_file.e from distro) [ERM, no, use get_text()]
--global constant BINARY_MODE = 1, TEXT_MODE = 2    -- now in psym.e [**NOT YET!**]

--global 
function read_file(object file, integer as_text = BINARY_MODE)
    string text = get_text(file)
    if as_text=TEXT_MODE then
        text = substitute(text,"\r","")
    end if
--integer fn
--object line
--string text = ""
--  if sequence(file) then
--      fn = open(file,iff(as_text=BINARY_MODE?"rb":"r"))
--  else
--      fn = file
--  end if
--  if fn = -1 then return -1 end if
--  while 1 do
--      line = gets(fn)
--      if atom(line) then exit end if
--      text &= line
--  end while
--  if sequence(file) then
--      close(fn)    
--  end if
    return text
end function
--*/

global function read_lines(object file)
--  integer fn = iff(string(file)?open(file,"r"):file)
--  if fn<0 then return -1 end if
--  sequence lines = get_text(fn,GT_LF_STRIPPED)
--  if string(file) then
--      close(fn)
--  end if
--  return lines
    return get_text(file,GT_LF_STRIPPED)
end function

--**
-- Write a sequence of lines to a file.
--
-- Parameters:
--      # ##file## : an object, either a file path or the handle to an open file.
--      # ##lines## : the sequence of lines to write
--
-- Returns:
--     An **integer**, 1 on success, -1 on failure.
--
-- Errors:
--      If [[:puts]]() cannot write some line of text, a runtime error will occur.
--
-- Comments:
--  If ##file## was a sequence, the file will be closed on completion. Otherwise, it will remain open, but at end of file.
--
-- Whatever integer the lines in ##lines## holds will be truncated to its 8 lowest bits so as to fall in the 0..255 range.
--
-- Example 1:
-- <eucode>
-- if write_lines("data.txt", {"This is important data", "Goodbye"}) != -1 then
--     puts(STDERR, "Failed to write data\n")
-- end if
-- </eucode>
--
-- See Also:
--     [[:read_lines]], [[:write_file]], [[:puts]]

global function write_lines(object file, sequence lines)
--global function write_file(object file, sequence lines)   -- NO!
    integer fn = iif(string(file)?open(file, "w"):file)
    if fn<0 then return false end if

    if string(lines) then
        puts(fn, lines)
    else
        for i=1 to length(lines) do
            string li = lines[i] -- (deliberate typecheck)
            puts(fn, li)
            puts(fn, '\n')
        end for
    end if

    if string(file) then
        close(fn)
    end if

    return true
end function


--/* SUG: (done better...)
String getContentType(String filename){
  if(filename.endsWith(".htm")) return "text/html";
  else if(filename.endsWith(".html")) return "text/html";
  else if(filename.endsWith(".css")) return "text/css";
  else if(filename.endsWith(".js")) return "application/javascript";
  else if(filename.endsWith(".png")) return "image/png";
  else if(filename.endsWith(".gif")) return "image/gif";
  else if(filename.endsWith(".jpg")) return "image/jpeg";
  else if(filename.endsWith(".ico")) return "image/x-icon";
  else if(filename.endsWith(".xml")) return "text/xml";
  else if(filename.endsWith(".pdf")) return "application/x-pdf";
  else if(filename.endsWith(".zip")) return "application/x-zip";
  else if(filename.endsWith(".gz")) return "application/x-gzip";
  return "text/plain";
}
--*/
