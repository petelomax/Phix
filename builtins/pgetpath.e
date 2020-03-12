--
--  pgetpath.e
--  ==========
--
-- Implements function get_proper_path(string filepath, object rootdir=0).
--  This is automatically included when needed; there should be no need 
--  to manually include this file, and in fact since it is used by both
--  current_dir() and command_line(), it will often be loaded already.
--
-- Converts eg c:\PROGRA~1\edita\EDITA.EXW
--          to C:\Program Files\Edita\edita.exw.
--
--  ie get rid of any short filenames (which are getting rare but will
--      probably always be around), and perhaps more importantly get the
--      actual case.
--
-- Usage:
--  actual = get_proper_path(filepath[, rootdir])
--
-- rootdir may be omitted, 0 or "", in which case if needed (ie when
--  filepath[2]!=':') it uses current_dir(). In truth, rootdir only 
--  exists because of the internal structure of edita.edb, so that 
--  vedb.exw could fix it, but it is quite likely someone will want 
--  a value other than current_dir() in some application. If you
--  already have a full path, just pass the one parameter.
--
-- Tested on Phix (0.5.9), RDS Eu 2.4 and 4.0, Windows 7 and XP.
--
--!/**/without debug
--
--/*
include machine.e
include dll.e
include sort.e
include misc.e
include file.e
--*/
--with trace

--include builtins\VM\pAlloc.e      -- allocate() [now in pHeap.e]
--include builtins\VM\pprntfN.e
--include builtins\pprntf.e     --DEV
--include builtins\VM\pHeap.e       -- allocate()

----/**/    include builtins\pcfunc.e   --DEV
--include builtins\VM\pcfunc.e
--/**/  include builtins\pcurrdir.e
--include builtins\VM\pcurrdirN.e [DEAD, so above should be fine]

--/**/include builtins\peekstr.e
--  filepaths = {`C:\Program Files (x86)\Phix\builtins\`,
--               `C:\Program Files (x86)\Phix\`}
--  filenames = {{1, "pgetpath.e"}, {1, "pcurrdir.e"}, {1, "pcfunc.e"},
--               {1, "pprntf.e"}, {1, "peekstr.e"}}
--

function cleanUpPath(sequence filepath, object rootdir)
integer k
    --
    -- First make sure there is a proper filepath.
    --
    if length(filepath)<2 or filepath[2]!=':' then
        if atom(rootdir) or length(rootdir)=0 then
            rootdir = current_dir()
        end if
        if rootdir[length(rootdir)]='\\'
        or length(filepath)=0 or filepath[1]='\\' then
            filepath = rootdir & filepath
        else
            filepath = rootdir & '\\' & filepath
        end if
    end if
    --
    -- Replace any / in filepath with \\
    --
    while 1 do
        k = find('/',filepath)
        if k=0 then exit end if
        filepath[k] = '\\'
    end while
    --
    -- check for and remove any \..\ in filepath
    --
    while 1 do
        k = match(`\..\`,filepath)
        if k=0 then exit end if
        for j=k-1 to 1 by -1 do
            if filepath[j]='\\' then
                -- remove `\xxx\..` (keeping filepath[k+3]==='\\')
--/**/          filepath[j..k+2] = ""                                           --/* -- Phix
                filepath = filepath[1..j-1] & filepath[k+3..length(filepath)]   --*/ -- RDS
                k = 0 -- signal found
                exit
            end if
        end for
        if k!=0 then
            puts(1,"Warning, cannot cleanup "&filepath&'\n')
--          ?9/0
            exit
        end if
    end while
    --
    -- repeat for any \.\
    --
    while 1 do
        k = match(`\.\`,filepath)
        if k=0 then exit end if
        -- remove `\.` (keeping filepath[k+2]==='\\')
--/**/  filepath[k..k+1] = ""                                           --/* -- Phix
        filepath = filepath[1..k-1] & filepath[k+2..length(filepath)]   --*/ -- RDS
    end while
    --
    -- repeat for any \\
    --
    while 1 do
        k = match(`\\`,filepath)
        if k=0 then exit end if
        -- remove 1st \ of 2
--/**/  filepath[k..k] = ""                                             --/* -- Phix
        filepath = filepath[1..k-1] & filepath[k+1..length(filepath)]   --*/ -- RDS
    end while

    return filepath
end function

function toString(sequence name)--, integer errcode)
-- Explicitly convert a dword-sequence to an 8-bit string
string res
integer nlen
object ch
    nlen = length(name)
    res = repeat(' ',nlen)
    for i=1 to nlen do
        ch = name[i]
        if atom(ch) then
            ch = and_bits(ch,#FF)
            res[i] = ch
        else
--          fatal(errcode)
            ?9/0
        end if
    end for
    return res
end function

atom kernel32
integer xGetLongPathName
integer xGetShortPathName
--integer xGetLastError
constant MAX_PATH = 260

integer gppinit
        gppinit = 0

-- (The following is because RDS Eu 2.4 does not support parameter defaults)
--DEV long gone?
-- (And I still want Edita to run on 2.4 as it is faster than OpenEu 4.0)
-- (Though if you really want it to be fast, run it on Phix!)

--/**/global function get_proper_path(sequence filepath, object rootdir=0)
--/*
global function get_proper_path(sequence filepath, object rootdir)
--*/
--
-- For an input filepath of eg c:\PROGRA~1\edita\EDITA.EXW,
--  returns eg C:\Program Files\Edita\edita.exw.
--  This routine was originally written so that edita does 
--  not accidentally open the same file twice. However, the
--  same processing ought to be performed for current_dir(),
--  and command_line().
--
-- Should work equally well for files and directories.
--  Of course the result is fairly meaningless if the 
--  the specified file or path does not exist, but if
--  you can open the input the same should be true for
--  the output.
--
-- rootdir may be omitted if filepath[2] is ':', or if 
--  current_dir() gives the correct directory, otherwise
--  specify the right directory to use. Specifically in
--  in the case of edita.edb/vedb.exw, filepath would be 
--  from T_files and rootdir from T_directories.
--
--/*
atom pFilePath
--*/
integer l
sequence res
atom buffer
    if not string(filepath) then
        filepath = toString(filepath)
    end if
--  if platform()=WINDOWS then
    if platform()=WIN32 then
        if not gppinit then
            --DEV locking as per pprntf.e
--/**/      enter_cs()
            kernel32 = open_dll("kernel32")
--#without reformat
            xGetLongPathName = define_c_func(kernel32,"GetLongPathNameA",
                {C_POINTER, --  LPCTSTR lpszShortPath // input
                 C_POINTER, --  LPTSTR lpszLongPath   // output
                 C_LONG},   --  DWORD cchBuffer // specifies size of *lpszLongPath
                C_INT)  -- DWORD
            xGetShortPathName = define_c_func(kernel32,"GetShortPathNameA",
                {C_POINTER, --  LPCTSTR lpszLongPath // input
                 C_POINTER, --  LPTSTR lpszShortPath   // output
                 C_LONG},   --  DWORD cchBuffer // specifies size of *lpszShortPath
                C_INT)  -- DWORD
--          xGetLastError = define_c_func(kernel32, "GetLastError",
--              {},
--              C_INT)      -- DWORD
--29/7/15:
--          buffer = allocate(MAX_PATH)
--#with reformat
            gppinit = 1
--/**/      leave_cs()
        end if
--puts(1,"get_proper_path, filepath:\n")
--pp(filepath)
        filepath = cleanUpPath(filepath,rootdir)
--pp(filepath)
--29/7/15:
        buffer = allocate(MAX_PATH)

--/**/  l = c_func(xGetShortPathName,{filepath,buffer,MAX_PATH}) --/* -- Phix
        pFilePath = allocate_string(filepath)                        -- RDS
        l = c_func(xGetLongPathName,{pFilePath,buffer,MAX_PATH})     -- RDS
        free(pFilePath)                                         --*/ -- RDS
        if l!=0 then
            filepath = peek_string(buffer)
        end if

--/**/  l = c_func(xGetLongPathName,{filepath,buffer,MAX_PATH}) --/* -- Phix
        pFilePath = allocate_string(filepath)                        -- RDS
        l = c_func(xGetLongPathName,{pFilePath,buffer,MAX_PATH})     -- RDS
        free(pFilePath)                                         --*/ -- RDS
        if l=0 then
--printf(1,"returning original, error code is %d\n",c_func(xGetLastError,{}))
            res = filepath
--10/1/18:
            for i=length(filepath) to 1 by -1 do
                if find(filepath[i],`\/`) then
                    res = get_proper_path(filepath[1..i-1])&filepath[i..$]
                    exit
                end if
            end for
        else
            res = peek_string(buffer)
        end if 
-- then ?0 ?c_func(xGetLastError,{}) poke(buffer,0) end if 
--pp(peek_string(buffer))
--      return peek_string(buffer)
        free(buffer)
--added 7/8/16: (no help with C:\Downloads)
--      for i=length(res) to 1 by -1 do
--          if res[i]='\\' then
--              res = get_proper_path(res[1..i-1])&res[i..$]
--              exit
--          end if
--      end for
    else -- linux
        --
        -- Replace any \ in filepath with /
        --
        while 1 do
            integer k = find('\\',filepath)
            if k=0 then exit end if
            filepath[k] = '/'
        end while

--all of this added 2/9/16:
        --
        -- Make sure there is a proper filepath.
        --
        if length(filepath)=0 or filepath[1]!='/' then
            if atom(rootdir) or length(rootdir)=0 then
                rootdir = current_dir()
            end if
            if rootdir[length(rootdir)]='/' then
                filepath = rootdir & filepath
            else
                filepath = rootdir & '/' & filepath
            end if
        end if

        --
        -- check for and remove any /../ in filepath
        --
        while 1 do
            integer k = match("/../",filepath)
            if k=0 then exit end if
            for j=k-1 to 1 by -1 do
                if filepath[j]='/' then
                    -- remove "/xxx/.." (keeping filepath[k+3]==='/')
--/**/              filepath[j..k+2] = ""                                           --/* -- Phix
                    filepath = filepath[1..j-1] & filepath[k+3..length(filepath)]   --*/ -- RDS
                    k = 0 -- signal found
                    exit
                end if
            end for
            if k!=0 then
                puts(1,"Warning, cannot cleanup "&filepath&'\n')
--              ?9/0
                exit
            end if
        end while
        --
        -- repeat for any /./
        --
        while 1 do
            integer k = match("/./",filepath)
            if k=0 then exit end if
            -- remove "/." (keeping filepath[k+2]==='/')
--/**/      filepath[k..k+1] = ""                                           --/* -- Phix
            filepath = filepath[1..k-1] & filepath[k+2..length(filepath)]   --*/ -- RDS
        end while
        --
        -- repeat for any //
        --
        while 1 do
            integer k = match("//",filepath)
            if k=0 then exit end if
            -- remove 1st "/" of 2
--/**/      filepath[k..k] = ""                                             --/* -- Phix
            filepath = filepath[1..k-1] & filepath[k+1..length(filepath)]   --*/ -- RDS
        end while

        res = filepath
    end if
    return res
end function

global function get_proper_dir(string filepath, bool remove_slash=false)
    filepath = get_proper_path(filepath,0)
    for i=length(filepath) to 1 by -1 do
        if find(filepath[i],`\/`) then
            filepath = filepath[1..i-remove_slash]
            exit
        end if
    end for
    return filepath
end function

--/* -- Defined in psym.e for Phix
constant CORRECT = 2
--*/

-- (sequence filepath should be fine, but needs testing and I'm not convinced that
--  get_proper_path deals well with dword_sequences/errors on nested subsequences)
global function canonical_path(string path_in, integer is_directory=0, integer case_flags=CORRECT)
    if is_directory then end if -- suppress warnings
    if case_flags!=CORRECT then ?9/0 end if
--  if case_flags!=2 then ?9/0 end if
    return get_proper_path(path_in,0)
end function

