--
-- get_interpreter.e
--
--DEV incomplete, broken on 64 bit... (see edita/CDmessage)

--/*
sequence interpreter

-- returns a list of interpreters that exist on disk
function get_interpreters()
sequence paths, try, exe, bin, result = {}
--integer index

    string ext = get_file_extension(file_name)
    if find(ext, {"ew","exw"}) then
--      bin = {"euiw", "eui", "exw", "ex"}
        bin = {"pw", "p", "exw", "ex"}
    else
--      bin = {"eui", "euiw", "ex", "exw"}
        bin = {"p", "pw", "ex", "exw"}
    end if
    if platform()=LINUX then
--      bin = append(bin, "exu")
        bin = append(bin, "phix")
    end if
    paths = parse_eu_cfg(dirname(file_name) & SLASH & "eu.cfg")
    paths &= include_paths(0) -- fallback to running interpreter
    for i=1 to length(paths) do
        try = paths[i]
        if try[$]=SLASH then
            try = try[1..$-1]
        end if
        for j=1 to length(bin) do
--/**/      exe = try & "..\\bin\\"&bin[j]                  --/*
            exe = try & join_path({"..", "bin", bin[j]})    --*/
            if platform()=WINDOWS then
                exe &= ".exe"
            end if
            exe = canonical_path(exe)
            if not find(exe, result) and file_exists(exe) then
                result = append(result, exe)
            end if
        end for
    end for
    if length(interpreter) and not find(interpreter, result) then
        result = prepend(result, interpreter)
    end if
    return result
end function

            elsif equal(key, "interpreter") then
                if not find(val, {"eui","euiw","ex","exw"}) then
                    interpreter = val
                end if
--*/

function add_paths(sequence pathset, sequence paths)
    for i=1 to length(paths) do
        string path = paths[i]
        if not find(path,pathset) then
            pathset = append(pathset,path)
        end if
    end for
    return pathset
end function

--function validexe(string s, sequence vset)
--function validexe(string s, sequence vset, integer plat=platform())
function validexe(string s, sequence vset, integer plat=platform())
--function validexe(string s, integer plat=platform())
-- s is a filename such as "p.exe"
--sequence vset -- permitted filenames
integer nlen,n  -- (development versions)
--  if platform()=WINDOWS then
    if plat=WINDOWS then
--      vset = {"pw.exe","p.exe","pw64.exe","p64.exe","pth.exe"}
        {nlen,n} = {7,3}
    else -- platform()=LINUX
--      vset = {"phix","pth"}
        {nlen,n} = {5,5}
    end if
    if find(s,vset) then
        return 1
    elsif length(s)=nlen
      and find(s[n],"123456789") then
        -- Feel free to ignore this part
        -- (I use pw1.exe..pw9.exe when developing phix,
        --  and phix1..phix9 when doing so on Linux)
        s[n..n] = ""
        if s=vset[1] then
            return 1
        end if
    end if
    return 0
end function

--function ibits(string filepath)
function ibits(string filepath, integer plat=platform())
-- filepath should be a fully qualified filename, verfied by validexe().
-- returns 32 or 64 by examining low-level headers, or 0 if 
--  the file is corrupt, empty, or otherwise not recognised.
integer fn = open(filepath,"rb")
integer bits = 0
--  if platform()=WINDOWS then
    if plat=WINDOWS then
        if seek(fn,#80)=SEEK_OK then
            sequence s6 = {}
            for i=1 to 6 do
                s6 = append(s6,getc(fn))
            end for
            if s6="PE\0\0\x01\x4C" then
                bits = 32
            elsif s6="PE\0\0\x86\x64" then
                bits = 64
            end if
        end if
    else -- platform()=LINUX
        sequence s5 = {}
        for i=1 to 5 do
            s5 = append(s5,getc(fn))
        end for
        if s5="\x7FELF\x01" then
            bits = 32
        elsif s5="\x7FELF\x02" then
            bits = 64
        end if
    end if
    close(fn)
    return bits
end function

--DEV make this a standard builtin? (see also demo/PGUI/pdemo.exw and filedump.exw)

-- Note this routine should forever be considered experimental/work in progress.
-- Every situation/installation/platform is subtly different, and this routine
-- is expected to require more than its fair share of tweaks and adjustments.
-- It is however much better than starting from scratch every time you need
-- something along these lines.

function get_interpreter(integer mb=machine_bits(), integer plat=platform())
-- returns "" on failure
sequence vset   -- permitted filenames
string filepath
    sequence cl = command_line()
    string res = cl[1]
    string file = get_file_name(res)
--  -- <hack>
--  --  I make copies of pw.exe as pw2.exe..pw9.exe, so that my work on
--  --  Phix does not immediately clobber anything, or need all running
--  --  apps to be closed before compilation can complete. Typically, I
--  --  might have a few apps running with pw7.exe, fix something, then
--  --  create pw8.exe, and change any and all "Run With" to use that.
--  --  (ie Edita/Edix, menu/taskbar shortcuts, registry entries, etc.)
--  --  (Will also do ph1ix -> phix. A messed up file==no matter.)
--  if length(file)>3 then
--      integer ch = file[3]
--      if ch>='0' and ch<='9' then
--          file[3..3] = ""     -- eg/ie "pw7.exe" -> "pw.exe"
--      end if
--  end if
--  -- </hack>
--  if find(file,{"pw.exe","p.exe","phix"}) then
--      return res
--  end if
--  res = get_file_path(res,dropslash:=0)
--  integer k = match(iff(platform()=WINDOWS?"\\phix\\":"/phix/"),lower(res))
--  if k!=0 then
--      res = res[1..k+5]   -- eg/ie ../Phix/demo/pGUI/ -> ../Phix/
--  end if
    sequence cpaths = split_path(res)
    string crun = cpaths[$]
    cpaths = cpaths[1..$-1]
    cpaths = cpaths[1..find("demo",lower(cpaths))-1]
    res = join_path(cpaths,1)
--  if not validexe(crun) then
--      crun = "pw.exe"
--  end if

--
    if platform()=WINDOWS then
        vset = {"pw.exe","p.exe","pw64.exe","p64.exe","pth.exe"}
--      nlen,n = {7,3}
    else -- platform()=LINUX
        vset = {"phix","pth"}
--      nlen,n = {5,5}
    end if
    if validexe(crun,vset) then
        filepath = join_path({res,crun})
    else
        string maybe = ""
        sequence paths = {res}
        paths = add_paths(paths,{current_dir()})
--      if platform()=WINDOWS then
        if plat=WINDOWS then
            paths = add_paths(paths,{"C:\\Program Files (x86)\\Phix"})
            paths = add_paths(paths,{"C:\\Program Files\\Phix"})
            paths = add_paths(paths,split(getenv("PATH"),';'))
--          file = join_path({res,"pw.exe"})
--          if is_file(file) then return file end if
--          file = join_path({current_dir(),"pw.exe"})
--          if is_file(file) then return file end if
--          file = "C:\\Program Files (x86)\\Phix\\pw.exe"  -- 32 bit?
--          if is_file(file) then return file end if
--          file = "C:\\Program Files\\Phix\\pw.exe"        -- 64 bit?
--          if is_file(file) then return file end if
--          sequence paths = split(getenv("PATH"),';')
        else
--          file = join_path({res,"phix"})
--          if is_file(file) then return file end if
--          file = join_path({current_dir(),"phix"})
--          if is_file(file) then return file end if
            --DEV?? %HOME/phix/phix? ~/phix/phix? /usr/bin/phix/phix?
        end if
        for i=1 to length(paths) do
            for j=1 to length(vset) do
                filepath = join_path({paths[i],vset[i]})
                if file_exists(filepath) 
                and get_file_type(filepath)=FILETYPE_FILE then
                    maybe = filepath
--                  if ibits(filepath)=machine_bits() then
                    if ibits(filepath,plat)=mb then
                        exit
                    end if
                end if
            end for
        end for
        filepath = maybe
    end if
--DEV optional??
    if find(' ',filepath) then
        filepath = '\"' & filepath & '\"'
    end if
    return filepath
end function

