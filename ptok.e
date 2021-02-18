--
-- ptok.e
--
-- File read and tokeniser.
--
----/**/without debug   -- no gain
without trace

integer fn
        fn = -1

--procedure loadFile()
---- load a single source into memory.
---- fn should be set on entry to an open filename (as found by
---- scanning EUDIR,EUINC, etc). It is closed on exit.
--
----/* RDS compatible code:
--integer textlen
--integer lllen
--object line
--string lastline
--
--  text = {}
--  while 1 do
--      line = gets(fn)
--      if atom(line) then exit end if
--      text = append(text,line)
--  end while
--  textlen = length(text)
--  if textlen then
--      lastline=text[textlen]
--      lllen=length(lastline)
--      if lllen=0 or lastline[lllen]!='\n' then
--          text[textlen]=append(lastline,'\n')
--      end if
--  end if
----*/
--  close(fn)
--  fn = -1
--end procedure

global --DEV 1/11/09 for psym.e
integer ltl         -- length(text)
        ltl = -1

procedure loadFile()
string msg
--
-- Load a single source into memory.
-- fn should be set on entry to an open filename (as found by
-- scanning EUDIR,EUINC, etc). It is closed on exit.
--

    if not minus_e then
        text = get_text(fn,GT_WHOLE_FILE)
    end if
    ltl = length(text)

-- 01/08/2013:
    col = 0
    if ltl>=3 and text[1..3]={#EF,#BB,#BF} then
        col += 3
    elsif ltl>=2 and find(text[1..2],{{#FF,#FE},{#FE,#FF}}) then
        msg = "UTF16 source files are not supported"
        if length(allfiles)=0 then
            -- (no source line to report against)
            Fatal(msg)
--          abort(1)    -- Fatal() does not return
        else
            Abort(msg)
        end if
    end if

    close(fn)
    fn = -1
end procedure

--
-- when I include misc\file.e, and file.e just includes file2.e, then I want it to
-- have a look in misc\. So we build a table of paths to search and extend it from
-- time to time.
--
--constant usegpp = 01
-- if usegpp:
--include builtins\peekstr.e
--include builtins\pgetpath.e

--DEV no longer quite sure what these do. I think the plan was that in eg:
--      main.e:
--          include arwen\arwen.ew
--          include thing.e
--      arwen\arwen.ew:
--          include misc_arwen.e (etc)
-- Then (obviously) it would look for the sub-include misc_arwen.e in the 
--  "active" path "arwen\" (which it does), but at EOF of arwen.ew, that 
--  would become "inactive" (no evidence of that I can find) and it would
--  NOT look for thing.e in "arwen\" (but it DOES). OK, this is a BUG.
-- (posted on the bugs forum 18/09/2010, asking for volunteers)

sequence activepaths
global integer alwaysactive         -- used in stripPathInfo() in pemit.e
               alwaysactive = 0


integer lastPath = 0
procedure addPath(sequence path)
--  if not usegpp then
--      path = lower(path)
--  end if
--DEV (temp)
    if platform()=LINUX then
        if match("./",path)=1 then
--          path = "/home/pete/phix/"&path[3..$]
            path = getenv("HOME")&"/phix/"&path[3..$]
--          path = mainpath&path[3..$]
--          path = current_dir()&SLASH&path[3..$]
--          if testall then
--              path = mainpath[1..-6]&path[3..$]
--          else
--              path = mainpath&path[3..$]
--          end if
        end if
    end if
--  if not find(path,filepaths) then
    lastPath = find(path,filepaths)
    if lastPath=0 then
        filepaths = append(filepaths,path)
        lastPath = length(filepaths)
        activepaths = append(activepaths,1)
    end if
end procedure

--with trace

--/**/-- not strictly necessary, but reduces opCallOnce/fwd calls/onDeclaration
--/**/include penv.e -- getenv()
--/**/include pdir.e -- dir()
--newEmit
--!/**/include psprint.e -- "?" --[DEV] ["VM\\psprintN.e"]

integer vmpath = 0

global procedure initFilePathSet()
object incpath
integer semicolon, sm1
    filepaths = {}
    activepaths = {}
    incpath = getenv("EUDIR")
    if not atom(incpath) then
        incpath = get_proper_path(incpath&SLASH&"include"&SLASH,"")
        addPath(incpath)
    end if
    incpath = getenv("EUINC")
    if not atom(incpath) then
        while 1 do
            semicolon = find(';',incpath)
            if semicolon=1 then
                -- skip nulls (ie/eg ;;) in EUINC
            elsif semicolon then
                sm1 = semicolon-1
                if incpath[sm1]!=SLASH then
                    incpath[semicolon] = SLASH
                    sm1 = semicolon
                end if
                addPath(get_proper_path(incpath[1..sm1],""))
            else
                if incpath[length(incpath)]!=SLASH then
                    incpath &= SLASH
                end if
                incpath = get_proper_path(incpath,"")
                addPath(incpath)
                exit
            end if
            incpath = incpath[semicolon+1..length(incpath)]
            if not length(incpath) then exit end if
        end while
    end if
    addPath(rootpath&"builtins"&SLASH)
--DEV
    addPath(rootpath&"builtins"&SLASH&"VM"&SLASH)   -- (added 8/4/15)
--  vmpath = length(filepaths)
--  vmpath = find(rootpath&"builtins"&SLASH&"VM"&SLASH,filepaths)
    vmpath = lastPath
    addPath(rootpath)
    addPath(mainpath)
    alwaysactive = length(filepaths)
--  activepaths = repeat(1,alwaysactive)
--printf(1,"initFilePathSet: ")
--?activepaths

    allfiles = {}
    exptext = {}
    expandedYet = {}
    unresolved_routine_ids = {}
    finalOptWarn = {}
    filenames = {}

end procedure

-- Files which should have a "Phix compatible" marker:
constant phixcompat = { "win32lib.ew",
                        "arwen.ew",
                        "database.e",
                        "dll.e",
                        "file.e",
                        "get.e",
                        "graphics.e",
                        "image.e",
                        "machine.e",
                        "misc.e",
                        "msgbox.e",
                        "pcfunc.e",
                        "pcfileio3.e",
                        "prtnid.e",
                        "ppp.e",
                        "syswait.ew",
                        "wildcard.e"
                      }
--DEV
--C:\euphoria\include\std\console.e:46   kernel_dll = machine_func(M_OPEN_DLL, "kernel32.dll"),
--C:\euphoria\include\std\convert.e:20 atom mem  = machine_func(M_ALLOC,4)
--C:\euphoria\include\std\filesys.e:304 return machine_func(M_DIR, name)
--C:\euphoria\include\std\graphcst.e:215    return machine_func(M_VIDEO_CONFIG, 0)
--C:\euphoria\include\std\io.e:849  return machine_func(M_SEEK, {fn, pos})
--C:\euphoria\include\std\os.e:75  return machine_func(M_INSTANCE, 0)
--C:\euphoria\include\std\rand.e:241    return machine_func(M_GET_RAND, {})
--C:\euphoria\include\std\regex.e:642  return machine_func(M_PCRE_COMPILE, { pattern, options })
--C:\euphoria\include\std\safe.e:298     machine_func(M_SLEEP,0.1)
--C:\euphoria\include\std\socket.e:49  return machine_func(M_SOCK_ERROR_CODE, {})


-- Files which should be checked for auto-include completeness:
constant vctrl = { {"pcfunc.e",{0,6,3}},
                   {"pcfileio3.e",{0,6,3}},
                   {"prtnid.e",{0,6,3}},
                  $}

sequence vmset = {}
--integer FincMax = 0

--DEV/temp:
--global function getFincMax()
--  return FincMax
--end function

--without warning -- needs to be around the definition
--forward global procedure AddFincParent() -- (defined in psym.e)
--with warning

global procedure Finc(string file)
-- Fake include. Pretends a builtins\VM file has already been loaded.
-- Note that an "include builtins\VM\xx.e" will actually open the file,
-- but then close it and return -fileno, rather than read/compile it.
--string text
--  text = "via optable"
--  allfiles = append(allfiles,text)
--  allpfiles = append(allpfiles,text)
--  exptext = append(exptext,0)
--  expandedYet = append(expandedYet,0)
--  unresolved_routine_ids = append(unresolved_routine_ids,0)
--  finalOptWarn = append(finalOptWarn,optset[OptWarning])
--  filenames = append(filenames,{vmpath,file})
--  FincMax = length(filenames)
--  AddFincParent()
-- ah, this already done in optable...
--if 01 then
--  if find(file,{"pHeap.e","pStack.e"}) then
--      file[-2..$] = "D.e"
--  end if
--end if
    if not find(file,vmset) then
        vmset = append(vmset,file)
    end if
end procedure

forward global procedure eof_processing()

--without trace
--with trace
include ppp.e

--integer icount = 0

--with trace
global procedure includeFile(string file, integer autoInclude, integer emitcol)
integer pathno, k, km1, kp1, kp2, kp3
object path
string thispath
sequence triedpaths
integer semicolon, sm1
string fatalmsg
sequence inpart
integer wasfileno
string msg
integer wline,wcol
integer ch
object cversion
--integer dbg
string thispath2

    fn = -1
if showfileprogress then
    puts(1,"includeFile started\n")
    ?file
end if
--if file="VM\\pcallfunc.e" then
--  icount += 1
--  if icount>2 then
--      ?9/0
--  end if
--else
--  icount = 0
--end if

    while 1 do
        k = find(WRONGSLASH,file)
        if not k then exit end if
        file[k] = SLASH
    end while
--ffs: it works if I debug it!!
--if file = "./test/t02parms.exw" then
--  ?"file = \"./test/t02parms.exw\""
--end if

--DEV (temp)
    if platform()=LINUX then
        if match("./",file)=1 then
--          file = "/home/pete/phix/"&file[3..$]
            file = getenv("HOME")&"/phix/"&file[3..$]
--          file = mainpath&file[3..$]
--          file = current_dir()&SLASH&file[3..$]
--          if testall then
--              file = mainpath[1..-6]&file[3..$]
--          else
--              file = mainpath&file[3..$]
--          end if
        end if
    end if

--dbg = 0
if platform()=WINDOWS then
    while 1 do
        k = match("\\\\",file)
        if k=0 then exit end if
--if dbg=0 then
--  dbg=1
---- ?file
----puts(1,"includeFile thing\n")
--end if
        file = file[1..k]&file[k+2..length(file)]
--?{file}
    end while
--elsif platform()=LINUX then
--  if length(file) and file[$]='\n' then file = file[1..$-1] end if
elsif platform()=LINUX then
    while 1 do
        k = match("//",file)
        if k=0 then exit end if
        file = file[1..k]&file[k+2..length(file)]
    end while
end if
--if dbg then
--puts(1,"match \\\\'d\n")
--end if
    triedpaths = {}
    if (platform()=WINDOWS and length(file)>1 and file[2]=':')
    or (platform()=LINUX and length(file)>=1 and file[1]='/')
    or (platform()=LINUX and length(file)>=2 and file[1..2]="./") then
if showfileprogress then
    ?"rooted"
end if
--      fn = open(file,"rb")
        fn = open(file,"r")
        thispath = ""
    elsif minus_e then
        thispath = ""
    else
        for i=length(filepaths) to 1 by -1 do
            if i<=alwaysactive
--26/7/17: (undone before testing, changed activepaths setup instead...)
            or (activepaths[i] and not autoInclude) then
--          or (i<=length(activepaths) and activepaths[i] and not autoInclude) then
                thispath = filepaths[i]
--              fn = open(thispath&file,"rb")
                fn = open(thispath&file,"r")
                if fn!=-1 then exit end if
                triedpaths = append(triedpaths,thispath)
            end if
        end for
        -- if this is top level file, search PATH.
--DEV or everything in allfiles is via Finc()...
        if fn=-1 and length(allfiles)=0 then
--      if fn=-1 and length(allfiles)=FincMax then
            path = getenv("PATH")
            if not atom(path) then
                while 1 do
                    semicolon = find(';',path)
                    if semicolon!=1 then -- skip nulls (ie/eg ;;) in path
                        if semicolon then
                            sm1 = semicolon-1
                            if path[sm1]!=SLASH then
                                path[semicolon] = SLASH
                                thispath = path[1..semicolon]
                            else
                                thispath = path[1..sm1]
                            end if
                        else
                            if path[length(path)]!=SLASH then
                                path &= SLASH
                            end if
                            thispath = path
                        end if
                        if not find(thispath,filepaths) then
--                          fn = open(thispath&file,"rb")
                            fn = open(thispath&file,"r")
                            if fn!=-1 then exit end if
                            if not find(thispath,triedpaths) then
                                triedpaths = append(triedpaths,thispath)    -- for error reporting
                            end if
                        end if
                    end if
                    if not semicolon then exit end if
                    path = path[semicolon+1..length(path)]
                    if not length(path) then exit end if
                end while
            end if
        end if
        if fn=-1 and find(SLASH,file) then
            thispath = ""
--          fn = open(file,"r")
--          fn = open(file,"rb")
            fn = open(file,"r")
        end if
    end if
    if fn!=-1 or minus_e then
--if usegpp then
--?{thispath,file}
if not minus_e then
        file = get_proper_path(thispath&file,"")
end if
--?{{file}}
--/*
global integer intellisense = 0
global sequence trapfile, trapkey
global integer trapline, trapcol, trapmode, trapns
--*/        
        if intellisense=1 then
            if file=trapfile then
                intellisense = 2 -- (more stuff gets done rsn)
--              printf(1,"intellisense:got %s\n",file)
--          else
--              printf(1,"intellisense:not %s\n",file)
            end if
        end if
--DEV 
--/*
        if intellisense=1
        and file = ??? then
            close(fn)
            file = ???? 
            fn = open(file,"rb")
            if fn=-1 then
                fatal("intellisense intercept error (%s)",file)
            end if
            intellisense = 2
        end if
--*/
--else
--      file = lower(thispath&file)
--      --
--      -- first check for and remove any \..\ in filepath
--      --
--      while 1 do
--          k = match("\\..\\",file)
--          if k=0 then exit end if
--          for j=k-1 to 1 by -1 do
--              if file[j]='\\' then
--                  -- remove "\\xxx\\.."
----/**/                file[j..k+2] = ""                               --/* -- Phix
--                  file = file[1..j-1] & file[k+3..length(file)]   --*/ -- RDS
--                  k = 0 -- signal found
--                  exit
--              end if
--          end for
--          if k!=0 then
--              puts(1,"Warning, cannot cleanup "&file&'\n')
----                ?9/0
--              exit
--          end if
--      end while
--      --
--      -- repeat for any \.\
--      --
--      while 1 do
--          k = match("\\.\\",file)
--          if k=0 then exit end if
--          -- remove "\\."
----/**/        file[k..k+1] = ""                               --/* -- Phix
--          file = file[1..k-1] & file[k+2..length(file)]   --*/ -- RDS
--      end while
--      --
--      -- repeat for any \\
--      --
--      while 1 do
--          k = match("\\\\",file)
--          if k=0 then exit end if
--          -- remove 1st "\\" of 2
----/**/        file[k..k] = ""                                 --/* -- Phix
--          file = file[1..k-1] & file[k+1..length(file)]   --*/ -- RDS
--      end while
--end if
        if find(SLASH,file) then
            for j=length(file) to 1 by -1 do
                if file[j]=SLASH then
                    thispath = file[1..j]
                    file = file[j+1..length(file)]
                    exit
                end if
            end for
        end if
--**NB** Do not remove (referenced in docs/Qu.htm):
--pHeapD hack:
--if 01 then
--  if find(file,{"pHeap.e","pStack.e"}) then
--      file[-2..$] = "D.e"
--      close(fn)
--      fn = open(thispath&file,"rb")
--      if fn=-1 then ?9/0 end if
--  end if
--end if
        pathno = find(thispath,filepaths)
--DEV/kludge
if platform()=LINUX then
        if pathno=0 then
            if match(mainpath,thispath)=1 then
                thispath2 = rootpath&thispath[length(mainpath)+1..$]
                pathno = find(thispath,filepaths)
                if pathno!=0 then
                    thispath = thispath2
                end if
            end if
        end if
end if
        if pathno=0 then
if showfileprogress then
?{"newpath",thispath}
end if
            filepaths = append(filepaths,thispath)
            pathno = length(filepaths)
            activepaths = append(activepaths,length(filenames)+1)
--printf(1,"includeFile pathno=0 case (%s): ",{file})
--?activepaths
        else
--          fileno = find({pathno,file},filenames)
            k = find({pathno,file},filenames)
--          if fileno then
            if k then
if showfileprogress then
?"already included"
end if
                close(fn)
                fn = -1
--              fileno = -fileno
                fileno = -k
--DEV: this may be required here:
--              col = 0
                return -- -fileno
            end if
--DEV
--?{pathno,vmpath,file,vmset,find(file,vmset)}
            if pathno=vmpath
            and find(file,vmset) then
                close(fn)
                fn = -1
                fileno = -1
                return
            end if
            if not activepaths[pathno] then
--printf(1,"includeFile pathno!=0 case (%s): ",{file})
--?activepaths
                activepaths[pathno] = length(filenames)+1
--?activepaths
            end if
        end if

        wasfileno = fileno
        loadFile()
--if filenames={} then ?text {} = wait_key() end if
if showfileprogress then
--?text
?"loadFile"
end if
        allfiles = append(allfiles,text)
        allpfiles = append(allpfiles,text)
        exptext = append(exptext,0)
        expandedYet = append(expandedYet,0)
        unresolved_routine_ids = append(unresolved_routine_ids,0)
        finalOptWarn = append(finalOptWarn,optset[OptWarning])
--DEV
        if length(filenames)=0 then
--      if length(filenames)=FincMax then
            -- (the testset (p -test) contains some ..\\)
if showfileprogress then
?"ptok.e line 583: thispath="&thispath
end if
            mainpath = thispath
            mainfile = file
        end if
        filenames = append(filenames,{pathno,file})
        fileno = length(filenames)
        if intellisense=2 then
            trapfileno = fileno
            if trapline!=0 then -- (unparsed builtin retry uses trapline of 0; see eaisense.ew)
                --
                -- crop the trapfile (so we can intercept EOF and do the lookup)
                --
                exptext[$] = expandIntoLines()
                exptext[$] = exptext[$][1..trapline]
                exptext[$][$] = exptext[$][$][1..trapcol-1]&'\n'
                linestarts = linestarts[1..trapline]
                expandedYet[$] = linestarts
                text = packLines(exptext[$])
                ltl = length(text)
                allfiles[$] = text
                allpfiles[$] = text
            end if
            intellisense = 3
        end if
        -- aside: this is actually almost the polar opposite of requires()...
        --        this is the compiler trying to make sure it is getting 
        --        the builtins it shipped with; the latter is anything or
        --        anyone asking the compiler to make an explicit check.
        cversion = {0}
        k = find(lower(file),phixcompat)
        if k then
            k = match("Phix compatible",text)
            if k then
--trace(1)
                k += length("Phix compatible")
                while 1 do
                    ch = text[k]
                    if ch=')' then exit end if
                    if ch!=' ' then
                        if ch='.' then
                            cversion &= 0
                        elsif ch<'0' or ch>'9' then
--                          Abort("invalid version")
                            exit
                        else
                            cversion[$] = cversion[$]*10+ch-'0'
                        end if
                    end if
                    k += 1
                end while
                if cversion>phixversion then
                    tokcol = k
                    tokline = 1
                    for i=1 to k do
                        if text[i]='\n' then
                            tokline += 1
                        end if
                    end for
                    Abort("invalid Phix compatible version")
--                  Fatal("invalid Phix compatible version")
                end if
                k = 0
                for i=1 to length(vctrl) do
                    if lower(file)=vctrl[i][1] then
                        if cversion<vctrl[i][2] then
                            k = 1
                        end if
                        exit
                    end if
                end for
            elsif length(text)>1000 then -- skip link stubs
                k = -1
            end if
        end if
        if k!=0 then
            if k<0 then
                msg = "not marked Phix compatible"
            else
                msg = "invalid Phix compatible version"
            end if
            wline = tokline
            wcol = tokcol
            if wasfileno then
                fileno = wasfileno
                if autoInclude then
                    -- no include statement, show warning on line 1 column 1
                    wcol = 1
                    wline = 1
--PL 5/7/13: wtf?
--                  wasfileno = 0
                else
                    -- display the warning on the include statement, with actual file in message
                    text = allfiles[fileno]
                    wcol = emitcol
-- 5/7/13: (moved out)
--                  msg = thispath&file&" "&msg
                end if
                msg = thispath&file&" "&msg
            end if
            Warn(msg,wline,wcol,0)
            if wasfileno then
                fileno = length(filenames)
--29/12/2010
--              text = allfiles[fileno]
                text = allpfiles[fileno]
            end if
        end if
--  return newfileno
--end function
        if showfileprogress then
            if not testall then
                puts(1,thispath&file&'\n')
                if wasfileno then
                    printf(1,"  included by file %s, line %d\n",{filenames[wasfileno][2],tokline})
                end if
            end if
        end if
        return -- fileno
    end if

    --
    -- allow eg "t01" as shorthand for "test\t01type",
    --
--trace(1)
    k = length(mainpath)
    if length(filenames)=0
    and autoInclude=0
    and k<length(file)
    and equal(file[1..k],mainpath) then
        kp1 = k+1
        kp2 = k+2
        kp3 = k+3
        if length(file)=k+7     -- ie as in "t01.exw"
        and file[kp1]='t'
        and file[kp2]>='0' and file[kp2]<='9'
        and file[kp3]>='0' and file[kp3]<='9'
        and equal(file[k+4..k+7],".exw") then
            path = dir(mainpath&"test"&SLASH&file[k+1..k+3]&"*.exw")
        elsif length(file)=k+5  -- ie as in "b.exw"
          and equal(file[k+1..k+5],"b.exw") then
            path = dir(mainpath&"bench"&SLASH&"bench.exw")
        elsif length(file)=k+5  -- ie as in "a.exw"
          and equal(file[k+1..k+5],"a.exw") then
--DEV -norun?
            path = dir(mainpath&"demo\\arwen\\arwen.ew")
            if sequence(path) and length(path)=1 then
                path[1][1] = "demo\\arwen\\"&path[1][1]
            end if
        elsif length(file)=k+5  -- ie as in "w.exw"
          and equal(file[k+1..k+5],"w.exw") then
--DEV -norun?
            path = dir(mainpath&"demo\\win32lib\\win32lib.ew")
            if sequence(path) and length(path)=1 then
                path[1][1] = "demo\\win32lib\\"&path[1][1]
            end if
        elsif length(file)=k+6  -- ie as in "bt.exw"
          and equal(file[k+1..k+6],"bt.exw") then
            path = dir(mainpath&"bench"&SLASH&"benchtst.exw")
        elsif length(file)=k+8  -- ie as in "edix.exw"
          and equal(file[k+1..k+8],"edix.exw") then
            path = dir(mainpath&"demo\\edix\\edix.exw")
            if sequence(path) and length(path)=1 then
                path[1][1] = mainpath&"demo\\edix\\edix.exw"
                mainpath &= "demo\\edix\\"
            end if
        elsif length(file)=k+9  -- ie as in "edita.exw"
          and equal(file[k+1..k+9],"edita.exw") then
--DEV newEmit...
if newEmit then
            path = dir(mainpath&"demo\\edita\\edita.exw")
--?path
            if sequence(path) and length(path)=1 then
--              path[1][1] = mainpath&path[1][1]
                path[1][1] = mainpath&"demo\\edita\\edita.exw"
                mainpath &= "demo\\edita\\"
--?mainpath
--?path
--          else
--?9/0
--              path = dir("C:\\Program Files (x86)\\Edita\\edita.exw")
--              if sequence(path) and length(path)=1 then
--                  path[1][1] = "C:\\Program Files (x86)\\Edita\\"&path[1][1]
--              end if
            end if
else -- old code
            path = dir("C:\\Program Files\\Edita\\edita.exw")
            if sequence(path) and length(path)=1 then
                path[1][1] = "C:\\Program Files\\Edita\\"&path[1][1]
            else
                path = dir("C:\\Program Files (x86)\\Edita\\edita.exw")
                if sequence(path) and length(path)=1 then
                    path[1][1] = "C:\\Program Files (x86)\\Edita\\"&path[1][1]
                end if
            end if
end if
--2/11/17 for tom: (if we could not find xx.exw, look for xx.ex)
        elsif length(file)>4 and file[-4..-1]=".exw" then
            path = {{file[1..$-1]}}
        else
            if find('*',file)>k then
                path = dir(file)
                if sequence(path) and length(path)=1 then
                    OptConsole = 1
if showfileprogress then
?"recurse"
end if
                    includeFile(mainpath&path[1][1],0,emitcol)
                    return
                end if
            end if
            path = 0
        end if
        if sequence(path) and length(path)=1 then
            path = path[1][1]
            OptConsole = 1
            if not find(SLASH,path) then
                if find(path[1],"tT") then
                    path = mainpath&"test"&SLASH&path
                elsif find(path[1],"bB") then
                    path = mainpath&"bench"&SLASH&path
                end if
            end if
if showfileprogress then
?"recurse2"
end if
            includeFile(path,0,emitcol)
            return
        end if
    end if

    -- allow a top-level "include actual\path\really\required\xxx.e"
    -- to do the job of stopping a nested (3rd party) "include xxx.e"
    -- spannering the whole show; you can't find it now but it has the
    -- same name as something managed prior, so just assume...
--DEV add this to the inc5 test set, eg:
--include test\t05\inc5\t2.e
--include t2.e
--?z
--abort(0)
--trace(1)
    for i=1 to length(filenames) do
        inpart = filenames[i]
        inpart = filepaths[inpart[1]]&inpart[2]
        k = match(file,inpart)
        km1 = k-1
        if k=length(inpart)-length(file)+1
        and (k=1 or inpart[km1]=SLASH) then
--DEV for j=i+1 to length(filenames) do
--      report ambiguity
            fileno = -i
            return
        end if
    end for

?current_dir()
    fatalmsg = "Cannot open "&file
    if autoInclude then
        fatalmsg = "Cannot open autoinclude "&file
    end if
    -- build list of directories searched:
    if length(triedpaths) then
        inpart = "\nin "
        for i=1 to length(triedpaths)-1 do
            fatalmsg &= inpart&triedpaths[i]&","
            inpart = "\n   "
        end for
        if equal(inpart,"\n   ") then
            inpart = "\nor "
        end if
        fatalmsg &= inpart&triedpaths[length(triedpaths)]&'.'
    end if
    tokcol = emitcol
    if length(allfiles)=0 
    or autoInclude then
        -- (no source line to report against)
        Fatal(fatalmsg)
--      abort(1)    -- Fatal() does not return
    else
        if intellisense
        and trapfileno = fileno then
            intellisense = 5
            eof_processing()
        end if
        Abort(fatalmsg)
    end if
end procedure

--without trace
constant Tmap = {"integer","atom","string","sequence","object"}

function unmapsig(sequence sig, integer ParmN)
-- (opposite of mapsig in psym.e)
string res
integer k
    res = ""
    for i=2 to length(sig) do
        k = sig[i]
        k = find(k,{T_integer,T_atom,T_string,T_sequence,T_object})
        --DEV if k>T_object? (I think we are only doing builtins here anyway, but there is a ttree way to get names...)
        if i-1=ParmN+1 then
            res &= "["
        end if
        if length(res) then
            res &= ","
        end if
        res &= Tmap[k]
    end for
    if length(sig)-1>ParmN then
        res &= "]"
    end if
    return res
end function

--forward global function getBuiltinName(integer routineNo) -- defined in psym.e
global integer r_getBuiltinName

--with trace

integer ifn

function isDumpIds(sequence name, integer node)
-- intellisense output
sequence si,sf
integer symidx,fno,nTyp
object v
string tiptext
integer p, minp, maxp

    symidx = tt[node+EQ]
    if symidx>=1 and symidx<=length(symtab) then
-- loop added 03/02/14:
        while symidx do
            si = symtab[symidx]
            fno = si[S_FPno]
            if fno=fileno or fno=0 or and_bits(si[S_State],K_gbl) then
                if trapmode=0 then              -- standard lookup
                    puts(ifn,name&"\n")
                elsif trapmode=1 then           -- intellilink
-- 3/2/14:
--                  if name=trapkey then
                    if name=trapkey
                    and (trapns=0 or trapns=fno) then
                        nTyp = si[S_NTyp]
                        if fno=0 then
                            if nTyp=S_Const then
                                v = si[S_value]
                                if integer(v) and v>15 then
                                    printf(ifn,"Defined in psym.e with a value of %d[#%08x]\n",{v,v})
                                else
                                    printf(ifn,"Defined in psym.e with a value of %s\n",{ppf(v)})
                                end if
                            elsif nTyp>=S_Type then
                                printf(ifn,"Defined in psym.e as builtin %s(%s)\n",{NTdesc[nTyp][1..$-1],unmapsig(si[S_sig],si[S_ParmN])})
                            else
                                printf(ifn,"Defined in psym.e as a %s\n",{NTdesc[nTyp]})
                            end if
                        elsif fno>length(filenames) then
                            -- picked apart and retried in eaisense.ew:
                            -- (ie/eg name=sort but we have hit EOF (as the file is cropped to cursor line/col) before 
                            --  pmain.e/checkforbuiltins() has had a chance to load/parse builtins\psort.e; eaisense
                            --  will try "sort" again but this time with the full text of rootpath\builtins\psort.e)
                            string bname = call_func(r_getBuiltinName,{symidx})
                            printf(ifn,"Defined in psym.e as an autoinclude; %s not yet resolved, see %sbuiltins"&SLASH&"%s\n",
                                       {name,rootpath,bname})
                        else
                            sf = filenames[fno]
                            if nTyp<=S_TVar then
                                tokcol = si[S_ErrV]
                            else
                                tokcol = si[S_ErrR]
                            end if
                            if equal(expandedYet[fno],0) then
                                text = allfiles[fno]
                                exptext[fno] = expandIntoLines()
                                expandedYet[fno] = linestarts
                                text = allpfiles[fno]
                            else
                                linestarts = expandedYet[fno]
                            end if
                            convertToLineColumn(tokcol)
                            printf(ifn,"%s%s:%d\n",{filepaths[sf[1]],sf[2],eLine})
                        end if
                    end if
                elsif trapmode=3 then   -- intellitip
--DEV parameters...
                    if name=trapkey
                    and (trapns=0 or trapns=fno) then
--                      puts(ifn,name&"(")
                        nTyp = si[S_NTyp]
                        if nTyp>=S_Type then
                            if fno=0 then
--                          if 1 then
                                printf(ifn,"%s %s(%s) [builtin]\n",{NTdesc[nTyp][1..$-1],name,unmapsig(si[S_sig],si[S_ParmN])})
--                              printf(ifn,"%s %s(%s)\n",{NTdesc[nTyp][1..$-1],name,unmapsig(si[S_sig],si[S_ParmN])})
                            else
-- NB: do not forget to limit hint to 80 chars... (here or eaisense.ew or both)
--              S_sig   = 7,    -- routine signature, eg {'F',T_integer} (nb S_sig must be = S_vtype)
--              S_Parm1 = 8,    -- first parameter. (idx to symtab, follow S_Slink)
--              S_ParmN = 9,    -- minimum no of parameters (max is length(S_sig)-1)
                                tiptext = sprintf("%s %s(",{NTdesc[nTyp][1..$-1],name})
                                p = si[S_Parm1]
                                minp = si[S_ParmN]
                                maxp = length(si[S_sig])-1
                                for i=1 to maxp do
                                    if i>1 then
                                        tiptext &= ", "
                                    end if
                                    if i=minp+1 then
                                        tiptext &= '['
                                    end if
                                    if p=0 then -- occurs for c_func...
                                        tiptext &= "??"
                                        exit
                                    end if
                                    si = symtab[p]
                                    tiptext &= getname(symtab[si[S_vtype]][S_Name],-2)&" "&getname(si[S_Name],-2)
                                    p = si[S_Slink]
                                end for
--                              si = symtab[symidx] -- (reset for outer loop) [erm, we'd need major reset to continure original traversal... return 0 added below]
                                if minp<maxp then
                                    tiptext &= ']'
                                end if
                                if length(tiptext)>78 then
                                    tiptext = tiptext[1..76]&".."
                                end if
                                tiptext &= ")\n"
                                puts(ifn,tiptext)
--puts(1,tiptext)
                                return 0
                            end if
                        else -- not a routine??
                            printf(ifn,"%s %s\n",{NTdesc[nTyp],name})
                        end if
--                      puts(ifn,")\n")
                    end if
                else
                    ?9/0
                end if
            end if
            symidx = si[S_Nlink]
        end while
    end if
    return 1
end function
constant r_isDumpIds = routine_id("isDumpIds")

global procedure eof_processing()
-- Sets Ch to -1 and clear activepaths
integer k
    if intellisense>=3 then
        if trapfileno = fileno then
--?9/0
--DEV "/usr/tmp/isense.txt" on lnx(?)
            ifn = open(getenv("TMP")&"\\isense.txt","w")
            if ifn=-1 then
                puts(1,"error opening isense.txt\n")
                if getc(0) then end if
            else                
                if intellisense>=4 then
                    --
                    -- Nonsense catches from stupid users or misplaced mouseclicks ;-)
                    --  These may not be particularly useful, but they are better than
                    --  a compiler error in a dos-box popup (which may remain hidden),
                    --  or, and perhaps a bit more helpfully, show precisely what the 
                    --  compiler (wrongly or just !=user) thinks it is doing.
                    --  
                    if intellisense=4 then
                        -- from pmain.e/IncludeFile()
                        -- (if you ctrl-click on "ns" in "include <file> as ns")
                        puts(ifn,"A locally declared namespace")
                    elsif intellisense=5 then
                        -- from ptok.e/includeFile() above
                        -- (if you ctrl-click on "file" in "include file")
                        puts(ifn,"The filename to include")
                    else
                        ?9/0
                    end if
                else
                    if trapkey="0" then
                        trapkey = ""
                    end if
                    tt_traverse(r_isDumpIds,trapkey,-2,0)
                end if
                close(ifn)
                sendCDISENS()
            end if
            abort(0)
        end if
    end if
    Ch = -1
    k = find(fileno,activepaths)
    if k and activepaths[k]=fileno then
--printf(1,"eof_processing (fileno=%d): ",fileno)
--?activepaths
        activepaths[k] = 0
--?activepaths
--if fileno=2 then ?9/0 end if
    end if
end procedure

--DEV/SUG: (to make it reusable)
--include builtins\tok.e

global procedure tokinit()
--  initFilePathSet()

    allpfiles = {}

    line = 1
    col = 0
--if not minus_e then
    ltl = -1
--end if
    tokline = 0
    tokcol = 0
    
    mapEndToMinusOne = 0

end procedure

global function getIncludeLine()
string res
    tokcol = col+1
    for i=col+1 to ltl do
        if text[i]='\n' then
            res = text[col..i]
            col = i--+1
            tokcol = i
--          line += 1
            return res
        end if
    end for
end function

global integer Tok9
global atom TokN

global procedure getCh()
-- this code inlined where possible.
-- called by main/include to initialise.
    col += 1
    if col>ltl then
        eof_processing() -- (sets Ch to -1 and clears activepaths)
        return
    end if
    if Ch='\n' then
        line += 1
    end if
    Ch = text[col]
end procedure

global function ChNext(integer ch)
-- used for named parameters, to check whether a Ch of ':' is part of a ":=".
    return text[col+1]=ch
end function

integer ch2
integer cp1, cp2, cp3

--with trace
procedure SkipBlockComment()
-- Skip nested /* block comments */
-- Note that "*/" inside a string is interpreted as end of comment, 
--  (since it is technically text not code, and for example we must
--   treat '/* if x="*/" then' as '" then'), though a "/*" and "*/" 
--   pair (of strings/within a string) behave as expected.
-- The opening /* has already been processed; if we cannot find a 
-- matching */ before EOF then display an error.
integer oline, ocol
--trace(1)
    ocol = col
    oline = line
-- if line!=0 then ?9/0 end if  -- we must maintain this!
    while 1 do
        col += 1
        if col>=ltl then exit end if -- Triggers error
        if Ch='\n' then
            line += 1
        end if
        Ch = text[col]
        if Ch='*' then
            cp1 = col+1
            ch2 = text[cp1]
            if ch2='/' then
                col += 2
                Ch = text[col]
                return
            end if
        elsif Ch='/' then
            cp1 = col+1
            ch2 = text[cp1]
            if ch2='*' then
                col = cp1
                SkipBlockComment()
                col -= 1
            end if
        end if
    end while
    tokline = oline
    tokcol = ocol
    Abort("missing closing block comment")
end procedure

global constant
    EOL     = 1,    -- End of line
    SPACE   = 2,    -- Spaces & tabs
    SYMBOL  = 3,    -- General symbols !&*+,./<>=?
    HEXDEC  = 4,    -- Hexadecimal (#) mark
--  ILASM   = 5,    -- #ilasm statement
--  TYPEIS = 6,     -- #type_is construct
    BRACES  = 5,    -- ()[]{}
    ELLIPSE = 6,    -- '..'
    SQUOTE  = 7,    -- Single quotation mark
    DQUOTE  = 8,    -- Double quotation mark
    BKTICK  = 9,    -- Back tick (string with no escape characters)
    ILLEGAL = 10,   -- illegal character
    FLOAT   = 11,   -- float, eg 1.0 or 1e4
    DIGIT   = 12,   -- 0..9
--  USCORE  = 12,   -- _
    LETTER  = 13,   -- A..Z,a..z
    HEXSTR  = 14,   -- Hexadecimal Byte String
    LABEL   = 15,   -- :: (as a hll label)
    DIGIBAD = -1    -- partials, eg "-."; "3.0e"

--string charset, identset, baseset, whiteacl   -- identset now in ptree.e
string charset, baseset
--, whiteacl
    charset = repeat(ILLEGAL,256)
    charset['\n'] = EOL
    charset['\r'] = EOL
    charset['\t'] = SPACE
    charset[' ']  = SPACE
    charset['!']  = SYMBOL
    charset['\"'] = DQUOTE
    charset['`']  = BKTICK
    charset['#']  = HEXDEC
    charset['&']  = SYMBOL
    charset['|']  = SYMBOL
    charset['\''] = SQUOTE
    charset['('..')'] = BRACES  -- () only
    charset['*'..'/'] = SYMBOL  -- *+,-./
    charset['0'..'9'] = DIGIT
    charset[':'..'?'] = SYMBOL  -- :;<=>?
    charset['A'..'Z'] = LETTER
--4/9/19: (nb changes must match identset in pttree.e)
--  charset[#80] = LETTER   -- more unicode
--  charset[#88] = LETTER   -- more unicode
--  charset[#94] = LETTER   -- for rosettacode/unicode (as ptok.e is not stored in utf8)
--  charset[#9A] = LETTER   -- for rosettacode/unicode
--  charset[#A3] = LETTER   -- for rosettacode/unicode
--  charset[#BB] = LETTER   -- for rosettacode/unicode
--  charset[#CE] = LETTER   -- for rosettacode/unicode
--  charset[#CF] = LETTER
--  charset[#E2] = LETTER
    charset[#80..#BF] = LETTER
    charset[#C2..#F4] = LETTER
--51102: #81(129), #82(130), #83(131), #84(132), #85(133), #86(134), #87(135), #89(137), #8A(138), #8B(139), #8C(140), #8D(141), #8E(142), #8F(143), 
--      #90(144), #91(145), #92(146), #93(147), #95(149), #96(150), #97(151), #98(152), #99(153), #9B(155), #9C(156), #9D(157), #9E(158), #9F(159), 
--      #A0(160), #A1(161), #A2(162), #A4(164), #A5(165), #A6(166), #A7(167), #A8(168), #A9(169), #AA(170), #AB(171), #AC(172), #AD(173), #AE(174), #AF(175), 
--      #B0(176), #B1(177), #B2(178), #B3(179), #B4(180), #B5(181), #B6(182), #B7(183), #B8(184), #B9(185), #BA(186), #BC(188), #BD(189), #BE(190), #BF(191)
--From https://rosettacode.org/wiki/Idiomatically_determine_all_the_characters_that_can_be_used_for_symbols
--or more accurately, after I posted that I ran some further tests, and determined the following:
--  Tip: 100% unicode coverage would be achieved by marking everything from #80 to #BF and #C2 to #F4, 
--       as LETTER, but not #C0, C1 or #F5..#FF (oh, what the heck, just do it! -- DONE!)
--  charset['_'] = ILLEGAL  -- Specifically checked for after 1st character of LETTER
--  charset['_'] = USCORE
--12/11/15:
    charset['_'] = LETTER
    charset['['] = BRACES
    charset[']'] = BRACES
    charset['a'..'z'] = LETTER
    charset['{'] = BRACES
    charset['}'] = BRACES
if ORAC then
    charset['~']  = SYMBOL
end if

global procedure SpecialHandling(integer char, integer chartype)
    charset[char] = chartype
end procedure
-- now in pttree.e:
--  identset = repeat(' ',256)  -- characters valid in an identifier
--  -- convert to "string of nulls" (repeat(0,256) makes a dword-sequence)
--  for i=1 to 256 do identset[i] = 0 end for
--  identset['0'..'9'] = 1
--  identset['A'..'Z'] = 1
--  identset['_'] = 1
--  identset['a'..'z'] = 1

--  whiteacl = repeat(' ',256)  -- whitespace and comment lead-ins
--  -- convert to "string of nulls" (repeat(0,256) makes a sequence)
--  for i=1 to 256 do whiteacl[i] = 0 end for
--  whiteacl[' '] = 1
--  whiteacl['\t'] = 1
--  whiteacl['\r'] = 1
--  whiteacl['\n'] = 1
--  whiteacl['-'] = 2
--  whiteacl['/'] = 3

    baseset = repeat(255,256)
    for i=0 to 9 do
        baseset['0'+i] = i
    end for
    for i=10 to 35 do
        baseset['A'+i-10] = i
        baseset['a'+i-10] = i
    end for

--DEV constants can be compiled better??
-- Subscripting a literal constant can be optimised better by the compiler, 
--  since it has a fixed base address (as opposed to a calculate-once constant).
-- The gain is small, maybe 3%[???], so don't worry about using the hll code below when experimenting.
--constant charset = {13,13,13,13,13,13,13,13, 1, 2,13,13, 2,13,13,13,13,13,13,13,13,13,13,13,13,13,13,13,13,13,13, 1,
--                   3, 4, 5,13,13, 3, 6, 7, 7, 3, 3, 3, 3, 3, 3, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 3, 3, 3, 3, 3, 3,13,
--                  10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10, 7,13, 7,13,13,13,
--                  10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10, 7,13, 7,13,13,13,
--                  13,13,13,13,13,13,13,13,13,13,13,13,13,13,13,13,13,13,13,13,13,13,13,13,13,13,13,13,13,13,13,13,
--                  13,13,13,13,13,13,13,13,13,13,13,13,13,13,13,13,13,13,13,13,13,13,13,13,13,13,13,13,13,13,13,13,
--                  13,13,13,13,13,13,13,13,13,13,13,13,13,13,13,13,13,13,13,13,13,13,13,13,13,13,13,13,13,13,13,13,
--                  13,13,13,13,13,13,13,13,13,13,13,13,13,13,13,13,13,13,13,13,13,13,13,13,13,13,13,13,13,13,13,13},
--      identset = {0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
--                  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,1,1,1,1,1,1,1,1,1,0,0,0,0,0,0,0,    -- 0..9
--                  1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,0,0,0,0,1,0,    -- A..Z,_
--                  1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,0,0,0,0,0,0,    -- a..z
--                  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
--                  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
--                  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
--                  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
--      baseset = {255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,
--                 255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,
--                 255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,  0,     -- '0'
--                   1,  2,  3,  4,  5,  6,  7,  8,  9,255,255,255,255,255,255,255,     -- '1'..'9'
--                  10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25,     -- 'A'..'Z'
--                  26, 27, 28, 29, 30, 31, 32, 33, 34, 35,255,255,255,255,255,255,
--                  10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25,     -- 'a'..'z'
--                  26, 27, 28, 29, 30, 31, 32, 33, 34, 35,255,255,255,255,255,255,
--                 255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,
--                 255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,
--                 255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,
--                 255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,
--                 255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,
--                 255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,
--                 255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,
--                 255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255}
--global constant whiteacl = {
--                  0,0,0,0,0,0,0,0,1,1,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,    -- \t, \n, \r, ' '
--                  0,0,0,0,0,0,0,0,0,0,0,0,2,0,3,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,    -- '-', '/'
--                  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
--                  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
--                  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
--                  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
--                  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
--                  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0}


--with trace
global procedure skipSpacesAndComments()
--global procedure skipSpaces()
--NB: idx out of bounds here (Ch=-1) should be fixed by testing
--    for that before the call, rather than here.
--    if Ch is 0, it should be fixed in loadFile [?].
--integer k
--integer t -- now global chartype
--  while whiteacl[Ch] do   -- whitespace and comment lead-ins
    while 1 do
--      k = whiteacl[Ch]
        chartype = charset[Ch]
        if chartype>SYMBOL then exit end if
--      if not k then exit end if
--      if k=1 then         -- Ch in " \t\r\n"
--      if Ch<=' ' then
        if chartype<=SPACE then
            col += 1
            if col>ltl then
                eof_processing() -- (sets Ch to -1 and clears activepaths)
                return
            end if
            if Ch='\n' then
                line += 1
            end if
            Ch = text[col]
--10/07/20
--      elsif Ch='-' then   -- check for comment
        elsif Ch!='-' and Ch!='/' then  -- check for comment
            exit
        else
--printf(1,"skipSpacesAndComments line 1383, Ch=%c, fileno=%d, col=%d\n",{Ch,fileno,col})
            cp1 = col+1
--          if text[cp1]!='-' then exit end if
            if text[cp1]!=Ch then
                if Ch!='/' then exit end if
                if text[cp1]!='*' then exit end if
                col = cp1
                SkipBlockComment()
            else
                cp2 = col+2
                Ch = text[cp2]
                cp3 = col+3
                -- check for "--/*" case:
--              if Ch='/' and text[cp3]='*' then
                if Ch='/' and text[cp3]='*' and text[cp1]='-' then
                    col = cp3
                    SkipBlockComment()
                else
--26/7/17:
--/*
                    integer ipstart = 0
                    if Ch='#' 
                    and length(text)>col+16
                    and text[col..col+15]="--#include_paths"
                    and find(text[col+16]," \t") then
                                        -- 12345678901234567
                        cp2 += 15 -- (ie col+17)
                        ipstart = cp2
                    end if
--*/
--19/05/2010:
--                  col = cp3
                    col = cp2
                    while Ch!='\n' do
                        col += 1
                        Ch = text[col]
                    end while
--/*
                    if ipstart!=0 then
                        string ip = text[ipstart..col-1]
                        ipstart = match("--",ip)
                        if ipstart then
                            ip = ip[1..ipstart-1]
                        end if
                        ip = get_proper_path(trim(ip),"")
--?{"#include_paths",ip}
                        addPath(ip)
                    end if
--*/
                    line += 1
                    while Ch<=' ' do
                        col += 1
                        if col>ltl then
                            eof_processing()    -- (sets Ch to -1 and clears activepaths)
                            return
                        end if
                        Ch = text[col]
--19/05/2010:
                        if Ch='\n' then
                            line += 1
                        end if
                    end while
                end if
            end if
--      else
--          if Ch!='/' then exit end if
--          cp1 = col+1
--          if text[cp1]!='*' then exit end if
--          col = cp1
--          SkipBlockComment()
        end if
    end while
end procedure

global procedure skipHashBangLine()
-- called from Compile() in pmain.e when 1st char is '#'
--19/03/2010 (allow #ilasm at very start)
    if ltl>1 and text[2]='i' then return end if
    getCh()
    if Ch!='!' then Expected("#!") end if
    while Ch!='\n' do
        col += 1
-- added 18/9, removed immediately as we should always have a '\n', I think.
--      if col>ltl then
--          Ch = ' '
--          exit
--      end if
        Ch = text[col]
    end while
--removed 1/10/19 (since it gets done in skipSpacesAndComments(), again)
--  line += 1
    skipSpacesAndComments()
end procedure

--with trace
global function allWhiteToTokcol()
integer c
    if Ch>0 then
        for i=tokcol-1 to 1 by -1 do
            c = text[i]
            if c='\n' then return 1 end if
            if charset[c]>SPACE then return 0 end if
        end for
    end if
    return 1
end function

global function isFLOAT(atom N)
--if useFLOAT then --(DEV/temp)
    if machine_bits()=32 then   -- (runtime)
        if X64=0 then           -- (target)
            -- 32 bit compiler -> 32 bit executable
            return not integer(N)
        else
            -- 32 bit compiler -> 64 bit executable (partial range coverage)
--          return N!=floor(N) or N<-#80000000 or N>+#80000000
            return N!=floor(N) or N<-#FFFFFFFF or N>+#FFFFFFFF
--7/7/17: (no help)
--                             or N<MININT     or N>MAXINT
        end if
    else -- machine_bits()=64   -- (runtime)
        if X64=1 then           -- (target)
            -- 64 bit compiler -> 64 bit executable
            return not integer(N)
        else
            -- 64 bit compiler -> 32 bit executable
            return N!=floor(N) or N<-#40000000 or N>+#3FFFFFFF
        end if
    end if
--else
--  return not integer(N)
--end if
end function

procedure setFLOAT()
--
-- from the manual:
--      When using a 32-bit compiler to create a 64-bit executable, be aware that the integer range is
--      redefined as +/-#FFFF_FFFF rather than -#4000_0000_0000_0000 to #3FFF_FFFF_FFFF_FFFF. See ptok.e/
--      setFLOAT() for all the nitty-gritty details.
-- details:
--      Technically, when using a 32-bit compiler to create a 64-bit executable, using 64-bit atoms with
--      53 bits of precision, a limit of +/-#20_0000_0000_0000 would apply (/significant complications
--      could be introduced by using a pair of atoms if we really wanted to go mad) but since the 32-bit
--      compiler can create a 64-bit compiler without problems, there seems little point trying harder.
--      (Besides, the harder you try, the more issues you'll get from still being on a 32 bit run-time.)
--      In practice (see above) we settle for a limit of +/-#FFFF_FFFF, to keep things reasonably simple,
--      because 32 bit versions of things like and_bits() really are limited to 32 bits, and not 53 bits.
--      Programs which use (very large) integer values/constants in the uncovered ranges are likely to be
--      incorrectly cross-compiled. Obviously these are only limits during compilation, not run-time, and
--      the source code of Phix itself does not use any such values, nor do any of the supplied demos.
--      (For obvious reasons the compiler itself should only ever contain completely 32-bit safe code.)
--      The absolute minimum range that we MUST cope with perfectly is (signed) #80000000 (ie -2147483648) 
--      through to (unsigned) #FFFFFFFF (ie + 4294967295), especially <-#40000000|>+#3FFFFFFF, otherwise
--      code that works perfectly on 32-bit may well exhibit cryptic mishaps on 64-bit. I am not overly
--      concerned with 64-bit code failing when compiled to a 32-bit executable, for obvious reasons.
--
--      The sources of the compiler have been modified to use isFLOAT() instead of integer(), but there
--      will inevitably be a few places that I have missed, eg builtins\timestamp.ew currently contains:
--          --DEV temp/32/64 bit issues:
--          --  GENERIC_READ =  #80000000
--              GENERIC_READ =  #8000
--              GENERIC_READ *= #10000
--      which is a symptom of such that ought to be properly investigated and fixed, just not right now.
--
--      Longer term, I expect to add fatal error messages when encountering any tricky cross-compilation 
--      issues; inevitably it will not be long before some 64-bit-only code exists, that makes no sense 
--      to attempt creation of a 32-bit executable from, and I plan to err on the side of caution/blunt 
--      refusal to entertain cross-compilation (32<->64) whenever there is any doubt that it will work.
--
-- This particular routine is concerned with correctly tagging tokens found in the source code; the above
--  isFLOAT() is factored out to apply the same logic elsewhere, eg/ie as part of constant propagation.
--
    if isFLOAT(TokN) then
        toktype = FLOAT
    end if
end procedure

--with trace
procedure completeFloat()
-- TokN contains the integer (mantissa) part of the float.
-- next char should be one of .eE
-- return the float value.
-- Note that "1.0" is effectively treated as "1".
atom dec
--integer ndp -- number of decimal places
integer exponent
integer esigned
--  ndp=0
atom fraction
    if Ch='.' then
        toktype = DIGIBAD
--      dec = 10
        dec = 1
        fraction = 0
        while 1 do
            col += 1
            Ch = text[col]
--          if charset[Ch]!=DIGIT then exit end if
--26/6/10...
            if Ch!='_' then
                if Ch<'0' or Ch>'9' then exit end if
--              TokN = TokN*10 + Ch-'0'
--27/10/15:
--              TokN += (Ch-'0') / dec
                fraction = fraction*10+(Ch-'0')
                dec *= 10
                toktype = DIGIT
--              ndp += 1
            end if
        end while
        TokN += fraction/dec
    end if
    if toktype=DIGIBAD then Abort("illegal") end if
    exponent = 0
    if Ch='e' or Ch='E' then
        toktype = DIGIBAD
        esigned = 0
        while 1 do
            col += 1
            Ch = text[col]
            --DEV this should not really be in the loop, eg "2.4e--"
--          if charset[Ch]!=DIGIT then
            if Ch<'0' or Ch>'9' then
                if Ch!='_' then
                    if toktype!=DIGIBAD then exit end if -- ie first time round only
                    if Ch='-' then
                        esigned = 1
                    elsif Ch!='+' then
                        exit
                    end if
                end if
            else
                exponent = exponent*10 + Ch-'0'
                toktype = DIGIT
            end if
        end while
        if esigned then
            exponent = -exponent
        end if
--  end if
    if toktype=DIGIBAD then Abort("illegal") end if
--  exponent-=ndp
--  if exponent then
        if exponent>308 then
            -- rare case: avoid power() overflow
            TokN *= power(10, 308)
            if exponent>1000 then
                exponent = 1000 
            end if
            for i=1 to exponent-308 do
                TokN *= 10
            end for
        elsif exponent<0 then
            TokN /= power(10,-exponent)
        else
            TokN *= power(10, exponent)
        end if
    end if

end procedure

-- Technical note:  These values achieve near immortality; for example, suppose
-- ==============   you want \g to represent BEL (7), but, of course you cannot
--  (aka gotcha)    put '\g' yet, so instead put 'g' as a placeholder, you will 
--                  find it stays 'g' forever. Obviously pretty easy to fix and
--                  for the same reasons once fixed will stay fixed forever ;-).
--                  (eg/ie use \x07 for one round of self-hosting the compiler)

constant escchar = "nrtb\"\'\\0eE#xuU",
                --  1234 5 6 78901234
         escbyte = "\n\r\t\b\"\'\\\0\e\E"
                --   1 2 3 4 5 6 7 8 9 0

constant eschash = 11,
         eschex  = 12

constant AFaf = "ABCDEFabcdef"
constant bases = {8,16,2,10}    -- NB: oxbd order
integer base
        base = -1
integer bCh
        bCh = -1
integer prevCh
        prevCh = -1

procedure getByteWiseOctal()
-- specialised bytewise octal handling
-- eg 0ob377377377377 is the same as #FFFFFFFF
    col += 1
    Ch = text[col]
    -- groups of 3:
    while 1 do
        -- first char 0..3
        bCh = baseset[Ch]
        if bCh>3 then exit end if
        toktype = DIGIBAD
        TokN = TokN*4 + bCh
        col += 1
        Ch = text[col]
        -- second char 0..7
        bCh = baseset[Ch]
        if bCh>7 then exit end if
        TokN = TokN*8 + bCh
        col += 1
        Ch = text[col]
        -- third char 0..7
        bCh = baseset[Ch]
        if bCh>7 then exit end if
        TokN = TokN*8 + bCh
        col += 1
        Ch = text[col]
        toktype = DIGIT
    end while
    if toktype=DIGIBAD then Abort("bytewise octal must be in strict {377} format") end if
end procedure

--with trace
procedure loadBase()
    if base=5 then      -- 0(nn) case
        base = 0
        while 1 do
            col += 1
            Ch = text[col]
            if Ch<'0' or Ch>'9' then exit end if
            base = base*10 + Ch-'0'
        end while
--      if not find(base,bases) then
        if base<2 or base>36 then
            -- TIP:
            -- If you want to allow eg base 64 literals as eg 0(64)9QJZB3FX (terminating space?)
            --  Then 1) fill in 17..64 in baseset and/or use a basesetBig with 'a'..'f' not
            --  the same as 'A'..'F'. 2) add the base to bases. 3) think of a letter (but not a-f 
            --  please) to use as eg 0y9QJZB3FX and change find(Ch,"oxbd(") to find(Ch,"oxbdy(") 
            --  or if no such letter is needed, change to say find(Ch,"oxbdo("). 4) Apply to all
            --  four cases: 0(64)NNN, 0yNNN, #(64)NNN, and #yNNN, and test thoroughly!
            Abort("unsupported number base")
        end if
        if Ch!=')' then
            tokcol = col
            Abort("')' expected")
        end if
--      col += 1
--      Ch = thisline[col]?? 1/11/09
    else
        base = bases[base]
    end if
    col += 1
    Ch = text[col]
    toktype = DIGIBAD
    if base=8 and Ch='b' then
        getByteWiseOctal()
    else
        while 1 do
            bCh = baseset[Ch]
            if bCh>base then
--              if toktype!=DIGIT then exit end if
--              if base!=10 or Ch!='_' then exit end if     -- allow eg 1_000_000 to mean 1000000
                if Ch!='_' then exit end if     -- allow eg 1_000_000 to mean 1000000 (any base)
            else
                TokN = TokN*base + bCh
                toktype = DIGIT
            end if
            col += 1
            Ch = text[col]
        end while
        if toktype=DIGIBAD then Abort("missing digits") end if
    end if
    setFLOAT()
end procedure

--with trace
procedure MultiLineString(integer termch)
-- termch should be '`' for backtick handling 
--               or '\"' for triplequote handling
integer trimlength
--trace(1)
--DEV (spotted in passing) shouldn't this be doing some:
--                  if Ch='\n' then
--                      line += 1
--                  end if
    TokStr = ""
    toktype = DQUOTE
    trimlength = 0
    if charset[Ch]=EOL then
        col += 1
        while col<=ltl do
-- Added 4/7/13:
            if Ch='\n' then
                line += 1
            end if
            Ch = text[col]
            if charset[Ch]!=EOL then exit end if
            col += 1        
        end while
        while Ch='_' do
            trimlength += 1
            col += 1
            if col>ltl then exit end if
            Ch = text[col]
        end while
    end if
    while 1 do
        if Ch=termch then
            if Ch='`' then exit end if
            if col<ltl-2
            and text[col+1]='\"'
            and text[col+2]='\"' then
                col += 2
                exit
            end if
        end if
        col += 1
        if col>ltl then
            Abort("missing end quote")
        end if
        if Ch!='\r' then
            TokStr = append(TokStr,Ch)
            if Ch='\n' then
                for i=1 to trimlength do
                    Ch = text[col]
                    if charset[Ch]!=SPACE then exit end if
                    col += 1
--                  if col>ltl then exit end if
                    if col>ltl then
                        Abort("missing end quote")
                    end if
                end for
--Added 4/7/13:
                line += 1
            end if
        end if
        Ch = text[col]
    end while
    col += 1
    Ch = text[col]
--          if Ch='`' then
--              col += 1
--              Ch = text[col]
--              return
--          end if
--          TokStr = append(TokStr,Ch)
--          col += 1
--          Ch = text[col]
--          if col>=ltl or Ch='\n' or Ch='\r' then
--              Abort("missing end quote")
--          end if
--      end while
end procedure

--with trace
constant HBSize = {2,4,8}
procedure HexadecimalByteString()
--
-- Returns the equivalent of a DQUOTE, which pmain.e processes further.
-- It does however check for invalid characters and converts 'a'..f' to 'A'..'F'.
--
    toktype = HEXSTR
    TokN = HBSize[find(Ch,"xuU")]
    TokStr = ""
    col += 1
    while 1 do
        col += 1
        Ch = text[col]
        if col>=ltl or Ch='\n' or Ch='\r' then
            tokcol = col
            Abort("missing end quote")
        elsif Ch='\"' then
            col += 1
            Ch = text[col]
            return
        elsif Ch='\t'
           or Ch=' '
           or Ch='_' then
            Ch = ' '
        elsif Ch>='a'
          and Ch<='f' then
            Ch -= #20
        elsif Ch>'F'
           or Ch<'0'
           or (Ch>'9' and Ch<'A') then
            tokcol = col
            Abort("hex digit expected")
        end if
        TokStr = append(TokStr,Ch)
    end while
end procedure

integer preprocactive = 0

forward procedure preprocess()

include builtins\utfconv.e

--without trace
--global procedure getToken()
--global procedure getToken(bool parse_dot_as_symbol=false)
-- A parse_dot_as_symbol of true simply means the next token cannot legally be a
--  floating point number. It may or may not be a valid dot-subscript.
--  A default of true would probably be more sensible (long-term)
--  [DEV ==> bool float_valid=false] (strip defaults, then kill)
--with trace

global procedure getToken(bool float_valid=false)
--
-- A float_valid of true fairly obviously means that a float is valid; by default
--  any '.' are returned as a separate SYMBOL (for ORAC, ignored if that is 0).
--  I would expect the odd get_token(false) when simply being explicit, and most
--  certainly that or () after },),],",',`,and $, otherwise I would only expect a
--  getToken(true) after most of {,(,[,=,>,+,-,*,/,&,?,..,and,or,xor,and not (but 
--  not all, esp format directives, routine definitions, multi-assign lhs, enums, 
--  and #ilASM, #istype, #isinit, and #isginfo).
--
--integer savecol
--integer signed, toklen

    while 1 do
        toktype = charset[Ch]                   -- NB: idx out of bounds here (Ch=-1) 
                                                --      should be fixed by testing for
                                                --      that before the getToken() call,
                                                --      rather than here.
                                                --      if Ch is 0, it should be fixed 
                                                --      in loadFile [?].

        if toktype>SYMBOL then exit end if
        if toktype<=SPACE then
            col += 1
            if col>ltl then
                toktype = EOL
                eof_processing()    -- (sets Ch to -1 and clears activepaths)
                return
            end if
            if Ch='\n' then
                line += 1
            end if
            Ch = text[col]
--10/07/20
--      elsif Ch='-' then   -- check for comment
        elsif Ch!='-' and Ch!='/' then  -- check for comment
            exit
        else    
--printf(1,"getToken line 1920, Ch=%c, fileno=%d, col=%d, line=%d\n",{Ch,fileno,col,line})
--if line=14 then trace(1) end if
--if fileno=2 then ?9/0 end if
            cp1 = col+1
--          if text[cp1]!='-' then exit end if
            if text[cp1]!=Ch then
                if Ch!='/' then exit end if
                if text[cp1]!='*' then exit end if
                col = cp1
                SkipBlockComment()
            else
                cp2 = col+2
                Ch = text[cp2]
                cp3 = col+3
                -- check for "--/*" case:
--              if Ch='/' and text[cp3]='*' then
                if Ch='/' and text[cp3]='*' and text[cp1]='-' then
                    col = cp3
                    SkipBlockComment()
                elsif Ch='*' and text[cp3]='/' then
                    tokcol = col
                    tokline = line
                    Abort("unexpected end block comment")
                else
--26/7/17:
--/*
                    integer ipstart = 0
                    if Ch='#' 
                    and length(text)>col+16
                    and text[col..col+15]="--#include_paths"
                    and find(text[col+16]," \t") then
                        cp2 += 15 -- (ie col+17)
                        ipstart = cp2
                    end if
--*/
                    col = cp2
                    while Ch!='\n' do
                        col += 1
                        Ch = text[col]
                    end while
--/*
                    if ipstart!=0 then
                        string ip = text[ipstart..col-1]
                        ipstart = match("--",ip)
                        if ipstart then
                            ip = ip[1..ipstart-1]
                        end if
                        ip = get_proper_path(trim(ip),"")
--?{"#include_paths",ip}
                        addPath(ip)
                    end if
--*/
                    while Ch<=' ' do
                        if Ch='\n' then
                            line += 1
                        end if
                        col += 1
                        if col>ltl then
                            toktype = EOL
                            eof_processing()    -- (sets Ch to -1 and clears activepaths)
                            return
                        end if
                        Ch = text[col]
                    end while
                end if
            end if
--      else
--          if Ch!='/' then exit end if
--          cp1 = col+1
--          if text[cp1]!='*' then exit end if
--          col = cp1
--          SkipBlockComment()
        end if
    end while

    tokline = line
    tokcol = col
    tokno = 0
    if toktype=LETTER then
        if find(Ch,"xuU") and text[col+1]='\"' then
            HexadecimalByteString()
            return
        end if
        tt_search()
        if ttidx=T_end then
            if mapEndToMinusOne>0 and not ORAC then
                mapEndToMinusOne = T_end
                toktype = DIGIT
                TokN = -1
            end if
        elsif ttidx=T_ifdef and preprocactive=0 then
            preprocactive = 1
--          call_proc(r_preprocess,{})
            preprocess()
--          getToken()
            getToken(float_valid)
            preprocactive = 0
        end if
        return
    end if  -- toktype = LETTER
    prevCh = Ch
    col += 1
    Ch = text[col]
    if toktype=SYMBOL then
        -- !&*+,-./:;<=>?|
        -- note that most compound symbols such as "!=" 
        -- are handled in the parser as two tokens, or
        -- by testing the global Ch for (say) '='.
        if prevCh='.' then
            if Ch='.' then
                col += 1
                Ch = text[col]
                toktype = ELLIPSE
                return
--          elsif charset[Ch]=DIGIT then -- ".4" is a number
--DEV (use fromsubss or parse_dot_as_symbol?)
--          elsif Ch>='0' and Ch<='9' then -- ".4" is a number
--          elsif (not ORAC) and Ch>='0' and Ch<='9' then -- ".4" is a number
--DEV this may want to be (not ORAC or float_valid)  [spotted in passing, 27/3/17]
            elsif (ORAC and float_valid)
              and Ch>='0' and Ch<='9' then -- ".4" is a number
                col -= 1
                Ch = '.'
                TokN = 0
                completeFloat()     -- will set toktype to DIGIT
                setFLOAT()
                return
            end if
        elsif prevCh=':'
              and Ch=':' then
            col += 1
            Ch = text[col]
            toktype = LABEL
            return
        end if
        toktype = prevCh
        return
    elsif toktype=BRACES then
        -- []{}()
        toktype = prevCh
        return
    elsif toktype=DIGIT then
        -- 0..9
        -- note that eg "1.0" is treated as the integer "1"
        --
        TokN = prevCh-'0'

        if TokN=0 then  -- check for 0x/o/b/d formats
-- 't' added as octal to match Open Euphoria 13/12/2010
--          base = find(Ch,"oxbd(")
--          base = find(Ch,"toxbd(")
            base = find(lower(Ch),"toxbd(")
            if base then
                if base>1 then
                    base -= 1
                end if
                loadBase()
                return
            end if
        end if

        while 1 do
            if Ch<'0' or Ch>'9' then
                if Ch!='_' then exit end if     -- allow eg 1_000_000 to mean 1000000
            else
                TokN = TokN*10 + Ch-'0'
            end if
            col += 1
            Ch = text[col]
        end while
        cp1 = col+1
--DEV (ORAC/fromsubss)
--      if Ch='.' then
        if Ch='.' and (not ORAC or float_valid) then
            ch2 = text[cp1]
        else
            if Ch='\'' then
                if text[cp1]=TokN and text[col+2]='\'' then
                    col += 3
                    Ch = text[col]
                    return
                elsif text[cp1]='\\' and text[col+3]='\'' then
                    -- (ch2 used as a scratch var here)
                    ch2 = find(text[col+2],escchar)
                    ch2 = escbyte[ch2]
                    if ch2 and ch2=TokN then
                        col += 4
                        Ch = text[col]
                        return
                    end if
                end if
            end if
            ch2 = '.'
        end if
--      if (Ch='.' and thisline[cp1]!='.')      -- fraction but not ellipse
        if ch2!='.'                             -- fraction but not ellipse
        or (Ch='e' or Ch='E') then              -- exponent ahead
            completeFloat()
        end if
        setFLOAT()
        return
    elsif toktype=DQUOTE then
        TokStr = ""
        while 1 do
            if Ch='\"' then
                col += 1
                Ch = text[col]
                if Ch='\"' and length(TokStr)=0 then
                    col += 1
                    Ch = text[col]
                    MultiLineString('\"')
                end if
                return
            elsif Ch='\\' then
                col += 1
                Ch = text[col]
                Ch = find(Ch,escchar)
                if Ch=0 then
                    tokcol = col
                    Abort("unrecognised escape character")
                elsif Ch=eschash or Ch=eschex then -- (ie \# or \x)
                    -- inline hex byte (max 2 digits):
                    col += 1
                    Ch = text[col]
                    Ch = baseset[Ch]
                    if Ch>16 then
                        tokcol = col
                        Abort("hex digit expected")
                    end if
                    col += 1
                    bCh = text[col]
                    bCh = baseset[bCh]
--replaced 11/6/2013:
--                  if bCh>16 then
--                      tokcol = col
--                      Abort("hex digit expected")
--                  end if
--                  Ch = Ch*16+bCh
                    if bCh<=16 then
                        Ch = Ch*16+bCh
                    else
                        col -= 1
                    end if

--DEV \b should be backspace... (removed 11/6/2013)
--              elsif Ch=10 then    -- (ie \b)
--                  -- inline binary byte
--                  col += 1
--                  Ch = text[col]
--                  savecol = col
--                  if Ch<'0' or Ch>'1' then
--                      tokcol = col
--                      Abort("binary digit expected")
--                  end if
--                  TokN = Ch-'0'
--                  while 1 do
--                      col += 1
--                      Ch = text[col]
--                      if not find(Ch,"01") then exit end if
--                      TokN = TokN*2 + Ch-'0'
--                  end while
--                  if TokN>255 then
--                      tokcol = savecol
--                      Abort("inline binary byte may not exceed 255")
--                  end if
----?? this may help? (untried)
----                    Ch = and_bits(TokN,#FF)
--                  Ch = TokN
--                  col -= 1
                elsif Ch=13 or Ch=14 then -- (ie \u or \U)
if 1 then   -- new code (2/7/16)
--DOC: unicode characters are converted to their utf-8 equivalents. 
--  Note that invalid characters (>#10FFFF or #D800..#DFFF) are converted to "\#EF\#BF\#BD".
--  Don't expect console displays (?my_unicode_string) or debug/trace screens to be pretty.
--  When using pGUI, do not forget to add an IupSetGlobal("UTF8MODE","YES") at the start.
                    integer nchars = (Ch-12)*4  -- (4 or 8 digits)
                    Ch = 0
                    for i=1 to nchars do
                        col += 1
                        bCh = text[col]
                        bCh = baseset[bCh]
                        if bCh>16 then
                            tokcol = col
                            Abort("hex digit expected")
                        end if
                        Ch = Ch*16+bCh
                    end for
                    string utf8 = utf32_to_utf8({Ch})
                    Ch = utf8[$]
                    TokStr &= utf8[1..-2]
else -- old code
--DEV... (of course we could just return a dword-sequence here, which would need a new type
--        instead of DQUOTE and pmain.e to T_Dseq it instead if T_string it (tiny job), but
--        much more significantly heavy testing of puts() etc...)
                    tokcol = col
                    Abort("Sorry, Phix does not [yet] support 2 or 4 byte unicode characters in 8-bit strings.")
end if
                else
                    Ch = escbyte[Ch]
                end if
            elsif Ch='\t' then
                tokcol = col
                Abort("tab character found in string - use spaces or \\t instead")
            end if
            TokStr = append(TokStr,Ch)
            col += 1
            Ch = text[col]
            if col>=ltl or Ch='\n' or Ch='\r' then
                tokcol = col
                Abort("missing end quote")
            end if
        end while
    elsif toktype=BKTICK then -- string with no escape characters (nb tabs treated as-is!)
        MultiLineString('`')
        return
    elsif toktype=HEXDEC then
        -- #
        if Ch='i' then
            -- #ilASM{inline assembly},
            -- #isType{var_id,var_type}, or
            -- #isInit{var_id,0/1}.
--          toktype = ILASM
            return
--      elsif Ch='t' then   --#type_is construct
--          toktype = TYPEIS
--          return
        end if
--      base = find(Ch,"ooox(") -- Note: #b0101 and #d99 notations not supported (treated as 721153 & 3481)
--      if base then
--          loadBase()
--          return
--      end if
        toktype = DIGIBAD -- ensure followed by >=1 hex digit
        TokN = 0
--      toklen = 0
--      signed = 0
--      if find(Ch,"-+") then
--          signed = (Ch='-')
--          col += 1
--          Ch = text[col]
--      end if
--DEV for X64=1, this wants to optionally return {dword,dword} (toktype=DIGIT64?)
        while 1 do
--          if Ch=-1 then exit end if
--          if charset[Ch]!=DIGIT then
--          if Ch<'0' or Ch>'9' then
            if Ch<'0' then exit end if
            if Ch>'9' then
                if Ch!='_' then
                    prevCh = find(Ch,AFaf) 
                    if not prevCh then exit end if
                    if prevCh>6 then
                        prevCh += 3 -- (7..12, aka a..f, --> 10..15)
                    else
                        prevCh += 9 -- (1..6, aka A..F, --> 10..15)
                    end if
                    TokN = TokN*16 + prevCh
--                  toklen += 1
                end if
            else
                TokN = TokN*16 + Ch-'0'
--              toklen += 1
            end if
            toktype = DIGIT
            setFLOAT()
            col += 1
            Ch = text[col]
        end while
        if toktype=DIGIBAD then Abort("illegal") end if
-- removed 24/09/2013:
--      if signed then
--          if not find(toklen,{2,4,8}) then
----                if 64-bit then  --DEV more work/testing rqd
----                    if toklen!=16 then
----                        Abort("Signed hexadecimal literals must be length 2, 4, 8, or 16")
----                    end if
----                else
--                  Abort("Signed hexadecimal literals must be length 2, 4, or 8")
----                end if
--          end if
--          if toklen=2 then
--              if and_bits(TokN,#80) then TokN = or_bits(#FFFFFF00,TokN) end if
--          elsif toklen=4 then
--              if and_bits(TokN,#8000) then TokN = or_bits(#FFFF0000,TokN) end if
----            elsif toklen=8 then
----            elsif toklen=16 then
--          end if
--          TokN = and_bits(-1,TokN)
--      end if
        setFLOAT()
        return
    elsif toktype=SQUOTE then
        if Ch='\\' then
            col += 1
            Ch = text[col]
            Ch = find(Ch,escchar)
            if not Ch or Ch>eschex then
                tokcol = col
                Abort("unrecognised escape character")
            elsif Ch=eschash or Ch=eschex then -- (ie \# or \x)
                -- inline hex byte:
                col += 1
                Ch = text[col]
                Ch = baseset[Ch]
                if Ch>16 then Abort("hex digit expected") end if
                col += 1
                bCh = text[col]
                bCh = baseset[bCh]
                if bCh>16 then Abort("hex digit expected") end if
                Ch = Ch*16+bCh
            else
                Ch = escbyte[Ch]
            end if
        elsif Ch='\t' then
            tokcol = col
            Abort("tab character - use space or \\t instead")
        end if
        Tok9 = Ch   -- DEV use Ch! [DOH, we cannot as Ch is NEXT char!!!]
        col += 1
        Ch = text[col]
        if col>ltl or Ch!='\'' then
            tokcol = col
            Abort("missing end quote")
        end if
--      getCh()
        col += 1
        Ch = text[col]
        return
    elsif toktype=ILLEGAL and mapEndToMinusOne then
        if prevCh='$' then
            if mapEndToMinusOne=-1      -- from DoConstant
            or mapEndToMinusOne=-2 then -- from DoEnum
                mapEndToMinusOne = '$'
            end if
            if T_end='$' then ?9/0 end if   -- sanity check (T_end was 224, and '$'=36 last time I checked)
            toktype = DIGIT
            TokN = -1
            return
        elsif prevCh='%' 
          and mapEndToMinusOne=-3 then  -- from preprocess()
            -- (preprocess() scans the whole file, which may contain #ilasm
            --  constructs. Allows preprocess() to quietly skip eg %isVar)
            toktype = DIGIT
            TokN = -1
            return
        end if
--23/7/16:
    elsif prevCh=#1A then   -- Ctrl Z
        eof_processing()    -- (sets Ch to -1 and clears activepaths)
        return
    else
--DEV:::
--puts(1,"Illegal character (eof assumed):\n")
--?{toktype,Ch,col,line}
--Ch=-1
--DEV:
--  if toktype=LETTER then
----        charset['\\']=ILLEGAL -- reset following include processing
----        skipSpaces()
--  end if
--  if toktype=ILLEGAL then
            Abort("illegal character")
----            illcheck()
--      end if
--  end if              
    end if
end procedure

--global procedure Semi()
---- Match a Semicolon
--  if toktype=';' then getToken() end if
--end procedure

--global procedure MatchChar(integer x)
--global procedure MatchChar(integer x, bool parse_dot_as_symbol=false)
-- A parse_dot_as_symbol of true simply means the next token cannot legally be a
--  floating point number. It may or may not be a valid dot-subscript.

global procedure MatchChar(integer x, bool float_valid=false)
--
-- Match a Specific Input Char, typically a symbol eg ')'
-- NB: This is only ever called when you already know the currrent token will match
--     It also makes the code a bit more self-documenting
--
-- By default float_valid is false ('.' treated as a separate SYMBOL) [for ORAC]
-- Typically I would expect:
--      MatchChar('=',true)     -- (ditto +-*/&)
--      MatchChar('{',true)     -- (except lhs of multi-assign)
--      MatchChar('(',true)     -- (on calls not definitions)
--      MatchChar(',',true)     --  "",""
--      MatchChar('[',true)
--      MatchChar('.',false)    -- (for ORAC)
--      MatchChar(')'[,false])
--      MatchChar('}'[,false])
--
--  if not equal(Ch,x) then Expected('\''&x&'\'') end if
    if not equal(toktype,x) then Expected('\''&x&'\'') end if
--  getToken()
    getToken(float_valid)
end procedure

--global procedure MatchString(integer T_ident)
--global procedure MatchString(integer T_ident, bool parse_dot_as_symbol=false)
-- A parse_dot_as_symbol of true simply means the next token cannot legally be a
--  floating point number. It may or may not be a valid dot-subscript.

global procedure MatchString(integer T_ident, bool float_valid=false)
--
-- Match a Specific Input String
-- NB: This is only ever called when you already know the currrent token will match,
--     or when it is mandatory, eg as per 'then' in 'if then else end if' 
--     It also makes the code a bit more self-documenting
--
-- By default float_valid is false ('.' treated as a separate SYMBOL) [for ORAC]
-- Typically I would expect:
--  MatchString(T_xxx,true) on T_and, T_or, T_xor, T_if, T_while, T_to, T_return,
--  T_switch, and T_case, with MatchString(T_xxx[,false]) on all other T_xxx and
--  some T_if, T_while, and T_return as appropriate.
--
    if toktype!=LETTER 
    or ttidx!=T_ident then
        Expected('\"'&getname(T_ident,-2)&'\"')
    end if
    getToken(float_valid)
end procedure

--with trace
-- routines for preprocess():
procedure KillString(integer T_ident)
    for i=tokcol to col-1 do
        text[i] = ' '
    end for
    if T_ident!=ttidx then
        allpfiles[fileno] = text
    end if
    MatchString(T_ident)
end procedure

procedure KillChar(integer x)
    text[tokcol] = ' '
    if toktype!=x then
        allpfiles[fileno] = text
    end if
    MatchChar(x)
end procedure

procedure KillToken()
    for i=tokcol to col-1 do
        text[i] = ' '
    end for
    getToken()
end procedure

procedure Aborpp(sequence msg)
--  if probable_logic_error then show_ple() end if      --DEV??
    allpfiles[fileno] = text
    Abort(msg)
end procedure

--with trace
procedure process_one_ifdef()
integer state = 0   -- 0 = normal start, looking for a branch to keep
                    -- 1 = suppress remaining elsifdef/elsedef/end ifdef
integer negate, nest, flag, thisflag, andor
integer wastokcol, wascol
sequence name

--trace(1)
    andor = 0
    while Ch>0 do
        if ttidx=T_ifdef
        or ttidx=T_elsifdef then
            KillString(ttidx)
            while Ch>0 do   -- process any and/or in the tests
                negate = 0
                if toktype='!' then
                    negate = 1
                    KillChar('!')
                elsif toktype=LETTER and ttidx=T_not then
                    negate = 1
                    KillString(T_not)
                end if
                if toktype!=LETTER then
                    Aborpp("a name is expected here")
                end if
                if ttidx=T_WIN32
                or ttidx=T_WINDOWS then
--                  thisflag = 1    -- DEV (PE==1 && X64==0)
                    thisflag = PE
                elsif ttidx=T_LINUX
--                 or ttidx=T_FREEBSD
--                 or ttidx=T_SUNOS
--                 or ttidx=T_OPENBSD
--                 or ttidx=T_OSX
                   or ttidx=T_UNIX then
--                  thisflag = 0    -- DEV (PE==0?)
                    thisflag = not PE
                elsif ttidx=T_WIN32_GUI
                   or ttidx=T_WIN32_CONSOLE then
                    thisflag = OptConsole
                    if thisflag=-1 then
                        --DEV until I put in an opcode for setting this depending on
                        --    whether p.exw or pw.exe (or some renamed copy of either)
                        --    is running, you must first explicitly specify one of
                        --    with/without console/gui before you can use the above.
                        --OR, we can call pemit.e/readAllHeaders()/getmzpe(#DC, WORD)
                        --    and "thisflag = (res=CUI)" (where CUI=3).
                        Aborpp("with/without console/gui must be explicitly specified first")
--                  else OptConsole is 0=gui, 1=console...
                    end if
                    if ttidx=T_WIN32_GUI then
                        thisflag = not thisflag
                    end if
                elsif ttidx=T_SAFE
                   or ttidx=T_DATA_EXECUTE
                   or ttidx=T_UCSTYPE_DEBUG
                   or ttidx=T_EU4_0
                   or ttidx=T_EU4_1
                   or ttidx=T_OSX
                   or ttidx=T_FREEBSD
                   or ttidx=T_SUNOS
                   or ttidx=T_OPENBSD
                   or ttidx=T_CRASH then
                    thisflag = 0
                elsif ttidx=T_BITS32 then
--                  thisflag = machine_bits()=32
                    thisflag = not X64
                elsif ttidx=T_BITS64 then
                    thisflag = X64
                elsif ttidx=T_PHIX then
                    thisflag = 1
                else
                    Aborpp("unrecognised")
                end if
                if negate then
                    thisflag = not thisflag
                end if
                KillToken()
                if andor=0 then -- neither encountered yet
                    flag = thisflag
                    if toktype!=LETTER then exit end if
                    if ttidx=T_and then
                        andor = 1
                    elsif ttidx=T_or then
                        andor = 2
                    else
                        exit
                    end if
                else
                    if toktype!=LETTER then exit end if
                    if andor=1 then -- we've had an and already
                        flag = flag and thisflag
                        if ttidx!=T_and then exit end if
                    elsif andor=2 then  -- we've had an or already
                        flag = flag or thisflag
                        if ttidx!=T_or then exit end if
                    else
                        ?9/0
                    end if
                end if
                KillToken() -- discard the T_and/T_or
            end while
            KillString(T_then)
        elsif ttidx=T_elsedef then
            flag = 1
            KillString(T_elsedef)
        else
            Aborpp("ifdef/elsifdef/elsedef expected\n") --???
        end if
        if flag and state=0 then
            -- keep this branch
            while Ch>0 do
                if toktype=LETTER then
                    if ttidx=T_end then
                        wastokcol = tokcol
                        wascol = col
                        MatchString(T_end)
                        if ttidx=T_ifdef then
                            for i=wastokcol to wascol-1 do
                                text[i] = ' '
                            end for
                            KillToken()
                            return      -- ALL DONE!
                        end if
                    elsif ttidx=T_elsifdef
                       or ttidx=T_elsedef then
                        exit
                    elsif ttidx=T_ifdef then
                        process_one_ifdef()
                    elsif ttidx=T_include then
                        -- avoid any '\' illegal char errors
                        name = getIncludeLine()
                    end if
                end if
                getToken()
            end while
            state = 1
        else
            -- wipe this branch (and any nested ifdefs)
            nest = 0
            while Ch>0 do
                if toktype=LETTER then
                    if ttidx=T_end then
                        KillString(T_end)
                        if ttidx=T_ifdef then
                            if nest=0 then exit end if
                            nest -= 1
                        end if
                    elsif ttidx=T_elsifdef
                       or ttidx=T_elsedef then
                        if nest=0 and state=0 then exit end if
                    elsif ttidx=T_ifdef then
                        nest += 1
                    elsif ttidx=T_include then
                        -- avoid any '\' illegal char errors
                        wastokcol = tokcol
                        name = getIncludeLine()
                        tokcol = wastokcol
                    end if
                end if
                KillToken()
            end while
            if ttidx=T_ifdef then
                KillToken()
                return      -- ALL DONE!
            end if
        end if
    end while
    Aborpp("\"end ifdef\" expected")
end procedure

procedure preprocess() -- called from getToken()
--
-- Converts eg "ifdef WIN32 then puts(1,"WIN32") elsedef puts(1,"NOT WIN32") end ifdef"
--          to "                 puts(1,"WIN32")                                      "
--
-- Linebreaks and comments are preserved. The result is stored in allpfiles, with the
--  original still in allfiles. Obviously further parsing occurs on the allpfiles
--  version but any errors/warnings/listings work off the allfiles original.
-- 
-- NB this is the **BARE MINIMUM NECESSARY** to support RDS Eu4 std/ files...
-- 
-- Specifically:
-- ============
--  WIN32/WINDOWS are assumed to be TRUE/DEFINED,
--  LINUX/FREEBSD/SUNOS/OPENBSD/OSX/UNIX are assumed FALSE/UNDEFINED [DEV?]
--  WIN32_GUI/WIN32_CONSOLE are dependent on with/without console/gui,
--      and /NOT/ on whether p.exe or pw.exe is running (**NB**). [DEV]
--  SAFE/DATA_EXECUTE/UCSTYPE_DEBUG/EU4_1/CRASH are assumed FALSE/UNDEFINED.
--  everything else is assumed FALSE/UNDEFINED.
--  
-- I am certainly no fan of ifdef. The popular argument that eg:
--
--      if platform()=WIN32 then
--          puts(1,"This is win32\n")
--      else
--          puts(1,"This is NOT win32\n")
--      end if
--
--  is somehow not as good as:
--
--      ifdef WIN32 then
--          puts(1,"This is win32\n")
--      elsedef
--          puts(1,"This is NOT win32\n")
--      end ifdef
--
--  is just nonsense. Try "-d"'ing the above snippets and you'll see.
--  It is of course far better to improve the compiler/language at a 
--  fundamental level than paper over cracks or add ugly warts!
--
--  Worse:
--      ifdef WIN32 then
--          include win32version.e
--      elsedef
--          include nonwin32version.e
--      end ifdef
--  is just weak-minded. If you are going to be cross-platform, then 
--  includes should be cross-platform! If anyone wants to use those
--  files, they are forced to type out those five lines, not just one.
--  Also, if anyone fixes a bug in one of those files, they are much
--  less likely to apply the equivalent fix in the other...
--
-- My other argument against ifdef is that things end up being defined 
--  in batch files (ie another language), config files (not a language)
--  or worse on the command line, generally unavailable at run-time and
--  nothing short of a blatent incitement to make packaging blunders.
--
-- Anyway, that's my rant for the day.
--
-- Actually, one case has cropped up which /is/ useful:
--
--      ifdef PHIX then
--          ...
--      elsedef
--          ...
--      end ifdef
--
--  which works out quite well (with absolutely no changes to OE).
--
string name
integer wasline, wascol
integer wasmapEndToMinusOne
--12/11/15: (either this or use a flag to switch off eof_processing)
sequence was_activepaths = activepaths

--trace(1)
    wascol = col
    wasline = line
    wasmapEndToMinusOne = mapEndToMinusOne
--DEV should this be inside the loop??
    mapEndToMinusOne = -3 -- treat '$' and '%' as normal symbols
    
    -- get the clone over and done with:
    text[col-1] = ' '

--?"preprocess!\n"
    while Ch!=-1 do -- keep going until end of file
--trace(1)
        process_one_ifdef()
        -- and check remainder of file...
        while Ch>0 do
--          getToken()
            if toktype=LETTER then
                if ttidx=T_end then
                    MatchString(T_end)
                    if ttidx=T_ifdef then Aborpp("no matching ifdef") end if
                elsif ttidx=T_elsifdef
                   or ttidx=T_elsedef then Aborpp("no matching ifdef")
                elsif ttidx=T_ifdef then
                    exit
                elsif ttidx=T_include then
                    name = getIncludeLine()
                end if
            end if
            getToken()
        end while
    end while       

    allpfiles[fileno] = text

    mapEndToMinusOne = wasmapEndToMinusOne
    col = wascol
    line = wasline
    Ch = ' '
--12/11/15:
    activepaths = was_activepaths

end procedure
--r_preprocess = routine_id("preprocess")


