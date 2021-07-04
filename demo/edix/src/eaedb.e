--
-- eaedb.e
--
-- This handles all traffic to/from edix.edb
--
-- **NO other programs in edix should update or directly read that file.**
--
without trace
--with trace

--/**/include builtins\database.e
--/**/include builtins\pgetpath.e
--/**/include builtins\pdir.e

--/* 4.0.2
include builtins\file.e
--*/

--/*
--include builtins\database.e
--*/

constant edixEDB = "edix.edb"

sequence dbversion
--global integer needVedb
--             needVedb = 0

constant Tversion       = "version",
         Tdirectories   = "directories",
         Tfiles         = "files",
         Tglobals       = "globals",
         Tfolds         = "folds",  -- and bookmarks
--DEV to go*2:
--       Tbackups       = "backups",
--       Tprojects      = "projects",   -- unused (DEV)
--DEV ini file setings?
         Tmacros        = "macros",
         Textset        = "extset",     -- added 19/6/07
         Tfindif        = "findif"      -- added 10/12/17

constant Tset=
{Tversion,      -- added 11/8. Value is {0,2,0} [not necc. in step with eaversion]
 Tdirectories,  -- Key is path, data is uniq. DO NOT DELETE (or set data to -1 and clean up later)
 Tfiles,        -- Key is {uniq[dir],file}, data is {uniq,size,filedate,c,i,b}. DO NOT DELETE ("")
                -- c is capture console flag, i for includeset, and b for includedby.
                -- 1/5/07: if length(data) is 7, then data[7]=isFTP.
 Tglobals,      -- Key is name (eg "db_open"), data is {uniq[file]}
                --  eg {7}, or {7,9,10} if the global is defined in three files.
 Tfolds,        -- copy of bookmarks[currfile], with line text appended. Key is fileuniq.
-- Tbackups,        -- Key is uniq[file], data is {int9} [eg 512312359 for "5C312359.exw"]
                --  -extension is determined from Tfiles[key][2].
-- Tprojects,   -- Key is uniq, data is {name,desc,date,type,fileset}          [***UNUSED***]
 Tmacros,       -- Key is text, data is {{type,details}}
 Textset,       -- Key is extension, data is {runwith,tabcolour}
 Tfindif}       -- Key is {1,dir} or {2,ext} or {3,"flags"}, data is 0/1 or {flags}

global --DEV for isShowBGprogress
integer gMFstate        -- getModifiedFile state flag/record no:
        gMFstate = 0    -- 0=load, then cycles 1..max(knownFiles).


constant RETRY=0, CRASH=1, CRASHALWAYS=2 -- params for DBfatal

procedure DBfatal(sequence msg, integer crashflag)
--integer b
----    trace(1) --DEV re-insert this if you get PC hangs.
--  b = MB_OK
--  if crash=CRASH then
--      b = MB_OKCANCEL
--  end if
--  if proemh("Error in eaedb.e",msg,b)=IDOK then
    if IupAlarm("Error in eaedb.e",msg,"OK",iff(crashflag=CRASH?"Cancel":NULL))=1 then
        if crashflag!=RETRY then ?9/0 end if
    end if
    if crashflag=CRASHALWAYS then ?9/0 end if
end procedure

integer isOpen
        isOpen = 0

--/* Not required for Positive
constant M_SLEEP = 64   -- as per misc.e
--*/

global integer alreadyBackGroundProcessing
               alreadyBackGroundProcessing = 0

procedure DBopen()
-- open edix.edb 
-- Warning: calls to doEvents()/DoEvents() from here seem to cause major problems!
integer errCode, retries
--sequence winTxt
atom t
    if not isOpen then
--isOpen=-1
        retries = 0
--/**/  sleep(0)                    --/*                                -- Positive
        machine_proc(M_SLEEP, 0)    -- sleep(0), but misc.e checks t>0  -- RDS --*/
        t = time()+1
        while 1 do
            errCode = db_open(initialcurrentdir&edixEDB,DB_LOCK_EXCLUSIVE)
            if errCode = DB_OK then exit end if
            if retries>4 then
                DBfatal(sprintf("open %s%s [%d]",{initialcurrentdir,edixEDB,errCode}),RETRY)
            else
                retries += 1
--DEV
--/*
                winTxt = IupGetAttribute(dlg,"TITLE")
                IupSetStrAttribute(dlg,"TITLE",xl("Edix") & " - DATABASE LOCKED")
                sleep(1)
                setText(Main,winTxt)
--*/
?"DATABASE LOCKED"
                sleep(1)
            end if
        end while
        isOpen = 1
--elsif isOpen=-1 then
--  trace(1)
--  ?1234
    end if
end procedure

global procedure DBclose()
    if isOpen=1 then
        db_close()
        isOpen = 0
    end if
end procedure

procedure SelectTable(sequence name)
    DBopen()
    if db_select_table(name)!=DB_OK then
        DBfatal("db_select_table "&name,CRASHALWAYS)
    end if
end procedure


global --used by loadRecoveryDetails
sequence knownDirectories
         knownDirectories = {}

--DEV one hit, one multiple of 32....
procedure bulkUpkD(integer k)
    while length(knownDirectories)<k do
        -- bulk up table to next multiple of 32
        knownDirectories &= repeat(0,32)
    end while
end procedure

--used by loadRecoveryDetails:
global function getDir(integer n)
-- return the actual text of the directory path.
object r
integer k
    bulkUpkD(n)
    r = knownDirectories[n]
    if atom(r) then
        SelectTable(Tdirectories)
        for i=1 to db_table_size() do
            k = db_record_data(i)
            bulkUpkD(k)
            if atom(knownDirectories[k]) then
                knownDirectories[k] = db_record_key(i)
            end if
--      if alreadyBackGroundProcessing then doEvents(0) end if  -- no help
        end for
        r = knownDirectories[n]
        SelectTable(Tfiles)
    end if
    return r
end function

global function getDirFiles(integer n, integer any)
-- used by eadir, for tree load.
-- Returns all known files in specified directory (if any=False).
-- Returns True if one or more file exists (if any=True)
sequence res, key, data
integer rn
    res = {}
    SelectTable(Tfiles)
    rn = -db_find_key({n,0})
    while rn<=db_table_size() do
        key = db_record_key(rn)
        if key[1]!=n then exit end if   -- different directory
        if any then return True end if
        data = db_record_data(rn)
        key = append(key,data[1])
        res = append(res,key)
        rn += 1
    end while
    if any then return False end if
    return res
end function

sequence knownFiles
         knownFiles = {}

procedure bulkUpkF(integer k)
    while length(knownFiles)<k do
        -- bulk up table to next multiple of 32
        knownFiles &= repeat(0,32)
    end while
end procedure

-- used below and by eaqj.ew:
global function getNameAndDir(integer n, integer expandDir, integer closeDB)
-- convert a unique file no (as found within the fileset of a global, for 
-- example) into the file directory and name.
-- expandDir is normally 1 for all display/comparison purposes, but if the
--  result is to be used to read the files table, it should be 0.
-- closeDB is normally 1 except during the project build scan, which calls
--  DBclose() at the end.
object r
integer k

    if n=-1 then    -- load req from getModifiedFile()
        r = 0
    else
        bulkUpkF(n)
        r = knownFiles[n]
    end if
    if atom(r) then
        SelectTable(Tfiles)
        for i=1 to db_table_size() do
            r = db_record_data(i)   -- {uniq,size,filedate,s,i,b}
            k = r[1]
            bulkUpkF(k)
            if atom(knownFiles[k]) then
                r = db_record_key(i)    -- {uniq[dir],file}
                knownFiles[k] = r
                if k=n then exit end if
            end if
--      if alreadyBackGroundProcessing then doEvents(0) end if  -- no help
        end for     
    end if
    if expandDir then
        r[1] = getDir(r[1])
    end if
    if closeDB then
        DBclose()
    end if
    return r
end function

integer showErrors
        showErrors = 1

function getDirNo(sequence directory)
-- add directory to the list of known directories, and the euphoria database if rqd
integer k
sequence r

--  if directory[length(directory)]!='\\' then trace(1) end if
    k = find(directory,knownDirectories)
    if k=0 then
        SelectTable(Tdirectories)
        k = db_find_key(directory)
        if k>0 then 
            -- already known about, just not opened this session
            k = db_record_data(k)
        else
            if showErrors then
                r = get_proper_path(directory,"")
                if not equal(r,directory) then
--                  if proemh("LogFile error","Trigger crash?",MB_OKCANCEL)=IDOK then
                    if IupAlarm("LogFile error","Trigger crash?","OK","CANCEL")=1 then
                        ?9/0
                    end if
                end if
                -- first one only (avoids making Edix unusable)
                showErrors = 0
            end if
            k = db_table_size()+1
            if db_insert(directory,k)!=DB_OK then
                DBfatal("error inserting record",CRASH)
                return 0
            end if
        end if
        bulkUpkD(k)
        knownDirectories[k] = directory
    end if
    return k
end function
--without trace

global function logFile(sequence path, sequence name, integer Tg)
-- this is called every time we open or save a file.
-- Tg is 1 if we should reselect Tglobals rather than close the db on exit
-- Tg is 2 for project build (so don't close the db on exit)
-- (? Tg can also be 0??)
--
integer k, f
--sequence sdt  -- size and date info
sequence r
--  if needVedb then
--      -- 0.2.0
--      path = upper(path)
--      name = lower(name)
--  else
        -- 0.3.3 and later
        if showErrors then
            r = get_proper_path(name,path)
            if not equal(path&name,r) then
--              if proemh("LogFile error","Trigger crash?",MB_OKCANCEL)=IDOK then
                if IupAlarm("LogFile error","Trigger crash?","OK","CANCEL")=1 then
                    ?9/0
                end if
            end if
            -- first one only (avoids making Edix unusable)
            showErrors = 0
        end if
--  end if
    k = getDirNo(path)
    f = find({k,name},knownFiles)
    if f=0 then
        SelectTable(Tfiles)
        f = db_find_key({k,name})   -- key is {uniq[dir],file}
        if f<=0 then
--      sdt=dir(path&name)
--      sdt=sdt[1][D_SIZE..D_SECOND]
            f = db_table_size()+1
--      if db_insert({k,name},{f,sdt[1],sdt[2..length(sdt)],0,0,0})!=DB_OK then
            if db_insert({k,name},{f,0,{},0,0,0})!=DB_OK then
                IupMessage("Error","error inserting record")
                return 0
            end if
        else
            r = db_record_data(f)   -- {uniq,size,filedate,s,i,b}
            f = r[1]
        end if
        bulkUpkF(f)
--DEV ioob here:
--index 409 out of bounds, assigning to sequence length 64
--  f = 409
--  knownFiles = {{1, "elng_ENG.exh"}, 
--                  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
--                0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
--                0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
--                0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0}
--  r = {409,0, {}, 0,0,0}
--
        knownFiles[f] = {k,name}
    end if
    if Tg=1 then
        SelectTable(Tglobals)
    elsif Tg!=2 then
        DBclose()
    end if
    return f
end function

--with trace
--/*
global procedure markAsFTP()
-- tag the current file as "Edit with Edita"
integer f, k
sequence d
--object dbg
--trace(1)
    f = logFile(filepaths[currfile],filenames[currfile],2)
    SelectTable(Tfiles)
--dbg = knownFiles[f]
    k = db_find_key(knownFiles[f])
    d = db_record_data(k)
    if length(d)>=7 then
        d[7] = isFTP
    else
        d &= isFTP
    end if
    db_replace_data(k,d)
    DBclose()
end procedure

global function needsFTPing(integer f)
integer k
sequence d
    SelectTable(Tfiles)
    k = db_find_key(knownFiles[f])
    d = db_record_data(k)
    DBclose()
    if length(d)>=7
    and d[7] then   -- isFTP
        return 1
    end if
    return 0
end function

global function getPrevFTPfiles()
sequence d, res
    SelectTable(Tfiles)
    res = {}
    for f=1 to db_table_size() do
        d = db_record_data(f)
        if length(d)>=7 and d[7] then
            d = db_record_key(f)
--          res = append(res,{d[2],getFileExtension(d[2]),getDir(d[1]),-1,-1,length(res)+1})
            res = append(res,{d[2],get_file_extension(d[2]),getDir(d[1]),-1,-1,length(res)+1})
        end if
    end for
    DBclose()
    return res
end function
--*/

object guniq
sequence gtkey
global integer gsize
global sequence gdate
global sequence gpath -- added 15/11
--global string gpath -- added 15/11

global procedure initForGlobalLogging(sequence path, sequence name)
-- prepare db for adding/updating a bunch of global symbols.
integer k, f
object datum
sequence r
--string r
--  if needVedb then
--      gpath = upper(path)
--      name = lower(name)
--  else
        gpath = path
        if showErrors then
            r = get_proper_path(name,path)
            if not equal(path&name,r) then
--              if proemh("initForGlobalLogging error","Trigger crash?",MB_OKCANCEL)=IDOK then
                if IupAlarm("initForGlobalLogging error","Trigger crash?","OK","CANCEL")=1 then
                    ?9/0
                end if
            end if
            -- first one only (avoids making Edix unusable)
            showErrors = 0
        end if
--  end if
    guniq = 0
    k = getDirNo(gpath)
    gtkey = {k,name}
    SelectTable(Tfiles)
    f = db_find_key({k,name}) -- key is {uniq[dir],file}
    if f<=0 then
?"file unrecognised eaedb.e line 444"
--      DBfatal("file unrecognised",CRASH)
        return
    end if
    datum = db_record_data(f)   -- {uniq,size,filedate,s,i,b}
    guniq = datum[1]
    gsize = datum[2]    -- it's more important when these are updated by backGroundProcessing()
    gdate = datum[3]    --      ""
    SelectTable(Tglobals)
--NB: DB is left open...
end procedure

sequence globalCache, globalFsets

global procedure clearGlobalCache()
-- also called as part of (vedb-invoked) full background scan.
    globalCache = {}
    globalFsets = {}
end procedure
clearGlobalCache()

global procedure addGlobal(sequence name)
--
-- Note that Tglobals is assumed selected, 
--  (initForGlobalLogging should just have been called).
--
integer gci, f
sequence fileset
    if guniq then
        gci = find(name,globalCache)
        if gci then
            fileset = globalFsets[gci]
            if not find(guniq,fileset) then
                globalFsets[gci] = 0    -- refcount down
                fileset = append(fileset,guniq)
                globalFsets[gci] = fileset
                f = db_find_key(name)
                if f>0 then --uh?
                    db_replace_data(f,fileset)
                end if
            end if
        else
            f = db_find_key(name)
            if f<0 then
                fileset = {guniq}
                if db_insert(name,fileset)!=DB_OK then ?9/0 end if
            else
                fileset = db_record_data(f)
                if not find(guniq,fileset) then
                    fileset = append(fileset,guniq)
                    db_replace_data(f,fileset)
                end if
            end if
--DEV ripplesort into position...
            globalCache = append(globalCache,name)
            globalFsets = append(globalFsets,fileset)
        end if
    end if
--NB: DB is left open...
end procedure


global integer rebuildProjectSet    -- set by eamenus to force rebuild
               rebuildProjectSet = 0


--used by eaqj:
global procedure updateIncSet(sequence incset)
--
-- final part of background processing.
--  (initForGlobalLogging should just have been called).
--
integer f, k
sequence data, fkey, r
    SelectTable(Tfiles)
    f = db_find_key(gtkey)  -- may have moved!  Key is {uniq[dir],file}
    if f>0 then
        data = db_record_data(f)        -- {uniq,size,filedate,s,i,b}
        if not equal(data[5],incset)
        or not equal(data[2],gsize)
        or not equal(data[3],gdate) then
            data[2] = gsize
            data[3] = gdate
            if not equal(data[5],incset) then
                checkProj = 1   -- force repaint
                rebuildProjectSet = 1 -- force reload when repainting
            end if
            data[5] = incset
            db_replace_data(f,data)
            -- and update the included by sets to match...
            SelectTable(Tfiles)
            for i=1 to length(incset) do
                fkey = getNameAndDir(incset[i],0,0)
                k = db_find_key(fkey) -- Key is {uniq[dir],file}
                r = db_record_data(k) -- {uniq,size,filedate,s,i,b}
                if atom(r[6]) then
                    r[6] = {guniq}
                    db_replace_data(k,r)
                elsif not find(guniq,r[6]) then
                    r[6] = r[6]&guniq
                    db_replace_data(k,r)
                end if
            end for
--DEV what about files which are no longer included...
        end if
    end if
--  DBclose()   -- not needed, done in eaqj next.
end procedure


global function findGlobal(sequence name)
integer f
--integer low,high,mid
object res
    f = find(name,globalCache)
--  f = 0
--  low = 1
--  high = length(globalCache)
    if f=0 then
        SelectTable(Tglobals)
        f = db_find_key(name)
        if f<=0 then
--          res = 0 --DEV...
            res = {}
        else
            res = db_record_data(f)
        end if
        globalCache = append(globalCache,name)
        globalFsets = append(globalFsets,res)
        DBclose()
    else
        res = globalFsets[f]
    end if
    return res
end function
r_findGlobal = routine_id("findGlobal") -- for easynclr.e

global procedure ScratchGlobal(sequence name, integer badun)
-- the routine name is no longer defined in file badun.
--  (or perhaps it is just no longer defined as global)
integer f,k
object res
    SelectTable(Tglobals)
    f = db_find_key(name)
    res = db_record_data(f)
    k = find(badun,res)
    res = res[1..k-1]&res[k+1..length(res)]
    if length(res) then
        db_replace_data(f,res)
    else
        db_delete_record(f)
    end if
    DBclose()
    f = find(name,globalCache)
    globalFsets[f] = res
end procedure


global function getCaptureFlag(sequence path, sequence name)
integer k, f
sequence data
--  if needVedb then
--      path = upper(path)
--      name = lower(name)
--  end if
    k = getDirNo(path)
    SelectTable(Tfiles)
    f = db_find_key({k,name}) -- key is {uniq[dir],file}
    data = db_record_data(f)    -- {uniq,size,filedate,s,i,b}
    DBclose()
    return data[4]
end function

global procedure setCaptureFlag(sequence path, sequence name, object flag)
integer k, f
sequence data
--  if needVedb then
--      path = upper(path)
--      name = lower(name)
--  end if
    k = getDirNo(path)
    SelectTable(Tfiles)
    f = db_find_key({k,name}) -- key is {uniq[dir],file}
    data = db_record_data(f)    -- {uniq,size,filedate,s,i,b}
    data[4] = flag
    db_replace_data(f,data)
    DBclose()
end procedure

--global function loadProjects()
--sequence projects
--  SelectTable(Tprojects)
--  projects = repeat(0,db_table_size())
--  for i=1 to length(projects) do
--      projects[i] = db_record_data(i)&i   --DEV re-do this with tedb numbering system.
--  end for
--  return projects
--end function

--global function writeProject(sequence record)
--integer k
--  SelectTable(Tprojects)
--  k = db_table_size()+1
--  if db_insert(k,record)!=DB_OK then
--      --DEV DBfatal?
----        void = proemh("Error","error inserting record",0)
--      IupMessage("Error","error inserting record")
--      return 0
--  end if
--  return k
--end function

-- used by eadir.e:
global function inProjectSet(sequence path, sequence name, sequence set)
    for i=1 to length(set) do
        if equal(path,set[i][3])
        and equal(name,set[i][2])
        and (set[i][4]!=2 or not equal(set[2][2],"..")) then
            return 1
        end if
    end for
    return 0
end function

function extendProject(sequence res, integer parent, object includeset, integer recurse)
sequence r, fkey
object dname
integer k
    if sequence(includeset) then
        for i=1 to length(includeset) do
            fkey = getNameAndDir(includeset[i],0,0) -- don't expand dir, don't close DB. fkey is {n,"filename")
            dname = getDir(fkey[1])
            if sequence(dname) then -- corruption check
                if not inProjectSet(dname,fkey[2],res) then -- not already included
                    k = db_find_key(fkey)
                    r = db_record_data(k)       -- {uniq,size,filedate,s,i,b}
                    res[parent][5] = 1          -- parent has children
                    res = append(res,{0,fkey[2],dname,parent,0})
                    if recurse then -- don't recurse the .. entries
                        res = extendProject(res,length(res),r[5],1)
                    end if
                end if
            end if
        end for
    end if
    return res
end function

--with trace
global function getProjectSet(sequence path, sequence name)
--
-- Builds the treeview info for the File Panel/Project tab.
-- Each entry is {handle, name, path, image, hasChildren}, where
--  handle is 0 here (saved when we addTVItem),
--DEV??
--  name is converted to/stored in lowercase, eg "edita.exw",
--  path is converted to/stored in uppercase, eg `D:\EDITA\`,
--  the root parent is 0,
--  hasChildren controls whether the "+" node appears.
--
sequence res, r, fkey, done
integer k
--if isDebug then
--  printf(1,"getProjectSet(%s,%s)\n",{path,name})
--end if
--  if needVedb then
--      path = upper(path)
--      name = lower(name)
--?9/0
--  end if
--if isDebug then
--  if equal(name,"c_decl.e") then
--      trace(1)
--  end if
--end if

    res={}
--6/12/09. got an infinite loop here, sequence "done" added to prevent it.
    done = {}
    k = logFile(path,name,2)
    SelectTable(Tfiles)
    while 1 do
        fkey = getNameAndDir(k,0,0)
        k = db_find_key(fkey) -- Key is {uniq[dir],file}
        r = db_record_data(k) -- {uniq,size,filedate,s,i,b}
        if equal(r[6],0)    -- blank includeby set, assume it's toplevel
        or length(r[6])!=1 then -- included by more than one project (or none)
            -- Note that hasChildren (res[1][5]) is set by extendProject
            res = {{0,fkey[2],getDir(fkey[1]),0,0}}
            if sequence(r[6]) then --length(r[6])>1 then
                res[1][5] = 1   -- set hasChildren
                res = append(res,{0,"..","",1,1})
                res = extendProject(res,2,r[6],0)
            end if
            res = extendProject(res,1,r[5],1)
            exit
        end if
        k = r[6][1]     -- get the only parent then
        if find(k,done) then exit end if
        done &= k
    end while
    DBclose()
    return res
end function

--global procedure logBackup(integer id, sequence path, sequence name)
--integer f, k
--  f = logFile(path,name,2)
--  SelectTable(Tbackups)
--  k = db_find_key(f)
--  if k>0 then
--      db_replace_data(k,append(db_record_data(k),id))
--  else
--      if db_insert(f,{id})!=DB_OK then
--          --DEV DBfatal?
----            void = proemh("Error","error inserting backup record",0)
--          IupMessage("Error","error inserting backup record")
--      end if
--  end if
--  DBclose()
--end procedure

--global function getBackupSet(integer f)
--integer k
--sequence res
--  res = {}
--  SelectTable(Tbackups)
--  k = db_find_key(f)
--  if k>0 then
--      res = db_record_data(k)
--      if length(res)=0 then
--          db_delete_record(k)
--      end if
--  end if
--  DBclose()
--  return res
--end function

--global procedure rewriteBackupSet(integer f, sequence backupSet)
--integer k
--  SelectTable(Tbackups)
--  k = db_find_key(f)
--  if length(backupSet) then
--      db_replace_data(k,backupSet)
--  else
--      db_delete_record(k)
--  end if
--  DBclose()
--end procedure

--global function getRestoreDetails(integer backNo, sequence backExt)
--sequence res
--  SelectTable(Tbackups)
--  for i=1 to db_table_size() do
--      if find(backNo,db_record_data(i)) then
--          res = getNameAndDir(db_record_key(i), 0, 0)
--          if match(backExt,res[2])=length(res[2])-length(backExt)+1 then
--              res[1] = getDir(res[1])
--              DBclose()
--              return res
--          end if
--      end if
--  end for
--  DBclose()
--  return 0
--end function


-- Background processing:
global integer bgIdx
global integer runFullBackgroundScan
               runFullBackgroundScan = 0

integer purgeBackups = 2
--, pDate

--include eadadj.e  -- simple date adjust routine.

--/*
procedure checkBackups(integer funiq, sequence extension)
sequence backset, backName
integer backMod, backNo
object dt

    backset = getBackupSet(funiq)
    backMod = 0
    if purgeBackups=2 then
        dt = date()
--DEV
        if not isAutoBackup or isRetainBackupsFor=0 then
            purgeBackups = 0
            return
        end if
--      dt[1] = 1900 + dt[1]
        dt = adjustDate(dt[1..3],-isRetainBackupsFor)
        if atom(dt) then return end if
        dt[1] = remainder(dt[1],10)
        pDate = dt[1]*100+dt[2]
        pDate = (pDate*100+dt[3])*10000
        purgeBackups = 1
    end if
    for i=length(backset) to 1 by -1 do
        backNo = backset[i]
        if backNo<pDate 
        and not (pDate-backNo>100000000) then -- year wrap, assume backup in new year, pDate in old,
                        --  (and therefore keep the backup, hence the 'not')
            backName = sprintf("%09d",backNo)
            if backName[2] = '1' then
                backName[3] = backName[3] + 'A' - '0'
            end if
            backName = backName[1]&backName[3..9]&'.'&extension
            backName = initialcurrentdir&`backup\`&backName
--          if not c_func(xDeleteFile,{allocate_StringZ(backName)}) then
            if not delete_file(backName) then
            -- no worries if gone already ;-))
            end if
            backMod = 1
            backset = backset[1..i-1]&backset[i+1..length(backset)]
        end if
    end for
    if backMod then
        rewriteBackupSet(funiq,backset)
    end if
end procedure
--*/

global function getModifiedFile()
-- returns -1 when all done.
-- uses gMFstate to break up processing into smaller chunks.
sequence r, fkey
integer k, skipThisFile
object sdt  -- size and date info
sequence ext
integer syntax
--sequence winTxt
object void

    if gMFstate=0 then
        SelectTable(Tfiles)
--      fkey = getNameAndDir(-1,0,0) -- causes dbload.
        void = getNameAndDir(-1,0,0) -- causes dbload.
        gMFstate = 1
        return 0
    end if
    if gMFstate>length(knownFiles) or atom(knownFiles[gMFstate]) then
        gMFstate = 1
        runFullBackgroundScan = 0
        purgeBackups = 0
        return -1   -- all done
    end if
    fkey = knownFiles[gMFstate]
--  ext = getFileExtension(fkey[2])
    ext = get_file_extension(fkey[2])
--  if purgeBackups then checkBackups(gMFstate,ext) end if
    syntax = find(ext,Extensions)
    if syntax and equal(SynNames[ExtensionNos[syntax]],"Euphoria") then
        SelectTable(Tfiles)
        k = db_find_key(fkey) -- Key is {uniq[dir],file}
        if k>0 then
            if runFullBackgroundScan then
                r = {0,0,0}
--              winTxt=getText(Main)    --DEV, but where to reset?
--              setText(Main,sprintf("%s - %s [%d]",{Edita,fkey[2],gMFstate}))
--              IupSetStrAttribute(dlg,"TITLE","%s - %s [%d]",{Edita,fkey[2],gMFstate}) --DEV
                IupSetStrAttribute(dlg,"TITLE","%s - %s [%d]",{"Edix",fkey[2],gMFstate})
                gMFstate += 1
            else
                r = db_record_data(k) -- {uniq,size,filedate,s,i,b}
            end if
            fkey[1] = getDir(fkey[1])

--          setText(SB6(->sbmsg),sprintf("Bg: gchk:%s%s",fkey))
            IupSetStrAttribute(sbmsg,"TITLE","Bg: gchk:%s%s",fkey)

            sdt = dir(fkey[1]&fkey[2])
            if sequence(sdt) and length(sdt)=1 then
                sdt = sdt[1][D_SIZE..D_SECOND]
                if not equal(r[2],sdt[1])
                or not equal(r[3],sdt[2..length(sdt)]) then
                    --
                    -- But make sure, if this file is open, it is not modified
                    --
                    skipThisFile = 0
                    bgIdx = 0
                    for c=1 to length(filenames) do
                        if equal(fkey[2],filenames[c])
                        and equal(fkey[1],filepaths[c]) then
                            if actionsave[c]!=actionptr[c] then
                                skipThisFile = 1
                            else
                                bgIdx = c
                            end if
                            exit
                        end if
                    end for
                    if not skipThisFile then
                        return {fkey,sdt}
                    end if
                end if
            end if
        end if
    end if
    gMFstate += 1
    return 0
end function


function stripspaces(sequence s)
-- used for comparing fold lines (w | w/out tabs<-->spaces)
integer k
    -- strip leading spaces and tabs
    while length(s) and find(s[1],"\t ") do
        s = s[2..length(s)]
    end while
    -- replace tabs with spaces
    while 1 do
        k = find('\t',s)
        if k = 0 then exit end if
        s[k] = ' '
    end while
    -- replace doublespaces with singlespaces
    while 1 do
        k = match("  ",s)
        if k = 0 then exit end if
        s = s[1..k]&s[k+2..length(s)]
    end while
    return s
end function


--with trace
global procedure saveFolds(integer c)
-- save bookmark and fold info; perform as much safety checking as possible;
-- store copies of relevant lines for later checking. (We don't /need/ to
-- stripspaces() the saved lines, but it cannot hurt any.)
integer prevStart, thisStart, thisEnd
integer f, k, BiT
sequence BackupSet
sequence errmsg
integer newLen
object bi
    f = logFile(filepaths[c],filenames[c],2)
    SelectTable(Tfolds)
    BackupSet = bookmarks[c]
    prevStart = -1
    -- Delete wrap entries (they all become invalid on a resize anyway).
    newLen = 0
    for i=1 to length(BackupSet) do
        bi = BackupSet[i]
        if not atom(bi) then
            BiT = bi[bfType]
            if BiT!=WRAP then
                BackupSet[i] = 0    -- reduce ref count
                if and_bits(BiT,WRAP) then
                    bi[bfType] = MARK
                    bi[bfEnd] = 0
                end if
                newLen += 1
                BackupSet[newLen] = bi
            end if
        end if
    end for
    BackupSet = BackupSet[1..newLen]
    errmsg = ""
    for i=1 to length(BackupSet) do
        thisStart = BackupSet[i][bfStart]
        if prevStart>=thisStart 
        or thisStart>=length(filetext[c]) then
--trace(1)
--          void = proemh("Error","error saving fold record[1]",0)
            errmsg = "error saving fold record[1]"
            BackupSet = {}
            exit
        end if
        prevStart = thisStart
        BackupSet[i] = append(BackupSet[i],stripspaces(filetext[c][thisStart+1]))
        if and_bits(BackupSet[i][bfType],FOLD) then
            thisEnd = BackupSet[i][bfEnd]
--          if thisEnd<=thisStart 
            if thisEnd<thisStart 
            or thisEnd>=length(filetext[c]) then
--trace(1)
-- Single line folds are OK now (while they can only be created in 
--  wordwrap mode, they can remain when ww switched off)
--              void = proemh("Error","error saving fold record[2]",0)
                errmsg = "error saving fold record[2]"
                BackupSet = {}
                exit
            end if
            BackupSet[i] = append(BackupSet[i],stripspaces(filetext[c][thisEnd+1]))
        end if
    end for
--DEV crashes here with current_db=-1 in database.e, after a proemh() call above...
--      isOpen=0 as well, and since SelectTable(Tfolds) above will leave it 1, we
--      can assume something else is calling DBclose....
--      Ah! IdleHandler does it! errmsg added.
    k = db_find_key(f)
    if k>0 then
        if length(BackupSet) then
            db_replace_data(k,BackupSet)
        else
            db_delete_record(k)
        end if
    elsif length(BackupSet) then
        if db_insert(f,BackupSet)!=DB_OK then
--          void = proemh("Error","error inserting fold record",0)
            errmsg = "error inserting fold record"
        end if
    end if
    if length(errmsg) then
--      void = proemh("Error",errmsg,0)
        IupMessage("Error",errmsg)
    end if
end procedure

--function sEqual(sequence a, sequence b)
function sEqual(sequence a, integer c, integer l)
--
-- perform a 'soft' match, ignoring any differences in spaces and tabs.
-- (since PackTab processing is deferred until IdleHandler gets bored)
--
    if l<0 or l>=length(filetext[c]) then         -- check l is 0..len-1 ;-))
        return 0
    end if
    return equal(stripspaces(a),stripspaces(filetext[c][l+1]))
--  return equal(stripspaces(a),stripspaces(b))
end function

--with trace
global procedure loadFolds(integer c)
-- Load previous folds and bookmarks, performing as many checks as possible.
-- Each bookmark and fold has copies of the lines it applied to saved with
-- it, check them before leaving just the line nos in bookmarks[c].
integer prevStart, thisStart
object thisEnd
integer f, k
sequence BackupSet, nb
    f = logFile(filepaths[c],filenames[c],2)
    SelectTable(Tfolds)
    k = db_find_key(f)
    if k>0 then
        BackupSet = db_record_data(k)
        DBclose()
        prevStart = -1
        for i=1 to length(BackupSet) do
            thisStart = BackupSet[i][bfStart]
            thisEnd = BackupSet[i][bfEnd]   -- nb garbage if and_bits(bfType,FOLD)=0
            if prevStart>=thisStart
            or not sEqual(BackupSet[i][4],c,thisStart)
            or (and_bits(BackupSet[i][bfType],FOLD)
--              and (thisEnd<=thisStart
                and (thisEnd<thisStart
                     or not sEqual(BackupSet[i][5],c,thisEnd))) then
--trace(1)
--              void = proemh("Error","error loading fold record",0)
                IupMessage("Error","error loading fold record")
                BackupSet = {}
                exit
            end if
            BackupSet[i] = BackupSet[i][1..3]
            prevStart = thisStart
        end for
    else
        DBclose()
        BackupSet = {}
    end if
    nb = repeat(0,length(filetext[c]))
    for i=1 to length(BackupSet) do
        nb[BackupSet[i][bfStart]+1] = BackupSet[i]
    end for
    bookmarks[c] = nb
end procedure

--
-- Extension records added 19/6/07:
--
global procedure SaveExtSet(sequence extset)
object key
    SelectTable(Textset)
    for i=db_table_size() to 1 by -1 do
        key = db_record_key(i)
        for j=1 to length(extset) do
            if equal(key,extset[j][1]) then
                db_replace_data(i,extset[j][2..3])
                extset[j] = extset[length(extset)]
                extset = extset[1..length(extset)-1]
                key = ""
                exit
            end if
        end for
        if length(key) then
            db_delete_record(i)
        end if
    end for
    for i=1 to length(extset) do
        if db_insert(extset[i][1],extset[i][2..3])!=DB_OK then
--          void = proemh("Error","error inserting record",0)
            IupMessage("Error","error inserting record")
            exit
        end if
    end for
    DBclose()
end procedure

global function GetExtSet()
sequence extset
    SelectTable(Textset)
    extset = repeat(0,db_table_size())
    for i=1 to length(extset) do
        extset[i] = prepend(db_record_data(i),db_record_key(i))
    end for
    DBclose()
    return extset
end function
--
-- Macro table handling routines:
--
global sequence MacroKeys   -- see eamacro.ew

global procedure insertMacroRecord(sequence key, sequence data)
    SelectTable(Tmacros)
    if db_insert(key,data)!=DB_OK then
        DBfatal("error inserting record",CRASHALWAYS)
    end if
    DBclose()
end procedure

global procedure updateMacroRecord(sequence key, sequence data)
integer rn
    SelectTable(Tmacros)
    rn = db_find_key(key)
    if rn<0 then ?9/0 end if
    db_replace_data(rn,data)
    DBclose()
end procedure

global procedure deleteMacroRecord(sequence rec)
-- rec is {key,data}
integer rn
    SelectTable(Tmacros)
    rn = db_find_key(rec[1])
    if rn<0 then ?9/0 end if
    if not equal(db_record_data(rn),rec[2]) then ?9/0 end if
    db_delete_record(rn)
    DBclose()
end procedure

global function readMacroData()
sequence recs
    SelectTable(Tmacros)
    recs = repeat(0,db_table_size())
    for i=1 to length(recs) do
        recs[i] = {db_record_key(i),db_record_data(i)}
    end for
    DBclose()
    return recs
end function

--global function load_fif(integer dirdict, extdict)
-- returns flags, or {} if none
global function get_fif(sequence key)
object res = {}
    SelectTable(Tfindif)
    integer rn = db_find_key(key)
    if rn>0 then
        res = db_record_data(rn)
    end if
    DBclose()
    return res
end function

global procedure store_fif(sequence key, object data)
    SelectTable(Tfindif)
    integer rn = db_find_key(key)
    if rn>0 then
        db_replace_data(rn,data)
    else
        if db_insert(key,data)!=DB_OK then ?9/0 end if
    end if
    DBclose()
end procedure
    


procedure initEdixEdb()
--
-- open/create the edix database
--
sequence tlist
integer errCode
sequence Fkey

    errCode = db_open(initialcurrentdir&edixEDB,DB_LOCK_EXCLUSIVE)
    if errCode!=DB_OK then
        if errCode = DB_LOCK_FAIL then
--          void = proemh("Error","edita.edb locked, aborting",0)  abort(0)
            IupMessage("Error","edix.edb locked, aborting")  abort(0)
        elsif db_create(initialcurrentdir&edixEDB,DB_LOCK_EXCLUSIVE)!=DB_OK then
--          void = proemh("Error","error creating "&initialcurrentdir&edixEDB,0)  ?9/0
            IupMessage("Error","error creating "&initialcurrentdir&edixEDB)  ?9/0
        end if
    end if
    isOpen = 1

    tlist = db_table_list()
    for i=1 to length(Tset) do
        if not find(Tset[i],tlist) then
            if i=1 then --"version" not present; delete the lot. (pre 0.2.0)
                for j=1 to length(tlist) do
                    db_delete_table(tlist[j])
                end for
                tlist = {}
            end if
            if db_create_table(Tset[i])!=DB_OK then
                DBfatal("error creating "&Tset[i]&" table",CRASHALWAYS)
            elsif i=1 then
--              if db_insert(1,{0,2,0})!=DB_OK then
                if db_insert(1,{0,3,3})!=DB_OK then
                    DBfatal("error inserting version record",CRASHALWAYS)
                end if
            end if
        end if
    end for

    --
    -- get the version
    --
    SelectTable(Tversion)
    dbversion = db_record_data(1)
--  if compare(dbversion,{0,3,3})<0 then
--      -- 0.2.0 (pre 0.3.3/usegpp)
----        needVedb = 1
--?9/0
--      -- (Tversion is [eventually] set to {0,3,3} by vedb.exw)
--  end if

    --
    -- Load the default macros now
    --  (A macro saved as eg "F7default" [by editing the top box] will load as
    --   the default for F7, whereas "F7" is "this session only". Note that it
    --   handles multiple macros starting F7 alphabetically, so if you defined
    --   both "F7default" and "F7zzz", the latter would override the former.
    --   Of course macros can hold something named eg "paste and find next",
    --   which will only be available after manually assigning it to a key.)
    --
    MacroKeys = repeat({},4)
    SelectTable(Tmacros)
    for i=1 to db_table_size() do
        Fkey = db_record_key(i)
        if length(Fkey)>2 then
            if Fkey[1]>'F' then exit end if
            if Fkey[1]='F' then
                if Fkey[2]>'9' then exit end if
                if Fkey[2]>='6' then
                    MacroKeys[Fkey[2]-'5'] = db_record_data(i)
                end if
            end if
        end if
    end for
    DBclose()

end procedure
initEdixEdb()
