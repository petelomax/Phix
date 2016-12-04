--
-- pdb.e
--
-- Simple database handler for pdemo.
-->--   Holds user-defined columns and appropriate values for any demo files we found.
-->--   No other modules should read or update pgui.edb.
--  No other modules should read or update pdemo.edb.
--
without trace
--with trace

--/**/include builtins\database.e
--/**/include builtins\pgetpath.e
--/**/include builtins\pdir.e

--constant pguiEDB = "pgui.edb"
constant pdemoEDB = "pdemo.edb"
--string pguifullpath
string pdemofullpath

sequence dmap

sequence dbversion

constant Tversion       = "version",
-->      Tenums         = "enums",
-->      Tcolumns       = "columns",
         Tdirectories   = "directories",
         Tfiles         = "files"

constant Tset=
{Tversion,      -- Key is 1, data is (>=){1,0,0} [not really used for anything much yet]
--> Tenums,     -- Key is id, data is {name,valueset}, eg {"yesno",{"yes","no"}}
--> Tcolumns,       -- Key is id, data is {name,width,rec,flags}
 Tdirectories,  -- Key is path, data is uniq. DO NOT DELETE (or set data to -1 and clean up later)
 Tfiles}        -- Key is {uniq[dir],file}, data is {LastRun,...}.
--constant T_LastRun = 1

--constant RETRY=0, CRASH=1, CRASHALWAYS=2 -- params for DBfatal

--procedure DBfatal(sequence msg, integer crashflag)
----integer b
----    trace(1) --DEV re-insert this if you get PC hangs.
----    b = MB_OK
----    if crashflag=CRASH then
----        b = MB_OKCANCEL
----    end if
----    if proemh("Error in eaedb.e",msg,b)=IDOK then
----    if messageBox("Error in eaedb.e",msg,b)=IDOK then
----    void = messageBox("Demos",htxt,MB_OK)
--  IupMessage("Error in db.e",msg)
----        if crashflag!=RETRY then ?9/0 end if
----    end if
----    if crashflag=CRASHALWAYS then ?9/0 end if
--end procedure

integer isOpen
        isOpen = 0

procedure DBopen()
--< open pgui.edb 
-- open pdemo.edb 
integer errCode, retries
sequence winTxt
atom t
    if DB_BROKEN then ?9/0 end if
    if not isOpen then
        retries = 0
        sleep(0)
        t = time()+1
        while 1 do
--          errCode = db_open(pguifullpath,DB_LOCK_EXCLUSIVE)
            errCode = db_open(pdemofullpath,DB_LOCK_EXCLUSIVE)
            if errCode=DB_OK then exit end if
            if retries>4 then
--              DBfatal(sprintf("open %s [%d]",{pguifullpath,errCode}),RETRY)
                IupMessage("Error in db.e",sprintf("open %s [%d]",{pdemofullpath,errCode}))
                abort(0)
            else
                retries += 1
--              winTxt = getText(pmain)
                winTxt = IupGetAttribute(dlg,"TITLE")
--              setText(pmain, "DATABASE LOCKED")
                IupSetAttribute(dlg,"TITLE","DATABSE LOCKED")
                sleep(1)
--              setText(pmain,winTxt)
                IupSetAttribute(dlg,"TITLE",winTxt)
                sleep(1)
            end if
        end while
        isOpen = 1
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
--      DBfatal("db_select_table "&name,CRASHALWAYS)
        IupMessage("Error in db.e","db_select_table "&name)
        ?9/0
    end if
end procedure

global procedure pgdbinit(string path)
--
--< open/create the pgui database
-- open/create the pdemo database
--
sequence tlist
integer errCode
--sequence data
--sequence name -- [DEV] not string

    if DB_BROKEN then ?9/0 end if
--  pguifullpath = path&pdemoEDB
--  pdemofullpath = path&pdemoEDB
    pdemofullpath = join_path({path,pdemoEDB})
    dmap = {}

    errCode = db_open(pdemofullpath,DB_LOCK_EXCLUSIVE)
    if errCode!=DB_OK then
        if errCode = DB_LOCK_FAIL then
--          void = proemh("Error","pgui.edb locked, aborting",0)  abort(0)
--          void = messageBox("Error","pgui.edb locked, aborting",0)  abort(0)
            IupMessage("Error",pdemofullpath&" locked, aborting") abort(0)
        elsif db_create(pdemofullpath,DB_LOCK_EXCLUSIVE)!=DB_OK then
--          void = proemh("Error","error creating pgui.edb",0)  ?9/0
--          void = messageBox("Error","error creating pgui.edb",0)  ?9/0
            IupMessage("Error","error creating "&pdemofullpath) ?9/0
        end if
    end if
    isOpen = 1

    tlist = db_table_list()
    for i=1 to length(Tset) do
        if not find(Tset[i],tlist) then
            if db_create_table(Tset[i])!=DB_OK then
--              DBfatal("error creating "&Tset[i]&" table",CRASHALWAYS)
                IupMessage("Error in db.e","error creating "&Tset[i]&" table")
                ?9/0
            elsif i=1 then -- Tversion
                if db_insert(1,{1,0,0})!=DB_OK then
--                  DBfatal("error inserting version record",CRASHALWAYS)
                    IupMessage("Error in db.e","error inserting version record")
                    ?9/0
                end if
            end if
        end if
    end for

    --
    -- get the version
    --
    SelectTable(Tversion)
    dbversion = db_record_data(1)
--  if dbversion<{1,0,0} then
    if compare(dbversion,{1,0,0})<0 then
--      needVedb = 1
        ?9/0
    end if

--/*
--  --
--  -- and the enums
--  --
--  SelectTable(Tenums)
--  enums = {}
--  esets = {}
--  for i=1 to db_table_size() do
--      data = db_record_data(i)
--      enums = append(enums,data[1])
--      esets = append(esets,data[2])
--  end for
--  if length(enums)=0 then
--      enums = {"yesno","noyes"}
--      esets = {{"yes","no"},{"no","yes"}}
--  end if
--*/

--/*
--  --
--  -- and the columns
--  --
--  SelectTable(Tcolumns)
--  for i=1 to db_table_size() do
--      data = db_record_data(i)
--      name = data[1]
--      for j=1 to length(columns) do
----            if name=columns[j][1] then
--          if equal(name,columns[j][1]) then
--              columns[j] = data[1..3]
--              cflags[j] = data[4]
--              data = {}
--              exit
--          end if
--      end for
--      if length(data) then
--          columns = append(columns,data[1..3])
--          cflags = append(cflags,data[4])
--      end if
--  end for
--*/
    
    DBclose()

end procedure

global procedure pgdbAddDir(string path)
integer k
    SelectTable(Tdirectories)
    k = db_find_key(path)
    if k<0 then
        k = db_table_size()+1
--?{"pgdbAddDir",path,k}
        if db_insert(path,k)!=DB_OK then
--          DBfatal("error inserting record",CRASH)
--          return
            IupMessage("Error in db.e","error inserting record")
            ?9/0
        end if
    end if
    dmap = append(dmap,k)
end procedure

--global function pgdbGetLastRun(integer k, sequence name)
global function pgdbGetFileInfo(integer k, sequence name)
--sequence data
object data
    SelectTable(Tfiles)
    k = dmap[k]
    k = db_find_key({k,name})
    if k>0 then
        data = db_record_data(k)
--      return data[T_LastRun]  -- as per date() 
        if sequence(data) then
            if length(data)<3 then
--?data
                if length(data)<2 then
                    data = append(data,0)   -- not broken
                end if
                data = append(data,"")  -- blank text
            end if
            return data
        end if
        printf(1,"warning: record[%d] (%s) deleted\n",{k,sprint(name)})
        db_delete_record(k)
    end if
    return 0
end function

global procedure pgdbSetFileInfo(integer k, sequence name, sequence fileinfo)
integer r
--sequence data
if length(fileinfo)!=3 then ?9/0 end if
    k = dmap[k]
    SelectTable(Tfiles)
    r = db_find_key({k,name})
--?{"pgdbSetFileInfo",k,name,fileinfo,r}
    if r>0 then
--      data = db_record_data(r)
--      data[1] = dateinfo
--      db_replace_data(r,data)
        db_replace_data(r,fileinfo)
--?{"db_replace_data",r,k,name,fileinfo}
    else
--      data = {dateinfo}
--      if db_insert({k,name},data)!=DB_OK then
        if db_insert({k,name},fileinfo)!=DB_OK then
--          DBfatal("error setting last run",CRASH)
            IupMessage("Error in db.e","error setting last run")
            ?9/0
        end if
--?{"db_insert",{k,name},fileinfo}
    end if
end procedure

global procedure pgdbCompress()
-- Note: you may need to manually delete the ".t01"..".t99" backups
    DBopen()
    if db_compress()!=DB_OK then
--      void = messageBox("Error","Unable to compress "&pdemofullpath,MB_OK)
        IupMessage("Error","Unable to compress "&pdemofullpath)
    else
--      void = messageBox("Success",pdemofullpath&" compressed",MB_OK)
--  void = messageBox("Demos",htxt,MB_OK)
        IupMessage("Success",pdemofullpath&" compressed")
    end if
    DBclose()
end procedure
