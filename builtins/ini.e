--
-- builtins\ini.e
-- ==============
--
--  A simple ini file handler. Implements ini_load() etc (see docs)
--
--  Experimental: to be used and fully tested in anger in theEditor.exw first.
--  Todo: Save backups/ini_name.1234.ini files...
--
local integer ini_idx = -1  -- currently active/open [should never be 0, -1==needs init]

--
-- Should you only be using a single ini file, any fiddling this may or may not do with the
-- following should be completely seamless, otherwise the application code may need several
-- ini_restore("ini_name") dotted about, before any init_set() or ? = ini_setting() calls,
-- with bMustExist use (below) //designed// to help quickly locate any such omissions.
--
local sequence ini_names,       -- filenames
               keynames,        -- pre-'=' from within ini_names[ini_idx]
               settings,        -- post-'='            """
               dirty,           -- false if no ini_set/delete happened/pending
               all_keynames,    -- [ini_idx] = 0, with actual/live in keynames
               all_settings,    -- """	"""  """					      settings
               known_dirs       -- (performancewise)

global function ini_filename(string ini_name)
    -- (made available in case it proves useful)
    if ini_name[1]='$' then 
        ini_name = ini_name[2..$]
        string config = "config"
        integer k = find('$',ini_name)
        if k then
            config = ini_name[k+1..$]
            ini_name = ini_name[1..k-1]
        end if
        if platform()=LINUX then
            return 9/0 -- DEV simply not decided on a suitable place yet...
        elsif platform()=WINDOWS then
            --
            -- Some suitable candidates:
            --ALLUSERSPROFILE=C:\ProgramData    -- (same as ProgramData=C:\ProgramData)
            --PUBLIC=C:\Users\Public            -- that might be better....[?]
            --
            string config_dir = join_path({getenv("ALLUSERSPROFILE"),ini_name})
--maybe:    make_ini_dir(config_dir)
            if not find(config_dir,known_dirs) then -- (speedwise)
                integer filetype = get_file_type(config_dir)
                if filetype!=FILETYPE_DIRECTORY then
                    assert(filetype=FILETYPE_NOT_FOUND)
                    assert(create_directory(config_dir))
                end if  
                known_dirs = append(known_dirs,config_dir)
            end if  
            return join_path({config_dir,config&".ini"})
        else
            ?9/0 -- unknown platform...
        end if
    end if
    if platform()=LINUX then
        return join_path({getenv("HOME"),"."&ini_name})
    elsif platform()=WINDOWS then
        return join_path({getenv("HOMEDRIVE")&getenv("HOMEPATH"),ini_name&".ini"})
    else
        ?9/0
    end if
end function

global procedure ini_load(string ini_name, ini_default="")
    if ini_idx=-1 then
        ini_names = {}
        all_keynames = {}
        all_settings = {}
        dirty = {}
        known_dirs = {}
    elsif ini_idx then
        all_keynames[ini_idx] = keynames
        all_settings[ini_idx] = settings
    end if
    ini_idx = find(ini_name,ini_names)
    assert(ini_idx=0)
    ini_names = append(ini_names,ini_name)
    all_keynames = append(all_keynames,0)
    all_settings = append(all_settings,0)
    dirty = append(dirty,false)
    ini_idx = length(ini_names)
    keynames = {}
    settings = {}
    object lines = -1
    if platform()!=JS then
        string config_filename = ini_filename(ini_name)
        lines = get_text(config_filename,GT_LF_STRIPPED)
    end if
    if lines=-1 then
        lines = split(ini_default,'\n')
    end if
    for line in lines do
        line = trim(line)
        -- aside: `= stuff` would be stored almost the same even
        --           if the following left comment set to false,
        --           but the = would get stripped by ini_save().
        bool comment = length(line)=0 or find(line[1],"-/;=")
        string keyname
        object setting
        if comment then
            keyname = ""
            setting = line
        else            
            integer k = find('=',line)
            if k=0 then
                keyname = line
                setting = ""
            else
                {keyname, setting} = apply(split(line,'='),trim)
            end if
            k = find(keyname,keynames)
            if k then
                string prev = settings[k]
                printf(1,"Warning: duplicate setting: %s = %s [was %s]\n",{keyname,setting,prev})
                -- trash it now... (but don't mark dirty yet, for debug reasons)
                keynames[k..k] = {}
                settings[k..k] = {}
            end if
            if is_integer(setting) then
                setting = to_integer(setting)
            end if
        end if
        keynames = append(keynames,keyname)
        settings = append(settings,setting)
    end for
end procedure

local procedure check_ini() -- (used by all except ini_load)
    assert(ini_idx>0,"no prior ini_load()?")
end procedure

global procedure ini_restore(string ini_name)
    -- only ever needed when there (may) have been multiple ini_load() calls.
    if length(ini_name) 
    and (ini_idx<=0 or ini_names[ini_idx]!=ini_name) then
        -- (^ do nowt is fast and cheap, not that do summat is slow/dear!)
        if ini_idx>0 then
            all_keynames[ini_idx] = keynames
            all_settings[ini_idx] = settings
        end if
        ini_idx = find(ini_name,ini_names)
        assert(ini_idx,"no such ini_name:"&ini_name)
        keynames = all_keynames[ini_idx]
        settings = all_settings[ini_idx]
        all_keynames[ini_idx] = 0
        all_settings[ini_idx] = 0
    else
        check_ini()
    end if
end procedure

global function ini_as_string(string ini_name)
    -- (primarily intended for JavaScript use..)
    -- (made available in case it proves useful)
    ini_restore(ini_name)
    string res = ""
    for i,keyname in keynames do
        object setting = settings[i]
        if keyname="" then -- comments
            assert(string(setting))
            res &= sprintf("%s\n",{setting})
        elsif setting="" then
            res &= sprintf("%s\n",{keyname})
        elsif integer(setting) then
            res &= sprintf("%s = %d\n",{keyname,setting})
        else
            res &= sprintf("%s = %s\n",{keyname,setting})
        end if
    end for
    return res
end function

global function ini_dirty(string ini_name)
    ini_restore(ini_name)
    return platform()!=JS and dirty[ini_idx]
end function

global procedure ini_save(string ini_name="")
    --
    -- Note there are no checks whatsoever that ini_save() is ever actually called.
    -- It can however be invoked multiple times per session with zero side effects,
    -- obviously, that is, apart from the ini file itself being overwritten.
    -- While I could let you omit ini_name it would offer no real saving, plus of
    -- course it is far safer to be sure you are in fact saving the correct thing.
    --
    if ini_dirty(ini_name) then
        string content = ini_as_string(ini_name),
               config_filename = ini_filename(ini_name)
        integer fn = open(config_filename,"w")
        puts(fn,content)
        close(fn)
        dirty[ini_idx] = false
    end if
end procedure

global procedure ini_close(string ini_name="", bool bIgnoreDirty=false)
    ini_restore(ini_name)
    if not bIgnoreDirty then
        assert(not ini_dirty(ini_name),"unsaved changes")
    end if
    integer l = length(ini_names)
    if ini_idx<l then
        ini_names[ini_idx] = ini_names[l]
        dirty[ini_idx] = dirty[l]
        all_keynames[ini_idx] = all_keynames[l]
        all_settings[ini_idx] = all_settings[l]
    end if
    l -= 1
    ini_names = ini_names[1..l]
    dirty = dirty[1..l]
    all_keynames = all_keynames[1..l]
    all_settings = all_settings[1..l]
    ini_idx = 0
end procedure

global function ini_keynames(string ini_name="")
    ini_restore(ini_name)
    return filter(keynames,"!=","") -- (not comments)
end function

local function ini_keydx(string keyname)
    -- (common code)
    check_ini()
    assert(length(keyname))
    return find(keyname,keynames)
end function

-- Note: bMustExist is intended to help by shouting out missing ini_restore(),
--       should you override it to false, and/or fail to mention something in 
--       the ini_default as passed to ini_load(), it becomes the application's 
--       sole responsiblity to ensure the correct ini file is open/active.
--       However some "name"s, esp filepaths, almost always need it overidden.

global function ini_setting(string keyname, object dflt=0, bool bMustExist=true)
    integer k = ini_keydx(keyname)
    if k then return settings[k] end if
    assert(not bMustExist)
    return dflt
end function

global procedure ini_set(string keyname, object setting, bool bMustExist=true)
    if string(setting) then
        -- aside: note that eg "\\n" and `\n` are perfectly fine, but the callee
        --        is expected to perform any escape char/sprintf/scanf handling.
        assert(not find('\n',setting),"no multiple lines")
    else
        assert(integer(setting),"int/str only") -- nb deliberately not floats
    end if
    integer k = ini_keydx(keyname)
    if k then
        if setting!=settings[k] then
            settings[k] = setting
            dirty[ini_idx] = true
        end if
    else
        assert(not bMustExist)
        keynames = append(keynames,keyname)
        settings = append(settings,setting)
        dirty[ini_idx] = true
    end if
end procedure

global procedure ini_delete_key(string keyname, bool bMustExist=true)
    integer k = ini_keydx(keyname)
    if k then
        keynames[k..k] = {}
        settings[k..k] = {}
        dirty[ini_idx] = true
    else
        assert(not bMustExist)
    end if
end procedure

--ini_load("xpEditer")
--  The xpEditer.ini file handler   -- (promote to builtins\ini_file_handler.e?)
-- Nah, make it gConfigLoad(string ini_name),
--              gConfigSave()
--              gConfig[Get/Set][Recent/Int/Str](..)
--              gConfigDialog[Closed|Show](gdx dlg)

-- ok on windows
--/*
--erm, on save: "Trash any[/%d] local changes and reload?" (if any..)

-- ini.e:
--  global procedure switchToIniGroup(object group)
--  global procedure defineIniHeading(string entryname, integer entrytype)
--  global procedure setIniValue(sequence entryname, object val)
--  global procedure setIniTextValue(object val)
--  global function getIniValue(string entryname, object defaultvalue)
--  global function getIniTextValues()
--  global function isNewIni()
--  global function decode11(string s, integer i)
--  global procedure loadINI(string configname=completeinipath)
--  global procedure iniSetPrev(sequence _prevfiles, integer _newcurrfile)
--  global procedure iniCreateTabs()
--  global function load_prev_file()
--  global procedure saveINI(sequence fdii={}, integer newcur=currfile) -- can be called many times. [???]

IUP docs:
--  Ihandle config = IupConfig() 
--  integer errcode = IupConfigLoad(Ihandle config)
--  integer errcode = IupConfigSave(Ihandle config)
--  IupConfigRecentInit(Ihandle config, Ihandle menu_list, cbfunc recent_cb, integer max_recent)
--  IupConfigRecentUpdate(Ihandle config, string filename)
--  IupConfigSetVariableInt(Ihandle config, string group, string key, integer v) 
--  --IupConfigSetVariableIntId(Ihandle config, string group, string key, integer id, integer v) 
--  --IupConfigSetVariableDouble(Ihandle config, string group, string key, atom v) 
--  --IupConfigSetVariableDoubleId(Ihandle config, string group, string key, integer id, atom v)
--  integer res = IupConfigGetVariableInt(Ihandle config, string group, string key, integer def=0) 
--  --integer res = IupConfigGetVariableIntId(Ihandle config, string group, string key, integer id, integer def=0) 
--  --atom res = IupConfigGetVariableDouble(Ihandle config, string group, string key, atom def=0) 
--  --atom res = IupConfigGetVariableDoubleId(Ihandle config, string group, string key, integer id, atom def=0) 
--  IupConfigSetVariableStr(Ihandle config, string group, string key, nullable_string v) 
--  --IupConfigSetVariableStrId(Ihandle config, string group, string key, integer id, nullable_string v) 
--  string res = IupConfigGetVariableStr(Ihandle config, string group, string key, nullable_string def=NULL) 
--  --string res = IupConfigGetVariableStrId(Ihandle config, string group, string key, integer id, nullable_string def=NULL) 
--  IupConfigDialogClosed(Ihandle config, Ihandle dialog, string name)
--  IupConfigDialogShow(Ihandle config, Ihandle dialog, string name, boolean maximised=false)

Searching for: IupConfig (deduped)
--  config = IupConfig()
--  integer errcode = IupConfigLoad(config)
--  integer errcode = IupConfigSave(config)
--  IupConfigRecentInit(config, recent_menu, Icallback("config_recent_cb"), 9)
--  IupConfigRecentUpdate(config, filename)
--  IupConfigDialogShow(config, dlg, "MainWindow")  -- maximised unused
--  IupConfigDialogClosed(config, dlg, "MainWindow")
--  IupConfigSetVariableStr(config,"KeyMappings",ks,km)
--  sequence KEYS = split(IupConfigGetVariableStr(config, "KeyMappings", "KEYS"),',')
--  IupConfigSetVariableInt(config, "KeyMappings", "Active", state)
--  integer active = IupConfigGetVariableInt(config, "KeyMappings", "Active", 1)

proposed:
* single cfg file in the user's home directory [unless full path specified on gConfigLoad()]
-- (an application could theoretically have several, if it called Load/Save often, with only 1 in memory at any given time)
--  global procedure gConfigLoad(string name="")
--  global procedure gConfigSave()
--  global function gConfigName() - returns the full pathname of the config file (for use when switching tabs).
--  global function gConfigState() - returns "modified %d times", which could be 0 ("").
-- (let's have a "check config state" option in xpEditer/main, default true, if false auto save on tab switch)
--  global procedure gConfigSetRecentHandler(rtn handler)
--*/

--/*

-- from edix.ini (manually edited):
[Main]
  currfile = 349
  Window Position = 924, -8
  Window Size = 949, 1096
  Window Maximized = 0
  Message Area Visible = 1
  Message Area Height = 30
  Watch Area Width = 35


[Extensions]
  bas, "C:\Go\FreeBASIC-1.05.0-win32\fbc.exe"
  bat, open
  eu, #exw
  ex, #exw
  exw, "C:\Program Files (x86)\Phix\pw9.exe"
  fb, "C:\Go-old\FreeBASIC-1.05.0-win32\fbc.exe" -b
  go, "C:\Go\bin\go.exe" run
  htm, open
  html, open
  jl, "C:\Users\Pete\AppData\Local\Programs\Julia 1.5.3\bin\julia.exe"
  php, open
  pl, "C:\Strawberry\perl\bin\perl.exe"
  py, python
  py35, "C:\Program Files (x86)\Python35-32\pythonw.exe"
  rb, "C:\Ruby22\bin\ruby.exe"

[previous session]
  C:\Users\Pete\edix.cfg,{0,0,0,0,0,0,429,0,0,0,6}
  C:\Users\Pete\edix.030919.cfg,{0,0,0,0,0,0,0,0,0,0,20}
-- SUG: replace currfile = 3 with leading >:
 >C:\Program Files (x86)\Phix\pdemo.exw,{0,0,0,0,0,0,0,0,0,0,17}

edix.cfg: - only obvious use is C:\Program Files (x86)\Phix\demo\edix\edix.exw:2328          elsif equal(filenames[c],"edix.cfg") then
dit=dit

[ExtDialog]
Height=348
MaxiSized=0
Maximized=0
Width=712
X=1041
Y=242

[Extensions]
bas="C:\Go\FreeBASIC-1.05.0-win32\fbc.exe"
bat=open
eu==exw
ex==exw
fb="C:\Go-old\FreeBASIC-1.05.0-win32\fbc.exe" -b
go="C:\Go\bin\go.exe" run
htm=open
html=open
jl="C:\Users\Pete\AppData\Local\Programs\Julia\Julia-1.5.3\bin\julia.exe"
php=open
py35="C:\Program Files (x86)\Python35-32\pythonw.exe"

[FileList]
Height=686
MaxiSized=0
Maximised=1
Width=991
X=0
Y=0

[FindDialog]
Height=170
MaxiSized=0
Maximised=0
Width=974
X=0
Y=0

[FindInFiles]
Height=760
MaxiSized=0
Maximized=0
Width=1388
X=483
Y=188

[KeyDialog]
Height=702
MaxiSized=0
Maximized=0
Width=712
X=1041
Y=242

[KeyMappings]
<Alt G>=<Ctrl G>
<Alt Insert>=<Ctrl '>'>
<Alt N>=<F3>
<Ctrl '\t'>=<Alt '>'>
<Ctrl Delete>=<Ctrl X>
<Ctrl Insert>=<Ctrl C>
<Ctrl Shift '\t'>=<Alt '<'>
<Ctrl Shift Delete>=<Ctrl Shift X>
<Ctrl Shift Insert>=<Ctrl Shift C>
<Shift Insert>=<Ctrl V>
Active=1
KEYS=<Alt Delete>,<Alt G>,<Alt Insert>,<Alt N>,<Ctrl '\t'>,<Ctrl Delete>,<Ctrl Insert>,<Ctrl Shift '\t'>,<Ctrl Shift Delete>,<Ctrl Shift Insert>,<Shift Insert>

[MainWindow]
Height=967
MaxiSized=1
Maximised=0
Maximized=0
Statusbar=ON
Toolbar=ON
Width=1219
X=713
Y=85

[Session]
Max=282
current=281
cursel1={0,26,0,0,1,0,9,0,0,0,0}
cursel100={0,54,42,0,0,0,55,0,0,0,2}
cursel99={0,0,0,0,0,0,0,0,0,0,10}
filepath1=C:\Users\Pete\edix.cfg
filepath10=C:\Program Files (x86)\Phix\demo\libcurl\websocket.exw
filepath99=C:\Program Files (x86)\Phix\demo\rosetta\Pierpont_primes.exw



Searching for: ini
 Files scanned 187, Directories scanned 12, Lines 234473
C:\Program Files (x86)\Phix\pwa\p2js.exw:673 global constant string initialcurrentdir = get_proper_dir(command_line()[2]),
C:\Program Files (x86)\Phix\pwa\p2js.exw:674                        inifile = join_path({initialcurrentdir,"p2js.ini"})
C:\Program Files (x86)\Phix\pwa\p2js.exw:686 bool bSaveIni = false
C:\Program Files (x86)\Phix\pwa\p2js.exw:1134                 bSaveIni = true
C:\Program Files (x86)\Phix\pwa\p2js.exw:1142                 bSaveIni = true
C:\Program Files (x86)\Phix\pwa\p2js.exw:1200         res[i] = join_path({initialcurrentdir,res[i]})
C:\Program Files (x86)\Phix\pwa\p2js.exw:1206 procedure load_ini()
C:\Program Files (x86)\Phix\pwa\p2js.exw:1208     object lines = get_text(inifile,GT_LF_STRIPPED)
C:\Program Files (x86)\Phix\pwa\p2js.exw:1265 procedure save_ini()
C:\Program Files (x86)\Phix\pwa\p2js.exw:1271     if bSaveIni and pdx then    --DEV: and platform()!=JS, probably...
C:\Program Files (x86)\Phix\pwa\p2js.exw:1272 --?"save_ini"
C:\Program Files (x86)\Phix\pwa\p2js.exw:1316         integer fn = open(inifile,"w")
C:\Program Files (x86)\Phix\pwa\p2js.exw:1317         if fn=-1 then crash("cannot open "&inifile) end if
C:\Program Files (x86)\Phix\pwa\p2js.exw:1334         bSaveIni = false
C:\Program Files (x86)\Phix\pwa\p2js.exw:1379 --      IupSetAttribute(filedlg,"DIRECTORY",initialcurrentdir)
C:\Program Files (x86)\Phix\pwa\p2js.exw:1402 --  IupSetStrAttribute(filedlg,"DIRECTORY",iff(d!=""?get_file_path(d,true)initialcurrentdir))
C:\Program Files (x86)\Phix\pwa\p2js.exw:1403     IupSetStrAttribute(filedlg,"DIRECTORY",iff(d!=""?get_file_path(d,true)initialcurrentdir))
C:\Program Files (x86)\Phix\pwa\p2js.exw:1404 --  IupSetStrAttribute(filedlg,"FILE",iff(d!=""?get_file_path(d,true)initialcurrentdir))
C:\Program Files (x86)\Phix\pwa\p2js.exw:2030     if bSaveIni then save_ini() end if
C:\Program Files (x86)\Phix\pwa\p2js.exw:2491         save_ini()
C:\Program Files (x86)\Phix\pwa\p2js.exw:2566         IupSetAttribute(config, "APP_PATH", initialcurrentdir)
C:\Program Files (x86)\Phix\pwa\p2js.exw:2633     -- initialize the current file
C:\Program Files (x86)\Phix\pwa\p2js.exw:2642         load_ini()

--procedure load_ini()
    dropfiles = {}
    object lines = get_text(inifile,GT_LF_STRIPPED)
    if lines!=-1 then
--      integer pdx = 0
--      sequence d = {}
        integer vi = 1
        for i=1 to length(lines) do
            if lines[i]!="" then
                string {lt,li} = apply(split(lines[i],":",true,2),trim)
                if lt="file" then
                    if find(li,dropfiles) then
                        crash("duplicate project")
                    end if
                    dropfiles = append(dropfiles,li)
--                  string fp = get_file_path(li)
--                  integer k = find(fp,d)
--  integer pdx = IupGetInt(chosen,"VALUE")
--                  if length(dropfiles)<=10 or k=0 then
--                      if k=0 then
--                          d = append(d,fp)
--                          dropfiles[$] = dropfiles[vi]
--                          dropfiles[vi] = li
--                      end if
--                      IupSetAttributeId(chosen,"",length(dropfiles),li)
                        IupSetAttributeId(chosen,"",vi,li)
                        vi += 1
--                  end if
--      IupSetInt(chosen,"VALUE",pdx)
--      dropfiles = append(dropfiles,{})
--      IupSetInt(chosen,"VALUE",1)
--              elsif lt="output" then
--                  outputs[pdx] = li
                else
                    ?9/0
                end if
            end if
        end for
        IupSetInt(chosen,"VISIBLEITEMS",vi) -- (no idea why +1 helps)
        droptags = tagset(length(dropfiles))
    end if
--DEV on reflection, we might/probably want to do this without a gui... [laters]
--  (then again, length 3 might be just this, ie you need -run/build/out to...)
    sequence cl = command_line()
    -- open a file from the command line (?allow file association in Windows?)
    if length(cl)>2 then
        string filename = cl[3]
        open_file(filename)
--DEV/SUG
--  else
--      if length(dropfiles)=0 then
--          dropfiles = demoset()
--      end if
    elsif length(dropfiles) then
        IupSetInt(chosen,"VALUE",1)
        open_file(dropfiles[1])
    end if
--end procedure

--procedure save_ini()
    if platform()=JS then ?9/0 end if
    integer pdx = IupGetInt(chosen,"VALUE")
--  if pdx>1 then {tags[1],tags[pdx]} = {pdx,1} end if
--  if pdx>1 then tags[1..pdx] = tags[pdx]&tags[1..pdx-1] end if
--  if pdx then
    if bSaveIni and pdx then    --DEV: and platform()!=JS, probably...
--?"save_ini"
--?dropfiles
--?droptags
--      sequence tags = tagset(length(dropfiles))
        integer l = length(dropfiles),
                mc = 0
        assert(sort(droptags)=tagset(l))
        sequence idroptags = repeat(0,l),
                 must_have = repeat(false,l),
                 seen_paths = {}
        -- invert the tags
        for i=1 to l do
            integer t = droptags[i]
            idroptags[t] = i
        end for
        -- keep at least one file from each directory
        for i=1 to l do
            integer t = idroptags[i]
            string fp = get_file_path(dropfiles[t])
            if not find(fp,seen_paths) then
                seen_paths = append(seen_paths,fp)
                must_have[t] = true
                mc += 1
            end if
        end for
        -- keep at least 5 recent files, and (provided
        -- the list is that long) at least 10 in total
        integer mmc = max(10,mc+5)
        for i=1 to l do
            integer t = idroptags[i]
            if not must_have[t] then
                must_have[t] = true
                mc += 1
                if mc>=mmc then exit end if
            end if
        end for
--      integer tp = tags[pdx]
--      while pdx>1 do
--          tags[pdx] = tags[pdx-1]
--          pdx -= 1
--      end while
--      tags[1] = tp
--      tags[1] = pdx
--      for i=2 to pdx do tags[i] = i-1 end for
        integer fn = open(inifile,"w")
        if fn=-1 then crash("cannot open "&inifile) end if
--1/5/21: keep the lot, for Ctrl T, and instead limit what we load into chosen
--      sequence d10 = {}
        for i=1 to l do
            integer ti = idroptags[i]
--          string fi = dropfiles[ti],
--                 fp = get_file_path(fi)
--          integer k = find(fp,d10)
--          if i<=10 or k=0 then
--              if k=0 then d10 = append(d10,fp) end if
--              printf(fn,"file: %s\n",fi)
            if must_have[ti] then
--?{ti,dropfiles[ti]}
                printf(fn,"file: %s\n",dropfiles[ti])
            end if
        end for
        close(fn)
        bSaveIni = false
    end if
--end procedure
--*/
