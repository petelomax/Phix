--
-- get_interpreter.e
--
--  Implements get_interpreter(), see docs for details.
--
--DEV make this a standard builtin? (see also demo/PGUI/pdemo.exw and filedump.exw)

function match_ew(string path, endswith)
    if length(endswith)=0 then return 1 end if
    sequence segments = split_path(path)
    return length(segments) and lower(segments[$])=lower(endswith)
end function

function add_path(sequence pathset, string path)
    if length(path)
    and not find(path,pathset)
    and get_file_type(path)=FILETYPE_DIRECTORY then
        pathset = append(pathset,path)
    end if
    return pathset
end function

function add_paths(sequence pathset, paths, endswiths)
    for i=1 to length(paths) do
        string path = paths[i]
        for j=1 to length(endswiths) do
            string endswith = endswiths[j]
            if match_ew(path,endswith) then
                pathset = add_path(pathset,path)
                exit
            end if
        end for
    end for
    return pathset
end function

function validexe(string s, sequence vset)
--
-- Just ensures that a filename is reasonable
--
-- s is a (path-less) filename such as "p.exe"
-- vset is a list of permitted filenames (platform dependent)
--
    if find(s,vset) then
        return 1
    else
        string vs1 = vset[1]            --      "pw.exe"/"p"
        integer {nlen, n} = iff(platform()=WINDOWS?{7,3}:{2,2})
        if length(s)=nlen               --    ?"pw9.exe":"p9"
        and find(s[n],"123456789") then
            -- Feel free to ignore this part
            -- (When developing phix, I use pw1.exe..pw9.exe 
            --  on Windows, and p1..p9 on Linux, so that any
            --  running applications need not be shutdown.)
            s[n..n] = ""
            if s=vs1 then
                return 1
            end if
        end if
    end if
    return 0
end function

function ibits(string filepath, integer plat=platform())
--
-- filepath should be a fully qualified filename, of FILETYPE_FILE.
-- returns 32 or 64 by examining low-level headers, or 0 if the file is 
-- corrupt, empty, for a different platform, or otherwise not recognised.
--
-- DEV/SUG may also be sensible to verify data section starts "Phix"?
--
    integer fn = open(filepath,"rb"), bits = 0
    if fn!=-1 then
        if plat=WINDOWS then
            if seek(fn,#80)=SEEK_OK then
                sequence s6 = {}
                for i=1 to 6 do
                    s6 = append(s6,getc(fn))
                end for
                if s6="PE\0\0\x4C\x01" then
                    bits = 32
                elsif s6="PE\0\0\x64\x86" then
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
    end if
    return bits
end function

--/* seems ok:
constant itestset = {{`C:\Program Files (x86)\Phix\pw.exe`,WINDOWS},
                     {`C:\Program Files (x86)\Phix\pth.exe`,WINDOWS},
                     {`C:\Program Files (x86)\Phix\phix`,LINUX}}
for i=1 to length(itestset) do
    {string d, integer plat} = itestset[i]
    ?{d,plat,ibits(d,plat)}
end for
--*/


--
-- aside: on windows, the pw/p ordering may [one day] be swapped to implement a preference,
--        for instance p32.exe hitting "requires(64)" should favour p64.exe over pw64.exe,
--            whereas pw32.exe hitting "requires(64)" should favour pw64.exe over p64.exe.
--
sequence vsets, platforms
bool vinit = false
procedure initv()
    vsets = {{},    -- subscripted by platform(), so [DOS(==1)] is blank.
--           {"pw.exe","p.exe","pw64.exe","p64.exe","pw32.exe","p32.exe"}, -- [WINDOWS]
             {"pw.exe","pw32.exe","pw64.exe","p64.exe","p32.exe","p.exe"}, -- [WINDOWS]
             {"p","p32","p64","pth"}} -- [LINUX]
    -- (must match constants in psym.e)
    platforms = {"DOS32","WINDOWS","LINUX","JAVASCRIPT"}
    vinit = true
end procedure

--/*
bool bPreferW = true
global procedure set_pref_w(bool b)
    bPreferW = b
end procedure

function reorderW(sequence set)
    sequence rl = {}, rh = {}
    for i=1 to length(s) do
        if find('w',s[i]) then
            rl = append(rl,s[i])
        else
            rh = append(rh,s[i])
        end if
    end for
    if not bPreferW then {rl,rh} = {rh,rl} end if
    set = rl & rh
    return set
end function
--*/

function enquote(string filepath)
    if find(' ',filepath) then
        filepath = '"'&filepath&'"'
    end if
    return filepath
end function

global function get_interpreter(bool bQuote=false, object mb=machine_bits(), integer plat=platform(), bool bPrefW=false)
--
-- returns "" (the empty string) on failure
--
    if not vinit then initv() end if
    integer mbmb = sequence(mb)
    if mbmb then
        mb = mb[1]
    end if
    if not find(mb,{32,64}) then ?9/0 end if -- sanity check

    sequence cl = command_line()
    string res = cl[1]
    string file = get_file_name(res)
    sequence cpaths = split_path(res)
    string crun = cpaths[$]
    cpaths = cpaths[1..$-1]
--p2js
--  cpaths = cpaths[1..find("demo",lower(cpaths))-1]
    cpaths = cpaths[1..find("demo",lower(deep_copy(cpaths)))-1]
    res = join_path(cpaths,1)   -- eg/ie ../Phix/demo/pGUI/ -> ../Phix/

--  sequence vset = reorderW(vsets[plat])
    sequence vset = vsets[plat]
    if not bPrefW then vset = reverse(vset) end if
    -- permitted filenames
--  if plat=WINDOWS then
--      vset = {"pw.exe","p.exe","pw64.exe","p64.exe","pw32.exe","p32.exe","pth.exe"}
--  else -- platform()=LINUX
----        vset = {"p","phix","phix64","phix32","pth"}
--      vset = {"p","p32","p64","pth"}
--  end if
    string filepath = join_path({res,crun})
    if not validexe(crun,vset)
    or get_file_type(filepath)!=FILETYPE_FILE
    or ibits(filepath,plat)!=mb then

        sequence paths = {res}
        paths = add_path(paths,current_dir())
        if plat=WINDOWS then
            paths = add_path(paths,`C:\Program Files (x86)\Phix`)
            paths = add_path(paths,`C:\Program Files\Phix`)
            paths = add_paths(paths,split(getenv("PATH"),';'),{"phix","bin"})
        else
            paths = add_path(paths,join_path({getenv("HOME"),"phix"}))
            paths = add_paths(paths,split(getenv("PATH"),':'),{"phix","bin"})
        end if

        string maybe = "", definitely = ""
        for i=1 to length(paths) do
            for j=1 to length(vset) do
                filepath = join_path({paths[i],vset[j]})
                if get_file_type(filepath)=FILETYPE_FILE then
                    integer mbi = ibits(filepath,plat)
                    if mbi!=0 then
                        maybe = filepath
                        if mbi=mb then
                            definitely = maybe
                            exit
                        end if
                    end if
                end if
            end for
            if length(definitely) then exit end if
        end for
        filepath = iff(mbmb?definitely:maybe)
    end if
    if bQuote then
        filepath = enquote(filepath)
    end if
    return filepath
end function

global procedure requires(object x, bool bPrefW=false)
--
-- x: should be eg "0.8.2" for a version() requirement, or a
--                  WINDOWS/LINUX/WEB platform() check, or
--                  32/64 for a machine_bits() check.
--
-- Note there is a hand-translated version of this in p2js.js
--
    if not vinit then initv() end if
    if string(x) then
        string v = version()
        sequence reqs = scanf(substitute(x,"."," "),"%d %d %d"),
                 acts = scanf(substitute(v,"."," "),"%d %d %d")
        if length(reqs)!=1
        or length(acts)!=1
        or reqs>acts then
            crash("requires %s, this is %s",{x,v},2)
        end if
--  elsif x>0 and x<length(platforms) then
    elsif x>=0 and x<31 then
        if x!=platform() then
            string that = platforms[min(max(x,1),JS)],
                   this = platforms[platform()]
            if x=5 then -- (technically 5==JS+DOS32...)[1]
                x = not find(platform(),{WINDOWS,LINUX})                -- (aka not JS)
                that = "WINDOWS or LINUX"
            elsif x=6 then -- (validly! 6==JS+WINDOWS)
                x = not find(platform(),{WINDOWS,JAVASCRIPT})           -- (aka not LINUX)
                that = "WINDOWS or JAVASCRIPT"
            elsif x=7 then -- (technically 7==JS+LINUX...)
                x = not find(platform(),{WINDOWS,LINUX,JAVASCRIPT})     -- (all[?])
                that = "WINDOWS or LINUX or JAVASCRIPT"
            elsif x=8 then -- (technically 8==WINDOWS+LINUX+JS...)
                x = not find(platform(),{LINUX,JAVASCRIPT})             -- (aka not WINDOWS)
                that = "LINUX or JAVASCRIPT"
            elsif x=0 then
                x = 1                                                   -- (aka none)
                that = "FIXING(!)"
            end if
            -- [1] note that WINDOWS+LINUX yields JS, not what you meant!
            if x then
                crash("requires %s, this is %s",{that,this},2)
            end if
        end if
    elsif x!=machine_bits() then
        integer m = abs(x)
        sequence cl = command_line()
        string {c1,c2} = cl, cmd, msg
        bool precompiled = (c1==c2)
        if not precompiled then
            if bPrefW=-1 then bPrefW = find('w',get_file_base(c1))!=0 end if
            string p = get_interpreter(false,{m},platform(),bPrefW)
            if c1!=p or m!=x then
                cl[1] = p
                cmd = join(apply(cl,enquote))
                msg = iff(m==x?sprintf("%d bit",m):"restart")
                printf(1,"requires %s: %s\n",{msg,cmd})
                printf(1,"Press Escape to abandon, Enter to retry as above...")
                if not find(upper(wait_key()),{'Q','N',#1B}) then
                    puts(1,"\n")
                    {} = system_exec(cmd)
                end if
                abort(0)
            end if
        end if
        msg = iff(precompiled?"incorrectly packaged?"
                :"no such interpreter could be found")
        crash("requires %d bit (%s)...",{m,msg},2)
    end if
end procedure

