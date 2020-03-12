--
-- run.e
--
-- Run an application using a worker thread, so that system_exec() does 
-- not stall the main GUI/entire application. (Alternatively, you could
-- use set_system_doevents, see below.)
--
--TODO: default for opening chm files

--#withtype Ihandle
--#withtype Ihandln

include src/ext.e as extns  -- (for getExtRunWith() namespace)

integer msgcs = 0   -- critical section for queue/count updates

integer rmsg = 0    -- number of messages in queue

integer wcount = 0  -- number of active worker threads
                    -- (note: wcount can drop to 0 with rmsg>0)

-- Aside: This message queue is fairly unlikely to ever be longer 
--        than a single element, but sometimes two processes will
--        decide to terminate near-simultaneously. 
--        Of course in such a situation the "jump to error" of at
--        least one will flicker on screen and be lost anyway, or
--        maybe an "ex.err in use" prompt will stall things which
--        might cause the message queue grow to several entries.
--        Whatever, the important thing is to cope not crash.
--        Put whatever you need in the message queue: there is no
--        constraint beyond what is in this one source file.

sequence rmsgs      -- message queue

Ihandle listener    -- timer for polling the message queue
                    -- (Of course I would prefer a more direct
                    --  wake-up/message, but 4 times/sec is not
                    --  going to hurt, plus we switch it off
                    --  when there are no more run() running.)

-- The following two routines could be inlined as part of run()
-- and listen_cb() respectively, but I thought it was slightly 
-- clearer to factor out the locking/manipulation of rmsgs[].

procedure add_msg(object msg)
-- called by worker thread, ie run()
    enter_cs(msgcs)
    rmsgs = append(rmsgs,msg)
    rmsg += 1
    leave_cs(msgcs)
end procedure

function getmsg()
-- called by main thread, via listen_cb()
object res
    enter_cs(msgcs)
    res = rmsgs[$]
    rmsgs = rmsgs[2..$]
    rmsg -= 1
    leave_cs(msgcs)
    return res
end function

function listen_cb(Ihandle /*ih*/)
-- called by main thread (on a timer)
    --
    -- NB: Order of these tests is critical; we must load wcount before rmsg.
    --     While logically we would rather test rmsg first, doing so causes
    --     a race condition, because run() may do rmsg+=1 then wcount-=1.
    --     If we load wcount first and find it 0, we know there are no more
    --     pending rmsg+=1, and hence the two values must correlate, but if
    --     loaded the other way round, any such certainty evaporates.
    --     (note that wcount+=1 only occurs in the same thread as this and 
    --      therefore cannot cause a race condition, same for rmsg-=1.)
    --     (I should also remark that it is perfectly safe to test shared
    --      integers for 0 without any locking, unlike atoms/sequences,
    --      though #100 <-> #0FF might be an issue on an 8-bit bus(!).)
    --     Alternatively, I might suggest:
    --      enter_cs(msgcs)
    --      integer {l_rmsg,l_wcount} = {rmsg,wcount} -- (either way round)
    --      leave_cs(msgcs)
    --     Then you can test the l_xxx in any order you like, since you
    --      know they must always form a corresponding pair/snapshot.
    --
    if wcount=0 and rmsg=0 then
        IupSetAttribute(listener,"RUN","NO")
    elsif rmsg!=0 then
        object res = getmsg()
--DEV: (open ex.err/jump to error)
--?"ex.err??"
-- (res is {res,cmd,path})
        pp({"run.e, listen_cb, line 91",res},{pp_Pause,0})
    end if
    return IUP_DEFAULT
end function

function quote(string s)
    if s[1]!='\"'
    and find(' ',s) then
        s = sprintf(`"%s"`,s)
    end if
    return s
end function

function get_cmd(integer c, integer return_args)
string name = filenames[c]
string ext = get_file_extension(name)
string cmd = extns:getExtRunWith(ext)
    if length(cmd)!=0 then
        string path = filepaths[c]
        string fullpath = path&name
        if match(`"%s"`,cmd) then
            cmd = sprintf(cmd,{fullpath})
        elsif match("%s",cmd) then
            cmd = sprintf(cmd,{quote(fullpath)})
        else
--          -- (quote(cmd) c/should have been done in ext.e, 
--          --  but it does not hurt to try it again here.)
            sequence args = {quote(cmd),quote(fullpath)}
            if return_args then return args end if
            cmd = sprintf("%s %s",args)
        end if
    end if
    return cmd
end function

Ihandln run_dlg = NULL,
        run_txt,
        t_comp,
        t_list,
        l_type,
        t_diag,
        t_nrun,
        t_lint

procedure run(string cmd, string path)
-- worker thread (so system_exec() does not stall the whole app)
    {} = chdir(path)
--  atom res = system_exec(cmd)
--open `C:\Program Files (x86)\Phix\demo\libxlsxwriter\js\pendulum1.html`
    if length(cmd)>5 
    and cmd[1..5]="open " then  -- (note the space)
        cmd = cmd[6..$]
    end if
    -- Hmm/DEV seems we need this... will likely fail, though,
    --         for eg "dir *.exe", "p.exe test > out.txt", ...
    integer mode = iff(match(".exe",cmd)?2:4)
    atom res = system_exec(cmd,mode)
--?{"run",cmd,res}
--?res
    if res!=0
    and res!=#C000013A then -- terminated by Ctrl C
        add_msg({res,cmd,path})
    end if
    enter_cs(msgcs)
    wcount -= 1
    leave_cs(msgcs)
end procedure
constant r_run = routine_id("run")

--/*Alternative:
procedure loop_step()
    if IupLoopStep()=IUP_CLOSE then
        -- note: might not be instant...
        IupExitLoop()
    end if
end procedure
set_system_doevents(routine_id("loop_step"),{})
Then you can use system_exec() directly, instead of create_thread and 
without a queue, any locking, or a listener on a timer. However you
might struggle to close Edix and leave any invoked apps running[??].
--*/

string last_cmd = ""

procedure run_currfile(string cmd)
atom hThread
    if length(cmd)!=0 then
        enter_cs(msgcs)
        wcount += 1
        leave_cs(msgcs)
        IupSetAttribute(listener,"RUN","YES")
--DEV reindent no likey
--      atom hThread = create_thread(r_run,{cmd,filepaths[currfile]})
        last_cmd = cmd
        hThread = create_thread(r_run,{cmd,filepaths[currfile]})
    end if
end procedure


procedure set_run_txt()
sequence args = get_cmd(currfile,1)
    if string(args) then
        IupSetStrAttribute(run_txt,"VALUE",args)
        IupSetInt({t_comp,t_list,l_type,t_diag,t_nrun,t_lint},"ACTIVE",0)
    else
        IupSetInt({t_comp,t_list,t_diag,t_nrun,t_lint},"ACTIVE",1)
        IupSetInt(l_type,"ACTIVE",IupGetInt(t_list,"VALUE"))
        if IupGetInt(t_comp,"VALUE") then
            args[1] &= " -c"
        end if
        if IupGetInt(t_list,"VALUE") then
            if IupGetInt(l_type,"VALUE")=1 then
                args[1] &= " -l"
            else
                args[1] &= " -dumpil"
            end if
        end if
        if IupGetInt(t_diag,"VALUE") then
            args[1] &= " -nodiag"
        end if
        if IupGetInt(t_nrun,"VALUE") then
            args[1] &= " -norun"
        end if
        if IupGetInt(t_lint,"VALUE") then
            args[1] &= " -lint"
        end if
        IupSetStrAttribute(run_txt,"VALUE","%s %s",args)
    end if
end procedure

function compile_cb(Ihandle /*t_comp*/, integer state)
-- (also used for nodiag, norun, and lint)
    set_run_txt()
    return IUP_DEFAULT
end function
constant cb_compile = Icallback("compile_cb")

function listing_cb(Ihandle /*t_list*/, integer state)
    IupSetInt(l_type,"ACTIVE",state)
    set_run_txt()
    return IUP_DEFAULT
end function
constant cb_listing = Icallback("listing_cb")

--int function (Ihandle *ih, char *text, int item, int state)
function type_cb(Ihandle /*l_type*/, atom /*pText*/, integer /*item*/, integer /*state*/)
    set_run_txt()
    return IUP_DEFAULT
end function
constant cb_type = Icallback("type_cb")

constant help_text = """
Full paths are always used, even when (eg) "pw test" would actually be fine.

Setting/unsetting the checkboxes inserts/removes the required flags. 

Phix produces two types of listing files: assembly and intermediate language;
the latter is normally only useful during the development of Phix itself.

The -lint option (rarely useful) is documented in Phix.chm/glossary.

Previously run commands from this session are listed and can be selected.

The default command is constructed using details from Options/File Extensions.

Note: if the command specified in Options/File Extensions contains "%s" then
the checkboxes in this dialog are disabled, however you can manually edit the
command string instead."""

--DEV bug: esc works fine, unless you F1/esc first, then it bells...
-- (actually, it bells first time in, but not subsequently???)
function help_cb(Ihandle /*ih*/)
--  IupSetGlobal("PARENTDIALOG", run_dlg)
    IupMessage("Parameterised Run Help",help_text)
--  IupSetFocus(run_dlg)
--  IupSetGlobal("PARENTDIALOG", dlg)
    return IUP_DEFAULT
end function
constant cb_help = Icallback("help_cb")

sequence prior_cmds = {}

function ok_cb(Ihandle /*bt_ok*/)
string cmd = IupGetAttribute(run_txt, "VALUE")
    if length(cmd)!=0 then
        -- first, maintain the text dropdown:
        if not find(cmd,prior_cmds) then
            prior_cmds = append(prior_cmds,cmd)
            IupSetAttribute(run_txt,sprintf("%d",length(prior_cmds)),cmd)
        end if
        run_currfile(cmd)
    end if
    return IUP_CLOSE
end function
constant cb_ok = Icallback("ok_cb")

function cancel_cb(Ihandle /*bt_cancel*/)
    return IUP_CLOSE
end function
constant cb_cancel = Icallback("cancel_cb")

function key_cb(Ihandle /*ih*/, atom c)
    if c=K_F1 then 
        return help_cb(run_dlg)
--  elsif c=K_ESC then
--      return IUP_CLOSE
    end if
    return IUP_DEFAULT
end function

procedure open_rundlg()
Ihandle run_lbl, buttons, command, box, bt_help, bt_ok, bt_cancel
    if run_dlg=NULL then
--DEV Run With / Treat as Extension, Always use specified command...
        run_lbl = IupLabel("cmd")
        run_txt = IupList("EXPAND=HORIZONTAL, DROPDOWN=YES, EDITBOX=YES")
        t_comp = IupToggle("&compile",cb_compile,"PADDING=10x2")
        t_list = IupToggle("&listing",cb_listing,"PADDING=10x2")
        l_type = IupList(cb_type,"DROPDOWN=YES, EDITBOX=NO")
        IupSetAttribute(l_type,"1","asm")
        IupSetAttribute(l_type,"2","il")
        IupSetInt(l_type,"VALUE",1)
        IupSetInt(l_type,"ACTIVE",0)
        t_diag = IupToggle("nodia&g",cb_compile,"PADDING=10x2")
        t_nrun = IupToggle("no&run",cb_compile,"PADDING=10x2")
        t_lint = IupToggle("l&int",cb_compile,"PADDING=10x2")
        bt_help = IupButton("Help (F1)",cb_help,"PADDING=10x2")
        bt_ok = IupButton("OK",cb_ok,"PADDING=10x2")
        bt_cancel = IupButton("Cancel",cb_cancel,"PADDING=10x2")

        command = IupHbox({run_lbl,run_txt},"NORMALIZESIZE=VERTICAL")
        buttons = IupHbox({t_comp,t_list,l_type,
                           IupFill(),t_diag,t_nrun,t_lint,
                           IupFill(),bt_help,bt_ok,bt_cancel},
                          "NORMALIZESIZE=VERTICAL")
        box = IupVbox({command,
                       buttons})
        IupSetAttribute(box, "MARGIN", "10x10")
        IupSetAttribute(box, "GAP", "5")

        run_dlg = IupDialog(box)
        IupSetAttribute(run_dlg, "TITLE", "Parameterised Run")
        IupSetAttributeHandle(run_dlg, "DEFAULTENTER", bt_ok)
        IupSetAttributeHandle(run_dlg, "DEFAULTESC", bt_cancel)
        IupSetAttributePtr(run_dlg, "PARENTDIALOG", dlg)
--      IupSetAttributeHandle(run_dlg, "PARENTDIALOG", dlg)
        IupSetCallback(run_dlg, "K_ANY", Icallback("key_cb"));
    end if
    set_run_txt()
    IupPopup(run_dlg, IUP_CENTERPARENT, IUP_CENTERPARENT)
end procedure

global procedure F5run(integer ctrl, integer shift)--, integer alt)
-- called by main thread
--?"F5run"
    if msgcs=0 then
        msgcs = init_cs()
        rmsg = 0
        rmsgs = {}
        wcount = 0
        listener = IupTimer(Icallback("listen_cb"),250)
    end if
    if ctrl then
        open_rundlg()
    elsif shift then
        if length(last_cmd) then
            run_currfile(last_cmd)
        else
            open_rundlg()
        end if
    else
        string cmd = get_cmd(currfile,0)
        if length(cmd) then
            run_currfile(cmd)
        else
            open_rundlg()
        end if
    end if
end procedure

--/*
--  atom pos = IupGetInt(tabs, "VALUEPOS")
--  if pos=IUP_INVALID then
--      -- nothing selected
--      return IUP_IGNORE
--  end if
--
--  atom sci = IupGetChild(tabs, pos)
--  sequence path = IupGetAttribute(sci, "PATH")
--  sequence name = get_file_name(path)
--
--if platform()=WINDOWS then
----                sequence eui = locate_file( "euiw.exe" )
----                sequence cmd = build_commandline({getenv("COMSPEC"), "/C", eui, name })
--else
----                sequence eui = locate_file( "eui" )
----                sequence cmd = build_commandline({"/usr/bin/x-terminal-emulator", "-e", eui, name })
--end if
--OR:
----    string cmd = iff(platform()=WINDOWS?getenv("COMSPEC"):"/usr/bin/x-terminal-emulator")
----    string flags = iff(platform()=WINDOWS?"/C":"-e")
----    string eui = locate_file(platform()=WINDOWS?"pw.exe":"phix")!
----    string eui = locate_file(platform()=WINDOWS?"pw.exe":"p")
----    string cmd = build_commandline({cmd, flags, eui, name })
--
--  {} = chdir(get_file_path(path))
--  ?9/0
--  --      pipeio:exec( cmd, pipeio:create() )
--


Hi,

  I guess the simplest solution would be to use a IupTimer (on the main
thread) and a mutex-condition pair, Inside the timer callback you check for
a flag, for instance. But that check must be protected (lock+check+unlock),
also the flag updated from the other thread must be protected too
(lock+update+unlock). Not the best solution, but a a simple one that works
fine.

Best,
Scuri

--
-- startw.exw 
-- Start an application via the command line. 
--  
-- Copyright (c) 2014-2016 Greg Haberek <ghaberek@gmail.com> 
--  
-- Permission is hereby granted, free of charge, to any person obtaining a copy 
-- of this software and associated documentation files (the "Software"), to deal 
-- in the Software without restriction, including without limitation the rights 
-- to use, copy, modify, merge, publish, distribute, sublicense, and/or sell 
-- copies of the Software, and to permit persons to whom the Software is furnished 
-- to do so, subject to the following conditions: 
--  
-- The above copyright notice and this permission notice shall be included in all 
-- copies or substantial portions of the Software. 
--  
-- THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR 
-- IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, 
-- FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE 
-- AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, 
-- WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR 
-- IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE. 
--  
-- History: 
-- v0.1 - Initial release 
-- v0.2 - Fixed issue with spaces 
--      - Added "-s/--show" option 
 
include std/cmdline.e 
include std/dll.e 
include std/filesys.e 
include std/map.e 
include std/machine.e 
include std/search.e 
include std/sequence.e 
include std/text.e 
without warning 
 
constant CMD_OPTS = { 
    { "d", "path", "Starting directory", {HAS_PARAMETER, ONCE}, -1 }, 
    { "s", "show", "Show method: normal, hidden, minimized, maximized (default: normal)", {HAS_PARAMETER,ONCE}, -1 } 
} 
 
constant HELP_TEXT = { 
    "usage:", 
    `   startw [-d path] [-s show] "command" [parameters]` 
} 
 
constant 
    SW_HIDE            =  0, 
    SW_SHOWNORMAL      =  1, 
    SW_SHOWMINIMIZED   =  2, 
    SW_SHOWMAXIMIZED   =  3, 
    SW_MAXIMIZE        =  3, 
    SW_SHOWNOACTIVATE  =  4, 
    SW_SHOW            =  5, 
    SW_MINIMIZE        =  6, 
    SW_SHOWMINNOACTIVE =  7, 
    SW_SHOWNA          =  8, 
    SW_RESTORE         =  9, 
    SW_SHOWDEFAULT     = 10, 
$ 
 
constant SHOW_NAMES = { 
    { "normal",    SW_SHOWNORMAL }, 
    { "hidden",    SW_HIDE }, 
    { "minimized", SW_SHOWMINIMIZED }, 
    { "maximized", SW_SHOWMAXIMIZED } 
} 
 
constant shell32 = open_dll( "shell32.dll" ) 
constant xShellExecute = define_c_func( shell32, "ShellExecuteA", {C_HANDLE,C_POINTER,C_POINTER,C_POINTER,C_POINTER,C_INT}, C_HANDLE ) 
 
-- acceptable values for lpOperation parameter of ShellExecute() 
constant OPERATIONS = { "edit", "explore", "find", "open", "print" } 
 
-- allocates a string (if sequence) or an amount of data (if atom) 
-- and returns NULL if the sequence is empty or the atom is zero 
function allocate_null( object s, integer cleanup = 0 ) 
     
    if sequence( s ) then 
         
        if length( s ) = 0 then 
            return NULL 
        end if 
         
        return allocate_string( s, cleanup ) 
    end if 
     
    if equal( s, 0 ) then 
        return NULL 
    end if 
     
    return allocate_data( s, cleanup ) 
end function 
 
-- performs an operation on a specified file 
function ShellExecute( atom hwnd, sequence operation, sequence file, sequence parameters, sequence directory, integer show_cmd ) 
     
    atom lpOperation    = allocate_null( operation, 1 ) 
    atom lpFile         = allocate_null( file, 1 ) 
    atom lpParameters   = allocate_null( parameters, 1 ) 
    atom lpDirectory    = allocate_null( directory, 1 ) 
    integer nShowCmd    = show_cmd 
     
    return c_func( xShellExecute, {hwnd,lpOperation,lpFile,lpParameters,lpDirectory,nShowCmd} ) 
end function 
 
procedure main() 
     
    map opts = cmd_parse( CMD_OPTS, {HELP_RID, HELP_TEXT} ) 
     
    sequence path   = map:get( opts, "path", current_dir() ) 
    sequence show   = map:get( opts, "show", "normal" ) 
    sequence extras = map:get( opts, EXTRAS ) 
     
    sequence operation = "open" 
    sequence command = "" 
    sequence parameters = "" 
    sequence directory = path 
     
    integer show_cmd = vlookup( show, 
        SHOW_NAMES, 1, 2, SW_SHOWNORMAL ) 
     
    if length( extras ) = 0 then 
         
        -- no options provided, run %ComSpec% (i.e. cmd.exe) 
        command = getenv( "ComSpec" ) 
         
    else 
         
        -- is the first option an operation? 
        if find( extras[1], OPERATIONS ) then 
             
            -- get the operation 
            operation = extras[1] 
             
            -- and trim it off the list 
            extras = extras[2..$] 
             
        end if 
         
        -- do we have an command? 
        if length( extras ) > 0 then 
             
            -- get the command 
            command = extras[1] 
             
            -- and trim it off the list 
            extras = extras[2..$] 
             
            -- get the path to the command 
            command = locate_file( command ) 
             
        end if 
         
        -- quote extras to preserve spaces 
        for i = 1 to length( extras ) do 
            extras[i] = quote( extras[i] ) 
        end for 
         
        -- join the extras into a string of parameters 
        parameters = stdseq:join( extras, ' ' ) 
         
    end if 
     
    -- translate the directory to its canonical path 
    directory = canonical_path( directory, 1 ) 
     
    -- run the command! 
    ShellExecute( NULL, operation, command, parameters, directory, show_cmd ) 
     
end procedure 
 
main() 
--*/
