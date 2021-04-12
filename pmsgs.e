--
-- pmsgs.e
-- Error and warning message subsystem for p.exw
--
-- Implements:  Warnings(), Warn(sequence msg), Abort(sequence msg), 
--              Duplicate(), Expected(sequence str),
--              Fatal(sequence msg), Undefined()
--

--DEV you can comment this lot out if needbe (removes gui messages)
----/* Not required for Phix (RDS include is just so... elegant ...)
--include builtins\dll.e
--include builtins\machine.e
--include builtins\misc.e
--include demo\arwen\misc_arwen.e
--include demo\arwen\message_box.ew -- stand alone message box dialog
--include demo\arwen\constants.ew -- Win32 constant declarations
--include demo\arwen\dll_links.ew -- Linking to various dll files
--include demo\arwen\structures.ew -- basic structure arrangement
--include demo\arwen\Quick_Allocations.ew -- code to manage pre-allocated memory blocks
--include demo\arwen\classes.ew -- predefined window/control class defaults
--include demo\arwen\arwen.ew
----*/
--include perror.ew -- error message handler (gui)

include peama.ew    -- edita message area routines
integer peama
        peama = 1
--DEV:: prevent a hang... dunno why, must fix this one day!
function nott00()
    -- (test/t00.exw is the scratch file for test/terror.exw, btw)
    -- (test/terror.exw is the "test error handling" thing, btw)
--  if equal(mainfile,"t00.exw") then
    if batchmode then
        peama = 0
    end if
    return peama
end function

--/* Not required for Phix (defined as opWaitKey):
include builtins\get.e  -- wait_key()
--*/
integer r_proemh
        r_proemh=-1
-- r_proemh = routine_id("proemh")
global function get_r_proemh()
    return r_proemh
end function

global procedure set_r_proemh(integer rid)
    r_proemh = rid
end procedure

constant MB_OK=0,
         MB_DEFBUTTON1=0

global procedure Message(sequence msg)
-- currently only used when chdir() attempts fail.
--  if peama then
    if nott00() then
        if eamsg(msg) then return end if
        peama = 0
    end if
--if not batchmode then
    if r_proemh=-1 then
        puts(1,msg)
        if wait_key() then end if
    else
        if call_func(r_proemh,{"Warning",msg,MB_OK}) then end if
    end if
--end if
end procedure

--with trace
global function expandIntoLines()
--
--  Converts a single string file to an array of lines, eg
--      "Dear Sir,\n\nThank you for..."
--  to
--      {"Dear Sir\n",
--       "\n",
--       "Thank you for..."}
--  The compiler reads source files as single strings since
--  it is much faster to allocate that compared to thousands
--  of strings, one for each individual line. However, once 
--  we have to report an error or perform source-level trace
--  it is far easier to deal with the latter format. Neither
--  do we care, in such cases, about an extra second or two.
--
--  "\r\n" and/or "\n\r" pairs are converted to single "\n",
--  and lone "\r" end up as lone "\n", so it should work on
--  Windows/Linux/OSX/etc without alteration, though it has
--  not been tested to do so as yet.
--
sequence res
integer start, ch, pair, skip
    res = {}
    linestarts = {}
    start = 1
    skip = 0
    for i=1 to length(text) do
        if skip then
            skip -=1
        else
            ch = text[i]
            if ch='\n'
            or ch='\r' then
                if ch='\r' then
                    text[i] = '\n'
                    pair = '\n'     -- pair \r with \n
                else
                    pair = '\r'     -- pair \n with \r
                end if
                res = append(res,text[start..i])
if ch='\r' then
    -- 29/1/10 put it back otherwise lines get counted twice!
    text[i]='\r'
end if
                linestarts = append(linestarts,start)
                start = i+1
                if start<=length(text) then -- avoid ioob
--trace(1)
                    ch = text[start]
                    if ch=pair then -- "\r\n" or "\n\r" found:
                        skip = 1
                        start += 1
--added 10/2/2012: (treat \n\n\r as \n\r)
                    elsif ch='\r'
                      and start<length(text)
                      and text[start+1]='\n' then
--puts(1,"double skip\n")
                        skip = 2
                        start += 2
                    end if
                end if
            end if
        end if
    end for
    return res
end function

global function packLines(sequence lines)
--
--  opposite of expandIntoLines (for intellisense)
--
string res
    res = ""
    for i=1 to length(lines) do
--      res &= lines[i]
        res &= lines[i][1..$-1]&"\r\n"
    end for
    return res
end function

--DEV not properly used yet??
global integer eLine, eCol
global procedure convertToLineColumn(integer idx)
-- Convert a single-string-source index to a line/column
--  (see also expandIntoLines() above)
integer li
-- 12/03/2010:
    if idx=0 then
        eLine = 1
        eCol = 1
        return
    end if
    for i=length(linestarts) to 1 by -1 do
        li = linestarts[i]
        if idx>=li then
--      if idx>=li or i=1 then      -- no go
            eLine = i
            eCol = idx-li+1
            return
        end if
    end for
    eLine=9/0   -- oops!
--DEV or, if certain that tokline is right, just:
--  return idx-linestarts[tokline]+1
end procedure



sequence warnmsgs
         warnmsgs={}
--  warnmsgs = append(warnmsgs,{txtline,tokcol,{msg,currFile(),tokline}})
-- (each element is a sequence of length 3 for invoking proemh)

global -- for profile pause (read only, test above "Press Enter...")
integer nwarns
        nwarns=0
integer nshown
        nshown=0

integer carat
sequence wi3, txtline
function wFormat(sequence wi, integer choppath)
sequence msg, filepath, lineno, witxt
integer msglen, k, filelen
            txtline = wi[1]
            carat = wi[2]
            wi3 = wi[3]

            msg = wi3[1]
            filepath = wi3[2]
            lineno = sprintf("%d",wi3[3])

            msglen = length(msg)
            k = carat+10+msglen-80
            if k>0 and k<carat then
                carat -= k
                txtline = ".."&txtline[k+3..length(txtline)]
            end if
            k = length(txtline)
if k then
            if k>78 then
                txtline=txtline[1..77]&".."
            end if
            witxt = sprintf("%s:%s\n%s\n%s^Warning: %s\n",
                            {filepath,lineno,txtline,repeat(' ',carat-1),msg})
            nshown+=3
else
--if 10+33+31+32>79 then
--  32 = ?..-1
--end if
--if 0 then -- 23/9/09... [DEV]
--          k = 67-length(lineno)-msglen
--          filelen = length(filepath)
--          if k<filelen then
--              filepath = ".."&filepath[filelen-k+2..filelen]
--          end if
--else
    if choppath then    -- added 5/3/2010
            filelen = length(filepath)
            if 11+filelen+length(lineno)+msglen>79 then
                -- chop the path to make more space for the message
                for j=filelen to 1 by -1 do
                    if find(filepath[j],"\\/") then
                        filepath = filepath[j+1..filelen]
                        exit
                    end if
                end for
            end if
    end if
--end if
            witxt = sprintf("Warning: %s:%s %s\n",{filepath,lineno,msg})
            nshown+=1
end if
    return witxt
end function

--integer pr    -- .err file, or 0
--      pr = 0

--with trace
--DEV call from pDiagN.e??
global procedure Warnings(integer fn)
object wi, buttons
sequence 
--wi3, msg, filepath, lineno, 
witxt
--, txtline
integer 
--k, 
fn1
, 
r
--msglen, 
--filelen
--, carat
    if nwarns then
if not batchmode then
        puts(1,"\n")    -- just in case
end if
        if fn!=1 then
            puts(fn,"\n")   -- just in case
        end if
        fn1 = 1
        buttons = {"Next","&Skip remainder",MB_DEFBUTTON1}
--      for i=1 to length(warnmsgs) do
        for i=length(warnmsgs) to 1 by -1 do    -- easier to edit this way (line nos don't change!)

            wi = warnmsgs[i]

            witxt = wFormat(wi,1)

            if fn!=1 then
                puts(fn,witxt)
            end if
            if nwarns>0 and fn1 then
                if r_proemh=-1 then
--                  nshown+=1
                    puts(1,witxt)
--                  if remainder(nshown,7)=0    -- 7 3-line-warnings +2 fit on 25-line screen, 8 don't.
--                  if remainder(nshown,6)=0    -- 7 3-line-warnings +2 fit on 25-line screen, 8 don't.
                    if nshown>=20   -- leave space for one more 3-line-warning to fit on 25-line screen
                    and i>1 then -- not if about to give final prompt.
                        nshown = 0
if not batchmode then
                        puts(1,"\nPress Enter for next page or 'q' to quit...\n")
                        if find(wait_key(),"qQ") then
                            -- stop displaying, carry on writing to pr, and prevent
                            -- final 'Press Enter' message:
--                          nwarns=-1
                            fn1=0
                        end if
end if
                    end if
                else
                    if i=1 then
                        buttons = MB_OK
                    end if
                    wi[1] = txtline
                    wi[2] = carat
                    wi[3] = sprintf("%s\n\n%s:%d",wi3)
if not batchmode then
                    r = call_func(r_proemh,{"Warning",wi,buttons})
end if
                    if not batchmode and r=2 then
--                      nwarns = 0
                        fn1 = 0
                    else
                        nwarns -= 1
                    end if
                end if
            end if
        end for
        if r_proemh=-1
        and nwarns>0
        and fn1
--      and (not testall or pauseOnWarnings) then
        and pauseOnWarnings
--      and not equal(mainfile,"t00.exw") 
        and not batchmode
        then
if DEBUG then
            puts(1,"\nPress Enter, or d for diagnostics...")
            if find(wait_key(),"dD") then ?9/0 end if
else
            puts(1,"\nPress Enter...")
            if wait_key() then end if
end if
            puts(1,"\n")
        end if 
        warnmsgs = {}
        nwarns = 0
    end if
-- added 3/3/10:
    if fn!=1 then
        close(fn)
    end if
    nshown = 0
end procedure

function currFile()
    {integer pathno, string res} = filenames[fileno]
    if pathno!=0 then res = filepaths[pathno]&res end if
    return res
end function

integer fromWarnU
        fromWarnU = 0

--DEV remove this someday:
global integer no_oops          -- (should only be set for fatal calls)
               no_oops = 0      -- (currently only used when backpatching fwd calls,
                                --  we should really be saving line numbers for that)

with trace
global procedure Abort(sequence msg)
-- error with source line
sequence errline, txtline, fni
integer lt, k, fn, sli, sfi
--trace(1)
    if equal(expandedYet[fileno],0) then
        text = allfiles[fileno]
        exptext[fileno] = expandIntoLines()
        expandedYet[fileno] = linestarts
        text = allpfiles[fileno]
    else
        linestarts = expandedYet[fileno]
    end if
--?linestarts
--?tokline
--DEV bug:
    if not fromWarnU then
        convertToLineColumn(tokcol)
        if tokline!=eLine then
            if no_oops=0 then
                printf(1,"oops: see Abort() [tokline:%d, eLine:%d, tokcol:%d, eCol:%d]...\n",
                        {tokline,eLine,tokcol,eCol})
                batchmode = 0
            end if
            tokline = eLine
        end if
        tokcol = eCol
    else
        fromWarnU = 0
    end if
    if tokline>length(exptext[fileno]) then
        txtline = "<end of file>"
        tokcol = 1
    else
        txtline = exptext[fileno][tokline]
    end if
    while 1 do
        lt = find('\t',txtline)
        if lt = 0 then exit end if
        txtline[lt] = ' '
    end while
    lt = length(txtline)
    if lt and txtline[lt] = '\n' then
        txtline=txtline[1..lt-1]
    end if
--  pr = open("p.err","w")
    fn = open("ex.err","w")
    if r_proemh=-1 then
if not batchmode then
        puts(1,"\n")    -- just in case
end if
        k = find('\n',msg)
        if k=0 then
            k = length(msg)
        end if
        k = tokcol+1+k-80
--31/1/17:
--      if k>0 then
        if k>0 
        and k+3<length(txtline) then
            tokcol-=k
            txtline=".."&txtline[k+3..length(txtline)]
        end if
        k = length(txtline)
        if k>78 then
            txtline=txtline[1..77]&".."
        end if
    end if
--DEV this may prove easier:
    if tokcol>0 then errline = repeat(' ',tokcol-1) else errline = "" end if -- avoid -ve repeat count
if repl then
    errline = sprintf("%s\n%s^ %s\n",{txtline,errline,msg})
else
    errline = sprintf("%s:%d\n%s\n%s^ %s\n",{currFile(),tokline,txtline,errline,msg})
end if
--  errline = sprintf("%s:%d\n%s\n%s^ %s\n",{currFile(),tokline,txtline,repeat(' ',tokcol-1),msg})

    if match("edita\\builtins\\",lower(errline)) then
        -- Stack frame and #ilasm changes mean Phix >=0.6.3 is incompatible with
        --  files in the builtins directory copy shipped with Edita <= 0.3.5
        --  (the Edita copy only exists so you can get going just by installing
        --   the Edita download only, without needing to install Phix as well)
        errline &= "\n\n***RENAMING THE EDITA\\BUILTINS DIRECTORY MAY FIX THIS***\n\n"
    end if
--  if peama then
    if nott00() then
        if not eamsg(errline) then
            peama = 0
        end if
    end if

--  puts(fn,errline)
--  if fn=-1 then puts(fn,errline) end if
    puts(max(fn,1),errline)
    if r_proemh=-1 then
if not batchmode then
--printf(1,"batchmode = %d\n",batchmode)
        puts(1,errline)
        puts(1,"\n\n")
end if
        for i=scopelevel-1 to 1 by -1 do
--          if scopelines[i] then
            sli = scopelines[i]
            if sli and scopetypes[i]=S_File then    -- 26/2
                sfi = scopefiles[i]
                fni = filenames[sfi]
                errline = sprintf("...included by %s%s, line %d\n",{filepaths[fni[1]],fni[2],sli})
if not batchmode then
                puts(1,errline)
end if
                puts(fn,errline)
            end if
        end for
        if nwarns=0 then
--if not equal(mainfile,"t00.exw") then
if not batchmode and not repl then
    if DEBUG then
            puts(1,"\nPress Enter, or d for diagnostics...")
            if find(wait_key(),"dD") then ?9/0 end if
    else
            puts(1,"\nPress Enter...")
            if wait_key() then end if
    end if
            puts(1,"\n")
end if
        else
            k = 6 -- 5 (sourcefile/line\n,
                  --    source text\n,
                  --    hat/error message\n,
                  --    puts(1,\n\n) above)
                  -- plus one for luck,
            for i=1 to length(msg) do
                if msg[i]='\n' then
                    -- plus one for any embedded \n's
                    k+=1
                end if
            end for
            -- equivalent to k/3 warnings (as each is lineno/txt/hat)
--          nshown = floor(k/3)
            nshown = k
        end if
    else
        errline = sprintf("%s\n\n%s:%d",{msg,currFile(),tokline})
if not batchmode then
        if call_func(r_proemh,{"Error",{txtline,tokcol,errline},MB_OK}) then end if
end if
    end if
    Warnings(fn)
    close(fn)
    abort(1)
end procedure

global procedure Abort_multi(sequence msg, sequence abort_set)
integer f, pathno
sequence ff
sequence dbg = repeat(0,length(abort_set))
    msg &= "\n" & getname(ttidx,-2) & " is defined in:\n"
--DEV
--  msg &= expand_set(abort_set)
    for i=1 to length(abort_set) do
--      f = symtab[abort_set[i]][S_FPno]
        f = abort_set[i]
        dbg[i] = symtab[f]
        f = symtab[f][S_FPno]
--      if f then
            ff = filenames[f]
            pathno = ff[1]
            msg &= "    " & filepaths[pathno]&ff[2]
--      else
--          msg &= "    <builtin>"
--      end if
        if i=length(abort_set) then
            msg &= ".\n"
        else
            msg &= ",\n"
        end if
    end for
--DEV
--  if length(also_set) then
--      msg &= "\n and also in:\n"
    Abort(msg)
end procedure

global procedure Duplicate()
    Abort("Duplicate identifier: "&getname(ttidx,-2))
end procedure

global procedure Expected(sequence str)
    Abort(str&" expected")
end procedure

global procedure Fatal(string msg)
-- error not relating to a particular source line.
integer fn
--!/**/ #isginfo{msg,string,MIN,MAX,integer,-2}

    fn = open("ex.err","w")
    puts(fn,msg)
--removed 28/2/10 (is a Fatal ever useful in edita? Either it cannot open the main file
--                  or there's something else on the commandline it cannot understand...)
--  if peama then
--  if nott00() then
--      if not eamsg(msg) then
--          peama = 0
--      end if
--  end if
    if r_proemh = -1 then
--if not equal(mainfile,"t00.exw") then
if not batchmode then
        puts(1,msg)
    if DEBUG then
        puts(1,"\n\n\nPress Enter, or d for diagnostics...")
        --DEV pass msg or fn to pdiag??
        if find(wait_key(),"dD") then ?9/0 end if
    else
        puts(1,"\n\n\nPress Enter...")
        if getc(0) then end if  --DEV??
--      if wait_key() then end if
    end if
        puts(1,"\n")
end if
    else
        if call_func(r_proemh,{"Error",msg,MB_OK}) then end if
    end if
    Warnings(fn)
    abort(1)
end procedure

global procedure Undefined()
sequence name
    name = getname(ttidx,-2)
--  if find(name,{"machine_func","machine_proc"}) then
--      -- actually, this would be an internal error, since these are (now)
--      --  defined in psym.e, so 'cannot find pmach.e' is expected first.
--      Abort(name&" undefined.\n\nThis may be a result of using an RDS rather than a Phix standard include.")
--  els
    if find(name,{--"VC_COLOR","VC_MODE","VC_LINES","VC_COLUMNS",
                  --"VC_XPIXELS","VC_YPIXELS","VC_NCOLORS","VC_PAGES",
                  "all_palette", "draw_line", "ellipse", "get_all_palette",
                  "graphics_mode", "palette", "polygon", "sound",
--                "video_config",
--<li>image.e <small>(get_screen_char[DEV],put_screen_char[DEV],<br>
                  "display_image", "save_image", "save_screen", 
                  "get_active_page", "set_active_page",
                  "get_display_page", "set_display_page",
--<li>machine.e <small>(crash_file[DEV], crash_message[DEV], crash_routine[DEV],
--             register_block[DEV], unregister_block[DEV],<br>
                  "allocate_low", "free_low", "dos_interrupt",
                  "get_vector", "set_vector", "lock_memory", "tick_rate", "use_vesa",
                  "get_mouse", "mouse_events", "mouse_pointer",
                  "get_pixel", "pixel"}) then
        Abort("undefined:"&name&".\n\nPhix does not support DOS programming.")
--  elsif equal(name,"my_dir") then
--      Abort("undefined:"&name&".\n\nYou may have meant set_walk_dir_sort_rtn().")
    else
        Abort("undefined identifier "&name)
    end if
end procedure

global constant SQ_WARN=1
global procedure Warn(sequence msg, integer tokline, integer tokcol, integer warn_class=0)
sequence txtline, wi, witxt
integer lt
    if optset[OptWarning] or warn_class=SQ_WARN then
--8/7/2013: (added/undone: wasn't what I needed at the time, but something similar mey yet be rqd)
--      if batchmode then
--          Abort(msg)
--      end if
        if equal(expandedYet[fileno],0) then
            --DEV check this gets properly reset by callee?
            -- better yet, restore it ourselves!
            text = allfiles[fileno]
            exptext[fileno] = expandIntoLines()
            expandedYet[fileno] = linestarts
            text = allpfiles[fileno]
        else
            linestarts = expandedYet[fileno]
        end if
        lt = length(exptext[fileno])
        convertToLineColumn(tokcol)
        if tokline!=eLine then
            printf(1,"oops: see pmsgs:Warn() [tokline:%d, eLine:%d, tokcol:%d, eCol:%d]...\n",
                        {tokline,eLine,tokcol,eCol})
?{msg,warn_class}
            tokline = eLine
        end if
        tokcol = eCol
        if tokline<=0 then
            txtline = "<start of file>"
            tokcol = 1
        elsif tokline>lt then
            txtline = "<end of file>"
            tokcol = 1
        else
            txtline = exptext[fileno][tokline]
            while 1 do
                lt = find('\t',txtline)
                if lt = 0 then exit end if
                txtline[lt] = ' '
            end while
            lt = length(txtline)
            if lt and txtline[lt] = '\n' then
                txtline=txtline[1..lt-1]
            end if
        end if
        wi = {txtline,tokcol,{msg,currFile(),tokline}}
--      if peama then
        if nott00() then
            witxt = wFormat(wi,0)
            if eamsg(witxt) then return end if
            witxt = wFormat(wi,1)
            peama = 0
        end if
        nwarns+=1

--          if r_proemh=-1 then
--              warnmsgs &= sprintf("%s:%d\n%s\n%s^warning:%s\n",
--                                  {wi[3][2],  wi[3][3],wi[1],repeat(' ',wi[2]-1),wi[3][1]}
--                                  {currFile(),tokline,txtline,repeat(' ',tokcol-1),msg})
--          else
--              warnmsgs = append(warnmsgs,{txtline,tokcol,{msg,currFile(),tokline}})
        warnmsgs = append(warnmsgs,wi)
    --                          OR:     append {currFile(),tokline,txtline,tokcol,msg}) then:
                                        -- wi[4]=repeat(' ',wi[4]-1) -- for r_proemh=-1,
                                        -- and printf as above, else (r_proemh!=1):
                                        -- wi={wi[3],wi[4],sprintf("%s\n\n%s:%d",{wi[5],wi[1],wi[2]})}

--          end if
--      elsif nwarns=4 then
--          nwarns=5
--          if r_proemh=-1 then
--              warnmsgs &= "Too many warnings, remainder skipped.\n"
--          else
--              warnmsgs = append(warnmsgs,"Other warnings skipped.")
--          end if
--      end if
    end if
end procedure

--constant NTdesc = {"constant ",
--                 "variable ",
--                 "parameter ",
--                 "namespace ",
--                 "reserved word",
--                 "type ",
--                 "function ",
--                 "procedure "}
--global constant S_Const = 1,  -- a constant
--              S_GVar2 = 2,    -- global or static variable
--              S_TVar = 3,     -- temp or threadstack (local) variable/parameter
--              S_Nspc = 4,     -- namespace
--              S_Rsvd = 5,     -- reserved word
--              S_Type = 6,     -- Type of thermal yellow portable encryptor
--              S_Func = 7,     -- Function of finding unusual nonsense comments
--              S_Proc = 8      -- Procedure for private rotating obstacle counter

global procedure WarnU(integer siNTyp, sequence si, object msg, integer N)
-- unused/undefined etc messages from final scan
sequence errname, thisline, wi
--integer fileno
string fmsg
--trace(1)
--19/7/16:
if batchmode then return end if
    ttidx = si[S_Name]
    errname = getname(ttidx,-2)
    if N<=T_Ainc
    and and_bits(si[S_State],S_fwd) then
        -- Cannot really do any better than this if, because it is an
        --  autoinclude, we do not have a file (or line) number...
        -- (si[S_FPno] is still an index into binftab, btw, and
        --  tokcol would be zero when symint() was called.)
        fmsg = "fatal problem with autoinclude \"%s\""
        fmsg = sprintf(fmsg,{errname})
        Fatal(fmsg)
    end if
--DEV should these be tokcol? (no biggie) [edits made 24/07, clean this up >= Aug 1st]
    if siNTyp>=S_Type then
--      tokline = si[S_ErrR]
        tokcol = si[S_ErrR]
    else
--      tokline = si[S_ErrV]
        tokcol = si[S_ErrV]
    end if
    fileno = si[S_FPno]
    if equal(expandedYet[fileno],0) then
        text = allfiles[fileno]
        exptext[fileno] = expandIntoLines()
        expandedYet[fileno] = linestarts
        text = allpfiles[fileno]
    else
        linestarts = expandedYet[fileno]
    end if
--(ditto)
--  convertToLineColumn(tokline)
    convertToLineColumn(tokcol)
    tokline = eLine
    tokcol = eCol
    if equal(msg,-1) then
        fromWarnU = 1   -- don't re-do tokline/tokcol
        Undefined()
--8/7/2013: (for test/terror) [removed 26/11/19...]
    elsif batchmode then
--MARKTYPES?? (NEWGSCAN, are unused types fatal??)
--  elsif batchmode or siNTyp=S_Type then
        fromWarnU = 1   -- don't re-do tokline/tokcol
        Abort(msg)
    end if
    if siNTyp=S_TVar then
        -- may be either a parameter or local variable...
        if not and_bits(si[S_State],K_othr+K_type) then     
            -- not a parameter then:
            siNTyp = S_GVar2    -- "variable"
            -- (ie:
            --      object a
            --      procedure p(object b)
            --      object c ...
            --      end procedure
            --  a is GVar, b and c are TVar, but we only want 
            --  b reported as "parameter", a & c as "variable")
        end if
    end if
    if fileno<=0 or fileno>length(exptext) then
        thisline = "<WarnU: ooops! (fileno)>"
    elsif tokline<=0 or tokline>length(exptext[fileno]) then
        thisline = "<WarnU: ooops! (tokline)>"
    else
        thisline = exptext[fileno][tokline]
    end if
    errname = NTdesc[siNTyp]&errname
--  if equal(msg,-1) then
----        fromWarnU = 1   -- don't re-do tokline/tokcol
--?9/0 -- (unused/unreachable code?) [added 21/8/10]
--      Undefined()
--  else
        if finalOptWarn[fileno] and repl=0 then
--          Warn(errname&msg,tokline,tokcol)
--          nwarns+=1
--          warnmsgs = append(warnmsgs,{"",0,{errname&msg,currFile(),tokline}})
            wi = {"",0,{errname&msg,currFile(),tokline}}
--pp(wi)
--          if peama then
            if nott00() then
                msg = wFormat(wi,0)
                if eamsg(msg) then return end if
                msg = wFormat(wi,1)
                peama = 0
            end if
            nwarns+=1
            warnmsgs = append(warnmsgs,wi)
        end if
--  end if
end procedure


