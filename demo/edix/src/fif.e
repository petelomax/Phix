--
-- fif.e
-- Find in files
--
ppOpt({pp_Pause,0})

--Rethink:
-- Allow named searches: holds root dir, skipped directories, and skipped extensions.
-- Also backup => skip directories that start with "backup" (ie length()>=6 and [1..6]=).
-- Case sensitive match - that one is missing from Edita!
-- Whole word match (default on) "" - rather than that spaces\cr nonsense.
-- ?Make default (NB this overrides the current directory)
-- Scan the directories in the background, as soon as any directory is selected.        [DONE]
-- Display summaries, with checkboxes: extension and directory (tree? - nah, list),     [DONE]
--  sorted (by default) by counts, highest first. Also show [total] sizes.
-- Show progress/cancel on the main window, not a popup.

--TODO:
--  default the find text to current selection, if any
--  add a browse button to directory
--  options/find in files/save settings - Y/N/Once
--    If Once is selected then any changes are saved the next time the find in files
--    dialog is closed (either because it completes or Cancel was pressed)
--    Better - a save changes option on-screen; initially disabled, and enabled by
--    any Y/N toggle, saves when dialog closed.
--  Any Y/N toggle must add/subtract quantities, and Y->N must disable all subdirs.         <<<*** [DONE]
--    (Note that N->Y will NOT re-enable existing subdirs, however if it was initially
--     N then N->Y will create any required Y subdirs. [or better yet delete if we can])    [now deletes]
--  Save settings, fifext/fifdir in edix.edb?
--    --> better, use an enum in the key, ie {1,dir} (data is 0/1), and {2,ext} (ditto data),
--        and [maybe] a key of {3,"flags"} in the same table holds the flag settings.


include eaedb.e as edb
--added: [DONE, including use here...]
--global function get_fif(sequence key) -- (returns {} if none)
--global procedure store_fif(sequence key, object data)
enum DIR=1, EXT=2, FLAGS=3  -- db access keys


--SUG: treat list.asm as a separate extension to .asm? [nah/tough! - me long stopped that!]
-- Default searchable: asm, bat, c, cfg?, cgi, cmd, cpp, cs, css, cxx, e, err? eu, ew,
--                     ===  ===  =                  ===           ===  =  ===  ==  ==
--                     ewx, ex, exh, exu, exw, exwc, fasm, h, htm, html?, inc, inf?, info?,
--                     ===  ==  ===  ===  ===  ====  ====  =  ===  ====   ===
--                     ini, java, js, json, log, love, lua, pas, php, pl, py, rb, rbw, reg, 
--                          ====  ==                        ===  ===  ==  ==  ==  ===
--                     sql, sym, syn, text, txt, wlua, xml
--                     ===

constant DEBUG = 1  -- (always left on/intended solely to make the code more readable)

--DEV/SUG:
--constant ignorefiles = {"list.asm"}


-- What about options/extensions, and/or 
-- C:\Program Files (x86)\Phix\demo\edix\src\synld.e:
--  Extensions[1..20] = {{},"bat","c","h","cpp","cxx","pas","asm","inc","fasm","htm","html","php","hhc","hhk","clr","java","js","pl","e"}
--  Extensions[21..38] = {"ex","ew","exw","exwc","eu","exu","exh","pro","ewx","eui","edx","err","gtk","plugin","py","rb","rbw","sql"}

--#withtype Ihandle
--#withtype Ihandln

Ihandln fifdlg = NULL

Ihandle find_text, match_case, whole_word, save_settings, dtext, browse, 
        matrix, progress, bt_help, bt_ok, bt_cancel

constant titles = {"active", "ext", "Directory", "Files", "Done"},
         twidth = { 50,       40,    250,         50,      50,  },
                     L = "ALEFT", C = "ACENTER", R = "ARIGHT",
         talign = {  C,        C,      L,          R,       R,  },
         minwid = { 50,       40,    100,         50,      50,  }

sequence lines = {},
         extensions = {},
         elines = {},
         directories = {},
         dlines = {},
         degrid = {}

-- Technical note: degrid is a matrix of how many files of a given extension were
--                 found in each loaded directory. When a directory is disabled,
--                 that equates to unloading and the entries are zeroed. However,
--                 when an extension is disabled then lines[F_files] is decreased
--                 but the number-breakdown is left intact, hidden inside degrid.

constant F_act=1,
         F_ext=2,
         F_dir=3,
         F_files=4,
         F_done=5

include synld.e as synld
constant ALLEXT = synld:Extensions&{"cmd","hgignore","ini","log","readme","1st",
                                    "reg","sql","syn","text","txt"}

--sequence fmap -- 'real' indexes after filtering effects

sequence tags

bool ldrunning = false
sequence todo, todor
integer vlines = 1


--integer terminatea

integer hits

sequence charmash

sequence searchstring

sequence result

sequence linesave = repeat(0,80)
integer col, line_no

function SaveHit(sequence file_name)
--integer work
--integer void
--  work = length(file_name)+length(sprintf("%d",line_no))+2
--  if work+col>90 then
--      col=90-work
--  end if
--  result&={sprintf("%s:%d %s",{file_name,line_no,linesave[1..col-1]})}
--some limit should apply:
--if col>132 then col = 132 end if
--  result = append(result,sprintf("%s:%d %s",{file_name,line_no,linesave[1..col-1]}))
    result = append(result,sprintf("%s:%d %s",{file_name,line_no,linesave[1..min(col-1,132)]}))
--  result &= {sprintf("%s:%d %s",{file_name,line_no,linesave})}
    hits += 1
--DEV
--  setText(c1,sprintf(xl("Found:%d "), hits))
--?{"found",hits}
    IupSetStrAttribute(progress,"TITLE","Found %d, Scanning %s",{hits,file_name})
    return true
end function

integer fileNum

procedure complete_line()
integer c
    -- get rest of line
    while true do
        c = getc(fileNum)
        if c<1 then exit end if
        if c='\n' then exit end if
        if c='\t' then c=' ' end if --DEV should pad?
        if col>length(linesave) then
            linesave &= 0
        end if
        if c>=' ' then
            linesave[col] = c
            col += 1
        end if
    end while
end procedure

integer files_scanned,
        directories_scanned,
        lines_scanned


--
-- To understand the phit array, imagine searching for "00b" in "000a00b", and
--  doing so *one character at a time*. Recall that search strings can contain
--  line breaks (^p), and the last thing we want to do is load the entire file
--  to memory just so we can skip back a few chars. Instead read a character,
--  and do all the processing we will ever need to on that before getting the 
--  next. Further, we are not interested in what column a (full) match occurs.
--
--  So after reading the first '0', we obviously have one partial match, of 
--  length 1. After reading the next '0', we have two matches, of lengths 1 
--  and 2. On the third '0', the existing length 2 match fails ('b'!='0', aka 
--  searchstring[3]!=nextch), so that gets trashed, but the existing length 1 
--  match gets extended to length 2 and we create a new length 1 match. 
--
--  Of course on the 'a', we throw away all our existing partial matches.
--  btw, phit[k] is actually 2 for what I just described as a length 1 match, 
--  and of course there is upper/lower etc char mapping going on all the time.
--
-- When we finally get to the input 'b', again we have two partial matches in 
--  hand, of lengths 1 and 2; completing one trashes the other (nphits=0).
--

procedure scan(sequence file_name)
-- read next file
integer c, cprev, nphits
sequence phit           -- array of partial hits found
--integer void

    --
    -- This is just because anything longer will not fit
    -- nicely into the *.err results in the current format.
    -- It could of course be split into several lines instead
    -- see also work+col>90 in saveHit()
    -- I'm also leaving this in to encourage more sensible directories & filenames,
    -- plus it might catch the odd silly error...
    --
-- Removed 16/5/04:
--  if length(file_name)>70 then
--      void = messageBox("Error: File name and path too long",
--                        file_name,
--                        MB_ICONEXCLAMATION + MB_OKCANCEL)                 
--      if void = IDCANCEL then terminatea=1 end if
--      return      
--  end if
    fileNum = open(file_name, "rb")
    if fileNum = -1 then
        return
    end if

-- (now done by caller)
--  setText(cf,xl("searching: ") & file_name & repeat(' ', 80) & '\r')
--?{"searching",file_name}
    
    files_scanned += 1

    line_no = 1
    cprev = 0
    col = 1
    nphits = 0
    phit = repeat(0,10)
    while true do
        c = getc(fileNum)
        if c<1 then exit end if
        if c='\n' then
            line_no += 1
            col = 1
        else
            if c='\t' then c=' ' end if --DEV should pad?
            if col>length(linesave) then
                linesave &= 0
            end if
--DEV see if we can get this back... (use 1st ':' only) [when we implement F12!]
            if c!=':' and c>=' ' then           -- colon marks line number for F12
                linesave[col] = c
                col += 1
            end if
            c = charmash[c]
            if c or cprev then
                if length(searchstring)>1 then
                    for k=nphits to 1 by -1 do          -- check each partial hit
                        if c==charmash[searchstring[phit[k]]] then -- still good
                            phit[k] += 1
                            if phit[k]>length(searchstring) then -- full match
                                nphits = 0
                                complete_line()
                                if not SaveHit(file_name) then return end if
                                line_no += 1
                                col = 1
                                exit
                            end if
                        else
                            if k<nphits then                -- if not last
                                phit[k] = phit[nphits]      -- keep last instead
                            end if
                            nphits -= 1                     -- scrap one partial hit.
                        end if
                    end for
                    if c==searchstring[1] then
                        nphits += 1
                        if nphits>length(phit) then
                            phit &= repeat(0,10)
                        end if
                        phit[nphits] = 2
                    end if
                else
                    if c==searchstring[1] then
                        complete_line()
                        if not SaveHit(file_name) then return end if
                        line_no += 1
                        col = 1
                    end if
                end if
            end if
        end if
    end while
    close(fileNum)
    lines_scanned += line_no-1
end procedure

procedure init_charmash()
integer c, cprev
    charmash=repeat(0,256)
    for i='a' to 'z' do
        charmash[i] = i 
        charmash[i-32] = i
    end for
    for i='0' to '9' do
        charmash[i] = i
    end for
    charmash['?']='?'
    charmash['_']='_'
    charmash['/']='/'
    charmash['<']='<'
    charmash['>']='>'
    -- everything else is considered "white space"

    c = 'a'
    for k = 1 to length(searchstring) do
        cprev = c
        c = searchstring[k]
        if c=0 then exit end if
        if c!=charmash[c] then
            if charmash[c]=0 then
                charmash[c] = c
            else
                c = charmash[c]
                searchstring[k] = c
            end if
        end if
        if c='\t' then c=' ' end if
        if c=' ' and cprev = c then
            for l=k to length(searchstring)-1 do
                searchstring[l] = searchstring[l+1]
            end for
            searchstring[length(searchstring)] = 0
        end if
    end for
    while searchstring[length(searchstring)]=0 do
        searchstring = searchstring[1..length(searchstring)-1]
    end while   
    charmash['\t'] = charmash[' ']
    charmash['\n'] = charmash[' ']
    charmash['\r'] = charmash[' ']
end procedure



--/*
procedure select()
integer k
--,k2
?9/0
    k = IupGetInt(matrix,"FOCUSCELL")
--/*
    if k and k<=length(fmap) then
        k = fmap[k]
        string filename = lines[k][F_dir]&lines[k][F_name]
        {} = openFile(0,filename,-1)
        k = lines[k][F_rec]
        for i=1 to length(lines) do
            k2=lines[i][F_rec]
            if k2=k then
                lines[i][F_rec]=1
            elsif k2<k then
                lines[i][F_rec]=k2+1
            end if
        end for

        IupHide(fifdlg)
    end if
--*/
end procedure
--*/

object semiperm

function value_cb(Ihandle /*self*/, integer l, integer c)
--?{"value_cb",l,c}
    if c>0 and c<=length(titles) then
        if l==0 then
            return IupRawStringPtr(titles[c])   -- column title
        end if
        if l<=length(lines) then
            l = tags[l]
            semiperm = lines[l][c]
            if not string(semiperm) then
                if c=F_act then
--                  semiperm = {"N","Y"}[semiperm+1]
                    semiperm = iff(semiperm?"Y":"N")
                else
                    semiperm = sprintf("%d",lines[l][c])
                end if
            end if
            return IupRawStringPtr(semiperm)
        end if
    end if
    return NULL
end function

--integer selected_line = 0

function enteritem_cb(Ihandle ih, integer l, integer c)
--?{"enteritem_cb",l,c}
    IupSetAttribute(ih,"MARKED", NULL);  /* clear all marks */
    IupMatSetAttribute(ih,"MARK", l, 0, "Yes");
    IupSetStrAttribute(ih,"REDRAW", "L%d", {l});
    IupSetStrAttribute(ih,"FOCUSCELL", "%d:%d", {l,c}); -- [1]
--  selected_line = tags[l]  -- no!
--  selected_line = l
    return IUP_DEFAULT
end function

--integer redraw_all = 0

integer sortcol = 0
integer sortdir = 1

-- Technical note: When you sort by extension, it actually sorts by {directory,extension},
-- which puts all the directory="" entries first. Likewise sorting by directory actually
-- sorts by {extension,directory}, putting all the extension="" first (and note that a
-- null extension is quietly replaced with " ", a single space, to improve ordering).
-- Also, sorting by active/files/done uses a secondary key of files, plus a trailing
-- (dir,ext) to resolve any matching entries in a predictable fashion. While that does
-- make files use {files,files,dir,ext}, it does no significant harm.

function by_column(integer i, integer j)
string di = lower(lines[i][F_dir]),
       dj = lower(lines[j][F_dir]),
       ei = lines[i][F_ext],
       ej = lines[j][F_ext]
object li, lj
    if sortcol=F_ext then
        -- put all directory="" (and therefore extension!="") entries first
        li = {di,ei}
        lj = {dj,ej}
    elsif sortcol=F_dir then
        -- put all extension="" (and therefore directory!="") entries first
        li = {ei,di}
        lj = {ej,dj}
    else
        -- (since F_done is likely to be all-0, and to make F_act more useful, use
        --   F_files as a secondary key. While {F_files,F_files,..} is pointless,
        --   it does no significant harm here.)
        li = {lines[i][sortcol],lines[i][F_files],di,ei}
        lj = {lines[j][sortcol],lines[j][F_files],dj,ej}
    end if      
    integer c = compare(li,lj)*sortdir
    return c
end function
constant r_col = routine_id("by_column")

function internal_error(integer line)
string msg = sprintf("Internal Error (line %d)",line)
    if IupAlarm("Error",msg,"OK","Cancel")=1 then
        IupHide(fifdlg)
        return true
    end if
    return false    -- carry on
end function

--
-- Technical note:
-- ==============
--  Directory load uses a simple todo list with calls to IupLoopStep() between each entry.
--  Technically this is co-operative multitasking, but certainly not multi-threading and
--  also not using any of the multitasking features (task_create etc) of Phix. It uses a
--  simple flag to ensure there is/only one load_dir() active: the call to load_start() 
--  at the end of the main find_in_files() routine may or may not complete without being
--  reinvoked by value_changed_cb, and after clearing done and resetting todo it checks
--  whether load_dir() is already running (and it is actually a directory). The call to
--  load_start() from dirchanged_cb() will either reset an already running instance,
--  of load_dir() that is, or fire off a new one. It is also worth noting that, as a
--  single albeit convoluted thread, there is no race condition, no point at which the
--  tests on ldrunning occur after load_dir() has left the loop but not yet reset the
--  flag (mentioned only for the benefit of anyone trying to add real threads).
--
--  The one and only caveat here is that load_dir() must not preserve any assumptions 
--  about todo or done, specifically not the length of or any index into either, but 
--  instead cope if they are completely reset sometime during that IupLoopStep() call.
--

procedure load_dir()
sequence sde
object r
bool bSkip
    ldrunning = true
    while length(todo) do
        string dname = todo[1]
        todo = todo[2..$]
        bool bRecurse = todor[1]
        todor = todor[2..$]
        object d = dir(dname)
        if sequence(d) then
            integer ddx = find(dname,directories), dx
            if ddx!=0 then
                dx = dlines[ddx]
--              if lines[dx][F_act]!="N" then ?9/0 end if
--              lines[dx][F_act] = "Y"
                lines[dx][F_files] = 0
                --?check degrid[ddx] is all-0?
            else
                directories = append(directories,dname)
                ddx = length(directories)
                lines = append(lines,{true,"",dname,0,0})
--              lines = append(lines,{bRecurse,"",dname,0,0})
                dx = length(lines)
                dlines = append(dlines,dx)
                if DEBUG then
                    sde = sort(dlines&elines)
                    if sde!=tagset(length(lines)) then
                        if internal_error(260) then exit end if
                    end if
                end if
                degrid = append(degrid,repeat(0,length(extensions)))
            end if

            for i=3 to length(d) do
                sequence di = d[i]
                string fname = di[D_NAME]
                if find('d',di[D_ATTRIBUTES]) then
--DEV check settings... (exclude backup* unless there is an explicit Y setting for it, ditto .hg, dead, .svn, .vs?, .git, ) [DONE]
                    string path = join_path({dname,fname},trailsep:=true)
                    r = edb:get_fif({DIR,path})
                    if r!={} then
                        bSkip = not r
                    else
                        bSkip = (length(fname)>=6 and fname[1..6]="backup") or 
                                 find(fname,{".hg", "dead", "DEAD", ".svn", ".vs", ".git"})
                    end if
                    if bRecurse and not bSkip then
                        todo = append(todo,path)
                        todor = append(todor,true)
                    else
                        lines = append(lines,{false,"",path,0,0})
                        directories = append(directories,path)
                        dlines = append(dlines,length(lines))
                        degrid = append(degrid,repeat(0,length(extensions)))
                    end if
                else
                    string ext = get_file_extension(fname)
                    if ext="" then ext = " " end if
                    integer edx = find(ext,extensions)
                    if edx=0 then
                        extensions = append(extensions,ext)
                        edx = length(extensions)
--DEV check settings - unless there is an explicit Y for it, only include these: bat, c?, cgi?, cfg?, cmd, cpp?, cs?, css, 
--  csv, cxx?, def?, doc?, e, err?, eu, ew, ex, ewx, exu, exw, fasm?, h, hgignore?, htm, html, ini, java?, js?, log, love, 
--  lua, out?, php, pl?, pro?, py, rb, readme, 1st, reg, sql?, sym, syn, text, txt.
--DEV this is if not setting found: [DONE]
                        bool active
                        r = edb:get_fif({EXT,ext})
                        if r!={} then
                            active = r
                        else
                            active = length(ext) and find(ext,ALLEXT)!=0
                        end if
                        lines = append(lines,{active,ext,"",0,0,0})
                        elines = append(elines,length(lines))
--DEBUG/IupAlarm error/close? [DONE?]
                        if DEBUG then
                            sde = sort(dlines&elines)
                            if sde!=tagset(length(lines)) then
                                if internal_error(301) then exit end if
                            end if
                        end if
                    end if

                    integer pad = edx-length(degrid[ddx])
                    if pad>0 then degrid[ddx] &= repeat(0,pad) end if
                    degrid[ddx][edx] += 1

                    edx = elines[edx]
                    lines[edx][F_files] += 1
--                  lines[edx][F_size] += di[D_SIZE]
                    if lines[edx][F_act] then
                        lines[dx][F_files] += 1
--                      lines[dx][F_size] += di[D_SIZE]
                    end if
                end if          
            end for
            tags = custom_sort(r_col,tagset(length(lines)))
            IupSetInt(matrix,"NUMLIN",max(length(lines),vlines))
            IupSetAttribute(matrix,"REDRAW","ALL")
            if length(todo) then
                IupSetStrAttribute(progress,"TITLE","Scanning...  "&todo[1])
            end if
        end if
        if IupLoopStep()=IUP_CLOSE then exit end if
    end while
    string f = IupGetAttribute(find_text,"VALUE")
    IupSetInt(bt_ok,"ACTIVE",length(lines)>0 and length(f)>0)
    IupSetAttribute(progress,"TITLE","")
    ldrunning = false
end procedure

function click_cb(Ihandle /*self*/, integer l, integer c, atom /*pStatus*/)
integer d, e, ee, sgn
sequence sde
    if c>0 and c<=length(titles) then
        if l=0 then -- title clicked, so sort that column (cycle through down/up/none)
            string sortsign = IupGetAttributeId(matrix,"SORTSIGN",c)
            if sortcol!=0 and sortcol!=c then
                IupSetAttributeId(matrix,"SORTSIGN",sortcol,"NO")
--DEV document this:
--  When sorting by column, a few subtle features help to improve things a bit:
--  ext is actually by {directory,extension}, which puts all the directory="" first, and
--  dir is actually by {extension,directory}, which puts all the extension="" first, and 
--  also note that a null extension is replaced with " ", a single space, to ensure that
--  it ends up next to all the other non-"" entries, rather than amidst the directories.
--  done is actually by {done,files}, since initially it is always going to be all-0.
--  active, files, and done are sorted up/down whereas dir/ext are down/up, and those
--  three also get a trailing {dir,ext} to resolve any matching entries in a sensible, 
--  clear, and predictable fashion (without said it sorta randomises on every click).
                sortsign = iff(find(c,{F_act,F_files,F_done})?"DOWN":"UP")
            end if
            IupSetAttributeId(matrix,"SORTSIGN",c,iff(sortsign="DOWN"?"UP":"DOWN"))
            sortdir = iff(sortsign="DOWN"?-1:1)
            sortcol = c
            tags = custom_sort(r_col,tags)
            IupSetAttribute(matrix,"REDRAW","ALL")
--          selected_line = 0
--      elsif iup_isdouble(pStatus) then
--          select()
        elsif c=F_act and l<=length(tags) then
--          ?{l,c}
            integer tl = tags[l]
            bool tlc = not lines[tl][c]
            sgn = iff(tlc?+1:-1)
            lines[tl][c] = tlc
            if lines[tl][F_dir]="" then
                string extension = lines[tl][F_ext] 
--              if IupGetAttribute(save_settings,"VALUE")="ON" then
                if IupGetInt(save_settings,"VALUE") then
                    edb:store_fif({EXT,extension},tlc)
                end if
                e = find(extension,extensions)
                if e=0 then ?9/0 end if -- (never yet triggered)
                for d=1 to length(degrid) do
                    integer dd = dlines[d]
                    if length(degrid[d])>=e
                    and lines[dd][F_act] then
                        lines[dd][F_files] += sgn*degrid[d][e]
                    end if
                end for
--              lines[tl][F_files] = 0 - NO!
            else
                string directory = lines[tl][F_dir]
--              if IupGetAttribute(save_settings,"VALUE")="ON" then
                if IupGetInt(save_settings,"VALUE") then
                    edb:store_fif({DIR,directory},tlc)
                end if
                d = find(directory,directories)
                if d=0 then ?9/0 end if -- (never yet triggered)
--pp({degrid,extensions,elines,directories,directory,d,dlines,lines,tags})
                if DEBUG then
                    sde = sort(dlines&elines)
                    if sde!=tagset(length(lines)) then
                        if internal_error(376) then return IUP_DEFAULT end if
                    end if
                end if
                if sgn=-1 then
                    for e=1 to length(degrid[d]) do
                        ee = elines[e]
--if lines[ee][F_act] then
                        lines[ee][F_files] -= degrid[d][e]
--end if
--if 0 then
                        degrid[d][e] = 0
--end if
                    end for
                    lines[tl][F_files] = 0
                    integer ld = length(directory)
                    --
                    -- <Technical aside>
                    -- Processing in reverse order makes deletions easier:
                    -- ie to delete d[k] we replace it with d[$], which is
                    --    either d[k] itself or something we already ok'd,
                    --    hence we can go straight on with k-1, whereas a
                    --    forward scan may well have to "stick" on k..
                    --
                    for s=length(directories) to 1 by -1 do
                        if s!=d 
                        and length(directories[s])>ld 
                        and directories[s][1..ld]=directory then
                            for e=1 to length(degrid[s]) do
                                ee = elines[e]
--if lines[ee][F_act] then
                                lines[ee][F_files] -= degrid[s][e]
--end if
                            end for
                            degrid[s] = degrid[$]
                            degrid = degrid[1..$-1]

                            integer ds = dlines[s]

                            if DEBUG then
                                if lines[ds][F_dir]!=directories[s] then
                                    if internal_error(408) then exit end if
                                end if
                            end if

                            directories[s] = directories[$]
                            directories = directories[1..$-1]
                            integer lastdline = dlines[$]
                            dlines[s] = lastdline
                            dlines = dlines[1..$-1]

                            integer ll = length(lines)
                            sequence lastline = lines[$]
                            lines[ds] = lastline
                            lines = lines[1..$-1]

                            integer kd, ke
                            if lastline[F_dir]="" then
                                -- moving an extension into a dline slot...
                                ke = find(ll,elines)
                                elines[ke] = ds
                                if DEBUG then
                                    if find(ll,dlines) then
                                        if internal_error(430) then exit end if
                                    end if
                                end if
                            else
                                -- moving a directory down
                                kd = find(ll,dlines)
                                if kd then
                                    dlines[kd] = ds
                                else
                                    -- (verify absence is because it was just deleted)
--DEV hmmm??? (not totally sure if ds==ll is a valid reason for absense... but it might be a rare case?)
--                                  if lastdline!=ll then
                                    if lastdline!=ll and ds!=ll then
                                         if internal_error(443) then exit end if
                                    end if
                                end if
                            end if
--sanity:
                            if DEBUG then
                                for dix=1 to length(directories) do
                                    if directories[dix]!=lines[dlines[dix]][F_dir] then
                                        if internal_error(451) then exit end if
                                    end if
                                end for
                                sde = sort(dlines&elines)
                                if sde!=tagset(length(lines)) then
                                    if internal_error(456) then exit end if
                                end if
                            end if
                            integer tx = find(ds,tags)
                            integer lx = find(ll,tags)
                            tags[lx] = ds
                            tags[tx] = 0
                        end if
                    end for
                    -- remove all subdirectories from todo
                    for i=length(todo) to 1 by -1 do
                        if length(todo[i])>=ld
                        and todo[i][1..ld]=directory then
                            todo[i] = todo[$]
                            todo = todo[1..$-1]
                            todor[i] = todor[$]
                            todor = todor[1..$-1]
                        end if
                    end for
                    sequence nt = {}
                    for t=1 to length(tags) do
                        if tags[t]!=0 then
                            nt &= tags[t]
                        end if
                    end for
                    if length(nt)!=length(lines) then
                        if internal_error(493) then end if
--                      if internal_error(493) then return IUP_CLOSE end if
                    elsif sort(nt)!=tagset(length(nt)) then
                        if internal_error(496) then end if
--                      if internal_error(496) then return IUP_CLOSE end if
                    end if
--?{"tags",tags,"nt",nt}
                    tags = nt
                    IupSetInt(matrix,"NUMLIN",max(length(lines),vlines))
--pp({degrid,extensions,elines,directories,directory,d,dlines,lines,tags})
--?custom_sort(r_col,tagset(length(lines)))
                else
                    if not IupGetInt(find_text,"ACTIVE") then -- OK already pressed
                        IupMessage("Error","Directories cannot be re-enabled after OK pressed")
                    else
                        todo = append(todo,directory)
                        bool bRecurse = IupGetInt(NULL,"SHIFTKEY")
                        todor = append(todor,bRecurse)
                        if not ldrunning then
                            load_dir()
                        end if
                    end if
                end if
            end if
            IupSetAttribute(matrix,"REDRAW","ALL")
        end if
    end if
    return IUP_DEFAULT
end function

--DOC The main extensions/directories window will quite likely populate with far more
--    entries than you are really interested in. Initially it is sorted by descending
--    number of files, which highlights the most important things to be skipped.
--
--ERM... [DEV] 
--  When the window is first opened, the directory defaults to that of the currently
--  open file, and that is loaded recursively. However, if you edit the directory,
--  it loads it non-recursively - otherwise changing it to eg `C:\` would start a
--  process, albeit an interruptable one, that could easily take two hours or more 
--  to complete, if it tried loading things like C:\Windows...
--
--    Note that it can take quite some time to load; my C:\Program Files (x86)\Phix 
--    takes just over a minute (I tend to download almost everything into there) and 
--    maybe even a couple of hours on C:\ - but it quickly completes once you stop it 
--    scanning silly places like C:\Windows. Besides, if it takes a long time just to 
--    run the dir() statements, you really do NOT want to start the scan proper!
--
--    Click on the left column to exclude a directory and all sub-directories, or to 
--    reverse that. If Save Settings is ticked, then it remembers for the next time - 
--    after a short learning curve, loading should start to get pretty quick.
--    [eg my C:\Program Files (x86)\Phix now takes less than one second.]
--
--    The program minimises the amount of "jumping about" that occurs when the left 
--    hand column is clicked: toggling extensions should not change the order, but
--    enabling or disabling a directory may have to add or remove sub-directories, 
--    and that works best only when the view is ordered by ascending directory.
--
--    The main window is responsive throughout both the load and scan phases, apart
--    from the few seconds that a dir() statement may take when a directory contains 
--    several thousand files, allowing the view to be sorted by any column, and lines 
--    enabled/disabled, with immediate effect on what is happening in the background.
--    However, if that is done after the OK button has been pressed, it won't delete 
--    any matches already found, and toggling a directory off and back on again might 
--    cause some results to be duplicated in the final output. [DEV or prohibit back on...]
--    any matches already found, and directories can only be toggled back on before
--    the OK button has been pressed.
--
--Named Search: Allows multiple previous search settings to be retained. As soon 
--as this is non-blank, the directory field becomes read-only, and the make default 
--flag is enabled, allowing this search to be the default whenever find in files starts 
--in this directory. (If there is a demand for a default no matter which directory it 
--starts in, that could be done fairly easily, but it would not be set on this screen.)

-- Match Case, Whole Word: Default to unchecked per edit session, ie when you start
-- Edix, they'll be unchecked, if amended they'll be unchanged when the find in files
-- window is re-opened, but if you exit Edix and restart, they'll be unchecked again.


constant help_text="""
Find: The text to search for.
Save Settings: Controls whether enable/disable actions in the main listview persist.
                         Note this flag setting itself is always saved between sessions.
Match Case, Whole Word: Self explanatory. Default no (per edit session).
Directory: Defaults to that of the currently open file, and that is loaded recursively.
                  If edited, however, it is loaded non-recursively (Shift-click to reload).

The main listview allows fine-grained control over where and what to search.
It may very well populate with far more entries than you are really interested in. 
Initially sorted by descending number of files, highlighting the most important 
things to be skipped, the view can also be sorted by any column.
Click on the left column to enable/disable lines. Directories auto expand/collapse
(works best when sorted by directory), hold down the Shift key to load recursively.
That column can be toggled during both the load phase (though you may want 
to sort the list to minimise how much it jumps around) and the scan phase.

After a short learning curve, the window should start to open with all the Y/N set 
just how you normally want them. TIP: When looking at the search results, you 
can quickly re-open Find in Files, and (with Save Settings checked) toggle a few 
Y/N, then close it, without necesssarily re-running the search.

Help: shows this screen
OK: starts the search (disabled until Find is non-blank and load has completed).
Cancel: close the window and return to the editor (or press escape)."""


function help_cb(Ihandle /*ih*/)
--DEV no help?
--  IupSetGlobal("PARENTDIALOG", fifdlg)
--  IupSetAttributeHandle(NULL,"PARENTDIALOG", fifdlg)
--8/10/2020:
--  IupMessage("Find in Files",help_text)
    Ihandln msgdlg = IupMessageDlg()
    IupSetAttribute(msgdlg,"TITLE","Find in Files")
    IupSetAttribute(msgdlg,"VALUE",help_text)
    IupSetAttributeHandle(msgdlg,"PARENTDIALOG", fifdlg)
    IupPopup(msgdlg)
    msgdlg = IupDestroy(msgdlg)
--  IupSetGlobal("PARENTDIALOG", dlg)
    return IUP_DEFAULT
end function
constant cb_help = Icallback("help_cb")

function close_cb(Ihandle /*fifdlg*/)
    if IupGetInt(fifdlg,"MAXIMIZED")=0 then
        IupConfigDialogClosed(config, fifdlg, "FindInFiles")
    end if
    IupHide(fifdlg) -- do not destroy, just hide
    todo = {}
    todor = {}
    return IUP_DEFAULT
end function
constant cb_close = Icallback("close_cb")

sequence find_texts = {}

function ok_cb(Ihandle /*bt_ok*/)
string directory
--?"ok_cb"
--DEV disable find_text/dtext/bt_ok, but leave the toggles and match_case/whole_word active [OK!]
--?Note that once you click on OK you cannot change the find string or directory, except by
-- cancelling/closing the find in files window and re-opening it. 
--  select()
    IupSetInt({find_text,dtext,bt_ok},"ACTIVE",false)

    searchstring = IupGetAttribute(find_text, "VALUE");
    if length(searchstring)!=0 then
        -- first, maintain the text dropdown:
        if not find(searchstring,find_texts) then
            find_texts = append(find_texts,searchstring)
--          IupSetAttribute(find_text,sprintf("%d",length(find_texts)),searchstring)
            IupSetStrAttributeId(find_text,"",length(find_texts),searchstring)
        end if

--  terminatea = 0
        hits = 0
        files_scanned = 0
        directories_scanned = 0
        lines_scanned = 0

        result = {}
--DEV
--  searchstring = getText(findiftext)
        result = append(result,"Searching for: "&searchstring)
        result = append(result," Files scanned %d, Directories scanned %d, Lines %d")
        init_charmash()

-- somewhere in the loop...
--  if terminatea then return 1 end if

-- somewhere in some handler...
--      elsif id = CANCEL then
--          terminatea = 1

--  scan("123")

--end if

        --
        -- (Re-)Using todo here allows directories to be toggled Y->N even after OK has been pressed.
        --  That won't delete any already-found results, but it will curtail further searching.
        --  Note this is co-operative multitasking, of a kind; no locking is required [as written].
        --
        todo = {}
        todor = {}

        for dx=1 to length(directories) do
            integer ddx = dlines[dx]
            if lines[ddx][F_act]
            and lines[ddx][F_files]>0 then
--              scan(directories[dx])
                directory = directories[dx]
                todo = append(todo,directory)
                todor = append(todor,0)     -- (not really used here, but clippable)
            end if
        end for

        while length(todo) do
            directory = todo[1]
            directories_scanned += 1
--?{"directory",directory}
            object d = dir(directory)
            if sequence(d) then
                for di=1 to length(d) do
                    if not find('d',d[di][D_ATTRIBUTES]) then
                        string file_name = d[di][D_NAME],
                               ext = get_file_extension(file_name)
                        if ext="" then ext = " " end if
                        integer edx = find(ext,extensions)
                        edx = elines[edx]
                        if lines[edx][F_act] then
                            string full_path = join_path({directory,file_name})
--?full_path
                            IupSetStrAttribute(progress,"TITLE","Found %d, Scanning %s",{hits,full_path})
                            scan(full_path)
                        end if
                        if IupLoopStep()=IUP_CLOSE then exit end if
                        if length(todo)=0 then exit end if
                        if directory!=todo[1] then exit end if
                    end if
                end for
            end if
            if length(todo) and directory=todo[1] then
                todo = todo[2..$]
                todor = todor[2..$]
            end if
        end while

--      {} = close_cb(fifdlg)

        -- and the rest...
        result[2] = sprintf(result[2], {files_scanned,directories_scanned,lines_scanned})
--pp(result)
        string errfile = join_path({filepaths[currfile],"ex.err"})
        integer log_file = open(errfile, "w")
--      if log_file = -1 then
--      void = messageBox(xl("Error"), 
--                        xl("Couldn't open ") & log_path & log_name & '\n',
--                        MB_ICONEXCLAMATION + MB_OK)
--      return
--  end if
        if log_file!=-1 then
            for i=1 to length(result) do
                puts(log_file,result[i]&'\n')
            end for
            close(log_file) 
        end if

--      if not openFile(errfile,1,0) then
        if not openFile(0,errfile,-1) then
            IupMessage("Error","Couldn't open " & errfile & '\n')
        else
            --
            -- lastly, build a new linelengths table for edix
            --
            sequence linelengths = repeat(0,80)
            for i=1 to length(result) do
                integer linelength = ExpLength(result[i])
                if linelength>=length(linelengths) then
                    linelengths &= repeat(0,linelength-length(linelengths)+1)
                end if
                linelengths[linelength+1] = linelengths[linelength+1] + 1
            end for
            filelinelengths[currfile] = linelengths
            filetext[currfile] = result
            bookmarks[currfile] = repeat(0,length(result))
            actions[currfile] = {}
            actionptr[currfile] = 0
            actionsave[currfile] = 0

            TopLine = 0
            CursorY = 0
            CursorX = 0
            selON = 0
            forceCursorOnscreen()
            paintall()
            return IUP_CLOSE            
        end if

    end if

    return IUP_DEFAULT
end function
constant cb_ok = Icallback("ok_cb")

function cancel_cb(Ihandle /*bt_cancel*/)
    return close_cb(bt_cancel)
end function
constant cb_cancel = Icallback("cancel_cb")

function resize_cb(Ihandle /*ih*/, integer width, integer height)
sequence widths = repeat(0,IupGetInt(matrix,"NUMCOL"))
integer wi, total_width = 0, new_width
    for i=1 to length(widths) do
        widths[i] = IupGetIntId(matrix,"RASTERWIDTH",i)
    end for
--?widths
    total_width = sum(widths)
    {width,height} = IupGetIntInt(fifdlg,"RASTERSIZE")
    width -= 90
    height -= 110
    IupSetIntId(matrix,"RASTERWIDTH",0,0)
    for i=1 to length(widths) do
        wi = widths[i]
        new_width = max(floor((wi/total_width)*width),minwid[i])
        IupSetIntId(matrix,"RASTERWIDTH",i,new_width)
    end for
    vlines = max(4,floor((height-1)/23)-6)
    IupSetInt(matrix,"NUMLIN",max(length(lines),vlines))
    IupSetInt(matrix,"NUMLIN_VISIBLE",vlines)
    IupRefresh(fifdlg)
--I used this to set MINSIZE: 
--IupSetAttribute(fifdlg, "TITLE", IupGetAttribute(fifdlg,"RASTERSIZE"))
    return IUP_DEFAULT
end function

function key_cb(Ihandle /*ih*/, atom c)
    if c=K_F1 then 
        return help_cb(dlg)
    elsif c=K_ESC then
        return close_cb(fifdlg)
--/*
    elsif c=K_CR then
        select()
--*/
    end if
--  if selected_line!=0 then
--      redraw_all = 0
--      integer l = tags[selected_line]
--      if redraw_all then
--          IupSetStrAttribute(matrix,"REDRAW", "ALL")
--      else
--          IupSetStrAttribute(matrix,"REDRAW", "L%d", {selected_line})
--      end if
--  end if
    return IUP_DEFAULT
end function

procedure load_start(bool bRecurse)
    string d = IupGetAttribute(dtext,"VALUE")
    todo = {}
    todor = {}
    if get_file_type(d)=FILETYPE_DIRECTORY then
        d = join_path({d,""},trailsep:=true)
        todo = {d} -- (must have trailsep)
        todor = {bRecurse}
    end if
    lines = {}
    tags = {}
    extensions = {}
    elines = {}
    directories = {}
    dlines = {}
    degrid = {}
    IupSetInt(bt_ok,"ACTIVE",false)
    if not ldrunning then
        IupSetAttribute(progress,"TITLE","Scanning...")
        load_dir()
    else
        IupSetAttribute(progress,"TITLE","")
    end if
end procedure

function findchanged_cb(Ihandle /*dtext*/)
bool bOK = false
    if not ldrunning then
        string f = IupGetAttribute(find_text,"VALUE")
        bOK = length(lines)>0 and length(f)>0
    end if
    IupSetInt(bt_ok,"ACTIVE",bOK)
    return IUP_DEFAULT
end function
constant cb_findchanged = Icallback("findchanged_cb")

function dirchanged_cb(Ihandle /*dtext*/)
    load_start(false)
    return IUP_DEFAULT
end function
constant cb_dirchanged = Icallback("dirchanged_cb")

Ihandln browsedlg = NULL

--/*
IupFileDlg(DIR) issues
(1) When the dialog opens it expands to the default directory, but at the last second seems to
perform an ensure_visible(node0) operation, so something like C:\Users is hidden off-screen.
(2) Tab/Shift-Tab only cycle through the buttons. You can put keyboard focus on the treeview,
with a mouse click, but not without changing the default node. I could almost live with (1),
if only I could back-tab onto the treeview and press up/down to reposition it.
--*/

function browse_cb(Ihandle /*browse*/)
--?"browse_cb"
    if browsedlg=NULL then
        browsedlg = IupFileDlg()
--      IupSetAttributes(browsedlg,`ALLOWNEW=NO,DIALOGTYPE=DIR,TITLE="Directory"`)  -- (no help)
        IupSetAttributes(browsedlg,`DIALOGTYPE=DIR,TITLE="Directory"`)
        IupSetAttributeHandle(browsedlg,"PARENTDIALOG",fifdlg)
    end if
    string d = IupGetAttribute(dtext,"VALUE")
    IupSetAttribute(browsedlg,"FILE",d)
    IupSetAttribute(browsedlg,"DIRECTORY",d)
--  IupSetAttribute(browsedlg,"VALUE",d)
    IupPopup(browsedlg, IUP_CENTER, IUP_CENTER)
    if IupGetInt(browsedlg, "STATUS")=0 then
        d = IupGetAttribute(browsedlg, "VALUE")
        IupStoreAttribute(dtext, "VALUE", d)
        load_start(false)
    end if
--  IupHide(browsedlg)
    return IUP_DEFAULT
end function
constant cb_browse = Icallback("browse_cb")

--DEV unused...
--/*
function dropcheck_cb(Ihandle /*ih*/, integer lin, col) 
    if col=F_act and lin<=length(tags) then return IUP_CONTINUE end if
    return IUP_IGNORE
end function
constant cb_dropcheck = Icallback("dropcheck_cb")

function togglevalue_cb(Ihandle /*ih*/, integer lin, col, status)
?{"togglevalue_cb",lin, col, status}
    return IUP_DEFAULT
end function
constant cb_togglevalue = Icallback("togglevalue_cb")
--*/
--/*
TOGGLEVALUEL:C  value of the toggle inside the cell. The toggle is shown only if the DROPCHECK_CB returns IUP_CONTINUE for the cell. 
When the toggle is interactively change the TOGGLEVALUE_CB callback is called. (Since 3 
Action generated before the current cell is redrawn to determine if a dropdown/popup menu feedback or a toggle should be shown. 
If this action is not registered, no feedback will be shown. 
If the callback is defined and return IUP_DEFAULT for a cell, to show the dropdown/popup menu the user can simply do a single click in the drop feedback area of that cell. 

function dropcheck_cb(Ihandle ih, integer lin, col) 

ih: identifier of the element that activated the event.
lin, col: Coordinates of the cell. 

Returns: IUP_DEFAULT will show a drop feedback, IUP_CONTINUE will show and enable the toggle button, or IUP_IGNORE to draw nothing. 
--*/

function settings_cb(Ihandle /*save_settings*/, integer state)
    edb:store_fif({FLAGS,"flags"},state)
    return IUP_DEFAULT
end function
constant cb_settings = Icallback("settings_cb")

procedure create_fif_dialog()

--DEV should be a dropdown...
--  find_text = IupText("EXPAND=HORIZONTAL")
    find_text = IupList("VISIBLECOLUMNS=20, EXPAND=HORIZONTAL, DROPDOWN=YES, EDITBOX=YES")
    IupSetCallback(find_text,"VALUECHANGED_CB",cb_findchanged)
    Ihandle hboxf = IupHbox({IupLabel("Find: "),find_text},"ALIGNMENT=ACENTER")
    save_settings = IupToggle("Save Settings",cb_settings)
    object r = edb:get_fif({FLAGS,"flags"})
    if r!={} then
        IupSetInt(save_settings,"VALUE",r)
    end if
    match_case = IupToggle("Match Case")
    whole_word = IupToggle("Whole Word")
    Ihandle options = IupHbox({save_settings,match_case,whole_word})
--ditto...??
    dtext = IupText("EXPAND=HORIZONTAL")
    IupSetCallback(dtext,"VALUECHANGED_CB",cb_dirchanged)
    browse = IupButton("Browse","ACTION",cb_browse,"PADDING=10x0")
    Ihandle hboxd = IupHbox({IupLabel("Directory: "),dtext,browse},"ALIGNMENT=ACENTER,GAP=5")

    matrix = IupMatrix()
    IupSetInt(matrix, "NUMCOL", length(titles))
    IupSetInt(matrix, "NUMCOL_VISIBLE", length(titles))
    IupSetInt(matrix, "NUMLIN", length(lines))
    IupSetInt(matrix, "NUMLIN_VISIBLE", 15)
    IupSetIntId(matrix, "RASTERWIDTH", 1, 80)
    IupSetAttribute(matrix, "ALIGNMENT", "ALEFT")
    for i=1 to length(twidth) do
        IupSetIntId(matrix, "RASTERWIDTH", i, twidth[i])
        IupSetAttributeId(matrix, "ALIGNMENT", i, talign[i])
    end for
    --IMPORTANT: HEIGHT0 tells IupMatrix that we are gonna have column titles at line 0
    IupSetInt(matrix, "HEIGHT0", 10);
    IupSetAttribute(matrix, "RESIZEMATRIX", "YES");
    IupSetAttribute(matrix, "MARKMODE", "LIN");
    IupSetAttribute(matrix, "MARKAREA", "CONTINUOUS");

    IupSetAttribute(matrix, "HIDEFOCUS", "YES");
    IupSetAttribute(matrix, "TOGGLECENTERED", "YES");
    IupSetAttribute(matrix, "TOGGLEVALUE1:1", "1");
    IupSetAttribute(matrix, "FRAMECOLOR", "220 220 220");
    IupSetAttribute(matrix, "BORDER", "NO");
    IupSetAttribute(matrix, "CURSOR", "ARROW");

    IupSetCallback(matrix, "VALUE_CB",      Icallback("value_cb"))
    IupSetCallback(matrix, "ENTERITEM_CB",  Icallback("enteritem_cb"));
    IupSetCallback(matrix, "CLICK_CB",      Icallback("click_cb"));
--  IupSetCallback(matrix, "DROPCHECK_CB",  cb_dropcheck)
--  IupSetCallback(matrix, "TOGGLEVALUE_CB",cb_togglevalue)

    progress = IupLabel("","EXPAND=HORIZONTAL,ALIGNMENT=ATOP")
 
    bt_help   = IupButton("Help (F1)","ACTION",cb_help,"PADDING=10x2")
--  lbl_fill = IupLabel("Filter")
--  filter = IupText("EXPAND=HORIZONTAL,ACTIVE=NO")

    bt_ok     = IupButton("OK","ACTION",cb_ok,"PADDING=10x2")
    bt_cancel = IupButton("Cancel","ACTION",cb_cancel,"PADDING=10x2")

    Ihandle buttons = IupHbox({bt_help,bt_ok,bt_cancel},
                              "MARGIN=20x5,GAP=20,ALIGNMENT=ABOTTOM")
    Ihandle box = IupVbox({hboxf,
                           options,
                           hboxd,
                           matrix,
                           progress,
--                         IupFill(),
                           buttons
                          });
    IupSetAttribute(box, "MARGIN", "5x5");
--  IupSetAttribute(box, "GAP", "5");

    fifdlg = IupDialog(box)

    IupSetAttribute(fifdlg,"MINSIZE","570x350")
--DEV this...
--  IupSetAttributePtr(fifdlg, "PARENTDIALOG", dlg)
    IupSetAttributeHandle(fifdlg, "DEFAULTENTER", bt_ok);
    IupSetAttributeHandle(fifdlg, "DEFAULTESC", bt_cancel);
    IupSetCallback(fifdlg, "CLOSE_CB", cb_close);
    IupSetAttribute(fifdlg, "TITLE", "Find In Files");
    IupSetCallback(fifdlg, "RESIZE_CB", Icallback("resize_cb"));
    IupSetCallback(fifdlg, "K_ANY", Icallback("key_cb"));
--DEV or this...
    IupSetAttributeHandle(fifdlg,"PARENTDIALOG",dlg)
end procedure

global procedure find_in_files()

    if actionsave[currfile] != actionptr[currfile] then
        if not saveCurrAs() then return end if
    end if

    if fifdlg=NULL then
        create_fif_dialog()
    elsif sortcol!=F_files then
        IupSetAttributeId(matrix,"SORTSIGN",sortcol,"NO")
    end if

    object sel = getSelection(SEL_COPY)
    if sequence(sel) and length(sel)=1 then
--24/1/18:
--      string str = sel[1]
        sequence str = sel[1]
        if not string(str) then str = utf32_to_utf8(str) end if
        IupSetStrAttribute(find_text, "VALUE", str);
    end if

--  IupSetInt({match_case,whole_word},"VALUE",false)    -- no, per session

--/*
    --
    -- remove any closed files...
    --
    integer found, k, k2
    for i=length(lines) to 1 by -1 do
        found = 0
        for j=1 to length(filenames) do
            if equal(lines[i][F_name],filenames[j])
            and equal(lines[i][F_dir],filepaths[j]) then
                found = 1
                exit
            end if
        end for
        if not found then
            k = lines[i][F_rec]
            for j=1 to length(lines) do
                k2 = lines[j][F_rec]
                if k2>k then
                    lines[j][F_rec]=k2-1
                end if
            end for
            lines = lines[1..i-1]&lines[i+1..length(lines)]
        end if
    end for
    --
    -- insert any new ones...
    --
    k = 0
    for i=length(filenames) to 1 by -1 do
        found = 0
        for j=1 to length(lines)-k do
            if equal(lines[j][F_name],filenames[i])
            and equal(lines[j][F_dir],filepaths[i]) then
                found = 1
                exit
            end if
        end for
        if not found then
            for j=1 to length(lines) do
                lines[j][F_rec] += 1
            end for
            lines = append(lines,{filenames[i],get_file_extension(filenames[i]),filepaths[i],-1,-1,1})
            k += 1
        end if
    end for
--*/
--  if currfile then
    IupSetAttribute(dtext,"VALUE",filepaths[currfile])
--  IupSetAttribute(progress,"ALIGNMENT","ATOP")-- = IupLabel("EXPAND=HORIZONTAL, ALIGNMENT=ARIGHT:ATOP")

    IupSetInt(bt_ok,"ACTIVE",false)
    IupSetInt({find_text,dtext},"ACTIVE",true)
    lines = {}
    tags = {}

    sortcol = F_files
    sortdir = -1
    IupSetAttributeId(matrix,"SORTSIGN",sortcol,"UP")

    IupSetAttribute(matrix,"FOCUSCELL","1:1")

    IupConfigDialogShow(config, fifdlg, "FindInFiles")
    integer {w,h} = IupGetIntInt(fifdlg,"CLIENTSIZE")
    {} = resize_cb(fifdlg,w,h)
    IupUpdate(fifdlg)
    load_start(true)
    -- make it modal:
    IupPopup(fifdlg,IUP_CURRENT,IUP_CURRENT)
end procedure

function fif_cb(Ihandle /*cb_filelist*/)
    find_in_files()
    return IUP_DEFAULT
end function
global constant cb_fif = Icallback("fif_cb")


--/*
--Some old scribbles; ignore for now:
--          Standard files. (default checked). Select this option to search standard 
--          euphoria/phix files with EXW, EW, EX, E, EXU, and EU suffixes.
--
--          Documentation files. (default unchecked). Select this option to search files
--          with DOC, TXT, ME, MAN, DIZ, NFO, TAG, LIC, HST, and 1ST suffixes.
--          Note: no attempt is made to distingiush between standard text files with a
--          ".DOC" suffix and the microsoft word format "*.DOC" files.*
--          [The latter can be detected as first two bytes of #D0CF]
--          
--          Html files. (default unchecked). Select this option to search files with HTM
--          and HTX suffixes. Note: this option has been created separate from the above
--          documentation files option because in the standard euphoria directory alot of
--          the information is repeated in .doc and .htm files.
--
--          Other files. (default unchecked). Select this option to search files with 
--          other, known suffixes such as C, java, etc. There is no guarantee this list
--          is complete but it should include most other language files.
--
--          Unknown files. (default unchecked). Select this option to search files which
--          apparently usually contain text.
--
--          All files. (default unchecked). Use this as a last resort, as it will attempt
--          to search files such as exe, bin, jpg, mpg, mp3, etc. The results will be
--          slow in coming and dubious at best, but at least complete. Be warned.
--          Also, of course, try not to expect good results when jumping to line 42,153,795
--          of a 2.4GB divx file where it somehow found "fred".
--          
--          Report unrecognised files. (default checked). This lists files that the 
--          program does not know as either text-ish or binary-ish. It does not search
--          these files (unless the All files option has been checked).
--          Note: please feel free to report the one-line messages reported; but please,
--          do *NOT* dump huge (or indeed small) binary files onto the message list when
--          querying results.
--      TIP: Speed may be a problem, especially if the current and sub directories
--          contain thousands of files or, for example a 50M movie file.
--          The latter can be avoided by leaving the All Files checkbox clear.
--without type_check

--/!* 4.0.2:
--include builtins\wildcard.e
--*!/
--!/!*!*!/include builtins\pcase.e as pcase

constant FindIF = create( Window, xl("Find in Files "),      0, Main,   100,  50, 365, 285, 0 )
constant flab = create( Label, xl("Find "),                  0, FindIF,   8,  15,  50,  20, SS_RIGHT )
constant findiftext = create( ComboDropDown, "",             0, FindIF,  64,  12, 271, 224, 0 )
constant cd = create( CheckBox, xl("Current Directory?"),    0, FindIF,   8,  45, 165,  20, 0 )
constant sd = create( CheckBox, xl("Sub-directories?"),      0, FindIF,   8,  65, 165,  20, 0 )
--DEV these are C:\Euphoria, what about C:\Program Files\Phix(or wherever)?
constant ed = create( CheckBox, xl("Euphoria Files?"),       0, FindIF,   8,  85, 165,  20, 0 )
constant ei = create( CheckBox, xl("Euphoria Includes?"),    0, FindIF,   8, 105, 165,  20, 0 )
constant bd = create( CheckBox, xl("Backup Directory?"),     0, FindIF,   8, 125, 165,  20, 0 )
constant sf = create( CheckBox, xl("Standard Files?"),       0, FindIF, 180,  45, 165,  20, 0 )
constant df = create( CheckBox, xl("Documentation Files?"),  0, FindIF, 180,  65, 165,  20, 0 )
constant hf = create( CheckBox, xl("Html Files?"),           0, FindIF, 180,  85, 165,  20, 0 )
constant of = create( CheckBox, xl("Other Files?"),          0, FindIF, 180, 105, 165,  20, 0 )
constant uf = create( CheckBox, xl("Unknown Files?"),        0, FindIF, 180, 125, 165,  20, 0 )
constant wf = create( CheckBox, xl("Win32lib Files?"),       0, FindIF, 180, 145, 165,  20, 0 )
constant af = create( CheckBox, xl("All Files?"),            0, FindIF, 180, 165, 165,  20, 0 )
constant kf = create( CheckBox, xl("Skip Files?"),           0, FindIF, 180, 185, 165,  20, 0 )
constant GO = create( Button, xl("Go"),                      0, FindIF, 129, 214,  80,  25, 0 )


setHint(ed,xl("Search all Euphoria - doc, demo, html, etc"))
setHint(ei,xl("Search only euphoria/include"))
setHint(bd,xl("Search Edita/backups"))
setHint(sf,"e,ex,ew,exw,eu,exu")
setHint(of,xl("Other languages, eg .java, .c, .bas"))
setHint(kf,xl("Files expected to be binary, eg exe files"))

setCheck(cd,True)
setCheck(sd,False)
setCheck(ed,False)
setCheck(ei,False)
setCheck(bd,False)

setCheck(sf,True)
setCheck(df,False)
setCheck(hf,False)
setCheck(of,False)
setCheck(uf,False)
setCheck(wf,False)
setCheck(af,False)

constant PROGRESS = create( Window, xl("Find in Files "), 0, FindIF, 400, 50, 580, 189, 0 )
constant c1 = create( Label, "", 0, PROGRESS, 8, 45, 214, 20, 0 )
constant cf = create( Label, "", 0, PROGRESS, 8, 81, 484, 20, 0 )
constant CANCEL = create( Button, xl("Cancel"), 0, PROGRESS, 189, 117, 80, 25, 0 )


-- files to skip:
--DEV plus, for ACE, par, rar, etc any xnn files eg r01, r02, etc.
--plus all files with more than one "." in their name

sequence skip_list
skip_list = {
    "EXE",  -- executable files
    "EX_",  -- distributed format?
    "BIN",  -- dos(?) executable files
    "DLL",  -- dynamic linked libraries
    "LIB",  -- some sort of program library
    "EXP",  -- and another
    "SO",   -- ? unix "shared object" / dll?
    "OBJ",  -- ? how many executable/library formats are there ;-)
    "COM",  -- ? "" REALLY!!
    "O",    -- yep
    "DRV",  -- ? Device driver
    "SWP",  -- ? swap file
    "PAR",  -- ? parity file
    "RAR",  -- rar archive files
    "ZIP",  -- zip files
    "GZ",   -- gnu zip
    "TAR",  -- tape archive
    "TGZ",  -- tape gnu zip archive
    "CAB",  -- Microsoft cabinet (archive) files
    "BMP",  -- bitmap (image) files
    "GIF",  -- gif (image) files
    "TIF",  -- tif (image) files
    "JPG",  -- jpeg (image) files
    "PNG",  -- png (image) files
    "WAV",  -- wav (sound) files
    "MP3",  -- mp3 (music) files
    "AVI",  -- movie files
    "MOV",  -- movie files
    "MPG",  -- movie files
    "RM",   -- real movie files
    "ICO",  -- icon files
    "RES",  -- ? files
    "CHM",  -- compiled help files (cannnot decipher :-( )
    "HLP",  -- see *.chm
    "CNT",  -- contents of help file
    "DAT",  -- data files
    "IDX",  -- index/data files
    "GID",  -- ? files
    "OEM",  -- ? Original Equipment Manufacturer? files
    "PDF",  -- Adobe portable document format files (cannot decipher :-( )
    "PS",   -- postscript (cannot decipher :-( )
    "XLS",  -- Excel spreadsheet (cannot decipher :-( )
    "ISU",  -- ?
    "OCX",  -- ?
    "TLB",  -- ?
    "PFB",  -- ? Adobe font?
    "MMM",  -- ? Adobe font?
    "PFM",  -- ? Adobe font?
    "ABT",  -- ? Adobe ?
    "CAT",  -- ? Adobe ?
    "WLD",  -- ? Adobe ?
    "DDD",  -- ? Adobe ?
    "DID",  -- ? Adobe ?
    "PDD",  -- ? Adobe ?
    "PLC",  -- ? Adobe ?
    "STP",  -- ? Adobe ?
    "TRN",  -- ? Adobe ?
    "API",  -- ? Adobe ?
    "CMP",  -- ? Adobe ?
    "STD",  -- ? Adobe ?
    "SYX",  -- ? Adobe ?
    "THD",  -- ? Adobe ?
    "LNG",  -- ? Adobe ?
    "RSD",  -- ? Adobe ?
    "RST",  -- ? Adobe ?
    "STC",  -- ? Adobe ?
    "SYD",  -- ? Adobe ?
    "INS",  -- internet communication settings
    "VXD",  -- Virtual device driver
    "PKG",  -- ?
    "Z",    -- ?
    "LNK",  -- Shortcut
    "DT",   -- ?
    "ILM",  -- ?
    "OP",   -- ?
    "PAT",  -- ?
    "FLT",  -- ?
    "DCF",  -- ?
    "FCS",  -- ?
    "HT",   -- Hyperterminal (ftp) file
    "EDB"   -- Euphoria/Phix database files
}

-- files to process:
sequence do_list
do_list = {
    "EXW",  -- Euphoria/Phix windows main files
    "EW",   -- Euphoria/Phix windows include files
    "EX",   -- Euphoria/Phix dos main files
    "E",    -- Euphoria/Phix general include files
    "EXU",  -- Euphoria/Phix linux main files
    "EU"    -- Euphoria/Phix linux include files
}
-- documentation_files:
sequence doc_list
doc_list = {
    "TXT",  -- text files
    "ME",   -- as in read.me
    "DOC",  -- documentation/word files --DEV: #D0CF
    "MAN",  -- manual file
    "DIZ",  -- info files
    "NFO",  -- info files
    "TAG",  -- info files
    "LIC",  -- ? License files
    "HST",  -- (version) history file
    "1ST"   -- as in readme.1st
}
-- html files:
sequence html_list
html_list = {
    "HTM",  -- html files
    "HTX"   -- html files
}

-- dubious files:
sequence other_list
other_list = {
    "PRO",  -- profile files
    "ERR",  -- error files
    "BAS",  -- basic files
    "PRJ",  -- IDE project files
    "CFG",  -- IDE ?
    "C",    -- C files
    "DEF",  -- def files(?)
    "H",    -- C header files
    "IN",   -- C configuration file
    "CPP",  -- C++ file
    "CXX",  -- ? C++ file
    "FL",   -- ? data file for fltk
    "TCL",  -- tcl/tk thingumabib
    "RB",   -- ruby
    "RBW",  -- ruby
    "JS",   -- javascript files
    "SCP",  -- script file
    "SET",  -- file set
    "INI",  -- configuration settings
    "BAT",  -- batch files
    "SYS",  -- system files
    "RTF",  -- rich text format files
    "WRI",  -- Microsoft write format files
    "HHC",  -- html ? control ? files
    "HHK",  -- html ? control ? files
    "HHP",  -- html ? control ? files
    "LST",  -- ? List file
    "XML",  -- xml, you know
    "ISS",  -- ? info files
    "LID",  -- ? info files
    "INF",  -- setup information files
    "OUT",  -- output files
    "DTA",  -- ?
    "RC",   -- ?
    "AV",   -- ? anti virus ?
    "BMK",  -- ?
    "CAM",  -- ?
    "CUE",  -- cue (CD writing) files
    "LOG",  -- log files
    "OLD",  -- ? backup files
    "BAK",  -- ? backup files
    "ORG",  -- original (backup) files
    "PRV",  -- ? just how many backup formats do we need ;-)
    "SYD",  -- ? "" REALLY!!
    "CSV",  -- comma separated variable
    "WSP",  -- JFE workspace details
    "XPM",  -- ? text/icon thingy?
    "XBM",  -- ?
    "DSP",  -- Microsoft developer studio project file
    "DSW",  -- Microsoft developer studio workspace file
    "PM",   -- ? wxwindows summat or sumfin ?
    "AM",   -- automake file
    "M4",   -- automake file
    "BC",   -- ? Borland C doobrie ?
    "CSS",  -- html/cascading style sheets
--}
--
--DUH! there is actually no point in having an unknown file types list!
-- Hence these are now just part of the other_list
--
---- unknown files:
--sequence unknown_list
--unknown_list = {
    "SUB",
    "GUESS",
    "HTML", -- html files
    "JAVA", -- java files
    "URL",  -- shortcut files
    "QTIF", -- quick time image files
    "PRO1",
    "AIFF", -- winamp media file
    "BASH",
    "EDIF",
    "PHP3",
    "PHP4",
    "SHTM",
    "VHDL",
    "SPEC",
    "RESP",
    "LIST",
    "DOXY",
    "OS2X",
    "MENU",
    "TRACE",
    "LGC",
    "LGD",
    "CLASS",    -- java class (binary) file
    "BUILD",
    "SHTML",
    "THEME",
    "SCALE",
    "FINAL",
    "WIN32",
    "CACHE",
    "MINGW",
    "SCRIPT",
    "DLLREG",
    "POLICY",
    "EXISTS",
    "INSTALL",
    "SECURITY",
    "CVSIGNORE",
    "PROPERTIES"
}


integer log_file
integer SLASH
sequence log_name, log_path
log_path={} --DEV

sequence stacked_dir
sequence processed_files
integer bddone

include builtins\file.e
--include eaformat.ew

"function look_at(sequence path_name, sequence entryX)
-- see if a file name qualifies for searching
sequence file_name, extension
object results
integer exidx

results={}  --DEV
    doEvents(0) -- let windows actions be done
    if terminatea then return 1 end if
    file_name = entryX[D_NAME]
    path_name &= SLASH
    if equal(path_name[1..2], '.' & SLASH) then
        path_name = path_name[3..length(path_name)]
    end if
    path_name &= file_name
    if equal(file_name, log_name) then
        return 0 -- avoid circularity
    end if
    if find('d', entryX[D_ATTRIBUTES]) then
        if not equal(upper(path_name),upper(initialcurrentdir&"BACKUP")) then
--          stacked_dir&={path_name}
            stacked_dir = prepend(stacked_dir,path_name)
        else
            if isChecked(bd) and bddone = 0 then
--              stacked_dir&={path_name}
                stacked_dir = prepend(stacked_dir,path_name)
            end if
            bddone = 1
        end if
        return 0 -- a directory
    end if
    exidx=find('.',file_name)
    if exidx then
        extension = upper(file_name[exidx+1..length(file_name)])
    else
        extension = ""
    end if
    if not isChecked(kf) then
        if exidx=0 then return 0 end if -- no extension
        if find(extension,skip_list) then return 0 end if
--PL removed 29/5/17:
--      if find('.',extension) then return 0 end if
        if length(extension)=3
        and find(extension[2],"0123456789")
        and find(extension[3],"0123456789") then
            return 0
        end if
    end if
    exidx=isChecked(af)
    if isChecked(sf) then
        if find(extension,do_list) then exidx=1 end if
    end if
    if isChecked(df) then
        if find(extension,doc_list) then exidx=1 end if
    end if
    if isChecked(hf) then
        if find(extension,html_list) then exidx=1 end if
    end if
    if isChecked(kf) then
        if find(extension,skip_list) then exidx=1 end if
    end if
    if isChecked(of) then
        if find(extension,other_list) then exidx=1 end if
    end if
    if isChecked(uf) then
        if find(extension,do_list)
        or find(extension,doc_list)
        or find(extension,html_list)
        or find(extension,skip_list)
        or find(extension,other_list) then
        else
            exidx=1
        end if
    end if
    if find(LowerCase(file_name),
            {"compat01.ew","tk_maths.e","tk_mem.e","tk_misc.e","tk_trim.e",
             "w32keys.e","win32lib.ew","clib.e","clib.txt","w32dll.ew",
             "w32comctl.ew","w32kernel.ew","w32shell.ew","w32comdlg.ew",
             "w32winmm.ew","w32gdi.ew","w32user.ew","ui_msgs.e",
             "menuchk.exw","types.e","winfile.ew","tracker.e"}) then
        if isChecked(wf) then
            exidx=1
        else
            return 0
        end if
    end if
    if exidx then
        scan(path_name)
    end if
    return 0
"end function

"procedure updateFifList()
sequence text
integer k
    text=getText(findiftext)
    if length(text) then
        k = getCount(findiftext)
        while k do
            if compare(text,getItem(findiftext,k))=0 then exit end if
            k-=1
        end while
        if k = 0 then
--          addItem(findiftext,text)
            void = insertItem(findiftext,text,0)
        end if
    end if
"end procedure

--with trace
"procedure onclick_GO()
-- find the noted text
object void
object d
sequence edir
sequence top_dir
integer c, cprev
integer linelength

--  result={}
--  searchstring = getText(findiftext)
    -- exit unless have some text to work with
    if not length(searchstring) then
        void = messageBox(xl("Error"), 
                          xl("Enter one or more words to find."),
                          MB_ICONEXCLAMATION + MB_OK)
        setFocus(findiftext)
        return
    end if
--  if isChecked(cd)+isChecked(ed)+isChecked(ei)=0 then
--      void = messageBox("Error", 
--                        "No directory selected.",
--                        MB_ICONEXCLAMATION + MB_OK)
--      return
--  end if
    if isChecked(sf)+isChecked(df)+isChecked(hf)+isChecked(of)+
       isChecked(kf)+isChecked(uf)+isChecked(wf)+isChecked(af)=0 then
        void = messageBox(xl("Error"), 
                          xl("No files selected."),
                          MB_ICONEXCLAMATION + MB_OK)
        return
    end if

    log_name = "ex.err"
    log_path = filepaths[currfile]

    if platform() = LINUX then -- should never happen
        SLASH = '/'
    else
        SLASH = '\\'
    end if

    log_file = open(log_path&log_name, "w")
    if log_file = -1 then
        void = messageBox(xl("Error"), 
                          xl("Couldn't open ") & log_path & log_name & '\n',
                          MB_ICONEXCLAMATION + MB_OK)
        return
    end if

    close(log_file) 

    updateFifList()
        
    init_charmash()

    -- quits after finishing current file
--  terminatea = 0
--  hits = 0 (done)
--  files_scanned = 0
--  directories_scanned = 0
--  lines_scanned = 0
--  result&={xl("Searching for: ")&searchstring}
--  result = append(result,xl("Searching for: ")&searchstring)
--  result = append(result," Files scanned %d, Directories scanned %d")
    setText(c1,"")

    addFocus(PROGRESS)
    openWindow(PROGRESS, SW_NORMAL)
        
    bddone = 0
    if isChecked(cd) then   
        top_dir = filepaths[currfile]
        
        stacked_dir={}
        while 1 do
            directories_scanned += 1
            if walk_dir(top_dir, routine_id("look_at"), False) then
            end if
            if isChecked(sd) and length(stacked_dir) then
                top_dir=stacked_dir[1]
                stacked_dir=stacked_dir[2..length(stacked_dir)]
            else
                exit
            end if
        end while
    elsif isChecked(ed)+isChecked(ei)=0 then
        scan(filepaths[currfile]&filenames[currfile])
    end if  

    edir=""
    if isChecked(ed) then
        edir="EUDIR"
    elsif isChecked(ei) then
        edir="EUINC"
    end if
    if length(edir) then
        processed_files={}
        d = getenv(edir)
        if atom(d) then
            if platform() = LINUX then
                void = messageBox(xl("Error"), 
                                  edir & xl(" not set"),    --EUDIR/EUINC
                                  MB_ICONEXCLAMATION + MB_OK)
                return
            else
--DEV: C:\Program Files\Phix (or wherever)?
                d = `C:\EUPHORIA`
                if isChecked(ei) then
                    d = d & `\INCLUDE`
                end if
            end if
        end if
        if isChecked(ed) then
            edir=""
        else
            edir="EUDIR"
        end if
        if sequence(dir(d)) then
            -- reduce noise in Euphoria Help
            skip_list &= {"*.HTM", "*.HTX", "*.DAT", "*.BAS", "*.BAT", "*.PRO",
                          "LW.DOC", "BIND.EX", "*.ERR"}
            stacked_dir={}
            while 1 do
                directories_scanned += 1
                if walk_dir(d, routine_id("look_at"), False) then end if
                if length(stacked_dir) then
                    d=stacked_dir[1]
                    stacked_dir=stacked_dir[2..length(stacked_dir)]
                else
                    if not length(edir) then exit end if
                    d = getenv(edir)
                    if atom(d) then exit end if
                    d&=`\include`
                    edir=""
                end if
            end while
        end if
    end if

    if bddone=0 then
        if isChecked(bd) then
            d = initialcurrentdir&`backup\`
            directories_scanned += 1
            if walk_dir(d, routine_id("look_at"), False) then
            end if
        end if
    end if
--  result[2] = sprintf(" Files scanned %d, Directories scanned %d, Lines %d",
--                        {files_scanned,directories_scanned,lines_scanned})


    removeFocus(PROGRESS)
    setVisible(PROGRESS, False)

    removeFocus(FindIF)
    setVisible(FindIF,False)
    setFocus(Main)

--  if not openFile(log_path&log_name) then
--  crashpath = log_path
--see below...
--  if not openAndShuffleErr(0,log_name) then
--  cperr = crashpath&ename
--  log_name = "ex.err"
--  log_path = 

"end procedure


--DEV: getEdit not in 0.55.1
--object findEdit -- edit box within the Combobox
--  findEdit = findChildren(findiftext)
--  if length(findEdit)!=1 then ?9/0 end if -- definition of ComboBox has changed
--  findEdit=findEdit[1][1]


without trace
"function fifHandler(integer id, integer msg, atom wParam, object lParam)
    if object(lParam) then end if
    if msg = WM_CHAR then
        if find(id,{PROGRESS,CANCEL}) then
            if find(wParam,{VK_RETURN,VK_ESCAPE}) then
                msg = WM_COMMAND
                id = CANCEL
            end if
        else
            if wParam = VK_RETURN then
                msg = WM_COMMAND
                id = GO
            elsif wParam = VK_ESCAPE then
                msg = WM_CLOSE
            end if
        end if
    end if
    if msg = WM_COMMAND then
        if id=flab then
            setFocus(findiftext)
        elsif find(id,{cd,sd,ed,ei,bd,sf,df,hf,of,kf,uf,wf,af}) then
            if lParam=1 then    -- accelerator key
                setCheck(id,not isChecked(id))
            else                -- space bar or mouse click
                setCheck(id,isChecked(id))
            end if
        elsif id = GO then
            onclick_GO()
        elsif id = CANCEL then
            terminatea=1
        end if
    elsif msg = WM_CLOSE then
        removeFocus(FindIF)
        setVisible(FindIF, False)
        setFocus(Main)
    end if
    return 0
"end function
setHandler({FindIF,flab,findiftext,findiftext+1,cd,sd,ed,ei,bd,
            sf,df,hf,of,kf,uf,wf,af,GO,PROGRESS,CANCEL},
            routine_id("fifHandler"))


"function pack(sequence text)
    return text[1]  --DEV
"end function

"global procedure FIF()
--sequence junk
    if currfile then
        if actionsave[currfile] != actionptr[currfile] then
            if not saveCurrAs() then return end if
        end if
--      if getVal(SAVEFLAG) then
--          if messageBox("Error", 
--                        "Modifications to "&getVal(FILENAME)&" not saved\n\n"&
--                        "Save and continue?\n",
--                        MB_OKCANCEL)=IDCANCEL then
--              return
--          else
--              onClick_SAVE(0,0,{})
--          end if
--      end if 
        if selON and selY=CursorY and selX!=CursorX then
            setText(findiftext,pack(getSelection(SEL_COPY)))
--          setCheck(CASE,True)
        end if
        void=sendMessage(findiftext+1,EM_SETSEL,0,-1)

--      if isTextSelected() then
--          junk=getTextSelection()
--          if length(junk)=1 then      -- not if > 1 line selected
--              setText(findiftext,junk[1]) -- set as search target
--          end if
--      end if
--      updateFifList()
        addFocus(FindIF)
        openWindow(FindIF, SW_NORMAL)
    end if
"end procedure
global constant r_FIF=routine_id("FIF")
--*/
