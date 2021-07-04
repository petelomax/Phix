--
-- psym.e
--
-- Symbol table handler
--
without trace

constant dumppsym = 0   -- 1: creates a possibly useful reference file, psym.txt
                        -- (maybe useful as plist.e kills unused symtab entries)
                        -- (tip: set to 1, then run "p p t01", then reset to 0)
                        -- (should have no impact on performance when left as 0,
                        --  and by "no impact" I mean maybe 0.0001s on running
                        --  "p -cp" but absolutely zero running p.exe itself.)
integer fnpsym  -- file no of psym.txt (iff dumpsym!=0)


sequence priorities,
--
-- This implements automatic global resolution, and answers the basic
--  question: When I look for variable X and more than one exists, then
--  which should I choose, and/or when to error, the english answer being:
--  * Locally defined first, then
--  * Direct and Indirect includes [treat as one], then
--  * The parent(s) along with whatever they include, then
--  * The grand_parent(s), etc.
--  Error out if >1 in whatever level you got up to.
--
-- The golden rule is this: If *something works standalone*, it should
--  work exactly the same way when included as part of a larger system.
--  Simple, really, the only difficult part is to realise this goal in
--  an uber-efficient and elegant way.
--
--      <aside>
--          Implicit forward calls may need to be made explicit, ie
--          you may need to insert a few "forward func/proc x", to
--          prevent resolution to a prior (global) instance of x.
--          Also, overriding a builtin or auto-include, a notion I
--          would strongly discourage, can really foul things up 
--          when you suddenly bolt together two chunks of code, but 
--          that's your problem not mine.
--      <\aside>
--
-- In case you ask "why would you have multiple global X?" the reality
--  is that with large projects built using components from several
--  authors, it is almost unavoidable - and certainly better to deal
--  with it in a logical and consistent way than simply ignore it, or
--  worse force you to edit one or more third party files despite the
--  fact they each work just fine when used individually.
--
-- A proper explanation most definitely requires a proper example:
--
-- fileno  desc       parents    priorities
--   1     inc7         {}      {2,1,1,1,1,1}
--   2       eric       {1}     {2,4,3,3,3,3}
--   3         bob      {2}     {2,4,6,5}
--   4           alice  {3}     {2,4,6,8}
--   5         diane    {2}     {2,4,4,4,10,9}
--   6           chris  {5}     {2,4,4,4,10,12}
-- [-4]        alice    =>{3,2}
-- [-6]        chris    =>{5,2}
--
-- The above shows the final table, having grown from an initial {2}.
--  (see test\t13inc7.exw and test\inc7\*.* for the actual code)
--
-- This is a non-trivial concept; take the time to understand it fully.
-- It is in fact a nice, elegant, and relatively simple solution to a
-- a fairly difficult problem, but may not seem so on first reading.
-- If you have not yet bothered to read test\t13inc7.exw etc yet, then
-- expect to get lost pretty quickly in the following explanation!
--
-- Hits in files with a higher priority get chosen; there is an
--  implied priorty of 0 for the builtins, otherwise 1 is the 
--  lowest priority, overridden by anything higher.
-- Multiple hits at a given priority with nowt better -> error.
-- A namespace now means "use the priority table from file N" (ish*).
--
-- Suppose we are hunting for a global X. Three examples:
--
--  1) At the end of inc7, if there is one in file 1 [inc7, pri 2], 
--       pick that, else if there is one (and only one) in files 2..6, 
--       [eric..chris, pri 1], pick that.  {2,1,1,1,1,1}.
--
--  2) At the end of eric, pick one in file 2 [eric, pri 4] if it exists,
--       else pick one (and only one) in files 3..6 [bob/alice/diane/chris, 
--       pri 3], else pick one in file 1 [inc7, pri 2].  {2,4,3,3,3,3}.
--
--  3) At the end of chris, obviously an X in chris has top priority(12), 
--      then one in diane(10), then one in eric/bob/alice(4), and lastly
--      one in inc7(2).  {2,4,4,4,10,12}
--
-- A namespace required/not specific enough error occurs if we find multiple 
--  (=ambiguous) instances at the highest priority level of any encountered.
--
-- An important aspect to notice is the diagonal 2-4-6-8-10-12 which should 
--  always be the highest value in any table entry. Although strictly speaking 
--  we do not need it to be so, this also means the priority table is perfect 
--  for looking up local variables. As an aside, length(priorites[1]) should 
--  be equal to length(priorities), as should length(priorities[$]), otherwise
--  the table is not strictly square, as shown above and detailed below.
--  When using a namespace, it will land on that diagonal, and do the right
--  thing for sub-includes. We start with cp = <diag-1> to skip other files:
--  that start value of cp is what I meant by (ish*) above, ie we do use
--  the priority table for file N, but only consider high enough entries,
--  for example bob:X uses the {2,4,6,5} entry with a cp of 5, see next.
--  (cp is the "current priority" variable, see InTable() below.)
--
-- Notice that once bob and alice are fully processed, the priority table is
--  no longer extended. This ensures that, say, our bob:X namespace lookup 
--  only bothers to look at the relevant file(s), ie with {2,4,6,5} & cp=5,
--  pickup an X in bob, or if none there, one in alice, but the cp=5 prohibits
--  any from inc7/eric, and the length of 4 prohibits any from chris/diane.
--
-- Notice how in chris and diane, the files bob and alice (now fully processed)
--  have been upgraded/promoted to the same priority as eric. The initial table
--  for diane ({2,4,4,4}) is copied from eric ({2,4,3,3}) using this rule,
--  since eric is the (first) parent of diane. What this means is that if X is
--  defined in eric and bob, an unqualified reference to X in diane triggers an
--  error, as it should, whereas both an appropriate eric:X or bob:X can pick
--  the right one by following the {4,3,3} or {4,6,5} info respectively.
--
-- Lastly, the inclusion of alice and chris (again) into bob has no effect,
--  whereas for other examples (eg inc8) a similar thing does, resulting in 
--  eg a priorities[5] of {2,2,2,9,10,9}.
--
        parents
--
-- See above. While itself this is pretty simple stuff, it is used to propagate 
--  changes up the priority table when a file is (re)included, and for nowt else.
--
--sequence localscopes  -- start of cleanup chain for dropScope -- now in pglobals
--sequence scopetypes,  -- now in pglobals
--
-- localscopes are simply a linked list start pointer to a chain of all locals added
--  since the last increaseScope. These need to be detached from the ternary tree.
--  Global variables are always linked into the ternary tree below any locals, to
--  simplify this delinking process (obviously they remain linked to the ternary
--  tree permanently). The detached locals are marked with a -2 so that they can 
--  be relinked to the ternary tree in pemit.e, for variable name output.
--
-- localscopes may be File or Routine (held in scopetypes).
--
--       scopefiles,    -- now in pglobals
--       scopelines,    -- ""
sequence scopeemits,    -- save of emitline (see pemit.e)
         scopelasts,    -- save of lastline (see pemit.e)
         scopetls,
--
-- scopefiles contains the filenos corresponding to localscopes (type File), for
--  restore when EOF of the included file is reached. Likewise scopelines entries
--  are only meaningful if the corresponding localscope is type File, and are used
--  to resume processing on the line after the include statement at EOF.
--NESTEDFUNC no longer true when scopetypes[scopelevel-1]=S_Rtn:
-- scopetls contains the corresponding toplevel sub.

--  There is very little difference between named and un-named (inline) constants, 
--  except for value pooling. Two named constants cannot refer to the same slot,
--  since two names need to be kept. An unnamed constant can refer to a previously
--  declared named constant of the same value, but not vice versa. [DEV test this]
--
        optsets
--
-- optsets just contains copies of optset at each scope level, ie is used to restore
--  the with/without settings when resuming on the line after an include statement.
--

--global integer scopelevel -- now in pglobals
--
-- scopelevel is the main ("current") index to localscopes, scopefiles, scopelines,
--  scopetypes, and optsets.
--

--without trace
--with trace
-- moved here 27/8/14:
global procedure apnds5(object code)
integer opLnv
--DEV (bug) compilation (p -cp) sometimes toggles between these two for reasons unknown.
--!/**/ #isginfo{code,0b0101,MIN,MAX,integer,-2} -- (integer|dseq of integer), any length
--!/**/ #isginfo{code,0b1101,MIN,MAX,integer,-2} -- (integer|dseq of integer), any length
--DEV borken:
--!/**/ #isginfo{s5,0b0100,MIN,MAX,integer,-2}      -- (as good a place as any to check this)
--!/**/ #isginfo{s5,0b0100,MIN,MAX,atom,-2}         -- WRONG!!
--!/**/ #isginfo{s5,0b1100,MIN,MAX,atom,-2}         -- EVEN WRONGER!! [DEV]
    if lastline!=emitline then
--DEV why oh why is this not just part of DoWithOptions?! (and optset=)
        opLnv = opLn
        if not bind then
            if    optset[OptProfile] then           opLnv = opLnp
--          elsif optset[OptProfileClone] then      opLnv = opLnpclone
--          elsif optset[OptProfileCoverage] then   opLnv = opLnpcover
            elsif optset[OptProfileTime] then       opLnv = opLnpt
            elsif optset[OptTrace] then             opLnv = opLnt
            end if
        end if
        s5 = append(s5,opLnv)       -- opLn/p/pt/t
        s5 = append(s5,emitline)
        lastline = emitline
--if emitline=2886 then trace(1) end if
    end if
    s5 &= code
end procedure

--
-- Lists of re-usable temporaries.
--
-- Each routine gets their own private temporaries so that when b() calls a()
--  it does not have to save it's own temps. (In practice when b() calls a() it
--  creates a new "frame", ie space for all locals/tmps/parameters in a().)
-- Top-level code also has temps, however although there may be a separate
--  top-level-sub [entry in symtab], one for each source file, actually they 
--  share the same tmp pool(s).
-- Atom temps are not re-used, to propagate any "must be integer" info better.
--
global sequence freetmplists    -- 15 (T_object) free list pointers
sequence freetmplistsX

integer currtls
        currtls = -1

--with trace
integer incfile, state

function upgradedParentPriorities(integer parent, integer newPriority)
--
-- Upgrades the priority of globals in files included by the parent
-- to be the same as the priority of those in the parent file, or
-- in other words a sub-include places no priority between globals
-- defined in the parent and/or the files the parent includes.
-- Also tags on the priority for the new file at the end.
--
-- Eg, assuming 8 is parent, {2,2,7,8,7} -> {2,2,8,8,8,12}.
--  (so while the parent, file 4, will pick an X from file 4 over
--   and above any in files 3 (a re-include) or 5 [since 8>7],
--   this new file (6), assuming it finds no X in file 6, must 
--   demand a namespace if it finds >1 X in files 3,4,5.)
--
--p2js:
    sequence res = deep_copy(priorities[parent])
    integer parentPriority = parent*2
    if res[parent]!=parentPriority then ?9/0 end if
    for i=1 to parent-1 do
        -- any re-includes?
        if res[i]=parentPriority-1 then
            res[i] = parentPriority
        end if
    end for
    for i=parent+1 to length(res) do
        -- the rest are all new
        res[i] = parentPriority
    end for
    return res & newPriority
end function

procedure addToParents(integer fno, integer elen)
-- Recursively extend the priority table for parents, adding the new
-- file at a priority one less than globals defined in the parent itself.
-- The elen (expected length) parameter is just to make sure recursion 
-- does not go crazy mad when files are included in more than one place.
integer k
sequence pk, pf
    pf = parents[fno]
    for i=1 to length(pf) do
        k = pf[i]
        pk = priorities[k]
        if length(pk)=elen then
            priorities[k] = 0       -- reduce ref count
            pk &= k*2-1
            priorities[k] = pk
            addToParents(k,elen)
        end if
    end for
end procedure

--without warning   -- (suppress "external forward reference; initialisation code may be skipped")
--global procedure AddFincParent()
--  parents = append(parents,{})
----    priorities = append(priorities,{})
--  priorities = append(priorities,{fileno*2})
----    priorities = append(priorities,{0})
--end procedure
--with warning

--with trace
global function increaseScope(integer scope, integer prevfile)
--
integer prevscope
--?{"increaseScope",scope, prevfile}
    if fileno<0 then    -- re-include
        --
        -- just adjust parents, priorities, and resume with same file
        --
        incfile = -fileno
        fileno = scopefiles[scopelevel]
        if fileno!=prevfile then ?9/0 end if    -- sanity check
--      col = 0
        if not find(fileno,parents[incfile]) then
            parents[incfile] = append(parents[incfile],fileno)
        end if
        -- and upgrade the priority of the include to match:
        if fileno!=incfile then -- not self-includer!
            priorities[fileno][incfile] = fileno*2-1
        end if
    else

        --
        -- First make sure there is a spare slot for the localscope
        --
        if scopelevel then
            optsets[scopelevel] = optset
        end if

        prevscope = scopelevel
        scopelevel += 1

        if scopelevel>length(scopetypes) then
            localscopes = append(localscopes,0)
            scopetypes = append(scopetypes,0)
            scopefiles = append(scopefiles,0)
            scopelines = append(scopelines,0)
            scopecols = append(scopecols,0)
            scopeemits = append(scopeemits,0)
            scopelasts = append(scopelasts,0)
            scopetls = append(scopetls,0)
            optsets = append(optsets,optset)
        end if

        localscopes[scopelevel] = 0     -- always start a new localscope,
        scopetypes[scopelevel] = scope  --   be it S_File or S_Rtn

        if scope=S_File then

            if prevfile then
                parents = append(parents,{prevfile})
                priorities = append(priorities,upgradedParentPriorities(prevfile,fileno*2))
                addToParents(fileno,fileno-1)
            else
                if fileno=0 then
--if newEmit then
--printf(1,"psym.e line 327: fileno=0, FincMax=%d\n",{getFincMax()})    -- 0...
--end if
                    parents = {}
                    priorities = {}
                else
                    parents = append(parents,{})
                    priorities = append(priorities,{fileno*2})
                end if
            end if
            --
            -- add to current includeset, create new File and Global scopes,
            --  and save line number so we can resume, before resetting to line 1 col 0. 
            --
            scopefiles[scopelevel] = fileno

            if scopelevel>1 then
                scopelines[prevscope] = line
                if Ch<=0 then
                    scopecols[prevscope] = -1   -- (equivalent to ltl+1)
                else
                    scopecols[prevscope] = tokcol
                end if

                line = 1
--              col = 0

                symlimit += 1

                symtab[currtls][S_il] = s5
                s5 = {}
                scopeemits[prevscope] = emitline
                scopelasts[prevscope] = lastline
                emitline = -1
                lastline = -1

                currtls = symlimit
                if symlimit>length(symtab) then
                    symtab &= repeat(0,2000)
                end if
                state = S_used+K_used
                if optset[OptDebug] then
                    state += K_wdb
                end if
                -- {name, type, file, state, link, scope, sig, par1/N/L, il, ltab, 1stl,s_efct}:
                if newEBP then
                    symtab[currtls] = {-1,          -- S_Name[1]    (a top_level_sub)
                                       S_Proc,  -- S_NTyp[2]
                                       fileno,  -- S_FPno[3]
                                       state,   -- S_State[4]
                                       0,       -- S_Nlink[5]
                                       0,       -- S_Slink[6]
                                       {'P'},   -- S_sig[7]
                                       0,       -- S_Parm1[8]
                                       0,       -- S_ParmN[9]
                                       0,       -- S_Ltot[10]
                                       0,       -- S_il[11]
                                       0,       -- S_ltab[12]
                                       1,       -- S_1stl[13]
                                       0}       -- S_Efct[14]
                else
                    symtab[currtls] = {-1,          -- S_Name[1]    (a top_level_sub)
                                       S_Proc,  -- S_NTyp[2]
                                       fileno,  -- S_FPno[3]
                                       state,   -- S_State[4]
                                       0,       -- S_Nlink[5]
                                       0,       -- S_Slink[6]
                                       {'P'},   -- S_sig[7]
                                       -1,      -- S_Parm1[8]
                                       0,       -- S_ParmN[9]
                                       0,       -- S_Ltot[10]
                                       0,       -- S_il[11]
                                       0,       -- S_ltab[12]
                                       1,       -- S_1stl[13]
                                       0}       -- S_Efct[14]
                end if
                scopetls[scopelevel] = currtls
            else
                scopetls[1] = T_maintls
            end if
        elsif scope=S_Rtn then

--NESTEDFUNC (needs a stack)
            freetmplistsX = freetmplists        -- save top_level_subs temps
            freetmplists = repeat(0,T_object)   -- create 15 new free list pointers

--          if DEBUG then
--              if scope!=S_Rtn then ?9/0 end if
--          end if
            --
            -- Start of new routine. Save code for _top_level_sub and set to {},
            -- and add a temp scope to hold params and locals.
            --
if NESTEDFUNC and scopelevel>1 and scopetypes[scopelevel-1]=S_Rtn then
            scopetls[scopelevel-1] = currRtn
            symtab[currRtn][S_il] = s5
            s5 = {}
else -- (old code)
            symtab[currtls][S_il] = s5
            s5 = {}
end if
--DEV/SUG (28/8/14):   scopelasts[prevscope] = lastline
-- (if "end procedure" is the very last line, the final file-level opRetf does not get an opLn, so ends up next to some earlier emitted code...)

--      elsif scope=S_Block then
--          -- (nothing else needs doing)
        end if
    end if
    return currtls
end function

--10/10/2020
--global procedure clearTLSDebug()
global procedure clearTLSDebug(bool optOn)
-- "without debug" applies to the whole file, including the top_level_sub,
--  which has normally been created before the "without debug" is found.
    state = symtab[currtls][S_State]
    if optOn then
        if not and_bits(state,K_wdb) then
            state += K_wdb
            symtab[currtls][S_State] = state
        end if
    else
        if and_bits(state,K_wdb) then
            state -= K_wdb
            symtab[currtls][S_State] = state
        end if
    end if
end procedure

global procedure s5thunk(object o)  -- (also used in pemit.e)
--/**/
--/**/  -- WARNING: Dirty trick. This does the same as the RDS Eu
--/**/  -- compatible code below, but in such a way as to foil the 
--/**/  -- gvar scan. As mentioned elsewhere, modifying hll vars 
--/**/  -- with #ilASM is dicey like that. We just swap o and s5 
--/**/  -- (thus avoiding any refcount issues, see below) and can 
--/**/  -- only do so because we are certain that saving/restoring 
--/**/  -- s5 to/from (eg) [S_il] does not change it's type. The
--/**/  -- equivalent RDS Eu hll code makes s5 an "object", when
--/**/  -- we are trying to keep it "sequence of integer". Whether
--/**/  -- that actually yields any measureable gain is unknown,
--/**/  -- but at least we get an error from s5[i]="string" etc.
--/**/
--/**/  #ilASM{ 
--/**/      [32]
--/**/          mov edx,[o]
--/**/          mov eax,[s5]
--/**/          mov [s5],edx
--/**/          mov [o],eax
--/**/      [64]
--/**/          mov rdx,[o]
--/**/          mov rax,[s5]
--/**/          mov [s5],rdx
--/**/          mov [o],rax
--/**/        }
--/* -- RDS Eu equivalent (but not "type-safe"):
        s5 = o
--*/
end procedure
-- Above I blythely said "thus avoiding any refcount issues"; it
-- is probably worthwhile explaining this in excrutiating detail.
-- First, consider the hll "s5=o" approach. On entry, o has a
-- refcount of 2, one for symtab[?][S_il] and one for o itself.
-- The s5=o line increases that to 3 and derefs/deallocs s5.
-- Finally the end procedure derefs o, leaving the refcount 2.
-- The #ilASM does no refcounting whatsoever, and it is the
-- end procedure which derefs s5 since that is now in o. Thus
-- the net effect is the same, but you should fully understand
-- the difference before using this in your application code,
-- and think carefully before "inlining" the above.

global integer builtinsReferenced
               builtinsReferenced = 0

sequence binftab
sequence agfiles,   -- filenames, eg "VM\pHeap.e"
         agfdone,   -- -1 after syminit, 0 = rqd, 1 after getBuiltin() (same length as agfiles)
         agtidx,    -- from tt_string(glabel,-3)s
         agfnos     -- corresponding index of agtidx[i] to agfiles[] (same length as agtidx)

--with trace
global procedure agchecktt(integer tidx, integer done=0)
integer k = find(tidx,agtidx)
integer fno
    if not emitON then ?9/0 end if
    if k!=0 then
        fno = agfnos[k]
        if agfdone[fno] = -1 then
--printf(1,"agcheck: %s set\n",{agfiles[fno]})
--trace(1)
--          agfdone[fno] = 0
            agfdone[fno] = done
if done=0 then
-- added 31/3/17:
--if suppressopRetf then ?9/0 end if
-- added 30/8/14:
            builtinsReferenced = 1
end if
        end if
    end if
end procedure

global procedure agcheckop(integer opcode)
integer tidx
    tidx = aatidx[opcode]
    if tt[tidx+EQ]=0 then
        agchecktt(tidx)
--      lblidx = get_lblidx(ttidx)
    end if
end procedure


without trace
--with trace
global function dropScope(integer routineNo, integer scopetype)
integer st, -- scope type
        scopechain,
        tidx,   -- scratch, fake ttidx
        tnxt,   -- scratch, scope clearance
        wastls,
        effects
sequence ss     -- copy of symtab[scopechain], speedwise
object sc       -- scratch var, copy of symtab[currtls]
--?{"dropScope",routineNo,scopetype}
    st = scopetypes[scopelevel]
    scopetypes[scopelevel] = 0      -- added 13/11
    if st!=scopetype then ?9/0 end if

    if fileno and st=S_File then
        finalOptWarn[fileno] = optset[OptWarning]
    end if

    optsets[scopelevel] = 0
    --
    -- unlink everything on the scope chain:
    --
    scopechain = localscopes[scopelevel]
    while scopechain do
        ss = symtab[scopechain]
        tnxt = ss[S_Nlink]
        if tnxt!=-2 then
            tidx = ss[S_Name]
            symtab[scopechain] = 0      -- reduce refcount on ss
            ss[S_Nlink] = -2
            symtab[scopechain] = ss
            tt[tidx+EQ] = tnxt
        end if
        scopechain = ss[S_Slink]
    end while

    scopelevel -= 1
    if st!=S_Block then
        if scopelevel then
            optset = optsets[scopelevel]
        end if
        wastls = 0

        if st=S_Rtn then

            -- drop the scope and save the code, before restoring _top_level_sub's code

            symtab[routineNo][S_il] = s5

--NESTEDFUNC (needs a stack)
            freetmplists = freetmplistsX    -- restore top_level_subs temps

        else --st=S_File then

            -- restore the file/line to continue after the include statement.

            sc = symtab[currtls]
            if length(s5) then
--if 01 then 
                -- new code (28/2/14, avoid opCallOnce to null-effect top-level
                --  subroutines; if I am to migrate opcodes to ilASM, I really 
                --  don't want every app to start with 200+ unnecessary calls.)
--DEV isJmpG,0,0<:%opRetf>? (newEmit)
                if length(s5)<11
                or s5[1]!=opLn 
                or s5[3]!=opAsm
                or s5[4]<5
                or s5[7]!=#E9 --jump_rel32
                or ((s5[8]!=isJmp or s5[11]!=length(s5)) and
-- 11/9/14: (when newEmit becomes permanent, might be able to get rid of isOpCode/opRetf, but leaving it in [forever] shouldn't hurt)
--                  (s5[8]!=isOpCode or s5[11]!=opRetf)) then
                    (s5[8]!=isOpCode or s5[11]!=opRetf) and
                    (s5[8]!=isJmpG or s5[11]!=tt[aatidx[opRetf]+EQ]) and
                    (s5[8]!=isJmpG or s5[11]!=tt[aatidx[opCallOnceYeNot]+EQ])) then
--                  s5 = append(s5,opRetf)
--if length(s5)<=12 then
--  ?{wastls,s5}
--else
--  ?{wastls,s5[1..12]}
--end if
--  {0,{213,20,     -- opLn
--      212,5,0,0,  -- opAsm
--      #E9,#900,0,0,12,224}}isJmpG, 
--if aatidx[opRetf] = ttidx then
--                      tt[ttidx+EQ] = lblidx
--  printf(1,"opRetf; lblidx=%d\n",lblidx)
--end if
                    wastls = currtls
                end if
                -- (oops, ilxlate crashes if we leave this off..)
-- 27/8/14: (does not help)
--              s5 = append(s5,opRetf)
--trace(1)
                emitline = line
-- 4/12/14:
if not suppressopRetf then
    if newEmit then
                agcheckop(opRetf)
    end if
                apnds5(opRetf)
end if
--else
--              s5 = append(s5,opRetf)
--              wastls = currtls
--end if
                effects = sc[S_Efct]
            end if
            symtab[currtls] = 0
--p2js:
            sc = deep_copy(sc)
            sc[S_il] = s5
            symtab[currtls] = sc

            if scopelevel then
-- 18/9/10
--          if checkbuiltin[fileno] then
--              for i=T_Bin to T_Ainc do
--                  if symtab[i][S_FPno]=fileno then
--                      if and_bits(symtab[i][S_State],S_fwd_and_used)=S_fwd_and_used then
--                          Abort("incorrect autoinclude version (missing "&getname(symtab[i][S_Name],-2)&')')
--                      end if
--                  end if
--              end for
--          end if
                fileno = scopefiles[scopelevel]
--29/12/2010:
--          text = allfiles[fileno]
                text = allpfiles[fileno]
                ltl = length(text)
                line = scopelines[scopelevel]
                scopelines[scopelevel] = 0      -- added 13/11/10
                col = scopecols[scopelevel]
                if col=-1 then
                    Ch = -1
                else
                    Ch = text[col]
                end if
                emitline = scopeemits[scopelevel]
                lastline = scopelasts[scopelevel]
            end if

        end if -- st=S_File
if NESTEDFUNC and st=S_Rtn and scopetypes[scopelevel]=S_Rtn then
--      ?9/0
        -- (as below, just leaving currtls undamaged, and no callonce)
        integer parentRtn = scopetls[scopelevel]
        sc = symtab[parentRtn]

        s5thunk(sc[S_il])   -- sets s5

        symtab[parentRtn] = 0 -- kill refcount
        sc[S_il] = 0        -- kill refcount
        symtab[parentRtn] = sc

        return parentRtn
else
        if scopelevel then
            currtls = scopetls[scopelevel]
        end if

        sc = symtab[currtls]

        s5thunk(sc[S_il])   -- sets s5

        symtab[currtls] = 0 -- kill refcount
        sc[S_il] = 0        -- kill refcount
        symtab[currtls] = sc
        if wastls then
--          s5 = append(s5,opCallOnce)
--          s5 = append(s5,wastls)
--trace(1)
            emitline = line
            apnds5({opCallOnce,wastls})
            if NOLT or bind or lint then
                if and_bits(effects,E_vars) then
                    ltCall(0,effects,length(s5)-1) -- clear rqd gvar info
                end if
            end if -- NOLT
        end if
end if
    end if
    return currtls
end function

--NESTEDFUNC:
global function hideScope()
    --
    -- hide localscope chain over nested function definition.
    -- restScope is whatever restoreScope() needs it to be.
    --
    sequence restScope = {}
    integer scopechain = localscopes[scopelevel]
    while scopechain do
        integer tnxt = symtab[scopechain][S_Nlink]
        if tnxt!=-2 and symtab[scopechain][S_NTyp]=S_TVar then
            integer tidx = symtab[scopechain][S_Name]
--          symtab[scopechain][S_Nlink] = -2
            if tt[tidx+EQ]!=scopechain then ?9/0 end if
            tt[tidx+EQ] = tnxt
            restScope &= scopechain
        end if
        scopechain = symtab[scopechain][S_Slink]
    end while
    return restScope
end function

global procedure restoreScope(sequence restScope)
--  for i=1 to length(restScope) do
    for i=length(restScope) to 1 by -1 do
        integer scopechain = restScope[i]
--      sequence ss = symtab[scopechain]
        integer tidx = symtab[scopechain][S_Name]
--      integer tnxt = symtab[scopechain][S_Nlink]
        tt[tidx+EQ] = scopechain
    end for 
end procedure

integer slink, glink, scope
        slink = -1
--      glink = 0
--integer p, op

without trace
--with trace

--DEV/SUG , integer minp=0[/-1?])  [needs a fairly thorough working through from initialSymEntry]

integer tidx, fno -- work vars
global function addSymEntry(integer ttidx, integer asGlobal, integer Stype, object sig, object code, integer state)
sequence ssl    -- symtab[slink/symlimit], speedwise
integer sl2     -- adjusted copy of scopelevel
integer minp    -- min params (for initialSymEntry; DoRoutineDef (and hence initialAutoEntry) does it later)

    symlimit += 1
    if symlimit>length(symtab) then
        symtab &= repeat(0,2000)
    end if
    if ttidx=-1 then
        slink = 0
    else
        slink = tt[ttidx+EQ]
    end if
    scope = 0
--  if asGlobal then
    if asGlobal and Stype!=S_Nspc then
        --
        -- keep the S_Nlink chain as {locals},{globals} so that dropScope can
        --  just pick off the top element, rather than go hunting for it.
        --
        glink = 0
        while slink do
            ssl = symtab[slink]
            if and_bits(ssl[S_State],K_gbl) then exit end if
            glink = slink
            slink = ssl[S_Nlink]
        end while
    else
        sl2 = scopelevel
--      if Stype>S_Rsvd
--      and scopetypes[sl2]=S_Rtn then
        if (Stype>S_Rsvd and scopetypes[sl2]=S_Rtn)
        or (Stype=S_Nspc and asGlobal) then
--?{"sl2",sl2}
            sl2 -= 1
            asGlobal = 0
        end if
        scope = localscopes[sl2]
    end if

    if asGlobal then
        state = or_bits(state,S_used_set_gbl)
    elsif not optset[OptWarning] then
        state = or_bits(state,S_used_and_set)
    elsif Stype=S_Const then
        state = or_bits(state,S_set)
    end if
    if optset[OptDebug] then
        state += K_wdb
    end if
    if Stype<=S_TVar then
        if Stype=S_TVar then
            TIDX -= 1
            if newEBP then
                LIDX -= 1
                tidx = LIDX
            else
                tidx = TIDX
            end if
--          fno = currRtn
        else -- Stype = S_GVar2/S_Const
            tidx = 0
--          fno = fileno
        end if
        ssl = {ttidx,           -- S_Name[1]
               Stype,           -- S_NTyp[2]
--26/11/19 (for nested functions, test...) [BUST...]
               fileno,          -- S_FPno[3]
--             fno,             -- S_FPno[3] (now currRtn for Tvar)
               state,           -- S_State[4]
               slink,           -- S_Nlink[5]
               scope,           -- S_Slink[6]
               sig,             -- S_vtype[7]
               0,               -- S_value[8]
               tidx,            -- S_Clink[9](=0)/S_Tidx[9](=TIDX)
               tokcol,          -- S_ErrV[10]
               0,               -- S_Init[11]
               sig,             -- S_ltype[12]
               0,               -- S_maxlv[13]
               0,               -- S_gInfo[14]
               0}               -- S_gNew[15]
    elsif Stype<=S_Rsvd then -- S_Nspc or S_Rsvd
        fno = fileno
        if fileno<0 then
            fno = -fileno
        end if
        ssl = {ttidx,           -- S_Name[1]
               Stype,           -- S_NTyp[2]
               sig,             -- S_FPno[3]
               state,           -- S_State[4]
               slink,           -- S_Nlink[5]
               scope,           -- S_Slink[6]
               fno}             -- S_nFno[7]
    else -- routines
        if newEBP then
            tidx = 0
        else
            tidx = -TIDX+1
        end if
        if sequence(sig) then
            minp = length(sig)-1
        else
            minp = 0
        end if
        ssl = {ttidx,           -- S_Name[1]
               Stype,           -- S_NTyp[2]
               fileno,          -- S_FPno[3]
               state,           -- S_State[4]
               slink,           -- S_Nlink[5]
               scope,           -- S_Slink[6]
               sig,             -- S_sig[7]
               tidx,            -- S_Parm1[8]
--             0,               -- S_ParmN[9]
               minp,            -- S_ParmN[9]
               0,               -- S_Ltot[10]
               code,            -- S_il[11]
               {},              -- S_ltab[12]
               0,               -- S_1stl[13]
               E_none,          -- S_Efct[14]
               tokcol}          -- S_ErrR[15]
    end if
    symtab[symlimit] = ssl
    if ttidx!=-1 then
        if asGlobal then
            if glink then
                symtab[glink][S_Nlink] = symlimit
            else
                tt[ttidx+EQ] = symlimit
            end if
        else
            tt[ttidx+EQ] = symlimit
            localscopes[sl2] = symlimit
        end if
    end if
    return symlimit
end function

global function addSymEntryAt(integer ttidx, integer asGlobal, integer Stype, object sig, object code, integer state, integer atcol)
integer savetokcol, N
    savetokcol = tokcol
    tokcol = atcol
    N = addSymEntry(ttidx,asGlobal,Stype,sig,code,state)
    tokcol = savetokcol
    return N
end function

--with trace
global procedure ReLinkAsGlobal(integer ttidx, integer N)
--
-- An implicit forward call has assumed local not global, but the actual
--  turned out to be a global. Technically a "forward proc/func xxx" is
--  an error (xxx was, by omission, explicitly defined as local) but we
--  just do this with no error (for now).
--
sequence ssl    -- symtab[slink/symlimit], speedwise
integer tlink, nlink
integer found = 0
integer sl2
--trace(1)
    tlink = tt[ttidx+EQ]
    slink = tlink
    glink = 0
    while slink do
        ssl = symtab[slink]
        if and_bits(ssl[S_State],K_gbl) then exit end if
        nlink = ssl[S_Nlink]
        if slink=N then
            if glink then
                symtab[glink][S_Nlink] = nlink
            else
                tt[ttidx+EQ] = nlink
            end if
            found = 1
        end if
        glink = slink
        slink = nlink
    end while
    if found=0 then ?9/0 end if
    if glink=0 then ?9/0 end if
--  ssl = {}
    symtab[N][S_Nlink] = slink
    if glink=N then
        tt[ttidx+EQ] = N
    else
        symtab[glink][S_Nlink] = N
    end if
    symtab[N][S_State] = or_bits(symtab[N][S_State],S_set+K_gbl)

    --
    -- And detach from the localscope...
    --
    nlink = symtab[N][S_Slink]
    sl2 = scopelevel
--  if scopetypes[sl2]=S_Rtn then
--      sl2 -= 1
--  end if
    scope = localscopes[sl2]
    if scope=N then
        localscopes[sl2] = nlink
    else
        while 1 do
            slink = scope
            scope = symtab[slink][S_Slink]
            if scope=N then
                symtab[slink][S_Slink] = nlink
                exit
            end if
        end while
    end if

end procedure

without trace


--DEV there's a global of this very value in pglobals.e, btw
--<constant Tmap = {T_integer,T_atom,T_string,T_sequence,T_object}

--DEV cleanme (should res be string or dword_sequence, does it matter?; try res=sig(or return sig) and the one-liner)
function mapsig(sequence sig)
-- (internal/builtin use only, no udts here)    [[29/7/19 bluff...]]
sequence res
integer k
    k = sig[1]
    res = repeat(k,length(sig))
    for i=2 to length(sig) do
        k = sig[i]
        k = find(k,"INSPO")
--<     k = Tmap[k]
        k = typeINSPO[k]
        res[i] = k
--<     res[i] = Tmap[find(sig[i],"INSPO")]
--      res[i] = typeINSPO[find(sig[i],"INSPO")]
    end for
    return res
end function


--/* Not required for Phix (not that it would hurt any)
include builtins\misc.e -- sprint()
--*/

global function addUnnamedConstant(object o, integer sig)
-- Add an unnamed constant - can re-use a named entry
--  Warning: small changes in this code can lead to dramatic changes
--           in overall performance...
integer state
integer wasttidx    --DEV may not be required (initialConstant relies on this, easily fixed that one tho)
integer snext
    wasttidx = ttidx
--if equal(o,1) then trace(1) end if

    if emitON=0 then return 0 end if
    if atom(o) then
        tt_atom(o)
--DEV sig=T_string?
--  elsif string(o) and sig!=T_sequence then
    elsif string(o) and sig!=T_sequence and sig!=T_Dsq then
        tt_string(o,-1)
    else
        tt_sequence(o)
    end if
    slink = tt[ttidx+EQ]
    if slink then
        ttidx = wasttidx
        return slink
    end if
    symlimit += 1
    if symlimit>length(symtab) then
        symtab &= repeat(0,2000)
    end if
    state = S_set
    if not optset[OptWarning] then
        state = S_used_and_set
    end if
    state += K_litnoclr
    if slink then
        snext = symtab[slink][S_Clink]
    else
        snext = 0
    end if
    symtab[symlimit] = {-1,             -- S_Name[1]
                        S_Const,        -- S_NTyp[2]
                        fileno,         -- S_FPno[3]    --DEV 0??
                        state,          -- S_State[4]
                        0,              -- S_Nlink[5]
                        0,              -- S_Slink[6]
                        sig,            -- S_vtype[7]
                        o,              -- S_value[8]
                        snext,          -- S_Clink[9]
                        0,              -- S_ErrV[10]
                        ttidx,          -- S_Init[11]
                        sig,            -- S_ltype[12]
                        0}              -- S_maxlv[13]
    -- NB: DoSequence() appends S_gInfo to the above for etype/len in the allconst case.
    if slink then
        symtab[slink][S_Clink] = symlimit
    else
        tt[ttidx+EQ] = symlimit
    end if
    ttidx = wasttidx
    return symlimit
end function

--with trace
global function addRoutineId(integer r)
-- Add an S_Const style entry for a compile-time-resolved routine_id.
-- During binding, this may need to be mapped, eg if unused entries
-- in the symtab are removed, so a) it needs to be marked with K_rtn,
-- and b) it should not 'share' - eg if r is say 373, mapping it must 
-- not affect any other literal 373 in the program.
--trace(1)
    symlimit += 1
    if symlimit>length(symtab) then
        symtab &= repeat(0,2000)
    end if
    symtab[symlimit] = {-1,                     -- S_Name[1]
                        S_Const,                -- S_NTyp[2]
                        fileno,                 -- S_FPno[3]
--21/2/14
--                      S_used_and_set+K_rtn,   -- S_State[4]
                        S_set+K_rtn,            -- S_State[4]
                        0,                      -- S_Nlink[5]
                        0,                      -- S_Slink[6]
                        T_integer,              -- S_vtype[7]
                        r,                      -- S_value[8]
                        0,                      -- S_Clink[9]
                        0,                      -- S_ErrV[10]
                        1,                      -- S_Init[11]
                        T_integer}              -- S_ltype[12]
    return symlimit
end function


global constant Shared = 1, Private = 0, FuncRes = -1
--with trace
--global function newTempVar(object sig, integer shared, integer init)
global function newTempVar(object sig, integer shared)
-- sig is T_object/T_sequence/T_string/T_Dsq/T_atom/T_integer; the main use
--  of which is to keep integers separate to avoid dealloc calls, oh,
--  and it may reduce typechecking in a few places, maybe.
-- shared is true (first constant above) for most things (ie reusable).
--  it is Private when allocating function return vars, and [DEV] currently
--  also for loop vars, which are theoretically re-usable, I think...
-- init should be zero except for T_integers, for which it can be 1 if the 
--  temp is about to be assigned real soon, ie will be by the next time
--  anything (in the compiler that is) checks it.
sequence sfl -- symtab[freetmp[i]list]
integer state
integer res
-- 1/10/2011 (top-level temps must be GVars for newEBP - seems to work <BSG> )
integer vartype
integer tidx

--1/10/2011:
    vartype = S_TVar
    if shared!=FuncRes
    and returnvar=-1 then
        vartype = S_GVar2
--no!       shared = FuncRes 
    end if
    if reusetmps then
        if shared>0 then
            --      if shared>0 and not and_bits(sig,T_N) then
            --DEV 9/1/13:
            if sig=0 then
                res = freetmplists[T_object]
            else
                res = freetmplists[sig]
            end if
            if res then
                sfl = symtab[res]
                freetmplists[sig] = sfl[S_Nlink]
                if DEBUG then
                    if sfl[S_Name]!=-1 then ?9/0 end if
--                  if sfl[S_NTyp]!=S_TVar then ?9/0 end if
                    if sfl[S_NTyp]!=vartype then ?9/0 end if
                    if sfl[S_sig]!=sig then ?9/0 end if
                end if
                return res
            end if
        end if
    end if
    symlimit += 1
    if symlimit>length(symtab) then
        symtab &= repeat(0,2000)
    end if
    if vartype=S_TVar then
        TIDX -= 1
        if newEBP then
            LIDX -= 1
            tidx = LIDX
        else
            tidx = TIDX
        end if
    else -- S_GVar2 (1/10/2011)
        tidx = 0
    end if
    if shared=FuncRes then
        state = S_set+K_Fres
    else
        state = S_set
    end if
    symtab[symlimit] = {-1,             -- S_Name[1]
--                      S_TVar,         -- S_NTyp[2]
                        vartype,        -- S_NTyp[2]
                        0,              -- S_FPno[3]        --DEV fileno??
                        state,          -- S_State[4]
                        0,              -- S_Nlink[5]
                        0,              -- S_Slink[6]
                        sig,            -- S_vtype[7]
                        0,              -- S_value[8]
                        tidx,           -- S_Tidx[9]
                        0,              -- S_ErrV[10]
                        1,              -- S_Init[11]
                        sig,            -- S_ltype[12]
                        0,              -- S_maxlv[13]
                        0,              -- S_gInfo[14]
                        0}              -- S_gNew[15]
--DEV flag for onDeclaration handling?
    return symlimit
end function

procedure initialConstant(sequence name, atom v)
integer N
integer state, sig, wasttidx, slink, nlink, snext
    tt_string(name,-2)
    wasttidx = ttidx
    nlink = tt[wasttidx+EQ]
    symlimit += 1
    N = symlimit
    if dumppsym then
        printf(fnpsym,"initialConstant %d:%s\n",{N,name})
    end if
    tt[wasttidx+EQ] = N
    if emitON=0 then ?9/0 end if
    tt_atom(v)
    slink = tt[ttidx+EQ]
    if slink then
        snext = symtab[slink][S_Clink]
    else
        snext = 0
    end if
--16/10/10:
    state = K_lit+K_gbl+K_noclr
--  state = K_lit+K_gbl
--  if integer(v) then
    if not isFLOAT(v) then
        sig = T_integer
    else
        sig = T_atom
    end if
    symtab[symlimit] = {wasttidx,       -- S_Name[1]
                        S_Const,        -- S_NTyp[2]
                        fileno,         -- S_FPno[3]    --DEV 0??
                        state,          -- S_State[4]
                        nlink,          -- S_Nlink[5]
                        0,              -- S_Slink[6]
                        sig,            -- S_vtype[7]
                        v,              -- S_value[8]
                        snext,          -- S_Clink[9]
                        0,              -- S_ErrV[10]
                        ttidx,          -- S_Init[11]
                        sig,            -- S_ltype[12]
                        0}              -- S_maxlv[13]
    if slink then
        symtab[slink][S_Clink] = symlimit
    else
        tt[ttidx+EQ] = symlimit
    end if
end procedure

--with trace
procedure initialSymEntry(object name, integer Stype, sequence sig, integer opcode, integer sideeffects)
integer N
    if sequence(name) then
        tt_string(name,-2)
    else
        ttidx = -1
        if Stype!=S_Type then ttidx = 9/0 end if
        if symlimit>16 then ttidx = 9/0 end if
    end if
    N = addSymEntry(ttidx,1,Stype,mapsig(sig),opcode,0)
    symtab[N][S_State] = K_gbl
    if sideeffects and Stype>=S_Type then
        symtab[N][S_Efct] = sideeffects
    end if
    if opcode>0 then
        opEfct[opcode] = sideeffects
    end if
    if dumppsym then
        if opcode then
            if name=-1 then
                name = "-1"
            end if
            printf(fnpsym,"initialSymEntry %d:%s\n",{N,name})
        else
            printf(fnpsym,"initialAutoEntry %d:%s\n",{N,name})
        end if
    end if
end procedure

--with trace
procedure AutoAsm(object name, integer Stype, sequence sig, sequence filename, integer opcode, string glabel, integer sideeffects, integer rtype)
-- Maps a hll-style call to a global label, eg getc() wants the equivalent of #ilASM{ call :%opGetc } which is defined in VM\pfileioN.e.
--  Contrast with :%opFrame which is handled via AutoGlabel(). Note that calls to AutoGlabels (eg :%opConcatN) will be found/autoincluded
--  automatically by the inline assembler as needed, whereas AutoAsm labels (eg :%opGetc) may need explicit include statements, that is if
--  called manually from within a #ilASM statement rather than via the corresponding hll statement (and "" sometimes not, of course).
integer fno
    initialSymEntry(name,Stype,sig,opcode,sideeffects)
    fno = find(filename,binftab)
    if fno=0 then
        binftab = append(binftab,filename)
        fno = length(binftab)
    end if
    symtab[symlimit][S_FPno] = fno
    symtab[symlimit][S_State] = S_fwd+K_gbl
--  symtab[symlimit][S_State] = S_fwd+K_gbl+K_wdb
    tt_string(glabel,-3)
    aatidx[opcode] = ttidx
    aasydx[opcode] = symlimit
    aartypes[opcode] = rtype
end procedure

procedure AutoGlabel(integer opcode, string glabel, sequence filename)
-- Maps an internal feature to a global label, eg opFrame wants the equivalent 
--  of #ilASM{ call :%opFrame } which is defined in VM\pStack.e (which in turn
--  needs VM\pHeap.e). :%opRetf (always rqd) is also defined in VM\pStack.e.
-- Note that there is /no/ corresponding symtab entry. 
-- Contrast with getc()/:%opGetc which is handled via AutoAsm().
    integer fno = find(filename,agfiles)
    if fno=0 then
        agfiles = append(agfiles,filename)
        agfdone = append(agfdone,-1)
        fno = length(agfiles)
--      if opcode=opRetf then                   -- (this one is a must have!)
--printf(1,"opRetf = %d\n",fno)
--          agfdone[fno] = 0
--      end if
    end if
    tt_string(glabel,-3)
    agtidx = append(agtidx,ttidx)
    agfnos = append(agfnos,fno)
    aatidx[opcode] = ttidx
--if opcode=opSubse1 then
--  ?{"opSubse1(AutoGlabel):",opSubse1,ttidx,aatidx[opcode]}
--end if
--  if opcode=opRetf then
--      printf(1,"opRetf = %d\n",length(agtidx))    -- 3
--  end if
end procedure

integer IAEType
integer do6
        do6 = 0
global sequence sq6         -- sqAble but for opJcc 
    sq6 = repeat(0,6)

-- Warning: sideeffects (last parameter) is not actually used, or rather
--          it gets clobbered when the real thing is actually compiled.
--          (No measurable gain would be had from removing it, and you
--           never know, it might come in useful for something one day.)

procedure initialAutoEntry(sequence name, integer Stype, sequence sig, sequence filename, integer opcode, sideeffects, minparm=-1)
integer void, fno
--DEV MARKTYPES/S_Type?
    if Stype=S_Func
    or Stype=S_Type then
        void = newTempVar(IAEType,FuncRes) -- allocate result var
--DEV can be S_Init for all IAEType=T_integer?
    end if
    initialSymEntry(name,Stype,sig,0,sideeffects)
    fno = find(filename,binftab)
    if fno=0 then
        binftab = append(binftab,filename)
        fno = length(binftab)
    end if
    symtab[symlimit][S_FPno] = fno
--  symtab[symlimit][S_State] = S_fwd+K_gbl
    symtab[symlimit][S_State] = S_fwd+K_gbl+K_wdb
--DEV temp (need something better for eval?) [total failure anyway...]
--if repl then
--  symtab[symlimit][S_State] = S_fwd+K_gbl+K_wdb+S_used
--end if
    if opcode then
        if do6 then
            sq6[opcode-opJge+1] = symlimit
        else
            sqAble[opcode] = symlimit
        end if
    end if
    if minparm!=-1 then
        symtab[symlimit][S_ParmN] = minparm
    end if
end procedure

procedure reservedWord(integer ttidx, integer state=0)
-- (note that state:=or_bits(state,#1103) in addSymEntry,
--  however "and_bits(si[S_State],K_fun)=K_fun" holds)
integer N
    N = addSymEntry(ttidx, 1, S_Rsvd, 0, 0, state)
end procedure

--constant asmfileio = 1
--constant hllfileio = 0    -- NB this has not been tried in a LONG time! (I'm going to nick it for ilASM/pfileio3.e)
    -- Only set ONE of the above!
--  if asmfileio=hllfileio then ?9/0 end if
--if newEmit then --DEV (kill both hllfileio and asmfileio)
--  if hllfileio then ?9/0 end if
--end if

--DEV agfiles
with trace
global integer gb_fwd = 0
global function getBuiltin()
integer state, fno
--puts(1,"getBuiltin() ")
    gb_fwd = 0
    fno = find(0,agfdone)
    if fno!=0 then
--?9/0
        -- Note: VM\pStack.e may be required for #ilASM{ call :%opRetf } or [hll] abort(0);
        --       in which case getBuiltin() may return "VM\pStack.e" twice, no big deal.
        --       Feel free to check binftab, scan down symtab, and reset builtinsReferenced
        --       in a similar fashion to the code below, if it floats your boat, that is.
        --       In which case might I suggest that you might achieve even better results 
        --       by adding agfdone-style handling to binftab. [DEV me very busy right now]
        agfdone[fno] = 1
--      if find(0,afgfdone)=0 then
--          builtinsReferenced = 0
--          ...
--              builtinsReferenced = 1
--          ...
--      end if
        return agfiles[fno]
    end if
    for i=T_Bin to T_Ainc do
        state = symtab[i][S_State]
        if and_bits(state,S_fwd_and_used)=S_fwd_and_used then
            gb_fwd = i
if newEmit then
            if i<=T_Asm then
                state -= S_fwd
                symtab[i][S_State] = state
            end if
end if
            builtinsReferenced = 0
            fno = symtab[i][S_FPno]
            for j=i+1 to T_Ainc do
                if symtab[j][S_FPno]!=fno then
                    state = symtab[j][S_State]
                    if and_bits(state,S_fwd_and_used)=S_fwd_and_used then
                        builtinsReferenced = 1
                        exit
                    end if
                end if
            end for
--/* no help...
            if repl then
                for j=T_Asm+1 to T_Ainc do
                    if symtab[j][S_FPno]=fno then
                        state = symtab[j][S_State]
                        if and_bits(state,S_fwd_and_used)=S_fwd then  -- (not used)
                            state += S_used
                            symtab[j][S_State] = state
                        end if
                    end if
                end for
            end if
--*/
--?binftab[fno]
            return binftab[fno]
        end if
    end for
    -- maybe an include defined more than one builtin?
--?"\"\""
    return ""
end function

global function getBuiltinName(integer routineNo)
-- used for auto-include warnings
--2/6/15:
--  return binftab[symtab[routineNo][S_FPno]]
integer state = symtab[routineNo][S_State]
integer fpno = symtab[routineNo][S_FPno]
    if and_bits(state,S_fwd) then
        return binftab[fpno]
    else
        return filenames[fpno][2]
    end if
end function
r_getBuiltinName = routine_id("getBuiltinName") -- (for ptok.e)

sequence aliases, alittxs
procedure Alias(sequence name, integer symidx)
    tt_string(name,-2)
    tt[ttidx+EQ] = symidx
    aliases = append(aliases,symidx)
    alittxs = append(alittxs,ttidx)
end procedure

--!/*
global procedure UnAlias(integer symidx)
-- reverse the effect of any Alias(), before we symtab[symidx] := 0
integer k = find(symidx,aliases)
    if k!=0 then
        tt[alittxs[k]+EQ] = 0
    end if
end procedure
--!*/

global procedure UnAliasAll()
-- invoked from the start of finalfixups2() [hmmm/DEV??]
    for i=1 to length(aliases) do
        tt[alittxs[i]+EQ] = 0
    end for
end procedure

global procedure Alias_C_flags()
--
-- Complete the last few C_XXX constants, after any DoFormat(), 
--  otherwise this is logically part of syminit() [below].
--
    integer wasttidx = ttidx
    --
    -- We already have these [using Z_int simply 'cos T_int already got used for #ilASM{}]:
    --
    --  initialConstant("C_INT",    #01000004)  Z_int = symlimit    -- a 32 bit signed integer
    --  initialConstant("C_UINT",   #02000004)  T_uint = symlimit   -- a 32 bit unsigned integer
    --  initialConstant("C_INT64",  #01000008)  T_int64 = symlimit  -- a 64 bit signed integer
    --  initialConstant("C_QWORD",  #02000008)  T_uint64 = symlimit -- a 64 bit unsigned integer
    --
    -- As per cffi.e, a long is 32-bits on 64-bit windows, but 64 on 64-bit linux:
    integer long  = iff(X64=0 or PE?Z_int:T_int64),
            ulong = iff(X64=0 or PE?T_uint:T_uint64),
            ptr   = iff(X64=0      ?T_uint:T_uint64)

    Alias("C_LONG",long)
    Alias("C_ULONG",ulong)
    Alias("C_SIZE_T",ulong)

    Alias("C_PTR",ptr)
    Alias("C_POINTER",ptr)
    Alias("C_WPARAM",ptr)
    Alias("C_LPARAM",ptr)
    Alias("C_HRESULT",ptr)
    Alias("C_HANDLE",ptr)
    Alias("C_HWND",ptr)

    ttidx = wasttidx
end procedure


--      k = addSymEntry(ttidx,0,S_Nspc,prevfile,0,0)
procedure defaultNamespace(sequence name)
-- for adding the "eu" and "phix" default namespaces
--integer k
integer slink
sequence ssl    -- symtab[slink/symlimit]
    tt_string(name,-2)
--  k = addSymEntry(ttidx,0,S_Nspc,0,0,0)
    symlimit += 1
    slink = tt[ttidx+EQ]

    ssl = {ttidx,           -- S_Name[1]
           S_Nspc,          -- S_NTyp[2]
           0,               -- S_FPno[3] (sig)
           0,               -- S_State[4] (state)
           slink,           -- S_Nlink[5]
           0,               -- S_Slink[6] (scope)
           0}               -- S_nFno[7] (fileno)

    symtab[symlimit] = ssl
    tt[ttidx+EQ] = symlimit
end procedure

sequence hll_stubs

global function get_hll_stub(integer N)
    return hll_stubs[N]
end function

without trace
--with trace
global procedure syminit()

    symtab = repeat(0,2000)
    aliases = {}
    alittxs = {}

    if dumppsym then
        fnpsym = open("psym.txt","w")
    end if

    --
    -- Do the builtin types first of all.
    --
    -- Using a bitmask approach allows symmetrical treatment 
    --  of say "if integer(x)" and "if not integer(x)" and
    --  of course simplifies both the state transitions at
    --  "else"/"elsif" and tests for "probable logic error".
    --
    -- The dummy types cater for eg:
    --
    --      object x
    --      -- roottype of x is now object (0b1111)
    --      if integer(x) then
    --          -- roottype of x is now integer (0b0001)
    --      else
    --          -- roottype of x is now "not integer" (0b1110)
    --          if string(x) then
    --              -- roottype of x is now string (0b1000)
    --          else
    --              -- roottype of x is now "not integer and not string" (0b0110)
    --          end if
    --      end if
    --      -- roottype of x must now revert to object (0b1111)
    --
    symlimit = 0                                                -- #00 / 0b0000   dummy type: unknown
    initialSymEntry("integer",  S_Type,"TI",opInt,  E_none)     -- #01 / 0b0001 integer
    initialSymEntry(-1,         S_Type,"T", -1,     E_none)     -- #02 / 0b0010   dummy type: flt (atom but not integer)
    initialSymEntry("atom",     S_Type,"TN",opAtom, E_none)     -- #03 / 0b0011 atom (ie flt|int)
    initialSymEntry(-1,         S_Type,"T", -1,     E_none)     -- #04 / 0b0100   dummy type: dseq (sequence but not string)
    initialSymEntry(-1,         S_Type,"T", -1,     E_none)     -- #05 / 0b0101   dummy type: dseq|int
    initialSymEntry(-1,         S_Type,"T", -1,     E_none)     -- #06 / 0b0110   dummy type: dseq|flt
    initialSymEntry(-1,         S_Type,"T", -1,     E_none)     -- #07 / 0b0111   dummy type: dseq|flt|int (not string)
    initialSymEntry("string",   S_Type,"TS",opStr,  E_none)     -- #08 / 0b1000 string
    initialSymEntry(-1,         S_Type,"T", -1,     E_none)     -- #09 / 0b1001   dummy type: str|int
    initialSymEntry(-1,         S_Type,"T", -1,     E_none)     -- #0A / 0b1010   dummy type: str|flt
    initialSymEntry(-1,         S_Type,"T", -1,     E_none)     -- #0B / 0b1011   dummy type: str|flt|int
    initialSymEntry("sequence", S_Type,"TP",opSq,   E_none)     -- #0C / 0b1100 sequence (ie dseq|str)
    initialSymEntry(-1,         S_Type,"T", -1,     E_none)     -- #0D / 0b1101   dummy type: dseq|str|int
    initialSymEntry(-1,         S_Type,"T", -1,     E_none)     -- #0E / 0b1110   dummy type: dseq|str|flt
    initialSymEntry("object",   S_Type,"TO",opObj,  E_none)     -- #0F / 0b1111 object (ie dseq|str|flt|int)

--  symlimit += 1                                   -- [16] directory table (T_pathset)
--  symlimit += 1                                   -- [17] file table (T_fileset)
--  symlimit += 1                                   -- [18] namespace link (T_nslink) (an integer)
--  symlimit += 1                                   -- [19] flag for command_line() (T_cmdlnflg)
--  symlimit += 1                                   -- [20] T_callstk (see pdiag.e/pdebug.e)

--  symlimit += 1                                   -- [21] T_maintls
--  symlimit += 6
--  symlimit += 1                                   -- [22] T_EBP (now spare)
    symlimit += 7
                    -- {name, type, file, state, link, scope, sig, par1/N/L, il, ltab, 1stl,s_efct}:
--  if newEBP then
        symtab[T_maintls] = {-1,                -- S_Name[1]
                             S_Proc,            -- S_NTyp[2]
                             1,                 -- S_FPno[3]
                             K_wdb+K_ran,       -- S_State[4]
                             0,                 -- S_Nlink[5]
                             0,                 -- S_Slink[6]
                             {'P'},             -- S_sig[7]
                             0,                 -- S_Parm1[8]
                             0,                 -- S_ParmN[9]
                             0,                 -- S_Ltot[10]
                             {},                -- S_il[11]
                             0,                 -- S_ltab[12]
                             1,                 -- S_1stl[13]
                             0}                 -- S_Efct[14]
--  else
--      symtab[T_maintls] = {-1,                -- S_Name[1]
--                           S_Proc,            -- S_NTyp[2]
--                           1,                 -- S_FPno[3]
--                           K_wdb+K_ran,       -- S_State[4]
--                           0,                 -- S_Nlink[5]
--                           0,                 -- S_Slink[6]
--                           {'P'},             -- S_sig[7]
--                           -1,                -- S_Parm1[8]
--                           0,                 -- S_ParmN[9]
--                           0,                 -- S_Ltot[10]
--                           {},                -- S_il[11]
--                           0,                 -- S_ltab[12]
--                           1,                 -- S_1stl[13]
--                           0}                 -- S_Efct[14]
--  end if
    -- _top_level_sub_
    currtls = T_maintls

    localscopes = {}
    scopetypes = {}
    scopefiles = {}
    scopelines = {}
    scopecols = {}
    scopeemits = {}
    scopelasts = {}
    scopetls = {}
    optsets = {}
    scopelevel = 0
--25/2/10: leave space for a proper T_Dsq header at the start of ds:
--  TIDX = 0
    TIDX = -5

    freetmplists = repeat(0,T_object)   -- 15 free list pointers

--DEV give these names of "-1", "0", "1"...
--  symlimit = addUnnamedConstant(-1,T_integer) -- [23] a -1 (T_constm1, used eg in DoSubScripts)
--  symlimit = addUnnamedConstant(0,T_integer)  -- [24] a 0 (T_const0, used eg in pemit.e)
--  symlimit = addUnnamedConstant(1,T_integer)  -- [25] a 1 (T_const1, used eg in DoFor and DoQu)
-- (untried)
    symlimit += 1                                   -- [23] T_ds4
    initialConstant("-1",-1)                        -- [24]
    initialConstant("0",0)                          -- [25] T_const0
    initialConstant("1",1)                          -- [26] T_const1

--maybe?:
--  symlimit = addUnnamedConstant("",T_string)
--  symlimit = addUnnamedConstant({},T_sequence)
--  symlimit = addUnnamedConstant({},T_Dsq)


    currtls = increaseScope(S_File,0)
    if currtls!=T_maintls then currtls = 9/0 end if

-- 10/12/2011
    defaultNamespace("eu")
    defaultNamespace("phix")

    -- from dll.e:
--DEV 64 bit?
--DEV doc
    initialConstant("C_BYTE",       #01000001)  -- an 8 bit signed integer
--  initialConstant("C_CHAR",       #01000001)  -- an 8 bit signed?? integer
    Alias("C_CHAR",symlimit)
    initialConstant("C_UBYTE",      #02000001)  -- an 8 bit unsigned integer
--  initialConstant("C_UCHAR",      #02000001)  -- an 8 bit unsigned?? integer
    Alias("C_UCHAR",symlimit)
    initialConstant("C_SHORT",      #01000002)  -- a 16 bit signed integer
--  initialConstant("C_WORD",       #01000002)
    Alias("C_WORD",symlimit)
    initialConstant("C_USHORT",     #02000002)                      -- a 16 bit unsigned integer
    initialConstant("C_INT",        #01000004)  Z_int = symlimit    -- a 32 bit signed integer
    initialConstant("C_UINT",       #02000004)  T_uint = symlimit   -- a 32 bit unsigned integer
    initialConstant("C_INT64",      #01000008)  T_int64 = symlimit  -- a 64 bit signed integer
    initialConstant("C_QWORD",      #02000008)  T_uint64 = symlimit -- a 64 bit unsigned integer
    Alias("C_BOOL",Z_int)
    Alias("C_DWORD",T_uint)
--25/1/20 replaced with Alias_C_flags():
--/*
    integer long  = iff(machine_bits()=32 or platform()=WINDOWS?#01000004:#01000008),
            ulong = iff(machine_bits()=32 or platform()=WINDOWS?#02000004:#02000008)
    initialConstant("C_LONG",       long)   T_long = symlimit   -- see also pmain.e/DoFormatHere() [27/5/19] [37]
    initialConstant("C_ULONG",      ulong)  T_ulong = symlimit  -- see also pmain.e/DoFormatHere() [27/5/19]
    Alias("C_SIZE_T",symlimit)
--?{"syminit() line 1626, long:",long,"PE",PE,"X64",X64,"Z64",Z64}
?{"syminit() line 1626, long:",long,"PE",PE,"X64",X64}
--??
--27/5/19:
--  initialConstant("C_WPARAM",     #01000004)
--  initialConstant("C_LPARAM",     #01000004)
--  initialConstant("C_HRESULT",    #01000004)
--  initialConstant("C_SIZE_T",     #02000004)
--  initialConstant("C_POINTER",    #02000004)
    integer ptr = iff(machine_bits()=32?#02000004:#02000008)
    initialConstant("C_PTR",        ptr)    T_ptr = symlimit    -- see also pmain.e/DoFormatHere() [27/5/19] [39]
--  [DEV] #01000008 on 64 bit
--  initialConstant("C_PTR",        #02000004)  -- ""
    Alias("C_POINTER",symlimit)
    Alias("C_WPARAM",symlimit)
    Alias("C_LPARAM",symlimit)
    Alias("C_HRESULT",symlimit)
    Alias("C_HANDLE",symlimit)
    Alias("C_HWND",symlimit)
--  initialConstant("C_HANDLE",     #02000004)  -- ""
--  initialConstant("C_HWND",       #02000004)  -- ""
--*/
--  initialConstant("C_WIDEPTR",    #02020004) [DEV/SUG]
    initialConstant("C_FLOAT",      #03000004)  -- a 32-bit float
    initialConstant("C_DOUBLE",     #03000008)  -- a 64-bit float
--  initialConstant("C_FLT32",      #03000004)  -- (DEV/SUG)
--  initialConstant("C_FLT64",      #03000008)  -- (DEV/SUG)
--  initialConstant("C_FLT80",      #0300000A)  -- (DEV/SUG)??
--  initialConstant("C_DWORDLONG",  #03000008)  [DEV? (has also surfaced as #03000002, I say "it shd be #02000008!?")]

--  signedmbi = iff(X64=0?#01000004:#01000008)
--  unsignmbi = iff(X64=0?#02000004:#02000008)
--  initialConstant("C_???",        signedmbi)  -- a signed machine_bit integer [NO]
--  reset_on_format(symlimit) [or just document they may be incorrect during cross-compile?]


-- For Phix-compiled .dll files [DEV when implemented!], and/or call_backs.
--  (Instead of the four E_XXX values below. See ??[DEV] for an example of
--   interfacing with a (closed-source) RDS-Eu-compiled .dll)
--  initialConstant("P_REF?",       #1F000004)?
--  initialConstant("P_INTEGER",    #11000004)
--  initialConstant("P_ATOM",       #13000004)
--  initialConstant("P_STRING",     #18000004)
--  initialConstant("P_SEQUENCE",   #1C000004)
--  initialConstant("P_OBJECT",     #1F000004)
--SUG:
--  implement the following memory management:
--      local object thing = dll:getthing()
--          ...
--          thing = dll:deallocate(thing)   -- (returns 0)
--  or, if you want better type handling:
--      local object tmp,
--            string thing = dll:getstring()
--          ...
--          tmp = thing
--          thing = ""
--          tmp = dll:deallocate(tmp)       -- (returns 0)
-- Explanation: because tmp(/thing) is local, it uses automatic pass by reference processing, with
--  tmp being left set to h4 (<unassigned>) over the call, and the parameter therefore avoids any
--  copy-on-write semantics, and gets deallocated properly by the originating heap manager.
--  (the dll: notation I just used is almost certainly some kind of shorthand for c_func calls)
-- The alternative is to have the app and the dll share the same heap manager, which means implementing
--  it in another separate dll, but we all know about the sticky mess that will end up causing. Maybe
--  we could implement it via COM, and watch "hello world" appear after just eight minutes or so...
--  (oh dear COM, we could have loved you so much, if only you hadn't eaten all that VB/A/S...)
--

-- These are for RDS-Eu-compiled .dll files: [DEV]
--  (The backend contains code to handled a *returned* E_SEQUENCE,
--   but nothing to xlate Phix refs to Eu-compatible parameters.
--   Ideally such .dll files should be recompiled using Phix and
--   P_REF, when Phix gets a -dll command line option that is..)
-- Erm, I'm never going to be able to properly support these 26/7/14
--  initialConstant("E_INTEGER",    #06000004)
--  initialConstant("E_ATOM",       #07000004)
--  initialConstant("E_SEQUENCE",   #08000004)
--  initialConstant("E_OBJECT",     #09000004)

    initialConstant("DLL_PROCESS_ATTACH", 1)
    initialConstant("DLL_PROCESS_DETACH", 0)
    initialConstant("DLL_THREAD_ATTACH",  2)
    initialConstant("DLL_THREAD_DETACH",  3)


    Alias("bool",T_integer)
    initialConstant("NULL", 0)
    Alias("null", symlimit)
    initialConstant("TRUE", 1)
    Alias("True", symlimit)
    Alias("true", symlimit)
    initialConstant("FALSE", 0)
    Alias("False", symlimit)
    Alias("false", symlimit)
    if ORAC then
        Alias("int",T_integer)
        Alias("seq",T_sequence)
    end if
    Alias("number",T_atom)

    -- from misc.e: (must match table in builtins/get_interpreter.e/initv())
    initialConstant("DOS32", 1)                             -- ex.exe (not supported!)
    initialConstant("WINDOWS", 2)   T_win32 = symlimit      -- exw.exe
    Alias("WIN32",symlimit) -- (deprecated, does not mean 32-bit)
    initialConstant("LINUX", 3)     T_linux = symlimit      -- exu      ("")
--  initialConstant("FREEBSD", 3)                           -- exu      ("")
--  initialConstant("OSX", 5)       -- ??
--20/10/2020
    initialConstant("WEB", 4)
    Alias("JS",symlimit)
    Alias("JAVASCRIPT",symlimit)

--DEV 64 bit? fldpi? E?
--/* (untried)  DOH: [32] is WRONG! ...  It would be X64... and x8/x10/something else, but NOT atom...
atom pi, inf, nan
    #ilASM{
            fldpi
        [32]
            lea edi,[pi]
        [64]
            lea rdi,[pi]
        []
            call :%StoreFlt }
    initialConstant("PI",pi)
    #ilASM{ fld1
            fldz
            fdivp
        [32]
            lea edi,[inf]
        [64]
            lea rdi,[inf]
        []
            call :%pStoreFlt }
    initialConstant("INF",inf)
    nan = -(inf/inf)
    initialConstant("NAN",nan)
--*/
    initialConstant("PI",3.141592653589793238)
    initialConstant("E",2.7182818284590452)
    initialConstant("INVLN10",0.43429448190325182765)
--  initialConstant("INVLN2",1.44269504088896340739)
--sug:  Phi (the golden ratio, = 1.618033988749895)

    -- from file.e:
    initialConstant("D_NAME",           1)
    initialConstant("D_ATTRIBUTES",     2)
    initialConstant("D_SIZE",           3)
    initialConstant("D_YEAR",           4)
    initialConstant("D_MONTH",          5)
    initialConstant("D_DAY",            6)
    initialConstant("D_HOUR",           7)
    initialConstant("D_MINUTE",         8)
    initialConstant("D_SECOND",         9)
    initialConstant("D_CREATION",       1)
    initialConstant("D_LASTACCESS",     2)
    initialConstant("D_MODIFICATION",   3)
    initialConstant("LOCK_SHARED",      1)
    initialConstant("LOCK_EXCLUSIVE",   2)  -- default
    initialConstant("LOCK_WAIT",        4)
    initialConstant("W_BAD_PATH",      -1)
    -- the following is re-calculated/overwritten in DoFormat():
    tt_string("SLASH",-2)
    T_SLASH = addSymEntry(ttidx,1,T_integer,S_Const,0,K_litnoclr)
    symtab[T_SLASH][S_value] = iff(platform()=WINDOWS?'\\':'/')

    -- from pfile.e:
    initialConstant("FILETYPE_UNDEFINED", -1)
    initialConstant("FILETYPE_NOT_FOUND",  0)
    initialConstant("FILETYPE_FILE",       1)
    initialConstant("FILETYPE_DIRECTORY",  2)
    -- (note that linux only uses DRIVE_FIXED)
    initialConstant("DRIVE_UNKNOWN",     0) -- The drive type cannot be determined.
    initialConstant("DRIVE_NO_ROOT_DIR", 1) -- The root path is invalid; for example, there is no volume mounted at the specified path.
    initialConstant("DRIVE_REMOVABLE",   2) -- The drive has removable media; for example, a floppy drive, thumb drive, or flash card reader.
    initialConstant("DRIVE_FIXED",       3) -- The drive has fixed media; for example, a hard disk drive or flash drive.
    initialConstant("DRIVE_REMOTE",      4) -- The drive is a remote (network) drive.
    initialConstant("DRIVE_CDROM",       5) -- The drive is a CD-ROM drive.
    initialConstant("DRIVE_RAMDISK",     6) -- The drive is a RAM disk.

    -- from pdate.e:
    initialConstant("DT_YEAR",          1)
    initialConstant("DT_MONTH",         2)
    initialConstant("DT_DAY",           3)
    initialConstant("DT_HOUR",          4)
    initialConstant("DT_MINUTE",        5)
    initialConstant("DT_SECOND",        6)
    initialConstant("DT_DOW",           7)
    initialConstant("DT_MSEC",          7)
    initialConstant("DT_DOY",           8)
    initialConstant("DT_GMT",          -1)

--  -- from get.e:
    initialConstant("GET_SUCCESS",      0)
    initialConstant("GET_EOF",         -1)
    initialConstant("GET_IGNORE",      -2)
    initialConstant("GET_FAIL",         1)

    -- from graphics.e:
    initialConstant("BLACK",            0)
    initialConstant("GREEN",            2)
    initialConstant("MAGENTA",          5)
    initialConstant("WHITE",            7)
    initialConstant("GRAY",             8)
    initialConstant("BRIGHT_GREEN",     10)
    initialConstant("BRIGHT_MAGENTA",   13)
    initialConstant("BRIGHT_WHITE",     15)
--  if platform() = LINUX then
--  initialConstant("BLUE",             4)
--  initialConstant("CYAN",             6)
--  initialConstant("RED",              1)
--  initialConstant("BROWN",            3)
--  initialConstant("BRIGHT_BLUE",      12)
--  initialConstant("BRIGHT_CYAN",      14)
--  initialConstant("BRIGHT_RED",       9)
--  initialConstant("YELLOW",           11)
--  else
    --DEV it would probably be better to do this mapping in text_color/bk_color()...
    initialConstant("BLUE",             iff(platform()=WINDOWS?1:4))
    initialConstant("CYAN",             iff(platform()=WINDOWS?3:6))
    initialConstant("RED",              iff(platform()=WINDOWS?4:1))
    initialConstant("BROWN",            iff(platform()=WINDOWS?6:3))
    initialConstant("BRIGHT_BLUE",      iff(platform()=WINDOWS?9:12))
    initialConstant("BRIGHT_CYAN",      iff(platform()=WINDOWS?11:14))
    initialConstant("BRIGHT_RED",       iff(platform()=WINDOWS?12:9))
    initialConstant("YELLOW",           iff(platform()=WINDOWS?14:11))
--  end if
    initialConstant("VC_COLOR",         1)
    initialConstant("VC_MODE",          2)
    initialConstant("VC_LINES",         3)
    initialConstant("VC_COLUMNS",       4)
    initialConstant("VC_XPIXELS",       5)
    initialConstant("VC_YPIXELS",       6)
    initialConstant("VC_NCOLORS",       7)
    initialConstant("VC_PAGES",         8)
    initialConstant("VC_SCRNLINES",     9)
    initialConstant("VC_SCRNCOLS",      10)
    initialConstant("BLINKING",         16)

    initialConstant("NO_CURSOR",                #2000)
    initialConstant("UNDERLINE_CURSOR",         #0607)
    initialConstant("THICK_UNDERLINE_CURSOR",   #0507)
    initialConstant("HALF_BLOCK_CURSOR",        #0407)
    initialConstant("BLOCK_CURSOR",             #0007)

    -- from image.e:
    initialConstant("BMP_SUCCESS",              0)
    initialConstant("BMP_OPEN_FAILED",          1)
    initialConstant("BMP_UNEXPECTED_EOF",       2)
    initialConstant("BMP_UNSUPPORTED_FORMAT",   3)
    initialConstant("BMP_INVALID_MODE",         4)

    -- from database.e:
    initialConstant("DB_OK",                 0)
    initialConstant("DB_OPEN_FAIL",         -1)
    initialConstant("DB_EXISTS_ALREADY",    -2)
    initialConstant("DB_LOCK_FAIL",         -3)
    initialConstant("DB_BAD_NAME",          -4)
    initialConstant("DB_FATAL_FAIL",      -404)
    initialConstant("DB_LOCK_NO",            0)
    initialConstant("DB_LOCK_SHARED",        1)
    initialConstant("DB_LOCK_EXCLUSIVE",     2)
    initialConstant("DB_LOCK_READ_ONLY",     3)

    -- from ppp.e:
    initialConstant("pp_File",       1)
    initialConstant("pp_Maxlen",     2)
    initialConstant("pp_Pause",      3)
    initialConstant("pp_StrFmt",     4)
    initialConstant("pp_IntFmt",     5)
    initialConstant("pp_FltFmt",     6)
    initialConstant("pp_Nest",       7)
    initialConstant("pp_Ascii",      8)
    initialConstant("pp_Date",       9)
    initialConstant("pp_Brkt",      10)
    initialConstant("pp_Indent",    11)
    initialConstant("pp_Q22",       12)
    initialConstant("pp_IntCh",     13)

    -- from msgbox.e:
    initialConstant("MB_OK",                        #00)    -- Message box contains one push button: OK
    initialConstant("MB_OKCANCEL",                  #01)    -- Message box contains OK and Cancel
    initialConstant("MB_ABORTRETRYIGNORE",          #02)    -- Abort, Retry, Ignore
    initialConstant("MB_YESNOCANCEL",               #03)    -- Message box contains Yes, No, and Cancel
    initialConstant("MB_YESNO",                     #04)    -- Message box contains Yes and No
    initialConstant("MB_RETRYCANCEL",               #05)    -- Message box contains Retry and Cancel
    -- nb precise appearance dependant on windows version and theme.
    initialConstant("MB_ICONHAND",                  #10)    -- A hand appears
    initialConstant("MB_ICONERROR",                 #10)    -- (red circle with white X in it)
    initialConstant("MB_ICONSTOP",                  #10)    -- = MB_ICONHAND
    initialConstant("MB_ICONQUESTION",              #20)    -- (white speech with blue ? in it)
    initialConstant("MB_ICONEXCLAMATION",           #30)    -- (yellow triangle with black ! in it)
    initialConstant("MB_ICONWARNING",               #30)    -- = MB_ICONEXCLAMATION
    initialConstant("MB_ICONINFORMATION",           #40)    -- Lowercase letter i in a circle appears
    initialConstant("MB_ICONASTERISK",              #40)    -- (white speech with blue i in it)
    initialConstant("MB_APPLMODAL",                 #00)    -- User must respond before doing something else
    initialConstant("MB_DEFBUTTON1",               #000)    -- First button is default button
    initialConstant("MB_DEFBUTTON2",               #100)    -- Second button is default button
    initialConstant("MB_DEFBUTTON3",               #200)    -- Third button is default button
    initialConstant("MB_DEFBUTTON4",               #300)    -- Fourth button is default button
    initialConstant("MB_SYSTEMMODAL",             #1000)    -- All applications suspended until user responds
    initialConstant("MB_TASKMODAL",               #2000)    -- Similar to MB_APPLMODAL 
    initialConstant("MB_USERICON",                  #80)    -- (see MSGBOXPARAMS)
    initialConstant("MB_HELP",                    #4000)    -- Windows 95: Help button generates help event
    initialConstant("MB_SETFOREGROUND",          #10000)    -- Message box becomes the foreground window 
    initialConstant("MB_DEFAULT_DESKTOP_ONLY",   #20000)
    initialConstant("MB_TOPMOST",                #40000)    -- create with the WS_EX_TOPMOST style
    initialConstant("MB_RIGHT",                  #80000)    -- Windows 95: The text is right-justified
    initialConstant("MB_RTLREADING",            #100000)    -- Windows 95: For Hebrew and Arabic systems
    initialConstant("MB_SERVICE_NOTIFICATION",  #200000)    -- Windows NT: The caller is a service 
    initialConstant("IDOK",         1)  -- OK button was selected.
    initialConstant("IDCANCEL",     2)  -- Cancel button was selected.
    initialConstant("IDABORT",      3)  -- Abort button was selected.
    initialConstant("IDRETRY",      4)  -- Retry button was selected.
    initialConstant("IDIGNORE",     5)  -- Ignore button was selected.
    initialConstant("IDYES",        6)  -- Yes button was selected.
    initialConstant("IDNO",         7)  -- No button was selected.
    initialConstant("IDTRYAGAIN",   10) -- Try Again button was selected.
    initialConstant("IDCONTINUE",   11) -- Continue button was selected.
    -- from pfileio3.e[?]
    initialConstant("SEEK_OK",      0)

    -- for pfile.e/get_text():
    initialConstant("GT_WHOLE_FILE", 0)     -- as one string, plus '\n' if rqd
    initialConstant("GT_LF_STRIPPED",1)     -- '\n'-stripped lines
    initialConstant("GT_LF_LEFT",    2)     -- '\n' left on
    initialConstant("GT_LF_LAST",    4)     -- '\n' left on, plus '\n' if rqd
    initialConstant("GT_KEEP_BOM",   8)     -- do not strip utf8 bom
    initialConstant("GT_BINARY",   #10)     -- open in binary mode

    -- from VM/pThreadN.e:
    initialConstant("CREATE_SUSPENDED", #00000004)
    initialConstant("INFINITE",         #FFFFFFFF)
    initialConstant("WAIT_ABANDONED",   #00000080)
    initialConstant("WAIT_TIMEOUT",     #00000102)
    initialConstant("WAIT_OBJECT_0",    #00000000)
    initialConstant("WAIT_FAILED",      #FFFFFFFF)
    initialConstant("STILL_ACTIVE",     259)

    -- for canonical_path (compatibility only)
    initialConstant("CORRECT",2)

    -- from sort.e
    initialConstant("ASCENDING",-1)
    initialConstant("NORMAL_ORDER",-1)
    initialConstant("DESCENDING",+1)
    initialConstant("REVERSE_ORDER",+1)

    -- from pDiagN.e (throw)
    initialConstant("E_CODE",1)
    initialConstant("E_ADDR",2)
    initialConstant("E_LINE",3)
    initialConstant("E_RTN", 4)
    initialConstant("E_NAME",5)
    initialConstant("E_FILE",6)
    initialConstant("E_PATH",7)
    initialConstant("E_USER",8)

    -- from pqueue.e
    initialConstant("MIN_HEAP",-1)
    initialConstant("MAX_HEAP",+1)

    -- for hash.e
    initialConstant("HSIEH30",-6)

    -- for builtins/unit_test.e
    initialConstant("TEST_QUIET",0)             -- (summary only when fail)
    initialConstant("TEST_SUMMARY",1)           -- (summary only [/always])
    initialConstant("TEST_SHOW_FAILED",2)       -- (summary + failed tests)
    initialConstant("TEST_SHOW_ALL",3)          -- (summary + all tests)
    initialConstant("TEST_ABORT",1)             -- (abort on failure, at summary)
--  initialConstant("TEST_QUIET",0)             -- (carry on despite failure)
    initialConstant("TEST_CRASH",-1)            -- (crash on failure, immediately)
    initialConstant("TEST_PAUSE",1)             -- (always pause)
--  initialConstant("TEST_QUIET",0)             -- (never pause)
    initialConstant("TEST_PAUSE_FAIL",-1)       -- (pause on failure)


--if not newEmit then
--
--  -- the following builtins return an integer:
--
--  if asmfileio then
--      initialSymEntry("getc",         S_Func,"FI",    opGetc,     E_other)
--  end if
----trace(1)
--  initialSymEntry("equal",            S_Func,"FOO",   opSeq,      E_none)     T_equal = symlimit
--  initialSymEntry("compare",          S_Func,"FOO",   opScmp,     E_none)     T_compare = symlimit
--  initialSymEntry("find",             S_Func,"FOPI",  opFind,     E_none)     T_find = symlimit
--  symtab[symlimit][S_ParmN] = 2
----    Alias("find_from", T_find)  -- removed 19/8/15
--  initialSymEntry("match",            S_Func,"FPPI",  opMatch,    E_none)     T_match = symlimit
--  symtab[symlimit][S_ParmN] = 2
----    Alias("match_from", T_match)    -- removed 19/8/15
--  initialSymEntry("length",           S_Func,"FP",    opLen,      E_none)     T_length = symlimit
--  initialSymEntry("remainder",        S_Func,"FNN",   opRmdr,     E_none)
--  initialSymEntry("floor",            S_Func,"FN",    opFloor,    E_none)     T_floor = symlimit
--  if asmfileio then
--      initialSymEntry("lock_file",    S_Func,"FIIP",  opLock,     E_other)
--      initialSymEntry("open",         S_Func,"FPP",   opOpen,     E_other)
--  end if
--  initialSymEntry("get_key",          S_Func,"F",     opGetKey,   E_other)
--  initialSymEntry("wait_key",         S_Func,"F",     opWaitKey,  E_other)
--  if asmfileio then
--      initialSymEntry("seek",         S_Func,"FII",   opSeek,     E_other)
--      initialSymEntry("where",        S_Func,"FI",    opWhere,    E_none)
--  end if
--  initialSymEntry("rand",             S_Func,"FI",    opRand,     E_none)
--
----    T_Nres = symlimit
--
--  -- the following builtin functions return an atom:
--
--  initialSymEntry("allocate",         S_Func,"FI",    opAlloc,    E_none)     -- (not newEmit)
--  initialSymEntry("allocate_data",    S_Func,"FI",    opAlloc,    E_none)     -- (not newEmit)
----DEV/SUG: (erm... what happened to T_Nres etc? [StoreVar took over job, methinks])
----    newstyleSymEntry("allocate",        S_Func,"FI",    "pHeap.e",0,E_other)    -- (not newEmit)
--  initialSymEntry("and_bits",         S_Func,"FNN",   opAndBits,  E_none)
--  initialSymEntry("or_bits",          S_Func,"FNN",   opOrBits,   E_none)
--  initialSymEntry("xor_bits",         S_Func,"FNN",   opXorBits,  E_none)
--  initialSymEntry("not_bits",         S_Func,"FN",    opNotBits,  E_none)
--  -- assume busy loop if routine calls time
--  initialSymEntry("time",             S_Func,"F",     opTime,     E_other)
--  initialSymEntry("power",            S_Func,"FNN",   opPow,      E_none)
--  initialSymEntry("log",              S_Func,"FN",    opLog,      E_none)
--  initialSymEntry("float32_to_atom",  S_Func,"FP",    op32toA,    E_none)
--  initialSymEntry("float64_to_atom",  S_Func,"FP",    op64toA,    E_none)
----    initialSymEntry("chdir",            S_Func,"FP",    opChDir,    E_other)
--  initialSymEntry("instance",         S_Func,"F",     opInstance, E_none)
--  initialSymEntry("cos",              S_Func,"FN",    opCos,      E_none)
--  initialSymEntry("sin",              S_Func,"FN",    opSin,      E_none)
--  initialSymEntry("sqrt",             S_Func,"FN",    opSqrt,     E_none)
--  initialSymEntry("tan",              S_Func,"FN",    opTan,      E_none)
--  initialSymEntry("arctan",           S_Func,"FN",    opArcTan,   E_none)
--
----    T_Pres=symlimit
--
--  -- following builtin functions return a sequence
--
--  initialSymEntry("append",           S_Func,"FPO",   opApnd,     E_none)
--  initialSymEntry("prepend",          S_Func,"FPO",   opPpnd,     E_none)
--  initialSymEntry("repeat",           S_Func,"FOI",   opRepeat,   E_none)
--  initialSymEntry("atom_to_float32",  S_Func,"FN",    opAto32,    E_none)
--  initialSymEntry("atom_to_float64",  S_Func,"FN",    opAto64,    E_none)
----    initialSymEntry("date",             S_Func,"F",     opDate,     E_none) -- now in pdate.e
--  if asmfileio then
--      initialSymEntry("get_position", S_Func,"F",     opGetPos,   E_none)
--  end if
--
--  -- the following builtin functions return object
--
----    initialSymEntry("upper",            S_Func,"FO",    opUpper,    E_none) -- now in pcase.e
----    initialSymEntry("lower",            S_Func,"FO",    opLower,    E_none) -- ""
--  initialSymEntry("peek",             S_Func,"FO",    opPeek,     E_none)
--  initialSymEntry("peek4s",           S_Func,"FO",    opPeek4s,   E_none)
--  initialSymEntry("peek4u",           S_Func,"FO",    opPeek4u,   E_none)
--
----    T_Bfunc = symlimit
--
--  -- the remainder are builtin procedures
--
--  initialSymEntry("abort",            S_Proc,"PI",    opAbort,    E_other)                T_abort = symlimit
--  if asmfileio then
--      initialSymEntry("bk_color",     S_Proc,"PI",    opBkClr,    E_other)
--  end if
--  if asmfileio then
--      initialSymEntry("clear_screen", S_Proc,"P",     opClrScrn,  E_other)
--      initialSymEntry("close",        S_Proc,"PI",    opClose,    E_other)
--  end if
--  initialSymEntry("crash_file",       S_Proc,"PO",    opCrshFile, E_other)
--  initialSymEntry("crash_message",    S_Proc,"PO",    opCrshMsg,  E_other)
----    initialSymEntry("EnterCriticalSection",S_Proc,"PI", opEnterCS,  E_other)    T_ecs = symlimit
--  if asmfileio then
--      initialSymEntry("flush",        S_Proc,"PI",    opFlush,    E_other)
--  end if
--  initialSymEntry("free",             S_Proc,"PN",    opFree,     E_other)
--  if asmfileio then
--      initialSymEntry("free_console", S_Proc,"P",     opFreeCons, E_other)
--  end if
----    initialSymEntry("LeaveCriticalSection",S_Proc,"PI", opLeaveCS,  E_other)    T_lcs = symlimit
--  initialSymEntry("mem_copy",         S_Proc,"PNNI",  opMemCopy,  E_other)
--  initialSymEntry("mem_set",          S_Proc,"PNII",  opMemSet,   E_other)
----    initialSymEntry("pixel",            S_Proc,"POP",   opPixel,    E_other)    --DOS
--  initialSymEntry("poke",             S_Proc,"PIO",   opPoke,     E_other)
--  initialSymEntry("poke4",            S_Proc,"PIO",   opPoke4,    E_other)
--  if asmfileio then
--      initialSymEntry("position",     S_Proc,"PII",   opPosition, E_other)
--      initialSymEntry("puts",         S_Proc,"PIO",   opPuts,     E_other)
--  end if
----DEV
----    initialSymEntry("put_screen_char",  S_Proc,"PNNP",  opPSCh,     E_other)
--  initialSymEntry("set_rand",         S_Proc,"PI",    opSetRand,  E_none)
--  initialSymEntry("sleep",            S_Proc,"PN",    opSleep,    E_other)
--  initialSymEntry("trace",            S_Proc,"PI",    opTrace,    E_other)
--  if asmfileio then
--      initialSymEntry("text_color",   S_Proc,"PI",    opTxtClr,   E_other)
--  end if
--  initialSymEntry("profile",          S_Proc,"PI",    opProfile,  E_other)
--  initialSymEntry("unlock_file",      S_Proc,"PIP",   opUnLock,   E_other)
--else -- newEmit:

    -- optimised away at compile-time:
    initialSymEntry("platform",         S_Func,"F",     opPlatform, E_none)     T_platform = symlimit
    initialSymEntry("machine_bits",     S_Func,"F",     opMachine,  E_none)     T_machine_bits = symlimit
    initialSymEntry("machine_word",     S_Func,"F",     opMachine,  E_none)     T_machine_word = symlimit
    initialSymEntry("version",          S_Func,"F",     opVersion,  E_none)     Z_version = symlimit

--end if -- newEmit ends

    T_Bin = symlimit --DEV this needs stashing somewhere (symtab[1]?) or maybe T_Bfunc?

    binftab = {}
    agfiles = {}
    agfdone = {}
    agtidx = {}
    agfnos = {}

if newEmit then

    -- Note: some/most of these have no corresponding symtab entries
    --       (ie/eg 'append' has both an AutoGlabel here and an AutoAsm() below, but '&' [opConcat etc] is AutoGlabel only)

    AutoGlabel(opLoadMint,  "%pLoadMint",   "VM\\pHeap.e")
    AutoGlabel(opStoreMint, "%pStoreMint",  "VM\\pHeap.e")
    AutoGlabel(opDealloc,   "%pDealloc",    "VM\\pHeap.e")
    AutoGlabel(opInitCS,    "%pInitCS",     "VM\\pHeap.e")
    AutoGlabel(opFrame,     "%opFrame",     "VM\\pStack.e")
    AutoGlabel(opRetf,      "%opRetf",      "VM\\pStack.e")
    AutoGlabel(opCallOnce,  "%opCallOnce",  "VM\\pStack.e")
    AutoGlabel(opTchk,      "%opTchk",      "VM\\pStack.e")
    AutoGlabel(opTcFail,    "%opTcFail",    "VM\\pStack.e")
--DEV nope...
--  AutoGlabel(opGetSP,     "%pGetSymPtr",  "VM\\pStack.e")
--  AutoGlabel(opSetSP,     "%pSetSymPtr",  "VM\\pStack.e")
    AutoGlabel(opGetST,     "%opGetST",     "VM\\pStack.e")
    AutoGlabel(opRunCleanup,"%opRunCleanup","VM\\pStack.e")
    AutoGlabel(opAbort,     "%opAbort",     "VM\\pStack.e")
    AutoGlabel(opCrashMsg,  "%pCrashMsg",   "VM\\pDiagN.e")
    AutoGlabel(opCrashRtn,  "%pCrashRtn",   "VM\\pDiagN.e")
    AutoGlabel(opTrace,     "%opTrace",     "VM\\pTrace.e")
    AutoGlabel(opLnt,       "%opLnt",       "VM\\pTrace.e")
    AutoGlabel(opClrDbg,    "%opClrDbg",    "VM\\pTrace.e")
    AutoGlabel(opSubse,     "%pSubse",      "VM\\pSubseN.e")
    AutoGlabel(opSubse1,    "%pSubse1",     "VM\\pSubseN.e")
    AutoGlabel(opSubse1i,   "%pSubse1i",    "VM\\pSubseN.e")
    AutoGlabel(opSubse1ip,  "%pSubse1ip",   "VM\\pSubseN.e")
    AutoGlabel(opSubse1is,  "%pSubse1is",   "VM\\pSubseN.e")
    AutoGlabel(opSubss,     "%pSubss",      "VM\\pSubssN.e")
    AutoGlabel(opSubsss,    "%pSubsss",     "VM\\pSubssN.e")
    AutoGlabel(opRepe,      "%pRepe",       "VM\\pRepeN.e")
    AutoGlabel(opRepe1,     "%pRepe1",      "VM\\pRepeN.e")
    AutoGlabel(opRepe1ip,   "%pRepe1ip",    "VM\\pRepeN.e")
    AutoGlabel(opRepe1is,   "%pRepe1is",    "VM\\pRepeN.e")
    AutoGlabel(opDeSeq,     "%pDeSeqip",    "VM\\pRepeN.e")
    AutoGlabel(opReps,      "%pReps",       "VM\\pRepsN.e")
    AutoGlabel(opMkSq,      "%pMkSq",       "VM\\pMkSqN.e")
--  AutoGlabel(opDiv0,      "%e02atdb0",    "VM\\pDiagN.e")
--dev (TEMP)
--  AutoGlabel(opUnassigned,"%pUnassigned", "VM\\pDiagN.e")
    AutoGlabel(opUnassigned,"%pUnassigned", "VM\\pUnassigned.e")
    AutoGlabel(opBadRetf,   "%pBadRetf",    "VM\\pUnassigned.e")
    AutoGlabel(opCallOnceYeNot, "!opCallOnceYeNot", "VM\\pUnassigned.e")
    AutoGlabel(opAddiii,    "%e01tcfAddiii","VM\\pUnassigned.e")
--  AutoGlabel(opAddiii,    "%pAddiii",     "VM\\pUnassigned.e")
    AutoGlabel(opDiviii,    "%e01tcfediDiv","VM\\pUnassigned.e")
    AutoGlabel(opDiv0,      "%pDiv0",       "VM\\pUnassigned.e")
--  AutoGlabel(opDivf2,     "%e02atdb0",    "VM\\pUnassigned.e")    -- erm, actually the above should cope...
    AutoGlabel(opRTErn,     "%pRTErn",      "VM\\pUnassigned.e")
--  AutoGlabel(opRTErf,     "%pRTErf",      "VM\\pUnassigned.e")
    AutoGlabel(opDivi2,     "%e01tcfDivi2", "VM\\pUnassigned.e")
    AutoGlabel(opMuliii,    "%e01tcfediMul","VM\\pUnassigned.e")
    AutoGlabel(opApnd,      "%opApnd",      "VM\\pApnd.e")
--  AutoGlabel(opPpnd,      "%opApnd",      "VM\\pApnd.e")  -- (same as opApnd)
    AutoGlabel(opConcat,    "%opConcat",    "VM\\pApnd.e")
    AutoGlabel(opCatsi,     "%opCatsi",     "VM\\pApnd.e")
    AutoGlabel(opConcatN,   "%opConcatN",   "VM\\pApnd.e")
    AutoGlabel(opJeq,       "%opJccE",      "VM\\pJcc.e")
    AutoGlabel(opJne,       "%opJccE",      "VM\\pJcc.e")
    AutoGlabel(opJlt,       "%opJcc",       "VM\\pJcc.e")
    AutoGlabel(opJle,       "%opJcc",       "VM\\pJcc.e")
    AutoGlabel(opJgt,       "%opJcc",       "VM\\pJcc.e")
    AutoGlabel(opJge,       "%opJcc",       "VM\\pJcc.e")
    AutoGlabel(opJif,       "%opJif",       "VM\\pJcc.e")
    AutoGlabel(opJnot,      "%opJif",       "VM\\pJcc.e")   -- (:%opJif is used for both opJif and opJnot)
    AutoGlabel(opScmp,      "%opScmp",      "VM\\pJcc.e")
    AutoGlabel(opInt0,      "%opInt0",      "VM\\pJcc.e")
    AutoGlabel(opAtom0,     "%opAtom0",     "VM\\pJcc.e")
    AutoGlabel(opSq0,       "%opSq0",       "VM\\pJcc.e")
    AutoGlabel(opStr0,      "%opStr0",      "VM\\pJcc.e")
    AutoGlabel(opInt,       "%opInt",       "VM\\pType.e")
    AutoGlabel(opAtom,      "%opAtom",      "VM\\pType.e")
    AutoGlabel(opStr,       "%opStr",       "VM\\pType.e")
    AutoGlabel(opSq,        "%opSq",        "VM\\pType.e")
    AutoGlabel(opObj,       "%opObj",       "VM\\pType.e")
--printf(1,"psym.e/syminit line 2077: aatidx[opObj=%d]=%d, agtidx[$=%d]=%d, agfnos[$=%d]=%d\n",{opObj,aatidx[opObj],length(agtidx),agtidx[$],length(agfnos),agfnos[$]})
    AutoGlabel(opUminus,    "%opUminus",    "VM\\pUnary.e")
    AutoGlabel(opNot,       "%opNot",       "VM\\pUnary.e")
    AutoGlabel(opNotBits,   "%opNotBits",   "VM\\pUnary.e")
    AutoGlabel(opFloor,     "%opFloor",     "VM\\pUnary.e")
    AutoGlabel(opAdd,       "%opAdd",       "VM\\pMath.e")
    AutoGlabel(opAddi,      "%opAddi",      "VM\\pMath.e")
    AutoGlabel(opSub,       "%opSub",       "VM\\pMath.e")
    AutoGlabel(opSubi,      "%opSubi",      "VM\\pMath.e")
    AutoGlabel(opDiv,       "%opDiv",       "VM\\pMath.e")
    AutoGlabel(opDivi,      "%opDivi",      "VM\\pMath.e")
    AutoGlabel(opDivf,      "%opDivf",      "VM\\pMath.e")
    AutoGlabel(opDiv2,      "%opDiv2",      "VM\\pMath.e")
    AutoGlabel(opMul,       "%opMul",       "VM\\pMath.e")
    AutoGlabel(opMuli,      "%opMuli",      "VM\\pMath.e")
    AutoGlabel(opAndBits,   "%opAndBits",   "VM\\pMath.e")
    AutoGlabel(opOrBits,    "%opOrBits",    "VM\\pMath.e")
    AutoGlabel(opXorBits,   "%opXorBits",   "VM\\pMath.e")
    AutoGlabel(opLen,       "%opLen",       "VM\\pLen.e")
    AutoGlabel(opRmdr,      "%opRmdr",      "VM\\pRmdr.e")
    AutoGlabel(opPeek,      "%opPeekNx",    "VM\\pMem.e")
    AutoGlabel(opPeek1s,    "%opPeekNx",    "VM\\pMem.e")
    AutoGlabel(opPeek1u,    "%opPeekNx",    "VM\\pMem.e")
    AutoGlabel(opPeek2s,    "%opPeekNx",    "VM\\pMem.e")
    AutoGlabel(opPeek2u,    "%opPeekNx",    "VM\\pMem.e")
    AutoGlabel(opPeek4s,    "%opPeekNx",    "VM\\pMem.e")
    AutoGlabel(opPeek4u,    "%opPeekNx",    "VM\\pMem.e")
    AutoGlabel(opPeek8s,    "%opPeekNx",    "VM\\pMem.e")
    AutoGlabel(opPeek8u,    "%opPeekNx",    "VM\\pMem.e")
    AutoGlabel(opPeekNS,    "%opPeekNx",    "VM\\pMem.e")
--  AutoGlabel(opPeeki,     "%opPeeki",     "VM\\pMem.e")   -- (untested/erm/inlined?)
--  AutoGlabel(opPoke,      "%opPoke",      "VM\\pMem.e")
--  AutoGlabel(opPoke4,     "%opPoke4",     "VM\\pMem.e")
    AutoGlabel(opMemCopy,   "%opMemCopy",   "VM\\pMem.e")
    AutoGlabel(opMemSet,    "%opMemSet",    "VM\\pMem.e")
    AutoGlabel(opOpen,      "%opOpen",      "VM\\pfileioN.e")
    AutoGlabel(opClose,     "%opClose",     "VM\\pfileioN.e")
    AutoGlabel(opSeek,      "%opSeek",      "VM\\pfileioN.e")
    AutoGlabel(opWhere,     "%opWhere",     "VM\\pfileioN.e")
    AutoGlabel(opFlush,     "%opFlush",     "VM\\pfileioN.e")
    AutoGlabel(opPuts,      "%opPuts",      "VM\\pfileioN.e")
    AutoGlabel(opGetc,      "%opGetc",      "VM\\pfileioN.e")
    AutoGlabel(opGets,      "%opGets",      "VM\\pfileioN.e")
    AutoGlabel(opGetKey,    "%opGetKey",    "VM\\pfileioN.e")
    AutoGlabel(opWaitKey,   "%opWaitKey",   "VM\\pfileioN.e")
    AutoGlabel(opLock,      "%opLock",      "VM\\pfileioN.e")
    AutoGlabel(opUnLock,    "%opUnLock",    "VM\\pfileioN.e")
    AutoGlabel(opGetText,   "%opGetText",   "VM\\pfileioN.e")
    AutoGlabel(opGetPos,    "%opGetPos",    "VM\\pfileioN.e")
    AutoGlabel(opWrap,      "%opWrap",      "VM\\pfileioN.e")
--  AutoGlabel(opScroll,    "%opScroll",    "VM\\pfileioN.e")   -- now in builtins\pScrollN.e (not part of the VM)
    AutoGlabel(opTextRows,  "%opTextRows",  "VM\\pfileioN.e")
    AutoGlabel(opBkClr,     "%opBkClr",     "VM\\pfileioN.e")
    AutoGlabel(opTxtClr,    "%opTxtClr",    "VM\\pfileioN.e")
    AutoGlabel(opClrScrn,   "%opClrScrn",   "VM\\pfileioN.e")
    AutoGlabel(opFreeCons,  "%opFreeCons",  "VM\\pfileioN.e")
    AutoGlabel(opPosition,  "%opPosition",  "VM\\pfileioN.e")
    AutoGlabel(opPow,       "%opPow",       "VM\\pPower.e")
    AutoGlabel(opCos,       "%opCos",       "VM\\pTrig.e")
    AutoGlabel(opSin,       "%opSin",       "VM\\pTrig.e")
    AutoGlabel(opTan,       "%opTan",       "VM\\pTrig.e")
    AutoGlabel(opArcTan,    "%opArcTan",    "VM\\pTrig.e")
    AutoGlabel(opLog,       "%opLog",       "VM\\pTrig.e")
    AutoGlabel(opSqrt,      "%opSqrt",      "VM\\pTrig.e")
    AutoGlabel(opJnotx,     "%opJnotx",     "VM\\pJnotx.e")
    AutoGlabel(opXor,       "%opXor",       "VM\\pXor.e")
    AutoGlabel(opTime,      "%opTime",      "VM\\pTime.e")
    AutoGlabel(opSetRand,   "%opSetRand",   "VM\\pRand.e")
    AutoGlabel(opRand,      "%opRand",      "VM\\pRand.e")
    AutoGlabel(opSleep,     "%opSleep",     "VM\\pSleep.e")
--  AutoGlabel(opRepeat,    "%opRepeat",    "VM\\pRepeatN.e")
--  AutoGlabel(opRepCh,     "%opRepCh",     "VM\\pRepeatN.e")
    AutoGlabel(opInstance,  "%opInstance",  "VM\\pInstance.e")
    AutoGlabel(opProfile,   "%opProfile",   "VM\\pProfile.e")
    AutoGlabel(opProfout,   "%opProfout",   "VM\\pProfile.e")
    AutoGlabel(opLnp,       "%opLnp",       "VM\\pProfile.e")
    AutoGlabel(opLnpt,      "%opLnpt",      "VM\\pProfile.e")
--  AutoGlabel(opOpenDLL,   "%opOpenDLL",   "VM\\pcfuncN.e")        -- NO!!
--  AutoGlabel(opDcfunc,    "%opDcfunc",    "VM\\pcfuncN.e")
--  AutoGlabel(opDcvar,     "%opDcvar",     "VM\\pcfuncN.e")
--  AutoGlabel(opCallback,  "%opCallback",  "VM\\pcfuncN.e")
--  AutoGlabel(opCbHandler, "%cbhandler",   "VM\\pcfunc.e")
    AutoGlabel(opCbHandler, "%cbhandler",   "VM\\cbhand.e")
    AutoGlabel(opCallFunc,  "%opCallFunc",  "VM\\pcallfunc.e")
    AutoGlabel(opCallProc,  "%opCallProc",  "VM\\pcallfunc.e")

--DEV (maybe)
--  AutoGlabel(opPuts1,     "%puts1",       "VM\\puts1.e",  vm_puts1)
--  vm_puts1 = 1,       -- :%puts1 in builtins\VM\puts1.e
--  vm_puts1r = 2,      -- :%puts1ediesi (32-bit) or :%puts1rdirsi (64-bit)
--  vm_puthex32a = 3,   -- :%puthex32a
--  vm_puthex32 = 4,    -- :%puthex32
--  vm_puthex64 = 5,    -- :%puthex64
--  vm_putsint = 6,     -- :%putsint
--  vm_getc0 = 7,       -- :%getc0


    -- the following functions return an integer:

    AutoAsm("equal",            S_Func,"FOO",   "VM\\pJcc.e",       opSeq,      "%opSeq",       E_none, T_integer)  T_equal = symlimit      -- (178)
    AutoAsm("compare",          S_Func,"FOO",   "VM\\pJcc.e",       opScmp,     "%opScmp",      E_none, T_integer)  T_compare = symlimit
    AutoAsm("getc",             S_Func,"FI",    "VM\\pfileioN.e",   opGetc,     "%opGetc",      E_other,T_integer)
    AutoAsm("get_key",          S_Func,"F",     "VM\\pfileioN.e",   opGetKey,   "%opGetKey",    E_other,T_integer)
    AutoAsm("init_cs",          S_Func,"F",     "VM\\pHeap.e",      opInitCS,   "%pInitCS",     E_other,T_integer)
    AutoAsm("open",             S_Func,"FSO",   "VM\\pfileioN.e",   opOpen,     "%opOpen",      E_other,T_integer)
    AutoAsm("seek",             S_Func,"FIN",   "VM\\pfileioN.e",   opSeek,     "%opSeek",      E_other,T_integer)
    AutoAsm("try_cs",           S_Func,"FI",    "VM\\pHeap.e",      opTryCS,    "%pTryCS",      E_other,T_integer)
    AutoAsm("wait_key",         S_Func,"F",     "VM\\pfileioN.e",   opWaitKey,  "%opWaitKey",   E_other,T_integer)
    AutoAsm("lock_file",        S_Func,"FIIP",  "VM\\pfileioN.e",   opLock,     "%opLock",      E_other,T_integer)
    AutoAsm("text_rows",        S_Func,"FI",    "VM\\pfileioN.e",   opTextRows, "%opTextRows",  E_other,T_integer)
    AutoAsm("length",           S_Func,"FP",    "VM\\pLen.e",       opLen,      "%opLen",       E_none, T_integer)  T_length = symlimit
--  AutoAsm("define_c_funcN",   S_Func,"FNSPN", "VM\\pcfuncN.e",    opDcfunc,   "%opDcfunc",    E_other,T_integer)
--  AutoAsm("define_c_procN",   S_Func,"FNSP",  "VM\\pcfuncN.e",    opDcfunc,   "%opDcfunc",    E_other,T_integer)  T_dcproc = symlimit

    -- the following functions return an atom:

--  AutoAsm("allocate",         S_Func,"FI",    "VM\\pHeap.e",      opAlloc,    "%pAlloc",      E_none, T_atom)
--DEV... (not documented either)
--  AutoAsm("allocate_data",    S_Func,"FI",    "VM\\pHeap.e",      opAlloc,    "%pAlloc",      E_none, T_atom)
    AutoAsm("remainder",        S_Func,"FNN",   "VM\\pRmdr.e",      opRmdr,     "%opRmdr",      E_none, T_atom)     T_remainder = symlimit
    AutoAsm("and_bits",         S_Func,"FNN",   "VM\\pMath.e",      opAndBits,  "%opAndBits",   E_none, T_atom)     T_and_bits = symlimit
    AutoAsm("or_bits",          S_Func,"FNN",   "VM\\pMath.e",      opOrBits,   "%opOrBits",    E_none, T_atom)     T_or_bits = symlimit
    AutoAsm("xor_bits",         S_Func,"FNN",   "VM\\pMath.e",      opXorBits,  "%opXorBits",   E_none, T_atom)     T_xor_bits = symlimit
    AutoAsm("not_bits",         S_Func,"FN",    "VM\\pUnary.e",     opNotBits,  "%opNotBits",   E_none, T_atom)     T_not_bits = symlimit
    AutoAsm("floor",            S_Func,"FN",    "VM\\pUnary.e",     opFloor,    "%opFloor",     E_none, T_atom)     T_floor = symlimit
    AutoAsm("instance",         S_Func,"F",     "VM\\pInstance.e",  opInstance, "%opInstance",  E_none, T_atom)
    AutoAsm("power",            S_Func,"FNN",   "VM\\pPower.e",     opPow,      "%opPow",       E_none, T_atom)     T_power = symlimit
    AutoAsm("cos",              S_Func,"FN",    "VM\\pTrig.e",      opCos,      "%opCos",       E_none, T_atom)     T_cos = symlimit
    AutoAsm("sin",              S_Func,"FN",    "VM\\pTrig.e",      opSin,      "%opSin",       E_none, T_atom)     T_sin = symlimit
    AutoAsm("tan",              S_Func,"FN",    "VM\\pTrig.e",      opTan,      "%opTan",       E_none, T_atom)     T_tan = symlimit
    AutoAsm("arctan",           S_Func,"FN",    "VM\\pTrig.e",      opArcTan,   "%opArcTan",    E_none, T_atom)     T_arctan = symlimit
    AutoAsm("log",              S_Func,"FN",    "VM\\pTrig.e",      opLog,      "%opLog",       E_none, T_atom)     T_log = symlimit
    AutoAsm("sqrt",             S_Func,"FN",    "VM\\pTrig.e",      opSqrt,     "%opSqrt",      E_none, T_atom)     T_sqrt = symlimit
    AutoAsm("time",             S_Func,"F",     "VM\\pTime.e",      opTime,     "%opTime",      E_none, T_atom)
    AutoAsm("rand",             S_Func,"FI",    "VM\\pRand.e",      opRand,     "%opRand",      E_none, T_atom)     T_rand = symlimit
    AutoAsm("where",            S_Func,"FI",    "VM\\pfileioN.e",   opWhere,    "%opWhere",     E_none, T_atom)
--  AutoAsm("open_dllN",        S_Func,"FP",    "VM\\pcfuncN.e",    opOpenDLL,  "%opOpenDLL",   E_other,T_atom)
--  AutoAsm("define_c_varN",    S_Func,"FNP",   "VM\\pcfuncN.e",    opDcvar,    "%opDcvar",     E_other,T_atom)
--  AutoAsm("call_backN",       S_Func,"FO",    "VM\\pcfuncN.e",    opCallback, "%opCallback",  E_other,T_atom)

    -- the following functions return a sequence (or string):

    AutoAsm("append",           S_Func,"FOO",   "VM\\pApnd.e",      opApnd,     "%opApnd",      E_none,T_sequence)  Z_append = symlimit
    AutoAsm("prepend",          S_Func,"FOO",   "VM\\pApnd.e",      opPpnd,     "%opApnd",      E_none,T_sequence)  Z_prepend = symlimit
                                                                            -- (:%opApnd is used for both append and prepend)
--  AutoAsm("repeat",           S_Func,"FOI",   "VM\\pRepeatN.e",   opRepeat,   "%opRepeat",    E_none,T_sequence)  T_repeat = symlimit
--  AutoAsm("repeatch",         S_Func,"FII",   "VM\\pRepeatN.e",   opRepCh,    "%opRepCh",     E_none,T_string)    T_repch = symlimit      -- (209)
    AutoAsm("get_position",     S_Func,"F",     "VM\\pfileioN.e",   opGetPos,   "%opGetPos",    E_none,T_sequence)

    -- the following functions return an object:

--  AutoAsm("call_func",        S_Func,"FIP",   "VM\\pcallfunc.e",  opCallFunc, "%opCallFunc",  E_other,T_object)
    AutoAsm("call_func",        S_Func,"FIP",   "VM\\pcallfunc.e",  opCallFunc, "%opCallFunc",  E_all,  T_object)
    AutoAsm("delete_routine",   S_Func,"FOI",   "VM\\pDeleteN.e",   opDelRtn,   "%opDelRtn",    E_other,T_object)
    AutoAsm("gets",             S_Func,"FI",    "VM\\pfileioN.e",   opGets,     "%opGets",      E_other,T_object)
    AutoAsm("peek",             S_Func,"FO",    "VM\\pMem.e",       opPeek,     "%opPeekNx",    E_none, T_object)
    AutoAsm("peek1s",           S_Func,"FO",    "VM\\pMem.e",       opPeek1s,   "%opPeekNx",    E_none, T_object)
    AutoAsm("peek1u",           S_Func,"FO",    "VM\\pMem.e",       opPeek1u,   "%opPeekNx",    E_none, T_object)
    AutoAsm("peek2s",           S_Func,"FO",    "VM\\pMem.e",       opPeek2s,   "%opPeekNx",    E_none, T_object)
    AutoAsm("peek2u",           S_Func,"FO",    "VM\\pMem.e",       opPeek2u,   "%opPeekNx",    E_none, T_object)   -- (215)
    AutoAsm("peek4s",           S_Func,"FO",    "VM\\pMem.e",       opPeek4s,   "%opPeekNx",    E_none, T_object)
    AutoAsm("peek4u",           S_Func,"FO",    "VM\\pMem.e",       opPeek4u,   "%opPeekNx",    E_none, T_object)
--  Alias("peek_pointer",symlimit)
    AutoAsm("peek8s",           S_Func,"FO",    "VM\\pMem.e",       opPeek8s,   "%opPeekNx",    E_none, T_object)
    AutoAsm("peek8u",           S_Func,"FO",    "VM\\pMem.e",       opPeek8u,   "%opPeekNx",    E_none, T_object)
    AutoAsm("peekNS",           S_Func,"FOII",  "VM\\pMem.e",       opPeekNS,   "%opPeekNx",    E_none, T_object)
--DEV/SUG:
--  AutoAsm("get_text",         S_Func,"FI[I]", "VM\\pfileioN.e",   opGetText,  "%opGetText",   E_none, T_object)   T_get_text = symlimit
    AutoAsm("get_textn",        S_Func,"FII",   "VM\\pfileioN.e",   opGetText,  "%opGetText",   E_none, T_object)
    symtab[symlimit][S_ParmN] = 1
--  Alias("get_textn", symlimit)    --DEV temp...

    -- the remainder are procedures:

--  AutoAsm("abort",            S_Proc,"PI",    "VM\\pAbort.e",     opAbort,    "%opAbort",     E_other,0)      T_abort = symlimit
    AutoAsm("abort",            S_Proc,"PI",    "VM\\pStack.e",     opAbort,    "%opAbort",     E_other,0)      T_abort = symlimit
    AutoAsm("bk_color",         S_Proc,"PI",    "VM\\pfileioN.e",   opBkClr,    "%opBkClr",     E_other,0)
    AutoAsm("call_proc",        S_Proc,"PIP",   "VM\\pcallfunc.e",  opCallProc, "%opCallProc",  E_all,  0)
    AutoAsm("clear_screen",     S_Proc,"P",     "VM\\pfileioN.e",   opClrScrn,  "%opClrScrn",   E_other,0)
    AutoAsm("close",            S_Proc,"PI",    "VM\\pfileioN.e",   opClose,    "%opClose",     E_other,0)
    AutoAsm("crash_file",       S_Proc,"PO",    "VM\\pDiagN.e",     opCrashFile,"%pCrashFile",  E_other,0)
    AutoAsm("crash_message",    S_Proc,"PO",    "VM\\pDiagN.e",     opCrashMsg, "%pCrashMsg",   E_other,0)
    AutoAsm("crash_routine",    S_Proc,"PI",    "VM\\pDiagN.e",     opCrashRtn, "%pCrashRtn",   E_other,0)
--  AutoAsm("delete",           S_Proc,"PO",    "VM\\pDeleteN.e",   opDelete,   "%opDelete",    E_other,0)
    AutoAsm("delete",           S_Proc,"PO",    "VM\\pDeleteN.e",   opDelete,   "%opDelete",    E_all,  0)
    AutoAsm("delete_cs",        S_Proc,"PI",    "VM\\pHeap.e",      opDeleteCS, "%pDeleteCS",   E_other,0)
    AutoAsm("enter_cs",         S_Proc,"PI",    "VM\\pHeap.e",      opEnterCS,  "%pEnterCS",    E_other,0)      T_EnterCS = symlimit
    symtab[symlimit][S_ParmN] = 0
    AutoAsm("flush",            S_Func,"PI",    "VM\\pfileioN.e",   opFlush,    "%opFlush",     E_other,0)
    AutoAsm("free_console",     S_Func,"P",     "VM\\pfileioN.e",   opFreeCons, "%opFreeCons",  E_other,0)
--  AutoAsm("free",             S_Proc,"PN",    "VM\\pHeap.e",      opFree,     "%pFree",       E_other,0)
    AutoAsm("leave_cs",         S_Proc,"PI",    "VM\\pHeap.e",      opLeaveCS,  "%pLeaveCS",    E_other,0)      T_LeaveCS = symlimit
    symtab[symlimit][S_ParmN] = 0
    AutoAsm("mem_copy",         S_Proc,"PNNI",  "VM\\pMem.e",       opMemCopy,  "%opMemCopy",   E_other,0)
    AutoAsm("mem_set",          S_Proc,"PNII",  "VM\\pMem.e",       opMemSet,   "%opMemSet",    E_other,0)
--  AutoAsm("poke",             S_Proc,"PIO",   "VM\\pMem.e",       opPoke,     "%opPoke",      E_other,0)
--  AutoAsm("poke4",            S_Proc,"PIO",   "VM\\pMem.e",       opPoke4,    "%opPoke4",     E_other,0)
    AutoAsm("poke",             S_Proc,"PNO",   "VM\\pMem.e",       opPoke1,    "%opPokeN",     E_other,0)
    AutoAsm("poke1",            S_Proc,"PNO",   "VM\\pMem.e",       opPoke1,    "%opPokeN",     E_other,0)
    AutoAsm("poke2",            S_Proc,"PNO",   "VM\\pMem.e",       opPoke2,    "%opPokeN",     E_other,0)
    AutoAsm("poke4",            S_Proc,"PNO",   "VM\\pMem.e",       opPoke4,    "%opPokeN",     E_other,0)
    AutoAsm("poke8",            S_Proc,"PNO",   "VM\\pMem.e",       opPoke8,    "%opPokeN",     E_other,0)
    AutoAsm("pokeN",            S_Proc,"PNOI",  "VM\\pMem.e",       opPokeN,    "%opPokeN",     E_other,0)
    AutoAsm("position",         S_Proc,"PII",   "VM\\pfileioN.e",   opPosition, "%opPosition",  E_other,0)
    AutoAsm("puts",             S_Proc,"PIO",   "VM\\pfileioN.e",   opPuts,     "%opPuts",      E_other,0)
    AutoAsm("unlock_file",      S_Proc,"PIP",   "VM\\pfileioN.e",   opUnLock,   "%opUnLock",    E_other,0)
    AutoAsm("wrap",             S_Proc,"PI",    "VM\\pfileioN.e",   opWrap,     "%opWrap",      E_other,0)
--  AutoAsm("scroll",           S_Proc,"PIII",  "VM\\pfileioN.e",   opScroll,   "%opScroll",    E_other,0)
    AutoAsm("text_color",       S_Proc,"PI",    "VM\\pfileioN.e",   opTxtClr,   "%opTxtClr",    E_other,0)
    AutoAsm("set_rand",         S_Proc,"PI",    "VM\\pRand.e",      opSetRand,  "%opSetRand",   E_other,0)
    AutoAsm("sleep",            S_Proc,"PN",    "VM\\pSleep.e",     opSleep,    "%opSleep",     E_other,0)
    AutoAsm("throw",            S_Proc,"POO",   "VM\\pDiagN.e",     opThrow,    "%pThrow",      E_other,0)      T_throw = symlimit
    symtab[symlimit][S_ParmN] = 1
    AutoAsm("trace",            S_Proc,"PN",    "VM\\pTrace.e",     opTrace,    "%opTrace",     E_other,0)
    AutoAsm("profile",          S_Proc,"PN",    "VM\\pProfile.e",   opProfile,  "%opProfile",   E_other,0)
--DEV hide/invoke via :%opProfout/reserved word/document??...
    AutoAsm("dump_profile",     S_Proc,"P",     "VM\\pProfile.e",   opProfout,  "%opProfout",   E_other,0)
--opProfout = 171,      -- profile_dump()

end if
--  T_mfree = symlimit
--              T_malloc = symlimit
--global constant I_malloc = tt_stringI("%opMallocX") -- (nb must match the label in pmalloc.e)
--global constant I_mfree = tt_stringI("%opMfreeX") -- (nb must match the label in pmalloc.e)


    T_Asm = symlimit
    hll_stubs = repeat(0,T_Asm)


    -- auto-includes which return an integer first:
    -- note that builtin overrides get a new symtab slot
    IAEType = T_integer

    initialAutoEntry("bankers_rounding",        S_Func,"FNI",   "pmaths.e",0,E_none,1)
    initialAutoEntry("binary_search",           S_Func,"FOP",   "bsearch.e",0,E_none)
    initialAutoEntry("chdir",                   S_Func,"FP",    "pchdir.e",0,E_other)
    initialAutoEntry("check_break",             S_Func,"F",     "pbreak.e",0,E_other)
    initialAutoEntry("copy_directory",          S_Func,"FSSI",  "pfile.e",0,E_other,2)
    initialAutoEntry("copy_file",               S_Func,"FSSI",  "pfile.e",0,E_other,2)
--  symtab[symlimit][S_ParmN] = 2
--DEV/SUG:
--  initialAutoEntry("clear_directory",         S_Func,"FS[I]", "pfile.e",0,E_other)
    initialAutoEntry("clear_directory",         S_Func,"FSI",   "pfile.e",0,E_other,1)
--  symtab[symlimit][S_ParmN] = 1
--DEV (needs MARKTYPES...)
--bool res = complex(object o)
--  initialAutoEntry("complex",                 S_Type,"TP",    "complex.e",0,E_none)

--  initialAutoEntry("create_directory",        S_Func,"FS[II]","pfile.e",0,E_other)
    initialAutoEntry("create_directory",        S_Func,"FSII",  "pfile.e",0,E_other,1)
--  symtab[symlimit][S_ParmN] = 1
    initialAutoEntry("db_create",               S_Func,"FPI",   "database.e",0,E_other)
    initialAutoEntry("db_open",                 S_Func,"FPI",   "database.e",0,E_other)
    initialAutoEntry("db_select",               S_Func,"FP",    "database.e",0,E_other)
    initialAutoEntry("db_select_table",         S_Func,"FP",    "database.e",0,E_other)
    initialAutoEntry("db_create_table",         S_Func,"FP",    "database.e",0,E_other)
    initialAutoEntry("db_find_key",             S_Func,"FO",    "database.e",0,E_other)
    initialAutoEntry("db_insert",               S_Func,"FOO",   "database.e",0,E_other)
    initialAutoEntry("db_table_size",           S_Func,"F",     "database.e",0,E_other)
    initialAutoEntry("db_compress",             S_Func,"F",     "database.e",0,E_other)
--if newEmit then
    initialAutoEntry("define_c_func",           S_Func,"FOOPN", "VM\\pcfunc.e",0,E_none)
    initialAutoEntry("define_c_proc",           S_Func,"FOOP",  "VM\\pcfunc.e",0,E_none)
--else
--  initialAutoEntry("define_c_func",           S_Func,"FOOON", "pcfunc.e",0,E_none)
--  initialAutoEntry("define_c_proc",           S_Func,"FOOO",  "pcfunc.e",0,E_none)
--end if
    initialAutoEntry("delete_file",             S_Func,"FS",    "pfile.e",0,E_other)
    initialAutoEntry("day_of_year",             S_Func,"FIII",  "pdates.e",0,E_none)
    initialAutoEntry("days_in_month",           S_Func,"FII",   "pdates.e",0,E_none)
--DEV (needs MARKTYPES...)
--  initialAutoEntry("dictionary",              S_Type,"TI",    "dict.e",0,E_none)
    initialAutoEntry("dict_size",               S_Func,"FI",    "dict.e",0,E_none,0)
--  symtab[symlimit][S_ParmN] = 1
    initialAutoEntry("file_exists",             S_Func,"FS",    "pfile.e",0,E_none)
    initialAutoEntry("find_any",                S_Func,"FPPI",  "pfindany.e",0,E_none)
    symtab[symlimit][S_ParmN] = 2
    initialAutoEntry("get_file_type",           S_Func,"FS",    "pfile.e",0,E_none)
    Alias("file_type", symlimit)
    initialAutoEntry("get_maxprime",            S_Func,"FN",    "primes.e",0,E_none)
    initialAutoEntry("get_test_abort",          S_Func,"F",     "unit_test.e",0,E_none)
    initialAutoEntry("get_test_logfile",        S_Func,"F",     "unit_test.e",0,E_none)
    initialAutoEntry("get_test_pause",          S_Func,"F",     "unit_test.e",0,E_none)
    initialAutoEntry("get_test_verbosity",      S_Func,"F",     "unit_test.e",0,E_none)
    initialAutoEntry("getd_index",              S_Func,"FOI",   "dict.e",0,E_other)
    symtab[symlimit][S_ParmN] = 1
--4/10/2020:
    initialAutoEntry("hll_atom",                S_Func,"FO",    "hll_stubs.e",0,E_none)     hll_stubs[T_atom] = symlimit
    initialAutoEntry("hll_compare",             S_Func,"FOO",   "hll_stubs.e",0,E_none)     hll_stubs[T_compare] = symlimit
    initialAutoEntry("hll_equal",               S_Func,"FOO",   "hll_stubs.e",0,E_none)     hll_stubs[T_equal] = symlimit
--X initialAutoEntry("hll_getc",                S_Func,"FI",    "hll_stubs.e",0,E_other)
--X initialAutoEntry("hll_get_key",             S_Func,"F",     "hll_stubs.e",0,E_other)
--X initialAutoEntry("hll_init_cs",             S_Func,"F",     "hll_stubs.e",0,E_other)
    initialAutoEntry("hll_integer",             S_Func,"FO",    "hll_stubs.e",0,E_none)     hll_stubs[T_integer] = symlimit
    initialAutoEntry("hll_length",              S_Func,"FP",    "hll_stubs.e",0,E_none)     hll_stubs[T_length] = symlimit
--X initialAutoEntry("hll_lock_file",           S_Func,"FIIP",  "hll_stubs.e",0,E_other)
    initialAutoEntry("hll_object",              S_Func,"FO",    "hll_stubs.e",0,E_none)     hll_stubs[T_object] = symlimit
--X initialAutoEntry("hll_open",                S_Func,"FSO",   "hll_stubs.e",0,E_other)
--X initialAutoEntry("hll_seek",                S_Func,"FIN",   "hll_stubs.e",0,E_other)
    initialAutoEntry("hll_sequence",            S_Func,"FO",    "hll_stubs.e",0,E_none)     hll_stubs[T_sequence] = symlimit
    initialAutoEntry("hll_string",              S_Func,"FO",    "hll_stubs.e",0,E_none)     hll_stubs[T_string] = symlimit
--X initialAutoEntry("hll_try_cs",              S_Func,"FI",    "hll_stubs.e",0,E_other)
--X initialAutoEntry("hll_wait_key",            S_Func,"F",     "hll_stubs.e",0,E_other)

    initialAutoEntry("include_file",            S_Func,"FI",    "pincpathN.e",0,E_none,0)
    initialAutoEntry("is_dict",                 S_Func,"FI",    "dict.e",0,E_none)
    initialAutoEntry("is_integer",              S_Func,"FS",    "to_int.e",0,E_none)
    initialAutoEntry("is_leap_year",            S_Func,"FI",    "pdates.e",0,E_none)
    initialAutoEntry("is_prime",                S_Func,"FN",    "pfactors.e",0,E_none)
    initialAutoEntry("is_struct",               S_Func,"FOS",   "structs.e",0,E_none)   T_is_struct = symlimit
    initialAutoEntry("isatty",                  S_Func,"FI",    "isatty.e",0,E_none)
    initialAutoEntry("islower",                 S_Func,"FI",    "pcase.e",0,E_none)
    initialAutoEntry("isupper",                 S_Func,"FI",    "pcase.e",0,E_none)
--if newEmit then --DEV (temp) (T_find/T_match will be rqd once the asm conversion is completed)
--  initialAutoEntry("find",                    S_Func,"FOPI",  "VM\\pFind.e",0,E_none,2)   T_find = symlimit
--  initialAutoEntry("rfind",                   S_Func,"FOPI",  "VM\\pFind.e",0,E_none,2)
    initialAutoEntry("find",                    S_Func,"FOPI",  "find.e",0,E_none,2)        T_find = symlimit
    initialAutoEntry("rfind",                   S_Func,"FOPI",  "find.e",0,E_none,2)
--  Alias("find_from", T_find)  (--DEV)
--  Alias("find_from", symlimit)    -- killed 3/8/15
--  initialAutoEntry("match",                   S_Func,"FOPII", "VM\\pMatch.e",0,E_none,2)  --T_match = symlimit
--  initialAutoEntry("rmatch",                  S_Func,"FOPII", "VM\\pMatch.e",0,E_none,2)
    initialAutoEntry("match",                   S_Func,"FOPII", "match.e",0,E_none,2)
    initialAutoEntry("rmatch",                  S_Func,"FOPII", "match.e",0,E_none,2)
--  Alias("match_from", T_match) (--DEV)
--  Alias("match_from", symlimit)   -- killed 3/8/15
--end if

    initialAutoEntry("move_file",               S_Func,"FSSI",  "pfile.e",0,E_other,2)
    initialAutoEntry("message_box",             S_Func,"FSSIN", "msgbox.e",0,E_other,2)
    initialAutoEntry("named_dict",              S_Func,"FS",    "dict.e",0,E_other)
    initialAutoEntry("new_dict",                S_Func,"FOI",   "dict.e",0,E_other,0)
    initialAutoEntry("remove_directory",        S_Func,"FSI",   "pfile.e",0,E_other,1)
    initialAutoEntry("rename_file",             S_Func,"FSSI",  "pfile.e",0,E_other,2)
    initialAutoEntry("routine_id",              S_Func,"FP",    "VM\\prtnidN.e",0,E_none)   T_routine = symlimit
    initialAutoEntry("pq_new",                  S_Func,"FII",   "pqueue.e",0,E_other,0)
    initialAutoEntry("pq_size",                 S_Func,"FI",    "pqueue.e",0,E_none,0)
    initialAutoEntry("pq_empty",                S_Func,"FI",    "pqueue.e",0,E_none,0)
--if newEmit then
--  --(getc is now an AutoAsm)
----    initialAutoEntry("open",                S_Func,"FSO",   "VM\\pfileioN.e",0,E_other)
----    initialAutoEntry("seek",                S_Func,"FII",   "VM\\pfileioN.e",0,E_other)
----    initialAutoEntry("where",               S_Func,"FI",    "VM\\pfileioN.e",0,E_other)
----    initialAutoEntry("lock_file",           S_Func,"FIIP",  "VM\\pfileioN.e",0,E_other)
--elsif hllfileio then
--  initialAutoEntry("getc",                    S_Func,"FN",    "pfileio.e",0,E_other)
--  initialAutoEntry("open",                    S_Func,"FSO",   "pfileio.e",0,E_other)
--  initialAutoEntry("seek",                    S_Func,"FII",   "pfileio.e",0,E_other)
--  initialAutoEntry("where",                   S_Func,"FI",    "pfileio.e",0,E_other)
--  initialAutoEntry("lock_file",               S_Func,"FIIP",  "pfileio.e",0,E_other)
--end if
    initialAutoEntry("set_file_date",           S_Func,"FSI",   "pfile.e",0,E_other,1)
    initialAutoEntry("sizeof",                  S_Func,"FI",    "dll.e",0,E_none)
    initialAutoEntry("square_free",             S_Func,"FNI",   "pfactors.e",0,E_none,1)
    initialAutoEntry("still_has_delete_routine",S_Func,"FO","hasdel.e",0,E_none)
    initialAutoEntry("struct",                  S_Type,"TO",    "structs.e",0,E_none)           Z_struct = symlimit
--  symtab[symlimit][S_State] = or_bits(symtab[symlimit][S_State],K_struc)
    Alias("class",symlimit)
    initialAutoEntry("task_create",             S_Func,"FIP",   "VM\\pTask.e",0,E_other)
    initialAutoEntry("task_self",               S_Func,"F",     "VM\\pTask.e",0,E_none)
    initialAutoEntry("task_status",             S_Func,"FI",    "VM\\pTask.e",0,E_none)
--DEV (needs MARKTYPES)
--  initialAutoEntry("timedate",                S_Type,"TO",    "timedate.e",0,E_none)
    initialAutoEntry("to_integer",              S_Func,"FSI",   "to_int.e",0,E_none,1)
    initialAutoEntry("save_bitmap",             S_Func,"FPP",   "image.e",0,E_other)
--DEV document/test/remove:
    initialAutoEntry("TlsAlloc",                S_Func,"F",     "ptls.ew",0,E_other)
    initialAutoEntry("wildcard_match",          S_Func,"FPP",   "wildcard.e",0,E_none)
    Alias("is_match", symlimit)
    initialAutoEntry("wildcard_file",           S_Func,"FPP",   "wildcard.e",0,E_none)

    T_AInt = symlimit   -- DEV: T_SAtm, T_AStr, T_ASeq, T_SObj...

    -- the following return an atom
    IAEType = T_atom

    initialAutoEntry("abs",                     S_Func,"FN",    "pmaths.e",0,E_none)
    initialAutoEntry("allocate",                S_Func,"FII",   "pAlloc.e",0,E_other,1)
--DEV doc
    initialAutoEntry("allocate_data",           S_Func,"FII",   "pAlloc.e",0,E_other)
    initialAutoEntry("allocate_string",         S_Func,"FPI",   "pAlloc.e",0,E_other,1)
    initialAutoEntry("allocate_word",           S_Func,"FIII",  "pAlloc.e",0,E_other,0)
    initialAutoEntry("allocate_wstring",        S_Func,"FPI",   "pAlloc.e",0,E_other,1)
    initialAutoEntry("and_bitsu",               S_Func,"FNN",   "ubits.e",0,E_none)
    initialAutoEntry("arccos",                  S_Func,"FN",    "misc.e",0,E_none)
    initialAutoEntry("arcsin",                  S_Func,"FN",    "misc.e",0,E_none)
    initialAutoEntry("atan2",                   S_Func,"FNN",   "pmaths.e",0,E_none)
    initialAutoEntry("bytes_to_int",            S_Func,"FPI",   "machine.e",0,E_none,1)
    initialAutoEntry("bits_to_int",             S_Func,"FP",    "machine.e",0,E_none)
    initialAutoEntry("ceil",                    S_Func,"FN",    "pmaths.e",0,E_none)
--if newEmit then
    --DEV or is c_func an object result?? (eg C:\Program Files\Phix\demo\SUDOKU\EuWinGUI.ew:359)
    initialAutoEntry("c_func",                  S_Func,"FIP",   "VM\\pcfunc.e",0,E_all)
    initialAutoEntry("call_back",               S_Func,"FO",    "VM\\pcfunc.e",0,E_none)
    initialAutoEntry("create_thread",           S_Func,"FIPI",  "VM\\pThreadN.e",0,E_other,2)
    initialAutoEntry("define_c_var",            S_Func,"FNS",   "VM\\pcfunc.e",0,E_none)
    initialAutoEntry("even",                    S_Func,"FN",    "pmaths.e",0,E_none)
    initialAutoEntry("exp",                     S_Func,"FN",    "pmaths.e",0,E_none)
    initialAutoEntry("float32_to_atom",         S_Func,"FP",    "VM\\pFloatN.e",0,E_none)
    initialAutoEntry("float64_to_atom",         S_Func,"FP",    "VM\\pFloatN.e",0,E_none)
    initialAutoEntry("float80_to_atom",         S_Func,"FP",    "VM\\pFloatN.e",0,E_none)
    initialAutoEntry("get_prime",               S_Func,"FI",    "primes.e",0,E_none)
    initialAutoEntry("get_proc_address",        S_Func,"FNS",   "VM\\pcfunc.e",0,E_none)
    initialAutoEntry("get_rand",                S_Func,"F",     "prnd.e",0,E_none)
    initialAutoEntry("hash",                    S_Func,"FON",   "hash.e",0,E_none,1)
    initialAutoEntry("hll_and_bits",            S_Func,"FNN",   "hll_stubs.e",0,E_none)     hll_stubs[T_and_bits] = symlimit
    initialAutoEntry("hll_not_bits",            S_Func,"FN",    "hll_stubs.e",0,E_none)     hll_stubs[T_not_bits] = symlimit
    initialAutoEntry("hll_or_bits",             S_Func,"FNN",   "hll_stubs.e",0,E_none)     hll_stubs[T_or_bits] = symlimit
    initialAutoEntry("hll_xor_bits",            S_Func,"FNN",   "hll_stubs.e",0,E_none)     hll_stubs[T_xor_bits] = symlimit
    initialAutoEntry("hll_remainder",           S_Func,"FNN",   "hll_stubs.e",0,E_none)     hll_stubs[T_remainder] = symlimit
    initialAutoEntry("hll_floor",               S_Func,"FN",    "hll_stubs.e",0,E_none)     hll_stubs[T_floor] = symlimit
    initialAutoEntry("hll_power",               S_Func,"FNN",   "hll_stubs.e",0,E_none)     hll_stubs[T_power] = symlimit
    initialAutoEntry("hll_cos",                 S_Func,"FN",    "hll_stubs.e",0,E_none)     hll_stubs[T_cos] = symlimit
    initialAutoEntry("hll_sin",                 S_Func,"FN",    "hll_stubs.e",0,E_none)     hll_stubs[T_sin] = symlimit
    initialAutoEntry("hll_tan",                 S_Func,"FN",    "hll_stubs.e",0,E_none)     hll_stubs[T_tan] = symlimit
    initialAutoEntry("hll_arctan",              S_Func,"FN",    "hll_stubs.e",0,E_none)     hll_stubs[T_arctan] = symlimit
    initialAutoEntry("hll_log",                 S_Func,"FN",    "hll_stubs.e",0,E_none)     hll_stubs[T_log] = symlimit
    initialAutoEntry("hll_sqrt",                S_Func,"FN",    "hll_stubs.e",0,E_none)     hll_stubs[T_sqrt] = symlimit
    initialAutoEntry("hll_rand",                S_Func,"FI",    "hll_stubs.e",0,E_none)     hll_stubs[T_rand] = symlimit
    initialAutoEntry("open_dll",                S_Func,"FP",    "VM\\pcfunc.e",0,E_none)
--else
--  initialAutoEntry("float80_to_atom",         S_Func,"FP",    "pfloat.e",0,E_none)
--  initialAutoEntry("c_func",                  S_Func,"FIP",   "pcfunc.e",0,E_all)
--  initialAutoEntry("call_back",               S_Func,"FO",    "pcfunc.e",0,E_none)
--  initialAutoEntry("define_c_var",            S_Func,"FNS",   "pcfunc.e",0,E_none)
--  initialAutoEntry("open_dll",                S_Func,"FP",    "pcfunc.e",0,E_none)
--end if
    initialAutoEntry("choose",                  S_Func,"FII",   "factorial.e",0,E_none)
    initialAutoEntry("factorial",               S_Func,"FN",    "factorial.e",0,E_none)
    initialAutoEntry("gcd",                     S_Func,"FON",   "gcd.e",0,E_none,1)
    initialAutoEntry("get_file_size",           S_Func,"FSII",  "pfile.e",0,E_none,1)
    Alias("file_length", symlimit)
    initialAutoEntry("get_thread_exitcode",     S_Func,"FN","VM\\pThreadN.e",0,E_none)
    initialAutoEntry("k_perm",                  S_Func,"FII",   "factorial.e",0,E_none)
    initialAutoEntry("lcm",                     S_Func,"FON",   "gcd.e",0,E_none,1)
    initialAutoEntry("log10",                   S_Func,"FN",    "log10.e",0,E_none)
    initialAutoEntry("log2",                    S_Func,"FN",    "log10.e",0,E_none)
    initialAutoEntry("mod",                     S_Func,"FNN",   "pmaths.e",0,E_none)
    initialAutoEntry("not_bitsu",               S_Func,"FN",    "ubits.e",0,E_none)
    initialAutoEntry("odd",                     S_Func,"FN",    "pmaths.e",0,E_none)
    initialAutoEntry("or_all",                  S_Func,"FO",    "porall.e",0,E_none)
    initialAutoEntry("or_allu",                 S_Func,"FO",    "porall.e",0,E_none)
    initialAutoEntry("or_bitsu",                S_Func,"FNN",   "ubits.e",0,E_none)
    initialAutoEntry("poke_string",             S_Func,"FNIP",  "pokestr.e",0,E_other)
    initialAutoEntry("poke_wstring",            S_Func,"FNIP",  "pokestr.e",0,E_other)
    initialAutoEntry("product",                 S_Func,"FON",   "psum.e",0,E_none,1)
    initialAutoEntry("prompt_number",           S_Func,"FSP",   "get.e",0,E_other,1)
    initialAutoEntry("rnd",                     S_Func,"F",     "prnd.e",0,E_none)
    initialAutoEntry("round",                   S_Func,"FNN",   "pmaths.e",0,E_none,1)
    initialAutoEntry("sign",                    S_Func,"FN",    "pmaths.e",0,E_none)
    initialAutoEntry("sum",                     S_Func,"FON",   "psum.e",0,E_none,1)
--  initialAutoEntry("sysexec",                 S_Func,"FP",    "syswait.ew",0,E_other)
    initialAutoEntry("system_exec",             S_Func,"FPI",   "syswait.ew",0,E_other,1)
    initialAutoEntry("system_wait",             S_Func,"FP",    "syswait.ew",0,E_other)
    initialAutoEntry("trunc",                   S_Func,"FN",    "pmaths.e",0,E_none)
    initialAutoEntry("xor_bitsu",               S_Func,"FNN",   "ubits.e",0,E_none)

    T_AAtm = symlimit

    -- the following return a string
    IAEType = T_string

    initialAutoEntry("atom_to_float32",         S_Func,"FN",    "VM\\pFloatN.e",0,E_none)
    initialAutoEntry("atom_to_float64",         S_Func,"FN",    "VM\\pFloatN.e",0,E_none)
    initialAutoEntry("atom_to_float80",         S_Func,"FN",    "VM\\pFloatN.e",0,E_none)
    initialAutoEntry("current_dir",             S_Func,"F",     "pcurrdir.e",0,E_none)
--  initialAutoEntry("current_dirN",            S_Func,"F",     "VM\\pcurrdirN.e",0,E_none)
    initialAutoEntry("decode_flags",            S_Func,"FPNS",  "pdecodeflags.e",0,E_none,2)
    initialAutoEntry("decode_base64",           S_Func,"FP",    "base64.e",0,E_none)
    initialAutoEntry("dict_name",               S_Func,"FI",    "dict.e",0,E_none,0)
    initialAutoEntry("encode_base64",           S_Func,"FPI",   "base64.e",0,E_none,1)
    initialAutoEntry("elapsed",                 S_Func,"FNNS",  "pelapsed.e",0,E_none,1)
    initialAutoEntry("elapsed_short",           S_Func,"FNNS",  "pelapsed.e",0,E_none,1)
    initialAutoEntry("file_size_k",             S_Func,"FNI",   "pfile.e",0,E_none,1)
    initialAutoEntry("get_file_base",           S_Func,"FS",    "pfile.e",0,E_none)
    Alias("filebase", symlimit)
    initialAutoEntry("get_file_path",           S_Func,"FSI",   "pfile.e",0,E_none,1)
--added 31/12/16:(!!)
    Alias("pathname", symlimit)
    initialAutoEntry("get_file_extension",      S_Func,"FS",    "pfile.e",0,E_none)
    Alias("fileext", symlimit)
    initialAutoEntry("get_file_name",           S_Func,"FS",    "pfile.e",0,E_none)
--removed 25/11/16
--  Alias("filename", symlimit)
    initialAutoEntry("get_interpreter",         S_Func,"FIOII", "get_interpreter.e",0,E_none,1)
    initialAutoEntry("get_proper_path",         S_Func,"FPO",   "pgetpath.e",0,E_none,1)
    initialAutoEntry("get_proper_dir",          S_Func,"FSI",   "pgetpath.e",0,E_none,1)
    initialAutoEntry("canonical_path",          S_Func,"FSII",  "pgetpath.e",0,E_none,1)
    initialAutoEntry("include_path",            S_Func,"FP",    "pincpathN.e",0,E_none, 0)
    initialAutoEntry("ord",                     S_Func,"FI",    "ordinal.e",0,E_none)
    initialAutoEntry("ordinal",                 S_Func,"FII",   "ordinal.e",0,E_none,1)
    initialAutoEntry("peek_string",             S_Func,"FN",    "peekstr.e",0,E_none)
    initialAutoEntry("prompt_string",           S_Func,"FS",    "get.e",0,E_other)
    initialAutoEntry("proper",                  S_Func,"FSS",   "pcase.e",0,E_none,1)
    initialAutoEntry("repeatch",                S_Func,"FIII",  "repeat.e",0,E_other,2)     T_repeatch = symlimit
    initialAutoEntry("shorten",                 S_Func,"FPSI",  "ptrim.e",0,E_other,1)
    initialAutoEntry("sprintf",                 S_Func,"FPO",   "VM\\pprntfN.e",0,E_none,1)
--  initialAutoEntry("sprint",                  S_Func,"FOII",  "VM\\psprintN.e",0,E_none,1)
    initialAutoEntry("sprint",                  S_Func,"FOIII", "VM\\pprntfN.e",0,E_none,1)
    initialAutoEntry("thread_safe_string",      S_Func,"FS",    "VM\\pThreadN.e",0,E_none)
    initialAutoEntry("to_string",               S_Func,"FOII",  "to_str.e",0,E_none,1)
    initialAutoEntry("utf16_to_utf8",           S_Func,"FP",    "utfconv.e",0,E_none)

    T_AStr = symlimit

    -- the following return a sequence
    IAEType = T_sequence

    initialAutoEntry("apply",                   S_Func,"FOIO",  "pApply.e",0,E_all, 2)
    initialAutoEntry("columnize",               S_Func,"FPOO",  "pcolumn.e",0,E_none)
    initialAutoEntry("command_line",            S_Func,"F",     "VM\\pcmdlnN.e",0,E_none)   T_command_line = symlimit
                                                                                            Z_command_line = 0
--DEV Eu4 has another 2 optional params..
--  initialAutoEntry("custom_sort",             S_Func,"FIP",   "sort.e",0,E_none)
    initialAutoEntry("custom_sort",             S_Func,"FOPOI", "sort.e",0,E_none,2)
    initialAutoEntry("sort_columns",            S_Func,"FPP",   "sort.e",0,E_none)
    initialAutoEntry("date",                    S_Func,"FI",    "pdate.e",0,E_none,1)
    initialAutoEntry("db_table_list",           S_Func,"F",     "database.e",0,E_none)
    initialAutoEntry("extract",                 S_Func,"FPPI",  "pextract.e",0,E_none,2)
    initialAutoEntry("factors",                 S_Func,"FNI",   "pfactors.e",0,E_none,1)
    initialAutoEntry("filter",                  S_Func,"FPOOS", "pFilter.e",0,E_none,2)
    initialAutoEntry("find_all",                S_Func,"FOPI",  "pfindall.e",0,E_none,2)
    initialAutoEntry("find_replace",            S_Func,"FOPOI", "findrepl.e",0,E_none,3)
    initialAutoEntry("flatten",                 S_Func,"FPP",   "pflatten.e",0,E_none,1)
    initialAutoEntry("get_file_path_and_name",  S_Func,"FSI",   "pfile.e",0,E_none,1)
    initialAutoEntry("get_file_name_and_path",  S_Func,"FSI",   "pfile.e",0,E_none,1)
    initialAutoEntry("get_logical_drives",      S_Func,"FP",    "pfile.e",   0,E_none)
    initialAutoEntry("get_primes",              S_Func,"FI",    "primes.e",  0,E_none,0)
    initialAutoEntry("get_primes_le",           S_Func,"FI",    "primes.e",  0,E_none)
    initialAutoEntry("get_routine_info",        S_Func,"FII",   "get_routine_info.e",0,E_none,1)
--sequence/string result:
    initialAutoEntry("hll_append",              S_Func,"FOO",   "hll_stubs.e",0,E_none)     hll_stubs[Z_append] = symlimit
    initialAutoEntry("hll_prepend",             S_Func,"FOO",   "hll_stubs.e",0,E_none)     hll_stubs[Z_prepend] = symlimit
--(killed off early)
--  initialAutoEntry("hll_repeat",              S_Func,"FOI",   "hll_stubs.e",0,E_none)     hll_stubs[T_repeat] = symlimit
    initialAutoEntry("join",                    S_Func,"FPO",   "pflatten.e",0,E_none,1)
    initialAutoEntry("join_by",                 S_Func,"FPIIOO","pflatten.e",0,E_none,3)
    initialAutoEntry("join_path",               S_Func,"FPI",   "pflatten.e",0,E_none,1)
--if newEmit then
----    initialAutoEntry("get_position",        S_Func,"F",     "VM\\pfileioN.e",0,E_none)
--elsif hllfileio then
--  initialAutoEntry("get_position",            S_Func,"F",     "pfileio.e",0,E_none)
--end if
    initialAutoEntry("get",                     S_Func,"FI",    "get.e",0,E_other)
    initialAutoEntry("get_bytes",               S_Func,"FII",   "get.e",0,E_other)
    initialAutoEntry("head",                    S_Func,"FPN",   "pseqc.e",0,E_none,1)
--if newEmit then
    initialAutoEntry("include_files",           S_Func,"F",     "pincpathN.e",0,E_none)
    initialAutoEntry("include_paths",           S_Func,"FI",    "pincpathN.e",0,E_none,0)
--else
--  initialAutoEntry("include_paths",           S_Func,"FI",    "pincpath.e",0,E_none,0)
--end if
    initialAutoEntry("insert",                  S_Func,"FPOI",  "pseqc.e",0,E_none)
    initialAutoEntry("int_to_bytes",            S_Func,"FNI",   "machine.e",0,E_none,1)
    initialAutoEntry("int_to_bits",             S_Func,"FNI",   "machine.e",0,E_none,1)
--  initialAutoEntry("match_all",               S_Func,"FOPIII","VM\\pMatch.e",0,E_none,2)
    initialAutoEntry("match_all",               S_Func,"FOPIII","match.e",0,E_none,2)
    initialAutoEntry("match_replace",           S_Func,"FOPOI", "matchrepl.e",0,E_none,3)
    initialAutoEntry("new",                     S_Func,"FOP",   "structs.e",0,E_other,1)        T_new = symlimit
    initialAutoEntry("pad",                     S_Func,"FSISI", "pseqc.e",0,E_none,2)
    initialAutoEntry("pad_head",                S_Func,"FSII",  "pseqc.e",0,E_none,2)
    initialAutoEntry("pad_tail",                S_Func,"FSII",  "pseqc.e",0,E_none,2)
    initialAutoEntry("peek_wstring",            S_Func,"FN",    "peekstr.e",0,E_none)
    initialAutoEntry("permute",                 S_Func,"FIP",   "permute.e",0,E_none)
    initialAutoEntry("peep_dict",               S_Func,"FII",   "dict.e",0,E_none,0)
    initialAutoEntry("pop_dict",                S_Func,"FII",   "dict.e",0,E_none,0)
    initialAutoEntry("prime_factors",           S_Func,"FNII",  "pfactors.e",0,E_none,1)
    initialAutoEntry("pq_pop",                  S_Func,"FI",    "pqueue.e",0,E_other,0)
    initialAutoEntry("pq_peek",                 S_Func,"FI",    "pqueue.e",0,E_other,0)
    initialAutoEntry("reinstate",               S_Func,"FPPPI", "pextract.e",0,E_none, 3)
    initialAutoEntry("remove",                  S_Func,"FPNN",  "pseqc.e",0,E_none,2)
    initialAutoEntry("remove_all",              S_Func,"FOP",   "premoveall.e",0,E_none)
    initialAutoEntry("replace",                 S_Func,"FPONN", "pseqc.e",0,E_none,3)
    initialAutoEntry("repeat",                  S_Func,"FOI",   "repeat.e",0,E_other)       T_repeat = symlimit
    initialAutoEntry("replace",                 S_Func,"FPONN", "pseqc.e",0,E_none,3)
    initialAutoEntry("reverse",                 S_Func,"FPP",   "misc.e",0,E_none,1)
    initialAutoEntry("scanf",                   S_Func,"FSS",   "scanf.e",0,E_none)
    initialAutoEntry("serialize",               S_Func,"FO",    "serialize.e",0,E_none)
    initialAutoEntry("shuffle",                 S_Func,"FP",    "shuffle.e",0,E_none)
    initialAutoEntry("sort",                    S_Func,"FPI",   "sort.e",0,E_none,1)
    initialAutoEntry("splice",                  S_Func,"FPOI",  "pseqc.e",0,E_none)
    initialAutoEntry("split",                   S_Func,"FPOII", "psplit.e",0,E_none,1)
    initialAutoEntry("split_any",               S_Func,"FPOII", "psplit.e",0,E_none,1)
    initialAutoEntry("split_by",                S_Func,"FPI",   "psplit.e",0,E_none)
    initialAutoEntry("split_path",              S_Func,"FPI",   "psplit.e",0,E_none,1)
    initialAutoEntry("substitute",              S_Func,"FPOOI", "substitute.e",0,E_none,3)
    initialAutoEntry("substitute_all",          S_Func,"FPOO",  "substitute.e",0,E_none)
    initialAutoEntry("tagset",                  S_Func,"FIIII", "ptagset.e",0,E_none,1)
    initialAutoEntry("tail",                    S_Func,"FPN",   "pseqc.e",0,E_none,1)
    initialAutoEntry("task_list",               S_Func,"F",     "VM\\pTask.e",0,E_none)
    initialAutoEntry("unique",                  S_Func,"FPS",   "punique.e",0,E_none,1)
    initialAutoEntry("utf8_to_utf16",           S_Func,"FP",    "utfconv.e",0,E_none)
    initialAutoEntry("utf16_to_utf32",          S_Func,"FP",    "utfconv.e",0,E_none)
    initialAutoEntry("utf32_to_utf16",          S_Func,"FP",    "utfconv.e",0,E_none)
    initialAutoEntry("ppf",                     S_Func,"FOP",   "ppp.e",0,E_none,1)
    initialAutoEntry("ppExf",                   S_Func,"FOP",   "ppp.e",0,E_none)
    initialAutoEntry("value",                   S_Func,"FP",    "get.e",0,E_none)
    initialAutoEntry("video_config",            S_Func,"F",     "pscreen.e",0,E_none)
    initialAutoEntry("vslice",                  S_Func,"FPOO",  "vslice.e",0,E_none)
    initialAutoEntry("get_screen_char",         S_Func,"FII",   "pscreen.e",0,E_other)

    T_ASeq = symlimit

    -- the following return an object
    IAEType = T_object

if newEmit then
--  initialAutoEntry("call_func",           S_Func,"FIP",   "VM\\pcfunc.e",0,E_all)
--  initialAutoEntry("call_func",           S_Func,"FIP",   "VM\\pcallfunc.e",0,E_all)
--  initialAutoEntry("call_funcN",          S_Func,"FIP",   "VM\\pcallfunc.e",0,E_all)
else
    initialAutoEntry("call_func",           S_Func,"FIP",   "pcfunc.e",0,E_all)
end if
    initialAutoEntry("day_of_week",         S_Func,"FIIII", "pdates.e",0,E_none,3)
    initialAutoEntry("db_record_data",      S_Func,"FI",    "database.e",0,E_none)
    initialAutoEntry("db_record_key",       S_Func,"FI",    "database.e",0,E_none)
    initialAutoEntry("deep_copy",           S_Func,"FOII",  "repeat.e",0,E_other,1)
if newEmit then
--  initialAutoEntry("delete_routine",      S_Func,"FOI",   "VM\\pDeleteN.e",0,E_other)
else
    initialAutoEntry("delete_routine",      S_Func,"FOI",   "pdelete.e",0,E_other)
end if
    initialAutoEntry("deserialize",         S_Func,"FOII",  "serialize.e",0,E_none,1)
    initialAutoEntry("dir",                 S_Func,"FPI",   "pdir.e",0,E_none,1)
    initialAutoEntry("fetch_field",         S_Func,"FPS",   "structs.e",0,E_none)       T_fetch_field = symlimit
    initialAutoEntry("ffree",               S_Func,"FO",    "pAlloc.e",0,E_other)       T_ffree = symlimit
    initialAutoEntry("get_field_flags",     S_Func,"FOSI",  "structs.e",0,E_none,2)
    initialAutoEntry("get_field_type",      S_Func,"FOSI",  "structs.e",0,E_none,2) --T_field_type = symlimit
    initialAutoEntry("get_file_date",       S_Func,"FPI",   "pfile.e",0,E_none,1)
    initialAutoEntry("get_text",            S_Func,"FOI",   "pfile.e",0,E_none,1)
    initialAutoEntry("getd",                S_Func,"FOI",   "dict.e",0,E_none,1)
    initialAutoEntry("getdd",               S_Func,"FOOI",  "dict.e",0,E_none,2)
    initialAutoEntry("getd_all_keys",       S_Func,"FI",    "dict.e",0,E_none,0)
    initialAutoEntry("getd_by_index",       S_Func,"FII",   "dict.e",0,E_none,1)
    initialAutoEntry("getd_partial_key",    S_Func,"FOII",  "dict.e",0,E_none,1)
    initialAutoEntry("getenv",              S_Func,"FS",    "penv.e",0,E_none)
--  if newEmit then
--      -- ("gets" done as AutoAsm above)
--  elsif hllfileio then
--      initialAutoEntry("gets",            S_Func,"FO",    "pfileio.e",0,E_other)  -- ("FI" for newEmit, btw)
--  end if
    initialAutoEntry("machine_func",        S_Func,"FIO",   "pmach.e",0,E_other)
    initialAutoEntry("max",                 S_Func,"FOO",   "pmaths.e",0,E_none)        T_max = symlimit
    initialAutoEntry("maxsq",               S_Func,"FPI",   "pmaths.e",0,E_none,1)      T_maxsq = symlimit
    initialAutoEntry("min",                 S_Func,"FOO",   "pmaths.e",0,E_none)        T_min = symlimit
    initialAutoEntry("minsq",               S_Func,"FPI",   "pmaths.e",0,E_none,1)      T_minsq = symlimit
    initialAutoEntry("peekns",              S_Func,"FOII",  "peekns.e",0,E_other,1)
    initialAutoEntry("peeknu",              S_Func,"FOII",  "peekns.e",0,E_other,1)
    initialAutoEntry("pq_pop_data",         S_Func,"FI",    "pqueue.e",0,E_other,0)
    initialAutoEntry("read_bitmap",         S_Func,"FS",    "image.e",0,E_none)
--  initialAutoEntry("round",               S_Func,"FOO",   "pmaths.e",0,E_none,1)
    initialAutoEntry("save_text_image",     S_Func,"FPP",   "pscreen.e",0,E_none)
--DEV... (delete?? [TlsGetValue])
    initialAutoEntry("TlsGetValue",         S_Func,"FI",    "ptls.ew",0,E_none)

    initialAutoEntry("upper",               S_Func,"FO",    "pcase.e",0,E_none)
    initialAutoEntry("lower",               S_Func,"FO",    "pcase.e",0,E_none)
--if newEmit then
----    initialAutoEntry("upperN",          S_Func,"FO",    "VM\\pcaseN.e",0,E_none)
----    initialAutoEntry("lowerN",          S_Func,"FO",    "VM\\pcaseN.e",0,E_none)
--  initialAutoEntry("peek2sN",             S_Func,"FO",    "VM\\ppoke2N.e",0,E_none)
--  initialAutoEntry("peek2uN",             S_Func,"FO",    "VM\\ppoke2N.e",0,E_none)
--else
--  initialAutoEntry("upper",               S_Func,"FO",    "pcase.e",0,E_none)
--  initialAutoEntry("lower",               S_Func,"FO",    "pcase.e",0,E_none)
if newEmit then
--DEV "get_text" is still missing above (via optable) [added]
else
    initialAutoEntry("get_text",            S_Func,"FII",   "pgettext.e",0,E_none)  -- (temp-ish)
    initialAutoEntry("peek2s",              S_Func,"FO",    "ppoke2.e",0,E_none)
    initialAutoEntry("peek2u",              S_Func,"FO",    "ppoke2.e",0,E_none)
end if
--end if
--  initialAutoEntry("read_lines",          S_Func,"FO",    "read_lines.e",0,E_none)
    initialAutoEntry("read_lines",          S_Func,"FO",    "pfile.e",0,E_other)
    initialAutoEntry("series",              S_Func,"FOOII", "pseries.e",0,E_none,2)
    initialAutoEntry("shift_bits",          S_Func,"FOI",   "shift_bits.e",0,E_none)
    initialAutoEntry("largest",             S_Func,"FPI",   "psmall.e",0,E_none)
    initialAutoEntry("smallest",            S_Func,"FPI",   "psmall.e",0,E_none)
    initialAutoEntry("set_file_size",       S_Func,"FSN",   "pfile.e",0,E_other)

    --DEV 23/3 we do /not/ want these 10 auto-converted to sq_xxx()...
    --29/4/2010 but keep elsewhere
    do6 = 1
    initialAutoEntry("sq_eq",               S_Func,"FOO",   "psqop.e",opJeq,E_none) --23/3: opSeq)
    initialAutoEntry("sq_ne",               S_Func,"FOO",   "psqop.e",opJne,E_none) --23/3: opSne)
    initialAutoEntry("sq_lt",               S_Func,"FOO",   "psqop.e",opJlt,E_none) --23/3: opSlt)
    initialAutoEntry("sq_le",               S_Func,"FOO",   "psqop.e",opJle,E_none) --23/3: opSle)
    initialAutoEntry("sq_gt",               S_Func,"FOO",   "psqop.e",opJgt,E_none) --23/3: opSgt)
    initialAutoEntry("sq_ge",               S_Func,"FOO",   "psqop.e",opJge,E_none) --23/3: opSge)
    do6 = 0
    initialAutoEntry("sq_int",              S_Func,"FO",    "psqop.e",0,E_none) --23/3: opInt)
    initialAutoEntry("sq_atom",             S_Func,"FO",    "psqop.e",0,E_none) --23/3: opAtom)
    initialAutoEntry("sq_str",              S_Func,"FO",    "psqop.e",0,E_none) --23/3: opStr)
    initialAutoEntry("sq_seq",              S_Func,"FO",    "psqop.e",0,E_none) --23/3: opSq)
    initialAutoEntry("sq_abs",              S_Func,"FO",    "psqop.e",0,E_none)         --T_sqabs = symlimit
    initialAutoEntry("sq_add",              S_Func,"FOO",   "psqop.e",opAdd,E_none)
    initialAutoEntry("sq_sub",              S_Func,"FOO",   "psqop.e",opSub,E_none)
    initialAutoEntry("sq_mul",              S_Func,"FOO",   "psqop.e",opMul,E_none)
    initialAutoEntry("sq_div",              S_Func,"FOO",   "psqop.e",opDiv,E_none)
    initialAutoEntry("sq_floor_div",        S_Func,"FOO",   "psqop.e",opDivf,E_none)    T_sqfloor_div = symlimit
    initialAutoEntry("sq_rmdr",             S_Func,"FOO",   "psqop.e",opRmdr,E_none)
    initialAutoEntry("sq_floor",            S_Func,"FO",    "psqop.e",opFloor,E_none)   T_sqfloor = symlimit
    initialAutoEntry("sq_round",            S_Func,"FOO",   "psqop.e",0,E_none,1)
    initialAutoEntry("sq_ceil",             S_Func,"FO",    "psqop.e",0,E_none)
    initialAutoEntry("sq_cmp",              S_Func,"FOO",   "psqop.e",0,E_none)
    initialAutoEntry("sq_odd",              S_Func,"FO",    "psqop.e",0,E_none)
    initialAutoEntry("sq_even",             S_Func,"FO",    "psqop.e",0,E_none)
    initialAutoEntry("sq_sign",             S_Func,"FO",    "psqop.e",0,E_none)
    initialAutoEntry("sq_mod",              S_Func,"FOO",   "psqop.e",0,E_none)
    initialAutoEntry("sq_trunc",            S_Func,"FO",    "psqop.e",0,E_none)
    initialAutoEntry("sq_and",              S_Func,"FOO",   "psqop.e",0,E_none)
    initialAutoEntry("sq_or",               S_Func,"FOO",   "psqop.e",0,E_none)
    initialAutoEntry("sq_xor",              S_Func,"FOO",   "psqop.e",opXor,E_none)
    initialAutoEntry("sq_rand",             S_Func,"FO",    "psqop.e",opRand,E_none)
    initialAutoEntry("sq_rnd",              S_Func,"FO",    "prnd.e",0,E_none)
    initialAutoEntry("sq_uminus",           S_Func,"FO",    "psqop.e",opUminus,E_none)
    initialAutoEntry("sq_not",              S_Func,"FO",    "psqop.e",opNot,E_none)
    initialAutoEntry("sq_and_bits",         S_Func,"FOO",   "psqop.e",opAndBits,E_none)
    initialAutoEntry("sq_or_bits",          S_Func,"FOO",   "psqop.e",opOrBits,E_none)
    initialAutoEntry("sq_xor_bits",         S_Func,"FOO",   "psqop.e",opXorBits,E_none)
    initialAutoEntry("sq_not_bits",         S_Func,"FO",    "psqop.e",opNotBits,E_none)
    initialAutoEntry("sq_cos",              S_Func,"FO",    "psqop.e",opCos,E_none)
    initialAutoEntry("sq_sin",              S_Func,"FO",    "psqop.e",opSin,E_none)
    initialAutoEntry("sq_tan",              S_Func,"FO",    "psqop.e",opTan,E_none)
    initialAutoEntry("sq_arccos",           S_Func,"FO",    "psqop.e",0,E_none) -- no implicts
    initialAutoEntry("sq_arcsin",           S_Func,"FO",    "psqop.e",0,E_none) -- no implicts
    initialAutoEntry("sq_arctan",           S_Func,"FO",    "psqop.e",opArcTan,E_none)
    initialAutoEntry("sq_log",              S_Func,"FO",    "psqop.e",opLog,E_none)
    initialAutoEntry("sq_log10",            S_Func,"FO",    "psqop.e",0,E_none)
    initialAutoEntry("sq_log2",             S_Func,"FO",    "psqop.e",0,E_none)
    initialAutoEntry("sq_power",            S_Func,"FOO",   "psqop.e",opPow,E_none)
    initialAutoEntry("sq_sqrt",             S_Func,"FO",    "psqop.e",opSqrt,E_none)
-- removed 31/3/21 (p2js):
--  initialAutoEntry("sq_upper",            S_Func,"FO",    "psqop.e",0,E_none)
--  initialAutoEntry("sq_lower",            S_Func,"FO",    "psqop.e",0,E_none)
    initialAutoEntry("sq_min",              S_Func,"FOO",   "psqop.e",0,E_none)
    initialAutoEntry("sq_max",              S_Func,"FOO",   "psqop.e",0,E_none)

    initialAutoEntry("temp_file",           S_Func,"FPPPP", "pfile.e",0,E_other,0)
    initialAutoEntry("to_number",           S_Func,"FSOI",  "scanf.e",0,E_none,1)
    initialAutoEntry("trim",                S_Func,"FOOI",  "ptrim.e",0,E_none,1)
    initialAutoEntry("trim_head",           S_Func,"FOOI",  "ptrim.e",0,E_none,1)
    initialAutoEntry("trim_tail",           S_Func,"FOOI",  "ptrim.e",0,E_none,1)
    initialAutoEntry("utf8_to_utf32",       S_Func,"FSI",   "utfconv.e",0,E_none,1)
    initialAutoEntry("utf32_to_utf8",       S_Func,"FPI",   "utfconv.e",0,E_none,1)
    initialAutoEntry("vlookup",             S_Func,"FOPIIO","pvlookup.e",0,E_none,4)
    initialAutoEntry("walk_dir",            S_Func,"FPIII", "file.e",0,E_all,2)

    -- the remainder are procedures

--  initialAutoEntry("add_block",           S_Proc,"P",     "primes.e",0,E_none)    -- removed 23/2/20 (not global...)
    initialAutoEntry("allow_break",         S_Proc,"PI",    "pbreak.e",0,E_other)
    initialAutoEntry("any_key",             S_Proc,"PSI",   "panykey.e",0,E_other,1)
    initialAutoEntry("assert",              S_Proc,"PISP",  "assert.e",0,E_other,1)
    initialAutoEntry("maybe_any_key",       S_Proc,"PSI",   "panykey.e",0,E_other,1)
--  if newEmit then
----        initialAutoEntry("abort",           S_Proc,"PI",    "VM\\pAbort.e",0,E_other)               T_abort = symlimit
----        initialAutoEntry("bk_color",        S_Proc,"PI",    "VM\\pfileioN.e",0,E_other)
----        initialAutoEntry("clear_screen",    S_Proc,"P",     "VM\\pfileioN.e",0,E_other)
----        initialAutoEntry("close",           S_Proc,"PI",    "VM\\pfileioN.e",0,E_other)
--  elsif hllfileio then
--      initialAutoEntry("bk_color",        S_Proc,"PI",    "pfileio.e",0,E_other)
--      initialAutoEntry("clear_screen",    S_Proc,"P",     "pfileio.e",0,E_other)
--      initialAutoEntry("close",           S_Proc,"PI",    "pfileio.e",0,E_other)
--  end if
--if newEmit then
    initialAutoEntry("c_proc",              S_Proc,"PIP",   "VM\\pcfunc.e",0,E_all)
    initialAutoEntry("call",                S_Proc,"PN",    "VM\\pcfunc.e",0,E_all)
--  initialAutoEntry("call_proc",           S_Proc,"PIP",   "VM\\pcallfunc.e",0,E_all)  -- now opCallProc
    initialAutoEntry("crash",               S_Proc,"PPOI",  "pCrashN.e",0,E_other)
    symtab[symlimit][S_ParmN] = 1
--  initialAutoEntry("crash_file",          S_Proc,"PO",    "VM\\pDiagN.e",0,E_other)   --DEV removed 14/12/16... (now opCrashFile?)
--  initialAutoEntry("crash_message",       S_Proc,"PO",    "VM\\pDiagN.e",0,E_other)   -- now opCrashMsg
--else
--  initialAutoEntry("c_proc",              S_Proc,"PIP",   "pcfunc.e",0,E_all)
--  initialAutoEntry("call",                S_Proc,"PN",    "pcfunc.e",0,E_all)
--  initialAutoEntry("call_proc",           S_Proc,"PIP",   "pcfunc.e",0,E_all)
--  initialAutoEntry("crash",               S_Proc,"PPO",   "pcrash.e",0,E_other,1)
--end if
    initialAutoEntry("cursor",              S_Proc,"PI",    "pscreen.e",0,E_other)
    initialAutoEntry("db_dump",             S_Proc,"PII",   "database.e",0,E_other)
    initialAutoEntry("check_free_list",     S_Proc,"P",     "database.e",0,E_other)
    initialAutoEntry("db_close",            S_Proc,"P",     "database.e",0,E_other)
    initialAutoEntry("db_delete_table",     S_Proc,"PS",    "database.e",0,E_other)
    initialAutoEntry("db_rename_table",     S_Proc,"PSS",   "database.e",0,E_other)
    initialAutoEntry("db_delete_record",    S_Proc,"PI",    "database.e",0,E_other)
    initialAutoEntry("db_replace_data",     S_Proc,"PIO",   "database.e",0,E_other)
    initialAutoEntry("deld",                S_Proc,"POI",   "dict.e",0,E_other,1)
--if newEmit then
----    initialAutoEntry("delete",              S_Proc,"PO",    "VM\\pDeleteN.e",0,E_other)
--else
--  initialAutoEntry("delete",              S_Proc,"PO",    "pdelete.e",0,E_other)
--end if
    initialAutoEntry("destroy_dict",        S_Proc,"PII",   "dict.e",0,E_other,1)
    initialAutoEntry("display_text_image",  S_Proc,"PPP",   "pscreen.e",0,E_other)
    initialAutoEntry("end_struct",          S_Proc,"P",     "structs.e",0,E_other)              T_end_struct = symlimit
    initialAutoEntry("extend_struct",       S_Proc,"PSS",   "structs.e",0,E_other,1)            T_extend_struct = symlimit
--if newEmit then
    initialAutoEntry("exit_thread",         S_Proc,"PI",    "VM\\pThreadN.e",0,E_other)
    initialAutoEntry("free",                S_Proc,"PO",    "pAlloc.e",0,E_other)               T_free = symlimit
--  initialAutoEntry("poke2N",              S_Proc,"PNO",   "VM\\ppoke2N.e",0,E_other)
--else
--  initialAutoEntry("poke2",               S_Proc,"PNO",   "ppoke2.e",0,E_other)
--end if
    initialAutoEntry("put_screen_char",     S_Proc,"PNNP",  "pscreen.e",0,E_other)
--if newEmit then
----    initialAutoEntry("flush",               S_Proc,"PI",    "VM\\pfileioN.e",0,E_other)
----    initialAutoEntry("free_console",        S_Proc,"P",     "VM\\pfileioN.e",0,E_other)
--elsif hllfileio then
--  initialAutoEntry("flush",               S_Proc,"PI",    "pfileio.e",0,E_other)
--  initialAutoEntry("free_console",        S_Proc,"P",     "pfileio.e",0,E_other)
--end if
    initialAutoEntry("machine_proc",        S_Proc,"PIO",   "pmach.e",0,E_other)
    initialAutoEntry("papply",              S_Proc,"POIO",  "pApply.e",0,E_all, 2)
    initialAutoEntry("poken",               S_Proc,"PNOI",  "peekns.e",0,E_other,2)
--if newEmit then
----    initialAutoEntry("position",            S_Proc,"PII",   "VM\\pfileioN.e",0,E_other)
--  -- (puts is now an AutoAsm)
--elsif hllfileio then
--  initialAutoEntry("position",            S_Proc,"PII",   "pfileio.e",0,E_other)
--  initialAutoEntry("puts",                S_Proc,"PIO",   "pfileio.e",0,E_other)
--end if
    initialAutoEntry("printf",              S_Proc,"PIPO",  "VM\\pprntfN.e",0,E_other,2)
--  initialAutoEntry("print",               S_Proc,"PIOI",  "VM\\psprintN.e",0,E_other,2)       T_print = symlimit
    initialAutoEntry("print",               S_Proc,"PIOI",  "VM\\pprntfN.e",0,E_other,2)        T_print = symlimit
    initialAutoEntry("pp",                  S_Proc,"POP",   "ppp.e",0,E_other,1)
    initialAutoEntry("ppOpt",               S_Proc,"PP",    "ppp.e",0,E_other)
    initialAutoEntry("ppEx",                S_Proc,"POP",   "ppp.e",0,E_other)
    initialAutoEntry("pq_add",              S_Proc,"PPI",   "pqueue.e",0,E_other,1)
    initialAutoEntry("pq_destroy",          S_Proc,"PIII",  "pqueue.e",0,E_other,0)
    initialAutoEntry("progress",            S_Proc,"PSP",   "progress.e",0,E_other,1)
    initialAutoEntry("putd",                S_Proc,"POOI",  "dict.e",0,E_other,2)
    initialAutoEntry("resume_thread",       S_Proc,"PN",    "VM\\pThreadN.e",0,E_other)
    initialAutoEntry("requires",            S_Proc,"POI",   "get_interpreter.e",0,E_other,1)
    initialAutoEntry("suspend_thread",      S_Proc,"PN",    "VM\\pThreadN.e",0,E_other)
    initialAutoEntry("set_system_doevents", S_Proc,"PIO",   "syswait.ew",0,E_other)
    initialAutoEntry("set_test_abort",      S_Proc,"PI",    "unit_test.e",0,E_other)
    initialAutoEntry("set_test_logfile",    S_Proc,"PS",    "unit_test.e",0,E_other)
    initialAutoEntry("set_test_module",     S_Proc,"PS",    "unit_test.e",0,E_other)
--  initialAutoEntry("set_test_section",    S_Proc,"PS",    "unit_test.e",0,E_other)
    Alias("set_test_section", symlimit)
    initialAutoEntry("set_test_pause",      S_Proc,"PI",    "unit_test.e",0,E_other)
    initialAutoEntry("set_test_verbosity",  S_Proc,"PI",    "unit_test.e",0,E_other)
    initialAutoEntry("setd",                S_Proc,"POOI",  "dict.e",0,E_other,2)
--  Alias("putd", symlimit)
    initialAutoEntry("setd_default",        S_Proc,"POI",   "dict.e",0,E_other)
    initialAutoEntry("store_field",         S_Proc,"PPOO",  "structs.e",0,E_other)              T_store_field = symlimit
    initialAutoEntry("struct_add_field",    S_Proc,"PSIOI", "structs.e",0,E_other,2)            T_struct_field = symlimit
    initialAutoEntry("struct_start",        S_Proc,"PISIS", "structs.e",0,E_other,2)            T_struct_start = symlimit
    initialAutoEntry("system",              S_Proc,"PSI",   "syswait.ew",0,E_other,1)
    initialAutoEntry("system_open",         S_Proc,"PS",    "syswait.ew",0,E_other)
--  initialAutoEntry("sysproc",             S_Proc,"PS",    "syswait.ew",0,E_other)

    initialAutoEntry("task_schedule",       S_Proc,"PIO",   "VM\\pTask.e",0,E_other)
    initialAutoEntry("task_suspend",        S_Proc,"PI",    "VM\\pTask.e",0,E_other)
    initialAutoEntry("task_delay",          S_Proc,"PN",    "VM\\pTask.e",0,E_other)
--22/2/17!
--  initialAutoEntry("task_yield",          S_Proc,"P",     "VM\\pTask.e",0,E_other)
    initialAutoEntry("task_yield",          S_Proc,"P",     "VM\\pTask.e",0,E_all)
    initialAutoEntry("task_clock_stop",     S_Proc,"P",     "VM\\pTask.e",0,E_other)
    initialAutoEntry("task_clock_start",    S_Proc,"P",     "VM\\pTask.e",0,E_other)
    initialAutoEntry("test_equal",          S_Proc,"POOSI", "unit_test.e",0,E_other,2)
    initialAutoEntry("test_fail",           S_Proc,"PS",    "unit_test.e",0,E_other,0)
    initialAutoEntry("test_false",          S_Proc,"PIS",   "unit_test.e",0,E_other,1)
    initialAutoEntry("test_not_equal",      S_Proc,"POOSI", "unit_test.e",0,E_other,2)
    initialAutoEntry("test_pass",           S_Proc,"PS",    "unit_test.e",0,E_other)
    initialAutoEntry("test_summary",        S_Proc,"PI",    "unit_test.e",0,E_other,1)
    initialAutoEntry("test_true",           S_Proc,"PIS",   "unit_test.e",0,E_other,1)
    initialAutoEntry("traverse_dict",       S_Proc,"PIOII", "dict.e",0,E_other,1)
    initialAutoEntry("traverse_dict_partial_key",S_Proc,"PIOOII","dict.e",0,E_other,1)

--DEV document*2
    initialAutoEntry("TlsSetValue",         S_Proc,"PIO",   "ptls.ew",0,E_other)
    initialAutoEntry("TlsFree",             S_Proc,"PI",    "ptls.ew",0,E_other)

--if newEmit then
--  initialAutoEntry("scroll",              S_Proc,"PIII",  "VM\\pfileioN.e",0,E_other)
    initialAutoEntry("scroll",              S_Proc,"PIII",  "pScrollN.e",0,E_other)
--  initialAutoEntry("text_color",          S_Proc,"PI",    "VM\\pfileioN.e",0,E_other)
--  initialAutoEntry("unlock_file",         S_Proc,"PIP",   "VM\\pfileioN.e",0,E_other)
--  initialAutoEntry("wrap",                S_Proc,"PI",    "VM\\pfileioN.e",0,E_other)
    initialAutoEntry("wait_thread",         S_Proc,"PO",    "VM\\pThreadN.e",0,E_other)
--elsif hllfileio then
--  initialAutoEntry("text_color",          S_Proc,"PI",    "pfileio.e",0,E_other)
--  initialAutoEntry("unlock_file",         S_Proc,"PIP",   "pfileio.e",0,E_other)
--end if

    --
    -- Lastly, reserved words:
    --
    --  (eg allowing "integer end", we might get:
    --      if blah then
    --          ...
    --          end = 1
    --              ^ "if" expected
    --          ...
    --      end if)
    --
--  reservedWord(T_integer)
    reservedWord(T_global)
    reservedWord(T_proc)
    reservedWord(T_func)
    reservedWord(T_type)
    reservedWord(T_not)
    reservedWord(T_then)
    reservedWord(T_do)
    reservedWord(T_and)
    reservedWord(T_or)
    reservedWord(T_xor)
    reservedWord(T_end)
    reservedWord(T_exit)
    reservedWord(T_elsif)
    reservedWord(T_else)
    reservedWord(T_to)
    reservedWord(T_by)
    reservedWord(T_if)
    reservedWord(T_for)
    reservedWord(T_while)
    reservedWord(T_return)
    reservedWord(T_constant)
    reservedWord(T_include)
    reservedWord(T_with)
    reservedWord(T_without)
    reservedWord(T_forward)
    reservedWord(T_public)
    reservedWord(T_export)
    reservedWord(T_enum)
    reservedWord(T_ifdef)       -- shouldn't actually need this, but no harm,
    reservedWord(T_elsifdef)    -- \ however these ought to be checked for as
    reservedWord(T_elsedef)     -- /  getToken() [in ptok.e] ignores them.
    reservedWord(T_switch)
    reservedWord(T_fallthru)
    reservedWord(T_fallthrough)
    reservedWord(T_case)
    reservedWord(T_default)
    reservedWord(T_break)
    reservedWord(T_continue)
--  reservedWord(T_strict)  --??
    reservedWord(T_iff)
    reservedWord(T_iif)
    reservedWord(T_catch,K_fun)
--DEV weird... this breaks self-hosting, fstp unrecognised in pDelete.e ???!!! (25/4/2020)
--  reservedWord(T_throw,K_fun)
    reservedWord(T_try)
    reservedWord(T_static)
--/* proposed:
    reservedWord(T_abstract)
    reservedWord(T_await)
    reservedWord(T_class)
    reservedWord(T_const)
    reservedWord(T_dynamic)
    reservedWord(T_extends)
    reservedWord(T_new)
    reservedWord(T_nullable)
    reservedWord(T_struct)
    reservedWord(T_this)
    reservedWord(T_private)
    reservedWord(T_true)
    reservedWord(T_false)
--*/
--/* javascript reserved words:
await -break -case -catch class const -continue debugger -default delete -do -else -export extends 
finally -for -function -if import in instanceof let new -return static super -switch this ?throw -try typeof var void 
-while -with yield
implements interface let package private protected -public static
abstract boolean byte char double final float goto int long native short synchronized throws transient volatile
Crockford's list:
abstract
boolean -break byte
-case -catch char -class const -continue
debugger -default delete -do double
-else -enum -export extends
-false final finally float -for -function
goto
-if implements import in instanceof int interface
long
native new null
package private protected -public
-return
short static super -switch synchronized
this throw throws transient true -try typeof
var volatile void
-while -with
undefined Nan Infinity
--*/
    T_Ainc = symlimit

--  LineTab = {}
    if dropScope(0,S_File) then end if

    if dumppsym then
        close(fnpsym)
    end if

--DEV/TEMP:
--if listing then ?binftab end if

end procedure

without trace

--global integer rLiteralPart2
global object nsPart2       -- used with InNS

global constant InVeryTop = 1,      -- for loop vars, parameters, and local vars
                InTop     = 2,      -- routine, constant, local definitions
                InAny     = 3,      -- general purpose lookup.
                InNS      = 4       -- used for routine_id("fred:thing") case only

integer spNTyp

without trace
with trace
global function InTable(integer inWhat)
-- see comments under "sequence priorities" above before trying to follow this!
integer p,          -- hash idx
        fno,        -- file number
        priority,
        cp,         -- current priority
        hits,       -- no found in ""
        r,          -- save of p when hit found, only used if hits=1
        t,          -- symtab[r][S_NTyp], speedwise
        k,
        u,
        fatal
object sp,      -- copy of symtab[p], speedwise
       abort_set
sequence msg

    cp = -1     -- InAny case
    fatal = 1
    if inWhat<0 then    -- non-fatal uses:
        --
        -- called from ParamsAndLocals with -InAny for eg:
        --      procedure x(integer boolean)
        --                          ^ warning: assumed to be a variable_id, not a type
        --      (ie if a following "boolean x" says "=" expected, the above helps a bit.)
        --      (and of course if "boolean" has not been defined yet, no problem at all.)
        --
        -- called from IncludeFile with -InTop for eg:
        --  include pcase.e as pcase
        --  include pcase.e as pcase
        --                     ^ Duplicate identifier pcase
        --      (nb in this latter case must not getToken/matchChar(':') etc.)
        --      (ok, so that is "fatal", but in IncludeFile, not here.)
        --
        fatal = 0
        inWhat = 0-inWhat
    elsif DEBUG then
        if tokno then ?9/0 end if   --DEV
    end if
--  if inWhat<InAny then
    if inWhat!=InAny then
        cp = fileno*2
    end if
    p = tt[ttidx+EQ]
    hits = 0
    t = 0
    while p do
        sp = symtab[p]
        spNTyp = sp[S_NTyp]
        fno = sp[S_FPno]
--26/11/19 (nested routines) [gave up w/o trying, would be multiple currRtn anyway...]
        if fno=fileno then
--      bool rightfile = iff(spNTyp=S_TVar?fno=currRtn:fno=fileno)
--      if rightfile then
--25/5/18:
--          if spNTyp=S_TVar then
            if spNTyp=S_TVar 
            or (returnvar=-1 and spNTyp=S_GVar2) then
                if fatal then   -- 26/2
                    u = sp[S_State]
                    if not and_bits(u,S_used) then
                        sp = 0  -- reduce ref count
                        u += S_used
                        symtab[p][S_State] = u
                    end if
                end if
                return p
            elsif inWhat=InTop then
                --          elsif inWhat<=InTop then
                -- 18/10/10 (allow (eg) multiple [global] constant true=1)
                if hits=1 and t=S_Const and spNTyp=S_Const
                and sp[S_vtype]=symtab[r][S_vtype]
                and sp[S_value]=symtab[r][S_value]
                and and_bits(sp[S_State],K_noclr)
--added 2/1/17: (no help)
--              and and_bits(sp[S_State],K_gbl)
                and and_bits(symtab[r][S_State],K_noclr) then
                -- allow (eg) multiple [global] constant true=1
                --? sp
                --? symtab[r]
                -- 6/1/14 skip namespaces if next char is not ':'
                --else
-- 16/6/14
--              elsif spNTyp!=S_Nspc or Ch=':' then
                elsif spNTyp!=S_Nspc or Ch=':' or fatal=0 then
                    r = p
                    t = spNTyp
                    hits += 1
                end if
            end if
        end if
--      if inWhat>=InAny then
        if inWhat>=InAny
--      and (fno=fileno or and_bits(sp[S_State],K_gbl)) then
--26/11/19 (nested rtns) [ditto]
        and (fno=fileno or fno=0 or and_bits(sp[S_State],K_gbl)) then
--      and (rightfile or fno=0 or and_bits(sp[S_State],K_gbl)) then
--      and (p<=T_Ainc or fno=fileno or and_bits(sp[S_State],K_gbl)) then
-- 

            priority = 0
            if fno then
                -- completed files do not have their priority tables extended:
                if fno>length(priorities[fileno]) then
                    priority = -1
                else
                    priority = priorities[fileno][fno]
                end if
            end if
            if priority>cp then
                cp = priority
                r = p
                t = spNTyp
                hits = 1
            elsif priority=cp then
                if hits=1 and t=S_Const and spNTyp=S_Const
                and sp[S_vtype]=symtab[r][S_vtype]
                and sp[S_value]=symtab[r][S_value]
                and and_bits(sp[S_State],K_noclr)
                and and_bits(symtab[r][S_State],K_noclr) then
                --  ? sp
                --  ? symtab[r]
                -- 6/1/14 skip namespaces if next char is not ':'
                --else
-- 16/6/14
--              elsif spNTyp!=S_Nspc or Ch=':' or inWhat=InNS then
                elsif spNTyp!=S_Nspc or Ch=':' or inWhat=InNS or fatal=0 then
                    r = p
                    t = spNTyp
                    hits += 1   -- probable error, duplicate symbols
                end if
                --DEV try this, speedwise, and logicwise:
                --          elsif priority<cp-1 then
                --              exit
            end if
        end if
        p = sp[S_Nlink]
    end while
    if hits=1 then
--      t = symtab[r][S_NTyp]
        sp = 0  -- reduce ref count
        if t!=S_Nspc then
            --      else            -- Gvar or routine
            if fatal then   -- added 26/2/09
                --          if t=S_GVar2 then -- routines are marked used by Call().
--MARKTYPES
                if t<=S_Type then -- routines are marked used by Call().
--              if t<=S_Type-MARKTYPES then -- routines are marked used by Call().
--              if t<S_Type then -- routines are marked used by Call().
                    u = symtab[r][S_State]
                    if not and_bits(u,S_used) then
                        symtab[r][S_State] = u+S_used
                    end if
                end if
            end if
            return r
--      if t=S_TVar then    -- we do not get TVar down here...
--          return r
--      elsif t=S_Nspc then
        else -- t=S_Nspc
--DEV (untested)
--          if symtab[r][S_FPno]!=fileno then
--              Abort("namespace does not exist (must be local to calling file)")
--          end if
--trace(1)
            fno = symtab[r][S_nFno]
            cp = 0
            if fno then
                cp = priorities[fno][fno]-1     -- see above, <diag-1>
            end if
            hits = 0
            if inWhat=InNS then                 -- in eg routine_id("fred:thing"), t now indexes fred;
                tt_string(nsPart2,-2)           -- "" ""    ""       get "thing" (as set in nsPart2 by pmain.e).
                nsPart2 = 0
            elsif inWhat=InTop and not fatal then
                -- called from IncludeFile just to test the " as name" for duplicates
                return r
            else
                if Ch!=':' then
                    if inWhat=InAny and not fatal then return 0 end if
                end if
                getToken()
                trapns = fno
                MatchChar(':',float_valid:=false)
                trapns = 0
            end if
            p = tt[ttidx+EQ]
            if fno=0 then
                while p do
                    sp = symtab[p]
                    k = sp[S_FPno]
                    if k=0 then
                        r = p
                        hits = 1
                        exit
                    end if
                    p = sp[S_Nlink]
                end while
            else
                while p do
                    sp = symtab[p]
--1/1/14:
--              if and_bits(sp[S_State],K_gbl) then exit end if
--              if sp[S_FPno]=fno or (and_bits(sp[S_State],K_gbl) and fno!=fileno)  then exit end if
                    if (sp[S_FPno]=fno and (sp[S_NTyp]<=S_GVar2 or sp[S_NTyp]>=S_Type))
                    or (and_bits(sp[S_State],K_gbl) and fno!=fileno) then
                        exit
                    end if
                    p = sp[S_Nlink]
                end while
                while p do
                    sp = symtab[p]
                    k = sp[S_FPno]
                    -- completed files do not have their priority tables extended:
-- 14/05/2010 (ioob on phix:define_c_func)
--              if k<=length(priorities[fno]) then
                    if k and k<=length(priorities[fno]) then
                        priority = priorities[fno][k]
                        if priority>cp then
                            cp = priority
                            r = p
                            hits = 1
                        elsif priority=cp then
                            if hits=1 and t=S_Const and spNTyp=S_Const
                            and sp[S_vtype]=symtab[r][S_vtype]
                            and sp[S_value]=symtab[r][S_value]
                            and and_bits(sp[S_State],K_noclr)
                            and and_bits(symtab[r][S_State],K_noclr) then
                            --? sp
                            --? symtab[r]
                            else
                                hits += 1   -- probable error, duplicate symbols
                                r = p
                            end if
                        end if
                    end if
                    p = sp[S_Nlink]
                end while
            end if
            if hits=1 then
--30/1/15:
--              if fatal then   -- added 26/2/09
                if fatal
--MARKTYPES
                and symtab[r][S_NTyp]<=S_Type then -- routines are marked used by Call().
--              and symtab[r][S_NTyp]<=S_Func-MARKTYPES then -- routines are marked used by Call().
--              and symtab[r][S_NTyp]<S_Type then -- routines are marked used by Call().
                    u = symtab[r][S_State]
                    if not and_bits(u,S_used) then
                        symtab[r][S_State] = u+S_used
                    end if
                end if
                return r
            end if
            -- error case
--DEV 1/1/14: (backed out 6/1)
            if inWhat>=InAny then return 0 end if
--          if inWhat>=InAny and fatal=1 then return -fno end if
            msg = "namespace qualifier not specific enough"
        end if -- Nspc
    else
        if hits=0 then return 0 end if
        msg = "a namespace qualifier is required"
        fno = fileno
    end if
--16/6/14 (include x as y; include x as y was /not/ generating a compilation error; specifically
--          the "if InTable(-InTop) then" on 2nd include [ns] was /not/ calling Duplicate()...)
--      [16/6 removed as did not help, but maybe I should have left it in anyway...?]
    if not fatal then return 0 end if
--  if not fatal then return -123456789 end if
        -- (no special meaning should be attached to that -123456789, other than it is non-zero,
        --  sufficiently non-sense to crash if it is ever actually used, and also potentially a 
        --  fairly unique-ish clue when spotted in an ex.err (from a bug in p.exw et al))
    p = tt[ttidx+EQ]
--DEV do we want to skip past locals, or will cp take care of it?
    abort_set = {}
    while p do
        sp = symtab[p]
        k = sp[S_FPno]
        -- completed files do not have their priority tables extended:
-- 19/9/2010:
--      if k<=length(priorities[fno]) then
        if k and k<=length(priorities[fno]) then
            if priorities[fno][k]=cp then
--          if k=0 or priorities[fno][k]=cp then
                abort_set &= p
            end if
        end if
        p = symtab[p][S_Nlink]
    end while
    Abort_multi(msg,abort_set)
end function



