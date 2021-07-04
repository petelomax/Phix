--
-- pmain.e
--
-- The main guts of the compiler.
--
-- DEV this might need to be in pHeap.e (:%pGetMCHK?): (so that pAlloc can store 0 in the era)
--  it may also want to be "with memory_leak_checking/full_memory_leak_checking"

--global constant NEWRETSLOT = 01       -- nb toggle in tandem with pStack/opRetf

global constant MARKTYPES = 0
global constant K_RIDT_UDTS = false -- added 28/02/19
global constant NEWGSCAN = false -- added 01/03/19 (note: if set, K_RIDT_UDTS is ignored, check MARKTYPES as well)
-- (for NEWGSCAN; move these to pglobals.e when done:)
global sequence g_scan, kridt_scan
-- g_scan is created as repeat(0,length(symtab)) on each iteration (actually kridt_scan, see below).
--  As we encounter each opFrame/opTchk, in routine vi [initially T_maintls==21], if g_scan[rtn]=0 
--  then insert into chain, ie g_scan[rtn]:=g_scan[vi]; g_scan[vi]:=rtn.
-- However some "scannables" need to be more permanent, specifically when K_ridt is set on them.
-- The same principle is applied to (both g_scan and) kridt_scan, and in fact we re-initialise
--  g_scan from kridt_scan at the start of each iteration, rather than all-zero it.
-- This replaces (some) prior use of [S_Slink] and fixes the type-routine issue, as well as 
--  doing a much better job of excluding routines due to constant propagtion effects.
-- Note that g_scan[T_maintls] (ditto kridt_scan) is initially "-1" as the chain terminator.
--  (It uses a string to prevent any chance of accidentally treating it as symtab[$].)

global constant NEWCATCH = 01

global constant NESTEDFUNC = 01

global constant FWARN = 01

--global constant useFLOAT = 01
global constant MCHK = 0        -- 0 = off, 1 = on

--  Note that MCHK is ignored if opLntpcalled!=0 or length(ptab)!=0, as otherwise
--                            opLnp/t will leave random floats etc on the "wrong heap"
--                            and therefore trigger spurious memory leak errors.

global constant ORAC = 1    -- adds int/eger seq/uence ~/length to/.. features.
                            -- note this disables s[end], just use s[$] instead

--SUG: check types and side-effects on auto-includes.
--
-- Programming notes:   (some general tips regarding bugs etc in p.exw itself)
-- =================
--  index out of bounds caused by opstack[opsidx[-1/2/3]] of 0: if emitON is 0, there
--                      is probably a missing "if emitON then" wrapper at that point.
--                      The missing entry from the test set is "constant DEBUG=0" with
--                      "if DEBUG then <any & all possible code constructs> end if",
--                      which ought to do nothing except compile cleanly.
--  try "p p -cp" when "p -cp" fails. Certainly any change to the calling convention for
--                      the backend VM (usually) requires this, similar build problems 
--                      have occured with #ilasm/updates to iload in pttree.e, and with
--                      some changes to #isginfo handling.
--erm:
--  stuff I rule out includes: goto: see notes/alternative in pops.e
--                             preprocessing/macros: make the compiler smarter rather
--                              than make it handle two different interlaced languages.
--                             no proven gain: don't care about "want", need proof.
--                              try whacky ideas yourself, be not premature with them.
--                             because C does it like that: so what? Go use C then.
--  open-source forks of the compiler are welcome, closed source ones are forbidden.
--
-- For a description of with/without console/gui, see file:docs/pfeat.htm#console
--
--DEV temp. removed (as it now triggers use of new emit, try putting back once pth works!)
--!/**/format "p.fmt"
----with gui 4.0        -- pw.exe/see verify_build() etc below
--!/**/with console 4.0     -- p.exe (this program does not run on RDS Eu!)
--DEV (this wants to be in a common file for p.exw/pth.exw)
--format PE32 4.0
--version { "FileDescription","Phix Programming Language",
--        "LegalCopyright","Pete Lomax",
--        "FileVersion","0.6.3",
--        "ProductName","Phix Programming Language",
--        "ProductVersion","0.6.3",
--        "OriginalFilename","pw.exe" }

--
-- Warning: it is not wise to trace/profile the compiler whilst that is also
--          trying to trace/profile a user app. In particular, "p p test" may
--          crash (error code 91) when one of these is enabled.
--          Update: I have stopped attempts to "share" a copy of pdebug.e and
--          as long as pdiag.e (which remains "shared") has "without debug",
--          this now seems better, albeit the sanity remains questionable...
--
--with profile_time
--with profile

--include demo\arwen\arwen.ew
--include demo\arwen\axtra.ew
--include builtins\timestamp.ew

--without trace
--without type_check
--with trace
--with type_check
--without debug -- no gain

--!/**/without licence      -- Disable licencing, since it quite deliberately cripples 
--                          --  most of the core compiler/interpreter functionality.
--                          -- (You may freely share modified compiler sources, with a
--                          --   an "official" p.exe binary to recompile them, however
--                          --   you may NOT ship a pre-built/closed source p.exe.)
--                          -- (TIP: If you add a language construct, think twice before
--                          --       using it in p.exw itself, otherwise you may need to
--                          --       ship two versions of the modified compiler source.
--                          --       Not that running p temp\pold -cp is a major burden
--                          --       over "p -cp", as a one-time installation task.)

global integer bind = 0         -- set by -c, -listing command line options (create .exe file)

global integer repl = 0         -- set by -repl command line option (read eval print loop)
global constant replDSvsize = 8032

global integer testall = 0
global integer pauseOnWarnings = 01     -- only used if testall!=0 (which it is for final file)

-- note there is a "copy" of this in pdiag.e:
global integer batchmode = 0    -- set by -batch command line option
                                -- 1=suppress displays/prompts [incomplete]

global integer safemode = false -- if true, running under -safe command line option.

global integer norun = 0        -- set by -norun, -listing command line options

global integer nodiag = 0       -- set by -nodiag command line option
                                --  (can make list.asm easier to follow)

global integer listing = 0      -- set by -listing command line option (also sets norun & bind)
                                --  (aka -d, -dump, -l, -list, command line option)
                                -- -1 indiates -d! (interpretive dump) listing
--DEV should no longer need this (rsn)
global integer listimports = 0  -- set by -import(s) command line option
                                -- 1 just dumps the import section to screen

global constant suppressopRetf = 0 -- debug aid, should be 0 in all releases
if suppressopRetf then
    puts(1,"warning (p.exw line 107): suppressopRetf is ON\n")
end if

--DEV this is not properly implemented yet; needs to do a full gvar_scan in pemit.e,
--      process #isginfo/opGchk properly, etc. (pltype.e should be OK)
--  (btw, apart from completely ignoring "without warning", this would/should not 
--        give you any more info than -c already does...)
global integer lint = 0         -- set by -lint command line option
                                --  (nb all x[i] = c_func will give warnings)

global integer dumpil = 0       -- set by -dumpil command line option
                                --  (nb only suitable for small programs/compiler debugging,
                                --      and also: not all errors/warnings will be displayed.)
global integer dilfn = 1        -- for use with dumpil option, output file "ildump.txt"
                                --  (opened here, written in pilx86, closed in pemit.e)

global integer minus_e = false  -- if true, -e "pgm"; ie no source file.

global constant showfileprogress = 0    -- show files, times, etc.

global constant collectstats = 0        -- see opStat, profile.e, eg emitHex5(), Branch().
                                        -- WARNING: this (=1) is currently broken.

global constant countTransTmpFer = 0    -- if set, writes a table of instructions which
                                        -- might be suitable for tmptransfer, sorted in
                                        -- order of occurrence, to ildump.txt only.
                                        -- NB: does nowt else, and collects "consumers"
                                        --  only, not potential producers (see pilx86.e).
global constant showOpCounts = 0        -- if set, writes a table of opcodes in order
                                        -- of number of times ilxlate() processed them.
if countTransTmpFer then
    if showOpCounts then ?9/0 end if    -- these (debug) options are mutually exclusive!
end if


global constant AutoIncWarn = 01        -- If 1, warn when files are auto-included.
                                        -- NB only has effect under -lint.

--include pcore.e
--include p6core.e
--
-- pcore.e
--
--  common incudeset for p.exw and pgui.exw
--
include pglobals.e

include builtins\ppp.e

include pops.e  -- opcode table

include pttree.e    -- ternary tree

include pmsgs.e -- Warnings/Warn/Abort/Duplicate/Expected/Fatal/Undefined

include ptok.e  -- tokeniser: getToken()
--include p6tok.e   -- tokeniser: getToken()

include pltype.e    -- localtypes handling

include psym.e      -- symbol table handler.
--include p6sym.e -- symbol table handler.

include pilx86.e    -- ilxlate(), (also includes psched.e)

--7/4/16:
--global sequence code_section
global string code_section
global integer CSvsize, DSvsize
--25/4/16: (pHeap.e now using mmap)
--global integer CSvaddr, DSvaddr, ImageBase, VMvaddr, VMraddr, DVraddr, VMvsize        --DEV temp, for listing
global atom CSvaddr, DSvaddr, ImageBase, VMvaddr, VMraddr, DVraddr, VMvsize     --DEV temp, for listing
global string divm  -- used by p2asm.e if dumpVM=1
global sequence VMep -- used by p2asm.e [DEV]

--include pemit.e
--include p6emit.e
include pEmit2.e

--DEV this does not appear in Edita's project tree... (Finc thing??)
--include pdebug.e  -- trace routines
include VM\pTrace.e -- trace routines


--DEV: kill off paramLines, opsline, calltokline (inc temp code in Warn()), 
--              tok_abort_line, wascalltokline, lblline, rtntokline, notumline,
--              sqline, relopline, savetokline, eqline,
--              (can opLchk and opGchk be shortened? if the latter then
--               also kill tl in isginfo().)
-- move this to bpset? pglobals?
    -- On forward called routines, which have not yet or are in the
    --  process of being defined, [S_il] takes the form:
    --      {callset} -- (one set for each call)
    -- where callset is
    --      {{tokcol, fileno, routineNo},   -- (for error reporting)
    --       {tokcol, offset[, ttidx, tokcolp]}}) -- (one per param)
    -- ie after bcptr=symtab[N][S_il], then length(bcptr) is the 
    --  number of forward calls to routine N, which need to get
    --  backpatched when it is actually defined, and likewise after 
    --  bi=bcptr[i], then bi[1] locates the call statement and the 
    --  remainder are either:
    --      {tokcol,offset}
    --  for normal/numbered parameters, or for named parameters:
    --      {tokcol,offset,ttidx,tokcolp}
    --  eg plonk(o,ctab:=table) might create a symtab[555][S_il] of:
    --      {{{7746,3,427}, -- called at 7746th character of file 3,
    --                     -- somewhere within routine number 427.
    --        {7774,35}, -- symtab[427][S_il][35] needs p1. Report
    --                  --  any error (type, too many) at ch 7774.
    --        {7783,40,3764,7778}}} symtab[427][S_il][40] needs the 
    --                  -- parameter which has a ttidx/[S_Name] 
    --                  -- of 3764. Report any error with "table"
    --                  -- at ch 7783; if plonk has no parameter
    --                  -- named "ctab", report that at ch 7778.
    --  Any length 2 (ie numbered) params are always at the start;
    --   it makes no sense to permit eg p(1,2,p7:=7,8), since it
    --   is just a bit too ambiguous where the 8 goes, plus if you
    --   know there is a parameter called p7, then almost certainly 
    --   you know the name of the parameter the 8 is meant for.
    --  While the total number of parameters is length(bi)-1, the
    --   presence of named parameters means a more detailed check
    --   may be required.
    --
    --  The amusing/interesting/confusing thing about forward calls
    --  is that we are using a dummy X to specify the changes we 
    --  need in other X when we get round to creating a real X.
    --   (for X read "routine" or "parameter" or "[S_il]"...)
    --

sequence opstack, opstype, opsltrl, opsline, opstcol
         opstack = repeat(0,4) -- var nos [index to symtab] or opcode [see also opTopIsOp]
         opstype = repeat(0,4) -- eg T_integer, or UDT, -1 for ops/literals, -2 on stack
         opsltrl = repeat(0,4) -- 0 (No), 1 (Yes), or allowTempReuse(-1) for temps
         opsline = repeat(0,4) -- for error reporting
         opstcol = repeat(0,4) --       ""

--procedure validate_opstack()
--  for i=1 to length(opstack) do
--      isInt(opstack[i],1)
--  end for
--end procedure


-- Technical/Linguistic point: The notion of "literal" bears some consideration.
--  The 1 and "hello" in say puts(1,"hello") are literals; the implicit 2 (ie
--  number of subscripts) in say s[5][6] is NOT a "literal" in this context,
--  but rather the same (opstype[]=-1) as opSubse.

-- verify that the compiler is setting these as "sequence of integer":
--DEV broken 23/4/21 (repeat.e) - fixme!
--!/**/ #isginfo{opstack,0b0100,MIN,MAX,integer,-2}
--       ^ gInfo is {10394,12,-1073741824,1073741823,15,-2}
--!/**/ #isginfo{opstype,0b0100,MIN,MAX,integer,-2}
--!/**/ #isginfo{opsltrl,0b0100,MIN,MAX,integer,-2}
--!/**/ #isginfo{opsline,0b0100,MIN,MAX,integer,-2}
--!/**/ #isginfo{opstcol,0b0100,MIN,MAX,integer,-2}
--               var,    type,  min,max,etype, len
-- #isginfo emits no code or otherwise alters compiler behaviour, it 
--  just verifies the result of gvar_scan. See pemit.e for details.

integer opsidx              -- index to above
        opsidx = 0
integer opsidxm1, opsidxm2, opsidxm3

integer isGlobal            -- set when "global" found
        isGlobal = 0        -- (0==false, 1==true, 2==export)

integer opTopIsOp       -- zero or one of the following groups
        opTopIsOp = 0

constant UnaryOp        = 1, 
         MathOp         = 2, 
         ConcatOp       = 3, 
         BranchOp       = 4, 
         LogicOp        = 5,
         BltinOp        = 6,
         SubscriptOp    = 7, 
         SliceOp        = 8, 
         MkSqOp         = 9, 
         NotBltinOp     = 16    -- opInt..opSq (opINSP) at least for for now
--       NotSubscriptOp = 17    -- not actually used, but logically this does exist (search comments).


--integer LastStatementWasAbort -- now in pglobals.e, for pilx86.e

integer onDeclaration           -- eg "object x=e" or "object x x=e" forms, no check/dealloc rqd on x. [DEV: BLUFF]
        onDeclaration = 0       -- also used for "constant y=e", some first ever uses of a var, and
                                -- all param setup between opFrame and opCall (since opFrame saves 
                                -- and then clears all params [and local vars and temps]).
integer exitBP          -- exit back patch link
        exitBP = -1     -- -1:not valid(not inside a loop) else backpatch chain (0 terminated)

integer breakBP
        breakBP = -1

integer continueBP
        continueBP = -1

-- moved to pglobals 1/10/2011:
--integer returnvar         -- -1: top_level/return illegal, 0: in a proc, +ve: func/type return var (symtab idx)
--      returnvar = -1      --          - see DoRoutineDef/DoReturn for more details.

integer returntype          -- best guess so far of the return type
        returntype = -1

integer returnint           -- 1: this is a type, ie a function which should return 0 or 1.
        returnint = 0


--
-- Probable Logic Errors (ple, aka plausibility tests) are things like:
-- =====================
--
--  integer i
--      ...
--      if sequence(i) then
--                  ^ warning: probable logic error (always false)
--  object x
--      if sequence(x) then
--          ...
--      elsif atom(x) then
--                 ^ warning: probable logic error (always true)
--
-- All "probable logic errors" are given as warnings.
-- These messages extend to user defined types and derivatives.
-- It is also possible to get these messages for subscripts in 
--  cases where the compiler has proved the sequence only ever 
--  contains elements of that type, for example in:
--      string t
--          t = "fred"
--          if integer(t[2]) then
--
-- Note that "if atom(i) then" and "if not atom(i) then" give
--  the same (always true) message, ie it is "atom(i)" which
--  is always true, rather than [say] "not atom(i)".
--
-- Also note that should the compiler detect that parameter X is only
--  ever assigned an integer, an "integer(X)" test (etc) does /NOT/ 
--  give a ple, otherwise general purpose code in library routines 
--  would get spannered by small programs that use it "lightly".
--  Even better, the compiler quietly suppresses code generation for
--  the bits that would never be executed, ie any code in an always 
--  false test, as well as any always true or always false tests.
--
integer probable_logic_error        -- a "used count".
        probable_logic_error = 0

sequence plecol, pleline, pletruth
    plecol = repeat(0,4)
    pleline = repeat(0,4)
    pletruth = repeat(0,4)

-- verify compiler gets these right:
--DEV broken 23/4/21 (repeat.e) - fixme!
--!/**/ #isginfo{plecol,0b0100,MIN,MAX,integer,-2}
--       ^ gInfo is {10422,12,-1073741824,1073741823,15,-2}
--!/**/ #isginfo{pleline,0b0100,MIN,MAX,integer,-2}
--!/**/ #isginfo{pletruth,0b0100,MIN,MAX,integer,-2}


procedure add_ple(integer truth)
    if emitON then -- test added 29/12/2011
        probable_logic_error += 1
        if probable_logic_error>length(plecol) then
            plecol &= repeat(0,4)
            pleline &= repeat(0,4)
            pletruth &= repeat(0,4)
        end if
        plecol[probable_logic_error] = opstcol[opsidx]
        pleline[probable_logic_error] = opsline[opsidx]
        pletruth[probable_logic_error] = truth
    end if
end procedure

procedure show_ple()
sequence tf
    for i=probable_logic_error to 1 by -1 do
        if pletruth[i] then
            tf = "true)"
        else
            tf = "false)"
        end if
        Warn("probable logic error (always "&tf,pleline[i],plecol[i],0)
    end for
    probable_logic_error = 0
end procedure

global procedure Aborp(sequence msg)
    if probable_logic_error then show_ple() end if
    Abort(msg)
end procedure

procedure Abork(sequence msg, integer k)
    tokline = opsline[k]
    tokcol  = opstcol[k]
    Aborp(msg)
end procedure

bool not_js = false     -- catch "with js" occurring too late...
string nj_reason

constant WITH=1, WITHOUT=0, FROMROUTINE=1
procedure DoWithOptions(integer OptOn, integer fromroutine=0)
integer k
    getToken()
    if toktype=LETTER then
        k = find(ttidx,{T_profile, T_profile_time, T_trace, T_warning, T_type_check, T_debug})
        if lint and k=OptWarning then
            -- ignore/do nowt
            getToken()
            if toktype=LETTER and ttidx=T_strict then getToken() end if
            return
        elsif k then
            if k=OptProfile
            or k=OptProfileTime then
                if OptOn then
                    if profileon and profileon!=k then
                        Aborp("cannot mix profile and profile_time")
                    end if
                    profileon = k
                end if
--/*
--  --added 7/7/16:
--                  optset[k] = OptOn
--  --DEV (spotted in passing) 28/6/16: I think I messed up for profile/profile_time...
--  --          elsif k=OptWarning then
--  --              finalOptWarn[fileno] = OptOn
--  --          end if
--  --          optset[k] = OptOn
--              elsif testall then
--                  if k=OptWarning then
--                      finalOptWarn[fileno] = OptOn
--                  end if
--                  optset[k] = OptOn
--              elsif k!=OptWarning then
--                  optset[k] = OptOn
--              end if
--*/
            elsif k=OptWarning then
                finalOptWarn[fileno] = OptOn
--11/5/21:
--          elsif k=OptTrace and OptOn and with_js=1 then
--              Aborp("cannot mix with trace and with js")
            end if
--p2js:
            optset = deep_copy(optset)
            optset[k] = OptOn
--10/10/2020:
--          if k=OptDebug and not OptOn and not fromroutine then
--              clearTLSDebug()
            if k=OptDebug and not fromroutine then
                clearTLSDebug(OptOn)
            end if
            getToken()
            if toktype=LETTER and ttidx=T_strict then getToken() end if
            return
        elsif not fromroutine then
            --DEV deprecated (see DoFormat)
            if ttidx=T_console
            or ttidx=T_gui then
                if ttidx=T_gui then
                    -- gui is antonym for console, ie
                    --  with gui == without console
                    --  without gui == with console
                    -- (the "with" forms should always be used, for readability's sake, but
                    --  it is harder to ban the "without" forms than it is to support them.)
                    OptOn = not OptOn
                end if
                OptConsole = OptOn
                getToken(float_valid:=true)
                if toktype=FLOAT or toktype=DIGIT then
                    -- optional subversion
                    if equal(TokN,3.10) then
                        getToken()
                        subvers = #000A0003         -- messes up gui, obviously
                    elsif equal(TokN,4.0) then
                        getToken()
                        subvers = #00000004         -- this is the default, btw
                    elsif equal(TokN,5.0) then
                        getToken()
                        subvers = #00000005         -- the 64-bit default, btw
                    else
                        Aborp("3.10, 4.0, or 5.0 expected")
                    end if
                end if
                return
            elsif ttidx=T_licence then
if not newEmit then
                if OptOn then Abort("invalid") end if
                OptLicence = 0
                getToken()
                return
end if
            elsif ttidx=T_indirect_incs
               or ttidx=T_inline then
                -- (OpenEuphoria only, ignored by Phix)
                getToken()
                return
            elsif ttidx=T_js
               or ttidx=T_javascript
               or ttidx=T_js_semantics then
--?"with js"
                if with_js!=2 and with_js!=OptOn then
                    Aborp("cannot mix with and without js")
                elsif with_js=2 and OptOn and not_js then
                    Aborp("p2js violation already skipped ("&nj_reason&")")
                end if
--11/5/21:
--              if optset[OptTrace] and OptOn then
--                  Aborp("cannot mix with js and with trace")
--              end if
                with_js = OptOn
                getToken()
-- no need, done directly in p.exw/main():
--              s5 &= {opWithJS,flag}
                return
            end if
        end if
    elsif toktype=DIGIT
       or toktype=FLOAT then
        -- old stamped file?
        Warn("if that is a stamp, then this is a /very/ old file.\n"&
             "     It is probably worth getting a newer one.",tokline,tokcol,0)
        getToken()
        return
    end if
    Aborp("unrecognised option")
end procedure

constant pbrON=1
--
-- Automatic pass by reference optimisation:
--  First, consider function myfunc(object x) and a line of code such as s = myfunc(s).
--  For clarity, when I refer to s I mean lhs of the caller, rather than the copy in x.
--  In the simplest case, then if s is a local, it /cannot/ be referenced in myfunc,
--  although of course x has a copy of the value. So rather than incref s we can just
--  "move" it to x and make s unassigned over the call. The same cannot be said when
--  s is a global or file-level variable. Some complications arise in a statement such
--  as s = r & s[1..match(".html",s)-1]; we cannot unassign s over the call to match()
--  since we need it again before it gets re-assigned. We set lhsvar and zero lhspos 
--  in Assignment() when the lhs is a plain (non-subscripted) tvar, lhspos is set to
--  the position of an onDeclaration flag, which is set after the full expression has
--  been parsed to 2, meaning that pilx86 should apply pbr, taking care to zero lhsvar 
--  (and therefore ignore lhspos) if multiple uses or subscripts etc are detected.
--  Note [DEV] multiple assignment does not (yet) have pbr optimisation applied.
--  Also note that pbinary.e in particular is written in such a way that it would
--  suffer terrible performance degradation should automatic pbr stop working, see
--  SetField() for example - you would have to replace
--      res = SetField(res,...)
--  with a file-level res and
--      res = local
--      local = {}
--      SetField(...)
--      local = res
--      res = {}
--  to get anywhere near a similar level of performance.
--

integer lhspos  -- locates the onDeclaration flag
integer lhsvar
        lhsvar = 0

--
-- The core workspace. Expresssions get pushed on here in Reverse Polish
-- (eg 1,2,+) and either get stored into the result var or a temporary
-- var when attempting to push another value/opcode atop an op, ie/eg
-- "(1+2)*3" -> {1,2,+} -> {tmp,3,*}.
-- "1+2*3" -> {1,2,3,*} -> {1,tmp,+}.
--
--procedure isInt(object o, integer f)
--  if integer(o)!=f then ?9/0 end if
--end procedure
--isInt("",0)

-- moved to psym.e 27/8/14:
----with trace
--global -- for pilasm.e
--procedure apnds5(object code)
--integer opLnv
----/**/    #isginfo{code,0b0101,MIN,MAX,integer,-2}    -- (integer|dseq of integer), any length
----/**/    #isginfo{s5,0b0100,MIN,MAX,integer,-2}      -- (as good a place as any to check this)
--  if lastline!=emitline then
----DEV why oh why is this not just part of DoWithOptions?! (and optset=)
--      opLnv = opLn
--      if not bind then
--          if    optset[OptProfile] then           opLnv = opLnp
----            elsif optset[OptProfileClone] then      opLnv = opLnpclone
----            elsif optset[OptProfileCoverage] then   opLnv = opLnpcover
--          elsif optset[OptProfileTime] then       opLnv = opLnpt
--          elsif optset[OptTrace] then             opLnv = opLnt
--          end if
--      end if
--      s5 = append(s5,opLnv)       -- opLn/p/pt/t
--      s5 = append(s5,emitline)
--      lastline = emitline
----if emitline=7 then trace(1) end if
--  end if
--  s5 &= code
--end procedure

--with trace

constant allowTempReuse = -1    -- Yes value

--without trace
procedure freeTmp(integer howmany)
    if reusetmps then
        while howmany do
            if opsltrl[opsidx]=allowTempReuse then
--and opstype[opsidx]!=-1 (maybe?)
                integer N = opstack[opsidx]
                if N then
                    object si = symtab[N]
                    if si[S_NTyp]=S_TVar                        -- a tvar (doh!)
                    and equal(si[S_Name],-1)                    -- unnamed
                    and not and_bits(si[S_State],K_Fres) then   -- but not a return var
                        si = si[S_vtype]    -- (also kills refcount on symtab[N], btw)
--p2js:
--                      symtab[N][S_Nlink] = freetmplists[si]
--                      symtab[N][S_ltype] = si     -- bugfix 9/4/9 (ltype:=vtype)
                        sequence sN = deep_copy(symtab[N])
                        sN[S_Nlink] = freetmplists[si]
                        sN[S_ltype] = si
                        symtab[N] = sN
--</p2js>
                        freetmplists[si] = N
                    end if
                    opsltrl[opsidx] = 0
                end if
            end if
            opsidx -= 1
            howmany += 1
        end while
    else
        opsidx += howmany
    end if
end procedure

-- now in pilx86.e:
--constant Bcde = {opJlt,opJle,opJeq,opJne,opJge,opJgt},
----            ie  { "<" ,"<=","=(=)", "!=", ">=",">" },       -- (= and == treated the same, for now)
--       Scde = {opSlt,opSle,opSeq,opSne,opSge,opSgt},
----         cc={jl_rel32,jle_rel32,je_rel32,jne_rel32,jge_rel32,jg_rel32},
----         scOp = {setl, setle,sete, setne,setge,setg},
--       tnot = {  5,    6,    4,    3,    1,    2  },
----            i.e.  jge   jg    jne   je    jl    jle
--       eJmp = {  0,    0,    1,    1,    0,    0  }   -- equality tests (when either is an integer)
----            i.e. BOTH,BOTH,EITHER,EITHER,BOTH,BOTH      --  eg/ie 1.1=1 can be tested using alu/cmp,
--                                                      --        but say 1.1>1 needs fld/fcmp/fnstsw etc.

-- chain of items to "uninitialise" on else, elsif, end if, end for, end while, end routine.
integer Ichain
        Ichain = -1

integer oIItype     -- scratch/rootType from a oneInitialisedInt call if S_Init
        oIItype = 0

integer constInt    -- only valid if oneInitialisedInt has just been called, or set manually
        constInt = 0
atom constIntVal -- "", meaningless unless constInt=True
        constIntVal = 0

integer oktoinit    -- avoid marking vars init in short-circuit cases
        oktoinit = 1

--without trace
function oneInitialisedInt(integer N, integer markInit)
    oIItype = 0
    if not emitON then return 0 end if  --DEV would 1 be better?
    sequence symtabN = symtab[N] -- (do this once not 4 times below)
    constInt = (symtabN[S_NTyp]=S_Const)
    if not symtabN[S_Init] then
        -- non-init S_Const are treated as init vars
        if not constInt then
            if oktoinit and markInit then
                symtab[N] = 0
                symtabN[S_Init] = Ichain
                symtab[N] = symtabN
                Ichain = N
            end if
            return 0
        end if
        constInt = 0
    end if
--  oIItype = rootType(symtabN[S_vtype])
--  oIItype = rootType(symtabN[S_ltype])
    oIItype = symtabN[S_ltype]
    if oIItype>T_object then oIItype = rootType(oIItype) end if
--  oIItype = symtabN[S_ltype]  --NO!
--DEV change param to oneInitialisedInt() to opsidx/k and use opstype[opsidx],N=opstack[opsidx]...?
    if oIItype!=T_integer then return 0 end if
    if constInt then
--      if not integer(symtabN[S_value]) then return 0 end if
        if isFLOAT(symtabN[S_value]) then return 0 end if
        constIntVal = symtabN[S_value]
    end if
    return 1
end function


-- for inc,dec,div2 optimisations, etc:
atom secondintval   -- only ever tested for >=-1, iff twoInitInts() returns true
     secondintval = -2
integer firstintval     -- only ever tested for >=-1, iff twoInitInts() returns true
        firstintval = -2
--integer bothconst     -- only valid if twoInitInts() returns true,
--      bothconst = 0   -- not actually used anywhere [yet] [DEV]
integer bothInit        -- alt rv for twoInitInts(). Valid except in the emitON=0
                        -- case, when we don't care anyway.

--with trace

constant BOTH = 0 --, EITHER = 1    -- nb should match eJmp

function twoInitInts(integer either)
--
-- the top 2 stack items must be initialised else return false, always(/irrespective of "either").
-- if "either" is true then only one of them need be an integer, else both must.
--
integer N1, N2, rInt, const1, const2
sequence symtabN1, symtabN2
    bothInit = 1
    if not emitON then return 0 end if
    opsidxm1 = opsidx-1     -- 2nd int
    opsidxm2 = opsidxm1-1   -- 1st int
    N1 = opstack[opsidxm2]  -- 1st int
    N2 = opstack[opsidxm1]  -- 2nd int
    symtabN1 = symtab[N1]   -- 1st int
    symtabN2 = symtab[N2]   -- 2nd int
    const2 = (symtabN2[S_NTyp]=S_Const)
    if not symtabN2[S_Init] then
        -- non-init S_Const are treated as init vars (ie their value is not 
        --  known at compile-time but instead gets calculated at run-time;
        --  however they always get set before any user code can ref them).
        if not const2 then
            -- assume eg "if X=1 then" traps a non-init X; subsequent refs
            --  to X (until next mergepoint) can/should treat X as init:
            if oktoinit then
                symtab[N2] = 0
                symtabN2[S_Init] = Ichain
                symtab[N2] = symtabN2
                Ichain = N2
            end if
            bothInit = 0
            return 0
        end if
        const2 = 0
    end if
    const1 = (symtabN1[S_NTyp]=S_Const)
    if not symtabN1[S_Init] then -- as above
        if not const1 then
            if oktoinit then
                symtab[N1] = 0
                symtabN1[S_Init] = Ichain
                symtab[N1] = symtabN1
                Ichain = N1
            end if
            bothInit = 0
            return 0
        end if
        const1 = 0
    end if
--  rInt = (rootType(symtabN1[S_vtype])=T_integer)
--  rInt = (opstype[opsidxm1]=T_integer)    -- 2nd int
    rInt = opstype[opsidxm1]    -- 2nd int
    rInt = (rInt=T_integer)
    if either then
        either = rInt
    else
        if not rInt then return 0 end if
    end if
--  bothconst = 1
    if rInt and const2 then
--3/2/15:
--      if not integer(symtabN2[S_value]) then return 0 end if
        if isFLOAT(symtabN2[S_value]) then return 0 end if
        secondintval = symtabN2[S_value]
    else
        secondintval = -2   -- NB only ever test for >=-1
--      bothconst = 0
    end if
--  rInt = (rootType(symtabN2[S_vtype])=T_integer)
--  rInt = (opstype[opsidxm2]=T_integer)    -- 1st int
    rInt = opstype[opsidxm2]    -- 1st int
    rInt = (rInt=T_integer)
    if not rInt and not either then return 0 end if
    if rInt and const1 then
        firstintval = symtabN1[S_value]
    else
        firstintval = -2    -- NB only ever test for >=-1
--      bothconst = 0
    end if
    return 1
end function


--integer N2type
--function twoInit(integer N, integer N2)
----    if not equal(symtab[N][S_Name],-1) then
--      if not symtab[N][S_Init] then return 0 end if
----    end if
--  N2type = symtab[N2][S_vtype]    -- ltype?
----    if equal(symtab[N2][S_Name],-1) then return 1 end if
--  return symtab[N2][S_Init]
--end function

--with trace
integer dpos
procedure emitHexMov(integer opcode, integer dest, integer src)
--procedure emitHexMov(integer opcode, object dest, integer src)
integer isInit, ltype
sequence symtabN

    if emitON then
        dpos=-1
        --  opMove,dest,src,Init,onDeclaration,ltype
        --  opMovsi,dest,src,Init,onDeclaration
        --  opMovti,dest,src,Init
        --  opMovbi,dest,src,Init   -- (nb 6/10/9 src can be -1)
        apnds5({opcode,dest,src})
        dpos = length(s5)-1
        --6/10/9:
        if src=-1 then
            -- (force 0/1, from makeBool)
            isInit = 0
            s5 = append(s5,isInit)
        else
            symtabN = symtab[src]
            isInit = symtabN[S_Init]
--DEV and oktoinit?
--19/3/09:
--      if not isInit then
            if not isInit and oktoinit and symtabN[S_NTyp]!=S_Const then
                symtab[src] = 0
                symtabN[S_Init] = Ichain
                symtab[src] = symtabN
                Ichain = src
            end if
--          if bind and symtabN[S_NTyp]=S_Const then
--              -- assume constants are init when binding
--              -- (can be broken by forward calls from above
--              --  the constant definition, that is when the
--              --  constant cannot be K_litnoclr'd.)
--              isInit = 1
--          end if
            s5 = append(s5,isInit)
            if opcode=opMove
            or opcode=opMovsi then
                s5 = append(s5,onDeclaration)
--DEV opLtyp
                if opcode=opMove then
--              s5 = append(s5,rootType(symtabN[S_ltype]))      -- (nb T_integer would be opMovsi/bi)
                    ltype = symtabN[S_ltype]                        -- (nb T_integer would be opMovsi/bi)
                    if ltype>T_object then ltype = rootType(ltype) end if
                    s5 = append(s5,ltype)
                end if
            end if
        end if
    end if  -- emitON
end procedure

procedure zero_temp(integer tmp)
    if tmp!=0 
    and symtab[tmp][S_Name]=-1 
--  and not and_bits(symtab[tmp][S_State],K_noclr) then
    and not and_bits(symtab[tmp][S_State],K_noclr+K_Fres) then
        -- (Not entirely sure why the K_noclr check was needed, but t57masgn 
        --  crashed without it, setting dmin to "22" in pilx86.e/getDest().)
        -- Could not get the reusetmps part to work either, then again since
        -- it was never doing it before anyway, I guess it's alright...
        -- (ah, probably because freeTmp(-1) has already been called?)
        emitHexMov(opMovsi,tmp,T_const0)
--/*
        if reusetmps then
            opsidx += 1
            opstack[opsidx] = tmp
            opsltrl[opsidx] = allowTempReuse
            freeTmp(-1)
        end if
--*/
    end if
end procedure

--without trace
constant NOTINTS=0,
         INTSTOO=1,
         PUSHEAX=2      -- (implies INTSTOO)    [DEV kill me... ?]

--integer fromQU = 0    --DEV tmp (t17incV)

--DEV no idea why opsidx is a parameter... (try removing it and see)
procedure saveFunctionResultVars(integer opsidx, integer intstoo)
--
-- In eg s = {f(1),g(2)}, the f(1) result var is initially left in opstack for
--  the pending (in this particular case) opMkSq call. However, the g(2) (or 
--  another direct call to f()) might clobber it, so transfer to a new temp var.
--
integer osi, vtype, state, ttyp, tvar, opMov
sequence symtabN
integer wasemitline
    for i=opsidx to 1 by -1 do
        osi = opstack[i]
        if osi                  -- skip emitON=0 cases
        and opstype[i]!=-1 then -- and skip opcodes/literals (such as no of subscripts)
            symtabN = symtab[osi]
            vtype = symtabN[S_NTyp]
            if vtype=S_TVar then
                state = symtabN[S_State]
                if and_bits(state,K_Fres) then
if intstoo=PUSHEAX then
                        wasemitline = emitline
--DEV we should use a flag for this instead[?]
                        emitline = lastline -- prevent apnds5 fouling eax
--                      apnds5(opPushEax)
                        emitline = wasemitline
else
--                  ttyp = rootType(symtabN[S_vtype])
--                  ttyp = rootType(symtabN[S_ltype])       --DEV opstype[i]??!! (prolly not!)
                    ttyp = symtabN[S_ltype]
--if fromQU then
--  printf(1,"symtab[%d][S_ltype] = %d\n",{osi,ttyp})
--end if
                    if ttyp>T_object then ttyp = rootType(ttyp) end if
--Added 1/4/2012:
                    if intstoo or ttyp!=T_integer then

--DEV onDeclaration here...? (at toplevel not in a loop anyway)
                        tvar = newTempVar(ttyp,Shared)
                        opMov = opMove
                        if ttyp=T_integer then
                            constInt = 0    --DEV suspect this is unnecessary..
                            opMov = opMovbi
                        end if
--if newEBP then
                        wasemitline = emitline
--DEV we should use a flag for this instead[?]
                        emitline = lastline -- prevent apnds5 fouling eax
                        emitHexMov(opMov,tvar,osi)
                        emitline = wasemitline
--else
--                      emitHexMov(opMov,tvar,osi)
--end if
                        opstack[i] = tvar
--validate_opstack()
                        opstype[i] = ttyp
                        opsltrl[i] = allowTempReuse -- mark for possible re-use
                    end if
end if
                end if
            end if
        end if
    end for
end procedure

procedure WarnX(sequence msg, integer k)
    Warn(msg,opsline[k],opstcol[k],0)
end procedure

--DEV object?? (no longer needed, it seems...)
--procedure sqErr(object opcode, integer k)
--  tokline = opsline[k]
--  tokcol  = opstcol[k]
--  if    equal(opcode,'+') then Abort("type error (use sq_add?)")
--  elsif equal(opcode,'-') then Abort("type error (use sq_sub?)")
--  elsif equal(opcode,'*') then Abort("type error (use sq_mul?)")
--  else                         Abort("type error (use sq_div?)") -- '/' and "divf"
--  end if
--end procedure


--with trace
integer forward_call    forward_call = 0 
integer routineNo       routineNo = 0

integer calltokcol  calltokcol = 0      -- saved position of "fred" in fred({1,2,3})
integer calltokline calltokline = 0

function sq_able()
-- returns true if we can auto-convert eg floor into sq_floor
--  (after we detect it has incompatible parameters for the builtin)
integer opcode, state
sequence pmsg, symtabN
--  if routineNo<=T_Bin then
    if routineNo<=T_Asm then
        opcode = symtab[routineNo][S_il]
        if sqAble[opcode] then
            routineNo = sqAble[opcode]
            symtabN = symtab[routineNo]
--          if sqopWarn then
            pmsg = getname(symtabN[S_Name],-2)&" assumed"
            Warn(pmsg,calltokline,calltokcol,SQ_WARN)
--          end if
            state = symtabN[S_State]
            forward_call = and_bits(state,S_fwd)
            if emitON and not and_bits(state,S_used) then
                builtinsReferenced = 1
                symtab[routineNo] = 0
                state += S_used
                symtabN[S_State] = state
                symtab[routineNo] = symtabN
            end if
            return true
        end if
    end if
    return false
--          return T_sequence
--trace(1)
--          Abork("arguments to power must be atoms (use sq_power instead)",opsidx)
end function

integer lastparam   -- saved in ParamList(), used in Branch(), for localtype info 
                    -- from (eg) "if udt(lastparam) then". By the time Branch() 
                    -- is invoked, only res:udt remains on opstack.

--integer laststringparam -- "" but string-only...

function plausible(integer sig, integer act)
--function plausible(integer sig, integer act, integer vidx)
--
-- Perform some simplistic type checking on actual vs formal parameter types.
--  Covers both builtin and user-defined routines, for example:
--
--      table = prepend(0,table)
--                      ^ incompatible type for routine signature
--
--  [It should be prepend(table,0) btw, first parameter must be sequence.]
--
--  Note that if either formal or actual parameter is type object, we cannot
--  perform any checking at parse time. This in no way affects fatal runtime 
--  typecheck errors such as <int> = <object> when the latter is a sequence.
--
--    <aside>
--      I originally planned something like this (implicit handling):
--        myproc(1)         -- ok, one parameter of type integer.
--        myproc("fred")    -- alright, make that integer|string.
--        procedure myproc(atom x)
--                              ^ incompatible type for routine signature
--      However, you cannot distinguish that (type-wise) from eg:
--        sequence s={1,"fred"}
--        myproc(s[i])      -- p1 is integer|string
--      So, implicit forward calls ==> run-time, not compile-time errors.
--      Obviously an explicit forward definition before the myproc("fred")
--      would (easily) catch that particular error at compile-time.
--    </aside>
--
-- Lastly, catch "probable logic errors", such as defining integer x and then
--  asking "if sequence(x) then": clearly nonsense/always false. Likewise this 
--  covers both builtin and user defined types, for example:
--
--  type positive_int(integer p) return p>1 end type
--  type hour (positive_int h) return p<=24 end type
--  hour h
--
--      if atom(h) then
--              ^ warning: probable logic error (always true)
--
-- NB1: "then end if" is treated specially, eg "if hour(h) then end if"
--      can safely be used to suppress warnings (see DoIf), not add them.
-- NB2: plausibility tests must always use hll/orig/local types.
--      If the compiler deduces that object o is in fact only
--      ever assigned an integer (aka the global type), then 
--      integer(o) must /not/ raise a probable logic error 
--      (since that would spanner any general purpose library 
--       code only "lightly used" in a given application).
--      However "if integer(x) then" may set a local type on x
--      (ie one which inverts at else/reverts at end if) and a
--      subsequent (nested) "if integer(x) then" /is/ a ple, as
--      long as x is not modified between the tests, of course.
--
integer rootAct, rootSig
--trace(1)
    if DEBUG then
        if sig then
            if symtab[sig][S_NTyp]!=S_Type then ?9/0 end if
        end if
    end if
    if act=-1 then -- an operator
        --DEV should we be popfactoring first?
        puts(1,"act=-1\n")
        act = T_object --DEV figure something better out (if fwd referencing)
    elsif DEBUG then
        if act then -- PL 29/4/09 avoid ioob(0).
            if symtab[act][S_NTyp]!=S_Type then ?9/0 end if
        end if
    end if
--DEV suspect use of forward here... (gets set by/defined before sq_able)
    if forward_call and routineNo>T_Ainc then
--      if sig=T_integer and (act=T_atom or act=T_N) then
--          sig = T_atom
--      elsif find(sig,T_intatm)
----          and (act=T_string or act=T_sequence or act=T_object) then
--          -- DEV: act=T_object should not upgrade, surely?
--        and find(act,T_seqstr) then
--          sig = T_object
--      elsif sig=T_string
--        and act=T_sequence then
--          sig = T_sequence
--      elsif find(sig,T_seqstr)
----          and (act=T_integer or act=T_atom or act=T_object) then
--        and find(act,T_intatm) then
--          sig = T_object
--      end if
--DEV 12/12/08:
--      if sig=T_object and act<=T_object then      -- 28/12
--DEV 6/2/9 (spotted in passing, untested)
        if sig<=T_object and act<=T_object then
            sig = or_bits(sig,act)
        end if
        return sig
    end if

    -- cannnot pass atom/int to string/sequence or vice versa,
    -- also type(type) indicates logic error (always true|false).
--  if (find(sig,T_intatm) and find(act,T_seqstr))
--  or (find(sig,T_seqstr) and find(act,T_intatm)) then
--DEV 12/12/08: (failed when  sig = 1, act = 724)
--    28/12/08: added these loops:
    rootAct = act
    while rootAct>T_object do
        rootAct = symtab[rootAct][S_sig][2]
    end while
    rootSig = sig
    while rootSig>T_object do
        rootSig = symtab[rootSig][S_sig][2]
    end while
    if (    not and_bits(rootSig,T_sequence)        -- if (     sig is an atom
        and not and_bits(rootAct,T_atom)    )       --      and act is a sequence)
    or (    not and_bits(rootSig,T_atom)            -- or (     sig is a sequence
        and not and_bits(rootAct,T_sequence)) then  --      and act is an atom)
        if routineNo<=T_sequence then
            add_ple(0)  -- probable logic error (always false)
        else
            bool bSq = sq_able()
            if not bSq or with_js=1 then
                Abork("incompatible type for routine signature",opsidx)
            end if
        end if
        not_js = true
        nj_reason = "1178"
    elsif routineNo=T_object then
--3/4/15: DEV only if param is init...
--      add_ple(1)  -- probable logic error (always true)
--      if symtab[vidx][S_Init] or symtab[vidx][S_Name]=-1 then
        if symtab[lastparam][S_Init] or symtab[lastparam][S_Name]=-1 then
            add_ple(1)  -- probable logic error (always true)
        end if
    elsif symtab[routineNo][S_NTyp]=S_Type then
        --
        -- Warn if type_routine(some_var) is provably always true/false.
        -- Eg:
        --  type tj(integer j) return j>=1 end type
        --  type tk(tj k) return k>=2 end type
        --==-- we now have a "type chain" of tk->tj->integer[->atom].
        --  (Technically, atom is not "on the chain", but dealt with
        --   manually, see under "Nevermind" comment below.)
        --  tj j    j=1
        --  tk k    k=2
        --  integer(j) --> ple(1)   (ie integer(j) CANNOT return false)
        --  atom(j)    --> ple(1)
        --  integer(k) --> ple(1)
        --  tj(j)      --> ple(1)
        --  tj(k)      --> ple(1)
        --  tk(j)       -- OK       (ie no warning rqd in this case)
        --  tk(k)      --> ple(1)
        --  sequence(j)--> ple(0)   (ie sequence(j) CANNOT return true)
        --  sequence(k)--> ple(0)
        --  type ts(sequence s) return length(s)=3 end type
        --  ts s    s = {1,2,3}
        --  integer(s) --> ple(0)
        --  ts(j)      --> ple(0)
        --  ts(k)      --> ple(0)
        --  ts(s)      --> ple(1)
        --  sequence(s)--> ple(1)
        --  string(s)   -- OK
        --
        -- Localtypes are also used, eg:
        --  object x,y
        --      x="string"
        --      string(x)   -- ple(1)
        --      atom(x)     -- ple(0)
        --      x=1
        --      string(x)   -- ple(0)
        --      integer(x)  -- ple(1)
        --      if integer(y) then
        --          string(y)   -- ple(0)
        --      end if
        --
        -- And, of course, builtin only, eg:
        --  string s
        --      sequence(s) -- ple(1)
        --      string(s)   -- ple(1)
        --      atom(s)     -- ple(0)
        --      integer(s)  -- ple(0)
        --
        -- Finally,
        --      object(s)   -- ple(1) (dealt with above)
        --
        -- so (1) if the routine we are calling is anywhere on the
        --        "type chain" of the parameter -> ple(1)
        --    (0) if rootTypes of routine and parameter have no
        --        common ground -> ple(0)
        --
        rootAct = act
        while 1 do
            if rootAct=routineNo then
                add_ple(1)  -- probable logic error (always true)
                exit
            end if
            -- I had kinda hoped to avoid this, by defining integer as "TN"
            --  string as "TP", and atom/sequence as "TO", but it spanners
            --  sig collection. Nevermind, (yoda) quite bearable this be.
            if rootAct<=T_object then
                if rootAct<T_atom then              -- ensure atom(<int>) gets ple(1)'d
                    rootAct = T_atom
                elsif rootAct=T_string              -- ensure sequence(<string>) gets ple(1)'d
                   or rootAct=T_Dsq then            -- ensure sequence(<dseq>) gets ple(1)'d
                    rootAct = T_sequence
                                                    -- (obviously integer(<int>), atom(<flt>),
                                                    --  string(<string>), and sequence(<string|dseq>) 
                                                    --  are handled without needing a similar fudge.)
                else
--                  sig = rootType(routineNo)
                    sig = routineNo
                    if sig>T_object then sig = rootType(sig) end if
                    if and_bits(sig,rootAct)=0 then
                        add_ple(0)  -- probable logic error (always false)
                    end if
                    exit
                end if
            else
                rootAct = symtab[rootAct][S_sig][2]     -- eg/ie tk->tj->integer
            end if
        end while
--  elsif routineNo=act then
--      add_ple(1)      -- probable logic error (always true)
--  elsif routineNo<=T_sequence then
--      sig = rootType(act)
--      if routineNo=sig
--      or (routineNo=T_atom and sig=T_integer)
--      or (routineNo=T_sequence and sig=T_string) then
--          add_ple(1)  -- probable logic error (always true)
--      end if
    end if
    return act
end function


integer t1, t2
function cmpcheck(integer scode)
--
-- Check that two items being compared are type sensibile,
--  eg atom x, sequence y -> 
--  "if x=y then" is just always going to be false.
--
sequence emsg   -- scratch error message var
    scode = find(scode,Bcde)    -- opJcc --> idx to Scde
    opsidxm1 = opsidx-1
    opsidxm2 = opsidx-2
    t1 = opstype[opsidxm2]
    t2 = opstype[opsidxm1]
    if (    not and_bits(t1,T_sequence)         -- if (     p1 is atom
        and not and_bits(t2,T_atom))            --      and p2 is sequence)
    or (    not and_bits(t1,T_atom)             -- or (     p1 is sequence
        and not and_bits(t2,T_sequence)) then   --      and p2 is atom)
        --           Bcde = {opJlt,opJle,opJeq,opJne,opJge,opJgt}
        --           Scde = {opSlt,opSle,opSeq,opSne,opSge,opSgt}
        --atom(t1),seq(t2):    T     T     F     T     F     F
        --seq(t1),atom(t2):    F     F     F     T     T     T
        emsg = "false"
        if not and_bits(t1,T_sequence) then -- t1 is atom (top line)
            if scode<=2 or scode=4 then
                emsg = "true"
            end if
        elsif scode>=4 then
            emsg = "true"
        end if
        WarnX("probable type error (always "&emsg&")",opsidxm1)
    end if
    return scode
end function

procedure Unassigned(integer sidx)
if newEmit then
    agcheckop(opUnassigned)
end if
    apnds5({opUnassigned,sidx})
end procedure

--without trace
--with trace

constant 
         opXxxBits = {opAndBits,opOrBits,opXorBits},
         opINSP = {opInt,opAtom,opStr,opSq},
         opINSPabits = {opInt,opAtom,opStr,opSq,opAndBits,opOrBits,opXorBits}


integer  p1, p2, p3

--integer wasMul
--      wasMul = 0

integer RHStype -- set by StoreVar, mainly used by Assignment, also DoReturn & DoConstant.
integer storeConst  -- used by DoConstant/StoreVar; Ntyp is a meaningless -1 (and will crash rootType)
        storeConst = 0  -- ie in constant x=expr, the type of x is set from RHStype.
integer isSubscript         -- 1 when result is a subscript, hence assume eg (int)+(int) is integer.
        isSubscript = 0
integer isCompound          -- 1 when Assignment finds eg s[i] += x
        isCompound = 0

--with trace
procedure StoreVar(integer N, integer NTyp)
--
-- Store a variable, applying any final operator as needed.
-- If N is zero, PopFactor (ie store in a new temporary variable of
--  the specified type). Otherwise N should be an index to symtab.
-- If storeConst is 1, NTyp is ignored/overridden, otherwise it
--  should usually be the declared or local type of N.
-- 
integer STyp
integer popFactor
integer scode, k, t, p4, p5, isInit
integer noofsubscripts, noofitems
sequence sytmp
integer resType
integer tii     -- twoInitInts()
sequence idii
atom w
--integer lblidx
--integer tidx

    popFactor = (N=0)
    scode = opstack[opsidx]

    if not opTopIsOp then           -- StoreVar,Plain
        if popFactor then ?9/0 end if
        STyp = opstype[opsidx]
--DEV this may want to be RHStype = symtab[opstack[opsidx]][S_v/ltype]) or RHStype=NTyp...
--  need to experiment with typecheck avoidance on udt=udt.
        RHStype = STyp
        if emitON and scode then
--          RHStype = symtab[scode][S_vtype]
            RHStype = symtab[scode][S_ltype]
        end if
        t = STyp
        if not storeConst then --(type not yet set!)
--          RHStype = rootType(NTyp)
--          t = rootType(NTyp)
            t = NTyp
            if t>T_object then t = rootType(t) end if
--DEV 7/11/2011...
if STyp!=0 then
            if not and_bits(STyp,t) then
--              Abort("type error")
                if not and_bits(STyp,T_atom)            -- so STyp(RHS) must be sequence
                and not and_bits(t,T_sequence) then     --   and t(LHS) must be atom
                    Aborp("type error (storing sequence in atom)")
                elsif not and_bits(STyp,T_sequence)     -- so STyp(RHS) must be atom
                  and not and_bits(t,T_atom) then       --   and t(LHS) must be sequence
                    if t=T_string then
                        Aborp("type error (storing atom in string)")
                    else
                        Aborp("type error (storing atom in sequence)")
                    end if
                elsif STyp=T_N
                  and t=T_integer then
                    Aborp("type error (storing float in integer)")
                else
                    -- (Extend above if more human-readable way than this:)
                    Abort(sprintf("type error (LHS=0b%04b, RHS=0b%04b)",{t,STyp}))
                end if
            end if
--else
--  if emitON then
--      puts(1,"internal warning: STyp=0 with emitON\n")
--  end if
end if
            -- btw, either STyp or t being T_object is enough to sidestep the above.
            --      (one must be 0bxx00 and the other 0b00xx, 0b1111 isn't either)
        end if
        if emitON then
            p1 = opstack[opsidx]
            if oneInitialisedInt(p1,0) then
                if t=T_integer or onDeclaration then
--              if t=T_integer then
-- or (exitBP=-1 -- not in a loop
--  and used=0
--  and (tvar or (Gvar and returnvar=1?)) then
                    scode = opMovbi
                else
                    scode = opMovsi
                end if
                RHStype = T_integer     -- 11/6/2012 (as below)
            else
                if t=T_integer then
                    scode = opMovti
                    RHStype = T_integer     -- 11/6/2012 (opMovti has built-in integer typecheck)
                else
                    scode = opMove
                end if
            end if
--9/1/17:
            if not symtab[p1][S_Init] then
                Unassigned(p1)
            end if

            emitHexMov(scode,N,p1)
        end if
        freeTmp(-1)

    elsif opTopIsOp=BltinOp then
--      if opsidx<2 then ?9/0 end if
--      opsidxm1 = opsidx-1
--      STyp = opstype[opsidxm1]
--DEV I do want some type checking, well lots really...
--      if find(STyp,"SP") then
--          Aborp("type error")
--      end if
--      scode = opstack[opsidx]
--      if STyp=T_object then
--          STyp = T_atom
--      end if
if newEBP then
        -- save eax if rqd
--DEV needed for opXxxBits (as they use leamov()):
--      saveFunctionResultVars(opsidx,NOTINTS)
        saveFunctionResultVars(opsidx,INTSTOO)
end if
-- 14/11/14:
--      if scode>T_Bin
--      and scode<=T_Asm then
        if newEmit
        and aasydx[scode]>T_Bin
        and aasydx[scode]<=T_Asm then
            STyp = aartypes[scode]
            -- (DEV: equivalent code to that below may be required here as opcodes are migrated, then again
            --       wouldn't it be better to have opFloori and opRmdrii, which are the only ones of note..)
        else
--if find(scode,{opInt,opAtom,opSq,opStr,opObj})=0 then
--  if newEmit then ?9/0 end if -- DEV 23/10/15 (quick test)    -- needed above guard, so i think it is just symtab[1..15]
--end if
            STyp = T_integer
--removed 23/10/15: (on the strength of the above test)
----            if scode>T_Pres then
--          if scode>opIres then
--              if scode<=opNres then
--                  STyp = T_atom
--                  if scode=opFloor then
--                      opsidxm1 = opsidx-1
--                      if opstype[opsidxm1]=T_integer then
--                          STyp = T_integer
--                      end if
--                  elsif scode=opRmdr then
---- uh?? undone (above 6 lines replaced) 24/2/09:
---- 12/12/08: No! floor(99/1e-208) is 9.9e209, not an int!
----                if scode=opFloor
----                or scode=opRmdr then
--                      opsidxm1 = opsidx-1
--                      opsidxm2 = opsidx-2
--                      if opstype[opsidxm1]=T_integer
--                      and opstype[opsidxm2]=T_integer then
--                          STyp = T_integer
--                      end if
--                  end if
--              elsif scode<=opSres then
--                  STyp = T_string
--              elsif scode<=opPres then
----DEV T_Dsq for some?
----        opAto32/opAto64 currently return T_Dsq but T_string theoretically valid, (updated: 11/01/10)
----        opGetPos does indeed always return T_Dsq
----        opGSCh is not yet written! (DOS only??)
----        (in conclusion: these four are not really worth worrying about)
--                  STyp = T_sequence
--                  if scode=opAto32
--                  or scode=opAto64 then
--                      STyp = T_string
--                  elsif scode=opGetPos then
--                      STyp = T_Dsq
--                  end if
--              elsif scode<=opOres then
--                  --05/06/2010:
--                  if scode=opGets then
--                      STyp = 9 -- 0b1001
--                  else
--                      STyp = T_object
--                  end if
--              end if
--          end if
        end if
        RHStype = STyp  -- for Assignment()
--if scode=opCallFunc then
--  ?RHStype    -- 15
--end if
        t = -1
        if popFactor then
--18/1/2013:
--          if scode=opObj then
--              N = T_const1 -- a literal 1
--              t = T_integer
--          else
                -- some opcodes make a better STyp guess later
                if not find(scode,opXxxBits)
                and scode!=opNotBits
                and scode!=opPeek
                and scode!=opApnd   -- 5/3
                and scode!=opPpnd   -- 6/3
                and emitON then
                    N = newTempVar(STyp,Shared)
                end if
                t = STyp
--          end if
        else
            if storeConst then
                NTyp = RHStype
            end if
--          t = rootType(NTyp)
            t = NTyp
            if t>T_object then t = rootType(t) end if
--          if STyp=T_atom and t=T_integer and scode>=opTime then
--              Aborp("type error (storing atom in integer)")
--          elsif SType=T_sequence and t=T_string then
--              Warn("storing general sequence in ascii string",tokline,tokcol,0)
            if not and_bits(STyp,T_atom)            -- so STyp(RHS) is a sequence
            and not and_bits(t,T_sequence) then     --   and t(LHS) is an atom
                Aborp("type error (storing sequence in atom)")

            elsif not and_bits(STyp,T_sequence)     -- so STyp(RHS) is an atom
              and not and_bits(t,T_atom) then       --   and t(LHS) is a sequence
--DEV might be able to sq_xxx
--  opLen/opFloor/opNotBits/opRand/opRmdr/opXxxBits/opPow/
--  opCos/opSin/opTan/opArcTan/opLog/opSqrt
                Aborp("type error (storing atom in sequence)")
            end if

        end if
        opsidx -= 1
        if scode<opOpen then        --DEV bad code smell!
            if scode=opLen then
                if emitON then
                    p2 = opstack[opsidx]
                    sytmp = symtab[p2]
                    isInit = sytmp[S_Init]
if newEmit then
                    agcheckop(opLen)
end if
                    apnds5({opLen,N,p2,isInit,opstype[opsidx]})
                end if  -- emitON
                freeTmp(-1)

--          elsif scode=opObj then  -- do nowt
--              -- triggered 4/10/08:  return object(a)
--              freeTmp(-1)
--
--          elsif scode>=opInt then -- opINSP
            elsif scode>=opInt then -- opINSPO
                if emitON then
                    p2 = opstack[opsidx]
                    sytmp = symtab[p2]
                    isInit = sytmp[S_Init]
--18/1/2013:
if scode!=opObj or isInit=0 then
if newEmit then
                    agcheckop(scode)
end if
                    apnds5({scode,N,p2,isInit})
else
                    N = T_const1 -- a literal 1
                    t = T_integer
end if
                end if  -- emitON
                freeTmp(-1)

--          elsif scode=opFind
--             or scode=opMatch then
--              if emitON then
--                  opsidxm1 = opsidx-1
--                  opsidxm2 = opsidx-2
--                  p2 = opstack[opsidxm2]
--                  p3 = opstack[opsidxm1]
--                  p4 = opstack[opsidx]
--                  apnds5({scode,N,p2,p3,p4})
--              end if  -- emitON
--              freeTmp(-3)
--
            else
                if emitON then
                    opsidxm1 = opsidx-1
                    p2 = opstack[opsidxm1]
                    p3 = opstack[opsidx]
                    if scode=opSeq then

                        apnds5({scode,N,p2,p3,0,0})

                    elsif scode=opScmp then

if newEmit then
                        agcheckop(opScmp)
end if
                        apnds5({scode,N,p2,p3})

                    else
                        ?9/0
                    end if
                end if  -- emitON
                freeTmp(-2)

            end if
        elsif scode=opFloor 
--DEV??
           or scode=opOpenDLL
           or scode=opGetc then
            if emitON then
                p2 = opstack[opsidx]
                if scode=opFloor
                and symtab[p2][S_NTyp]=S_Const
                and symtab[p2][S_vtype]=T_atom then
                    w = floor(symtab[p2][S_value])
--3/1/16:
--                  if integer(w) then
                    if not isFLOAT(w) then
                        p2 = addUnnamedConstant(w,T_integer)
                        if t=T_integer then
                            scode = opMovbi
                        else
                            scode = opMovsi
                        end if                          
                        emitHexMov(scode,N,p2)
                        scode = 0
                        RHStype = T_integer
                    end if
                end if
                if scode then
if newEmit then
                    agcheckop(scode)
end if
                    apnds5({scode,N,p2})
                end if
            end if  -- emitON
            freeTmp(-1)

        elsif scode=opGets
           or scode=opWhere --DEV currently hll (as below)
--         or scode=opChDir
           or scode=opAlloc --DEV currently hll (but not for long)
           or scode=opNotBits
           or scode=opRand then
--DEV (spotted in passing) op80toA? or: these are hll now...
--         or scode=op32toA
--         or scode=op64toA then

            if emitON then
                p2 = opstack[opsidx]
                if scode=opNotBits then
                    if opstype[opsidx]=T_integer then
                        -- not_bits(integer) is always an integer
                        -- (first two bits of an integer are 00 or 11)
                        RHStype = T_integer
                        STyp = T_integer
                    end if
                    if popFactor then
                        N = newTempVar(STyp,Shared)
                    end if
                end if
                isInit = symtab[p2][S_Init]
--DEV added 27/7/14 (to get terror.exw working again):
                if not symtab[p2][S_Init] then
                    Unassigned(p2)
                end if
if newEmit then
                agcheckop(scode)
end if
                apnds5({scode,N,p2,isInit})
            end if  -- emitON
            freeTmp(-1)
--DEV (temp)
--      elsif scode=opMalloc then
--          if emitON then
--              p2 = opstack[opsidx]
--              apnds5({scode,N,p2})
--          end if  -- emitON
--          freeTmp(-1)
        elsif scode>=opRmdr and scode<=opPow then   -- or opXxxBits

            if emitON then
                opsidxm1 = opsidx-1
                p2 = opstack[opsidxm1]
                p3 = opstack[opsidx]
                if scode=opRmdr or scode=opPow then

--if newEmit then
                    bothInit = 1
--else
--                  bothInit = (symtab[p2][S_Init] and symtab[p3][S_Init])
--end if

                else    -- opAndBits/opOrBits/opXorBits
                    opsidx += 1 -- we damaged this above... (re-done below)
                    if twoInitInts(BOTH) then
                        -- result will also be integer, just in case that is not obvious:
                        -- first two bits are either 11 or 00 (for all short integers):
                        --      . 11 00       . 11 00        . 11 00
                        -- and 11 11 00   or 11 11 11   xor 11 00 00
                        --     00 00 00      00 11 00       00 00 00
                        -- hence first two bits of result are 11 or 00;  
                        --  a short int in all (twelve) possible cases.
                        RHStype = T_integer
                        if popFactor then
                            N = newTempVar(T_integer,Shared)
                            NTyp = T_integer
                        end if
                        bothInit = 1    -- (not exactly as returned from twoInitInts?)

                    else    -- not twoInitInts()
                        if scode=opAndBits then
                            -- if either was a positive integer, result will be too (not Or/Xor though)
                            -- (may still need to load params via FPU though)
                            if oneInitialisedInt(p3,1) then
                                if constInt
                                and constIntVal>0 then
                                    RHStype = T_integer
                                end if
                            elsif oneInitialisedInt(p2,1) then
                                if constInt
                                and constIntVal>0 then
                                    RHStype = T_integer
                                end if
                            end if
                        end if
                        if popFactor then
                            if RHStype=T_integer then
                                STyp = T_integer
                            end if
                            N = newTempVar(STyp,Shared)
                        end if
                        bothInit = 0

                    end if
                    opsidx -= 1 -- re-do what we undid...
                end if
-- 16/10/14:
--if newEmit then
                if not symtab[p2][S_Init] then
                    Unassigned(p2)
                end if
                if not symtab[p3][S_Init] then
                    Unassigned(p3)
                end if
                agcheckop(scode)
--end if
                apnds5({scode,N,p2,p3,bothInit})
            end if  -- emitON
            freeTmp(-2)

        elsif scode=opApnd
           or scode=opPpnd
--         or scode=opRepeat
--         or scode=opRepCh
           or scode=opOpen
           or scode=opCallFunc
           or scode=opSeek then

            if emitON then
                opsidxm1 = opsidx-1
                p2 = opstack[opsidxm1]
                p3 = opstack[opsidx]

                if scode=opApnd
                or scode=opPpnd then
--if 01 then    -- 5/3:
--DEV is something similar appropriate for concat/prepend?
                    if popFactor then
                        -- allocate a new tmp, except for pop(append(tmp1,x)), which
                        --  becomes tmp1=append(tmp1,x) rather than tmp2=append(tmp1,x):
--DEV 20/11/2011: (there may be more like this)
--                      if symtab[p2][S_Name]=-1 and symtab[p2][S_NTyp]=S_TVar then
                        if symtab[p2][S_Name]=-1 and symtab[p2][S_NTyp]!=S_Const then
                            N = p2
                            -- prevent freeTmp doing p2 (a later freeTmp on N does the job).
                            opsltrl[opsidxm1] = 0
                        else
                            N = newTempVar(STyp,Shared)
                        end if
--DEV one like that 20/11/2011 right here?
--29/6/21:
--                  elsif symtab[p2][S_Name]=-1 and symtab[p2][S_NTyp]=S_TVar and N!=p3 then
                    elsif symtab[p2][S_Name]=-1 and symtab[p2][S_NTyp]!=S_Const and N!=p3 then
                        -- replace b=append(tmp,x) with b=tmp tmp=0 b=append(b,x):
                        -- (function results are treated as tmp in this regard)
                        emitHexMov(opMove, N, p2)
                        p2 = N
                    end if
--else
--                  if symtab[p2][S_Name]=-1 and symtab[p2][S_NTyp]=S_TVar and N!=p3 then
--                      -- replace b=append(tmp,x) with b=tmp tmp=0 b=append(b,x):
--                      -- (function results are treated as tmp in this regard)
--                      emitHexMov(opMove, N, p2)
--                      p2 = N
--                  end if
--end if
                end if
-- 10/4/14: (newEmit)
                if not symtab[p2][S_Init] then
                    Unassigned(p2)
                end if
                if not symtab[p3][S_Init] then
                    Unassigned(p3)
                end if
if newEmit then
--         if scode=opOpen
--         or scode=opSeek then
--puts(1,"agcheck opOpen/opSeek omitted, line 1411 pmain.e\n")
--          else
                agcheckop(scode)
--          end if
end if
                apnds5({scode,N,p2,p3})
            end if  -- emitON
            freeTmp(-2)

        elsif scode=opCurrDir
           or scode=opInstance
           or scode=opTime
           or scode=opWaitKey
           or scode=opGetKey
           or scode=opGetPos then

            if emitON then
                agcheckop(scode)
                apnds5({scode,N})
            end if

--      elsif (scode>=opCos and scode<=opSqrt)  -- opCos/opSin/opTan/opArcTan/opLog/opSqrt [92..97]
--         or scode=opAto32
--         or scode=opAto64 then
        elsif scode=opCos
           or scode=opSin
           or scode=opTan
           or scode=opArcTan
           or scode=opLog
           or scode=opSqrt then

            if emitON then
                p2 = opstack[opsidx]
                agcheckop(scode)
                apnds5({scode,N,p2})
            end if
            freeTmp(-1)
                
        elsif scode=opPeek
           or scode=opPeek1s
           or scode=opPeek1u
           or scode=opPeek2s
           or scode=opPeek2u
           or scode=opPeek4s
           or scode=opPeek4u
           or scode=opPeek8s
           or scode=opPeek8u
           or scode=opPeekNS then

            if emitON then
--DEV or opPeek1u (?) [but we should probably drop opPeeki and opUnassign/inline peek(1|2)(s|u) anyway]
-- (fyi, opPeeki does not occur anywhere else in the front end other than the next 30-odd lines)
                if scode=opPeek then

                    k = not and_bits(opstype[opsidx],T_sequence)    -- peek(int|atm) yields 0..255
                                                                    -- (whereas peek(seq) yields string)
                    if t=T_integer then
if not newEmit then -- DEV calling opAddiii?? 17/1/15...
                        scode = opPeeki
end if
                        STyp = T_integer
                        RHStype = T_integer
                    elsif k then
--DEV added 22/3/09...
                        if popFactor then
if not newEmit then -- ditto
                            scode = opPeeki
end if
                        end if
                        STyp = T_integer
                        RHStype = T_integer
                    end if
                    if popFactor then
                        N = newTempVar(STyp,Shared)
                    end if

                end if

                p2 = opstack[opsidx]

if newEmit then
                if not symtab[p2][S_Init] then
                    Unassigned(p2)
                end if
                agcheckop(scode)
end if
                if scode=opPeeki then
                    isInit = (symtab[p2][S_Init] and k)
                    apnds5({scode,N,p2,isInit})
                elsif scode=opPeekNS then
                    p3 = p2 
                    p2 = opstack[opsidx-1]
                    p1 = opstack[opsidx-2]
                    if not symtab[p2][S_Init] then
                        Unassigned(p2)
                    end if
                    if not symtab[p1][S_Init] then
                        Unassigned(p1)
                    end if
                    apnds5({scode,N,p1,p2,p3})  -- opPeekNS,addr,size,sign
--                  freeTmp(-2)
                else
                    apnds5({scode,N,p2})
                end if

            end if  -- emitON
            if scode=opPeekNS then
                freeTmp(-3)
            else
                freeTmp(-1)
            end if

        elsif scode=opLock then
--if newEmit then ?9/0 end if
            if emitON then
                opsidxm1 = opsidx-1
                opsidxm2 = opsidx-2
                p4 = opstack[opsidx]
                p3 = opstack[opsidxm1]
                p2 = opstack[opsidxm2]

                apnds5({opLock,N,p2,p3,p4})

            end if  -- emitON

            freeTmp(-3)

--/*
    opInitCS = 234,     -- opInitCS,dest
    opDeleteCS = 235,   -- opDeleteCS,src
    opEnterCS = 236,    -- opEnterCS,src(/T_const0)
    opTryCS = 237,      -- opTryCS,dest,src
    opLeaveCS = 238,    -- opLeaveCS,src(/T_const0)
--*/
        elsif scode=opInitCS then
            if emitON then
                agcheckop(scode)
                apnds5({opInitCS,N})
            end if  -- emitON
        elsif scode=opTryCS
           or scode=opCallback
           or scode=opTextRows then
            if emitON then
                p2 = opstack[opsidx]
                if not symtab[p2][S_Init] then
                    Unassigned(p2)
                end if
                agcheckop(scode)
                apnds5({scode,N,p2})
            end if  -- emitON
            freeTmp(-1)
        elsif scode=opGetText
           or scode=opDcvar
           or scode=opDelRtn then
            if emitON then
                opsidxm1 = opsidx-1
                p2 = opstack[opsidxm1]
                if not symtab[p2][S_Init] then
                    Unassigned(p2)
                end if
                p3 = opstack[opsidx]
--DEV 12/7/16 bit harsh innit??? constants only?? (K-aod only??)
                if scode=opDelRtn
                and p3!=T_const0
                and and_bits(symtab[p3][S_State],K_rtn)=0 then
                    Aborp("routine_id expected")
                end if
                if not symtab[p3][S_Init] then
                    Unassigned(p3)
                end if
                agcheckop(scode)
                apnds5({scode,N,p2,p3})
            end if  -- emitON
            freeTmp(-2)
        elsif scode=opDcfunc then
            if emitON then
                p2 = opstack[opsidx-3]
                if not symtab[p2][S_Init] then
                    Unassigned(p2)
                end if
                p3 = opstack[opsidx-2]
                if not symtab[p3][S_Init] then
                    Unassigned(p3)
                end if
                p4 = opstack[opsidx-1]
                if not symtab[p4][S_Init] then
                    Unassigned(p4)
                end if
                p5 = opstack[opsidx]
                if not symtab[p5][S_Init] then
                    Unassigned(p5)
                end if
                agcheckop(scode)
                apnds5({opDcfunc,N,p2,p3,p4,p5})
            end if  -- emitON
            freeTmp(-4)
        else
            printf(1,"internal error pmain.e line 1664, unknown scode(%d=%s), tokline=%d)\n",{scode,opNames[scode],tokline})
            ?9/0    -- unknown scode
        end if

    elsif opTopIsOp=SubscriptOp then
        -- scode is always opSubse, btw
        --added 29/4/12
        if newEBP then
            -- save eax if rqd
            saveFunctionResultVars(opsidx,INTSTOO)
        end if

        opsidx -= 1
        noofsubscripts = opstack[opsidx]
        opsidx -= 1
        k = opsidx-noofsubscripts+1
        --Alternatively, see DoSubScripts? (if error messages not quite right)
        for i=k to opsidx do
            t = opstype[i]
            if not and_bits(t,T_atom) then
                Abork("type error (subscript must be an atom)",i)
            end if
        end for
        t = opstype[k-1]
        if not and_bits(t,T_sequence) or (noofsubscripts>1 and t=T_string) then
            Abork("type error (attempt to subscript an atom)",k)            -- ie obj := int/atm/char[idx]
        end if
        RHStype = T_object
        if t=T_string then
            if not popFactor
            and not and_bits(rootType(NTyp),T_atom) then
                Abork("type error (storing character in sequence)",opsidx-1)    -- ie seq/str := string[i]
            end if
            RHStype = T_integer
        end if
        if emitON then -- (necessary; else rejoin patches knacker previous code)
            if popFactor then
                N = newTempVar(RHStype,Shared)
            end if
            if noofsubscripts=1 then
                opsidxm1 = opsidx-1
                p1 = opstack[opsidxm1]      -- ref
                if p1=lhsvar then
                    lhsvar = 0              -- (added 14/8/15)
                end if
                p2 = opstack[opsidx]        -- idx
                --
                -- opSubse1 is res, ref, idx:   res:=ref[idx], aka N=p1[p2].
                --
                -- Is res an integer?
                --
                resType = RHStype
                if not popFactor and not storeConst then
--                  resType = rootType(NTyp)
                    resType = NTyp
                    if resType>T_object then resType = rootType(resType) end if
                end if

                scode = opSubse1
                if resType=T_integer then
                    if t=T_string then
                        scode = opSubse1is
--                     (RHStype = T_integer is already set above)
--                  elsif t=T_sequence then
                    elsif not and_bits(t,T_atom) then   -- nb: opSubse1i chokes on float/int refs.
                        scode = opSubse1i
                        RHStype = T_integer -- (opSubse1i has builtin integer typecheck)
--                  else
--                      -- leave scode as opSubse1
--                      -- pilxl.e may further deduce opSubse1ip (after gscan complete)
                    end if
                end if
--              isInit = (symtab[p1][S_Init] and symtab[p2][S_Init])
                isInit = 3 -- both
--DEV no longer needed for opSubse1ip (which is decided on in pilx86.e) [ie kill when/if we clear the lot]
                if not symtab[p1][S_Init] then
--DEV not sure this helps: (we can emit this later, if we change isInit to 3=both, 1=p1 only, 2=p2 only, 0=neither)
                    Unassigned(p1)              -- ref
                    isInit = 2 -- p2 only
                end if
-- (...whereas I plan on keeping this [opUnassigned(idx)] forever)
                if not symtab[p2][S_Init] then
                    Unassigned(p2)              -- idx
                    isInit -= 2
                end if
if newEmit then
                agcheckop(scode)
end if
                apnds5({scode,N,p1,p2,isInit,isCompound})   -- N=p1[p2]

            else    -- noofsubscripts!=1
                --
                -- opSubse is N, res, idxN..idx1, ref
                --      implements res := ref[idx1][idx2]~[idxN]
                --
                idii = {}
                for j=opsidx to k by -1 do
                    p1 = opstack[j]
                    if not symtab[p1][S_Init] then
                        Unassigned(p1)
                    end if
                    idii &= p1
                end for
if newEmit then
                agcheckop(opSubse)
--              ttidx = aatidx[opSubse]
--              if tt[ttidx+EQ]=0 then
--                  agcheck(ttidx)
----                    lblidx = get_lblidx(ttidx)
--              end if
end if
                apnds5({opSubse,noofsubscripts,N})
--14/8/15
--              idii &= opstack[k-1]
                p2 = opstack[k-1]
                idii &= p2
                if p2=lhsvar then
                    lhsvar = 0
                end if
                apnds5(idii)
            end if
        end if -- emitON
        freeTmp(-noofsubscripts-1)
        isCompound = 0

    elsif opTopIsOp=MathOp then
        if DEBUG then
            if opsidx<3 then ?9/0 end if
        end if
if newEBP then
        -- save eax if rqd
--DEV needed for opMaths (as they use leamov()):
--      saveFunctionResultVars(opsidx,NOTINTS)
        saveFunctionResultVars(opsidx,INTSTOO)
end if
        opsidxm2 = opsidx-2
        t2 = opstype[opsidxm2]
-- DEV these errors appear to have been superceded by warnings (search for sqopWarn)
--      if not and_bits(t2,T_atom) then
--          sqErr(scode,opsidxm2)
--      end if
        opsidxm1 = opsidx-1
        t1 = opstype[opsidxm1]
--      if not and_bits(t1,T_atom) then
--          sqErr(scode,opsidxm1)
--      end if

        RHStype = T_atom
        if storeConst then
            NTyp = T_atom
        end if
--DEV but not opDiv?? (opDiviii, opDivf should be ok)
        if isSubscript and t1=T_integer and t2=T_integer then
            RHStype = T_integer
            NTyp = T_integer
        end if

        p3 = opstack[opsidxm1]
        p2 = opstack[opsidxm2]

--23/8/14: (/before/ twoInitInts gets called!)
        if emitON then
            if not symtab[p2][S_Init] then
                Unassigned(p2)
            end if
            if not symtab[p3][S_Init] then
                Unassigned(p3)
            end if
        end if  -- emitON
--if o2 then
--      if scode=opDiv
--      and NTyp=T_integer then -- target is integer
--          scode=opDivf
--      end if
--end if
        tii = twoInitInts(BOTH)
        if scode<=opDiv then    --opAdd/opSub/opMul/opDiv [only!]
            if NTyp=T_integer then          -- target is integer
                if tii then
                    scode += 2  -- opAddiii/opSubiii/opMuliii/opDiviii
                else
                    scode += 1  -- opAddi/opSubi/opMuli/opDivi
                end if
                RHStype = T_integer
            end if
        elsif scode=opDivf then -- from T_floor check in Call()
            if tii then
--DEV (move/copy this to pilxl86)
                -- floor(int/int) is int except for minint / -1
                --  (and of course /0, which crashes anyway)
                -- firstintval should only ever be tested for >-2,
                --  seems pretty pointless to bother with that.
                -- secondintval should only ever be tested for >=-1
                --  We deal with 1,2 straightafter, seems better for
                --  the opcode to handle -1,0, so >2 test it is
                if secondintval>2 then  -- (set to -2 if a var, btw)
                    RHStype = T_integer
                end if
                secondintval = find(secondintval,{#1,#2,#4,#8,
                                                  #10,#20,#40,#80,
                                                  #100,#200,#400,#800,
                                                  #1000,#2000,#4000,#8000,
                                                  #10000,#20000,#40000,#80000,
                                                  #100000,#200000,#400000,#800000,
                                                  #1000000,#2000000,#4000000,#8000000,
                                                  #10000000,#20000000})
                if secondintval then -- now power of 2 plus 1
                    scode = opDivf2
                    p3 = secondintval
                    RHStype = T_integer
                end if
            end if
        end if

        if emitON then
            if popFactor then
-- added 6/4/2012 (no need for temp if pilx86 is just going to call e02atdb0)
                if scode=opDiviii
                and t1=T_integer
                and p3!=0
                and symtab[p3][S_NTyp]=S_Const
                and symtab[p3][S_Init]
                and symtab[p3][S_value]=0 then
                    N = 0
                else
                    N = newTempVar(RHStype,Shared)
                end if
            end if
if newEmit then
            agcheckop(scode)
--  printf(1,"scode=%d, no agcheckop, line 1707 pmain.e\n",{scode})
end if
            apnds5({scode,N,p2,p3})
        end if  -- emitON

        freeTmp(-3)

    elsif opTopIsOp=ConcatOp then   -- opConcat, opConcatN
        if DEBUG then
            if opsidx<3 then ?9/0 end if
        end if
if newEBP then
        -- save eax if rqd
--DEV needed for opApnd/Ppnd (as they use leamov()):
--      saveFunctionResultVars(opsidx,NOTINTS)
  if scode=opConcat then -- normal binary concat
        saveFunctionResultVars(opsidx,INTSTOO)
  else
        saveFunctionResultVars(opsidx,NOTINTS)
  end if
end if
        opsidxm1 = opsidx-1
        opsidxm2 = opsidx-2
        scode = opstack[opsidx]
        if scode=opConcat then -- normal binary concat
            RHStype = T_sequence
            t1 = opstype[opsidxm1]  -- NB this is p3
            t2 = opstype[opsidxm2]  -- this is p2
            if t1=T_string and t2=T_string then
                RHStype = T_string
            end if
            if popFactor then
                N = newTempVar(RHStype,Shared)
            elsif not storeConst then
--              t = rootType(NTyp)              -- this is p1
                t = NTyp                        -- this is p1
                if t>T_object then t = rootType(t) end if
                if not and_bits(t,T_sequence) then  -- so t(LHS) must be atom
                    Abork("type error (storing sequence in atom)",opsidxm1) -- ie/eg i=x&y
                end if
            end if
            p2 = opstack[opsidxm2]
            p3 = opstack[opsidxm1]

            if emitON then

                if not and_bits(t1,T_atom)              -- NB this is p3    (if p3 is a sequence)
                and not and_bits(t2,T_sequence) then    -- this is p2       (if p2 is an atom)

                    -- replace a=atm&seq with a=prepend(seq,atm)...

--DEV !=S_Const?
--21/6/21 
--                  if symtab[p3][S_Name]=-1 and symtab[p3][S_NTyp]=S_TVar and N!=p2 then
                    if symtab[p3][S_Name]=-1 and symtab[p3][S_NTyp]!=S_Const and N!=p2 then
                        -- ... and replace b=prepend(tmp,x) with b=tmp tmp=0 b=prepend(b,x):
                        -- (function results are treated as tmp in this regard)
                        emitHexMov(opMove, N, p3)   -- gets the Tvar/temp treatment
                        p3 = N
                    end if

--                  apnds5({opPpnd,N,p3,p2})
--                  tmp = p2
--                  p2 = p3
--                  p3 = tmp
--                  scode = opPpnd
                    {scode,p2,p3} = {opPpnd,p3,p2}

                else
--DEV ditto
--                  if symtab[p2][S_Name]=-1 and symtab[p2][S_NTyp]=S_TVar and N!=p3 then
                    if symtab[p2][S_Name]=-1 and symtab[p2][S_NTyp]!=S_Const and N!=p3 then
                        -- replace b=tmp&x with b=tmp tmp=0 b=b&x:
                        -- (function results are treated as tmp in this regard)
                        emitHexMov(opMove, N, p2)   -- gets the Tvar/tmp treatment
                        p2 = N
                    end if
                    if not and_bits(t1,T_sequence)      -- NB this is p3    (if p3 is an atom)
                    and not and_bits(t2,T_atom) then    -- this is p2       (if p2 is a sequence)

                        -- replace a=seq&atm with a=append(seq,atm)
                        scode = opApnd

                    end if
--                  apnds5({scode,N,p2,p3})
                end if
                if not symtab[p2][S_Init] then
                    Unassigned(p2)
                end if
                if not symtab[p3][S_Init] then
                    Unassigned(p3)
                end if
if newEmit then
                agcheckop(scode)
end if
                apnds5({scode,N,p2,p3})
            end if  -- emitON
            freeTmp(-3)
        else                                -- opConcatN
--trace(1)
            RHStype = T_string
            noofitems = opstack[opsidxm1]
            opsidx -= 2
            if emitON then
                k = opsidx-noofitems+1
                for i=k to opsidx do
                    t = opstype[i]
                    if t!=T_string then
                        -- check for constant literal chars such as '\n':
                        if t!=T_integer then
                            RHStype = T_sequence
                            exit
                        else
                            sytmp = symtab[opstack[i]]
                            if sytmp[S_NTyp]!=S_Const
                            or sytmp[S_vtype]!=T_integer
                            or sytmp[S_value]<0
                            or sytmp[S_value]>255 then
                                RHStype = T_sequence
                                exit
                            end if
                        end if
                    end if
                end for
                if popFactor then
                    N = newTempVar(RHStype,Shared)
                end if
                --
                -- opConcatN is N, refN..ref1, res
                --      implements res := ref1&ref2..&refN
                --
                idii = {}
                for j=opsidx to k by -1 do
                    p1 = opstack[j]
                    if not symtab[p1][S_Init] then
                        Unassigned(p1)
                    end if
                    idii &= p1
                end for
if newEmit then
                agcheckop(opConcatN)
end if
                apnds5({opConcatN,noofitems})
                idii &= N
                apnds5(idii)
            end if -- emitON

            freeTmp(-noofitems)
        end if

    elsif opTopIsOp=BranchOp then   -- branch, I mean relational...

        --added 29/3/17
        if newEBP then
            -- save eax if rqd
            saveFunctionResultVars(opsidx,INTSTOO)
        end if

        scode = cmpcheck(scode)
--      scode = Scde[scode]

        if not storeConst --(type not yet set!)
        and not popFactor then
            t = NTyp
            if t>T_object then t = rootType(t) end if
            if not and_bits(t,T_integer) then
--DEV can we not sq_xxx this??
-- (see mods dated 29/04/2010 in Assignment)
                Abort("type error (storing integer in sequence)")
            end if
        end if

        RHStype=T_integer

        if emitON then
            if popFactor then
                N = newTempVar(T_integer,Shared)
                NTyp = T_integer
            elsif NTyp>T_object then
                NTyp = rootType(NTyp)
            end if
            p2 = opstack[opsidxm2]
            p3 = opstack[opsidxm1]
            tii = twoInitInts(eJmp[scode])  -- eq/ne are EITHER, lt/le/gt/ge are BOTH
            scode = Scde[scode]
if newEmit then
            agcheckop(opJeq)    -- (I /think/ that will do the trick...)
--?9/0
--  printf(1,"scode=%d, no agcheckop, line 1880 pmain.e\n",{scode})
end if
            apnds5({scode,N,p2,p3,tii,bothInit,NTyp=T_integer}) -- opScc,res,ref,ref,tii,bothInit,isInt

        end if  -- emitON
        freeTmp(-3)

    elsif opTopIsOp=SliceOp then
        -- scode is always opSubss, btw
        --added 29/4/12 (spotted in passing)
        if newEBP then
            -- save eax if rqd
            saveFunctionResultVars(opsidx,INTSTOO)
        end if
        opsidx -= 1
        noofsubscripts = opstack[opsidx]
        opsidx -= 1
        k = opsidx-noofsubscripts-1
        t = opstype[k]
-- 25/3:
--      if not and_bits(t,T_sequence) or (noofsubscripts>1 and t=T_string) then
        t = and_bits(t,T_sequence)
        if t=0 or (noofsubscripts>1 and t=T_string) then
            Abork("type error (attempt to slice an atom)",k)
        end if
        RHStype = t
        for i=k+1 to opsidx do
            t = opstype[i]
            if not and_bits(t,T_atom) then
                Abork("type error (subscript must be an atom)",i)
            end if
        end for
        if emitON then
            if popFactor then
                N = newTempVar(RHStype,Shared)
            end if
            p2 = opstack[k]
            if p2=lhsvar then
                lhsvar = 0      -- (added 14/8/15)
            end if
            --
            -- opSubss is N, res, sliceend, idxN..idx1, ref
            --      implements res := ref[idx1]~[idxN..sliceend]
            --
            if not symtab[p2][S_Init] then -- ref
                Unassigned(p2)
            end if
            idii = {}
            for j=opsidx to k+1 by -1 do
                p1 = opstack[j]
                if not symtab[p1][S_Init] then
                    Unassigned(p1)
                end if
                idii &= p1
            end for
if newEmit then
            agcheckop(opSubss)
end if
            apnds5({opSubss,noofsubscripts,N})      -- n, res
            idii &= p2
            apnds5(idii)
        end if  -- emitON
        freeTmp(-noofsubscripts-2)

    elsif opTopIsOp=UnaryOp then
        if DEBUG then
            if opsidx<2 then ?9/0 end if
        end if
--DEV broke t04... (investigation rqd)
-- 27/08/2020... t04 now fine, p-cp/p-test fine, but p p -test fails on t64... [DEV]
--      --added 29/4/12 (spotted in passing)
--      if newEBP then
--          -- save eax if rqd
            saveFunctionResultVars(opsidx,INTSTOO)
--      end if
        opsidxm1 = opsidx-1
        STyp = opstype[opsidxm1]
        if not and_bits(STyp,T_atom) then
            -- should have been replaced with sq_xxx() in GetFactor():
            Aborp("internal error (sq_upop)")
        end if
        if scode=opNot then
            RHStype=T_integer       -- 0/1 is an integer ;-)
        elsif scode=opUminus then
            RHStype=T_atom      -- (-minint = maxint+1, so ends up a float)
        else ?9/0
        end if
        if emitON then
            if popFactor then
                N = newTempVar(RHStype,Shared)
                NTyp = RHStype
            end if
            p2 = opstack[opsidxm1]
if newEmit then
            if not symtab[p2][S_Init] then
                Unassigned(p2)
            end if
            agcheckop(scode)
end if
            apnds5({scode,N,p2})    -- opNot/opUminus
        end if  -- emitON
        freeTmp(-2)

    elsif opTopIsOp=LogicOp then    -- and,or,xor
        if DEBUG then
            if opsidx<3 then ?9/0 end if
            if scode!=opXor then ?9/0 end if -- and/or always short-circuited
        end if
        --added 29/4/12 (spotted in passing)
        if newEBP then
            -- save eax if rqd
            saveFunctionResultVars(opsidx,INTSTOO)
        end if
        opsidxm1 = opsidx-1
        opsidxm2 = opsidx-2
        t1 = opstype[opsidxm1]
        t2 = opstype[opsidxm2]
        if not and_bits(t1,T_atom)          -- if p1 or p2 is sequence-only
        or not and_bits(t2,T_atom) then
            -- should have been replaced with sq_xor() in Expr():
            Abork("internal error (sq_xor)",opsidxm1)
        end if

        RHStype = T_integer

        if emitON then
            if popFactor then
                N = newTempVar(T_integer,Shared)
                NTyp = T_integer
            end if
            p2 = opstack[opsidxm2]
            p3 = opstack[opsidxm1]
--DEV extend with init flags?
if newEmit then
            agcheckop(opXor)
end if
            apnds5({opXor,N,p2,p3})
        end if  -- emitON
        freeTmp(-3)

    elsif opTopIsOp=MkSqOp then
        -- scode is always opMkSq, btw
--DEV (untried; we may not need this at all)
--      --added 29/4/12 (spotted in passing)
-- looks like it, added for real 8/6/2012:
        if newEBP then
            -- save eax if rqd
            saveFunctionResultVars(opsidx,INTSTOO)
        end if
        opsidxm1 = opsidx-1
        noofitems = opstack[opsidxm1]
        opsidx -= 2

        RHStype = T_Dsq
        if emitON then
            if popFactor then
                N = newTempVar(T_Dsq,Shared)
            end if
--DEV what, not opUnassigned? (spotted in passing)
--added 21/09/2013:
            for i=noofitems to 1 by -1 do
                p1 = opstack[opsidx-noofitems+i]
                if not symtab[p1][S_Init] then
                    k = symtab[p1][S_vtype]
                    k = rootType(k)
                    if k=T_integer then
                        -- (non-T_integers get caught properly by incref, see pilx86.e)
                        Unassigned(p1)
                    end if
                end if
            end for
--<21/09/2013 ends>
if newEmit then
            agcheckop(opMkSq)
--          tidx = aatidx[opMkSq]
--          if tt[tidx+EQ]=0 then
--              agcheck(tidx)
----                lblidx = get_lblidx(tidx)
--          end if
end if
            apnds5({opMkSq,noofitems,N})
            for i=noofitems to 1 by -1 do
                k = opstack[opsidx-noofitems+i]
                s5 = append(s5,k)
            end for
        end if  -- emitON
        freeTmp(-noofitems)

    else ?9/0 -- unrecognised opTopIsOp
    end if

    if popFactor then
        if emitON=0 then N = 0 end if
        STyp = RHStype
        if STyp>T_object then STyp = rootType(STyp) end if
        opsidx += 1
        opstack[opsidx] = N
        opsltrl[opsidx] = allowTempReuse
        opstype[opsidx] = STyp
    end if

    opTopIsOp = 0
end procedure

procedure PopFactor()   -- only called if opTopIsOp
    StoreVar(0,-1)
end procedure

constant Invert = 1,    NoInvert = 0

--integer lastparam -- saved in ParamList(), used in Branch(), for localtype info 
--                  -- from (eg) "if udt(lastparam) then". By the time Branch() 
--                  -- is invoked, only res:udt remains on opstack.
--
integer scBP
        scBP = 0

integer noofbranches
        noofbranches = 0

--with trace
function Branch(integer invert, integer emitElse,integer mergeSet, integer backpatch)
--
-- Emit a conditional jump statement.
--
--  eg "if a!=b then z", where z is not exit/end if, then DoIf calls
--      this routine with {a,b,"ne"} on the stack, invert=true (Invert),
--      so emit an "opJeq,a,b,backpatch". The <backpatch> will be
--      replaced when we actually hit the "end if" (or whatever).
--  Alternatively, in "while x or y do", at the point we hit the "or",
--   Expr() calls this routine with x (alone) on the stack and invert
--   of false (NoInvert), so emit an "opJif,x,<backpatch>", and again
--   the <backpatch> will be replaced when we hit the "do".
--
--  Note that "while x or y do" should emit:
--      opJif <do> x
--      opJnot <end while> y
--  which should help clarify what "invert" is needed for, otherwise
--  this routine would not know whether Jif or Jnot was rqd.
--  (this routine picks up the x/y from opstack)
--
--  backpatch is one of exprBP, -- (loosely speaking) innermost fail
--                      scBP, -- (loosely speaking) innermost success
--                      exitBP, -- to containing end for/while (or continueBP)
--                      breakBP, -- to containing end select
--                      ifBP, -- (loosely speaking) if-level fail (if --> elsif --> else)
--                      (EndIfBP is unconditional/not passed to this routine)
--
--    -- naturally there are catches galore in this area.
--
--   Note that there are no conditional backward jumps. Equally there are no forward 
--    jumps in a one-pass compiler where we know where to land. Hence the targets are
--    /always/ put on a backpatch list, processed at the appropriate "end if" etc.
--
-- emitElse is a (weak) optimisation from DoIf. In a if/elsif/elsif chain, suppose one 
--  of them is a dead cert (eg constant f=2 if f=1 then.. elsif f=2.. elsif f=3..)
--  a) we may or may not have omitted the f=1 case(s), but when we find f=2 there is
--     no point further bothering with f=3 etc (we must parse it but emit no code).
--  b) but also there is no point actually emitting the f=2 test, something we have 
--     just found always true. All use of emitElse in here concerns b) only.
--      Update: emitElse of 2 is used in makeBool() to distinguish that call from
--              the other (emitElse=1) call with mergeSet of scMerge in Expr, for
--              differences in required error messages only.
--
--  This latter point has most merit in "constant DEBUG=0 ... if DEBUG then" style code,
--   which allows you to leave debug code available for possible future use yet without 
--   making the exe any bigger or slower than it would be if you deleted it all.
--   To be completely honest, this is much more of a "feels good to know" thing rather 
--   than something that actually makes a big or even noticeable difference in practice.
--    (nb "if debug then if x then" works much better than "if debug and x then"...)
--
integer p2,p3
integer opcode
integer jcode
sequence sytmp
integer ltype, ptype
integer tmask,p2typ,p3typ
integer tii
integer isInit
object symtabN
integer flippable

    noofbranches += 1
    if opTopIsOp then
        opcode = opstack[opsidx]
--      if collectstats and not bind then
--          -- Example use of opStat. Warning, broken: pilxl needs this code under opStat,
--          --  pgscan needs to skip it, and below should be say s5&={opStat,opcode} ...
--          --  oh, and pilxl/pgscan may be pilx86.e by the time you read this.
--          -- I used this to count (executed) opcodes immediately before a jump, and used
--          --  that info to decide that eg opSq should be inlined if possible. (BTW, opFind
--          --  is not a suitable candidate for inlining and is handled pretty well anyway)
--          -- You might prefer to move this to the end of the "if opTopIsOp then" block,
--          --  so that it only shows opcodes which have not been handled.
--          -- Note that should your test program(s) execute any one opcode more than
--          --  1,073,741,823 times then the count would just show "maxed out".
--          emitHex5w(mov_eax_imm32,opcode)             -- mov eax,opcode
--          emitHex5opX(opStat)                         -- call opStat
--          -- ecx,edx damaged, plus the eax above, all other regs preserved
--          clearReg(eax)   -- (better yet, do a pushad/popad pair)
--          clearReg(ecx)
--      end if
-- 9/3/09:
        if opTopIsOp=BltinOp and opcode=opScmp then
            -- map "if compare(a,b) then" to "if a!=b then":
            opTopIsOp=BranchOp   opcode=opJne
        end if
        if opTopIsOp=BranchOp then
--?opsidx
--          if opsidx!=3 then ?9/0 end if
            if emitElse then
                opcode = cmpcheck(opcode)
                if emitON then
                    if invert then
                        opcode = tnot[opcode]
                    end if
                    p2 = opstack[opsidxm2]
                    p3 = opstack[opsidxm1]
                    tii = twoInitInts(eJmp[opcode])
--DEV: (untried)
--                  if eJmp[opcode]=EITHER then
--                      bothInit = symtab[p2][S_Init] or symtab[p3][S_Init]
--                  else -- BOTH
--                      bothInit = symtab[p2][S_Init] and symtab[p3][S_Init]
--                  end if
                    opcode = Bcde[opcode]
if newEmit then
                    agcheckop(opcode)   -- opJcc
end if
                    apnds5({opcode,mergeSet,0,backpatch,p2,p3,tii,bothInit})
                    backpatch = length(s5)-4
                    if NOLT=0 or bind or lint then
                        if t1!=t2   -- (as set by cmpcheck)
                        and opcode=opJne then
                        -- In the body of an if/while p2=p3, p2/p3 is an int if p3/p2 is.
                        -- Note we cannot make any type assumption from eg while x!=0 (opJeq),
                        -- since 1, 0.1, "x", and {} are all not equal to 0. Likewise for the
                        -- other conditional tests, hence this is emphatically opJne only, 
                        -- and absolutely does not apply to Jeq/lt/gt/le/ge.

                        -- [DEV] perhaps we could extend this (still opJne only!) to say
                        -- The same is true for eg if name="fred" then: we know that name is 
                        --  definitely string-like, although NB {'f','r','e','d'} == "fred"),
                        --  or if Q is type 0b1001 and P is T_atom(0b0011), "if P=Q then" 
                        --  ===> both must be integer!!.

-- added 31/7/09:       -- Actually, we could also part handle <,<=,>=,>: if something is
                            -- <= an atom, it must also be an atom, conversely if something
                            -- is >= a sequence, it must also be a sequence. Note this is not
                            -- flippable (doh), and the current infrastructure would not cope
                            -- well with attempts to implement:
                            --  if x>5 then
                            --      -- <no type info added for x here>
                            --  else
                            --      -- <here we know x must be an atom>
                            --  end if
                            p2typ = symtab[p2][S_ltype]
                            p3typ = symtab[p3][S_ltype]
--DEV what about udts? Maybe (idea taken from plausible):
--  while 1 do
--      while p2typ>T_object and p2typ>p3typ do
--          p2typ = symtab[p2typ][S_sig][2]
--      end while
--      while p3typ>T_object and p3typ>p2typ do
--          p3typ = symtab[p3typ][S_sig][2]
--      end while
--      if p2typ=p3typ then exit end if
--      if p2typ<=T_object and p3typ<=T_object then exit end if
--  end while
--or did I just mean:
--      while p2typ>T_object do
--          p2typ = symtab[p2typ][S_sig][2]
--      end while
--      while p3typ>T_object do
--          p3typ = symtab[p3typ][S_sig][2]
--      end while
--aka
--      p2typ = rootType(symtab[p2][S_ltype])
--      p3typ = rootType(symtab[p3][S_ltype])
                            if p2typ!=p3typ
                            and p2typ<=T_object
                            and p3typ<=T_object then
--DEV why not opstype? (untried:)
--                      p2typ = opstype[opsidxm2]
--                      p3typ = opstype[opsidxm1]
                                tmask = and_bits(p2typ,p3typ)
                                if and_bits(p2typ,T_sequence)       -- Since {'a'}=="a", convert
                                and and_bits(p3typ,T_sequence) then -- any combo of 01xx/10xx/11xx
                                    tmask = or_bits(tmask,T_sequence)   -- ==> 11xx
                                end if
                                ltype = and_bits(p2typ,tmask)
--                          if p2typ!=ltype then
                                if ltype and p2typ!=ltype then
                                    noofbranches = 2    -- prevent flip
                                    ltAdd(TEST,p2,p2typ,ltype,backpatch-3)
                                end if
                                ltype = and_bits(p3typ,tmask)
--                          if p3typ!=ltype then
                                if ltype and p3typ!=ltype then
                                    noofbranches = 2    -- prevent flip
                                    ltAdd(TEST,p3,p3typ,ltype,backpatch-3)
                                end if
                            end if
                        end if
                    end if -- NOLT
                end if  -- emitON
            end if
            freeTmp(-3)
            opTopIsOp = 0
            return backpatch
        end if  -- BranchOp
--      if opTopIsOp=NotSubscriptOp or opTopIsOp=NotBltinOp then
        if opTopIsOp>=NotBltinOp then
            opTopIsOp -= 10
            invert = not invert
        end if
        opsidxm1 = opsidx-1
        if opTopIsOp=SubscriptOp and opstack[opsidxm1]=1 then
            opsidx = opsidxm1
            if emitElse then
                if emitON then
                    opsidxm1 = opsidx-1
                    opsidxm2 = opsidx-2
                    p2 = opstack[opsidxm2]
                    p3 = opstack[opsidxm1]
if newEmit then
-- erm, we have var nos...
--                  if not symtab[p2][S_Init] then
--                      Unassigned(p2)
--                  end if
--                  if not symtab[p3][S_Init] then
--                      Unassigned(p3)
--                  end if
                    agcheckop(opJnotx)
--  puts(1,"opJnotx line 2362 pmain.e\n")
end if
                    apnds5({opJnotx,mergeSet,0,backpatch,p2,p3,invert})
                    backpatch = length(s5)-3
                end if -- emitON
            end if
            freeTmp(-3)
            opTopIsOp = 0
            return backpatch
        end if  -- SubscriptOp

        if opTopIsOp=BltinOp then
            t2 = find(opcode,opINSP)
            if t2 then  -- if [not] integer/atom/string/sequence(x) then
--              opsidxm1 = opsidx-1     -- already done above
                p1 = opstack[opsidxm1]
                if emitON then
                    t1 = symtab[p1][S_Init]
                else
                    t1 = 1
                end if

                ltype = typeINSPO[t2]
                opsidx -= 1
                opTopIsOp = 0
                if emitON then
                    flippable = (noofbranches=1)
if newEmit then
--                  if not symtab[p1][S_Init] then
                    if not t1 then
                        Unassigned(p1)
                    end if
--                  agcheckop(opJtyp) -- need to ensure opInt0 etc are available...
                    agcheckop(opInt0) -- need to ensure opInt0 etc are available...
--  puts(1,"opJtyp line 2314 pmain.e\n") [will get a message from pilx86 when needed]
end if
                    apnds5({opJtyp,mergeSet,0,backpatch,0,flippable,t1,p1,invert,ltype})
                    backpatch = length(s5)-6
                    if NOLT=0 or bind or lint then
                        --                  if p1 and not and_bits(symtab[p1][S_State],K_Fres) then
                        --31/7/09 removed "p1 and" (on a whim)
                        if not and_bits(symtab[p1][S_State],K_Fres) then
                            if not invert then
                                ltype = xor_bits(ltype,T_object)
                            end if
                            ptype = symtab[p1][S_ltype]
                            if ptype!=ltype then
                                ltAdd(TEST+flippable,p1,ptype,ltype,backpatch-3)
                            end if
                        end if  -- K_Fres
                    end if -- NOLT
                end if -- emitON
                freeTmp(-1)
                return backpatch
            elsif find(opcode,opXxxBits) then
                if twoInitInts(BOTH) then   -- else let popfactor deal with it.
                    opsidx -= 1
                    opsidxm1 = opsidx-1
                    opTopIsOp = 0
                    if emitON then
                        p3 = opstack[opsidx]
                        p2 = opstack[opsidxm1]
--if newEmit then
--                  agcheckop(opJbits)
--?9/0
--puts(1,"opJbits no agcheckop line 2376 pmain.e\n")    -- it's a virtual op!
--end if
                        apnds5({opJbits,mergeSet,0,backpatch,opcode,p2,p3,invert})
                        -- (tii=1, btw, in ALL cases for opJbits)
                        backpatch = length(s5)-4
                    end if -- emitON
                    freeTmp(-2)
                    return backpatch
                end if          
            elsif opcode=opLen then
                opTopIsOp = 0
                if emitON and emitElse then -- umm, added 25/9/08...
                    p1 = opstack[opsidxm1]
                    -- optimise if p1 is known to be init sequence:
                    sytmp = symtab[p1]
                    isInit = sytmp[S_Init]
--DEV opLen0
                    if isInit and not and_bits(opstype[opsidxm1],T_atom) then
----                if sytmp[S_Init] then   -- DEV may be better to rely on gtype??
----                if sytmp[S_Init] then   -- DEV may be better to pass ltype??
                        p2 = 0
                    else
                        p2 = newTempVar(T_integer,Shared)
                    end if
if newEmit then
                    agcheckop(opLen) -- (yes, opLen not opJlen)
end if
                    apnds5({opJlen,mergeSet,0,backpatch,p1,p2,isInit,invert})
                    backpatch = length(s5)-4
                end if -- emitON and emitElse
                freeTmp(-2)
                return backpatch

            end if
            PopFactor()
        elsif opcode=opNot then
            -- If there is a unary not atop the stack, just (re-)invert the jump.
            opsidx -= 1
            opTopIsOp = 0
            invert = not invert
        else
            PopFactor()
        end if
    end if
    t1 = opstype[opsidx]
--  if not and_bits(t1,T_atom) and scBP=0 then
    if not and_bits(t1,T_atom) then
        -- Aside: there is little point trying to call sq_and/or here, since
        --        a) would only work on "{} and/or 1", not "1 and/or {}",
        --        b) thus far we only have the left hand operand.
        if mergeSet=exprMerge then                                  -- from Expr()/T_and
            Aborp("type error (sq_and() needed?)")
        elsif mergeSet=scMerge then
            if emitElse=1 then                                      -- from Expr()/T_or
                Aborp("type error (sq_or() needed?)")
            else                                                    -- from makeBool()
                Abork("type error (sq_and/or() needed?)",opsidx)
            end if
        elsif scBP=0 then
            Abork("type error (true/false condition must be ATOM)",opsidx)
        end if
    end if
    if emitElse then
        if emitON then
            p1 = opstack[opsidx]

            jcode = opJif
            if invert then
                jcode = opJnot
            end if
            isInit = symtab[p1][S_Init]
            if not isInit then
                -- non-init S_Const are treated as init vars (when testing them)
                if symtab[p1][S_NTyp]!=S_Const then
                    if oktoinit then
                        symtabN = 0 -- kill refcount
                        symtab[p1][S_Init] = Ichain
                        Ichain = p1
                    end if
                end if
            end if

            if invert   -- cannot make any useful assumption from "if not udt()"
            and and_bits(symtab[p1][S_State],K_Fres)    -- is a function result
            and symtab[p1+1][S_sig][1]='T'              -- is a type routine
            and not and_bits(symtab[lastparam][S_State],K_Fres) then
                -- ^ lastly, but not eg if sequence(f()) then: do not mark
                -- res:f a sequence, not least because we could not reference
                -- it again without re-invoking the function and therefore
                -- changing the value anyway.

                -- so, p1 is a res:udt, create some localtype info.
                -- (leaving lastparam non-0 triggers more code below,
                --  and also in opJif/opJnot processing in pilx86.e)

            else
                lastparam = 0
            end if  -- invert/Fres/T

--          flippable = 0 (do not try and log "if not udt()" info)

if newEmit then
--DEV opUnassigned?
            agcheckop(jcode)
end if
            apnds5({jcode,mergeSet,0,backpatch,p1,isInit,lastparam})
            backpatch = length(s5)-3

            if NOLT=0 or bind or lint then
                if lastparam then
                    ltAdd(TEST,lastparam,symtab[lastparam][S_ltype],p1+1,backpatch-3)
                end if
            end if -- NOLT

        end if -- emitON
    end if -- emitElse
    freeTmp(-1)
    return backpatch
end function

procedure opslack()
    opstack &= repeat(0,4)
    opstype &= repeat(0,4)
    opsltrl &= repeat(0,4)
    opsline &= repeat(0,4)
    opstcol &= repeat(0,4)
end procedure

--without trace
procedure PushOp(integer opcode, integer optype)
--
-- Push an opcode (eg opAdd) onto the stack.
-- optype is UnaryOp/BranchOp/MathOp/SubscriptOp/SliceOp etc
-- "1+2*3" -> {1,2,3,opMul} -> {1,tmp,opAdd}.
--
    if opTopIsOp then PopFactor() end if
if newEBP then
        -- save eax if rqd
-- (tried 27/08/2020, made no difference to what I was working on)
        saveFunctionResultVars(opsidx,NOTINTS)
--      saveFunctionResultVars(opsidx,INTSTOO)
end if

    opsidx += 1
    if opsidx>length(opstack) then opslack() end if
    opstack[opsidx] = opcode
    opstype[opsidx] = -1
    opsltrl[opsidx] = 0
    opsline[opsidx] = tokline
    opstcol[opsidx] = tokcol
    opTopIsOp = optype
--validate_opstack()
end procedure

procedure PushSubOp(integer opcode, integer optype, integer noofsubscripts)
--
-- Push a variable length opcode onto the stack.
--  opcode is one of opSubss,opSubse,opMkSq
--  optype is one of SliceOp,SubscriptOp,MkSqOp
--  (noofsubscripts is really len in the opMkSq case)
--
    if opTopIsOp then PopFactor() end if
if newEBP then
        -- save eax if rqd
        saveFunctionResultVars(opsidx,NOTINTS)
end if
    opsidx += 1
    if opsidx>=length(opstack) then opslack() end if
    opstack[opsidx] = noofsubscripts
--added 1/4/2012:
    opstype[opsidx] = -1
    opsidx += 1
    opstack[opsidx] = opcode
    opstype[opsidx] = -1
    opsltrl[opsidx] = 0
    opsline[opsidx] = tokline
    opstcol[opsidx] = tokcol
    opTopIsOp = optype
--validate_opstack()
end procedure

procedure PushFactor(integer n, integer isLiteral, integer etype)
--
-- Keep partial results, parameters etc on a stack until needed.
-- NB etype is rootType()'d by callee if rqd/a udt, and should be 
--    -1 for literals such as counts and var nos (ie/eg such as
--    the 2 (count of indexes) needed for say s[5][6] as opposed 
--    the 1 and "hello" literals needed for puts(1,"hello").
--
    if opTopIsOp then PopFactor() end if
--if newEBP then
--      -- save eax if rqd
--      saveFunctionResultVars(opsidx)
--end if
    opsidx += 1
    if opsidx>length(opstack) then opslack() end if
--14/8/15: (NO!!)
--if n=lhsvar then
--  lhsvar = 0
--end if
if n=lhsvar and isLiteral=0 and lhspos!=0 then
    lhsvar = 0
end if
    opstack[opsidx] = n
    opsltrl[opsidx] = isLiteral
    opstype[opsidx] = etype
    opsline[opsidx] = tokline
    opstcol[opsidx] = tokcol
--validate_opstack()
end procedure

forward procedure Expr(integer p, toBool)
forward procedure GetFactor(integer toBool)

-- second parameter for Expr (or 0/false if we want sc/exprBP chains):
constant asBool=1,          -- return 0/1
         asNegatedBool=-1   -- return 0/-1
--       asInvertedBool=-9  -- return 1/0       [DEV unused/broken?]

--/*
constant ZZops = "*!/+-&<>=!|&<>",
         ZZpre = "9!988655443377",
--*/
constant ZZops = "*/+-&|&<>=!<>",
         ZZpre = "9988655443377",
         -- precedence levels:
--       pSubsc   = 11, -- []. (unused/handled directly in GetFactor/DoSubScripts/Assignment)
         pUnary   = 10, -- unary +,-,not,~ (handled directly in GetFactor)
         pMuldiv   = 9, -- * /
         pAddsub   = 8, -- + -
         pBitshift = 7, -- >> <<
         pConcat   = 6, -- &
--       pBitops   = 5, -- && ||
         pRelops   = 4, -- < > <= >=
         pEqorne   = 3, -- == !=
--/*
         pRelops   = 5, -- < > <= >=
         pEqorne   = 4, -- == !=
         pBitops   = 3, -- && ||
--*/
         pLogop    = 2, -- and or xor
         pAllops   = 0, -- [full, effectively the same as pLogop]
--       ZZopcodes = {opMul,opDiv,opAdd,opSub,opConcat,opDivf,0,0,0,opOrBits,opAndBits,opPow,opPow},
         ZZopcodes = {opMul,opDiv,opAdd,opSub,opConcat,opOrBits,opAndBits,opDivf,0,0,0,opPow,opPow},
                                        --  (kludge, see k = 8 below (opDivf ^)
--       ZZlong = {"mul","div","add","sub","","floor_div"},
         ZZlong = {"mul","div","add","sub","","","","floor_div"},
--(nb different order to Bcde etc:)
         ZZjcc = {"ge","lt","eq","ne","gt","le"},
         Z_andorxor = {"and","or","xor"}
string ZZdesc

constant T_andorxor = {T_and,T_or,T_xor}


global -- for pilasm.e
integer SideEffects
        SideEffects = E_none

integer asConst = -1    -- else isGlobal

--27/10/19:
--procedure DoSequence()
procedure DoSequence(integer och='{', len=0, bAllConst=false)
-- Process a sequence
integer N, t
object v
integer allconst, etype
sequence constseq
sequence symtabN    -- copy of symtab[opstack[opsidx]]
integer wasMapEndToMinusOne = mapEndToMinusOne
--sequence isRID --DEV temp
integer VAmask = 0
sequence vaset = {}
integer wasSideEffects = SideEffects
integer ech = iff(och='{'?'}':iff(och='('?')':9/0))
integer nestedConst = 0, wastokcol
    SideEffects = 0
    mapEndToMinusOne = -2
--  MatchChar('{',float_valid:=true)
    MatchChar(och,float_valid:=true)
--  allconst = 1
--p2js:
--  allconst = (och='{' and len=0)
--  allconst = false
    allconst = bAllConst
    etype = 0
    constseq = {}
--isRID = {}
--  while toktype!='}' do
    while toktype!=ech do
        if mapEndToMinusOne='$' and toktype=DIGIT and TokN=-1 then  
            mapEndToMinusOne = 0
            getToken()
            exit
        end if
        mapEndToMinusOne = 0
--20/6/2020:
        if asConst!=-1 then
            skipSpacesAndComments()
            if Ch=':' and ChNext('=') then
                N = InTable(-InAny)
                if N>0 then
--21/01/2021
--                  if symtab[N][S_NTyp]=S_Rsvd then
                    if N<=T_Asm or symtab[N][S_NTyp]=S_Rsvd then
                        Aborp("illegal use of a reserved word")
                    elsif InTable(InTop) then
                        Duplicate()
                    end if
                end if
                nestedConst := ttidx
                wastokcol = tokcol
                getToken()
                MatchChar(':',false)
                MatchChar('=',float_valid:=true)
            end if
        end if
        Expr(pAllops, asBool)
        if nestedConst then
--/!*
--1/11/2020: (nested constants not properly marked as such, eg SPREAD in p2js_basics.e)
if not opTopIsOp 
--and opsidx=1
and opsltrl[opsidx]=1 then
            -- (I just mimiced the one in DoConstant())
--          N = addSymEntryAt(wasttidx,isGlobal,S_Const,Ntype,0,K_litnoclr,wastokcol)
            integer O = opstack[opsidx]
            sequence symtabO = symtab[O]
            if and_bits(symtabO[S_State],K_Fres) then ?9/0 end if
            N = addSymEntryAt(nestedConst,asConst,S_Const,T_object,0,K_litnoclr,wastokcol)
            symtab[N][S_value] = symtabO[S_value]
            symtab[N][S_Init]  = symtabO[S_Init]
            symtab[N][S_Clink] = symtabO[S_Clink]
            symtab[O][S_Clink] = N
            RHStype = opstype[opsidx]
--          symtab[N][S_vtype] = RHStype (done below anyway)
            symtab[N][S_ltype] = RHStype
            symtab[N][S_ErrV] = wastokcol
            opsidx -= 1
else
--*!/
            N = addSymEntryAt(nestedConst,asConst,S_Const,T_object,0,S_used,wastokcol)
            if opsidx=1 then
                integer k = opstack[1]
                if and_bits(symtab[k][S_State],K_Fres)
                and not find(symtab[k+1][S_Efct],{E_none,E_other}) then
                    symtab[N][S_State] = or_bits(symtab[N][S_State],S_used)
                end if
            end if
            storeConst = 1
            onDeclaration = 1
            StoreVar(N,T_object)
            storeConst = 0
            onDeclaration = 0
end if
            symtab[N][S_vtype] = RHStype
            PushFactor(N,false,RHStype)
            nestedConst = 0
        end if
        integer tidx = opstack[opsidx]
        if not opTopIsOp
        and tidx!=0
        and symtab[tidx][S_NTyp]=S_GVar2 then
            VAmask = or_bits(VAmask,power(2,remainder(tidx,29)))
            vaset &= tidx
        end if
        if allconst then
            if opTopIsOp or opsltrl[opsidx]!=1 then
                allconst = 0
            elsif emitON then
                symtabN = symtab[tidx]
--isRID = append(isRID,and_bits(symtabN[S_State],K_rtn))
--8/6/15:
if and_bits(symtabN[S_State],K_rtn) then
    allconst = 0
else
                t = opstype[opsidx]
                etype = or_bits(etype,t)
--DEV/SUG if not compiling (with matching changes to pemit2.e)
--                  v = symtabN[S_value]
                if t=T_string then
--                  v = {-symtabN[S_Name]}  -- aka {-ttidx}
                    v = {-symtabN[S_Init]}  -- aka {-ttidx}
                elsif and_bits(t,T_sequence) then   -- T_sequence or T_Dsq
--                  v = {symtabN[S_Name]}   -- aka {ttidx}
                    v = {symtabN[S_Init]}   -- aka {ttidx}
                else
--DEV this might need to change for floats under newEmit...
--if newEmit then -- 9/10/14... nope...
--                  v = {symtabN[S_Init]}   -- aka {ttidx}
--else
                    v = symtabN[S_value]
if newEmit then
--3/1/16:
--  if not integer(v) then
    if isFLOAT(v) then
        v = symtabN[S_Init]+0.5
    end if
end if
--end if
                end if
                constseq = append(constseq,v)
end if
            end if
        end if
        len += 1
        if opTopIsOp then PopFactor() end if
        if toktype!=',' then exit end if
        mapEndToMinusOne = -2
        MatchChar(',',float_valid:=true)
    end while
    mapEndToMinusOne = wasMapEndToMinusOne

    if allconst then
--if newEmit and len=1 and tokline=12 then
--if len=2 and tokline=12 then
--  printf(1,"allconst, len=%d, tokline=%d\n",{len,tokline})
--?isRID
--end if
        freeTmp(-len)
        N = addUnnamedConstant(constseq, T_Dsq)
        if N then -- (N=0 can happen if emitON=0)
            if length(symtab[N])<S_gInfo then -- if not a duplicate
                symtab[N] = append(symtab[N],{T_Dsq,0,0,etype,len})
            end if
        end if
        PushFactor(N,true,T_Dsq)    -- yep, this is a literal!
    else
        PushSubOp(opMkSq,MkSqOp,len)
--      if and_bits(VAmask,wasSideEffects)!=and_bits(VAmask,SideEffects) then
--if fileno=1 then
--  printf(1,"VAmask:%08x, SideEffects:%08x\n",{VAmask,SideEffects})
--end if
--      if and_bits(VAmask,SideEffects)
--      and SideEffects!=E_all then
--removed 16/10/18 (over a ba_sprint call.../just too difficult to fix right now)
--/*
        if and_bits(VAmask,SideEffects) then
            if lint 
            or SideEffects!=E_all then
--if fileno=1 then
--  printf(1,"VAmask:%08x, SideEffects:%08x\n",{VAmask,SideEffects})
--  ?vaset
--  ?symtab[vaset[1]]
--end if
                Warn("suspect evaluation order",tokline,tokcol,0)
            end if
        end if
--*/
    end if
    SideEffects = or_bits(SideEffects,wasSideEffects)
--  MatchChar('}')
    MatchChar(ech)
end procedure

--with trace
procedure DoHexStr()
--
-- Convert a DQUOTE-like HEXSTR token into a sequence.
-- TokStr should only contain {0-9,A-F,space} by now, and
-- TokN should be 2,4,8 for x,u,U respectively.
--
sequence constseq
integer idx, ch
atom v
integer etype, ctype
integer N

--  constseq = {}
    constseq = ""
    idx = 1
    etype = T_integer
    while idx<=length(TokStr) do
        ch = TokStr[idx]
        if ch=' ' then
            idx += 1
        else
            v = 0
            for i=1 to TokN do
                if ch>='A' then
                    ch -= 'A'-10
                else
                    ch -= '0'
                end if
                v = v*#10 + ch
                idx += 1
                if idx>length(TokStr) then exit end if
                ch = TokStr[idx]
                if ch=' ' then exit end if
            end for
            constseq &= v
--?{v,constseq,string(constseq)}
--3/1/16:
--          if not integer(v) then
            if isFLOAT(v) then
                etype = T_atom
            end if
        end if
    end while
    if string(constseq) then
        ctype = T_string
    else
        ctype = T_Dsq
    end if
    N = addUnnamedConstant(constseq, ctype)
    if N then -- (N=0 can happen if emitON=0)
        if length(symtab[N])<S_gInfo then -- if not a duplicate
            symtab[N] = append(symtab[N],{ctype,0,0,etype,length(constseq)})
        end if
    end if
    PushFactor(N,true,ctype)    -- yep, this is a literal!
    getToken()
end procedure


--without trace
with trace
integer Q_Routine

--object Part2  -- now in psym.e
--procedure LiteralPart2()
---- called by InTable to grab second half of eg "fred:myroutine" when
---- trying to resolve routine_id("fred:myroutine") at compile-time.
--  tt_string(Part2,-2)
--  Part2 = 0
--end procedure
--rLiteralPart2 = routine_id("LiteralPart2")

function resolveRoutineId()
--
-- (factored out of ParamList for readability reasons only)
--DEV
-- (now also used by getOneDefault)
-- Process routine_id(<literal string>), which we have proved is the case, 
--  right up to and including the closing ")", before calling this routine.
-- If successfully resolved return 1 (true) and leave Q_Routine!=0, for Call().
--
integer nsi, k
string emsg
    nsi = find(':',TokStr)
    if nsi then
        nsPart2 = TokStr[nsi+1..length(TokStr)]&" "
        TokStr = TokStr[1..nsi-1]
        tt_string(TokStr,-2)
        Q_Routine = InTable(InNS)
        -- (btw: these msgs could equally be under "if listing or bind>1", as below)
        if sequence(nsPart2) then
            Warn("namespace not resolved",tokline,tokcol,0)
            if Q_Routine!=0 then ?9/0 end if    -- cannot possibly be!
--      elsif Q_Routine=0 then
        elsif Q_Routine<=T_object then
            Warn("routine name not resolved",tokline,tokcol,0)
        end if
    else
        tt_string(TokStr,-2)
        Q_Routine = InTable(InAny)
        -- For testing purposes only: syswait.e/doEvents, pmsgs.e/proemh, t40/X, 
        --  and t45/p2 all serve as counter-examples. Note that I am keen to keep
        --  p.exw itself unresolved routine_id free, to avoid linking in prtnid.e
        if listing or bind>1 or Q_Routine<0 then
            -- ie, show this message for "p -d xxx" or in rounds 2/3/4 of "p -cp",
            --      but not in the common everyday "p xxx" and/or "p -c xxx" cases.
--          if Q_Routine=0 then
--DEV should this not be T_Bin (ditto below)?
            if Q_Routine<=T_object then
--trace(1)
--tt_string(TokStr,-2)
--Q_Routine = InTable(InAny)
                Warn("routine name not resolved at compile-time",tokline,tokcol,0)
            end if
        end if
    end if
--4/10/2020:
if Q_Routine>0 and Q_Routine<=T_Asm then
    Q_Routine = get_hll_stub(Q_Routine)
end if
--  if Q_Routine!=0 then
    if Q_Routine>T_object then
        k = symtab[Q_Routine][S_NTyp]

        if k<S_Type then
            -- oops!
            if k=S_Nspc then
                emsg = "namespace)"
            elsif k=S_Rsvd then
                emsg = "reserved word)"
            elsif k=S_Const then
                emsg = "constant)"
            elsif k=S_GVar2 then
                k = symtab[Q_Routine][S_State]
                if and_bits(k,K_gbl) then
                    emsg = "global variable)"
                else
                    emsg = "file-level variable)"
                end if
            elsif k=S_TVar then
                emsg = "local variable)"
--DEV autoasm? (test routine_id("platform"), routine_id("append"), etc)
            else
                emsg = "???)"   -- BUG!
            end if
            Aborp("invalid routine name (this is a "&emsg)
        end if

--4/12/18:
 if Q_Routine>T_Asm then
--emitON? (or is that covered by Q_Routine!=0)
if 01 then
        Or_K_ridt(Q_Routine, S_used+K_ridt)
else
        k = symtab[Q_Routine][S_State]
        k = or_bits(k,S_used+K_ridt)
        symtab[Q_Routine][S_State] = k
end if
--removed PL 26/11/15:
--      getToken()
--      MatchChar(')')
        return 1
 end if
    end if
    Q_Routine = 0
    return 0
end function

--without trace

sequence paramNames = {0},  -- ttidx values
         paramTypes = {0},  -- eg/ie T_integer..T_object, or udts
         paramLines = {0},
         paramCols  = {0},
         paramDflts = {0},  -- 0=no default, else 
                            -- (-ve integer): -ve index to paramNames, eg in
                            --      f(object a,b=a), paramDflts[2]=-1, else
                            -- 2=divide by zero, else
                            -- (+ve integer): var/const-id, else
                            -- (sequence): {T_command_line}
                            --          or {T_length,N}
                            --                 where N can be +/- as above
--DEV (24/11/15)
                            --          or {T_routine_id,N}
                            -- NB/IE general expressions are NOT supported,
                            --  just the bare minimum to get by...
                            --  T_routine is only supported for the immediately
                            --  preceding parameter... a string [DEV see how we get on]
                            --
                            -- Also note this is different to the default value 
                            --  in opTchk il, which is (always integer):
                            --      0=no default, else
                            --      +ve=var/const-id, else
                            --      -T_command_line=command_line(), else
                            --      -ve=length(-varno).
                            -- (if other routines need to be supported, then
                            --  -ve would best become an index to a separate
                            --  "function call table", rather than mess up s5.)
         paramUsed = {0}    -- in rtn(x, y=x) etc, ensure we mark x with S_used.

-- verify the compiler is setting these as "sequence of integer":
--/**/  #isginfo{paramNames,0b0100,MIN,MAX,integer,-2}
--/**/  #isginfo{paramTypes,0b0100,MIN,MAX,integer,-2}
--/**/  #isginfo{paramLines,0b0100,MIN,MAX,integer,-2}
--/**/  #isginfo{paramCols,0b0100,MIN,MAX,integer,-2}
--/**/  #isginfo{paramDflts,0b0100,MIN,MAX,object,-2}
--/**/  #isginfo{paramUsed,0b0100,MIN,MAX,integer,-2}

integer nParams
        nParams = -1

--with trace
procedure getOneDefault()
--
-- process parameter defaults
--
--  Sets paramDflts[nParams]
--
--  As noted above, this intentionally only supports limited expressions,
--  mainly because parameters have not been formally added to symtab yet,
--  and to avoid shuffling blocks of code about (eg if we had a routine
--  f(sequence s, integer l=length(s)), then full Expr() handling after
--  the '=' would baulk at the (second) s, and if we got round that, it 
--  would emit the opLen(s) before the opTchk(l), not at all what we want.
--  Better to add things as needed in real-world situtations, eg length()
--  and command_line() are supported as special cases.)
--DEV: I think I missed a trick here: increaseScope or similar on the '='
--      and create a dummy routine/s5. Stash that somewhere and copy it
--      back inside the opTchk stuff. Would require that we increaseScope
--      at the opening '(' of the routine declaration.
--
-- TIP:
--  routine xxx(...,atom N=0,...)
--      if N=0 then N=allocate(24) end if
--  is usually just as effective, providing you have a suitable "illegal"
--  value to trigger the defaulting.
-- SUG/DEV:
--  You can also use the following trick:
--  routine xxx(...,object O=<undefined>,...)
--      if not object(O) then
--          O = ???
--      end if
integer k, N
object dsig
--DEV code needs to be added to toplevel, not locally.
--                  if not allowDefaulting then
--                      Aborp("defaulting not allowed")
--                  end if
--  if isparam then
--                      signature[length(signature)] += 32  -- lower case=optional
----                    ?9/0 -- needs to do an exchange with the return address, or something. [DONE]
--                      -- also, ban this on "forward" declarations.
--                      defaulted = 1
--      MatchChar('=')
-- oktoinit = 1
--NO!!
--                      Expr(pAllops,asBool)
--                      k = 0
--trace(1)
integer state, isForward, wasUsed
object Default

    if toktype='+' then
        MatchChar('+',float_valid:=true)
    end if
    if toktype=LETTER then
        k = find(ttidx,paramNames)
        if k and k<nParams then
            -- eg (object a,b=a)
--          paramDflts[nParams] = -k
            paramUsed[k] = 1
            Default = -k
            getToken()
        else
            N = InTable(InAny)
            if N<=0 then Aborp("unrecognised") end if
            dsig = symtab[N][S_sig]
--21/01/2021
            if symtab[N][S_NTyp]=S_Rsvd then
--          if N<=T_Asm or symtab[N][S_NTyp]=S_Rsvd then
                Aborp("illegal use of reserved word")
            elsif sequence(dsig) then
                if not find(dsig[1],"FT") then
                    Aborp("a procedure cannot be used to default a parameter")
                end if
                -- I'm only supporting length and command_line,
                -- at least for now anyways, for compatibility 
                -- with RDS Eu's std\ files.
                if N=T_length then
                    getToken()
                    MatchChar('(',float_valid:=true)
                    if toktype=LETTER then
                        k = find(ttidx,paramNames)
                        if k and k<nParams then
                            -- eg (sequence a,b=length(a))
--                          paramDflts[nParams] = {T_length,-k}
                            paramUsed[k] = 1
                            Default = {T_length,-k}
                        else
                            N = InTable(InAny)
                            if N<=0 then Aborp("unrecognised") end if
                            dsig = symtab[N][S_sig]
                            if sequence(dsig) then
                                Aborp("unsupported")
--21/01/2021
--                          elsif symtab[N][S_NTyp]=S_Rsvd then
                            elsif N<=T_Asm or symtab[N][S_NTyp]=S_Rsvd then
                                Aborp("illegal use of a reserved word")
                            end if
--                          paramDflts[nParams] = {T_length,N}
                            Default = {T_length,N}
                        end if
                    else
                        Aborp("unsupported")
                    end if
                    getToken()
                    MatchChar(')')
                elsif N=T_command_line then
                    state = symtab[T_command_line][S_State]
                    wasUsed = and_bits(state,S_used)
--                  if not wasUsed then
                    if emitON and not wasUsed then
                        symtab[T_command_line][S_State] = state+S_used+K_used
                        isForward = and_bits(state,S_fwd)
                        if isForward then
                            if not builtinsReferenced then
                                if AutoIncWarn then
                                    if lint then
                                        Warn("command_line() auto-included",tokline,tokcol,0)
                                    end if
                                end if
                                builtinsReferenced = 1
                                --
                                -- We normally link up routines via opFrame/opCall, but if 
                                --  command_line() might only ever get invoked to set up a 
                                --  parameter default then link up at the start of pemit.e
                                --
                                Z_command_line = 1
                            end if
                        end if
                    end if
                    getToken()
                    MatchChar('(')
--                  paramDflts[nParams] = {T_command_line}
                    Default = {T_command_line}
--                  MatchChar(')')
                    MatchChar(')')
--DEV
--!/*
                elsif N=T_routine then
                    getToken()
                    MatchChar('(',float_valid:=true)
                    if toktype=LETTER then
                        k = find(ttidx,paramNames)
                        if k!=0 and k<nParams then
                            -- eg (sequence a, integer b=routine_id(a))
                            if k!=nParams-1 
--                          if (k!=nParams-1 and k!=nParams-2)
                            or paramDflts[k]!=0 then
                                Aborp("unsupported")
                            end if
                            paramUsed[k] = 1
                            -- (must be resolved at compile-time)
--                          Default = {T_routine,-k?}
                            Default = {T_routine}
--                      else
--                          N = InTable(InAny)
--                          if N<=0 then Aborp("unrecognised") end if
--                          dsig = symtab[N][S_sig]
--                          if sequence(dsig) then
--                              Aborp("unsupported")
--21/01/2021
----                        elsif symtab[N][S_NTyp]=S_Rsvd then
--                          elsif N<=T_Asm or symtab[N][S_NTyp]=S_Rsvd then
--                              Aborp("illegal use of a reserved word")
--                          end if
----or resolve now..
--                          Default = {T_routine,N}
                        end if
--                  elsif toktype=DQUOTE then
--                      Q_Routine = 0
--                      if Ch!=')' then skipSpacesAndComments() end if
--                      if Ch!=')'      -- not eg "do"&opname[i]
--                      or resolveRoutineId()=0 then
--                          Aborp("unsupported")
--                      end if
--DEV I think this would cause problems/requires addRoutineId(Q_Routine)
--  - plus we're expecting {T_routine}...
--                      Default = Q_Routine --{T_routine,N}
--                      Default = addRoutineId(Q_Routine)?
--                      Default = {T_routine,addRoutineId(Q_Routine)}?
--                      Q_Routine = 0
                    else
                        Aborp("unsupported")
                    end if
                    getToken()
                    MatchChar(')')
--!*/
                elsif N=T_platform then
                    if PE then
                        Default = T_win32
                    else
                        Default = T_linux
                    end if
                    getToken()
                    MatchChar('(')
                    MatchChar(')')
                elsif N=T_machine_bits then
                    if X64 then
                        Default = addUnnamedConstant(64, T_integer)
                    else
                        Default = addUnnamedConstant(32, T_integer)
                    end if
                    getToken()
                    MatchChar('(')
                    MatchChar(')')
                elsif N=T_machine_word then
                    if X64 then
                        Default = addUnnamedConstant(8, T_integer)
                    else
                        Default = addUnnamedConstant(4, T_integer)
                    end if
                    getToken()
                    MatchChar('(')
                    MatchChar(')')
                else
                    Aborp("unsupported")
                end if
            else
--              paramDflts[nParams] = N
                Default = N
                getToken(float_valid:=true)
            end if
        end if
    elsif toktype=DIGIT then
        if Ch='/' then
            getToken()
            MatchChar('/',float_valid:=true)
            if toktype!=DIGIT or TokN!=0 then
                Aborp("/0 expected")
            end if
--          paramDflts[nParams] = 2
            Default = 2
        else
--          paramDflts[nParams] = addUnnamedConstant(TokN, T_integer)
            Default = addUnnamedConstant(TokN, T_integer)
        end if
        getToken()
    elsif toktype='-' then
        MatchChar('-',float_valid:=true)
        if toktype=DIGIT then
--          paramDflts[nParams] = addUnnamedConstant(-TokN, T_integer)
            Default = addUnnamedConstant(-TokN, T_integer)
            getToken()
        elsif toktype=FLOAT then
--          paramDflts[nParams] = addUnnamedConstant(-TokN, T_atom)
            Default = addUnnamedConstant(-TokN, T_atom)
            getToken()
        else
            Aborp("unrecognised")
        end if
    elsif toktype=FLOAT then
--      paramDflts[nParams] = addUnnamedConstant(TokN, T_atom)
        Default = addUnnamedConstant(TokN, T_atom)
        getToken()
    elsif toktype=SQUOTE then
        -- squotes are stored as literal integers.
--      paramDflts[nParams] = addUnnamedConstant(Tok9, T_integer)
        Default = addUnnamedConstant(Tok9, T_integer)
        getToken()
    elsif toktype='{' then
--p2js:
--      DoSequence()
        DoSequence('{',0,true)
        if opTopIsOp then
            Aborp("unsupported")
        end if
--      paramDflts[nParams] = opstack[opsidx]
        Default = opstack[opsidx]
        opsidx -= 1
    elsif toktype=DQUOTE then
--      paramDflts[nParams] = addUnnamedConstant(TokStr,T_string)
        Default = addUnnamedConstant(TokStr,T_string)
        getToken()
    elsif toktype=HEXSTR then
        DoHexStr()
--      paramDflts[nParams] = opstack[opsidx]
        Default = opstack[opsidx]
        opsidx -= 1
--DEV/SUG:
--  elsif toktype='<' then
--      MatchChar('<')
--      MatchString("undefined")
--      MatchString('>')
--      paramDflts[nParams] = {}
--      Default = {}
    else
        Aborp("unrecognised")
    end if
    paramDflts[nParams] = Default
end procedure

integer r_Assignment

--with trace
--function Params()
function Params(sequence signature = {})
-- process the parameter list
--  invoked from DoRoutineDef and DoForwardDef
integer N
integer Typ
integer k
--sequence signature
object used
integer default_found
--integer state
integer SNtyp

--broke 1/12/19 (type is now seq/str, etype is now T_object...)
-- verify compiler gets this right:
--!/**/ #isginfo{signature,0b0100,MIN,MAX,integer,-2}
--p2js: following line broke this...
--!/**/ #isginfo{signature,0b1100,MIN,MAX,object,-2}    --DEV (broken)

--p2js:
    signature = deep_copy(signature)
--  signature = {}
--  nParams = 0
    nParams = length(signature)
    default_found = 0

    if toktype=LETTER then
        tokno = InTable(-InAny)
    end if
    while 1 do
        if toktype!=LETTER then exit end if
        if tokno<=0 then exit end if
        used = symtab[tokno]
        if used[S_NTyp]!=S_Type then exit end if
        used = used[S_State]    -- (also kills a refcount)
        if not and_bits(used,S_used) then
            symtab[tokno][S_State] = used+S_used
        end if
        Typ = tokno
        getToken()
        while 1 do
            if toktype!=LETTER then
if default_found then
--              Aborp("a parameter name is expected here")
                if (toktype!=':' or Ch!='=')
                and toktype!='=' then
                    Aborp("a parameter name or value is expected here")
                end if
end if
                ttidx = -1
--          end if
            else
                N = InTable(-InAny)     -- -ve means no errors (eg/ie namespace rqd)
                if N>0 then
                    SNtyp = symtab[N][S_NTyp]
                    if SNtyp=S_Type then
                        Warn("assumed to be a variable_id, not a type",tokline,tokcol,0)
--21/01/2021
--                  elsif SNtyp=S_Rsvd then
                    elsif N<=T_Asm or SNtyp=S_Rsvd then
                        Aborp("illegal use of a reserved word")
                    end if
                end if
--              signature = append(signature,Typ)
                -- Defer param creation until after ')'.
                k = find(ttidx,paramNames)
--              if ttidx=rtnttidx or (k and k<=nParams) then Duplicate() end if
                if k and k<=nParams then Duplicate() end if
            end if
            nParams += 1                
            if nParams>length(paramNames) then
                paramNames = append(paramNames,ttidx)
                paramTypes = append(paramTypes,Typ)
                paramLines = append(paramLines,tokline)
                paramCols  = append(paramCols, tokcol)
                paramDflts = append(paramDflts,0)
                paramUsed  = append(paramUsed,0)
            else
                paramNames[nParams] = ttidx
                paramTypes[nParams] = Typ
                paramLines[nParams] = tokline
                paramCols [nParams] = tokcol
                paramDflts[nParams] = 0
                paramUsed [nParams] = 0
            end if
if ttidx!=-1 then
            getToken()
end if
-- added 6/4/2012: (may confuse default with named parameter a bit, but it is optional)
            if toktype=':' and Ch='=' then MatchChar(':',false) end if
            if toktype='=' 
            or default_found then
                --
                -- Do not allow a non-defaulted parameter to follow a defaulted one,
                --  otherwise in eg f(integer a=1, b, c=2), an f(0) call would need
                --  to be mapped, somehow, to f(1,0,2), yuk, and double-yuk were that 
                --  f(0) an implicit forward call, mixed in with f(3,2,1)'s etc...
                --
--DEV docs:
                -- Also: Phix does NOT support missing parameters using ",," syntax.
                --  Consider, for example:
                --      function find(object o, sequence s, 
                --                    integer fromIdx=1, toIdx=length(s), 
                --                    direction=+1, step=1)
                --  then instead of
                --      find(o,s,,,,2)
                --  use
                --      find(o,s,step:=2)
                --
                MatchChar('=',float_valid:=true)
                getOneDefault()     -- sets paramDflts[nParams]
--DEV major masking rqd!
--                  Typ += K_oparm
-- NO! put it(^^) on symtab[param]
                -- Once a default is found, all remaining parameters must also be 
                --  defaulted, otherwise it just gets far too messy and confusing.
                default_found = 1
            end if
--end if
            signature = append(signature,Typ)
            if toktype!=',' then
-- removed 2/1/14 (we are going to do an Aborp() or MatchChar(')') rsn anyway)
--              if toktype=LETTER then
--                  tokno = InTable(-InAny)
--              end if
                exit
            end if
            getToken()
            if toktype=LETTER then
                tokno = InTable(-InAny)
                if tokno>0 and symtab[tokno][S_NTyp]=S_Type then exit end if
            end if
--          if isparam then --DEV is this needed at all? [DOH, yeah: procedure p(integer a, integer b) /*<- params just fine; locals quite wrong->*/ integer a, integer b]
                -- treat "integer a,b" as two integers when b is unknown (tokno=0) or not a type,
                -- but in "integer a,atom" treat "atom" as the type of the next parameter:
--              if tokno and symtab[tokno][S_NTyp]=S_Type then exit end if
--          end if
        end while
    end while
--  if isparam and allowDefaulting then
--      return {varnos,signature}
--  end if
    return signature
end function

forward procedure MultipleAssignment(integer isDeclaration, integer Typ)

procedure Locals(integer AllowOnDeclaration)
-- process local declarations
--  invoked from DoRoutineDef and Block
integer N
integer Typ
object used
integer SNtyp

--  if toktype=LETTER then
--      tokno = InTable(-InAny)
--  end if
    while 1 do
        if toktype!=LETTER then exit end if
        tokno = InTable(-InAny)
        if tokno<=0 then exit end if
        used = symtab[tokno]
        if used[S_NTyp]!=S_Type then exit end if
        used = used[S_State]    -- (also kills a refcount)
        if not and_bits(used,S_used) then
            symtab[tokno][S_State] = used+S_used
        end if
        Typ = tokno
--      getToken()
        while 1 do
            getToken()
            if toktype='{' then
--              mapEndToMinusOne = 0
                MultipleAssignment(S_TVar,Typ)
                if toktype!=',' then exit end if
            elsif toktype!=LETTER then
                Aborp("a name is expected here")
            else
                N = InTable(-InAny)     -- -ve means no errors (eg/ie namespace rqd)
                if N>0 then
                    SNtyp = symtab[N][S_NTyp]
                    if SNtyp=S_Type then
                        Warn("assumed to be a variable_id, not a type",tokline,tokcol,0)
--21/01/2021
                    elsif SNtyp=S_Rsvd then
--                  elsif N<=T_Asm or SNtyp=S_Rsvd then
                        Aborp("illegal use of a reserved word")
                    end if
                end if
                tokno = 0 --(since we just fiddled with it to check for types)
                if N then
                    if InTable(-InVeryTop) then Duplicate() end if
                end if
                N = addSymEntry(ttidx,0,S_TVar,Typ,0,0)
                getToken()
--22/2/17:
                if toktype=':' and Ch='=' then MatchChar(':',false) end if
                if toktype='=' then
--                  onDeclaration = 1
                    onDeclaration = AllowOnDeclaration
                    call_proc(r_Assignment,{N,Typ})
                    onDeclaration = 0
                end if  -- toktype='='
                if toktype!=',' then exit end if
            end if
        end while
        if toktype=';' then getToken() end if
    end while
end procedure



--DEV 18/6/2013:
--integer currRtn   -- routine currently being defined (may be a top_level_sub)
--      currRtn = 1


--
-- The flag top_level_abort controls final processing after abort.
-- Set to 0 in Block, 1 in TopLevel. For example if we have:
-- abort(0)
-- "some stuff
-- <EOF>
-- Then the final getToken() via MatchChar(')') within ParamList() will invoke
-- Aborp("missing closing quote") [same with bad "numbers" & some illegal chars].
-- That's fine if the abort was in an if block or routine, but at top level just
-- don't bother after the ")" - we're going to quit rsn anyway.
-- Note that it should not need initialising else (ie via FwdProc/Factor/DoQu)
--
integer top_level_abort
        top_level_abort = 0
integer tok_abort_line, tok_abort_col   -- See TopLevel()

--object dbg
--without trace
--with trace

integer just_sq_floor_div_d
        just_sq_floor_div_d = 0

integer no_of_fwd_calls     -- determines whether we need to dealloc (see t45aod).
        no_of_fwd_calls = 0

-- 5/3/09: fast handling of s[1] = append(s[1],x): (multiple subscripts too!)
--  WARNING: result is (as usual) undefined for eg s[j] = append(s[j],modj()) 
--           where modj is a function which modifies the subscript j. [DEV do we get a warning? (NO 7/3/9)]
integer fastSubscriptLHS        fastSubscriptLHS = 0        -- s in "s[1] = append(s[1],x)"
integer fastSubscriptIcount     fastSubscriptIcount = 0     -- no of subscripts (in opstack[1..n])

--/*
function get_lblidx(integer tidx)
integer lblidx = tt[tidx+EQ]
    if lblidx=0 then
        glblused = append(glblused,G_used)
        glboffset = append(glboffset,0)
--      glbttidx = append(glbttidx,tidx)
        glblabel = append(glblabel,fileno)
        glblline = append(glblline,tokline)
        glblcol = append(glblcol,tokcol)
        glblname = append(glblname,"?")
        lblidx = length(glblused)
        tt[tidx+EQ] = lblidx
    else
        if lblidx>length(glblused) then
            Aborp("label index error")
        end if
        if not and_bits(glblused[lblidx],G_used) then
            glblused[lblidx] += G_used
        end if
    end if
    return lblidx
end function
--*/

include builtins\structs.e as structs
integer new_struct = 0,
        class_def = T_const0
sequence stids = {},    -- symtab indexes of implicit udts
         srids = {},    -- addRoutineId()s of ""
         snames = {},   -- string names of ""
         snids = {},    -- unnamed const of ""
         c_or_s = {}    -- 1="class", 2="struct" (for compile-time error msgs only)

integer N_Icallback = 0,
        N_Icallbacki = 0

--function ParamList(integer fwd, sequence signature, integer paramsOnStack)
--with trace
function ParamList(sequence signature, integer paramsOnStack)
--
-- Process the Parameter List for a Procedure Call
-- Note that forward_call(=fwd) is typically 0 for no, S_fwd (#04) for yes.
--
--  Only invoked from Call().
--
integer sig,act
integer k, pidx, v, pfirst, pthis, pN, mcode, siglen, sigidx, ltype
--plast, 
sequence actsig
integer wasRoutineNo

sequence bpset,     -- backpatch set for a single forward call (see [S_il] in pglobals.e [DEV?])
         txids,     -- ttidx's for named parameter handling
         txcols,    -- tokcols for parameter names
         pmap,      -- map named parameters to actual
         pdone      -- check for duplicate named parameters

integer thisttidx, bpcol
--, namecol
integer rest_must_be_named, minsiglen
--object dbg
--integer fwd       -- bugfix 16/10/10 -- removed..
sequence ptext
integer maxparms
integer was_new_struct = new_struct
    new_struct = 0

--integer lblidx

--object DBG

-- verify compiler gets this right:
--/**/  #isginfo{actsig,0b0100,MIN,MAX,integer,-2}

    wasRoutineNo = routineNo

--trace(1)
--  fwd = forward_call
    bpset = {{tokcol,fileno,currRtn}}
    txids = {}

    if forward_call and routineNo>T_Asm then
        k = -1
        txcols = {}
        minsiglen = 0
    else
        if routineNo<=T_Asm then
            -- (aside: builtins cannot have optional parameters,
            --         although auto-includes obviously can.
            --         That said, there is code below (T_find
            --         and T_match) to "adjust things".)
            minsiglen = length(symtab[routineNo][S_sig])-1
        else
if newEBP then
            minsiglen = symtab[routineNo][S_ParmN]
else
            k = symtab[routineNo][S_Parm1]
            minsiglen = symtab[routineNo][S_ParmN]
            minsiglen -= k-1
end if
        end if
    end if
    thisttidx = ttidx
    pidx = 0
    rest_must_be_named = 0
    actsig = {}
    siglen = length(signature)
    sigidx = 1
--  laststringparam = 0
    if not paramsOnStack then
        tok_abort_line = tokline    -- save for possible use by TopLevel
        tok_abort_col = tokcol      -- ""

        getToken()
        MatchChar('(',float_valid:=true)
        if routineNo=T_routine then -- a call to routine_id()
            -- NB named parameter(!s!) expressly not supported here...
--trace(1)
            Q_Routine = 0
            if toktype=DQUOTE then
                if Ch!=')' then skipSpacesAndComments() end if
                if Ch=')' then  -- not eg "do"&opname[i]
                    if resolveRoutineId() then
                        getToken()
                        MatchChar(')')
                        -- (btw, this return value is not actually used, as
                        --  routineNo=T_routine and Q_Routine is non zero,
                        --  although it does get stored, but then ignored.)
                        return {}
                    end if
                end if  -- Ch=')'
            end if  -- toktype=DQUOTE
            if emitON then
                no_of_fwd_calls = -1
            end if
        elsif routineNo=T_repeat then
--25/10/19:
--          if newEmit and toktype=SQUOTE then
--          if newEmit and toktype=SQUOTE and Ch=',' then
            if toktype=SQUOTE and Ch=',' then
                routineNo = T_repeatch
            end if
        end if -- routineNo=T_routine/T_repeat
        while toktype!=')' do
            if just_sq_floor_div_d then ?9/0 end if
            pidx += 1
            -- check for named parameters
            if ttidx=thisttidx and Ch=':' then
                -- this is the rare/verbose proc(proc:field=value) case.
                -- normally, the inner "proc" paramspace is omitted.
                getToken()
            end if
--          if toktype=':' then
--tryme pt1 of 3: (allows (a:=1) as well as (:a=1) [for named parameters on a call, that is])
            if toktype=':'                      -- :a=1 form
            or (Ch=':' and ChNext('=')) then    -- a:=1 form
                if rest_must_be_named=0 then
                    rest_must_be_named = pidx
                    if not forward_call then
                        --
                        -- populate txids from S_Parm1..S_ParmN
                        --
--                      if routineNo<=T_Bin then
                        if routineNo<=T_Asm then
                            -- (Unless syminit/initialSymEntry create otherwise
                            --  unused symbol table entries for use here...
                            --  20/11/2011: looking again at the list, there is
                            --  not much that might benefit, perhaps lock_file
                            --  and similar; a hll wrapper similar to those in
                            --  pmach.e might be easier than modifying psym.e)
                            Aborp("named parameters not permitted on builtins")
                        end if
                        pfirst = symtab[routineNo][S_Parm1]
                        maxparms = length(symtab[routineNo][S_sig])-1
                        txids = repeat(0,maxparms)
                        txcols = repeat(0,maxparms)
                        for i=1 to maxparms do
                            txids[i] = symtab[pfirst][S_Name]
                            pfirst += 1
                        end for
                        pmap = repeat(0,pidx-1)     -- effectively {1,2,3,...} [DEV use tagset()? or do we check for 0s below?]
                    end if
                end if
-- tryme pt2 of 3:
                if toktype=':' then
                    MatchChar(':',float_valid:=false)
                end if
                if forward_call then
                    while pidx>length(txids) do
                        txids &= 0
                    end while
                    txids[pidx] = ttidx -- save for use below
                else
                    -- lookup ttidx, set pidx accordingly
                    pidx = find(ttidx,txids)
                    if pidx=0 then Aborp("no such parameter") end if
                    pmap &= pidx
                end if
                while pidx>length(txcols) do
                    txcols &= 0
                end while
                txcols[pidx] = tokcol
                getToken()
--tryme pt3 of 3:
--              if Ch=':' then
                if toktype=':' and Ch='=' then MatchChar(':',false) end if
                MatchChar('=',float_valid:=true)
--?             sigidx = pidx
            elsif rest_must_be_named then
                MatchChar(':')  -- trigger an error
--? else
--?     
            end if
            Expr(pAllops, asBool)
            if routineNo = T_floor 
            and opTopIsOp
            and opstack[opsidx]=opDiv then
--              opstack[opsidx] = opDivf    -- now done in Call()
                MatchChar(')')
                return {T_atom}
            elsif just_sq_floor_div_d then
--              just_sq_floor_div_d = 0
                MatchChar(')')
--              if opstack[opsidx]!=routineNo-1 then ?9/0 end if
--              opsidx -= 1         -- res:sq_floor_div is re-pushed in a second...
                return {T_Dsq}
            elsif fastSubscriptIcount then
                -- check for fast s[1] = append(s[1],x) handling:
                --  (this is immediately after "]=append(", ie the first parameter)
                if opTopIsOp!=SubscriptOp
                or opstack[opsidx]!=opSubse
                or opstack[opsidx-1]!=fastSubscriptIcount
                or opstack[fastSubscriptIcount+1]!=fastSubscriptLHS then
                    -- not x[?] = append(x[?], form:
                    fastSubscriptLHS = 0
                else
                    for j=1 to fastSubscriptIcount do
                        -- no fast handling for s[i] = append(s[j],x)!
                        if opstack[j]!=opstack[fastSubscriptIcount+2] then
                            fastSubscriptLHS = 0
                            exit
                        end if
                        fastSubscriptIcount += 1
                    end for
                end if
                -- if fastSubscriptLHS survived, we have a winner!
                --  (this now allows GetFactor() to clear fastSubscriptLHS,
                --   should it reappear anywhere further on, and Call()
                --   to likewise clear should it be a Gvar.)
                fastSubscriptIcount = 0
            end if
            if opTopIsOp then PopFactor() end if
            if sigidx>siglen then
--              if forward_call then
--                  actsig &= plausible(T_object,opstype[opsidx],forward_call,routineNo)
--              else
--              if not forward_call then
--DEV tryme (4/3/10):
                if not forward_call
                or routineNo<=T_Ainc then
                    -- (if you change a builtin/autoinclude, you may/will need to
                    --   modify routine syminit() in psym.e to match.)
                    Aborp("too many arguments")
                end if
--4/3/10:
--              sig = T_object
                sig = 0
                --DEV?? (see note at top of routine)
                siglen += 1
--28/06/2020 (named params on forward auto-includes)
            elsif forward_call and rest_must_be_named then
                sig = T_object
--              -- or, as noted in readme.txt, maybe we should
--              Aborp("named params require explicit include").
--              -- (any maybe get binftab[routineNo] from psym.e)
            else
                sig = signature[pidx]
--              actsig = append(actsig,plausible(signature[sigidx],opstype[opsidx],forward_call,routineNo))
--              actsig = append(actsig,plausible(signature[sigidx],symtab[opstack[opsidx]][S_vtype],forward_call,routineNo))
--              sigidx += 1
            end if
            sigidx += 1 -- 26/2
            lastparam = opstack[opsidx]     -- used in Branch, for localtype info off (eg) "if udt() then".
--trace(1)
--          if not emitON or lastparam=0 or symtab[lastparam][S_Name]=-1 then
            if not emitON or symtab[lastparam][S_Name]=-1 then
                act = opstype[opsidx]
            else
--              act = symtab[lastparam][S_vtype]
--dbg = symtab[lastparam]
                act = symtab[lastparam][S_ltype]
            end if
--          if act=T_string then
--              laststringparam = lastparam
--          end if
--          actsig = append(actsig,plausible(sig,act,forward_call,routineNo))
--          actsig = append(actsig,plausible(sig,act,routineNo))
--          wasRoutineNo = routineNo    -- 13/11/10 moved to top
-- (bugfix 16/10/10: this call reset forward_call; local fwd added to replace it)
-- (nope; if we assume sq_rand instead of sq_rand, then it can become forward_call!)
--4/10/2020 (Icallback->Icallbacki mapping)
if wasRoutineNo = N_Icallback
and signature = {8,1}
and actsig = {}
and sig = 8
and act = 1 then
    wasRoutineNo = N_Icallbacki
    routineNo = N_Icallbacki
    signature = {1}
    sig = 1
    minsiglen = 1
end if
            actsig = append(actsig,plausible(sig,act))
--          actsig = append(actsig,plausible(sig,act,lastparam))
            if sigidx=2                 -- first param!
            and length(signature)=2     -- of 2
--          and wasRoutineNo<T_Bin
--          and routineNo>T_Bin then    -- assume sq_able just did the deed
            and wasRoutineNo<T_Asm
            and routineNo>T_Asm then    -- assume sq_able just did the deed
                signature[2] = T_object
            end if

--DEV (spotted in passing) [erm, ..PROBABLY NOT..]
--  surely there should be this here??? (untried)
--          routineNo = wasRoutineNo

            if toktype!=',' then exit end if
            MatchChar(',',float_valid:=true)
        end while -- toktype!=')'

    end if

    if wasRoutineNo=T_new then
--trace(1)
--      printf(1,"pmain.e/ParamList() line 4173: wasRoutineNo=T_new, sigidx=%d, opsidx=%d, new_struct=%d\n",{sigidx,opsidx,new_struct})
        if opTopIsOp then PopFactor() end if -- (temp-store any opMkSq of {imm}, before we test/swap things)
        if sigidx=1                                                 -- () ==> (rid)
        or (sigidx=2 and                                            -- ({imm}) ==> (rid,{imm})
            not find(opstype[opsidx],{T_string,T_integer})) then    -- but (int/str) ==> as-is   (ditto (i/s,{imm}))
            k = find(was_new_struct,stids) 
--          ?{"ParamList (pmain.e line 4187); new_struct is",was_new_struct,", k=>",k}
            if k=0 or was_new_struct=0 then
                -- person p = new() is shorthand for person p = new(routine_id("person")), but for eg
                -- object p = new() then the compiler has no idea what it should actually create...
                -- (The programmer may use new("person"), but the compiler always uses routine_id().)
                -- (The programmer can now also use new(person), added since this was first written.)
                -- (The "or new_struct=0" test part should not be needed, but no harm leaving it in.)
                Aborp("a class/struct type must be explicitly specified here")
            elsif and_bits(structs:get_struct_flags(was_new_struct),S_ABSTRACT) then
--              Aborp("may not instantiate abstract class/struct")
                Aborp("abstract "&{"class","struct"}[c_or_s[k]])
            end if
            k = srids[k]
--addRoutineId()
            PushFactor(k,true,T_integer)            -- default sdx to new_struct
            actsig &= T_integer
            sigidx += 1
            if sigidx=3 then                        -- ({imm},rid) need swapping:
                opsidxm1 = opsidx-1
                integer t1, t2
                t1 = opstack[opsidx];  t2 = opstack[opsidxm1];  opstack[opsidx] = t2;  opstack[opsidxm1] = t1;
                t1 = opsltrl[opsidx];  t2 = opsltrl[opsidxm1];  opsltrl[opsidx] = t2;  opsltrl[opsidxm1] = t1;
                t1 = opstype[opsidx];  t2 = opstype[opsidxm1];  opstype[opsidx] = t2;  opstype[opsidxm1] = t1;
                -- (opsline, opstcol left as-is)
                t1 = actsig[1];        t2 = actsig[2];          actsig[1]       = t2;  actsig[2]         = t1;
--              actsig = reverse(actsig) -- oh dear, messes up #isginfo...
            end if
        elsif sigidx=2
          and opsidx=1
          and opsltrl[1]
          and opstype[1]=T_string then
            -- literal string, eg new("person"), we can check now.
            --  (A run-time check also exists in new() itself)
            string s = symtab[opstack[1]][S_value]
            if and_bits(structs:get_struct_flags(s),S_ABSTRACT) then
                k = find(s,snames)
                Aborp("abstract "&{"class","struct"}[c_or_s[k]])
            end if
        end if
    -- map min(s1), max(s1) to minsq(s1), maxsq(s1). (added 11/12/14)
    elsif siglen=2
      and sigidx=2 then
        if routineNo=T_min then
            routineNo = T_minsq
            minsiglen = 1
        elsif routineNo=T_max then
            routineNo = T_maxsq
            minsiglen = 1
        end if
    elsif wasRoutineNo=T_EnterCS 
       or wasRoutineNo=T_LeaveCS then -- opEnterCS or opLeaveCS
        if sigidx=1 then
            PushFactor(T_const0,true,T_integer) -- default cs to 0
            actsig &= T_integer
            sigidx = 2
            siglen = 1
            minsiglen = 1
        end if
    end if
    -- 26/11/15 (routine x(string name, rid=routine_id(name)) support):
    if length(actsig)>0 
    and length(actsig)=minsiglen-1
    and paramsOnStack=0
    and not forward_call then
        if lastparam=0 then
--      if laststringparam=0 then
            if emitON=0 then
                -- 11/3/16, cope with Icallback("xx") under emitON=0
                --  (we just have to assume the routine name would
                --   resolve - at least we do not emit wrong code.)
                k = symtab[wasRoutineNo][S_Parm1]+length(actsig)
                if and_bits(symtab[k][S_State],K_drid) then
                    PushFactor(T_const0,true,T_integer)
                    actsig &= T_integer
                    sigidx += 1
                    siglen += 1
                end if  -- (K_drid)
--          else
--              (report missing params as normal below)
            end if
        else
            if symtab[lastparam][S_Name]=-1
            and symtab[lastparam][S_NTyp]=S_Const
            and symtab[lastparam][S_ltype]=T_string
            and and_bits(symtab[lastparam][S_State],K_litnoclr)=K_litnoclr then
                k = symtab[wasRoutineNo][S_Parm1]+length(actsig)
                if and_bits(symtab[k][S_State],K_drid) then
                    TokStr = symtab[lastparam][S_value]
                    Q_Routine = 0
                    if resolveRoutineId() then
                        k = addRoutineId(Q_Routine)
--DEV 1? (5/1/17)
                        PushFactor(k,true,T_integer)
--                      PushFactor(k,false,T_integer)
                        actsig &= T_integer
                        sigidx += 1
                        siglen += 1
                    else
                        Aborp("error resolving routine_id")
                    end if
                    Q_Routine = 0
                end if  -- (K_drid)
            end if -- (literal string)
        end if -- lastparam=0 (emitON=0)
    end if -- (actsig one too short, etc)

    -- initialise the parameter backpatch list if needed:
    if forward_call then
        if emitON then
            if no_of_fwd_calls>=0 then
                no_of_fwd_calls += 1
            end if
        end if
--DEV...
        symtab[routineNo][S_1stl] += 1  -- count of fwd calls to this routine.
        if equal(symtab[routineNo][S_il],0) then
            if emitON then -- 29/08/2010
                symtab[routineNo][S_il] = {}
            end if
        end if
    end if

    --
    -- Typical case to consider: function a(x,y) calls itself recursively as a(y,x).
    -- 1) By the time we pass y, x has been overridden (or vice versa). Should we really
    --    need to pass an earlier, already overwritten parameter, restore it from the stack.
    -- 2) You cannot "scan" for (nested) uses of x, hence all the parameters are calculated 
    --    and left on the stack, the actual setup is deferred until as late as possible. In
    --    the a(1,g(x)) case, this means that x is fine, rather than 1, when we invoke g().
    -- 3) In the case of a forward call, a backpatch set is maintained for each parameter.
    --    This is stored in S_il, and will be overwritten when the actual routine is defined.
    --    Each parameter defines a single link in the form {routineNo,code offset}, and at that
    --    place is a link to the previous backpatch (in the same format) or 0 at end of chain.
    --
--  if routineNo>T_Bin then
    if routineNo>T_Asm then
--DEV try "" or not integer[S_il]
        --
        -- NB When pemit.e finds an isOpCode at [i], with an opFrame at [i+3],
        --     then it patches noofparams at dword[i-10], and first at [i-12].
        --    This is needed for forward routines, as noofparams actually 
        --     includes local vars and temporaries. Theoretically you could 
        --     class this as a bug insofar as pmain.e could/should backpatch 
        --     it properly, but we must use opFrame to chain up our routines
        --     to-do-list in pemit.e anyway. Since there is no point setting
        --     things up half the time (things like printf are fwd routines)
        --     we may as well always leave first & N set to 0 here.
        --
        if opTopIsOp then PopFactor() end if
if newEBP then
        -- save eax if rqd
        saveFunctionResultVars(opsidx,INTSTOO)
end if
        if emitON then
if newEmit then
            agcheckop(opFrame)
--          ttidx = aatidx[opFrame]
--          if tt[ttidx+EQ]=0 then
--              agcheck(ttidx)
----                lblidx = get_lblidx(ttidx)
--          end if
--                  lblidx = find(aatidx[opFrame],glbttidx)

--          if glbopFrame=0 then ?9/0 end if
--          ttidx = gblttidx[glbopFrame]
--          if tt[ttidx+EQ]=0 then
--              agcheck(glbopFrame)
--              lblidx = get_lblidx(ttidx)
--          end if
--          if tt[glttidxFrame+EQ]=0 then
--              agcheck(glttidxFrame)
--          end if
--          if glbopFrame=0 then
--              glbopFrame = get_lblidx(glttidxFrame)
--          end if
end if
            apnds5({opFrame,routineNo})
        end if
        pfirst = symtab[routineNo][S_Parm1]
        if not paramsOnStack then
            siglen = length(actsig)
        end if
        if emitON then
            onDeclaration = 1
--          onDeclaration = 2
            if siglen then          -- skip this lot in the zero parameter case
                k = opsidx-siglen+1     -- locate first param in opstack
                pidx = 1
                if forward_call then
                    while 1 do
                        v = opstack[k]
--if pbrON then
--  onDeclaration = 1
--  if v=lhsvar then
--      for j=k+1 to opsidx do
--          if opstack[j]=lhsvar then
--              lhsvar = 0
--              exit
--          end if
--      end for
--      if lhsvar then
--          onDeclaration = 2
--          lhsvar = 0
--      end if
--  end if
--end if
--DEV with implicit forward definitions, we cannot be certain of parameter types, eg:
--      p(1)    -- we think it is int
--      p(2)    -- we still think it is int
--      p(2.5)  -- ah, we now think it is atom
--      procedure p(  -- only here do we find out for sure... it could yet be object.
-- However, after:
--      forward procedure p(xxx)
-- we /do/ know (and we still need to set up the backpatch chain).
-- But this code ignores any such possibility - no opMovbi/ti, the [untested] fwd 
--  handling of them in emitHexMov is currently commented out.
--DEV [[gscan ought to deal with this better/properly anyhows]]

                        if oneInitialisedInt(v,0) then
                            mcode = opMovsi
                        else
                            mcode = opMove
                        end if
--NB: use of schedule/schend must not be allowed/invoked in this case...
if newEBP then
                        if symtab[v][S_NTyp]=S_TVar then
--DEV if fRes then apnds5({opPopVar,-9}); dpos=length(s5) if v=lhsvar then ?9/0 --mcode=opPopVar
                            ltype = symtab[v][S_ltype]
                            if ltype>T_object then ltype = rootType(ltype) end if
                            apnds5({opFrst,-9,v,ltype,onDeclaration})
                            dpos = length(s5)-3  -- lodate that -9 (for later replacement)
                            mcode = opFrst
                        else
                            emitHexMov(mcode,-9,v)
                        end if
else
                        emitHexMov(mcode,-9,v)
end if
                        if pbrON then
                            if v=lhsvar then
                                if mcode=opMove and lhspos=0 then
                                    lhspos = length(s5)-1 -- locate that onDeclaration
                                elsif mcode=opFrst and lhspos=0 then
                                    lhspos = length(s5)   -- locate that onDeclaration
                                else
                                    lhsvar = 0
                                end if
                            end if
                        end if
                        bpcol = opstcol[k]
                        -- see bcptr in DoRoutineDef
                        if pidx<=length(txids) and txids[pidx] then
                            bpset = append(bpset,{bpcol,dpos,txids[pidx],txcols[pidx]})
                        else
                            bpset = append(bpset,{bpcol,dpos})
                        end if
                        if k=opsidx then exit end if
                        k += 1
                        pidx += 1
                    end while
                    symtab[routineNo][S_il] = append(symtab[routineNo][S_il],bpset)

                else -- not forward_call
--DEV
--  if rest_must_be_named then ?9/0 end if
                    pthis = pfirst
                    if rest_must_be_named then
                        -- check for duplicates (eg f(x:=1,...,...,x:=2))
--2/6/14: (might be worth re-examining this...)
--                      pdone = repeat(0,siglen)
                        pdone = repeat(0,length(signature))
-- added 6/6/14:
                        for i=1 to rest_must_be_named-1 do
                            pdone[i] = 1
                        end for
                    end if
                    while 1 do  -- until k=opsidx
                        pN = pidx
                        if rest_must_be_named and pidx>=rest_must_be_named then
--                          if pidx>length(pmap) then
--                              Aborp("missing parameters")
--                          end if  
                            pN = pmap[pidx]
--2/6/14: (undone, w/o ever testing)
                            if pN>length(pdone) then
--                          if pN>=rest_must_be_named
--                          and pN>length(pdone) then
                                Aborp("missing parameters")
                            end if
                            if pN<rest_must_be_named    -- clash with unnamed(/numbered/normal) params
                            or pdone[pN] then
                                tokcol = txcols[pN]
-- DEV? (untried)               no_oops = 1
                                Aborp("duplicated parameter")
                            end if
                            pdone[pN] = 1
                            pthis = pfirst+pN-1
                        end if
                        if DEBUG then
                            if symtab[pthis][S_NTyp]!=S_TVar then ?9/0 end if
                        end if
                        v = opstack[k]
-- if fRes then apnds5({opPopVar,pthis}) if v=lhsvar then ?9/0
--                      if v>=pfirst and v<=plast
--                      if (newEBP or (v>=pfirst and v<=plast)) -- (newEBP: next comment is wrong)
--                      and symtab[v][S_NTyp]=S_TVar then   -- must be inside the routine itself then
                        if symtab[v][S_NTyp]=S_TVar then    -- must be inside the routine itself then
--                          lhsvar = 0 --??
                            ltype = symtab[v][S_ltype]
                            if ltype>T_object then ltype = rootType(ltype) end if
--DEV pbr
                            apnds5({opFrst,pthis,v,ltype,onDeclaration})
if newEBP then
                            if pbrON then
                                --  onDeclaration = 1
                                if v=lhsvar then
                                    if lhspos=0 then
                                        lhspos = length(s5)
                                    else
                                        lhsvar = 0
                                    end if
                                end if
                            end if
end if
                        else
                            if symtab[routineNo][S_sig][pN+1]=T_integer then
                                if oneInitialisedInt(v,0) then
                                    mcode = opMovbi
                                else
                                    mcode = opMovti
                                end if
                            elsif symtab[v][S_Name]=-1
                              and symtab[v][S_NTyp]=S_TVar then
                                -- a temporary: dest:=src, src=0, ie no incref/decref/dealloc.
--                              onDeclaration = 1
                                mcode = opMove
                            else
                                if oneInitialisedInt(v,0) then
                                    mcode = opMovsi
                                else
                                    mcode = opMove
                                end if
                            end if
                            emitHexMov(mcode,pthis,v)
--                          onDeclaration = 0
                            if pbrON then
                                --  onDeclaration = 1
                                if v=lhsvar then
                                    if mcode=opMove and lhspos=0 then
                                        lhspos = length(s5)-1  -- locate the onDeclaration
                                    else
                                        lhsvar = 0
                                    end if
                                end if
                            end if
                        end if
--                      opsidx -= 1 --NB not freeTmp(-1) [DEV??]
                        if k=opsidx then exit end if
                        k += 1
                        pidx += 1
                        pthis += 1
                    end while
-- put back 6/6/14:
--                  if rest_must_be_named
--                  and find(0,pdone) then
--                      Aborp("missing parameters")
                    if rest_must_be_named then
                        pidx = find(0,pdone)
                        if pidx and pidx<=minsiglen then
                            Aborp("missing parameters")
                        end if
                    end if
--  (we need a mapping from opstack to S_Parm1..S_ParmN)

                end if  -- forward_call/not forward_call
                if opsidx!=k then ?9/0 end if
            end if  -- siglen
            if siglen then
                freeTmp(-siglen)
            end if
            onDeclaration = 0
--DEV: pbr: (as per append(), so we can zero lhs in assignment, if used precisely once)
--          (actually, I want {a,b[5],c} = f(a,b[5],c) to do triple-pbr, so leave this for now...)
--          PushOp(opCall,CallOp)
--          s5 = append(s5,opCall)
            s5 &= opCall
        else -- not emitON
            if siglen then
                freeTmp(-siglen)
            end if
        end if -- emitON
    end if

    if not paramsOnStack then
        if sigidx<=siglen then
--25/5/16 (find is now hll)
--          if (wasRoutineNo=T_find or wasRoutineNo=T_match) -- opFind or opMatch
            if 0
            and sigidx=3 then
                PushFactor(T_const1,true,T_integer) -- default "find/match from" to 1
                actsig &= T_integer
                sigidx += 1
--          elsif (wasRoutineNo=T_EnterCS or wasRoutineNo=T_LeaveCS) -- opEnterCS or opLeaveCS
--            and sigidx=1 then
--              PushFactor(T_const0,true,T_integer) -- default cs to 0
--              actsig &= T_integer
--              sigidx += 1
--          elsif wasRoutineNo=T_get_text
--            and sigidx=2 then
--              k = addUnnamedConstant(-2,T_integer)
--              PushFactor(k,true,T_integer)            -- default option to -2
--              actsig &= T_integer
--              sigidx += 1
            elsif wasRoutineNo=T_throw
              and sigidx=2 then
                k = addUnnamedConstant({},T_Dsq)
                PushFactor(k,true,T_Dsq)                -- default user_data to {}
                actsig &= T_Dsq
                sigidx += 1
            end if
        end if

        if sigidx<=minsiglen then
            Aborp("missing parameters")
        end if

        if routineNo=T_abort
        and top_level_abort then
            -- avoid eg "abort(0)\n~$" from generating
            -- an unnecessary 'invalid character' error,
            -- deep inside the following MatchChar().
            -- (nb we want "abort(0<EOF>" to error)
            -- (needs fixup if we go the flat string route)
            if col<length(text) then
--trace(1)
if 1 then   -- 29/12/2010

                allpfiles[fileno] = 0   -- reduce ref count
--              text = text[1..col]
                text = text[1..col] & "\r\n"    -- 1/10/2011
                allpfiles[fileno] = text

                ptext = allfiles[fileno]
                allfiles[fileno] = 0    -- reduce ref count
--              ptext = ptext[1..col]
                ptext = ptext[1..col] & "\r\n"  -- 1/10/2011
                allfiles[fileno] = ptext

                ltl = col
else
                allfiles[fileno] = 0    -- reduce ref count
                text = text[1..col]
                allfiles[fileno] = text
                ltl = col
end if
            end if
        end if
        MatchChar(')')
    end if
    return actsig
end function

constant PROC='P', FUNC='F', TYPE='T', PFT = "PFT"

global -- for pilasm.e
integer lMask       -- local variables affected by a loop
        lMask = E_none

integer VAmask
        VAmask = 0

integer tpwarnshown
        tpwarnshown = 0

--with trace
procedure Call(integer rtnNo, sequence signature, integer rType, integer paramsOnStack)--, integer fwd)
--
-- NB: fwd indicates whether this call is from ForwardProc, not whether backpatch
-- handling is required [which is and_bits(symtab[rtnNo][S_state],S_fwd)].
--
-- paramsOnStack is used for the ? shorthand and things like realising that 
--  eg 1 + {3,4} needs to use sq_add(), (see sqopWarn) but:
--      a) we only realise that when 1,{3,4} are loaded, and
--      b) there is no ( , , ) for ParamList anywhere anyway.
--
integer R, s1, opcode, p1, p2, tidx, ftype
integer state, isForward, wasUsed
--integer osi
--object dbg
integer thisEffect
integer wasRoutineNo, wasFwd, wascalltokcol, wascalltokline
integer isLiteral
--integer Xttidx
--integer lblidx
object dbg -- DEV (temp)

--trace(1)
    s1 = signature[1]

    wasRoutineNo = routineNo
    routineNo = rtnNo

--DEV if routineNo!=0/emitON
    state = symtab[routineNo][S_State]
    isForward = and_bits(state,S_fwd)
    if isForward and routineNo>T_Ainc then
        thisEffect = E_all
    else
        thisEffect = symtab[routineNo][S_Efct]
    end if
    if routineNo!=currRtn then
        SideEffects = or_bits(SideEffects,thisEffect)
    end if
    if and_bits(VAmask,thisEffect) then
--removed/triggered 1/12/19, adding structs...
--      if rType=PROC then ?9/0 end if  -- sanity check
        if lint then
            Warn("might modify LHS variable.",tokline,tokcol,0)
        end if
        VAmask = 0
    end if

--DEV ripped out 22/10/09 (see e6.exw)
--  if fastSubscriptLHS 
--  and fastSubscriptIcount=0
--  and rtnNo>=T_command_line
--  and symtab[fastSubscriptLHS][S_NTyp]=S_GVar2 then
--      fastSubscriptLHS = 0
--  end if

    wasFwd = forward_call
    forward_call = isForward

    wascalltokcol = calltokcol
    wascalltokline = calltokline
    calltokcol = tokcol     -- in case needed for warnings by plausible
    calltokline = tokline

    wasUsed = and_bits(state,S_used)
    if not wasUsed
--  and emitON
--  and routineNo!=T_routine then
    and routineNo!=T_routine
    and ttidx!=T_trace
    and ttidx!=T_profile then
--DEV 29/08/2010:
-- We must do this for user-routines, under emitON=0, to suppress not used warnings.
-- However "no warning" != "generate code".... but at the mo I think it is.
--guard added 30/1/15: (removed prior to testing, as per code following...)
--if routineNo>T_Ainc then
        symtab[routineNo][S_State] = state+S_used
--end if
        if isForward
        and routineNo>T_Bin     -- Ah!!
--      and routineNo>T_Asm
        and routineNo<=T_Ainc then
if not repl or integer(symtab[routineNo][S_Name]) then
            tidx = symtab[routineNo][S_Name]
            if tidx=T_machine_func
            or tidx=T_machine_proc then
                Warn(getname(tidx,-2)&" is deprecated\n",tokline,tokcol,0)
            end if
            if emitON then
                if not builtinsReferenced then
                    if AutoIncWarn then
                        if lint then
                            Warn(getBuiltinName(routineNo)&" auto-included",tokline,tokcol,0)
                        end if
                    end if
                    builtinsReferenced = 1--and_bits(symtab[routineNo][S_State],S_fwd)
                end if
            else
                symtab[routineNo][S_State] = state  -- undo
            end if
end if
        end if
    end if

    if rType=PROC then
        if DEBUG then
            if not paramsOnStack then
                if opsidx!=0 then ?9/0 end if
            end if
        end if
        if find(s1,"FT") then
            Aborp("function result must be assigned or used")
        end if
        if DEBUG then
            if s1!=PROC then ?9/0 end if
        end if
        signature = signature[2..length(signature)]
        signature = ParamList(signature,paramsOnStack)
--DEV should we keep these separate?
--      if routineNo<=T_Bin then
        if routineNo<=T_Asm then
--DEV and integer symtab[routineNo][S_il]?? (for say poke8() overriding AutoAsm)
            if opTopIsOp then PopFactor() end if
            if emitON then
                opcode = symtab[routineNo][S_il]
--/*
-- now a hll in get_interpreter.e:
                if routineNo=Z_requires then
--?{"requires",opsidx,opsltrl[1]}
                    integer opst1 = opstype[1]
                    if opsidx!=1
                    or opsltrl[1]!=1
                    or (opst1!=T_string and opst1!=T_integer) then
                        Abork("must be literal",1)
                    end if
                    if opst1=T_string then
                        string v = substitute(symtab[opstack[1]][S_value],"."," ")
--?{"v",v}
                        sequence r = scanf(v,"%d %d %d")
                        if length(r)!=1
                        or r[1]>phixversion then
                            Abork("this is "&phixverstr,1)
                        end if
                    else
                        integer b = symtab[opstack[1]][S_value]
--?{"b",b}
                        if b=32 then
                            if X64=1 then
                                -- (You may only have a 32-bit dll...)
                                Abork("requires 32 bit",1)
                            end if
                        elsif b=64 then
                            if X64=0 then
                                -- (Much more common than the above, I'll wager...)
                                Abork("requires 64 bit",1)
                            end if
                        else
                            Abork("only 32 or 64 bit architectures supported",1)
                        end if
                    end if
--                  return -- emit nothing!
                    routineNo = 0 -- (no opCall please!)
--if opcode=opDeleteCS then ?9/0 end if
                els
--*/
                if opsidx=0 then
                    if DEBUG then
                        if opcode!=opClrScrn
                        and opcode!=opFreeCons
                        and opcode!=opProfout then
--                      and opcode!=opTestN
--                      and opcode!=opTestM then
                            ?9/0
                        end if
                    end if
--                  if opcode=opTestN
--                  or opcode=opTestM then
----                    elsif opcode=opTestN then
----                        ?9/0
----                        lblidx = get_lblidx(opcode)
--if not newEmit then ?9/0 end if
--                      lblidx = get_lblidx(aatidx[opcode])
----/*
----            Xttidx = aatidx[opcode]
----            lblidx = tt[Xttidx+EQ]
----            if lblidx=0 then
----                glblused = append(glblused,G_used)
------              glblused = append(glblused,0)
----                glboffset = append(glboffset,0)
----                glblabel = append(glblabel,fileno)
----                glblline = append(glblline,tokline)
----                glblcol = append(glblcol,tokcol)
----                glblname = append(glblname,"?")
----                lblidx = length(glblused)
----                tt[Xttidx+EQ] = lblidx
----            else
----                if lblidx>length(glblused) then
----                    Aborp("label index error")
----                end if
----                if not and_bits(glblused[lblidx],G_used) then
----                    glblused[lblidx] += G_used
----                end if
----            end if
------          if not and_bits(glblused[lblidx],G_used) then
------              glblused[lblidx] += G_used
------              -- mark for autoinclude... [may not need to]
------          end if
----
----*/
--                      apnds5({opcode,lblidx})
--                  else
                        apnds5(opcode)
--                  end if
                elsif opsidx=1 then
                    -- added 1/12/15 (as a way of saying what this does [I got confused by opDelete])
                    if DEBUG then
                        if opcode!=opTrace
                        and opcode!=opFree
                        and opcode!=opClose
                        and opcode!=opFlush
                        and opcode!=opSleep
                        and opcode!=opBkClr
                        and opcode!=opTxtClr
                        and opcode!=opSetRand
                        and opcode!=opDelete
                        and opcode!=opEnterCS
                        and opcode!=opLeaveCS
                        and opcode!=opDeleteCS
                        and opcode!=opCrashFile
                        and opcode!=opCrashMsg
                        and opcode!=opCrashRtn
                        and opcode!=opAbort
                        and opcode!=opWrap
                        and opcode!=opProfile then
--                      and opcode!=opThrow then
                            ?9/0
                        end if
                    end if
--DEV I think we need similar (not bind) handling for opProfile! (also, unsure about that 6/12/09 change... elsif??)
--if opcode=opProfile and not bind then
--  -- (it does not really matter as long as opLnp/opLnpt are not invoked, btw,
--  --  which may explain why this has never been a problem in the past... and
--  --  likewise that suspect 6/12/09 change may be ok if without any opLnt.)
--  puts(1,"pmain.e line 4341: warning: opProfile handling suspect\n")  --DEV
--end if
                    if opcode=opTrace then
-- 6/12/09:
--                      if not bind and optset[OptTrace] then
                        if not bind then
                            if optset[OptProfile]
                            or optset[OptProfileTime] then
                                if not tpwarnshown then
                                    -- NB this should NOT be hidden by "without warning"!
                                    --  (it helps me a great deal, and besides, if you have
                                    --   got a with profile[_time] on, why would you need
                                    --   without warning to be honoured anyway...?)
                                    puts(1,"warning: trace() is disabled when profiling\n")
                                    tpwarnshown = 1
                                end if
                            else
--DEV we shouldn't need this?! (trace(func()), even so what's going to trash eax?) [erm... opFrame?!]
if newEBP then
        -- save eax if rqd
        saveFunctionResultVars(opsidx,NOTINTS)
end if
                                p1 = opstack[1]
--DEV added 27/7/14 (to get terror.exw working again):
                                if not symtab[p1][S_Init] then
                                    Unassigned(p1)
                                end if
                                apnds5({opcode,p1})
                            end if
                        end if
--now done via T_throw, with {}:
--                  elsif opcode=opThrow then
--                      if not optset[OptDebug] then
--                          Aborp("without debug is in force")
--                      end if
--                      p1 = opstack[1]
--                      if not symtab[p1][S_Init] then
--                          Unassigned(p1)
--                      end if
--                      agcheckop(opThrow)
--                      apnds5({opThrow,p1,0})
--                  else -- (not opTrace/Throw)
                    elsif opcode!=opProfile or not bind then
--                      if not integer(opcode) then ?9/0 end if
                        if opcode<1 or opcode>maxVop then ?9/0 end if   -- (added 19/11/14)
if newEBP then
                        -- save eax if rqd
                        saveFunctionResultVars(opsidx,NOTINTS)
end if
                        p1 = opstack[1]
--DEV added 27/7/14 (to get terror.exw working again):
                        if not symtab[p1][S_Init] then
                            Unassigned(p1)
                        end if
if newEmit then
                        if opcode=opAbort then
                            agcheckop(opcode)
                        end if
end if
                        apnds5({opcode,p1})
                    end if
                elsif opsidx=2 then
                    if DEBUG then
                        if opcode!=opPuts
                        and opcode!=opPoke
                        and opcode!=opPoke1
                        and opcode!=opPoke2
                        and opcode!=opPoke4
                        and opcode!=opPoke8
                        and opcode!=opPosition
                        and opcode!=opCallProc
                        and opcode!=opUnLock
                        and opcode!=opThrow then
                            tidx = symtab[routineNo][S_Name]
--                          ?{routineNo,getBuiltinName(routineNo),getname(tidx,-2)}
                            ?routineNo ?{getBuiltinName(routineNo),getname(tidx,-2)}
                            ?9/0
                        end if
                    end if
if newEBP then
                    -- save eax if rqd
                    saveFunctionResultVars(opsidx,NOTINTS)
end if
                    p1 = opstack[1]
                    p2 = opstack[2]
if newEmit then
                    if not symtab[p1][S_Init] then
                        Unassigned(p1)
                    end if
                    if opcode=opThrow then
                        if not optset[OptDebug] then
                            Abork("without debug is in force",1)
                        end if
                    end if
                    if not symtab[p2][S_Init] then
                        Unassigned(p2)
                    end if
                    agcheckop(opcode)
end if
                    apnds5({opcode,p1,p2})

--                  if opcode=opCallProc then
--?9/0 --DEV
--                      if NOLT=0 or bind or lint then
--                          ltCall(0,E_vars,length(s5)-2)   -- clear all gvar type info
--                      end if -- NOLT
--                  end if

                elsif opsidx=3 then
                    if DEBUG then
                        if opcode!=opMemCopy
                        and opcode!=opMemSet
                        and opcode!=opScroll
                        and opcode!=opPokeN then
                            ?9/0
                        end if
                    end if
if newEBP then
        -- save eax if rqd
        saveFunctionResultVars(opsidx,NOTINTS)
end if
                    p1 = opstack[1]
                    p2 = opstack[2]
                    p3 = opstack[3]
--DEV added 27/7/14 (to get terror.exw working again):
                    if not symtab[p1][S_Init] then
                        Unassigned(p1)
                    end if
                    if not symtab[p2][S_Init] then
                        Unassigned(p2)
                    end if
                    if not symtab[p3][S_Init] then
                        Unassigned(p3)
                    end if
if newEmit then
                    agcheckop(opcode)
end if
                    apnds5({opcode,p1,p2,p3})
                else ?9/0   -- unknown opsidx
                end if
            end if -- emitON
            freeTmp(-opsidx)
            if routineNo=T_abort then LastStatementWasAbort = 1 end if
        end if
--DEV removed for multiple assignment (idx may be on the stack)
--      if DEBUG then
--          if opsidx!=0 then ?9/0 end if
--      end if
    else -- rType=FUNC|TYPE
--trace(1)
        if not find(s1,"FT") then
            if s1=PROC then
                Aborp("a procedure name is not expected here")
            end if
            Aborp("signature error ("&sprint(signature)&")")
        end if
--DEV what about [call,call_proc,]call_func,[c_proc,]c_func? (just document it?)
-- (or, before /any/ such, save (any and) /all/ function result vars in opstack)
--      if routineNo>T_Bin then
        if routineNo>T_Asm then
            --
            -- Before making a function call, "backup" all function result vars,
            --  otherwise eg x=g(1)+g(2) acts as Tmp=g(1) Tmp=g(2) x=Tmp+Tmp.
            -- Likewise/obviously in y=a(1)+b(2), routine b() might use a(), we 
            --  cannot be sure, so just save the lot.
            --
            if opTopIsOp then PopFactor() end if
            saveFunctionResultVars(opsidx,INTSTOO)
        end if
--?signature
--if routineNo = T_equal then trace(1) end if
        signature = signature[2..length(signature)]
--?{signature,paramsOnStack}
--      signature = ParamList(routineNo,isForward,signature,paramsOnStack)
        signature = ParamList(signature,paramsOnStack)
--DEV not sure about this:
--      if routineNo<=T_Bin then
        if routineNo<=T_Asm then
--      if rtnNo<=T_Bin then
            if routineNo = T_floor 
            and opTopIsOp
            and opstack[opsidx]=opDiv then
                opstack[opsidx] = opDivf
--validate_opstack()
--Moved below 24/3:
--          elsif rtnNo=T_floor and routineNo!=T_floor then
--              -- assume an inner sq_floor_div happened
----DEV try this (instead of clearing above):
--              if just_sq_floor_div_d!=1 then ?9/0 end if
--              just_sq_floor_div_d = 0
----    oops, that would be fine expect we'v replaced floor with sq_floor_div
----            and hence spannered that routineNo<=T_Bin above.
            elsif routineNo=T_equal then
                PushOp(opJeq,BranchOp)
            elsif routineNo=T_platform then
                if PE then
                    PushFactor(T_win32,true,T_integer)
                else
                    PushFactor(T_linux,true,T_integer)
                end if
            elsif routineNo=T_machine_bits then
                if X64 then
                    PushFactor(addUnnamedConstant(64, T_integer),true,T_integer)
                else
                    PushFactor(addUnnamedConstant(32, T_integer),true,T_integer)
                end if
            elsif routineNo=T_machine_word then
                if X64 then
                    PushFactor(addUnnamedConstant(8, T_integer),true,T_integer)
                else
                    PushFactor(addUnnamedConstant(4, T_integer),true,T_integer)
                end if
            elsif routineNo=Z_version then
--              PushFactor(addUnnamedConstant(phixversion, T_Dsq),true,T_Dsq)
                PushFactor(addUnnamedConstant(phixverstr, T_string),true,T_string)
            else
dbg = symtab[routineNo]
                opcode = symtab[routineNo][S_il]
                PushOp(opcode,BltinOp)
            end if
-- Added 24/3:
--      elsif rtnNo=T_floor and routineNo!=T_floor then
        elsif rtnNo=T_floor and routineNo=T_sqfloor_div then
            -- assume an inner sq_floor_div happened
            if just_sq_floor_div_d!=1 then ?9/0 end if
            just_sq_floor_div_d = 0
        else -- routineNo>T_bin
            R = routineNo-1                     -- result variable
            isLiteral = 0
            if routineNo=T_routine then
--trace(1)
                ftype = T_integer
--              if Q_Routine then
                if Q_Routine>T_object then
--dbg = symtab[T_routine]
--dbg = builtinsReferenced
                    -- DEV: 30/8: we now mark builtinsReferenced above; it it were a count
                    --            not a flag, the include could be avoided?
                    R = 0
                    if emitON then
--                      Q_Routine = addRoutineId(Q_Routine)
                        R = addRoutineId(Q_Routine)
--                  else
--                      Q_Routine = 0
                    end if
--                  PushFactor(Q_Routine,true,T_integer)
--                  return
                    isLiteral = 1
--                  isLiteral = 0
--              else
                elsif emitON then
                    if isForward and not wasUsed then
                        symtab[routineNo][S_State] = state+S_used
                        builtinsReferenced = 1
--                      unresolved_routine_ids = 1
                    end if
                    unresolved_routine_ids[fileno] = 1
--                  PushFactor(R,false,T_integer)
--                  ftype = T_integer
                end if
            elsif routineNo<=T_ASeq then
                if    routineNo<=T_AInt then    ftype = T_integer
                elsif routineNo<=T_AAtm then    ftype = T_atom
                elsif routineNo<=T_AStr then    ftype = T_string
                else                            ftype = T_sequence                      end if
            elsif rtnNo<T_Bin then              ftype = T_sequence
--          else                                ftype = rootType(symtab[routineNo-1][S_ltype])
            else                                ftype = symtab[routineNo-1][S_ltype]
                if ftype>T_object then ftype = rootType(ftype) end if
            end if
            PushFactor(R,isLiteral,ftype)
--if newEmit then
--  if fromQU then
--      if ftype!=8 then puts(1,"ftype!8 line 4517 pmain.e\n") end if
--      if opsidx!=1 then puts(1,"opsidx!=1 line 4518 pmain.e\n") end if
--  end if
--end if
--if newEBP then
-- DEV: this might be overkill!! (it sure is!)
--          saveFunctionResultVars(opsidx,BLURGH!)
--end if
        end if -- routineNo>T_bin
    end if -- rType=FUNC|TYPE
    if routineNo>T_Ainc then
        if isForward then
            symtab[routineNo][S_sig] = rType&signature
        end if
        if NOLT=0 or bind or lint then
            if emitON
            and and_bits(thisEffect,E_vars) then
                ltCall(0,thisEffect,length(s5))
            end if
        end if -- NOLT
    end if
    routineNo = wasRoutineNo
    forward_call = wasFwd
    calltokcol = wascalltokcol
    calltokline = wascalltokline
end procedure

procedure clearIchain(integer saveIchain)
integer tmp
    while Ichain!=-1 do
        tmp = symtab[Ichain][S_Init]
        symtab[Ichain][S_Init] = 0
        Ichain = tmp
    end while
    Ichain = saveIchain
end procedure

--with trace
function backpatch(integer bplink, integer bpmin, integer mergeSet)
--
-- plant a label and point (some) previous jumps to it.
--
-- NB: Should NOT be called if nowt to backpatch!
--
integer s5len, bpnext

    s5 &= {opLabel,mergeSet,0,bplink}
    s5len = length(s5)
    while 1 do
--      s5[bplink-2] = mergeSet     -- no! do this in linkup
        if s5[bplink-2]!=mergeSet then ?9/0 end if
        s5[bplink-1] = s5len
        bpnext = s5[bplink]
--      if bpnext<=bpmin then exit end if
        if bpnext=bpmin then exit end if
        bplink = bpnext
    end while
    s5[bplink] = 0 -- remember to break the chain!

    if NOLT=0 or bind or lint then
        ltFlip(s5len-3)
    end if -- NOLT

    return bpmin
end function

function bprelink(integer oldchain, integer newchain, integer oldMergeSet, integer newMergeSet)
-- move exprBP or scBP ==> exitBP, breakBP, or ifBP.
-- scan down the new chain, replacing the mergeSet.
-- replace the 0 link at the end of the new chain with the oldchain.
-- return the newchain start as the start of the merged chains.
integer this, next
    if newchain=0 then ?9/0 end if  -- do not call in this case!
--  if newchain=0 then return oldchain end if   -- [DEV??]
    this = newchain
    while 1 do
        if s5[this-2]!=oldMergeSet then ?9/0 end if
        s5[this-2] = newMergeSet
        next = s5[this]
        if next=0 then exit end if
        this = next
    end while
    s5[this] = oldchain
    return newchain
end function


integer exprBP      -- expression/short circuit back patch link
        exprBP = 0

procedure DoIff()
--
-- Recognize and translate an "iff" construct
-- (note: this is a rudely hacked copy of DoIf, some names/comments might still reflect that)
--
integer emitElse, wasEmit

integer ifBP, saveIchain
integer EndIfBP = 0, tmp

sequence sytmp

--integer elsevalid -- initially 2, == first conditional test,
                    -- can 2=>3 for "if then return" handling,
                    -- then 1 until we find an "else",
                    -- then 0 to force "if else else" to error.

integer wasSideEffects
--integer plain, testfor
integer iftop, ctrlink, ctrltyp
--integer scode, wasEmit2
integer iffvar
integer wasExprBP,
        wasScBP

--added 18/5/16:
    if opTopIsOp then PopFactor() end if
    saveFunctionResultVars(opsidx,INTSTOO)

--29/1/19:
--  if exprBP!=0 then ?9/0 end if   -- [we may yet have a problem with "if iff() then" [or "while iff() do"], when enough and/or/() get involved] [DEV]
    wasExprBP = exprBP
    wasScBP = scBP
    exprBP = 0
    scBP = 0

    saveIchain = Ichain
    Ichain = -1
--  MatchString(T_iff)  or T_iif
    getToken()  -- T_iff/T_iif
    MatchChar('(',float_valid:=true)

--  elsevalid = 2

    ctrlink = 0
    if emitON then
        apnds5({opCtrl,IF,0,emitline})
        iftop = length(s5)-1    -- patched at/pointed to the end if
        ctrlink = iftop         -- where to point next elsif/else/endif
        if NOLT=0 or bind or lint then
            ltCtrl(iftop)
        end if -- NOLT
    end if

    wasSideEffects = SideEffects
    SideEffects = E_none
    wasEmit = emitON
    emitElse = emitON   -- (minor optimisation [cmp vs opJcc] 18/3/09)

    if exprBP!=0 then ?9/0 end if

    noofbranches = 0

    Expr(pAllops,false) -- full, notBool/asIs

    ifBP = 0
    if exprBP then
        ifBP = bprelink(ifBP,exprBP,exprMerge,ifMerge)
        exprBP = 0
    end if

--  MatchString(T_then)
    if toktype='?' then
        MatchChar('?',float_valid:=true)
    else
        MatchChar(',',float_valid:=true)
    end if

    emitElse = emitON
    tmp = opstack[1]
    if tmp>0 and scBP=0 and ifBP=0 then
        if (opsidx=1 and not opTopIsOp)
        or (opsidx=2 and opstack[2]=opNot) then
            sytmp = symtab[tmp]
            if sytmp[S_NTyp]=S_Const
            and sytmp[S_Init]
            and sytmp[S_vtype]=T_integer then
                tmp = sytmp[S_value]
                if (opsidx=1 and tmp!=0)
                or (opsidx=2 and tmp=0) then
                    emitElse = 0
                else
                    emitON = 0
                end if
                opTopIsOp = 0
                opsidx = 0
            end if
        end if
    end if
    --
    -- jump_not to elsif/else/endif, eg
    -- if x then => Jnot <endif> x, or
    -- if not x then => Jif <endif> x, or
    -- if a<b => Jge <endif> a,b
    --
    if opsidx then
        ifBP = Branch(Invert,emitElse,ifMerge,ifBP)
    end if

    -- patch short circuits (eg if a or b then -> jump after a to if-block)
    if scBP>0 then
        scBP = backpatch(scBP,0,scMerge)
        if scBP then ?9/0 end if
    end if

    oktoinit = 1    -- just done in Statement!

    iffvar = newTempVar(T_object,Shared)

--DEV (untried, see pEmit2.e bool doit)
--  Expr(0,0)
    Expr(pAllops,asBool)

    RHStype = T_object
    if not opTopIsOp and opsidx=1 then
        RHStype = opstype[1]
    end if
    StoreVar(iffvar,RHStype)

    clearIchain(-1)
    if exprBP!=0 then ?9/0 end if

--  if toktype!=LETTER then exit end if
--      if ttidx=T_elsif then
--          ctrltyp = ELSIF
--      else
--      if ttidx!=T_else then exit end if
        ctrltyp = ELSE
--      end if
    if emitline<tokline then
        emitline = tokline-1
    end if
--  elsevalid = 0
--      MatchString(ttidx)
    if toktype=':' then
        MatchChar(':',float_valid:=true)
    else
        MatchChar(',',float_valid:=true)
    end if

    emitON = (emitON and emitElse)
    if emitON then
        apnds5({opJmp,endIfMerge,0,EndIfBP})
        EndIfBP = length(s5)
    end if
    -- patch previous if condition jumps to this elsif test [DEV?]
    if ifBP>0 then
        ifBP = backpatch(ifBP,0,ifMerge)
        if ifBP then ?9/0 end if
    end if
    if emitON then
        s5 &= {opCtrl,ctrltyp,ctrlink,emitline}
        ctrlink = length(s5)-1
        if NOLT=0 or bind or lint then
            ltCtrl(ctrlink)
        end if -- NOLT
    end if

--      emitline = line
    if allWhiteToTokcol() then
        emitline = line-1
    else
        emitline = line
    end if
    emitON = (wasEmit and emitElse)
    if exprBP!=0 then ?9/0 end if
    oktoinit = 0

--DEV (untried, see pEmit2.e bool doit)
--  Expr(0,0)
    Expr(pAllops,asBool)

    RHStype = T_object
    if not opTopIsOp and opsidx=1 then
        RHStype = opstype[1]
    end if
    StoreVar(iffvar,RHStype)

    clearIchain(-1)
    if exprBP!=0 then ?9/0 end if

    -- patch any remaining if/elsif (and no else) jumps to s5len (end if)
    -- patch the jumps before elsif and else statements to s5len (end if)

    if ifBP>0 then
        ifBP = backpatch(ifBP,0,ifMerge)
        if ifBP then ?9/0 end if
    end if
    if EndIfBP>0 then
        EndIfBP = backpatch(EndIfBP,0,endIfMerge)
        if EndIfBP then ?9/0 end if
    end if
    if ctrlink then
        s5 &= {opCtrl,END+IF,ctrlink,emitline}
        ctrlink = length(s5)-1
        s5[iftop] = ctrlink
        if NOLT=0 or bind or lint then
            ltCtrl(ctrlink)
        end if -- NOLT
    end if

--  MatchString(T_end)
--  MatchString(T_if)
    MatchChar(')')
    emitON = wasEmit
    if exprBP!=0 then ?9/0 end if

    clearIchain(saveIchain)

    SideEffects = or_bits(SideEffects,wasSideEffects)

--DEV merge RHStypes:
    PushFactor(iffvar,false,T_object)

--29/1/19:
    if exprBP!=0 then ?9/0 end if
    if scBP!=0 then ?9/0 end if
    exprBP = wasExprBP
    scBP = wasScBP

end procedure

procedure ForwardProc(integer isFunc)
--
-- Handle implicit forward definitions (first instance).
-- Called when an unrecognised token is followed by '('.
-- May be function or procedure depending on context.
--
integer N, stype
sequence sig

-- verify compiler gets this right:
--/**/  #isginfo{sig,0b0100,MIN,MAX,integer,1}

    if lint then
        Warn("implicit forward call",tokline,tokcol,SQ_WARN)
    end if

    if isFunc=FUNC then
--DEV if emitON...
        N = newTempVar(T_object,FuncRes)    -- allocate return var
        stype = S_Func
    else
        stype = S_Proc
    end if
    sig = {isFunc}

--DEV 20/9: triggered in global constant edita=xl("edita")
--  if isGlobal then ?9/0 end if

    N = addSymEntry(ttidx,0,stype,sig,0,S_fwd+S_used)
    --                    ^asGlobal of false

    symtab[N][S_Efct] = E_all   -- must assume the worst

    Call(N,sig,isFunc,false)

end procedure


--17/6/16:
constant NO_BREAK = #01
--06/9/16:
--constant DLL_MAIN = #02

--integer fdwReasonttidx

--procedure get_fdwReason()
--  if toktype!=LETTER
--  or ttidx!=fdwReasonttidx then
--      Aborp("fdwReason expected")
--  end if
--  getToken()
--end procedure

--integer rBlock
forward procedure Block(integer flags=0)

constant R_Proc = 1, 
         R_Func = 2, 
         R_Type = 3

constant S_Map = {S_Proc,S_Func,S_Type},
         R_Map = "PFT",
         E_Map = {"procedure","function","type"}

integer CheckForFunctionReturn  -- if 1, there was a return statement somewhere.

--with trace

include pilasm.e    -- ilasm() and label_fixup()
--include pilasm2.e -- ilASM() and label_FIXUP()

constant T_topset = {T_proc,T_func,T_type,T_constant,T_global,T_public,T_export,
                     T_include,T_with,T_without,T_forward,T_namespace,T_enum,
--                   T_include,T_with,T_without,T_forward,T_enum,
                     T_format,T_end,T_override,T_struct,T_class,T_abstract},
--                   T_end,T_elsifdef,T_elsedef}
         Tglobal = {T_global,T_public,T_export},
         Tstruct = {T_struct,T_class,T_abstract}

--integer tracefor
--      tracefor=0
--integer traceif
--      traceif=0
--integer dFlag
--      dFlag=0

integer r_lambda
--string r_routine_name
bool bFromStruct = false

--11/07/20:
forward procedure DoConstant()
bool just_static = false
forward procedure TopDecls(integer AllowOnDeclaration)

with trace
--25/11/19:
--procedure DoRoutineDef(integer Rtype)
procedure DoRoutineDef(integer Rtype, bool bLambda=false, sequence thissig={})
--
-- Parse and translate a procedure, function, or type declaration
--
integer rtnttidx
integer N, pN
--, rtn, cidx
integer wasGlobal
integer fwd
--sequence thissig
integer Stype, Ktype

object bcptr

integer rtntokcol, rtntokline   -- saved position of "fred" in procedure fred(object x)
                                -- ("fred" is not added until the ")")
--integer   wastokcol, wastokline   -- saved position of ")" in procedure fred(object x)
integer wastokline  -- saved position of ")" in procedure fred(object x)

integer tmp, saveIchain, pTi, pCol, act, state, killUsed
object fCheck, pDef
integer pidx
--, phit
--sequence bj, bj1, bjz, pdone

integer tvarstart, pfirst, plast
sequence wasoptset
integer savettidx

integer exported = 0

integer wasreturnvar = returnvar    -- (NESTEDFUNC)

--sequence bi
--object dbg

-- verify compiler gets this right:
--DEV 28/9/9: (was getting sequence not T_Dsq)
--!/**/ #isginfo{thissig,0b0100,MIN,MAX,integer,-2}
--/**/  #isginfo{thissig,0b1100,MIN,MAX,object,-2}  --DEV (broken)

    wasGlobal = isGlobal
    isGlobal = 0
    wasoptset = optset
    saveIchain = Ichain
    Ichain = -1

--if fileno=92 and tokline>=818 then
--  trace(1)
--end if
    MatchString(T_topset[Rtype])

    fwd = 0
    rtntokcol = tokcol
    rtntokline = tokline
if bLambda then
--  if Rtype!=R_Func then Aborp("uh") end if
    if Rtype!=R_Func and Rtype!=R_Proc then Aborp("uh") end if
    if wasGlobal then Aborp("uh") end if
    N = 0
    rtnttidx = -1
    --DEV ...++?
else
--15/01/19 (fudge...)
--  if toktype!=LETTER then
    if toktype!=LETTER or ttidx=T_end then
        Aborp("a name is expected here")
    end if

    rtnttidx = ttidx
--  r_routine_name = ??     -- for DoStruct()
--?rtnttidx
--if ttidx=T_end then trace(1) end if

    if NOLT=0 or bind or lint then
        ltclear(0)  -- (hanging onto file-level localtypes over a 
                --  routine definition not deemed worthwhile)
                -- [erm, actually we do hang onto said during
                --  the gscan phase, just not during parsing]
        -- (btw we call ltclear again below once currRtn is set)
        -- (DEV only necessary when pltype diag is on.. minor tho)
    end if -- NOLT

--  rtntokcol = tokcol
--  rtntokline = tokline

--if rtnttidx=T_dump_listing then tracefor=1 end if
--if rtnttidx=T_f then traceif=1 end if
    N = InTable(InTop)
    if N then
        if N<=T_Asm then Aborp("builtin overrides are not permitted in Phix\n") end if
        state = symtab[N][S_State]
        if and_bits(state,S_fwd) then
            Stype = symtab[N][S_NTyp]
--          if Stype!=S_Map[Rtype] then
            if (Stype=S_Proc)!=(Rtype=R_Proc) then
                Aborp("previously declared/invoked as a "&E_Map[find(Stype,S_Map)])
            end if
            fwd = 1
--added 26/12/2012:
            if wasGlobal and not and_bits(state,K_gbl) then
                --
                -- Note: I am not distinguishing between the implicit
                --          Proc()
                --  ie when the compiler assumes local not global, and
                --          forward procedure Proc()
                --  ie when, by omission, we explicitly state local.
                -- Technically the latter is an error, since the actual
                --  is global (we just tested wasGlobal), but I make no 
                --  attempt to detect or report it as such.
                --
                ReLinkAsGlobal(rtnttidx,N)
            end if
--21/01/2021
--      elsif symtab[N][S_NTyp]=S_Rsvd then
        elsif N<=T_Asm or symtab[N][S_NTyp]=S_Rsvd then
--          if (rType!=FUNC or and_bits(symtab[N][S_State],K_fun)!=K_fun) then
            Aborp("illegal use of a reserved word")
        else
            Duplicate()
        end if
    elsif wasGlobal then
-- 28/4/11
--      N = InTable(InAny)
        N = InTable(-InAny) -- no errors (namespace rqd etc)
--if N=325 then trace(1) end if
        if N then
            if N<=T_Asm then Aborp("builtin overrides are not permitted in Phix\n") end if
--dbg = symtab[N]
            state = symtab[N][S_State]
            if and_bits(state,S_fwd) then
--DEV: set up a warning limit for symtab reference.
-- 2/6/15:
--if newEmit and N<=T_Asm then
--              Warn("builtin overidden\n",tokline,tokcol,0)
--              N = 0   -- define a new one then...
--else
                if symtab[N][S_FPno]!=fileno then
                    if N>T_Ainc then
                        -- ie/eg constant monthlen = {31,28+isLeapYear,31,30...
                        --       global function getDays(integer month)
                        --          return monthlen[month]
                        --                 ^ monthlen has not been assigned a value!!
                        -- (I only give this warning for externally forward  
                        --  referenced routines; if the call and definition 
                        --  are in the same source file you get exactly the 
                        --  same error, but it is clearer what is going on,
                        --  and you can just move the constant higher up.)
                        -- DEV nb external fwd refs not fully tested yet..
                        --DEV2: of course:
                        --    someotherproc()
                        --    ?getDays()/include of/definition of getDays
                        --    procedure someotherproc() ?getDays()
                        --  *will* trigger the unassigned error, *without*
                        --  this warning being emitted (test that theory...)
                        Warn("external forward reference; initialisation code may be skipped\n",tokline,tokcol,0)
                    end if
                    symtab[N][S_FPno] = fileno
                    -- added 23/08/13 for intellilink:
                    symtab[N][S_ErrR] = rtntokcol
                end if
                fwd = 1
                if not optset[OptDebug] and and_bits(state,K_wdb) then
                    --
                    -- assume: psym.e just said (eg)
                    --  initialAutoEntry("printf",S_Proc,"PIPO","pprntf.e")
                    -- which creates a fwd symtab entry with normal debug,
                    -- but pprntf.e said "without debug", so we must patch
                    -- out the debug flag, ie hide stuff from pdiag/pdebug.
                    -- (ie/eg we want "error in format string" to occur on
                    --  the printf statement, /not/ line 298 of pprntf.e,
                    --  and/or we do not want to trace into printf calls.)
                    --
                    state -= K_wdb
                    symtab[N][S_State] = state
                end if
--end if
--21/01/21
            elsif symtab[N][S_NTyp]=S_Rsvd
--          elsif (N<=T_Asm or symtab[N][S_NTyp]=S_Rsvd)
              and (Rtype!=R_Func or and_bits(symtab[N][S_State],K_fun)!=K_fun) then
                Aborp("illegal use of a reserved word")
            else
                N = 0   -- define a new one then...
            end if
        end if
    end if

    getToken()
end if
if newEBP then
--  TIDX = 1
    LIDX = 1
end if
    MatchChar('(')
    thissig = Params(thissig)
if bFromStruct then
--?{"DoRoutineDef line 5762","bFromStruct",bFromStruct,"Ch",Ch}
    if Ch=';' then
        -- virtual function definition (signalled by the ';').
        MatchChar(')')
        r_lambda = 0
        return
    end if
end if
--if bLambda then
--  ?{"nParams",nParams}
--  ?paramNames
--  ?paramTypes
--  ?thissig
--end if
    if toktype=LETTER then
        Aborp("a type is expected here")
    end if
    wastokline = tokline
--2/7/13:
--  if toktype!=')' then Expected("\")\"") end if   -- validate but get next later...
if intellisense!=3 then --23/08/13
    MatchChar(')')

    if    ttidx=T_with    then DoWithOptions(WITH,FROMROUTINE)
    elsif ttidx=T_without then DoWithOptions(WITHOUT,FROMROUTINE)
    end if
    savettidx = ttidx
end if

-- 21/6/10:
    if fwd then
        fCheck = symtab[N][S_ltab]
        if sequence(fCheck)
        and length(fCheck)=1 then
            fCheck = fCheck[1]
            if sequence(fCheck)
            and (not equal(fCheck[1],nParams) or
                 not equal(fCheck[2][1..nParams],paramNames[1..nParams]) or
                 not equal(fCheck[3][1..nParams],paramTypes[1..nParams]) or
                 not equal(fCheck[4][1..nParams],paramDflts[1..nParams])) then
                tokcol = rtntokcol
                tokline = rtntokline
                Aborp("does not match forward definition")
            end if
        end if
    end if

--if equal(routineName,"opCode") then trace(1) end if
--if equal(routineName,"dump_listing") then dFlag=1 end if

    returnvar = 0
    returntype = 0
    Ktype = K_othr
    if Rtype=R_Type then    -- ie processing a "type xxx(...) ... end type" declaration
        if nParams!=1 then
            tokcol = rtntokcol
            tokline = rtntokline
            Aborp("user defined types must have exactly one parameter")
        end if
        returnint = 1
        Ktype = K_type
    end if

    killUsed = 0
    if fwd then
        if Rtype=R_Func or Rtype=R_Type then
            returnvar = N-1
        end if
if newEBP then
--DEV 13/12/2011:
--      symtab[N][S_Parm1] = LIDX-1
        symtab[N][S_Parm1] = 0
else
        symtab[N][S_Parm1] = TIDX-1
end if
--      symtab[N][S_FPno] = fileno  -- ??
    else
        if Rtype=R_Func or Rtype=R_Type then
--24/8/18:
--          returnvar = newTempVar(T_object,FuncRes)
            if Rtype=R_Func then
                returnvar = newTempVar(T_object,FuncRes)
            elsif Rtype=R_Type then
                returnvar = newTempVar(T_integer,FuncRes)
            end if
if newEBP then
--          TIDX = 1
            LIDX = 1
end if
        end if
        Stype = S_Map[Rtype]
        state = iff(bLambda?S_used:0)
        N = addSymEntryAt(rtnttidx,wasGlobal,Stype,0,0,state,rtntokcol)
-- 4/10/2020:
if rtnttidx = T_Icallback then
    N_Icallback = N
elsif rtnttidx = T_Icallbacki then
    N_Icallbacki = N
end if  
        if wasGlobal=2 and fileno=1 and DLL=1 then 
            if Stype=S_Proc then
                Aborp("export procedures are not supported")
            end if
--DEV
--          ?Aborp("export routine parameters must be atom")
--              from pcfunc.e:
--              if and_bits(sigi,T_atom)=0 then
--                  fatalN(2,e75cbrpmaba) -- call back routine parameters must all be atoms
--              end if
--              noofparams = si[S_ParmN]
--              if noofparams!=length(sig)-1 then
--                  fatalN(2,e16cbchop) -- call_backs cannot have optional parameters
--              end if
--              SUG: should we check the return type for non-atom as well? (warning)
--          ?Aborp("export routines must return an atom")
            if rtnttidx=T_DLLMAIN then
                exports = prepend(exports,N)
                exportaddrs = prepend(exportaddrs,0)
            else
                exports = append(exports,N)
                exportaddrs = append(exportaddrs,0)
            end if
            exported = 1
        end if
--1/12/19:
--      killUsed = not wasGlobal
        killUsed = not wasGlobal and not bLambda
--      killUsed = ((not wasGlobal) or lint)    -- maybe more annoying than useful?
    end if

    if fwd then
        bcptr = symtab[N][S_il]
        if sequence(bcptr) then
            if no_of_fwd_calls>0 then
                no_of_fwd_calls -= length(bcptr)
            end if
        end if
    end if
    symtab[N][S_1stl] = rtntokline
--restoreScope(restScope)
--NESTEDFUNC
    sequence restScope = {}
    if scopetypes[scopelevel]=S_Rtn then
        restScope = hideScope()
    end if
    if increaseScope(S_Rtn,-1) then end if

--11/07/20: (routine-level static and constant vars)
    if ttidx=T_constant or ttidx=T_static then
        while true do
            if ttidx=T_constant then
                DoConstant()
            elsif ttidx=T_static then
                getToken()
                tokno = InTable(InAny)
                if tokno=0 or symtab[tokno][S_NTyp]!=S_Type then
                    Aborp("a type is expected here")
                end if
                just_static = true
                TopDecls(1)
                just_static = false
            else
                exit
            end if
        end while
        savettidx = ttidx
--erm...
--      emitline = lastline
        wastokline = tokline
    end if

--DEV may not be rqd under newEBP?
    tvarstart = symlimit+1

    currRtn = N

    if NOLT=0 or bind or lint then
            ltclear(N) -- (see above)
    end if -- NOLT

    --  optionalParameterExpression=1
--trace(1)
    for i=1 to nParams do
        ttidx = paramNames[i]
        pTi = paramTypes[i]
        pCol = paramCols[i]
        pDef = paramDflts[i]
        if sequence(pDef) then
            -- length(x) ==> -var_no(x), command_line() ==> -T_command_line
            if length(pDef)=2 then
                if pDef[2]<0 then
--                  pDef[2] = paramNames[-pDef[2]]
--                  pDef = -paramNames[-pDef[2]]
--                  pDef = -paramTypes[-pDef[2]]    -- holds symidx by now
                    pDef = -paramCols[-pDef[2]] -- holds symidx by now
                else
                    pDef = -pDef[2]
                end if
--?             pTi = T_integer
            elsif pDef={T_routine} then
                Ktype = or_bits(Ktype,K_drid)
                pDef = 0
            else
                pDef = -T_command_line
--?             pTi = T_Dsq
            end if
        elsif pDef<0 then
--          pDef = paramNames[-pDef]
            pDef = paramCols[-pDef]
--?!!       pTi = paramTypes[i]
        end if
        if pDef then
--DEV (untried)
--          if pDef and pDef!=2 then
            -- set the "this parameter has a default" bit
            Ktype = or_bits(Ktype,K_dlft)
            paramDflts[i] = pDef
        end if
        --4/3/10:
--      if fwd then
--      if fwd and i<length(symtab[N][S_sig]) then
        if 0 then
-- (removed again 26/6/10)
--trace(1)
            act = symtab[N][S_sig][i+1]
            opsidx += 1     -- for Abork()
            opstcol[opsidx] = pCol
            forward_call = 0
            routineNo = N
            tmp = plausible(pTi,act)
--          tmp = plausible(pTi,act,0)
            routineNo = 0
            opsidx -= 1
        end if
        if paramUsed[i] then
            Ktype += S_used
        end if
        pN = addSymEntryAt(ttidx,0,S_TVar,pTi,0,Ktype,pCol)
        if and_bits(Ktype,K_drid) then
            Ktype -= K_drid
        end if
        if and_bits(Ktype,S_used) then
            Ktype -= S_used
        end if
--      paramNames[i] = pN
        --
        -- In eg p(object a, b=a), paramDflts[2] is -1, so when
        --  i=1 save pN for use when i=2 (see above).
        --
--      paramTypes[i] = pN
        paramCols[i] = pN
--DEV pN==symlimit, surely...
--if pN!=symlimit then ?9/0 end if
        symtab[pN][S_Init] = 1
        tmp = symtab[pN][S_State]
        tmp = or_bits(tmp,S_set+K_used)
        symtab[pN][S_State] = tmp
        tmp = optset[OptTypeCheck]
        if not tmp then
            if pTi>T_object then pTi = rootType(pTi) end if
        end if
        if pTi=T_object then
            tmp = 0
        end if
        if tmp or pDef then
            emitline = wastokline
            if emitON then
                pidx = pDef -- should be integer by here...
if newEmit then
                agcheckop(opTchk)
end if
                apnds5({opTchk,pN,tmp,pidx})
            end if
        end if
    end for

    -- backpatch any forward calls
    if fwd then
--if 1 then
        BackPatchForwardCalls(N,nParams,paramCols,paramNames)
--else
--      bcptr = symtab[N][S_il]
--      if sequence(bcptr) then
--          if length(bcptr)=0 then
--              if nParams!=0 then
--                  pN = paramCols[1]
--                  if not and_bits(symtab[pN][S_State],K_dlft) then
--                      tokcol = rtntokcol
--                      tokline = rtntokline
--                      Aborp("routine previously called with no parameters")
--                  end if
--              end if
--          else
--              for j=1 to length(bcptr) do
--                  bj = bcptr[j]   -- (all params for one forward call)
--                  bj1 = bj[1]     -- {tokcol, fileno, routineNo} of the call statement
--                  rtn = bj1[3]
--                  pdone = {}
--                  for z=2 to length(bj) do
--                      bjz = bj[z] -- {tokcol, offset[, ttidx]} for each parameter
--                      if length(bjz)=2 then   -- normal/numbered parameter
--                          pidx = z-1
--                      else                    -- named parameters
--                          if length(pdone)=0 then
--                              pdone = repeat(1,z-2)
--                          end if
--                          pidx = find(bjz[3],paramNames)
--                          for addslots=length(pdone)+1 to pidx do
--                              pdone &= 0
--                          end for
--                          if pidx=0
--                          or pdone[pidx] then
--                              fileno = bj1[2]
----                                tokcol = bjz[1]     -- \ see notes
--                              tokcol = bjz[4]     -- / (top of this file)
--                              no_oops = 1
--                              if pidx=0 then
--                                  Aborp("no such parameter")
--                              end if
--                              Aborp("duplicate parameter")
--                          end if
--                          pdone[pidx] = 1
--                      end if
--                      cidx = bjz[2]
--                      if symtab[rtn][S_il][cidx]!=-9 then ?9/0 end if
--                      if pidx>nParams then
--                          fileno = bj1[2]
--                          tokcol = bjz[1]
--                          no_oops = 1
--                          Aborp("too many parameters")
--                      end if
--                      pN = paramCols[pidx]
--                      symtab[rtn][S_il][cidx] = pN
--                  end for -- z=2 to length(bj)
--                  if length(pdone)=0 then
--                      if pidx<nParams then
--                          pidx += 1
--                      else
--                          pidx = 0
--                      end if
--                  else
--                      pidx = find(0,pdone)
--                  end if
--                  if pidx then
--                      pN = paramCols[pidx]
--                      if not and_bits(symtab[pN][S_State],K_dlft) then
--                          fileno = bj1[2]
--                          tokcol = bj1[1]
--                          no_oops = 1
--                          Aborp("missing non-defaulted parameter ["&getname(symtab[pN][S_Name],-2)&"]")
--                      end if
--                  end if
--              end for -- i=1 to length(bcptr)
--          end if -- length(bcptr)=/!=0
--      end if -- sequence(bcptr)
--end if
    end if -- fwd

    -- store frame info:
--trace(1)
    pfirst = 0
--DEV rename / do this with TIDX/LIDX?
if newEBP then
    plast = 0
else
    plast = -1
end if
    for i=tvarstart to symlimit do
        if symtab[i][S_NTyp]=S_TVar then
            if not pfirst then
                pfirst = i
            end if
            tmp = symtab[i][S_State]
            if and_bits(tmp,K_dlft) then exit end if
if newEBP then
            plast += 1
else
            plast = i
end if
        end if
    end for
    symtab[N][S_Parm1] = pfirst
    symtab[N][S_ParmN] = plast

--DEV sig must be a dword-sequence, not a string[?]:
--  symtab[N][S_sig] = R_Map[Rtype]&thissig
    thissig = -1&thissig
    thissig[1] = R_Map[Rtype]
    symtab[N][S_sig] = thissig

-- 2/7/13:
if intellisense=3 then --23/08/13
    MatchChar(')')
    if    ttidx=T_with    then DoWithOptions(WITH,FROMROUTINE)
    elsif ttidx=T_without then DoWithOptions(WITHOUT,FROMROUTINE)
    end if
else
    ttidx = savettidx
end if

--DEV may need to be after Block() for side effects?
    if fwd then
        symtab[N][S_State] -= S_fwd
    end if

--9/2/16: moved above Locals()
    SideEffects = E_none

    Locals(1)
--DEV can use returntype?
    CheckForFunctionReturn = 0

--  SideEffects = symtab[N][S_Efct]     -- pick up S_Ainc initial values, why not.
--  SideEffects = E_none
    symtab[N][S_Efct] = E_all   -- recursive calls must assume the worst

--  call_proc(rBlock,{})
--  if exported and rtnttidx=T_DLLMAIN then
--      if toktype!=LETTER
--      and not find(ttidx,{T_if, T_switch}) then
--          Aborp("DllMain: if or select expected")
--      end if
--      if nParams!=3 then
--          Aborp("DllMain: requires 3 parameters")
--      end if
--      fdwReasonttidx = paramNames[2]
--      Block(DLL_MAIN)
--  else
        Block()
--  end if

    symtab[N][S_Efct] = SideEffects

--  if fwd then
--      symtab[N][S_State] -= S_fwd
--  end if
--  if length(lblchain) then

    label_fixup()

if NESTEDFUNC then
    returnvar = wasreturnvar
else -- (old code)
    returnvar = -1
end if
    returnint = 0

    if not CheckForFunctionReturn then
        if Rtype = R_Func then
            Aborp("function does not return a value")
        elsif Rtype = R_Type then
            Aborp("type does not return a true/false value")
        end if
    end if
-- 2/9/14:
--  emitline = line-1
    emitline = line
--  if emitline>rtntokline then
    if emitline>lastline then
        emitline -= 1
    end if
    if Rtype=R_Proc then
--DEV surely tokline matches the "end" of "end procedure"...
--DEV surely opBadRetf deserves similar...
--28/8/14: (it looks better if the opRetf is inside the end procedure, and moved above as suggested)
--      emitline = line     -- this will match "procedure" of "end procedure".

if newEmit then
        agcheckop(opRetf)
end if
        apnds5(opRetf)

    else
        symtab[N-1][S_vtype] = returntype
--DEV opLtyp??
        symtab[N-1][S_ltype] = returntype

if newEmit then
        agcheckop(opBadRetf)
end if
        apnds5(opBadRetf)

    end if

    clearIchain(saveIchain)

    currRtn = dropScope(N,S_Rtn)
    restoreScope(restScope)

--if fileno = 1 then trace(1) end if
    MatchString(T_end)
--if no_tmap then --DEV following test has been spannered by changed meaning of [S_Parm1]
--                      a) count tvars below, b) write a test routine with 2000+ locals....
--else                  c) check for corruption of 1st/last and memory leaks etc.
--      update 7/1/9:   d) surely this is now just "if plast-pfirst>2034 then"...
--  if -TIDX+symtab[N][S_Parm1]>2034 then   --DEV actual number probably out of date
--      Aborp("internal limit of 2034 stack entries exceeded; split routine into smaller chunks\n")
--  end if
--end if
    MatchString(T_topset[Rtype])

    if SideEffects=E_none and Rtype=R_Proc then
        --
        --  Either a) the procedure really doesn't do anything, does
        --              not modify any vars or display a message etc,
        --              so there really is no point calling it,
        --      or b) we have missed some side effect, ie it is a
        --              bug in the compiler.
        --  We could live without the warning for point a, but point b
        --   makes it a good idea to leave it in.
        --
        Warn("procedure has no side effects",rtntokline,rtntokcol,0)
    end if

--trace(1)
    pfirst = 0
--DEV rename / do this with TIDX/LIDX?
    plast = 0
    for i=tvarstart to symlimit do
        if symtab[i][S_NTyp]=S_TVar then
            if pfirst=0 then
                pfirst = i
                symtab[N][S_Parm1] = pfirst
            else
                symtab[pfirst][S_Slink] = i
                pfirst = i
            end if
            plast += 1
        end if
    end for
    symtab[N][S_Ltot] = plast
    if pfirst then
        symtab[pfirst][S_Slink] = 0
    end if
    if killUsed then
        state = symtab[N][S_State]
        if and_bits(state,S_used) then
            -- avoid leaving recursive routines marked as used.
            state -= S_used
            symtab[N][S_State] = state
        end if
    end if

    if NOLT=0 or bind or lint then
        ltclear(-N)
    end if -- NOLT

--if fileno=92 and tokline>=818 then
--  trace(1)
--end if

    if exported then
--?N
        Or_K_ridt(N, S_used+K_ridt)
--      Or_K_ridt(N, S_used+K_used+K_ridt)  -- no help
        {} = addRoutineId(N)
--?symtab[N]
--iNNN = N
    end if

    optset = wasoptset
--  if bLambda then r_lambda = N end if
    r_lambda = N    -- (also used in DoStruct)

end procedure

--with trace
procedure DoForwardDef()
--
-- handle forward [global] routine(...) [no body and no end routine]
-- and/or [global] forward routine(...) [            "			   ]
--
-- A forward definition must exactly match the actual definition, including defaults.
--  For example,
--      forward function f(string s="true")
--      ...
--      function f(sequence t="false")
--  should produce a compilation error, for three separate reasons.
--
integer sig1
integer rType, rtnttidx
integer N
integer wasGlobal
sequence sig

    MatchString(T_forward)
    if not isGlobal then    -- allow "global forward" === "forward global" but not "global forward global"!
--      if toktype=LETTER and ttidx=T_global then
        if toktype=LETTER and (ttidx=T_global or ttidx=T_public or ttidx=T_export) then
            isGlobal = 1
            getToken()
        end if
    end if
    wasGlobal = isGlobal
    isGlobal = 0
    N = 0
    if toktype=LETTER then
        N = find(ttidx,{T_proc,T_func,T_type})
    end if
    if N=0 then
        Expected("\"procedure\" or \"function\"") 
    end if
    sig1 = PFT[N]
    rType = S_Map[N]
    getToken()
    if toktype!=LETTER then
        Aborp("a name is expected here")
    end if
    N = InTable(InTop)
    -- btw, you /cannot/ code stuff like:
    --  fwdrtn(...)                             -- implicit fwd call, then
    --  [global] forward routine fwdrtn(...)    -- explicit fwd definition
    -- (obviously, it must be either implicit or explicit, not both!)
    if N then Duplicate() end if
    N = InTable(InAny)
    if N>0 then
--21/01/2021
--      if symtab[N][S_NTyp]=S_Rsvd then
        if N<=T_Asm or symtab[N][S_NTyp]=S_Rsvd then
--      and (rType!=FUNC or and_bits(symtab[N][S_State],K_fun)!=K_fun) then
            Aborp("illegal use of a reserved word")
        end if
    end if

    if rType=S_Func then
        N = newTempVar(T_object,FuncRes)
    elsif rType=S_Type then
        N = newTempVar(T_integer,FuncRes) -- allocate return var
    end if

    rtnttidx = ttidx
    N = addSymEntry(ttidx,wasGlobal,rType,0,0,S_fwd)

--if rType = S_Type then
--  printf(1,"DoForwardDef(%d)\n",{N})
--end if

    symtab[N][S_Efct] = E_all   -- must assume the worst

    getToken()
    MatchChar('(')

    if increaseScope(S_Rtn,-1) then end if

    forward_call = S_fwd
    sig = sig1&Params()

    symtab[N][S_sig] = sig
    forward_call = 0

    -- verify these when we get to the actual definition (see/search for fCheck):
    symtab[N][S_ltab] = {{nParams, paramNames, paramTypes, paramDflts}}

    MatchChar(')')

    currRtn = dropScope(N,S_Rtn)

    symtab[N][S_il] = 0     -- important!

end procedure

bool fromsubss = false

--with trace
procedure DoSubScripts()
-- called from GetFactor() [only] [effectively/partially implements pSubsc precedence]
    integer wasMapEndToMinusOne,
            noofsubscripts,
            N = opstack[opsidx],
            etype = opstype[opsidx]
    bool wasdot = false,
         wasfromsubss = fromsubss,
         bStruct = false

    if N!=0 and opsltrl[opsidx]=0 then
--31/1/21:
--      integer stype = symtab[N][S_ltype]
        integer stype = symtab[N][S_vtype]
        if find(stype,stids) then
            bStruct = true
            etype = stype
        end if
    end if

--DEV/SUG test/enhance for <string>[i][j]... (just a thought in passing)
    if not bStruct and not and_bits(etype,T_sequence) then
        Aborp("attempt to subscript an atom")
    end if

    wasMapEndToMinusOne=mapEndToMinusOne
    noofsubscripts = 1
    while 1 do
        mapEndToMinusOne = 1
        if ORAC and toktype='.' then
            MatchChar('.',float_valid:=false)
            wasdot = true
            if bStruct then
                if toktype!=LETTER then
                    Aborp("invalid")
                end if
            else
                fromsubss = true
                GetFactor(false)
                fromsubss = wasfromsubss
            end if
        else
            MatchChar('[',float_valid:=true)
            wasdot = false
            Expr(pAllops, asBool)
        end if
        if bStruct then
            if opTopIsOp then ?9/0 end if
            integer sv,st
            string field_name
            if wasdot then
                sv = opstack[opsidx]
                st = opstype[opsidx]
                field_name = text[tokcol..col-1]
                integer fN = addUnnamedConstant(field_name,T_string)
                PushFactor(fN,true,T_string)            -- (field_name)
            else
                bool const = opsltrl[opsidx]
                if const and opstype[opsidx]!=T_string then
                    Aborp("invalid (string field name expected)")
                end if
            end if
            -- ? (varno/struct already on the stack from GetFactor)
            PushFactor(class_def,true,T_integer)    -- (context)
            Call(T_fetch_field,{FUNC,T_sequence,T_string,T_integer},FUNC,true)
            
            if get_struct_type(etype)=S_CFFI then
                etype = T_atom
                bStruct = false
                new_struct = 0
            else
                if wasdot then
                    etype = structs:get_field_type(etype,field_name)
                    if etype=T_integer and Ch='(' then
                        getToken()
                        PushFactor(sv,false,st)
                        DoSequence('(',1)
                        SideEffects = or_bits(SideEffects,E_all)
                        PushOp(opCallFunc,BltinOp)
                        exit
                    end if
                    bStruct = find(etype,stids)!=0
                    new_struct = iff(bStruct?etype:0)
                else
                    etype = T_object
                    bStruct = false
                    new_struct = 0
                end if
            end if
            opstype[opsidx] = etype
            if wasdot then
                getToken()
            else
                MatchChar(']')
            end if
            if not find(toktype,".[") then exit end if
            if not bStruct and not and_bits(etype,T_sequence) then
                Aborp("attempt to subscript an atom")
            end if

        else -- (not bStruct)

            if toktype=ELLIPSE
            or (ORAC and not wasdot and toktype=LETTER and ttidx=T_to) then
                getToken(float_valid:=true)
                Expr(pAllops, asBool)
                opsidxm1 = opsidx-1
                opsidxm3 = opsidx-3
                if opTopIsOp=BltinOp
                and opstack[opsidx]=opLen
                and opstack[opsidxm1]=opstack[opsidxm3] then
                    -- replace x[..length(x)] with x[..-1]:
                    opTopIsOp = 0
                    opsidx -= 1
                    opstack[opsidx] = T_constm1     -- (symtab[T_constm1] is a -1 S_Const)
                    opstype[opsidx] = T_integer
                end if
                PushSubOp(opSubss,SliceOp,noofsubscripts)
                mapEndToMinusOne = 0
                MatchChar(']')
                exit
            end if
--DEV as above, replace x[length(x)] with x[-1]?
            mapEndToMinusOne = 0
            if toktype=',' and (not ORAC or not wasdot) then    -- (don't allow x.i,j)
                toktype = '['
            else
                if not ORAC or toktype!='.' then
                    if not wasdot then
                        MatchChar(']')
                    end if
                    if toktype!='[' or wasdot then
                        isSubscript = 1
                        PushSubOp(opSubse,SubscriptOp,noofsubscripts)
                        isSubscript = 0
                        exit
                    end if
                end if
            end if
            noofsubscripts += 1
        end if -- bStruct
    end while
    mapEndToMinusOne = wasMapEndToMinusOne
end procedure

procedure istype()
--
-- The #istype{var,t} construct tests correct operation of the compiler,
--  specifically the (fleeting) compile-time "localtype" handling.
--  eg
--          object o
--              o=1
--              #istype{o,integer}
-- These "localtypes" are also set by eg "if sequence(x) then" and reset/
--  flipped by eg end if/else/etc. t may be one of the standard builtins,
--  ie integer/atom/sequence/string/object, or a digit 1..15 (T_integer
--  to T_object, see syminit() in psym.e), usually in 0bNNNN format (eg 
--  0b1001) for "string or integer", or a user defined type.
--
-- The #istype construct does not emit any code or change program function.
-- It is usually prefixed with "--/**/" to prevent problems with RDS Eu.
--
-- The #istype construct may occur at any point a statement such as "?1"
--  may appear, plus additionally after "and"|"or"|"xor". The latter allows
--  eg "if integer(c) and #istype{c,integer} c>=0 and c<=255 then" (but nb
--  not say "if integer(c) and c>=0 #istype{c,integer} then); compare the 
--  -d output with say "if MYint(c) and c>=0 and c<=255 then", where MYint 
--  is defined as function MYint(object o) return integer(o), to see why.
--  (Also compare against a udt extension of integer to see that works.)
--
-- Real-world applications are at liberty to use this, though I doubt they
--  will find any practial purpose. Permitting this construct should, btw,
--  add less than 0.01% overhead to compilation times, though admittedly
--  the whole "localtype" thing may sometimes impact much more heavily,
--  especially when interpreting rather than compiling [DEV measureme?].
--
integer N, T, ltype, k
sequence symtabN, b1, b2
object dbg

    MatchString(T_istype)
    MatchChar('{')
    if toktype!=LETTER then Aborp("A variable name is expected here") end if
    N = InTable(InAny)
    if N<=0 then Undefined() end if
    symtabN = symtab[N]
    if symtabN[S_NTyp]>S_TVar then Aborp("illegal") end if
    ltype = symtabN[S_ltype]
    getToken()
    MatchChar(',')
    if toktype=LETTER then
        T = InTable(InAny)
        if T<=0 then Undefined() end if
        symtabN = symtab[T]
        if symtabN[S_NTyp]!=S_Type then Aborp("A type is expected here") end if
    elsif toktype=DIGIT then    -- (includes 0bNNNN format)
        T = TokN
        if T<T_integer or T>T_object then Aborp("Invalid") end if
    else
        Aborp("unrecognised")
    end if
    if NOLT=0 or bind or lint then
        if emitON then      -- allows eg  if usetdsq then #istype{s,0b0100} else #istype{s,0b1100} end if
            if ltype!=T then
                b1 = "0000"
                b2 = "0000"
                k = 8
                for i=1 to 4 do
                    if and_bits(ltype,k) then b1[i] = '1' end if
                    if and_bits(    T,k) then b2[i] = '1' end if
                    k = floor(k/2)
                end for
                if ltype>T_object then
--              if ltype>T_object or find(ltype,typeINSPO) then
                    dbg = symtab[ltype]
--DEV some very similar code in pilx86.e...
                    b1 = getname(symtab[ltype][S_Name],-2)
                else
                    b1 = "0b"&b1
                    if find(ltype,typeINSPO) then
                        b1 = b1 & '(' & getname(symtab[ltype][S_Name],-2) & ')'
                    end if
                end if
                if T>T_object then
--              if T>T_object or find(T,typeINSPO) then
                    b2 = getname(symtab[T][S_Name],-2)
                else
                    b2 = "0b"&b2
                    if find(T,typeINSPO) then
                        b2 = b2 & '(' & getname(symtab[T][S_Name],-2) & ')'
                    end if
                end if
                Aborp(sprintf("fail: ltype[%d] is %s, not %s",{N,b1,b2}))
            end if
            s5 &= {opLchk,N,T,tokline,tokcol,fileno}
        end if -- emitON
    end if -- NOLT
    getToken()
    MatchChar('}')
end procedure

procedure isinit()
--
-- No code is emitted by the #isinit construct, it is just a check.
--
-- The #isinit{var,0/1/2} construct tests correct operation of the compiler,
--  specifically the (fleeting) compile-time "is initialised" handling.
--  eg
--          integer i
--              if blah then
--                  #isinit{i,0}
--                  i = 1
--                  #isinit{i,1}
--              elsif blah then
--                  #isinit{i,0}
--              else
--                  #isinit{i,0}
--                  i = 2
--                  #isinit{i,1}
--              end if
--              #isinit{i,0}
--              if blah then
--                  i = 1
--              else
--                  i = 2
--              end if
--  --**--      #isinit{i,2}    -- i was init on every branch, but see below.
--
-- symtab[N][S_Init] can be 0 when the var is actually initialised, but it
--  had better not be non-0 when the variable is unassigned. Note that S_Init
--  has a subtly different meaning on S_Const entries (we know the value of
--  an init const at compile-time; a non-init const we do not, however it 
--  will be set before it is used, so we treat said as we do init vars).
--
--DEV not implemented...
-- In the last case, (--**-- above), there is a potentially huge performance
--  cost, keeping multiple tables of prior init states and merging them at
--  endif/else/elsif. For example "if <set 300 vars> else <set 300 vars> end"
--  could cost O(90,000) if done naively. The cost is reduced significantly
--- by keeping these sets in order, but that is not free either. We elect to
--  only incur this penalty when compiling, but not when interpreting, and
--  use 2 to mean "init when compiling, not init when interpreting".
--
-- Real-world applictions are at liberty to use this, though I doubt they
--  will find any practial purpose. At the time of writing, there is not
--  much, if any, use made of this in the compiler tests, but it seemed
--  an obvious addition at the time #istype went in.
-- Update: t48init.exw now exists, but shows improvements are possible.
--
integer N, init
sequence symtabN
    MatchString(T_isinit)
    MatchChar('{')
    if toktype!=LETTER then Aborp("A variable name is expected here") end if
    N = InTable(InAny)
    if N<=0 then Undefined() end if
    symtabN = symtab[N]
    if symtabN[S_NTyp]>S_TVar then Aborp("illegal") end if
    init = (symtabN[S_Init]!=0)
    getToken()
    MatchChar(',')
    if toktype!=DIGIT then Aborp("unrecognised") end if
    if TokN<0 or TokN>2 then Aborp("Invalid") end if
    if TokN=2 then TokN = bind end if
    if init!=TokN then Aborp("fail") end if
    getToken()
    MatchChar('}')
end procedure


procedure isginfo()
--
--  #isginfo{var,type,min,max,etype,length}
--      --  (1st 2nd  3rd 4th 5th   6th   )
--
--  The compiler stacks these for final testing after gvar_scan is complete.
--    (in the form of opGchk virtual instructions)
--  In interpreted mode (which curtails gvar_scan early) it does nowt.
--   (update: gvar_scan is now "ilxlate() in pilx86.e with isGscan=1")
--  No final code whatsoever is emitted as a result of #isginfo statements,
--  nor do they alter compiler behaviour, apart from reporting any errors.
--
--  #isginfo is primarily used to test correct operation of the compiler, eg:
--
--  object x
--      x={1,2,3}   -- sequence of integer, length 3
--      x=5         -- integer 5 (becomes min)
--      x=7         -- integer 7 (becomes max)
--      #isginfo{x,0b0101,5,7,integer,3}
--      -- (0b0101 is dword sequence|integer)
--
--  Note that entries between { and } are trivially parsed (see below),
--  rather than proper/normal expressions. You could not, for example,
--  use "T_Dsq+T_integer" instead of 0b0101 since T_Dsq and T_integer are
--  neither valid symbols in the application code nor supported by the
--  code below, and "+" (etc) is also simply not supported. Of course
--  you are more than welcome to improve matters here should you want.
--
--  var         (1st) must be a normal variable identifier.
--              Note the placement of #isginfo is critical. Just after
--              the variable definition will get "final global info", ie    <<[currently for all]
--              a program-wide state, whereas mid-code gets on-the-fly
--              state, eg [***DEV not implemented yet!]
--                  object O
--                      --/**/ #isginfo{O,object,MIN,MAX,object,-2}
--                      O = 1
--                      --/**/ #isginfo{O,0b0001,1,1,object,-1}
--                      O += 1
--                      --/**/ #isginfo{O,0b0001,1,MAX,object,-1}
--                      O -= 1
--                      --/**/ #isginfo{O,0b0001,MIN,MAX,object,-1}
--                      O = repeat(0,10)
--                      --/**/ #isginfo{O,0b0100,MIN,MAX,integer,10}
--                      O = 1.5
--                      --/**/ #isginfo{O,0b0010,MIN,MAX,integer,10}
--                      O = "fred"
--                      --/**/ #isginfo{O,0b1000,MIN,MAX,integer,4}
--                      O={1.5,"fred",{}}
--                      --/**/ #isginfo{O,0b0100,MIN,MAX,0b1110,3}
--              Note also that should a routine call modify a variable
--              in any way (see sideEffects), it is reset to the program
--              wide state rather than a merge of all possible states at
--              all return statements within that routine (if you think 
--              that would be easy you are welcome to give it a try..)
--              and likewise the ginfo at routine start is also program
--              wide state rather than a merge of all possible states at
--              all call statements to that routine (ditto, with added
--              note of caution concerning routine_ids and call_backs).
--  type/etype  (2nd/5th) may use integer/atom/string/sequence/object in 
--              place of say 0b0101 aka 5 in the first example. These are
--              "rootTypes", in the range 1..15 only - udts live in the
--              S_vtype and S_ltype fields, which are not tested here.
--              See psym.e/syminit(), or any list.asm that has a symtab 
--              dump, for a full list.
--  min/max     (3rd/4th) may use MIN/MAX in place of -1073741824 aka 
--              -#40000000, and 1073741823 aka #3FFFFFFF respectively.
--  min/max/len (3rd/4th/6th) may use MAXLEN in place of 805306368 or 
--              #30000000, the assumed maximum result from length().
--  Otherwise, literal integers (in decimal/hex/octal/binary) must be
--  used, no named constants/operators/subscripts/functions/etc.
--
--  Two shortcuts to get the type..length can be used: a) look in list.asm
--  (with dumpSymtab in plist.e set true), b) just add any old nonsense, eg 
--  #isginfo{fred,1,1,1,1,1}, then in both cases just copy & paste. After,
--  that is, you have verified them for reasonableness, of course. As this
--  implies, #isginfo is more suited to "retro-fitting" than new code, and
--  mostly valuable in verifying that some source change to p.exw does not
--  accidentally mangle something else.
--
--  In theory you can use #isginfo as an alternative to some user defined 
--  types, eg instead of type boolean(integer b) return b=0 or b=1, just
--  define integer flag #isginfo{flag,1,0,1,15,-1}, and when you compile
--  (as opp. to interpret) the code, it will fail if it cannot prove that
--  flag is always 0 or 1. Naturally, there is no guarantee on said being 
--  suitable for any, let alone all circumstances, and yes, "boolean" is
--  a fair bit easier to type when you have to do so several times, as
--  well as being considerably easier to read. Like I said, in theory.
--  Again, naturally you are more than welcome to implement support for
--  eg "type boolean(#isginfo,integer,0,1,object,-1)" should you want.
--
--  Typically #isginfo is preceded by --/**/ for the benefit of RDS Eu.
--  See t49ginfo.exw and/or search the sources of p.exw for examples.
--
sequence symtabN
integer tl,tc,N,typ,iMin,iMax,etyp,len,ok

    tl = tokline
    tc = tokcol
    MatchString(T_isginfo)
    MatchChar('{')
--1) var name
    if toktype!=LETTER then Aborp("A variable name is expected here") end if
    N = InTable(InAny)
    if N<=0 then Undefined() end if
    symtabN = symtab[N]
    if symtabN[S_NTyp]>S_TVar then Aborp("illegal") end if
    getToken()
    MatchChar(',')
--2) dtype
    if toktype=LETTER then
        typ = InTable(InAny)
        if typ<=0 then Undefined() end if
    elsif toktype=DIGIT and integer(TokN) then
        typ = TokN
    else
        Aborp("unrecognised")
    end if
--DEV this should probably be "typ<T_integer or typ>T_object"
    if typ<=0 or typ>=length(symtab) or symtab[typ][S_NTyp]!=S_Type then
        Aborp("A type is expected here")
    end if
    getToken()
    MatchChar(',')
--3) min
    ok = 0
    if toktype=LETTER then
        if ttidx=T_MIN then
            iMin = MININT   ok = 1
        elsif ttidx=T_MAX then
            iMin = MAXINT   ok = 1
        elsif ttidx=T_MAXLEN then
            iMin = MAXLEN   ok = 1
        end if
    else
        if toktype='-' then
            getToken(float_valid:=true)
            TokN=-TokN
        end if
        if toktype=DIGIT and integer(TokN) then
            iMin = TokN     ok = 1
        end if
    end if
    if not ok then
        Aborp("unrecognised")
    end if
    getToken()
    MatchChar(',')
--4) max
    ok = 0
    if toktype=LETTER then
        if ttidx=T_MIN then
            iMax = MININT   ok = 1
        elsif ttidx=T_MAX then
            iMax = MAXINT   ok = 1
        elsif ttidx=T_MAXLEN then
            iMax = MAXLEN   ok = 1
        end if
    else
        if toktype='-' then
            getToken(float_valid:=true)
            TokN=-TokN
        end if
        if toktype=DIGIT and integer(TokN) then
            iMax = TokN     ok = 1
        end if
    end if
    if not ok then
        Aborp("unrecognised")
    end if
    getToken()
    MatchChar(',')
--5) etyp
    if toktype=LETTER then
        etyp = InTable(InAny)
        if etyp<=0 then Undefined() end if
--      symtabN = symtab[etyp]
--      if symtabN[S_NTyp]!=S_Type then Aborp("A type is expected here") end if
    elsif toktype=DIGIT and integer(TokN) then
        etyp = TokN
    else
        Aborp("unrecognised")
    end if
--DEV this should probably be "etyp<T_integer or etyp>T_object"
    if etyp<=0 or etyp>=length(symtab) or symtab[etyp][S_NTyp]!=S_Type then
        Aborp("A type is expected here")
    end if
    getToken()
    MatchChar(',')
--6) len
    ok = 0
    if toktype=LETTER then
        if ttidx=T_MAXLEN then
            len = MAXLEN    ok = 1
        end if
    else
        if toktype='-' then
            getToken(float_valid:=true)
            TokN = -TokN
        end if
        if toktype=DIGIT and integer(TokN) then
            len = TokN      ok = 1
        end if
    end if
    if not ok then
        Aborp("unrecognised")
    end if
    getToken()
    MatchChar('}')

    if bind then
        s5 &= {opGchk,N,typ,iMin,iMax,etyp,len,tl,tc,fileno}
    end if
end procedure


--without trace
--with trace

integer sqopNo

function sqopNeeded(integer two)
-- two is actually 1 for unary ops and 2 for binary ops.
    if opTopIsOp then PopFactor() end if
--?two
--?opstype[1..opsidx]
--if getc(0) then end if
    if not and_bits(opstype[opsidx],T_atom) then return 1 end if
    if two=2 then
        if not and_bits(opstype[opsidx-1],T_atom) then return 1 end if
    end if
    return 0
end function

procedure p2jssqv(string op, integer line, col)
    if with_js=1 then
        Abort("p2js violation: "&op&"() must be used here")
    end if
    not_js = true
    nj_reason = op&"() assumed"
    Warn(nj_reason,line,col,SQ_WARN)
end procedure

--<old notes>
-- somewhere, I also want the following mapping:
--  (by somewhere I might have meant opJeq etc in Branch()...)
-- let C be the result from compare(), when we detect comparison with a literal integer:
--  C=0 -> opSeq  1(0=C)        C=-1 -> opSlt   3(-1=C)         C=1 -> opSgt    4(1=C)
--  C!=0 -> opSne 2(0!=C)       C!=-1 -> opSge  6(-1!=C)        C!=1 -> opSle   5(1!=C)
--  C<0 -> opSlt  3(0>C)        C<-1 leaf        (-1>C)         C<1 -> opSle    5(1>C)
--  C>0 -> opSgt  4(0<C)        C>-1 -> opSge   6(-1<C)         C>1 -> leaf      (1<C)
--  C<=0 -> opSle 5(0>=C)       C<=-1 -> opSlt? 3(-1>=C)        C<=1 -> leat     (1>=C)
--  C>=0 -> opSge 6(0<=C)       C>=-1 leat       (-1<=C)        C>=1 -> opSgt?  4(1<=C)
-- (hope I got them right. Also if compare(x,y) then -> opSne 2.)
-- where leaf means "logic error (always false)"    \ Similar for any S_Const integer 
--   and leat means "logic error (always true)"     /  which is not one of -1/0/+1.
--  The two entries marked ? are equivalent to a plain = and should probably generate a warning.
--</old notes>

constant usecmap=01     -- (this should work fine, but as there are lots of cases, have 
                        --  left this as an easy way to turn it off should you need to.)

procedure cmapush(integer flag)
--  Common code to throw up some warnings and replace 
--  eg compare(a,b)<relop><literal int> with 1 or 0.
integer k, opsidxp1
    opsidxm1 = opsidx-1
    t1 = opstype[opsidxm1]
    t2 = opstype[opsidx]
    if  not and_bits(t1,T_sequence)         -- if   p1 is atom
    and not and_bits(t2,T_atom) then        --  and p2 is sequence)
        -- compare(atom,sequence)
        WarnX("probable type error (always -1)",opsidxm1)
    elsif not and_bits(t1,T_atom)           -- elsif p1 is sequence
      and not and_bits(t2,T_sequence) then  --   and p2 is atom)
        -- compare(sequence,atom)
        WarnX("probable type error (always 1)",opsidxm1)
    end if
    opsidxp1 = opsidx+1
    k = opsline[opsidxp1]
    opsline[opsidx] = k
    k = opstcol[opsidxp1]
    opstcol[opsidx] = k
    add_ple(flag)
    if flag then
        flag = T_const1
    else
        flag = T_const0
    end if
    freeTmp(-2)
    PushFactor(flag,true,T_integer)
end procedure

--
--Reminder/analysis:
--
--global constant Bcde = {opJlt,opJle,opJeq,opJne,opJge,opJgt}
--                          1     2     3     4     5     6
--  1) compare(a,b)<relop k>literal
-- map  literal<-1:         f     f     f     t     t     t
--      literal=-1:         f     1?    1     5     t     5     -- warning: "<=-1" equivalent to "=-1" here
--      literal= 0:         1     2     3     4     5     6
--      literal= 1:         2     t     6     2     6?    f     -- warning: ">=1" equivalent to "=1" here
--      literal> 1:         t     t     f     t     f     f
--
--  2) literal<relop k>compare(a,b) [nb: this table is mostly handled via a trev[k]]
-- map  literal<-1:         t     t     f     t     f     f
--      literal=-1:         5     t     1     5     1?    f     -- warning: "-1>=" equivalent to "-1=" here
--      literal= 0:         6     5     3     4     2     1
--      literal= 1:         f     6?    6     2     t     2     -- warning: "1<=" equivalent to "1=" here
--      literal> 1:         f     f     f     t     t     t

function cmap(integer constid, integer opidx, integer flipped)
-- maps eg "if compare(a,b)>=0 then" to "if a>=b then"
--  (ie corresponds to middle line of first table, opJge column:
--      the "5" we find there/return here means "map it to a>=b"...)
--  (when flipped is -1, we're actually dealing with the reverse, ie/eg
--      0<=compare(a,b), but opidx has already been opidx=trev[opidx]'d.
--      Here, flipped is only used to correct a few compiler messages.)
integer cvalue
    cvalue = symtab[constid][S_value]
    opsidx -= 1
    if cvalue<-1 then       -- line 1 of top table
        cmapush(opidx>3)        -- always false*3/true*3
        return 0
    elsif cvalue=-1 then    -- line 2 of above table
        if opidx=5 then     -- compare(a,b)>=-1
            cmapush(1)          -- always true
            return 0
        elsif opidx=4       -- compare(a,b)!=-1     \ ie if compare(a,b)
           or opidx=6 then  -- compare(a,b)>-1      /     is 0 or +1
            return 5            -- use .ge.
        elsif opidx=1 then  -- compare(a,b)<-1
            cmapush(0)          -- always false
            return 0
        end if
        if opidx=2 then
            if flipped then -- -1>=compare(a,b)
                WarnX("\"-1>=\" equivalent to \"-1=\" here",opsidx+1)
            else            -- compare(a,b)<=-1
                WarnX("\"<=-1\" equivalent to \"=-1\" here",opsidx+1)
            end if
        end if
        return 1                -- use .lt.
    elsif cvalue=0 then     -- line 3 of above table
        -- do nowt case (all compare(a,b)<relop>0 map to a<relop>b)
        return opidx
    elsif cvalue=1 then     -- line 4 of above table
        if opidx=6 then     -- compare(a,b)>1
            cmapush(0)          -- always false
            return 0
        elsif opidx=1       -- compare(a,b)<1       \ ie if compare(a,b)
           or opidx=4 then  -- compare(a,b)!=1      /     is 0 or -1
            return 2            -- use .le.
        elsif opidx=2 then  -- compare(a,b)<=1
            cmapush(1)          -- always true
            return 0
        end if
        if opidx=5 then
            if flipped then -- 1<=compare(a,b)
                WarnX("\"1<=\" equivalent to \"1=\" here",opsidx+1)
            else            -- compare(a,b)>=1
                WarnX("\">=1\" equivalent to \"=1\" here",opsidx+1)
            end if
        end if
        return 6                -- use .gt.
    elsif cvalue>1 then     -- line 5 of above table
        cmapush(opidx=4 or opidx<3)     -- always false*3/true*3
        return 0
    end if
    -- oops
end function

--integer notcode

--with trace
--object etype  --7/1/09
--integer etype, isLit
--object sig
--integer N

--
-- Factor/Expr are implemented using Precedence Climbing
--

--DEV only ever set to 0...
integer notFdone    -- only used if Expr is passed <=-1 in toBool

procedure GetFactor(integer toBool)
integer notumline, notumcol, wasNamespace, nsttidx
integer N, etype, isLit
object sig

    emitline = tokline

    if toktype=LETTER then
        if ttidx=T_not then     -- (pUnary precedence)
            notumline = tokline
            notumcol = tokcol
            getToken()
            notFdone = 0
            GetFactor(asBool)   -- get factor only
            opsidxm1 = opsidx-1
            if notFdone then
                -- a compound op was converted to inverted bool
                -- eg/ie in not(a and b), the "a and b" expr was  
                --       converted to 1/0 instead of the usual 0/1
                etype = T_integer
            elsif opTopIsOp=BranchOp then
                integer notcode = find(opstack[opsidx],Bcde)        -- opJ lt, le, eq, ne, ge, gt
                notcode = tnot[notcode]                             -- ==> ge, gt, ne, eq, lt, le
--              opstack[opsidx] = Bcde[notcode] -- not type-safe?   [DEV investigateme]
                N = Bcde[notcode]
                opstack[opsidx] = N
--validate_opstack()
                etype = 0
            elsif (   (opTopIsOp=SubscriptOp and opstack[opsidxm1]=1)           -- single subscript
--                 or (opTopIsOp=BltinOp and find(opstack[opsidx],opINSP)))     -- or builtin type test
                   or (opTopIsOp=BltinOp and 
                       find(opstack[opsidx],opINSPabits)))  -- or builtin type test/Jbits
                    -- eg if not x[i] then // if not sequence(x) then.
              and toktype=LETTER
              and find(ttidx,{T_then,T_do,T_and,T_or}) then
                -- Can assume this will be handled in Branch() rsn,
                -- or soon undone in Expr(), see below
                opTopIsOp += 10     -- SubscriptOp -> NotSubscriptOp; BltinOp ->NotBltinOp
                etype = 0
            elsif sqopNeeded(1) then
                p2jssqv("sq_not",notumline,notumcol)
                N = sqAble[opNot]
                sig = symtab[N][S_sig]
                Call(N,sig,FUNC,true)
                opstype[opsidx] = T_sequence
                etype = T_sequence
            else
--DEV BltinOp
                PushOp(opNot,UnaryOp)
                etype = T_atom
            end if
        elsif ttidx=T_iff
           or ttidx=T_iif then
            DoIff()
        else
            wasNamespace = (Ch=':')
            nsttidx = ttidx
            N = InTable(InAny)
            if N=T_free then N=T_ffree end if
            integer wascol = col,
                    wasline = line
            skipSpacesAndComments()
            if N<=0 then -- forward function call?
                -- 26/12/19: (implicit "this")
                if class_def!=T_const0 then
                    if class_def!=srids[$] then ?9/0 end if
                    etype = stids[$]
                    string field_name = trim(text[tokcol..wascol-1],`" `)
                    if structs:get_field_type(etype,field_name)!=NULL then
                        toktype = '.'
                        ttidx = T_this
                        N = InTable(InAny)
                        if N<=0 then ?9/0 end if
                        PushFactor(N,false,etype)
                        col = tokcol
                        line = wasline
                        Ch = field_name[1]
                        DoSubScripts()
                        return
                    end if
                end if
                if Ch!='(' then Undefined() end if
                if FWARN then
                    if not testall then
                        Warn("forward call assumed",tokline,tokcol,0)
                    end if
                end if
                ForwardProc(FUNC)
--              ForwardProc(FUNC,wasNamespace,nsttidx)
                etype = T_object
            else
--21/01/2021
                if symtab[N][S_NTyp]=S_Rsvd then
--              if N<=T_Asm or symtab[N][S_NTyp]=S_Rsvd then
--              and (rType!=FUNC or and_bits(symtab[N][S_State],K_fun)!=K_fun or not sequence(sig)) then
                    Aborp("illegal use of reserved word")
                end if
--              etype = symtab[N][S_vtype]
--              if sequence(etype) then
                sig = symtab[N][S_sig] -- nb same as [S_vtype], which is an integer
                if sequence(sig) then
--11/02/20: (first-class routine_ids part 2)
--4/10/2020:
--                  if Ch!='(' and pleeurghh=0 and N>T_Asm then
--5/11/2020:
--                  if Ch!='(' and pleeurghh=0 then
                    if Ch!='(' then
                        if N<=T_Asm then
                            -- replace with hll_xxx:
--                          N = 9/0
--                          ?"line 7291 pmain.e, N<=T_Asm"
                            N = get_hll_stub(N)
                            if N=0 then Aborp("invalid") end if
                        end if
                        Or_K_ridt(N, S_used+K_ridt)
                        integer k = addRoutineId(N)
                        PushFactor(k,true,T_integer)
                        if Ch!=-1 then
                            getToken()
                        end if
                        etype = T_integer
                    else
--                      top_level_abort = 0 -- not needed
--                      Call(N,etype,FUNC,false)
                        Call(N,sig,FUNC,false)
--DEV tryme: (erm, may need the <T_Bin ranges thingy) (still untried:)
--                      if N<=T_Ainc then
--                          etype = symtab[N-1][S_vtype]
--                      else
                        etype = T_object
--                      end if
                    end if
--27/10/19: (first-class routine_ids)
--23/5/21 (remove the integer check)
--              elsif sig=T_integer and Ch='(' then
                elsif Ch='(' then
--SUG:              DoFirstClassRid(FUNC)
--;  35 call_proc(r_show,{})
--  mov eax,[#0040293C] (r_show)          ;#004270A5: 241 3C294000               uv 01 00  1   4      
--  mov esi,[#0040225C]                   ;#004270AA: 213065 5C224000            vu 40 00  1   4      
--  call #00432016 (:%opCallProc)         ;#004270B0: 350 61AF0000               v  00 00  1   5      

--DEV...?
--          if emitON then
                    isLit = (and_bits(symtab[N][S_State],K_lit)=K_lit)
if isLit then ?9/0 end if   -- I think we shd just use false!
                    PushFactor(N,isLit,T_integer)
                    getToken()
                    -- (aside: not dot, no defaulted struct!)
                    DoSequence('(')
                    SideEffects = or_bits(SideEffects,E_all)
--/*
--if newEBP then
                    -- save eax if rqd
                    saveFunctionResultVars(opsidx,NOTINTS)
--end if
                    if opTopIsOp then ?9/0 end if
                    if opsidx!=2 then ?9/0 end if
                    integer p1 = opstack[1],
                            p2 = opstack[2],
                            opcode = opCallFunc
--if newEmit then
                    if not symtab[p1][S_Init] then
                        Unassigned(p1)
                    end if
                    if not symtab[p2][S_Init] then
                        Unassigned(p2)
                    end if
                    agcheckop(opcode)
--end if
                    apnds5({opcode,p1,p2})
                    freeTmp(-opsidx)
--*/
                    PushOp(opCallFunc,BltinOp)

                else

--DEV ripped out 22/10/09:
--if fastSubscriptIcount=0 then
--  if fastSubscriptLHS=N then
--      fastSubscriptLHS = 0
--  end if
--end if

--                  vno? = N
--                  etype = rootType(etype) --DEV see below, (may need a new tmp)
--                  PushFactor(N,and_bits(symtab[N][S_State],K_lit)=K_lit,etype)
--                  PushFactor(N,and_bits(symtab[N][S_State],K_lit)=K_lit,rootType(etype))
--                  PushFactor(N,and_bits(symtab[N][S_State],K_lit)=K_lit,rootType(symtab[N][S_ltype]))
                    etype = symtab[N][S_ltype]

--removed 4/11/19: (structs) - failed, breaks self-hosting, eg byterange[1] gives "type error (attempt to subscript an atom)".
                    if etype>T_object then etype = rootType(etype) end if

--                  PushFactor(N,and_bits(symtab[N][S_State],K_lit)=K_lit,etype)
                    isLit = (and_bits(symtab[N][S_State],K_lit)=K_lit)
--14/8/15 nope, too...
--if N=lhsvar then
--  lhsvar = 0
--end if
                    PushFactor(N,isLit,etype)
--DEV tryme: (finally tried 4/11/19, for structs...)
--                  leave etype as is!
--                  etype = T_object
--4/11/19 (!!)
                    if Ch!=-1 then
                        getToken()
                    end if
                end if
            end if
        end if
    elsif toktype=DIGIT then
        PushFactor(addUnnamedConstant(TokN, T_integer),true,T_integer)
        getToken()
        etype = T_integer
    elsif toktype='{' then
        DoSequence()
        etype = T_Dsq
    elsif toktype=DQUOTE then
        PushFactor(addUnnamedConstant(TokStr,T_string),true,T_string)
        getToken()
        etype = T_string
    elsif toktype=HEXSTR then
        DoHexStr()
        etype = T_Dsq
    elsif toktype=SQUOTE then
        -- squotes stored as literal integers.
        PushFactor(addUnnamedConstant(Tok9,T_integer),true,T_integer)
        getToken()
        etype = T_integer
    elsif toktype='-' then  -- (pUnary precedence)
        notumline = tokline
        notumcol = tokcol
        MatchChar('-',float_valid:=true)
        notFdone = 0
        Expr(pUnary, asNegatedBool) -- get factor only (negated)
        if notFdone then
            -- a compound op was converted to negated bool
            -- eg/ie in -(a and b), the "a and b" expr was  
            --       converted to 0/-1 instead of the usual 0/1
            etype = T_integer
        else
            if emitON then  --DEV 17/7 umm?
                etype = opstype[opsidx]
                if opsltrl[opsidx]=1
                and and_bits(symtab[opstack[opsidx]][S_State],K_rtn)=0
                and not and_bits(etype,T_sequence) then
                    TokN = -symtab[opstack[opsidx]][S_value]
--3/1/16:
--                  if integer(TokN) then   -- -1073741824 aka -#40000000 case
                    if not isFLOAT(TokN) then
--                      opstype[opsidx] = T_integer
-- added 27/8/2020 (!!!):
                        etype = T_integer
-- added 8/6/2012 (!!!):
                    else
--                      opstype[opsidx] = T_atom
                        etype = T_atom
                    end if
                    opstype[opsidx] = etype
                    opstack[opsidx] = addUnnamedConstant(TokN,etype)
--validate_opstack()
                    etype = 0
                elsif sqopNeeded(1) then
                    p2jssqv("sq_uminus",notumline,notumcol)
                    N = sqAble[opUminus]
                    sig = symtab[N][S_sig]
                    Call(N,sig,FUNC,true)
                    opstype[opsidx] = T_sequence
                else
--DEV BltinOp
                    PushOp(opUminus,UnaryOp)
                end if
            end if -- emitON
        end if  -- notFdone
    elsif toktype='(' then
        getToken(float_valid:=true)
--DEV pass our asBool?
-- 20/06/2011 bugfix... see end of t51 for an example...
--  (what can I say, all tests pass, notBool makes no sense here, yet obviously I wrote it)
--  (ah: notBool of 1 rqd for assignments, 0 for conditionals!!)
--NEW_PREC
        Expr(pAllops, toBool)
        MatchChar(')')
    elsif toktype=FLOAT then
--      PushFactor(addUnnamedConstant(TokN,T_atom),true,T_atom)
        PushFactor(addUnnamedConstant(TokN,T_N),true,T_N)
        getToken()
--      etype = T_atom
        etype = T_N
    elsif toktype='+' then -- (ignore, pUnary precedence)
        MatchChar('+',float_valid:=true)
        GetFactor(asBool)
    elsif toktype='~' and ORAC then -- (pUnary precedence)
        -- T_length
        MatchChar('~',float_valid:=false)
        GetFactor(false)
        PushOp(opLen,BltinOp)
    else
        Aborp("syntax error - an expression is expected here")
    end if
    if toktype='['
    or (ORAC and (not fromsubss) and toktype='.') then  -- (pSubsc precedence)
        if opTopIsOp then
            PopFactor()
            etype = opstype[opsidx]
        end if
--      DoSubScripts(etype)
        DoSubScripts()
    end if
end procedure


--DEV re-test this. I thought this was uber-neat now...
-- Aside: this makeBool lark can get quite messy for eg (1 and 1)=1, any 
--        improvements are welcome. (Perhaps for "if (1 and 1)=1 then", 
--        it might be better to change pixl.e rather than here, but for
--        eg a=(b and c), then we probably need to shift this kind of
--        processing down into assignment, at a guess...? Or perhaps we
--        should be passing tidx from Assignment to Expr instead of 1?)
--        However, first/also examine an assembly listing of t51nstc.exw; 
--        perhaps I unwittingly made a compromise here for that, or not?

--integer mBopCode          -- usually opMovbi, can temporarily be set to  
--      mBopCode = opMovbi  -- opMovsi for proper handling/dealloc in eg 
--                          -- "<object> = (x and/or y)", see Assignment.)

function makeBool(integer toBool, integer wasScBP, integer wasExprBP, integer BN)
--
-- We need a boolean result (-ish: see Expr for possible values of toBool).
-- wasSc/ExprBP allow us to boolean-ize just the stuff we recently added.
-- BN is a work var, possibly reused several times.
--
--  emits the following code:
--
--      jcc (last var/condition) scBP
--    exprBP: (fail chain of compound expression)
--      mov [BN], 0 -- (or 1)
--      jmp done
--    scBP: (success chain of compound expression)
--      mov [BN], 1 -- (or -1 or 0)
--    done:
--
integer djmp, T_const

    if BN=0 then
        BN = newTempVar(T_integer,Shared)
    end if
    noofbranches = 2    -- prevent flip
    scBP = Branch(NoInvert,2,scMerge,scBP)
    -- backpatch exprBP (fail) to here:
    if exprBP>wasExprBP then
        exprBP = backpatch(exprBP,wasExprBP,exprMerge)
    end if
    if toBool=-9 then                   -- inverted         (asInvertedBool)
        T_const = T_const1
    else                                -- normal/negated
        T_const = T_const0
    end if
    emitHexMov(opMovbi,BN,T_const)      -- <var BN>:=0 (or 1 if inverted)
--  emitHexMov(mBopCode,BN,T_const)     -- <var BN>:=0 (or 1 if inverted)
    -- Technically we could use a mergeSet of say -1 here (and on the opLabel),
    --  as long as pilxl was made to cope with it (do nowt). As it stands, the 
    --  extra push/pop of (empty?) reginfo is not going to hurt/cost anyone.

    --(DEV 17/10/09 surely these emitON could/should cover more code...?)

    if emitON then
        apnds5({opJmp,exitMerge,0,0})       -- jmp done
        djmp = length(s5)   -- linked to opLabel below
    end if
    -- backpatch scBP (succeed) to here:
    if scBP>wasScBP then
        scBP = backpatch(scBP,wasScBP,scMerge)
    end if
    if toBool=1 then                    -- normal   (asBool)
        T_const = T_const1
    elsif toBool=-1 then                -- negated  (asNegatedBool)
        T_const = T_constm1
    elsif toBool=-9 then                -- inverted (asInvertedBool)
        T_const = T_const0
    end if
    if emitON then
        emitHexMov(opMovbi,BN,T_const)      -- <var BN>:=1 (or -1 or 0)
        s5 &= {opLabel,exitMerge,0,djmp}    -- done:
        s5[djmp-1] = length(s5)

        emitHexMov(opMovbi,BN,-1)           -- force 0/1 in pilx86.e

        if NOLT=0 or bind or lint then
            Lmin = 0    -- \ (these are not really of any use   )
            Lmax = 1    -- / (whatsoever, but they don't hurt...)
            ltAdd(SET,BN,T_integer,T_integer,length(s5))
        end if -- NOLT
    end if -- emitON

    PushFactor(BN, false, T_integer)

--DEV tryme:
--  RHStype = T_integer

    return BN
end function


--7/4/16 allow values such as #FFFFFFFF when a 32bit p.exe creates a 64-bit exe:
--integer lhsli, rhsli  -- scratch vars to evaluate <literal int><relop><literal int> now.
type int3264(atom a)
    if isFLOAT(a) then ?9/0 end if
    return 1
end type
int3264 lhsli, rhsli    -- scratch vars to evaluate <literal int><relop><literal int> now.

--with trace
procedure Expr(integer p, integer toBool)
--
-- Parse an expression.
--
-- p is the precedence level we should parse to, eg/ie
--
--    pUnary   = 10 : Factor only (may as well just call GetFactor)
--    pMuldiv   = 9 : "" and * /
--    pAddsub   = 8 : "" and + -
--    pBitshift = 7 : "" and >> << (equivalent to (floor(/)|*)power(2,rhs))
--    pConcat   = 6 : "" and & (string/sequence concatenation)
--    pBitops   = 5 : "" and && || (equivalent to and/or_bits)
--    pRelops   = 4 : "" and relops(<,>,<=,>=)
--    pEqorne   = 3 : "" and == != (a single = means ==, sometimes)
--/*
--    pRelops   = 5 : "" and relops(<,>,<=,>=)
--    pEqorne   = 4 : "" and == != (a single = means ==, sometimes)
--    pBitops   = 3 : "" and && || (equivalent to and/or_bits)
--*/
--    pLogop    = 2 : "" and logicops (and,or,xor)
--    pAllops   = 0 : [full, effectively the same as pLogop]
--
--  obviously, parentheses override any setting of p.
--
-- toBool indicates whether the caller wants/can deal with 
--  exprBP/scBP chains (0, eg DoIf) or whether they must be 
--  converted into a boolean(!=0, eg Assignment) before exit.
--  NB only has effect for logic ops, eg "a and b", it does 
--     nowt on other expressions such as "a>b", "c+d", etc.
--  There are three non-zero settings of this parameter:
--   1 (asBool) means return the usual 0/1,
--  -1 (asNegatedBool) means return "" negated, ie 0/-1, (eg?! i=-(a and b))
--  -9 (asInvertedBool) means return "" inverted, ie 1/0. (eg i=not(a and b)) [DEV]
--
integer k, mklen, thisp, wastok
integer N
integer LogicTok
integer sqline, sqcol, relopline, relopcol
integer all_and
integer wasExprBP,
        wasScBP
integer compOp  -- for mapping eg compare(a,b)=-1 to lt
integer lhsliteral
atom vlhs, vrhs
integer vtype
integer BN  -- temp var for any bool results we need
object sig

    if p=pAllops and toktype=LETTER and ttidx=T_func then
        -- lambda expression, not propely documented until nested functions finished...
        DoRoutineDef(R_Func,true)
        k = addRoutineId(r_lambda)
        PushFactor(k,true,T_integer)
        return
    end if
        
    BN = 0

    if toBool>1 then
        BN = toBool
        toBool = 1
    end if

    all_and = 1

    wasExprBP = exprBP
    wasScBP = scBP

--if tokline=1 then trace(1) end if
    GetFactor(toBool)

    while 1 do
--      k = find(toktype,ZZops) -- "*/+-&<>=!|&<>"
        k = find(toktype,ZZops) -- "*/+-&|&<>=!<>"
        if k then               --  1234567890123
--          if toktype=Ch and find(Ch,"&|<>") then
--          if toktype=Ch and find(Ch,"&<>") then
            bool bChCh = (toktype=Ch and find(Ch,"&|<>"))
            if bChCh then
                -- (pConcat -> pBitops, pRelops -> pBitshift)
                k = rfind(toktype,ZZops)
            end if
            thisp = ZZpre[k]-'0'
            if thisp<p then exit end if
--          if k>=10 then
            if bChCh then
                MatchChar(Ch)
            end if
            if scBP>wasScBP             -- eg "(a or b)+?" -> 0/1+?
            or exprBP>wasExprBP then    -- eg "(a and b)+?" -> 0/1+?
                BN = makeBool(asBool,wasScBP,wasExprBP,BN)
            end if
--DEV removed 26/2:
--          all_and = 0
            wastok=toktype
            sqline=tokline
            sqcol=tokcol

            if thisp>=pAddsub then  -- MathOps
                lhsliteral = (opTopIsOp=0 and 
                              opsltrl[opsidx]=1 and 
                              opstype[opsidx]<=T_atom)
--DEV [not]fromsubss??
                getToken(float_valid:=true)
                -- aside: Expr(thisp+1,asBool) should be fine, but not tested...
                if thisp=pAddsub then       -- +,-
                    Expr(pMuldiv,asBool)    -- subexpression involving *,/ only
                else                        -- *,/
                    GetFactor(asBool)       -- just get a factor
                end if
                if emitON
                and lhsliteral 
                and opTopIsOp=0 
                and opstack[opsidx]!=0
                and opsltrl[opsidx]=1 
                and opstype[opsidx]<=T_atom 
-- 8/6/2012: DEV (fudge to take away the pain!) This is way too soon, it should be
--               done when we are about to emit code (ie in StoreVar(), search for
--               "elsif opTopIsOp=MathOp then", should be fairly straightforward).
                and (k!=2 or symtab[opstack[opsidx]][S_value]!=0) then  -- not "/0"
--              and k!=2 then   -- not "/" full stop (opFloor has better optimisations)
                    vlhs = symtab[opstack[opsidx-1]][S_value]
                    vrhs = symtab[opstack[opsidx]][S_value]
                    if k=1 then     -- *
                        vlhs *= vrhs
                    elsif k=2 then  -- /
                        vlhs /= vrhs
                    elsif k=3 then  -- +
                        vlhs += vrhs
                    elsif k=4 then  -- -
                        vlhs -= vrhs
                    else ?9/0
                    end if
                    freeTmp(-2)
--3/1/16:
--                  if integer(vlhs) then
                    if not isFLOAT(vlhs) then
                        vtype = T_integer
                    else
                        vtype = T_atom
                    end if
                    N = addUnnamedConstant(vlhs, vtype)
                    if N then -- (N=0 can happen if emitON=0)
                        if length(symtab[N])<S_gInfo then -- if not a duplicate
                            symtab[N] = append(symtab[N],{vtype,vlhs,vlhs,0,-1})
                        end if
                    end if
                    PushFactor(N,true,vtype)    -- yep, this is a literal!
                elsif sqopNeeded(2) then
                    --DEV:
                    if routineNo=T_floor and wastok='/' then
                    --  trace(1)
                    --  ZZdesc = "sq_floor_div"
--                      k = 6
                        k = 8
                    end if
                    ZZdesc = "sq_"&ZZlong[k]
                    p2jssqv(ZZdesc,sqline,sqcol)
--                  sqopNo = get_Sqop(ZZdesc)
                    sqopNo = sqAble[ZZopcodes[k]]
                    sig = symtab[sqopNo][S_sig]
                    Call(sqopNo,sig,FUNC,true)
                    opstype[opsidx] = T_sequence
                    if k=6 then
                    --DEV tmp:
                        if sqopNo!=T_sqfloor_div then ?9/0 end if
                        routineNo = sqopNo
                        just_sq_floor_div_d = 1
                    end if
                else
                    PushOp(ZZopcodes[k],MathOp)
                end if

            elsif thisp=pConcat then    -- wastok='&'
                k = 0
                while 1 do
                    if opTopIsOp=MkSqOp then
                        -- convert eg ...&{a,b,c}[&...] to ...&a&b&c[&...],
                        --  but only if a,b,c are all atoms (while "ab"&{'c','d'} 
                        --  is = "abcd" [ = opConcatN("ab",'c','d') ], of course
                        --  "ab"&{"cd","ef"} is != "abcdef").
                        -- Aside: theoretically eg "ab"&{'c','d'} should yield the
                        --        sequence {'a','b','c','d'} rather than the string
                        --        string "abcd". In practice it makes very little
                        --        difference, namely explicit string() tests only.
                        --        could easily leave the opMkSq in place when the 
                        --        left-hand sub-expression is still a string, eg
                        --        {'a','b'}&"cd" would be much more difficult;
                        --        we'd need say opConcatd/Nd to force the T_Dsq.
                        --        BTW: equal({'a','b'},"ab") yields true.
                        opsidxm1 = opsidx-1
                        mklen = opstack[opsidxm1]
-- 7/12/14: (on "result=0&{str}", which [DEV] ought to be treated as "result={0,str}", but not sure how...)
--          (same applies to "0&{str}&0&0&0", obviously, and that definitely cannot be opConcatN...)
--          (possibly issue "warning: potentially wasteful construct", under -lint or something?)
--                      --DEV 12/5:
--                      if mklen=1 and k=1 then
--                          -- replace x&{y} with append(x,y)
--                          opsidx = opsidxm1
--                          opstack[opsidx] = opApnd
--                          opTopIsOp = BltinOp
--                      else
                            for z=1 to mklen do
                                opsidxm1 -= 1
                                if opstype[opsidxm1]>T_atom then
                                    mklen = -1
                                    exit
                                end if
                            end for
                            if mklen>=0 then
                                opTopIsOp = 0
                                opsidx -= 2
                                k += mklen
                            else
                                k += 1
                            end if
--                      end if
                    else
                        k += 1
                    end if
                    if toktype!='&' then exit end if    -- (never exits on first iteration)
                    MatchChar('&',float_valid:=true)
                    -- aside: Expr(thisp+1,asBool) should be fine, but not tested...
                    Expr(pBitshift,asBool)  -- subexpression involving */+-<<>> but not &/rel/logicops
                end while
                if k=1 then
                    -- assume opApnd (alone) occurred
                elsif k=2 then
                    PushOp(opConcat,ConcatOp)
                else            
                    PushFactor(k,false,-1)  -- this is a count!
                    PushOp(opConcatN,ConcatOp)
                end if

            elsif thisp=pRelops         -- <, <=, =>, >
               or thisp=pEqorne then    -- ==, !=
                if usecmap then -- map eg compare(a,b)>=0 to a>=b?
                    compOp = 0
--23/10/16(!!)
if emitON then
                    if opTopIsOp=BltinOp and opstack[opsidx]=opScmp then
                        -- lhs is compare(x,y), check for <relop><literal int>
                        compOp = opsidx
                        opTopIsOp = 0
--                  elsif not opTopIsOp and opsltrl[opsidx] and opstype[opsidx]=T_integer then
--bugfix 19/10/09: (if emitON=0, compOp was getting left 0, which meant that in eg 1=2, this
--                  "ate" the 1, then left {2,Jne} on the stack, and ioob[0] in cmpchk..)
--1/11/2011:
--                  elsif not opTopIsOp and opsltrl[opsidx] and opstype[opsidx]=T_integer and opstack[opsidx]!=0 then
                    elsif not opTopIsOp and opsltrl[opsidx]=1 and opstype[opsidx]=T_integer and opstack[opsidx]!=0 then
                        -- lhs is <literal int>, check for <relop>compare(x,y)
                        compOp = -opstack[opsidx]
                        opsidx -= 1
                    end if
end if
                end if
                relopline = tokline
                relopcol = tokcol

                getToken(float_valid:=true)
                if toktype!='=' then
                    k = find(wastok,"< =  >")   -- one char ops
                    --10/4/2012 Allow <> to mean !=
                    if toktype='>' and k=1 then
                        MatchChar('>',float_valid:=true)
                        k = 4
                    end if
                else
                    k = find(wastok," <=!> ")   -- two char ops (<=,==,!=,>=)
                    MatchChar('=',float_valid:=true)
                end if
                if k=0 then Aborp("Unrecognised op") end if
                -- aside: Expr(thisp+1,asBool) should be fine, but untested...
                Expr(pConcat,asBool)            -- subexpression involving */+-<<>>& only

                if usecmap then -- map eg compare(a,b)>=0 to a>=b? [part 2]
                    if compOp>0 then    -- lhs was compare(x,y)
                        if not opTopIsOp and opsltrl[opsidx]=1 and opstype[opsidx]=T_integer then
                            -- ok, map to plain
                            -- (opstack is [a,b,opScmp,literal] --> [a,b,BranchOp])
                            compOp = opstack[opsidx]
                            opsidx -= 1
                            opsline[opsidx] = relopline
                            opstcol[opsidx] = relopcol
                            k = cmap(compOp,k,0)
                        else
                            -- oops! do that Scmp now then..
--27/8/2020:
                            saveFunctionResultVars(opsidx,INTSTOO)
                            if opTopIsOp then PopFactor() end if
                            if compOp!=opsidx-1 then ?9/0 end if
                            opsidx = compOp
                            opTopIsOp = BltinOp
                            PopFactor()
                            -- ..and relocate the (PopFactor'd) rhs from 5 lines up.
                            opsidx += 1
                            compOp += 1
                            thisp = opstack[compOp]     opstack[opsidx] = thisp
                            thisp = opstype[compOp]     opstype[opsidx] = thisp
                            thisp = opsltrl[compOp]     opsltrl[opsidx] = thisp
                            thisp = opsline[compOp]     opsline[opsidx] = thisp
                            thisp = opstcol[compOp]     opstcol[opsidx] = thisp
--validate_opstack()
                            -- <plus a PushOp which occurs below>
                        end if
                    elsif compOp<0 then -- lhs was literal int
                        compOp = 0-compOp
                        k = trev[k]     --  (eg 1<xxx becomes xxx>1)
                        if opTopIsOp=BltinOp and opstack[opsidx]=opScmp then
                            -- ok, map to plain
                            -- (opstack is [a,b,opScmp] --> [a,b,BranchOp])
                            opTopIsOp = 0
                            opsline[opsidx] = relopline
                            opstcol[opsidx] = relopcol
                            k = cmap(compOp,k,-1)

--1/11/2011:
--                      elsif not opTopIsOp and opsltrl[opsidx] and opstype[opsidx]=T_integer then
                        elsif not opTopIsOp and opsltrl[opsidx]=1 and opstype[opsidx]=T_integer then
                            -- <literal int><relop><literal int> - do it now...

                            --7/3/2010:
                            if emitON then
                                N = opstack[opsidx]
                                lhsli = symtab[compOp][S_value]
                                rhsli = symtab[N][S_value]
                            
                                -- reminder:
                                -- Scde = {opSlt,opSle,opSeq,opSne,opSge,opSgt}
                                --           1,    2,    3,    4,    5,    6
                                -- trev = {  6,    5,    3,    4,    2,    1}

                                if    k=1 then  k = (rhsli<lhsli)
                                elsif k=2 then  k = (rhsli<=lhsli)
                                elsif k=3 then  k = (rhsli=lhsli)
                                elsif k=4 then  k = (rhsli!=lhsli)
                                elsif k=5 then  k = (rhsli>=lhsli)
                                elsif k=6 then  k = (rhsli>lhsli)
                                else            k = 9/0
                                end if
                                if k then
                                    k = T_const1
                                else
                                    k = T_const0
                                end if
                                opstack[opsidx] = k
                            end if
                            k = 0

                        else
                            -- oops! put the literal back (with inverted condition)
                            PushFactor(compOp,true,T_integer)
                        end if
                    end if
                    if k then
                        PushOp(Bcde[k],BranchOp)    -- Bcde is {opJlt,opJle,opJeq,opJne,opJge,opJgt}
                    end if
                else -- (not usecmap)
                    PushOp(Bcde[k],BranchOp)    -- Bcde is {opJlt,opJle,opJeq,opJne,opJge,opJgt}
                                                --    i.e. { "<" ,"<=", "=(=)","!=",">=" ,">" }
                    -- Aside: The '=' in two char ops above allows "==", so eg the assignment
                    --        statements "a = b == c" and "a = b = c" are equivalent, but say
                    --        "a == b" and "a == b = c" (as standalone assignment statements)
                    --        are invalid. Prohibit "==" by spaceing the '=' from two char ops; 
                    --        prohibit single "=" meaning "equal()" by """ in one char ops.
                    --        (Untested/obviously breaks backward compatibility/legacy code.)
                end if

            else    -- pBitshift ( << and >> ), pBitops ( && and || )

                getToken(float_valid:=true)
                integer op = ZZopcodes[k]
                if op=opPow then -- pBitshift
                    PushFactor(addUnnamedConstant(2, T_integer),true,T_integer)
                end if
                Expr(thisp+1,asBool)    -- subexpression of higher precedence only
                PushOp(op,BltinOp)
                if op=opPow then
                    PushOp(iff(ZZops[k]='<'?opMul:opDivf),MathOp)
                end if
            end if

        else
            if toktype!=LETTER then exit end if
            if p>pLogop then exit end if
            k = find(ttidx,T_andorxor)
            if k=0 then exit end if
            noofbranches = 2    -- prevent flip
            LogicTok = ttidx -- no mixed expressions
            N = 0
            while 1 do
                if LogicTok=T_and then
--trace(1)
                    --
                    -- jump_not to to failpoint of "and", eg
                    -- while x and y do => Jnot <endwhile> x, or
                    -- while not x and y do => Jif <endwhile> x
                    --  (the "y" parts get done later, in DoWhile)
                    --
                    exprBP = Branch(Invert,1,exprMerge,exprBP)
                    -- backpatch scBP (succeed) to here:
                    if scBP>wasScBP then
                        scBP = backpatch(scBP,wasScBP,scMerge)
                    end if
                    oktoinit = 0
                elsif LogicTok=T_or then
                    all_and = 0
                    --
                    -- jump to yespoint of "or", eg
                    -- while x or y do ==> Jif <do> x
                    --  (the "y" part gets done later, in DoWhile)
                    --
                    scBP = Branch(NoInvert,1,scMerge,scBP)
                    -- backpatch exprBP (fail) to here:
                    if exprBP>wasExprBP then
                        exprBP = backpatch(exprBP,wasExprBP,exprMerge)
                    end if
                    oktoinit = 0
                else
                    if LogicTok!=T_xor then ?9/0 end if
                    all_and = 0
                    if opTopIsOp then PopFactor() end if
                    sqline = tokline
                    sqcol = tokcol
                end if
                MatchString(LogicTok,float_valid:=true) -- "and", "or", or "xor"

                if toktype=HEXDEC then
                    -- allow #istype/#isinit mid-expression (for testing, doh!)
                    getToken()
                    if    ttidx=T_istype then istype()
                    elsif ttidx=T_isinit then isinit()
                    else Aborp("#istype, or #isinit expected")
                    end if
                end if

--              Expr(pBitops,false) -- a sub-expression with */+-&<>=! etc, but not and/or/xor.
                Expr(pEqorne,false) -- a sub-expression with */+-&<>=! etc, but not and/or/xor.

                if LogicTok=T_xor then
                    if opTopIsOp then PopFactor() end if
                    if sqopNeeded(2) then
                        p2jssqv("sq_xor",sqline,sqcol)
                        sqopNo = sqAble[opXor]
                        sig = symtab[sqopNo][S_sig]
                        Call(sqopNo,sig,FUNC,true)
                        opstype[opsidx] = T_sequence
                    else
                        PushOp(opXor,LogicOp)
                    end if
                end if
                if toktype!=LETTER or ttidx!=LogicTok then
                    exit
                end if
            end while
            if toktype=LETTER then
                k = find(ttidx,T_andorxor)
                if k then
                    Aborp('\"'&Z_andorxor[k]&"\" ambiguous: add parenthesis")
                end if
            end if
        end if
    end while
    if toBool
    and (scBP>wasScBP or exprBP>wasExprBP) then
        BN = makeBool(toBool,wasScBP,wasExprBP,BN)
    end if
end procedure

--procedure MarkWritten(integer N)
--integer state
--sequence symtabN
--  symtabN = symtab[N]
--  symtab[N] = 0
--  state = symtabN[S_State]
----    if and_bits(state,S_for) then
----        Aborp("for loop variable may not be assigned")
----    els
--  if not and_bits(state,S_set) then
----        symtabN[S_State] = state+S_set
--      state += S_set
--      symtabN[S_State] = state
--  end if
--  if symtabN[S_Init]=0 then
--      symtabN[S_Init] = Ichain
--      Ichain = N
--  end if
--  symtab[N] = symtabN
--end procedure

constant compoundlongs={"add","sub","div","mul","concat","orbits","andbits"},
         compoundops = {opAdd,opSub,opDiv,opMul,opConcat,opOrBits,opAndBits},
         compoundtypes = {MathOp,MathOp,MathOp,MathOp,ConcatOp,BltinOp,BltinOp}

integer ntype
        ntype = 0

--DEV rename/recode as needsTypeCheck(lhstype,rhstype) [swap params]
--  (but leave the old code available at the flick of a constant
--   for at least 12 months, maybe even longer...)
function mergeType2(integer ntype, integer ptype)
--  if flag=TYPECHECK then
integer xtype

    --
    -- called from Assignment: "do we need to typecheck after this?"
    --  (test is: if Type!=mergeType(RHStype,Type,TYPECHECK) then)
    --  (btw, if ntype==ptype we just return ntype unaltered)
    --
    xtype = ntype
    while 1 do
        if xtype>ptype and xtype>T_object then
            xtype = symtab[xtype][S_sig][2] -- ie t2->t1 in "type t2(t1 x)".
            if xtype=ptype then             -- if RHS is superclass of LHS then
                ntype = ptype               --  avoid typecheck. eg int a, tI b, 
                exit                        --  then a:=b needs no typecheck.
            end if
        else
            if ptype<=T_object              -- for builtins (LHS&RHS) only,
            and xtype<=T_object
            and xtype=and_bits(xtype,ptype) then    -- no 1's in xtype(RHS) are 0 in ptype(LHS)
                -- ie/eg <obj>:=<int> OK ( 1 == 1 && 15 )
                --       <int>:=<obj> not ( 15 != 15 && 1 )
                ntype = ptype                       -- skip typecheck then
            end if
            exit
        end if
    end while
    return ntype
end function


function mergeType3(integer ntype, integer ptype)
--  elsif flag=NEWLTYPE then
    --
    -- called from Assignment: "determine best possible new ltype"
    -- eg/ie ntype=mergeType(RHStype,Type,NEWLTYPE)
    --
integer xtype

    if ntype>T_object then
        while ptype>ntype and ptype>T_object do
            ptype = symtab[ptype][S_sig][2]
        end while
        if ptype<=T_object then
            xtype = rootType(ntype)
            if xtype<ptype then
                ntype = xtype
            end if
        end if
    end if

    return ntype
end function

--without trace
--with trace
integer fromTopDecls = 0

procedure Assignment(integer tidx, integer Type)--, integer onDeclaration)
-- Parse and Translate an Assignment Statement
integer CompoundAssignment
integer subscript, sstokline, sstokcol
integer noofsubscripts
integer wasMapEndToMinusOne
integer savetokline, savetokcol, eqline, eqcol
integer osi
integer tvar, w
--,t
--, t1
sequence symtabN
--integer circular
integer snNtyp, state
--DEV tmp:
integer lprev
integer rtype
integer statemod
integer LHStype
sequence idii
bool wasdot = false,
     wasfromsubss = fromsubss
sequence struct_fields = {}
integer pstype, etype, petype, fN, s, const

    new_struct = Type

    if DEBUG then
        if class_def=T_const0 then  -- (26/12/19: allow implicit "this")
            if opsidx!=0 then ?9/0 end if
        end if
    end if
--p2js: (erm, undone without further testing, due to "= {}" ten lines down...)
    symtabN = symtab[tidx]
--  symtabN = deep_copy(symtab[tidx])
    snNtyp = symtabN[S_NTyp]
    if snNtyp=S_Const then
        Aborp("may not change the value of a constant")
    else
        state = symtabN[S_State]
        if and_bits(state,S_for) then
            Aborp("may not change the value of a for loop control variable")
        end if
    end if
    symtabN = {}
    CompoundAssignment = 0
    subscript = 0
    noofsubscripts = 1
--  rtype = rootType(Type)
    rtype = Type
    if rtype>T_object then rtype = rootType(rtype) end if
--?{"Assignment line 8186, toktype",toktype&""}
    if toktype='['
    or (ORAC and toktype='.') then
--4/11/19 (structs)
        etype = symtab[tidx][S_ltype]
--1/2/21:
--      bool bStruct = find(etype,stids)!=0
        bool bStruct = find(etype,stids)!=0 or etype=struct
--      if not bStruct and not and_bits(rtype,T_sequence) then
        if not and_bits(rtype,T_sequence) then
            -- eg/ie int[i]=o
            Aborp("attempt to subscript an atom (assigning to it)")
        end if
        if snNtyp=S_GVar2 then
            VAmask = power(2,remainder(tidx,29))
        end if
        wasMapEndToMinusOne=mapEndToMinusOne
        while 1 do
            mapEndToMinusOne = 1
            if ORAC and toktype='.' then
                MatchChar('.',float_valid:=false)
                sstokline = tokline
                sstokcol = tokcol
                wasdot = true
                if bStruct then
                    if toktype!=LETTER then 
                        Aborp("invalid")
                    end if
                else
                    fromsubss = true
                    GetFactor(false)
                    fromsubss = wasfromsubss
                end if
            else
                wasdot = false
                MatchChar('[',float_valid:=true)
                sstokline = tokline
                sstokcol = tokcol
--              if not bStruct then
                    Expr(pAllops,asBool)
--              end if
            end if
            if bStruct then
--              if toktype!=LETTER and toktype!=DQUOTE then
--X             if toktype!=LETTER and (wasdot or toktype!=DQUOTE) then
--X             if wasdot and toktype!=LETTER then
--                  Aborp("invalid")
--              end if
--              if class_def=T_const0 then  -- (26/12/19: allow implicit "this")
----                    if opsidx!=0 then ?9/0 end if
--                  if opsidx!=0 then Aborp("?9/0") end if
--              end if
                if length(struct_fields) then
--                  PushFactor(struct_fields[$][1],false,T_sequence)    -- (varno/struct)
--                  integer fN = addUnnamedConstant(struct_fields[$][2],T_string)
                    {s,fN,const} = struct_fields
                    PushFactor(s,false,T_sequence)                      -- (varno/struct)
                    PushFactor(fN,const,T_string)                       -- (field_name)
                    PushFactor(class_def,true,T_integer)                -- (context)
                    Call(T_fetch_field,{FUNC,T_sequence,T_string,T_integer},FUNC,true)
--6/6/20: (no help)
--opstack[opsidx] = etype
--?{"line 8203",tokline,opstack[opsidx],etype}
--                  if opsidx!=1 then ?9/0 end if
                end if
--              string field_name = trim(text[tokcol..col-1],`"`)
                string field_name
                if wasdot then
                    field_name = text[tokcol..col-1]
                    fN = addUnnamedConstant(field_name,T_string)
                    struct_fields = {tidx,fN,true}
                else
                    if opTopIsOp then PopFactor() end if
                    const = opsltrl[opsidx]
                    if const and opstype[opsidx]!=T_string then
                        Aborp("invalid (string field name expected)")
                    end if
                    struct_fields = {tidx,opstack[opsidx],const}
                    opsidx -= 1
                end if
                pstype = structs:get_struct_type(etype)
--              struct_fields = {{tidx,field_name}}
                if opsidx=1 then
                    tidx = opstack[opsidx]
                    opsidx -= 1
                end if
                if get_struct_type(etype)=S_CFFI then
                    bStruct = false
                else
                    if wasdot then
                        petype = etype
                        etype = structs:get_field_type(etype,field_name)
                        if etype=T_integer and Ch='(' then
--                          if length(struct_fields)!=1 then ?9/0 end if
--                          PushFactor(struct_fields[$][1],false,T_sequence)    -- (varno/struct)
--                          integer fN = addUnnamedConstant(struct_fields[$][2],T_string)
                            {s,fN,const} = struct_fields
                            PushFactor(s,false,T_sequence)                      -- (varno/struct)
                            PushFactor(fN,const,T_string)                       -- (field_name)
                            PushFactor(class_def,true,T_integer)                -- (context)
                            Call(T_fetch_field,{FUNC,T_sequence,T_string,T_integer},FUNC,true)
                            if opsidx!=1 then ?9/0 end if

                            getToken()
                            PushFactor(tidx,false,petype)
                            DoSequence('(',1)
                            SideEffects = or_bits(SideEffects,E_all)
                            -- save eax if rqd
                            saveFunctionResultVars(opsidx,NOTINTS)

                            if opTopIsOp then PopFactor() end if
                            if opsidx!=2 then ?9/0 end if
                            integer p1 = opstack[1],
                                    p2 = opstack[2]
                            if not symtab[p1][S_Init] then
                                Unassigned(p1)
                            end if
                            if not symtab[p2][S_Init] then
                                Unassigned(p2)
                            end if
                            agcheckop(opCallProc)
                            apnds5({opCallProc,p1,p2})
                            freeTmp(-opsidx)
                            new_struct = 0
                            return
                        end if
                        bStruct = find(etype,stids)!=0
                        new_struct = iff(bStruct?etype:0)
                    else
                        bStruct = false
                        new_struct = 0
                    end if
                end if
                if wasdot then
                    getToken()
                else
                    MatchChar(']')
                end if
                if not find(toktype,".[") then exit end if
            else
                if toktype=ELLIPSE
                or (ORAC and not wasdot and toktype=LETTER and ttidx=T_to) then
                    getToken(float_valid:=true)
                    Expr(pAllops,asBool)
--DEV replace x[..length(x)] with x[..-1] as per DoSubScripts()...?
                    subscript = SliceOp
                else
                    subscript = SubscriptOp
                end if
                if toktype=',' and subscript=SubscriptOp and ((not ORAC) or (not wasdot)) then
                    toktype = '['
                else
                    mapEndToMinusOne = 0
                    if not ORAC or toktype!='.' then
                        if not wasdot then
                            MatchChar(']')
                        end if
                        mapEndToMinusOne = wasMapEndToMinusOne
                        if subscript=SliceOp then exit end if
                        if toktype!='[' then exit end if
                    end if
                end if
                if rtype=T_string then Aborp("attempt to subscript an atom (char of string)") end if
                noofsubscripts += 1
            end if
        end while
-- Added 9/1/9:
        if opTopIsOp then
            isSubscript = 1
            PopFactor()
            isSubscript = 0
        end if
    end if
    eqline = tokline
    eqcol = tokcol

--14/07/20
--  if Ch='=' then -- compound assignments
    if find(Ch,"=&|") then -- compound assignments
--      if bStruct then Aborp("illegal") end if
--DEV?? s.a += s.b??
        new_struct = 0
--trace(1)
--      if subscript=SliceOp then
--          --DEV replace with sq_op?
--          Aborp("sequence operations not supported")
--      end if
        CompoundAssignment=find(toktype,"+-/*&|&")
        if CompoundAssignment then

--7/11/17(!):
            symtab[tidx][S_State] = or_bits(symtab[tidx][S_State],S_used)

--14/07/20
            if toktype='&' and Ch='&' then
                CompoundAssignment += 2
--              getCh()
                MatchChar('&')
                if toktype!='&' then Aborp("&&= expected") end if
            elsif toktype='|' then -- (nb there is no |= op)
                MatchChar('|')
                if toktype!='|' then Aborp("||= expected") end if
            end if

            if CompoundAssignment=5 -- (ie toktype was '&')
            and subscript=SliceOp then
                --
                -- [DEV open to discussion]
                -- It is simply not clear what such an operation should do.
                --  (On RDS Eu (except for &={}) it always yields run-time 
                --   error "lengths do not match on assignment to slice".)
                -- Two candidates (for s[i..j] &= xxx) exist:
                --  s[j+1..j] = xxx (simple insertion, common sensical, but,
                --                   it begs the question what is i doing?)
                --  for z=i to j do s[z]&=xxx end for (logically better fit,
                --                   but not perhaps immediately intuitive.)
                -- OK, so I favour the latter, but not about to implement
                --  anything without real-world use/need being proven.
                --
                Aborp("illegal/meaningless/ambiguous operation ( s[i..j] &= xxx )")
            end if
            if length(struct_fields)!=0 then
                if subscript then
                    -- doable, probably, but I think we want (eg) s.count += 1 first!
                    Aborp("unsupported operation ( s.f &= xxx )")
                end if
--6/6/20:
--?9/0 -- tbc (fetch tidx [loop?] - see T_store_field 50 lines below, but T_fetch_field)
--/*
                if length(struct_fields)!=3 then ?9/0 end if
                {s,fN,const} = struct_fields
                PushFactor(s,false,T_sequence)                      -- (varno/struct)
                PushFactor(fN,const,T_string)                       -- (field_name)
                PushFactor(class_def,true,T_integer)                -- (context)
                Call(T_fetch_field,{FUNC,T_sequence,T_string,T_integer},FUNC,true)
opstype[opsidx] = etype
--?{"line 8363",tokline,opstype[opsidx],etype}
--*/
--??? do/should we zero CompoundAssignment here ???
            else
                PushFactor(tidx,false,rtype)
                if subscript then
                    if subscript=SubscriptOp then
                        for i=1 to noofsubscripts do
                            PushFactor(opstack[i],opsltrl[i]=1,opstype[i])
                        end for
                        PushSubOp(opSubse,SubscriptOp,noofsubscripts)
                        if noofsubscripts=1 then    -- NB while/since tmptransfer only looks at opSubse1[i][p/s]...
                            isCompound=1    -- in eg s[i+j] += 1, force store of tmp (for i+j, as it is used twice)
                        end if
                    else -- SliceOp
                        for i=1 to noofsubscripts+1 do
                            PushFactor(opstack[i],opsltrl[i]=1,opstype[i])
                        end for
                        PushSubOp(opSubss,SliceOp,noofsubscripts)
                    end if
                    PopFactor()     -- gonna happen anyway...
                end if
            end if
            MatchChar(toktype)
        end if
    end if
-- added 6/4/2012 (allow ":=" as well as "=")
    if toktype=':' and Ch='=' then MatchChar(':',false) end if
    MatchChar('=',float_valid:=true)

--  if (not CompoundAssignment) and (not subscript) then
--      MarkWritten(TableEntry)
--  end if
--if tokline=89 then trace(1) dbg = symtab[397] end if

    fastSubscriptLHS = 0

--DEV incorrect: this makes eg "s.tables[IDS] &= id" impractical, may need some duplicating...
if length(struct_fields) then
--if 0 then
--?"pmain.e line 8333 - needs logic restructure"
        --DEV (need to fetch/modify/store)
--      if CompoundAssignment then
--6/6/20
--          Aborp("not yet supported")
--?"line 8402:Aborp(\"not yet supported\")"
--/* maybe... [NO]
            PushFactor(tidx,false,T_Dsq)                            -- (varno/struct)
            {s,fN,const} = struct_fields
            PushFactor(fN,const,T_string)                           -- (field_name)
>>
                if length(struct_fields)!=3 then ?9/0 end if
                {s,fN,const} = struct_fields
                PushFactor(s,false,T_sequence)                      -- (varno/struct)
                PushFactor(fN,const,T_string)                       -- (field_name)
                PushFactor(class_def,true,T_integer)                -- (context)
                Call(T_fetch_field,{FUNC,T_sequence,T_string,T_integer},FUNC,true)
--*/
--      elsif subscript then
        if subscript then
--      if subscript then
            tokline = sstokline
            tokcol = sstokcol
            Aborp("not yet supported")
        end if
        if opsidx!=0 then ?9/0 end if
        PushFactor(tidx,false,T_Dsq)                            -- (varno/struct)
        {s,fN,const} = struct_fields
--      PushFactor(s,false,T_sequence)                          -- (varno/struct)
        PushFactor(fN,const,T_string)                           -- (field_name)
        if CompoundAssignment then
--      PushFactor(tidx,false,T_Dsq)                            -- (varno/struct)
            if length(struct_fields)!=3 then ?9/0 end if
--          {s,fN,const} = struct_fields
            PushFactor(s,false,T_sequence)                      -- (varno/struct)
            PushFactor(fN,const,T_string)                       -- (field_name)
            PushFactor(class_def,true,T_integer)                -- (context)
            Call(T_fetch_field,{FUNC,T_sequence,T_string,T_integer},FUNC,true)
            opstype[opsidx] = etype
--?{"line 8448",tokline,opstype[opsidx],etype}
        end if
        Expr(pAllops,asBool)                                    -- (v)
        if CompoundAssignment then
            PushOp(compoundops[CompoundAssignment],compoundtypes[CompoundAssignment])
--?{"line 8437",tokline,CompoundAssignment,compoundops[CompoundAssignment],compoundtypes[CompoundAssignment]}
--else
--?{"line 8439",tokline,CompoundAssignment}
        end if
        if opTopIsOp then PopFactor() end if
        if opsidx!=3 then ?9/0 end if
--      integer rep = opstack[opsidx]
        --erm... (because Call() is not expected on lhs of assignment...)
        VAmask = 0      -- (... as per the same below/before rhs)
        PushFactor(class_def,true,T_integer)                    -- (context)
        Call(T_store_field,{PROC,T_sequence,T_string,T_object,T_integer},PROC,true)
else
    if CompoundAssignment
    or subscript then
--fail:
--      if length(struct_fields) then
--          PushFactor(tidx,false,T_Dsq)                            -- (varno/struct)
--          {s,fN,const} = struct_fields
--          PushFactor(fN,const,T_string)                           -- (field_name)
--      end if
        if not CompoundAssignment
        and length(struct_fields)=0
        and (ttidx=T_append or ttidx=T_prepend) then
--          if length(struct_fields) then Aborp("not supported") end if -- (erm, can this ever happen?!)
            -- Try to optimise the s[i]=append(s[i],xxx) case, by zeroing
            --  s[i] over the opApnd(/opPpnd) when it is safe to do so.
            -- Effectively it becomes:
            --      tmp = s[i]
            --      s[i] = 0        (<-- this is the only new bit)
            --      tmp = append(tmp,xxx)
            --      s[i] = tmp
            -- (of course should xxx look sideways at s, all bets are off)
            -- Note there is still some "unnecessary" ref counting going on
            -- here; it is the clone we avoided. An incref has already been
            -- done and the decref(s[i]) and incref(tmp) are yet to come.
            -- (A more general fix than just this specific case is sought!)
            fastSubscriptIcount = opsidx
            fastSubscriptLHS = tidx
        end if
        Expr(pAllops,asBool)
        if fastSubscriptLHS=tidx then
            --DEV 22/10/09:
            ------if opTopIsOp then PopFactor() end if
if emitON then -- added 18/6/10
--trace(1)
            if opstack[opsidx-1]!=tidx then
                -- kill refcount on original element:
                -- DEV(maybe never) or patch some opSubse[1][s/p] to not incref?
                if newEBP then
                    -- save eax if rqd
                    saveFunctionResultVars(opsidx,NOTINTS)
                end if
-- 6/6/14:
                if not symtab[tidx][S_Init] then
                    Unassigned(tidx)
                end if
                if noofsubscripts=1 then
                    --
                    -- opRepe1, ref[idx]=rep.
                    --
                    p2 = opstack[1]
                    if not symtab[p2][S_Init] then
                        Unassigned(p2)
                    end if
if newEmit then
                    agcheckop(opRepe1)
end if
                    apnds5({opRepe1,tidx,p2,T_const0})          -- ref[idx]=0 -- opRepe1,dest,idx,rep
                else -- noofsubscripts!=1
                    --
                    -- opRepe is n (noofsubscripts), rep, idxn..idx1, ref
                    --      implements ref[idx1][idx2]~[idxn] := rep
                    -- see also opRepe1
                    --
                    idii = {}
                    for j=noofsubscripts to 1 by -1 do
                        p1 = opstack[j]
                        if not symtab[p1][S_Init] then
                            Unassigned(p1)
                        end if
                        idii &= p1
                    end for
if newEmit then
                    agcheckop(opRepe)
end if
                    apnds5({opRepe,noofsubscripts,T_const0})    -- ref[idx1]..[idxn]=0
                    idii &= tidx
                    apnds5(idii)
                end if
            end if
end if -- emitON
        end if
    else
--      Expr(pAllops,tidx)
-- 28/9/9: (see DEV comments in plist.e (also dated 28/9/9 - retest once fully re-self-hosted))
-- (OR: set a special flag in here for makeBool to dealloc (ie emit opMovsi instead of opMovbi),
--      clear "" both at end of makeBool [for nested jobbies] and here [in case makeBool was
--      not actually called]. Hows about mBopCode: usually opMovbi, but can be set to opMovsi 
--      for proper handling/dealloc in eg "<object> = (x and/or y)" cases, see Assignment.)
        if rtype=T_integer then
--?{"Assignment line 8606"}
--trace(1)
            Expr(pAllops,tidx)
            if opsidx=1 and opstack[opsidx]=tidx then
                opsidx = 0
                symtabN = symtab[tidx]  -- (needed below)
            end if
        else
            if pbrON then
                if snNtyp=S_TVar then   -- locals only!
--trace(1)
                    lhsvar = tidx
                    lhspos = 0
                end if
            end if
            Expr(pAllops,asBool)
            if pbrON then
                if lhsvar then
--added 2/6/14:
                    for j=1 to opsidx do
                        if opstack[j]=lhsvar then
                            lhspos = 0
                            exit
                        end if
                    end for
                    if lhspos then
                        if s5[lhspos]!=1 then ?9/0 end if
                        s5[lhspos] = 2  -- onDeclaration (/pbr)
                    end if
                    lhsvar = 0
                end if
            end if
        end if
--ie (untried)
--      if rtype!=T_integer then
--          mBopCode = opMovsi  -- (causes makeBool to dealloc)
--      end if
--      Expr(pAllops,tidx)
--      if opsidx=1 and opstack[opsidx]=tidx then
--          if mBopCode!=opMovbi then   -- hmmm...
--              Warn("makeBool internal error (or x=x statement?)",eqline,eqcol,0)
--          end if
--          opsidx = 0
--          symtabN = symtab[tidx]  -- (needed below)
--      end if
--      mBopCode = opMovbi      -- (in case makeBool not called)
    end if

    VAmask = 0

    if CompoundAssignment then
        if onDeclaration then
            Abork("compound op on declaration",opsidx)
        end if
        if CompoundAssignment=5 then    -- (ie toktype was '&')
--trace(1)
--          if opTopIsOp=MkSqOp then
--              if opsidx=4 and opstack[3]=1 then
--          if opTopIsOp=MkSqOp and opsidx=4 and opstack[3]=1 and opstype[2]<=T_atom then
---- don't think this can happen...
--trace(1)
--                  -- replace x &={z} with x &= z
--                  -- ie x,[z,1,opMkSq],opConcat -> x,z,opConcat
--                  opsidx = 3
--                  opstack[3] = opConcat
----DEV: BUG: this only works if x is an atom...
----                else
----                    -- replace x&={a1,a2..aN} with x=x&a1&a2...&aN
----                    -- ie x,[a1,a2..aN,N,opMkSq],opConcat -> x,a1,a2..aN,N+1,opConcatN
----                    opsidxm1 = opsidx-1
----                    opstack[opsidxm1] = opstack[opsidxm1]+1
----                    opstack[opsidx] = opConcatN
----                end if
--              opTopIsOp = ConcatOp
-- new 18/2/9:
--          if 0 then
            if opTopIsOp=ConcatOp then
                if opstack[opsidx]=opConcat then
                    -- replace x &= y&z with x = x&y&z
                    -- ie x,[y,z,opConcat] -> x,y,z,3,opConcatN ( & StoreVar x )
                    opTopIsOp = 0
                    opsidx -= 1
--erm? (pushSubOp?)
--                  PushFactor(3,true,T_integer)
                    PushFactor(3,false,-1)      -- this is a count!
                    PushOp(opConcatN,ConcatOp)
                else
                    -- replace eg w &= x&y&z with w = w&x&y&z
                    -- ie w,[x,y,z,3,opConcatN] -> w,x,y,z,4,opConcatN ( & StoreVar w )
                    w = opstack[opsidx-1]+1
                    opstack[opsidx-1] = w
                    -- (btw: opstack[i-1]+=1 screws up ginfo of sequence of integer;
                    --       it becomes sequence of atom, which we'd rather avoid.)
--validate_opstack()
                end if
--DEV 12/5:
            elsif opTopIsOp=MkSqOp and opsidx=4 and opstack[3]=1 then
--              -- replace x &={z} with x = append(x,z)
                opsidx -= 1
                opstack[opsidx] = opApnd
--validate_opstack()
                opTopIsOp = BltinOp
            else
                PushOp(opConcat,ConcatOp)
            end if
        else
            if sqopNeeded(2) then
                string clca = "sq_"&compoundlongs[CompoundAssignment]
                p2jssqv(clca,eqline,eqcol)
                sqopNo = sqAble[compoundops[CompoundAssignment]]
                Call(sqopNo,symtab[sqopNo][S_sig],FUNC,true)
                opstype[opsidx] = T_sequence
            else
--14/7/20
--              PushOp(compoundops[CompoundAssignment],MathOp)
                PushOp(compoundops[CompoundAssignment],compoundtypes[CompoundAssignment])
            end if
        end if
--6/6/20: (undone)
if length(struct_fields) then ?9/0 end if   -- missing code? [copy from above]
--fail:
--if length(struct_fields) then
--      if subscript then ?9/0 end if   -- ???
--      if opTopIsOp then PopFactor() end if
--      if opsidx!=3 then ?9/0 end if
--?     integer rep = opstack[opsidx]
--      --erm... (because Call() is not expected on lhs of assignment...)
--      VAmask = 0      -- (... as per the same below/before rhs)
--      PushFactor(class_def,true,T_integer)                    -- (context)
--      Call(T_store_field,{PROC,T_sequence,T_string,T_object,T_integer},PROC,true)
--      return
--end if
    end if
    RHStype = -1
    if subscript then
        if onDeclaration then
            Abork("subscript on declaration",opsidx)
        end if
--      if Type=T_sequence then
--DEV:?
--      if Type=T_sequence or Type=T_Dsq then
--      if and_bits(Type,T_Dsq) then
        if and_bits(rtype,T_Dsq) then
            RHStype = T_sequence    -- Type?
--      elsif Type=T_string and not opTopIsOp then
        elsif rtype=T_string and not opTopIsOp then
            w = opstype[opsidx]
            if w=T_string and subscript=SliceOp then
                -- string[a..b]=string
                RHStype = T_string
--DEV 1/11/2011:
--          elsif w=T_integer and opsltrl[opsidx] then
            elsif w=T_integer and opsltrl[opsidx]=1 then
                if emitON then -- (else opstack[opsidx] may be 0)
                    w = symtab[opstack[opsidx]][S_value]
                    if w>=#07 and w<=#FF then
                        -- string[i]=char
                        RHStype = T_string
                    end if
                end if
            end if
        end if
        if opTopIsOp then PopFactor() end if
        --
        -- For circular references, copy into temp var now:
        -- (eg z[1][2]=z becomes tmp=z, z[1][2]=tmp)
        --
--if 0 then -- 16/11/09...
--if subscript=SliceOp then -- "" (oops!)
        -- opReps "replaces ref at orignal address" early on,
        --  so "manually" clone rep first.
-- (oops^2!) OK,OK: we need this. Although I fixed a problem
--  (namely mov eax,[refcount]/dec eax/inc [refcount]/mov [refount],eax)
--  we still test for refcount of 1 (to avoid clone/do things in situ)
--  before the incref... DEV reconsider this when/if opRepXX get moved
--  to hll/#ilasm{}.
--  if 1 then -- 16/11/09 new code:
        osi = opstack[opsidx]
        if osi=tidx then
            tvar = newTempVar(T_object,Shared)
            emitHexMov(opMove,tvar,osi)
            opstack[opsidx] = tvar
            opstype[opsidx] = T_object
            opsltrl[opsidx] = allowTempReuse -- mark for possible re-use
--validate_opstack()
        end if
--Added 16/11/09:
--      w = opstype[opsidx]
        w = symtab[tidx][S_ltype]
        if w>T_object then w = rootType(w) end if
        if not and_bits(w,T_sequence)
        or (noofsubscripts>1 and and_bits(w,T_sequence)=T_string) then
            Abork("type error (attempt to slice an atom)",1)
        end if
--  else -- pre-16/11/09 code:
----DEV if opRepe etc incref first, we can avoid this...
----    circular = 0
----DEV aren't opstack[1..opsix-1] indexes... can we get away with only checking opstack[opsidx]?
--      for i=opsidx to 1 by -1 do
--          osi = opstack[i]
--          if osi=tidx then
----DEV set flag, avoid opRepe1 inline below...
----    circular = 1
--              tvar = newTempVar(T_object,Shared)
--              emitHexMov(opMove,tvar,osi)
--              opstack[i] = tvar
--              opstype[i] = T_object
--              opsltrl[i] = allowTempReuse -- mark for possible re-use
--          end if
--      end for
--  end if
--end if

--trace(1)
--DEV:
--      opsidxm1 = opsidx-1
--      noofsubscripts = opstack[opsidxm1]
--      opsidx -= 2
--      w = opsidx-noofsubscripts
--      t = opstype[w-1]
--          if not and_bits(t,T_sequence) or (noofsubscripts>1 and t=T_string) then
--          Abork("type error (attempt to slice an atom)",w)
--      end if
--      RHStype = t
        for i=1 to opsidx-1 do
            w = opstype[i]
            if not and_bits(w,T_atom) then
                Abork("type error (subscript must be an atom)",i)
--DEV to replace the above loop?
--          else
--              osi = opstack[i]
--              if osi=tidx then
--                  -- eg z[z]=x !!
--                  Abork("type error (cannot be both sequence and index)",i)
--              end if
            end if
        end for

        if subscript=SliceOp then
            if DEBUG then
                if opsidx!=noofsubscripts+2 then ?9/0 end if
            end if
            if emitON then
if newEBP then
                -- save eax if rqd
                saveFunctionResultVars(opsidx,NOTINTS)
end if
                --
                -- opReps is n (noofsubscripts), rep, sliceend, idxn..1, ref
                --      (implements ref[idx1]~[idxn..sliceend] := rep)
                --
                idii = {}
                for j=opsidx to 1 by -1 do
                    p1 = opstack[j]
                    if not symtab[p1][S_Init] then
                        Unassigned(p1)
                    end if
                    idii &= p1
                end for
if newEmit then
                agcheckop(opReps)
                if not symtab[tidx][S_Init] then
                    Unassigned(tidx)
                end if
end if
                apnds5({opReps,noofsubscripts})
                idii &= tidx
                apnds5(idii)    -- rep,sliceend,idxn..idx1, ref
            end if -- emitON
--      else -- not sliceop
        elsif subscript=SubscriptOp then
            if DEBUG then
                if opsidx!=noofsubscripts+1 then ?9/0 end if
            end if
            if emitON then  -- (necessary optimisation; tmps may be 1)
if newEBP then
        -- save eax if rqd
        saveFunctionResultVars(opsidx,NOTINTS)
end if
--6/6/14:
                if not symtab[tidx][S_Init] then
                    Unassigned(tidx)
                end if
                if noofsubscripts=1 then
                    --
                    -- opRepe1, ref[idx]=rep.
                    --
                    p2 = opstack[1]
                    p3 = opstack[2]
                    if not symtab[p2][S_Init] then
                        Unassigned(p2)
                    end if
                    if not symtab[p3][S_Init] then
                        Unassigned(p3)
                    end if
if newEmit then
                    agcheckop(opRepe1)
end if
                    apnds5({opRepe1,tidx,p2,p3})    -- opRepe1,dest,idx,rep
--15/5/21:
                    zero_temp(p3)

                else -- noofsubscripts!=1
                    --
                    -- opRepe is n (noofsubscripts), rep, idxn..idx1, ref
                    --      implements ref[idx1][idx2]~[idxn] := rep
                    -- see also opRepe1
                    --
                    idii = {}
                    for j=opsidx to 1 by -1 do
                        p1 = opstack[j]                 -- rep,idxn..idx1
                        if not symtab[p1][S_Init] then
                            Unassigned(p1)
                        end if
                        idii &= p1
                    end for
if newEmit then
                    agcheckop(opRepe)
end if
                    apnds5({opRepe,noofsubscripts})     -- ref[idx1]..[idxn]=rep
                    idii &= tidx
                    apnds5(idii)
--15/5/21:
                    zero_temp(idii[1])
--                  zero_temp(idii[-2])
                end if
            end if  -- emitON
        else
            ?9/0    -- subscript=? (I don't think this can ever happen)
        end if
--      if opstype[1]=T_sequence then
--DEV?
        if and_bits(opstype[1],T_Dsq) then
            -- avoid pointless type check
            -- NB T_string checks are however necessary, eg str[5]=-1 => str is now a dseq.
            RHStype = T_sequence    -- T_Dsq?
        end if
        freeTmp(-opsidx)
--      opsidx = 0
        symtabN = symtab[tidx]
        symtab[tidx] = 0
    else -- (not subscript)
--if not onDeclaration then
--  puts(1,"(line 5760 in Assignment): killme!\n")
--  trace(1)
--end if
--
        statemod = S_set
        if opsidx=1 then
            RHStype = opstype[1]
--p2js: (gave up at this point, for now)
            symtabN = symtab[tidx]  -- get latest update, eg/ie setUsed in psym2.e
--          symtab[tidx] = 0
--          symtabN = deep_copy(symtab[tidx])   -- get latest update, eg/ie setUsed in psym2.e
            state = symtabN[S_State]
--added 28/04/2010:
            LHStype = symtabN[S_vtype]
            if LHStype>T_object then LHStype = rootType(LHStype) end if
--DEV:
-- (temp. removed 27/7/9 for ltype2.e debug reasons - being that with this
--          in place we get an ltAdd() but no corresponding opMove...)
--DEV put back in 2/10/9:
--DEV taken back out 2/4/10:
--          if opstack[1]=tidx then
----    statemod = 0
--              opsidx = 0
--          elsif not opTopIsOp and onDeclaration 
--DEV can you ever have opTopIsOp with opsidx=1?
--21/12/10:
--          if not opTopIsOp and onDeclaration 
            if not opTopIsOp and (onDeclaration or fromTopDecls)
            and RHStype=T_integer and opsltrl[1]=1
            and snNtyp=S_GVar2      --DEV t45aod...  (or no fwd calls outstanding) [umm, see below]
--30/12/14:
            and and_bits(symtab[opstack[1]][S_State],K_rtn)=0
--??        and (snNtyp=S_GVar2 or no_of_fwd_calls=0)
-- 28/04/2010:
--          and (symtabN[S_vtype]=T_integer or no_of_fwd_calls=0) then
--21/12/10:
--          and (LHStype=T_integer or (and_bits(LHStype,T_integer) and no_of_fwd_calls=0)) then
            and (LHStype=T_integer or (and_bits(LHStype,T_integer) and (no_of_fwd_calls=0 or fromTopDecls))) then
-- 30/10/2011:
--13/11/2011 [DEV]put back so integer x=0 gets unused error.. (need to rethink this)
                statemod = K_noclr
--              statemod = or_bits(statemod,K_noclr)
                symtab[tidx] = 0                -- kill refcount
                symtabN[S_value] = symtab[opstack[1]][S_value]
                opsidx = 0
            else
--DEV 15/6 (to be tried)
if not opTopIsOp and (onDeclaration or fromTopDecls)
and opsltrl[1]=1 then
-- 30/10/2011
--13/11/2011 [DEV] put back in sympathy with above
--19/11/2011 and flipped again due to a memory leak (in t37)
--  statemod = K_aod
--05/07/2013: (a hail mary)
--  statemod = or_bits(statemod,K_aod)
    statemod = or_bits(statemod,K_aod+K_used)
end if
                if exitBP=-1 -- not in a loop
--              and (TableEntry<0 or returnvar=1?)  -- Ah, Gvars can be set by (fwd) function calls...
--              and snNtyp=S_TVar
                and (snNtyp=S_TVar or (returnvar=-1 and no_of_fwd_calls=0)) -- [umm, think I meant this/t45aod]
                and not and_bits(state,S_set) then
                    onDeclaration = 1   -- no need to dealloc then
                end if
            end if
-- 29/04/2010: (DEV might not be the bet place for it)
        elsif opTopIsOp=BranchOp then
            w = opstack[opsidx]
            if w<opJge or w>opJle then ?9/0 end if
            symtabN = symtab[tidx]  -- get latest update, eg/ie setUsed in psym2.e
            LHStype = symtabN[S_vtype]
            if LHStype>T_object then LHStype = rootType(LHStype) end if
            if not and_bits(LHStype,T_integer) then
                --
                -- if //and only if// lhs cannot hold integer, map to sq_xxx()
                --
                if not and_bits(opstype[opsidx-1],T_atom)
                or not and_bits(opstype[opsidx-2],T_atom) then
                    opsidx -= 1 
                    opTopIsOp = 0
                    w -= (opJge-1)
                    ZZdesc = "sq_"&ZZjcc[w]
--                  if sqopWarn then
--                      Warn(ZZdesc&"() assumed",sqline,sqcol,SQ_WARN)
--DEV ^^ save the last/outer ones on exit from Expr?
                    Warn(ZZdesc&"() assumed",eqline,eqcol,SQ_WARN)
--                  end if
                    sqopNo = sq6[w]
                    Call(sqopNo,symtab[sqopNo][S_sig],FUNC,true)
                    opstype[opsidx] = T_sequence
                end if
            end if
        end if

        if opsidx then
            savetokline = tokline
            savetokcol = tokcol
            tokline = eqline
            tokcol = eqcol
            symtabN = {}    -- Avoid clone/confusion should [S_Init] (etc) be updated
--NESTEDFUNC... (PopFactor if we have to)
            StoreVar(tidx,Type)
            onDeclaration = 0
            tokline = savetokline
            tokcol = savetokcol
            symtabN = symtab[tidx]  -- grab fresh copy
            symtab[tidx] = 0    -- kill refcount
        end if
        state = symtabN[S_State]
        state = or_bits(state,statemod)
        symtabN[S_State] = state
    end if

    if symtabN[S_Init]=0 then
        symtabN[S_Init] = Ichain
        Ichain = tidx
    end if
    if snNtyp=S_GVar2 then
        SideEffects = or_bits(SideEffects,power(2,remainder(tidx,29)))
    elsif snNtyp=S_TVar then
        lMask = or_bits(lMask,power(2,remainder(tidx,29)))
    end if

--
-- Programming note:
-- =================
--  This code fragment may be as incomplete as it is indecipherable!
--  It seems to cover the basics, see t47ltth.exw before amending.
--
--4/4/20:
--  if subscript then
    if subscript
    or length(struct_fields) then
-- usetdsq?
        RHStype = T_sequence    -- treat a[i]=x as a=<seq/str>, rather than a=x.
        --DEV what about string[i]=char??? [would need to be literal char?]
    end if
    -- do we need to typecheck?
--  if Type!=mergeType(RHStype,Type,TYPECHECK) then
    if Type!=mergeType2(RHStype,Type) then
        --DEV new code 23/10/09:
        if 0 then
            w = optset[OptTypeCheck]
            ntype = Type
--DEV (spotted in passing 14/6/10) should this be w=0??!!
            if w then
                if ntype>T_object then ntype = rootType(ntype) end if
            end if
            if emitON and ntype!=T_object then
--  if newEmit then
                agcheckop(opTchk)
--  end if
                apnds5({opTchk,tidx,w,0})
            end if

        else
            if optset[OptTypeCheck]
            and emitON then
--  if newEmit then
                agcheckop(opTchk)
--  end if
                apnds5({opTchk,tidx,1,0})
            end if
        end if
        ntype = Type    -- if we typecheck, use that as the new localtype
    else
        -- determine best possible new ltype
--      ntype = mergeType(RHStype,Type,NEWLTYPE)
        ntype = mergeType3(RHStype,Type)
    end if
    lprev = symtabN[S_ltype]
    symtab[tidx] = symtabN
    symtabN = {}    -- (avoids clone in ltAdd)
    if NOLT=0 or bind or lint then
        if emitON then
--PL 6/3/17(??)
--          Lmin = MININT
--          Lmax = MAXINT
            ltAdd(SET,tidx,lprev,ntype,length(s5))
        end if
    end if -- NOLT
    --DEV if we have say:
    --  integer I
    --  object O
    --      I=O
    --  Then we can also deduce O has a localtype of integer.....
    --  We probably don't want to stomp on a better udt though,
    --   eg/ie if is_even_int(O) then
    --              I=O
    --   should leave O's localtype of is_even_int alone - and
    --   let's have "type is_even_int(integer x)" return false
    --   rather than crash should you pass it a sequence etc.
    --   However, note that is_even_int("string") would yet be
    --   correct in triggering a compilation error, btw.
end if -- (struct_fields)

    if DEBUG then
        if opsidx!=0 then ?9/0 end if
    end if
    new_struct = 0
end procedure
r_Assignment = routine_id("Assignment")

--DEV this is not quite right...
--constant T_endelseelsif = {T_end,T_else,T_elsif,T_elsedef,T_elsifdef,
--constant T_endelseelsif = {T_end,T_else,T_elsif,T_case,T_default,T_break}
constant T_endelseelsif = {T_end,T_else,T_elsif,T_case,T_catch,T_default,T_fallthru,T_fallthrough,T_until}
constant T_endelseelsifbreak = T_endelseelsif&T_break
--(one possible[spotted in passing]: fallthr(u|ough) missing?) -- (added 14/2/11)

-- for DoTry():
integer in_try = 0      -- exit/break/continue invalid when >=1...
integer loopage = 0,    -- (used only for setting/comparing against the following)
        try_loopage     -- ...and loopage==try_loopage (as at last in_try+=1)

procedure DoExit()
    if exitBP=-1 then Aborp("exit statement must be inside a loop") end if
    if in_try and loopage=try_loopage then
        Aborp("invalid (circumvents try handler reset)")
    end if
    MatchString(T_exit)
    if emitON then
        apnds5({opJmp,exitMerge,0,exitBP})
        exitBP = length(s5)
    end if
--  LastStatementWasReturn = 0
--  LastStatementWasExit = 1
--DEV unreachable code if next token is not T_end??
    if toktype=';' then
        getToken()
    end if
    if toktype!=LETTER or not find(ttidx,T_endelseelsifbreak) then
        --DEV or warning unreachable code, and
        --          emitON = 0
        Aborp("elsif, else, or end if expected.") -- (no point saying the other five)
    end if
end procedure

procedure DoBreak()
    if breakBP=-1 then Aborp("break statement must be inside a select") end if
    if in_try and loopage=try_loopage then
        Aborp("invalid (circumvents try handler reset)")
    end if
    MatchString(T_break)
    if emitON then
        apnds5({opJmp,breakMerge,0,breakBP})
        breakBP = length(s5)
    end if
    if toktype=';' then
        getToken()
    end if
    if toktype!=LETTER or not find(ttidx,{T_end,T_else,T_elsif,T_case,T_default}) then
        --DEV or warning unreachable code, and
        --          emitON = 0
        Aborp("elsif, else, case, default, or end if expected.")
    end if
end procedure

procedure DoContinue()
    if continueBP=-1 then Aborp("continue statement must be inside a loop") end if
    if in_try and loopage=try_loopage then
        Aborp("invalid (circumvents try handler reset)")
    end if
    MatchString(T_continue)
    if emitON then
        apnds5({opJmp,exitMerge,0,continueBP})
        continueBP = length(s5)
    end if
--  LastStatementWasReturn = 0
--  LastStatementWasExit = 1
--DEV unreachable code if next token is not T_end??
    if toktype=';' then
        getToken()
    end if
    if toktype!=LETTER or not find(ttidx,T_endelseelsifbreak) then
        --DEV or warning unreachable code, and
        --          emitON = 0
        Aborp("elsif, else, or end if expected.") -- ("")
    end if
end procedure


--without trace
--with trace
--procedure DoIf(integer flags)
procedure DoIf()
--
-- Recognize and translate an "if" construct
--
integer emitElse, wasEmit

integer ifBP, saveIchain
integer EndIfBP, tmp

sequence sytmp

integer elsevalid   -- initially 2, == first conditional test,
                    -- can 2=>3 for "if then return" handling,
                    -- then 1 until we find an "else",
                    -- then 0 to force "if else else" to error.
--integer prev

--integer ltype, ltvar
--object dbg

integer wasSideEffects
integer plain, testfor
integer iftop, ctrlink, ctrltyp
integer scode, wasEmit2
--trace(1)
    if opsidx then ?9/0 end if  -- leave in (outside if DEBUG then)
    EndIfBP = 0

--if traceif then trace(1) end if
    if exprBP!=0 then ?9/0 end if

    saveIchain = Ichain
    Ichain = -1
--
----DEV 14/7:
--  LastStatementWasReturn = 0 -- put back in 21/10
--  LastStatementWasExit = 0
--trace(1)
    MatchString(T_if,float_valid:=true)
--if fileno=1 and tokline=78 then trace(1) end if

    elsevalid = 2

    ctrlink = 0
--if NOLT=0 or bind or lint then
    if emitON then
        apnds5({opCtrl,IF,0,emitline})
        iftop = length(s5)-1    -- patched at/pointed to the end if
        ctrlink = iftop         -- where to point next elsif/else/endif
        if NOLT=0 or bind or lint then
            ltCtrl(iftop)
        end if -- NOLT
    end if
--end if -- NOLT

    wasSideEffects = SideEffects
    SideEffects = E_none
    wasEmit = emitON
    emitElse = emitON   -- (minor optimisation [cmp vs opJcc] 18/3/09)

--if fileno=1 then trace(1) end if
--oktoinit = 1  -- just done in Statement!
    while 1 do
--trace(1)
--dbg = symtab[416]
        if elsevalid then
            if exprBP!=0 then ?9/0 end if

            noofbranches = 0

--          if and_bits(flags,DLL_MAIN) then
----                get_fdwReason()
--              Matchstring(fdwReasonttidx)
--              MatchChar('=')
----                MatchString(T_DLLATTACH)
--              ?9/0
--              flags = 0
--          else
                Expr(pAllops,false) -- full, notBool/asIs
--          end if

            ifBP = 0
            if exprBP then
                ifBP = bprelink(ifBP,exprBP,exprMerge,ifMerge)
                exprBP = 0
            end if

            MatchString(T_then)
            emitElse = emitON
            tmp = opstack[1]
--DEV see note in DoWhile[??]:
            if tmp>0 and scBP=0 and ifBP=0 then
--DEV 31/10/09: ("if wait_key() then end if" was doing nowt, actually using 
--                symtab[opWaitKey(=55)] which is MAGENTA(=5) as a Const...)
--              if opsidx=1 
                if (opsidx=1 and not opTopIsOp)
                or (opsidx=2 and opstack[2]=opNot) then
                    sytmp = symtab[tmp]
                    if sytmp[S_NTyp]=S_Const
                    and sytmp[S_Init]
                    and sytmp[S_vtype]=T_integer then
--DEV and K_noclr???
--DEV emitON=0; emitBlock may be 1          ***********************
                        tmp = sytmp[S_value]
                        if (opsidx=1 and tmp!=0)
                        or (opsidx=2 and tmp=0) then
                            emitElse = 0
                        else
                            emitON = 0
                        end if
                        opTopIsOp = 0
                        opsidx = 0
                    end if
                end if
            end if
        end if  -- elsevalid
        if toktype=LETTER and ttidx=T_end then
--trace(1)
            --
            -- optimise away jump for "then end if".
            --
            if elsevalid then
                --
                -- The conditional expression is still fully evaluated; eg a statement
                --  such as "if c_func(Xxxx,{blah}) then end if" would otherwise
                --  result in the c_func being completely omitted. The assumption
                --  is made that in the majority of cases some side effect is
                --  required, but eg "if string(x) then end if" (typically used to
                --  suppress warnings) could be further optimised, ie when we know
                --  there are no side-effects.
                --
                if opTopIsOp then
--                  if hasSideEffects(blah) then        --DEV fixme
-- 27/2: (untried)
--if opTopIsOp=BltinOp
--and find(opstack[opsidx],{opGetc,opGets,opSeek,opOpen,opWaitKey,opGetKey,opLock}) then
--  if opstack[opsidx]=opOpen then
--      Aborp("file id must be saved\n")
--  end if
                    if opTopIsOp>=NotBltinOp then   -- added 25/7/09 [DEV elsewhere?]
                        -- ignore the pesky "not"
                        opTopIsOp -= 10
                    end if
                    scode = opstack[opsidx]
                    if opEfct[scode]=E_none then
                        wasEmit2 = emitON
                        emitON = 0
                        PopFactor()
                        emitON = wasEmit2
                    else
                        PopFactor()
                    end if
                    if DEBUG then
                        if opsidx!=1 then ?9/0 end if
                    end if
                    freeTmp(-1)
--                  else
--                      freeTmp(-opsidx)
--                  end if
                elsif opsidx then
                    freeTmp(-1)
                end if
                -- patch short-circuits; eg in "if a or b then end if",
                --  the jump after a has already been emitted - all we are
                --  currently doing here is omitting the jump after b. 
                --  Eg for "if waitkey()='Y' and c_func(blah) then end if"
                if scBP>0 then
                    scBP = backpatch(scBP,0,scMerge)
                    if scBP then ?9/0 end if
                end if
--              opsidx = 0
                opTopIsOp = 0
            end if
            --
            -- kill off any ple, eg allow "if hour(h) then end if" to
            --  suppress unused warnings, and not replace said with
            --  "probable logic error (always true)" warnings.
            --
            probable_logic_error = 0
            exit
        end if -- not "then end" (or "else end")

        if toktype=LETTER and find(ttidx,{T_exit,T_break,T_continue}) and elsevalid then
            if in_try and loopage=try_loopage then
                Aborp("invalid (circumvents try handler reset)")
            end if
            if ttidx=T_exit then
                if exitBP=-1 then Aborp("exit statement must be inside a loop") end if
                if opsidx then
                    -- tag any scBP entries onto exitBP chain
                    -- eg/ie: "if a or b then exit"
                    --   --> move jump after a onto exit list
                    if scBP then
                        exitBP = bprelink(exitBP,scBP,scMerge,exitMerge)
                        scBP = 0
                    end if
                    exitBP = Branch(NoInvert,emitElse,exitMerge,exitBP) -- jump to exit point

                elsif emitON then
                    if emitElse then ?9/0 end if
                    -- handles eg "if [not] DEBUG then exit end if"
                    apnds5({opJmp,exitMerge,0,exitBP})
                    exitBP = length(s5)
                end if

                MatchString(T_exit)
            elsif ttidx=T_break then
                if breakBP=-1 then Aborp("break statement must be inside a select") end if
                if opsidx then
                    -- tag any scBP entries onto breakBP chain
                    -- eg/ie: "if a or b then break"
                    --   --> move jump after a onto break list
                    if scBP then
                        breakBP = bprelink(breakBP,scBP,scMerge,breakMerge)
                        scBP = 0
                    end if
                    breakBP = Branch(NoInvert,emitElse,breakMerge,breakBP) -- jump to end select

                elsif emitON then
--                  if emitElse then ?9/0 end if
                    -- handles eg "if [not] DEBUG then break end if"
                    apnds5({opJmp,breakMerge,0,breakBP})
                    breakBP = length(s5)
                end if

                MatchString(T_break)
            else -- ttidx=T_continue
                if continueBP=-1 then Aborp("continue statement must be inside a loop") end if
                if opsidx then
                    -- tag any scBP entries onto continueBP chain
                    -- eg/ie: "if a or b then continue"
                    --   --> move jump after a onto continue list
                    if scBP then
                        continueBP = bprelink(continueBP,scBP,scMerge,exitMerge)
                        scBP = 0
                    end if
                    continueBP = Branch(NoInvert,emitElse,exitMerge,continueBP) -- jump to continue point

                elsif emitON then
                    if emitElse then ?9/0 end if
                    -- handles eg "if [not] DEBUG then continue? end if"
                    apnds5({opJmp,exitMerge,0,continueBP})
                    continueBP = length(s5)
                end if

                MatchString(T_continue)
            end if

            if toktype!=LETTER or not find(ttidx,T_endelseelsifbreak) then
                --DEV or warning unreachable code, and
                --          emitON = 0
                --          call_proc(rBlock,{})
                -- (same message rqd if above condition still true after Block)
                Aborp("elsif, else, or end if expected.")
            end if
            emitON = 0  --DEV ugh, just need to skip else jump...(??)
----trace(1)
--          LastStatementWasExit = 1 -- DEV: Whoops, can't do that, it is a conditional exit!
--          LastStatementWasReturn = 0

        elsif toktype=LETTER and ttidx=T_return and returnvar=0 and elsevalid then

            -- handle then-return statements inside procedures (avoids jcc @f; jmp opRetf pairs)
            -- (obviously, then-returns in types/functions must be jcc @f; mov blah; jmp opRetf)

            if opsidx then
                plain = (scBP=0 and ifBP=0) -- plain test, not involving "and"/"or"?
                noofbranches = 2    -- prevent flip
--DEV no longer sure why I did this... (re-test/get rid of testfor)
--              scBP = Branch(NoInvert,emitElse,scMerge,scBP) -- fake jump, patched rsn:
                scBP = Branch(NoInvert,emitElse,0,scBP) -- fake jump, patched rsn:

            elsif emitON then
                -- rare; handles eg "if [not] DEBUG then return end if"
                if emitElse then ?9/0 end if
if newEmit then
                agcheckop(opRetf)
end if
                apnds5(opRetf)
            end if

            -- patch any scBP entries (includes jump just emitted) to opRetf
            -- eg if a or b then return:
            --          cmp a jnz scBP cmp b jnz scBP
            --       -> cmp a jnz opRetf cmp b jnz opRetf
            --
            testfor = 0
            while scBP do
                if DEBUG then
                    if s5[scBP-2]!=testfor then ?9/0 end if
                    testfor = scMerge
                end if
                tmp = s5[scBP]
if newEmit then -- (or set a flag)
                agcheckop(opRetf)
end if
                s5[scBP] = opRetf
                s5[scBP-2] = isOpCode --(replaces mergeSet, tested for this in pilx86)
                scBP = tmp
            end while

            MatchString(T_return)
            if toktype!=LETTER or not find(ttidx,T_endelseelsifbreak) then
                --DEV or warning unreachable code, and
                --          emitON = 0
                --          call_proc(rBlock,{})
                Aborp("elsif, else, or end if expected.")
            end if
            emitON = 0
        else -- not "then exit"
            if elsevalid then
                --
                -- jump_not to elsif/else/endif, eg
                -- if x then => Jnot <endif> x, or
                -- if not x then => Jif <endif> x, or
                -- if a<b => Jge <endif> a,b
                --
                if opsidx then
--DEV delete this comment next time we get schedule working again:
--BUGFIX 4/9/08: w/o this, ifBP was 100, fixedup to 105 in schend (via StoreVar), but then
--          backpatch=100 still planted in emitted code...
--if sched then
--              if schidx then schend() end if
--end if
                    ifBP = Branch(Invert,emitElse,ifMerge,ifBP)
                end if

                -- patch short circuits (eg if a or b then -> jump after a to if-block)
                if scBP>0 then
                    scBP = backpatch(scBP,0,scMerge)
                    if scBP then ?9/0 end if
                end if
            end if

            if toktype=LETTER and find(ttidx,T_endelseelsifbreak) then
                probable_logic_error = 0
            else

--              call_proc(rBlock,{})
                Block()

                clearIchain(-1)
                if exprBP!=0 then ?9/0 end if
            end if
        end if
        if not elsevalid then exit end if

        if toktype!=LETTER then exit end if
--      if ttidx=T_elsif then
---- 7/1/09:
--          if emitline<tokline then
--              emitline = tokline-1
--          end if
--          MatchString(T_elsif)
--          elsevalid = 1
--      else
--          if ttidx!=T_else then exit end if
---- 7/1/09:
--          if emitline<tokline then
--              emitline = tokline-1
--          end if
--          MatchString(T_else)
--          elsevalid = 0
--      end if
-- 7/2/09:
--4/9/09:
        if ttidx=T_elsif then
            ctrltyp = ELSIF
        else
            if ttidx!=T_else then exit end if
            ctrltyp = ELSE
        end if
--DEV removed 2/10/09:
--DEV put back 19/10... (trace really threw me, appearing to execute code [under emitON=0] it does not!)
        if emitline<tokline then
            emitline = tokline-1
        end if
        elsevalid = (ttidx=T_elsif)
        MatchString(ttidx)  -- T_else/T_elsif

        emitON = (emitON and emitElse)
----DEV 14/7: (27/2/09: I think is is handled OK now by jskip)
--      if not (LastStatementWasReturn=1) and not LastStatementWasExit then
        if emitON then
            apnds5({opJmp,endIfMerge,0,EndIfBP})
            EndIfBP = length(s5)
        end if
        -- patch previous if condition jumps to this elsif test
        if ifBP>0 then
            ifBP = backpatch(ifBP,0,ifMerge)
            if ifBP then ?9/0 end if
        end if
--if NOLT=0 or bind or lint then
        if emitON then
--DEV 29/3/10... (no gain)
--          apnds5({opCtrl,ctrltyp,ctrlink,emitline})
            s5 &= {opCtrl,ctrltyp,ctrlink,emitline}
            ctrlink = length(s5)-1
            if NOLT=0 or bind or lint then
                ltCtrl(ctrlink)
            end if -- NOLT
        end if
--end if -- NOLT

--      emitline = line
        if allWhiteToTokcol() then
            emitline = line-1
        else
            emitline = line
        end if
        emitON = (wasEmit and emitElse)
--      if DEBUG then
        if exprBP!=0 then ?9/0 end if
--      end if
        oktoinit = 0
    end while
--  if emitON then  **NO!!!
    -- patch any remaining if/elsif (and no else) jumps to s5len (end if)
    -- patch the jumps before elsif and else statements to s5len (end if)

    if ifBP>0 then
        ifBP = backpatch(ifBP,0,ifMerge)
        if ifBP then ?9/0 end if
    end if
    if EndIfBP>0 then
        EndIfBP = backpatch(EndIfBP,0,endIfMerge)
        if EndIfBP then ?9/0 end if
    end if
--  end if -- emitON
--if NOLT=0 or bind or lint then
--DEV8:
--  if emitON then  -- oops!
    if ctrlink then
--DEV 29/3/10... (no gain)
--      apnds5({opCtrl,END+IF,ctrlink,emitline})
        s5 &= {opCtrl,END+IF,ctrlink,emitline}
        ctrlink = length(s5)-1
        s5[iftop] = ctrlink
        if NOLT=0 or bind or lint then
            ltCtrl(ctrlink)
        end if -- NOLT
        --DEV tryme:
        -- (if showprogress then puts(1,"msg\n") end if generated the following il:)
        --   3:  opCtrl,2,8,?,                       opCtrl,IF,link,emitline
        --   6:  opCtrl,3,5,?,                       opCtrl,END+IF,link,emitline
        --if ctrlink=iftop+3 then
        --  s5 = s5[1..iftop-3]
        --end if
    end if
--end if -- NOLT

    MatchString(T_end)
    MatchString(T_if)
    emitON = wasEmit
    if exprBP!=0 then ?9/0 end if

    clearIchain(saveIchain)

    SideEffects = or_bits(SideEffects,wasSideEffects)

end procedure

--with trace
procedure DoWhile()
--
-- Parse and Translate a WHILE Statement
--
integer wasEmit,emitBlock,saveIchain
--
integer saveExitBP, saveContinueBP, tmp, loopTop

sequence sytmp
integer wasSideEffects
integer waslMask
--DEV14:
integer thispt
--15/10/2020:
integer wasttidx = ttidx

    loopage += 1    -- for DoTry()
--if fileno=1 then trace(1) end if
    saveExitBP = exitBP
    exitBP = 0  -- valid, end of chain
    saveContinueBP = continueBP
    continueBP = 0
    saveIchain = Ichain
    Ichain = -1
    wasSideEffects = SideEffects
    SideEffects = E_none
    waslMask = lMask
    lMask = E_none

    if emitON then
        if NOLT=0 or bind or lint then
            apnds5({opLoopTop,E_vars,E_vars,0})     -- opLoopTop,lmask,gmask,end
            thispt = length(s5)+1
--          ltlooptop(thispt-3)
            ltlooptop(thispt-4)
        --??? (untried:)
        --else -- NOLT
        --      thispt = length(s5)+1
        end if -- NOLT
        s5 &= {opLabel,0,0,0}   -- opLabel,mergeSet,0/x86loc,link
        loopTop = length(s5)    -- addr link field
    end if

--  MatchString(T_while,float_valid:=true)

    if exprBP!=0 then ?9/0 end if
    if continueBP!=0 then ?9/0 end if
    wasEmit = emitON
    emitBlock = emitON

if ttidx!=T_do then
    MatchString(T_while,float_valid:=true)

    Expr(pAllops,false) -- full, notBool/asIs

    if probable_logic_error then show_ple() end if

    if exitBP!=0 then ?9/0 end if
    if continueBP!=0 then ?9/0 end if

    if exprBP then
        -- (except for eg while 1 do)
        exitBP = bprelink(exitBP,exprBP,exprMerge,exitMerge)
        exprBP = 0
    end if

--DEV test for explicit false (while debug do) and 'not const', why not (see DoIf):
--  wasEmit = emitON
--  emitBlock = emitON
    tmp = opstack[1]
-- 28/1/09: [DEV 4/2 why would we care about exitBP?! scBP sure (while <any> or false),
--                   but exitBP (while <any> and false), there's just no point...]
--  aha: exitBP is just exprBP under another name. Just asking "is it a simple test?".
--  24/2    erm, now I see that the above is a real query worth investigating further...
--          (changed above to use "<any>" to make the point a little bit clearer)
--          ==> just try "if tmp>0 and scBP=0 then" next time we've got everything else working...
    if tmp>0 and scBP=0 and exitBP=0 then
--DEV 31/10/09: (mirroring change in DoIf)
--      if opsidx=1 
        if (opsidx=1 and not opTopIsOp)
        or (opsidx=2 and opstack[2]=opNot) then
            sytmp = symtab[tmp]
            if sytmp[S_NTyp]=S_Const
            and sytmp[S_Init]
            and sytmp[S_vtype]=T_integer then
----DEV and K_noclr???
                emitON = 0
                tmp = sytmp[S_value]
                if (opsidx=1 and tmp!=0)
                or (opsidx=2 and tmp=0) then
--emitON=0
--                  emitElse = 0
                else
                    emitBlock = 0
--                  emitON = 0
                end if
            end if
        end if
    end if

--DEV emitElse may have merit here too:
    --
    -- jump_not to end while (== exit point), eg
    -- while x do => Jnot <endwhile> x, or
    -- while not x do => Jif <endwhile> x
    -- (note that while a and b and c do has already
    --  put "a and b" on exitBP, see exprBP above.)
    --
    exitBP = Branch(Invert,1,exitMerge,exitBP)
    if emitON then
        -- patch short-circuits (while a or b or c do -> point the jumps after a & b
        -- at s5len, ie the loop body ["not c" (=above exitBP) jumps to end while])
        if scBP>0 then
            scBP = backpatch(scBP,0,scMerge)
            if scBP then ?9/0 end if
        end if
    end if -- emitON

--  wasEmit=emitON
--  -- short-circuit if a constant:
--  if opsidx=1 and opsltrl[1]=1 and opstype[1]=T_integer then
--      if opstack[1]=T_const0 then -- a literal/constant 0
--          emitON = 0
--      else
--          opsidx -= 1
--      end if
--  else
--      LastStatementWasReturn = 0
--  end if
end if
    MatchString(T_do)
    emitON = emitBlock

    Block()

    -- patch any continue statements at s5len, ie before end while

    if continueBP>0 then
        if backpatch(continueBP,0,exitMerge) then ?9/0 end if
    end if
    continueBP = saveContinueBP

    if wasttidx=T_do then
        MatchString(T_until)
        if exprBP then ?9/0 end if
        Expr(pAllops,false) -- full, notBool/asIs
        if probable_logic_error then show_ple() end if

--      if exitBP!=0 then ?9/0 end if
--      if continueBP!=0 then ?9/0 end if

--nb: Branch() states:
--   Note that there are no conditional backward jumps. Equally there are no forward 
--    jumps in a one-pass compiler where we know where to land. Hence the targets are
--    /always/ put on a backpatch list, processed at the appropriate "end if" etc.
-- so we'll do this the hard way: put it on a backpatch list and immediately backpatch...

--      if exprBP then
--          -- (except for eg while 1 do)
--          exitBP = bprelink(exitBP,exprBP,exprMerge,exitMerge)
--          exprBP = 0
----/*
--          if emitON then
--              while 1 do
--                  if s5[exprBP-2]!=exprMerge then ?9/0 end if
--                  s5[exprBP-1] = loopTop
--                  exprBP = s5[exprBP]
--                  if exprBP=0 then exit end if
--              end while
--          end if -- emitON
----*/
--      end if
--?9/0

--      exitBP = Branch(Invert,1,exitMerge,exitBP)
--      exitBP = Branch(NoInvert,1,loopTop,exitBP)
--      exprBP = Branch(NoInvert,1,exprMerge,exprBP)
--      exprBP = Branch(NoInvert,1,0,exprBP)
?{"exitPB",exitBP}
        exprBP = Branch(Invert,1,0,exprBP)
?{"exitBP",exitBP}
?{"exprBP",exprBP}
--      exitBP = Branch(NoInvert,1,loopTop,exprBP)
--Branch(integer invert, integer emitElse,integer mergeSet, integer backpatch)
--                  scBP = Branch(NoInvert,1,scMerge,scBP)
--      if backpatch(exitBP,0,exitMerge) then ?9/0 end if
        while 1 do
--          if s5[exprBP-2]!=exprMerge then ?9/0 end if
            if s5[exprBP-2]!=0 then ?9/0 end if
            s5[exprBP-1] = loopTop
            exprBP = s5[exprBP]
            if exprBP=0 then exit end if
        end while


        if emitON then
--          -- patch short-circuits (while a or b or c do -> point the jumps after a & b
--          -- at s5len, ie the loop body ["not c" (=above exitBP) jumps to end while])
            if scBP>0 then
?9/0
                while 1 do
                    if s5[scBP-2]!=scMerge then ?9/0 end if
                    s5[scBP-1] = loopTop
                    scBP = s5[scBP]
                    if scBP=0 then exit end if
                end while
            end if
        end if -- emitON
--?9/0
    end if

    if emitON then
        emitline = line
if wasttidx!=T_do then
        apnds5({opJmp,0,loopTop,0})
end if
        --      s5[loopTop] = length(s5)    -- NO! bckwd jumps should not be linked from opLabel!
        if NOLT=0 or bind or lint then
--          if s5[loopTop-6]!=opLoopTop then ?9/0 end if
--          s5[loopTop-5] = lMask
--          s5[loopTop-4] = SideEffects
            if s5[loopTop-7]!=opLoopTop then ?9/0 end if
            s5[loopTop-6] = lMask
            s5[loopTop-5] = SideEffects
        end if -- NOLT

    end if -- emitON
if wasttidx!=T_do then
    MatchString(T_end)
    MatchString(T_while)
end if
    emitON = wasEmit --DEV (todo: omit all code in while 0 do).

    -- patch any exit statements at s5len, ie past end while,
    -- as well as (via exitBP = exprBP etc above) any cond-fails 
    --  [eg in 'while a and b do', the jumps after the a & b ].

    if exitBP>0 then
        if backpatch(exitBP,0,exitMerge) then ?9/0 end if
    end if
    exitBP = saveExitBP
    if NOLT=0 or bind or lint then
        if emitON then
            s5 &= {opCtrl,END+LOOP,thispt-3,emitline}
            -- s5[loopTop??] = length(s5)   --DEV
            s5[thispt-1] = length(s5)-1
            if NOLT=0 or bind or lint then
                ltCtrl(length(s5)-1)
            end if -- NOLT
        end if
    end if -- NOLT

    clearIchain(saveIchain)

    SideEffects = or_bits(SideEffects,wasSideEffects)
    lMask = or_bits(lMask,waslMask)
    loopage -= 1

end procedure


--with trace
procedure DoFor()
--
-- Parse and Translate a For Statement
--
integer CN, N, NI, NL, NS
--integer wasLtrlNI, wasLtrlNL, wasLtrlNS -- save opstrl[1..3]
integer state, newScope
integer controlvar
integer cvtype
integer ftyp    -- copy of opstype[opsidx] used in for loop type checking
integer bpFor
--, jmpoffset
integer saveExitBP, saveContinueBP, saveIchain
integer savettidx, do_ttidx
integer ivar,tvar,bvar,mcode
--sequence sytmp
--object dbg
integer wasSideEffects
integer waslMask
integer lens5
integer src     -- 13/5/2012
integer flags
integer cnTyp

    loopage += 1    -- for DoTry()
    wasSideEffects = SideEffects
    SideEffects = E_none
    waslMask = lMask
    lMask = E_none

    saveIchain = Ichain
    Ichain = -1

    MatchString(T_for)
    if toktype!=LETTER then
        Aborp("a loop variable name is expected here")
    end if
    savettidx = ttidx
--23/9/16!!
--  CN = InTable(InTop)
    CN = InTable(InVeryTop)
    if CN>0 then
        -- permit re-use of local variable rather than erroring, but it must be appropriate type.
-- 1/9/14:
        cnTyp = symtab[CN][S_NTyp]
--      if symtab[CN][S_NTyp]=S_Rsvd then
--          Aborp("illegal use of a reserved word")
        if cnTyp=S_Const or cnTyp>S_TVar then
            Aborp("already declared as a "&NTdesc[cnTyp])
--25/5/18:
--      elsif symtab[CN][S_vtype]!=T_integer then
        else
            integer cnvtyp = symtab[CN][S_vtype]
            if cnvtyp>T_object
            or not and_bits(cnvtyp,T_integer) then
                Aborp("type error (for loop control variable must be an INTEGER)")
            end if
        end if
        N = CN
        state = symtab[N][S_State]
        if and_bits(state,S_for) then
            Aborp("already in use as control loop variable")
        end if
--      MarkWritten(CN)
        newScope = 0
    else
        CN = InTable(-InAny)
        if CN>0 then
--21/01/2021
--          if symtab[CN][S_NTyp]=S_Rsvd then
            if CN<=T_Asm or symtab[CN][S_NTyp]=S_Rsvd then
                Aborp("illegal use of a reserved word")
            end if
        end if
--added 29/12/2011:
        CN = 0
--      increaseScope(S_For,-1)
        newScope = 1
    end if
-- 19/6/10. Above edited, this removed...
--  if toktype!=LETTER then
--      Aborp("a loop variable name is expected here")
--  end if
    controlvar = ttidx
    getToken()
-- added 6/4/2012 (allow ":=" as well as "=")
    if toktype=':' and Ch='=' then MatchChar(':',false) end if
    MatchChar('=',float_valid:=true)
    if opsidx!=0 then ?9/0 end if -- leave in (ie outside if DEBUG then)
--  ?? = tokline
--  ?? = tokcol
    Expr(pAllops,asBool)
    flags = 0
    if opTopIsOp then
--      ivar = newTempVar(T_integer,Private)
--      StoreVar(ivar,T_integer)
--      PushFactor(ivar,false,T_integer)
--      opsltrl[opsidx] = 0--allowTempReuse -- mark for possible re-use
        PopFactor()
        flags += #01    -- init is init
    else
--      ftyp = opstype[1]
--      if not and_bits(ftyp,T_integer) then Abork("illegal expression type",opsidx) end if
--DEV not sure we really need to do this (for tvar & bvar, we do, but since this is only[?]
--                                          used within the opFor, nowt can modify it...)
--      if opsltrl[opsidx] then
----            if ftyp!=T_integer then checkForType() end if
--          ivar = 0
--      else
--DEV 1/11/2011:
--      if not opsltrl[opsidx] then
--DEV 13/5/2012:
--      if opsltrl[opsidx]!=1 then
        if opsidx!=1 then ?9/0 end if
        if opsltrl[1]!=1 then
--          ivar = newTempVar(T_integer,Private)
            ftyp = opstype[1]
            ivar = newTempVar(ftyp,Private)
            src = opstack[1]
            if ftyp=T_integer then
--DEV 13/5/2012:
--              mcode = opMovbi
                if symtab[src][S_Init] then
                    mcode = opMovbi
                else
                    mcode = opMovti
                end if
            else
--              checkForType()
--DEV why is this commented out??
--              mcode = opMovti
                mcode = opMove
            end if
            constInt = 0
--          emitHexMov(mcode,ivar,opstack[opsidx])
--          emitHexMov(mcode,ivar,opstack[1])
            emitHexMov(mcode,ivar,src)
--          opstack[opsidx] = ivar
            opstack[1] = ivar
--validate_opstack()
--          opstype[opsidx] = T_integer
--          opstype[opsidx] = ftyp
--          opsltrl[opsidx] = 0--allowTempReuse -- mark for possible re-use
            if symtab[src][S_Init] then
                flags += #01
            end if
        else
            flags += #01
        end if
    end if
    if opsidx!=1 then ?9/0 end if -- leave in
    ftyp = opstype[1]
    if not and_bits(ftyp,T_integer) then
--      tokline = >opsline[k]
--      tokcol  = ?opstcol[k]
--      Abork("illegal expression type",opsidx)
        Abork("illegal expression type",1)
    end if

    MatchString(T_to,float_valid:=true)
    Expr(pAllops,asBool)
    if opTopIsOp then
--      tvar = newTempVar(T_integer,Private)
--      StoreVar(tvar,T_integer)
--      PushFactor(tvar,false,T_integer)
--      opsltrl[opsidx] = 0--allowTempReuse -- mark for possible re-use
        PopFactor()
        flags += #02    -- limit is init
    else
--      ftyp = opstype[2]
--      if not and_bits(ftyp,T_integer) then Abork("illegal expression type",opsidx) end if
--      if opsltrl[opsidx] then
----            if ftyp!=T_integer then checkForType() end if
--          tvar = 0
--      else
--DEV 1/11/2011:
--      if not opsltrl[opsidx] then
--DEV 13/5/2012:
--      if opsltrl[opsidx]!=1 then
        if opsidx!=2 then ?9/0 end if
        if opsltrl[2]!=1 then
--          tvar = newTempVar(T_integer,Private)
            ftyp = opstype[2]
            tvar = newTempVar(ftyp,Private)
            src = opstack[2]
            if ftyp=T_integer then
--DEV 13/5/2012:
--              mcode = opMovbi
                if symtab[src][S_Init] then
                    mcode = opMovbi
                else
                    mcode = opMovti
                end if
            else
--              checkForType()
--DEV ditto
--              mcode = opMovti
                mcode = opMove
            end if
            constInt = 0
--          emitHexMov(mcode,tvar,opstack[opsidx])
--          emitHexMov(mcode,tvar,opstack[2])
            emitHexMov(mcode,tvar,src)
--          opstack[opsidx] = tvar
            opstack[2] = tvar
--validate_opstack()
--          opstype[opsidx] = T_integer
--          opstype[opsidx] = ftyp
--          opsltrl[opsidx] = 0--allowTempReuse -- mark for possible re-use
            if symtab[src][S_Init] then
                flags += #02
            end if
        else
            flags += #02
        end if
    end if
    if opsidx!=2 then ?9/0 end if
--  if find(opstype[opsidx],"SPN") then Aborp(iet) end if
    ftyp = opstype[2]
    if not and_bits(ftyp,T_integer) then Abork("illegal expression type",opsidx) end if
    if ttidx=T_by then
        getToken()
        Expr(pAllops,asBool)
        if opTopIsOp then
--          bvar = newTempVar(T_integer,Private)
--          StoreVar(bvar,T_integer)
--          PushFactor(bvar,false,T_integer)
--          opsltrl[opsidx] = 0--allowTempReuse -- mark for possible re-use
            PopFactor()
            flags += #04    -- step is init
        else
--          ftyp = opstype[opsidx]
--          if not and_bits(ftyp,T_integer) then Abork("illegal expression type",opsidx) end if
--          if opsltrl[opsidx] then
--          if opsltrl[opsidx]=1 then
----                if ftyp!=T_integer then checkForType() end if
--              bvar = 0
--          else
--DEV 1/11/2011:
--          if not opsltrl[opsidx] then
--DEV 13/5/2012:
--          if opsltrl[opsidx]!=1 then
            if opsidx!=3 then ?9/0 end if
            if opsltrl[3]!=1 then
--              bvar = newTempVar(T_integer,Private)
                ftyp = opstype[3]
                bvar = newTempVar(ftyp,Private)
                src = opstack[3]
                if ftyp=T_integer then
--DEV 13/5/2012
--                  mcode = opMovbi
                    if symtab[src][S_Init] then
                        mcode = opMovbi
                    else
                        mcode = opMovti
                    end if
                else
--                  checkForType()
--Ditto
--                  mcode = opMovti
                    mcode = opMove
                end if
                constInt = 0
--              emitHexMov(mcode,bvar,opstack[opsidx])
--              emitHexMov(mcode,bvar,opstack[3])
                emitHexMov(mcode,bvar,src)
--              opstack[opsidx] = bvar
                opstack[3] = bvar
--validate_opstack()
--              opstype[opsidx] = T_integer
--              opstype[opsidx] = ftyp
--              opsltrl[opsidx] = 0--allowTempReuse -- mark for possible re-use
                if symtab[src][S_Init] then
                    flags += #04
                end if
            else
                flags += #04
            end if
        end if
        if opsidx!=3 then ?9/0 end if   -- may need popFactor() (DEV)
--      if find(opstype[opsidx],"SPN") then Aborp(iet) end if
        ftyp = opstype[3]
        if not and_bits(ftyp,T_integer) then Abork("illegal expression type",opsidx) end if
    else
        PushFactor(T_const1,true,T_integer) -- literal 1 (symtab[T_const1] is 1)
--      tvar = 0
        flags += #04    -- step is init
    end if
    if opsidx!=3 then ?9/0 end if -- leave in
    do_ttidx = ttidx -- (13/02/14 save for checking against T_do)
    if CN=0 then
        ttidx = savettidx
        cvtype = S_TVar
        if returnvar=-1 then    -- top_level loops need a gvar
            cvtype = S_GVar2
        end if
        N = addSymEntry(controlvar,0,cvtype,T_integer,0,0)
        CN = symlimit
    else
        N = CN
    end if
    state = symtab[CN][S_State]
    state = or_bits(state,S_for_used_set)
    symtab[CN][S_State] = state

    if symtab[CN][S_Init]=0 then
        symtab[CN][S_Init] = saveIchain
        saveIchain = CN
    end if

    NI = opstack[1] -- init var
    NL = opstack[2] -- limit var
    NS = opstack[3] -- step var
    saveExitBP = exitBP
    exitBP = 0  -- valid, end of chain
    saveContinueBP = continueBP
    continueBP = 0
    if emitON then
        if NOLT=0 or bind or lint then
            apnds5({opLoopTop,E_vars,E_vars,0}) -- opLoopTop,lmask,gmask,end
            ltlooptop(length(s5)-3)
        end if -- NOLT
        apnds5({opFor2,flags,NI,N,NS,NL,0})
        bpFor = length(s5)
--hmm 25/10/17: (nope)
--      exitBP = bpFor
    end if
--  freeTmp(-3) -- hmmm?? (no testing, just panic) [DEV]
--DEV 1/11/2011 (apparently unused [and probably should have been "(opsltrl[n]=1)"])
--  wasLtrlNI = opsltrl[1]
--  wasLtrlNL = opsltrl[2]
--  wasLtrlNS = opsltrl[3]
-- see below...
    opsidx = 0

--  MatchString(T_do)
--13/2/14:
--  getToken()  -- "", except we've clobbered ttidx
    ttidx = do_ttidx
    MatchString(T_do)
--if tracefor then trace(1) end if
--trace(1)

    Block()

    emitline = line
    if newScope then
        -- hide non-pre-declared for loop control vars
        tt[savettidx+EQ] = symtab[CN][S_Nlink]
        symtab[CN][S_Nlink] = -2
    else
        symtab[CN][S_State] = state-S_for
    end if

    if continueBP>0 then
        if backpatch(continueBP,0,exitMerge) then ?9/0 end if
    end if
    continueBP = saveContinueBP

    if emitON then
--  83:  opJmp,3,95,0,                       opJmp,exitMerge,tgt,link
--  87:  opLn,14,                            --: end for
--  89:  opEndFor,17,55,                         opEndFor,END+LOOP,bpFor
--27/9/2019. DEV/Note there is now code in jskip() to perform the backpatch, and hence
--           this may work fine when simply commented out... [GOT IT NOW, PLAIN EXIT]
--      if s5[-4]=opJmp
--      and s5[-3]=exitMerge then
--          -- 25/10/17: unconditional exit immediately preceding end for is not allowed.
----            Aborp("illegal/unsupported construct.") -- (dotless one in pilx86.e...)
--          ?9/0 -- as above, simply try deleting this test (and add to tests) [DONE]
--      end if
--      apnds5({opCtrl,END+LOOP,bpFor-8,emitline})
        apnds5({opEndFor,END+LOOP,bpFor})   -- link opEndFor to opFor
        lens5 = length(s5)
        s5[bpFor] = lens5                   -- and opFor to opEndFor
        if NOLT=0 or bind or lint then
            ltCtrl(lens5)
            if s5[bpFor-10]!=opLoopTop then ?9/0 end if
            s5[bpFor-9] = lMask
            s5[bpFor-8] = SideEffects
            s5[bpFor-7] = lens5
        end if -- NOLT
    end if

    MatchString(T_end)
    MatchString(T_for)
--  if newScope then
--      dropScope(0)
--  end if
--  LastStatementWasReturn = 0

    if exitBP>0 then
        if backpatch(exitBP,0,exitMerge) then ?9/0 end if
    end if
    exitBP = saveExitBP

    clearIchain(saveIchain)

    SideEffects = or_bits(SideEffects,wasSideEffects)
    lMask = or_bits(lMask,waslMask)
    loopage -= 1
end procedure

--with trace
procedure DoReturn()
--
--  returnvar is:
--     -1: in top_level code: return statement is illegal
--      0: in a procedure: return takes no parameter
--    +ve: in a function or type: return takes one parameter, stored here
--
--object dbg

    if returnvar=-1 then
        Aborp("return must be inside a procedure or function")
    end if
--Actually, returns are perfectly safe (since frame/handler is discarded anyway)
--  if in_try and loopage=try_loopage then  -- NO!
--  if in_try then
--      Aborp("invalid (circumvents try handler reset)")
--  end if

    MatchString(T_return,float_valid:=true)
    if returnvar then
        Expr(pAllops,asBool)
if newEBP then
        if opTopIsOp then PopFactor() end if
        if not opTopIsOp and opsidx=1 and returnvar=opstack[opsidx] then
            --DEV is this a tail recursion optimisation?
            opsidx=0
        elsif returnint then    -- (a type definition, which should return an integer(0|1))
            StoreVar(returnvar,T_integer)
            returntype = T_integer
        else
            RHStype = T_object
            if opsidx=1 then
                RHStype = opstype[1]
--DEV tryme (as below)
--          else
--  ?9/0
            end if
--DEV tryme (when you get round to p4.exw):
            StoreVar(returnvar,T_object)
--          StoreVar(returnvar,RHStype)
            if emitON then
                if returntype=0 then
                    returntype = RHStype
                elsif RHStype<T_object
                  and returntype<T_object then
                    returntype = or_bits(returntype,RHStype)
                else
                    returntype = T_object
                end if
            end if
        end if
else -- oldstyle (not newEBP)
        if not opTopIsOp and opsidx=1 and returnvar=opstack[opsidx] then
            opsidx=0
        elsif returnint then
            StoreVar(returnvar,T_integer)
            returntype = T_integer
        else
            RHStype = T_object
            if opsidx=1 then
                RHStype = opstype[1]
            end if
            StoreVar(returnvar,T_object)
            if emitON then
--              if returntype=0 then
--                  returntype = RHStype
--              elsif returntype!=RHStype then
--                  if (returntype=T_integer and RHStype=T_atom) 
--                  or (returntype=T_atom and RHStype=T_integer) then
--                      returntype = T_atom
--                  elsif (returntype=T_string and RHStype=T_sequence)
--                     or (returntype=T_sequence and RHStype=T_string) then
--                      returntype = T_sequence
--                  else
--                      returntype = T_object
--                  end if
--              end if
--DEV 12/12/08:
                if returntype=0 then
                    returntype = RHStype
--              elsif returntype!=RHStype then
                elsif RHStype<T_object
                  and returntype<T_object then
                    returntype = or_bits(returntype,RHStype)
                else
                    returntype = T_object
                end if
--if returntype>T_object then ?9/0 end if
            end if
        end if
end if -- newEBP
    end if
    if emitON then
if newEmit then
        agcheckop(opRetf)
end if
        apnds5(opRetf)
    end if
--  LastStatementWasReturn = 1
    if emitON then
        CheckForFunctionReturn = 1
    end if
end procedure

--procedure DoSwitch(integer flags)
procedure DoSwitch()
--  switch <expr> [(with|without) (fallthru|fallthrough|warning)] [do]
--      {case <expr>{,<expr>} [:|then] <block> [break|fallthru|fallthrough]}
--      [(([case] else)|default) [:|then] <block>]
--  end switch
--DEV default does not have to be last?? [set with warning on the opCtrl,
--    change pilx86.e to defer the update]         ^ or error/jump_table
--global constant T_case        = 1552  tt_stringF("case",T_case)
--global constant T_default     = 1576  tt_stringF("default",T_default)
--global constant T_break       = 1596  tt_stringF("break",T_break)
--  MatchString(T_switch)
--  Expr(pAllops,asBool)
--procedure DoIf()
--
-- Recognize and translate a "switch" construct
--
integer emitElse, wasEmit
integer wasExprBP

integer switchBP, saveIchain
integer EndSwitchBP
--, tmp
integer saveBreakBP

--sequence sytmp

integer elsevalid   -- initially 2, == first conditional test,
                    -- can 2=>3 for "if then return" handling,
                    -- then 1 until we find an "else",
                    -- then 0 to force "if else else" to error.

integer casefound = 0   -- added 2/11/16

integer wasSideEffects
--integer plain, testfor
integer switchtop, ctrlink, elsectrl, ctrltyp, withsaid
--integer scode, wasEmit2

integer N, isLit, etype

integer link

--trace(1)
    if opsidx then ?9/0 end if  -- leave in (outside if DEBUG then)
    loopage += 1    -- for DoTry()
    EndSwitchBP = 0

    if exprBP!=0 then ?9/0 end if

    saveIchain = Ichain
    Ichain = -1

--11/6/16:
    saveBreakBP = breakBP
    breakBP = 0

    MatchString(T_switch,float_valid:=true)

    elsevalid = 2
    elsectrl = -1

--  if and_bits(flags,DLL_MAIN) then
----        get_fdwReason()
--      Matchstring(fdwReasonttidx)
--  else
        Expr(pAllops,asBool)
--  end if
    if opTopIsOp then PopFactor() end if

--28/9/15:
    saveFunctionResultVars(opsidx,INTSTOO)

    ctrlink = 0
    if emitON then
        apnds5({opCtrl,IF,0,emitline})
--      apnds5({opCtrl,IF+SWITCH,0,emitline}) -- NO!!
        switchtop = length(s5)-1    -- patched at/pointed to the end if
        ctrlink = switchtop         -- where to point next elsif/else/endif
        if NOLT=0 or bind or lint then
            ltCtrl(switchtop)
        end if -- NOLT
    end if

    wasSideEffects = SideEffects
    SideEffects = E_none
    wasEmit = emitON
    emitElse = emitON   -- (minor optimisation [cmp vs opJcc] 18/3/09)

--15/10/15:
--  Expr(pAllops,asBool)
--  if opTopIsOp then PopFactor() end if
--
----28/9/15:
--  saveFunctionResultVars(opsidx,INTSTOO)

    N = opstack[opsidx]
--DEV 1/11/2011:
--  isLit = opsltrl[opsidx]
    isLit = (opsltrl[opsidx]=1)
--  opsltrl[opsidx] = 0--allowTempReuse -- [DEV?]
    etype = opstype[opsidx]
    opsidx -= 1

    if toktype=LETTER
    and (ttidx=T_with or ttidx=T_without) then
        if ttidx=T_with then
            ctrltyp = IF
--          ctrltyp = IF+SWITCH
        else
            -- stay 0 always; "without xxx"==default (explicitly stated)
            ctrltyp = 0
        end if
        withsaid = 0
        MatchString(ttidx)  -- T_with/T_without
        while 1 do
            if ttidx=T_fallthru
            or ttidx=T_fallthrough then
                if and_bits(withsaid,FALLTHRU) then Aborp("duplicate") end if
                withsaid += FALLTHRU
                if ctrltyp then
                    ctrltyp += FALLTHRU
                end if
                MatchString(ttidx)  -- T_fallthru/T_fallthrough
            elsif ttidx=T_jump_table then
                if and_bits(withsaid,SWTABLE) then Aborp("duplicate") end if
                withsaid += SWTABLE
                if ctrltyp then
                    ctrltyp += SWTABLE
                end if
                MatchString(T_jump_table)
            else
                Expected("fallthr(u|ough) | jump_table")
            end if
            if toktype!=',' then exit end if
            MatchChar(',',float_valid:=true)
        end while
        if emitON and ctrltyp then
            s5[switchtop-1] = ctrltyp
        end if
    end if
    if toktype=LETTER 
    and ttidx=T_do then
        MatchString(T_do)
    end if  
        
    switchBP = 0
    while 1 do
--DEV
--T_end,T_break
--      if and_bits(flags,DLL_MAIN) then
--          MatchString(T_case)
----            MatchString(T_DLLATTACH)
--          if toktype=':' then
--              MatchChar(':')
--          elsif toktype=LETTER
--            and ttidx=T_then then
--              MatchString(T_then)
--          end if
----opInit?
--          flags -= DLL_MAIN
--          ?9/0
--      elsif toktype!=LETTER then
        if toktype!=LETTER then
            exit
        elsif ttidx=T_case
           or ttidx=T_else
           or ttidx=T_default then
            if ttidx=T_case then
                casefound = 1
                MatchString(T_case,float_valid:=true)
            end if
            if ttidx=T_else
            or ttidx=T_default then
                if emitON then
                    ctrltyp = s5[ctrlink-1]
                    --DEV for "switch i default", we'll have to emit (eg) Jne i,i and specifically test for that.
                    -- or perhaps better, from
--                          IF = #2,        -- 0b00000010
--                          ELSIF = #6,     -- 0b00000110 -- \ (*NB* also used for 
--                          ELSE = #E,      -- 0b00001110 -- /  notall, see pltype.e)
--                          SWTABLE = #80,  -- 0b10000000 -- the "switch with warning" flag, see below.
                    -- use (say)
--                          ??? = #8A,      -- 0b10001010 -- "first branch of switch is default" FBOSID
                    -- and treat it somewhat like opCtrl,IF...
--25/8/2012:
                    if ctrltyp=IF then
                        s5[ctrlink-1] = IF+SWFOID
                    else
--12/9/15:
--                      if ctrltyp!=ELSIF then ?9/0 end if
                        if ctrltyp!=ELSIF and ctrltyp!=ELSE then ?9/0 end if
                        s5[ctrlink-1] = ELSE
                    end if
                end if
                if not elsevalid then
                    Aborp("duplicate else/default")
                end if
                MatchString(ttidx)  -- T_else/T_default
                elsevalid = 0
                if toktype=':' then
                    MatchChar(':')
                elsif toktype=LETTER
                  and ttidx=T_then then
                    MatchString(T_then)
                end if
            else
                if not elsevalid then
                    if emitON then
                        ctrltyp = s5[switchtop-1]
                        ctrltyp = or_bits(ctrltyp,SWTABLE)
                        s5[switchtop-1] = ctrltyp
                    end if
                end if
                wasExprBP = exprBP
                switchBP = 0
                while 1 do
                    if exprBP!=0 then ?9/0 end if
                    noofbranches = 0
                    PushFactor(N,isLit,etype)
                    Expr(pAllops,false) -- full, notBool/asIs
                    PushOp(opJeq,BranchOp)
--DEV/SUG:
--                  if opTopIsOp then
--                      if and_bits(ctrltyp,SWTABLE) then -- or FALLTHRU?
--                          Aborp("incompatible with jump_table")
--                      else
--                          swtableillegal := 1
--                      end if
--                  end if
--eg: (much testing rqd)
--constant numbers = {34553, 235452} 
-- 
--procedure CheckNum(atom number) 
-- 
----    switch number do 
--  switch number with fallthrough do 
--      case numbers[1] then 
--          puts(1,"Base location\n") 
--      case numbers[2] then 
--          puts(1,"Second location\n") 
--  end switch 
--       
--end procedure 
-- 
--CheckNum(34553) 
                    if exprBP then
                        switchBP = bprelink(switchBP,exprBP,exprMerge,ifMerge)
                        exprBP = 0
                    end if
                    if toktype=',' then
                        MatchChar(',',float_valid:=true)
                    else
                        if toktype=':' then
                            MatchChar(':')
                        elsif toktype=LETTER
                          and ttidx=T_then then
                            MatchString(T_then)
                        end if
                        -- smart switch: treat "case 1 case 2" as "case 1,2"
                        -- ditto "case 1 fallthr(u|ough) case 2".
                        if toktype!=LETTER then exit end if
                        if ttidx=T_fallthru or ttidx=T_fallthrough then
                            if emitON then
                                ctrltyp = s5[switchtop-1]
                                ctrltyp = or_bits(ctrltyp,FALLTHRU)
                                s5[switchtop-1] = ctrltyp
                            end if
                            MatchString(ttidx)  -- T_fallthru/T_fallthrough
                        else
    --Hmm: case x break; if we haven't emitted any code yet then don't... (just clear opstack?)
    --      ah, no: "case 5 break; else" needs the 5 not to do the else...
                            if not find(ttidx,{T_case,T_else,T_default}) then exit end if
                        end if
--12/1/16: (while "case x then else" was caught, "case x then case else" was not) [also added a comment]
--                      if ttidx=T_else
--                      or ttidx=T_default then
--                          Aborp("invalid construct (\"case x [fallthrough] else|default\" - omit \"case x\")")
--                      end if
--                      MatchString(T_case)
                        --
                        -- This is a consequence of so-called "smart switch" processing: if
                        --      case 3:
                        --      case 4: <block>
                        -- should behave the same as
                        --      case 3,4: <block>
                        -- then the same principle should equally apply to
                        --      case x:
                        --      default:
                        -- which should behave the same as
                        --      default:
                        -- so yes, it really should be omitted, or someone missed out a break statement.
                        --
                        if ttidx!=T_else
                        and ttidx!=T_default then
                            MatchString(T_case,float_valid:=true)
                        end if
                        if ttidx=T_else
                        or ttidx=T_default then
                            Aborp("invalid construct (\"case x [fallthrough] else|default\" - omit \"case x\", or add \"break\")")
                        end if
                    end if
                    -- add a success for this value (patched to start of Block() below),...
                    scBP = Branch(NoInvert,1,scMerge,scBP)
                    -- ... and any fail within that to the next test: (? "case (a and b)" == "if c=(a and b)" ?)
                    -- backpatch exprBP (fail) to here:
                    if exprBP>wasExprBP then
                        exprBP = backpatch(exprBP,wasExprBP,exprMerge)
                    end if
                    oktoinit = 0
                end while
    --DEV check for T_break/T_exit/T_return?
                switchBP = Branch(Invert,emitElse,ifMerge,switchBP)
                -- treating "switch c case a,b" as "if c=a or c=b then":
                -- patch short circuits (eg if a or b then -> jump after a to if-block)
                if scBP>0 then
                    scBP = backpatch(scBP,0,scMerge)
                    if scBP then ?9/0 end if
                end if
            end if
            
--      elsif ttidx=T_else
--         or ttidx=T_default then
--          if not elsevalid then
--              Aborp("duplicate else/default")
--          end if
--          MatchString(ttidx)
--          elsevalid = 0
--          if toktype=':' then
--              MatchChar(':')
--          elsif toktype=LETTER
--            and ttidx=T_then then
--              MatchString(T_then)
--          end if
        else
            exit            
        end if
        if toktype!=LETTER or not find(ttidx,{T_end,T_else,T_default,T_break}) then

            Block(NO_BREAK)

            clearIchain(-1)
            if exprBP!=0 then ?9/0 end if
        end if
        if toktype!=LETTER then exit end if
--DEV?
--      emitON = (emitON and emitElse)
if emitON then
        ctrltyp = s5[switchtop-1]
end if
        if ttidx=T_break then
            MatchString(T_break)
            -- added 18/12/15:
            if toktype=';' then
                getToken()
            end if
--          if ttidx=T_end then exit end if
            ctrltyp = 0 -- (clear FALLTHRU bit)
-- added 14/2/11:
        elsif ttidx=T_fallthru
           or ttidx=T_fallthrough then
--DEV ? we may want to force ctrltyp/s5[switchop-1] to have SWTABLE bit set..?
if emitON then
            if not and_bits(ctrltyp,SWTABLE) then
                ctrltyp += SWTABLE
                s5[switchtop-1] = ctrltyp
            end if
end if
            MatchString(ttidx) -- T_fallthru/T_fallthrough
--          ctrltyp = or_bits(ctrltyp,FALLTHRU)
            ctrltyp = FALLTHRU  -- (set FALLTHRU bit!)
        end if  
        if ttidx=T_end then exit end if
        if emitON then
            if not and_bits(ctrltyp,FALLTHRU) then
                apnds5({opJmp,endIfMerge,0,EndSwitchBP})
                EndSwitchBP = length(s5)
            end if
        end if
        if ttidx=T_case then
            ctrltyp = ELSIF
        else
            if ttidx!=T_else
            and ttidx!=T_default then
                exit
            end if
            ctrltyp = ELSE
        end if
        if emitline<tokline then
            emitline = tokline-1
        end if
--(old code moved past end procedure)
        -- patch previous if condition jumps to this elsif test
        if switchBP>0 then
            switchBP = backpatch(switchBP,0,ifMerge)
            if switchBP then ?9/0 end if
        else
            -- add a dummy opLabel for redirects
            s5 &= {opLabel,ifMerge,0,0}
        end if
        if emitON then
            s5 &= {opCtrl,ctrltyp,ctrlink,emitline}
            ctrlink = length(s5)-1
            if ctrltyp=ELSE then
                elsectrl = ctrlink
            end if
            if NOLT=0 or bind or lint then
                ltCtrl(ctrlink)
            end if -- NOLT
        end if

        if allWhiteToTokcol() then
            emitline = line-1
        else
            emitline = line
        end if
--DEV?
--      emitON = (wasEmit and emitElse)
        if exprBP!=0 then ?9/0 end if
        oktoinit = 0
    end while
    -- patch any remaining if/elsif (and no else) jumps to s5len (end if)
    -- patch the jumps before elsif and else statements to s5len (end if)

    if switchBP>0 then
        switchBP = backpatch(switchBP,0,ifMerge)
        if switchBP then ?9/0 end if
        if elsectrl!=-1 and elsectrl!=ctrlink then
            -- swap all the links (if "else" was not last):
            if s5[ctrlink-2]!=opCtrl then ?9/0 end if
            if s5[ctrlink-3]!=0 then ?9/0 end if
            if s5[ctrlink-6]!=opLabel then ?9/0 end if
            if s5[elsectrl-2]!=opCtrl then ?9/0 end if
            if s5[elsectrl-6]!=opLabel then ?9/0 end if
            link = s5[elsectrl-3]
            s5[elsectrl-3] = 0
            s5[ctrlink-3] = link
            while link do
                if s5[link-1]!=elsectrl-3 then ?9/0 end if
                s5[link-1] = ctrlink-3
                link = s5[link]
            end while
--DEV what are we doing here? (not sure of that -1, added for opCtrl emitline)
            link = s5[length(s5)-1]
            s5[length(s5)-1] = 0
            s5[elsectrl-3] = link
            while link do
                if s5[link-1]!=length(s5)-1 then ?9/0 end if
                s5[link-1] = elsectrl-3
                link = s5[link]
            end while
        end if
    end if
--  if and_bits(flags,DLL_MAIN) then
--      Aborp("case DLL_PROCESS_ATTACH expected")
--  end if
    if EndSwitchBP>0 then
        EndSwitchBP = backpatch(EndSwitchBP,0,endIfMerge)
        if EndSwitchBP then ?9/0 end if
    end if
--11/6/16:
    if breakBP>0 then
        if backpatch(breakBP,0,breakMerge) then ?9/0 end if
    end if
    breakBP = saveBreakBP

    if ctrlink then
        s5 &= {opCtrl,END+IF,ctrlink,emitline}
        ctrlink = length(s5)-1
        s5[switchtop] = ctrlink
        if NOLT=0 or bind or lint then
            ltCtrl(ctrlink)
        end if -- NOLT
    end if

--2/11/16:
    if casefound=0 then
        MatchString(T_case) -- fatal error
    end if

    MatchString(T_end)
    MatchString(T_switch)
    emitON = wasEmit
    if exprBP!=0 then ?9/0 end if

    clearIchain(saveIchain)

    SideEffects = or_bits(SideEffects,wasSideEffects)
    loopage -= 1

end procedure

constant try_ltype = true

procedure DoTry()
--SideEffects??
integer prev, tlnk, savettidx, E, tryBP, esp4,
        wasEmit = emitON
bool newScope
integer was_loopage = loopage
--  opName("opTry",opTry,4)             -- opTry,tmp,tgt,esp4
--  opName("opTryend",opTryend,5)       -- opTryend,mergeSet(0),tgt,link,tlnk
--  opName("opCatch",opCatch,3)         -- opCatch,tlnk,e
--  opName("opThrow",opThrow,3)         -- opThrow,e,user
--25/2/19:
integer saveIchain = Ichain,
        wasSideEffects = SideEffects,
        waslMask = lMask,
        thispt

    Ichain = -1
if try_ltype then
    SideEffects = E_none
    lMask = E_none

    if emitON then
        if NOLT=0 or bind or lint then
            apnds5({opLoopTop,E_vars,E_vars,0}) -- opLoopTop,lmask,gmask,end
            thispt = length(s5)+1
            ltlooptop(thispt-4)
        end if -- NOLT
    end if
end if

    in_try += 1
    try_loopage = loopage

    MatchString(T_try)
    if emitON then
        exceptions_in_use = 1
        prev = newTempVar(T_atom,Shared)
        esp4 = newTempVar(T_integer,Shared)
        apnds5({opTry,prev,0,esp4})
        tlnk = length(s5)-3
--/*
opCtrl

        if NOLT=0 or bind or lint then
            if s5[thispt-4]!=opLoopTop then ?9/0 end if
            s5[thispt-3] = lMask
            s5[thispt-2] = SideEffects
        end if -- NOLT

    SideEffects = or_bits(SideEffects,wasSideEffects)
    lMask = or_bits(lMask,waslMask)

--or... [NO]
integer iftop, ctrlink, ctrltyp

    ctrlink = 0
    if emitON then
        apnds5({opCtrl,IF,0,emitline})
        iftop = length(s5)-1    -- patched at/pointed to the end if
        ctrlink = iftop         -- where to point next elsif/else/endif
        if NOLT=0 or bind or lint then
            ltCtrl(iftop)
        end if -- NOLT
    end if

    if ttidx=T_elsif then
        ctrltyp = ELSIF
    else
        if ttidx!=T_else then exit end if
        ctrltyp = ELSE
    end if

    if emitON then
        s5 &= {opCtrl,ctrltyp,ctrlink,emitline}
        ctrlink = length(s5)-1
        if NOLT=0 or bind or lint then
            ltCtrl(ctrlink)
        end if -- NOLT
    end if

    if ctrlink then
        s5 &= {opCtrl,END+IF,ctrlink,emitline}
        ctrlink = length(s5)-1
        s5[iftop] = ctrlink
        if NOLT=0 or bind or lint then
            ltCtrl(ctrlink)
        end if -- NOLT
    end if


--*/
    end if

    Block()

    emitON = wasEmit

    clearIchain(-1)

if try_ltype then
    if emitON then
        if NOLT=0 or bind or lint then
            if s5[thispt-4]!=opLoopTop then ?9/0 end if
            s5[thispt-3] = lMask
            s5[thispt-2] = SideEffects
            s5 &= {opCtrl,END+LOOP,thispt-3,emitline}
            s5[thispt-1] = length(s5)-1
            ltCtrl(length(s5)-1)

--          apnds5({opLoopTop,E_vars,E_vars,0}) -- opLoopTop,lmask,gmask,end
--          thispt = length(s5)+1
--          ltlooptop(thispt-4)
        end if -- NOLT
    end if
end if

    MatchString(T_catch)

    if toktype!=LETTER then
        Aborp("an exception variable name is expected here")
    end if
    E = InTable(InVeryTop)
    if E>0 then
        -- permit re-use/extend scope of local variable
        integer cnTyp = symtab[E][S_NTyp],
                vtype = symtab[E][S_vtype]
        if cnTyp=S_Const or cnTyp>S_TVar then
            Aborp("already declared as a "&NTdesc[cnTyp])
        elsif vtype>T_object or not and_bits(vtype,T_Dsq) then
            -- Note: there is no type checking on a catch clause,
            --       hence user defined types are NOT permitted.
            Aborp("type error (exception variable must be sequence, or object)")
        end if
        symtab[E][S_State] = or_bits(symtab[E][S_State],S_set)
        newScope = false
    else
        E = InTable(-InAny)
        if E>0 then
--21/01/2021
--          if symtab[E][S_NTyp]=S_Rsvd then
            if E<=T_Asm or symtab[E][S_NTyp]=S_Rsvd then
                Aborp("illegal use of a reserved word")
            end if
        end if
        integer cvtype = S_TVar
        if returnvar=-1 then    -- (top_level try statements need a gvar)
            cvtype = S_GVar2
        end if
        E = addSymEntry(ttidx,false,cvtype,T_Dsq,0,S_set)
        savettidx = ttidx
        newScope = true
    end if
    if emitON then
        -- put "reset exception handler" on prior/blank line...
        -- (as long as we haven't already emitted code for it)
        emitline = line - (lastline<emitline)
        -- opTryend,mergeSet(0),tgt,link,tlnk
        apnds5({opTryend,0,0,0,tlnk})
        -- ... and the actual catch itself on the catch line.
        emitline = line
        tryBP = length(s5)-1

if try_ltype then
        if NOLT=0 or bind or lint then
            apnds5({opLoopTop,E_vars,E_vars,0}) -- opLoopTop,lmask,gmask,end
            thispt = length(s5)+1
            ltlooptop(thispt-4)
        end if -- NOLT
end if

        -- opCatch,tlnk,e
        apnds5({opCatch,tlnk,E})
    end if
    getToken()

    in_try -= 1
    try_loopage = was_loopage

    if ttidx=T_end then
        Warn("empty catch block",tokline,tokcol,0)
        -- ensure "" is not obscured by a "var is not used":
        symtab[E][S_State] = or_bits(symtab[E][S_State],S_used)
    else
        Block()
    end if

--  clearIchain(-1)
--  clearIchain(saveIchain)
if try_ltype then
    if emitON then
        if NOLT=0 or bind or lint then
            if s5[thispt-4]!=opLoopTop then ?9/0 end if
            s5[thispt-3] = lMask
            s5[thispt-2] = SideEffects
            s5 &= {opCtrl,END+LOOP,thispt-3,emitline}
            s5[thispt-1] = length(s5)-1
            ltCtrl(length(s5)-1)
        end if -- NOLT
    end if
end if

    if newScope then
        -- hide non-pre-declared exception variable
        tt[savettidx+EQ] = symtab[E][S_Nlink]
        symtab[E][S_Nlink] = -2
    end if

    if emitON then
        tryBP = backpatch(tryBP,0,0)
--erm, PushFactor???
--      freeTmp(prev,esp4)
    end if
    MatchString(T_end)
    MatchString(T_try)
--??
    clearIchain(saveIchain)
    SideEffects = or_bits(SideEffects,wasSideEffects)
    lMask = or_bits(lMask,waslMask)

end procedure

--without trace
--with trace
integer C_cr
        C_cr = 0
sequence puts1
         puts1 = {opPuts,T_const1,-1}   -- (-1 is replaced before use)

--with trace
procedure DoQu()
--
-- The '?' shorthand.
--
--DEV/SUG allow ?"Name: ",name (and make the quotes optional somehow, "with[out] quotes"?)
integer t,O
sequence symtabN
--trace(1)
--?{"DoQu",ttidx}
    MatchChar('?',float_valid:=true)
    PushFactor(T_const1,true,T_integer)
--?ttidx
--trace(1)
--fromQU = 1
    Expr(pAllops,asBool)
--fromQU = 0
--?{opTopIsOp} -- 0
    if opTopIsOp then
        opsidxm1 = opsidx-1
        -- check for ?<any>/0
        if opTopIsOp=MathOp
        and opstack[opsidx]=opDiv
        and opstype[opsidxm1]=T_integer then
            p3 = opstack[opsidxm1]
            if p3 then  -- avoid emitON=0 case
                symtabN = symtab[p3]
                if symtabN[S_NTyp]=S_Const
                and symtabN[S_Init]
                and symtabN[S_value]=0 then
                    -- special case: eg ?9/0, just do tmp=9/0 only
                    --      (and pilxl will emit call e02atdb0 only)
--trace(1)
                    p3 = 0
                    opstack[opsidx]=opDiviii
--validate_opstack()
                end if
            end if
            if p3=0 then
                PopFactor()
                freeTmp(-2)         -- kill tmp and the 1 we pushed above.
                SideEffects = or_bits(SideEffects,E_other)
                return
            end if
        end if
        PopFactor() -- (we need an opstype[opsidx] for the signature)
-- added 7/10/2012:
    else
if newEBP then
        -- save eax if rqd
--?opstype
--?opsidx
--fromQU = 1
        saveFunctionResultVars(opsidx,NOTINTS)
--fromQU = 0
--?opstype
end if
    end if
    t = opstype[opsidx]
--?t
--14/9/15:
--  if t=T_string then  -- (added 13/10/09)
    if 0 then -- (I now want consistent quotes...)
        if emitON then  -- (added 10/02/10)
            O = opstack[opsidx]
            puts1[3] = O
--if newEmit then (done below)
--          agcheckop(opPuts)
--end if
            apnds5(puts1)
--          apnds5({opPuts,T_const1,O})
        end if
        freeTmp(-2)
    else
        Call(T_print,{PROC,T_integer,t},PROC,true)
    end if
    -- above is print(1,<expr>), follow it with puts(1,'\n'):
    if emitON then
        if C_cr=0 then
            C_cr = addUnnamedConstant('\n',T_integer)
        end if
        puts1[3] = C_cr
if newEmit then
        agcheckop(opPuts)
end if
        apnds5(puts1)
--      apnds5({opPuts,T_const1,C_cr})
--14/8/15:
        SideEffects = or_bits(SideEffects,E_other)
    end if
end procedure

procedure DoGoto()
--  if in_try and loopage=try_loopage then
    if in_try then
        Aborp("invalid (potentially circumvents try handler set or reset)")
    end if
--?{"GOTO",toktype,LETTER,LABEL,tokline,tokcol}
    ilASM(true) 
    skipSpacesAndComments()
    getToken()
--?{"GOTO2",toktype,tokline,tokcol}
--trace(1)
--  tokline = line
--  tokcol = col
end procedure

--with trace
procedure TopDecls(integer AllowOnDeclaration)
-- Parse and Translate Data Declarations
integer N, Name
integer Typ, rootInt
--, wasAllowOnDeclaration = AllowOnDeclaration
--trace(1)
--if tokline=52 then trace(1) end if
--if fileno=1 and tokline=17 then trace(1) end if

    while 1 do
        Typ = tokno
--      rootInt = (rootType(Typ)=T_integer)
--      rootInt = rootType(Typ)
        rootInt = Typ
        if rootInt>T_object then rootInt = rootType(rootInt) end if
        rootInt = (rootInt=T_integer)
        while 1 do
            emitline = line
            if AllowOnDeclaration=2 then
                AllowOnDeclaration = 1
                tokno = 0
            else
                getToken()
            end if
            if toktype!=LETTER then
                if mapEndToMinusOne='$' and toktype=DIGIT and TokN=-1 then
                    mapEndToMinusOne = 0
                    getToken()
                    exit
                elsif toktype='{' then
                    mapEndToMinusOne = 0
                    MultipleAssignment(S_GVar2,Typ)
                    if toktype!=',' then exit end if
                else
                    Aborp("a name is expected here")
                end if
            else
                mapEndToMinusOne = 0
-- untried (see docs? [not yet written])
--              if not just_static then
                    if InTable(InTop) then Duplicate() end if
--              end if
                N = InTable(-InAny)
                if N>0 then
--21/01/2021
--                  if symtab[N][S_NTyp]=S_Rsvd then
                    if N<=T_Asm or symtab[N][S_NTyp]=S_Rsvd then
                        Aborp("illegal use of a reserved word")
                    end if
                end if
                Name = ttidx
                N = addSymEntry(Name,isGlobal,S_GVar2,Typ,0,0)
--DEV (not yet supported)
--              if isGlobal=2 and fileno=1 and DLL then exports = append(exports,N) end if
                getToken()
                --
                -- Assignment on declaration:
                --
--22/2/17:
                if toktype=':' and Ch='=' then MatchChar(':',false) end if
                if just_static and toktype!='=' then MatchChar('=',false) end if -- error
                if  toktype='='
                or (toktype=LETTER and Name=ttidx) then
                    if toktype=LETTER then
                        -- treat integer x x=1 exactly the same as integer x=1.
                        getToken(float_valid:=false)
                    end if
    --              onDeclaration = rootInt     --DEV or no fwd calls outstanding... (see t45aod.exw)
--                  onDeclaration = (rootInt or no_of_fwd_calls=0)
                    onDeclaration = AllowOnDeclaration and (rootInt or no_of_fwd_calls=0)
--                  fromTopDecls = 1
                    fromTopDecls = AllowOnDeclaration
                    Assignment(N,Typ)
                    fromTopDecls = 0
                    onDeclaration = 0
                end if
                if toktype!=',' then exit end if
            end if
            mapEndToMinusOne = -1
        end while
        if just_static then exit end if
--      Semi()
        if Ch<=0 then exit end if
        if toktype!=LETTER then exit end if
--      if wasAllowOnDeclaration=2 then exit end if
        if ttidx=T_struct
        or ttidx=T_class then
            exit
        end if
        tokno = InTable(InAny)
        if tokno<=0 then exit end if
        if symtab[tokno][S_NTyp]!=S_Type then exit end if
--      if 
--if stids!={} then
--  ?{"TopDecls line 11121",tokno,ttidx,stids,srids,T_struct,Z_struct}
--end if
        isGlobal = 0
    end while
--DEV this messes up -list?
--          emitline = tokline
-- (added 6/1/2013:)
    if toktype=';' then
        getToken()
    end if
-- added 17/2/2020 (!!!) [over ';' in "string e = elapsed(time()-t0); t0 = time()" at top-level]
    if tokno=0 and toktype=LETTER then
        tokno = InTable(InAny)
    end if
end procedure

integer saf_uniq = 0 -- unique test value for struct_add_field()

procedure DoStruct()
--
-- Note this both invokes builtins/structs.e routines to aid further compilation,
--      and emits (almost exactly) the same calls, to be executed at run-time.
--      [The main difference being the compiler has no use for default values.]
--
    -- this isn't actually recursive... (so this should never ever trigger...)
    if class_def!=T_const0 then Aborp("nested structures not supported") end if

    bool bAbstract = false,
         bDynamic = false,
         bNullable = false
    if ttidx=T_abstract then
        bAbstract = true
        getToken()
    end if
    -- allow "global abstract" === "abstract global"
    if not isGlobal then -- but not "global abstract global"!
        if toktype=LETTER and (ttidx=T_global or ttidx=T_public) then
            isGlobal = 1
            getToken()
        end if
    end if
    integer T_class_struct = ttidx,
            cors = find(ttidx,{T_class,T_struct})
    if cors=0 then Aborp("struct or class expected") end if
    MatchString(T_class_struct)
    integer struct_flags = {S_CLASS,S_STRUCT}[cors]
    if toktype!=LETTER then
        Aborp("a name is expected here")
    end if
    integer name_ttidx = ttidx,
            rtntokcol = tokcol,
            rtntokline = tokline

    string struct_name = text[tokcol..col-1],
           base_name = ""
--DEV 7/3/2020. Not ideal...
    if find(struct_name,snames) then Aborp("duplicate") end if
    sequence base_names = {}
    emitline = tokline
    skipSpacesAndComments()
    if not bAbstract and find(Ch,"=:;,") then
        --
        -- A variable then. There are four cases here:
        --  struct a = expr     [ie normal assigned variable[s]...]
        --  struct a := expr    [     ""      ""       ""         ]
        --  struct a;           [ie a single unassigned variable] (nb ';' is mandatory)
        --  struct a,           [ie >1 variable, with first unassigned]
        --
        tokno = Z_struct
        TopDecls(2)
        return
    end if
    integer wasGlobal = isGlobal
    isGlobal = 0
    getToken()
    while toktype=LETTER do
        if    ttidx=T_nullable then bNullable = true
        elsif ttidx=T_dynamic then  bDynamic = true
        else exit 
        end if
        getToken()
    end while
    if toktype=LETTER and ttidx=T_extends then
        getToken()
        base_name = text[tokcol..col-1]
        if not find(base_name,snames) then Aborp("unknown") end if
        if find(base_name,{"class","struct","sequence",""}) then
            Aborp("invalid") -- (run-time equiv also in structs:struct_start)
        end if
        while Ch=',' do -- multiple inheritance...
            getToken()
            if toktype!=',' then ?9/0 end if
            getToken()
            string sbn = text[tokcol..col-1]
            if not find(sbn,snames) then Aborp("unknown") end if
            base_names = append(base_names,{sbn,tokline,tokcol})
        end while
        getToken()
    end if
    if toktype=DQUOTE then
        if bAbstract then Aborp("c structs cannot be abstract") end if
        if bDynamic then Aborp("c structs cannot be dynamic") end if
        if length(base_name) then Aborp("c structs cannot be extended") end if
        if length(base_names) then ?9/0 end if -- (cannot trigger?)
        struct_flags = S_CFFI
        cors = 0
    elsif toktype!=LETTER then
        Aborp("unrecognised")
    elsif bAbstract then
        -- (NB: I am no longer sure about this particular limitation... as per
        --      docs, I am open to persuasion if you have a proper use for it.)
        if bDynamic then Aborp("abstract structs cannot be dynamic") end if
        struct_flags += S_ABSTRACT
    elsif bDynamic then
        struct_flags = S_DYNAMIC
    end if
    if bNullable then
        struct_flags += S_NULLABLE
    end if
    --
    -- First, add a user defined type for this struct definition, eg
    --
    --  "[global] struct person" ==>
    --  [global] type person(object s) return structs:is_struct(s,"person") end type
    --
    integer sN = addUnnamedConstant(struct_name,T_string),
            returnvar = newTempVar(T_integer,FuncRes)
    LIDX = 1 -- (needed for the one tvar)

--  integer state = K_struc+S_set+K_used+S_used,
    integer state = S_set+K_used+S_used,
            N = addSymEntryAt(name_ttidx,wasGlobal,S_Type,{'T',T_object},0,state,rtntokcol),
            pN = addSymEntryAt(-1,0,S_TVar,T_object,0,K_type+S_used,rtntokcol),
            rN = addRoutineId(N)
    emitline = rtntokline

    stids &= N
    srids &= rN
    snids &= sN
    snames = append(snames,struct_name)
    c_or_s = append(c_or_s,cors)

    symtab[N][S_Parm1] = pN
    symtab[N][S_Ltot] = 1
    symtab[N][S_1stl] = rtntokline
    symtab[pN][S_Init] = 1

    if increaseScope(S_Rtn,-1) then end if
    currRtn = N

    if NOLT=0 or bind or lint then
        ltclear(N) -- (see above)
    end if -- NOLT

    PushFactor(pN,true,T_integer)
    PushFactor(rN,true,T_integer)
    Call(T_is_struct,{FUNC,T_integer,T_integer},FUNC,true)
    StoreVar(returnvar,T_integer)
    returntype = T_integer

    agcheckop(opRetf)
    apnds5(opRetf)

    currRtn = dropScope(N,S_Rtn)
    if NOLT=0 or bind or lint then
        ltclear(-N)
    end if -- NOLT
    returnvar = -1
    returnint = 0
    emitline = tokline

    -- </end user_defined_type>

    if cors=0 then  -- S_CFFI
        base_name = TokStr
        getToken()
    end if
--19/2/21
--  structs:struct_start(struct_flags,struct_name,N,base_name)
    try
        structs:struct_start(struct_flags,struct_name,N,base_name)
    catch e
        Aborp(e[E_USER])
    end try
    -- and emit "" (aside: N above isn't pukka, but is "unique enough" for compile-side)
    integer qN = addUnnamedConstant(struct_flags,T_integer),
            bN = addUnnamedConstant(base_name,T_string)
    PushFactor(qN,true,T_integer)
    PushFactor(sN,true,T_string)
    PushFactor(rN,true,T_integer)
    PushFactor(bN,true,T_string)
    Call(T_struct_start,{PROC,T_integer,T_string,T_integer,T_string},PROC,true)
    for i=1 to length(base_names) do
        base_name = base_names[i][1]
        structs:end_struct()
        try
            structs:extend_struct(struct_name,base_name)
        catch e
            ?e
            {?,tokline,tokcol} = base_names[i]
            Aborp("duplicate fields...")
        end try
        integer bX = find(base_name,snames)
        bN = snids[bX]
        Call(T_end_struct,{PROC},PROC,true)
        PushFactor(sN,true,T_string)
        PushFactor(bN,true,T_string)
        Call(T_extend_struct,{PROC,T_string,T_string},PROC,true)
    end for

    if cors!=0 then         -- not S_CFFI
        class_def = rN
        -- initially, fields are private in classes but public in structs,
        --            and methods are public (though usually class-only).
        bool fPrivate = (T_class_struct==T_class),
             mPrivate = false
        while toktype=LETTER and ttidx!=T_end do
            if ttidx=T_type then            -- no udt in classes...
                Aborp("not supported")
            end if
            integer is_method = find(ttidx,{T_proc,T_func}),
                    iPrivate = iff(is_method?mPrivate:fPrivate)
            if ttidx=T_private or ttidx=T_public then
                iPrivate = (ttidx==T_private)
                if Ch=':' then
                    fPrivate = iPrivate
                    mPrivate = iPrivate
                    getToken()
                end if
                getToken()
                is_method = find(ttidx,{T_proc,T_func})
            end if  
if ttidx=T_virtual then ?9/0 end if
if ttidx=T_final then ?9/0 end if
            if is_method then
                integer wasttidx = ttidx
                skipSpacesAndComments()
                bool dtor = (Ch='~')
                if dtor then getToken() end if
                getToken()
                string routine_name = text[tokcol..col-1]
                integer pf = {SF_PROC,SF_FUNC}[is_method]
                if dtor then
                    if ttidx!=name_ttidx then Aborp(routine_name&" expected") end if
                    if wasttidx!=T_proc then Aborp("destructor must be a procedure") end if
                    routine_name = '~'&routine_name
                elsif ttidx=name_ttidx then -- constructor
                    if wasttidx!=T_func then Aborp("constructor must be a function") end if
                else
                    -- 22/12/2020: prevent replacement of function <==> procedure, etc.
                    integer ff = structs:get_field_flags(struct_name,routine_name)
                    if ff!=NULL and and_bits(ff,SF_RTN)!=pf then
                        Aborp("invalid override type")
                    end if
                end if
                nParams = 1             
                paramNames[nParams] = T_this
                paramTypes[nParams] = N
                paramLines[nParams] = tokline
                paramCols [nParams] = tokcol
                paramDflts[nParams] = 0
                paramUsed [nParams] = true
                ttidx = wasttidx    -- (sneaky...)
                iPrivate += pf
                structs:struct_add_field(routine_name,T_integer,iPrivate)
                bFromStruct = true -- allow "show();" - ie abstract/r_lambda==0 
                DoRoutineDef(is_method,true,{N})
                bFromStruct = false

                -- and emit "", plus any default.
                integer rnN = addUnnamedConstant(routine_name,T_string),
                        pvN = addUnnamedConstant(iPrivate,T_integer),
                        mbN = iff(r_lambda!=0?addRoutineId(r_lambda):T_const0)
                sequence sig = {PROC,T_string,T_integer,T_integer,T_integer,T_integer}
                PushFactor(rnN,true,T_string)       -- name
                PushFactor(T_const1,true,T_integer) -- (T_const1==T_integer)
                PushFactor(pvN,true,T_integer)      -- flags (public/private)
                PushFactor(mbN,true,T_integer)      -- default (NULL, or maybe a lamdba)
                PushFactor(T_const1,true,T_integer) -- (T_const1==true)
                Call(T_struct_field,sig,PROC,true)  -- (aka struct_add_field(name,ST_INTEGER,flags,dflt,true))

            else -- (not is_method, ie a field, such as "integer n [= 1]")

                -- string typetok = text[tokcol..col-1]
                tokno = InTable(InAny)
                if tokno<=0 or symtab[tokno][S_NTyp]!=S_Type then
                    Aborp("a type is expected here")
                end if
                integer Typ = tokno,
--5/11/2020:
--                      tN = addUnnamedConstant(Typ,T_integer)
                        tN = 0
                while true do
                    getToken()
                    if toktype!=LETTER or ttidx=T_end then
                        Aborp("a name is expected here")
                    end if
                    string fieldname = text[tokcol..col-1]
                    if structs:get_field_type(struct_name,fieldname)!=NULL then
                        Aborp("duplicate")
                    end if
                    saf_uniq += 1
                    try
                        structs:struct_add_field(fieldname,Typ,iPrivate,saf_uniq)
                    catch e
                        ?e
                        Aborp("duplicate")
                    end try
                    -- and emit "", plus any default.
                    integer fN = addUnnamedConstant(fieldname,T_string),
                            vN = addUnnamedConstant(iPrivate,T_integer)
                    sequence sig = {PROC,T_string,T_integer,T_integer}
                    --  struct_add_field(  name  ,   tid,     flags   [, dflt, bDflt])       
                    PushFactor(fN,true,T_string)
--05/11/2020:
--                  PushFactor(tN,true,T_integer)
                    integer tid = find(Typ,stids)
                    if tid!=0 then
                        tid = srids[tid]
                    else
                        if tN=0 then
--4/2/21...
--                          tN = addUnnamedConstant(Typ,T_integer)
                            if Typ>T_object then
                                tN = addRoutineId(Typ)
                            else
                                tN = addUnnamedConstant(Typ,T_integer)
                            end if
                        end if
                        tid = tN    
                    end if
--??? routine_id?
                    PushFactor(tid,true,T_integer)
                    PushFactor(vN,true,T_integer)
                    getToken()
                    if toktype=':' and Ch='=' then MatchChar(':',false) end if
                    if toktype='=' then
                        MatchChar('=',float_valid:=true)
                        new_struct = Typ
                        Expr(pAllops,asBool)
                        new_struct = 0
                        PushFactor(T_const1,true,T_integer) -- (bDflt:=true)
                        sig = {PROC,T_string,T_integer,T_integer,T_object,T_integer}
                    end if
                    Call(T_struct_field,sig,PROC,true)      -- (aka struct_add_field(name,tid,bPrivate[,<expr>,true]))
                    if toktype!=',' then exit end if
--                  getToken()
                end while
            end if
            if toktype=';' then
                getToken()
            end if
        end while
        structs:end_struct()
        -- and emit ""
        Call(T_end_struct,{PROC},PROC,true)
        class_def = T_const0
    end if
    MatchString(T_end)
    MatchString(T_class_struct)
    if toktype=';' then
        getToken()
    end if
end procedure

--with trace

function GetMultiAssignSet(sequence subscripts, integer isDeclaration, integer Typ)
--
-- subscripts is {} when first called, but not when called recursively. {a,{b,c},d}=rhs
--  recurses with {2}, to get the desired a=rhs[1]; b=rhs[2][1]; c=rhs[2][2]; d=rhs[3];
-- isDeclaration is S_TVar from Locals()  - no subtypes allowed,
--                  S_GVar2 from TopDecls()  - no subtypes allowed,
--                  0 from Statement()  - subtypes allowed,
--                  S_Const from DoConstant()  - subtypes iff typ=T_object only.
-- Typ is 0 from Statement(), concrete (ie fixed) from Locals()/TopDecls(), and
--  defaulted to T_object if not explicitly stated from DoConstant().
--
-- Aside:
--   By "subtypes" I mean the statement {string name, integer id} = f() is fine,
--   as is constant {string name, integer id} = f(), whereas something like
--   integer {string name, integer id} is just nonsense and deserves an error.
--   However we do permit constant object {string name, integer id} = ... but
--   only because it is too much trouble to bother with that one odd case, ie
--   there is no distinction between the MultipleAssignment(S_Const,T_object)
--   and MultipleAssignment(S_Const,Ntype) when Ntype=T_object calls as they
--   are currently invoked from DoConstant().)
--
sequence res = {} -- <list of vars & subscripts to be assigned>
integer i = 1
integer localsubscripts
integer noofsubscripts
object Type
integer varno
integer rtype, etype
integer wasDeclaration = isDeclaration,
        wasTyp = Typ
bool wasdot = false,
     wasfromsubss = fromsubss,
     allowtypes = Typ=0 or (Typ=T_object and isDeclaration=S_Const),
     bNestedStruct = false
--sequence struct_fields = {}

integer pstype, petype

    VAmask = 0  -- (should already be so)
    mapEndToMinusOne = -2
    MatchChar('{',float_valid:=false)
    while toktype!='}' do
        if mapEndToMinusOne='$' and toktype=DIGIT and TokN=-1 then  
            mapEndToMinusOne = 0
            getToken()
            exit
        end if
        mapEndToMinusOne = 0
        -- allow eg {string s, integer i} = .... (added 18/6/17)
        if allowtypes then  
            skipSpacesAndComments()
            if toktype=LETTER then
                tokno = InTable(InAny)
                if tokno>0 then
                    Type = symtab[tokno][S_vtype]   -- (==S_sig)
                    if sequence(Type) and length(Type)=2 and Type[1]='T' then
                        Typ = tokno
                        if isDeclaration=0 then
                            isDeclaration = iff(returnvar=-1?S_GVar2:S_TVar)
                        end if
                        getToken()
                    end if
                end if
                tokno = 0   -- (prevents crash in InTable(InAny) below)
            end if
        end if
        if toktype='{' then
            res &= GetMultiAssignSet(subscripts&i,isDeclaration,Typ)
            MatchChar('}')
        elsif toktype='?' then
            -- missing element, eg {a,?,b} = {1,2,3} is a=1 b=3
            -- {} does the same (as '?') because the nested call
            -- (4 lines above) returns {} (ie res is unchanged).
            MatchChar('?')
        else
--          getvarno() -- literals etc are an error!
            if toktype!=LETTER then
                Aborp("unrecognised")
            end if
            if isDeclaration then
--12/11/15...
--              if InTable(InTop) then Duplicate() end if
                if InTable(InVeryTop) then Duplicate() end if
--1/11/17:
--          end if
--          tokno = InTable(InAny)
                tokno = 0   -- force creation
            else
                tokno = InTable(InAny)
            end if
            bool implied_this = false
            if tokno<=0 then
                if isDeclaration then
                    tokno = addSymEntryAt(ttidx,isGlobal,isDeclaration,Typ,0,0,tokcol)
--DEV (not yet supported)
--                  if isGlobal=2 and fileno=1 and DLL then exports = append(exports,tokno) end if
                elsif class_def!=T_const0 then
                    if class_def!=srids[$] then ?9/0 end if
                    etype = stids[$]
--                  string field_name = trim(text[tokcol..col-1],`" `)
                    string field_name = text[tokcol..col-1]
--?{"line 11511",field_name}
                    if structs:get_field_type(etype,field_name)==NULL
                    and structs:get_struct_type(etype)!=S_DYNAMIC then
                        Aborp(sprintf("no such field (this.%s)",{field_name}))
                    end if
                    toktype = '.'
--integer wasttidx = ttidx
                    ttidx = T_this
                    tokno = InTable(InAny)
                    if tokno<=0 then ?9/0 end if
--                  PushFactor(tokno,false,etype)
                    col = tokcol
                    line = tokline
                    Ch = field_name[1]
--                  Ch = tokcol[1]
--ttidx = wasttidx
--DEV(11/4/20) do this below?
--                  string field_name = trim(text[tokcol..col-1],`"`)
--                  varno = stids[find(class_def,srids)]
--                  if get_field_type(varno, field_name)=NULL
--                  and get_struct_type(varno)!=S_DYNAMIC then
--                      Aborp("no such field")
--                  end if
                    implied_this = true
                else
                    Aborp("undefined")
                end if
--21/01/2021
--          elsif symtab[tokno][S_NTyp]=S_Rsvd then
            elsif tokno<=T_Asm or symtab[tokno][S_NTyp]=S_Rsvd then
                Aborp("illegal use of reserved word")
            end if
--if not implied_this then
            Type = symtab[tokno][S_vtype]   -- (==S_sig)
            if sequence(Type)
            or symtab[Type][S_NTyp]!=S_Type then
                Aborp("Identifier " & getname(ttidx,-2) & " cannot be used here")
            end if
            varno = tokno
if not implied_this then
            getToken()
end if
--end if
--              Assignment(N,Type)
--              Assignment(N,symtab[N][S_ltype])    -- NO NO! 
--              (above would cause errors if we'd just stored an integer in an object, then
--               try to store a sequence in it)
            localsubscripts = 0
            noofsubscripts = 0
--struct_fields = {}
            if toktype='['
            or (ORAC and toktype='.') then
--          or implied_this then
                if isDeclaration!=0 then
                    Aborp("illegal")
                end if
--11/5/21:
--              if with_js=1 and returnvar=-1 then
----                    Aborp("not supported under with js")
--                  Aborp("only allowed inside routine definitions under with js")
--              end if
                rtype = symtab[varno][S_vtype]
                if rtype>T_object then rtype = rootType(rtype) end if
--4/11/19 (structs)
--              if not and_bits(rtype,T_sequence) then
                etype = symtab[varno][S_ltype]
                bool bStruct = find(etype,stids)!=0
--              if not bStruct and not and_bits(rtype,T_sequence) then
                if not and_bits(rtype,T_sequence) then
                    Aborp("attempt to subscript an atom (assigning to it)")         -- eg/ie int[i]=o
                end if
                if symtab[varno][S_NTyp]=S_GVar2 then
--2/3/17:
--                  VAmask += power(2,remainder(varno,29))
                    VAmask = or_bits(VAmask,power(2,remainder(varno,29)))
                end if
                while 1 do
                    mapEndToMinusOne = 1
--integer wastoktype = toktype
                    if bStruct then
                        PushFactor(varno,false,T_sequence)
                    end if
                    if ORAC and toktype='.' then
                        wasdot = true
                        MatchChar('.',float_valid:=false)
                        if bStruct then
                            if toktype!=LETTER then 
                                Aborp("invalid")
                            end if
                        else
                            fromsubss = true
                            GetFactor(false)
                            fromsubss = wasfromsubss
                        end if
                    else
--                  elsif not implied_this then
                        wasdot = false
                        MatchChar('[',float_valid:=true)
                        noofsubscripts += 1
                        Expr(pAllops,asBool)
                    end if
--                  implied_this = false
                    if bStruct then
--                      if toktype!=LETTER and toktype!=DQUOTE then
--                          Aborp("invalid")
--                      end if
--                      if opsidx!=0 then ?9/0 end if
--                      if length(struct_fields) then
                        if bNestedStruct then
--                          PushFactor(struct_fields[$][1],false,T_sequence)
--                          integer fN = addUnnamedConstant(struct_fields[$][2],T_string)
--                          PushFactor(fN,true,T_string)

                            -- (varno/struct, field_name already on the stack)
                            PushFactor(class_def,true,T_integer)    -- (context)
                            Call(T_fetch_field,{FUNC,T_sequence,T_string,T_integer},FUNC,true)
                            if opsidx!=1 then ?9/0 end if
                            noofsubscripts -= 1
                        end if
-->>>
                        string field_name
                        if wasdot then
                            field_name = text[tokcol..col-1]
                            integer fN = addUnnamedConstant(field_name,T_string)
--                  struct_fields = {tidx,fN,true}
                            PushFactor(fN,true,T_string)            -- (field_name)
--                  struct_fields = {tidx,fN,true}
                        else
                            bool const = opsltrl[opsidx]
                            if const and opstype[opsidx]!=T_string then
                                Aborp("invalid (string field name expected)")
                            end if
--                  struct_fields = {tidx,opstack[opsidx],const}
--              opsidx -= 1
                        end if
--<<<
                        pstype = structs:get_struct_type(etype)
--                      struct_fields = {{varno,field_name}}
                        bNestedStruct = true
--                      PushFactor(varno,false,T_sequence)
--                      integer fN = addUnnamedConstant(field_name,T_string)
--                      PushFactor(fN,true,T_string)
--subscripts = struct_fields
                        noofsubscripts += 1
                        if opsidx=1 then
                            varno = opstack[opsidx]
                            opsidx -= 1
                        end if
                        localsubscripts = T_store_field
                        petype = etype
                        if wasdot then
                            etype = structs:get_field_type(etype,field_name)
--DEV(11/4/20) if etype=NULL as from above...
--                      if etype=NULL
--                      and get_struct_type(petype)!=S_DYNAMIC then
--                          Aborp("no such field")
--                      end if
                            bStruct = find(etype,stids)!=0
                            getToken()
-- not in multi-assignment...
                            if not bStruct then exit end if     -- no: s.name[1] should be permitted...
--                          if not find(toktype,".[") then exit end if
                        else
                            bStruct = false
                            exit
                        end if
                    else
                        if toktype=ELLIPSE
                        or (ORAC and not wasdot and toktype=LETTER and ttidx=T_to) then
--16/5/21:
                            if with_js=1 then
                                Aborp("p2js violation: JavaScript does not support slice destructuring")
                            end if
                            not_js = true
                            nj_reason = "slice destructure"
                            getToken(float_valid:=true)
                            Expr(pAllops,asBool)
--DEV replace x[..length(x)] with x[..-1] as per DoSubScripts()...?
                            localsubscripts = SliceOp
--                          noofsubscripts += 1
                        else
                            localsubscripts = SubscriptOp
                        end if
                        mapEndToMinusOne = 0
-- 7/2/17:
--                      if toktype = ',' then
                        if toktype=',' and ((not ORAC) or (not wasdot)) then
                            toktype = '['
                        else
                            if not ORAC or toktype!='.' then
                                if not wasdot then
                                    MatchChar(']',float_valid:=false)
                                end if
                                if localsubscripts=SliceOp then exit end if
                                if toktype!='[' or wasdot then exit end if
                            end if
                        end if
                        if rtype=T_string then Aborp("attempt to subscript an atom (char of string)") end if
--                      noofsubscripts += 1
                    end if
                end while
                -- (in say {a[i+j]} = b, we must do the i+j here)
                if opTopIsOp then
                    isSubscript = 1
                    PopFactor()
                    isSubscript = 0
                end if
            end if -- (if [ or .)
            if localsubscripts=T_store_field then
                if subscripts!={} then ?9/0 end if
                if noofsubscripts!=1 then ?9/0 end if
                -- (aside: varno is already on the stack [along with a field name])
                res = append(res,{varno,localsubscripts,noofsubscripts,{i}})
                bNestedStruct = false
            else
                res = append(res,{varno,localsubscripts,noofsubscripts,subscripts&i})
            end if
--struct_fields = {}
        end if
        if toktype='}' then exit end if
        mapEndToMinusOne = -2
        MatchChar(',',float_valid:=false)
--      if toktype='$' then MatchChar('$') exit end if -- allow ",$}"
        i += 1
        -- reset (see docs)
        isDeclaration = wasDeclaration
        Typ = wasTyp
    end while
    mapEndToMinusOne = 0
--  MatchChar('}')
    return res
end function

procedure MultipleAssignment(integer isDeclaration, integer Typ)
--
-- See tests\t57masgn.exw for examples of what this handles
--
sequence assignset
bool allequal
integer tmp, tmpN, tmpI
sequence ai
integer varno
integer localsubscripts
integer noofsubscripts
sequence subscripts
integer isInit, isCompound
integer idx
sequence idii
integer snNtyp
integer wastokline, wastokcol
integer get_from_stack, noofitems
sequence rhstack
--integer tchk
integer ntype
integer lprev
--integer lblidx
--integer tidx
sequence rhs_stack

    assignset = GetMultiAssignSet({},isDeclaration,Typ)
    integer la = length(assignset)
--?assignset
    SpecialHandling('@', SYMBOL)
    MatchChar('}',float_valid:=false)
    SpecialHandling('@', ILLEGAL)
    allequal = false
    if toktype='@' then
        allequal = true
        MatchChar('@',float_valid:=false)
    end if

    if toktype=':' and Ch='=' then MatchChar(':',false) end if
    MatchChar('=',float_valid:=allequal) -- [eg {a,b,c} @= 3.5]

    wastokline = tokline
    wastokcol = tokcol
    integer wasopsidx = opsidx
    Expr(pAllops,asBool)
--17/11/13:
    get_from_stack = 0
    if opTopIsOp=MkSqOp 
    and not allequal
    and emitON then
        opsidxm1 = opsidx-1
        noofitems = opstack[opsidxm1]
        if noofitems=length(assignset) then
            get_from_stack = 1
            rhstack = opstack[opsidx-noofitems-1..opsidx-2]
            for i=length(assignset) to 1 by -1 do
                ai = assignset[i]
                varno = ai[1]
                if find(varno,rhstack) then get_from_stack = 0 exit end if
                localsubscripts = ai[2]
--18/5/21 (why was this ever??) [at first I cancelled it but T_store_field was needed for t64...]
--              if localsubscripts!=0 then get_from_stack = 0 exit end if
                if localsubscripts=T_store_field then get_from_stack = 0 exit end if
-- (I /think/ this is for {{f},d} = x...)
                subscripts = ai[4]
                noofsubscripts = length(subscripts)
                if noofsubscripts!=1 then get_from_stack = 0 exit end if
            end for
            if get_from_stack then
                for i=length(assignset) to 1 by -1 do
                    assignset[i][4] = {}
--                  assignset[i][4] = assignset[i][4][1..$-1]
                end for
                opsidx -= 2
                opTopIsOp = 0
                rhs_stack = opstack[wasopsidx+1..opsidx]
                if length(rhs_stack)!=la then ?9/0 end if
                opsidx = wasopsidx
            end if
        end if
    end if
    if opTopIsOp then PopFactor() end if
    if emitON then
        saveFunctionResultVars(opsidx,INTSTOO)
    end if
    if not allequal
    and length(assignset)>0
    and not get_from_stack
    and not and_bits(opstype[opsidx],T_sequence) then
        Abork("type error (sequence expected)",opsidx)
    end if
--23/5/21: (move the with_js check into pilx86.e)
--16/5/21:
--  if with_js=1 then
    if la and emitON then
        apnds5({opDeSeq,T_const1})
    end if
--  else
--      not_js = true
--      nj_reason = "opDeSeq omitted"
--  end if
--8/2/18:
    if not get_from_stack then
        tmp = opstack[opsidx]
--  if emitON and not get_from_stack then
        tmpN = 0
        opsidx -= 1
--8/2/18:
--      if not emitON and tmp=0 then
        if emitON and tmp=0 then ?9/0 end if
--      elsif allequal and symtab[tmp][S_Name]=-1 and symtab[tmp][S_NTyp]!=S_Const then
--      if allequal and symtab[tmp][S_Name]=-1 and symtab[tmp][S_NTyp]!=S_Const then
        if allequal and tmp!=0 and symtab[tmp][S_Name]=-1 and symtab[tmp][S_NTyp]!=S_Const then
--10/11/15:
            -- find an lhs element we can use...
            for i=length(assignset) to 1 by -1 do
                ai = assignset[i] -- {varno, localsubscripts, noofsubscripts, subscripts}
                varno = ai[1]
                if symtab[varno][S_Name]!=-1 then
                    localsubscripts = ai[2]     -- 0/SliceOp/subscriptOp
                    if localsubscripts=0 then
                        subscripts = ai[4]
                        if length(subscripts)!=1 then ?9/0 end if
                        if emitON then
--if fileno=1 then
--?{"pmain line 12090, varno=",varno}
--end if
                            emitHexMov(opMove, varno, tmp)
                        end if
--17/1/2020 (sequence {this,that} @= repeat({},4) said that not assigned)
                        symtab[varno][S_State] = or_bits(symtab[varno][S_State],S_set)
                        tmp = varno
                        assignset[i..i] = {}
                        exit
                    end if          
                end if          
            end for
--          ?9/0
            if symtab[tmp][S_Name]=-1 then
                tokline = wastokline
                tokcol  = wastokcol
                Aborp("sorry, construct requires at least one plain var")
                -- eg {a[i],b[j]} @= f()
                -- The trouble is that the result of f() ends up in an un-named temporary, and
                -- moving anything out of said avoids an unnecessary incref by h4-ing the temp.
                -- In a statement such as {a[i],b,c[k]} @= f(), the compiler performs the b=f()
                -- first, and uses b instead of the unnamed temp for the remaining assignments.
                -- However when nothing with a name is available, then you get the above error.
                -- If this gets really irksome, we /could/ (possibly) enhance the compiler to
                -- automatically perform a[i] = b[j] to complete the operation, but unless we
                -- /really/ have to, I'd prefer to avoid such complications. Bearing in mind
                -- that we might have to repeat that trick for several terms, the difficulty
                -- of adequate testing, and the probability of adding further bugs, it seems 
                -- better to force the programmer to use a named temp for the result of f().
                -- (the "obvious" solution of stashing the rhs in a new temp achieves nowt.)
            end if
        end if
    end if
    for i=length(assignset) to 1 by -1 do
        ai = assignset[i] -- {varno, localsubscripts, noofsubscripts, subscripts}
        varno = ai[1]
        if not get_from_stack and varno=tmp and i>1 then
--          tokline = wastokline
--          tokcol  = wastokcol
--          Aborp("in {..}=x, can only assign x in the leftmost lhs element")
            -- Create a temp and stash it... (the new temp will always be
            -- subscripted, so we don't have the @= problem outlined above)
            if emitON then
--              varno = newTempVar(Typ,Shared)
                varno = newTempVar(T_object,Shared)
                emitHexMov(opMove,varno,tmp)
                tmp = varno
                varno = ai[1]
            end if
        end if
        localsubscripts = ai[2]     -- 0/SliceOp/subscriptOp
        if localsubscripts=0 then
--if fileno=1 then
--?{"pmain line 12138, varno=",varno}
--end if
            symtab[varno][S_State] = or_bits(symtab[varno][S_State],S_set)
        end if
        if symtab[varno][S_Init]=0 then
            symtab[varno][S_Init] = Ichain
            Ichain = varno
        end if
        snNtyp = symtab[varno][S_NTyp]
        if snNtyp=S_GVar2 then
            SideEffects = or_bits(SideEffects,power(2,remainder(varno,29)))
        elsif snNtyp=S_TVar then
            lMask = or_bits(lMask,power(2,remainder(varno,29)))
        end if
        if emitON then
            subscripts = ai[4]
            if allequal then -- all become equal to (@=)
                subscripts = subscripts[2..$]
            end if
            noofsubscripts = length(subscripts)
            if noofsubscripts then
                if get_from_stack then ?9/0 end if -- sanity check
                if localsubscripts=0 then
                    tmpI = varno    -- store directly,
                    varno = 0       -- and signal that we have
                else
                    if tmpN=0 then
                        tmpN = newTempVar(T_object,Shared)
                    end if
                    tmpI = tmpN     -- (we need to do a tmpI:=tmp[...] first)
                end if

                if noofsubscripts=1 then
    
                    idx = addUnnamedConstant(subscripts[1],T_integer)
                    isInit = 3 -- both
                    isCompound = 0
if newEmit then
                    agcheckop(opSubse1)
--                  tidx = aatidx[opSubse1]
--                  if tidx=0 then ?9/0 end if
--                  if tt[tidx+EQ]=0 then
--                      agcheck(tidx)
----                        lblidx = get_lblidx(tidx)
--                  end if
end if
                    apnds5({opSubse1,tmpI,tmp,idx,isInit,isCompound})   -- tmpI := tmp[idx]

                else    -- noofsubscripts!=1
                    --
                    -- opSubse is N, res, idxN..idx1, ref
                    --      implements res := ref[idx1][idx2]~[idxN]
                    --
                    idii = {}
                    for j=noofsubscripts to 1 by -1 do
                        idx = addUnnamedConstant(subscripts[j],T_integer)
                        idii &= idx
                    end for
if newEmit then
                    agcheckop(opSubse)
--                  tidx = aatidx[opSubse]
--                  if tidx=0 then ?9/0 end if
--                  if tt[tidx+EQ]=0 then
--                      agcheck(tidx)
----                        lblidx = get_lblidx(tidx)
--                  end if
end if
                    apnds5({opSubse,noofsubscripts,tmpI})               -- tmpI := tmp[idx1][idx2]...
                    idii &= tmp
                    apnds5(idii)
                end if
            else
                if get_from_stack then
--18/5/21
--                  tmpI = opstack[opsidx]
--more??
--                  freeTmp(-1)
--                  opsidx -= 1
                    tmpI = rhs_stack[$]
                    rhs_stack = rhs_stack[1..$-1]
                else
                    tmpI = tmp
                end if
            end if
        end if
        if localsubscripts then -- (= SliceOp or SubscriptOp or T_store_field)
            noofsubscripts = ai[3]
            if localsubscripts=SliceOp then
                --
                -- opReps is n (noofsubscripts), rep, sliceend, idxn..1, ref
                --      (implements ref[idx1]~[idxn..sliceend] := rep)
                --
                idii = {}
                for j=noofsubscripts+1 to 1 by -1 do
                    if emitON then
                        idx = opstack[opsidx]               -- sliceend,idxn..idx1
                        if not symtab[idx][S_Init] then
                            Unassigned(idx)
                        end if
                        idii &= idx
                    end if
                    freeTmp(-1)
                end for
                if emitON then
if newEmit then
                    agcheckop(opReps)
                    -- (no need to Unassigned(tmpI), I assume)
end if
                    apnds5({opReps,noofsubscripts,tmpI})
                    idii &= varno
                    apnds5(idii)    -- sliceend,idxn..idx1, ref
                end if -- emitON
            elsif localsubscripts=SubscriptOp then
--6/6/14:
                if not symtab[varno][S_Init] then
                    Unassigned(varno)
                end if
                if noofsubscripts=1 then
                    --
                    -- opRepe1, ref[idx]=rep.
                    --
                    if emitON then
                        idx = opstack[opsidx]
                        if not symtab[idx][S_Init] then
                            Unassigned(idx)
                        end if
if newEmit then
                        agcheckop(opRepe1)
end if
                        apnds5({opRepe1,varno,idx,tmpI})    -- opRepe1,dest,idx,rep
                    end if -- emitON
                    freeTmp(-1)

                else -- noofsubscripts!=1
                    --
                    -- opRepe is n (noofsubscripts), rep, idxn..idx1, ref
                    -- opRepe is n (noofsubscripts), ref, rep, idx1..idxn           -- ref[idx1][idx2]~[idxn] := rep
                    --      implements ref[idx1][idx2]~[idxn] := rep
                    --
                    idii = {}
                    for j=noofsubscripts to 1 by -1 do
                        if emitON then
                            idx = opstack[opsidx]               -- rep,idxn..idx1
                            if not symtab[idx][S_Init] then
                                Unassigned(idx)
                            end if
                            idii &= idx
                        end if
                        freeTmp(-1)
                    end for
                    if emitON then
if newEmit then
                        agcheckop(opRepe)
end if
                        apnds5({opRepe,noofsubscripts,tmpI})    -- ref[idx1]..[idxn]=rep
                        idii &= varno
                        apnds5(idii)
                    end if -- emitON
                end if -- noofsubscripts
            elsif localsubscripts=T_store_field then
                if noofsubscripts!=1 then ?9/0 end if
--              tokline = wastokline
--              tokcol  = wastokcol
--              Aborp("localsubscripts=T_store_field")  --DEV... (not yet attempted)
--              ?9/0
--/*
--keep:
                -- varno and field name already on stack...

                if opstype[opsidx]!=T_string then ?9/0 end if
                if opsltrl[opsidx]!=true then ?9/0 end if
                integer fN = opstack[opsidx]
--              freeTmp(-1) -- no!
                opsidx -= 1 -- (it is going right back on!)
--X             PushFactor(tidx,false,T_Dsq)
--              PushFactor(tmpI,false,T_Dsq)            -- ref (struct)
                PushFactor(varno,false,T_Dsq)           -- ref (struct)
--X subscripts = {{1048,`b1`},4}
--X             integer fN = addUnnamedConstant(struct_fields[$][2],T_string)
--              integer fN = addUnnamedConstant(subscripts[1][2],T_string)
--              integer fN = addUnnamedConstant(opstack[opsidx],T_string)
--              PushFactor(fN,true,T_string)            -- field name
                PushFactor(fN,true,T_string)            -- field name
--*/
--X     Expr(pAllops,asBool)
--              PushFactor(varno,false,T_object)        -- value (rep)
                PushFactor(tmpI,false,T_object)         -- value (rep)
--      if opTopIsOp then PopFactor() end if
--      if opsidx!=3 then ?9/0 end if
--      integer rep = opstack[opsidx]
        --erm... (because Call() is not expected on lhs of assignment...)
--      VAmask = 0      -- (... as per the same below/before rhs)
                PushFactor(class_def,true,T_integer)    -- context
--T_fetch_field
                Call(T_store_field,{PROC,T_sequence,T_string,T_object,T_integer},PROC,true)
--*!/
            else
                ?9/0
            end if -- Sliceop/SubscriptOp/T_store_field

        else -- not SliceOp or SubscriptOp
            if emitON then
                if varno!=0 then -- (not already done above)
                    onDeclaration = 0
                    emitHexMov(opMove,varno,tmpI)
                end if
                varno = ai[1]
--/*
                ntype = symtab[varno][S_vtype]
                tchk = optset[OptTypeCheck]
                if not tchk then
                    if ntype>T_object then ntype = rootType(ntype) end if
                end if
                if tchk and ntype!=T_object then
--                  emitline = tokline  -- erm?
if newEmit then
                    agcheckop(opTchk)
end if
                    apnds5({opTchk,varno,tchk,0})
                end if
--*/
            end if -- emitON
        end if -- not SliceOp or SubscriptOp
--18/5/21:
        if get_from_stack then
            zero_temp(tmpI)
        end if
--4/1/15:
        ntype = symtab[varno][S_vtype]
-- added 16/1/15:
        if emitON and optset[OptTypeCheck] then
--          ntype = symtab[varno][S_vtype]
--DEV why are we doing this? (spotted in passing) [removed 4/2/15, see what bites]
--          if ntype>T_object then ntype = rootType(ntype) end if
            if ntype!=T_object then
--              emitline = tokline  -- erm?
if newEmit then
                agcheckop(opTchk)
end if
--              apnds5({opTchk,varno,tchk,0})
                apnds5({opTchk,varno,1,0})
            end if
        end if
--4/2/15:
        if NOLT=0 or bind or lint then
            if emitON then
--PL 6/3/17(??)
--              Lmin = MININT
--              Lmax = MAXINT
                lprev = symtab[varno][S_ltype]
                ltAdd(SET,varno,lprev,ntype,length(s5))
            end if
        end if -- NOLT
    end for
    VAmask = 0
--23/5/21 ditto (move the with_js check to pilx86.e).
--16/5/21:
--  if with_js=1 then
    if la and emitON then
        apnds5({opDeSeq,T_const0})
    end if
--  end if
--27/4/21:
    if get_from_stack then
        if length(rhs_stack)!=0 then ?9/0 end if
    else
        zero_temp(tmp)
    end if
--/*

--
-- Programming note:
-- =================
--  This code fragment may be as incomplete as it is indecipherable!
--  It seems to cover the basics, see t47ltth.exw before amending.
--
    if subscript then
-- usetdsq?
        RHStype = T_sequence    -- treat a[i]=x as a=<seq/str>, rather than a=x.
        --DEV what about string[i]=char??? [would need to be literal char?]
    end if
    -- do we need to typecheck?
--  if Type!=mergeType(RHStype,Type,TYPECHECK) then
    if Type!=mergeType2(RHStype,Type) then
        --DEV new code 23/10/09:
        if 0 then
            w = optset[OptTypeCheck]
            ntype = Type
--DEV (spotted in passing 14/6/10) should this be w=0??!!
            if w then
                if ntype>T_object then ntype = rootType(ntype) end if
            end if
            if emitON and ntype!=T_object then
if newEmit then
                agcheckop(opTchk)
end if
                apnds5({opTchk,tidx,w,0})
            end if

        else
            if optset[OptTypeCheck]
            and emitON then
if newEmit then
                agcheckop(opTchk)
end if
                apnds5({opTchk,tidx,1,0})
            end if
        end if
        ntype = Type    -- if we typecheck, use that as the new localtype
    else
        -- determine best possible new ltype
--      ntype = mergeType(RHStype,Type,NEWLTYPE)
        ntype = mergeType3(RHStype,Type)
    end if
    lprev = symtabN[S_ltype]
    symtab[tidx] = symtabN
    symtabN = {}    -- (avoids clone in ltAdd)
    if NOLT=0 or bind or lint then
        if emitON then
            ltAdd(SET,tidx,lprev,ntype,length(s5))
        end if
    end if -- NOLT
    --DEV if we have say:
    --  integer I
    --  object O
    --      I=O
    --  Then we can also deduce O has a localtype of integer.....
    --  We probably don't want to stomp on a better udt though,
    --   eg/ie if is_even_int(O) then
    --              I=O
    --   should leave O's localtype of is_even_int alone - and
    --   let's have "type is_even_int(integer x)" return false
    --   rather than crash should you pass it a sequence etc.
    --   However, note that is_even_int("string") would yet be
    --   correct in triggering a compilation error, btw.

    if DEBUG then
        if opsidx!=0 then ?9/0 end if
    end if

--*/
    if opsidx then ?9/0 end if  -- leave in (outside if DEBUG then)
end procedure

--integer Z_format -- T_format until include/code processed
integer Z_format -- T_format until Statement() processed

--with trace
procedure Statement()
--procedure Statement(integer flags)
--
-- Parse and Translate a single Statement
--
object Type
integer N, isLit, etype
-- 25/1/20
--integer wasZformat -- quick restore for trace()/profile() [only!] (DEV: iff we can keep it totally cross-platform)
--
--  wasZformat = Z_format
--  Z_format = 0
--if fileno=1 then
--  ?{"Statement",tokno,tokline}
--end if

    if Z_format!=0 
    and ttidx!=T_trace
    and ttidx!=T_profile then
        Z_format = 0
        Alias_C_flags()
    end if
----    LastStatementWasReturn = 0 -- removed 6/9/04 (see sort.e)
    LastStatementWasAbort = 0
--  LastStatementWasExit = 0
    emitline = line
    oktoinit = 1
--puts(1,"Statement\n")
    if toktype!=LETTER then
        if toktype='?' then
            DoQu()
        elsif toktype=HEXDEC then
            getToken()
--          if    ttidx=T_ilasm then ilasm()
--          if    ttidx=T_ilasm then Warn("deprecated\n",tokline,tokcol,0) ilasm()
            if    ttidx=T_ilASM then ilASM()
            elsif ttidx=T_istype then istype()
            elsif ttidx=T_isinit then isinit()
            elsif ttidx=T_isginfo then isginfo()
            else Aborp("ilasm, istype, isinit, or isginfo expected")
            end if
        elsif toktype='{' then
            MultipleAssignment(0,0)
        elsif toktype=LABEL then
            DoGoto()
        else
            Aborp("unrecognised")
        end if
--  elsif ttidx=T_if then           DoIf(flags)
    elsif ttidx=T_if then           DoIf()
    elsif ttidx=T_for then          DoFor()
--  elsif ttidx=T_while then        DoWhile()
    elsif ttidx=T_while 
       or ttidx=T_do then           DoWhile()
--DEV elsif ttidx=T_try then        DoTry()?
    elsif ttidx=T_exit then         DoExit()
    elsif ttidx=T_break then        DoBreak()
    elsif ttidx=T_continue then     DoContinue()
    elsif ttidx=T_return then       DoReturn()
--  elsif ttidx=T_switch then       DoSwitch(flags)
    elsif ttidx=T_switch then       DoSwitch()
    elsif ttidx=T_try then          DoTry()
    elsif ttidx=T_goto then         DoGoto()
    elsif NESTEDFUNC 
      and ttidx=T_func then         DoRoutineDef(R_Func)
    else
        skipSpacesAndComments()
        N = tokno
        if N<=0 then -- forward procedure call?
            -- 26/12/19: (implicit "this")
            if class_def!=T_const0 then
                if class_def!=srids[$] then ?9/0 end if
                etype = stids[$]
                string field_name = trim(text[tokcol..col-1],`" `)
                if structs:get_field_type(etype,field_name)==NULL then
                    Aborp(sprintf("no such field (this.%s)",{field_name}))
                end if
                toktype = '.'
                ttidx = T_this
                N = InTable(InAny)
                if N<=0 then ?9/0 end if
                PushFactor(N,false,etype)
                col = tokcol
                Ch = field_name[1]
                Type = symtab[N][S_vtype]   -- (==S_sig)
                Assignment(N,Type)
            else
                if Ch!='(' then Undefined() end if
                if FWARN then
                    if not testall then
                        Warn("forward call assumed",tokline,tokcol,0)
                    end if
                end if
                ForwardProc(PROC)
            end if
--21/01/2021
        elsif symtab[N][S_NTyp]=S_Rsvd then
--      elsif N<=T_Asm or symtab[N][S_NTyp]=S_Rsvd then
            Aborp("illegal use of reserved word")
        else
            Type = symtab[N][S_vtype]   -- (==S_sig)
            if sequence(Type) then
--25/1/20
----                if find(ttidx,{T_trace,T_profile}) then
--              if ttidx=T_trace
--              or ttidx=T_profile then
--                  Z_format = wasZformat
----if bind then trace(1) end if
----if not bind then
----                Call(N,Type,PROC,false)
----end if
--              end if
--if newEmit and N=T_abort then
--?{N,T_abort}
--puts(1,"abort()->Ch=-1 line 9422 pmain.e\n")
--getToken()
--Ch = -1
--else
                Call(N,Type,PROC,false)
--end if
--27/10/19: (first-class routine_ids)
--          elsif Type=T_integer and toktype='(' then
--23/5/21:
--          elsif Type=T_integer and Ch='(' then
            elsif Ch='(' then
--SUG:          DoFirstClassRid(PROC)
--;  35 call_proc(r_show,{})
--  mov eax,[#0040293C] (r_show)          ;#004270A5: 241 3C294000               uv 01 00  1   4      
--  mov esi,[#0040225C]                   ;#004270AA: 213065 5C224000            vu 40 00  1   4      
--  call #00432016 (:%opCallProc)         ;#004270B0: 350 61AF0000               v  00 00  1   5      

--DEV...?
--          if emitON then
                isLit = (and_bits(symtab[N][S_State],K_lit)=K_lit)
                PushFactor(N,isLit,T_integer)
                getToken()
                -- (aside: no dot, no defaulted struct!)
                DoSequence('(')
                SideEffects = or_bits(SideEffects,E_all)
--if newEBP then
                -- save eax if rqd
                saveFunctionResultVars(opsidx,NOTINTS)
--end if
--              if opTopIsOp then ?9/0 end if
--              if opTopIsOp then Aborp("illegal") end if
                if opTopIsOp then PopFactor() end if
                if opsidx!=2 then ?9/0 end if
                integer p1 = opstack[1],
                        p2 = opstack[2],
                        opcode = opCallProc
--if newEmit then
                if not symtab[p1][S_Init] then
                    Unassigned(p1)
                end if
                if not symtab[p2][S_Init] then
                    Unassigned(p2)
                end if
                agcheckop(opcode)
--end if
                apnds5({opcode,p1,p2})
--??            apnds5({opCleanup,p2})
                freeTmp(-opsidx)
--              ?9/0
            elsif symtab[Type][S_NTyp]=S_Type then
--if fileno=1 then ?{Type,T_integer,toktype,'('} end if
--if tokline=201 and Ch='.' then trace(1) end if
--if tokline=200 and fileno=1 then trace(1) end if
--?tokline
                getToken()
                Assignment(N,Type)
--              Assignment(N,symtab[N][S_ltype])    -- NO NO! 
--              (above would cause errors if we'd just stored an integer in an object, then
--               try to store a sequence in it)
            else
                Aborp("Identifier " & getname(ttidx,-2) & " cannot be used here")
            end if
        end if
    end if
--  Semi()
    if toktype=';' then
        getToken()
    end if
    if toktype=LETTER then
        tokno = InTable(InAny)
    end if
end procedure

--without trace
--with trace
procedure Block(integer flags=0)
--
-- Parse and Translate a Block of Statements
--
integer drop_scope = 0

----    LastStatementWasReturn = 0
--  if not tokno then
    if not tokno and toktype=LETTER then
        tokno = InTable(InAny)
    end if
--if fileno=1 then
--  ?{"Block",tokno,tokline}
--  if tokline=195 then trace(1) end if
--end if
    LastStatementWasAbort = 0
    top_level_abort = 0
    while 1 do
        if toktype!=LETTER then
            if not find(toktype,{HEXDEC,'?','{',LABEL}) then exit end if
        else
            if find(ttidx,T_endelseelsif) then exit end if
--          if find(ttidx,{T_end,T_else,T_elsif,
--                         T_case,T_default,--T_break,
--                         T_fallthru,T_fallthrough}) then exit end if
            if and_bits(flags,NO_BREAK)
            and ttidx=T_break then
                exit
            end if
            if tokno<=0 then
                tokno = InTable(InAny)
            end if
            if tokno>0 and symtab[tokno][S_NTyp]=S_Type then
                if not drop_scope then
                    drop_scope = 1
                    if increaseScope(S_Block,-1) then end if
                end if
                if returnvar=-1 then    -- bugfix 9/9/15
--if emitON=0 then
--  -- TopDecls() has no emitON handling.
--  -- Fixing this may be much simpler that I first feared.
--  Aborp("sorry, not (yet) supported under emitON=0")
--end if
                    TopDecls(0)
                else
                    Locals(0)
                end if
                if find(ttidx,T_endelseelsif) then exit end if
--              if find(ttidx,{T_end,T_else,T_elsif,
--                             T_case,T_default,--T_break,
--                             T_fallthru,T_fallthrough}) then exit end if
                if and_bits(flags,NO_BREAK)
                and ttidx=T_break then
                    exit
                end if
            end if
        end if
--      Statement(flags)    -- (NO_BREAK has no meaning any deeper:)
--      Statement(and_bits(flags,DLL_MAIN))
        Statement()
    end while
    if drop_scope then
        --DEV we should really decref things here...
        if dropScope(-1,S_Block) then end if
    end if
    LastStatementWasAbort = 0
    if probable_logic_error then show_ple() end if
end procedure
--rBlock = routine_id("Block")


with trace

procedure DoConstant()
integer N, Ntype, state
integer wasttidx        -- save over Expr(pAllops)
integer O               -- copy of opstack[opsidx]
sequence symtabN, symtabO   -- copies of symtab[x]
integer slink, glink, scope
integer wastokcol, wastokline
integer SNtyp
--object dbg
--if isGlobal then
--  trace(1)
--end if
--if tokline=404 then trace(1) end if
    if DEBUG then
        if opsidx!=0 then ?9/0 end if
    end if
    Ntype = T_object
    asConst = isGlobal
    while 1 do
        getToken()  -- "constant" and ","
        if toktype!=LETTER then
            -- support constant a=1,$ for OpenEu compatibility
            if mapEndToMinusOne='$' and toktype=DIGIT and TokN=-1 then
                mapEndToMinusOne = 0
                getToken()
                exit
            elsif toktype='{' then
                mapEndToMinusOne = 0
                MultipleAssignment(S_Const,T_object)
                if toktype!=',' then exit end if
            else
                Aborp("a name is expected here")
            end if
        else
            mapEndToMinusOne = 0
--30/5/16 (moved below)
--          if InTable(InTop) then Duplicate() end if
--28/7/16:
--          N = InTable(InAny)
            N = InTable(-InAny)
            if N>0 then
                SNtyp = symtab[N][S_NTyp]
--21/01/2021
                if SNtyp=S_Rsvd then
--              if N<=T_Asm or SNtyp=S_Rsvd then
                    Aborp("illegal use of a reserved word")
-- added 6/4/2012: allow eg
-- constant integer Main=create(Window...)  -- typechecks if create returns {}.
-- also valid, in the name of optionally allowing the programmer to be doubly-explicit:
-- constant integer K=1
-- constant integer A=1,B=2,C=3, sequence T="T",U="U",V="V"
-- Omitting the type, as all legacy code does, is effectively the same as coding
-- "constant object", which means "no type check; infer the type as best you can".
                elsif SNtyp=S_Type then
                    skipSpacesAndComments()
--6/3/18:
--                  if Ch!=-1 and chartype=LETTER then
                    if Ch!=-1 and (chartype=LETTER or Ch='{') then
                        Ntype = N
--?{"DoConstant (pmain.e line 12588); new_struct is",new_struct,"=>",N}
                        new_struct = N
                        getToken()
                    else
                        Warn("variable name assumed, not type",tokline,tokcol,0)
                    end if
--30/5/16:
                else
                    if InTable(InTop) then Duplicate() end if
                end if
            end if
            if toktype='{' then
                MultipleAssignment(S_Const,Ntype)
            else
                --
                -- In eg constant x={1,2}, Expr may create an unnamed S_Const {1,2},
                --  so where possible re-use that entry for the named constant.
                --
                wasttidx = ttidx
                wastokcol = tokcol
                wastokline = tokline
                getToken()
        -- added 6/4/2012 (allow ":=" as well as "=")
                if toktype=':' and Ch='=' then MatchChar(':',false) end if
                MatchChar('=',float_valid:=true)
                Expr(pAllops,asBool)
                if opsltrl[opsidx]!=1 then
        --          N = addSymEntryAt(wasttidx,isGlobal,S_Const,T_object,0,0,wastokcol)
                    N = addSymEntryAt(wasttidx,isGlobal,S_Const,Ntype,0,0,wastokcol)
--23/6/16:
                    if opsidx=1 then
                        integer k = opstack[1]
                        if and_bits(symtab[k][S_State],K_Fres)
                        and not find(symtab[k+1][S_Efct],{E_none,E_other}) then
                            symtab[N][S_State] = or_bits(symtab[N][S_State],S_used)
                        end if
                    end if
--                  storeConst = 1
                    storeConst = (Ntype==T_object)
                    onDeclaration = 1
--                  StoreVar(N,-1)
                    StoreVar(N,Ntype)
                    storeConst = 0
                    onDeclaration = 0
                    symtab[N][S_vtype] = RHStype
                else
                    O = opstack[opsidx]
                    symtabO = symtab[O]
                    if Ntype!=T_object
--                  and not and_bits(Ntype,symtabO[S_vtype]) then
                    and not and_bits(rootType(Ntype),rootType(symtabO[S_vtype])) then
                        tokline = wastokline
                        tokcol  = wastokcol
                        Aborp("type error")
                    end if
        --DEV and the right fileno?
                    if equal(symtabO[S_Name],-1) then
                        --
                        -- In eg P({1,2}) constant y={1,2} the second Expr() [ie after "y="]
                        --  yields the unnamed constant, so put our name etc on it.
                        --
                        N = O
                        slink = tt[wasttidx+EQ]
                        scope = 0
                        if isGlobal then
                            --
                            -- keep the S_Nlink chain as {locals},{globals} so that dropScope can
                            --  just pick off the top element, rather than go hunting for it.
                            --
                            glink = 0
                            while slink do
                                symtabN = symtab[slink]
                                if and_bits(symtabN[S_State],K_gbl) then exit end if
                                glink = slink
                                slink = symtabN[S_Nlink]
                            end while
                        else
                            scope = localscopes[scopelevel]
                        end if
--p2js:
--                      symtabN = symtab[N]
                        symtabN = deep_copy(symtab[N])
                        symtab[N] = 0
                        symtabN[S_Name] = wasttidx
                        symtabN[S_FPno] = fileno
                        symtabN[S_Nlink] = slink
                        if isGlobal then
                            if glink then
                                symtab[glink][S_Nlink] = N
                            else
                                tt[wasttidx+EQ] = N
                            end if
                        else
                            tt[wasttidx+EQ] = N
                            localscopes[scopelevel] = N
                        end if
        --              if isGlobal or not optset[OptWarning] then      --DEV see also addsymentry
                        if isGlobal then
                            state = symtabN[S_State]
                            state = or_bits(state,S_used_set_gbl)
                            symtabN[S_State] = state
                        end if
                        symtabN[S_Slink] = scope

                    else -- S_Name!=-1
                        --
                        -- In eg constant x={1,2}, y={1,2} the second Expr() [ie after "y="]
                        --  yields the named constant x, so clone and S_Clink it.
                        -- DEV: could we get away with just setting tt[wasttidx+EQ] to O (if same fileno)?
                        --
        --              N = addSymEntryAt(wasttidx,isGlobal,S_Const,T_object,0,K_litnoclr,wastokcol)
                        N = addSymEntryAt(wasttidx,isGlobal,S_Const,Ntype,0,K_litnoclr,wastokcol)
                        symtabN = symtab[N]
--2/1/17: (re-fetch in case S_Nlink updated by addSymEntry! [full desc also added to readme.txt])
                        symtabO = symtab[O]
                        symtab[N] = 0
                        symtab[O] = 0
                        symtabN[S_value] = symtabO[S_value]     -- 8
                        symtabN[S_Init]  = symtabO[S_Init]      -- 11
        --21/3/9:
                        symtabN[S_Clink] = symtabO[S_Clink]     -- 9
                        symtabO[S_Clink] = N
        --16/4/09:
        --              symtabN[S_Clink] = tt[C_ttidx+EQ]
        --              tt[C_ttidx+EQ] = N
        --              symtabN[S_Clink] = O
        --if tt[wasttidx+EQ] != N then ?9/0 end if
        --              tt[wasttidx+EQ] = N
                        
        -- 9/1/9
                        if length(symtabO)>=S_gInfo then    -- as added by DoSequence
        --                  symtabN = append(symtabN,symtabO[S_gInfo])
                            symtabN[S_gInfo] = symtabO[S_gInfo]
                        end if
                        symtab[O] = symtabO
                    end if
                    RHStype = opstype[opsidx]
                    symtabN[S_vtype] = RHStype
                    symtabN[S_ltype] = RHStype
                    symtabN[S_ErrV] = wastokcol
                    opsidx -= 1
                    symtab[N] = symtabN
                end if
            end if
            if toktype!=',' then exit end if
        end if
        mapEndToMinusOne = -1
    end while
    new_struct = 0
    asConst = -1
    if DEBUG then
        if opsidx!=0 then ?9/0 end if
    end if
-- added 18/10/13:
    if toktype=';' then
        getToken()
    end if
end procedure


--with trace
integer prevfile
sequence default_namespaces

procedure IncludeFile()
string name, nameSpace
integer k, kp1, ln, emitcol, N, reinclude, newfile, wasttidx, qch
--integer dbg
--  Z_format = 0
    emitline = tokline

    name = getIncludeLine()
--?name
--?name&-1
--if name = " dll.e\r\n" then
--  trace(1)
--end if

    ln = length(name)
    emitcol = tokcol-ln -- for possible error reporting...
--dbg=1
    --
    -- replace any tabs with spaces
    --
    while 1 do
        k = find('\t',name)
        if k=0 then exit end if
        name[k] = ' '
    end while
    while ln and find(name[ln],"\r\n") do
        ln -= 1
    end while
    --
    -- kill any comment
    --
    k = match("--",name)
    if k then
        ln = k-1
        name = name[1..ln]
    end if
    k = match("//",name)
    if k then
        ln = k-1
        name = name[1..ln]
    end if
    --
    -- remove leading/trailing spaces
    --
    k = 1
    while k<=ln and name[k]=' ' do
        k += 1
    end while
    emitcol += k
    while ln and name[ln]=' ' do
        ln -= 1
    end while
--dbg=2
--?name
    if ln>=k then
        name = name[k..ln]
    else
        name = ""   -- may happen if you ctrl-hover on "builtins" in "include builtins\dll.e" and click on it...
    end if
--?name
--dbg=3
    ln = length(name)
    --
    -- check for namespace
    --
--dbg=4
    k = match(" as",name)
    if k then
        if with_js=1 then
            Abort("namespaces are not supported under with js")
        end if
        not_js = true
        nj_reason = "namespace detected"
        kp1 = k+4
        while kp1<=ln and name[kp1]=' ' do
            kp1 += 1
        end while
        if kp1>ln then
            if intellisense=3
            and trapfileno = fileno then
                intellisense = 4
                eof_processing()    -- (does not return)
            end if
            tokcol = emitcol+kp1-1
            Abort("a namespace is expected here")
        end if
        nameSpace = name[kp1..ln]
        if find(' ',nameSpace) then
            tokcol = emitcol+kp1-1
            Abort("unrecognised")
        end if
        tt_string(nameSpace,-2)
--trace(1)
        if InTable(-InTop) then
            tokcol = emitcol+kp1-1
            Duplicate()
        end if
        N = InTable(-InAny)
        if N then
--21/01/2021
--          if symtab[N][S_NTyp]=S_Rsvd then
            if N<=T_Asm or symtab[N][S_NTyp]=S_Rsvd then
                Aborp("illegal use of a reserved word")
            end if
        end if
        ln = k-1
        while ln and name[ln]=' ' do
            ln -= 1
        end while
        name = name[1..ln]
    end if
    toktype = EOL -- otherwise, with ttidx still T_include, it loops
    if ln then
        qch = name[1]
        if qch='\"' 
        or qch='`' then
            if name[ln]!=qch then Aborp("missing end quote") end if
            ln -= 1
            name = name[2..ln]
        end if
    end if
    prevfile = fileno
--puts(1,"do we get this far?\n")
--?name
--if name="builtins\\VM\\pfindN.e" then 
--  trace(1)
--end if
    includeFile(name,0,emitcol)
--puts(1,"but not this far?\n")
    if k then
        k = addSymEntry(ttidx,0,S_Nspc,prevfile,0,0)
    end if
    reinclude = 0
    if fileno<0 then
        reinclude = -fileno
        line += 1
    end if
    currRtn = increaseScope(S_File,prevfile)
    getCh()
    if Ch>0 then getToken() end if
--added 30/09/2012:
--if 01 then
    if reinclude=0 then
        if ttidx=T_namespace then
            if with_js=1 then
                Abort("namespaces are not supprted under with js")
            end if
            not_js = true
            nj_reason = "namespace detected"
            getToken()
            if toktype!=LETTER then
                Aborp("a name is expected here")
            end if
            if k=0 or ttidx!=symtab[k][S_Name] then
                -- ie no "as", or a different name
                newfile = fileno
                fileno = prevfile
                if InTable(-InTop) then
                    fileno = newfile
                    Duplicate()
                end if
                N = InTable(-InAny)
                fileno = newfile
                if N then
--21/01/2021
--                  if symtab[N][S_NTyp]=S_Rsvd then
                    if N<=T_Asm or symtab[N][S_NTyp]=S_Rsvd then
                        Aborp("illegal use of a reserved word")
                    end if
                end if
--              k = addSymEntry(ttidx,0,S_Nspc,prevfile,0,0)
                -- S_Nspc asGlobal == local in prior scope (see addSymEntry!)
                k = addSymEntry(ttidx,1,S_Nspc,prevfile,0,0)
            end if
            while length(default_namespaces)<fileno do
                default_namespaces &= 0
            end while
            default_namespaces[fileno] = ttidx
--DEV1114: psym.e:namespace(ttidx)
--          k = addSymEntry(ttidx,0,S_Nspc,newfile,0,0)
            k = addSymEntry(ttidx,0,S_Nspc,fileno,0,0)
            getToken()
        end if
    elsif reinclude<=length(default_namespaces)
-- added 27/2/13:
      and default_namespaces[reinclude]!=0 then 
        if k=0 or default_namespaces[reinclude]!=symtab[k][S_Name] then
            -- ie no "as", or a different name
            wasttidx = ttidx
            ttidx = default_namespaces[reinclude]
            if InTable(-InTop) then
--DEV (27/2/13) kp1 has not been assigned a value...
--              (I suspect we want to save tokcol from when we set default_namespaces[fileno] just above) - not really, though
if k!=0 then
                tokcol = emitcol+kp1-1
end if
                Duplicate()
            end if
            N = InTable(-InAny)
            k = addSymEntry(ttidx,0,S_Nspc,prevfile,0,0)
            ttidx = wasttidx
        end if
    end if
--end if
end procedure


--with trace
string autoinclude

--integer ntrace = 0
integer cfb_count = 0

function checkforbuiltins()
--  if builtinsReferenced and not isGlobal 
integer wasfileno
    if builtinsReferenced
    and scopelevel=1 then
        if allWhiteToTokcol() then
            prevfile = fileno
-- 15/9/14:
if 01 then
--puts(1,"checkforbuiltins()\n")
--if ntrace then trace(1) end if
            sequence done = {}  -- (untested [needs bad psym.e]: might need to be static)
            while 1 do
                autoinclude = getBuiltin()
                if length(autoinclude)=0 then exit end if
                if gb_fwd!=0 then
                    if find({autoinclude,gb_fwd},done) then
?{"autoinclude",autoinclude,"gb_fwd",gb_fwd,getname(symtab[gb_fwd][S_Name],-2)}
cfb_count += 1 if cfb_count>100 then ?9/0 end if
                        trace(1)
                    end if
                    done = append(done,{autoinclude,gb_fwd})
                end if
                wasfileno = fileno
                includeFile(autoinclude,1,tokcol)
                if fileno<0 then
                    fileno = wasfileno
                    if builtinsReferenced=0 then exit end if
                else
                    currRtn = increaseScope(S_File,prevfile)
                    getCh()
                    if Ch>0 then getToken() end if
--if autoinclude="VM\\pApnd.e" then ntrace=1 end if
--puts(1,autoinclude&"\n")
                    return 1
                end if
            end while
--puts(1,"0\n")
else -- (old code)
            autoinclude = getBuiltin()
            if length(autoinclude) then
--trace(1)
wasfileno = fileno
                includeFile(autoinclude,1,tokcol)
--1/1/14:
if fileno<0 then
    fileno = wasfileno
else
                currRtn = increaseScope(S_File,prevfile)
                getCh()
                if Ch>0 then getToken() end if
                return 1
end if
            end if
end if
        end if
    end if
    return 0
end function

--DEV This is a near complete copy of DoConstant(): Factor out the common code.
--DEV not documented
--with trace
procedure DoEnum()
integer nxt, prev
integer wasttidx, N, O
integer wastokcol, rtntokcol, rtntokline
integer slink, glink, scope, state
sequence symtabN, symtabO   -- copies of symtab[x]
integer step = 1, mul = 0, typeid = 0, tmp, pN, typesetN
sequence typeset
bool prevset = false

    nxt = 1
    MatchString(T_enum)
    if ttidx=T_type then
        if with_js=1 then
            Aborp("invalid under with js")
        end if
        not_js = true
        nj_reason = "enum type"
        getToken()
        if toktype!=LETTER then
            Aborp("a name is expected here")
        end if
        N = InTable(InTop)
        if N=0 and isGlobal then
            N = InTable(-InAny) -- no errors (namespace rqd etc)
        end if
        if N then
            if N<=T_Asm then
                Aborp("builtin overide")
            elsif and_bits(symtab[N][S_State],S_fwd) then
                Aborp("forward reference")
--21/01/2021
--          elsif symtab[N][S_NTyp]=S_Rsvd then
            elsif N<=T_Asm or symtab[N][S_NTyp]=S_Rsvd then
                Aborp("illegal use of a reserved word")
            else
                Duplicate()
            end if
        end if
        typeid = ttidx
        typeset = {}
        rtntokcol = tokcol
        rtntokline = tokline
        getToken()
    end if
    if ttidx=T_by then
--DEV test under pwa/p2js:
        --
        -- Aside: we don't support non-integer step, but it is still
        --      getToken(float_valid:=true) to reject them properly!
        --
        integer sgn = 1
        getToken(float_valid:=true)
        if toktype='*' then
            mul = 1
            getToken(true)
        elsif toktype='+' then
            getToken(true)
        elsif toktype='-' then
            sgn = -1
            getToken(true)
        end if
        if toktype!=DIGIT then
            Aborp("a number is expected here")
        end if
        step = sgn*TokN
        getToken()
    end if
    while 1 do
        emitline = line
        if toktype!=LETTER then
            if mapEndToMinusOne='$' and toktype=DIGIT and TokN=-1 then  
                mapEndToMinusOne = 0
                getToken()
                exit
            end if
            Aborp("a name is expected here")
        end if
        mapEndToMinusOne = 0
        if InTable(InTop) then Duplicate() end if
        N = InTable(InAny)
        if N then
--21/01/2021
--          if symtab[N][S_NTyp]=S_Rsvd then
            if N<=T_Asm or symtab[N][S_NTyp]=S_Rsvd then
                Aborp("illegal use of a reserved word")
            end if
        end if
        wasttidx = ttidx
        wastokcol = tokcol
        getToken()
        N = 0
-- added 6/4/2012 (allow ":=" as well as "=")
        if toktype=':' and Ch='=' then MatchChar(':',false) end if
        if toktype='=' then
            mapEndToMinusOne = -2       
            MatchChar('=',float_valid:=true)
            if mapEndToMinusOne='$' and toktype=DIGIT and TokN=-1 then  -- ...,x=$ case
                -- (mapEndToMinusOne of '$' used as a flag later on)
--DEV may need a prev (spotted in passing) [FIXED]
--              O = addUnnamedConstant(nxt-1, T_integer)
                if not prevset then Aborp("no prior value") end if
                O = addUnnamedConstant(prev, T_integer)
                symtabO = symtab[O]
            else
                mapEndToMinusOne = 0
                GetFactor(asBool)
                if opTopIsOp
--DEV 1/11/2011
--              or not opsltrl[opsidx]
                or opsltrl[opsidx]!=1
                or opstype[opsidx]!=T_integer then
                    Aborp("a literal integer is expected here")
                end if
                O = opstack[opsidx]
                symtabO = symtab[O]
                nxt = symtabO[S_value]
                opsidx -= 1
            end if
        else
            O = addUnnamedConstant(nxt, T_integer)
            symtabO = symtab[O]
        end if

        if equal(symtabO[S_Name],-1) then
            --
            -- We already have an unnamed constant of the right value, 
            --  so slap our name etc on it.
            --
            N = O
            slink = tt[wasttidx+EQ]
            scope = 0
            if isGlobal then
                --
                -- keep the S_Nlink chain as {locals},{globals} so that dropScope can
                --  just pick off the top element, rather than go hunting for it.
                --
                glink = 0
                while slink do
                    symtabN = symtab[slink]
                    if and_bits(symtabN[S_State],K_gbl) then exit end if
                    glink = slink
                    slink = symtabN[S_Nlink]
                end while
            else
                scope = localscopes[scopelevel]
            end if
            symtabN = symtab[N]
            symtab[N] = 0
            symtabN[S_Name] = wasttidx
            symtabN[S_FPno] = fileno
            symtabN[S_Nlink] = slink
            if isGlobal then
                if glink then
                    symtab[glink][S_Nlink] = N
                else
                    tt[wasttidx+EQ] = N
                end if
            else
                tt[wasttidx+EQ] = N
                localscopes[scopelevel] = N
            end if
            if isGlobal then
                state = symtabN[S_State]
                state = or_bits(state,S_used_set_gbl)
                symtabN[S_State] = state
            end if
            symtabN[S_Slink] = scope

        else -- S_Name!=-1
            --
            -- symtab[O] already has a name, so clone and S_Clink it.
            --
            N = addSymEntryAt(wasttidx,isGlobal,S_Const,T_integer,0,K_litnoclr,wastokcol)
            symtabN = symtab[N]
            symtab[N] = 0
            symtab[O] = 0
            symtabN[S_value] = symtabO[S_value]     -- 8
            symtabN[S_Init]  = symtabO[S_Init]      -- 11
            symtabN[S_Clink] = symtabO[S_Clink]     -- 9
            symtabO[S_Clink] = N
            symtab[O] = symtabO
        end if

--no: put duplicates in to yield gaps in the ordinal index result for duplicated values
--      if typeid!=0
--      and not find(nxt,typeset) then
        if typeid!=0 then
--DEV possibly ttidx based...
--DEV to support/test floats...
--          if isFLOAT(nxt) then
--              v = symtabN[S_Init]+0.5     << from DoSeq, could be symtabN/symtabO/other here...
--              typeset = append(typeset,v)
--          else
                typeset = append(typeset,nxt)
--          end if
        end if

        symtabN[S_ErrV] = wastokcol
        symtab[N] = symtabN

        if mapEndToMinusOne='$' then -- ..,x=$ case
            mapEndToMinusOne = 0
            getToken()
-- removed 24/2/18:
--          exit
        else
            prev = nxt
            prevset = true
            if mul then
                nxt *= step
            else
                nxt += step
            end if
        end if
        if toktype!=',' then exit end if
        mapEndToMinusOne = -2
        MatchChar(',',float_valid:=false)
    end while
    if typeid!=0 then
--      typeset = append(typeset,nxt)
--DEV...
--?     PushFactor(N,true,T_Dsq)    -- yep, this is a literal!

        returnvar = newTempVar(T_object,FuncRes)
        LIDX = 1

        N = addSymEntryAt(typeid,isGlobal,S_Type,0,0,0,rtntokcol)
--      pN = addSymEntryAt(T_object,0,S_TVar,T_object,0,K_type+S_used,rtntokcol)
--      pN = addSymEntryAt(T_object,0,S_TVar,0,0,K_type+S_used,rtntokcol)
--      pN = addSymEntryAt(-1,0,S_TVar,0,0,K_type+S_used,rtntokcol)
        pN = addSymEntryAt(-1,0,S_TVar,T_integer,0,K_type+S_used,rtntokcol)
--increaseScope?
        symtab[pN][S_Init] = 1
        tmp = symtab[pN][S_State]
        tmp = or_bits(tmp,S_set+K_used)
        symtab[pN][S_State] = tmp

        typesetN = addUnnamedConstant(typeset, T_Dsq)
        if length(symtab[typesetN])<S_gInfo then -- if not a duplicate
--          symtab[typesetN] = append(symtab[typesetN],{T_Dsq,0,0,etype,len})
            symtab[typesetN] = append(symtab[typesetN],{T_Dsq,0,0,T_integer,length(typeset)})
        end if

        -- store frame info:
        symtab[N][S_Parm1] = pN
        symtab[N][S_ParmN] = 1
        symtab[N][S_Ltot] = 1

        symtab[N][S_sig] = {'T',T_object}
        symtab[N][S_Efct] = E_none

--  emitline = line
--  if emitline>lastline then
--      emitline -= 1
--  end if
        symtab[N-1][S_vtype] = T_integer
--DEV opLtyp??
        symtab[N-1][S_ltype] = T_integer

        if increaseScope(S_Rtn,-1) then end if
        symtab[N][S_1stl] = rtntokline
        currRtn = N

        if NOLT=0 or bind or lint then
            ltclear(N) -- (see above)
        end if -- NOLT

        PushFactor(pN,true,T_integer)
        PushFactor(typesetN,true,T_Dsq)
--DEV (spotted while copying for DoStruct) shouldn't this be FUNC not TYPE?? [only try changing this once DoStruct is proven]
        Call(T_find,{TYPE,T_integer,T_Dsq},TYPE,true)
        StoreVar(returnvar,T_integer)
        returntype = T_integer

if newEmit then
        agcheckop(opRetf)
end if
        apnds5(opRetf)

--
--opFrame
--      agcheckop(opFrame)
--      apnds5({opFrame,T_find})
--opFrst
--opMove
--opCall
--opMovbi
--opRetf

        currRtn = dropScope(N,S_Rtn)
        if NOLT=0 or bind or lint then
            ltclear(-N)
        end if -- NOLT
        returnvar = -1
        returnint = 0

--/*
--  routine 721 (type color() in C:\Program Files (x86)\Phix\e06.exw):
--  ===========
--     1:  opLn,11,                              --:enum by *2 RED=4, GREEN, BLACK, BLUE, PINK type color(object object) return find(object,{RED, GREEN, BLACK, BLUE, PINK}) end type
--     3:  opFrame,319,                          -- (find)
--     5:  opFrst,725,722,15,1,                  opFrst,dest,src,srctype,pbr
--    10:  opMove,726,723,20744,1,4,             opMove,dest,src,isInit,onDeclaration,ltype
--    16:  opCall,                               opCall
--    17:  opMovbi,720,318,1,                    opMovbi,dest,src,isInit
--    21:  opRetf,
--    22:  opBadRetf,
--
--  new:
--  routine 721 (type color() in C:\Program Files (x86)\Phix\e06.exw):
--  ===========
--     1:  opLn,10,                              --:enum type color by *2 RED=4, GREEN, BLACK, BLUE, PINK end type
--     3:  opFrame,319,                          -- (find)
--     5:  opFrst,725,722,0,1,                   opFrst,dest,src,srctype,pbr
--    10:  opMove,726,723,20744,1,4,             opMove,dest,src,isInit,onDeclaration,ltype
--    16:  opCall,                               opCall
--    17:  opMovbi,720,318,1,                    opMovbi,dest,src,isInit
--    21:  opRetf,
--
--
--  ;    11 enum by *2 RED=4, GREEN, BLACK, BLUE, PINK type color(object object) return find(object,{RED, GREEN, BLACK, BLUE, PINK}) end type
--      mov ecx,7                             ;#0042CE98: 271 07000000               uv 02 00  1   1      
--      mov edx,78                            ;#0042CE9D: 272 4E000000               vu 04 00  1   1      
--      call #0042CC0D (:%opFrame) (find)     ;#0042CEA2: 350 66FDFFFF               v  00 00  1   2      
--      mov edi,[ebp+20] (prevebp)            ;#0042CEA7: 213175 14                  uv 80 20  1   3      
--      mov eax,[edi]                         ;#0042CEAA: 213007                     uv 01 80  1   6 80 *80*
--      mov [ebp] (x),eax                     ;#0042CEAC: 211105 00                  uv 00 21  1   7 01   
--      mov esi,[#004027D8]                   ;#0042CEAF: 213065 D8274000            vu 40 00  1   7      
--      mov [ebp-4] (s),esi                   ;#0042CEB5: 211165 FC                  uv 00 60  1   8      
--      add dword[ebx+esi*4-8],1              ;#0042CEB8: 203104263 F8 01            u  00 48  3  10    *40*
--X     mov [ebp+16] (retaddr),#0042CEC9      ;#0042CEBD: 307105 10 C9CE4200         vu 00 20  1  12      
--      mov [ebp+28] (retaddr),#0042CEC9      ;#0042CEBD: 307105 10 C9CE4200         vu 00 20  1  12      
--      jmp #00423960 (code:find)             ;#0042CEC4: 351 976AFFFF               v  00 00  1  13      
--      jmp #0042CCAE (:%opRetf)              ;#0042CEC9: 351 E0FDFFFF               v  00 00  1  14      
--
--  new:
--  ;    10 enum type color by *2 RED=4, GREEN, BLACK, BLUE, PINK end type
--      mov ecx,7                             ;#0042CE98: 271 07000000               uv 02 00  1   1      
--      mov edx,78                            ;#0042CE9D: 272 4E000000               vu 04 00  1   1      
--      call #0042CC0D (:%opFrame) (find)     ;#0042CEA2: 350 66FDFFFF               v  00 00  1   2      
--      mov edi,[ebp+20] (prevebp)            ;#0042CEA7: 213175 14                  uv 80 20  1   3      
--      mov eax,[edi]                         ;#0042CEAA: 213007                     uv 01 80  1   6 80 *80*
--      mov [ebp] (x),eax                     ;#0042CEAC: 211105 00                  uv 00 21  1   7 01   
--      mov esi,[#004027D8]                   ;#0042CEAF: 213065 D8274000            vu 40 00  1   7      
--      mov [ebp-4] (s),esi                   ;#0042CEB5: 211165 FC                  uv 00 60  1   8      
--      add dword[ebx+esi*4-8],1              ;#0042CEB8: 203104263 F8 01            u  00 48  3  10    *40*
--X     mov [ebp+16] (retaddr),#0042CEC9      ;#0042CEBD: 307105 10 C9CE4200         vu 00 20  1  12      
--      mov [ebp+28] (retaddr),#0042CEC9      ;#0042CEBD: 307105 10 C9CE4200         vu 00 20  1  12      
--      jmp #00423960 (code:find)             ;#0042CEC4: 351 976AFFFF               v  00 00  1  13      
--      jmp #0042CCAE (:%opRetf)              ;#0042CEC9: 351 E0FDFFFF               v  00 00  1  14      
--
--  symtab[922]:{-1,S_TVar,0,(S_set+K_Fres),0,0,integer,{integer,0,MAXLEN,object,-1},(eax)}
--  symtab[923]:{color,S_Type,1,(S_used+K_used+K_wdb),0,926,{84'T',15},924,1,1,#0042CE98}
--  symtab[924]:{object,S_TVar,1,(S_used+S_set+K_used+K_wdb+K_type),15,0,object,{integer,4,64,object,-1},[esp]}
--  symtab[925]:{-1,S_Const,1,(S_used+S_set+K_sqr+K_noclr+K_lit),0,498/#004027D8,T_Dsq,{4,8,16,32' ',64'@'}}
--
--  new:
--  symtab[922]:{-1,S_TVar,0,(S_set+K_Fres),0,0,integer,{integer,0,MAXLEN,object,-1},(eax)}
--  symtab[923]:{color,S_Type,1,(S_used+K_used+K_wdb),0,926,{84'T',15},924,1,1,#0042CE98}
--  >symtab[924]:{-1,S_TVar,1,(S_used+S_set+K_used+K_wdb+K_type),0,923,integer,{integer,4,64,object,-1},[esp]}
--  symtab[925]:{-1,S_Const,1,(S_used+S_set+K_sqr+K_noclr+K_lit),0,498/#004027D8,T_Dsq,{4,8,16,32' ',64'@'}}
--
--
--*/
        MatchString(T_end)
        MatchString(T_type)
    end if
--16/3/17:
    if toktype=';' then
        getToken()
    end if
end procedure

procedure DoFormatHere()
--
-- Process a format statement once any redirects have been taken care of (see DoFormat())
--
-- format ((PE32|PE64) [GUI|console] [3.10|4.0|5.0] [DLL]) [icons] [version] [manifest])|
--        (ELF32|ELF64) [SO]
--        (ARM?)
--
-- (An [at #7000000 on 'stub.exe'] clause may be added in the future)
--
integer k = 0
string filename
sequence iconids

    if toktype=LETTER then
        k = find(ttidx,{T_PE32,T_ELF32,T_PE64,T_ELF64})
    end if
    if k=0 then
        Aborp("PE32, ELF32, PE64, OR ELF64 expected")
    end if
--DEV PE needs setting from command_line[1]... (see pglobals.e)
-- (this will all need proper testing, once I've got alot further along...)
    if bind then        -- (DEV X64 needs setting for interpretation - not!)
        X64 = (k>=3)
        if not norun
        and PE!=and_bits(k,1) then
            -- ie command_line[1]!=specified format
            -- (unless you're on a machine that can run both PE and ELF...)
            Abort("-norun command line option required")
        end if
        PE = and_bits(k,1) -- (k=1 or k=3)
        symtab[T_SLASH][S_value] = iff(PE?'\\':'/')
--25/1/20 (replaced with Alias_C_flags().)
--/*
--27/5/19:
        if not PE then -- long is 64 bit on linux
            symtab[T_long][S_value] = #01000008
            symtab[T_ulong][S_value] = #02000008
        end if
        if X64 then
            symtab[T_ptr][S_value] = #02000008
        end if
?{"pmain.e/FoFormatHere() line 13559, PE:",PE,"X64:",X64}
--*/
    end if
    MatchString(ttidx,float_valid:=true)    -- T_PE32,T_ELF32,T_PE64,T_ELF64
    if PE then          -- PE32/PE64
        -- (it matters not what we set these to when interpreting, btw)
        if k=1 then             -- PE32
            subvers = #00000004
        else                    -- PE64 (k=3)
            subvers = #00000005
        end if
        k = find(ttidx,{T_GUI,T_gui,T_CONSOLE,T_console})
        if k then
            OptConsole = (k>=3)
            MatchString(ttidx,float_valid:=true) -- T_GUI,T_gui,T_CONSOLE,T_console
        end if
        if toktype=FLOAT or toktype=DIGIT then
            if equal(TokN,3.10) then
                getToken()
                subvers = #000A0003         -- messes up gui, obviously
            elsif equal(TokN,4.0) then
                getToken()
                subvers = #00000004         -- this is the default, btw
            elsif equal(TokN,5.0) then
                getToken()
                subvers = #00000005         -- this is the default for 64-bit, btw [DEV]
            else
                Aborp("3.10, 4.0, or 5.0 expected")
            end if
        end if
        k = T_DLL
    else                -- ELF32/ELF64
        -- ... and I've not even googled how one might do this (T_SO) yet!
        -- ( a position-independent data section may be a significant challenge!)
        k = T_SO
--      ?9/0
    end if
    if ttidx=k then
--  if PE and ttidx=k then
--DEV intellisense?
        if bind=0 or norun=0 then
            Aborp("requires -c and -norun command line options (or -dll)")
        end if
        agcheckop(opCbHandler)
        MatchString(ttidx)  -- T_DLL/T_SO
        DLL = 1
    else
        DLL = 0
    end if
    if ttidx=T_icons then
        -- eg icons {"ok.ico",{1,0}}
        MatchString(T_icons)
        MatchChar('{')
        if toktype!=DQUOTE then
            Abort("filename expected")
        end if
        filename = TokStr
        getToken()
        MatchChar(',')
        MatchChar('{')
        iconids = {}
        while 1 do
            if toktype!=DIGIT then
                Abort("digit expected")
            end if
            iconids = append(iconids,TokN)
            getToken()
            if toktype!=',' then exit end if
            MatchChar(',')
        end while
        MatchChar('}')
        MatchChar('}')
        rs_icon = {filename,iconids}
    end if
    if ttidx=T_version then
        MatchString(T_version)
        MatchChar('{')
        rs_version = {}
        while 1 do
            if toktype!=DQUOTE then
                Abort("version (string) expected")
            end if
            rs_version = append(rs_version,TokStr)
            getToken()
            if toktype!=',' then exit end if
            MatchChar(',')
        end while
        if and_bits(length(rs_version),1) then
            Abort("uneven number of version strings")
        end if
        MatchChar('}')
    end if
    if ttidx=T_manifest then
        MatchString(T_manifest)
        if toktype!=DQUOTE then
            Abort("manifest (string) expected")
        end if
        rs_manifest = TokStr
        getToken()
    end if
end procedure

--with trace
procedure DoFormat()
--
-- Allow max. 2 redirects, eg p.exw can have 'format "p.fmt"' 
--                        and p.fmt can have 'format "pe32.fmt"'
--  but then pe32.fmt must be the real thing (DoFormatHere).
--  (this allows eg p.exw and pth.exw to share settings, and 
--   p.exw also kinda needs pdiag.e to share the same file.) [DEV?]
--  (also, writing p.fmt (eg demo/pgui, test/terror) might
--   be slightly easier as a redirect than the full monty.)
--
integer emitcol = tokcol -- for possible error reporting...
integer prevfile = fileno
string filename

--trace(1)
    if not newEmit then Abort("requires -e2 command line option, for now") end if
--  newEmit = 1
    --DEV (temp)
--  if not bind and intellisense=0 then Abort("currently only supported under bind") end if
    MatchString(T_format)
    if toktype=DQUOTE then
        filename = TokStr
        getToken()
        if bind then
            includeFile(filename,0,tokcol)
            if fileno<0 then
                fileno = prevfile
            else
                currRtn = increaseScope(S_File,prevfile)
                getCh()
                getToken()
                MatchString(T_format)
                if toktype=DQUOTE then
                    filename = TokStr
                    getToken()
                    includeFile(filename,0,tokcol)
                    currRtn = increaseScope(S_File,prevfile)
                    getCh()
                    getToken()
                    MatchString(T_format)
                    DoFormatHere()
                    if Ch!=-1 then
                        Abort("EOF expected")
                    end if
                    currRtn = dropScope(0,S_File)
                else
                    DoFormatHere()
                end if
                if Ch!=-1 then
                    Abort("EOF expected")
                end if
                currRtn = dropScope(0,S_File)
                getToken()
            end if
        end if
    else
        DoFormatHere()
    end if
end procedure

procedure DoNameSpace()
    if with_js=1 then
        Abort("namespaces are not supoprted under with js")
    end if
    not_js = true
    nj_reason = "namespace detected"
    getToken()
    if toktype!=LETTER then
        Aborp("a name is expected here")
    end if
    integer k = addSymEntry(ttidx,0,S_Nspc,fileno,0,0)
    getToken()
end procedure

procedure TopLevel()
integer t

    isGlobal = 0
    while Ch>0 do -- until end of file
        if toktype=LETTER then
            t = find(ttidx,T_topset)
            if t then
                if    t<=3                then DoRoutineDef(t)  isGlobal = 0
                elsif ttidx=T_constant    then DoConstant()     isGlobal = 0
                elsif ttidx=T_enum        then DoEnum()         isGlobal = 0
                elsif find(ttidx,Tstruct) then DoStruct()       isGlobal = 0
                elsif ttidx=T_include     then IncludeFile()    isGlobal = 0
                elsif isGlobal            then Aborp("procedure, function, type, constant, enum, or vartype expected")
                elsif ttidx=T_override    then Aborp("override of builtins is not and never will be permitted")
                elsif ttidx=T_export      then getToken()       isGlobal = 2
                elsif find(ttidx,Tglobal) then getToken()       isGlobal = 1
                elsif ttidx=Z_format      then DoFormat()
                elsif ttidx=T_forward     then DoForwardDef()
                elsif ttidx=T_namespace   then DoNameSpace()
                elsif ttidx=T_with        then DoWithOptions(WITH)
                elsif ttidx=T_without     then DoWithOptions(WITHOUT)
                else                           Aborp("invalid")
                end if
            else
                if tokno=0 then
                    tokno = InTable(InAny)
                end if
                if tokno>0 and symtab[tokno][S_NTyp]=S_Type then
                    TopDecls(1)
                    isGlobal = 0
                else
                    if isGlobal then
                        Aborp("procedure, function, type, constant, or vartype expected")
                    end if
                    top_level_abort = 1
                    Statement()
                    if LastStatementWasAbort then
                        --
                        -- Note that tok_abort_line/col are set for every routine (see ParamList())
                        -- Note also the special handling at the end of ParamList, inside 
                        -- 'if routineNo=T_abort and top_level_abort', which avoids eg 'abort(0)\n~$' 
                        -- from generating an unnecessary 'invalid character' error, by truncating
                        -- the text.
                        --
                        Warn("statement after abort() will never be executed\n",tok_abort_line,tok_abort_col,0)
                        Ch = 0
                        exit
                    end if
                end if
            end if
        else
            Statement()
        end if
        if probable_logic_error then show_ple() end if
--      if not isGlobal then
-- 6/3/14:
        if not isGlobal and Z_format=0 then
            if checkforbuiltins() then end if
        end if
    end while 
    --added 19/3/09:
    --  while Ichain!=-1 do
    --      t = symtab[Ichain][S_Init]
    --      symtab[Ichain][S_Init] = 0
    --      Ichain = t
    --  end while
    if NOLT=0 or bind or lint then
        ltclear(currRtn)
    end if -- NOLT
end procedure

--with trace
global procedure Compile(bool bInit = true)

    opsidx = 0
    if bInit then   -- 9/2/10 (for repl)
--  resetOptions()
--trace(1)
        profileon = 0
        with_js = 2 -- (2=="any/without")
        not_js = false
        probable_logic_error = 0
        default_namespaces = {}
        APIlibs = {}
        APIerrlib = {}
        APINames = {}
        APIerritem = {}
        glblused = {}
        glboffset = {}
--      glbttidx = {}
        glblabel = {}
        glblline = {}
        glblcol = {}
        glblname = {}
        gexch = 0
--      PE = 1
--      DLL = 0 -- no! (spanners -dll option)
        exports = {}
        exportaddrs = {}
--EXCEPT
        exceptions_in_use = 0

--DEV
--if newEmit then
        if not bind then
            vminit()
        end if
--end if
    end if

--trace(1)
    getCh() -- start on first char
    if Ch='#' then skipHashBangLine() end if

    --DEV::
    tokcol = 1

if bInit then
    currRtn = increaseScope(S_File,0)
end if
    if DEBUG then
        if currRtn!=T_maintls then ?9/0 end if
    end if
-- 18/6/14:
    if newEmit then
--DEV: (no, methinks)
--if DLL then
--      s5 = {}
--      s5 = {opDLL?}
--else
        s5 = {opInit}
--end if
--      s5 = {opLn,1,opInit}
        if DLL then
            agcheckop(opCbHandler)
        end if
    else
        s5 = {}
    end if
--  LineTab = {}
--  ltline = 0  -- DEV needed?
--trace(1)
    if repl then
        Z_format = 0
        Alias_C_flags()
    else    
        Z_format = T_format

--20/3/15: (hopefully temp!)
        if bind then
--      if bind or newEmit then
--      if bind and not listing then    --DEV make this a setting in p.ini/pgui.exw
--          if not nodiag then
            if not nodiag or not suppressopRetf then
                -- load error routine if we cannot use interpreter's
-- 6/3/14:
--              includeFile("pdiag.e",0,1)
--              currRtn = increaseScope(S_File,1)
--              getCh()
                if Ch>0 then
                    getToken()
                    while toktype=LETTER
                    and find(ttidx,{T_format,T_namespace,T_with,T_without,T_include}) do
                        if    ttidx=T_format    then DoFormat()
                        elsif ttidx=T_namespace then DoNameSpace()
                        elsif ttidx=T_with      then DoWithOptions(WITH)
                        elsif ttidx=T_without   then DoWithOptions(WITHOUT)
                        elsif ttidx=T_include   then IncludeFile()
                        else ?9/0 end if
                        if Ch<=0 then exit end if
                    end while
                    Alias_C_flags()
                    Z_format = 0
                    prevfile = fileno
--DEV newEmit->pdiag2.e? (has invoke SetUnhandledExceptionFilter,finalExceptionHandler etc)
--if newEmit then
                    if not nodiag then
                        includeFile("VM\\pDiagN.e",0,1)
                    else
--?"Stack"
                        includeFile("VM\\pStack.e",0,1)
                    end if
--else
--              includeFile("pdiag.e",0,1)
--end if
--23/3/14:
--                  currRtn = increaseScope(S_File,1)
                    currRtn = increaseScope(S_File,prevfile)
                    getCh()
                end if
            end if
--removed 2/3/21... (let Statement() do it!)
--put back 20/3/21 (soloud wrapper said C_POINTER not defined)
        else
            Z_format = 0
            Alias_C_flags()
        end if
    end if

    LastStatementWasAbort = 0

--trace(1)
--DEV1114 namespace in main file? (needed for intellisense on OE /std)
    while 1 do
        if Ch>0 then    -- not EOF or top-level-abort.
            getToken()
            TopLevel()
        end if
        if scopelevel=1 then exit end if
        currRtn = dropScope(0,S_File)
    end while
    --
    -- Most auto-includes should have been processed by now, unless
    -- we hit end of file on a statement that was also the first use.
    -- To be honest, I have tried a couple of times to rework the
    -- above and below into a single loop, but failed. Nevermind, it
    -- appears to work perfectly well enough and the presence of a
    -- second loop seems to be an utterly insignificant overhead.
    --
--trace(1)
    while 1 do
        if scopelevel<=1 then
            if isGlobal then ?9/0 end if
            if not checkforbuiltins() then exit end if
        end if
        TopLevel()
        currRtn = dropScope(0,S_File)
        if scopelevel>1 then getToken() end if
    end while

--oops, triggered when unresolved [fwd] calls remained, which are
--      later correctly caught as Undefined() in unused_cleanup().
--  if no_of_fwd_calls>0 then ?9/0 end if   -- sanity check

    if not repl then
        symtab = symtab[1..symlimit]
    end if

    -- NB s5 is still open for T_maintls/opCleanUp in pemit.e, 
    --    which also does the equivalent of one last dropScope().

end procedure

