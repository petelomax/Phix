--
-- pglobals.e
-- ==========
--
--global integer Z_ridN = 0 -- DEV/temp (pilasm.e/pemit2.e

--DEV - time for this one to go...
--global constant newEmit = 0
global integer newEmit
               newEmit = 01

--DEV togo*3:
global constant newBase = 2 -- 0 = old style, with base @ ref*4-20 on seq/str
                            -- 2 = new style, with slack @ ref*4-20 for seq only.
                            -- (nb: an error occurs if newBase does not match the backend.)

global constant newEBP = 04 -- 4=on, 0=off(ie old style/working)

--global constant pxversion = {0,6,8}   -- 0.6.8
--global constant pxversion = {0,6,9}   -- 0.6.9    -- 25/04/16
--global constant pxversion = {0,7,0}   -- 0.7.0    -- 09/09/16
--global constant pxversion = {0,7,1}   -- 0.7.1    -- 04/12/16
--global constant pxversion = {0,7,2}   -- 0.7.2    -- 10/02/17
--global constant pxversion = {0,7,3}   -- 0.7.3    -- 12/02/17 (internal)
--global constant pxversion = {0,7,4}   -- 0.7.4    -- 15/02/17 (internal, misnamed)
--global constant pxversion = {0,7,5}   -- 0.7.5    -- 22/02/17 (internal)
--global constant pxversion = {0,7,6},  -- 0.7.6    -- 30/06/17
--global constant pxversion = {0,7,7},  -- 0.7.7    -- 09/01/18
--global constant pxversion = {0,7,8},  -- 0.7.8    -- 13/03/18
--global constant pxversion = {0,7,9},  -- 0.7.9    -- 13/04/18
--global constant pxversion = {0,8,0},  -- 0.8.0    -- 05/04/19 (uploaded 21/04/19)
--global constant pxversion = {0,8,1},  -- 0.8.1    -- 23/03/20
--global constant pxversion = {0,8,2},  -- 0.8.2    -- 24/11/20
--global constant pxversion = {0,8,3},  -- 0.8.3    -- 02/02/21
global constant phixversion = {1,0,0},  -- 1.0.0    -- 04/07/21
                phixverstr = sprintf("%d.%d.%d",phixversion)
sequence phixver = phixversion  -- (debug aid, otherwise unused)
if sequence(phixver) then end if

--DEV todo:
global constant q86 = 01    -- quick x86 handling for interpretation, possibly more...
                            --  0 = old style handling, (DEV remove)
                            --  1 = just speed up isJump scan
                            --  2 = full-on "in situ" operation for interpretation
--                          --  (Challenge:)
--                          --  blurph() takes ~10% of "p p w" time; if we can do
--                          --  things in situ, then:
--                          --      a) LineTab needs no mods
--                          --      b) "cs" is not needed; we can poke directly
--                          --          into CSvaddr.
--                          --      c) replace scanforShortJmp/blurph with a new
--                          --          routine, "insitu()", which must:
--                          --            c1) branch straighten
--                          --            c2) have isOpCode etc chain(s) ready

--DEV not newEmit:
global constant scramble=0 -- Makes reverse engineering an exe to source much more
                            -- difficult. Of course while a would-be hacker can easily
                            -- remove this, it does not really help them figure out
                            -- what happened some months ago on a different pc...
-- (no longer true under newEmit:)
                            -- This is just the hll's two cents, since it is pretty 
                            -- trivial and does not impact any on performance, but 
                            -- equally of course other related and more significant  
                            -- measures also exist in the asm back end.

global constant NOLT=0      -- if 1, suppress all pltype.e handling [DEV]

-- I added this when working on t02, to see just how much could be optimised away...
global constant reusetmps = 01  -- (0 causes 12.5-25% performance loss as things stand)


-- Optimisation Control.
-- ====================
--  The following constants are failed attempts to improve performance.
--  Some micro-benchmarks exhibit benefit, but on larger "real-world"
--  applications these gain nothing or even add a few percent.
--  What this proves, at least to me, is that any further optimisations
--  must be found at the "global" rather than the "micro" level.
--  

--global constant sched = 0         -- schedule/peephole optimisations (see psched.e)
                                    -- Overall this makes the compiler about 6.5% slower,
                                    -- though it was never properly measured on other pgms.
                                    -- WARNING: this (=1) is very badly broken. However,
                                    -- the info encoded in schedule() parameters may turn
                                    -- out to be very useful if a rewrite is attempted.

global constant DEBUG = 01          -- It turns out, despite code being completely omitted,
                                    -- this saves less than 1%, so may as well leave it on.

global constant SLASH       = iff(platform()=WINDOWS?'\\':'/')
global constant WRONGSLASH  = iff(platform()=WINDOWS?'/':'\\')

--DEV str
--/*
global type string(object s)
object c
    if not sequence(s) then return 0 end if
    for i=1 to length(s) do
        c=s[i]
        if not integer(c) or c<0 or c>255 then return 0 end if
    end for
    return 1
end type
--*/
global string rootpath,     -- should be (eg) C:\Program Files\Phix  (used in p.exw/pemit.e/ptok.e)
              mainpath,     -- should be (eg) C:\Program Files\Edita (used in p.exw/pemit.e/plist.e/pltype.e/ptok.e)
              mainfile      --                                       (used in p.exw/ptok.e [since testset contains some ..\\])
              mainfile = "?"    -- (avoid unassigned error in Fatal())

-- not strictly necessary, but reduces opCallOnce/fwd calls/onDeclaration
--DEV wrong one for newEmit:
--!/**/include builtins\pcmdln.e        -- command_line()
include builtins\pgetpath.e         -- get_proper_path()

procedure set_rootpath()
sequence cl
    cl = command_line()
    rootpath = get_proper_path(cl[1],"")
--?rootpath
    for j=length(rootpath) to 0 by -1 do
        if find(rootpath[j],"\\/") then
--DEV methinks a fix for get_proper_path() is in order... [done 10/1/18]
            rootpath = rootpath[1..j]
--          rootpath = get_proper_path(rootpath[1..j])
            exit
        end if
    end for
--?rootpath
end procedure
set_rootpath()


global sequence text    -- of file currently being parsed

global string TokStr        -- for DQUOTE etc

global integer tokline
               tokline = -1
global integer tokcol
               tokcol = -1
global integer tokno
               tokno = -1

global sequence filepaths, filenames

global integer fileno
               fileno=0

--
-- Symbol table constants/structure
--
global constant S_Name  = 1,    -- const/var/rtn name (now a ttidx number or -1)
                S_NTyp  = 2,    -- Const/GVar/TVar/Nspc/Type/Func/Proc
                S_FPno  = 3,    -- File and Path number
                S_State = 4,    -- state flag. S_fwd/S_used/S_set etc
                S_Nlink = 5,    -- name chain (see below)
                S_Slink = 6,    -- scope/secondary chain (see below)
                -- constants and variables [S_NTyp<=S_TVar]
                S_vtype = 7,    -- variable type [see notes below]
                S_value = 8,    -- value [see note below]
                S_Clink = 9,    -- constant chain (S_NTyp=S_Const only, see below)
                S_Tidx  = 9,    -- thread idx (S_NTyp=S_Tvar only) [BLUFF/DEV]
                S_ErrV  = 10,   -- {'v', file, line, col}; see pmain.e[-35]
--DEV not newEmit?:
                S_ConstChain = 10,  -- see notes below (constant ref/count optimisations)
                S_Init  = 11,   -- Initialised chain (known init if non-0/see S_Const note below)
                S_ltype = 12,   -- local type (see pltype.e)
                S_maxlv = 13,   -- last entry for var (see pltype.e)
                S_gInfo = 14,   -- (see note below)
                S_gNew  = 15,
                -- namespaces
                S_nFno  = 7,    -- namespace fileno [see note below]
                -- routines [S_NTyp>=S_Type]
                S_sig   = 7,    -- routine signature, eg {'F',T_integer} (nb S_sig must be = S_vtype)
                S_Parm1 = 8,    -- first parameter. (idx to symtab, follow S_Slink)
                S_ParmN = 9,    -- minimum no of parameters (max is length(S_sig)-1)
                S_Ltot  = 10,   -- total no of parameters, locals, and temporary vars
                                -- (needed to allocate the stack frame space)
                S_il    = 11,   -- intermediate code (also backpatch list)
                S_ltab  = 12,   -- line table
                S_1stl  = 13,   -- first line (of "procedure"/"function"/"type" keyword)
                S_Efct  = 14,   -- side effects
                S_ErrR  = 15    -- {'R', file, line, col}; see pmain.e[-60]

-- symtab[i][S_NTyp] values:
global constant S_Const = 1,    -- a constant
                S_GVar2 = 2,    -- global or static variable
                S_TVar  = 3,    -- temp or threadstack (local) variable/parameter
                S_Nspc  = 4,    -- namespace
                S_Rsvd  = 5,    -- Reserved word (with S_State=K_fun modifier)
                S_Type  = 6,    -- Type of thermal yellow portable encryptor
                S_Func  = 7,    -- Function of finding unusual nonsense comments
                S_Proc  = 8     -- Procedure for private rotating obstacle counter

global constant NTdesc = {"constant ",
                          "variable ",
                          "parameter ",
                          "namespace ",
                          "reserved word ",
                          "type ",
                          "function ",
                          "procedure "}

-- symtab[i][S_State] values:
global constant S_used = #000001,   -- bit for not used warnings
                S_set  = #000002,   -- bit for never assigned a value warnings
                S_fwd  = #000004,   -- routine is not yet defined (explicit "forward" or implicit call)
                S_for  = #000008,   -- variable in current use as for loop control var (cleared on end for)
                S_used_and_set = #03,   --S_used+S_set,
                S_fwd_and_used = #05,   --S_fwd+S_used,
                S_for_used_set = #0B,   --S_used+S_set+S_for
--DEV these should be S_xxx not K_xxx... (but K_used and S_used are completely different)
                K_used = #000010,   -- isParam on tvars & to link reachable routines, see notes below
                K_sqr  = #000020,   -- sequence rebuilt flag when interpreting
                K_aod  = #000040,   -- assignment on declaration (avoid warnings/force cleanup)
                K_fun  = #000080,   -- reserved word that can be used as a function name
                K_wdb  = #000100,   -- with debug setting (default is on)
                K_noclr= #000200,   -- do not clear on load (ie assignment on declaration occurred)
                                    -- only used for GVars
                K_rtn  = #000400,   -- indicates value is a routine_id (for bind)
                K_ran  = #000800,   -- set once routine has been called (for tls routines)
                K_gbl  = #001000,   -- a true "global"
                S_used_set_gbl = #1003, -- K_gbl+S_set+S_used       -- DEV S_gsu
                K_Fres = #002000,   -- a function result
                K_lit  = #004000,   -- literal flag
                K_litnoclr = #4200, -- K_lit+K_noclr,               -- DEV S_lnc
                K_type = #008000,   -- type() routine parameter
                K_othr = #010000,   -- other routine parameter
                K_ridt = #020000,   -- known routine_id target
--DEV oldil:
-- K_ilasm = #8000          -- gvar may be modified by #ilasm; skip it in gvar_scan().
-- optional_params:
--              K_oparm = #20000000,    -- this parameter is optional
--              K_omask = #1FFFFFFF     -- mask to remove K_optp bit
                K_dlft = #040000,   -- defaulted param (must be a Tvar with K_used (=isParam) set)
--DEV (24/11/15)
                K_drid = #080000,   -- defaulted routine_id (must be resolved at compile-time)
                                    -- ie/eg gbl func Icallback(str name, int rid = rtn_id(name))
                K_asmm = #100000,   -- asm-modified: assume [S_gNew] becomes "anything", within
                                    --               reason, ie still matching [S_vtype].
--              K_struc = #200000, -- a structure type or variable (see builtins/structs.e)
$

-- symtab[i][S_Efct] values:
global constant E_none = 0,
                E_other = #20000000,
                E_vars = #1FFFFFFF,
                E_all = #3FFFFFFF

-- S_gInfo and S_gNew elements:
-- ===========================
--  A value of 0 means "we don't know anything yet".
--  pemit.e repeatedly invokes gvar_scan in pilx86.e (which then
--  invokes ilxlate() with isGscan=1), to build [S_gNew], then 
--  copies the results to [S_gInfo] for the next round.
--  [S_gInfo] should always be used to obtain any gvar info when 
--  reading a var, and [S_gNew] updated when writing to a var, 
--  including that in x+=y, we have a bit of both, ie
--  x[S_gNew] := Or(x[S_gNew],x[S_gInfo],y[S_gInfo]), where "Or"
--  is ficticious, and which may make much more sense later on.
--
--  Both [S_gInfo] and [S_gNew] may contain five elements:
--   (our imaginary "Or" would read/merge/write all 5,
--    with quite different logic for each element.)
--
global constant gType = 1,  -- T_integer..T_object, or 0 for "we don't know yet".
                            -- An [S_gNew][gType] of 0 is changed to T_object
                            --  before it is copied to [S_gInfo].
                gMin = 2,   -- The minimum integer value of the var
                gMax = 3,   -- The maximum integer value of the var
                            -- A {MAXINT,MININT} pair means "we don't know yet".
                            -- gMin and gMax only have meaning if gType can be
                            --  an integer but not a float, ie if
                            --      and_bits([gType],T_atom)=T_integer
                gEtyp = 4,  -- The possible element type(s) of the var, as above
                            --  0 or 1..15 (ie T_integer..T_object).
                            -- A value of 0 means "we don't know yet", and also
                            --  becomes T_object when copied to [S_gInfo].
                            -- NB The T_integer bit is *NOT* maintained for strings,
                            --  ie if and_bits([gType],T_string) then you must 
                            --  logically or_bits([gEtyp],T_integer).
                gLen = 5    -- Meaningless unless and_bits(gType,T_sequence)!=0.
                            -- -1 means "we don't know yet"
                            -- -2 means "may have multiple lengths"
                            -- >=0 means var always has this length, at least
                            --  when it is a sequence.
                            -- gEtyp and gLen may still contain useful info
                            --  when gType is (eg) object, though if the var 
                            --  may be any length and contain all kinds of
                            --  element, they will be T_object and -2.


-- A note on S_Const and S_Init:
-- ============================
--
--  On a variable, S_Init is a used to hold a -1 terminated chain of assigned
--  items, or 0 when the assignment state is unknown. On fixed string and sequence
--  S_Const entries it is in fact a (non-zero) ttidx, rather than 1. More details
--  can be found in pmain.e/DoSequence(), which represents string and subsequence 
--  elements using {-ttidx} and {ttidx} respectively; psym.e/AddUnnamedConstant() 
--  and (eg) pttree.e/tt_sequence() also contain relevant code and comments. The
--  rest of this section focuses on the zero and non-zero differences.
--
--  On a normal variable, S_Init is set non-0 at assignment (/use) and cleared
--   at some following end if/end procedure etc, and so indicates whether the 
--   variable is assigned at the current location in the code, whereas on a 
--   constant, S_Init is (no surprise here) set once and never amended again.
--   Note there is no setting of S_Init (on anything) to indicate unassigned.
--   On a normal var a non-zero S_Init means it definitely contains something
--   valid, whereas zero means it may or may not contain something.
--
--  constant ZERO=0 creates an S_Const with S_Init set, so that eg "=ZERO" can 
--    be translated to "cmp reg,0" (and perhaps more importantly there is no
--    point ever wasting a precious register by loading ZERO into it), whereas
--  constant Main=create(Window,...) leaves S_Init zero, indicating that the 
--    value is not known until runtime, hence "=Main" must (with some poetic 
--    licence) be translated to "cmp reg,[Main]".
--  In fact, of course, Main /will/ be assigned by the time any code references
--    it, and hence any non-S_Init S_Const should be treated as an init var.
--    It is worth taking a moment to fully digest that last point.
--
--  Thus S_Init has a completely different meaning on S_Const entries.
--
-- S_Nlink:
-- ========
--  This chain is for all symtab entries with the same name, eg:
--
--      p0.exw:
--      integer fred                                    -- [1]
--      include i1.e
--          i1.e:
--          global procedure fred() ?9/0 end procedure  -- [2]
--          include i2.e:
--              integer fred                            -- [3]
--              procedure x()
--                  for fred=1 to 10 do                 -- [4]
--
-- Inside the for loop, there will be no less than four symtab entries on
--  the S_Nlink chain, which starts in some tt[ttidx+EQ], and is resolved
--  without problem by the scope rules. At the end for, and eof of i2/p0,
--  one entry is delinked, so by the time we reach pemit.e (ie at the end
--  of compilation, after all source files are processed), only 1 remains.
--  At that point, for bind at least, the other 3 (locals) are put back 
--  onto the chain (see relink() in pttree.e, to facilitate this [S_Nlink] 
--  is set to -2 at delink) so that the .exe contains just one name "fred" 
--  referenced from 4 symtab entries (so it can write a readable ex.err).
--
--  S_Nlink is also used to keep a list of temporary (unnamed) vars, see
--  newTempVar() in psym.e and freeTempVar() in pmain.e - technically of
--  course these also all have the same name (-1). (There is probably no 
--  particular reason why S_Slink could not be used for that instead.)
--
-- S_Slink:
-- ========
--  This is the scope or secondary chain. It begins life as a pure scope
--  chain, for delinking all symtab entries off the S_Nlink chain at the
--  places mentioned above, eg at eof of i2.e we start at a localscopeX
--  and follow the S_Slink chain to unlink i2.e's locals.
--  Later on it has four other purposes:
--  *   Final var-idx for S_Const and S_Gvar entries is stored here,
--      see the start[DEV] of finalfixups() in pemit.e.
--(Erm, NEWGSCAN:)
--  *   The "routine_to_do_list" for pemit.e is built in S_Slink, see
--      finalfixups() and scanforShortJmp() in pemit.e.
--  *   Namespaces are linked together in pemit.e - while compile-time
--      resolution occurs using the normal tt[ttidx+EQ]/S_Nlink chain,
--      run-time resolution uses S_Slink, see builtins/prtnid.e
--  *   S_TVar entries are linked together for debug/diag purposes at
--      the start of pemit.e (DEV but ain't it already so??)
--
-- S_Clink:
-- ========
--  This is a special link for S_Const entries with the same value.
--  Clearly if we use the constant value "hello" five times in a
--  program, ideally we only write one copy of it to the .exe, with
--  a reference count of 5 as needed. Take for example the following
--  code fragment:
--      puts(1,"hello")
--      constant hello="hello",
--               hello2="hello"
--  The first line creates an unnamed constant; the second line can
--  "hijack" this and put a name on that symtab entry, however the
--  third line needs another symtab entry. So in that particular case
--  there would be one "hello" in the .exe with a reference count of
--  two, and two symtab entries for it, even though we actually used
--  the value three times in the source code. For more details, refer
--  to addUnnamedConstant() in psym.e, DoConstant() in pmain.e, and
--  (eg) DumpString() in pemit.e.
--
--  There are two rules for S_Clink:
--    * The first ever entry always remains the first. This is to
--      prevent eg:
--          puts(1,<something>)
--          constant AnOtherOne=1
--          puts(1,<something else>)
--      from using T_Const1 on the first puts but AnOtherOne on the
--      second puts, which would make list.asm/debugging confusing.
--      (They might both use AnOtherOne, at least it's consistent.)
--      However there is no requirement for any other ordering, so
--      typically we might expect the S_Clink chain to be:
--          first,last,last-but-one,...,third,second.
--    * While as above there may be Named and Unnamed constants, in
--      practice something like constant hello="hello" first creates
--      an unnamed constant (within Factor()) and then "hijacks" it
--      (within DoConstant()), and in fact if an Unnamed entry exists,
--      (ie [S_State]=-1) it will *always* be the _first and *only*_
--      entry on the S_Clink chain, since any futher occurrences use
--      any(/the first) existing Named entry. This means we never 
--      have to go looking for an Unnamed entry, and by that I mean 
--      at any point where we might want to re-use/slap a name on it.
--
--  S_Clink may one day be changed from 9 to 10 so that there is a
--  (positive) S_Tidx entry on S_Const and S_Gvar. [DEV/see S_Slink]
--
-- S_vtype:
-- ========
--  For builtin types, this is 1..15, see T_integer..T_object below,
--   and/or psym.e/syminit(). Note these are carefully arranged as a
--   bit-pattern so that or_bits/and_bits/not_bits can be used on them.
--  For user defined types, this is the routine no of the type check
--   routine, typically say 385..20,000. Use rootType() to discover
--   the underlying builtin type.
--
--  T_N is a dummy internal type, meaning "float but not integer".
--  This is not available for application coders for a number of
--  somewhat obsure reasons, including but not limited to:
--      3.0 (for example) is stored as the integer 3 for performance,
--          eg find(3,blah) does not have to check for 3 or 3.0, and
--          "if f=3" can just cmp rather than cmp/fld/fcomp/fnstsw.
--      1.5+1.5 ought to type check if stored in a T_N variable.
--  Likewise other dummy internal types are not available since they
--   would cause far more confusion than be of any help.
--
-- S_value:
-- ========
--  Mainly of use on S_Const entries, as S_nFno on namespaces (erm?), 
--  and not present/as S_sig on routine entries.
--  Note that when binding this contains the "seq_tree"'d version of
--  sequence constants, ie {-tidx} for string elements and {tidx} for
--  subsequences, to optimise the output - see DumpSequence in pemit.e
--  When interpreting, [S_value] contains the "proper" value.
--
-- S_nFno:
-- =======
--  Just to be absolutely clear on this, S_FPno is the file it is defined
--  in, and S_nFno is the file it refers to. Suppose main.exw (file 1) is
--
--      integer L
--      include win32lib.ew as w32
--
--  such that win32lib.ew is file 2, then w32 has the same S_FPno as L
--  (ie both L and w32 are locals "owned" by file 1) and the S_nFno on
--  the w32 symtab (/namespace) entry is 2. Hence "w32:X" means first 
--  look for a (local) namespace in currFile(1), ie the "w32" part, then 
--  go look for a global "X" defined (or [indirectly] included) by the
--  file indicated by that namespace's S_nFno(2). All dreadfully simple,
--  but sometimes it is all too easy to get confused by this, when playing
--  with the compiler innards, that is.
--  [Be advised that pdiag.e etc usually means win32lib.ew is > file 2.]
--
-- =======
--
--  S_ErrV, S_Init, S_Efct, and S_ErrR entries are discarded when writing a .exe.
--
--  S_ErrV just holds a line number for unused/not assigned etc messages, which
--          are emitted from unused_cleanup() in pilx86.e.
--          Note that after that point, the slot is resued for S_ConstChain:
--
--  S_ConstChain holds a chain of constant refs and recounts which must be fixed
--          up after the data segment has been completed. For instance, suppose
--          you have the call:
--              someproc(somestring,"fred")
--          The somestring /variable/ must be loaded and increfed eg as follows:
--              mov ecx,[somestring]    -- load
--              mov [param1],ecx        -- store
--              inc [ebx+ecx*4-8]       -- incref   (AGI stall occurs here ;-(()
--          (with some poetic licence, for instance we might call opMove,
--           to check for unassigned src or to deallocate the target, etc,
--           but the basics are all pretty much the same whatever we use.)
--DEV newEmit: we cannot relocate refs so no [in DLLs] (but refcounts are fine)
--          However: doing the same thing for a constant is silly considering
--          we know (or will) exactly what the ref is, so there should be no
--          need to load it, and we also know exactly where the refcount is 
--          or will be stored, so we can use a literal address and avoid that
--          AGI stall. Unfortunately, even by the time we "finalise" the x86
--          (in scanforShortJmp/blurph in pemit.e), we still do not know the
--          final address. S_ConstChain is zeroed in unused_cleanup() for 
--          S_Const entries and then keeps a linked list of places that need 
--          to be fixed up in the x86 binary, once we have decided exactly 
--          where the constant will live. isConstRef and isConstRefCount are
--          used to mark these things. As S_ErrV use ends in unused_cleanup,
--          S_ConstChain can reuse that slot in the symbol table.
--

-- K_used bit:
--      [do not confuse K_used with S_used; K_used should definitely be called      [DEV]
--       something else, but I cannot think of anything appropriate off hand..]
--       (how about K_param(=#10) and K_rlink(=#40)... [DEV])
--  This now has several meanings.
--      On a Tvar, it indicates the variable is a routine parameter. If a normal
--      variable is set but not used, no matter, eg void=xxx(), whereas for params 
--      (which are set whenever there is a call), you should get an unused warning.
--      On a routine, it initially indicates a top-level-sub, and is later used to      [DEV latter not NEWGSCAN (search&destroy?)]
--      link up all reachable routines during the bind phase.
--  Used in pmain.e (once), psym.e (once), pgscan(four times), and pemit.e (lots).
--  UPDATE: 05/07/2013. Also used in pmain.e/Assignment, when assigning a literal
--          on declaration. Bit of a fudge but it seems to work perfectly, plus I
--          have been trying for ages to think of a way to get a suitable warning
--          message ("x is unused") for eg "sequence x = {}", without things like
--          Main = create(Window...) throwing similar when clearly not wanted (ie
--          when symtab[N][S_State] does not end up with S_Used [nb yes S_] set).

global sequence opNames
global sequence opSkip
global sequence sqAble
global sequence opEfct

-- Note the builtin types are carefully arranged for bit-fiddling,
--      see psym.e, and pgscan.e, pltype.e amongst others.
global constant T_integer   = 1,
                T_N         = 2,    -- float but not atom (internal use only)
                T_atom      = 3,
                T_Dsq       = 4,    -- dword sequence but not string (internal use only)
                T_string    = 8,
                T_sequence  = 12,
                T_object    = 15,
                typeINSPO   = {T_integer,T_atom,T_string,T_sequence,T_object},
                T_pathset   = 16,
                T_fileset   = 17,
                T_nslink    = 18,
                T_cmdlnflg  = 19,
--              T_callstk   = 20,   -- spare (newEmit) DEV T_prevsym/T_optable?
--29/3/15:
                T_optable   = 20,
                T_maintls   = 21,
--              T_EBP       = 22,   -- spare (DEV T_level/T_EBP) -- compiled=0, interpreted={ebp4,esp4,sym4,gvar4,level}
                T_EBP       = 22,   -- compiled/listing=0, interpreted={ebp4,esp4,sym4} (set at last possible moment)
                T_ds4       = 23,   -- compiled = start of data section, same but /4 when interpreted ([T_EBP]!=0)
                T_constm1   = 24,
                T_const0    = 25,
                T_const1    = 26        -- (also used as limit in pcfunc.e and pdiag.e)

--
-- Note that symtab[T_EBP] is set immediately prior to running (interpreting) the just-generated code,
--                                                  and will/should appear as 0 in all listing files.
--      and that symtab[T_ds4] is floor(DSvaddr/4) when interpreted, but start of code section (without
--                                                  any of that division by 4) when compiled.

--      BTW, The internal types "float" and "dword_sequence" are 
--      not available for application programmers, since eg:
--          float F     F=1.5+1.5
--      would crash with typecheck error (we store 3.0 as integer 3,
--      in *all* cases, so that eg if N=3 can just be a simple cmp,
--      rather than test/cmp/fld/fcomp/fnstw/lahf/etc); and in
--          dword_sequence DS
--              DS = -1&72&73
--              DS = 71&72&73
--      the first assigment is OK, but the second would also crash
--      with a typecheck error since it is equivalent to DS="GHI".
--      It is the compiler's job to figure out when sequence/atom
--      really mean T_Dsq/T_N, rather than down to the programmer.
--
--      To be pedantic, I suppose T_string should really have been 4
--      and T_Dsq 8 (T_sequence unchanged) but it makes no difference
--      really and would now be a huge amount of work, not least with
--      updating all the comments, for absolutely no measureable gain.
--

--newEBP (this is all now gibberish [aka a wee bit out of date]:)
--  Phix uses a doubly-linked chain of "virtual stack blocks" (8K each)
--   instead of using the system stack:
--
--              0
--              ^
--               vsb
--              ^   V
--      ebp-->   vsb
--              ^   V
--               vsb
--                  0
--
--  While traditional languages use [ebp+/-nn] to locate local variables, and 
--  obviously fixed addresses for global/file-level variables and constants, 
--  Phix always uses fixed addresses, with opFrame saving locals in a vsb and 
--  opRetf restoring them. (Admittedly the [ebp+/-nn] approach might have made
--  Phix programs a bit smaller and faster, but the "one size fits all" method 
--  has the great advantage of being simpler.)
--
--  Now, Phix is written in Phix, and in "p p test" aka "p.exe p.exw test.exw",
--  this is all going on for p.exe, p.exw, and test.exw, ie there are not just
--  one but three completely independent vsb chains.
--
--  In particular, there are call_backs in both pdebug.e (the trace screen) and
--  pdiag.e (the code that creates ex.err files after an error). Further, the
--  compiler "re-uses" the pdiag.e compiled into p.exe when interpreting, rather
--  than load fresh copies for p.exw and test.exw. Application code will usually 
--  execute its mainstream and call_back code all in the same vsb chain, but I'm 
--  sure there could be similar cases there too.
--
--  When a bug occurs, we need to call pdiag.e, which needs to operate on/in the 
--  lowest of the three vsb chains, no matter what level the bug occured at.
--
--  When tracing "p test" (btw there is no claim that eg "p p test" will trace 
--  anything other than p.exw, and may in fact crash if it encounters trace(1) 
--  etc in test.exw), then it must alternate between executing test.exw and 
--  pdebug.e, in different vsb chains.
--
--  Of course, when executing normally the CPU register ebp is enough, but when
--  any call_back is invoked you must switch to the right chain.
--
--  FYI, every vsb block, as pointed to by ebp, carries a pointer to symtab, 
--       which contains an ebp. One hop round this circular loop would, if it
--       were ever needed, map "any old ebp" to the current live value.
--
--  NB: symtab[T_edb] is a 32-bit value, neither a Phix integer nor a Phix atom,    [DEV no such thing any more]
--      and should not be referred to by hll code in any way, that is after
--      retrieving symtab with opGetST (see pops.e, and prtnid.e is probably the 
--      most obvious of several examples in the builtins directory).
--

--DEV sug:  (as per ttree thingies)
--  T_Bin = 200
--  T_abort = 176
--  T_Ainc = 384
--  T_AInt = 234
--  T_AAtm = 248
--  T_AStr = 252
--  T_ASeq = 272
--  T_compare = 128
--  T_equal = 127
--  T_length = 131
--  T_floor = 133
--  T_floor_div = 0
--  T_sqfloor = 324
--  T_sqfloor_div = 320
--  T_routine = 222
--  T_print = 379
--  T_define_c_proc = 154
--  T_call_func = 174??

--25/1/20: (Alias_C_flags)
--global integer T_long         T_long = 0
--global integer T_ulong        T_ulong = 0
--global integer T_ptr          T_ptr = 0
global integer T_ptr            T_ptr = 0
global integer Z_int            Z_int = 0   -- (T_int already in use by pttree.e/pilasm.e)
global integer T_uint           T_uint = 0
global integer T_int64          T_int64 = 0
global integer T_uint64         T_uint64 = 0
global integer T_win32          T_win32 = 0
global integer T_linux          T_linux = 0
global integer T_Bin            T_Bin = 0
global integer T_Asm            T_Asm = 0
global integer T_abort          T_abort = 0
global integer T_repeat = 0,
               T_repeatch = 0
global integer T_Ainc           T_Ainc = 0
global integer T_AInt           T_AInt = 0
global integer T_AAtm           T_AAtm = 0
global integer T_AStr           T_AStr = 0
global integer T_ASeq           T_ASeq = 0
global integer T_compare        T_compare = 0       -- map "if compare(a,b)<relop>-1/0/1" to "if a<relop>b"
--global integer T_find         T_find = 0          -- for defaulting 3rd param to 1
global integer T_find           T_find = 0          -- for enum type
global integer T_match          T_match = 0         -- for defaulting 3rd param to 1
--global integer T_get_text     T_get_text = 0      -- for defaulting 2nd param to -2
global integer T_throw          T_throw = 0     -- for defaulting 2nd param to {}
global integer T_equal          T_equal = 0         -- map "if equal(a,b)" to "if a=b"
global integer T_length         T_length = 0        -- map s[..length(s)] to s[..-1]
global integer T_floor          T_floor = 0         -- map "floor(a/b)" to opDivf a,b
global integer T_floor_div      T_floor_div = 0     -- ""
global integer T_sqfloor        T_sqfloor = 0       -- map "floor(p/s)" to sq_floor(p,s)
global integer T_sqfloor_div    T_sqfloor_div = 0   -- ""
--global integer T_malloc           T_malloc = 0
--global integer T_mfree            T_mfree = 0
global integer T_routine        T_routine = 0
global integer T_print          T_print = 0
--DEV no longer needed?
--global integer T_call_func        T_call_func = 0
--global integer T_dcproc       T_dcproc = 0        -- map define_c_proc(l,n,a) to define_c_func(l,n,a,0)
global integer T_command_line   T_command_line = 0  -- (for parameter defaulting)
global integer Z_command_line   Z_command_line = 0  -- (1 iff needs linking for parameter defaulting)
global integer T_SLASH          T_SLASH = 0
global integer T_platform       T_platform = 0
global integer T_machine_bits   T_machine_bits = 0
global integer T_machine_word   T_machine_word = 0
global integer Z_version        Z_version = 0       -- (T_version used in format)
--global integer Z_requires     Z_requires = 0      -- (consistent with T_version)
global integer T_min            T_min = 0
global integer T_minsq          T_minsq = 0
global integer T_max            T_max = 0
global integer T_maxsq          T_maxsq = 0
global integer T_EnterCS        T_EnterCS = 0
global integer T_LeaveCS        T_LeaveCS = 0
global integer T_is_struct      T_is_struct = 0
global integer T_new            T_new = 0
global integer T_struct_start   T_struct_start = 0
global integer T_struct_field   T_struct_field = 0
global integer T_end_struct     T_end_struct = 0
global integer T_extend_struct  T_extend_struct = 0
--global integer T_field_type   T_field_type = 0
global integer T_fetch_field    T_fetch_field = 0
global integer T_store_field    T_store_field = 0
global integer Z_struct         Z_struct = 0
global integer T_free           T_free = 0
global integer T_ffree          T_ffree = 0

-- these are actually temps for setting up hll_stubs, but may as well make them global:
global integer T_and_bits = 0, 
               T_not_bits = 0,
               T_or_bits = 0,
               T_xor_bits = 0,
               T_remainder = 0,
               T_power = 0, 
               T_cos = 0,
               T_sin = 0,
               T_tan = 0,
               T_arctan = 0,
               T_log = 0,
               T_sqrt = 0,
               T_rand = 0,
               Z_append = 0,    -- (T_xxx got nicked by ttree.e)
               Z_prepend = 0    --              ""


global integer TIDX
               TIDX = -1
--DEV (for newEBP)
-- threadstack should not need any tvars, but pemit.e messed up.
-- so, left TIDX as-is and added LIDX for the new way of doing things...
global integer LIDX
               LIDX = -1    
-- update:
-- the problem may be just (from psym.e) no equiv in pemit.e after newEBP resets TIDX everywhere..
----25/2/10: leave space for a proper T_Dsq header at the start of ds:
----    TIDX = 0
--  TIDX = -5

--with trace
----with type_check -- ineffective, need to kill the without in p.exw
----DEV not good on p p t8...
--constant iNNN=1116  -- item to monitor
--global integer iNNN = 0
--object sNNN sNNN=0
--type symt(sequence s)
--  if iNNN and length(s)>=iNNN then
--      if not equal(s[iNNN],sNNN) then
----?1
----            trace(1)
--          sNNN = s[iNNN]
----if sNNN=0 then ?9/0 end if
----if sequence(sNNN) then
--  ?sNNN
----end if
--if atom(sNNN) then ?9/0 end if
--      end if
--  end if
--  return 1
--end type
--global symt symtab

global sequence symtab

global integer symlimit
               symlimit = -1

global sequence allfiles,
                allpfiles,  -- after ifdef processing (or ==allfiles if none); parse/compile from this
                expandedYet,
                exptext,    -- from allfiles (not allpfiles); errors/listings from this
                linestarts,
                unresolved_routine_ids,
                finalOptWarn

global sequence aatidx,     -- ttidx of autoasm label names (indexed by opcode)
                aasydx,     -- coresponding symtab index (indexed by opcode)
                aartypes    -- result types (indexed by opcode)

global integer some_unresolved_rtnids
--EXCEPT
global integer exceptions_in_use = 0

global integer line         -- shadowed by tokline
               line = -1
global integer col          -- shadowed by tokcol
               col = -1

--DEV move to VM\pProfileN.e?
global integer profileon    -- set to OptProfile[Time] by with profile[_time]
               profileon = 0 -- prevents mixing the two, triggers profile.e call.

global sequence ptab        -- profile table
global object opstat        -- opstats table (or 0)

global integer emitON       -- eg avoid writing 'if debug then' blocks when debug=0
               emitON = 1

global integer OptConsole
               OptConsole = -1      -- default is "unknown", ie p -c == console, pw -c = gui
--                           0      -- if zero, it uses (IMAGE_SUBSYSTEM_WINDOWS_GUI=2)
--                          +1      -- if true, it uses (IMAGE_SUBSYSTEM_WINDOWS_CUI=3)
--DEV to go:
global integer OptLicence
               OptLicence = 1       -- default = apply licence, if any
                                    -- p.exw must NOT apply licence, it cripples it...
global integer subvers
               subvers = #00000004 -- default is 4.0
--             subvers = #000A0003 -- value for 3.10

-- see psym.e for more details about the following five fields:
global integer scopelevel
               scopelevel = 0

global sequence scopefiles,
                scopelines,
--(DEV: oneString only)
                scopecols

global sequence localscopes -- start of cleanup chain for dropScope
global sequence scopetypes

global constant S_File=2,   -- Scope types
                S_Rtn=3,
                S_Block=4

global integer toktype
               toktype = -1

global integer Ch       -- current char, -1=EOF, 0=abort() at top level.
               Ch=-1

global integer chartype -- set by skipSpacesAndComments(), unless Ch=-1.

--
-- The very simple rule for compilation options is that they are always 
-- inherited by an include file, and restored at the end of each file.
--
global constant OptProfile      = 1, 
                OptProfileTime  = 2, 
                OptTrace        = 3, 
                OptWarning      = 4, 
                OptTypeCheck    = 5, 
                OptDebug        = 6

global sequence optset

global procedure resetOptions()
    optset = {0,0,0,    -- profile[time]\trace: off,
              1,1,1}    -- warning\typecheck\debug: on
end procedure
--resetOptions()

global integer ltline       -- line no that LineTab[$] refers to
--             ltline = -1

global sequence LineTab

global sequence s5
                s5 = {}

global integer lastline     -- last line no emitted
               lastline = -1

global integer emitline     -- line no to emit (pre Expr())
               emitline = -1

global integer currRtn
               currRtn = 1

global integer LastStatementWasAbort -- avoid emitting opCleanUp after an abort.

-- mergeSets. See pilx86.e (also used in pmain.e)
global constant scMerge=1, exprMerge=2, exitMerge=3, ifMerge=4, endIfMerge=5, breakMerge=6

--DEV should these be set to -#FFFFFFFF .. #FFFFFFFF when a 32-bit p.exe is creating a 64-bit exe??
global constant MININT =-#40000000,                                             -- -1073741824
                MAXINT = #3FFFFFFF,                                             -- +1073741823
--              MINATM = float64_to_atom({#FF,#FF,#FF,#FF,#FF,#FF,#EF,#FF}),    -- -1.7976931348623146e308
--              MAXATM = float64_to_atom({#FF,#FF,#FF,#FF,#FF,#FF,#EF,#7F}),    -- +1.7976931348623146e308
                MAXLEN = #30000000  -- assume eg length(s)+1 is an integer

global constant 
    -- (values #00..#FF are as-is bytes)
    --                          -- [read all the following 4s as 8s for 64-bit]
    isOpCode        = #000100,  -- 4 byte offset, fixup from VMep[idx] (once actual code address known)
--  isApiFn         = #000101,  -- 4 byte location for indirect WinAPI call
    isAPIfn         = #000102,  -- 4 byte location from (NB existing!) import section (for fast file I/O).
    isVar           = #000200,  -- 4 byte absolute, fixup as DSvaddr+idx*4-4 (once CSvsize hence DSvaddr known).
    isVar1          = #000201,  --  "" but with 1-byte literal yet on instruction (needed for rip addressing)
    isVar4          = #000204,  --  "" but with 4-byte literal yet on instruction (needed for rip addressing)
    isVno           = #000300,  -- 4 byte symtab index (that will need mapping if the symtab is packed)
    isData          = #000400,  -- 4 byte absolute, fixup as DSvaddr+offset (eg symtabptr is mov esi/rsi,[ds+8])
--DEV implement after newEBP fully working:
--these two added 26/11/2011, see pmain.e/ilasm()
--  isD8            = #100300,  -- 1 byte offset to ebp, for tvars only
--  isD32           = #100400,  -- 4 byte offset to ebp, for tvars only
--DEV dead?? (newEmit)
    isConst         = #000600,  -- 4 byte constant value, substitiuted immediately (ie in ilasm() itself)
    isConst8        = #000608,  -- 1 byte constant value, substitiuted immediately (ie in ilasm() itself)
--DEV these two added ~ 15/10/09, no performance gain observed, programs are a bit smaller though.
-- not newEmit (unless interpreting, perhaps [isConstRefCount is fine]):
    isConstRef      = #000700,  -- 4 byte absolute, replace with const's (fixed) ref
    isConstRefCount = #000800,  -- 4 byte absolute, replace with (fixed) address of const's refcount field
    isJmpG          = #000900,  -- 4 byte offset to global label (never shortened)
    isGaddr         = #000A00,  -- 4 byte absolute to global label (never shortened)
    isILa           = #001000,  -- 4 byte absolute, fixup from symtab[routineNo][S_il]
    isIL            = #002000,  -- 4 byte offset, fixup from symtab[routineNo][S_il]
    isBase          = #003000,  -- 4 byte absolute, fixup ignoring isShortJmp (jump table base)
    isAddr          = #004000,  -- 4 byte absolute, fixup from (offset-isShortJmp*3)
    isJmp           = #008000   -- 4 byte offset, examine for isShortJmp-able, fixup -isShortJmp*3
--constant                       -- not global: use only in scanforShortJmp/blurph, not before.
--  isShortJmp      = #010000   -- isJmp that has been found to fit in a byte, fixup as isJmp, patch 5->2 or 6->2
                                -- (but that value, in pemit2.e, must not clash with anything in here.)
global constant --DEV unused/broken[?]:
    isDead          = #080000   -- indicates dead code, eg #080005 means 5 dead bytes (see scanforShortJmp).

-- constref tables for listing, see pemit.e/plist.e/p2asm.e
global sequence craddr, cridx

global string cl1       -- command_line[1], aka "the stub". [DEV to go?]
                        -- ie when creating a new .exe, most of this file is copied
                        --    byte-by-byte as a starting point.

global integer returnvar        -- -1: top_level/return illegal, 0: in a proc, +ve: func/type return var (symtab idx)
               returnvar = -1   --          - see DoRoutineDef/DoReturn for more details.

--
-- isAPIfn uses; collected in ilasm(), resolved in readAllHeaders, applied in blurph()
--
global sequence APIlibs,    -- eg {"kernel32.dll","user32.dll",...}
                APIerrlib,  -- first use file/col of each APIlibs entry
                APINames,   -- eg {{1,"AllocConsole"},{1,"ExitProcess"},{2,"MessageBoxA"},{1,"GetStdHandle"},...}
                APIerritem  -- first use file/col of each APINames entry

--
-- global labels
--
global constant G_declared  = #01,  -- (absense is only an error when processing an emitted isJmpG)
                G_used      = #02,  
                G_set       = #04,  -- (set when opLogPos processed)
                G_init      = #08,  -- a :>init style label
                G_bang      = #10,  -- a :!bang style label
--28/2/16:
--              G_stop      = #20   -- a :<stop style label (not yet implemented)
                G_exch      = #20   -- an :<exception_handler label (not yet implemented)

global sequence glblused,   -- G_declared/G_used/G_set
                glboffset,
--              glbttidx,
                glblabel,   -- if G_declared a currtls else a fileno (0 if from optable)
                glblline,
                glblcol,
                glblname

global integer gexch = 0    -- index of exception handler (PE64 only)

--global integer glbopFrame = 0

global integer intellisense
               intellisense = 0
global sequence trapfile, trapkey
global integer trapline, trapcol, trapmode, trapfileno, trapns

--
-- Resource Section defaults
--
constant default_rs_icon = {"ok.ico",{0,1}}
global sequence rs_icon
                rs_icon = default_rs_icon
global sequence rs_version
                rs_version = {}
constant default_rs_manifest = 
"<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"yes\"?>\r\n"&
"<assembly xmlns=\"urn:schemas-microsoft-com:asm.v1\" manifestVersion=\"1.0\">\r\n"&
 "<application xmlns=\"urn:schemas-microsoft-com:asm.v3\">\r\n"&
  "<windowsSettings>\r\n"&
   "<dpiAware xmlns=\"http://schemas.microsoft.com/SMI/2005/WindowsSettings\">True</dpiAware>\r\n"&
  "</windowsSettings>\r\n"&
 "</application>\r\n"&
-- "<assemblyIdentity name=\"x.x.x\" processorArchitecture=\"x86\" version=\"5.1.0.0\" type=\"win32\"/>\r\n"&
 "<assemblyIdentity name=\"phix\" processorArchitecture=\"x86\" version=\"5.1.0.0\" type=\"win32\"/>\r\n"&
-- "<description>no</description>\r\n"&
 "<description>phix</description>\r\n"&
 "<dependency>\r\n"&
  "<dependentAssembly>\r\n"&
   "<assemblyIdentity type=\"win32\" name=\"Microsoft.Windows.Common-Controls\" version=\"6.0.0.0\" "&
--DEV try "*" here instead of replace_all in pemit2...
--          "processorArchitecture=\"x86\" publicKeyToken=\"6595b64144ccf1df\" language=\"*\" />\r\n"&
            "processorArchitecture=\"*\" publicKeyToken=\"6595b64144ccf1df\" language=\"*\" />\r\n"&
  "</dependentAssembly>\r\n"&
 "</dependency>\r\n"&
 "<trustInfo xmlns=\"urn:schemas-microsoft-com:asm.v3\">\r\n"&
  "<security>\r\n"&
   "<requestedPrivileges>\r\n"&
    "<requestedExecutionLevel level=\"asInvoker\" uiAccess=\"false\"/>\r\n"&
   "</requestedPrivileges>\r\n"&
  "</security>\r\n"&
 "</trustInfo>\r\n"&
"</assembly>\r\n"
--/*
  <assembly xmlns="urn:schemas-microsoft-com:asm.v1" manifestVersion="1.0"> 
    <compatibility xmlns="urn:schemas-microsoft-com:compatibility.v1"> 
      <application> 
        <!--The ID below indicates application support for Windows Vista --> 
          <supportedOS Id="{e2011457-1546-43c5-a5fe-008deee3d3f0}"/> 
        <!--The ID below indicates application support for Windows 7 --> 
          <supportedOS Id="{35138b9a-5d96-4fbd-8e2d-a2440225f93a}"/> 
      </application> 
    </compatibility>
  </assembly>
--[DEV] untested, was told it don't work...
 "<compatibility xmlns=\"urn:schemas-microsoft-com:compatibility.v1\">\r\n"&
  "<application>\r\n"&
--      <!--The ID below indicates application support for Windows Vista --> 
   "<supportedOS Id=\"{e2011457-1546-43c5-a5fe-008deee3d3f0}\"/>\r\n"&
--      <!--The ID below indicates application support for Windows 7 --> 
   "<supportedOS Id=\"{35138b9a-5d96-4fbd-8e2d-a2440225f93a}\"/>\r\n"&
  "</application>\r\n"&
 "</compatibility>\r\n"&
--*/
--/*
: from http://stackoverflow.com/questions/20096706/how-does-windows-decide-whether-to-display-the-uac-prompt :
<assembly xmlns="urn:schemas-microsoft-com:asm.v1" manifestVersion="1.0"> 
 <assemblyIdentity version="1.0.0.0" processorArchitecture="X86" name="client" type="win32"/> 
 <description>Update checker</description> 
</assembly>
:and from a gamer site:
<assembly xmlns="urn:schemas-microsoft-com:asm.v1" manifestVersion="1.0">
 <dependency>
  <dependentAssembly>
   <assemblyIdentity type="win32" name="Microsoft.VC90.CRT" version="9.0.21022.8" processorArchitecture="x86" publicKeyToken="1fc8b3b9a1e18e3b" />
  </dependentAssembly>
 </dependency>
</assembly>
:and from codeproject:
<assembly xmlns="urn:schemas-microsoft-com:asm.v1" manifestVersion="1.0">
</assembly>
<assembly xmlns="urn:schemas-microsoft-com:asm.v1" manifestVersion="1.0">
  <assemblyIdentity type="win32"
                    name="AutoLogon"
                    version="1.0.0.0"
                    processorArchitecture="X86" />
  <description>
    Auto logon tool
  </description>
</assembly>
 
--*/
global sequence rs_manifest
                rs_manifest = default_rs_manifest

global procedure ResetResourceSectionDefaults()
-- (not really needed, but called at the end of the testset loop just in case)
    rs_icon = default_rs_icon
    rs_version = {}
    rs_manifest = default_rs_manifest
end procedure

--DEV these need setting based on the p[w].exe being run:
-- (also, gui/console and subvers belong with this lot)
--global integer PEtrap = 0
--type PEinteger(integer i)
--  if PEtrap then
--      if i!=0 then ?9/0 end if
--  end if
--?{i,PEtrap}
--  return 1
--end type
global integer PE
--             PE = 1
               PE = (platform()==WINDOWS)
global integer X64
--             X64 = 0
               X64 = (machine_bits()==64)
--             X64 = (bits()==64)
global integer DLL
               DLL = 0
--global integer GUI
--             GUI = -1 -- (instead of OptConsole)
--global integer SSV
--             SSV = #00000004  -- subsystem version

global sequence knownAddr
                knownAddr = {}
global sequence knownNames
                knownNames = {}

global integer opLntpcalled = 0 -- (skip MCHK if set)

global integer use_pini_time = 0

global sequence exports
global sequence exportaddrs -- addresses of static callbacks that invoke routine symtab[exports[i]]
                            -- (note: the first is always DllMain and includes all :>init calls)

global integer mapEndToMinusOne         -- 0=no, -ve = $ only (-1..-4 signal where set for debug),
               mapEndToMinusOne=0       -- +ve = $ and end, set to T_end/'$' when triggered.
                                        -- (oops, only set to '$' from -1/DoConstant)

global integer with_js = 2      -- 0: without js, 1: with js, 2: default/any, treat as 0
