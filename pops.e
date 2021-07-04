--
-- pops.e
--
-- Phix opcodes
--

--
--DEV This is overdue for a complete rewrite. Most of the "careful ordering" stuff
--  has been replaced with explicit declarations in psym.e, and optable.e seems to
--  cope quite effectively with any reordering. The distinction between "real" and
--  "virtual" opcodes is no longer important. Only {opInt,opAtom,opSq,opStr,opObj}
--  still require any ordering, according to a quick test 23/10/15 (see pmain.e).   [now gone as per update below]
--  It really deserves enum/nxt()/columnize, and some of the calling conventions
--  shown below may now be out of date (see pilx86.e|builtins/VM for the gospel).
--  Obviously, leave this as-is, create a pops2.e, toggle the include statement
--  in p.exw to test, and only properly overwrite once fully tested. I expect it
--  will need to be ~300 lines before it compiles cleanly.
--  UPDATE: my first attempt at pops2.e was an abject failure. I have however 
--  applied a few lessons learnt here (deleted a few things, including maxNVop,
--  and opIres/opNres/opSres/opPres/opOres). I think we can suffer a few append,
--  but constant opMove=1 is probably just about worth it over opMove=nxtOP():
--  it makes editing this painful, but gives pilx86 a proper fixed jump table.
--  (I may even have finally killed the 255 limit, but don't quote me on that.)
--  I suppose the best thing would be to add entries for all opcodes to Phix.chm,
--  and leave this looking rather sparse/clean.

--
-- Opcodes are used to locate entry points in the Virtual Machine, and hence the
--  following definitions must exactly match the equivalent table in the assembly
--  back-end [VMep]. Hence when/if Phix gets an "enum", this is probably the last
--  place you'd want to use it. [DEV VMep vanished quite some time ago...]
-- It may help to consider the VM as similar in theory to clib, with the compiler 
--  electing to inline/emit machine code directly for as many trivial/common cases 
--- as possible, but some things such as puts, s[i][j][k]=t, etc., are always 
--  performed by the VM. (If you want a look at the actual code in the VM, see 
--  the constant dumpVM in plist.e.)
--
-- Opcodes also inherently define the intermediate code, as emitted by the parser, 
--  which is carefuly analysed before emitting the final x86 machine code.
--
-- At the end of this file I have replicated the calling conventions [DEV!]
--
--   (TIP: inserting and/or removing entries can be tricky:
--          if "p -cp" fails, try "p p -cp" instead.)
--
without trace

integer opUsed
        opUsed=0
procedure opName(sequence label, integer chk, integer skip)
    opUsed += 1
    if opUsed!=chk then opUsed = 9/0 end if
    opNames[opUsed] = label
    opSkip[opUsed] = skip
end procedure

global constant
    --
    opMove = 1,     -- p1 = p2  (p3=onDeclaraion, p4=srcInit, p5=srcLtype)
        -- verifies p2 has been assigned a value, also when needed deallocates p1 & increfs p2
                                                             -- nb VMep[opMove] is NULL
    -- optimised forms of opMove, when the compiler knows, for certain, that source, target or
    --  both are (initialised) integer. These are virtual opcodes/always inlined.
    opMovbi = 2,        -- opMove when both are integer, and source init.
--DEV opStoreFlt = 2, [and make opMovbi a proper virtual opcode!]
                        --  no need to incref/dealloc either -- nb VMep[opMovbi] is StoreFlt (was AllocSeq)
    opMovsi = 3,        -- opMove when source is init integer (p3=onDeclaration)
                        --  no need to incref src            -- nb VMep[opMovsi] is deallocX
    opMovti = 4,        -- opMove when target is integer
                        --  no need to dealloc target        -- nb VMep[opMovti] is unassigned/TchkFail

    --
    -- The following opcodes have fixed literal int as first param (up to opCallOnce):
    --
--  opStat = 5,         -- opstats[opcode]+=1 (for development/analysis use)
                        -- [NB may be broken/not tested prior to any release]
--DEV/SUG replace with opLn,line,flags (flags of 0=>opLn, 1=>opLnt, 2=>opLnp, 3=> opLnpt)??
--DEV better order is: opLn=1..opLnpt=4,opTchk=5,opTcFail=6,opMove=7..opMovti=10, opStat elsewhere
--      (oh, forgot about the fixed literal int thing...)
    opLnt = 6,          -- traceable line number
        -- output under "with trace" only.
        -- prior to a trace(1) call, has no effect, else calls show_trace(). See pTrace.e
    opLnp = 7,          -- profile line number
        -- output under "with profile" only.
        -- after a profile(0) call, has no effect, else updates ptab. See pProfile.e
    opLnpt = 8,         -- profile_time line number
        -- output under "with profile_time" only.
        -- after a profile(0) call, has no effect, else updates ptab. See pProfile.e
--opLnpclone    profile_clone line number
--opLnpcover    profile_coverage line number

    opTchk = 9,         -- type_check(symtab[p1])
        -- call the user defined type routine after assignment has occurred.
        -- Builtin types are inlined/use opTcFail

    opTcFail = 10,      -- error: typecheck (builtin), See also opTchk.

    opBadRetf = 11,     -- code should not reach this!
        -- emitted at the end of every function and type as a safety measure

    opUnassigned = 12,
        -- usage: (only) when we cannot be sure a variable is assigned, so
        --               have to test, and the test fails! See notes below.
--DEV to go (only used in <=pfileio4.e)
--  opAllocStr = 13,

    opCallOnceYeNot = 13,
        -- Used at the start of many builtin\VM sources, albeit in the form 
        --  "jmp :!opCallOnceYeNot". An include inc1.e statement, where inc1.e 
        --  begins with puts(1,"started\n"), issues an opCallOnce so that the
        --  puts (or whatever) is executed as expected. However, if there is 
        --  no top-level code (or it is just "jmp :%opRetf", which pulls off 
        --  exactly the same trick as "jmp :!opCallOnceYeNot") then obviously 
        --  no opCallOnce is either needed or wanted. Given there are 40-odd
        --  source files in builtins\VM, we want to keep numbers down a bit.
        -- Of course :!opCallOnceYeNot is a real error, and can be triggered 
        --  if you add initialisation code to a builtins\VM source code file, 
        --  but forget about changing over to a "jmp :%opRetf". I decided to
        --  use :!opCallOnceYeNot instead of :%opRetf as both the intent is
        --  clearer, and it triggers should I accidentally add such code.
--opTchk2 = 13,
--opTcFail2 = 14,
--  opWithJS = 14,
    opDeSeq = 14,   -- (re)set ma_ip flag in pRepeN.e
    -- 15,16 spare
    --
    -- opFor,init,ctl,step,limit,tgt/x86loc
    -- opFor checks control var/init/limit/step are integer and sets C flag if
    --   loop should be iterated zero times. For most loops, opFor should only
    --   generate code for "i:=init"; init/limit/step may need to be opUnassigned
    --   and the backend code is only called when they may be floats or it does
    --   not know the sign of step at compile time. Note that in Phix, any float
    --   is automatically an error, by design, since they are deeply unreliable.
    --   (eg on RDS Eu/OpenEuphoria, 
    --           for i=0 to 1 by 0.1 do iterates 11 times
    --       but for i=1 to 2 by 0.1 do iterates 10 times. Try it and see.)
    --
--DEV to go...
--  opFor = 16,

    opFrame = 17,       -- create a call frame (save vars)
        -- opFrame is routine no, first, last [DEV: LIAR]
        -- saves parameters, local variables, and temporaries first..last, and
        -- clears them all to "unassigned".
    opCallOnce = 18,    -- call tls once
        -- if I have main.exw which includes misc.exw, then there will be two tls
        -- (top level subroutine) entries, one for main [==symtab[T_maintls]] and 
        -- one for misc; opCallOnce is emitted at the include statement to perform 
        -- includee tls, or before a forward (auto-include) call to ensure any 
        -- initialisation gets done. Multiple opCallOnce to same tls have no effect.
--DEV may yet be required:
--  opCalli = opCode("opCalli",1?2),        -- call tls if rqd, then routine ("")

    --end of first param fixed (plus skip <0, ie opMkSq..opSubss).

    opRetf = 19,        -- return from frame
        -- used for procedures, functions, and types
    --
    -- opMkSq is n (literal length), res, en,...,e2,e1
    --
    opMkSq = 20,        -- res={e1,e2,..en}
    --
    -- opMkSqi is an optimised version of opMkSq, when res is known to be
    --          a sequence of integer (/not/ just that e1..en are ints);
    --          hence res can be reused (if long enough, refcount of 1 and
    --          without a delete_routine) without deallocating contents.
    --          [opMkSqi is never emitted in il]
    --
    opMkSqi = 21,       -- res={e1,e2,..en}
    --
    -- opRepe is n (noofsubscripts), ref, rep, idx1..idxn           -- ref[idx1][idx2]~[idxn] := rep
--erm...
    -- opRepe is n (noofsubscripts), rep, idxn..idx1, ref
    -- see also opRepe1
    --
    opRepe = 22,
    --
    -- opReps is n (noofsubscripts), ref, idx1..n, sliceend, rep    -- ref[idx1]~[idxn..sliceend] := rep
    --
    opReps = 23,
    --
    -- opSubse is N, ref, idx1..idxN, res                           -- res := ref[idx1][idx2]~[idxN]
    --
    opSubse = 24,
    --
    -- opSubss is N, ref, idx1..idxN, sliceend, res                 -- res := ref[idx1]~[idxN..sliceend]
    --
    opSubss = 25,

    --
    -- opConcatN is N, res, ref1..refN                              -- res := ref1&ref2..&refN
    --
    opConcatN = 26,

    --
    -- opRepe1 is ref, idx, rep
    -- optimised form of opRepe for the single subscript case (common cases are inlined)
    --
    opRepe1 = 27,                               -- ref[idx] := rep
    opRepe1ip = 28,                             --   "", when ref is T_Dsq of integer
-- dropped 26/07/14, resurrected 9/10/14:
    opRepe1is = 29,                             --   "", when ref is string

    --
    -- opSubsss is ref, slicestart, sliceend, res       -- res := ref[slicestart..sliceend]
    --
    opSubsss = 30,                              -- s = t[i..j], when s,t are strings
--  opSubssp = 31??
    -- 31,32 spare
    --
    -- opSubse1 is res, ref, idx, isInit[ref and idx]:  res:=ref[idx], aka N=p1[p2].
    -- optimised forms of opSubse for the single subscript case (common cases are inlined)
    --
    opSubse1 = 33,                              -- res := ref[idx]
    opSubse1i = 34,                             -- res := ref[idx], when res is integer
    opSubse1is = 35,                            -- res := ref[idx], "" and ref is string
    opSubse1ip = 36,                            -- res := ref[idx], "" "" T_Dsq of integer
--DEV/SUG:
--  opSubse1tp = ??,                            -- res := ref[idx], when res is <t> and ref is T_Dsq of <t> (t!=T_integer)

    -- 37 spare

    --
    -- Conditional tests on initialised integers are always inlined; these
    --  opcodes handle floating point, string and nested sequence comparison, 
    --  unassigned errors, etc. Behind the scenes, the VM (virtual machine) 
    --  actually only implements three (real) opcodes for the following eight
    --  (which are virtual/il opcodes), being opJif(=opJnot), opJccE (eq/ne), 
    --  and opJcc (ge/lt/gt/le). For example:
    --
    --                      "if a>=b then"
    --  is this in il:
    --                      opJlt,a,b,<end if> 
    --  and this in asm:
    --                      mov edi,[b]
    --                      mov eax,[a]
    --                      call opJcc
    --                      jl <end if>
    --
    -- (more often than not hll conditions generate inverted machine jumps)
    -- Note: "fred"=1 is simply false (ie 0), rather than {0,0,0,0} or the
    --       infamous "true/false condition must be an ATOM" - though that 
    --       error can still occur via opJif/opJnot[x].
    --
    opJif = 38,         -- if b then            \ inlined if 
    opJnot = 39,        -- if not b then        /  b is atom

    opJge = 40,         -- if b>=c then         \ inlined if
    opJlt = 41,         -- if b<c then          /   both int
    opJeq = 42,         -- if b=c then           \ inlined if
    opJne = 43,         -- if b!=c then          / either int
    opJgt = 44,         -- if b>c then          \ inlined if
    opJle = 45,         -- if b<=c then         /   both int

--DEV14: dead (see invert on opJnotx)
--  opJifx = 46,        -- if b[c] then
    opJnotx = 47,       -- if not b[c] then

    --
    -- Set cc opcodes are similar to opJcc, except first param is bool result
    --

    opSge = 48,         -- a = (b>=c)
    opSlt = 49,         -- a = (b<c)
    opSeq = 50,         -- a = (b=c) aka a = equal(b,c) -- NB a=sequence(b) is opSq.
    opSne = 51,         -- a = (b!=c)
    opSgt = 52,         -- a = (b>c)
    opSle = 53,         -- a = (b<=c)
    -- (nb keep opSge..Sle mappable to/from opJge..opJle)

    opScmp = 54,        -- a = compare(b,c) [see also cmap() in pmain.e]

--  opFind = 55,        -- a = find(b,c[,from])
--  opMatch = 56,       -- a = match(b,c[,from])

    opXor = 57,         -- a = b xor c  (a is set to 1 or 0)
    -- btw "and", "or" are always short-circuited: opAnd, opOr simply do not exist at all.

    -- builtins which return an integer (to opIres):
    opInt = 58,         -- a = integer(b)
    opAtom = 59,        -- a = atom(b)
    opSq = 60,          -- a = sequence(b)  -- NB opSeq is a=(b=c).
    opStr = 61,         -- a = string(b)
    opObj = 62,         -- a = object(b) [always 1] [DEV]
        -- opInt..Str are only called when the result (a) is non-integer and may need deallocating,
        --            or b is not init/may need an unassigned error. opObj is not called at all.
    opInt0 = 63,        -- \
    opAtom0 = 64,       --  \ optimised forms  (the 0/1 result is just left)
    opSq0 = 65,         --  /  used by opJtyp  (in edx, not stored anywhere)
    opStr0 = 66,        -- /

    opLen = 67,         -- a = length(b)
    opNot = 68,         -- a = not b        -- NB b must be atom (see sq_not)
    opOpen = 69,        -- a = open(b,c)
    opGetc = 70,        -- a = getc(b)
    opGetKey = 71,      -- a = get_key()
    opWaitKey = 72,     -- a = wait_key()
    opSeek = 73,        -- a = seek(b,c)
    opWhere = 74,       -- a = where(b)
    opLock = 75,        -- a = lock_file(b,c,d)                         -- 55
    opUnLock = 76,      -- unlock_file(a,b)     --DEV??
    opRand = 77,        -- a = rand(b)      -- NB b must be atom (see sq_rand)
--  opChDir = 78,       -- a = chdir(b)
    -- 78 spare
--DEV broken on newEmit? (reorder??)
    opPeeki = 79,       -- a = peek(b) where a is integer
-->>5)
-->> put 2 spare here
    -- 80,81 spare
--  opIres = 81,

    -- builtins which return an atom (to opNres):
    opAdd = 82,         -- a = b+c      -- NB b,c must be atoms (see sq_add etc)
    opAddi = 83,        -- a = b+c      --                      when a is integer
    opAddiii = 84,      -- a = b+c      --                      .. and b,c init ints        (:%e01tcfAddiii)
    opSub = 85,         -- a = b-c      -- ""
    opSubi = 86,        -- a = b-c                              -- ""
    opSubiii = 87,      -- a = b-c                                -- ""                     (:%e01tcfAddiii)
    opMul = 88,         -- a = b*c      -- ""
    opMuli = 89,        -- a = b*c                              -- ""
    opMuliii = 90,      -- a = b*c                                -- ""                     (:%e01tcfediMul)
    opDiv = 91,         -- a = b/c      -- ""
    opDivi = 92,        -- a = b/c                              -- ""
    opDiviii = 93,      -- a = b/c                                -- ""                     (:%e01tcfediDiv)
    opDiv2 = 94,        -- a = b/2              -- b is init int, a is atom or int
    opDivi2 = 95,       -- a = b/2              -- b is init int, a is integer
    opDivf = 96,        -- a = floor(b/c)       -- a,b are atom or int
    opDivf2 = 97,       -- a = floor(b/2)       -- b is init int, a is atom or int
                                                -- nb VMep[opDivf2] is e02atdb0 (not for newEmit)
    --
    -- opAdd is the general purpose opcode, a,b,c can be ints or atoms (see sq_add)
    -- opAddi is used when a is an integer, meaning no dealloc rqd, has built-in typecheck
    -- opAddiii is used when a is int, and b, c are known to be initialised integers (fastest of all)
    -- opAddiii is in fact always inlined, the VMep is an error handler (overflow), likewise +/*/div.  (newEmit)
    -- Ditto for Sub/Mul/Div, the latter has extra forms:
    -- opDiv2 is used when b is init integer (and obviously c=2). result (a) can be int or atom.
    -- opDivi2 is used when the result must be an integer, has built-in typecheck.
    -- opDivf is general purpose, a,b,c can be ints or atoms. (see sq_floor_div)
    --        (nb result may be atom, eg floor(1e308) is 1e308)
    -- opDivf2 is only used when b is an init integer, and result will therefore be an integer.
    --
--6)
-->> put 2 spare here
    -- 98,99,100 spare

--DEV no longer used:
--DEV opLen0?
--  oXpInc = 101,       -- a += 1               -- a must be init integer
-- 28/11/09 hijacked, see below:
--  oXpDec = 102,       -- a -= 1               -- ""
    -- if a is atom, or not known to be initialised, then opAdd/Sub a,a,1 is used instead of Inc/Dec.
    -- The VMep entries for opInc and opDec are in fact error handlers (overflow) [opInc/Dec no longer used]

-->>move me (this becomes spare):
--  oXpCatsi = 102,     -- p1 &= p2, when p1 is a gvar-scan-proven sequence of integer
        
    opFloor = 103,      -- a = floor(b)         -- NB b must be atom (see sq_floor)
        -- nb: while most times floor() yields an integer, eg floor(1e308) yields 1e308, ie an atom.
    opRmdr = 104,       -- a = remainder(b,c)   -- NB b,c must be atom|int (see sq_rmdr)
    opUminus = 105,     -- a = -b               -- ""                   (see sq_uminus)
    opAndBits = 106,    -- a = and_bits(b,c)    -- b,c must not exceed 32 bits
    opOrBits = 107,     -- a = or_bits(b,c)     -- ""
    opXorBits = 108,    -- a = xor_bits(b,c)    -- ""
    opNotBits = 109,    -- a = not_bits(b)      -- ""
    opPow = 110,        -- a = power(b,c)       -- NB b,c must be atoms (see sq_pow)
    opTime = 111,       -- a = time()
    opAlloc = 112,      -- a = allocate(b)
    opCos = 113,        -- a = cos(b)           -- b must be atom, see sq_cos etc
    opSin = 114,        -- a = sin(b)           -- ""
    opTan = 115,        -- a = tan(b)           -- ""
    opArcTan = 116,     -- a = arctan(b)        -- ""
    opLog = 117,        -- a = log(b)           -- ""
    opSqrt = 118,       -- a = sqrt(b)          -- ""
--  op32toA = 119,      -- a = float32_to_atom(b)
--  op64toA = 120,      -- a = float64_to_atom(b)
    opCallFunc = 119,   -- o = call_func(rid,params)
    opCallProc = 120,   -- call_proc(rid,params)
--DEV?
    opOpenDLL = 121,    -- a = open_dll(s)      -- (:%opOpenDLL in pcfunc.e) [DEV togo]
    opDcfunc = 122,     -- i = define_c_func(l,n,a,r) -- (:%opDcfunc "")
    opDcvar = 123,      -- i = define_c_var(l,n)      -- (:%opDcvar "")
    opInstance = 124,   -- a = instance()
--DEV?
    opCallback = 125,   -- a = call_back(id)          -- (:%opCallback "")
    opCallA = 126,      -- call(addr)
    opCfunc = 127,      -- a = c_func(rid,params)
    opCproc = 128,      -- c_proc(rid,params)
    opGpct = 129,       -- call :%opGpct (prev3/[edi] = fget_pcfunc_tables())
    opRpct = 130,       -- call :%opRpct (frestore_pcfunc_tables(prev3/[esi]))
    opCbHandler = 131,  -- call :%cbhandler (for dll use only)
    -- 126,127 spare
--  opNres = 127,

    -- builtins which return a string (to opSres):
--  opCurrDir = 128,    -- a = current_dir()
----DEV delete for newEBP (or rewrite)
--  opCatsi = 129,      -- p1 &= p2, when p1 is a gvar-scan-proven sequence of integer
    -- 130,131 spare
--  opSres = 131,

    opApnd = 132,       -- a = append(b,c)
    opPpnd = 133,       -- a = prepend(b,c)
    opConcat = 134,     -- a = b&c, see also opConcatN
--  opRepeat = 135,     -- a = repeat(b,c)
    opCurrDir = 136,    -- a = current_dir()
--DEV delete for newEBP (or rewrite)
    opCatsi = 137,      -- p1 &= p2, when p1 is a gvar-scan-proven sequence of integer

--  opAto32 = 136,      -- a = atom_to_float32(b)
--  opAto64 = 137,      -- a = atom_to_float64(b)
--  opDate = opCode("opDate",1),        -- a = date()   -- now in pdate.e
--DEV
--  oXpGSCh = 138,      -- a = get_screen_char(b,c)
    opGetRand = 138,    -- (opposite of opSetRand)
    opGetPos = 139,     -- s = get_position()   --DEV newEmit (now just a standard routine in pfileioN.e [BLUFF!])
    -- 140,141 spare
--  opPres = 141,

    -- builtins which return an object (to opOres):
--  opUpper = opCode("opUpper",2),      -- a = upper(b) -- now in pcase.e
--  opLower = opCode("opLower",2),      -- a = lower(b) -- ""
--DEV reorder as below...
    opPeek = 142,       -- a = peek(b)      -- (returns integer if b is atom, string if b is sequence)
    opPeek4s = 143,     -- a = peek4s(b)    -- (returns atom if b is atom, sequence if b is sequence)
    opPeek4u = 144,     -- a = peek4u(b)    -- (returns atom if b is atom, sequence if b is sequence)
    opGets = 145,       -- a = gets(b)      -- (returns string or -1, ie T_IS)
    opGetText = 146,    -- op-- -- 146 spare
--  opMoveFRes = 147,   -- a = get_function_result_for_routine_no(b)
    -- 147,148 spare
--DEV this should be named say lastObjRes, likewise things above
--  opOres = 148,

    -- builtin procedures:

--  opCallA = 150,      -- call(a)  NB opCall (paired with opFrame) is the normal/usual routine call
--DEV togo
--  opCallProc = 151,   -- call_proc(a,b)
--DEV reorder..
    opPokeN = 149,      --DEV newEmit opPokeN,addr,value,size
    opPoke1 = 150,
    opPoke2 = 151,
    opPoke8 = 152,
    opClrScrn = 153,    -- clear_screen()
    opFreeCons = 154,   -- free_console()
    opFree = 155,       -- free(a)
    opMemCopy = 156,    -- mem_copy(a,b,c)
    opMemSet = 157,     -- mem_set(a,b,c)
--  opPixel = opCode("opPixel",2),      -- pixel(a,b)   --DOS!
--DEV becomes opPoke1 for newEmit:
    opPoke = 158,       -- poke(a,b)
    opPoke4 = 159,      -- poke4(a,b)
    opPosition = 160,   -- position(a,b)
    opPuts = 161,       -- puts(a,b)
--DEV
--  oXpPSCh = 162,      -- put_screen_char(a,b,c)
--  opGetProcA = 162,   -- (see pcfunc.e)
    opFlush = 163,      -- flush(a)
    opClose = 164,      -- close(a)
    opSetRand = 165,    -- set_rand(a)
    opSleep = 166,      -- sleep(a)
    opTxtClr = 167,     -- text_color(a)
    opBkClr = 168,      -- bk_color(a)
    opTrace = 169,      -- trace(a)
    opProfile = 170,    -- profile(a)
    opProfout = 171,    -- profile_dump()

-- internals:

    opClrDbg = 172,     -- clear debug screen (notes below)
--  opTrap = 173,       -- temp (debug aid)
--DEV to go:
    opRTErn = 174,      -- internal, pdiag.e/eg pprntf.e. Trigger error by number.
--DEV to go(?):
    opGetST = 175,      -- internal, a=symtab (NB 32-bit!), b=flag/crashmsg, c=crashfile.
--opGetSP = 176,
--opSetSP = 177,
    opDelRtn = 176,     -- see %opDelRtn in pDeleteN.e
    opDelete = 177,     -- see %opDelete in pDeleteN.e
--  opGetVMep = 176,    -- internal, p1=VMep,p2=debugleak see pemit.e/t97mleak
-->>12)
--  opReadVM = 177,
--no longer used
--  opPokeRef = 178,    -- internal, [p2]=p1, see pemit.e
--DEV no longer used??
--  opGetRaw = 179,     -- internal, [p2]=raw(p1), (debug aid) see pemit.e/t97mleak
--newEmit
--  opLicence = 180,    -- internal, see pemit.e
--  opSetDbg = 181,     -- internal, see pexec.e/pdebug.e
--  opCrshRtn = 182,    -- internal, see pexec.e/pdiag.e
--newEmit
--  opRbldIds = 183,    -- internal, see pemit.e/calling convention below.
--  opCrshMsg = 184,    -- implements crash_message() [see pDiagN.e]
--DEV togo/newEmit:
--  opDelRtn = 185,     -- internal, see pdelete.e/calling covention below.
--  opCrshFile = 186,   -- implements crash_file()
--  opInterp = 187,     -- interpret(symtab,CSvaddr,DSvaddr,opstat,ptab,errorcode) [internal routine]
--  opCrshRtn = 188,    -- implements crash_routine(rid) [see pDiagN.e]
    opAbort = 178,      -- abort(a)
--newEmit
--  opCleanUp = 189,    -- internal, for debugleak check
--  opCleanUp1 = 190,   -- internal, for debugleak check
-->>13)
-->> (becomes 7 spare)
--newEmit
--  opGetRRC = 191,
--  opSetBatchMode = 192,
--DEV???
--newEmit
--  opLichk = 193,
--  opDelRef = 194,
--  opCrsh = 195,       -- implements crash()
--opInterp2 = 195,
--spare:
--  opSq0 = 196,
--DEV to go *2:
--opHeapAlloc = 196,
--  opStr0 = 197,
--opTlsGetValue = 197,
--  maxNVop = 197       -- max non-virtual opcode (checked against VM table in pemit.e)
--  maxNVop = 0     -- max non-virtual opcode (checked against VM table in pemit.e)
--$
    opLoadMint = 179,   -- edx:eax := int64(eax)
    opStoreMint = 180,  -- [edi] := eax, as float if rqd
--EXCEPT
    opTry = 181,
    opTryend = 182,
    opCatch = 183,
    opThrow = 184,

--DEV newEmit...
-- virtual opcodes (ie no actual code in the VM for these)
--                  (technically, opMovbi/opMovsi/opMovti/opAddiii/opSubiii/opMuliii/opDiviii
--                   are also kinda virtual, with VMep[i] being an error handler or somesuch.
--                   Also opObj should probably really be here rather than above, I think.)
--global constant
    opCall = 201,
    opFrst = 202,       -- "Frame ReSTore"; used between opFrame and opCall, where all
                        -- local vars must be retrieved from the old stack frame.
--NESTEDFUNC (DEV/SUG:)
--                      -- (opFrst is also used in nested functions (one level deep!?)
--opFstr = 209          -- "Frame Store"; used in nested functions...
--                      -- (may not be much use for subscripting etc...)
--<<opJcci? (above 8)
--
--  opMov0 = 166,       -- instead of incref, do decref(tgt), tgt=src, src=0.
--  opMov0nd = 167,     --      "" w/o the decref (as onDeclaration)
--  opMovnd = 168,      -- normal opMove, w/o decref (as onDeclaration)
--  opMovncd = 169,     -- as opMovnd but no need to cmp h4 (as src is init seq/str)
                        -- [DEV this may be better as just src is init, opMovndsi]
--  opMovsind = 170     -- as opMovsi [DEV do I mean opMovbi?]

    opJmp = 203,
    opJtyp = 204,
    opJbits = 205,
    opJlen = 206,
    opLabel = 207,      -- opLabel,mergeSet,0/x86loc,link
    opNop = 208,
--DEV unused:
--  opNopN = 209,
    opLoopTop = 210,    -- opLoopTop,lmask,gmask,end
    opEndFor = 211,
    opAsm = 212,        -- opAsm,len/loc,next,jlink
    opLn = 213,
    opGchk = 214,       -- implements #isginfo{} checks (no final code emitted)
    opLchk = 215,       -- implements #istype{} checks (no final code emitted)
    opCtrl = 216,       -- opCtrl,stmt,link,emitline
                        --  where stmt is one of IF/ELSIF/ELSE/END+IF/LOOP/END+LOOP
                        --  and link joins them all up, reverse-circle-wise.
                        --  emitline is for switch(/if) error reporting only

    opInit = 217,       -- initialise system/heap/stack/diag/etc
    opDealloc = 218,    -- used to link to :%pDealloc in VM\pHeap.e, should never actually occur in IL  [same for opFrame [DEV]]
--DEV to go*4: (when ilasm->ilASM completed)
--  opLoadMem = 217,    -- opLoadMem,reg,var (in ilasm only)
--  opLeaMov = 218,     -- opLeaMov,reg,var (in ilasm only)
--  opStoreMem = 219,   -- opStoreMem,reg,var (in ilasm only)
--  opFildMem = 220,    -- opFildMem,reg,var (in ilasm only)
--  opObort = 219,      -- test/temp
--219 spare
--  opRepCh = 220,

    opFor2 = 221,
    
--DEV not in use*2:
--  opPushEax = 222,    -- \ used in saveFunctionResultVars from paramList (only)
--  opPopVar = 223,     -- /   "   "                                "		 "
--DEV temp[?]
    opRunCleanup = 222,


    opLogPos = 224,     -- implements global labels (save the x86 offset)
--temp/test:
--  opMalloc = 225,     -- opMalloc,dest,size
--  opMfree  = 226,     -- opMfree,addr
--  opTestN  = 227,     -- opTestN,lblidx
--  opTestM  = 228,     -- opTestM,lblidx
    opPlatform = 225,   -- implements platform() [resolved in pmain.e]
    opMachine = 226,    -- implements machine_bits() [resolved in pmain.e]
    opDiv0 = 227,       -- mapped to :%e02atdb0 in VM\\pDiagN.e (fatal error)
--DEV reorder...
--  opPeek = 142,       -- a = peek(b)      -- (returns integer if b is atom, string if b is sequence)
--  opPeek4s = 143,     -- a = peek4s(b)    -- (returns atom if b is atom, sequence if b is sequence)
--  opPeek4u = 144,     -- a = peek4u(b)    -- (returns atom if b is atom, sequence if b is sequence)
    opPeek1s = 228,     -- \
    opPeek1u = 229,     -- \\
    opPeek2s = 230,     --   } all done via :%opPeekNx, 
    opPeek2u = 231,     --   }  in builtins\VM\pMem.e
    opPeek8s = 232,     -- //
    opPeek8u = 233,     -- /
    opPeekNS = 234,     --/

    opInitCS = 235,     -- opInitCS,dest
    opDeleteCS = 236,   -- opDeleteCS,src           [DEV ,init]
    opEnterCS = 237,    -- opEnterCS,src(/T_const0) [,init]
    opTryCS = 238,      -- opTryCS,dest,src         [,init]
    opLeaveCS = 239,    -- opLeaveCS,src(/T_const0) [,init]
    -- (the last 4 currently rely on opUnassigned, but will accept varno in esi, if that helps any)

    opCrashMsg = 240,   -- opCrashMsg,cm
--  opCrash = 241,      -- opCrash,fmt,data
--  opCrash1 = 242,     -- opCrash,fmt
    opCrashFile = 242,  -- opCrashFile,file_path
    opCrashRtn = 243,   -- opCrashRtn,rid

    opWrap = 244,       -- opWrap,flag
    opScroll = 245,     -- opScroll,amount,top,bottom
    opTextRows = 246,   -- opTextRows,res,lines
    opVersion = 247,    -- implements version() [resolved in pmain.e]
--??
--opDcfp = 247,         -- define_c_func/proc
    maxVop = 247

    opNames = repeat(0,maxVop)
    opSkip = repeat(-20000,maxVop)  -- instruction lengths (mostly)
    sqAble = repeat(0,maxVop)       -- eg sqAble[opXor] -> "sq_xor_bits"
    opEfct = repeat(0,maxVop)       -- does opcode have side effects?
    -- these next three should remain 0 for <=T_Bin and >T_Asm:
    aatidx = repeat(0,maxVop)       -- ttidx of glabels
    aasydx = repeat(0,maxVop)       -- corresponding symtab index
    aartypes = repeat(0,maxVop)     -- return types

    opName("opMove",opMove,6)
    opName("opMovbi",opMovbi,4)
    opName("opMovsi",opMovsi,5)
    opName("opMovti",opMovti,4)
--  opName("opStat",opStat,0)   -- DEV??
    opUsed += 1
    opName("opLnt",opLnt,2)
    opName("opLnp",opLnp,2)
    opName("opLnpt",opLnpt,2)
    opName("opTchk",opTchk,4)           -- opTchk,varno,wasOptTypeCheck,default
    opName("opTcFail",opTcFail,0)
    opName("opBadRetf",opBadRetf,0)
    opName("opUnassigned",opUnassigned,2)
--  opName("opCallOnceYeNot",opCallOnceYeNot,0)
--  opName("opAllocStr",opAllocStr,2)   -- opAllocStr,res,len (only called from ilASM, so far)
--  opUsed += 4 -- spare/opCallOnceYeNot?
    opUsed += 1 -- spare/opCallOnceYeNot?
    opName("opDeSeq",opDeSeq,2)
--  opName("opWithJS",opWithJS,1)
    opUsed += 2 -- spare

--  opName("opFor",opFor,6)             -- opFor,init,ctl,step,limit,tgt/x86loc (see notes below)
                                        -- (29/12/2011: "end" on the opLoopTop should now be used
                                        --              in place of tgt, since that gets clobbered
                                        --              with x86loc on the last pass.)
    opName("opFrame",opFrame,0)         -- opFrame,routineNo (0==handled separately)
    opName("opCallOnce",opCallOnce,2)
    opName("opRetf",opRetf,0)
    opName("opMkSq",opMkSq,-3)
    opName("opMkSqi",opMkSqi,-3)
    opName("opRepe",opRepe,-4)
    opName("opReps",opReps,-5)
    opName("opSubse",opSubse,-4)
    opName("opSubss",opSubss,-5)
    opName("opConcatN",opConcatN,-3)
    opName("opRepe1",opRepe1,4)
    opName("opRepe1ip",opRepe1ip,0) --??
    opName("opRepe1is",opRepe1is,0) --??
--  opUsed += 1 -- spare
    opName("opSubsss",opSubsss,0) --??
    opUsed += 2 -- spare
    opName("opSubse1",opSubse1,6)
    opName("opSubse1i",opSubse1i,6)
    opName("opSubse1is",opSubse1is,6)
    opName("opSubse1ip",opSubse1ip,0)   --??            -- ""
    opUsed += 1 -- spare

    opName("opJif",opJif,7)
    opName("opJnot",opJnot,7)
    opName("opJge",opJge,8)
    opName("opJlt",opJlt,8)
    opName("opJeq",opJeq,8)
    opName("opJne",opJne,8)
    opName("opJgt",opJgt,8)
    opName("opJle",opJle,8)
--  opName("opJifx",opJifx,7)   -- ??
    opUsed += 1 -- spare
    opName("opJnotx",opJnotx,7)
    opName("opSge",opSge,7)
    opName("opSlt",opSlt,7)
    opName("opSeq",opSeq,7)
    opName("opSne",opSne,7)
    opName("opSgt",opSgt,7)
    opName("opSle",opSle,7)
    opName("opScmp",opScmp,4)
--  opName("opFind",opFind,5)
--  opName("opMatch",opMatch,5)
    opUsed += 2 -- spare
    opName("opXor",opXor,4)
    opName("opInt",opInt,4)
    opName("opAtom",opAtom,4)
    opName("opSq",opSq,4)
    opName("opStr",opStr,4)
    opName("opObj",opObj,4)     --DEV??
--  opUsed += 4 -- (reserved for opInt0 etc)

-- sub-opcodes for opJtyp:
--  (these exist in the backend, and "-list" will show calls to them,
--   but they do not occur in the intermediate code, but are instead
--   generated off non-init opJtyp il opcodes)
    opName("opInt0",opInt0,0)
    opName("opAtom0",opAtom0,0)
    opName("opSq0",opSq0,0)
    opName("opStr0",opStr0,0)

    opName("opLen",opLen,5)
    opName("opNot",opNot,3)
    opName("opOpen",opOpen,4)
    opName("opGetc",opGetc,3)           -- opGetc,res,fn
    opName("opGetKey",opGetKey,2)
    opName("opWaitKey",opWaitKey,2)
    opName("opSeek",opSeek,4)
    opName("opWhere",opWhere,4)
    opName("opLock",opLock,5)
    opName("opUnLock",opUnLock,3)
    opName("opRand",opRand,4)
--  opName("opChDir",opChDir,4)
    opUsed += 1 -- spare
    opName("opPeeki",opPeeki,4)
    opUsed += 2 -- spare
    opName("opAdd",opAdd,4)
    opName("opAddi",opAddi,4)
    opName("opAddiii",opAddiii,4)
    opName("opSub",opSub,4)
    opName("opSubi",opSubi,4)
    opName("opSubiii",opSubiii,4)
    opName("opMul",opMul,4)
    opName("opMuli",opMuli,4)
    opName("opMuliii",opMuliii,4)
    opName("opDiv",opDiv,4)
    opName("opDivi",opDivi,4)
    opName("opDiviii",opDiviii,4)
    opName("opDiv2",opDiv2,3)
    opName("opDivi2",opDivi2,3)
    opName("opDivf",opDivf,4)
    opName("opDivf2",opDivf2,4)
--  opUsed += 3 -- spare
    opUsed += 5 -- spare
--  opName("oXpInc",oXpInc,2)           --DEV no longer used
--  opName("oXpDec",oXpDec,2)           -- ""
--opName("oXpCatsi",oXpCatsi,2)
    opName("opFloor",opFloor,3)
    opName("opRmdr",opRmdr,5)
    opName("opUminus",opUminus,3)
    opName("opAndBits",opAndBits,5)
    opName("opOrBits",opOrBits,5)
    opName("opXorBits",opXorBits,5)
    opName("opNotBits",opNotBits,4)
    opName("opPow",opPow,5)
    opName("opTime",opTime,2)
    opName("opAlloc",opAlloc,4)
    opName("opCos",opCos,3)
    opName("opSin",opSin,3)
    opName("opTan",opTan,3)
    opName("opArcTan",opArcTan,3)
    opName("opLog",opLog,3)
    opName("opSqrt",opSqrt,3)
--  opName("op32toA",op32toA,4)
--  opName("op64toA",op64toA,4)
--  opUsed += 2 -- spare
    opName("opCallFunc",opCallFunc,4)   -- opCallFunc,res,rid,params
    opName("opCallProc",opCallProc,3)   -- opCallProc,rid,params
--DEV?
    opName("opOpenDLL",opOpenDLL,3)
    opName("opDcfunc",opDcfunc,6)       -- opDcfunc,res,lib,name,args,rtyp
    opName("opDcvar",opDcvar,4)         -- opDcvar,res,lib,name
    opName("opInstance",opInstance,2)
    opName("opCallback",opCallback,3)   -- opCallback,res,id
--  opUsed += 4 -- spare
--  opName("opCurrDir",opCurrDir,2)
--  opName("opCatsi",opCatsi,2)
--  opUsed += 2 -- spare
    opName("opCallA",opCallA,2)
    opName("opCfunc",opCfunc,4)
    opName("opCproc",opCproc,3)
    opName("opGpct",opGpct,0)       -- no opSkip/should not occur in il/#ilasm only
    opName("opRpct",opRpct,0)       -- no opSkip/should not occur in il/#ilasm only
    opUsed += 1 -- spare
    opName("opApnd",opApnd,4)
    opName("opPpnd",opPpnd,4)
    opName("opConcat",opConcat,4)
--  opName("opRepeat",opRepeat,4)
    opUsed += 1
--  opName("opAto32",opAto32,3)
--  opName("opAto64",opAto64,3)
--  opUsed += 2 -- spare
    opName("opCurrDir",opCurrDir,2)
    opName("opCatsi",opCatsi,2)
--  opName("opGSCh",opGSCh,0)       -- ??!!! [DEV]
    opName("opGetRand",opGetRand,0) -- no opSkip/should not occur in il/#ilasm only
    opName("opGetPos",opGetPos,2)
    opUsed += 2 -- spare
    opName("opPeek",opPeek,3)
    opName("opPeek4s",opPeek4s,3)
    opName("opPeek4u",opPeek4u,3)
    opName("opGets",opGets,4)
    opName("opGetText",opGetText,4) -- opGetText,res,fn,option
--  opName("opMoveFRes",opMoveFRes,3)
    opUsed += 2 -- spare
--  opUsed += 3 -- spare
--  opName("opPokeN",opPokeN,5)     -- opPokeN,Nflag,base,offset,value
    opName("opPokeN",opPokeN,4)     -- opPokeN,addr,value,size
--DEV togo
--  opName("opCallA",opCallA,2)
--  opName("opCallProc",opCallProc,3)
--  opUsed += 3 -- spare
    opName("opPoke1",opPoke1,3)
    opName("opPoke2",opPoke2,3)
    opName("opPoke8",opPoke8,3)
    opName("opClrScrn",opClrScrn,1)
    opName("opFreeCons",opFreeCons,1)
    opName("opFree",opFree,2)
    opName("opMemCopy",opMemCopy,4)
    opName("opMemSet",opMemSet,4)
    opName("opPoke",opPoke,3)
    opName("opPoke4",opPoke4,3)
    opName("opPosition",opPosition,3)
    opName("opPuts",opPuts,3)
--  opName("oXpPSCh",oXpPSCh,0)         -- ??!!!! [DEV]
--  opName("opGetProcA",opGetProcA,0)       -- (#ilasm only, see pcfunc.e) [to go?]
    opUsed += 1
    opName("opFlush",opFlush,2)
    opName("opClose",opClose,2)
    opName("opSetRand",opSetRand,2)
    opName("opSleep",opSleep,2)
    opName("opTxtClr",opTxtClr,2)
    opName("opBkClr",opBkClr,2)
    opName("opTrace",opTrace,2)
    opName("opProfile",opProfile,2)
--  opUsed += 1 -- spare
    opName("opProfout",opProfout,1)
    opName("opClrDbg",opClrDbg,0)
--  opName("opTrap",opTrap,0)   -- ??   -- untested/ buried in #ilasm block and hence
    opUsed += 1
    opName("opRTErn",opRTErn,0) -- ??   --  there is no need for opSkip handling.
--DEV to go:
    opName("opGetST",opGetST,0) -- ??                   -- ""
--if newEmit then
--  opName("opGetSP",opGetSP,0) -- ??                   -- ""
--  opName("opSetSP",opSetSP,0) -- ??                   -- ""
--else
--  opUsed += 2
    opName("opDelRtn",opDelRtn,4) -- ??                 -- ""
    opName("opDelete",opDelete,2) -- ??                 -- ""
    opName("opAbort",opAbort,2)
--  opName("opGetVMep",opGetVMep,0) --??
--  opName("opReadVM",opReadVM,0) --??
--end if
--  opName("opPokeRef",opPokeRef,0) -- ??
--  opName("opGetRaw",opGetRaw,0)   --??
    opUsed += 2
--  opUsed += 20
--DEV::
--EXCEPT
    opName("opTry",opTry,4)             -- opTry,tmp,tgt,esp4
    opName("opTryend",opTryend,5)       -- opTryend,mergeSet(0),tgt,link,tlnk
    opName("opCatch",opCatch,3)         -- opCatch,tlnk,e
    opName("opThrow",opThrow,3)         -- opThrow,e,user

    opUsed += 16
--  opName("opLicence",opLicence,0) -- (#ilASM only, see pemit.e)
--  opName("opSetDbg",opSetDbg,0)   --??
--  opName("opCrshRtn",opCrshRtn,0) --??
--  opName("opRbldIds",opRbldIds,0) --??
--  opName("opCrshMsg",opCrshMsg,2)
--  opUsed += 2
--  opName("opDelRtn",opDelRtn,0)   --??
--  opName("opCrshFile",opCrshFile,2)
--  opUsed += 2
--  opName("opInterp",opInterp,0)   --??
--  opName("opAbort",opAbort,2)
--  opUsed += 3
--  opName("opCleanUp",opCleanUp,0) --??
--  opName("opCleanUp1",opCleanUp1,0)   --??

--  opUsed += 7
--  opName("opGetRRC",opGetRRC,0)   -- (#ilASM only, see pgui.exw)
--  opName("opSetBatchMode",opSetBatchMode,0)
--  opName("opLichk",opLichk,0) -- (#ilASM only, TEMP!)
--  opName("opDelRef",opDelRef,0)
--  opName("opCrsh",opCrsh,0)
--opName("opInterp2",opInterp2,0)
--opName("opHeapAlloc",opHeapAlloc,0) -- (#ilASM only) [TEMP, to go]
--opName("opTlsGetValue",opTlsGetValue,0) -- (#ilASM only) [TEMP, to go]

--  opUsed += 4
--  opUsed += 2
--  opUsed += 3


-- virtual opcodes:

    opName("opCall",opCall,1)       -- opCall (routine to call is left on stack by opFrame)
    opName("opFrst",opFrst,5)
    opName("opJmp",opJmp,4)         -- opJmp,mergeSet,tgt,link\n"
    opName("opJtyp",opJtyp,10)
    opName("opJbits",opJbits,8)
    opName("opJlen",opJlen,8)
    opName("opLabel",opLabel,4)
    opName("opNop",opNop,1)
--  opName("opNopN",opNopN,-1)
    opUsed += 1
    opName("opLoopTop",opLoopTop,4) -- opLoopTop,lmask,gmask,end
    opName("opEndFor",opEndFor,3)   -- opEndFor,ctnr,bpFor
    opName("opAsm",opAsm,-4)
    opName("opLn",opLn,2)
    opName("opGchk",opGchk,10)      -- opGchk,N,typ,min,max,etyp,len,tokline,tokcol,fileno
    opName("opLchk",opLchk,6)       -- opLchk,N,typ,tokline,tokcol,fileno
    opName("opCtrl",opCtrl,4)       -- opCtrl,stmt,link[,emitline] (see ltCtrl in pltype.e)
                                    -- (emitline is for switch(/if) error reporting only)
    opName("opInit",opInit,1)
--  opName("opDealloc",opDealloc,1) -- used to link to :%pDealloc in VM\pHeap.e, should never actually occur in IL
    opUsed += 2
--  opName("opObort",opObort,2)     -- opObort,N
--  opName("opRepCh",opRepCh,4)     -- opRepCh,dest,item,count
    opUsed += 1
--DEV to go?
--  opName("opLoadMem",opLoadMem,5) -- opLoadMem,reg,var (see loadMem in pilx86.e)
--  opName("opLeaMov",opLeaMov,5)   -- opLeaMov,reg,var (see leamov in pilx86.e)
--  opName("opStoreMem",opStoreMem,5) -- opStoreMem,reg,var (see storeMem in pilx86.e)
--  opName("opFildMem",opFildMem,5) -- opFildMem,reg,var (see fildMem in pilx86.e)
--  opUsed += 3
    opName("opFor2",opFor2,7)       -- opFor,flags,init,ctl,step,limit,tgt/x86loc

--DEV (spotted in passing) Are these used? 'cos they cannot possibly be right...
--  opName("opPushEax",opPushEax,0) -- opPushEax
--  opName("opPopVar",opPopVar,1)   -- opPopVar,N
--  opUsed += 2
    opName("opRunCleanup",opRunCleanup,0)
    opUsed += 1

    opName("opLogPos",opLogPos,2)   -- opLogPos,N
--DEV (test)
--  opName("opMalloc",opMalloc,3)   -- opMalloc,dest,size
--  opName("opMfree",opMfree,2)     -- opMfree,addr
--  opName("opTestN",opTestN,2)     -- opTestN,lblidx
--  opName("opTestM",opTestM,2)     -- opTestM,lblidx
    opName("opPlatform",opPlatform,0)
    opName("opMachine",opMachine,0)
    opName("opDiv0",opDiv0,0)
    opName("opPeek1s",opPeek1s,3)   -- opPeek1s,res,addr
    opName("opPeek1u",opPeek1u,3)
    opName("opPeek2s",opPeek2s,3)
    opName("opPeek2u",opPeek2u,3)
    opName("opPeek8s",opPeek8s,3)
    opName("opPeek8u",opPeek8u,3)
    opName("opPeekNS",opPeekNS,5)   -- opPeekNS,res,addr,size,sign

    opName("opInitCS",opInitCS,2)
    opName("opDeleteCS",opDeleteCS,2)
    opName("opEnterCS",opEnterCS,2)
    opName("opTryCS",opTryCS,3)
    opName("opLeaveCS",opLeaveCS,2)

    opName("opCrashMsg",opCrashMsg,2)
--  opName("opCrash",opCrash,3)
--DEV??
--  opName("opCrash1",opCrash1,2)
    opUsed += 1
    opName("opCrashFile",opCrashFile,2)
    opName("opCrashRtn",opCrashRtn,2)

    opName("opWrap",opWrap,2)
    opName("opScroll",opScroll,4)
    opName("opTextRows",opTextRows,3)
    opName("opVersion",opVersion,0)


--DEV move this to the help text, and fill in any gaps

--  The anonymous label (@@:) can be used with @f (forward) and @b (backward) jumps:
--
--      cmp eax,h4
--      jle @f
--          add dword[ebx+eax*4-12],1   -- incref
--    @@:
--      lodsd
--      cmp al,'\n'
--      jne @b
--
--  The point, of course, is to reduce the burden of having to devise unique names.
--  The @f and @b simply locate the next @@: in the specified direction; there is
--  no way to specify 2 or more away, for that use a proper (alphanumeric) label.
--
--  While labels in top-level #ilASM statements only have to be unique within that
--  one #ilASM statement, all labels in all #ilASM residing in the same routine
--  must be uniquely named, otherwise a compilation error occurs much as you might
--  expect should you try to declare "integer k" twice in that self same routine.
--
--  Generally speaking, the anonymous label is best suited for short inner jumps 
--  and should be avoided for important control points, such as "::looptop", or 
--  anything which spans over more than one screenful. Be advised that there is no
--  error or warning should an anonymous label not be referenced, but of course a
--  fatal compilation error occurs for both @f and @b with no suitable target.
--

--DEV complete rewrite, include above note
--  Introduction to inline assembly.
--  ================================
--    The Phix compiler supports inline assembly using the #ilASM{} construct, eg:
--      #ilASM{ mov eax,[filevar]               -- mov eax,[mem32]
--              mov edx,[localvar]              -- mov edx,[ebp-NN]
--              cmp eax,-1
--              jne :label
--                mov [filevar],edx
--            ::label }
--      Supported mnemonics are defined in pttree.e; the assembler is contained in pilasm.e.
--       Note the assembler is designed to be easily extended ("add what you need") rather
--       that ever reach "complete" status. There are dozens of "?9/0 -- placeholder for 
--       more code" statements in pilasm.e but in most cases you should be able to crib from
--       something similar elsewhere in that source file. For instance I know little of SSE
--       (let alone 2/3/4) so see little reason to add reams of code that I am both unlikely
--       to use and have no way of adequately testing.
--      Labels can be defined at the start of a statement by specifying a double :: followed 
--       by the label name. Labels are always created using a double :: but referred/jumped 
--       to using a single : (technically optional, see below).
--      Naturally, ilASM is pretty dumb: just as you cannot eg mov eax,[ecx]-edi, ilASM
--       constructs do not accept complex [or even simple] sub-expressions. Of course,
--       were you to allow mov eax,s[i] but getting s[i] trashed all registers (as it
--       tends to) then very soon it would all get rather confusing to say the least.
--       Of course the inline assembler helps out where it can, for example mov al,8+4+1
--       is perfectly valid and equivalent to mov al,13 (but with much clearer intent).
--      You cannot define asm workspace but must instead use hll variables (integers) or
--       allocate space on the heap or stack manually. Obviously you must not leave any
--       hll variable >#40000000 (unless it is a pukka Phix reference) when exiting from 
--       a routine or ending the application. A good trick to know is storing a Phix ref
--       in "/4" form, for an example see iThis in pfileioN.e (read the section entitled
--       "Technical notes")
--      The largest existing example of #ilASM can currently be found in pfileioN.e. --DEV builtins\VM
--
--UPDATE: #ilASM, technically still a lang-within-lang, I find perfectly acceptable...
-- I accept that ilasm is a language-within-a-language; a concept I generally find rather
--  obscene, however it seems a necessary evil, and hopefully one most users will avoid.
--  Perhaps in time it can be eliminated, but right now I (really) cannot even begin to 
--  imagine how (especially, in order, pHeap.e, pStack.e, pFEH.e, pDiag.e, ...)
--
--DEV old, more things (global labels, etc) need to be explained...
--      TIP: instructions need no prefix as #ilasm is expecting them, whereas labels 
--       need a : (or two), and directives/opCodes/etc need a %. However as you can 
--       see hll vars need no prefix (since they are "in scope"). These rules mean
--       that you could have the label ::eax, the iload-defined %eax, and a hll var
--       named eax, all without any confusion (to the compiler that is).
--
--  You should also note that #ilASM constructs can interfere with compiler
--  optimisations - specifically knowing a variable type/value, or the fact
--  that we currently have a live copy of it in register xxx. The compiler 
--  always assumes #ilASM blocks, no matter how trivial thay actually are, 
--  potentially trash all registers and/or modify all variables. Hence in 
--  some rare cases #ilASM may actually slow things down, cause additional 
--  loads, and occasionally extra type checking. 
--
-- Gotcha: what is wrong with this?
--
--  global function getc(integer fn)
--  integer ch -- result
--      ...
--      ch = -1
--      ...
--      #ilASM {...
--              mov [ch],ecx
--              ...}
--      return ch
--  end function
--  ...
--          if getc(fn)=-1 then exit end if
--  ...
--
--  The compiler "does its thing" and determines that ch is only ever set
--  to -1, hence the result of getc is always -1, hence always exits. The
--  "fix" is to add a "ch = 255" somewhere in getc(). I cannot think of a
--  practical way for ilASM() to affect ilxlate [isGscan=0] in pilx86.e; 
--UPDATE: DUH/DEV You need a flag on symtab[N][S_State], akin to K_aod/
--                K_noclr/K_rtn/K_Fres/K_ridt/K_dlft/?... which means
--                "this var touched by ilASM: do NOT propagate values".
--                (and analyse which asm ins. modify var, in pilasm.e)
--                (which is probably a get_operand(bool first) thing,
--                 or maybe we can restrict it to P1TYPE=MEMORY(?).]
--                Oh, lea reg,[hll] should assume the worst, period.
--                (if you are setting, or just examining, a hll var
--                 in asm, it is reasonable to assume there is little
--                 or no performance-critical hll code that uses it.)
--                Note that modifying a hll var in #ilASM (and with 
--                 lea reg,[hll] assuming the worst) prevents compiler 
--                 optimisations such as type and value propagation on 
--                 that variable. Albeit unlikely, it is theoretically 
--                 possible to slow the whole thing down by speeding 
--                 one bit up.
--  adding opLset, a "set" version of opLchk, is a possibility, but not 
--  really an improvement on the "ch = 255" solution. At least for now, 
--  you must carefully inspect listing files of all #ilasm you write.
--  In other words, avoid #ilASM unless you really really really need it.
--
--DEV re-check this handling, I think it is now fixed.
-- Another quirk: if you code mov [hllvar],ax then the hll heritage of the inline
--  assembler will kick in, see "[hllvar]" and go "dword" (or "qword" on 64-bit),
--  then either (if you are lucky) emit a compiler error because ax is the wrong
--  size, or simply get it wrong. You need to code mov word[hllvar],ax instead.
--  (Such 16-bit code is required for FPU control words, eg see VM\pFPU.e.)
--  (Feel free to rummage around in pilasm.e changing all integer size to a pair
--   of {explicit,inferred}, but for my money it simply ain't worth the effort.)
--
--
--DEV add to docs (Internals/ilASM/goto):
-- A brief note from the author about goto.
-- ========================================
--  The subject of many a holy war, perhaps the most compelling argument against
--  goto is that were it not pure evil we would all still be coding in assembly.
--  High level constructs, such as if, for, while, procedure, and so on, along
--  with sensible indentation, clearly make code far easier to read/understand.
--
--  There is no shame in using a sensibly-named flag to control processing, and 
--  in many cases that additional sensible name also improves code legibility.
--
--  There is no hll goto statement in Phix. However goto/jmp is unavoidable in
--  assembly programming, and of course Phix supports inline assembly, hence in 
--  the very rare cases a goto is genuinely needed in hll code (I have seen just
--  /three/ in the last two decades), the following construct(s) may be used:
--
--          #ilASM{ jmp :meaningful_label }
--          ...
--          #ilASM{ ::meaningful_label }
--
--  In top level code, label scope is restricted to a single ilASM construct, 
--  but within a routine, the scope is across all the ilASM in that routine. 
--
--  There is quite deliberately no support for jumping from the middle of one 
--  routine into another: without a frame, then quite simply parameters and 
--  local variables have not been allocated and cannot be used/referenced.
--  (See also global labels discussed below.)
--
--  Making "goto" somewhat more difficult to type in this manner ensures that
--  it is far less likely to be abused, and discourages newbie programmers 
--  from adopting it as a weapon of choice, as usually(/always) happens with 
--  a hll goto.
--
--  Ultimately, and paradoxically, the real problem with goto is that as soon
--  as they get daisy-chained together, something will inevitably end up in 
--  the wrong place, usually with no clue as to how or why they got there.
--  Minor savings when writing code should of course always be balanced against
--  potential wasted time trying to understand or debug it at some later date.
--  Meaningless label names (eg "label4") make code substantially harder to 
--  understand. Error handling is much better and easier with normal hll code 
--  such as "return error(..)" or "error(..); return" - not only will any 
--  diagnostics have a clear "called from", but you can pass any required 
--  arguments, have just one error handler which serves several different 
--  routines, and very easily write unit tests for it - which you simply 
--  cannot do should it be embedded as a label inside something else.
--  It is plain wrong to use more than one "goto" a year in hll code, the
--  only real justification is when you find an algorithm which works and
--  there is no simple way to replace jumps with proper hll constructs, or
--  (extremely rarely) when doing so significantly impacts performance.
--
--DEV test this: (and rewrite) [I think labels have their own namespace now]
--  Gotcha: what is wrong with this?
--
--          #ilASM{ jmp :ret }
--          ...
--          #ilASM{ ::ret }
--
--  Well, ret is already a valid id, as set by iload("ret",#C3) in pttree.e;
--  which will make things go horribly wrong. I can also imagine isVar etc
--  being accidentally misused but such clashes should be fairly rare. I did
--  hit a problem in builtins/pfileio2.e trying to define :%opGetc when it 
--  had already been iload'd from opNames. It is of course the programmer's 
--  responsibility to avoid/resolve such things.
--

-- Invoking hll code from #ilASM
--  The easiest/safest way to do this is to keep such code near the end of
--  file, to avoid accidentally messing things up, eg:
--
--          #ilASM{ jmp :%pRetf     -- (there's one of these at every eof)
--                  :%somelabel
--                }
--          puts(1,"hello from somelabel\n")  -- [keep this short/simple]
--          #ilASM{ ret }
--          <EOF>
--
--  Obviously any hll code between the ret and EOF would not be executed,
--  and it might prove very distressing if (say) there is a constant which
--  gets left set to <NOVALUE> at run-time. Also obviously, I hope, I have
--  made somelabel global otherwise there would be no way to invoke it. You
--  can achieve a similar effect inside a routine using local label(s), but
--  of course there would be no way to invoke that code from outside of the
--  routine where it is defined (any you shouldn't want to try, since there
--  would be no stack frame, so any parameters/local variables would contain
--  garbage and modifying them would cause instant memory corruption).
--

--opFor: (virtual opcode)
--  This is somewhat trickier than you might at first suspect.
--  The trivial case, eg
--      for i=1 to 4 do
--  should just generate
--      mov [i],1
--  Slightly more complex, we have
--      for i=1 to length(s) do
--  which must obtain the length of s, check it is >=1, and set i to 1.
--  At the most extreme, we have
--      object ctrl, start, limit, step
--      ...
--      for ctrl=start to limit by step do
--  Which must check start/limit/step for unassigned/31-bit integer, and may
--  need to properly decref the previous contents of the ctrl var. We also
--  make private copies of limit/step to prevent any unwanted side effects.
--  More significantly, step>=1 terminates when ctrl>limit whereas step<=-1
--  terminates when ctrl<limit, and obviously if we do not know the sign
--  of step at compilation time, we need code for both, that is in the
--  check for zero iterations, as well as in opEndFor, and must generate a 
--  run-time check that step is not zero. It can all get really messy, but 
--  is justified because the majority of loops emit quite trivial/elegant 
--  code, especially at the start of the loop.
--

--opAsm: (design notes)
--  This is a virtual "wrapper" for #ilasm statements; in effect it states to the 
--  backend "here are N bytes of x86 code to be copied verbatim". However there is
--  a catch: if a jmp/label are in the same #ilasm, the offset could be calculated 
--  in the front end, however jumps between #ilasm constructs need to be fixed up 
--  once the length of any x86 code generated for the intervening hll is known. In
--  practice we always go the latter route and fixup all offsets only once we know
--  the start/length of all x86 blocks. opAsm has three header elements:
--
--      1: length of opAsm block, which is set in pmain.e ;later used for location,
--          (s5/x86) which is set in pilx86.e
--      2: next opAsm block (code/il idx, set/only used in pilx86.e)
--      3: first jump needing fixup in this block (code/il offset)
--
--  Each jump is created from eg #ilASM{ ... jmp :label ...}; four bytes (%isJmp,
--  0,0,offset) are reserved, which are automagically converted to short form if 
--  -128<=offset and offset<=127 otherwise the %isJmp and the two zeros are 
--  overwritten with the offset spilt into bytes (see scanforShortJmp() in 
--  pemit.e). In practice we use these four slots to (also) cope with jumps 
--  between blocks, as follows:
--
--      1: %isJmp
--      2: next jump needing fixup in this block (0-terminated chain)
--      3: relevant opAsm (code/il idx)
--      4: code/il index -> s5/x86 offset
--
--  Each call to ilxlate() in pilx86.e creates s5/x86, and copies any opAsm 
--  blocks verbatim. The main (il opcode) loop both tags each opAsm with the
--  actual s5/x86 location (overwriting length) and builds a chain of opAsm 
--  for processing in a final loop. Since we know both the start offsets of 
--  the blocks containing the jmp and target and can readily calculate the
--  offset within those blocks, converting the code/il index to the required 
--  s5/x86 offset is then relatively trivial (ie about half a dozen +/-'s).
--
--  Aside: Jumps/labels in #ilASM are completely different to those from hll
--         code since we have no need/desire to track register contents; the
--         decision over which registers to use is entirely under programmer
--         control.
--BLUFF: you can now use global labels! (test: but not inside routines!) [DEV]
--  Aside2: Jumps between #ilASM statements are not permitted in top-level 
--          code primarily because of scope issues, eg:
--              #ilASM{ jmp :fred }
--              procedure x()
--                  #ilASM{ jmp :fred }
--                  #ilASM{ ::fred }
--              end procedure
--              #ilASM{ ::fred }
--          Let T_fred be the ttidx (ternary tree index) of "fred". We do not
--          want to save/restore tt[T_fred+EQ] over routine definitions /
--          nested (out-of-scope) #ilASM constructs / include statements, as
--          we would in the above were the outer two fred permitted/expected 
--          to link up.
--          (I sincerely hope no-one thinks #ilASM labels should be global!)    [DEV]
--          (I also hope no-one thinks they should be allowed to jump out of
--           x() to the top-level ::fred - though as mentioned above it is 
--           theoretically possible, just don't expect me to show you how,
--           and don't expect me to care if you screw up your frame stack.)
--          Restricting jumps between blocks to within a single routine is 
--          not only sensible but also *much* easier to implement.
--
--  Although I have used x86 freely in the this description, there is nothing
--  above which would hinder code generation for other processors.
--

-- Global Labels: (design notes)
-- =============================
--  These were added to enable the replacement of (closed source) asm back end 
--  code with (open source) hll/#ilASM. In particular, tests showed that getc() 
--  would suffer badly with an opFrame/opCall overhead, besides it might prove 
--  rather difficult to ever implement opFrame/opCall via themselves!
--
--  The "brief note about goto" above explained the use of local labels, which
--  are deliberately rather limited due to the opFrame requirement. It is also
--  possible to declare global labels, which are superficially similar however 
--  quite different internally, and have a different set of restrictions:
--
--          #ilASM{ call :%unique_label }
--          ...
--          #ilASM{ jmp :skip
--                 :%unique_label
--                  ret
--                 ::skip }
--
--  Global labels cannot be declared inside a routine, since without an opFrame 
--  there are no parameters or local variables, and as shown (almost always) 
--  require a skip construct. It is up to the programmer to ensure global labels 
--  are unique across the entire application, including any and all third party
--  sources, and as mentioned above anything already set in pttree.e/iload(). 
--  Note that global labels are both declared and referenced with ":%". 
-- 
--  Whereas local labels are resolved using various offsets and are automatically
--  converted into short form when the offset fits in a byte, global labels are
--  resolved as absolute addresses. Since they are intended to be used with call, 
--  which has no short form, no such similar packing is attempted.
--
--  To implement global labels I added several new tables to pglobals.e (glblused etc)
--  and opLogPos to pops.e. The latter records the x86 offset in pilx86.e, which is
--  adjusted at the end of scanforShortJmp() in pemit.e (and used in blurph()).
--
--  Since global labels were implemented for the specific purpose of migrating back
--  end code, use with caution, particularly so for any shared code. There is no
--  attempt to permit the use of namespaces or anything similar. You may want to
--  minimise the risk of clashes by using eg "printPL20130508" instead of "print".
--
-- Special Labels
-- ==============
--  ":>init" introduces an init label in #ilASM; pmain.e kick-starts things with opInit which calls all such labels at the start of execution.
--  ":<exch" is reserved for an exception handler; a fatal error occurs on attempt to define more than one handler, or !PE64. (see pFEH.e)
--  ":!bang" introduces a crash label in #ilASM; pDiagN.e (and anything else that wants to) can "cmp reg,:!bang"/"je :???" which will obviously 
--  generate the expected code when the specified label has been defined, but - just as importantly - will just quietly resolve as 0 when the
--  label has /not/ been defined. Obviously an optional jump is a no-brainer, as it will either actually go there or carry on at the next line
--  in the source code. To optionally call a bang label, make the callee leave the return address on the stack, so it always needs a discard;
--  should it resolve to call 0, which pushes the same return address as a non-0 call but goes nowhere, the same cleanup is needed, ie:
--
--          :!bang
--              ...
--              mov eax,[esp]   -- preserve the return address      -- ( or: mov rax,[rsp]
--              jmp eax         -- (instead of ret)                 --       jmp rax
--          ...
--              call :!bang     -- if not linked in, does nowt      --       call :!bang
--              add esp,4                                           --       add rsp,8 )
--
--DEV now :<exch
--  I originally considered reserving ":<stop" to compliment ":>init", before realising that bang labels probably fulfil any such requirement.
--  (Not to mention that a single point of stoppage is a tad trickier than a single point of initialisation, and at this level it probably is
--   better to force the poor programmer to figure something out, rather than risk a fresh can of fiddly edge cases that I never considered.)
--  (Some evidence of that ("stop") is almost certainly still floating around in the code, but it was never used and certainly never worked.)
--
--  Do not assume that :>init are called in any particular order. Should, for example, :>debuginit need :>stackinit, the obvious pattern is:
--
--      <stack.e:>
--          integer sinit = 0 -- (a normal hll var)
--              ...
--            :>stackinit
--              cmp [sinit],0
--              jne :dont_do_everything_twice   -- (pretty much the whole point of me even mentioning this!)
--                  mov [sinit],1
--                  ...
--            ::dont_do_everything_twice
--
--      <debug.e:>
--          include stack.e
--              ...
--            :>debuginit
--              call :>stackinit
--
--  I am assuming you can use stack.e /without debug.e/; if they always went everywhere together you'd just use a normal global label.
--  Warning: while that asm will do exactly what you told it to sinit, the hll half of the compiler may apply constant propagation of
--   that hll "= 0" to anywhere sinit is referenced in hll code, that is assuming it never gets assigned non-zero by hll code.
--
--  These special labels (init and bang) are exactly the same as normal global labels, but with an extra bit setting or two. As things stand,
--  in some circumstances you might get away with calling one using the ":%name" syntax, but that's just an accident of picking the cheapest
--  and quickest implementation strategy, which I'll get round to fixing (/testing) if and only if it starts to cause me some serious grief.
--  There is not and never will be a way for identically-named special and/or global labels to co-exist (eg :>lbl1, :!lbl1, :%lbl1, === NO! )
--
--  The following init labels are pre-defined in the standard backend:
--
--      :>initFEH       builtins\VM\pFEH.e      (final exception handler)
--      :>initFPU       builtins\VM\pFPU.e      (initialise floating point unit)
--      :>initStack     builtins\VM\pStack.e    (frame/call/retf)
--      :>Time0         builtins\VM\pTime.e     (time() function, set t0)
--      :>Rand0         builtins\VM\pRand.e     (initial seed for rand())
--
--  Most applications will start with calls to the first three, and many all five, which is      (DEV make stack call the other 4?)
--   still extremely lightweight compared to what most other languages do at startup.
--  Note that when interpreting, the init labels within the VM called when p.exe starts up
--   are NOT re-invoked at the start of the code generated for the main .exw file.
--
--  #ilASM{ cmp [count],0
--          jl :!e52rcmbnni }   -- (if -nodiag, jl 0 == next instruction, which is obviously
--                              --  going to crash and burn, badly, but you did say -nodiag)


-- Calling conventions.
-- ====================
--  You may replace "-- /**/" with "--/**/" to use the following as template code.
--  See also pilx86.e for further details/any opcodes not listed here.

--DEV search/check:
--opGetVMep
-- /**/         #ilASM{ lea edi,[VMep]
-- /**/                 lea ecx,[debugleak]
-- /**/                 call %opGetVMep }   -- [edi]=VMep; [ecx]=debugleak

--              ; VMep is a table of entry points to the Virtual Machine, including
--              ;  opGetVMep itself, and must exactly match the number/order above.
--              ; debugleak is a build setting, which is most likely to be 0 on all 
--              ;  public releases. See pemit.e/t97mleak.exw for examples of use.

--DEV to go (see pStack.e)
--opGetST
-- /**/         #ilASM{ lea edi,[symtab]
-- /**/                 lea esi,[o]             -- flag/crashmsg
-- /**/         [--]    lea ecx,[crashfile]
-- /**/                 call %opGetST }         -- [edi]=symtab [DEV++]
--              ; Obtains a copy of the symbol table.
--              ; Currently used in pdebug.e, pdiag.e, prtnid.e, pcmndln.e, and pcfunc.e
--              ; Since Phix is self-hosted, there are two symtab in memory during
--              ;  interpretation: one for p.exe itself and one for the application.
--              ; pdebug, prtnid, pcmndln and pcfunc always want the currently executing 
--              ;  one (and do not need crashmsg/crashfile) whereas pdiag wants the one
--              ;  active at the point of failure. During interpretation, pdiag is 
--              ;  running in the one that came with p.exe, and passes a 0 in [esi]
--              ;  to retrieve the symtab which was active at the point of failure.
--              ;  passing 0 in [esi] when there has been no point of failure is
--              ;  fairly obviously going to retrieve garbage and/or crash.
--              ; if [esi] is 1 it is not altered and ecx need not be set.
--              ; Technically speaking, p2/p3 should be declared as type object.
--              ; [edi] recieves a copy of symtab (now with a refcount of 2).
--              ;  callstack info is returned via symtab[T_callstk] in all cases.
--              ; Note that the symtab returned in p1 is not a "proper" sequence, just 
--              ;  raw data made to look much like one for convenience. Examining some
--              ;  elements and/or modifying it may cause a fatal crash. In contrast, 
--              ;  see plist.e, where symtab is a 100% pukka sequence, and/or modify
--              ;  constant dumpSymTab therein, then either "p -cp ; p -d test" or 
--              ;  "p p -d test" and examine the resulting list.asm.
--              ; Trying to modify the returned symtab, especially by using hll code,
--              ;  is a classic example of attempting to pull the rug out from under
--              ;  your own feet. NB see also opRbldIds below.

--DEV newEmit
--opInterp
-- /**/     #ilASM{ call :%save_ebp         -- (see pcfunc.e)
-- /**/             mov [local_ebp],eax
-- /**/             mov eax,[symtab]
-- /**/             mov edx,[CSvaddr]
-- /**/             mov esi,[DSvaddr]
-- /**/             mov ebx,[opstat]
-- /**/             mov edi,[ptab]
-- /**/             lea ecx,[errorcode]
-- /**/             call %opInterp2
-- /**/             mov edx,[local_ebp]
-- /**/             call :%restore_ebp
-- /**/     }
--              ; Used in p.exw.
--              ; nb it is not possible to ship closed-source executables that invoke opInterp.

--opAlloc
-- /**/         #ilASM{ lea edi,[result]
-- /**/                 lea esi,[size]
-- /**/                 call %opAlloc }     -- [edi] = allocate([esi])

--opFree
-- /**/         #ilASM{ lea edi,[addr]
-- /**/                 call %opFree }      -- free([edi])

--DEV..
--opMalloc
-- /**/         #ilASM{ lea edi,[result]
-- /**/                 mov eax,[size]
-- /**/                 call :%opMalloc }   -- [edi] := malloc(eax)

--opMfree
-- /**/         #ilASM{ mov eax,[addr]
-- /**/                 call :%opMfree }    -- mfree(eax)

--newEmit (to go)
--opRbldIds
-- /**/         #ilASM{ lea edi,[rbicb]
-- /**/                 call %opRbldIds }   -- save rebuild callback
--              ; First, see pttree: variable-ids are /not/ stored *anywhere* as
--              ;  straightforward ascii strings during compilation.
--              ; Next, see pemit.e: when binding, or when interpreting and unresolved 
--              ;  routine_ids exist, pemit.e reconstructs the var-ids automatically, 
--              ;  otherwise (when interpreting) it sets a callback in case pdebug or
--              ;  pdiag are going to need them. In both cases, rbicb is called before
--              ;  any of the code in pdebug.e/pdiag.e and the saved copy of rbicb is
--              ;  cleared to prevent it being called unnecessarily again.
--              ; If you want to use opGetST and get a complete symtab with ascii ids,
--              ;  the easiest thing to do is leave a void=routine_id("gobbledegook")
--              ;  call in the code to trick pemit.e into reconstructing them (at the
--              ;  cost of including prtnid.e).

--opSetDbg
-- /**/         #ilASM{ mov edi,[debugcb]
-- /**/                 mov esi,[clrdbgcb]
-- /**/                 call %opSetDbg }    -- save edi,esi
--              ; Used by pdebug.e to set the debug and clear_debug callbacks.
--              ; The debug callback is called via opLnt after a trace(>=1).
--              ; The debug callback clears the clear_debug callback during operation,
--              ;  but otherwise the clear_debug callback is called via eg puts(1,xxx) 
--              ;  to remove the trace window (temporarily) for normal console display.
--              ; Note that once a debug callback is set, subsequent attempts to set
--              ;  another are ignored. This means that when invoking "p.exe p.exw"
--              ;  any trace window occurs via the pdebug.e compiled into p.exe,
--              ;  rather than any just loaded via p.exw. To avoid this you can 
--              ;  build a p.exe without including pdebug.e, ie comment out the
--              ;  include pdebug.e statement in p.exw, run p -cp, uncomment that
--              ;  line and then test pdebug.e "live" in interpreted mode. This
--              ;  is to avoid any confusion (ha ha) over multiple copies of it.
--              ; Use in pgui.exw has not yet been finalised but it seems reasonable
--              ;  that debugcb will be similar and clrdbgcb unused.
--              ; Must not be used for any other purpose.

--opCrshRtn
-- /**/         #ilASM{ lea edi,[diagcb]
-- /**/                 call %opCrshRtn }   -- save [edi]
--              ; Used by pdebug.e to set the diag callback, which is called following
--              ; a fatal error to display a message and create ex.err.
--              ; Must not be used for any other purpose.

--opDelRtn
-- /**/         #ilASM{ lea edi,[delcb]
-- /**/                 call %opDelRtn }    -- save rebuild callback
--              ; See builtins\pdelete.e. Saves the callback address so that it can be
--              ;  called when a reference count drops to zero.

--opDelRef
-- /**/         #ilASM{ call %opDelRef
-- /**/                 mov [o_common],eax }        -- store the parameter
--              ; See builtins\pdelete.e. Ideally I suppose I'd fix up call_back so that
--              ;  anything, not just atoms, could be passed. This was easier.


--opRTErn
-- /**/         #ilASM{ mov al,imm8     -- (or mov eax,[vname])
-- /**/                 mov edi,[ep1]
-- /**/                 mov esi,[ep2]
-- /**/                 call %opRTErn }     -- fatal error eax
--              ; Trigger a fatal error. Currently only used in pprntf.e
--              ; Note that without debug in pprntf.e allows an opRTErn to appear on
--              ;  the printf call, rather than in pprntf.e itself, provided that
--              ;  constant swod in pdiag.e is zero.
--              ; ep1 and ep2 are message specific and may not be required; see
--              ;  pdiag.e, if not used then edi/esi should be set to zero.
--              ; eg e58bfn is "invalid file number (%d)\n" and hence
--              ;  mov al,58; mov edi,[fn]; xor esi,esi looks about right.
--              ; Theoretically you can use new error numbers and modify pdiag.e to 
--              ;  match, but be advised to get any mods to pdiag.e incorporated 
--              ;  into the official version to prevent repeated future breakage.
--              ; Message numbers may not exceed 256 (if necessary subcategorise
--              ;  using ep1/ep2 which allows more than 1000 error messages for
--              ;  every second the universe has thus far been in existence).
--              ;  Message numbers over 127 have not yet been tested.
--
--opClrDbg
-- /**/         #ilASM{ call %opClrDbg }
--              ; Used during (the experimental hll) screen i/o, see pfileio.e.
--              ; Invokes debug_screen(0) in pdebug.e; note this is part of p.exe,
--              ; on a bound executable calling this has null effect, and in the
--              ; "p.exe p.exw xxx" case, the "interpreted copy" of pdebug.e does
--              ; //NOT// override the active copy in p.exe. Further note that
--              ; pdebug.e invokes opSetDbg on entry to and exit from debug(), 
--              ; which makes opClrDbg temporarily have null effect. Should you 
--              ; want to test any changes to pdebug.e, you must rebuild - use 
--              ; pth.exw if you are worried about breaking p.exe.
--
--DEV out of date (newEBP) (...and then some, newEmit; see VM\pStack.e)
--opFrame (makeFrame)
--;--
--;-- on Entry, eax is addr first local to be saved
--;--         ecx is number of locals to be saved
--;--         esi is "called from" addr
--;--         edi is routineNo to call
--;-- On Exit, edx contains the new, saved, value for [vsb_used]. (no longer needed)
--;--        esi contains addr first (was in eax), used by opTchk
--;--        ecx is 0, used by opTchk
--;-- edi is addr first saved item in callstack, for use instead of opFrst
--;--        In fact, and in case you can find any use for these, (make a note here!)
--;--        eax is h4
--;-- (gone:         ebx is return addr (was in esi))
--;-- (gone:         edi is [last param/local+4])
--;--        Also readily available if you want, 
--;--           (ie possibly for free vs. callee push/pop|mov):
--;--           N (was in ecx),
--;--           the calling (current) routine,
--;--           routineNo to call (was in edi)
--;--
--;-- called from opFrame (after which params are set via opMove/opFrst)
--;-- OH: and opTchk, opCallOnce, and cbhandler(see pcfunc.e)...
--;--
--;-- A frame (on the virtual stack [block]) is:
--;--
--;--  called from address (for ex.err reporting)
--;--  first (address of first item being saved)
--;--  items 1..N (params, locals, tmps as they were before the call)
--;--  N (number of params and locals which got saved)
--;--  calling routine (index to symtab)
--;--  routine being called (index to symtab)
--;--  return address
--;--
--;-- opFrame ensures there is enough space remaining in the current block,
--;-- OpRetf is responsible for shifting down a block when needed.
--;--
--;

--opUnassigned
-- /**/         #ilASM{ mov eax,[p1]
-- /**/                 cmp eax,h4
-- /**/                 jne @f
-- /**/                   lea esi,[p1]
-- /**/                   call %opUnassigned -- fatal error
-- /**/               @@: }
--              ; We only perform such tests when a variable might be unassigned.
--              ; The front end (pmain.e) generally has the better idea regarding
--              ; whether a variable is initialised or not, eg in:
--              ;       if flag then
--              ;           ...             [1]
--              ;           X = 1
--              ;           ...             [2]
--              ;       else
--              ;           ...             [3]
--              ;       end if
--              ;       ...                 [4]
--              ; Obviously X /is/ init at [2], but we cannot be sure at [1/3/4].
--              ; <aside>
--              ;       There is no "merge" (yet?); were there an X=2 at [3], it
--              ;       has no mechanism to know X /is/ init at [4] - although it 
--              ;       may look simple, I cannot find a cheap way to do it...
--              ;       The problem is not so much the merge, but rather the
--              ;       creation of (thousands of) lists of initialised vars, 
--              ;       that is, without doubling all compilation times...
--              ;       I feel I should offer some advice at this point:
--              ;           1. Try before you buy. If you think the compiler 
--              ;               would be vastly improved by something, add a 
--              ;               command line option -o/-optimise which assumes
--              ;               (in this case) that everything is initialised.
--              ;               Then measure/time some programs.
--              ;           2. Is everything covered? Would there be better
--              ;               gains to be had in say:
--              ;                   sequence X
--              ;                   function f()
--              ;                       return length(X) -- [1]
--              ;                   end function
--              ;                   X = "this"  ?f()
--              ;                   X = "that"  ?f()
--              ;               Again, while we humans can easily spot that X 
--              ;               will always be assigned at [1], making the 
--              ;               compiler /know/ that is not so trivial.
--              ;               Have you just measured gains from something
--              ;               like that rather than if-statement branches?
--              ;           3. Test the fail cases. Is making the compiler 5%
--              ;               faster worth changing a helpful message such as
--              ;               "var X unassigned" to a far less than helpful
--              ;               (say) "unknown machine error at #00408156"?)
--              ; </aside>
--              ; However, this information is lost, unless preserved in the il
--              ;   stream, which tends to be the exception rather than the norm
--              ;   by the time we get to pemit.e/pilx86.e. Instructions such as
--              ;   opSubse, which can take any number of (possibly unassigned)
--              ;   subscripts/indexes, must therefore be preceded by explicit
--              ;   {opUnassigned,N} il instructions.

