--
-- builtins\VM\optable.e
-- =====================
--
--*Temporary note: you /will/ need to "p -cp" b4u "p -d -e2 p7" if you change this file.
--*That may (eventually) translate to needing "p p -cp" instead of "p -cp".
--
--  I suppose that technically this is part of p[w].exe rather than part of the VM.
--  The optable is built from scratch when compiling (doh) but copied when interpreting.
--  In "p p p test", test will be interpreted using a copy of a copy of optable, which
--  will therefore contain addresses from the (leftmost) p.exe rather than either of
--  the intermediate p.exw, iyswim.
--
-- optable rules
-- =============
--  Any and all builtins\VM\xxx files that are part of the optable can ONLY have global
--  label entry points (:%xxx, and their :!xxx, :>xxx variants) as externally accessible. 
--  In truth it could be acceptable to have some hll globals shared by one or more VM 
--  components, but not meant to be externally visible, however it is easy enough to use 
--  low-level getter/setter global label entry points anyway. The latter do not usually 
--  need to be part of the optable proper if they are only meant for internal use.
--  As a related example, pLenN.e declares :!opLene36or92 for use in pDiagN.e but since 
--  both pLenN.e and pDiagN.e are in the optable, :!opLene36or92 need not be. (Not quite
--  the same thing, since it is more "there or not there" in that particular case.)
--
--  NB: Having a "global function open" in pfileioN.e and an AutoAsm("open",.."%opOpen")
--  in psyme.e led to some very difficult to diagnose problems. Renaming the routine in
--  pfileioN with (local) "function fopen" solved the issue. Make of that what you will.
--
--  Adding items to the optable may involve: writing new code to actually implement it,
--  changes to pmain.e, especially in relation to T_Asm, changes to pilx86.e, typically 
--  related to using emitHex5callG and whatever calling convention the new code requires,
--  and changes to psym.e, AutoGlabel/AutoAsm (instead of) initialAutoEntry, as well as
--  new entries in the vm_names table below. My advice is to pick something similar and
--  perform a global search/try to follow what that does. Closest match being func/proc
--  and no/type of params and result, rather than similar behaviour/area/functionality.
--
--  TIP: when changing existing labels, it may help to compile with both the old and new
--  names active, then remove the old: eg pUnassigned.e :%pAddiii -> :%e01tcfAddiii was
--  one case where this was required. Changing pUnassigned/optable/psym all at the same
--  time failed but adding both to pUnassigned.e/recompile/change optable.e/recompile/
--  change psym.e/recompile/and finally delete old from pUnassigned.e worked just fine.
--  (Fewer steps may also work, but it only takes ~50s for the long method anyhow.)
--  Likewise it can help to put a new unused label into optable and recompile, and only
--  then attempt to actually use it and recompile, rather than try both in one step.
--  

--
-- Technical notes
-- ===============
--  builtins\VM\puts1.e: The first seven entries can be used as a zero-dependency subset 
--  to get the ball rolling*. In contrast the rest of the table is kinda all-or-nothing.
--  [*It ought to be safe to ignore opUnassigned/opCallOnceYeNot when getting it going.]
--  :%opTrace/:%opLnt/:%opProfile/:%opLpn/:%opLnpt are the absolute must-haves in terms
--  of being in the optable/part of p.exe rather than simply re-included, since they need
--  to refer to pglobals.e/allfiles for the source code text, and probably other stuff.
--  pDiagN.e is potentially tricky. When interpreting, but not when compiled, it needs to 
--  invoke pemit2.e/rebuild_callback(), which implies but does not technically force it
--  to be part of p[w].exe/the optable. However testing for :!opLene36or92 and the like
--  leads directly to the "all or nothing" situation mentioned previously.
--  pHeap.e/pDeleteN.e/pcfuncN.e and to a lesser extent pfileioN.e form another subset
--  that can be very beneficial when shared by executables and dymanically loaded files.
--  Note that routine_id(), and subsequent use in call_proc/func and call_back() all
--  assume that the "currently active" symbol table is appropriate. Hence such things
--  are not generally transferrable between processes or across opInterp and/or dll
--  boundaries, except under that advisement. In some cases, inline assembly can or
--  must be used to directly manipulate symtabptr (aka [ds+8]) to make such work. (:%pGetSymPtr)
--

-- todo:
--  symtab[T_EBP] (pAbort outstanding)
--  :%pAlloc/Free [DONE]
--  :%opTrace/:%opLnt/:%opClrDbg/needclr
--  :%opProfile/:%opLpn/:%opLnpt
--  mov edx,routine_id(debug_screen)    -- mov edx,imm32 (sets K_ridt)  [DONE]
--  mov ecx,$_Ltot                      -- symtab[debug_screen][S_Ltot] [DONE]
--  jmp $_il                            -- (symtab[debug_screen][S_il]) [DONE]
--  pcfunc -> opcodes, :>cfinit, :<cfinit (or self managed via [ds+8]) (:%pGetSymPtr)
--  pCritSecN.e -> opcodes in pHeap.e [DONE]
--  pStack: ::trimStack, :%popStack/:<killStack (or self managed via [ds+8]) (:%pGetSymPtr)
--  pDelete.e -> opcodes (or self managed via [ds+8]?) (:%pGetSymPtr)
--  pDiagN.e -> opcodes (:%pCrashMsg, :%pCrash, :%pCrashFile), smvds8
--  pprntfN.e -> opcodes (:%opSprintf, :%opPrintf)
--  psprintN.e -> opcodes (:%opSprint, :%opPrint)
--  pcase.e -> opcodes (:%opUpper, :%opLower, :%isUpper, :%isLower, low priority)
--  pcurrdir.e -> opcodes (:%opCurrDir, low priority)
--  pfileioN.e -> opcodes (:%opOpen, :%opFlush, :%opClose, ?:%opClosem9?,
--                          :%opSeek, :%opWhere, :%opLock, :%opUnlock,
--                          :%opGetText, (:%fiodiag), :%opGetPos, :%opWrap,
--                          :%opScroll, opTextRows, :%opBkClr, :%opTxtClr,
--                          :%opClrScrn, :%opFreeCons, :%opPosition)
--               (already as opcodes: :%opGetc, :%opGets, :%opPuts,
--                                    :%opGetKey, :%opWaitKey)
--  ppp.e -> opcodes? (ppOpt, ppEx, pp, ppf, ppExf)??

--/*

--      --DEV leave this alone for now...
--      pDeleteN.e  :%pDelRtn (see :%pDelete)
--      ========    global function delete_routine(object o, integer rid)
--                  global procedure delete(object o)
--          includes
--                  pcfuncN.e
--
--      pDiagN.e    (done,ish)
--      ========    :%pCrashMsg,
--                  :%pCrash,
--                  :%pCrashFile,
--                  :!SetBatchMode,
--                  :!iDiag,
--                  :!fehDiag
--          includes
--      --include builtins\VM\pcfuncN.e
----/!**!/          include builtins\VM\pprntfN.e
----/!**!/          include builtins\VM\psprintN.e
----/!**!/          include builtins\pcurrdir.e
----/!**!/          include builtins\pcase.e
--      --include builtins\VM\pfileioN.e
--      include builtins\VM\pAbort.e
--      --include pgets0.ew     --DEV removed 16/6/08...
--      --include builtins\VM\pppN.e
--      include builtins\ppp.e
--      pStack
--      pHeap
--                  call :%opGetST  -- [edi]:=symtab (see pStack.e)
--      --(not part of optable)
--                  cmp edx,:!opLene36or92          (pLen.e)
--                  cmp edx,:!opSubse1ipRe92a       (pSubseN.e)
--                  cmp edx,:!Jccp2Intp3Ref         (pJcc.e)
--                  cmp edx,:!Jccp2NotInt           (pJcc.e)
--                  cmp edx,:!Jife92                (pJcc.e)
--                  cmp edx,:!opJnotxe92a           (pJnotx.e)
--                  cmp edx,:!opJnotxe92b           (pJnotx.e)
--                  cmp edx,:!opXore92a             (pXor.e)
--                  cmp edx,:!opXore92b             (pXor.e)
--                  cmp edx,:!blockfound            (pHeap.e)
--                  cmp edx,:!blockfoundC0000005    (pHeap.e)
--                  cmp edx,:!bf_midchain           (pHeap.e)
--                  cmp edx,:!GetPoolnotTCBa        (pHeap.e)
--                  cmp edx,:!FreePoole32a          (pHeap.e)
--      --DEV:
--      --          (:%e01tcfAddiii) [just this second commented out...]
--
--      pTrace.e    (done,ish)
--      =========
----/!**!/      include image.e -- save_text_image()
--      --include file.e    -- for seek()
--      --include machine.e -- allocate_string()
--      --include dll.e
--      --include misc.e    -- needed for WIN32
--      --include graphics.e    -- needed for BLACK etc
--      --include builtins\graphics.e -- video_config()
--      --include builtins\dll.e        -- open_dll() etc
--      --include builtins\image.e  -- save_text_image()
--
--          puts(1,"") -- ensure console exists
--          xKernel32 = open_dll("kernel32.dll")
--          xGetStdHandle = define_c_func(xKernel32,"GetStdHandle",
--          xCSBI = allocate(sizeof_CSBI)
--          pMode = allocate(4)
--          stdout = c_func(xGetStdHandle,{STD_OUTPUT_HANDLE})
--          screenCols = peek2u(xCSBI+CSBI_SIZEX)
--          dScreen = save_text_image({1,1},consoleSize)
--          runPos = get_position()
--          position(1,1)   -- ensure top line is visible
--          charMap = repeat(OTHER,255)
--          bk_color(blat_bk)
--          text_color(blat_txt)
--          if find(word,keywords) then
--          ptr = sprintf("%5d%s",{line,ptr})
--          Qnames = prepend(Qnames,symtab[symidx][S_Name])
--          if and_bits(WrapMode,ENABLE_WRAP_AT_EOL_OUTPUT) then
--          o = ppf(o)
--
--      function show_trace(integer fileno, integer line, integer trclvl)
--!         if rbldrqd then
--              rebuild_callback()
--          end if
--          call :%opGetST      -- [edi]:=symtab (see pStack.e)
--      ?9/0 --DEV get this from prev_ebp...
--          s8 = symtab[T_callstk]  -- {ep1,ep2,era,etd,ern,ebp,vsb_root,dcount} (see pdiag.e for full details)
--          abort(1)
--          key = get_key()     -- (ie no wait)
--          key = wait_key()
--              elsif key='q' then res = 0 exit                         -- quit (resume normal execution)
--              elsif key='Q' then res = -1 exit                        -- Quit ("", permanently)
--              elsif key='!' then res = -2 exit                        -- abort (create ex.err)
--          :%opTrace
--          :%opLnt -- trace this line (only valid when interpreting)
--          :%opClrDbg
--
--      pcfuncN.e
--      =========
--      include builtins\VM\pHeap.e     -- :%pStoreFlt etc
--      include builtins\VM\pStack.e    -- :%opFrame etc
--      --include builtins\VM\pDiagN.e  -- e02atdb0 (DEV/now in pUnassigned)
--      --include builtins\VM\pprntfN.e
--      --include platform.e
--      include builtins\VM\pUnassigned.e   -- :%pRTErn
--                  call :%pRTErn
--                  call :%pStoreFlt                    -- ([edi]:=ST0)
--      --          enter_cs()
--      --          leave_cs()
--                  global function open_dll(sequence filename)
--                  global function define_c_func(object lib, object fname, sequence args, atom return_type)
--                  global function define_c_proc(object lib, object name, sequence args)
--                  global function define_c_var(atom lib, sequence name)
--      --          (:%opOpenDll, :%opDefineCfunc, :%opDefineCproc, :%opDefineCvar???)
--          if not tinit then
--              table = {}
--              tinit = 1
--          end if
--              previd = {}
--              prevcb = {}
--              pinit = 1
--                  global function call_back(object id)
--      --          (:%opCallBack???)
--                  call :%opGetST  -- [rdi]:=symtab (see pStack.e)
--              r = allocate(16)    -- STDCALL needs 13 bytes, CDECL 11 (round up to 4 dwords):
--                  call :%opFrame
--                  call :%e02atdb0                 -- (DEV better: call back returned non-atom?)
--                  call :%pDealloc0
--                  global procedure call(atom addr)
--      --          (:%opCallAddr??)
--                :%save_ebp
--                :%restore_ebp
--                  call :%save_ebp
--                  call :%restore_ebp
--                  global function c_func(integer rid, sequence args={})
--                  global procedure c_proc(integer rid, sequence args={})
--                  global function call_func(integer rid, sequence params)
--                  global procedure call_proc(integer rid, sequence params)
--      --          (:%opCFunc, :%opCProc, :%opCallFunc, :%opCallProc??)
--
--
--      pprntfN.e
--      =========
--
--          includes builtins\VM\pUnassigned.e  -- :%pRTErn
--
--                  f /= power(10,exp)
--                  if f>5 or (f=5 and remainder(digit,2)=1) then
--                  dot = result[i]
--                  result[i] = '0'
--                  and (find('.',result) or find('!',result)) then
--                  result = result[1..tmp]
--                  res = repeat('n',3)
--                  digit = floor(f)
--                  result &= capE
--                  reve = append(reve,floor(remainder(exp,10)+'0'))
--                  result &= reve
--                  call :%pRTErn
--              global function sprintf(sequence format, object args)       => opSprintf
--      --      enter_cs()
--      --      leave_cs()
--      --DEV move this (once newEmit is done)
--      -- note: printf is now defined in pfileioN.e [BLUFF]
--              global procedure printf(integer fn, sequence format, object args={})
--              puts(fn,sprintf(format,args))
--
--      psprintN.e
--      ==========
--      include pprntfN.e
--              global function sprint(object x, integer l=-1, integer nest=0)
--              return sprintf("%.10g", x)
--      --DEV move this to pfileioN.e:
--              global procedure print(integer fn, object x, integer l=-1)
--              puts(fn,sprint(x,l))
--
--      pcurrdir.e
--      ==========
--      include builtins\pgetpath.e -- not strictly necessary, but why not.     (** NOT part of VM **)
--      include builtins\VM\pcfuncN.e -- not strictly necessary, but why not.
--              global function current_dir()   ==> opCurrDir
--          res = repeat(' ',l-1)
--          l = c_func(xGetCurrentDirectory,{l,res})
--          res = get_proper_path(res)
--
--      pcase.e
--      =======*
--          toUpper = repeat(255,255)
--              global function upper(object x)
--              global function lower(object x)
--              global function isupper(integer ch)
--              global function islower(integer ch)
--
--      pfileioN.e  (Phix compatible 0.6.4)
--      ==========
--          call :%pRTErn       -- fatal error
--          if abs(amount)>abs(bottom-top) then
--
--              global function open(sequence filepath, object openmode)    => opOpen   [DONE, ish]
--              :%opOpen
--      --      :%n_flush_esiedi    (internal)
--      --      :%n_flush_esi2      "
--      --      :%n_flush_rsirdi        "
--      --      :%n_flush_rsi2      "
--              global procedure flush(integer fn)                          => opFlush
--              global procedure close(integer fn)                          => opClose
--      ?       :%opClosem9?
--              global function seek(integer fn, atom pos)                  => opSeek
--              global function where(integer fn)                           => opWhere
--              :%opGetc
--      --      :%n_initC   (internal)
--              :%opGets
--              :%opPuts
--              :%opGetKey                     -- [edi]=get_key()
--              :%opWaitKey                    -- [edi]=wait_key()
--              global function lock_file(integer fn, lock_type locktype, byte_range byterange)     => opLock [DONE]
--              global procedure unlock_file(integer fn, byte_range byterange)                      => opUnlock [DONE]
--              global function get_text(integer fn, integer option=-2) -- = GT_WHOLE_FILE)         => opGetText [DONE]
--      --      (global procedure diag(integer fn))
--              global function get_position()                                          => opGetPos [DONE]
--              global procedure wrap(integer flag)                                     => opWrap [DONE]
--              global procedure scroll(integer amount, integer top, integer bottom)    => opScroll [moved to builtins\pScrollN.e]
--              global function text_rows(integer newrows)                              => opTextRows [DONE]
--              global procedure bk_color(integer color)                                => opBkClr [DONE]
--              global procedure text_color(integer color)                              => opTxtClr [DONE]
--              global procedure clear_screen()                                         => opClrScrn [DONE]
--              global procedure free_console()                                         => opFreeCons [DONE]
--              global procedure position(integer line, integer col)                    => opPosition [DONE]
--
--      ppp.e
--      =====
--          puts(ppp_File,pline[1..plen])   [opPuts]
--          if wait_key() then end if       [opWaitKey]
--          txt = sprintf(ppp_IntFmt,cl)    [opSprintf]
--          if find(getc(0),"dD") then ?9/0 end if
--          abort(1)
--
--              global procedure ppOpt(sequence options)
--              global procedure ppEx(object o, sequence options)
--              global procedure pp(object o)
--              global function ppf(object o)
--              global function ppExf(object o, sequence options)
--
--      pFind.e
--      =======*
--          global function find(object s1, sequence s2, integer start=1)
--
--      pMatch.e
--      ========*
--          global function match(object s1, sequence s2, integer start=1)
--
--      pmaths.e
--      ========*
--          global function abs(object o) (etc)
--
--*/

--
--include builtins\VM\pStack.e  -- :%opGetST

-- had to do this in pMath.e:
--include builtins\VM\pXor.e        -- not actually used in p.exe, but in optable for pdiagN.e

--
-- Note: the "p -test" set may one day contain various format directives, hence
--       this routine should cope if invoked with different X64 etc settings.
--
-- The following groupings (1..4) are just comments, see also "_readme.txt"
constant vm_names = {
                     -- group 1: proof of concept, optional, no dependencies
                    --           (left in for future porting efforts..)
                     {"puts1.e",   {"%puts1",
                                    {"%puts1ediesi","%puts1rdirsi"},
                                    "%puthex32a",
                                    "%puthex32",
                                    "%puthex64",
                                    "%putsint",
                                    "%getc0"}},
                     -- group 2: mandatory
                     {"pDiagN.e",  {"%pCrashMsg",
--                                  "%pCrash",
--                                  "%pCrash1",
                                    "%pCrashFile",
                                    "!SetBatchMode",
                                    "!iDiag",
                                    "!fehDiag"}},
                    -- NB the next seven entries (opTrace..opLnpt) are often all zero in .exe other than 
                    --  p[w].exe, since normally only p.exw includes the files pTrace.e and pProfile.e.
                     {"pTrace.e",  {"%opTrace",
                                    "%opLnt",
                                    "%opClrDbg"}},
                     {"pProfile.e",{"%opProfout",
                                    "%opProfile",
                                    "%opLnp",
                                    "%opLnpt"}},
                     -- group 3: probably mandatory
                     {"pHeap.e",    {"%pGetpGtcb",  -- (diagnostics only)
--                                   "%pClearpGtcb",
--                                   "%pRestorepGtcb",
                                     "%pNewGtcbChain",
                                     "%pRestoreGtcbChain",
                                     "%pSetSaveEBP",
--                                   "%pRestoreEBP",
                                     "%pSetDel",
                                     "%pAllocStr",
                                     "%pAllocSeq",
                                     "%pStoreFlt",
                                     "%pStoreMint",
                                     "%pLoadMint",
                                     "%pDealloc",
                                     "%pDealloc0",
                                     "%pGetPool",   -- (pStack only)
                                     "%pFreePool",  -- (pStack/pApnd only)
                                     "%pAlloc",
                                     "%pFree",
--                                  (:%opIncRef, :%opDecRef) [not yet used]
--                                   "%pCleanupAll",    --???[DEV] (unused, does nowt, may not be necessary)
                                     "%pInitCS",
                                     "%pDeleteCS",
--DEV (once we've finished playing with heap/d)
--  :%pGetStdCS                 -- [edi]:=get_stdcs()
                                     "%pEnterCS",
                                     "%pTryCS",
                                     "%pLeaveCS"}},
                     {"pStack.e",   {"%pGetSymPtr",
                                     "%pSetSymPtr",
                                     "!opGetST",
                                     "%opGetST",
                                     "%newStack",
--DEV I think this can/should now go:
                                     ">initStack",
--                                   {"%opGetArgELF32","%opGetArgELF64"} -- not yet used
                                     "%opFrame",
                                     "%opCallOnce",
                                     "%opRetf", 
                                     "%pFreeStack",
                                     "%opTchk",
                                     "%opTcFail",
                                     "%SetCCleanup",
                                     "%RunCleanup",     -- (DEV may not be necessary) [erm, maybe for p p p p parlour tricks]
                                     "%opAbort"}},
--                   {"pAbort.e",   {"%opAbort"}},
                     -- group 4: optional, except for pDiagN (ie without these, pDiagN will start doing cmp 0's, and
                     --                                       hence some errors may start being incorrectly reported,
                     --                                       but all the non-error stuff should be just fine.)
                     {"pType.e",    {"%opInt",
                                     "%opAtom",
                                     "%opStr",
                                     "%opSq",
                                     "%opObj"}},
                     {"pFPU.e",     {"%down53",
                                     "%near53",
                                     "%trunc53",
                                     "%down64",
                                     "%near64",
                                     "%trunc64"}},
                     {"pFEH.e",     {}},                -- (:>initFEH)
                     {"pUnassigned.e",
                                    {"%pUnassigned",
                                     "%pBadRetf",
                                     "%e01tcfAddiii",
                                     "%e01tcfediDiv",
                                     "%pRTErn",
                                     "%pDiv0", 
                                     "%e02atdb0",
                                     "%e01tcfDivi2",
                                     "%e01tcfediMul",
                                     "!opCallOnceYeNot"}},
                     {"pMath.e",    {"%opAdd",
                                     "%opAddi",
                                     "%opSub",
                                     "%opSubi",
                                     "%opDiv",
                                     "%opDivi",
                                     "%opDivf",
                                     "%opDiv2",
                                     "%opMul",
                                     "%opMuli",
                                     "%opAndBits",
                                     "%opOrBits",
                                     "%opXorBits"}},
                     {"pLen.e",     {"%opLen"}},
                     {"pFixup.e",   {"%fixupIndex",
                                     "%fixupSliceStart",
                                     "%fixupSliceEnd"}},
                     {"pSubseN.e",  {"%pSubse",
                                     "%pSubse1",
                                     "%pSubse1i",
                                     "%pSubse1ip",
                                     "%pSubse1is"}},
                     {"pSubssN.e",  {"%pSubss",
                                     "%pSubsss"}},
                     {"pJcc.e",     {"%opJcc",
                                     "%opJccE",
                                     "%opJif",
                                     "%opScmp",
                                     "%opInt0",
                                     "%opAtom0",
                                     "%opStr0",
                                     "%opSq0"}},
                     {"pJnotx.e",   {"%opJnotx"}},
                     {"pXor.e",     {"%opXor"}},
                     {"pPower.e",   {"%opPow"}},
                     {"pRmdr.e",    {"%opRmdr"}},
                     {"pRepeN.e",   {"%pRepe",
                                     "%pRepe1",
                                     "%pRepe1ip",
                                     "%pRepe1is"}},
                     {"pRepsN.e",   {"%pReps"}},
                     {"pRepeatN.e", {"%opRepCh",
                                     "%opRepeat"}},
                     {"pUnary.e",   {"%opUminus",
                                     "%opNot",
                                     "%opNotBits",
                                     "%opFloor"}},
-- (I assume we do not need :%opPpndSA here)
                     {"pApnd.e",    {"%opApnd",     -- (append/prepend)
                                     "%opConcat",
                                     "%opCatsi",
                                     "%opConcatN"}},
                     {"pMem.e",     {"%opPeeki",
                                     "%opPeekNx",
--                                  (:%opPokeNS)    (additional safety checks)
                                     "%opPokeN",
                                     "%opMemCopy",
                                     "%opMemSet"}},
                     {"pMkSqN.e",   {"%pMkSq"}},
                     {"pTrig.e",    {"%opCos",
                                     "%opSin",
                                     "%opTan",
                                     "%opArcTan",
                                     "%opLog",
                                     "%opSqrt"}},
                     {"pfileioN.e", {"%opOpen",
                                     "%opClose",
                                     "%opSeek",
                                     "%opWhere",
                                     "%opFlush",
                                     "%opPuts",
                                     "%opGetc",
                                     "%opGets",
                                     "%opGetKey",
                                     "%opWaitKey",
                                     "%opLock",
                                     "%opUnLock",
                                     "%opGetText",
                                     "%opGetPos",
                                     "%opWrap",
                                     "%opTextRows",
                                     "%opBkClr",
                                     "%opTxtClr",
                                     "%opClrScrn",
                                     "%opFreeCons",
                                     "%opPosition"}},
--/*
--group3 (probably mandatory) [erm, too much hll cleanup rqd]
--                   {"pcfuncN.e",  {"%opOpenDLL",
--                                   "%opDcfunc",
--                                   "%opDcvar",
--                                   "%opCallback",
--                                   "%opCallA",
--                                   "%opCfunc",
--                                   "%opCproc",
--                                   "%opCallFunc",
--                                   "%opCallProc",
--                                   "%opGpct",
--                                   "%opRpct"}},
--                   {"pDeleteN.e", {"%opDelRtn",
--                                   "%opDelete"}},
--*/
--/*
                     {"puts1.e",   {"%puts1",
                                    {"%puts1ediesi","%puts1rdirsi"},
                                    "%puthex32a",
                                    "%getc0"}},
--*/
                     $
                    }
--!*/


--global function get_vm_names()
---- for filedump.exw (and plist.e)
--sequence res = vm_names, glabel
--  for i=1 to length(res) do
--      glabel = res[i]
--      if not string(glabel) then
----            if X64 then
----                res[i] = glabel[2]
----            else
----                res[i] = glabel[1]
----            end if
--          res[i] = glabel[X64+1]
--      end if
--  end for
--  return res
--end function

--!/*
global function get_vm_names()
--
-- "flattens" vm_names into a plain list of all labels.
-- (global for filedump.exw and plist.e)
--
sequence res = {}, labelset, glabel
    for i=1 to length(vm_names) do
        labelset = vm_names[i][2]
        for j=1 to length(labelset) do
            glabel = labelset[j]
            if not string(glabel) then
                glabel = glabel[X64+1]
            end if
            res = append(res,glabel)
        end for
    end for
    return res
end function

--function vm_length()
--integer res = 0
--  for i=1 to length(vm_names) do
--      res += length(vm_names[i][2])
--  end for
--  return res
--end function
--!*/

--global procedure vminit()
----
---- Initialise things for interpretation; this is not invoked during compilation.
---- Pre-populate the global labels table with VM entry points, and generate (via
---- Finc()) a table of builtins/VM/xxx files to be treated as "already included".
----
--sequence prevsym      -- previous symtab
--sequence glabel
--integer offset, lblidx
--sequence optable
--  if not newEmit then ?9/0 end if
--  if bind then ?9/0 end if
--  if intellisense=0 then
--      Finc("puts1.e")
----        ... (more to follow)
--if newEmit then -- ([TEMP] - without this :%opGetST gets an undeclared error)
--      #ilASM{ 
--          [32]
--              lea edi,[prevsym]
----                call :%opGetST  -- [edi]:=symtab (see pStack.e)
--              call :!opGetST  -- [edi]:=symtab (see pStack.e)
--              add esp,4
--          [64]
--              lea rdi,[prevsym]
----                call :%opGetST  -- [rdi]:=symtab (see pStack.e)
--              call :!opGetST  -- [rdi]:=symtab (see pStack.e)
--              add rsp,8
--          []
--            }
--      optable = prevsym[T_optable]
--      if length(optable)!=length(vm_names) then ?9/0 end if
--      for i=1 to length(vm_names) do
--          glabel = vm_names[i]
--          if not string(glabel) then
----                if X64 then
----                    glabel = glabel[2]
----                else
----                    glabel = glabel[1]
----                end if
--              glabel = glabel[X64+1]
--          end if
--          offset = optable[i]
--          if offset=0 then ?9/0 end if --DEV... (no want the Finc()?)
--          tt_string(glabel,-3)
--          glblused = append(glblused,G_declared+G_set)    -- + G_optable? +G_used?
--          glboffset = append(glboffset,offset)
--          glblabel = append(glblabel,0)
--          glblline = append(glblline,0)
--          glblcol = append(glblcol,0)
--          glblname = append(glblname,glabel)
--          lblidx = length(glblused)
--          tt[ttidx+EQ] = lblidx
--          agchecktt(ttidx,1)
--      end for
--end if
--
--  end if
--end procedure

global procedure vminit()
--
-- Initialise things for interpretation; this is not invoked during compilation.
-- Pre-populate the global labels table with VM entry points, and generate (via
-- Finc()) a table of builtins/VM/xxx files to be treated as "already included".
--
sequence prevsym        -- previous symtab
sequence glabel
integer offset, lblidx
sequence optable
integer opidx = 0, opidx0
object vmi1
sequence labelset
    if not newEmit then ?9/0 end if
    if bind then ?9/0 end if
    if intellisense=0 then
--      Finc("puts1.e")
--      ... (more to follow)
--if newEmit then -- ([TEMP] - without this :%opGetST gets an undeclared error)
        #ilASM{ 
            [32]
                lea edi,[prevsym]
--              call :%opGetST  -- [edi]:=symtab (see pStack.e)
                call :!opGetST  -- [edi]:=symtab (see pStack.e)
                add esp,4
            [64]
                lea rdi,[prevsym]
--              call :%opGetST  -- [rdi]:=symtab (see pStack.e)
                call :!opGetST  -- [rdi]:=symtab (see pStack.e)
                add rsp,8
            []
              }
        optable = prevsym[T_optable]
        prevsym = {}
        for i=1 to length(vm_names) do
            {vmi1,labelset} = vm_names[i]
            opidx0 = opidx
            for j=1 to length(labelset) do
                opidx += 1
                if optable[opidx]=0 then
?labelset[j]
                    vmi1 = 0
                    exit
                end if
            end for
--?opidx
            if string(vmi1) then
                Finc(vmi1)
                for j=1 to length(labelset) do
                    glabel = labelset[j]
                    if not string(glabel) then
                        glabel = glabel[X64+1]
                    end if
                    opidx0 += 1
                    offset = optable[opidx0]
                    if offset=0 then ?9/0 end if    -- sanity check
                    tt_string(glabel,-3)
                    glblused = append(glblused,G_declared+G_set)    -- + G_optable? +G_used?
                    glboffset = append(glboffset,offset)
                    glblabel = append(glblabel,0)
                    glblline = append(glblline,0)
                    glblcol = append(glblcol,0)
                    glblname = append(glblname,glabel)
                    lblidx = length(glblused)
                    tt[ttidx+EQ] = lblidx
                    agchecktt(ttidx,1)
                end for
            end if
        end for
        if opidx!=length(optable) then ?9/0 end if
--end if
    end if -- intellisense=0
end procedure


--global function create_optable()
--global function create_optable(sequence flatsym2)
----
---- Invoked during compilation, not during interpretation, ie this is the logical
---- counterpart to vminit(). Of course when compiling p.exe we expect that all
---- entries will be non-zero, technically we don't need an optable when compiling
---- anything else, but it does not hurt any. Also, some being null should be ok.
---- NB returned offsets are relative to code section and will require relocation.
----
--integer vmi,flags,offset,symidx
--sequence res = repeat(0,length(vm_names))
--sequence glabel
--  if not bind then ?9/0 end if
--  for i=1 to length(vm_names) do
--      glabel = vm_names[i]
--      if not string(glabel) then
--          glabel = glabel[X64+1]
--      end if
--      vmi = find(glabel,glblname)
--      if vmi=0 then
--          offset = 0
--      else
--          flags = glblused[vmi]
----?{flags,G_bang+G_declared,and_bits(flags,G_bang+G_declared),G_bang}
--          if and_bits(flags,G_bang+G_declared)=G_bang then
--              offset = 0
--          else
----                    if and_bits(flags,G_declared)=0 then
----                        fileno = glblabel[vno]
----                        tokline = glblline[vno]
----                        tokcol = glblcol[vno]
----                        Abort("not declared")
------                      Abort(glblname[vno]&" not declared")    -- got a '?'
----                    end if
--              if and_bits(flags,G_set)=0 then ?9/0 end if
--              offset = glboffset[vmi]
--              symidx = glblabel[vmi]
----                offset += symtab[symidx][S_il]
--              offset += flatsym2[symidx][S_il]
--          end if
--      end if
--      res[i] = offset
--  end for
--  return res
--end function

global function create_optable(sequence flatsym2)
--
-- Invoked during compilation, not during interpretation, ie this is the logical
-- counterpart to vminit(). Of course when compiling p.exe we expect that all
-- entries will be non-zero, technically we don't need an optable when compiling
-- anything else, but it does not hurt any. Also, some being null should be ok.
-- NB returned offsets are relative to code section and will require relocation.
--
integer vmi,flags,offset,symidx
--sequence res = repeat(0,length(vm_names))
sequence vmnames = get_vm_names()
sequence res = repeat(0,length(vmnames))
sequence glabel
    if not bind then ?9/0 end if
--  for i=1 to length(vm_names) do
    for i=1 to length(vmnames) do
--      glabel = vm_names[i]
        glabel = vmnames[i]
--      if not string(glabel) then
--          glabel = glabel[X64+1]
--      end if
        vmi = find(glabel,glblname)
        if vmi=0 then
            offset = 0
        else
            flags = glblused[vmi]
--?{flags,G_bang+G_declared,and_bits(flags,G_bang+G_declared),G_bang}
            if and_bits(flags,G_bang+G_declared)=G_bang then
                offset = 0
            else
--                  if and_bits(flags,G_declared)=0 then
--                      fileno = glblabel[vno]
--                      tokline = glblline[vno]
--                      tokcol = glblcol[vno]
--                      Abort("not declared")
----                        Abort(glblname[vno]&" not declared")    -- got a '?'
--                  end if
                if and_bits(flags,G_set)=0 then ?9/0 end if
                offset = glboffset[vmi]
                symidx = glblabel[vmi]
--              offset += symtab[symidx][S_il]
if offset!=0 then
                offset += flatsym2[symidx][S_il]
end if
            end if
        end if
        res[i] = offset
    end for
    return res
end function


-- OLD STUFF [DEV deleteme]
--NO:
--OH CRAP!!! - ok, pcfunc needs a symtabptr associated with every routine_id... and similar for callbacks -- too much work
--  Clearly routine_id() operates on the "currently active" symbol table, whereas for
--  maximum flexibility c_func/call_back,call_proc etc should be "currently active"-
--  agnostic, ie/eg when the pre-compiled pHeap.e invokes a delete_routine() that was
--  set in test.exw, it /could/ just assume the active symtab is the right one, but it 
--  is safer to save (a non-refcounted raw pointer) at the point of obtaining the said
--  routine_id, and use that. When/if shared libaries are implemented, that may become
--  more critical, or we may be forced down a "shared thunk table" approach.
--ALT:

--function optableitem(string glabel)
--integer ki,flags,offset,symidx
--  ki = find(glabel,glblname)
--  if ki=0 then
--      offset = 0
--  else
--      flags = glblused[ki]
----?{flags,G_bang+G_declared,and_bits(flags,G_bang+G_declared),G_bang}
--      if and_bits(flags,G_bang+G_declared)=G_bang then
--          offset = 0
--      else
----                    if and_bits(flags,G_declared)=0 then
----                        fileno = glblabel[vno]
----                        tokline = glblline[vno]
----                        tokcol = glblcol[vno]
----                        Abort("not declared")
------                      Abort(glblname[vno]&" not declared")    -- got a '?'
----                    end if
--          if and_bits(flags,G_set)=0 then ?9/0 end if
--          offset = glboffset[ki]
--          symidx = glblabel[ki]
--          offset += symtab[symidx][S_il]
--      end if
--  end if
--  return offset
--end function

--global 
--constant 
--  vm_maxop = 7,
--  -- builtins\VM\puts1.e:
----    vm_puts1 = 1,       -- :%puts1
----    vm_puts1raw = 2,    -- :%puts1ediesi (32-bit) or :%puts1rdirsi (64-bit)
----    vm_puthex32a = 3,   -- :%puthex32a
----    vm_puthex32 = 4,    -- :%puthex32
----    vm_puthex64 = 5,    -- :%puthex64
----    vm_putsint = 6,     -- :%putsint
----    vm_getc0 = 7,       -- :%getc0
--$
--  if vm_maxop!=7 then ?9/0 end if

--/*
    glblname = {"??(%opRetf)", "%pUnassigned", "??(!iDiag)", "%pBadRetf",
                "%puts1", "%puthex32", "%pAddiii", "??(%pStoreFlt)",
                "%pDiviii", "%pRTErn", "%pDiv0", "%e02atdb0",
                "%e01tcfDivi2", "%e01tcfediMul", "!opCallOnceYeNot",
                "%puts1ediesi", "%puthex32a", "%puthex64", "%putsint",
                "%getc0", "%save_ebp", "%restore_ebp"}

--DEV (maybe)
--  AutoGlabel(opPuts1,     "%puts1",       "VM\\puts1.e",  vm_puts1)

(FOR REFERENCE ONLY) procedure AutoGlabel(integer opcode, string glabel, sequence filename)
-- Maps an internal feature to a global label, eg opFrame wants the equivalent 
--  of #ilASM{ call :%opFrame } which is defined in VM\pStack.e (which in turn
--  needs VM\pHeap.e). :%opRetf (always rqd) is also defined in VM\pStack.e.
-- Note that there is /no/ corresponding symtab entry. 
-- Contrast with getc()/:%opGetc which is handled via AutoAsm().
integer fno
    fno = find(filename,agfiles)
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

:%puts1             ;#0043CDB2:         ;#00014DB2:     -- OUT BY 00428000 [FIXED]
:%puts1ediesi       ;#0043CDBB:         ;#0043CDBB:
:%puthex32a         ;#0043CDCF:         ;#0043CDCF:
:%puthex32          ;#0043CDE9:         ;#0043CDE9:
:%puthex64          ;#0043CE30:         ;#0043CE30:
:%putsint           ;#0043CE7F:         ;#0043CE7F:
:%getc0             ;#0043CEE6:         ;#0043CEE6:
printf(1,"warning: optable[%d] (%s) is 0\n",{?})
--*/
--if length(vm_names)!=vm_maxop then ?9/0 end if

--if X64 then
--  vm_names[find("%puts1ediesi",vm_names)] := "%puts1rdirsi"
--end if

--procedure set_vm_names()
--string glabel
--  vm_names = repeat("??",vm_maxop)
--  vm_names[vm_puts1] = "%puts1"
--  if X64 then
--      glabel = "%puts1rdirsi"
--  else
--      glabel = "%puts1ediesi"
--  end if
--  vm_names[vm_puts1raw] = glabel
----    vm_names[vm_puts1raw] = iff(X64,"%puts1rdirsi","%puts1ediesi")
--  vm_names[vm_puthex32a] = "%puthex32a"
--  vm_names[vm_puthex32] = "%puthex32"
--  vm_names[vm_puthex64] = "%puthex64"
--  vm_names[vm_putsint] = "%putsint"
--  vm_names[vm_getc0] = "%getc0"
--end procedure
--  global constant G_declared  = #01,  -- (absense is only an error when processing an emitted isJmpG)
--                  G_used      = #02,  
--                  G_set       = #04,  -- (set when opLogPos processed)
--                  G_init      = #08,  -- a :>init style label
--                  G_bang      = #10,  -- a :!bang style label
--                  G_stop      = #20   -- a :<stop style label (not yet implemented)
--  ?DEV            G_optable   = #40   -- created from an optable entry (requires different fixup)
--
--  global sequence glblused,   -- G_declared/G_used/G_set
--                  glboffset,
--                  glblabel,   -- if G_declared a currtls else a fileno
--                  glblline,
--                  glblcol,
--                  glblname

--  set_vm_names()
--  res[vm_puts1]       = optableitem("%puts1")
--  if X64 then
--      glabel = "%puts1rdirsi"
--  else
--      glabel = "%puts1ediesi"
--  end if
--  res[vm_puts1raw]    = optableitem(glabel)
----    res[vm_puts1raw]    = optableitem(iff(X64,"%puts1rdirsi","%puts1ediesi"))
--  res[vm_puthex32a]   = optableitem("%puthex32a")
--  res[vm_puthex32]    = optableitem("%puthex32")
--  res[vm_puthex64]    = optableitem("%puthex64")
--  res[vm_putsint]     = optableitem("%putsint")
--  res[vm_getc0]       = optableitem("%getc0")
----    ... (more to follow)
--  for i=1 to vm_maxop do

--      res[i] = optableitem(vm_names[i])

-- follow <isJmpG,0,0,lblidx> processing to generate the optable.. erm, isGaddr...

