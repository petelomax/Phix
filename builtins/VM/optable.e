--
-- builtins\VM\optable.e
-- =====================
--
--  I suppose that technically this is part of p[w].exe rather than part of the VM.
--  [this file is also part of filedumpN.exw, purely for get_vm_names()]
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
--  pHeap.e/pDeleteN.e/pcallfunc.e and to a lesser extent pfileioN.e form another subset
--  that can be very beneficial when shared by executables and dymanically loaded files.
--  Note that routine_id(), and subsequent use in call_proc/func and call_back() all
--  assume that the "currently active" symbol table is appropriate. Hence such things
--  are not generally transferrable between processes or across opInterp and/or dll
--  boundaries, except under that advisement. In some cases, inline assembly can or
--  must be used to directly manipulate symtabptr (aka [ds+8]) to make such work. (:%pGetSymPtr)
--

--include builtins\VM\pStack.e  -- :%opGetST

-- had to do this in pMath.e:
--include builtins\VM\pXor.e        -- not actually used in p.exe, but in optable for pdiagN.e

include builtins\VM\pDeleteN.e
include builtins\VM\pcallfunc.e

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
                                    "%pCrashRtn",
                                    "%pThrow",
                                    "!SetBatchMode",
                                    "!fatalN",
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
                     {"pHeap.e",    {"%pGetTCB",
                                     "%pGetpGtcb",  -- (diagnostics only)
--                                   "%pClearpGtcb",
--                                   "%pRestorepGtcb",
                                     "%pNewGtcbChain",
                                     "%pRestoreGtcbChain",
                                     "%pSetSaveEBP",
--                                   "%pRestoreEBP",
--                                   "%pSetDel",
                                     "%pAllocStr",
                                     "%pWithJS",
                                     "%pAlloClone",
                                     "%pAllocSeq",
                                     "%pLoadFlt",
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
                                     "%pNewStack",
--DEV I think this can/should now go:
                                     ">initStack",
                                     "%opGetArgELF",
                                     "%opFrame",
                                     "%opCallOnce",
                                     "%opRetf", 
                                     "%pFreeStack",
                                     "%opTchk",
                                     "%opTcFail",
                                     "%SetCCleanup",
                                     "%RunCleanup",     -- (DEV may not be necessary) [erm, maybe for p p p p parlour tricks]
                                     "%opAbort"}},
                     {"cbhand.e",   {"%cbhandler",
                                     "!cb_ret"}},
                     {"pDeleteN.e", {"%opDelRtn",
                                     "%opDelete"}},
                     {"pcallfunc.e",{"%opCallFunc",
                                     "%opCallProc",
                                     "!cf_ret"}},
                     -- group 4: optional, except for pDiagN (ie without these, pDiagN will start doing cmp 0's, and
                     --                                       hence some errors may start being incorrectly reported,
                     --                                       but all the non-error stuff should be just fine.)
                     {"pType.e",    {"%opInt",
                                     "%opAtom",
                                     "%opStr",
                                     "%opSq",
                                     "%opObj"}},
                     {"pFPU.e",     {"%down53",
                                     "%up53",
                                     "%near53",
                                     "%trunc53",
                                     "%down64",
                                     "%up64",
                                     "%near64",
                                     "%trunc64"}},
                     {"pFEH.e",     {">initFEH"}},      -- (same as :<exch64 on PE64)
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
                                     "%pSubse1is",
                                     "%pDeSeqip2"}},
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
                                     "%pRepe1is",
                                     "%pDeSeqip"}},
                     {"pRepsN.e",   {"%pReps"}},
--                   {"pRepeatN.e", {"%opRepCh",
--                                   "%opRepeat"}},
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
                                     "!opClosem9",
                                     "!SetSafeMode",
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
--                   {"pcfunc.e",   {"%opOpenDLL",
--                                   "%opDcfunc",
--                                   "%opDcvar",
--                                   "%opCallback",
--                                   "%opCallA",
--                                   "%opCfunc",
--                                   "%opCproc",
--                                   "%opGpct",
--                                   "%opRpct"}},
--*/
                     $
                    }

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
        #ilASM{ 
            [32]
                lea edi,[prevsym]
                call :!opGetST  -- [edi]:=symtab (see pStack.e)
                add esp,4
            [64]
                lea rdi,[prevsym]
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
    end if -- intellisense=0
end procedure


global function create_optable(sequence flatsym2)
--
-- Invoked during compilation, not during interpretation, ie this is the logical
-- counterpart to vminit(). Of course when compiling p.exe we expect that all
-- entries will be non-zero, technically we don't need an optable when compiling
-- anything else, but it does not hurt any. Also, some being null should be ok.
-- NB returned offsets are relative to code section and will require relocation.
--
integer vmi,flags,offset,symidx
sequence vmnames = get_vm_names()
sequence res = repeat(0,length(vmnames))
sequence glabel
    if not bind then ?9/0 end if
    for i=1 to length(vmnames) do
        glabel = vmnames[i]
        vmi = find(glabel,glblname)
        if vmi=0 then
            offset = 0
        else
            flags = glblused[vmi]
            if and_bits(flags,G_bang+G_declared)=G_bang then
                offset = 0
            else
                if and_bits(flags,G_set)=0 then ?9/0 end if
                offset = glboffset[vmi]
                symidx = glblabel[vmi]
                if offset!=0 then
                    offset += flatsym2[symidx][S_il]
                end if
            end if
        end if
        res[i] = offset
    end for
    return res
end function


