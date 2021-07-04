--
-- pEmit2.e
-- ========
--
--constant TRAP = #1144
--constant TRAP = #1158
--constant TRAP = #118C
--constant TRAP = #1234 -- DEV investigate remaining uses thoroughly, then remove.
-- This is the long-awaited final stage: creating the executable. In the case of -c,
--  that means a PE format .exe/.dll, or ELF format file, whereas when interpreting,
--  that means some allocated memory that gets filled with code (ditto data).
--
-- To summarise, we have read and tokenised the source (ptok.e), parsed and generated
--  intermediate code (pmain.e and psym.e), scanned, analysed, and converted that to
--  a "flat" machine code representation (pilx86.e), and lots of other stuff I have 
--  not even mentioned, all before we got to here. If you have not yet done so, then 
--  I must strongly suggest running demo\arwendemo\filedump.exw on a few test files, 
--  to get a feel for the scope and complexity of the task now at hand, and bear in 
--  mind I have kept things as simple as possible! No, really, I have!! Honest!!!
--

--- GOALS (short term)
--  * make symtab[i][S_State] of 0 mean unused/deleted [done]
--  * make symtab[1..15] just 1..15 in the exe, and cope with that as needed. [done]
--  * get T_pathset/T_fileset emit working  [done]
--  * get filedump to show mov edx,[] (hw) [done]
--  * try alternate output orders, to defeat false positives... [done, as constant datab4code]
--  * general cleanup... (get the full test set working first!) [compiled done]
--  * 64-bit hello world [done]
--  * plant h4s in gvar table [done]

constant pathslast = 0  -- (broken/never got working)
                        -- (the idea was that moving an app from one directory or machine to
                        --  another, should not suddenly make it stop (or start) working.)

--global -- now needed in pbinary.e (23/3/16)
constant mapsymtab = 01 -- (implies newEmit) [expected to be 1 in all releases][DEV] [DEV broken]
integer symtabmax
--global -- now needed in pbinary.e (23/3/16)
sequence symtabmap
--constant mapgvars = 0
--sequence gvarmap (we are assigning them here anyway...)
--DEV newEBP (I think this can go, what with filedump.exw 'n all)
--constant debug = 0

--DEV broken:
--constant show_full_symtab = 0 -- You should NOT want or need to set this to 1.
                                -- (It was used to improve the use of opCleanup.)
                                -- If anything, if you are new to symtab dumps you
                                -- probably want a cut down version - by using the
                                -- "-nodiag" command line option.

constant isShortJmp = #010000   -- isJmp that has been found to fit in a byte.
                                -- (fixup as isJmp, patch 5->2 or 6->2)
                                -- (not global: use only in scanforShortJmp/blurph, 
                                --  and not before, but otherwise like isJmp etc,
                                --  and therefore must not clash with pglobal.e)

--
-- Fixup code: branch straighten, check for dword offsets that fit in a byte,
-- ==========   calculate actual code size, convert opcode indexes to dword
--              offsets into VM, relative addresses and variable indexes into
--              absolute addresses, and adjust all other offsets as necessary
--              by the changing of any dword->byte form instructions. (whew)
--
-- if binding then create .exe file (using specified resource file) else
-- if interpreting just poke to memory and execute it immediately.
--
-- if binding, then we need to write symtab to section 6, whereas
-- if interpreting we can just use the existing symtab directly.
--
-- Note that if binding, there is no source at the point of execution, hence
--  there can be no trace, profile, or profile_time going on.
--
--DEV
-- Also note that critical parts of this process, specifically those where
--  performance matters and anything to do with licencing issues, have been
--  coded in low level assembly (not open source for obvious reasons).
--

-- Technical note/sug: it may be sensible to generate separate constant values by source file,
--  to mitigate/minimise the locking requirements on shared constants, which never felt right.
--  Obviously, if pglobals.e defines global constant phixversion = {0,6,3} then that (single)
--  value is used wherever "phixversion" is used, and the app must be thread-safe/lock aware
--  if it is indeed using "phixversion" for whatever reason in separate threads. However, if 
--  barney.e also happens to use "{0,6,3}", that should not interfere in any way whatsoever 
--  with phixversion, but as things stand, it /does/, /even/ for constant private = {0,6,3}... [DEV]

--dev/sug:
--  Note: #ilASM treats [0], [4], etc as offsets into the data section. If for some reason you
--  (still, given that DOS days are long since gone) want to reference a fixed literal address,
--  then you must code it using something more like "mov eax,#40000", "mov edx,[eax]".

-- Data Section
-- ============
--  The data section of a Phix-generated executable contains the following:
--  sigPhx, The signature and layout fields really only exist for communication between pemit2.e 
--  layout  and filedump.exw, and in fact this was (re-)written with the latter firmly in mind.
--          The value of layout has no specific meaning; it can be eg a version, or a bitfield.
--          Note these are 4+4 bytes on both 32 and 64-bit, whereas all following 4/h4 go 8.
--          Update: on file, layout is as above some small number for the use of filedump.exw,
--          but at runtime, that dword [ds+4] is used to hold a 32-bit random number seed.  [[ DEV why?  see VM\pRand.e (only file that uses it/use a local var in there instead) ]]
--  symptr  When this is non-zero (see below), it should be maxgvar*4+40, on 32-bit,
--          or *8+72 on 64-bit, ie it locates symtab[1] rather than slack/maxlen/length etc.
--27/2/15:
--          *NB* For nested opInterp/thread safety, use [vsb_root+8/16] in preference to this.
--23/3/15: (maybe)
--          Note that maxgvar/gvarptr are correctly located via [symptr][T_EBP].
--          Obviously they are right here when compiled, but things get switched about when
--          interpreting...
--DEV: maxop/optable are deprecated; this is now done via symtab[T_optable]
--   (I am however far more inclined to leave a maxop of 0 unused than reclaim it) [DEV will be reclaimed for relocs:]
--< maxop   The number of entries in the following table, set from pops2.e/maxNVop2?. [DEV]
--< optable When interpreting, there is already a copy of the heap manager, file i/o, printf(), 
--<         subscripting, append/prepend, peek/poke, and so on, already in memory, so we may as 
--<         well use them and save ourselves some time. Note that a maxop of zero is perfectly 
--<         valid, as is any optable[n], either of which trigger appropriate recompilation. 
--<         The lack of an opInterpret is also a fair reason to deliberately set maxop to 0.
--<         (At the time of writing, there are no confirmed entries, just ideas/suggestions.)
--> relocs  When this is non-zero (dll only) points to a reloc-ref table (0=done/none)
--  maxgvar The number of entries in the following table.
--DEV       *NB* the correct way to locate maxgvar/gvars[N] is via symtab[T_ds4], since that
--          will match the symtab located via symtabptr @ [ds+8], whereas this ([ds+16] or so)  --DEV :%pGetSymPtr
--          matches the one as compiled, and often "wrong" for code invoked via the optable.
--  gvars   A gvar is a static global/file-level variable or constant. Each occupies precisely
--          one slot in memory, which is shared by all threads (requiring, of course, that the
--          application use approriate locking) and is unaffected by recursion. In contrast, 
--          tvars, which include all parameters and local variables, exist on the (virtual, 
--          heap-based) stack, so they can hold private values for different nested/recursive 
--          invocations, and likewise are owned and can only be accessed by a single thread.
--          Note that constants may be duplicated on a per-source basis, to reduce the locking
--          requirements/gotchas for multiple threads, especially on reference count updates.
--          (The precise details of such duplications are yet to be designed/confirmed. [DEV])
        --          (So pmain.e/psym.e must create additional symtab entries to contain the different
        --           S_FPno (plus an S_Slink for var idx, as set below), and this program creates the
        --           duplicate copies for each different S_FPno it encounters on the S_Clink chain.
--           As always, the primary driving force here is that what works in standalone code
--           works just as well when it is incorporated into a larger program, and hence any
        --           duplication is to keep multiple instances of literals such as "hello" separate, 
        --           as opposed to a global constant hello="hello" being used in more than one file.)   [ DEV: still not happy with this...]
        --          BIG PROBLEM: things like {}.......
        --          TIP: use repeat(0,0) instead of {} and repeat(' ',0) instead of "" to ensure thread safety... [see test ideas in _TODO.TXT]
--          A reference to gvar[n] can be resolved as (max[NV]op+n)*4+16, or *8+24 on 64-bit, 
--          obviously fixup()'d in pbinary.e relative to the start of the data section. Note
--          that when interpreting, a gvar[n] reference directly uses the symtab[n][S_value].   --DEV what about unassigned?
--  symtab  The symbol table, required for diagnostics, dynamic routine-id, etc. If you have 
--          not yet done so, I must recommend running "p -d -nodiag test.exw" on a few snippets
--          of code, and examining the resulting list.asm, as well as skimming pglobals&psym.e.
--          Note that, unlike optable and gvars, the symtab is a bona fide Phix-style sequence,
--          with a full header including type, length, etc. This allows it to be used directly
--          in hll code (pdiag, prtnid, etc) both when the program is interpreted and compiled.
--          The one exception is [S_value], which may contain the unintialised value #40000000,
--          something which a genuinue hll-built sequence could never possibly contain.
--          The compiler (psym.e/syminit()) preloads the symbol table with over 500 entries at
--          the start, so it ain't exactly small, no matter how hard I try to remove duplicates
--          and unused entries. If -nodiag is specified, and such things as routine_id are not 
--          used, or resolved at compile-time, it may be possible to omit the symbol table, and 
--          set the pointer at offset 8 to zero, but that has not yet been attempted or tested.         **DEV (symptr of 0, for smaller executables) [low priority]
--          Update: note that pcmdln.e uses symtab[T_cmdlnflg] and symtab[T_fileset][1], and
--          abort([0]) also uses symtab[T_cmdlnflg] to decide whether to ExitProcess or ret.    [BLUFF/DEV]
--          I will however say that an extra 100K+ per executable (or 0.00004% or 1/2,500,000th
--          of a these-days-quite-small-250Gb drive) is a small price to pay for human-readable 
--          error messages and being able to jump directly to the offending source code line.
--  misc    float/string/sequence element values for gvar/symtab[i]. Note that some of these
--          may have/need a reference count >1. These are deliberately emitted after everything
--          else, with appropriate entries "backpatched" as we do so, in order to keep the
--          "locate x" code as simple as possible, and make life easier for filedump.exw. Also
--          note these are emitted "inside out", for example if the program source contains say 
--          38 instances of a variable/parameter named "count", the executable/symbol table
--          only needs one "count" label/string, quite possibly with a reference count of 38.
--          This is achieved using tt_traverse() and (eg) ReconstructSequence(), that is instead
--          of "for i=1 to length(symtab/gvars) do recursive_dump(i)" or somesuch.
--
--  32-bit:
--  00000000    sigPhx              x4  "Phix"
--  00000004    layout              h4  #00000001
--  00000008    symptr              h4  #00001024   (raw address of symtab[1], h8 on 64-bit)
--              -                                   opcodes
--< 0000000C    maxop               4   238         (0 /is/ valid, at #00000010 on 64-bit)
--< 00000010    opInit              h4  #0000204C   opcode[1] (entry point, in code section)
--<                         ...
--> 0000000C    relocs              4   #00003047   raw ptr to ref relocs (dll only, 0=done/none)
--              -                                   gvars:
--< 000003C4    maxgvar             4   785         (at maxop*4+16, or *8+24 on 64-bit)
--> 00000010    maxgvar             4   785         size of following table
--              gvar[1]             h4  #40004016   (name replaced, except for temps)
--              gvar[2]             h4  #4000406C   (see #000101B0)
--              gvar[3]             h4  #00000001   1
--                          ...
--              -                                   symtab (at maxgvar*4+20, or *8+32)
--  00001010    slack               4   0           (should be 0)
--  00001014    maxlen              4   6836        (should be length*4+20, or *8+40)
--  00001018    length              4   1704
--  0000101C    refcount            4   1           (should be 1, some later on may be >1)
--  00001020    type                h4  #80000000   (should be #80000000)
--  00001024    symtab[n] (name)    h4  #40230023   (see #008C008C) [0 is valid too]
--                          ...
--              -                                   gvar[n]/symtab[m]
--  00002010    slack               4   0           (should be 0)
--  00002014    maxlen              4   56          (should be length*4+20, or *8+40)
--  00002018    length              4   12
--  0000201C    refcount            4   1           (should be 1)
--  00002020    type                h4  #80000000   (should be #80000000)
--  00002024    S_Name              h4  #40027436   <desc> eg i (#00004024) [-1 /is/ valid]
--  00002028    S_NTyp              h4  #00000001   S_Const
--  0000202C    S_FPno              4   1
--  00002030    S_State             h4  #00000001   <copy csome code from plist.e>
--                          ...
--  00003047    reloc offset table  1   0           bytes 4..255 are offsets,
--                                                  (ie offset+=byte; dword[ds+offset]+=refdiff)
--                                                  (3 unused/illegal), 
--                                                  2=word offset follows
--                                                  1=dword offset follows
--                                                  0=table terminator(byte)
--/*
                S_Nlink = 5,    -- name chain (see below)
                S_Slink = 6,    -- scope/secondary chain (see below)
                -- constants and variables [S_NTyp<=S_TVar]
                S_vtype = 7,    -- variable type [see notes below]
                S_value = 8,    -- value [see note below]
                S_Clink = 9,    -- constant chain (S_NTyp=S_Const only, see below)
                S_Tidx  = 9,    -- thread idx (S_NTyp=S_Tvar only)
                S_ErrV  = 10,   -- {'v', file, line, col}; see pmain.e[-35]
--DEV not newEmit:
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
                S_1stl  = 13,   -- first line
                S_Efct  = 14,   -- side effects
                S_ErrR  = 15    -- {'R', file, line, col}; see pmain.e[-60]
--*/
--  64-bit:
--  00000000    sigPhx              x4  "Phix"
--  00000004    layout              h4  #00000001
--  00000008    symptr              h8  #0000000000000068   (raw address of symtab[1])
--              -                                           opcodes
--< 00000010    maxop               8   1                   (0 /is/ valid)
--< 00000018    opInit              h8  #000000000000204C   opcode[1] (entry point, in code section)
--<                         ...
--> 00000010    relocs              8   #0000000000003047   raw ref relocs (dll only, 0=done/none)
--              -                                           gvars
--< 00000020    maxgvar             8   3                   (at maxop*8+24)
--< 00000028    gvar[1]             h8  #4000000000004016   (name replaced, except for temps)
--< 00000030    gvar[2]             h8  #400000000000406C   (see #000101B0)
--< 00000038    gvar[3]             h8  #0000000000000001   1
--> 00000018    maxgvar             8   3                   (at maxop*8+24)
--> 00000020    gvar[1]             h8  #4000000000004016   (name replaced, except for temps)
--> 00000028    gvar[2]             h8  #400000000000406C   (see #000101B0)
--> 00000030    gvar[3]             h8  #0000000000000001   1
--                          ...
--              -                                           symtab (at (maxop+maxgvar)*8+32)
--  00000040    slack               8   0                   (should be 0)
--  00000048    maxlen              8   248                 (should be length*8+40)
--  00000050    length              8   26
--  00000058    refcount            8   1                   (should be 1, others may be >1)
--  00000060    type                h8  #8000000000000000   (should be #8000000000000000[+1])
--  00000068    symtab[n] (name)    h8  #4000000000230023   (see #008C008C) [0 is valid too]
--                          ...
--              -                                           gvar[n]/symtab[m]
--  00002010    slack               8   0                   (should be 0)
--  00002018    maxlen              8   116                 (should be length*8+20)
--  00002020    length              8   12
--  00002028    refcount            8   1                   (should be 1)
--  00002030    type                h8  #8000000000000000   (should be #8000000000000000[+1])
--  00002038    S_Name              h8  #4000000000027436   <desc> eg i (#00004024) [-1 /is/ valid]
--  00002040    S_NTyp              h8  #0000000000000001   S_Const
--  00002038    S_FPno              8   1
--  00002050    S_State             h8  #0000000000000001   <copy some code from plist.e>

--integer maxop
integer maxgvar
--  
--string mzpe
--integer 
--   ImageBase          -- dword @ #B4, must be #400000
--integer SectionAlignment, -- dword @ #B8, must be #1000
--      FileAlignment,  -- dword @ #BC, must be #200 
--      SAless1,            -- #00000FFF for rounding to section alignment
--      SAmask,         -- #FFFFF000
--      FAless1,            -- #000001FF for rounding to file alignment
--      FAmask,         -- #FFFFFE00
--      SubsystemVersion,   -- dword @ #C8, must be ssv310 or ssv400
--      SizeOfImage,        -- dword @ #D0, rounded up to SectionAlignment, eg #8000
--      Subsystem,      -- word @ #DC, must be CUI or GUI
--      ITaddr,         -- RVA Import Table address, == ISvaddr         -- dword @ #100, eg #2000
--      ITsize,         -- RVA Import Table size, == ISvsize            -- dword @ #104, eg #31D
--      RTaddr,         -- RVA Resource Table address == RSvaddr        -- dword @ #108, eg #5000
--      RTsize          -- RVA Resource Table size == RSvsize           -- dword @ #10C, eg #504
--integer 
--   -- 6 sections:                                     -- offset:examples     vsize      vaddr      rsize      raddr
--   DVvsize, DVvaddr, DVrsize, DVraddr, -- data for vm (fixed)         -- #180:#54C  #184:#1000 #188:#600  #18C:#400
--   ISvsize, ISvaddr, ISrsize, ISraddr, -- import section (fixed)      -- #1A8:#31D  #1AC:#2000 #1B0:#400  #1B4:#A00
--   VMvsize, VMvaddr, VMrsize, VMraddr, -- the virtual machine (fixed) -- #1D0:#1ED8 #1D4:#3000 #1D8:#2000 #1DC:#E00
--   RSvsize, RSvaddr, RSrsize, RSraddr, -- resource section (var len)  -- #1F8:#504  #1FC:#5000 #200:#600  #204:#2E00
--   CSvsize, CSvaddr, CSrsize, CSraddr, -- user code (var start & len) -- #220:#10   #224:#6000 #228:#200  #22C:#3400
--   DSvsize, DSvaddr, DSrsize, DSraddr  -- user data (var start & len) -- #248:#4    #24C:#7000 #250:#200  #254:#3600
--
--constant
--       ssv310 = #000A0003,    -- SubsystemVersion: 3.10
--       ssv400 = #00000004,    -- SubsystemVersion: 4.00
--       CUI = 3,               -- Subsystem: console app
--       GUI = 2                -- Subsystem: gui app
--
constant --WORD = 2,
         DWORD = 4, 
         QWORD = 8
--       bytemul = {0,#100,#10000,#1000000}

--DEV *2
--global 
--string divm   -- used by p2asm.e if dumpVM=1

-- verify the compiler is working properly:
--!/**/ #isginfo{divm,0b0100,MIN,MAX,integer,-2}    -- 0b1000 better?! (see aside in readdivm())
--!/**/ #isginfo{divm,0b1000,MIN,MAX,integer,-2}    -- (nb 0b1100 (ie 12) is /worse/ )
--!/**/ #isginfo{divm,0b1100,MIN,MAX,integer,-2}    -- I can live with this...
--!/**/ #isginfo{divm,0b1000,MIN,MAX,integer,-2}    -- Yay! (23/02/10)
--!/**/ #isginfo{divm,0b1000,MIN,MAX,integer,-1}    -- OK? (24/06/10)
--!/**/ #isginfo{divm,0b1000,MIN,MAX,integer,-2}    -- Yay! (18/01/12)

--function divmDword(integer i)
---- NB: i is 0-based
--  return divm[i+1]+divm[i+2]*#100+divm[i+3]*#10000+divm[i+4]*#1000000
--end function

--procedure setdivm(integer offset, atom v, integer dsize)
---- breakup v into dsize bytes in divm at offset.
---- used I think only to locate symtab and threadstack.
---- Note that offset as passed is 0-based, adjusted here(+1) to index divm.
---- dsize is WORD or DWORD
--  for i=1 to dsize do
--      divm[offset+i] = and_bits(v,#FF)
--      v = floor(v/#100)
--  end for
--end procedure

integer outfn,
--      fnr,
--      asmoptions,
--      vmaxpos
$

--sequence Names            -- eg {"kernel32.dll",...}
--sequence HintNames        -- eg {{{#40470,...},{"AllocConsole",...}},{..}}
--sequence thunkaddrs   -- \ scratch vars, set from HintNames[i],
--sequence thunknames   -- / where i is eg find("kernel32.dll",Names)


--constant dorsrc = 0
--sequence rsfilename
--integer rsrcRSraddr, rsrcRSrsize, rsrcRSvsize, rsrcCSvaddr, rsrcCSraddr

-- used in p2asm.e, plist.e
--DEV (USING THE ONE IN PEMIT.E!)
--global sequence code_section
--  code_section = repeat(0,rand(1000)) -- DEV Temp!

--sequence data_section
--type dst(sequence d)
--  return string(d)
--end type
--dst data_section
sequence data_section
--DEV (7/7/17) broken on lnx64...
--!/**/ #isginfo{data_section,0b1000,MIN,MAX,integer,-2} -- verify this is a string

function isString(object x)
-- avoid "probable logic errors" testing that data_section really is a string
--  (because p.exw contains "without type_check"....)
    return string(x)
end function
if isString(0) then end if  -- and prevent the compiler from optimising it away!

constant m4 = allocate(4),
         m44 = {m4,4},
         m42 = {m4,2}

procedure setcsDword(integer i, atom v)
-- set a dword in code_section
-- NB: offset passed here is 1-based index
    poke4(m4, v) -- faster than doing divides etc. (idea from database.e)
    code_section[i..i+3] = peek(m44)
end procedure

--DEV setcsQword?

--function gets5Dword(integer i)
---- used to get routine no for patching parameter info on forward calls
---- NB: i is 1-based
--  return s5[i]+s5[i+1]*#100+s5[i+2]*#10000+s5[i+3]*#1000000
--end function

--procedure sets5Dword(integer i, atom v)
---- used for patching parameter info on forward calls
---- NB: offset passed here is 1-based index
--  poke4(m4, v)
--  s5[i..i+3] = peek(m44)
--end procedure

--DEV sets5Qword?

function getdsDword(integer i)
-- used to get refcount for subsequence/substring patch
-- NB: i is 1-based (** unlike setdsDword **)
    return data_section[i]+data_section[i+1]*#100+data_section[i+2]*#10000+data_section[i+3]*#1000000
end function

--NO: get two dwords independently
--      only properly useful for (31-bit, or at least <53-bit) integer results
function getdsQword(integer i) -- (integer result, see notes below [DEV])
-- NB: i is 1-based (** unlike setdsDword **)
--atom res = data_section[i+7]
integer res = data_section[i+7]
    for j=i+6 to i by -1 do
        res = res*#100+data_section[j]
    end for
    return res
end function

procedure setdsDword(integer i, atom v)
-- NB: i is 0-based index
    poke4(m4, v)
    data_section[i+1..i+4] = peek(m44)
end procedure

-- CAUTION:
--  In a purely 32-bit world, 64-bit floats have 53 bits of precision, so eg poke4(addr,#8000_0001) is fine.
--  In a purely 64-bit world, 80-bit floats have 64 bits of precision, so the equivalent is also fine.
--  However, a 32-bit app trying to poke8(a,#8000_0000_0000_0001), as might happen when a 32-bit compiler
--  is asked to produce a 64-bit executable, is, quite simply, going to go badly wrong.
--  The far better idea is to poke two dwords independently, without ever adding/shifting things 'n wotnot.

--DEV make me a builtin/autoinclude:
--global
--procedure poke8(atom addr, object v)
--atom vi
--  if atom(v) then
--      poke4(addr,and_bits(v,#FFFFFFFF))
--      poke4(addr+4,floor(v/#100000000))
--  else
--      for i = 1 to length(v) do
--          vi = v[i]
--          poke4(addr,and_bits(vi,#FFFFFFFF))
--          addr += 4                       
--          poke4(addr,floor(vi/#100000000))
--          addr += 4                       
--      end for
--  end if
--end procedure

--7/4/16 values such as -#FFFFFFFF are now valid for 32-bit p.exe creating 64-bit exe's:
--procedure setdsQword(integer i, integer v)
procedure setdsQword(integer i, atom v)
-- NB: i is 0-based index
    if isFLOAT(v) then ?9/0 end if
    poke4(m4, and_bits(v,#FFFFFFFF))
    data_section[i+1..i+4] = peek(m44)
    poke4(m4, floor(v/#100000000))
    data_section[i+5..i+8] = peek(m44)
end procedure

integer dsidx -- 1-based index to data_section
integer dsize   -- DWORD or QWORD

--procedure setds(atom v, integer sethigh=0, integer didx=dsidx)    -- (DEV/SUG)
procedure setds(atom v, integer sethigh=0)
--DEV a bit of #ilASM might be even better...
-- set a dsize value at dsidx to v.
-- if the target should be a ref, we add #80000000 here, which is 
-- "shr2+#20000000" in pbinary.e to create a "shr2+#40000000" ref.
    if dsize=DWORD then
        if sethigh then v += #80000000 end if
        poke4(m4, v)
        data_section[dsidx..dsidx+3] = peek(m44)
        dsidx += 4
    else
        poke4(m4, and_bits(v,#FFFFFFFF))
        data_section[dsidx..dsidx+3] = peek(m44)
        dsidx += 4
        v = floor(v/#100000000)
        if sethigh then v += #80000000 end if
        poke4(m4, and_bits(v,#FFFFFFFF))
        data_section[dsidx..dsidx+3] = peek(m44)
        dsidx += 4
    end if
end procedure

procedure appenddsDword(atom v)
-- (aside: v is an atom mainly for 32-bit p.exe creating 64-bit exe's,
--         but otherwise should always be integer for 32->32 & 64->64)
string s
    if isFLOAT(v) then ?9/0 end if
    poke4(m4, v) -- faster than doing divides etc. (idea from database.e)
    s = peek(m44)
    data_section &= s
    if X64 then
        v = floor(v/#100000000)
        poke4(m4, v)
        s = peek(m44)
        data_section &= s
    end if
end procedure

procedure appenddsType(integer t)
--DEV 30/7/2013 plant a dummy (illegal) delete_routine:
--  data_section = append(data_section,0)
    data_section = append(data_section,1)
    data_section = append(data_section,0)
    data_section = append(data_section,0)
    if X64 then
        data_section = append(data_section,0)
        data_section = append(data_section,0)
        data_section = append(data_section,0)
        data_section = append(data_section,0)
    end if
    data_section = append(data_section,t)
end procedure

procedure appenddsBytes(sequence s)
integer ch
    for i=1 to length(s) do
        ch = and_bits(s[i],#FF)
        data_section = append(data_section,ch)
    end for
--if not isString(data_section) then ?9/0 end if
end procedure

procedure APIerror(integer i, string msg)
sequence x = APIerritem[i]
    fileno = x[1]
    tokline = x[2]
    tokcol = x[3]
    Abort(msg)
end procedure

without trace

integer d_addr
sequence s5sizes, s5v
--, s5symn

integer thisCSsize

--integer vi        -- index into symtab
integer vi_active       -- index into symtab

integer opbyte, i3

constant call_rel32     = #E8           -- 0o350 imm32              -- call rel32
constant jump_rel32     = #E9           -- 0o351 imm32              -- jmp rel32

--with trace
procedure scanforShortJmp(integer vi)
--
-- At heart, this routine scans for/counts/flags any possible
--        jmp dword (#E9 xx xx xx xx) to jmp byte (#EB xx)  (ie 5->2)
--    and jcc dword (#0F 8x xx xx xx xx) to jcc byte (#7x xx)  (ie 6->2)
-- obviously, when the offset will fit in a byte, that is.
--
-- It also removes "isDead" blocks, including associated fixups to the LineTab,
--  links up routines to be processed/fixups opFrame parameter info, and 
--  just to be flash about it, performs branch straightening...
--
-- Note that while all offsets are adjusted by isDead removed, we cannot be 
--  certain of the final values as affected by isShortJmp until the loop quits
--  (because someDone=0), by which time it is too late, hence we recalculate 
--  them in (the eloquently named) blurph(), using the linked lists setup here.
--
integer short52,    -- master counts for entire routine
        short62,    -- ""
--dead5j,
--dead6j,
        someLeft,   -- more to do: phase 1: 1=some backward jumps remain
                    --                      2=some fwd but no backward jumps
                    --  phase 2: resets 2->0 as it rescans the fwd (only).
                    -- else 0 means we've done the lot (all isShortJmp'd)
        someDone,   -- outer loop control (phase 3)
        jmpOpRetf,  -- control flow flag for branch straightening to opRetf, phase 1
        i,          -- gp idx to s5 (+1 in phase 1, linked list in phase 2/3)
        c,          -- s5[i] etc. nb long active liverange, take special care
        c2,         -- scratch version of c when we don't want to damage it.
        k,          -- gp dogsbody
--      kfirst,     -- copy of [S_Parm1]
        klast,      -- jmp tgt limit for chainwalks
        s5len,      -- length(s5)
        deadCode,   -- phase 1:flag, phase 2: LineTab fixup & final sanity check
        first,      -- start of linked list of isDead/isJmp
        last,       -- end of linked list of isDead/isJmp
        next,       -- scratch chainwalk var
        prev,       -- ""
        chunkend,   -- block end for code packing (phase 2)
        oidx,       -- output idx for code packing (phase 2)
        ltj         -- LineTab entry (phase 2)

integer offset      -- offset of the jump currently being examined
--atom offset           -- offset of the jump currently being examined

--object symk       -- symtab entry for opFrame patch (phase 1)
--integer u         -- "" used flag

--trace(1)
    --
    -- Phase 1 - dead code removal: linkup all isDead and isJmp entries,
    -- =======                      correct offsets on backward jumps by isDead jumped over,
    --                              fixup opFrame/add to chain of routines to process, (now done in pilx86.e)
    --                              and perform branch straightening.
    --
    --          (phase 2 corrects fwd jmp offsets and removes isDead blocks)
    --          (phase 3 completes the isJmp --> isShortJmp flagging)
    --
    -- A small pseudo-example:
    --
    --   s5 input:                       s5 output (after all 3 phases):
    --  1: {1A,2B,3C,4D,                1: {1A,2B,3C,4D,
    --  5:  isDead+3,0,0,               5:  8E,9F,10,
    --  8:  8E,9F,10,                   8:  isShortJmp,15,0,-8,
    --  11: isJmp,0,0,-11,              12: 15,16,17,
    --  15: 15,16,17,                   15: isShortJmp,0,8,3,
    --  18: isJmp,0,0,6,                19: 22,23,24,
    --  22: 22,23,24,                   22: 28,29,30,31}
    --  25: isDead+3,0,0,
    --  28: 28,29,30,31}
    --
    --  Where nn: is just a guide index to show where s5[nn] is,
    --        1A,2B,3C,4D, .. 31 represent as-is x86 binary,
    --        the -11 and 6 after isJmp are relative offsets, which
    --        as you can see are adjusted to -8 and 3 respectively
    --        after removal of the isDead blocks they jump over[*1],
    --        the 15 and 8 after isShortJmp [*2] are indexes to
    --        the next/prev is[Short]Jmp.
    --        Note [*1] jump offsets are /not/ adjusted for jumping
    --                  over isShortJmps here, but in blurph().
    --             [*2] temporary, ie those two byte positions will 
    --                  be ignored/clobbered in blurph().
    --  The updates as shown above are performed "in situ" within s5.
    --


    short52 = 0
    short62 = 0
--dead5j = 0
--dead6j = 0
if q86 then
--trace(1)
    s5len = length(s5)-5
    deadCode = 0
    someLeft = 0
    first = s5[s5len+1]
    last = s5[s5len+2]
--DEV if q86 is 2 then +3/+4 are other (ie isIL etc) first/last
    i = first
    while i do
        c = s5[i]
next = s5[i+1]
--DEV no longer used under oldil...
        if c>=isDead then
            c -= isDead
            if DEBUG then
                if c<3 then ?9/0 end if
            end if
            deadCode = 1    -- flag some found
-- 14/4/10 all isAddr now left as-is, otherwise it fouls up error handling...
        else -- isAddr/isJmp
--      elsif c!=isAddr then
            i3 = i+3
            offset = s5[i3]
            --
            -- Branch Straighten:
            --  (NB: fairly obvious but worth stating, branch straightening 
            --       does not occur if there is an opLnt/p/pt in the way.)
            --
            jmpOpRetf = 0
--DEV 21/11/10 I think this is here because it messes up error handling?
--if c!=isAddr then
if c>isAddr then
            while 1 do
                c2 = i+4+offset -- target addr
                opbyte = s5[c2]
                if opbyte!=jump_rel32 then exit end if
                -- target is an unconditional jump:
                c2 += 1
                opbyte = s5[c2]
                c2 += 3
--DEV try c2 = s5[c2] here...
                if opbyte<isJmp or opbyte>isShortJmp then   -- nb backwd jump may hit an isShortJmp
                    if opbyte=isOpCode and s5[c2]=opRetf then
-- we need to unlink this from our chain...
prev = s5[i+2]
if prev then
    s5[prev+1] = next
else
    first = next
end if
if next then
    s5[next+2] = prev
end if
                            -- special case: jmp/jcc,isJmp,0,0,offset to #E9,isOpCode,0,0,opRetf:
                            --      -->      jmp/jcc,isOpCode,0,0,opRetf (nb opRetf only)
                            s5[i] = isOpCode
puts(1,"pemit2.e line 1306 (opRetf)\n")
?9/0
--                  (s5[8]!=isJmpG or s5[11]!=tt[aatidx[opRetf]+EQ])) then

                            s5[i3] = opRetf
--                      end if
                        jmpOpRetf = 1   -- no linkup, resume in outer loop
                    end if
                    exit
                end if
                c2 = s5[c2]+5
                offset += c2        -- ;-)
                s5[i3] = offset
--31/1/21
if c2=0 then exit end if
            end while
end if
if c!=isBase then
            if not jmpOpRetf then
                if offset<=0 then
                    --
                    -- walk back down the chain, adjust offset by any isDead we leap over:
                    --
                    k = s5[i+1]
                    klast = i+4+offset
                    while k and k > klast do
                        c2 = s5[k]
                        if c2>isDead then
                            c2 -= isDead
                            offset += c2
                        end if
                        k = s5[k+2]     -- k:=prev
                    end while
                    s5[i3] = offset
--24/4/2013:
--                  if c!=isAddr then
                    if c!=isAddr 
                    and s5[i-1]!=call_rel32 then  -- (there is no "call byte offset" instruction on the x86, only jmp)
                        -- test as if this becomes a short jump:
                        if s5[i-1]=jump_rel32 then
                            k = (offset>=-131)  -- += 3
                        else
                            k = (offset>=-132)  -- += 4
                        end if
                        if k then
--if i=13 then ?9/0 end if
                            s5[i] = isShortJmp
                            if s5[i-1]=jump_rel32 then
                                short52 += 1
                            else
                                short62 += 1
                            end if
                        else
                            someLeft = 1
                        end if
                    end if
                elsif not someLeft then
                    someLeft = 2
                end if
            end if
end if
        end if
--      i = s5[i+1]
        i = next
    end while
else -- (not q86)
?9/0 --isBase will not work here
    s5len = length(s5)
    deadCode = 0
    someLeft = 0
    first = 0
    last = 0
    i = 1
    jmpOpRetf = 0
    while i<=s5len do
        c = s5[i]
        if c<isOpCode then  -- as-is byte
            i += 1
--      elsif c=isOpCode then
--          i3 = i+3
--          c = s5[i3]
--          i += 4
--      elsif c<isAddr then -- c=isVar/isILa/isIL
--      elsif c<isAddr then -- c=isOpCode/isVar/isILa/isIL
        elsif c<isAddr then -- c=isOpCode/isAPIFn/isVar/isConstRef/isConstRefCount/isILa/isIL
            i += 4
        else                -- c=isAddr/isJmp/isDead (no isShortJmp yet, btw)
--DEV no longer used under oldil...
            if c>=isDead then
                c -= isDead
                if DEBUG then
                    if c<3 then ?9/0 end if
                end if
                deadCode = 1    -- flag some found
            else -- isAddr/isJmp
                i3 = i+3
                offset = s5[i3]
                --
                -- Branch Straighten:
                --  (NB: fairly obvious but worth stating, branch straightening 
                --       does not occur if there is an opLnt/p/pt in the way.)
                --
                while 1 do
                    c2 = i+4+offset -- target addr
                    opbyte = s5[c2]
                    if opbyte!=jump_rel32 then exit end if
                    -- target is an unconditional jump:
                    c2 += 1
                    opbyte = s5[c2]
                    c2 += 3
                    if opbyte<isJmp or opbyte>isShortJmp then   -- nb backwd jump may hit an isShortJmp
                        if opbyte=isOpCode and s5[c2]=opRetf then
                            -- special case: jmp/jcc,isJmp,0,0,offset to #E9,isOpCode,0,0,opRetf:
                            --      -->      jmp/jcc,isOpCode,0,0,opRetf (nb opRetf only)
puts(1,"pemit2.e line 1421 (opRetf)\n")
?9/0
--                  (s5[8]!=isJmpG or s5[11]!=tt[aatidx[opRetf]+EQ])) then
                            s5[i] = isOpCode
                            s5[i3] = opRetf
                            i += 5
                            jmpOpRetf = 1   -- no linkup, resume in outer loop
                        end if
                        exit
                    end if
                    c2 = s5[c2]+5
                    offset += c2        -- ;-)
                    s5[i3] = offset
                end while

                if not jmpOpRetf then
                    if offset<=0 then
                        --
                        -- walk back down the chain, adjust offset by any isDead we leap over:
                        --
                        k = last
                        klast = i+4+offset
                        while k and k > klast do
                            c2 = s5[k]
                            if c2>isDead then
                                c2 -= isDead
                                offset += c2
                            end if
                            k = s5[k+2]     -- k:=prev
                        end while
                        s5[i3] = offset
                        if c!=isAddr then
                            -- test as if this becomes a short jump:
                            if s5[i-1]=jump_rel32 then
                                k = (offset>=-131)  -- += 3
                            else
                                k = (offset>=-132)  -- += 4
                            end if
                            if k then
--if i=13 then ?9/0 end if
                                s5[i] = isShortJmp
                                if s5[i-1]=jump_rel32 then
                                    short52 += 1
                                else
                                    short62 += 1
                                end if
                            else
                                someLeft = 1
                            end if
                        end if
                    elsif not someLeft then
                        someLeft = 2
                    end if
                    c = 4   -- remainder of isJmp, as opposed to isDead bytes to be skipped
                end if
            end if
            if jmpOpRetf then
                jmpOpRetf = 0
            else
                --
                -- linkup item:
                --
                s5[i+1] = 0     -- <next> = <end of chain>
                s5[i+2] = last  -- <prev> = last
                if last then
                    s5[last+1] = i  -- last's <next> := this
                else
                    first = i
                end if
                last = i
                i += c
            end if
        end if
    end while
end if

    --
    -- Phase 2 - dead code removal: follow isDead/isJmp chain,
    -- =======                      fixup fwd isJmp offsets by isDead jumped over,
    --                              fixup LineTab entries by isDead blocks, and
    --                              pack code (remove isDead blocks, relink chain w/o them)
    --
    if deadCode then
        if someLeft=2 then  -- all backward jumps were in range, so reset,
            someLeft = 0    -- as we are now going to rescan all fwd jumps.
        end if
        i = first
        first = 0   -- create a brand new chain
        last = 0
        while 1 do
            c = s5[i]
            if c>isDead then
                -- from now on, we have to pack the code.
                --  (one of the most likely places for an isDead is a return statement;
                --   avoid any needless s5[i]=s5[i] byte copying as much as possible)
                oidx = i    -- output idx
                --
                -- deal with that pesky LineTab thing first though...
                --
                deadCode = c-isDead
                for j=1 to length(LineTab) do
                    ltj = LineTab[j]
                    if ltj>=i then  -- skip -ve and <i entries at start
                        klast = s5[i+1]
                        k = j
                        while 1 do
                            while klast and ltj>=klast do
                                c2 = s5[klast]
                                if c2>isDead then
                                    deadCode += c2-isDead
                                end if
                                klast = s5[klast+1]
                            end while
                            LineTab[k] = ltj-deadCode
                            k += 1
                            if k>length(LineTab) then exit end if
                            ltj = LineTab[k]
                            if ltj<0 then
                                k += 1
                                ltj = LineTab[k]
                            end if
                        end while
                        exit
                    end if
                end for
--last = i
                --
                -- now onto copying the non-Dead bytes:
                --
                deadCode = 0
                while i do  -- process isDead blocks (outer loop)
                    c -= isDead
                    deadCode += c   -- count size in bytes (for sanity check)
                    next = s5[i+1]
                    while 1 do  -- process/relink isJmp entries (inner loop)
                        -- (c=4 here when processing consecutive isJmp entries, btw)
                        if next then
                            -- copy bytes i+c..next-1 to oidx++
                            chunkend = next-1
                        else
                            -- copy bytes i+c..s5len to oidx++
                            chunkend = s5len
                            -- (aside: next=0 which soon exits)
                        end if
                        for j=i+c to chunkend do
                            c2 = s5[j]
                            s5[oidx] = c2
                            oidx += 1
                        end for
                        i = next

                        if i=0 then exit end if     -- *ALL DONE*

                        c = s5[i]
                        if c>=isDead then exit end if -- resume in outer loop

                        -- isJmp/isShortJmp/isAddr, copy and relink:

                        next = s5[i+1]
                        --
                        -- check for fwd jmp
                        --
                        i3 = i+3
                        offset = s5[i3]
                        if offset>0 then    -- nb can/must not be isShortJmp yet
                            --
                            -- walk up the chain, adjust offset by any isDead we leap over:
                            --
                            k = next
--                          klast = next+4+offset
                            klast = i+4+offset
                            while k and k < klast do
                                c2 = s5[k]
                                if c2>isDead then
                                    c2 -= isDead
                                    offset -= c2
                                end if
                                k = s5[k+1]     -- k:=next
                            end while
--                          s5[i3] = offset         -- nb differs from non-packing chainwalk
                            if c!=isAddr then
                                if offset<=127 then
--                                  s5[i] = isShortJmp  -- nb differs from non-packing chainwalk
--if oidx=13 then ?9/0 end if
                                    c = isShortJmp
                                    if s5[i-1]=jump_rel32 then
                                        short52 += 1
                                    else
                                        short62 += 1
                                    end if
                                else
                                    someLeft = 1
                                end if
                            end if
                        end if

                        -- create a new chain, w/o the isDead, as we copy
                        -- the flagged dword from i..i+3 to oidx..oidx+3:
                        
                        s5[oidx] = c        -- isJmp/isShortJmp/isAddr
                        s5[oidx+1] = 0      -- <next> = <end of chain>
                        s5[oidx+2] = last   -- <prev> = last
                        s5[oidx+3] = offset
                        if last then
                            s5[last+1] = oidx   -- last's <next> := this
                        else
                            first = oidx
                        end if
                        last = oidx
                        oidx += 4
                        c = 4   -- copy i+4..(next-1 or s5len) bytes
                    end while   -- while i (isJmp/inner loop)
                end while   -- while i (isDead/outer loop)
                oidx -= 1
                if DEBUG then
                    if oidx+deadCode!=s5len then ?9/0 end if    -- sanity check
                end if
                s5 = s5[1..oidx]
                s5len = oidx

                exit    -- *ALL DONE* (phase 2 anyway)

            end if
if c!=isBase then
            i3 = i+3
            offset = s5[i3]
            if offset>0 then    -- nb can/must not be isShortJmp yet
                --
                -- walk up the chain, adjust offset by any isDead we leap over:
                --
                k = i
                klast = i+4+offset
                while k and k < klast do
                    c2 = s5[k]
                    if c2>isDead then
                        c2 -= isDead
                        offset -= c2
                    end if
                    k = s5[k+1]     -- k:=next
                end while
                s5[i3] = offset         -- nb differs from packing chainwalk
                if c!=isAddr then
                    if offset<=127 then
--if i=13 then ?9/0 end if
                        s5[i] = isShortJmp  -- nb differs from packing chainwalk
                        if s5[i-1]=jump_rel32 then
                            short52 += 1
                        else
                            short62 += 1
                        end if
                    else
                        someLeft = 1
                    end if
                end if
            end if
end if

            -- create a new chain, w/o the isDead:

            if last then
                s5[last+1] = i  -- last's <next> := this
            else
                first = i
            end if
            s5[i+2] = last
            last = i

            i = s5[i+1]     -- i:=next
            s5[last+1] = 0
        end while
    end if
    --
    -- Phase 3 - complete isJmp -> isShortJmp flagging; 
    -- =======                      repeat until entire pass leaves someDone 0, or someLeft 0
    --
    while someLeft do
        someLeft = 0
        someDone = 0
        i = first
        while i do
            c = s5[i]
--17/1/2013:
--          if c=isJmp then     -- skip already isShortJmp and isAddr entries
            if c=isJmp and      -- skip already isShortJmp and isAddr entries
               s5[i-1]!=call_rel32 then  -- (there is no "call byte offset" instruction on the x86, only jmp)
                i3 = i+3
                offset = s5[i3]
                if offset<0 then    -- backward jump
                    --
                    -- The following diagram explains the start/limit of the scan loop.
                    -- Obviously we're at i, an isJmp dword, looking for isShortJmp dwords
                    -- that we're jumping over, and hence our offset needs decreasing
                    -- (+3/+4 since the offset is -ve):
                    --
                    --  i+4+offset: opcode of destination instruction (at least one byte)
                    --  i+5+offset:  first possible dword/isShortJmp flag here
                    --  i+6+offset:  dword byte 2
                    --  i+7+offset:  dword byte 3
                    --  i+8+offset:  dword byte 4 (may hold big value)
                    --      ...
                    --  i-1 : opcode of at least one byte ("ours")
                    --  i   :  dword byte 1, isJmp flag we just found is here   <**
                    --  i+1 :  dword byte 2
                    --  i+2 :  dword byte 3, <<ptr to prev isJmp/isShortJmp/isDead flag>>
                    --  i+3 :  dword byte 4 (may hold big value)                <<- offset
                    --  i+4 : next instruction (offset is relative to this)
                    --
                    k = s5[i+2]     -- k = prev
                    klast = i+5+offset
                    -- make final test work as if this itself is a short jump:
                    if s5[i-1]=jump_rel32 then offset += 3 else offset += 4 end if
                    while k and k>=klast do
                        c2 = s5[k]
                        if c2=isShortJmp then   -- skip isAddr and still isJmp entries
                            if s5[k-1]=jump_rel32 then
                                -- jmp dword #E9 xx xx xx xx --> jmp byte #EB xx  (ie 5->2)
                                offset += 3
                            else
                                -- jcc dword #0F 80..8F xx xx xx xx -> jcc byte #70..7F xx  (ie 6->2)
                                offset += 4
                            end if
                            if offset>=-128 then exit end if        -- it'll fit, no need to carry on.
                        end if
                        k = s5[k+2]     -- k = prev
                    end while
                    if DEBUG then
                        if offset>0 then ?9/0 end if    -- sanity check
                    end if
                else  -- forward jump
                    --
                    -- The following diagram explains the start/limit of the scan loop.
                    -- Obviously we're at i, an isJmp dword, looking for isShortJmp dwords
                    -- that we're jumping over, and hence our offset needs decreasing
                    -- (the more obvious -3/-4 in this case, since the offset is +ve):
                    --
                    --  i-1 : opcode ("ours"), not that we're interested in it here (may be 2 bytes).
                    --  i   :  dword byte 1, isJmp flag we just found is here   <**
                    --  i+1 :  dword byte 2, <<ptr to next isJmp/isShortJmp/isDead flag>>
                    --  i+2 :  dword byte 3
                    --  i+3 :  dword byte 4 (may hold big value)
                    --  i+4 : opcode of next instruction (offset relative to this) (at least one byte)
                    --      ...
                    --  i+offset: last possible dword/isShortJmp flag here
                    --  i+1+offset:  dword byte 2
                    --  i+2+offset:  dword byte 3
                    --  i+3+offset:  dword byte 4 (may hold big value)
                    --  i+4+offset: opcode of destination instruction (at least one byte)
                    --
                    k = s5[i+1]     -- k = next
                    klast = i+offset
                    while k and k<=klast do
                        c2 = s5[k]
                        if c2=isShortJmp then   -- skip isAddr and still isJmp entries
                            if s5[k-1]=jump_rel32 then
                                -- jmp dword #E9 xx xx xx xx --> jmp byte #EB xx (ie 5->2)
                                offset -= 3
                            else
                                -- jcc dword #0F 80..8F xx xx xx xx -> jcc byte #70..7F xx (ie 6->2)
                                offset -= 4
                            end if
                            if offset<=127 then exit end if     -- it'll fit, no need to carry on.
                        end if
                        k = s5[k+1]     -- k = next
                    end while
                    if DEBUG then
                        if offset<0 then ?9/0 end if    -- sanity check
                    end if
                end if
--DEV should this also check s5[i-1]!=call_rel32?? [fixed 17/1/2013]
                if offset>=-128 and offset<=127 then
--if i=13 then ?9/0 end if
                    s5[i] = isShortJmp      -- flag set, offset remains unaltered.
                    if s5[i-1]=jump_rel32 then
--if offset=0 then
--  dead5j += 5
--else
                        short52 += 1
--end if
                    else
--if offset=0 then
--  dead6j += 6
--else
                        short62 += 1
--end if
                    end if
                    someDone = 1
                else
                    someLeft = 1
                end if
            end if
            i = s5[i+1]     -- i:=next
        end while
        if not someDone then exit end if
    end while
    --
    -- So now we can say how big this code chunk will really be:
    --
    thisCSsize = s5len - (short52*3) - (short62*4)
--  thisCSsize = s5len - (short52*3) - (short62*4) - dead5j -dead6j
    --
    -- One last job: adjust any global labels in this block by preceding shortened jumps
    -- DEV not at all sure this will cope with isDead...
    --
--?length(glblused)
    for gidx=1 to length(glblused) do
        if glblabel[gidx]=vi then
if not repl then
            if and_bits(glblused[gidx],G_declared+G_set)!=G_declared+G_set then ?9/0 end if
end if
            offset = glboffset[gidx]
--?{offset,gidx}
            k = 0
            i = first
            while i!=0 and i<offset do
--          while i!=0 and i<glboffset[gidx] do
                if s5[i]=isShortJmp then
--?i
                    if s5[i-1]=jump_rel32 then
--                      offset -= 3
                        k += 3
                    else
--                      offset -= 4
                        k += 4
                    end if
                end if
                i = s5[i+1]     -- i:=next
--?{i}
            end while
            glboffset[gidx] = offset-k
        end if
    end for
end procedure


sequence imports
--datab4code:
--DEV global constant DATA=1, CODE=2 (after/when we swap)
global constant CODE=1, DATA=2, IMPORTS=3, REFS=3
sequence relocations
        --
        -- relocations is initially {{{},{},{}},{{},{},{}}}
        -- ie, if they are non-empty, the four lists of dword offsets are:
        -- relocations[DATA][DATA][1..$] in data relative to start of data section,
        -- relocations[DATA][CODE][1..$] in data relative to start of code section,
        -- relocations[DATA][REFS][1..$] in data relative to start of data section,
        --  [latter is DLL only, and only for pStack/initStack, not LoadLibrary]
        -- relocations[CODE][DATA][1..$] in code relative to start of data section,
        -- relocations[CODE][CODE][1..$] in code relative to start of code section.
        -- relocations[CODE][IMPORTS][1..$] are code references to import section.
        --  [latter is DLL only, and only for LoadLibrary, not pbinary.e/fixup()]
        -- eg a mov reg,[var] needs to put an entry in relocations[CODE][DATA],
        --    a lea reg,:label needs to put an entry in relocations[CODE][CODE],
        --    a constant x="yz" needs to put an entry in relocations[DATA][DATA],
        --    and, lastly, all the [S_il] need an entry in relocations[DATA][CODE].
        -- Note that refs are generated as #80000000+offset here, and, once the
        --  start of data has been applied, get shr 2 + #20000000 in pbinary.e,
        --  to complete the right-shifted-by-2 + #40000000 that is a Phix ref.
        -- (And obviously same deal with 8 extra zeroes for 64-bit executables.)
        --  

atom thunktable

--DEV (not a good idea, see notes in e6.exw 17/11/09)
--integer o0        -- emit warning once only
--      o0 = 1

--with trace
procedure blurph(integer lineno)
-- take the fully scanned s5 blocks and pack them to output (code_section),
--  that is now that we know the size of, and hence where, everything will be.
-- the parameter lineno is just a quick debugging aid
integer cin, cout, cm1, c, c2, k, klast, s5len
--25/4/16 (pHeap using mmap)
--integer offset    --7/1/09
atom offset
integer vno

integer ltidx,  -- LineTab idx
        nilte   -- next interesting line tab entry

sequence sv     -- copy of symtab[vno], speedwise

object constvalue
integer libidx,fnidx
--object dbg
--trace(1)
--?LineTab
--?lineno
    nilte = -1
    for i=1 to length(LineTab) do
        nilte = LineTab[i]
        if nilte>0 then
            lineno += 1
            nilte += 1      -- zero-based offset to 1-based idx
            ltidx = i
            exit
        end if
        lineno -= nilte
    end for

    cout = CSvsize+1
    CSvsize += thisCSsize
    --
    -- The Final Fixups:
    --
    cin = 1
if q86 then
    s5len = length(s5)-5
else
    s5len = length(s5)
end if
    c = 0  c2 = 0   --DEV test
    while cin<=s5len do
        c = s5[cin]

        if c<isOpCode then          -- ie as-is byte, #00..#FF.

--          code_section[cout] = c
            code_section[cout] = and_bits(c,#FF)
            if cin=nilte then   -- next interesting linetab entry?
                LineTab[ltidx] = cout-CSvsize+thisCSsize-1      -- (1-based idx(ie cout) to zero-based offset)
                nilte = 0
                for i=ltidx+1 to length(LineTab) do
                    nilte = LineTab[i]
                    if nilte>0 then
                        lineno += 1
                        nilte += 1                              -- (zero-based offset to 1-based idx(ie cin))
                        ltidx = i
                        exit
                    end if
                    lineno -= nilte
                end for
            end if
            cin += 1
            cout += 1

        elsif c<=isIL then          -- isOpCode/isAPIFn/isVar[1|4]/isVno/isData/isConstRef/isConstRefCount/isJmpG/isGaddr/isILa/isIL

            vno = s5[cin+3]
            if c=isOpCode then          -- 4 byte relative offset (to VM entry point).
if newEmit then ?9/0 end if -- (we still might want this, one day...)
                offset = 9/0
                if DEBUG then
                    if offset=0 then puts(1,"\n\nERROR: "&opNames[vno]&" = 0\n") ?9/0 end if
                end if
            elsif c=isAPIfn then
                if bind then
                    {libidx,fnidx} = APINames[vno]
                    offset = imports[libidx][3][fnidx]
                    imports[libidx][3][fnidx] = cout-1
                    if DLL then
                        relocations[CODE][IMPORTS] = append(relocations[CODE][IMPORTS],cout-1)  -- (dword)
                    end if
                else
                    if X64 then
                        offset = thunktable+vno*8-8
                        offset -= CSvaddr+cout+3
                    else
                        offset = thunktable+vno*4-4
                    end if
                end if
            else
if c!=isData
and c!=isJmpG
and c!=isGaddr then
--24/4/15:
                    if atom(symtab[vno]) then
printf(1,"pemit2.e line 1817: symtab[%d]=%d\n",{vno,symtab[vno]})
                        sv = {-1,S_GVar2,0,0,0,-1}
                    else
                        sv = symtab[vno]
                    end if
end if
                if c=isVar                  -- 4 byte absolute address (eg mov eax,[p1])
                or c=isVar1                 -- "" with 1 byte imm yet to follow on the instruction
                or c=isVar4 then            -- "" with 4 byte imm yet to follow on the instruction

                    if sv[S_NTyp]<=S_GVar2 then
                        vno = sv[S_Slink]   --DEV +gbase
                    else
if newEmit then ?9/0 end if
                        vno = -sv[S_Tidx]   --DEV vmap or reassign!
                    end if
                    if bind then
if vno=0 then ?9/0 end if   -- yep, thought so...
--DEV tryme
--                      offset = (vno+2)*dsize+8
                        if X64 then
                            offset = (vno)*8+24
                            if c=isVar1 then
                                offset -= 1
                            elsif c=isVar4 then
                                offset -= 4
                            end if
                        else
                            offset = (vno)*4+16
                        end if
                        relocations[CODE][DATA] = append(relocations[CODE][DATA],cout-1)    -- (dword)
                    else
                        if X64 then
                            offset = DSvaddr+vno*8+24
                            if c=isVar1 then
                                offset -= 1
                            elsif c=isVar4 then
                                offset -= 4
                            end if
                            offset -= CSvaddr+cout+3
                        else --32
                            offset = DSvaddr+vno*4+16
                        end if
                    end if
                elsif c=isVno then
                    if bind and mapsymtab then
                        vno = symtabmap[vno]
                    end if
                    offset = vno
                elsif c=isData then     -- 4 byte absolute offset
                    offset = vno
                    if bind then
                        relocations[CODE][DATA] = append(relocations[CODE][DATA],cout-1)    -- (dword)
                    else
                        offset += DSvaddr
                        if X64 then
                            offset -= CSvaddr+cout+3
                        end if
                    end if
                elsif c<=isConstRefCount then
                    if bind then
if newEmit then
--  if not sequence(sv) then ?9/0 end if -- DEV/temp
    if c=isConstRef then ?9/0 end if -- (isConstRefCount is fine) (fine for interp?)
end if
                        -- link them up for fixup later (in fixupConstRefs, via DumpXxx)...
                        symtab[vno] = 0                                 -- (kill refcount)
                        if sv[S_NTyp]!=S_Const then ?9/0 end if
                        code_section[cout] = sv[S_ConstChain]
                        sv[S_ConstChain] = cout
                        code_section[cout+1] = (c=isConstRef)
                        symtab[vno] = sv
                    else
                        constvalue = sv[S_value]
                        #ilASM{
                            [32]
                                mov eax,[constvalue]
                                and eax,#3FFFFFFF           -- (raw address/4)
                                mov [offset],eax
                            [64]
                                mov r15,h4
                                mov rax,[constvalue]
                                sub r15,1                   -- #3FFF_FFFF_FFFF_FFFF
                                and rax,r15                 -- (raw address/4)
                                add r15,1
                                mov [offset],rax
                            []
                              }
                        --
                        -- explanation of the above:
                        --  if a sequence(/string/float) is located at say #00040204 (and 
                        --  that address will aways be a multiple of 4, ie end in 0b00), a 
                        --  "ref" is formed by shifting it right two bits and putting 0b01
                        --  at the front, giving #40010081. The above extracts the #10081
                        --  part (always an integer) from which we can create the right 
                        --  atom value (by adding #40000000, or by calculating the address
                        --  of the refcount) that we need to emit/poke. Hopefully, if your
                        --  hex maths is up to it, you can see #00010081 * 4 == #00040204.
                        --
                        if c=isConstRef then
--if newEmit then ?9/0 end if -- (actually, fine for interpretation)
--DEV X64??
                            setcsDword(cout,offset+#40000000) -- (make an atom value == to the ref)
                        else -- isConstRefCount
                            setcsDword(cout,offset*4-8) -- (make an atom value == to the refcount addr)
                        end if
                    end if
                    c = isDead -- avoid setcsDword call below.
--DEV 28/4/13 maybe:
                elsif c=isJmpG
                   or c=isGaddr then
                    k = glblused[vno]
--?{k,G_bang+G_declared,and_bits(k,G_bang+G_declared),G_bang}
if and_bits(k,G_bang+G_declared)=G_bang then -- (added 28/10/14)
            offset = 0
else
                    if and_bits(k,G_declared)=0 then
                        fileno = glblabel[vno]
                        tokline = glblline[vno]
                        tokcol = glblcol[vno]
                        Abort("not declared")
--                      Abort(glblname[vno]&" not declared")    -- got a '?'
                    end if
if not repl then
                    if and_bits(k,G_set)=0 then ?9/0 end if
end if
                    offset = glboffset[vno]
if newEmit then
                    fileno = glblabel[vno]
                    if fileno!=0 then -- part of vm(?) [DEV check G_optable?]
                        offset += symtab[fileno][S_il]
                    end if
else
                    offset += symtab[glblabel[vno]][S_il]
end if
--  if newEmit then
                    if c=isJmpG then
--does this want ImageBase2+BaseOfCode2? (not available yet, but anyway, no!)
                        offset -= cout+3
if not bind then
                        offset -= CSvaddr
end if
                    else -- c=isGaddr
--if not bind then ?9/0 end if -- 27/2/15, just see (triggered first go... [DEV])
--added 28/10/14:
--11/9/16: (if bind wrapper)
if bind then
                        relocations[CODE][CODE] = append(relocations[CODE][CODE],cout-1)    -- (dword)
end if
                    end if
--  else -- not newEmit
--                  if c=isJmpG then
--                      offset -= CSvaddr+ImageBase+cout+3
--                  end if
--  end if
end if
                else
                    offset = sv[S_il]   -- isILa = 4 byte absolute address of symtab[routineNo][S_il]
--if newEmit then
                    if c=isIL then      -- isIL  = 4 byte relative offset to symtab[routineNo][S_il]
--                      ?9/0
                        offset -= cout+3
if not bind then
                        offset -= CSvaddr
end if
--DOH! these are not available yet!!
--                      offset += ImageBase2+BaseOfCode2
--  if X64 then
--                      relocations[CODE][CODE] = append(relocations[CODE][CODE],cout-1)    -- (dword)
--  end if
                    else
--if not bind then ?9/0 end if -- 27/2/15 just see (triggered on t38, 23/3/15...)
if bind then
                        relocations[CODE][CODE] = append(relocations[CODE][CODE],cout-1)    -- (dword)
end if
                    end if
--else
--                  if c=isIL then      -- isIL  = 4 byte relative offset to symtab[routineNo][S_il]
--                      offset -= CSvaddr+ImageBase+cout+3  -- cout+3 is really (cout-1)+4
--                  end if
--end if
                end if
            end if
            if c!=isDead then -- except for isConstRef[Count]:
                setcsDword(cout,offset)
--if c=isVar1 then
--?offset
--end if
            end if
            cin += 4
            cout += 4

        else                    --        else: 4 byte offset. (c=isAddr/isJmp/isShortJmp)
            offset = s5[cin+3]
if c!=isBase then
            --
            -- First adjust offset by any isShortJmps flagged betwixt origin and destination:
            --
            if offset<0 then        -- backward jump
                --
                --  cin+4+offset: opcode of destination instruction (at least one byte)
                --  cin+5+offset: first possible dword/isShortJmp flag here
                --  cin+6+offset:  dword byte 2 <next Jmp/ShortJmp link>
                --  cin+7+offset:  dword byte 3 <prev Jmp/ShortJmp link>    -- ...until 0 or < cin+4+offset
                --  cin+8+offset:  dword byte 4 (actual value/index/offset)
                --      ...
                --  cin-1 : opcode of at least one byte ("ours")
                --  cin   :  dword byte 1, isJmp flag we just found is here     <**
                --  cin+1 :  dword byte 2 <next Jmp/ShortJmp link>
                --  cin+2 :  dword byte 3 <prev Jmp/ShortJmp link>          -- follow this link...
                --  cin+3 :  dword byte 4 (actual value/index/offset)
                --  cin+4 : next instruction (offset is relative to this)
                --
                k = s5[cin+2]       -- k = prev
                klast = cin+5+offset
                while k and k>=klast do
                    c2 = s5[k]
                    if c2=isShortJmp then
                        if s5[k-1]=jump_rel32 then
                            -- jmp dword #E9 xx xx xx xx --> jmp byte #EB xx (ie 5->2)
                            offset += 3
                        else
                            -- jcc dword #0F 80..8F xx xx xx xx -> jcc byte #70..7F xx (ie 6->2)
                            offset += 4
                        end if
                    end if
                    k = s5[k+2]     -- k = prev
                end while
                if DEBUG then
                    if offset>0 then ?9/0 end if    -- sanity check
                end if
            else                        -- forward jump
                --
                --  cin-1 : opcode ("ours"), not that we're interested in it here (may be 2 bytes).
                --  cin   :  dword byte 1, isJmp flag we just found is here     <**
                --  cin+1 :  dword byte 2 <next Jmp/ShortJmp link>              -- follow this link...
                --  cin+2 :  dword byte 3 <prev Jmp/ShortJmp link>
                --  cin+3 :  dword byte 4 (actual value/index/offset)
                --  cin+4 : opcode of next instruction (offset relative to this) (at least one byte)
                --      ...
                --  cin+offset: last possible dword/isShortJmp flag here
                --  cin+1+offset:  dword byte 2 <next Jmp/ShortJmp link>        -- ...until 0 or > cin+4+offset
                --  cin+2+offset:  dword byte 3 <prev Jmp/ShortJmp link>
                --  cin+3+offset:  dword byte 4 (actual value/index/offset)
                --  cin+4+offset: opcode of destination instruction (not marked isJmp [only the dwords are])
                --
                k = s5[cin+1]   -- k = next
                klast = cin+offset
                while k and k<=klast do
                    c2 = s5[k]
                    if c2=isShortJmp then
                        if s5[k-1]=jump_rel32 then
                            -- jmp dword #E9 xx xx xx xx --> jmp byte #EB xx (ie 5->2)
                            offset -= 3
                        else
                            -- jcc dword #0F 80..8F xx xx xx xx -> jcc byte #70..7F xx (ie 6->2)
                            offset -= 4
                        end if
                    end if
                    k = s5[k+1]     -- k = next
                end while
                if DEBUG then
                    if offset<0 then ?9/0 end if    -- sanity check
                end if
            end if
end if
            if c=isShortJmp then        -- isJmp that has been found to fit in a byte
                cm1 = cout-1
                c = code_section[cm1]
                if offset<0 then
                    if c=jump_rel32 then offset += 3 else offset += 4 end if
                end if
                if DEBUG then
                    if offset<-128 or offset>127 then ?9/0 end if -- sanity check
--                  if offset=0 then
--                      if o0 then
--                          puts(1,"pemit.e: warning jmp 0 emitted\n")
----if getc(0) then end if
--                          o0 = 0
--                      end if
--                  end if
                end if
                --
                -- Finally, convert the dword instruction to short form...
                --
--17/11/09 no good... need to do this in scanforShortJmp!!
--if offset=0 then
--  if c=jump_rel32 then
--      cout = cm1
--  else
--      cout -=2
--  end if
--else
--DEV fixed 17/1/2013:
--/**/  -- (DEV pemit.e cannot cope with call offset that fits in a byte...)
--          (there is no call byte offset instruction on the x86, so it
--           would have to leave it as-is, which means we got CSvsize (etc) wrong..)
--          (see (untested) "should this also check s5[i-1]!=call_rel32??" above)
                if c=jump_rel32 then
                    -- jmp dword, #E9 xx xx xx xx --> jmp byte, #EB xx
                    code_section[cm1] = #EB
                else
                    -- jcc dword, #0F 80..8F xx xx xx xx -> jcc byte, #70..7F xx
                    cout -= 1
                    cm1 -= 1
                    if DEBUG then
                        if code_section[cm1]!=#0F then ?9/0 end if
                        if c<#80 or c>#8F then ?9/0 end if
                    end if
                    code_section[cm1] = c-#10
                end if
--              code_section[cout] = offset
                code_section[cout] = and_bits(offset,#FF)
                cout += 1               -- .. and this is what it was all about !!
                                        --    (if you can fit 25% more code in the on-chip 
                                        --     cache, it'll run alot more than 25% faster!)
--end if
                cin += 4

            else
                if c=isJmp then                     -- 4 byte offset that does not fit in byte
--17/1/2013:
if s5[cin-1]!=call_rel32 then
                    if offset>=-128 and offset<=127 then ?9/0 end if -- sanity check
end if
                elsif c<=isAddr then                -- 4 byte offset, fixup as absolute address (push <return addr>)
--if newEmit then
--  ?9/0 --DEV not really sure about this:
                    offset += cout+3
if bind then
                    relocations[CODE][CODE] = append(relocations[CODE][CODE],cout-1)    -- (dword)
else
                    offset += CSvaddr
end if
--else
--                  offset += CSvaddr+ImageBase+cout+3  -- cout+3 is really (cout-1)+4
--end if
-- 2/4/10: (needed for jump table entries)
                    if cin=nilte then   -- next interesting linetab entry?
                        LineTab[ltidx] = cout-CSvsize+thisCSsize-1      -- (1-based idx(ie cout) to zero-based offset)
                        nilte = 0
                        for i=ltidx+1 to length(LineTab) do
                            nilte = LineTab[i]
                            if nilte>0 then
                                lineno += 1
                                nilte += 1                              -- (zero-based offset to 1-based idx(ie cin))
                                ltidx = i
                                exit
                            end if
                            lineno -= nilte
                        end for
                    end if
                end if
                setcsDword(cout,offset)
                cin += 4
                cout += 4
            end if
        end if
    end while
    if cin=nilte then
--if newEmit then
--      ?9/0
--else
        LineTab[ltidx] = cout-CSvsize+thisCSsize-1      -- 1-based idx to zero-based offset
--end if
    end if
    if cout!=CSvsize+1 then ?9/0 end if -- sanity check
--?LineTab

end procedure

-- results from flatdump(/DEV:dumpString):
atom s_addr     -- symbolic address (a "ref" in the range #40000001..#7FFFFFFF,
                --  ie a 2-bit-shifted dword-aligned pointer with 0b01 prefix)

--SUG: (tryme!)
--procedure dumpString(string name, integer refcount)
--integer l = length(name),
--      w = iff(X64?8:4),
--      pad = w-and_bits(l,w-1)
--  if pad then
--      name &= repeat('\0',pad)
--  end if
--  pad += l+w*4
--  d_addr += pad
--  appenddsDword(pad)              -- maxlen (in bytes)
--  appenddsDword(l)                -- length
--  appenddsDword(refcount)         -- refcount
--  appenddsType(#82)
--  s_addr = length(data_section)
--  appenddsBytes(name)
--end procedure
procedure dumpString(sequence name, integer refcount)
integer l, l4
    l = length(name)
    if X64 then
        l4 = and_bits(l+8,#3FFFFFF8)+32
    else
        l4 = and_bits(l+4,#3FFFFFFC)+16
    end if
--  l4 = and_bits(l+w,#400000000-w)+w*4
    d_addr += l4
    appenddsDword(l4)               -- maxlen (in bytes)
    appenddsDword(l)                -- length
    appenddsDword(refcount)         -- refcount
    appenddsType(#82)
    s_addr = length(data_section)
--  l = w-and_bits(l,w-1)
--  name &= repeat('\0',l)
    appenddsBytes(name)
    if X64 then
        for i=1 to 8-and_bits(l,7) do
--      for i=1 to w-and_bits(l,w-1) do
--      for i=1 to l do
            data_section = append(data_section,0)
        end for
    else
        for i=1 to 4-and_bits(l,3) do
            data_section = append(data_section,0)
        end for
    end if
--if not isString(data_section) then ?9/0 end if
--  l = 4-and_bits(l,3)
--  l = dsize-and_bits(l,dsize-1)
--  if l then
--      data_section &= repeat(0,l)
--      data_section &= repeat('\0',l)  --DEV try this (much much later)
--  end if  
--  DSvsize += l4
end procedure

--integer track996 = 0
--object was996
--type symt(sequence s)
--object s996v
--  if track996 then
--      s996v = s[996]
--      if atom(s996v) then
----            puts(1,"pemit2.e line 2192: atom(s[996])\n")
--      else
--          s996v = s996v[S_value]
--          if s996v!=was996 then
----                trace(1)
----                ?9/0
--              puts(1,"pemit2.e line 2198: s996v: ")
--              ?{s996v,was996}
--          end if
--      end if
--  end if
--  return 1
--end type
--symt flatsym2

sequence flatsym2

--DEV/SUG: Report "possible conflicts" under -lint. 
--         if there are any globals (set a flag), 
--          print whole set to ex.err, in the form
--          C:\Programs\xxx.e:147 xxx (local)

--integer oops2shown = -1
integer oopsshown = 0
integer oops2shown = 0
function ReconstructIds(sequence name, integer node)
integer slink
----DEV 26/02/2012 Check the reference count on symtab is 1
----/**/integer refcount
----/**/    #ilASM{ mov eax,[symtab]
----/**/            mov eax,[ebx+eax*4-8]
----/**/            mov [refcount],eax }
----/**/    if refcount!=1 then
----/**/        puts(1,"\n\noops, non-1 reference count on symtab!")
----/**/        if getc(0) then end if
----/**/    end if
object prev
    slink = tt[node+EQ]
    while slink do
--if oops2shown=-1 then
--  printf(1,"[1]ReconstructIds(%s)\n",{name})
--  oops2shown = 0
--end if
        -- This was to be temp (bugfix 1/3/2013?) but proved worth leaving in.
        if slink>length(symtab) then
--      if slink>length(symtab) 
--      or not sequence(symtab[slink]) then
            if oopsshown=0 then
                printf(1,"oops in ReconstructIds(%s)\n",{name})
                oopsshown = 1
            end if
            exit
        end if
        prev = symtab[slink]
        if atom(prev) then
            if oops2shown=0 then
                printf(1,"oops2 in ReconstructIds(%s)\n",{name})
                oops2shown = 1
            end if
            exit
        end if
--  slink = 721
--  symtab[721][1..9] = {-1,1,2,16898,0,315,8,0,0}  -- (call_common)
--  symtab[716..727] = {0,0,0,0,0,0,0,0,0,0,0,0}    -- (pglobals.e)
--  dbg = 0
        prev = prev[S_Name]
        if atom(prev) then
            symtab[slink][S_Name] = name
--      else -- (good, find not replaced with find_from, it should only be (max 2) aliases this affects)
--          printf(1,"ReconstructIds: symtab[%d][S_Name], %s not replaced with %s\n",{slink,prev,name})
        end if
        slink = symtab[slink][S_Nlink]
    end while
    return 1
end function
constant r_ReconstructIds = routine_id("ReconstructIds")

--ppOpt({pp_Pause,0})

global integer rbldrqd = 0  -- global for pTrace.e; pDiagN.e has a shadow copy

--include builtins\puts1h.e     --DEV breaks self-host (newEmit=0)

global procedure rebuild_callback()
--DEV see pTrace.e/show_trace())
-- called (via rbicb, see below) when interpreting (and this has not
--  already been done because of unresolved routine_ids) before 
--  pdiag.e/pdebug.e get invoked, so they can use symtab[i][S_Name] normally.
--BUGFIX 17/03/2012:
--  We must, however, ensure that symtab has a refcount of 1 (- it will not
--  if an opGetST has been called without the result dropping out of scope)
--  so that our efforts to fixup symtab do not cause a clone of it. We must,
--  btw, incref symtab in opGetST, to prevent attempts to deallocate it.
integer StmAddr
atom refaddr
integer refcount
    rbldrqd = 0
--puts1("rebuild_callback() [pemit2.e line 2382]\n")
    #ilASM{
        [32]
            mov eax,[symtab]
            and eax,#3FFFFFFF       -- (raw address/4)
            mov [StmAddr],eax
        [64]
            mov r15,h4
            mov rax,[symtab]
            sub r15,1               -- #3FFF_FFFF_FFFF_FFFF
            and rax,r15             -- (raw address/4)
            add r15,1
            mov [StmAddr],rax
        []
          }
--DEV: (and we need poke8s below) [I assume we're getting away with this because of little endian and refcounts<1billion]
--  if machine_bits()=32 then
        refaddr = StmAddr*4-8
        refcount = peek4s(refaddr)
--  else
--      refaddr = StmAddr*4-8
--      refcount = peek8s(refaddr)
--  end if
--printf(1,"rebuild_callback: symtab is at #%08x, refcount is %d\n",{StmAddr*4,refcount})
    if refcount!=1 then
        poke4(refaddr,1)
    end if
    relink()
    tt_traverse(r_ReconstructIds,"",-2)             -- identifiers
    if refcount!=1 then
        poke4(refaddr,refcount)
    end if
end procedure

--
-- When compiled, the ids have already been reconstructed by the time the .exe is written,
--  however when interpreting that is left until an error occurs, for performance reasons.
--  pDiagN.e is not only part of p[w].exe, and needs to invoke rebuild_callback() when the
--  program is being interpreted, but is also part of any test.exe, in which case it does
--  not need to call it. There are (at least) four possible options: 1) use :!rbidscb as
--  below and assume that in the pDiagN.e in test.exe it will be resolved as 0 but in any
--  case will not be called, 2) call some (new) global in pDiagN.e passing a call_back or
--  other label address, 3) use routine_id in pDiagN.e to locate and invoke it, or 4) use
--  a bog-standard hll "rebuild_callback()" in pDiagN.e, but also include a special dummy
--  version that does nothing in every test.exe. In the end I went with option number 1.
--
--  There is however one slight complication: :%opFrame is not available under newEmit=0,
--  so we cannot use it directly while we still want the ability to rebuild an old-style
--  p[w].exe. Likewise under old-style :!diagFrame won't be available and resolves to 0,
--  and similar when specifying -nodiag on the command line. However, since :!rbidscb is
--  only ever invoked from pDiagN.e, where :!diagFrame lives, it'll be fine (flw).
--  There is also the messy business of fiddling with symtabptr, aka [ds+8]...  (:%pGetSymPtr)
--
--!/*
if newEmit then
#ilASM{ jmp :fin
      :!rbidscb
        [32]
            push dword[esp]                         -- (leave the ret addr on stack)
            cmp [rbldrqd],0
            je :rbidsret
--mov eax,[symtab]
            mov edx,routine_id(rebuild_callback)    -- mov edx,imm32 (sets K_ridt)
            mov ecx,$_Ltot                          -- mov ecx,imm32 (=symtab[rebuild_callback][S_Ltot])
--          call :%opFrame
            call :!diagFrame
            add esp,4
--EXCEPT
--          mov dword[ebp+16],:rbidsret
            mov dword[ebp+28],:rbidsret
            jmp $_il                                -- jmp code:rebuild_callback
        [64]
            push qword[rsp]                         -- (leave the ret addr on stack)
            cmp [rbldrqd],0
            je :rbidsret
--pop al
            mov rdx,routine_id(rebuild_callback)    -- mov edx,imm32 (sets K_ridt)
            mov rcx,$_Ltot                          -- mov ecx,imm32 (=symtab[rebuild_callback][S_Ltot])
--          call :%opFrame
            call :!diagFrame
            add rsp,8
--EXCEPT
--          mov qword[rbp+32],:rbidsret
            mov qword[rbp+56],:rbidsret
            jmp $_il                                -- jmp code:rebuild_callback
        []
          ::rbidsret    
            ret
      ::fin
      }
end if
--!*/

--procedure fixupConstRefs(integer cref, integer offset, integer idx)
---- This is only called when binding (via DumpXxx), there is no
----    const chain when interpreting (done immediately instead)
--integer cnxt
----                    code_section[cout] = sv[S_ConstChain]
----                    sv[S_ConstChain] = cout
----                    code_section[cout+1] = (c=isConstRef)
--  if cref then
--      if listing then
--          craddr = append(craddr,d_addr+offset)
--          cridx = append(cridx,idx)
--      end if
--      while 1 do
--          cnxt = code_section[cref]
--          if code_section[cref+1] then    -- isConstRef
--if newEmit then ?9/0 end if
--              setcsDword(cref,s_addr)
--          else                -- isConstRefCount
--              setcsDword(cref,d_addr+offset)
--          end if
--          if cnxt=0 then exit end if
--          cref = cnxt
--      end while
--  end if
--end procedure

function DumpString(sequence name, integer node)
-- (bind only)
integer slink, refcount
sequence si
integer x_addr
integer rd = iff(DLL?REFS:DATA)
--if length(name) and name[1]='a' then
--?{"DumpString",name,node}
--end if
    slink = tt[node+EQ]
if slink then
    x_addr = length(data_section)+dsize*4
    refcount = 0
    while slink do
if flatsym2[slink][S_State]=0 then
        slink = flatsym2[slink][S_Clink]
else
--      flatsym2[slink][S_value] = s_addr   --DEV oh crud...
        flatsym2[slink][S_value] = x_addr+#800000000
--if find(slink,{996,1337}) then
--if slink=996 then
--  printf(1,"pemit2.e line 2383 DumpString: flatsym2[%d][S_value]:=%s\n",{slink,sprint(flatsym2[slink][S_value])})
--end if
--      flatsym2[slink][S_value] = x_addr+#80000000
        si = flatsym2[slink]
        if X64 then
            dsidx = (si[S_Slink])*8+25
        else
            dsidx = (si[S_Slink])*4+17
        end if
--      dsidx = (si[S_Slink]+3)*dsize+5 --DEV tryme, but see the one below first
--if dsidx-1=TRAP then ?9/0 end if
--      relocations[DATA][DATA] = append(relocations[DATA][DATA],dsidx-1)   -- (qword)
        relocations[DATA][rd] = append(relocations[DATA][rd],dsidx-1)   -- (qword)
        setds(x_addr,1)
    if si[S_NTyp]=S_Const then
        if si[S_ConstChain] then ?9/0 end if
    end if
        refcount += 1
        slink = si[S_Clink]
end if
    end while
    if refcount then
--if length(name) and name[1]='a' then
--?{"dumpString",name, refcount}
--end if
        dumpString(name, refcount)
    end if
end if
    return 1
end function
constant r_DumpString = routine_id("DumpString")

--DEV (wrong one)
--include builtins\pfloat.e -- atom_to_float80() [DEV make that an autoinclude!]
--Note: atom_to_float80() is intended for use in 64-bit applications only. The
--      atom type in 32-bit programs is a 64-bit float, which means the above
--      will discard 11 bits of precision. (or is that float80_to_atom()?)

--with trace
function DumpAtom(atom v, integer node)
-- (bind only)
integer slink, refcount
--, a
--, vchk
sequence si, x8
integer x_addr
integer rd = iff(DLL?REFS:DATA)
--  v = tt[node+CH]
--trace(1)
--  if not integer(v) then
    if isFLOAT(v) then
--SUG:
--      x_addr = length(data_section)+2*dsize
        if X64 then
            x_addr = length(data_section)+16
        else
            x_addr = length(data_section)+8
        end if
        slink = tt[node+EQ]
        refcount = 0
        while slink do
            si = flatsym2[slink]
if si[S_State]=0 then
else
            flatsym2[slink][S_value] = x_addr+#800000000    -- (added 9/10/14)
--if find(slink,{996,1337}) then
--if slink=996 then
--  printf(1,"pemit2.e line 2444 DumpAtom: flatsym2[%d][S_value]:=%s\n",{slink,sprint(flatsym2[slink][S_value])})
--end if
            if si[S_NTyp]!=S_Const then ?9/0 end if
--sug:      vno = si[S_Slink]
--          dsidx = (vno+2)*dsize+9
            if X64 then
                dsidx = (si[S_Slink])*8+25
            else
                dsidx = (si[S_Slink])*4+17
            end if
--          dsidx = (si[S_Slink]+3)*dsize+5 (probably wrong...)
--if dsidx-1=TRAP then ?9/0 end if
--          relocations[DATA][DATA] = append(relocations[DATA][DATA],dsidx-1)   -- (qword)
            relocations[DATA][rd] = append(relocations[DATA][rd],dsidx-1)   -- (qword)
            setds(x_addr,1)
            if si[S_ConstChain] then ?9/0 end if
            refcount += 1
end if
            slink = si[S_Clink]
        end while
        if refcount then
            appenddsDword(refcount)
--if not isString(data_section) then ?9/0 end if
            appenddsType(#12)
--if not isString(data_section) then ?9/0 end if
            if X64 then
                x8 = atom_to_float80(v) -- (x10 really)
                data_section &= x8
                data_section &= "\0\0"
                d_addr += 28
            else
                x8 = atom_to_float64(v)
                data_section &= x8
                d_addr += 16
            end if
--if not isString(data_section) then ?9/0 end if
        end if
    else
        slink = tt[node+EQ]
        while slink do
            si = flatsym2[slink]
if si[S_State]=0 then
else
            if si[S_NTyp]=S_Const
--DEV K_rtn prolly not on this chain...
            or and_bits(si[S_State],K_noclr+K_rtn) then
-- DEV 17/4/06 if this never triggers, safe to delete the entire else block/
--      not call DumpAtom (from within tt_traverseA) in the first place.
--vchk = getdsDword(si[S_Slink]*4 - 3)  -- (NB: 1-based, unlike setdsDword)
--if vchk!=v then puts(1,"setdsDword *IS* needed\n") end if
-- DEV 17/4/06 sanity check added:
--if newEmit then ?9/0 end if
--DEV 21/6/19 triggers for #02000004,"!=",#02000008 when p32'ing p64...
--              if v!=si[S_value] then ?9/0 end if
--S_Name
--      ,i,symtabmap[i]
--sv = si[S_Name]
--k = symtab[i][S_FPno]
--                              printf(1,"xType=0 on symtab[%d] (%s in %s)\n",{i,getname(sv,-2),filenames[k][2]})
                if v!=si[S_value] then
-- erm, I think this screws up our traverse...
--                  integer vfn = si[S_FPno]
--                  string vname = getname(si[S_Name],-2),
--                         vfile = iff(vfn=0?"???":filenames[vfn][2])
--                  ?{"9/0 pEmit2.e line 2183",v,"!=",si[S_value],"slink=",slink,"name=",vname,"in",vfile}
                    ?{"9/0 pEmit2.e line 2183",v,"!=",si[S_value],"slink=",slink,"slink=",slink,"symtabmap[slink]",symtabmap[slink]}
                    ?si
                end if
--29/12/14: (removed immediately: no effect)
--              if bind and mapsymtab and and_bits(si[S_State],K_rtn) then
--                  v = symtabmap[v]
--              end if
                if X64 then
--                  if not integer(v) then ?9/0 end if
                    if isFLOAT(v) then ?9/0 end if
                    setdsQword((si[S_Slink])*8+24, v)
                else
                    setdsDword((si[S_Slink])*4+16, v)
                end if
--DEV tryme:
--              setdsDword((si[S_Slink]+2)*dsize+8, v)
--              fixupConstRefs(si[S_ConstChain],0,slink)
if si[S_ConstChain] then
--  S_Name
    ?9/0
end if
            end if
end if
            slink = si[S_Clink]
        end while
    end if
    return 1
end function
constant r_DumpAtom = routine_id("DumpAtom")

procedure flatdump(sequence s, integer refcount, integer reloc=0)
integer l, l20
    s_addr = length(data_section)+dsize*5 -- (+20 or +40)
    l = length(s)
    l20 = (l+5)*dsize               -- (l*4+20 or l*8+40)
    d_addr += l20
    appenddsDword(0)                -- slack
    appenddsDword(l20)              -- maxlen (in bytes)
    appenddsDword(l)                -- length
    appenddsDword(refcount)         -- refcount
    appenddsType(#80)
    for i=1 to l do
        if reloc and s[i]!=0 then
            -- (for the optable)
            relocations[DATA][CODE] = append(relocations[DATA][CODE],length(data_section))
        end if
        appenddsDword(s[i])
    end for
end procedure

procedure DumpSequences()
--
--  Fairly obviously, this only deals with "all-constant" (sub)sequences, not things that need to be
--  built at run-time (using opMkSq/repeat/append etc). One case this specifically covers is (eg):
--
--      sequence s = {1,2,3}
--
--  which generates an unnamed constant {1,2,3} and a variable "s" that initially shares that value.
--  Obviously we need the constant for cases where s is repeatedly initialised then mauled, or when
--  the same all-constant (sub)sequence occurs several times in the source code. If, instead, s had 
--  been declared constant we could create just one symbol table entry. This (obviously) dumps that
--  {1,2,3} and links it up to any and all symbol table entries that need it. Technically, we ought
--  to increase reference counts on sub-items, but in practice that proves unnecessary. When we set
--  the reference count below, it is just large enough to cover any non-constants that get a ref at 
--  compile-time, and therefore not at run-time, but can later get a decref. (it must never hit 0)
--
--  Note: Since strings and atoms cannot contain nested references to other strings/atoms/sequences,
--  it is reasonable to blat them out in any order, including tree traversal, which has the advantage
--  or tallying up the refcount precisely once. However, with constant sequences it is considerably
--  more useful to have already processed any (nested) content, so symtab order is used.
--
object si
integer x_addr
sequence s
integer slink
integer refcount
integer l, l20
integer tidx
integer sidx
integer rd = iff(DLL?REFS:DATA)
--puts(1,"DumpSequences()\n")
    if X64 then
        if dsize!=8 then ?9/0 end if
    else
        if dsize!=4 then ?9/0 end if
    end if
    for i=1 to symlimit do
        si = flatsym2[i]
        if sequence(si)
        and si[S_NTyp]<=S_GVar2
        and and_bits(si[S_State],K_noclr+K_lit)=K_noclr+K_lit
        and si[S_vtype]=T_Dsq   --DEV erm?? (test type x(object y).. constant x thing={1,2,3})
        and sequence(si[S_value]) then  -- (nb relies on strings already replaced with x_addr+#80000000)
            x_addr = length(data_section)+5*dsize
            s = si[S_value]
            slink = i
            si = 0
            refcount = 0
            while slink do
                if flatsym2[slink][S_State]!=0 then
                    flatsym2[slink][S_value] = x_addr+#800000000
--DEV..?
                    if flatsym2[slink][S_NTyp]=S_Const then
                        if flatsym2[slink][S_ConstChain] then ?9/0 end if
                    end if
                    dsidx = (flatsym2[slink][S_Slink]+2)*dsize+9
                    relocations[DATA][rd] = append(relocations[DATA][rd],dsidx-1)   -- (qword)
                    setds(x_addr,1)
                    refcount += 1
                end if
                slink = flatsym2[slink][S_Clink]
            end while
            if refcount then
                l = length(s)
                l20 = (l+5)*dsize
                appenddsDword(0)                -- slack
                appenddsDword(l20)              -- maxlen (in bytes)
                appenddsDword(l)                -- length
                appenddsDword(refcount)         -- refcount
                appenddsType(#80)
                for j=1 to l do
                    si = s[j]
--                  if not integer(si) then
                    if not atom(si) or isFLOAT(si) then
                        -- see pmain.e/DoSequence for tidx handling, ie
                        --      subsequence => {ttidx},
                        --      string => {-ttidx},
                        --      atom => ttidx+0.5,
                        --      else integer (as-is)
                        if sequence(si) then
                            tidx = si[1]
                            if tidx<0 then tidx = -tidx end if
                        else
                            tidx = si-0.5
                        end if
                        sidx = tt[tidx+EQ]
                        si = flatsym2[sidx][S_value] -- = x_addr+#800000000
                        dsidx = length(data_section)+1
                        relocations[DATA][rd] = append(relocations[DATA][rd],dsidx-1)   -- (qword)
                        appenddsDword(0)
                        setds(and_bits(si,#3FFFFFFF),1)
                    else
                        appenddsDword(si)
                    end if
                end for
            end if
        end if
    end for
end procedure


integer rescan, rescancount
function ReconstructSequence(sequence s, integer node)
integer l, tidx, k, slink, state
object si
sequence sk     -- copy of symtab[k]
integer skt     -- verify sk[S_vtype], check for rescan
    slink = tt[node+EQ]
--printf(1,"ReconstructSequence: slink=%d\n",{slink})
    si = symtab[slink]
    state = si[S_State]
    if not and_bits(state,K_sqr) then   -- not already processed
        l = length(s)
        -- fixup substrings and subsequences...
        for i=1 to l do
            si = s[i]
--          if not integer(si) then
--DEV spotted in passing... cleanup suggestion (24/5/21) [get rid of outer test and replace else??]
--          if sequence(si) or isFLOAT(si) then
                if sequence(si) then
                    tidx = si[1]
                    skt = T_Dsq
                    if tidx<0 then -- a substring
                        tidx = 0-tidx
                        skt = T_string
                    end if
                    k = tt[tidx+EQ]
                    sk = symtab[k]
                    if DEBUG then
                        if sk[S_NTyp]!=S_Const then ?9/0 end if
                        if sk[S_vtype]!=skt then ?9/0 end if
                    end if
                    if skt = T_Dsq then
                        state = sk[S_State]
                        if not and_bits(state,K_sqr) then
--printf(1,"ReconstructSequence: rescan on %d, ",node) ?s
--if getc(0) then end if
                            rescan = 1
                            return 1
                        end if
                    end if
                    s[i] = sk[S_value]
-- (24/5/21)
--              else
                elsif isFLOAT(si) then
                    tidx = si-0.5
                    k = tt[tidx+EQ]
                    sk = symtab[k]
                    if DEBUG then
                        if sk[S_NTyp]!=S_Const then ?9/0 end if
                        if not find(sk[S_vtype],{T_N,T_atom}) then ?9/0 end if
                    end if
                    s[i] = sk[S_value]
                end if
--              end if
        end for
--printf(1,"ReconstructSequence: fixup on %d, ",node) ?s
--if getc(0) then end if
        while slink do
            si = symtab[slink]
            symtab[slink] = 0   -- kill refcount
            si[S_value] = s
            state = si[S_State]
            state = or_bits(state,K_sqr)
            si[S_State] = state
            symtab[slink] = si
            slink = si[S_Clink]
--printf(1,"ReconstructSequence: S_Clink=%d\n",{slink})
        end while
--else
--printf(1,"ReconstructSequence: K_sqr already set\n",{})
    end if
    return 1
end function
constant r_ReconstructSequence = routine_id("ReconstructSequence")


procedure dumpPathSet(sequence s)
-- dump pathset, aka symtab[T_pathset], which is {string,string,string,....}
--   or fileset[i], which is {n,string[,string,string,...]}
object si
integer x_addr
integer l, l20
integer rd = iff(DLL?REFS:DATA)

    l = length(s)
    l20 = (l+5)*dsize               -- (l*4+20 or l*8+40, ie elements+seq header)
    appenddsDword(0)                -- slack
    appenddsDword(l20)              -- maxlen (in bytes)
    appenddsDword(l)                -- length
    appenddsDword(1)                -- refcount
    appenddsType(#80)
    x_addr = length(data_section)
    for i=1 to l do
        appenddsDword(0)
    end for
    for i=1 to l do
        si = s[i]
        if string(si) then
            dumpString(si,1)
            dsidx = x_addr+(i-1)*dsize+1
            relocations[DATA][rd] = append(relocations[DATA][rd],dsidx-1)   -- (qword)
            setds(s_addr,1)
        else
            dsidx = x_addr+(i-1)*dsize+1
            setds(si)
        end if
    end for
    s_addr = x_addr
end procedure

procedure dumpFileSet(sequence s)
-- dump fileset, aka symtab[T_fileset], which is {{n,string}}, except for
--  the first, which may be {n,string[,string,string,..]}.
integer x_addr
integer l = length(s),
        ml = (l+5)*dsize            -- (l*4+20 or l*8+40, ie elements+seq header)
integer rd = iff(DLL?REFS:DATA)
    appenddsDword(0)                -- slack
    appenddsDword(ml)               -- maxlen (in bytes)
    appenddsDword(l)                -- length
    appenddsDword(1)                -- refcount
    appenddsType(#80)
    x_addr = length(data_section)
    for i=1 to l do
        appenddsDword(0)
    end for
    for i=1 to l do
        dumpPathSet(s[i])
        dsidx = x_addr+1+(i-1)*dsize
        relocations[DATA][rd] = append(relocations[DATA][rd],dsidx-1)   -- (qword)
        setds(s_addr,1)
    end for
    s_addr = x_addr
end procedure

sequence Signatures
sequence SigLinks

function CollectSignatures(sequence s, integer node)
integer slink = tt[node+EQ],
        k = 0
    while slink do
        -- Signal the symtab entry is "in use"; this info is 
        --  converted into a backpatch chain in DumpSymTab().
        if slink>T_object then
            if k=0 then
                Signatures = append(Signatures,s)
                SigLinks = append(SigLinks,0)
                k = length(SigLinks)
            end if
            flatsym2[slink][S_sig] = k
        end if
        slink = flatsym2[slink][S_Nlink]
    end while
    return 1
end function
constant r_CollectSignatures = routine_id("CollectSignatures")

procedure DumpSignatures()
integer refcount, slink, k
integer rd = iff(DLL?REFS:DATA)
    for i=1 to length(Signatures) do
        refcount = 0
        if X64 then
            s_addr = length(data_section)+#28
        else
            s_addr = length(data_section)+#14
        end if
        if bind and mapsymtab then
            for j=2 to length(Signatures[i]) do
                k = Signatures[i][j]
                if k>T_object then 
                    Signatures[i][j] = symtabmap[k] 
                end if
            end for
        end if
        slink = SigLinks[i]
        while slink do
            dsidx = slink
            slink = getdsDword(slink)
            relocations[DATA][rd] = append(relocations[DATA][rd],dsidx-1)   -- (qword)
            setds(s_addr,1)
            refcount += 1
        end while
        flatdump(Signatures[i],refcount)
    end for
end procedure

sequence Ids
sequence IdLinks

function CollectIds(sequence s, integer node)
integer slink, k
    slink = tt[node+EQ]
    Ids = append(Ids,s)
    IdLinks = append(IdLinks,0)
    k = length(IdLinks)
    while slink do
        -- Signal the symtab entry is "in use"; this info is 
        --  converted into a backpatch chain in DumpSymTab().
--DEV
--if atom(flatsym2[slink]) then exit end if
if atom(symtab[slink]) then exit end if
--if k=0 then
--  Ids = append(Ids,s)
--  IdLinks = append(IdLinks,0)
--  k = length(IdLinks)
--end if
--      flatsym2[slink][S_Name] = k
--      slink = flatsym2[slink][S_Nlink]
        symtab[slink][S_Name] = k
        slink = symtab[slink][S_Nlink]
    end while
    return 1
end function
constant r_CollectIds = routine_id("CollectIds")

procedure DumpIds()
integer refcount, slink
integer rd = iff(DLL?REFS:DATA)
    for i=1 to length(Ids) do
        refcount = 0
        s_addr = length(data_section)+4*dsize
        slink = IdLinks[i]
        if slink!=0 then
            while slink do
                dsidx = slink
                slink = getdsDword(slink)
                relocations[DATA][rd] = append(relocations[DATA][rd],dsidx-1)   -- (qword)
                setds(s_addr,1)
                refcount += 1
            end while
            dumpString(Ids[i],refcount)
        end if
    end for
end procedure

procedure DumpSignaturesAndLineTables()
integer rlink, tEQ
sequence si
--object lt
    clear_seq_tree()    -- create a new "signature pool"
    rlink = T_maintls
--if newEmit then
--  LineTabs = {}
--end if
    while rlink do
        si = flatsym2[rlink]
        flatsym2[rlink] = 0 -- kill ref count
        -- throw signature into the pool
        tt_sequence(si[S_sig])
--      si[S_sig] = tt[ttidx+EQ]    -- leave this comment in for searches
        tEQ = ttidx+EQ
--13/04/2010:
--      si[S_sig] = tt[tEQ]
        si[S_Nlink] = tt[tEQ]
        tt[tEQ] = rlink
--if not newEmit then -- now done in DumpSymTab() [DEV]
--      -- dump the linetab (non-pooled!)
--      lt = si[S_ltab]
--      if sequence(lt) then
----if newEmit then
----            LineTabs = append(LineTabs,{lt,rlink})
----            si[S_ltab] = length(LineTabs)
----else
--          flatdump(lt,1)
--          si[S_ltab] = s_addr
----end if
--      end if
--end if
        flatsym2[rlink] = si
if NEWGSCAN then
        rlink = g_scan[rlink]
        if rlink=-1 then exit end if
else
        rlink = si[S_Slink]
end if
    end while
    for i_to_o =T_integer to T_object do
        si = flatsym2[i_to_o]
        flatsym2[i_to_o] = 0 -- kill ref count
        tt_sequence(si[S_sig])
--      si[S_sig] = tt[ttidx+EQ]    -- leave this comment in for searches
        tEQ = ttidx+EQ
        si[S_Nlink] = tt[tEQ]
        tt[tEQ] = i_to_o
        flatsym2[i_to_o] = si
    end for
    Signatures = {}
    SigLinks = {}
    tt_traverseQ(r_CollectSignatures)
end procedure

function ltpack(sequence linetab, integer i)
-- (assumes linetab[$] is positive)
integer last = 0
string res = repeat(' ',length(linetab))
string c3 = repeat(#81,3)
string c5 = repeat(#80,5)
string this
sequence chunks = {}
integer lti, ltl
if linetab!={-2}
and linetab[$]<0 then
    if bind and mapsymtab then
        printf(1,"oops, pemit2.e/ltpack: linetab[$]<0 on symtab[%d(%d)]\n",{i,symtabmap[i]})
    else
        printf(1,"oops, pemit2.e/ltpack: linetab[$]<0 on symtab[%d]\n",{i})
    end if
    ?linetab    -- {-2}
end if
    -- first convert +ve offsets to deltas 
    --  (nowt sensible to be done with -ve skips)
    for i=1 to length(linetab) do
        lti = linetab[i]
        if lti>=0 then
            if last>0 then
                linetab[last] = lti-ltl
            end if
            last = i
            ltl = lti
        end if
    end for
    -- pack to binary bytes, held in a string
    --  (requiring just over 25%, for 32-bit,
    --   ymmv, and just over 12.5% for 64-bit)
    for i=length(linetab) to 1 by -1 do
        lti = linetab[i]
        if lti>=-126 and lti<=127 then
            res[i] = and_bits(lti,#FF)
        else
            if lti>=-#8000 and lti<=#7FFF then
                c3[2] = and_bits(floor(lti/#100),#FF)
                c3[3] = and_bits(lti,#FF)
                this = c3
            else
                c5[2] = and_bits(floor(lti/#1000000),#FF)
                c5[3] = and_bits(floor(lti/#10000),#FF)
                c5[4] = and_bits(floor(lti/#100),#FF)
                c5[5] = and_bits(lti,#FF)
                this = c5
            end if
            chunks = append(chunks,this&res[i+1..$])
            res = res[1..i-1]
        end if
    end for
    for i=length(chunks) to 1 by -1 do
        res &= chunks[i]
    end for
    return res
end function

include builtins\VM\optable.e

--global sequence e2optable
--with trace
procedure DumpSymTab()
object si
object lt
integer siNTyp, lensi
--integer p, N
atom fi
integer k, ltdsidx
object v
sequence optable
--integer l, l20
integer dsidx0
atom pathaddr, nameaddr
--object wassi      --DEV temp?
integer rd = iff(DLL?REFS:DATA)
    if not bind then ?9/0 end if
--SUG: flatsym2[T_integer..T_object] = tagset(T_object) -- {1..15}
    for i=T_integer to T_object do
        flatsym2[i] = i
    end for
if pathslast then
    flatsym2[T_pathset] = 0
    flatsym2[T_fileset] = 0
else
    dumpPathSet(filepaths)                      -- specials 1
    flatsym2[T_pathset] = s_addr+#80000000
    dumpFileSet(filenames)                      -- specials 2
    flatsym2[T_fileset] = s_addr+#80000000
end if
--29/3/15:
if newEmit then
    optable = create_optable(flatsym2)
    flatdump(optable, 1, 1)
if listing then
    symtab[T_optable] = optable     -- (for listing)
end if
----procedure flatdump(sequence optable, integer refcount)
--  s_addr = length(data_section)+dsize*5
--  l = length(optable)
--  l20 = (l+5)*dsize
--  d_addr += l20
--  appenddsDword(0)                -- slack
--  appenddsDword(l20)              -- maxlen (in bytes)
--  appenddsDword(l)                -- length
--  appenddsDword(1)                -- refcount
--  appenddsType(#80)
--  for i=1 to l do
----if newEmit then
----        si = s[i]
----        if and_bits(si,#80000000) then
----            relocations[DATA][DATA] = append(relocations[DATA][DATA],length(data_section))
----        end if
----        appenddsDword(si)
----else
--      relocations[DATA][CODE] = append(relocations[DATA][CODE],length(data_section))
--      appenddsDword(optable[i])
----end if
--  end for
--  DSvsize += l20
--end procedure
--wrong=10

    flatsym2[T_optable] = s_addr+#80000000
--  flatsym2[T_ds4] = 0+#80000000
    flatsym2[T_ds4] = 0
end if
    flatsym2[T_constm1] = flatsym2[T_constm1][S_Slink]  -- gvar[1]
    flatsym2[T_const0] = flatsym2[T_const0][S_Slink]    -- gvar[2]
    flatsym2[T_const1] = flatsym2[T_const1][S_Slink]    -- gvar[3]

    for i=1 to symlimit do
        si = flatsym2[i]
        if sequence(si) 
        and si[S_State]!=0 then
            siNTyp = si[S_NTyp]
            if siNTyp<=S_TVar then
                flatsym2[i] = 0 -- kill refcount
                v = si[S_value]
if not integer(v) then
                si[S_value] = 0 -- [DEV to go?]
--30/12/15:
--elsif bind and mapsymtab and and_bits(si[S_State],K_rtn) then
elsif bind and and_bits(si[S_State],K_rtn) then
    if mapsymtab then
                v = symtabmap[v]
    end if
                si[S_value] = v
                if listing then
                    symtab[i][S_value] = v
                end if
end if
                lensi = length(si)
                if siNTyp=S_TVar then
                    if lensi>S_Tidx then
                        si = si[1..S_Tidx]  -- discard compile-time cruft [S_ErrV]
                    end if
                else
                    if lensi>S_Clink then
                        si = si[1..S_Clink] -- discard compile-time cruft [S_ErrV]
                    end if
                end if
                -- Delete unused entries. Function results depend on    --DEV misplaced comment?!
                -- the routine entry (i+1), and (may) get deleted in 
                -- the next iteration, some 55 lines above.
                k = si[S_Name]
                if k!=-1 then
                    dsidx = length(data_section)+1+(S_Name+4)*dsize
if k>length(IdLinks) then
--  printf(1,"oops, bad IdLinks index, symtab[%d], line 2807 pEmit2.e\n",i)
    printf(1,"oops, bad IdLinks index, symtab[%d(%d)], line 2807 pEmit2.e\n",{i,symtabmap[i]})
?si
?getname(si[S_Name],-2)
--  si[S_Name] = "***OOPS***"
    si[S_Name] = 0
else
                    si[S_Name] = IdLinks[k]
                    IdLinks[k] = dsidx
end if
                end if
if bind and mapsymtab then
                if siNTyp=S_TVar then
                    k = si[S_Slink]     if k!=0 then si[S_Slink] = symtabmap[k] end if
--                  k = si[S_Clink]     if k!=0 then si[S_Clink] = symtabmap[k] end if
                    k = si[S_Nlink]     if k!=0 then si[S_Nlink] = symtabmap[k] end if
                end if
                k = si[S_vtype]     if k>T_object then si[S_vtype] = symtabmap[k] end if
end if
                flatdump(si,1)
                flatsym2[i] = s_addr+#80000000
            else -- siNTyp>=S_Nspc
                if siNTyp>S_Rsvd then
                    lensi = length(si)
                    if lensi>S_1stl then
                        si = si[1..S_1stl]      -- discard compile-time cruft [S_Efct,S_ErrR]
                    end if
--DEV (23/6/16) maybe we should always keep S_Efct: (trouble is, pretty meaningless, and doubly so for a packed symtab)
--                  if lensi>S_Efct then
--                      si = si[1..S_Efct]      -- discard compile-time cruft [S_ErrR]
--                  end if
                end if
                if siNTyp>=S_Type then
                    dsidx = length(data_section)+1+(S_sig+4)*dsize
--                  if show_full_symtab 
--                  or integer(si[S_sig]) then
                    if integer(si[S_sig]) then
                        k = si[S_sig]
                        si[S_sig] = SigLinks[k]
                        SigLinks[k] = dsidx
                    end if
                    lt = si[S_ltab]
                    si[S_ltab] = 0
--4/11/19 (structs)
if lt!={} then
                    lt = ltpack(lt,i)
end if
                    if listing then
                        symtab[i][S_ltab] = lt  -- no...
                    end if
                    ltdsidx = length(data_section)+1+(S_ltab+4)*dsize
                end if
            k = si[S_Name]
            if k!=-1 then
                dsidx = length(data_section)+1+(S_Name+4)*dsize
--if show_full_symtab then
                if k>length(IdLinks) then
if bind and mapsymtab then
                    printf(1,"k(%d)>length(IdLinks)(%d) for symtab[%d(?%d)][S_Name]\n",{k,length(IdLinks),i,symtabmap[i]})
else
                    printf(1,"k(%d)>length(IdLinks)(%d) for symtab[%d][S_Name]\n",{k,length(IdLinks),i})
end if
                else
                    si[S_Name] = IdLinks[k]
                    IdLinks[k] = dsidx
                end if
            end if
if bind and mapsymtab then
                k = si[S_Slink]         if k!=0 then si[S_Slink] = symtabmap[k] end if
--temp: (triggers lots!)
--if NEWGSCAN then if k!=0 then ?"pEmit2.e line 2854 (k!=0)" end if end if
                k = si[S_Nlink]         if k!=0 then si[S_Nlink] = symtabmap[k] end if
                if siNTyp>=S_Type then
                    k = si[S_Parm1]     if k!=0 then si[S_Parm1] = symtabmap[k] end if
                end if
--/*
 (added untested 24/6/15) [this is >=S_Nspc anyway]
--*/
--  NO          k = si[S_vtype]     if k!=0 then si[S_vtype] = symtabmap[k] end if
end if
                flatdump(si,1)
                flatsym2[i] = s_addr+#80000000
                if siNTyp>=S_Type then
                    -- Add a relocation for [S_il]. note that [S_il] itself is left as an
                    -- offset to codesection (ie/eg symtab[T_maintls][S_il] is 0), hence
                    -- (for newEmit) plist.e performs a +ImageBase2+BaseOfCode2 to match this.
                    -- We have already calculated ltdsidx(==S_ltab) so (-dsize)==S_il.
if not bind then ?9/0 end if -- 27/2/15 just see
                    relocations[DATA][CODE] = append(relocations[DATA][CODE],ltdsidx-1-dsize)   -- (qword)
                    if sequence(lt) then
                        relocations[DATA][rd] = append(relocations[DATA][rd],ltdsidx-1)     -- (qword)
                        dsidx = ltdsidx
                        setds(length(data_section)+4*dsize,1)
                        dumpString(lt,1)
                    end if
                end if
            end if
        end if
    end for

    -- relocate symtab[T_ds4] (start of data section, when compiled) normally:
    if X64 then
        dsidx = getdsQword(9)+1
--if (dsidx-9+T_ds4*8)=TRAP then ?9/0 end if --NO
        relocations[DATA][DATA] = append(relocations[DATA][DATA],dsidx-9+T_ds4*8)
    else
        dsidx = getdsDword(9)+1
--if (dsidx-5+T_ds4*4)=TRAP then ?9/0 end if -- NO
        relocations[DATA][DATA] = append(relocations[DATA][DATA],dsidx-5+T_ds4*4)
    end if
--printf(1,"dsidx=%08x\n",dsidx)
--?dsidx
    if pathslast then
        dsidx0 = dsidx
    end if
--wrong=60

if bind and mapsymtab then
    for i=1 to length(symtabmap) do
        fi = symtabmap[i]
        if fi!=0 then
            fi = flatsym2[i]
--          if X64 then ?9/0 end if
            if and_bits(fi,#80000000) then
                fi = and_bits(fi,#7FFFFFFF)
                if and_bits(dsidx-1,1) then ?9/0 end if
--              relocations[DATA][DATA] = append(relocations[DATA][DATA],dsidx-1)   -- (qword)
                relocations[DATA][rd] = append(relocations[DATA][rd],dsidx-1)   -- (qword)
                setds(fi,1)
            else
                setds(fi)
            end if
--1/8/14:
--      elsif not show_full_symtab then
        else
            if listing then
                symtab[i]=0
            end if
        end if
    end for
else
    for i=1 to length(symtab) do
if atom(flatsym2[i]) then
        fi = flatsym2[i]
--      if X64 then ?9/0 end if
        if and_bits(fi,#80000000) then
            fi = and_bits(fi,#7FFFFFFF)
            if and_bits(dsidx-1,1) then ?9/0 end if
--          relocations[DATA][DATA] = append(relocations[DATA][DATA],dsidx-1)   -- (qword)
            relocations[DATA][rd] = append(relocations[DATA][rd],dsidx-1)   -- (qword)
            setds(fi,1)
        else
            setds(fi)
        end if
else
    setds(0)
end if
    end for
end if
--wrong=100
    DumpSignatures()
--wrong=200
    DumpIds()
--wrong=300
    if pathslast then
        dumpPathSet(filepaths)                      -- specials 1
-- is this needed for listing or anything?
        flatsym2[T_pathset] = s_addr+#80000000
        pathaddr = s_addr
        dumpFileSet(filenames)                      -- specials 2
        flatsym2[T_fileset] = s_addr+#80000000
        nameaddr = s_addr
        dsidx = dsidx0+T_pathset*dsize
        setds(pathaddr,1)
        setds(nameaddr,1)
    end if
--wrong=999
end procedure

--with trace
procedure dumpDLLrelocs()
-- output packed offset table for pStack/initStack to fixup refs (DLL only)
integer rdri, offset, last=0
sequence rdr = sort(relocations[DATA][REFS])
--?rdr
--trace(1)
    for i=1 to length(rdr) do
        rdri = rdr[i]
        offset = rdri-last
--if i<=4 then
--printf(1,"%08x,%x\n",{rdri,offset})
--end if
        last = rdri
        if offset<4 then ?9/0 end if
        if offset<=#FF then
--          data_section &= offset  -- fits in a byte
            data_section &= and_bits(offset,#FF)    -- fits in a byte
        elsif offset<=#FFFF then
            data_section &= #02     -- word follows
            poke2(m4, offset)
            string s = peek(m42)
            data_section &= s
        elsif offset<=#FFFFFFFF then
            data_section &= #01     -- dword follows
            appenddsDword(offset)
        else
            ?9/0
        end if
--if i<=4 then
--?data_section[-12..-1]
--end if
    end for
    data_section &= '\0'            -- terminator
end procedure

--DEV...
atom k32=0, xVirtualProtect, pDword
--constant k32 = open_dll("kernel32.dll"),
----         xLoadLibrary = define_c_func(k32,"LoadLibraryA",{C_PTR},C_PTR),
----         xGetProcAddress = define_c_func(k32,"GetProcAddress",{C_PTR,C_PTR},C_PTR),
--       xVirtualProtect = define_c_func(k32,"VirtualProtect",{C_PTR,C_INT,C_INT,C_PTR},C_INT),
--       PAGE_EXECUTE_READWRITE = #40,
--       pDword = allocate(8)
constant PAGE_EXECUTE_READWRITE = #40

--DEV I've hard-coded 4096 here, might instead want to do something like:
--       xGetSystemInfo = define_c_proc(k32,"GetSystemInfo",{C_POINTER}),
--       SYSTEM_INFO = allocate(44),    -- (36 on 32bit, 44 on 64bit[?])
--       SYSTEM_INFO_dwPageSize = 4
--
--  c_proc(xGetSystemInfo,{SYSTEM_INFO})
--
--constant dwPageSize = peek4s(SYSTEM_INFO+SYSTEM_INFO_dwPageSize),
--       psm1 = dwPageSize-1,
--       highbits = xor_bits(psm1,#3FFFFFFF)
--then, instead,
-- round block up to next dwPageSize bytes
--  bytesize = and_bits(highbits,bytesize+psm1)

--/*
            mallopt(int param, int value);
            M_MMAP_THRESHOLD, (>128*1024)
constant M_MMAP_THRESHOLD = -3
--*/

function AllocateBlock(integer bytesize)
-- round block up to next 4096 bytes
    bytesize = and_bits(#3FFFF000,bytesize+#FFF)
    return allocate(bytesize)
end function

include pbinary.e

-- also used by pilasm.e, pmain.e:
global procedure Or_K_ridt(integer symidx, integer flags)
integer p, maxparams, useplus1
    --
    -- Set the "known target of routine_id" flag...
    --
    if and_bits(flags,K_ridt)=0 then ?9/0 end if    -- sanity check
    symtab[symidx][S_State] = or_bits(symtab[symidx][S_State],flags)
--if NEWGSCAN then  --DEV??
--  kridt_scan[symidx] = ???
--end if
    --
    -- ...and mark all parameters as used to avoid warnings:
    --
    if symtab[symidx][S_NTyp]<S_Type then ?9/0 end if -- sanity check
    p = symtab[symidx][S_Parm1]
    maxparams = length(symtab[symidx][S_sig]) - 1
-- 23/1/17 [IDE.exw crash calling routine_id(x) mid-procedure x().]
--  useplus1 = (maxparams>1 and symtab[p][S_Slink]=0)
-- 22/2/20 p==0 (masking    string sr = sort[n[i..$]]
--                                          ^ attempt to subscript an atom)
    useplus1 = (maxparams>1 and p!=0 and symtab[p][S_Slink]=0)
--23/7/2019 (routine_id("abs") when pmaths.e had not been included...)
--  while maxparams do
    while maxparams and p do
        if DEBUG then
            if symtab[p][S_NTyp]!=S_TVar then ?9/0 end if
            -- sanity check: K_noclr must NOT be set on any params!
            if and_bits(symtab[p][S_State],K_noclr) then ?9/0 end if
        end if
        symtab[p][S_State] = or_bits(symtab[p][S_State],S_used)
        if useplus1 then
            p += 1
        else
            p = symtab[p][S_Slink]
        end if
        maxparams -= 1
    end while
end procedure

--/*
constant KB = 1024,
         MB = KB*KB,
         GB = KB*MB,
         --
         -- the following written so that a single change to sw affects all:
         --
         sw = 11,                               -- size width of 11 characters
         dpsfmt = sprintf("%%%d.2f%%s",sw-2),   -- with decimal places and suffix
                   -- eg/ie "%9.2f%s"
         sfmt = sprintf("%%%d.0f%%s",sw-2),     -- no d.p, but still suffix
                 -- eg/ie "%9.0f%s"
         bfmt = sprintf("%%%d.0f",sw)           -- no d.p, no suffix (size in bytes)
                -- eg/ie "%11.0f"

function Size00(atom size, integer factor, sequence suffix)
-- common code for Size function, to avoid ".00" displaying
sequence res, params
    params = {floor(size/factor),suffix}
    if remainder(size,factor) then
        res = sprintf(dpsfmt, params)           -- eg 9.2GB
        if equal(res[sw-4..sw-2],".00") then
            res = sprintf(sfmt, params)         -- eg   9MB (not "9.00MB" when really 9.00004701MB)
        end if
    else
        res = sprintf(sfmt, params)             -- eg 100KB (that is, when really 100.00000000KB)
    end if
    return res
end function    

function Size(atom size)
    if size>=GB then
        return Size00(size,GB,"GB")
    elsif size>=MB then
        return Size00(size,MB,"MB")
    elsif size>=KB then
        return Size00(size,KB,"KB")
    end if
    return sprintf(bfmt, size)                  -- eg     0 (ie absolute size in bytes)
end function

--*/

--with trace
global procedure finalfixups2(sequence path, sequence outfile, atom t)
--DEV
-- In bind mode copy the stub cl1 (usually p.exe) to outfile, 
--  dump code linked to this stub, along with the symtab.
-- In interpret mode outfile is not used, code is linked to 
--  the VM under the compiler and existing symtab in memory.
integer vmax, k, symidx, u, nTyp, ridlink, nslink, f, lensi, vtype
object sv, svil, xi, line1, si, svname
sequence s5sets
atom t0
--, rbicb

integer siNTyp

--integer kfirst, klast
integer xType
atom xMin,xMax

integer isKridt
sequence re 

integer node, slink, snext

integer firsttime

integer p, maxparams, N

--object dbg
object opName -- for dumpil (to dump opNames[])

string s
atom lib
integer libidx
atom thunk

integer dslen
integer maxlen
integer offset
integer DSvaddr4
--integer CSvaddr4
sequence prevsym        -- previous symtab
sequence optable
integer relptr
integer showmapsymtab = 0
--object dbg

--1/1/18:
if not repl then
    UnAliasAll()
end if
--if Z_ridN!=0 then -- still OK...
----    Z_ridN = 181
--  ?symtab[Z_ridN]
--end if

--puts(1,"finalfixups2 started\n")

--/*
-- imports should be eg {{"kernel32.dll",{"LocalFree","GetLastError","FormatMessageA"},{#4C,#17,#58}},
--                       {"user32.dll",{"MessageBoxA"},{#69}}},
--          or (rarely) {} if no imports whatsoever are required.
--                         Routine names get automatically [tag]sorted per dll, as required.
--                         The {#4C,#17,#58} and {#69} are link chain starts; after creating 
--                         the MZ/PE headers these get patched with the rqd thunk addresses.

    APIlibs = {"kernel32.dll"}
    APIerrlib = {{1,8,218}}
    APINames = {{1, "GetStdHandle"}, {1, "WriteFile"}}
    APIerritem = {{1,8,229}, {1,17,698}}

    for i=1 to length(APIlibs) do
        s = APIlibs[i]
        k = find(s,Names)
        if k=0 then
            APIerritem = APIerrlib
            APIerror(i, "no such library")
            ?9/0 -- sanity check
        end if
        APIlibs[i] = k
    end for
    for i=1 to length(APINames) do
        libidx = APIlibs[APINames[i][1]]
        nameidx = find(APINames[i][2],HintNames[libidx][2])
        if nameidx=0 then
            APIerror(i, "not found in import table")
            ?9/0 -- sanity check
        end if
        offset = HintNames[libidx][1][nameidx]
        APINames[i] = offset
    end for
--*/
if bind and mapsymtab then
    symtabmap = repeat(0,length(symtab))
--  for i=1 to T_maintls do
--      symtabmap[i] = 1
--  end for
-- or[?]:
--  symtabmap = repeat(1,length(symtab))
end if

--DEV use readheader? - NO!
--DEV stash info for plist (32 and 64 bit), esp under bind=0/-d!
    imports = repeat(0,length(APIlibs))
    for i=1 to length(APIlibs) do
        s = APIlibs[i]
        if not norun or not bind then
            lib = open_dll(s)
            if lib=NULL then
                APIerritem = APIerrlib
                APIerror(i, "error loading library")
            end if
            APIerrlib[i] = lib
        end if
        imports[i] = {s,{},{}} -- {dll name, entry point names, link chain starts}
    end for
    for i=1 to length(APINames) do
        {libidx,s} = APINames[i]
        if not norun or not bind then
            thunk = get_proc_address(APIerrlib[libidx],s)
            if thunk=NULL then
                APIerror(i, "no such function")
            end if
        end if
        if bind then
            imports[libidx][2] = append(imports[libidx][2],s) -- add entry name
            imports[libidx][3] = append(imports[libidx][3],0) -- link chain:=NULL, for now
            APINames[i][2] = length(imports[libidx][3])
        end if
        if not norun or not bind then
            APIerritem[i] = thunk
        end if
    end for
    if not bind then
        --added 19/10/15:
        if length(APIerritem)!=0 then
            if X64 then
                thunktable = allocate(length(APIerritem)*8)
                poke8(thunktable,APIerritem)
            else
                thunktable = allocate(length(APIerritem)*4)
                poke4(thunktable,APIerritem)
            end if
        end if
    end if
    if bind then
--if DLL then
        relocations = {{{},{},{}},  --  ie [CODE][DATA],[CODE][CODE],[CODE][IMPORTS],
                       {{},{},{}}}  --  ie [DATA][DATA],[DATA][CODE],[DATA][REFS]
--else
--      relocations = {{{},{}},{{},{}}}     --  ie [DATA][DATA],[DATA][CODE],[CODE][DATA],[CODE][CODE]
                                            -- (or [CODE][CODE],[CODE][DATA],[DATA][CODE],[DATA][DATA])
                                            -- eg [CODE][DATA] contains the offsets in CODE that refer
                                            --    to DATA, so we need to pbinary.e/fixup() that once we
                                            --    know exactly how big and where they both are.
--end if
    end if

    -- check for unused/undefined/unassigned and if debugleak emit cleanup code (see pilx86.e):

    unused_cleanup(0)

--puts(1,"finalfixups2 line 3718\n")

    if listing then
        craddr={}
        cridx={}
--DEV 11/4/14:
        ImageBase = 0
    elsif bind>=2 then
        if bind>3 then re="re-" else re="" end if
        printf(1,"Self-host round %d: compiled OK (%3.2fs), %screating %s ",{bind-1,t,re,outfile})
    end if

--  if not LastStatementWasAbort then
--      Emit1(opRetf)
--  end if
--20/1/15 (opRetf was past last line of file, thus not appearing in the list.asm)
--  emitline = line
--22/1/15 (going bananas on a t00, "if ltline>skipline then ?9/0 end if -- major guff" in lineinfo())
--  emitline = line-1
    emitline = line-(emitline<line)     -- (works fine, btw, but I think the following is clearer)
--  if emitline<line then emitline = line-1 end if
--  if emitline>line then ?9/0 end if   -- sanity check
--  emitline = tokline
--puts(1,"warning: opRetf omitted, line 4022 pemit2.e\n")
    if not suppressopRetf then
        agcheckop(opRetf)
        apnds5(opRetf)
    end if

    symtab[T_maintls][S_il] = s5

--pp(filepaths)
--pp(filenames)

--puts(1,"finalfixups2 line 3778\n")

    --
    -- finalise gvar nos and do initial linkup of udts and toplevelsubs:
--newEmit:
    -- initial linkup of udts and toplevelsubs:
    --
--DEV I think this can go:
    vmax = -TIDX
--?vmax
--  vi = 0
    vi_active = 0
if NEWGSCAN then
    kridt_scan = repeat(0,length(symtab))
--  vi_active = T_maintls
--  kridt_scan[T_maintls] = T_maintls   -- (circular loop [so we can test if last is on it])
--  kridt_scan[T_maintls] = -1
--  kridt_scan[T_maintls] = -9          -- (nick existing processing (??)) [doh, that's not here anyway and means "NOT in S_Slink"...]
    vi_active = -1
end if
    for v=1 to symlimit do
--tryme (MARKTYPES)
--  for v=T_Asm+1 to symlimit do
        sv = symtab[v]
        if sequence(sv) then
            nTyp = sv[S_NTyp]
if NEWGSCAN then
  if v!=T_maintls then
            if (nTyp>=S_Type and and_bits(sv[S_State],K_used+K_ridt) and sequence(sv[S_il]) and length(sv[S_il])) -- top level and K_ridt subs
            or (v=T_command_line and Z_command_line) then -- link up T_command_line if rqd
-- instead of using S_Slink:
--SUG: force K_used on sv[S_State]??
--              kridt_scan[v] = kridt_scan[T_maintls]
--              kridt_scan[T_maintls] = v
--erm, or ?? (and the final "" below)
                kridt_scan[v] = vi_active
                vi_active = v
            end if
  end if
else
--MARKTYPES...
            if (nTyp=S_Type and v>T_Asm)                            -- user defined types
--          if (MARKTYPES=0 and nTyp=S_Type and v>T_Asm)            -- user defined types
            or (nTyp>S_Type and and_bits(sv[S_State],K_used) and sequence(sv[S_il]) and length(sv[S_il])) -- top level subs
            or (v=T_command_line and Z_command_line) then -- link up T_command_line if rqd
--DEV if nTyp>SType and sv[S_il] = jmp opRetf then we could kill off the entry, as long as 
--      we also kill all opCallOnce to it (or merge to one dummy). Not that you'd ever be
--      able to measure the savings...
                --
                -- We do this because we don't track opCallOnce or opTchk in the	[DEV NEWGSCAN for opTchk]
                -- same way that we do with opFrame (see scanforShortJmp).
                -- True, there may be some udt processed that are not strictly
                -- needed, but they are usually quite small, compared to say
                -- pretty_print, wildcard_match or maybe half of win32lib.
                -- Alternatively: don't auto-linkup S_Type here but track opTchk
                --  in scanforShortJmp; I think you'd find that slower.
                --  update: erm, track opTchk would now be in pilx86.e, I think.
                --
----DEV temp!
--printf(1,"pemit.e line 2088: linking symtab[%d] on vi chain\n",v)
                sv = 0
--              symtab[v][S_Slink] = vi
                symtab[v][S_Slink] = vi_active
--DEV/SUG (28/02/19, inline)
--                          Or_K_ridt(i,K_used+K_ridt)
--20/12/15:
--28/02/19:
--              symtab[v][S_State] = or_bits(symtab[v][S_State],K_used)
if K_RIDT_UDTS and nTyp=S_Type and v>T_Asm then
                symtab[v][S_State] = or_bits(symtab[v][S_State],K_used+K_ridt)
else
                symtab[v][S_State] = or_bits(symtab[v][S_State],K_used)
end if
--              vi = v
                vi_active = v
--9/2/20:
--          elsif nTyp>S_Type then
            elsif nTyp>S_Type and not repl then
--          elsif nTyp>S_Type or (MARKTYPES and nTyp=S_Type and v>T_Asm) then
--this should work, when MARKTYPES=1: (or c/should we T_Asm[+1]..symlimit?? - yes)
--          elsif nTyp>=S_Type and v>T_Asm then
--(with new loop:)
--          elsif nTyp>=S_Type then
                symtab[v][S_Slink] = -9     -- (not on symtab[T_maintls][S_Slink] chain)
            end if
end if -- NEWGSCAN
        end if
    end for

if NEWGSCAN then
    --DEV test this on an empty file (both interpret and compile[-nodiag])
    if vi_active=T_maintls then ?9/0 end if -- sanity check... (circular loop?)
--  if vi_active=-1 then ?9/0 end if    -- sanity check... (empty/broken loop?)
    kridt_scan[T_maintls] = vi_active
else
--  symtab[T_maintls][S_Slink] = vi -- more entries will be dynamically added to this chain...
    symtab[T_maintls][S_Slink] = vi_active -- more entries will be dynamically added to this chain...
end if

--DEV test code (seems fine):
--  u = 0
--  for v=symlimit to 1 by -1 do
--      sv = symtab[v]
--      if sequence(sv) then
--          nTyp = sv[S_NTyp]
--          if nTyp<=S_GVar2 then   -- S_Const and S_Gvar2
--              u = sv[S_Slink]
--              exit
--          elsif nTyp=S_TVar
--            and u=0 then
--              u = sv[S_Tidx]
--          end if
--      end if
--  end for
--  if u!=vmax then ?9/0 end if


--  check_symtab()          -- ... some in here, the rest in scanforShortJmp.

--DEV/SUG can we not just do this in psym.e?
    for i=1 to T_object do
        si = symtab[i]
        if sequence(si) then
            u = si[S_State]
if not and_bits(u,K_used) then
--  ?"setting K_used line 3433 pEmit2.e"
            u = or_bits(u,K_used)
            symtab[i] = 0           -- kill refcount
            si[S_State] = u
            symtab[i] = si
end if
        end if
    end for

--puts(1,"finalfixups2 line 3839\n")

    if some_unresolved_rtnids then
        for i=T_Bin+1 to symlimit do
            si = symtab[i]
            if sequence(si)
--MARKTYPES
--          and si[S_NTyp]>S_Type then  -- ie func or proc
            and si[S_NTyp]>S_Type-NEWGSCAN then -- ie (func or proc) or [NEWGSCAN] (type or func or proc)
--          and si[S_NTyp]>=S_Func-MARKTYPES then   -- ie func or proc
--NEWGSCAN:
--          and si[S_NTyp]>=S_Type then -- ie type or func or proc
                u = si[S_State]
--DEV 20/09/2013 try pulling this tooth then... (solves problem of [indirect] routineid("open_dll") getting -1)
--              if i>T_Ainc or and_bits(u,S_used) then
                if not atom(si[S_il]) then
                    f = si[S_FPno]
--DEV (16/03/2013) if we add parentscope to routine_id then we must add locals...
-- while fixing this, try an inner "if atom(si[S_il]) then ?9/0 end if -- sanity check"
-- (5/7/13: in 4.0.5 you can procedure p(integer rid=routine_id("fred")) and it will
--          resolve to a fred declared in the caller, not the callee... ittwima)
--DEV f=0 only applies to T_integer..T_object, I think...
if f=0 then ?9/0 end if
                    if and_bits(u,K_gbl)
--                  or (not atom(si[S_il]) and f and unresolved_routine_ids[f]) then
                    or (f and unresolved_routine_ids[f]) then
                        if not and_bits(u,K_used) then  -- not top_level_sub
                            si = 0
                            Or_K_ridt(i,K_used+K_ridt)
if NEWGSCAN then
--                          Or_K_ridt(i,K_ridt) -- (sug)
--  ?9/0 -- (or just a "not NEWGSCAN" here, if Or_K_ridt() is going to manage?... [NAH])
--                          if kridt_scan[i]!=0 then ?9/0 end if    -- (or if==0 then next two?)
                            if kridt_scan[i]=0 then
                                kridt_scan[i] = kridt_scan[T_maintls]
                                kridt_scan[T_maintls] = i
                            end if
else
                            symtab[i][S_Slink] = symtab[T_maintls][S_Slink]
                            symtab[T_maintls][S_Slink] = i
end if
                        end if
                    end if
                end if
            end if
        end for
    end if

--puts(1,"finalfixups2 line 3872\n")

--  dbg = symtab[601]
--DEV if not bind and addRoutineId was never called then return

    for i=symlimit to 1 by -1 do
        si = symtab[i]
        if sequence(si) then
            u = si[S_State]
            if and_bits(u,K_rtn) then
                if bind then
                    lensi = length(si)
                    if lensi>S_Clink then
                        symtab[i] = 0
--DEV as above (needs S_Tidx moving up first) [check if this ever triggers]
                        si = si[1..S_Clink] -- discard compile-time cruft [S_Tidx..S_Init]
                        symtab[i] = si
                    end if
                end if
if not NEWGSCAN then
                k = si[S_value]     -- the routine number
                si = symtab[k]
                u = si[S_State]
                if not and_bits(u,K_used) 
--erm...
--              and si[S_NTyp]>S_Type then
                and si[S_NTyp]>S_Type-NEWGSCAN then
if NEWGSCAN then printf(1,"9/0 line 3516 pEmit2.e (symtab[%d], state=%x)\n",{i,u}) showmapsymtab = i end if -- (why not already K_ridt'd?) [or just skip k=si[S_value] and everything after?]
--symtabmap[i]
--      showmapsymtab = i

--              and si[S_NTyp]>=S_Func-MARKTYPES then
--              and si[S_NTyp]>=S_Type then
--DEV new routine, AddToSlink(i,K_used), also marks all params etc as "in use"...?
--                  si = 0
--                  AddToSlink(i,K_used)
                    u = or_bits(u,K_used)
                    symtab[k] = 0   -- kill refcount
                    si[S_State] = u
                    si[S_Slink] = symtab[T_maintls][S_Slink]
                    symtab[k] = si
                    symtab[T_maintls][S_Slink] = k
--28/02/19 let's see how often this is done... (?? routines still in symtab that were never 0'd ??)
--?{"pEmit2.e line 3467: S_Slink'd",k,si}
--on my test case: pAlloc.e:free, sort.e:tagsort and column_compare. All had a routine_id, though
--were not actually called (directly). [OK]
                end if
end if --(NEWGSCAN)
            end if
        end if
    end for
    si = 0  -- kill refcount

--puts(1,"finalfixups2 line 3908\n")

    -- finalise code segments
--  DSvsize = vmax*4
    CSvsize = 0
    s5sets = {}
    s5sizes = {}
    s5v = {}
--  if debug then
--      s5symn = {}
--  end if
    if showfileprogress then
        printf(1,"performing final fixups, gvar_scan started at %3.2f...\n",time()-t)
        t0 = time()
    end if

    if bind and not dumpil then
        rescancount = 0

        while 1 do  -- while gvar_scan improves matters
if NEWGSCAN then
            g_scan = kridt_scan
end if
--          vi = T_maintls  -- follow (volatile) K_used chain from top_level_sub[main], aka symtab[T_maintls][S_Slink].
            vi_active = T_maintls   -- follow (volatile) K_used chain from top_level_sub[main], aka symtab[T_maintls][S_Slink].
--          while vi do
            while vi_active do
--4/11/19: (structs)
if symtab[vi_active][S_il]!=0 then
--              s5thunk(symtab[vi][S_il]) -- sets s5
                s5thunk(symtab[vi_active][S_il]) -- sets s5
--              symtab[vi][S_il] = 0    -- kill refcount
                symtab[vi_active][S_il] = 0 -- kill refcount
--              gvar_scan(vi)
                gvar_scan(vi_active)
--              symtab[vi][S_il] = s5
                symtab[vi_active][S_il] = s5
end if
if NEWGSCAN then
                vi_active = g_scan[vi_active]
                if vi_active=-1 then exit end if
else
--              vi = symtab[vi][S_Slink]
                vi_active = symtab[vi_active][S_Slink]
end if
            end while
        
            -- kill any gvar info gathered for parameters of routines that could be
            --  invoked via call_proc/call_func/call_back (targets of routine_id),
            --  since that will not include any values/types/etc from such calls.
--if NEWGSCAN then
--          -- temp? (as below) [NB:P this replace of g_scan with kridt is, erm, silly...]
--          for i=1 to length(g_scan) do
--              if g_scan[i]!=0 
--              and kridt_scan[i]=0 then
--                  if and_bits(symtab[i][S_State],K_ridt) then ?9/0 end if
--              end if                  
--          end for
--          g_scan = kridt_scan     --erm, or use it direct here?
--                                  -- (actually, it doesn't matter, if we rebuild every time, including the last)
--          -- (one thing we could do is check nowt in g_scan not in kridt_scan has K_ridt, maybe/separately..[as above])
--end if
--          vi = T_maintls  -- hop down all routines again
            vi_active = T_maintls   -- hop down all routines again
--          while vi do
            while vi_active do  
--DEV/SUG NEWGSCAN:
--          while true do
--              u = symtab[vi][S_State]
                u = symtab[vi_active][S_State]
                isKridt = and_bits(u,K_ridt)    -- known routine_id target?
--DEV 20/09/2013 no idea why this lot was commented out, put back in:
--      "" (ah-ha: we are scanning the K_used chain here... no good)
--              if not isKridt then
--                  if some_unresolved_rtnids then
--                      if and_bits(u,K_gbl) then
--                          isKridt = 1
--                      else
--                          k = symtab[vi][S_FPno]
--                          if k and unresolved_routine_ids[k] then
--                              isKridt = 1
--                          end if
--                      end if
--                  end if
--              end if
-- (20/09 ends)
                if isKridt then
                    -- routine is/could be target of routine_id...
                    -- so destroy any gvar info gathered by gvar_scan for the 
                    --  routine parameters (since that will not include any 
                    --  values from call_proc/call_func/call_back)
--                  p = symtab[vi][S_Parm1]
                    p = symtab[vi_active][S_Parm1]
--                  maxparams = length(symtab[vi][S_sig]) - 1
                    maxparams = length(symtab[vi_active][S_sig]) - 1
                    while maxparams do
                        if DEBUG then
                            if symtab[p][S_NTyp]!=S_TVar then ?9/0 end if
                            -- sanity check: K_noclr must NOT be set on any params!
                            if and_bits(symtab[p][S_State],K_noclr) then ?9/0 end if
                        end if
                        symtab[p][S_gNew] = {T_object,MININT,MAXINT,T_object,-2}
                        p = symtab[p][S_Slink]
                        maxparams -= 1
                    end while
                end if
if NEWGSCAN then
                vi_active = g_scan[vi_active]
--              if vi_active=T_maintls then exit end if
                if vi_active=-1 then exit end if
else
--              vi = symtab[vi][S_Slink]
                vi_active = symtab[vi_active][S_Slink]
end if
            end while

            rescan = 0
            for i=1 to length(symtab) do
                si = symtab[i]
                if sequence(si) then
                    siNTyp = si[S_NTyp]
                    if siNTyp <= S_TVar
                    and (siNTyp!=S_Const or not and_bits(si[S_State],K_lit+K_rtn)) then
--24/4/21:
--                      xi = si[S_gNew]
                        xi = iff(and_bits(si[S_State],K_asmm)?{si[S_vtype],MAXINT,MININT,T_object,-2}:si[S_gNew])
                        if sequence(xi) then
                            vtype = si[S_vtype]
sv = si[S_Name]
                            si = si[S_gInfo]
                            symtab[i][S_gNew] = 0
                            if not equal(si,xi) then
----DEV 05/01: don't allow MIN/MIN or MAX/MAX...
--   (makes testing easier: problem is that Min=Max means fixed, but MAX is (eg) +=1 indicator;
--    ie MAX/MAX would be created by i=#3FFFFFFF i+=1 but Min=Max would mean all refs(/stores)
--       of i are treated as #3FFFFFFF, and hence no overflow occurs when obviously it should.
--       Minor downside is of course i=#3FFFFFFF i+=1 leaves i as MIN/MAX, a bit "odd" but ok.)
                                xType = xi[gType]
                                if xType=0 then
--DEV 15/4/2010. skip temps (see challenge0001)
if not equal(sv,-1) then
--?xi
--?symtab[i]
    if bind and mapsymtab then
--      printf(1,"symtab[%d(%d)]\n",{i,symtabmap[i]})
        showmapsymtab = i
    end if
                                    k = symtab[i][S_FPno]
--                                  string mapi = iff(bind and mapsymtab and 
--                                  printf(1,"xType=0 on symtab[%d(=%s)] (%s in %s)\n",{i,mapi,getname(sv,-2),filenames[k][2]})
--DEV (this should get much rarer under NEWGSCAN...  find those cases i've put workarounds in for this [in builtins, eg/ie "abc"="def"/DEV])
                                    printf(1,"xType=0 on symtab[%d] (%s in %s)\n",{i,getname(sv,-2),filenames[k][2]})
end if
--                                  xType = rootType(vtype)
                                    if vtype>T_object then vtype = rootType(vtype) end if
--                                  xi[gType] = xType
                                    xi[gType] = vtype
                                end if
                                if xi[gEtyp]=0 then
-- removed 7/2/14:
--                                  if and_bits(xType,T_sequence)
--                                  and xi[gLen]!=0 then    -- let p={} (and nowt else) pass by unnoticed.
--                                      printf(1,"xEtyp=0 on symtab[%d]\n",i)
--                                      ?xi
--                                  end if
                                    xi[gEtyp] = T_object
                                end if
                                xMin = xi[gMin]
                                xMax = xi[gMax]
                                if and_bits(xType,T_atom)!=T_integer
--DEV isFLOAT?
                                or not integer(xMin)
                                or not integer(xMax)
                                or xMin>xMax
--                              or (xMin=xMax and (xMin=MININT or xMin=MAXINT)) then
                                or (xMin=xMax and (xMin=MININT or xMin=MAXINT))
-- SUG: as below (untried)
                                or rescancount>7 then
                                    xi[gMin] = MININT
                                    xi[gMax] = MAXINT
                                    if and_bits(xType,T_atom)=T_integer then
                                        xi[gType] = or_bits(xType,T_atom)
                                    end if
                                end if

--No good:
--                      if sequence(si)         -- rely on best when dealing with circulars
--                      and rescancount>1 then  -- avoid early dependence on K_noclr values
--                          xi[gMin] = max(xi[gMin],si[gMin])
--                          xi[gMax] = min(xi[gMax],si[gMax])
--                      end if
                                if not equal(si,xi) then
--                              if sequence(si) then
--                                  xi[gMin] = min(xi[gMin],si[gMin])
--                                  xi[gMax] = max(xi[gMax],si[gMax])
--                              end if

-- If you get problems with this:
--  1) If it really is an infinite loop, I suspect it really is a problem.
--      (maybe try commenting out getc(0) and let it run for 10 mins?)
--  2) If it resolves in a few more iterations, obviously increase to fit.
--      See example ("The process of repeated iteration." in pilx86.e) 
--      requiring 9 scans in pilx86.e, if we find a real-world program 
--      that needs a silly number of iterations (eg 7,654), then maybe a 
--      reasonable cap really might be in order...
--  3) You /might/ get a bit further uncommenting the >16 exit below, at
--      least be able to get a list.asm with a few more clues in it.
--  Update 18/1/16: I /believe/ the worst case is 1 multiplied by 2 every 
--  gvar_scan, taking 31/63 iterations to blow an integer. Quite probably 
--  after "7 or 8 iterations without settling down", what we should do is 
--  simply give up and flag the value as unbounded/possibly atom (ie just
--  add "or rescancount>7" to the above test). I also suspect the 16/20
--  (as experimentally found) derive from decimal number processing; in
--  particular printf()'s precision and minfieldwidth were culprits, not
--  that I've dotted the i's and crossed the t's on that one.
--9/1/16: (no joy) [18/1/16: yes joy]
if rescancount>20 then
    ?i
    ?xi
    ?si
    ?symtab[i]
    k = symtab[i][S_FPno]
    printf(1,"rescancount>20 on symtab[%d] (%s in %s)\n",{i,getname(sv,-2),iff(k=0?"??k=0":filenames[k][2])})
--  if getc(0) then end if
end if
                                    symtab[i][S_gInfo] = xi
                                    rescan = 1
                                end if  -- si!=xi
                            end if  -- si!=xi
                        end if  -- sequence(xi)
                    end if  -- tvar/rt-assigned const
                end if  -- sequence(si)
            end for

            si = 0  -- kill refcount
            xi = 0  -- ""
            if not rescan then
                ltDiagMsg("*** final pass ***\n")   -- (pltype.e diagnostics)
                exit
            end if
            rescancount+=1
--          if rescancount>16 then exit end if      -- RDS/Knuth suggest 7 as a reasonable max
                                                    -- (though technically I suspect Knuth was 
                                                    --  referring to a different problem.)
            if rescancount>20 then
                if rescancount>22 then exit end if
                puts(1,"\nrescanning...\n")
            end if
            ltDiagMsg("*** rescanning ***\n")   -- (pltype.e diagnostics)
        end while

        -- test set is mainly 4; t40,t45 are 3, t21,t36 are 5, t43,t46 are 6.
        --  win32lib is 7, euex.exw 12(!!), arwen 6, edita 9, and p.exw is 6.
--      printf(1,"gvar_scan: %d iterations\n",rescancount)

    else -- not bind, or dumpil

--puts(1,"finalfixups2 line 4097\n")

if NEWGSCAN then
        g_scan = kridt_scan
end if

        if showfileprogress then
            rescancount = -1
        end if
--      if not dumpil then
--          rbldrqd = 1
--      end if
        if dumpil then
            relink()
            tt_traverse(r_ReconstructIds,"",-2)             -- identifiers
        end if

--puts(1,"finalfixups2 line 4110\n")

--      vi = T_maintls  -- follow (volatile) K_used chain from top_level_sub[main], aka symtab[T_maintls][S_Slink].
        vi_active = T_maintls   -- follow (volatile) K_used chain from top_level_sub[main], aka symtab[T_maintls][S_Slink].
--      while vi do
        while vi_active do
--4/11/19 (structs)
if symtab[vi_active][S_il]!=0 then
--?vi
--          s5thunk(symtab[vi][S_il])   -- set s5
            s5thunk(symtab[vi_active][S_il])    -- set s5
--          gvar_scan_nobind(vi)
            gvar_scan_nobind(vi_active)
end if
if NEWGSCAN then
            vi_active = g_scan[vi_active]
            if vi_active=-1 then exit end if
else
--          vi = symtab[vi][S_Slink]
            vi_active = symtab[vi_active][S_Slink]
end if
        end while
        if dumpil then
--if 01 then
            puts(dilfn,"\n\n Opcodes:\n")
            puts(dilfn," (obviously, s5 appears in trace() as say {210,1,213,...};\n")
            puts(dilfn,"  and you may need these to map it to {opLn,1,opCtrl,...}.)\n")
            for i=1 to length(opNames) do
                opName = opNames[i]
                if opName=0 then
                    opName = "<spare>"
                end if
                printf(dilfn,"%4d:%s\n",{i,opName})
            end for
--end if
            Warnings(dilfn)     -- (closes the file)
--          close(dilfn)
            puts(1,"listing file "&mainpath&"ildump.txt created.\n")
            abort(0)
        end if

    end if -- bind/dumpil

--puts(1,"finalfixups2 line 4124\n")

--  gopshow() -- (see pgscan.e)
--  abort(0)

    if showfileprogress then
--      printf(1,"performing final fixups, scanforShortJmp started at %3.2f...\n",time()-t)
        printf(1,"scanforShortJmp started at %3.2f, gvar_scan[%d] took %3.2f seconds...\n",
                 {time()-t,rescancount,time()-t0})
        t0 = time()
    end if

--trace(1)
--  vi = T_maintls  -- follow (volatile) K_used chain from top_level_sub[main], aka symtab[T_maintls][S_Slink].
    vi_active = T_maintls   -- follow (volatile) K_used chain from top_level_sub[main], aka symtab[T_maintls][S_Slink].

--DEV setup a gvar mapping thing now, patch in ilxlate()...

--  while vi do
    while vi_active do
--4/11/19: (structs)
if symtab[vi_active][S_il]!=0 then
--      sv = symtab[vi]
        sv = symtab[vi_active]
--      symtab[vi] = 0                              -- kill refcount
        symtab[vi_active] = 0                               -- kill refcount
        s5thunk(sv[S_il])   -- set s5
        sv[S_il] = CSvsize
        LineTab = {}    -- build a new one!
        ltline = 0
--      symtab[vi] = sv
        symtab[vi_active] = sv
        sv = sv[S_Name]                             -- save name for debug, also kills refcount
--      ilxlate(vi)
        ilxlate(vi_active)
--sv = getname(sv,-2)
--puts(1,sv&'\n')
--      scanforShortJmp(vi)
        scanforShortJmp(vi_active)
        if length(LineTab) then
--          symtab[vi][S_ltab] = LineTab
            symtab[vi_active][S_ltab] = LineTab
        else
--          symtab[vi][S_ltab] = 0
            symtab[vi_active][S_ltab] = 0
        end if
        s5sets = append(s5sets,s5)
        s5sizes = append(s5sizes,thisCSsize)
--      s5v = append(s5v,vi)
        s5v = append(s5v,vi_active)
        CSvsize += thisCSsize
--      if debug then
--          if equal(sv,-1) then    -- a top_level_sub (no name)
--              s5symn = append(s5symn,0)
--          else
--              -- reserve space for a cmp eax,<name> debug aide,
--              -- the non-zero value in s5symn indicates it is present, and
--              --  later in the pre-dump() compacted version of this table
--              --  triggers the addr name patch as name is dump()'d [phew]:
--              -- (This is probably only of use if you have a debug build of 
--              --  the VM, which I do not plan to publically release, btw)
--              s5symn = append(s5symn,vi)
--              CSvsize += 5
--          end if
--      end if
end if
if NEWGSCAN then
        vi_active = g_scan[vi_active]
        if vi_active=-1 then exit end if
else
--      vi = symtab[vi][S_Slink]
        vi_active = symtab[vi_active][S_Slink]
end if
    end while

--?"ff2 line 3924"
    if x86showmapsymtab!=0 then
        showmapsymtab = x86showmapsymtab
        x86showmapsymtab = 0
    elsif ltAddshowmapsymtab!=0 then
        showmapsymtab = ltAddshowmapsymtab
        ltAddshowmapsymtab = 0
    end if

    if countTransTmpFer then
        opshow()
        abort(1)
    end if

    if showfileprogress then
        printf(1,"blurph started at %3.2f, scanforShortJmp took %3.2f seconds...\n",{time()-t,time()-t0})
        t0 = time()
    end if

--puts(1,"finalfixups2 line 4196\n")
--?{"symtab[1]",symtab[1]}

--if Z_ridN!=0 then -- still OK...
--  ?symtab[Z_ridN]
--end if

    ridlink = T_maintls
    nslink = T_nslink
    for v=1 to symlimit do
        sv = symtab[v]
        if sequence(sv) then
            nTyp = sv[S_NTyp]
            u = sv[S_State]     -- DEV can't see this is used (cleanup first!)
--/*
            if nTyp>S_Type      -- types are not properly marked as used... yet.
--          if nTyp>=S_Func-MARKTYPES
--          if nTyp>=S_Type
            and sv[S_Slink]=-9 then -- (not on symtab[T_maintls][S_Slink] chain)
--DEV (NEWGSCAN)
            if nTyp>=S_Type
            and g_scan[v]==0 then
--*/
            bool doit = iff(NEWGSCAN ? nTyp>=S_Type and g_scan[v]==0
                                     : nTyp>S_Type and sv[S_Slink]=-9 )
            if doit then
-- 17/3/15:
if bind or listing then

--if not show_full_symtab then
--DEV (added 12/12/15, factor this out...)
                node = sv[S_Name]
                slink = tt[node+EQ]
                if slink=v then
                    tt[node+EQ] = sv[S_Nlink]
                else
--DEV as per/factor out...
                    if nTyp=S_Rsvd then ?9/0 end if     -- sanity check (added 07/01/2012)
                    while slink do
                        snext = symtab[slink][S_Nlink]
                        if snext=v then
                            symtab[slink][S_Nlink] = sv[S_Nlink]
                            exit
                        end if
                        slink = snext
                    end while
                end if
                UnAlias(v)
                symtab[v] = 0                   -- delete unused routine...
--end if
--23/4/15:
--                  if v>T_Bin then
                    if v>T_Asm then
--MARKTYPES
--                      if nTyp=S_Func then
--                      if nTyp=S_Func or (MARKTYPES and nTyp=S_Type) then
                        if nTyp=S_Func or (NEWGSCAN and nTyp=S_Type) then -- (use next, when clearing NEWGSCAN)
--                      if nTyp<=S_Func then
--if not show_full_symtab then
--  if v-1=Z_ridN then ?9/0 end if
                            symtab[v-1] = 0 -- ... any return var ...
--end if
                        end if
                        -- ... and any params & locals.
                        -- NB. tvars only. Avoid any temptation in eg (unused)
                        --          procedure hi()
                        --              puts(1,"hello")
                        --          end procedure
                        -- to get rid of the S_Const "hello" - there is as yet no
                        -- way to tell whether it is used sometime later or not.
                        -- BTW: For builtins and fwd routines, the tvars may be a
                        --      long way from the routine entry.
                        p = sv[S_Parm1]
                        N = sv[S_Ltot]
                        while p do
                            sv = symtab[p]
--                          if not sequence(sv) then ?9/0 end if
                            if not sequence(sv) then puts(1,"oops, pemit2.e line 4579\n") exit end if
                            nTyp = sv[S_NTyp]
                            if nTyp!=S_TVar then ?9/0 end if
--if not show_full_symtab then
--  if p=Z_ridN then ?9/0 end if
                            symtab[p] = 0
--end if
                            p = sv[S_Slink]
                            N -= 1
                        end while
--DEV
--                      if N!=0 then ?9/0 end if
                        if N!=0 then puts(1,"oops, pemit2.e line 4590 (N!=0)\n") end if

                    end if --v>T_Bin
--end if -- listing=-1 and v!=T_maintls
end if -- (17/3/15)
            else
--if 0 then -- 17/3/15 (done below)
                -- link up routines and namespaces for faster scan in routine_id
--              if nTyp>=S_Type then
--              elsif nTyp=S_Nspc then
--              elsif ( nTyp=S_Rsvd or
                if ( nTyp=S_Rsvd or
                    (nTyp<=S_GVar2 and not and_bits(u,S_used+S_set+K_Fres+K_lit)))
                and integer(sv[S_Name]) -- 6/3/2010 (was d!'ing at the time)
                and not equal(sv[S_Name],-1) then
if bind or listing then -- 17/3/15
                    node = sv[S_Name]
                    slink = tt[node+EQ]
                    if slink=v then
                        tt[node+EQ] = sv[S_Nlink]
                    else
--                      if nTyp=S_Rsvd then ?9/0 end if     -- sanity check (added 07/01/2012)
-- 28/7/17 (catch can now be overidden...)
--                      if nTyp=S_Rsvd then
--                          k = symtab[slink][S_FPno]
--                          printf(1,"9/0 LINE 4696 pEmit2.e (symtab[%d] - %s in %s)",
--                                      {slink,getname(symtab[slink][S_Name],-2),
--                                       filenames[k][2]})
----sv = si[S_Name]
----symtab[slink]
----showmapsymtab = slink
--                      end if
                        while slink do
                            snext = symtab[slink][S_Nlink]
                            if snext=v then
--28/7/17...
                                if symtab[slink][S_NTyp]=S_Rsvd then ?9/0 end if    -- sanity check (added 07/01/2012)
                                symtab[slink][S_Nlink] = sv[S_Nlink]
                                exit
                            end if
                            slink = snext
                        end while
                    end if
                    nTyp = 999  -- (avoid stuff below! (not strictly necessary))
--if not show_full_symtab then
--  if v=Z_ridN then ?9/0 end if
                    symtab[v] = 0
--printf(1,"symtab[%d] unlinked\n",v)
--end if
end if
                end if
            end if
        end if
    end for

--puts(1,"finalfixups2 line 4404\n")
--if Z_ridN!=0 then -- still OK?...
--  ?symtab[Z_ridN]
--end if

    if bind then
        if mapsymtab then
--DEV remind me, what is this loop for, exactly? (below is my best guess)
            -- delete unused top-level gvars
            for i=1 to symlimit do
                si = symtab[i]
                if sequence(si) then
                    siNTyp = si[S_NTyp]
                    if siNTyp<=S_GVar2  -- added 8/1/09
                    and not and_bits(si[S_State],S_used+S_set+K_Fres)
                    and not equal(si[S_Name],-1) then
                        symtab[i][S_State] = 0
                    end if
                end if
            end for

            symtabmax = 0
            vmax = 0
            for i=1 to length(symtabmap) do
                if sequence(symtab[i])
                and symtab[i][S_State]!=0 then
                    symtabmax += 1
                    symtabmap[i] = symtabmax
                    if symtab[i][S_NTyp]<=S_GVar2 then  -- S_Const and S_Gvar2
                        vmax += 1
                        symtab[i][S_Slink] = vmax
--                      symtab[i][S_Tidx] = vmax --NO!
                    end if
-- 11/9/14:
----dev 26/4...
--              elsif i=T_pathset
--                 or i=T_fileset
--                 or i=T_nslink
--                 or i=T_cmdlnflg
--                 or i=T_callstk
--                 or i=22 then -- T_EBP/spare
                elsif i<=T_const1 then
--                  symtabmap[i] = 16000000 -- (any daft but debuggable value)
                    symtabmap[i] = i
                    symtabmax += 1
                else
                    symtabmap[i] = 0
--1/8/14: (caused a crash/hang...)
--symtab[i] = 0
                end if
            end for
            if showmapsymtab then
                printf(1,"symtab[%d(%d)]\n",{showmapsymtab,symtabmap[showmapsymtab]})
            end if
        else -- not mapsymtab
--puts(1,"pemit2.e line 4405 suspect not-mapsymtab handling...\n") [actually should be fine...]
            vmax = 0
            for i=1 to length(symtab) do
                if sequence(symtab[i])
                and symtab[i][S_State]!=0
                and symtab[i][S_NTyp]<=S_GVar2 then -- S_Const and S_Gvar2
                    vmax += 1
                    symtab[i][S_Slink] = vmax
--                  symtab[i][S_Tidx] = vmax -- NO!
                end if
            end for
            symtabmax = length(symtab)
        end if
        maxgvar = vmax --DEV (use maxgvar above?)

--if Z_ridN!=0 then
--  ?symtab[Z_ridN]
--end if

        relink()
        Ids = {}
        IdLinks = {}
        tt_traverse(r_CollectIds,"",-2)

    else -- interpret
        vmax = 0
        for i=1 to length(symtab) do
            if sequence(symtab[i])
            and symtab[i][S_State]!=0
            and symtab[i][S_NTyp]<=S_GVar2 then -- S_Const and S_Gvar2
                vmax += 1
                symtab[i][S_Slink] = vmax
--              symtab[i][S_Tidx] = vmax -- NO!
            end if
        end for
        symtabmax = length(symtab)
        CSvaddr = AllocateBlock(CSvsize)
--DEV:
-- (DSvsize not assigned/and now not declared)
--puts(1,"warning: symtabptr[ds+8] not accounted for (pemit2.e line 4162)\n") (:%pGetSymPtr)
        if X64 then
            DSvsize = vmax*8 + 32
        else -- 32
            DSvsize = vmax*4 + 20
        end if
if not repl or DSvaddr=0 then
    if repl then
        if DSvsize>replDSvsize then ?9/0 end if
        if DSvaddr=0 then
--?"allocate DSvaddr pEmit2.e line 4231" -- check once only... [good]
            DSvaddr = AllocateBlock(replDSvsize)
        end if
    else
        DSvaddr = AllocateBlock(DSvsize)
    end if
--if platform()=LINUX then
if 0 then
printf(1,"(pemit2.e line 4805) DSvaddr=#%08x, CSvaddr=#%08x\n",{DSvaddr,CSvaddr})
--printf(1,"(pemit2.e line 4806) DSvsize=%s, DSvaddr=#%08x, CSvsize=%s, CSvaddr=#%08x\n",{Size(DSvsize),DSvaddr,Size(CSvsize),CSvaddr})
end if
--end if
        poke(DSvaddr,"Phix")            -- sigPhx
        DSvaddr4 = floor(DSvaddr/4)
        if X64 then
            poke4(DSvaddr+4,0)              -- layout/rand seed (unused) [DEV level?]
--          poke8(DSvaddr+8,{0,             -- symptr
--                           0,             -- relocs
--                           vmax,          -- maxgvar
--                   #4000000000000000})    -- unassigned
            poke4(DSvaddr+8,{0,0,           -- symptr
                             0,0,           -- relocs
                             0,vmax,        -- maxgvar
                             0,#40000000})  -- unassigned
            #ilASM{
                [32]
--                  pop al (not convinced we really need this) [triggered 17/1/16, p -d! -x64 e03]
--                  int3
                    mov eax,[symtab]
                    mov edi,[DSvaddr4]
                    shl eax,2
                    mov [ebx+edi*4+12],eax
                [64]
                    mov rax,[symtab]
                    mov rdi,[DSvaddr4]
                    shl rax,2
                    mov [rbx+rdi*4+8],rax
                []
                  }
--DEV/SUG: (earlier on somewhere...)
--          #ilASM{
--              [32]
--                  mov [X64],ebx
--              [64]
--                  mov qword[X64],1
--              []
--                }
            mem_copy(DSvaddr+40,DSvaddr+32,DSvsize-40)
        else -- 32
            poke4(DSvaddr+4,{0,             -- layout/rand seed (unused) [DEV level?]
                             0,             -- symptr
                             0,             -- relocs
                             vmax,          -- maxgvar
                             #40000000})    -- unassigned
            #ilASM{
                [32]
                    mov eax,[symtab]
                    mov edi,[DSvaddr4]
                    shl eax,2
                    mov [ebx+edi*4+8],eax
                [64]
--                  pop al
                    int3
--                  mov eax,[symtab]
--                  mov edi,[DSvaddr4]
--                  shl eax,2
--                  mov [ebx+edi*4+12],eax
                []
                  }
            mem_copy(DSvaddr+24,DSvaddr+20,DSvsize-24)
        end if
end if

        -- for isIL, we must do them all in one go, before calling blurph()
        for v=1 to length(s5sets) do
            symidx = s5v[v]
            -- see emitinit(). When binding CSvaddr is picked up 
            -- from the .exe, when interpreting we must calculate CSvsize 
            -- first and then allocate() it to obtain CSvaddr.
            svil = symtab[symidx][S_il]
            symtab[symidx][S_il] = svil+CSvaddr
        end for

--EXCEPT
        if listing 
        or some_unresolved_rtnids
        or exceptions_in_use then

--puts(1,"calling relink and reconstructids... (pemit2.e line 4336)\n")
            relink()
--?{"symtab[1][S_Name] (pEmit2.e line 4089)",symtab[1][S_Name]}
            tt_traverse(r_ReconstructIds,"",-2)             -- identifiers
--puts(1,"returned from reconstructids... (pemit2.e line 4339)\n")
--?{"symtab[1][S_Name] (pEmit2.e line 4095)",symtab[1][S_Name]}

        end if
--9/10/10: moved here (to be before blurph, for isConstRef[Count])
        -- reconstruct any nested sequences
        while 1 do
            rescan = 0
--puts(1,"ReconstructSequence scan started\n")
            tt_traverseQ(r_ReconstructSequence)             -- sequence constants
--?rescan
            if not rescan then exit end if
        end while

    end if -- bind/interpret


--puts(1,"finalfixups2 line 4587\n")

    code_section = repeat('\0',floor((CSvsize+3)/4)*4)
    for i=CSvsize+1 to length(code_section) do
        -- (1-byte [n]ops ensure disassembly/listing does not overrun section)
        code_section[i] = #90 -- nop
    end for

    CSvsize = 0
    for v=1 to length(s5sets) do

        s5thunk(s5sets[v])  -- sets s5

        thisCSsize = s5sizes[v]
        symidx = s5v[v]
        xi = symtab[symidx]
        svil = xi[S_ltab]
        line1 = xi[S_1stl]
        xi = xi[S_Name]
        if sequence(svil) then
            LineTab = svil
            svil = 0
        else
--DEV try {} here and 0 below
            LineTab = {-2}
        end if
        symtab[symidx][S_ltab] = 0
        blurph(line1)
        symtab[symidx][S_ltab] = LineTab
    end for
    if showfileprogress then
        printf(1,"symtab dump started at %3.2f, blurph took %3.2f seconds...\n",{time()-t,time()-t0})
        t0 = time()
    end if

--puts(1,"finalfixups2 line 4631\n")

    --
    -- Code done, now the threadstack and global data, and the symtab.
    --

    if bind then
        dslen = maxgvar
        dsize = DWORD
        if X64 then
            dsize = QWORD
            dslen = (dslen)*8+32
        else
            dslen = (dslen)*4+20
        end if
        maxlen = (symtabmax+5)*dsize
        data_section = repeat(' ',dslen+maxlen)
        dsidx = 1
        dsize = DWORD
        setds(#78696850)    -- "Phix"
        setds(1)            -- layout
        relocations[DATA][DATA] = append(relocations[DATA][DATA],dsidx-1)   -- (qword)
        if X64 then
            dsize = QWORD
        end if
        setds(dslen+5*dsize)        -- symptr
        if DLL then
            relocations[DATA][DATA] = append(relocations[DATA][DATA],dsidx-1)   -- (qword)
            relptr = dsidx
        end if
        setds(0)                    -- DEV relocs
        setds(maxgvar)
        for i=1 to maxgvar do       -- gvars[i] [DEV]
            if X64 then
                setds(#4000000000000000)
            else
                setds(#40000000)
            end if
        end for

        setds(0)                    -- slack
        setds(maxlen)               -- maxlen
        setds(symtabmax)            -- length
        setds(1)                    -- refcount
        setds(1,1)                  -- type (tee hee)
        for i=1 to symtabmax do
            -- h4 might be better... but then again never left in the file...
            setds(0)
        end for
        d_addr = dsidx-1
    end if

    -- trash any unused entries in the symtab:
--puts(1,"finalfixups2 line 4693\n")

    ridlink = T_maintls
    nslink = T_nslink
    for v=1 to symlimit do
        sv = symtab[v]
        if sequence(sv) 
        and sv[S_State]!=0 then
            nTyp = sv[S_NTyp]
            u = sv[S_State]
----/*
----MARKTYPES
--          if nTyp>S_Type      -- types are not properly marked as used... yet.
----            if nTyp>=S_Func-MARKTYPES
----            if nTyp>=S_Type
--          and sv[S_Slink]=-9 then -- (not on symtab[T_maintls][S_Slink] chain)
----DEV (NEWGSCAN)
--          if nTyp>=S_Type
--          and g_scan[v]==0 then
----*/
--DEV as aboive,...
--          bool doit = iff(NEWGSCAN ? nTyp>=S_Type and g_scan[v]==0
--                                   : nTyp>S_Type and sv[S_Slink]=-9 )
            bool doit
            if NEWGSCAN then
--              doit = nTyp>=S_Type and g_scan[v]==0
                doit = v>T_object and nTyp>=S_Type and g_scan[v]==0
            else
                doit = nTyp>S_Type and sv[S_Slink]=-9
            end if
            if doit then
--if not show_full_symtab then
if bind or listing then -- 17/3/15
                ?9/0 -- should have been removed earlier!
end if
--end if
            else
                -- link up routines and namespaces for faster scan in routine_id
                if nTyp>=S_Type then
                    if v>T_Bin and atom(sv[S_il]) then
-->erm (30/12/14...)
--                          if bind and mapsymtab and and_bits(u,K_rtn) then
--                              xi = symtabmap[xi]
--                          end if
                        symtab[ridlink][S_Slink] = v
                        ridlink = v
                    end if
                elsif nTyp=S_Nspc then
                    if nslink=T_nslink then
                        symtab[T_nslink] = v
                    else
                        symtab[nslink][S_Slink] = v
                    end if
                    nslink = v
                elsif ( nTyp=S_Rsvd or
                       (nTyp<=S_GVar2 and
                        not and_bits(u,S_used+S_set+K_Fres+K_lit)))
                  and integer(sv[S_Name]) -- 6/3/2010 (was d!'ing at the time)
                  and not equal(sv[S_Name],-1) then
--if not show_full_symtab then
if bind or listing then -- 17/3/15
--                  ?9/0    -- should have gone earlier!
printf(1,"warning: symtab[%d] not 0\n",v)
end if
--end if
                end if
                if not bind then
                    if nTyp<=S_GVar2 then
                        xi = sv[S_value]
                        if not integer(xi)
                        or and_bits(u,K_noclr+K_rtn) then
                            if X64 then
                                d_addr = floor(DSvaddr/8)+sv[S_Slink]+3
                                #ilASM{
                                    [32]
                                        mov edi,[d_addr]
                                        mov eax,[xi]
                                        mov [ebx+edi*8],eax
--DEV?  
                                        mov [xi],ebx
                                    [64]
                                        mov rdi,[d_addr]
                                        mov rax,[xi]
                                        mov [rbx+rdi*8],rax
                                        mov [xi],rbx
                                    []
                                      }
                            else --32
                                d_addr = floor(DSvaddr/4)+sv[S_Slink]+4
                                #ilASM{
                                    [32]
                                        mov edi,[d_addr]
                                        mov eax,[xi]
                                        mov [ebx+edi*4],eax
--DEV?
                                        mov [xi],ebx
                                    [64]
                                        mov rdi,[d_addr]
                                        mov rax,[xi]
--                                      mov [rbx+rdi*4],rax
                                        mov [rbx+rdi*4],eax
                                        mov [xi],rbx
                                    []
                                      }
                            end if
                        end if
                    end if
                else    -- bind
                    if nTyp<=S_GVar2 then
--See DEV comment in DumpAtom():
                        xi = sv[S_value]
                        if integer(xi)
                        and and_bits(u,K_noclr+K_rtn) then
                            -- (we know that bind is true here, but the test 
                            --  costs little but adds consistency and some
                            --  robustness against refactoring or cut&paste)
                            if bind and mapsymtab and and_bits(u,K_rtn) then
                                xi = symtabmap[xi]
                            end if
                            offset = (sv[S_Slink]+2)*dsize+8
                            if X64 then
                                setdsQword(offset,xi)
                            else
                                setdsDword(offset,xi)
                            end if
                        end if
                    end if
                end if
            end if
        end if
    end for


    if nslink=T_nslink then
        symtab[T_nslink] = 0
    else
        symtab[nslink][S_Slink] = 0
    end if
    symtab[ridlink][S_Slink] = 0

--puts(1,"finalfixups2 line 4858\n")

    if bind then
--23/3/16:
        if gexch!=0 then    -- global exception handler (PE64 only)
            glboffset[gexch] += symtab[glblabel[gexch]][S_il]
--      section = SetField(section,#10,DWORD,BaseOfCode+glboffset[gexch]+symtab[glblabel[gexch]][S_il])
-->             offset = glboffset[vno]
--              offset += symtab[glblabel[vno]][S_il]
        end if
        if DLL then
            sequence export_names = {}
--          sequence export_offsets = {}
            for i=1 to length(exports) do
                si = symtab[exports[i]]
                if integer(si[S_Name]) then
                    k = si[S_Name]
                    if k>=1 and k<=length(Ids) then
                        si[S_Name] = Ids[k]
                    end if
                end if
                export_names = append(export_names,si[S_Name])
--              export_offsets = append(export_offsets,si[S_il])
--              export_offsets = append(export_offsets,exportaddrs[i])
            end for
--          exports = {outfile,export_names,export_offsets}
            exports = {outfile,export_names,exportaddrs}
            OptConsole = 0
        end if

        flatsym2 = symtab

        if not listing then
            symtab = {}
-- 1/8/14:
        else
-- 11/10/14:
            symtab[T_constm1][S_Name] = -1
            symtab[T_const0][S_Name] = -1
            symtab[T_const1][S_Name] = -1
            for i=1 to length(symtab) do
                si = symtab[i]
--DEV 23/3/15 erm...
--              if sequence(si) then
                if sequence(si)
                and integer(si[S_Name]) then
                    k = si[S_Name]
                    if k>=1 and k<=length(Ids) then
                        symtab[i][S_Name] = Ids[k]
                    end if
                end if
            end for
--DEV also when interpreted?...
if NEWGSCAN then
            slink = g_scan[T_maintls]
else
            slink = symtab[T_maintls][S_Slink]
end if
--          while slink!=0 do
            while slink>0 do
                si = symtab[slink]
                if si[S_Name]=-1 
                and si[S_NTyp]=S_Proc
                and si[S_Nlink]=0
                and si[S_sig]={'P'} 
                and si[S_Parm1]=0
                and si[S_ParmN]=0
                and si[S_Ltot]=0 then
                    si = {}
--DEV: <tls for xxx>
                    symtab[slink][S_Name] = "<tls>"
                end if
if NEWGSCAN then
                slink = g_scan[slink]
else
                slink = symtab[slink][S_Slink]
end if
            end while
        end if

----puts(1,"finalfixups2 line 5079\n")
        tt_traverse(r_DumpString,"",-1)             -- literal strings
        tt_traverseA(r_DumpAtom)                    -- float constants
        DumpSequences()

        if listing then
            -- reconstruct any nested sequences
            while 1 do
                rescan = 0
                tt_traverseQ(r_ReconstructSequence)             -- sequence constants
                if not rescan then exit end if
            end while
        end if
--puts(1,"finalfixups2 line 4936\n")

        DumpSignaturesAndLineTables()               -- [S_sig] on routine entries

--puts(1,"finalfixups2 line 4940\n")
        DumpSymTab()                                -- the symtab itself
--puts(1,"finalfixups2 line 5112\n")

        if DLL then
            relocations[DATA][REFS] = append(relocations[DATA][REFS],length(data_section))
            appenddsDword(length(data_section)+4)
            dsidx = relptr
            setds(length(data_section))
            dumpDLLrelocs()
        end if

        if not isString(data_section) then ?9/0 end if

        if showfileprogress then
            printf(1,"exe write started at %3.2f, symtab dump took %3.2f seconds...\n",{time()-t,time()-t0})
        end if

        outfile = path&outfile
        firsttime = 1   -- (sleep(1) once only)
        while 1 do
            outfn = open(outfile,"wb")
            if outfn!=-1 then exit end if
            if firsttime then
                sleep(1)
                firsttime = 0
            else
                puts(1,"Error opening "&outfile&" - retry?")
                if find(wait_key(),"nN") then abort(1) end if
                puts(1,"\n")
            end if
        end while

--puts(1,"finalfixups2 line 4973\n")

--puts(1,"calling CreateExecutable\n")
        CreateExecutable(outfn, imports, exports, relocations, data_section, code_section)
--puts(1,"returned from CreateExecutable\n")
--DEV (for listing)
--DEV (breaks self-host)
--      data_section = fixedupdata
        code_section = fixedupcode

--puts(1,"finalfixups2 line 4981\n")

    else -- not bind
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
        symtab[T_optable] = optable
        symtab[T_ds4] = floor(DSvaddr/4)    -- (not bind)
        if listing then
            symtab[T_pathset] = -999
            symtab[T_fileset] = -999
        else
            symtab[T_pathset] = filepaths
            symtab[T_fileset] = filenames
        end if
--DEV use allocate_code above instead
        if platform()=WINDOWS and machine_bits()=64 then
            if k32=0 then
                k32 = open_dll("kernel32.dll")
                xVirtualProtect = define_c_func(k32,"VirtualProtect",{C_PTR,C_INT,C_INT,C_PTR},C_INT)
                pDword = allocate(8)
            end if
            poke8(pDword,CSvaddr)
            if c_func(xVirtualProtect,{CSvaddr,CSvsize,PAGE_EXECUTE_READWRITE,pDword})=0 then ?9/0 end if
        end if
        poke(CSvaddr,code_section)
        ImageBase2 = 0
        BaseOfCode2 = CSvaddr
        SizeOfCode2 = CSvsize
        BaseOfData2 = DSvaddr
        SizeOfData2 = DSvsize
        dsize = 4
--printf(1,"BaseOfCode2: %08x\n",{BaseOfCode2})
--printf(1,"BaseOfData2: %08x\n",{BaseOfData2})
--printf(1,"ImageBase2: %08x\n",{ImageBase2})
    end if

--puts(1,"finalfixups2 line 5028\n")

    if listing then
        for glidx=1 to length(glblname) do
svname=glblname[glidx]
            offset = glboffset[glidx]
            if offset=0 and and_bits(glblused[glidx],G_bang) then -- (added 28/10/14)
                glblabel[glidx] = 0
            else
                k = glblabel[glidx]
                if k=0 then --DEV newEmit/vm/G_optable
                    if bind then ?9/0 end if
                    thunk = offset
                else
                    if bind and mapsymtab then
                        symidx = symtabmap[k]
                    else
                        symidx = k
                    end if
--                  sv = symtab[symidx]
                    sv = symtab[k]
if atom(sv) then
    puts(1,"pemit2.e line 5589: error resolving "&svname)
    if getc(0) then end if
    ?9/0
end if --DEV...
                    if bind then
                        thunk = offset+sv[S_il]+ImageBase2+BaseOfCode2
                    else
                        thunk = offset+sv[S_il]
                    end if
                end if
                knownAddr = append(knownAddr,thunk)
                knownNames = append(knownNames,":"&glblname[glidx])
                glblabel[glidx] = symidx
            end if
        end for

        for v=1 to symlimit do
            sv = symtab[v]
            if sequence(sv) then
                nTyp = sv[S_NTyp]
                if nTyp<=S_GVar2 then
                    svname = sv[S_Name]
                    if sequence(svname) and length(svname)>0 then
                        offset = (sv[S_Slink]+2)*dsize+8
                        offset += ImageBase2+BaseOfData2
                        knownAddr = append(knownAddr,offset)
                        knownNames = append(knownNames,svname)
                    end if
                end if
            end if
        end for
        if bind and mapsymtab then
            for i=T_maintls to length(symtabmap) do
                symidx = symtabmap[i]
                if symidx!=0 then
                    si = symtab[i]
                    if sequence(si) then
                        symtab[i] = 0
                        siNTyp = si[S_NTyp]
                        if siNTyp<=S_TVar then
                            k = si[S_vtype]     if k>T_object then si[S_vtype] = symtabmap[k] end if
                        end if
                        if siNTyp>S_GVar2 then
                            k = si[S_Slink]     if k!=0 then si[S_Slink] = symtabmap[k] end if
                            k = si[S_Nlink]     if k!=0 then si[S_Nlink] = symtabmap[k] end if
                            if siNTyp>=S_Type then
                                k = si[S_Parm1] if k!=0 then si[S_Parm1] = symtabmap[k] end if
--20/12/15:
                              for l=2 to length(si[S_sig]) do
                                k = si[S_sig][l]    if k!=0 then si[S_sig][l] = symtabmap[k] end if
                              end for
                            end if
                        end if
                        symtab[symidx] = si
                    end if
                end if
            end for
            symtab = symtab[1..symtabmax]
            symlimit = symtabmax
        end if

    end if -- listing
--puts(1,"finalfixups2 ended\n")
end procedure

global procedure pemit2free()
    free(m4)
end procedure
