--
-- plist.e
-- =======
--
-- Create a text listing file for the recently compiled program.
--
--    (key constants here are: dumpVM, dumpSymTab, doOneInclude, and
--                              showlinetable)
--
-- This file is part of p.exw. It is invoked after compilation, when 
--  -listing (or -d, -dump, -list) is specified on the command line.
--  (-c, -norun are implied. If you have a problem which only occurs
--   when interpreted but not when compiled you probably need to let 
--   me deal with it.)
--  Update: "-d!" option added for "interpretative listing" of the
--   non-optimised code generated when interpreting.
--
-- Note: The listing file is designed to be human-readable; there is 
--  no intention whatsoever of creating a file that can be assembled.
--  The .exe file is **not** created from list.asm, but vice-versa.
--  It has the .asm extension mainly because I already had a syntax 
--  file (FASM.syn) for Edita. Note: asm does not have escape chars,
--  so eg "error name!=\"fred\"" is not coloured the same way in a
--  list.asm file as it would be in list.exw. This is not a bug, 
--  except perhaps of the "don't use .asm for two things" kind.
--
--  The listing is arranged to match your source files, if you look 
--  carefully at the memory locations you will see they are far from 
--  contiguous. There are no data declarations anywhere, instead the 
--  hll identifiers are used, and no existing assembler could manage
--  to disambiguate the numerous "i" of "for i=" fame which tend to 
--  be scattered throughout hll code. That said, I have no problem 
--  with you trying to do something clever with the listing file, I 
--  just want to avoid (being accused of) misleading anyone.
--
-- The resulting text file may be useful for debugging purposes, but
--  that is not the primary goal. I use it more often to help modify 
--  the compiler to generate better code.
--
-- The listing files are not an excuse for you to shout at me over 
--  some poor quality code being emitted! If you think you can do
--  better, well, that is what the listing/source/etc is there for.
--
-- I would also urge you to use common sense, although perhaps I am
--  quite guilty of not doing that. In all probability, saving some
--  half-a-dozen bytes off some rare code fragment will never make
--  any real-world program any faster; in the worst case everyone
--  has a go and we end up with a zillion pointless edge cases for
--  no measurable gain, but a much slower compiler... Some of the
--  test programs have already been over-optimised to the point
--  where they don't really test anything anymore.
--
-- Some files, eg pdiag.e, may be shown as - skipped (without debug).
--  Obviously, if you want a listing remove the "without debug" in the
--  indicated source. I should note that you cannot "just print it out
--  anyway, with debug or not", because there is no LineTable, hence
--  there is simply no way to marry up source code with binary, nor
--  for that matter to determine how long the binary actually is.
--  Alternatively: edit DoWithOptions() and skip any OptDebug setting.
--  AlternativeII: always keep a lineTable, irrespective of K_wdb.
--
-- Just so you know, the last time I ran p.exe -list p.exw it created
--  a 10.5MB listing file 116,939 lines long, would probably need over
--  1750 sheets/3 reams of A4 to print, and take longer to read than
--  War and Peace. The constant doOneInclude is an easy/recommended 
--  way to keep listing sizes more manageable, -nodiag even better.
--
--  The "analysis" contains seven columns:
--
--      pairing: np, u, v, uv, plus vu, which is a uv that landed in v.
--      modified registers   : \    eax=#01, ecx=#02, edx=#04, ebx=#08,
--      referenced registers : /    esp=#10, ebp=#20, esi=#40, edi=#80
--      clocks,
--      cycle,  (each block [/source line] starts at 1)
--      dependency,  (instruction stalls waiting for prior to finish)
--      *AGI stall*  (address generation interlock)
--
--  cycle is only meaningful for straightline code. For example, in
--          cmp [a1],h4                     ;    u  00 00  2   1      
--          jl @f                           ;    v  00 00  1   1      
--          call opIchkFail                 ;    v  00 00  1   3      
--     @@:  <next instruction>              ;    u  00 00  1   4      
--  it should be pretty clear the disassembler has not "carried over" 
--  the cycles from the jl to after the call opIchkFail even though 
--  in this case it would be the more interesting number, and since
--  opIchkFail does not return, the cycle against @@: is useless.
--    (opIchkFail has since been replaced with opTcFail, btw.)
--  The above also shows an earlier bug in the implementation of 
--  cycle - the jl should end in "1  2" as it pairs with the last 
--  clock of the cmp, not the first. While this specific instance
--  has now been fixed (search for 10/05/08 in p2asm.e), I believe
--  non-1-clock instructions need more work (see "" ?9/0), though
--  Phix rarely emits non-1-clock instructions anyway.
--  Also note there is no memory dependency checking, eg it claims
--  mov edx,[a1]; mov [a1],ecx occur in the same cycle.
--  (update: psched.e now attempts to remove memory dependencies,
--   a long time before this program gets its mitts on the binary.)
--  (update2: psched.e is now long-time broken)
--
--  Hopefully the dependency and AGI columns are most often blank.
--  Do not overly trust this, though. Since as above I doubt that
--  cycle is properly implemented, AGI stalls may not always be
--  correctly predicted, unless the instruction clocks are all 1.
--  Similar to above, e92movti does not return, hence in eg:
--          cmp ecx,h4                      ;    uv 00 02  1   5
--          jl @f                           ;    v  00 00  1   5      
--          mov esi,#0040C16C (e)           ;    uv 40 00  1   6      
--          mov edx,#0040C14C (e)           ;    vu 04 00  1   6      
--          call e92movti                   ;    v  00 00  1   7      
--    @@:   mov [ebp+edx*4+20],#0040B1CA    ;    uv 00 24  1  10    *04*
--  the AGI stall on edx as shown does not in fact occur, though
--  equally there could be one occurring which is not reported.
--   update: call now clears such dependency/agi stall reporting,
--    however any (rare) <modify reg>/jump/<use reg in addr> sets
--    make no attempt to indicate (possible) dependency/agi.
--
--  Let me know if you find anything else which is clearly wrong.
--
--
--DEV this should be a command line option??
constant dumpVM = 0
--  Well, I guessed someone would try it anyway, so I may as well
--  offer a helping hand. Set the above constant to 1 to create a
--  listing of the (closed source) VM - run eg "p p -d test" (you 
--  could rebuild the compiler [p -cp] but why bother) & remember
--  to reset the above constant to zero when done.
--
--  There may be other (free) tools you can download that do a 
--  better job, so this isn't really giving all that much away.
--  Reconstructing the data declarations is left as an exercise 
--  for the reader ;-))
--
--  One thing you might do with such a file is grab the code for
--  a particular opcode, hack at pmain.e/pemit.e to generate a 
--  replacement, and then either patch VMep[opXxxxxxx] or do an 
--  isILa-style thing. No need to be afraid to ask for help, so
--  long as you are not spitefully trying to rip out licencing..
--
--DEV delete this protectionist codswallop!
--  (replace with something along the lines of:
--      I have no intention of releasing the asm sources of the
--      backend, instead preferring to migrate desired chunks to
--      hll/#ilasm code, as and when needed. I will gladly drop
--      any licencing code (specifically the "enforced" part of
--      the "enforced open source" concept) in return for help
--      with this migration, that is at the point when it becomes
--      100% Phix open source code with no need for fasm etc.)
--  Fwiw, it is probably the trivia I am most protective of.
--  As far as I am concerned, any fool can write a block of code 
--  to perform a particular task, it is the subtleties between that
--  count. As an example, the pertinent address after a file full 
--  error is at [esp+36]. Hundreds of similar cases represent, at
--  least in my eyes, real value, months if not years of effort 
--  (such work is considered incomplete and ongoing, btw). If you 
--  can actually name something you want to experiment with then 
--  it is probably something I would be comfortable releasing. 
--  (abeit perhaps with somewhat watered-down error handling)
--  While admittedly I have not yet finalised or tested the means
--  of making this a practical reality, it goes something like:
--
--      * Set dumpVM to 1, rebuild via p -cp, then compile any 
--          program under -d, and open the list2.asm file just 
--          created. You will need to reset dumpVM to 0 and
--          reuild again for the final phase.
--      * Locate the code you want to experiment with. I may be
--          able to point you in the right direction, or someone
--          else may have worked in this area and left you an 
--          already half-finished present. See note below.
--      * Hack the code into #ilasm format. For an example, see
--          the one in pttree.e: that was itself hacked from a 
--          listing of pttree.e, of course! During this process,
--          be prepared to add the odd new opcode to init_il().
--      * Create/save a ttidx (or two) for the opcode(s) under
--          attack, say in init_il(), for matching against a
--          label in your #ilasm code - remember the routine
--          number and offset in DoRoutineDef().
--      * Modify pemit.e to hook your new code where the old
--          opcode was. Use p -d test and inspect the resulting 
--          list.asm until everything is perfect. This is the
--          trickiest part and always involves some trial and
--          error. It makes a great deal of sense to first get
--          everything working as-is, before attempting any
--          modifications to the given assembly code.
--      * Note that some error handling, unassigned variables
--          in particular, relies on exception handling which
--          is not available to #ilasm constructs. If needed,
--          add explicit tests and calls to opRTErn (examples
--          in pprntf.e). This is of course expected to change
--          when (/if) proper exception handling is supported
--          in the hll language, in which case exceptions that
--          occur within a #ilasm construct would probably be
--          caught via a surrounding hll try/catch statement.
--  
--  Once you are happy with your modifications, submit them to 
--  me for inclusion in the official root. When I have time, I
--  will (provided I agree with them) ship them in pilasm.e, 
--  as per the comments at the top of that non-compilable and 
--  unsupported file, for the basis of future experimentation.
--
--
--  The listing may also be of some minor help if you get a fatal 
--  exception in the range #403000 to #40A000 (the VM is supposed 
--  to trap any that can occur in that range).
--
--
-- Possible future plans:
-- ======================
--
--  Improved notion of integer variables and intermediates.
--   Generally speaking the most gains can be found by detecting more 
--   and more things (variables) that are short integer. At the moment 
--   even i=j+k+l causes problems because it does not know that (j+k) 
--   is an integer, ie/eg in #3F+#3F+(-#3F) all the components and the
--   result are integer, but there is an intermediate which is not.
--   By "problems" I just mean i=j+k+m is slower than i=j+k i+=m;
--    a test program gives 4.03:1.37[!], on RDS I get 4.65:3.12.
--
--  Smarter type handling. At the moment the compiler has a very static
--   view of whether something is an integer or not. It ought to be able
--   to do something like this: [DEV: done, a while back: see pltype.e]
--
--          object x
--          if atom(x) then             -- mark x as atom
--              if integer(x) then      -- mark x as integer
--              end if                  -- reset x as atom
--          else                        -- mark x as sequence (not atom)
--              if string(x) then       -- mark x as string
--              else                    -- mark x as dword_seq (sequence but not string)
--              end if                  -- reset x as sequence
--          end if                      -- reset x as object
--
--   Seems to require multiple (6) overlapping chains, possibly more.
--   I doubt there is much gain in the (even) more complex model:
--
--      object x
--      if cond1 and atom(x) and cond2 then
--          -- treat x as atom here
--      else
--          -- back to treating x as object here
--          -- ie: saying x is "not atom" here would be wrong.
--      end if
--
--   While "integer" is the most important, there is also the
--   potential to avoid typechecking:
--
--      object x
--      atom y
--      if atom(x) then
--          y=x
--      end if
--
--  Obviously this applies to udts as much as builtin types, as long as
--  the udt has no side effects (not currently tested/able for).
--
--
--  Improved notion of 4-byte-per-element sequences. At the moment strings
--   get special treatment because they cannot be dword-sequences. If 
--   the compiler could detect sequences which are "not string" better
--   then it could apply similar treatment to them.
--   Update: while significant gains have been seen in the micro-benchmarks,
--   they have not carried through to larger programs. The T_sequence/string
--   gain has mostly been dropped.
--
--  Sequence of integer handling. If the compiler could detect that some
--   sequences only ever had integer elements, it could optimise better.
--   I am actually talking about making the compiler smarter rather than
--   implementing an "of" keyword in the language and throwing typechecks
--   at runtime here. The latter is a big job and a separate issue - I'd
--   personally rather focus on the handling of legacy code, eg win32lib,
--   at least in the first instance.
--
--  Trivial function inlining. If the code actually being invoked is 
--   smaller than the opFrame/Move/Call currently emitted, then it
--   should be wiser just to duplicate the binary. Of course this
--   is only possible for previously declared routines and probably 
--   too difficult except for at most single parameter routines.
--   We could easily avoid calling procedures with null bodies.
--   We should not need to worry about non-leaf or recursive routines 
--   since they would immediately fail the size test, which means we
--   also need not worry about using the existing parameters and/or 
--   return var - there is no way they could be active, though we
--   might want to direct store rather than go via the return var.
--   It may also be necessary to restructure the compiler so that
--   some of the work carried out by scanforShortJmp and blurph
--   in pemit.e is done earlier, for the length() tests.
--
--  Peephole optimisation. The routines emitHex1..emitHex6 in pemit.e
--   are currently utterly ignorant of what passes through them.
--   They could buffer the output, accept additional parameters to
--   detail what is being affected (ie/eg what registers are being
--   modified and what registers are being referenced, what the
--   registers actually contain, and/or pairing, clocks, etc) and
--   use this to generate improved code. I would say that generally
--   you are perfectly free to blur the boundary of code emitted by
--   one line and the next, as long as calls to opCodes are kept in
--   order and the LineTab covers the opCodes, eg in
--          call opCode1        -- from line 17
--          mov [bluh],eax      -- from line 17, code point A
--          mov edi,[bleh]      -- from line 18, code point B
--          call opCode2        -- from line 18, code point C
--   Then the LineTab entry for line 18 can be at A, B or C, and
--   A and B can be switched if desired. This even holds true for 
--   the opLnt/Lnp/Lnpt opcodes. Note that this program (alone?) 
--   demands that LineTab entries fall on instruction boundaries,
--   it may be possible to relax that rule if necessary.
--   Of course if you change the distance between A and C then the
--   LineTab must be updated to match.
--   Note that many opcodes have strange rules, which may hamper
--   things, such as "NB p1 obtained from [esp]-15 on error".
--   Some of the worst offenders at this could be changed; I have
--   recently changed opTcFail for a very similar reason.
--   As far as I have thought this through, I think the best plan
--   would be for the emitHex() routines to slap code into s5 as
--   they do now, but maintain a mini table of {offset,length},
--   and pepper pmain.e with HexFlush() or something.
--
--  Further to the above, it may make sense to emit "long" forms
--   of certain instructions rather than the shorter eax-specifics,
--   eg see loadReg(), ie #8B 05 xx xx xx xx works just as well as
--   #A1 xx xx xx xx for mov eax,[N]. The point of doing this would
--   be to effectively throw reg-agnostic code at the peepholer,
--   which could then "rename" register use when it spots that var
--   x needs to be in (say) edi later on. Note however that packing 
--   such back to short eax form might prove difficult as there is 
--   no isDead-1-byte marker and it may prove more costly to add one
--   than this gains, though if just leaving the #8B05's as is gets
--   us a measureable gain (in say p.exe rather than just a micro-
--   benchmark) then we'll gladly take it. I must admit that the
--   more I think of this, the more impractical it all seems, but 
--   after all it is just a suggestion.
--
--  Second stage. Either alternatively or in addition to peephole, 
--   and this is similar to what scanforShortJmp()/blurph() in
--   pemit.e do, after an s5 is complete, possibly with some idea
--   of improved type info, desires for loop hoisting, etc the
--   s5 bytes can be copied one by one fairly quickly/easily to
--   a new improved s5. As a quick example, for i=1 to 1000 do
--   k=j end for does not want to test j is initialised every 
--   iteration, better to (say) opIchk it once at the start,
--   so somewhere you build a list of code insertions that you 
--   realised were wanted a tad too late to actually emit in 
--   the first pass, plug them in during this second stage.
--   A simpler approach might be to plant a modest isDead block
--   at the top of each routine or loop, bolt the insert code 
--   onto the very end of the s5 block, and patch that isDead 
--   to a jmp.
--
--  Register allocation (probably linear scan) might be rather nice.
--   You should assume that calling an opcode trashes all registers,
--   unless it says otherwise somewhere. See also the comment about 
--   "long" forms above.
--
--  Refless temps. There are some places, most noticeably conditions,
--   where an incref is unnecessary. Note that in say a=append(a,tmp)
--   it is critical that tmp has a refcount - if it happened to be,
--   for instance, a copy of a without an incref, then opApnd could
--   create an illegal circular reference (iirc, opApnd does test
--   for p2!=p3, but does not also test for [p2]!=[p3], instead
--   it relies on the refcount to avoid that and other cases).
--   Example: "if a[3]=x" has no need whatsoever to incref(a[3]). 
--   There are some wierd cases such as "if a[3]=modifya3() then", 
--   which do not particularly bother me except that if it is going 
--   to crash, it had better behave consistently when interpreted
--   and compiled, ie/and/or use normal incref'd tmps over function
--   calls etc. It is of course only the common/simple cases that 
--   we really need to care about.
--
--   With 20/20 hindsight, I realise that opJnotx and opJifx are
--   actually a specific implementation of this and could perhaps
--   be removed once this is in place (subject to timing tests).
--
--   Also note there is a temp ref optimisation already in place:
--          p(s[1])
--   makes a ref-counted copy of s[1] in a temp, then ParamList
--   moves it into the parameter of p() and zeroes it, avoiding
--   the incref/decref one stage later. Any change in this area 
--   should not upset that (for more details see emitHexMov).
--
--  Deallocate all temps and function results asap. Particularly in
--   the case of a function result, it is decref/deallocated at
--   each return statement. It might be much better to do it once
--   at the start. There are almost certainly similar situations 
--   with other tmps, but as the previous paragraphs explained it 
--   can all get a bit fuzzy.
--
--[DONE, I THINK:]
--  Log presence of possible forward calls to improve gvar handling.
--   Consider the following listing fragment:
--
--          ;    33 atom ai, af
--          ;    34      ai=1
--  xor ecx,ecx                           ;#0040B000: 063311                     uv 02 02  1   1      
--  mov edx,[#0040C2B4] (ai)              ;#0040B002: 213025 B4C24000            vu 04 00  1   1      
--  mov [#0040C2B4] (ai), dword 1         ;#0040B008: 307005 B4C24000 01000000   u  00 00  1   2      
--  cmp edx,h4                            ;#0040B012: 201372 00000040            vu 00 04  1   2      
--  jle #0040B026                         ;#0040B018: 176 0C                     v  00 00  1   3      
--  sub dword [ecx+edx*4-8],1             ;#0040B01A: 203154221 F8 01            u  00 06  3   4      
--  jne #0040B026                         ;#0040B01F: 165 05                     v  00 00  1   6      
--  call deallocX                         ;#0040B021: 350 1683FFFF               v  00 00  1   7      
--
--   At first it seems clear that the cmp edx,h4 .. call deallocX will never 
--   trigger and just mov [ai],1 ought to be enough. However there may be a 
--   forward call to a routine which sets ai to 1.1+1.2, hence removing the 
--   cmp edx,h4 etc could cause a memory leak since that 2.3 would never be 
--   freed. Note that tvars have much tighter scope rules which mean that a
--   similar situation is simply not possible. There is some code towards
--   the end of Assignment() which does the above optimisation for tvars, 
--   look for elsif ExitBP=-1 and TableEntry<0 ... onDeclaration=1.
--   The proposal (for similar handling of gvars) is kept simple: 
--   Once any possible forward calls are detected, we have to dealloc:
--
--              fwd_routine()
--              atom a1, a2
--              a1=1    -- need h4
--              procedure fwd_routine().... end procedure
--              a2=1    -- omit h4 if S_set bit clear
--
--  In other words once all possible forward calls have been resolved, then
--   we can revert to omitting the provably unnecessary code. In the first
--   instance I would suggest maintaining a count of things we threw on a 
--   backpatch list less how many we backpatched, but there is clearly 
--   something pertaining to routine_id which at the moment escapes me,
--   possibly a list of routine names to look out for with a "-1" thing
--   for the routine_id("do"&Name[idx]) cases, and something else ontop
--   for "integer r_Proc  ... call_proc(r_Proc,{}) ... r_Proc=r_id()".
--   AHA: the r_Proc case is "forever" - which is what I was missing.
--   Probably the trickiest thing would be to spot/ignore forward calls 
--   from fallow code, however if you get the called/defined bounds right
--   then you can probably rely on the programmer moving gvar assignment
--   out of such ranges.
--
--  Note: This is probably precisely the sort of "gotcha" that accounts
--   for a certain person's notorious reluctance to permit forward calls,
--   and/or yield -1 for routine_id("fwd_routine") until after the 
--   actual routine definition. It is probably true that "atom a1=1" and
--   "atom a1 a1=1" already exhibit a problem of this nature and require
--   a fix along these lines to avoid it, in TopDecls().
--
--DEV: newBase killed this, I think...
--  Detect prepend use and propagate it (eg if b=a occurs after a=prepend
--   then treat as if b=prepend had also occurred). Also slices that do
--   not start with a literal 1. Variables not affected in any way by 
--   such operations would not have to load the base (search pmain.e 
--   for "-20]"). Not sure any gain would be even measurable.
--
--  Overlapped typechecks, eg (currently, 4 clocks):
--      procedure check5(integer a1, integer b2)
--      mov eax,[a1]
--      cmp eax,h4
--      jl @f
--      mov edx,a1
--      call opTcFail
--     @@:
--      mov ecx,[b2]
--      cmp ecx,h4
--      jl @f
--      mov edx,b2
--      call opTcFail
--     @@:
--      ...
--      call opRetf
--
-- should be slightly faster (3 clocks) as:
--      procedure check5(integer a1, integer e5)
--      mov eax,[a1]
--      mov ecx,[b2]
--      cmp eax,h4
--      jl @f
--      mov edx,a1
--      call opTcFail
--     @@:
--      cmp ecx,h4
--      jl @f
--      mov edx,b2
--      call opTcFail
--     @@:
--      ...
--      call opRetf
--
--  or even (also 3 clocks, but less "cold code" in the cache):
--      procedure check5(integer a1, integer e5)
--    L0:
--      mov eax,[a1]
--      mov ecx,[b2]
--      cmp eax,h4
--      jge L1
--      cmp ecx,h4
--      jge L2
--      ...
--      call opRetf
--     L1:
--      mov edx,a1
--      jmp @f
--     L2:
--      mov edx,b2
--     @@:
--      push L0     ;; fake return addr
--      jmp opTcFail
--
--  I have to admit this is possibly alot of work for small gain.
--
--  [ED: Erm, that is enough for now.] I trust you've gotten the general idea
--  by now that the possibilities are boundless.
--

constant dumpSymTab = 01,
         doOneInclude = 0, -- NB "without debug" prevents listing, see above
--       doOneFileName = "ptok.e"
--       doOneFileName = "pmain.e"
--       doOneFileName = "pemit.e"
--       doOneFileName = "machine.e"
--       doOneFileName = "plist.e"
--       doOneFileName = "pdiag.e"
--       doOneFileName = "pttree.e"
--       doOneFileName = "pgscan.e"
--       doOneFileName = "pilxl.e"
--       doOneFileName = "p.exw"
--       doOneFileName = "eafonts.ew"
--       doOneFileName = "p2asm.e"
--       doOneFileName = "psym.e"
--       doOneFileName = "pmsgs.e"
--       doOneFileName = "pgets0.ew"
--       doOneFileName = "pltype.e"
--       doOneFileName = "pilx86.e"
         doOneFileName = "t01type.e",
         showlinetable = 0

global sequence vmap    -- variable map; var address --> offset into threadstack
                        --  (a flat array of all static and dynamic var refs)
                        -- ==> index into symtab for var name, type, etc.

global sequence codeTable, codeIdx

global integer outFrame = 0
global integer inFrame = 0

include p2asm.e         -- dissassembler engine

integer listfn          -- main xxx.asm output file

atom base1,             -- effective address of cs[1]
     tIL,               -- effective address of current top_level_sub
     sIL                -- effective address of current subroutine

object si               -- copy of symtab[i], speedwise
integer fNo             -- scratch (copy of symtab[i][S_FPno])

string name,            -- scratch (copy of filenames[i][2])
       path             -- scratch (copy of filepaths[f[i][1]])

sequence ctrl           -- sorted table of routine entries:
constant C_Fno = 1,     -- file no (idx to filetab, allfiles, etc) (from symtab[i][S_FPno])
         C_lNo = 2,     -- first line number (from symtab[i][S_1stl])
         C_sti = 3,     -- symtab index
         C_wdb = 4      -- modified K_wdb flag
integer cidx            -- idx to ctrl
sequence ci             -- copy of ctrl[cidx]

integer newBlock
        newBlock = 1

constant ALIGNASM = 1

function findboth(string d1word, string tjword, sequence stringset)
    return (find(d1word,stringset) and find(tjword,stringset))
end function
-- 29/5/14:
procedure alignasm(sequence dres, string tj)
string dres1 = dres[1] 
integer ch
integer d1pos=-1, tjpos=-1
string d1word="", tjword=""
string fmt = "    %-36s  ;#%08x: %-26s %s\n"
    for i=1 to length(dres1) do
        ch = dres1[i]
        if ch!=' ' then
            d1pos = i
            d1word = dres1[i..find(' ',dres1,i+1)-1]
            exit
        end if
    end for
    for i=1 to length(tj) do
        ch = tj[i]
        if ch='#' and match("#ilASM{",tj)=i then
            tj[i..i+6] = "       "
            ch = ' '
        end if
        if ch!=' ' then
            tjpos = i+4
            tjword = tj[i..find(' ',tj,i+1)-1]
            if length(tjword) and tjword[$]='\n' then
                tjword = tjword[1..$-1]
            end if
            exit
        end if
    end for
--DEV if we findboth(), why not substitute into dres?
    if length(d1word) 
    and (d1word=tjword or
--       {d1word,tjword}={"call","testN()"} or
         findboth(d1word,tjword,{"je","jz"}) or
         findboth(d1word,tjword,{"jne","jnz"}) or
         findboth(d1word,tjword,{"jb","jc","jnae"}) or
         findboth(d1word,tjword,{"jae","jnb","jnc"}) or
         findboth(d1word,tjword,{"ja","jnbe"}) or
         findboth(d1word,tjword,{"jbe","jna"}) or
         findboth(d1word,tjword,{"jge","jnl"}) or
         findboth(d1word,tjword,{"jle","jng"}) or
         findboth(d1word,tjword,{"jl","jnge"}) or
         findboth(d1word,tjword,{"jg","jnle"}) or
         findboth(d1word,tjword,{"jp","jpe"}) or
         findboth(d1word,tjword,{"jnp","jpo"}) or
         findboth(d1word,tjword,{"sete","setz"}) or
         findboth(d1word,tjword,{"setne","setnz"}) or
         findboth(d1word,tjword,{"setb","setc","setnae"}) or
         findboth(d1word,tjword,{"setae","setnb","setnc"}) or
         findboth(d1word,tjword,{"seta","setnbe"}) or
         findboth(d1word,tjword,{"setbe","setna"}) or
         findboth(d1word,tjword,{"setge","setnl"}) or
         findboth(d1word,tjword,{"setle","setng"}) or
         findboth(d1word,tjword,{"setl","setnge"}) or
         findboth(d1word,tjword,{"setg","setnle"}) or
         findboth(d1word,tjword,{"setp","setpe"}) or
         findboth(d1word,tjword,{"setnp","setpo"}) or
         findboth(d1word,tjword,{"lea","mov"}))
    and tjpos>d1pos then
        if not find(tjword,{"lea","mov"}) then
            dres[1][d1pos..d1pos+length(d1word)-1] = tjword
        end if
        d1word = repeat(' ',tjpos-d1pos) 
        puts(listfn,d1word)
        d1pos = match(d1word,dres1,d1pos)
        if length(dres1)<=36-length(d1word) then
            fmt = sprintf("    %%-%ds  ;#%%08x: %%-26s %%s\n",36-length(d1word))
        else
            fmt = "    %s  ;#%08x: %-26s %s\n"
        end if
    end if
    printf(listfn,fmt,dres)
end procedure

function is_data(atom v)
    return (v>=ImageBase2+BaseOfData2 and v<=ImageBase2+BaseOfData2+SizeOfData2)
end function
--if newEmit then
--  r_isdata = routine_id("is_data")
--end if

function is_code(atom v)
--if v=4202626 then
--  ?{v,ImageBase2,BaseOfCode2,SizeOfCode2}
--end if
    return (v>=ImageBase2+BaseOfCode2 and v<=ImageBase2+BaseOfCode2+SizeOfCode2)
end function
--if newEmit then
--  r_iscode = routine_id("is_code")
--end if

--with trace
--DEV temp (22/2/15)
procedure disassemble(integer eaddr, integer fromoffset, integer tooffset, string tj)
--procedure disassemble(integer eaddr, atom fromoffset, atom tooffset, string tj)
sequence dres, dres1
integer last, ro,rc,so,sc
atom ctrl1
integer machine
--trace(1)
    if newBlock then
--DEV need to set the machine!
if newEmit then
        r_isdata = routine_id("is_data")
        r_iscode = routine_id("is_code")
        machine = 32
        if X64 then
            machine = 64
        end if
--printf(1,"disassemble(eaddr=%08x,fromoffset=%08x,tooffset=%08x)\n",{eaddr,fromoffset,tooffset})
        decodeinit(eaddr+fromoffset, eaddr-base1+fromoffset,machine,iff(PE?arch_PE:arch_ELF))
else
        decodeinit(eaddr+fromoffset, eaddr-base1+fromoffset)
end if
        newBlock = 0
    end if
    last = eaddr+1+tooffset
    if dumpVM then
        ctrl1 = ctrl[1][1]
--?ctrl1
    end if
    while addr<last do
        if dumpVM then
            if addr=ctrl1 then
--DEV always true?
--              if string(ctrl[1][2]) then
                    dres = ctrl[1][2]
                    printf(listfn,"%s::\n;-%s\n",{dres,repeat('-',length(dres))})
--              end if
--DEV should this be done at the ctrl = sort(ctrl) stage?
--              while 1 do
                    ctrl = ctrl[2..length(ctrl)]
--                  if length(ctrl)=0 then exit end if
--                  if string(ctrl[1][2]) then exit end if
--              end while
                if length(ctrl) then
                    ctrl1 = ctrl[1][1]
                end if
            end if
            dres = decode()     -- {asm,addr,hex,analysis}
        else
            dres = decode()     -- {asm,addr,hex,analysis}
--DEV newEmit:
--DEV oops:
--if X64=0 then
--if wasOpCode=opCallOnce then trace(1) end if
            if (wasOpCode=opCallOnce or wasOpCode=opFrame or wasOpCode=opTchk)
            and (lastmov>1 and lastmov<=length(symtab)) 
            and sequence(symtab[lastmov]) then
                si = symtab[lastmov]
                if wasOpCode=opCallOnce then
                    fNo = si[S_FPno]
if fNo=0 then
puts(1,"warning: fNo is 0, plist.e line 721\n")
                    name = "*** fNo is 0 ***"
else
                    name = filenames[fNo][2]
end if
--23/4/15:
--6/1/17:
--              elsif si[S_Name]=-1 then
                elsif integer(si[S_Name]) then
-- added 1/5/15: (for the "call :%opFrame" in pDiagN.e/:!diagFrame, where edx got set in pemit2.e)
--8/7/15:
--if trim(tj)="call :%opFrame" then
if match("call :%opFrame",trim(tj))=1 then
                    name = ""
else
--printf(1,"warning: symtab[%d][S_Name] is -1, plist.e line 728\n",{lastmov})
printf(1,"warning: symtab[%d][S_Name] is %d, plist.e line 728\n",{lastmov,si[S_Name]})
?tj
--                  name = "??-1??"
                    name = sprintf("??%d??",si[S_Name])
end if
                else
                    name = si[S_Name]
                    if wasOpCode=opFrame then
                        inFrame = lastmov
lastmov = 0 -- added 4/11/14. (nowt specific, just think we should keep this 0 as much as possible. Main mod today was in p2asm.e)
                    end if
                end if
if length(name) then
                dres[1] &= " ("&name&")"
end if
                wasOpCode = 0
                resetmods()
            end if
--end if
            dres1 = dres[1]
            if length(dres1)>36 then    -- shorten if possible:
                so = match("dword #",dres1)
                if so then
                    dres1[so..so+5] = ""
--                  dres1 = dres1[1..so-1] & dres1[so+6..length(dres1)]
                    dres[1] = dres1
                end if
            end if
            if length(dres1)>36 then    -- shorten if possible:
                so = find('[',dres1)
                sc = find(']',dres1)
                ro = find('(',dres1)
                rc = find(')',dres1)
                if ro=sc+2 and sc>so and rc>ro then
                    -- convert eg mov [#0040C164] (res:useFlatString), dword 0
                    --         to mov [res:useFlatString], dword 0
                    if rc<length(dres1) and dres1[rc+1]=',' and dres1[sc+1]=' ' then
                        -- "[xxx] (yyy)," -> "[xxx]](yyy),"; pretend xxx 1 longer.
                        sc += 1
                        dres1[sc] = ']'
                    end if
                    dres1 = dres1[1..so]                -- ...[
                           &dres1[ro+1..rc-1]           -- xxx of (xxx)
--                         &dres1[ro..rc]               -- (xxx)
                           &dres1[sc..ro-1]             -- ]..(-1
                           &dres1[rc+1..length(dres1)]  -- )+1..$
                    dres[1] = dres1
                elsif ro=16 and rc>ro and equal(dres1[1..6],"call #") then
                    --  convert eg call #0040383A (opFrame) (sq_floor_div)...
                    --          to call opFrame (sq_floor_div)...
                    dres1 = dres1[1..5]                 -- "call "
                           &dres1[ro+1..rc-1]           -- xxx of (xxx)
                           &dres1[rc+1..length(dres1)]  -- )+1..$
                    dres[1] = dres1
                elsif ro=36 and rc>ro and equal(dres1[1..26],"mov [ebp+edx*4+20], dword ") then
                    --  convert eg mov [ebp+edx*4+20], dword 004038F0 (opRetf)
                    --          to mov [ebp+edx*4+20], opRetf
                    dres1 = dres1[1..20]                -- "mov [ebp+edx*4+20], "
                           &dres1[ro+1..rc-1]           -- xxx of (xxx)
--                         &dres1[ro..rc]               -- (xxx)
                           &dres1[rc+1..length(dres1)]  -- )+1..$
                    dres[1] = dres1
                end if
            end if
        end if
        if ALIGNASM then
            alignasm(dres, tj)
        else
            printf(listfn,"    %-36s  ;#%08x: %-26s %s\n",dres)
        end if
        if terminal then
            puts(listfn,"*** ERROR: decode of this segment aborted ***\n")
            puts(1,"*** ERROR: decode of this segment aborted ***\n")
--          if getc(0) then end if
            addr = last -- suppress message below
            exit
        end if
--      if addr>=last then exit end if
----    printf(1,"%08x %-22s %s\n",{addr,hex,asm})
--flush(listfn)
    end while
    if addr!=last then
--  if addr>last then   -- no help last time I tried...
        puts(listfn,"*** ERROR: decode of this segment ended at wrong address ***\n")
        puts(1,"*** ERROR: decode of this segment ended at wrong address ***\n")
--      if getc(0) then end if
    end if
end procedure

--global --temp, for pemit2 tests
function unpacklt(sequence linetab)
integer skip = 0, byte
atom word, dword
sequence res = {}
integer base = 0, tmp
    for i=1 to length(linetab) do
        if skip then
            skip -=1
        else
            byte = linetab[i]
            if byte>#7F then
                if byte>#81 then
                    res = append(res,byte-#100)
                elsif byte=#81 then
                    word = linetab[i+1]*#100+linetab[i+2]
                    if word>#7FFF then
                        word -= #10000
                    end if
                    res = append(res,word)
                    skip = 2
                elsif byte=#80 then
                    dword = linetab[i+1]*#1000000+linetab[i+2]*#10000+linetab[i+3]*#100+linetab[i+4]
                    if dword>#7FFFFFFF then
                        dword -= #100000000
                    end if
                    res = append(res,dword)
                    skip = 4
                else
                    ?9/0
                end if
            else
                res = append(res,byte)
            end if
        end if
    end for
    for i=1 to length(res) do
        if res[i]>0 then
            tmp = base
            base += res[i]
            res[i] = tmp
        end if
    end for
    return res
end function

sequence slt,           -- LineTab for next subroutine to be processed
         tlt            -- LineTab for current top_level_sub
integer sltidx,         -- index into slt
        tltidx          -- index into tlt
integer lti             -- scratch (copy of xlt[xltidx])
--, name, text, 
integer slNo,           -- line no in subroutine to look out for
        tlNo            -- line no in top_level_sub to look out for
integer slO,            -- subroutine code offset next to be processed
        tlO             -- toip_level_sub code offset next to be processed

integer newSub

procedure getSub(integer i)
--
-- Process the next ctrl entry. If it is a routine in file i, set up
--  slt, slNo, sltidx, etc. (If it is not "" it will be handled at
--  the top of the for i=1 to length(allfiles) loop, ie tlt, tlNo,
--  tltidx, etc will be set up from it).
--
integer slsi    -- scratch (idx to symtab)
    newSub = 1
    newBlock = 1
    cidx += 1
    slNo = -1
    if cidx<=length(ctrl) then
        ci = ctrl[cidx]
        if ci[C_Fno]=i then
            slsi = ci[C_sti]    -- symtab index
            si = symtab[slsi]
outFrame = slsi
            slt = si[S_ltab]
--4/11/19: (structs)
if slt!={} then
            sIL = si[S_il]
if newEmit then
    if listing!=-1 then
            slt = unpacklt(slt)
    end if
while length(slt)>1 and slt[$]<0 do
printf(1,"oops, slt[$]<0 (symtab[%d])\n",slsi) --DEV
    slt = slt[1..-2]
end while
    if not newEmit or listing!=-1 then -- 27/2/15
            sIL += BaseOfCode2+ImageBase2
    end if
end if
            slNo = ci[C_lNo]
            lti = slt[1]
            sltidx = 1
            if equal(slt,{-2}) then
--              slNo = -1   -- no good
                getSub(i)
            else
--              if lti<0 then
                if lti<0 and length(slt)>=2 and slt[2]=0 then
                    slNo -= lti
                    sltidx = 2
                end if
            end if
            slO = 0
end if
        end if
    end if
end procedure

constant tdesc={"S_Const","S_GVar","S_TVar","S_Nspc","S_Rsvd","S_Type","S_Func","S_Proc"}

integer siState -- scratch (copy of symtab[i][S_State])     \
--string sState -- human readable version of siState        / Also used for filenames, paths, S_vtype, S_Efct, etc.
sequence sState -- human readable version of siState        / Also used for filenames, paths, S_vtype, S_Efct, etc.

procedure stateDesc(sequence txt, integer bit)
    if and_bits(siState,bit) then
        if length(sState)>1 then sState &= '+' end if
        sState &= txt
        siState -= bit
    end if
end procedure

function namestring(object name)
    if not string(name) then
        name = sprint(name)
    end if
    return name
end function

constant day = {"Sun","Mon","Tue","Wed","Thu","Fri","Sat"}

--with trace
global procedure dump_listing()
integer siNTyp,     -- scratch (copy of symtab[i][S_NTyp])
        lNo,        -- scratch (copy of symtab[i][S_1stl])
        tlsi,       -- scratch (idx to symtab)
        X,          -- scratch, modified and_bits(symtab[i][S_State],K_wdb)
        c,          -- scratch, var no used to build varmap
        k,          -- scratch, used for printing S_gInfo
        putafter,   -- flag: use hll code to split tls and subroutine code...
        vmax,
        wasRtnLit,
        wasNoClr
sequence etxt       -- copy of allfiles[i]
string outfile,     -- scratch (list[2].asm)
       tj,          -- scratch (copy of etxt[j])
--     ptxt,        -- scratch (printed text)
       options      -- "-d!"|("-d"[" -nodiag"])
sequence ptxt
object gi           -- for printing S_gInfo
atom gi2,gi3        -- ""
integer sidx, last0 -- for new si[i..j]=0 loop
sequence d          -- for generated date
sequence wasopNames -- for testall
integer offset
sequence vm_names
string vm_part
integer vm_len
integer sii
string file1

--trace(1)
    if dumpVM then
        outfile = mainpath&"list2.asm"
    else
        outfile = mainpath&"list.asm"
    end if

    printf(1,"creating listing file %s...",{outfile})
    listfn = open(outfile,"w")
    if listfn=-1 then
        puts(1,"unable to open file\n")
        if getc(0) then end if
        abort(0)
    end if

--  puts(listfn,";;\n;; Phix dissassembly listing.  (nb for human readership only; see plist.e)\n")

    d = date()                                              -- {year,month,day,hour,minute,second,dow,doy}
    d = d[4..6]&{day[d[7]]}&d[3]&d[2]&remainder(d[1],100)   -- {hour,minute,second,dow,day,month,year}
    if listing=-1 then
        options = "-d! (-nodiag)"
    elsif nodiag then
        options = "-d -nodiag"
    else
        options = "-d"
    end if
    d = append(d,options)
    printf(listfn,";;\n;; Phix dissassembly listing.  Generated at %d:%02d:%02d on %s %02d/%02d/%02d.  (%s)\n",d)
    puts(listfn,";;  (NB: Intended for human readership only; see plist.e for details)\n")

--if newEmit then
    -- **DO NOT DELETE** The next line can be extremely useful when diagnosing listing file problems.
--  printf(listfn,";; ImageBase2=#%08x, BaseOfCode2=#%08x, SizeOfCode2=#%08x, BaseOfData2=#%08x, SizeOfData2=#%08x\n",
--            {ImageBase2,BaseOfCode2,SizeOfCode2,BaseOfData2,SizeOfData2})
    printf(listfn,";; ImageBase2=#%08x, BaseOfCode2=#%08x (#%08x), SizeOfCode2=#%08x (limit=#%08x),\n"&
                  ";;                       BaseOfData2=#%08x (#%08x), SizeOfData2=#%08x (limit=#%08x)\n",
              {ImageBase2,BaseOfCode2,(ImageBase2+BaseOfCode2),SizeOfCode2,(ImageBase2+BaseOfCode2+SizeOfCode2),
                          BaseOfData2,(ImageBase2+BaseOfData2),SizeOfData2,(ImageBase2+BaseOfData2+SizeOfData2)})
--;; ImageBase2=#00400000, BaseOfCode2=#00116000, SizeOfCode2=#000FF5B0, BaseOfData2=#00002000, SizeOfData2=#00113EBC
--;; ImageBase2=#00400000, BaseOfCode2=#00116000 (#0051600), SizeOfCode2=#000FF5B0 (limit=#005???),
--;;                         BaseOfData2=#00002000 (#0040200), SizeOfData2=#00113EBC (limit=#00513EBC)
    printf(listfn,";; X64=%d, PE=%d\n",{X64,PE})
--end if

    if listing=-1 then
        puts(listfn,";;  (NB2 Interpreted/\'-d!\' option: using generic non-optimised code.)\n")
if newEmit then -- (DEV make it so that "p -e! test" sends mainpath B/SoC/D to Edita which then updates offsets, if line 8 matches mainpath)
--      puts(listfn,";;  (    Run with \'-e!\' to make Edita update this listing with real addresses.)\n")
--      puts(listfn,";;  (    *** NB -e! has caused addresses to be updated to match the running copy ***\n")
-- Note that every time you interpret a program it may have different addresses. My guess is lifo queues on each cache level to improve
--  compatibility of some (naughty) applications that were getting away with referencing memory shortly after releasing it, or at least
--  that matches my observations. Whatever it is, it has definitely changed between Windows XP and Windows 7. To counter this, after a
--  p -d! creates a listing file, p -e! signals Edita to update the addresses, so the file on disk or a printed copy may no longer match 
--  the on-screen version. In contrast, compilation and plain -d (without the bang) listing and executable files always use exactly the 
--  same fixed absolute addresses, unless there are changes to the source files, or possibly some (eg -nodiag) command line options.
else
        puts(listfn,";;  (    Run with otherwise null-effect \'-e!\' to match this listing.)\n")
end if
        --  ( Making the interpreter do all, or even some, of the things that -c (and/or -d) does,
        --    fairly obviously would have a rather noticeable effect on overall performance..!! )
        --  If you are running OllyDbg, you may want to run "p -d! test" under ollydbg first,
        --   then "p -e! test", but my attempts to do so were not always very successful.
    end if
    puts(listfn,";;\n")

-- replace some otherwise confusing opNames:
    if testall then wasopNames = opNames end if
--DEV newEmit...
    opNames[opMovbi] = "StoreFlt"
    opNames[opMovsi] = "deallocX"
    opNames[opMovti] = "e92movti"   -- (or e110tce)
    opNames[opAddiii] = "e01tcfAddiii"
    opNames[opMuliii] = "e01tcfediMul"
    opNames[opDiviii] = "e01tcfediDiv"
    opNames[opDivi2] = "e01tcfDivi2"
    opNames[opDivf2] = "e02atdb0"
--  opNames[opInc] = "opIncDecError"    -- DEV 12/3/09 no longer in use
    opNames[opJge] = "opJcc"
--  opNames[opJlt] = "opJcc"
    opNames[opJeq] = "opJccE"
--  opNames[opJne] = "opJccE"
--  opNames[opJgt] = "opJcc"
--  opNames[opJle] = "opJcc"

    codeTable = {}  -- routine code addresses

    if dumpVM then --DEV not newEmit!
if newEmit then ?9/0 end if
        if listing=-1 then
            puts(1,"You cannot use -d! when dumpVM is on!\n")
            puts(1,"(VM is the same interpreted and compiled!)\n")
            if getc(0) then end if
            abort(0)
        end if
        code_section = divm
        base1 = ImageBase + VMvaddr - (VMraddr-DVraddr)
        vmap = {}
        ctrl = {}
        VMep[opJlt] = 0
        VMep[opJne] = 0
        VMep[opJgt] = 0
        VMep[opJle] = 0
--DEV: untested, should be easier to read this way round:
--      VMep[opJifx] = 0
        VMep[opJnotx] = 0
    
        for i=1 to length(VMep) do
--          if VMep[i] and string(opNames[i]) then
            if VMep[i] and VMep[i]>=ImageBase+VMvaddr and string(opNames[i]) then
                ctrl = append(ctrl,{VMep[i],opNames[i]})
            end if
        end for
        ctrl = sort(ctrl)
--?ctrl
        disassemble(ImageBase+VMvaddr,0,VMvsize-1,"")
        close(listfn)
        if testall then opNames = wasopNames end if
        return
    end if -- dumpVM

    -- collect routine info from symtab, and sort by file/line
    -- create (sigh, another!) varmap while we are here...

    ctrl = {}
--  codeTable = {}  -- routine code addresses
    codeIdx = {}
    vmap = {}
    vmax = 0
--trace(1)
    for i=1 to length(symtab) do
--if i!=T_pathset
--and i!=T_fileset then
if i!=T_optable then
        si = symtab[i]
        if sequence(si) then
--?i
--if i=202 then
--?si
--end if
            siNTyp = si[S_NTyp]
            if siNTyp>=S_Type then
                fNo = si[S_FPno]
--if listing=-1 then
--if sequence(si[S_ltab]) and length(si[S_ltab]) then
--              codeTable = append(codeTable,si[S_il])
--              codeIdx = append(codeIdx,i)
--end if
--end if
--------6/12/09:
------if 01 then
-- 28/9/9
    gi = si[S_ltab]
--?gi
--DEV*** memory leak!! ******************************************* (28/9/9)
--  (I think the mods in pmain/Assignment (dated 28/9/9) will sort this out...
--   but not until we are back fully self-hosted.)
--  gi = (sequence(gi) and length(gi))
    if sequence(gi) and length(gi) then
        gi = 1
    else
        gi = 0
    end if
------end if
                if fNo then
                    lNo = si[S_1stl]
--if 0 then
--                  slt = si[S_ltab]
----                    X = and_bits(si[S_State],K_wdb)
---- 19/8/9:
--                  X = (and_bits(si[S_State],K_wdb) and length(slt))
--else
---- 28/9/9:
--6/12/09 (oops, DEV added to e6.exw):
--                  X = (and_bits(si[S_State],K_wdb) and gi)
                    X = and_bits(si[S_State],K_wdb)
--end if
--if not length(slt) then X=0 end if
                    ctrl = append(ctrl,{fNo,lNo,i,X})   -- {C_Fno,C_lNo,C_sti,C_wdb}
                end if
-- 3/6/09:
--X = si[S_il]
--if X then
--              codeTable = append(codeTable,X)
--if sequence(si[S_ltab]) and length(si[S_ltab]) then
-- 28/9/9
if gi then
    if newEmit then
        if listing=-1 then -- 27/2/15
                codeTable = append(codeTable,si[S_il])
        else
                codeTable = append(codeTable,si[S_il]+ImageBase2+BaseOfCode2)
--              codeTable = append(codeTable,si[S_il])      -- (might yet be right)
        end if
    else
                codeTable = append(codeTable,si[S_il])
    end if
                codeIdx = append(codeIdx,i)
end if
            else
                if siNTyp<=S_GVar2 then
--DEV
if newEmit then
else
                    c = si[S_Slink]
                    if c>length(vmap) then
                        vmap &= repeat(0,c-length(vmap))
                        if vmax<c then vmax = c end if
                    end if
    --DEV
    if c=0 then
        -- can we also put this in the listing file?
        puts(1,"oops, c=0 line 1189 in plist.e\n")
    else
                    vmap[c] = i
    end if
end if
--              elsif siNTyp=S_TVar3 then
                elsif siNTyp=S_TVar then
--DEV newEBP
if not newEBP then -- ??? what else ???
                    c = -si[S_Tidx]
                    if c>length(vmap) then
                        vmap &= repeat(0,c-length(vmap))
                        if vmax<c then vmax = c end if
                    end if
                    vmap[c] = i
end if
                end if
            end if
        end if
end if
--end if
    end for

    ctrl = sort(ctrl)
if NESTEDFUNC then
    if 0 then
        puts(1,"\n")
        for i=1 to length(ctrl) do
--constant C_Fno = 1,   -- file no (idx to filetab, allfiles, etc) (from symtab[i][S_FPno])
--       C_lNo = 2,     -- first line number (from symtab[i][S_1stl])
--       C_sti = 3,     -- symtab index
--       C_wdb = 4      -- modified K_wdb flag
--          ?ctrl[i]
            printf(1,"C_Fno:%d, C_lNo:%d, C_sti:%d, C_wdb:%08x\n",ctrl[i])
            ?symtab[ctrl[i][C_sti]]
--/*
C:\Program Files (x86)\Phix>p p -d! e01
creating listing file C:\Program Files (x86)\Phix\list.asm...
C_Fno:1, C_lNo:1, C_sti:21, C_wdb:100
{-1,        8=PROC,1,#900,0,857,{80},0,0,0,14915648,{-20,0,62,74},1,0}
C_Fno:1, C_lNo:7, C_sti:857, C_wdb:100
{"MakeList",7=FUNC,1,#111,0,861,{70,8},858,0,8,14915722,{0,51,-5,54,67,74,146,164,175},7,0,181}
C_Fno:1, C_lNo:9, C_sti:861, C_wdb:100
{"MakeItem",7=FUNC,1,#111,0,0,{70},866,0,2,14915897,{-1,0,-1,23,107},9,0,243}
--*/
        end for
    end if
end if

--puts(1,"\n")
--k = find(">initFEH",glblname)
--if k=0 then
--  puts(1,"k=0\n")
--else
--  printf(1,">initFEH = symtab[%d][S_il]=%08x, offset %08x\n",{glblabel[k],symtab[glblabel[k]][S_il],glboffset[k]})
--end if
--glboffset
--c2 = glblabel[i]
--tlsbase = symtab[c2][S_il]


    if doOneInclude then
        -- output only one include file:
        for j=1 to length(filenames) do
            if equal(filenames[j][2],doOneFileName) then
                X = 0
                cidx = 0
                for i=1 to length(ctrl) do
                    ci = ctrl[i]
                    fNo = ci[C_Fno]
                    if fNo=j then       -- keep all entries for specified file
                        cidx += 1
                        ctrl[cidx] = ci
                    elsif X!=fNo then   -- just keep one entry for other files
                        X = fNo
                        cidx += 1
                        ci[C_wdb] = 0
                        ctrl[cidx] = ci
                    end if
                end for
                ctrl = ctrl[1..cidx]
            end if
        end for
    end if

    cidx = 1
    base1 = symtab[T_maintls][S_il]         -- effective address of code_section[1]
if newEmit then
    if listing!=-1 then
        base1 += BaseOfCode2+ImageBase2
    end if
end if

    putafter = 0
    for i=1 to length(allfiles) do
        name = filenames[i][2]
        path = filepaths[filenames[i][1]]
        if equal(expandedYet[i],0) then
            text = allfiles[i]
            exptext[i] = expandIntoLines()
            expandedYet[i] = linestarts
        end if
        etxt = exptext[i]
if cidx>length(ctrl) then       -- t43: as t42 below but this time psprint.e was last.
    ci = {0}    -- force "if ci[C_Fno]=i" to (quietly) fail 
else
        ci = ctrl[cidx]
end if
if ci[C_Fno]=i then
--DEV/SUG: (is this a good idea or is it horrid?)
--if cidx<=length(ctrl) and (ci:=ctrl[cidx])[C_Fno]=1 then
--      if ci[C_Fno]!=i then ?9/0 end if    -- removed/replaced by above 29/9/08.
--                                          -- t42: all calls to [ps]print[.e] (aka "?") are
--                                          -- wrapped by "if showprogress then" (which is =0),
--                                          -- and now the empty tls is also being suppressed.
--6/3/09:
        if ci[C_wdb] then   -- if with debug/valid LineTable
--      if ci[C_wdb]        -- if with debug/valid LineTable
--      and length(symtab[ci[C_sti]][S_ltab]) then
--6/3/09 ends (undone)
            tlsi = ci[C_sti]    -- symtab index
outFrame = tlsi
--?tlsi
            si = symtab[tlsi]
--?si
            tlt = si[S_ltab]
            tIL = si[S_il]
if newEmit then
--printf(1,"plist.e:symtab[%d][S_il]=%08x+%08x+%08x\n",{tlsi,tIL,ImageBase2,BaseOfCode2})
    if listing!=-1 then
            tlt = unpacklt(tlt)
            tIL += BaseOfCode2+ImageBase2
    end if
end if
            tlNo = ci[C_lNo]
--01/08/2013
if tlNo!=1 then
    cidx -= 1
    tlNo = -1
else
--          lti = tlt[1]
            tltidx = 1
--6/12
--          if equal(tlt,{-2}) then
            if find(tlt,{{-2},{},0}) then
                -- may no longer be required...
                tlNo = -1
            else
--6/12
lti = tlt[1]
--              if lti<0 and length(tlt)>=2 and tlt[2]=0 then
                if lti<0 and tlt[2]=0 then
                    tlNo -= lti
                    tltidx = 2
                end if
            end if
end if
            tlO = 0
----01/08/2013
--if ci[C_lNo]!=1 then
--  cidx -= 1
--end if
            getSub(i)
            puts(listfn,";"&path&name&":\n")
            puts(listfn,";"&repeat('=',length(path)+length(name))&"=\n")
            for j=1 to length(etxt) do
                tj = etxt[j]
                if tlNo=slNo and j=tlNo then
                    -- Special case: code emit occurred for both tls and a routine:
                    --  Use the hll code line to split the two. This can occur for
                    --  example if line 1 is function x(integer i) - the opCallOnce
                    --  (say for pdiag.e) and the typecheck i are both on line 1,
                    --  but of course the binary is actually miles apart.
                    putafter=1
                else
                    printf(listfn,"; %5d %s",{j,tj})
                end if
                if j=tlNo then
                    lti = tlt[tltidx]
                    if lti>=0 then
                        tlNo += 1
                        tltidx += 1
                        lti = tlt[tltidx]
                    end if
                    if lti<0 then
                        tlNo -= lti
                        tltidx += 1
                        lti = tlt[tltidx]
                    end if
--?tIL
--trace(1)
                    disassemble(tIL,tlO,lti-1,tj)   -- base,from,to
                    if tltidx<length(tlt) then
                        tlO = lti
                    else
                        tlNo = -1
                    end if
                    if putafter then
                        printf(listfn,"; %5d %s",{j,tj})
                        putafter = 0
                    end if
                end if
                if j=slNo then  -- nb j=tlNo=slNo is rare, but possible
                    newBlock = newSub
                    newSub = 0
                    lti = slt[sltidx]
                    if lti>=0 then
                        slNo += 1
                        sltidx += 1
                        lti = slt[sltidx]
                    end if
                    if lti<0 then
                        slNo -= lti
                        sltidx += 1
--DEV ioob here because slt[$]<0, "fixed" above...
                        lti = slt[sltidx]
                    end if
                    disassemble(sIL,slO,lti-1,tj) -- base,from,to
                    if sltidx<length(slt) then
                        slO = lti
                    else
                        getSub(i)
                    end if
                end if
            end for
        else
            if doOneInclude then
--              puts(listfn,";"&path&name&": - skipped (doOneInclude=1)\n")
--              ptxt = ";"&path&name&": - skipped (doOneInclude=1)\n"
                ptxt = "doOneInclude=1"
            else
--              puts(listfn,";"&path&name&": - skipped (without debug)\n")
--              ptxt = ";"&path&name&": - skipped (without debug)\n"
                ptxt = "without debug"
            end if
            ptxt = ";"&path&name&": - skipped ("&ptxt&")\n"
            puts(listfn,ptxt)
            puts(listfn,";"&repeat('=',length(ptxt)-1)&"\n")
            -- skip any routines for file i that managed to get into ctrl:
            for j=cidx+1 to length(ctrl) do
                if ctrl[j][C_Fno]!=i then
                    cidx = j
                    exit
                end if
            end for
        end if
else
--              puts(listfn,";"&path&name&": - skipped (no code entries)\n")
        ptxt = ";"&path&name&": - skipped (no code entries)\n"
        puts(listfn,ptxt)
        puts(listfn,";"&repeat('=',length(ptxt)-1)&"\n")
end if
    end for
    if dumpSymTab then
        puts(listfn,"\n\n; Symtab Dump.\n")
        puts(listfn,    "; ============\n\n")
        puts(listfn,";global constant S_Name  = 1,  -- const/var/rtn name\n")
        puts(listfn,";                S_NTyp  = 2,  -- Const/GVar/TVar/Nspc/Type/Func/Proc\n")
        puts(listfn,";                S_FPno  = 3,  -- File and Path number\n")
        puts(listfn,";                S_State = 4,  -- state flag. S_fwd/S_used/S_set\n")
        puts(listfn,";                S_Nlink = 5,  -- name chain\n")
        puts(listfn,";                S_Slink = 6,  -- scope/secondary chain\n")
        puts(listfn,";                -- constants and variables [S_NTyp<=S_TVar]\n")
        puts(listfn,";                S_vtype = 7,  -- variable type or namespace fileno\n")
        puts(listfn,";                 (plus gInfo,varno/addr/thread idx/value)\n")
--DEV:++S_gNew etc
        puts(listfn,";                -- routines [S_NTyp>=S_Type]\n")
        puts(listfn,";                S_sig   = 7,  -- routine signature\n")
        puts(listfn,";                S_Parm1 = 8,  -- first parameter. (idx to symtab, follow S_Slink)\n")
        puts(listfn,";                S_ParmN = 9,  -- minimum no of parameters (max is length(S_sig)-1)\n")
        puts(listfn,";                S_Ltot  = 10, -- total no of parameters, locals, and temporary vars\n")
        puts(listfn,";                S_il    = 11, -- intermediate code\n")
        if showlinetable then
        puts(listfn,";                S_ltab  = 12, -- line table\n")
        puts(listfn,";                S_1stl  = 13  -- first line\n")
        puts(listfn,";                S_Efct  = 14, -- side effects\n")
        end if
--DEV S_Efct=14
--DEV if state_as_hex then
--      puts(listfn,";\n")
--      puts(listfn,";global constant S_used = #000001, -- symtab[i][S_State] values.\n")
--      puts(listfn,";                S_set  = #000002, -- used/set are for not used/never assigned a value warnings.\n")
--      puts(listfn,";                S_fwd  = #000004, -- routine is not yet defined (explicit \"forward\" or implicit call)\n")
--      puts(listfn,";                S_for  = #000008, -- variable in current use as for loop control var (cleared on end for)\n")
--      puts(listfn,";                K_used = #000010, -- reserved for compile (aka bind) version of used\n")
--      puts(listfn,";                K_sqr  = #000020, -- sequence rebuilt flag when interpreting\n")
--      puts(listfn,";                K_aod  = #000040, -- assignment on declaration (avoid warnings/force cleanup)\n")
--      puts(listfn,";                K_wdb  = #000100, -- with debug setting\n")
--      puts(listfn,";                K_noclr= #000200, -- do not clear on load (ie assignment on declaration occurred)\n")
--      puts(listfn,";                                  -- only used for GVars\n")
--      puts(listfn,";                K_rtn  = #000400, -- indicates value is a routine_id (for bind)\n")
--      puts(listfn,";                K_ran  = #000800, -- set once routine has been called (for tls routines)\n")
--      puts(listfn,";                K_gbl  = #001000, -- a true \"global\"\n")
--      puts(listfn,";                K_Fres = #002000, -- a function result\n")
--      puts(listfn,";                K_lit  = #004000  -- literal flag\n")
--      puts(listfn,";                K_type = #008000  -- type() routine parameter\n")
--      puts(listfn,";                K_othr = #010000  -- other routine parameter\n")
--      puts(listfn,";                K_ridt = #020000  -- known routine_id target\n")
--      puts(listfn,";                K_dlft = #040000  -- defaulted param\n")
--      puts(listfn,";                K_drid = #080000  -- default routine_id\n")
        puts(listfn,";\n")
--    end if
--?{"symtab[1][S_Name] (psym.e line 1501)",symtab[1][S_Name]}
--      symtab[1][S_Name] = "integer"
        symtab[2][S_Name] = "T_N"   -- a float (but not an integer)
--      symtab[3][S_Name] = "atom"      -- (int|flt)
        symtab[4][S_Name] = "T_Dsq" -- a Dword sequence (but not a string)
        symtab[5][S_Name] = "0b0101"
        symtab[6][S_Name] = "0b0110"
        symtab[7][S_Name] = "0b0111"
--      symtab[8][S_Name] = "string"
        symtab[9][S_Name] = "0b1001"
        symtab[10][S_Name] = "0b1010"
        symtab[11][S_Name] = "0b1011"
--      symtab[12][S_Name] = "sequence" -- (dseq|str)
        symtab[13][S_Name] = "0b1101"
        symtab[14][S_Name] = "0b1110"
--      symtab[15][S_Name] = "object"
        symtab[T_maintls][S_Name] = "T_maintls"
        if sequence(symtab[T_constm1])
        and symtab[T_constm1][S_Name]=-1 then
            symtab[T_constm1][S_Name] = "T_constm1"
        end if
        if sequence(symtab[T_const0])
        and symtab[T_const0][S_Name]=-1 then
            symtab[T_const0][S_Name] = "T_const0"
        end if
        if sequence(symtab[T_const1])
        and symtab[T_const1][S_Name]=-1 then
            symtab[T_const1][S_Name] = "T_const1"
        end if
--added 4/6/10:
        ppOpt({pp_Q22,1})
        sidx = 0
        while sidx<symlimit do
            sidx += 1
--if sidx=388 then trace(1) end if
--if sidx!=T_pathset
--and sidx!=T_fileset then
            si = symtab[sidx]
            if atom(si) then
                if equal(si,0) and sidx>T_maintls then
                    last0 = sidx+1
                    while last0<=symlimit and equal(symtab[last0],0) do
                        last0 += 1
                    end while
                    last0 -= 1
                    if sidx=last0 then
                        printf(listfn,"symtab[%d]:0\n",sidx)
                    else
                        printf(listfn,"symtab[%d..%d]:0\n",{sidx,last0})
                        sidx = last0
                    end if
                elsif sidx=T_pathset then
                    file1 = sprintf("symtab[%d=filepaths]",{sidx})
                    for i=1 to length(filepaths) do
                        sState = ppf(filepaths[i])
                        printf(listfn,"%s[%d]:%s\n",{file1,i,sState})
                        file1 = "                    "
                    end for
                elsif sidx=T_fileset then
                    file1 = sprintf("symtab[%d=filenames]",{sidx})
                    for i=1 to length(filenames) do
                        sState = ppf(filenames[i])
                        printf(listfn,"%s[%d]:%s\n",{file1,i,sState})
                        file1 = "                    "
                    end for
                else
                    if sidx=T_nslink then
                        sState = sprintf("%d (T_nslink)",si)
                    elsif sidx=T_cmdlnflg then
                        sState = sprintf("%d (T_cmdlnflg)",si)
--                  elsif sidx=T_callstk then   --DEV T_optable
--                      sState = sprintf("%d (for T_callstk)",si)
                    elsif sidx=T_ds4 then
                        sState = sprintf("#%08x[#%08x] (T_ds4)",{si,si*4})
                    else
                        sState = ppf(si)
                    end if
                    printf(listfn,"symtab[%d]:%s\n",{sidx,sState})
                end if
            elsif sidx=T_optable then
                vm_names = get_vm_names()
--              if length(si)!=length(vm_names) then ?9/0 end if
                if length(si)!=length(vm_names) then
                    printf(1,"plist.e line 1518 oops: length(si)=%d, length(vm_names)=%d\n",{length(si),length(vm_names)})
                end if
if 0 then
                sState = "{"
                vm_len = 24
                for i=1 to length(si) do
                    if i!=1 then
                        sState &= ", "
                        vm_len += 2
                    end if
                    sii = si[i]
--                  if bind then
                    if bind and sii!=0 then
                        sii += ImageBase2+BaseOfCode2
                    end if
                    vm_part = sprintf("#%08x (%d:%s)",{sii,i,vm_names[i]})
                    vm_len += length(vm_part)
                    if vm_len>122 then
                        sState &= "\n                        "
                        vm_len = 24+length(vm_part)
                    end if
                    sState &= vm_part
                end for
                sState
                 &= "}"
                printf(listfn,"symtab[%d] (T_optable):%s\n",{sidx,sState})
--                         123456789012345678901234
else
                    file1 = sprintf("symtab[%d=optable]",{sidx})
--                  for i=1 to length(filenames) do
                    for i=1 to length(si) do
                        sii = si[i]
                        if bind and sii!=0 then
                            sii += ImageBase2+BaseOfCode2
                        end if
                        printf(listfn,"%s[%d]:#%08x (:%s)\n",{file1,i,sii,vm_names[i]})
                        file1 = "                  "
                    end for
end if
            else
                printf(listfn,"symtab[%d]:",sidx)
--              if equal(si[S_Name],-1) then
                if atom(si[S_Name]) then
                    if not equal(si[S_Name],-1) then
-- Should no longer happen. If it does there is some linkage problem.
--  Last occurred 12/09 because addSymEntry was slapping S_used_set_gbl
--  (which includes K_gbl) on non-global entries under without warning,
--  which in turn meant those locals were not being skipped over in the
--  S_Nlink chain (also in addSymEntry, as well as DoConstant/InTable)
--  before adding a real global. It now uses S_used_and_set.
-- Take your pick of the following, but _never_ delete this test.
--  (Besides, we don't much care about the speed of the -d option.)
--                      trace(1)
                        si[S_Name] = sprintf("%d",si[S_Name])
--                      si[S_Name] = getname(si[S_Name],-2)
--                      si[S_Name] = getname(si[S_Name],-2) & "**??**?"
--DEV
--                      si[S_Name] = 9/0
--                      si[S_Name] = "9/0"
                    else
                        si[S_Name] = "-1"
                    end if
                end if
                siNTyp = si[S_NTyp]
                si[S_NTyp] = tdesc[siNTyp]
                siState = si[S_State]
                wasRtnLit = and_bits(siState,K_rtn+K_lit)
                wasNoClr = and_bits(siState,K_noclr)
                if siState=0 then
                    sState = "#0"
                else
                    sState = "("
                    stateDesc("S_used",S_used)
                    stateDesc("S_set",S_set)
                    stateDesc("S_fwd",S_fwd)
                    stateDesc("S_for",S_for)
                    stateDesc("K_used",K_used)
                    stateDesc("K_sqr",K_sqr)
                    stateDesc("K_aod",K_aod)
                    stateDesc("K_wdb",K_wdb)
                    stateDesc("K_noclr",K_noclr)
                    stateDesc("K_rtn",K_rtn)
                    stateDesc("K_ran",K_ran)
                    stateDesc("K_gbl",K_gbl)
                    stateDesc("K_Fres",K_Fres)
                    stateDesc("K_lit",K_lit)
                    stateDesc("K_type",K_type)
                    stateDesc("K_othr",K_othr)
                    stateDesc("K_ridt",K_ridt)
                    stateDesc("K_dlft",K_dlft)
                    stateDesc("K_drid",K_drid)
                    stateDesc("K_asmm",K_asmm)
--                  stateDesc("K_struc",K_struc)
                    if siState then
                        sState &= sprintf(" + *** #%x ***??",siState)
                    end if
                    sState &= ')'
                end if
                siState = si[S_State]
                si[S_State] = sState
                if siNTyp<=S_TVar then
--if not newEmit then -- 26/10/14 (temp/DEV)
--                  sState = symtab[si[S_vtype]][S_Name]    -- eg 3 -> "atom" (udts too)
                    k = si[S_vtype]
if k<1 or k>length(symtab) then
                    sState = sprintf("***ioob: symtab[1..%d][%d]***",{length(symtab),k})
                    printf(1,"%s\n",{sState})
else
                    sState = namestring(symtab[k][S_Name])  -- eg 3 -> "atom" (udts too)
end if
--end if
if 1 then   -- show S_gInfo
--DEV 10/5/14:
--                  if siNTyp!=S_Const
--                  or not wasRtnLit
--                  or length(si)>=S_gInfo then
                    if siNTyp!=S_Const
                    and not wasRtnLit
                    and length(si)>=S_gInfo then
                        gi = si[S_gInfo]
                        if atom(gi) then
                            if si[S_vtype]!=T_integer or not wasNoClr then
--                          if si[S_vtype]=T_integer and wasNoClr then
--                              sState &= sprintf(",{T_integer,%d,%d}",si[S_value])
--                          else
                                sState &= ",?"
                            end if
                        else
                            k = gi[1]
                            if k then
                                gi[1] = symtab[k][S_Name]
                            else
                                gi[1] = "0"
                            end if
                            gi2 = gi[2]
                            gi3 = gi[3]
                            if gi2=MAXINT then
                                if gi3=MININT then
                                    gi[2] = "-"
                                    gi[3] = ""
                                else
                                    gi[2] = "MAXINT"
                                    if gi3=MAXINT then
                                        gi[3] = "MAXINT"
                                    else
                                        gi[3] = sprint(gi3)
                                    end if
                                end if
                            else
                                if gi2=MININT then
                                    gi[2] = "MININT"
                                else
                                    gi[2] = sprint(gi2)
                                end if
                                if gi3=MININT then
                                    gi[3] = "MININT"
                                elsif gi3=MAXINT then
                                    gi[3] = "MAXINT"
                                elsif gi3=MAXLEN then
                                    gi[3] = "MAXLEN"
                                else
                                    gi[3] = sprint(gi3)
                                end if
                            end if
                            k = gi[4]
                            if k then
                                gi[4] = symtab[k][S_Name]
                            else
                                gi[4] = "0"
                            end if
                            -- gi[5] is length (-2=any,-1=unknown or n/a, else
                            --                  >=0 is the exact/known length)
                            sState &= sprintf(",{%s,%s,%s,%s,%d}",gi)
                        end if
                    end if
end if -- show gInfo
                    si[S_vtype] = sState
                    k = si[S_Slink]
if newEmit then
                    if siNTyp<=S_GVar2 then -- S_Const and S_Gvar2
--DEV 32/64 bit...?
--?9/0
--/*
--                      relocations[CODE][DATA] = append(relocations[CODE][DATA],cout-1)
--
--      if machine=M32 then
----            code = fixup(code,relocations[CODE][CODE],machine,0,codeBase+ImageBase)
--          code = fixup(code,relocations[CODE][DATA],machine,0,dataBase+ImageBase)
----            data = fixup(data,relocations[DATA][CODE],machine,0,codeBase+ImageBase)
----            data = fixup(data,relocations[DATA][DATA],machine,0,dataBase+ImageBase)
--*/
                        if X64 then
                            offset = (k)*8+24
                        else
                            offset = (k)*4+16
                        end if
                        offset += ImageBase2+BaseOfData2
--                      offset += ImageBase2+DSvaddr
                        sState = sprintf("%d/#%08x",{k,offset})
                    else
                        sState = sprintf("%d",k)
                    end if
else
                    if siNTyp<=S_GVar2 then -- S_Const and S_Gvar2
                        sState = sprintf("%d/#%08x",{k,ImageBase+DSvaddr+(k-1)*4})
                    else
                        sState = sprintf("%d",k)
                    end if
end if
                    si[S_Slink] = sState
                    sState = ppf(si[S_value])
                    if siNTyp=S_TVar then
--DEV newEBP (we probably want to show eg "[ebp+12]" here)
if newEmit then
                        if and_bits(siState,K_Fres) then
--                          sState = "n/a (K_Fres)"
                            if X64 then
                                sState = "(rax)"
                            else
                                sState = "(eax)"
                            end if
                        else
                            k = si[S_Tidx]
--                          sState = '['&sState&"],"&sprintf("%d/?#%08x",{k,0})
                            if k>0 then ?9/0 end if -- sanity check
                            if X64 then
                                if k=0 then
                                    sState = "[rsp]"
                                else
                                    sState = sprintf("[rsp%d]",k*8)
                                end if
                            else
                                if k=0 then
                                    sState = "[esp]"
                                else
                                    sState = sprintf("[esp%d]",k*4)
                                end if
                            end if
                        end if
else
                        k = si[S_Tidx]
                        sState = '['&sState&"],"&sprintf("%d/#%08x",{k,ImageBase+DSvaddr+(-1-k)*4})
end if
                        -- ('[',']' since TVar value is unlikely to be meaningful, btw)
                    end if
                    si[S_value] = sState
                    printf(listfn,"{%s,%s,%d,%s,%d,%s,%s,%s}\n",si[1..S_value])
--              elsif siNTyp=S_Nspc then
                elsif siNTyp<=S_Rsvd then
                    printf(listfn,"{%s,%s,%d,%s,%d,%d,%d}\n",si)
                else    -- type/function/procedure(inc top_level_subs)
--21/8/10 (possibly temp?)
if sequence(si[S_sig]) then
if not newEmit then -- (temp/DEV 26/10/14)
                    si[S_sig][1] = {si[S_sig][1]}
                    for j=2 to length(si[S_sig]) do
                        si[S_sig][j] = symtab[si[S_sig][j]][1]
                    end for
end if
end if
                    ptxt = ppExf(si[S_sig],{pp_StrFmt,-1})
--23/6/16: (this was always going to be temporary anyway, needs S_Efct preserved in pEmit2 [same date])
if 0 then
--                  if siNTyp>=S_Type then
                        if length(si)>=S_Efct then
                            k = si[S_Efct]
                            if k=E_none then
                                ptxt &= " [E_none]"
                            elsif k=E_other then
                                ptxt &= " [E_other]"
                            elsif k=E_all then
                                ptxt &= " [E_all]"
                            else
                                ptxt &= sprintf(" [%08x]",k)
                            end if
--global constant E_none = 0,
--              E_other = #20000000,
--              E_vars = #1FFFFFFF,
--              E_all = #3FFFFFFF
                        else
                            ptxt &= sprintf(" [len=%d]",length(si))
                        end if
--                      end if
end if

                    si[S_sig] = ptxt
if newEmit then
    if listing!=-1 then
                    si[S_il] = si[S_il]+BaseOfCode2+ImageBase2
    end if
end if
--DEV make the linetab optional (and/or put it on a newline, properly folded):
--9/2/15:
--                  ptxt = sprint(si[S_ltab])
if showlinetable then
                    ptxt = ppExf(si[S_ltab],{pp_StrFmt,1})
                    si[S_ltab] = ptxt
--                  printf(listfn,"{%s,%s,%d,%s,%d,%d,%s,%d,%d,%d,#%08x,%s,%d}\n",si[1..S_1stl])
-- to show [S_Efct]:
                    if equal(si[S_Name],"-1") then          -- S_Efct is meaningless on tls
                        sState = "0"
                    else
                        siState = si[S_Efct]
                        if siState=E_none then
                            sState = "E_none"
                        elsif siState=E_other then
                            sState = "E_other"
                        elsif siState=E_all then
                            sState = "E_all"
                        else
                            sState = sprintf("#%08x",siState)
                        end if
                    end if
                    si[S_Efct] = sState
                    printf(listfn,"{%s,%s,%d,%s,%d,%d,%s,%d,%d,%d,#%08x,%s,%d,%s}\n",si[1..S_Efct])
else
                    printf(listfn,"{%s,%s,%d,%s,%d,%d,%s,%d,%d,%d,#%08x}\n",si[1..S_il])
end if
                end if
            end if
--end if
--      end for
        end while
    end if  -- dumpSymTab
    close(listfn)
    puts(1,"\n")
    if testall then opNames = wasopNames end if
end procedure

