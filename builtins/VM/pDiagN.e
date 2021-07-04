--
-- pDiagN.e
-- =======
--
-- code responsible for writing the ex.err file
--
constant diagdiag = 0   -- show progress messages for debugging this source.
                        --  (0=none, 1=all, 2=almost all, ... N=last only.)
constant show_bad_era = 01

constant show_low_level_diagnostics = 0

-- Technical note:
--  This should be coded as defensively as possible, rather than relying on
--  itself to give meaningful messages about errors in itself. In other words,
--  pdiag.e should not rely on pdiag.e to catch runtime errors. Strangely, it
--  tends to manage better than expected, but even so that does not make it
--  a good idea, or mean you should be surprised, when it self-implodes.
--  This means (without going overboard) that variables should be defined as 
--  object and then explicitly tested for the expected type, instead of being
--  declared as the expected type and relying on the builtin type checking, 
--  that all subscripts should be explicitly tested to be in range, and that  
--  all peeks are checked first with xIsBadReadPtr, plus anything else that
--  you can think of!
--
--[DEV rewrite]
--  If you get any error message (before you start hacking this code) which 
--  begins with "diag.e: oops," then please reduce the program to the smallest
--  possible one which still exhibits the error and send it to me. Since this
--  has been passed a nonsense [era] or [ep1], there is nothing you can do to 
--  improve matters here; it is a low-level bug that I alone must fix, sorry.
--  The same is true for line numbers of -1, except of course when an error
--  is being reported in a dll, or some non-#ilasm assembly code, or maybe
--  when "without debug" is in force at the point where the error occurred.
--
--27/2/15:
--  **NOTE** [DEV gibberish...]
--  This (pdiagN.e) runs in the context of the debugee. The symtabptr and
--  gvarptr available via e/rbp are pertinent to the program being debugged,
--  rather than the (phix) interpreter that this is linked in as part of.
--  Hence routine_id/call_func/proc/opCallOnce are all liable to miserable
--  failure if you try anything clever without that in mind. Likewise a
--  delete_routine() triggering out-of-context may well fail, ie/eg if such
--  was used/saved during initialisation, and later invoked from diag().
--  I suppose it might be possible to combine prtnid.e and pcfunc.e into
--  a context-free module that used indexes to a {symtaptr,symtabidx} table
--  as the integer ids, all in an optable rippled down the opInterp chain, 
--  but don't look at me (like that) mate!
--
--include builtins\pmt.e    -- test pmach.e (passed with flying colours) [DEV newEmit needs work! but it can wait]
--
--global constant diagBase = 2  -- temp, checked by p.exw to match newBase

-- See also pmsgs.e, which is responsible for the two-liners created by
--  compile-time errors.
--
-- This file is automatically included as part of any exe file.
--
-- ******************************************
-- ******************************************
-- *****  WARNING: FRAGILE CODE AHEAD!  *****
-- ******************************************
-- ******************************************
--
-- Needless to say, I hope, take extra care here as if this should crash,
-- well, it will probably crash while trying to report the bug in itself...
--
-- When interpreting, errors in the user app are handled by the copy of 
--  pdiag.e in p.exe, which is also sitting ready to handle any errors
--  in the p.exw it was compiled from, whereas, of course, all compiled
--  applications must be shipped with their own private copy. This also
--  means you CANNOT "edit/test" this code in interpreted mode; instead
--  you MUST use -c to actually execute any modifications. "p -c test"
--  is strongly advised as opposed to using "p -c p" for testing, since
--  the latter may confuse by always being "one-step-out-of-date" (plus
--  of course you really do not want a broken p.exe lying around).
--
-- In general I would say there is no way to trace() this file, in any
--  case not when it is actually handling a crash. While theoretically
--  it may be possible to fudge a partial trace "in situ", it is going
--  to be far easier if you just copy/paste/rename and try out any new
--  ideas in some other/new/temporary file.
-- In many cases I have needed to add a slew of console displays to
--  narrow down the location of a bug in this code.
-- Of course it took me a while to figure out the proper way to code
--  this is to test everything and leave clues in the [partial] ex.err
--  should anything go wrong, such as those at the start of getValue().
--
-- TIP: In some cases where "p -c test" does not seem to want to work,
--  "p p -c test" may do the trick, particularly when adding/changing 
--  an opcode or making some other change to the p.exw sources.
-- A favourite trick is to replace the "if bind then" in Compile() in
--  pmain.e with "if 0 then", and [perhaps] manually include pdiag.e 
--  in a test program, to force the issue. YMMV, though.
--
-- TO DO: [DONE, I think]
--  Unify compile-time and run-time file handling so that the 
--  warning messages (from pmsgs.e) can be written to a run-time
--  .err report. Or perhaps just re-open it in append mode?
--
--!/**/without debug -- removal may or may not ease debugging!
                    -- (this option probably makes v. little difference
                    --  here, but see without type_check below.)
-- NB: the above "without debug" propagates into ppp.e and prntf.e, 
--     since they are used in the following code.

--/**/  -- not really needed, but avoids an opCallOnce:
--include builtins\VM\pcfunc.e
--/**/  include builtins\VM\pprntfN.e
--!/!*!*!/  include builtins\VM\psprintN.e
--/**/  include builtins\pcurrdir.e
--/**/  include builtins\pcase.e
--include builtins\VM\pfileioN.e
--include builtins\VM\pAbort.e -- (now in pStack.e)


integer edi4
constant CTB = "**CORRUPT TYPE BYTE**"
procedure show_corruption(string msg)
--
-- If this triggers you should assume a bug in builtins\VM, although it
-- could be a rogue poke() or possibly some application-specific #ilASM{}.
-- Such problems usually require OllyDbg/FDBG/edb or similar for analysis.
--
-- The idea is that if you can reproduce the error with a consistent diff
-- then you can trap after pGtcb has been set, and then trap the expected
-- memory location where you expect the corruption will occur. The actual
-- location will most likely be different on every run. This may need to
-- follow the pGtcb chain N steps (see pHeap) to obtain any consistency.
--
integer pGtcb
    #ilASM{
        call :%pGetpGtcb
        [32]
            mov [pGtcb],eax
        [64]
            mov [pGtcb],rax
        []
          }
    printf(1,"\n\n%s at #%08x(-1), pGtcb=#%08x, *4=#%08x, diff=#%08x\n\n",
             {msg,edi4*4,pGtcb,pGtcb*4,edi4*4-pGtcb*4})
end procedure

--include pgets0.ew     --DEV removed 16/6/08...
--include builtins\VM\pppN.e
include builtins\ppp.e
-- 01/08/2013: (DEV... some misunderstanding here, file was created 30/7/14)
--include builtins\VM\pdeleteN.e

--constant MAXLEN=5000

integer fn

-- added 17/5/15:
--constant MAXLINELEN = 77  -- approximate screen/printer width
constant MAXLINELEN = 129   -- approximate screen/printer width
                            -- (77 rather than 80 as sprint(o,l) may tag
                            --  a ".." in addition to the passed length)

--
-- Note: The following just curbs the excesses, and is not meant to be accurate 
--       or have the desired effect when less than the above MAXLINELEN constant. 
--       It is quite arbitrary. Feel free to add an extra 0 or two on the end, but 
--       there is no point in making it so big it takes weeks to finish a report 
--       that is either too big to load into an editor, or takes another two weeks 
--       to do so, and equally of course a "no limit" option is just plain daft. 
--       Quite often, debugging can be a bit easier when this is fairly small,
--       ie/eg 500 equates to around 10 page downs to get past a big sequence,
--       whereas, obviously, 20000 could easily make that around 400 keystrokes.
--       Equally, while 500 might be best for day-to-day development, 50,000
--       might be more sensible on live end-user systems, to ensure you capture
--       everything needed to diagnose some rare intermittent problem.
--       DEV make this configurable in pgui, and default to 50000? Or maybe 50000 for compiled programs, 500 for interpreted?
--       Be warned that at 50000 I just start to notice the odd pregnant pause 
--       as it (p.exe) struggles to give birth to some monster ex.err files, and
--       would fully expect exponential slowdown as things get even bigger.
--
constant MAXLENN = 20000 -- longest string/sequence you will ever really need
--constant MAXLENN = 1000000 -- longest string/sequence you will ever really need

-- Note: The following may not honor MAXLENN like it should/used to (which is, if 
--       anything, a problem in ppp.e rather than here). You may want this if, in 
--       order to reproduce a problem, you intend to cut/paste values out of an 
--       ex.err directly into the source of a test program.
--
constant OLDSTYLE = 0       --DEV/SUG make this a command line or pgui setting

--
-- Of course the first notion is to print things as and when lines get full, however
--  consider s = repeat(0,20); s[10] = repeat(0,20): if, as I do, you want to see:
--  s[1..9] = {0,0,0,0,0,0,0,0,0}
--  s[10] = {0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0}
--  s[11..20] = {0,0,0,0,0,0,0,0,0,0,0}
--  (assuming that 20 was enough to break past MAXLINELEN and force the linebreaks)
--  then while printing (or splitting) s[10] you've got 9 elements in hand, somewhere
--  higher up the call stack, that you haven't had reason to print yet, and of course 
--  with longer and more deeply nested structures things can get far worse. Instead,
--  just append things to a "printstack" along with a key to sort everything into the
--  right order before printing. The routines below chuck around a fair few partial
--  results, start and current indexes, and return the same in subtly different ways
--  depending on what just happened, that I would struggle to explain any better than
--  the source code itself does.
--

sequence printstack -- each element contains 3 items:
                    --  indicii - for sorting, eg {21,1}
                    --  name    - eg "symtab[21][1..11]" (matches that {21,1})
                    --  text    - the formatted value

procedure addtostack(sequence idii, integer idxr, string name, string text)
    idii = deep_copy(idii)
    idii[$] = idxr
--  if idii={11} then ?9/0 end if
    printstack = append(printstack,{idii,name,text})
end procedure

function subscr(string prev, string name, integer prst, integer prdx)
-- Helper routine for setting subscripts, typically for things 
--  that are just about to be chucked onto the printstack.
-- Note this is not meant to be called for any [1..$] cases.
    if prst=prdx then
        name = sprintf("%s[%d]",{name,prst})
    else
        name = sprintf("%s[%d..%d]",{name,prst,prdx})
        prev = "{"&prev&"}"
    end if
    return {prev,name}
end function

constant tnr = "tnr\\\"\'0e"
function allascii(string x)
-- Phix allows "strings" to hold binary data, so double check 
-- before printing it as a string.
integer c, jstart = 0
    for i=length(x) to 1 by -1 do
        c = x[i]
        if c<' ' or c>#7E or c='`' then
            if jstart=0 then jstart = i end if
            for j=jstart to 1 by -1 do
--added 9/4/20!!!:
                c = x[j]
--              if c<' ' or c>#7E or find(c,"\\\"\'") then
                if c<' ' or c>#7E or find(c,`\"'`) then
--              if c<' ' or c>#FE or find(c,"\\\"\'") then
                    c = find(c,"\t\n\r\\\"\'\0\e")
                    if c then
                        x[j..j] = '\\'&tnr[c]   -- NB does not work on RDS Eu/OpenEuphoria
                    else
                        return 0
                    end if
                end if
            end for
            return '"'&x&'"'
--      elsif jstart=0 and find(c,"\\\"\'") then
        elsif jstart=0 and find(c,`\"'`) then
            jstart = i
        end if
    end for
    return '`'&x&'`'
end function

--
--DEV known "bug":
--If this outputs say (there's 160s at [22781..2], so allascii() says 0):
--  text[1..11] = {60'<',33'!',68'D',79'O',67'C',84'T',89'Y',80'P',69'E',32' ',104'h'}
--  text[12..21] = {116't',109'm',108'l',62'>',60'<',104'h',116't',109'm',108'l',32' '}
--  text[22753..22762] = {68'D',110'n',111'o',116't',101'e',68'D',62'>',60'<',115's',116't'}
--  text[22763..22772] = {114'r',111'o',110'n',103'g',62'>',78'N',111'o',116't',101'e',60'<'}
--  text[22773..22782] = {47'/',115's',116't',114'r',111'o',110'n',103'g',62'>',160' ',160' '}
--  text[22783..22792] = {73'I',110'n',32' ',87'W',105'i',110'n',100'd',111'o',119'w',115's'}
--  text[22793..22802] = {160' ',56'8',32' ',97'a',110'n',100'd',32' ',108'l',97'a',116't'}
--  text[22803..22812] = {101'e',114'r',32' ',111'o',112'p',101'e',114'r',97'a',116't',105'i'}
--Then Edita/reformat ex.err munges it to:
--  text[1..11] = "<!DOCTYPE h"
--  text[12..21] = "tml><html "
--  text[22753..22762] = "DnoteD><st"
--  text[22763..22772] = "rong>Note<"
--  text[22773..22782] = {47'/',115's',116't',114'r',111'o',110'n',103'g',62'>',160' ',160' '}
--  text[22783..22792] = "In Windows"
--  text[22793..22802] = {160' ',56'8',32' ',97'a',110'n',100'd',32' ',108'l',97'a',116't'}
--  text[22803..22812] = "er operati"
--a) it looks awful (all scrunched up), b) we could perhaps do better in the first place, here,
--c) we would still want to leave some dword-sequences, in the same way that eaerr.e does.
--d) we might want a length>40 and >95% of elements are ascii or similar.
--e) why not just \xHH those in 1..255 we're not sure of?
--
--20/2/2021
--4/2/21:
--string cdi_filename = "",
--     cdi_varname = ""
string cdi_filename,
       cdi_varname

function cdi(string name, string prev, integer prst, integer prdx, object o, sequence idii)
--
-- Clever display of i'th item. Recursive.
--  name is eg "symtab", but may get "symtab[1]", "symtab[1][1]", etc.
--  prev is previously collected stuff to output: concatenate to it, or split the line.
--  prst is a starting index where (a non-empty) prev began.
--  prdx is the element index of name that we just got passed in o.
--  idii is a list of (start) indexes, to be used in the final sort.
--
-- Returns extended or purged {prst,prev}
--
-- ps: not totally sure I got return "1,2,3" vs return "{1,2,3}" precisely right, 
--      but, touch wood, it seems fine in all the cases I have tested so far...
--
object this     -- Scratch var: string representation of o or next element of o.
                -- In the key recursive call below, "this" gets passed to "prev",
                -- and grows/shrinks from what that returns rather than directly.

string namedx   -- Scratch var: name with some or other idx tacked on the end.

integer newprst,                -- Scratch/innner version of prst.
        lo,                     -- length(o) shorthand
        lp,                     -- length(prev) shorthand
        lt,                     -- length(name)+4+length(this) shorthand
        dotdot = 0,             -- add ".." on the end
        stringo = 0,            -- string(o)/allascii(o) shorthand
        wasstacklen             -- to check if something got dumped

-- added 15/10/15:
    if not integer(o) then
        this = CTB --"**CORRUPT TYPE BYTE**"
        #ilASM{
            [32]
                mov eax,[o]
                mov cl,byte[ebx+eax*4-1]
            [64]
                mov rax,[o]
                mov cl,byte[rbx+rax*4-1]
            []
                cmp cl,#12
                je @f
                cmp cl,#80
                je @f
                cmp cl,#82
--6/4/16:
--              jne :badtypebyte
                je @f
                    [32]
                        lea edi,[ebx+eax*4]
                        shr edi,2
                        mov [edi4],edi
                    [64]
                        lea rdi,[rbx+rax*4]
                        shr rdi,2
                        mov [edi4],rdi
                    []
                jmp :badtypebyte
              @@:
            }
    end if
    wasstacklen = length(printstack)

    if string(o) then
        if length(o)>MAXLENN then
            o = o[1..MAXLENN]
            dotdot = 1
        end if
        this = allascii(o)
        if string(this) then
            stringo = 1
        end if
    end if
    if atom(o) then
        this = sprintf("%.10g", o)
        if integer(o) then
--9/2/20:
--          if o>=#20 and o<=#FF then
            if o='\\' then
                this &= "'\\\\'"
            elsif o>=#20 and o<=#7D then
                this &= sprintf("'%s'",o)
            end if
        elsif not find('.',this)
          and not find('e',this)        -- eg 1e308
          and not find('n',this) then   -- (inf/nan)
            --
            -- Ensure you can tell 5 and 5.000000000001 apart.
            -- Note that while you can infer from the presence
            -- of ".0" it is "not integer", in no way does the
            -- /absence/ mean anything at all about whether a 
            -- variable was declared integer/atom/object/udt.
            --
            this &= ".0"
        end if
    elsif not stringo then
        if length(idii) then
            namedx = sprintf("%s[%d]",{name,prdx})
        else
            namedx = name
        end if
        this = ""
        newprst = 1
        lo = length(o)
        if lo>MAXLENN then
            o = o[1..MAXLENN]
            dotdot = 1
        end if
        for i=1 to length(o) do -- (nb not lo)
--          {newprst,this} = cdi(namedx,this,newprst,i,o[i],idii&i)
            {newprst,this} = cdi(namedx,this,newprst,i,o[i],deep_copy(idii)&i)
        end for
        if newprst>1
        or (length(idii) and
            wasstacklen<length(printstack)) then
            --
            -- Something just got printed, so empty prev and 
            -- force linebreaks all the way back up the call 
            -- stack (which is done by returning prdx+1)
            --
            if length(prev) then
                {prev,namedx} = subscr(prev,name,prst,prdx-1)
                addtostack(idii,prst,namedx,prev)
            end if
            if newprst<=lo then
                if prdx!=-1 then
                    name = sprintf("%s[%d]",{name,prdx})
                end if
                {this,name} = subscr(this,name,newprst,lo)
                if dotdot then
                    this &= ".."
                end if
--              addtostack(idii&newprst,newprst,name,this)
                addtostack(deep_copy(idii)&newprst,newprst,name,this)
            end if
            return {prdx+1,""}
        end if
        this = "{"&this&"}"
    end if
    if dotdot then
        this &= ".."
    end if
    lp = length(prev)
    lt = length(name)+4+length(this)    -- (+4 is for " = " and a ',')

    while 1 do -- (max 2 iterations, before splitting long strings)
        if lp=0 then
            if not stringo              -- (any splitting already done)
            or lt+lp<MAXLINELEN then    -- (or no need to split string)
                return {prst,this}
            end if
            exit
        else
--4/2/21
--          if lt+lp<MAXLINELEN then
            if lt+lp<MAXLINELEN
            and (cdi_filename!="dict.e" or cdi_varname!="trees" or prdx=1 or remainder(prdx-1,5)!=0) then
                -- note that we are clearly returning a partial result here,
                -- of say "1,2,3" rather than "{1,2,3}".
                return {prst,prev&','&this}
            else
                {prev,namedx} = subscr(prev,name,prst,prdx-1)
                addtostack(idii,prst,namedx,prev)
                lp = 0
                prst = prdx
            end if
        end if
    end while

    if stringo then
        --
        -- One that wouldn't fit, so split if needed and
        -- force linebreaks all the way back up the call 
        -- stack (which is done by returning prdx+1)
        --
        if prdx!=-1 then
            name = sprintf("%s[%d]",{name,prdx})
        end if
        newprst = 1
        idii = deep_copy(idii)
        idii &= 1
        lo = length(o)
        while lo-newprst+length(name)+13>MAXLINELEN do
            lp = newprst+MAXLINELEN-14-length(name)
            if lp<newprst+5 then exit end if    -- added 1/3/2021...
            this = allascii(o[newprst..lp])
            namedx = sprintf("%s[%d..%d]",{name,newprst,lp})
            addtostack(idii,newprst,namedx,this)
            newprst = lp+1
        end while
        if newprst!=1 then
            name = sprintf("%s[%d..%d]",{name,newprst,lo})
            this = allascii(o[newprst..lo])
            if dotdot then
                this &= ".."
            end if
        end if
        addtostack(idii,newprst,name,this)
        return {prdx+1,""}
#ilASM{ ::badtypebyte }
        show_corruption(CTB)
    end if
    return {prdx,this}
end function

procedure clever_dump(string name, object o)
--
-- Display "name = sprint(o)" but with indexes 
-- as needed, together with smart line splitting.
--
integer prst
string s
    if OLDSTYLE then
        printf(fn,"    %s = %s\n",{name,ppExf(o,{pp_Indent,length(name)+7})})
    else
--      printstack = {}
        printstack = repeat(0,0)
--4/2/21:
cdi_varname = name
        {prst,s} = cdi(name,"",1,-1,o,{})
        if length(s) then
            addtostack({prst},prst,name,s)
        end if
--/* ah... it must be local!
--DEV erm...
        printstack = deep_copy(printstack)
if length(printstack)>1 then
sequence dbg = repeat("pDiagN.e",1)
dbg = sort(dbg)
        printstack = sort(printstack)
end if
        for i=1 to length(printstack) do
            printf(fn,"    %s = %s\n",printstack[i][2..3])
        end for
--*/
        if length(printstack) then
            sequence l_printstack = deep_copy(printstack)
--29/6/21:
--          l_printstack = deep_copy(l_printstack)
            l_printstack = sort(l_printstack)
            for i=1 to length(l_printstack) do
                printf(fn,"    %s = %s\n",l_printstack[i][2..3])
            end for
        end if
    end if
end procedure

-- DEV wants to be a function[?] for use in eg "type check failure, %s is %s"
--procedure short_dump(string name, object o)
--  printf(1,"%s = %s\n",{name,sprint(o,MAXLINELEN-length(name)-3)})    --(DEV -e2 only)
--  printf(1,"%s = %s\n",{name,sprint(o)})
--end procedure


integer lines
object crash_msg = 0

-- copy of the one in p.exw:
integer batchmode       -- set by -batch command line option
        batchmode = 0   -- 1=suppress displays/prompts [incomplete]

procedure putz(string name, object o, integer novalue)
string s
    if novalue then
        if novalue=1 then
            s = "<novalue>"
        elsif novalue=2 then
            s = "*** INVALID REF ***"
        elsif novalue=3 then
            s = "*** CORRUPT TYPE BYTE ***"
        else
            s = "*** INVALID ??? ***"
        end if
    end if
    if not batchmode then
        if lines<15 then    --DEV this (15) should be a parameter (or setting?):
--DEV should this be in put2? (and not here)
--          if sequence(crash_msg) then
--              if length(crash_msg)=0 then
--                  crash_msg = "?? length 0 crash_msg ??\n"
--              elsif crash_msg[$]!='\n' then
--                  crash_msg &= "\n"
--              end if
--              puts(1,crash_msg)
--              lines = 999
--          else
                lines += 1
                if not novalue then
                    s = sprint(o,MAXLINELEN-7-length(name))
                end if
                printf(1,"    %s = %s\n",{name,s})
--          end if
        end if
    end if
    if fn!=-1 then
        if novalue then
            printf(fn,"    %s = %s\n",{name,s})
        else
            clever_dump(name, o)
        end if
    end if
end procedure

procedure put2(string emsg)
    if not batchmode then
        if lines<15 then
            if sequence(crash_msg) then
                if length(crash_msg)=0 then
                    crash_msg = "?? length 0 crash_msg ??\n"
                elsif crash_msg[$]!='\n' then
                    crash_msg &= "\n"
                end if
                puts(1,emsg&"\n")
                puts(1,crash_msg)
                lines = 999
            else
                lines += 1
                puts(1,emsg)
            end if
        end if
    end if
    if fn!=-1 then
        puts(fn,emsg)
    end if
end procedure

without type_check  -- NB. This code is just too low-level.
-- If you remove the above in the hope that it will help, you will 
--  probably be disappointed. You tend to get eg:
--      diag looping (minimal diagnostics follow); error code is:30
--      ep1 is C0000005
--      ep2 is 00000000
--      era is 00409E9B
--      ern is 3186

constant swod = 0 -- 1=show without debug routines and vars

--
-- Symbol table constants/structure
--  duplicates from pglobals.e, needed for bound .exes
--   (there are *no globals* in this file, unless you count
--    external refs to printf/sprintf/stuff from ppp.e)
--
constant 
         S_Name = 1,    -- const/var/rtn name
         S_NTyp = 2,    -- Const/GVar/TVar/Nspc/Type/Func/Proc
         S_FPno = 3,    -- File and Path number
         S_State = 4,   -- state flag. S_fwd/S_used/S_set
--       S_Nlink = 5,   -- hash link
         S_Slink = 6,   -- localscopeX link
-- constants and variables [S_NTyp<=S_TVar]
--       S_vtype = 7,   -- variable type or namespace fileno
--       S_value = 8,   -- value
         S_Tidx = 9,    -- thread idx (S_NTyp=S_Tvar only)
--       S_ErrV = 10,   -- {'v', file, line, col}; see pmain.e[-35]
--       S_Init = 11,   -- Initialised chain (known init if non-0/see S_Const note below)
-- routines [S_NTyp>=S_Type]
--       S_sig  = 7,    -- routine signature
         S_Parm1 = 8,   -- first parameter. (idx to symtab, follow S_Slink)
--       S_ParmN = 9,   -- minimum no of parameters (max is length(S_sig)-1)
--       S_Ltot = 10,   -- total no of parameters, locals, and temporary vars
                        -- (needed to allocate the stack frame space)
         S_il   = 11,   -- intermediate code
         S_ltab = 12,   -- line table (packed when compiled, raw when interpreted)
         S_1stl = 13,   -- first line (of "procedure"/"function"/"type" keyword)
--       S_Efct = 14,
--       S_ErrR = 15    -- {'R', file, line, col}; see pmain.e[-60]
$

constant 
--       S_Const = 1,   -- symtab[i][S_NTyp] values
         S_GVar2 = 2,   -- global or static variable
         S_TVar3 = 3,   -- temp or threadstack (local) variable/parameter
--       S_Nspc = 4,    -- namespace
--       S_Rsvd = 5,
         S_Type = 6,    -- Type of thermal yellow portable encryptor
--       S_Func = 7,    -- Function of finding unusual nonsense comments
--       S_Proc = 8     -- Procedure for private rotating obstacle counter
         K_wdb = #100   -- with debug setting

constant
         T_pathset = 16,
         T_fileset = 17,
         T_cmdlnflg = 19,
--       T_callstk = 20,
         T_maintls = 21,
         T_EBP     = 22,    -- compiled/listing=0, interpreted={ebp4,esp4,sym4} (set at last possible moment)
         T_ds4     = 23,    -- compiled = start of data section, same but /4 when interpreted ([T_EBP]!=0)
         T_const1  = 26

function convert_offset(atom era, sequence sr)
--printf(1,"pDiag.e line 1112: convert_offset(era=#%08x, sr=%v)\n",{era, sr})
integer lineno = sr[S_1stl]     -- line no of "procedure"/"function"/"type" keyword
sequence linetab = sr[S_ltab]
    if linetab={} then return -1 end if -- added 20/12/19
integer lastline = linetab[$]
atom returnoffset = era-sr[S_il]
integer thisline, linenxt, skip, base = 0, lti, tmp
    --
    -- Convert the offset to a line number.
    -- A raw line table, as built in pilx86.e, is {skip|offset} where
    --  skip is <0, meaning -n lines emitted no code, and
    --  offset is >=0, a start offset of the next code-emitting line.
    --  There is also a dummy max entry added at the end of ilxlate().
    --  A line table should never have two (or more) skips in a row.
    -- When interpreting, we can just use the raw table directly: skip down the
    --  table until the offset is (b)reached, though we only know that when we 
    --  hit the next entry, as detailed in the following.
    --  Example1: a raw linetab of {-2,0,24,36} means offsets 0..23 are S_1st+2,
    --            24..35 are S_1st+3, and <0 or >=36 are out-of-bounds (so leave 
    --            lineno as -1). We only know that we should have stopped for an 
    --            offset of 17 when we hit the 24, and the lineno we want is that 
    --            before the +1 triggered by the 0 (or whatever line adjustment 
    --            we made on [2] when we decide to stop on [3]).
    --  Example2: for a linetab of {-14,#14,-3,#47...} and offset of #22, we only 
    --            know that #14 (S_1st+14) is the right entry when we hit the #47
    --            (S_1st+18), so there is a +1 and -(-3) that we must ignore.
    --            Note that while an exception at offset #47 means S_1st+18, a 
    --            return address of #47 would be the last call made by S_1st+14;
    --            hence add -1 when using a return address to report an error.
    -- When compiled, the linetab is packed: the offsets are converted to deltas
    --  (so most will be <=127) with #81,#80 used as lead-ins for word,dword to
    --  hold values that will not fit in a byte, and lastly stored as a "string"
    --  of binary (#00..#FF) bytes, making it just over 25% of the size, on 32
    --  bit systems, and just over 12.5% on 64 bit systems (ie worth having).
    --  A fairly straightforward decode of the next raw value (into lti) is
    --  followed by the same logic as above, except that when we start with a
    --  raw table we can test lastline directly, but not when unpacking it.
    -- Lastly note that [era] is incredibly fiddly to set, especially for
    --  low-level routines isolated several calls deep from user code. If
    --  the following yields -1, first suspect [era] rather than this code,
    --  except for e30ume, in which case suspect that "Address Mapping" in
    --  :!fehDiag (below) is missing a test/label.
    --
    if string(linetab) then -- compiled (deltas packed to string)
        lineno = -1
        if returnoffset>=0 then
            thisline = sr[S_1stl]
            linenxt = thisline
            skip = 0
            base = 0
            for i=1 to length(linetab) do
                if skip then
                    skip -= 1
                else
                    lti = linetab[i]
                    if lti>#7F then
                        if lti>#81 then
                            lti = lti-#100
                        elsif lti=#81 then
                            lti = linetab[i+1]*#100+linetab[i+2]
                            if lti>#7FFF then
                                lti -= #10000   -- (skip < -128)
                            end if
                            skip = 2
                        elsif lti=#80 then
                            lti = linetab[i+1]*#1000000+linetab[i+2]*#10000+linetab[i+3]*#100+linetab[i+4]
                            if lti>#7FFFFFFF then
                                lti -= #100000000 -- (skip < -32,768?? [very rare, if ever])
                            end if
                            skip = 4
                        else
                            ?9/0    -- (sanity check, should never happen)
                        end if
                    end if
                    if lti<0 then       -- -n lines emitted no code
                        linenxt -= lti
                    else                -- start offset of next line
                        tmp = base
                        base += lti
                        lti = tmp
--                              if returnoffset<=lti then exit end if   -- all done
                        if returnoffset<lti then
                            lineno = thisline
                            exit
                        end if  -- all done
                        thisline = linenxt
                        linenxt += 1
                    end if
                end if
            end for
        end if
    else -- interpreted (raw linetab, a dword-sequence, not converted to deltas/packed)
--      lineno = sr[S_1stl]
--      lastline = linetab[$]
        if returnoffset<0 or returnoffset>=lastline then
            lineno = -1
        else
            linenxt = lineno
            for i=1 to length(linetab) do
                lti = linetab[i]
                if lti<0 then       -- -n lines emitted no code
                    linenxt -= lti
                else                -- start offset of next line
--                          if returnoffset<=lti then exit end if   -- all done
                    if returnoffset<lti then exit end if    -- all done
                    lineno = linenxt
                    linenxt += 1
                end if
            end for
        end if
    end if
    return lineno
end function


--DEV should this just be a parameter to getVal?
--integer lc    -- limit counter (set to 500)
--integer showellipse -- set if lc blown
--integer novalue

--constant
--  kernel32 = open_dll("kernel32.dll"),
--
--  C_PTR = C_POINTER,
--
----#without reformat
--  xIsBadReadPtr = define_c_func(kernel32, "IsBadReadPtr",
--      {C_PTR,     --  CONST VOID  * lp,   // address of memory block
--       C_INT},    --  UINT  ucb   // size of block
--      C_INT)      -- BOOL
----#with reformat

procedure die()
-- a non-catchable fatal error.
    -- first, kill any exception handler:
    #ilASM{
        [32]
            mov [ebp+16],ebx    -- catch addr
        [64]
            mov [rbp+32],rbx    -- catch addr
          }
    ?9/0
end procedure

--now in psym.e:
--enum E_CODE, -- (atom) hardware and operating system exceptions usually have bit #800000000 set, user exceptions can be any atom value, even 0. 
--   E_ADDR, -- (atom) a machine address indicating where the exception ocurred.
--   E_LINE, -- (integer, optional) the source code line matching E_ADDR in E_RTN.
--   E_RTN,  -- (integer, optional) equivalent to routine_id(), an index to the symbol table identifying the routine.
--   E_NAME, -- (string|integer, optional) the human-readable name of E_RTN.
--   E_FILE, -- (string|integer, optional) the source filename containing E_NAME.
--   E_PATH, -- (string|integer, optional) the directory containing E_FILE.
--   E_USER  -- (object, optional) user defined/application specific content.
object throwee

procedure throw(object e, object user_data={})
--
-- (invoked via :%pThrow, see below)
--  This is, of course, just a trigger - the real implementation(/challenge) 
--  of exception handling lies within the call stack and opTry/opCatch.
--
-- Note: the default of {} is actually provided in pmain.e, see T_throw.
--
    throwee = 0
    if user_data!={} then
        if not atom(e) then
            if string(e) and find('%',e) and sequence(user_data) then
                e = sprintf(e,user_data)
                throwee = e
                e = {0,-1,-1,-1,-1,-1,-1,e}
            else
                die()
            end if
        else
            e = {e,-1,-1,-1,-1,-1,-1,user_data}
        end if
    elsif atom(e) then
        e = {e,-1,-1,-1,-1,-1,-1}
    elsif string(e) then
        throwee = e
        e = {0,-1,-1,-1,-1,-1,-1,e}
    elsif length(e)<E_ADDR
       or not atom(e[E_CODE])
       or not atom(e[E_ADDR])
       or (length(e)>=E_LINE and not integer(e[E_LINE]))
       or (length(e)>=E_RTN  and not integer(e[E_RTN]))
       or (length(e)>=E_NAME and not string(e[E_NAME]) and e[E_NAME]!=-1)
       or (length(e)>=E_FILE and not string(e[E_FILE]) and e[E_FILE]!=-1)
       or (length(e)>=E_PATH and not string(e[E_PATH]) and e[E_PATH]!=-1) then
        die()
    end if

    while length(e)<E_PATH do e &= -1 end while

    sequence symtab
    #ilASM{
        [32]
            lea edi,[symtab]
            call :%opGetST      -- [edi]=symtab (ie our local:=the real symtab)
        [64]
            lea rdi,[symtab]
            call :%opGetST      -- [rdi]=symtab (ie our local:=the real symtab)
        []
          }

    integer rtn = e[E_RTN]
    if rtn=-1 then              -- replace with the calling routine number
        #ilASM{
            [32]
                mov eax,[ebp+20]    -- prev_ebp
                mov eax,[eax+8]     -- calling routine no
                mov [rtn],eax
            [64]
                mov rax,[rbp+40]    -- prev_ebp
                mov rax,[rax+16]    -- calling routine no
                mov [rtn],rax
              }
        e[E_RTN] = rtn
    end if

    if rtn>=1 and rtn<=length(symtab)
    and sequence(symtab[rtn])
    and symtab[rtn][S_NTyp]>=S_Type then
        if e[E_NAME]=-1 then
            object name = symtab[rtn][S_Name]
            if not string(name) then
                name = sprint(name)
            end if
            e[E_NAME] = name
        end if
        if e[E_FILE]=-1 then
            integer fno = symtab[rtn][S_FPno]
            if fno<1 or fno>length(symtab[T_fileset]) then
                e[E_FILE] = sprintf("?? (fno=%d)",{fno})    -- should not happen!
            else
                e[E_FILE] = symtab[T_fileset][fno][2]
                if e[E_PATH]=-1 then
                    e[E_PATH] = symtab[T_pathset][symtab[T_fileset][fno][1]]
                end if
            end if
        end if
    elsif e[E_NAME]=-1 then
        e[E_NAME] = sprintf("?? (rtn=%d)",rtn)  -- should not happen!
        rtn = -1  -- (only real addresses in real routines get mapped to a line no)
    end if
    if e[E_ADDR]=-1 then -- replace with called from address from the throw() call:
        atom addr
        #ilASM{
            [32]
                mov eax,[ebp+12]    -- called from (not :throwret below!)
                lea edi,[addr]
                call :%pStoreMint   -- [edi]:=eax, as float if rqd
            [64]
                mov rax,[rbp+24]
                lea rdi,[addr]
                call :%pStoreMint   -- [rdi]:=rax, as float if rqd
              }
        e[E_ADDR] = addr
--5/9/19:
--      if e[E_LINE]=-1 and rtn!=-1 then
--          integer lineno := convert_offset(addr-1,symtab[rtn])
--          e[E_LINE] = lineno
--      end if
    end if 
    if e[E_LINE]=-1 and rtn!=-1 then
        integer lineno := convert_offset(e[E_ADDR]-1,symtab[rtn])
        e[E_LINE] = lineno
    end if
    #ilASM{
            -- 1) if no exception handler then e55ue
        [32]
            cmp [ebp+16],ebx        -- catch addr/flag
            jne @f
                mov al,55           -- e55ue
                mov edx,[ebp+12]    -- called from address
                mov ebp,[ebp+20]    -- prev_ebp
                sub edx,1
                jmp :!iDiag
                int3
        [64]
            cmp [rbp+32],rbx        -- catch addr/flag
            jne @f
                mov al,55           -- e55ue
                mov rdx,[rbp+24]    -- called from address
                mov rbp,[rbp+40]    -- prev_ebp
                sub rdx,1
                jmp :!iDiag
                int3
        []
          @@:

        [32]
            mov eax,[e]
            mov [e],ebx
        [64]
            mov rax,[e]
            mov [e],rbx
        []
            -- 3) while 1 issue fake opRetf (including this routine!)
          ::fakeRetLoop
        [32]
            mov ecx,[ebp+16]        -- catch addr/flag
            cmp ecx,1
            jne @f
                mov dword[ebp+28],:fakeRetLoop  -- replace return address
                jmp :%opRetf
        [64]
            mov rcx,[rbp+32]        -- catch addr/flag
            cmp rcx,1
            jne @f
                mov dword[rbp+56],:fakeRetLoop  -- replace return address
                jmp :%opRetf
        []
      @@:
            -- 4) jump to error handler (catch clause)
        [32]
            jmp ecx
        [64]
            jmp rcx
        []
          }
end procedure

--/*
function getVal(atom addr)
object  result,
        o
integer vtyp, len, keep
    novalue = 0         -- control flag, to prevent ppExf of "<novalue>" result
--DEV 64bit
    if machine_bits()=32 then
        result = peek4s(addr)   --DEV crash here after e91pie
        if result<#40000000 then    -- a 31-bit integer
            return result
        end if
        result -= #40000000
    else
        result = peek8s(addr)   --DEV crash here after e91pie
        if result<#4000000000000000 then    -- a 63-bit integer
            return result
        end if
        result -= #4000000000000000
    end if
    if result=0 then
        novalue = 1
        return "<novalue>"
    end if
    addr = result*4

--  if c_func(xIsBadReadPtr,{addr,1}) then
--      --DEV 64bit? (should be alright...)
--      result = sprintf("<**diag.e: bad ptr** (#%08x)>\n",addr)
--      puts(1,result)
--      return result
--  end if

    vtyp = peek(addr-1)
    if vtyp=#12 then        -- a 64-bit float
        if machine_bits()=32 then
            result = peek({addr,8})
            return float64_to_atom(result)
        else
            result = peek({addr,10})
            return float80_to_atom(result)
        end if
    end if
    if machine_bits()=32 then
        len = peek4s(addr-12)
    else
        len = peek8s(addr-24)
    end if
--  if not diagBase then    -- (old style handling)
--      addr = peek4u(addr-20)
--  end if
    if vtyp=#82 then        -- an 8-bit ascii string
        if len>lc then
            len = lc
            lc = 0
            showellipse = 1
        end if
        return peek({addr,len})
    end if
    if vtyp!=#80 then       -- sanity check: must be a sequence then.
        novalue = 1
        result = sprintf("<**GARBAGE/CORRUPT TYPE BYTE** (#%02x at [#%08x])>\n",{vtyp,addr-1})
        puts(1,result)
        return result
    end if
    result = {}
    while len and lc do
        lc -= 1
        len -= 1
--#without reformat
if 0 then -- new code 12/6/10 (show more of eg allfiles)
        if lc>99 and len and showellipse=0 then
            keep = lc-99
            lc = 99
            o = getVal(addr)
            if showellipse then
                if string(o) then
                    o &= "..."
                    showellipse = 0
                else
                    -- (oops!) quit, so ellipse shows near the break
                    -- (which is added in getValue, once outta here)
                    exit
                end if
            end if
            result = append(result,o)
            lc += keep
        else
            result = append(result,getVal(addr))
        end if
else
        result = append(result,getVal(addr))
end if
--#with reformat
        if machine_bits()=32 then
            addr += 4
        else
            addr += 8
        end if
    end while
    if len then
        showellipse = 1
    end if
    return result
end function
--*/

--
-- NB: the or_xxx are held as refs, eg after string str="abc" #ilASM{ mov ecx,[str] }
--  then [or_ecx] might be the atom #40100888 such that shl ecx,2 yields #00402220, and
--  [#0040221F] is #82 and [#00402220] is 'a'. Likewise for floats/dword-sequences.
--  (see builtins\VM\pHeap.e notes for some more details if any of that confuses you)
--
--  Also, h4 (unassigned) may show as 1073741824 (32bit) or 4.6116860185e+18 (64bit),
--  (just for the or_xxx), at least until some extra code is added somewhere in here.
--  Of course, should any of these values (or anything else from pDiagN.e) appear in 
--  an ex.err (presumably because you took out the "without debug"), they are being 
--DEV is that just or_ebp?
--  manipulated on-the-fly and bear little or no resemblance to values at the point 
--  of the crash, and should (with quite some justification) be regarded as complete 
--  and utter gibberish. To be precise, they are far more believable if deliberately 
--  displayed on-screen, in here or pFEH.e, than when they are incidentally printed 
--  as a run-of-the-mill type thing, as is the case if they ever appear in an ex.err.

--integer exception = 0
--integer rtn           -- routine no, initially from symtab[T_callstk], then from callstack
integer or_ebp          -- from the exception context, or the real ebp (pre-diag()) [stored/4]
atom or_esp,            -- from the exception context, or the real esp
     xceptn,            -- exception code or 0
     xcepta,            -- exception address or 0
     or_eax,            -- from the exception context, but never the real eax
     or_ecx,            -- from the exception context, or the real ecx
     or_era,            -- effective return address (may==xcepta)
     or_edx,            -- from the exception context - not available if xceptn==0!
     or_esi,            -- from the exception context, or the real esi
     or_edi             -- from the exception context, or the real edi
--   era                -- return address, initially from symtab[T_callstk] then callstack
--   etd                -- threadstack addr

--/*
SUG:
--function or_string(atom a)
function or_ref(atom a)
object o
--  if integer(a) then return sprint(a) end if
    if integer(a) then return a end if
    #ilASM{
        [32]
            mov eax,[a]
            push ebx
            fld qword[ebx+eax*4]
            fistp dword[esp]
            pop eax
            add dword[ebx+eax*4-8],1
            mov [o],eax
        [64]
            mov rax,[a]
            push rbx
            fld tbyte[rbx+rax*4]
            fistp qword[rsp]
            pop rax
            add qword[rbx+rax*4-16],1
            mov [o],rax
        []
          }
--  return sprint(o)
    return o
end function
--*/

--constant repch = "\r\n\t",
--       repstrs = {"\\r","\\n","\\t"}

--object symtab     -- copy of symtab obtained via :%opGetST (see pStack.e)

--constant unassigned = "<unassigned>"

integer ds4

function getGvarValue(integer gidx)
integer novalue
object res
    #ilASM{
            mov [novalue],0
        [32]
            mov esi,[ds4]
            mov edx,[gidx]
            shl esi,2
            mov esi,[esi+edx*4+16] -- ([ds+(gidx+4)*4] == gvar[gidx])
            cmp esi,h4
            jne @f
                mov [novalue],1
                mov [res],0
                jmp :done
          @@:
            jl @f
                -- Note: this is fairly simplistic: we could go much further,
                -- (DEV) with a recursive check on nested sequences (including
                --       that it ain't circular), the entire length is valid,
                --       and there is a pRoot locating "SBH\0" as per pHeap.e
                --       Yes, I know MSDN says "This function is obsolete and
                --       should not be used", but it /is/ reasonable here as
                --       we will very shortly terminate the program anyway.
                lea edi,[ebx+esi*4-1]
            [PE32]
                push 1      -- UINT_PTR ucb
                push edi    -- LPVOID lp
                call "kernel32.dll","IsBadWritePtr"
                test eax,eax
                jz :typeaddrok
                    mov [novalue],2 -- invalid ref
                    mov [res],0
                    jmp :done
              ::typeaddrok
            [ELF32]
--              pop al -- (or we could just skip this entirely)
            [32]
                mov cl,[edi]
                cmp cl,#12
                je :typebyteok
                cmp cl,#80
                je :typebyteok
                cmp cl,#82
                je :typebyteok
                    mov [novalue],3 -- corrupt type byte
                    mov [res],0
--6/4/16:
                    add edi,1
                    shr edi,2
                    mov [edi4],edi
                    jmp :done
              ::typebyteok
                add dword[ebx+esi*4-8],1
          @@:
            mov [res],esi
        [64]
--pop al
            mov rsi,[ds4]
            mov rdx,[gidx]
            shl rsi,2
            mov rsi,[rsi+rdx*8+24] -- ([ds+(gidx+3)*8] == gvar[gidx])
            mov r15,h4
            cmp rsi,r15
            jne @f
                mov [novalue],1
                mov [res],0
                jmp :done
          @@:
            jl @f
                -- ditto (as [32])
                lea rdi,[rbx+rsi*4-1]
            [PE64]
                mov rcx,rsp -- put 2 copies of rsp onto the stack...
                push rsp
                push rcx
                or rsp,8    -- [rsp] is now 1st or 2nd copy:
                            -- if on entry rsp was xxx8: both copies remain on the stack
                            -- if on entry rsp was xxx0: or rsp,8 effectively pops one of them (+8)
                            -- obviously rsp is now xxx8, whatever alignment we started with
                sub rsp,8*5                 -- shadow space and align
                mov rdx,1                   -- UINT_PTR ucb
                mov rcx,rdi                 -- LPVOID lp
                call "kernel32.dll","IsBadWritePtr"
--              add rsp,8*5
--              pop rsp     -- restore, equivalent to rsp += (either #08 or #10)
                mov rsp,[rsp+8*5]   -- equivalent to add/pop
                test rax,rax
                jz :typeaddrok
                    mov [novalue],2 -- invalid ref
                    mov [res],0
                    jmp :done
              ::typeaddrok
            [ELF64]
--              pop al  -- (or we could just skip this entirely)
            [64]
                mov cl,[rdi]
                cmp cl,#12
                je :typebyteok
                cmp cl,#80
                je :typebyteok
                cmp cl,#82
                je :typebyteok
                    mov [novalue],3 -- corrupt type byte
                    mov [res],0
--6/4/16:
                    add rdi,1
                    shr rdi,2
                    mov [edi4],rdi
                    jmp :done
              ::typebyteok
                add qword[rbx+rsi*4-16],1
          @@:
            mov [res],rsi
        []
          ::done
          } 
--  res = sprintf("%s [gidx=%d, ds4=%d]",{sprint(res),gidx,ds4})
    if novalue=3 then show_corruption(CTB) end if
    return {novalue,res}    -- ({0,whatever} or {1\2\3,0})
end function

function getTvarValue(integer tidx)
--
-- tidx is 0,-1,-2,-3, etc as per symtab[N][S_Tidx],
--  for [ebp], [ebp-4], [ebp-8], [ebp-12], [ebp-16],
--   or [rbp], [rbp-8], [rbp-16], [rbp-24], [rsp-32],
--  where ebp/rbp is as stored in or_ebp.
--
integer novalue
object res
    #ilASM{
            mov [novalue],0
        [32]
            mov esi,[or_ebp]
            mov ecx,[tidx]
            shl esi,2
            mov esi,[esi+ecx*4]
            cmp esi,h4
            jne @f
                mov [novalue],1
                mov [res],0
                jmp :done
          @@:
            jl @f
                -- ditto (as getGvarValue/[32])
                lea edi,[ebx+esi*4-1]
            [PE32]
                push 1      -- UINT_PTR ucb
                push edi    -- LPVOID lp
                call "kernel32.dll","IsBadWritePtr"
                test eax,eax
                jz :typeaddrok
                    mov [novalue],2 -- invalid ref
                    mov [res],0
                    jmp :done
              ::typeaddrok
            [ELF32]
--              pop al
            [32]
                mov cl,[edi]
                cmp cl,#12
                je :typebyteok
                cmp cl,#80
                je :typebyteok
                cmp cl,#82
                je :typebyteok
                    mov [novalue],3 -- corrupt type byte
                    mov [res],0
--6/4/16:
                    add edi,1
                    shr edi,2
                    mov [edi4],edi
                    jmp :done
              ::typebyteok
                add dword[ebx+esi*4-8],1
          @@:
            mov [res],esi
        [64]
            mov rsi,[or_ebp]
            mov rcx,[tidx]
            shl rsi,2
            mov r15,h4
            mov rsi,[rsi+rcx*8]
            cmp rsi,r15
            jne @f
                mov [novalue],1
                mov [res],0
                jmp :done
          @@:
            jl @f
                -- ditto (as getGvarValue/[32])
                lea rdi,[rbx+rsi*4-1]
            [PE64]
                mov rcx,rsp -- put 2 copies of rsp onto the stack...
                push rsp
                push rcx
                or rsp,8    -- [rsp] is now 1st or 2nd copy:
                            -- if on entry rsp was xxx8: both copies remain on the stack
                            -- if on entry rsp was xxx0: or rsp,8 effectively pops one of them (+8)
                            -- obviously rsp is now xxx8, whatever alignment we started with
                sub rsp,8*5                 -- shadow space and align
                mov rdx,1                   -- UINT_PTR ucb
                mov rcx,rdi                 -- LPVOID lp
                call "kernel32.dll","IsBadWritePtr"
--              add rsp,8*5
--              pop rsp     -- restore, equivalent to rsp += (either #08 or #10)
                mov rsp,[rsp+8*5]   -- equivalent to add/pop
                test rax,rax
                jz :typeaddrok
                    mov [novalue],2 -- invalid ref
                    mov [res],0
                    jmp :done
              ::typeaddrok
            [ELF64]
--              pop al
            [64]
                mov cl,[rdi]
                cmp cl,#12
                je :typebyteok
                cmp cl,#80
                je :typebyteok
                cmp cl,#82
                je :typebyteok
                    mov [novalue],3 -- corrupt type byte
                    mov [res],0
--6/4/16:
                    add rdi,1
                    shr rdi,2
                    mov [edi4],rdi
                    jmp :done
              ::typebyteok
                add qword[rbx+rsi*4-16],1
          @@:
            mov [res],rsi
        []
          ::done
          } 
    if novalue=3 then show_corruption(CTB) end if
    return {novalue,res}    -- ({0,whatever} or {1,0})
end function

--/*
function getValue(integer symidx, integer limit, integer indent, integer crop)
object  o,
        ss   -- symtab[symidx]
integer r, k,
        nTyp, tidx

    lc = limit
    showellipse = 0
    -- obviously none of these should ever happen, but if they do then leave
    --  as many clues as you can in the ex.err to help resolve things.
    if symidx<0 or symidx>length(symtab) then
        return sprintf("pdiag:getValue bad symidx[=%d]",symidx)
    end if
    ss = symtab[symidx]
    if atom(ss) then
        return sprintf("pdiag:symtab[symidx[=%d]] is an atom",symidx)
    end if
    nTyp = ss[S_NTyp]
    if nTyp>S_TVar3 or nTyp<S_Const then
        return sprintf("pdiag:getValue bad symtab[symidx][S_NTyp]=%d",nTyp)
    end if
--newEmit... and 64bit
    if nTyp=S_TVar3 then
        tidx = ss[S_Tidx]
        o = getVal(or_ebp*4+tidx*4)
    else
        tidx = ss[S_Slink]
        o = getVal(etd+tidx*4-4)
    end if
    if not novalue then
--DEV try that new routine here...?
        if indent then
            o = ppExf(o,{pp_Indent,indent+7})
        else
            o = ppf(o)
        end if
        if showellipse then
            if crop then
                lc = find('\n',o)
                if lc then o = o[1..lc-1] end if
            end if
            lc = length(o)
            if o[lc]='}' then
                o[lc..lc] = ",...}"
            else
                o &= "..."
            end if
        end if
--#without reformat
--DEV 24/6/10 (need to experiment a bit here...)
if 0 then -- (added 21/8/2010)
        r = 1
        while 1 do
            k = find(repch[r],o)
            if k then
                o[k..k] = repstrs[r]
            else
                r += 1
                if r>length(repch) then exit end if
            end if
        end while
end if
--#with reformat
    end if
    return o
end function
--*/

--integer dcount        -- number of dropped callstack blocks

--newEBP... 64bit
--/*
function retN()
    if machine_bits()=32 then
        {N,rtn,from_addr,ret_addr,prev_ebp,ebp_root} = peek4u({or_ebp*4+4,6})
    else -- machine_bits()=64
        {N,rtn,from_addr,ret_addr,prev_ebp,ebp_root} = peek8u({or_ebp*4+8,6})
    end if

atom prev_ebp
    while 1 do
        prev_ebp = peek4u(or_ebp*4+20)
        if prev_ebp=0 then return 0 end if
        era = peek4u(or_ebp*4+16)   -- return address
        or_ebp = floor(prev_ebp/4)
        if era=0 then
            put2("(^^^) call_back from Windows/dll/asm\n")
        else
--          if c_func(xIsBadReadPtr,{or_ebp*4,12}) then
--              put2(sprintf("<**diag.e: bad prev_ebp** (#%08x)>\n",or_ebp*4))
--              return 0
--          end if
            rtn = peek4u(or_ebp*4+8)
            era -= 1    -- ensure within code emitted for line [DEV??]
            return 1
        end if
    end while
end function
--*/
--function retDX()
--  --
--  -- The callstack is managed as a linked list of 8K virtual stack blocks (vsb).
--  -- The header of each block is 24 bytes:
--  --
--  --  vsb_prev, vsb_next, spare, [threadstackptr], vsb_used, [symtabptr]
--  --
--  --  [threadstackptr] and [symtabptr] are not relevant here, included as a
--  --   precursor to full thread handling, and subject to change. In fact we
--  --   got ecs (which now points at this header) from symtab[T_callstk], which 
--  --   also contains etd, the real threadstack address, when we used opGetST as
--  --   the very first step.
--  --
--  -- spare is set to #DC0DC0DC at the point where dcount blocks were 
--  --  dropped, that is in the e77phroom case.
--  --
--  -- The remainder of each block contains frames, minimum of 6 dwords each:
--  --
--  --  called_from addr
--  --  first (address of first item being saved)
--  --  items 1..N (params and locals as they were before the call)
--  --  N (number of params and locals which got saved)
--  --  calling routine (index to symtab)
--  --  routine being called (index to symtab)
--  --  return addr
--  --
--  -- Each block can hold up to 339 frames, or a single frame can contain 
--  --  up to 2032 parameters, local variables, and temporaries.
--  -- To simplify handling, frames do not span blocks. If there is not
--  --  enough space for the new frame, it is put in a new vsb and the old
--  --  one is left not-quite-full. Each frame is intended to be read from
--  --  the end downwards; attempts to read forwards are doomed to failure
--  --  - though that should cause no great difficulty for anyone.
--  -- The called from address is used for debug handling only and will 
--  --  match the routine name, whereas the return address will match with
--  --  the closing ')' or possibly the following statement.
--  -- While strictly speaking the calling routine is redundant, it does
--  --  allow some verification as the call stack is walked.
--  --
--  -- At startup, the lowest callstack block is created with a dummy pair
--  -- of {T_maintls(=21),0}, which helps opRetf behave correctly and can/
--  -- should be used to signal the bottom of stack.
--  -- 
----puts(1,"retD()\n")
--  while 1 do
--      if vsb_used=2 then return 0 end if  -- must be our {21,0} pair.
--      if vsb_used=0 then
--if newEBP then -- (DEV [nonsense])
--          if dcount then
--              put2(sprintf("<%d callstack blocks skipped>\n",dcount))
--              dcount = 0
--          end if
--else
--          if peek4u(ecs+8)=#DC0DC0DC then
--              put2(sprintf("<%d callstack blocks skipped>\n",dcount))
--          end if
--end if
--          ecs = peek4u(ecs)   -- follow vsb_prev link
----DEV: not newEBP
--          vsb_used = peek4u(ecs+16)
--      end if
----o = peek4u({ecs,vsb_used+20})
----?o
----?vsb_used
--      base = ecs+vsb_used*4
--      rtnX = peek4u(base+16)
--      if rtn!=rtnX then
--          printf(1,"diag callee internal error (rtn %d!=%d)\n",{rtn,rtnX})
--          return 0
--      end if
--      rtn = peek4u(base+12)
----printf(1,"new rtn=%d\n",rtn)
--      N = peek4u(base+8)
--      if N<0 or N>vsb_used then
--          printf(1,"diag callee internal error (N=%d, vsb_used=%d)\n",{N,vsb_used})
--          return 0
--      end if
----printf(1,"N=%d\n",N)
--      base += 4-N*4
--      first = peek4u(base)
--      if N=0 then
--          if first!=0 then
--              printf(1,"diag callee internal error (N=0, first=%08x)\n",first)
--              return 0
--          end if
--      else
--          if first<etd or first>etd+vmax*4then
--              printf(1,"diag callee internal error (first=%08x, etd=%08x, vmax=%d)\n",{first,etd,vmax})
--              return 0
--          end if
----printf(1,"first=%d\n",first)
--          mem_copy(first,base+4,N)
--      end if
--      era = peek4u(base-4)
----printf(1,"new era=%d\n",era)
--      vsb_used -= (N+6)
----?vsb_used
--      if era=0 then
----            if vsb_used=2 then exit end if
--          put2("(^^^) call_back from Windows/dll/asm\n")
------      vsb_used -= ??
--      else
--          era -= 1    -- ensure within code emitted for line [DEV??]
--          return 1
--      end if
--  end while
--end function

integer vmax        -- highest permitted threadstack entry
        vmax = 0    -- (equal to the length of vmap)

sequence vmap -- variable map; var address --> offset into threadstack
                --  (a flat array of all static and dynamic var refs)
                -- ==> index into symtab for var name, type, etc.

function varIdx(atom addr, integer rtn, sequence symtab)
integer gvar0   -- addr gvar[0] (==[maxgvar]) [may need to be atom, or gvar04]
integer maxgvar
integer varno
integer tidx
object sr
integer nTyp
    #ilASM{
        [32]
            mov eax,[ds4]
            shl eax,2
            add eax,16
            mov [gvar0],eax
            mov eax,[eax]
            mov [maxgvar],eax
        [64]
            mov rax,[ds4]
            shl rax,2
            add rax,24
            mov [gvar0],rax
            mov rax,[rax]
            mov [maxgvar],rax
        []
          } 
--DEV (untried)
--  varno = floor((addr-gvar0)/machine_word())
    if machine_bits()=32 then
        varno = floor((addr-gvar0)/4)
    else
        varno = floor((addr-gvar0)/8)
    end if
--?{varno,maxgvar,addr,gvar0}
--{-400121,561,#F4850,#402018}
--DEV/SUG:
--  if maxgvar!=vmax then return verror("pdiag.e/VarIdx: maxgvar(%d)!=vmax(%d)\n",{maxgvar,vmax})
    if varno>0 and varno<maxgvar then
        -- a gvar
        --(DEV we could do some more error checking here...)
        --DEV** we need a gvar mapping...
--      if varno>0 and varno<=length(vmap) then
        if varno>0 and varno<=vmax then
            return vmap[varno]
        end if
        printf(1,"bad varno(%d)\n",{varno})
        return -1
--      return varno
--DEV/SUG:
--      return verror("bad varno(%d)\n",{varno})
    end if
    -- a tvar
    if rtn<1 or rtn>length(symtab) then
        printf(1,"pdiag.e/VarIdx: bad rtn(%d)\n",{rtn})
        return -1
    end if
    sr = symtab[rtn]
    if atom(sr) then
        printf(1,"pdiag.e/VarIdx:atom(symtab[%d])!\n",{rtn})
        return -1
    end if
    nTyp = sr[S_NTyp]
    if nTyp<S_Type then
        printf(1,"pdiag.e/VarIdx:bad type(symtab[%d][S_NTyp]=%d)!\n",{rtn,nTyp})
        return -1
    end if
--DEV (untried)
--  tidx = floor((or_ebp*4-addr)/machine_word())
    if machine_bits()=32 then
        tidx = floor((or_ebp*4-addr)/4)
    else
        tidx = floor((or_ebp*4-addr)/8)
    end if
--?tidx
    varno = sr[S_Parm1]
    while 1 do
        if varno<1 or varno>length(symtab) then
            printf(1,"pdiag.e/VarIdx:bad S_Parm1/S_link chain (varno=%d)\n",varno)
            return -1
        end if
        sr = symtab[varno]
        if atom(sr) then
            printf(1,"pdiag.e:atom(symtab[%d])!\n",varno)
            return -1
        end if
        nTyp = sr[S_NTyp]
        if nTyp!=S_TVar3 then
            printf(1,"pdiag.e/VarIdx:bad type(symtab[%d][S_NTyp](%d)!=S_TVar3)\n",{rtn,nTyp})
            return -1
        end if
        if tidx=0 then exit end if
        varno = sr[S_Slink]
        tidx -= 1
    end while
    return varno
--/*
integer tidx, stidx
integer N, rtnid
object sr
    stidx = floor((addr-etd)/4)+1
--  stidx = floor((etd-addr)/4)+1
    if stidx>0 and stidx<=length(vmap) then
        return vmap[stidx]
    end if
    -- a local var then:
    N = peek4u(or_ebp*4+4)
--  tidx = floor((addr-or_ebp*4)/4)
    tidx = floor((or_ebp*4-addr)/4)
--DEV isn't this <=0? aren't both N and tidx +ve?!
--  if tidx<0 and tidx<N then
    if tidx>=0 and tidx<N then
        rtnid = peek4u(or_ebp*4+8)  --DEV?? rtn not good enough for ya?
-- this may be temp!
if rtnid!=rtn then
    puts(1,"pdiag.e:varIdx - rtnid!=rtn\n")
end if
        if rtnid<1 or rtnid>length(symtab) then
            puts(1,"pdiag.e:symtab[rtnid] ioob!\n")
            return -1
        end if
        sr = symtab[rtnid]
        if atom(sr) then
            puts(1,"pdiag.e:atom(symtab[rtnid])!\n")
            return -1
        end if
        nTyp = sr[S_NTyp]
        if nTyp>=S_Type then
            N = tidx
            tidx = sr[S_Parm1]
            while N do
                if tidx<1 or tidx>length(symtab) then
                    puts(1,"\n\n**pdiag.e:bad S_Parm1/S_link chain!\n\n")
                    return -1
                end if
                sr = symtab[tidx]
                if atom(sr) then
                    printf(1,"pdiag.e:atom(symtab[tidx(=%d)])!\n",tidx)
                    return -1
                end if
                tidx = sr[S_Slink]
                N -= 1
            end while
            return tidx
        end if
    end if
    printf(1,"\n**pdiag.e:tidx(=%d) out of range!\n",tidx)
    printf(1,"  (addr=#%08x, ebp=#%08x, N=%d)\n",{addr,or_ebp*4,N})
    printf(1,"  (stidx=%d, etd=#%08x, length(vmap)=%d)\n",{stidx,etd,length(vmap)})
--*/
--  return -1   -- oops!
end function

constant cmp_eax_imm32  = #3D           -- 0o075 imm32              -- cmp eax,imm32

--DEV...
object crashfile = 0

integer diaglooping = 0
constant ecode = "diag looping, error code is "
constant erais = ", era is #"

integer msg_id = 0  -- 1..255  (should we ever need more, then I suggest if al is #FF, 
                    --          then instead of "and e/rax,#FF", "shr e/rax,8" allows
                    --          the high 3 bytes of eax to contain another 16 million,
                    --          and leaves "mov al,#01".."mov al,#FE" still working.)

function addrS(atom a)
    if a=0 then return "0" end if
    return sprintf("%08x",a)
end function

--DEV this may in fact be pointless...
integer rbldrqd = 1 -- (shadow copy of the one in pemit2.e)

--17/4/16:
include builtins\puts1h.e

object crash_rtn = -1

procedure set_crash_routine(integer rid)
-- implements crash_routine()
-- specify the routine id of a function to call in the event
-- that your program must be shut down due to an error. The
-- function should accept one parameter (currently always 0)
-- and return 0 to allow any other crash routines to run.
    if rid=-1 then
        crash_rtn = -1
    elsif crash_rtn=-1 then
        crash_rtn = {rid}
    else
        crash_rtn = append(crash_rtn,rid)
    end if
end procedure

--function diag(atom msg_id)
procedure diag()
--
-- create the ex.err.
--
-- on entry (only called from below) the following must be set:
--  msg_id, 1..255
--  xceptn, exception code (may be 0)
--  xcepta, exception address (iff xceptn!=0)
--  or_era, effective return address
--  and from the exception context or the real register,
--  or_ebp, (should match ebp, pre-diag()) [stored/4]
--  or_ecx,
--  or_edi,
--  or_esi,
--  or_esp (should match esp)
--
-- note that ep1 and ep2 (if used) are (now) msg_id-dependent.
--

object symtab   -- copy of symtab obtained via :%opGetST (see pStack.e)

object  si,             -- copy of symtab[i]
        sp,             -- copy of symtab[<param/local var>]
        name,           -- var name or -1 for temporaries we should skip
        o--,o2          -- output vars
integer lineno,         -- linenumber as calculated from return addr/offset & linetab
--      linenxt,        -- see lineno calculation
--      lti,            -- copy of linetab[i] used in lineno calculation
--      thisline,       -- needed for lineno in packed linetab case
--      skip,           -- needed for packed linetabs
        fileno,         -- for grouping symtab entries into files
        fpno,           -- copy of si[S_FPno]
        sNTyp           -- copy of sr[S_NTyp]
integer c               -- scratch var
atom    --returnoffset, -- era as offset into code block, used in lineno calc
        TchkRetAddr,    -- value of !opTchkRetAddr in pStack.e
        cb_ret_addr,    -- value of !cb_ret in pcfunc.e
        cf_ret_addr     -- value of !cf_ret in pcallfunc.e
--      cc_ret_addr     -- value of !cc_ret in pcallfunc.e  [drat: global labels not allowed inside routines!!!]

sequence msg,           -- error message, from msgs[msg_id] plus any params
         wmsg,          -- work var, used for building msg
--       s8,            -- copy of symtab[T_callstk], see below
         sr,            -- copy of symtab[rtn]
--       linetab,       -- copy of symtab[rtn][S_ltab]
         filename,      -- output var
         pathset,       -- copy of symtab[T_pathset] with mainpath added if .exe
         x6             -- e30->e92 fixup

--atom ep1, ep2         -- error parameters

--integer lastline
--integer tmp

integer p

--DEV temp (29/4/19):
--integer N, rtn
atom N, rtn
--DEV from_addr is not really used!! (if we can get away without it...)
atom from_addr, ehand, ret_addr, prev_ebp, ebp_root
atom vsb_prev, vsb_next, vsb_magic
string magicok
integer abortcode
integer rtype   -- 1 skip (lineno=-1),
                -- 2 normal
                -- 3 without debug
integer tidx
integer varno
integer novalue
string msg2

--DEV:
atom symtabptr
atom gvarptr

--9/6/21:
    #ilASM{
        [32]
            xor eax,eax
            call :%pWithJS
            call :%pDeSeqip
        [64]
            xor rax,rax
            call :%pWithJS
            call :%pDeSeqip
        []
          }

--20/2/2021
    cdi_filename = ""
    cdi_varname = ""

--26/4/2021 (moved from constants to inner, due to p2js-related changes to pmain.e/DoSequence().)
--
-- *NB* These must be kept in very strict order, never delete or insert entries.
--
sequence msgs =
{
 "type check failure, %s is %s\n",                              -- e01tcf
    -- As called from opTchk, when var-id is known (idx in ecx).
    -- See also e110tce, called when var_id not known (addr in ecx).
    -- Note: s[i+1] gives a type check failure with a ???[S_name]=0
    --  if i is #3FFFFFFF, as unnamed temporary index sums are given 
    --  an integer type (for performance reasons). Obviously that is
    --  less than ideal and ought to be fixed one day. [DEV]
 "attempt to divide by 0\n",                                    -- e02atdb0
 "true/false condition must be an ATOM\n",                      -- e03tfcmbaa
    -- Usually only happens on "if x then" where x is not 
    -- a relational expression (eg a=b) but is either a 
    -- single variable or a function result. (unlike RDS)
    -- see also e14soa. Note this message may not occur
    -- on subscripted items when a program is compiled, eg
    -- if x is {1,2,"fred",4} then "if x[3] then" is just 
    -- treated as true (not zero), though you should get
    -- an error when the same code is interpreted. This is
    -- a deliberate optimisation.
--DEV maybe we shouldn't inline unless it's a sequence of integer?
 "attempt to subscript an atom\n",                              -- e04atsaa
 "subscript is not an atom\n",                                  -- e05sinaa
 "index %d out of bounds, assigning to sequence length %d\n",   -- e06ioob
 "slice start is less than 1 (%d)\n",                           -- e07ssilt1
    -- Note that the value shown is that after adjustment 
    -- for negative indexes, eg if length(x) is 10, then
    -- x[-11..10] will complain ssilt1 (0) as -11 maps to 0.
    -- see e10sspeos. In fact e07ssilt1 only ocurs for 0. [Erm? DEV test that]
    -- Obviously if the slice start is a variable, rather
    -- than an expression, the "true" value can be found
    -- elsewhere in the ex.err file.
 "slice end is not an integer\n",                               -- e08seinai
 "slice length is negative [%d..%d]\n",                         -- e09slin
    -- values shown are as adjusted for negative indexes, [DEV?]
    --  eg if length(s)=4, then s[-1..-3] shows as [4..2]
    -- see also comments against e07ssilt1.
 "slice starts past end of sequence (%d > %d)\n",               -- e10sspeos
    -- or slice start(%d) less than negative length(%d), see below
 "slice ends past end of sequence (%d > %d)\n",                 -- e11sepeos
    -- or slice end(%d) less than negative length(%d), see below
 "program aborted\n",                                           -- e12pa
    -- Operator has typed '!' in the trace() window.
 "attempt to exit a function without returning a value\n",      -- e13ateafworav
    -- For an example of why this cannot/should not be trapped 
    -- as a compile-time error, see isChecked() in arwen.ew.
 "sequence op (%s) attempted (use sq_%s?)\n",                   -- e14soa
    -- Phix does not support implicit/infix sequence ops;
    -- you must use explicit function-style calls, ie/eg
    -- replace "{1,2}+3" with "sq_add({1,2},3)" to get {4,5}.
    --  (Acutally, in the name of compatibility with legacy code,
    --   it will replace some of the most blatently obvious cases,
    --   see sqopWarn in p.exw/pmain.e)
    -- Note that name="Pete" yields 1 or 0 (True/False) on Phix,
    -- instead of eg {0,1,0,1}, "sequence lengths not the same",
    -- or the infamous "true/false condition must be an ATOM".
    -- (the latter can still happen, just nowhere near as often)
    -- Forcing "+" to be replaced with "sq_add" is better, IMNSHO, than
    -- forcing "=" to be replaced with "equal", as happens with RDS Eu.
    -- (nb some legacy code may need "=" to be replaced with "sq_eq")
    -- Also the compile-time errors "type error (use sq_add?)" et al
    -- catch a significant number of cases before it gets to this.
    -- Lastly, there is no sense, for example, in changing the infix
    -- relational ops (<,<=,=,!=,>=,>) to always return a boolean but
    -- still allowing maths ops (+,-,*,/) to do sequence ops. This 
    -- would spanner all legacy code even more, for example the old
    -- upper/lower would work fine on chars but leave all sequences/
    -- strings completely unchanged. It is far more helpful to sound
    -- this alarm than silently go wrong.
 "unrecognised c_func return type\n",                           -- e15ucfrt
    -- Note that C_FLOAT, E_INTEGER, E_ATOM, and E_OBJECT have not
    --  been attempted/tested and hence report this error.
    -- (for the latter 3 I require a suitable RDS-Eu-compiled DLL)  --DEV
    -- BTW: E_INTEGER, E_ATOM, E_SEQUENCE, and E_OBJECT are only
    --  used for RDS-Eu-compiled DLLs, new values (P_XXX?) will
    --  have to be devised for Phix-compiled DLLs, if/when that
    --  becomes possible.
 "call_backs cannot have optional parameters\n",                -- e16cbchop
    -- There is no way for Phix to determine how many parameters
    --  some C/asm/other language has pushed onto the stack, shy 
    --  of entire program dissassembly/analysis that is, and not
    --  that I have ever seen a callback with anything other than
    --  a fixed number of parameters (and if I ever did, then my
    --  answer would be a separate "call_back_var_args" routine).
    -- If you want optional/defaulted parameters for the benefit
    --  of other Phix code, then you may need a "thin wrapper":
    --  function varfunc(a=?, b=?,...)
    --      ....
    --  end function
    --  ---nono = call_back(routine_id("varfunc")) -- this error
    --  function fixfunc(a,b,...)
    --      return varfunc(a,b,...) -- a "thin wrapper"
    --  end function
    --  cb_xx = call_back(routine_id("fixfunc"))
    -- Hence you can call varfunc with more or less parameters,
    --  whereas cb_xx is always invoked with a known fixed set.
 "sequence op (mul) attempted (use sq_mul?)\n",                 -- e17soma      -- """"""""""""""" --
 "sequence op (remainder) attempted (use sq_remainder?)\n",     -- e18sora      -- """"""""""""""" --
 "sequence op (floor) attempted (use sq_floor?)\n",             -- e19sofa      -- """"""""""""""" --
 "invalid match start index\n",                                 -- e20imsi
 "invalid find start index\n",                                  -- e21ifsi
    -- In find('3',"123",s), s of 1..3 and -1..-3 yield 3,
    --  4 yields 0, but all other values, including non-atoms,
    --  unassigned variables, 0, and s<=-4, yield this error.
    --  Of course -1, being shorthand for length(), is the 
    --  same as 3 in the above, and -3 is the same as 1.
    -- Aside: find('.',filename,-5) could be used to quickly
    --  find a file extension of 4 or less characters. While
    --  length+1 can be helpful, as resuming on lastresult+1
    --  is a common idiom, there is no similar equivalent for
    --  negative subscripts. It may turn out that just simply
    --  ignoring bad (integer) starts and returning 0 makes
    --  for an easier life, I could easily do that if the 
    --  common consus suggests it would be better, though it
    --  seems to me more likely to catch bugs/typos this way.
 "invalid mem_copy length\n",                                   -- e22imcl
    -- number of bytes to copy is negative
 "invalid mem_set length\n",                                    -- e23imsl
    -- number of bytes to set is negative
    --  (ditto)
 "invalid mem_copy memory address\n",                           -- e24imcma
    -- a machine exception occurred in a mem_copy operation
 "invalid mem_set memory address\n",                            -- e25imsma
    -- a machine exception occurred in a mem_set operation
 "invalid argument type for integer := peek()\n",               -- e26iatfpi
    -- Occurs, for example, in integer i = peek(x), when x is
    --  assigned to something like {addr,4}.
    -- The compiler emits opPeeki rather than opPeek because 
    --  the result is an integer, however opPeeki does not
    --  have any code to deal with a sequence argument, and 
    --  even if it did, a typecheck on i would occur anyway.
 "argument to rand() must be >= 1\n",                           -- e27atrmbge1
    -- (Acutally this only triggers for 0; -1 is treated as
    --  MAXUINT, which has turned out to be quite handy.)
 "argument to %s() must be an atom (use sq_%s?)\n",             -- e28NNatXmbausq
-- no longer used (e48atlmmba triggers instead)
-- "argument to set_rand() must be an atom\n",                  -- e29atsrmba
 -1,
-- "with/without js conflict\n",                                    -- e29wojsc
    -- something said with js, something else said without js.
    -- (a compile-time error, non-catchable except by p.exw itself)
    -- (in the end, this was done directly in pmain.e/DoWith())
 "fatal exception %s at #%08x\n",                               -- e30ume
    -- Unknown machine error.
    --
    -- It is pretty much the job of this program, with help
    -- from the back end, to map such errors in Phix hll
    -- code to human-readable form. As per the note above,
    -- when an error occurs in some dll/asm code, this is
    -- about the best I can do (with a line no of "-1", unless
    -- it is part of a #ilasm statement), and hopefully there 
    -- are enough clues later on in the ex.err to guide you 
    -- towards solving the problem. However this message should 
    -- not occur for "pure hll code".
    --
    -- There are literally thousands of places in the backend 
    -- where it catches/maps exceptions, and without any doubt
    -- there will be several left that were accidentally missed.
    --
    -- Generally speaking, addresses in the range #00400000 
    -- to #0040C000 indicate a problem in the back-end, please
    -- contact the author (Pete Lomax) for assistance, and/or
    -- see plist.e, flag dumpVM/the list2.asm that creates.
    --
 "memory corruption: eax is #%08x, edx is #%08x\n",             -- e31mce
    -- only occurs on debug builds
 "heap corruption [era=%08x, edi=%08x]\n",                      -- e32hc(era,edi)
    -- oh dear...
-- "argument to arctan() must be atom (use sq_arctan?)\n",      -- e33atatmba   -- no longer in use (see e28)
 "memory allocation failure\n",                                 -- e33maf
    -- oh dear...
 "power() function underflow\n",                                -- e34pfu
    -- result is less than -1.7976931348623146e308
    -- (technically the term underflow is usually
    --  used to mean "too near zero", btw, which
    --  just makes power() quietly return a zero.)
 "power() function overflow\n",                                 -- e35pfo
    -- result is more than +1.7976931348623146e308
 "length of an atom is not defined\n",                          -- e36loaaind
 "argument to allocate() must be positive integer\n",           -- e37atambpi
--DEV e38 no longer used?
 "argument to free() must be an atom\n",                        -- e38atfmba
 "arguments to mem_copy() must be atoms\n",                     -- e39atmcmba
 "arguments to mem_set() must be atoms\n",                      -- e40atmsmba
 "first argument to poke() must be atom\n",                     -- e41fatpmba
--no longer used:
-- "first argument to poke4() must be atom\n",                  -- e42fatp4mba
-- -1,
 "abort(%d)\n",                                                 -- e42a(ecx)
    -- only invoked when an error hander is present.
    -- normally abort(n) terminates the application,
    -- however inside a try block (or with a handler
    -- detected somewhere higher up in the stack) it 
    -- is mapped to throw(42,"abort(%d)").
    -- Note that e87acmbi may be triggered first.
 "argument to peek() must be atom or sequence of two atoms\n",  -- e43atpmbaoso2a
 "peek size must be 1|2|4|8\n",                                 -- e44psmb1248
 "attempt to get square root of negative number\n",             -- e45atgsqronn

 "argument to float32_to_atom() must be sequence of length 4\n", -- e46atf32tambsol4
 "argument to float64_to_atom() must be sequence of length 8\n", -- e47atf64tambsol8
    -- btw, the above messages occur for an unassigned argument, rather
    --  than the usual e92/"variable xxx has not been assigned a value".
-- "argument to chdir() must be string\n",                      -- e48atcdmbs
-- -1,                                                          -- no longer in use
 "argument to :%LoadMint must be an atom\n",                    -- e48atlmmba
 "argument to atom_to_float32() must be atom\n",                -- e49atatf32mba
 "argument to atom_to_float64() must be atom\n",                -- e50atatf64mba
 "HeapFree error code [%08x]\n",                                -- e51hfec
    -- Should not happen. Suggests that your program has
    -- corrupted memory, the operating system free chain, 
    -- for instance. Try using safe.e (see that file for
    -- instructions) and/or a debug version of p.exe. -- DEV
    -- Make a copy of the program source, then repeatedly
    -- delete as many lines as possible while the error
    -- still occurs. If you can get it to under 100 lines
    -- (program no longer has to do anything useful, btw)
    -- then you can submit it for further investigation.
-- "repeat count must be non negative integer\n",               -- e52rcmbnni
 "p2js violation: JavaScript does not support string subscript destructuring\n",    -- e52jsdnssd
-- -1,--"for loop error\n",                                     -- e53fle           --DEV see e120,1
 "memory corruption at #%08x (pGtcb*4=#%08x, diff=#%08x)\n",    -- e53mcat(esi,ecx)
    -- internal error. If you can reproduce this problem,
    -- ideally on a compiled program with a listing file,
    -- and get a consistent diff then it should be fairly
    -- easy to trap after pGtcb has been set and predict
    -- the memory location that needs trapping. If you do
    -- not get a consistent diff, or intermittent errors,
    -- this will likely be very difficult to track down.
 "attempt to raise negative number to non-integer power\n",     -- e54atrnntnip
    -- mathematically, power(-3,-3) is an imaginary number.
 "unhandled exception\n",                                       -- e55ue
    -- from throw.e - which contains discussion and code to
    -- report the error on the throw statement (default) or 
    -- within the throw() routine itself.
-- -1,--"first argument to append() must be sequence\n",                -- e55fatambs
    -- You may mean a&b instead of append(a,b)
    -- Note that append("one","two") is {'o','n','e',"two"},
    -- whereas "one"&"two" is "onetwo", although they 
    -- give the same results if b is an atom.
-- -1,--"first argument to prepend() must be sequence\n",           -- e56fatpmbs
    -- You may mean b&a instead of prepend(a,b)
    -- Note that prepend("two","one") is {"one",'t','w','o'},
    -- whereas "one"&"two" is "onetwo", although they 
    -- give the same results if b is an atom.
-- -1, -- spare (dict.e now invokes crash(msg,args,3))
-- "invalid dictionary id\n",                                   -- e56idi
    -- triggered from builtins\dict.e via :!fatalN so that
    -- the error occurs on the calling statement.
 "p2js violation: relies on copy on write semantics\n",         -- e56rocow
    -- something needs a rewrite or deep_copy()...
    -- this (runtime) error is deemed non-catchable.
 "invalid file name\n",                                         -- e57ifn
    -- A common cause of this is using append instead of &:
    --  append(`C:\test\`,"fred.txt") returns the nested
    --  {'C',':','\','t','e','s','t','\',"fred.txt"}, whereas
    --  `C:\test\`&"fred.txt" returns `C:\test\fred.txt`.
    -- Remember that "append(s,x)" always returns a sequence (or 
    --  string) of length(s)+1, whereas "s&x" returns a sequence 
    --  (or string) of length(s)+length(x) [that is, except when 
    --  x is an atom, in which case they are equivalent].
 "invalid file number (%d)\n",                                  -- e58bfn(edi) [e58ifn...]
    -- file must be open for getc, puts, seek, where, etc.
 "wrong file mode for attempted operation\n",                   -- e59wfmfao
    -- eg attempt to read a file after open(x,"w").
 "file number is not an integer\n",                             -- e60fninai
    -- this error is also common for unassigned vars.
 "invalid open mode\n",                                         -- e61iom
    -- second parameter to open must be (r|w|a|u)[b].
    -- BTW, Phix allows single-character modes, eg 'r',
    -- whereas RDS Eu does not.
 "file number %d is not open\n",                                -- e62fnnino
 "second parameter of seek() must be an atom\n",                -- e63sposmba
 "seek fail on open append\n",                                  -- e64sfooa
    -- after successfully opening a file for append
    -- (fn=open(xxx,"a")), it automatically seeks to
    -- the end of file. This seek has failed.
    -- This should not happen, maybe you found a bug,
    -- or maybe your hard drive has errors.
 "sequence found in character string\n",                        -- e65sfics
    -- second parameter to puts or [s]printf may not
    -- contain nested sequences.
    -- Try using pp(), ppf(), ?, or [s]print().
 "invalid lock type\n",                                         -- e66ilt
 "byterange must be {} or pair of atoms\n",                     -- e67bre
-- -1,--"argument to dir() must be string\n",                   -- e68atcdmbs (not actually used/see pdir.e)
    -- See e73atodmbs
 "crash(%s)\n",                                                 -- e68crash
    -- crash() invoked inside a try block
 "error in format string\n",                                    -- e69eifs (see pprntf.e/badfmt())
    -- Missing or unrecognised format character after a '%',
    --  eg "%", "%3.2", "%q". See also e73atodmbs.
 "insufficient values for (s)printf()\n",                       -- e70ivfpf
-- -1,--"argument to getenv() must be string",                  -- e71atgmbs (not actually used/see penv.e)
    -- See e73atodmbs
 "call_func/proc parameter error\n",                            -- e71cfppe
    -- The second argument ("params") is not a sequence.
 "invalid routine_id(%d)\n",                                    -- e72iri(edi)
    -- The first argument to call_proc/func, or call_back (which
    -- can also accept {'+',rtnid} as the first argument) is not
    -- an integer, is not in the range 1..length(symtab), or
    -- symtab[i] is not a type, function, or procedure. Usually 
    -- occurs after a previous call to routine_id, define_c_func, 
    -- etc returned -1.
    -- Also invoked directly from delete_routine() aka :%opDelRtn.
--DEV++
 "argument to open_dll() must be string\n",                     -- e73atodmbs
    -- Either the parameter is not a sequence, or some element
    -- of it is not a character. Note that strings and flat
    -- dword sequences are equally acceptable, eg/ie "kernel32"
    -- or {'k','e','r','n','e','l','3','2'} work the same.
 "define_c_func/proc parameter error\n",                        -- e74dcfpe
    -- the first argument to define_c_func/proc is:
    --   an atom, and the second is either unassigned,
    --                       a sequence of length zero,
    --               or a sequence containing non-chars, or
    --   a sequence, with non-zero length, or the second
    --               parameter is unassigned or sequence.
    -- ie the legal forms of define_c_func/proc are:
    --      define_c_func/proc(atom,name,...)
    --      define_c_func/proc({},addr,...)
 "call back routine parameters must all be atoms\n",            -- e75cbrpmaba
 "%c requires an atom value\n",                                 -- e76pcraav
 "program has run out of memory\n",                             -- e77phroom
 "attempt to get_text() >1GB file\n",                           -- e78atgtgt1gbf
    -- Very large files can (obviously) be read line-by-line, or 
    --  byte-by-byte, or via seeks, but you may *not* load the 
    --  whole thing into memory at once (1GB ~=300 bibles).
    --  get_text() is not really suitable for files >5MB.
 "argument to rand() must be an atom\n",                        -- e79atrmba
 "call_back returned non-atom\n",                               -- e80cbrna(esi)
    -- note this error occurs after the callback has returned,
    -- hence none of the parameters or locals can be shown.
 "insufficient parameters in call_func/proc()\n",               -- e81ipicfp
    -- second argument to call_func/proc must be a sequence
    -- containing at least the number of non-defaluted elements 
    -- declared as parameters for the specified routine.
 "argument to call() must be atom\n",                           -- e82atcmba                --DEV 8/6/15: I suspect this is no longer in use...
    -- Note that Phix allows a call() to a call_back()
    -- whereas RDS Eu suffers a machine exception.
 "arguments to position() must be integer\n",                   -- e83atpmbi
 "call_back parameter must be routine_id or {'+',routine_id}\n", -- e84cbpmbropr
 "unknown type byte (not 0x12, 0x80, or 0x82)\n",               -- e85utb
    -- usually caused by memory corruption, has also occurred
    -- due to compiler emitting invalid refs & fixup failures.
 "argument to trace() must be integer 0..3\n",                  -- e86attmbi03
    -- technically -1 is also valid, and implements the same as
    -- keying 'Q' in the trace() window, ie permanently off.
 "abort() code must be integer\n",                              -- e87acmbi
 "arguments to c_%sc() must be atoms or strings\n",             -- e88atcfpmbaos(edi)
    -- (edi=1 -> c_func, else c_proc)
 "too many parameters in call_func/proc()\n",                   -- e89tmpicfp
 "argument to profile() must be 0 or 1\n",                      -- e90atpmb01
-- "profile internal error\n",                                  -- e91pie   [DEV]
 "variable %s has not been assigned a value\n",                 -- e91vhnbaav(ecx)
    -- or_ecx is var no
 "variable %s has not been assigned a value\n",                 -- e92vhnbaav(esi)
    -- or_esi is var no
 "variable %s has not been assigned a value\n",                 -- e93vhnbaav(edi) [:%opPpndSA]
    -- or_edi is var address
 "variable %s has not been assigned a value\n",                 -- e94vhnbaav(edx,esi)  [if integer(esi), report as e04atsaa]
    -- or_edx is var no
--DEV these appear untested:::
 "text_color error [%08x]\n",                                   -- e95tce
 "bk_color error [%08x]\n",                                     -- e96bce
 "heap error [%s]\n",                                           -- e97he
 "flush error [%s]\n",                                          -- e98fiofe
    -- internal kernel32 WriteFile failure when writing the
    -- contents of a file buffer. Code is from Microsoft.
    -- Unlikely, should not happen, maybe a scandisk
    -- is needed, maybe your hard drive is failing...
 "invalid peek memory address\n",                               -- e99ipma
    -- A MEMORY VIOLATION (#C0000005) exception occured when
    --  trying to read from the supplied memory address.
    -- Can occur on peek[(2|4|8)(u|s)]() calls, if they are
    --  passed a bad location or an impossible (-ve) length.
 "invalid poke memory address\n",                               -- e100ipma
    -- A MEMORY VIOLATION (#C0000005) exception occured when
    --  trying to write to the supplied memory address.
    -- Can occur in poke[(1|2|4|8)]() calls, if they are 
    --  passsed a bad location or an invalid length.
 "attempt to allocate string of negative length\n",             -- e101atasonl
    -- internal error in the back end. The only way application
    -- code could attempt something similar is repeat(' ',-n),
    -- which is caught as e52rcmbnni before getting this far,
    -- and, e37atambpi handles -ve values passed to allocate().
    -- NB: a line no of -1 is expected should this occur; there
    --     is no known way to deliberately cause this error.
    -- (this message was added to catch bugs in gets().)
 "attempt to raise 0 to power <= 0\n",                          -- e102cr0tple0
 "attempt to get remainder of a number divided by 0\n",         -- e103atgrondb0
 "call back error?\n",                                          -- e104cbe [DEV no longer used]
 "not enough format strings to print data\n",                   -- e105nefstpd (pprntf.e only)
    -- May be removed for compatibility reasons, see pprntf.e.
 "index %d out of bounds, reading sequence length %d\n",        -- e106ioob
    -- (edi,edx)
 "invalid free memory address\n",                               -- e107ifma
 "position error [%d]\n",                                       -- e108pe(edi)
    -- Maybe the co-ordinates specified are outside the boundaries
    -- of the (Windows) screen buffer. See also e83atpmbi, which
    -- occurs for attempts to position at negative coordinates.
    -- Note this error is used by get_position() and postion().
 "clear_screen error\n",                                        -- e109cse
    -- Internal error, should not happen (and in fact this
    --  message has never been successfully triggered)
 "type check failure, %s is %s\n",                              -- e110tce(ecx)
    -- as e01tcf but ecx is var addr not idx
 "bitwise operations are limited to 32-bit numbers\n",          -- e111bolt32b
    -- DEV: it may be sensible to permit and_bits(x,#FFFFFFFF),
    -- or in fact any and_bits op where either param is 32-bit:
    -- In the case of and_bits, this message only occurs if both
    -- arguments are larger than 32 bits.
 "second argument of find() must be a sequence\n",              -- e112saofmbs
 "second argument of match() must be a sequence\n",             -- e113saommbs
 "sequence to be poked must only contain atoms\n",              -- e114stbpmoca
 "argument to sleep() must be atom\n",                          -- e115atsmba
 "routine requires %d parameters, not %d\n",                    -- e116rrnp
    -- either the define_c_func/proc statement is wrong, or
    --  the c_func/proc statement is wrong.
 "routine does not return a value\n",                           -- e117rdnrav
    -- typically this means the program is using c_func
    --  to invoke a routine defined using define_c_proc
 "routine returns a value\n",                                   -- e118rrav
    -- typically this means the program is using c_proc
    --  to invoke a routine defined using define_c_func
 "assertion failure%s\n",                                       -- e119af(edi)
    -- an assertion has failed, doh
 "for loop error, %s is %s\n",                                  -- e120fle
    -- Phix does not permit floating point for loops, since
    -- they do not work (eg on RDS Eu, try for x=1.1 to 1.3 
    -- by 0.1 do ?x end for; you only get 1.1 and 1.2 output).
    -- Replace eg 'for x=1.0 to 2.0 by 0.1 do ... end for'
    -- with 'atom x=1.0 for j=10 to 20 do ... x+=0.1 end for'
    -- Can also be triggered by using large integers.
    -- The "illegal expression type" compile-time error also 
    -- helps to catch most such problems in legacy code.
    -- NB: ep1 is init value (not var no)
 "for loop error, limit is %s, step is %s\n",                   -- e121flelimstep
    -- As above, Phix does not permit floating point for loops.
    -- This extends to final values, for example if you get
    --  for loop error, limit is 900,000,000, step is 800,000,000
    -- then it is because 1,700,000,000 is > 1,073,741,823.
    -- NB: ep1 is limit value, ep2 is step value (no var nos)
 "invalid poke size\n",                                         -- e122ips
 "delete_routine already set\n",                                -- e123dras
 -1}

                                                                -- e14soa(edi:)
sequence e14ops = {"add","sub","div","mul",                     -- 1,2,3,4
                   "remainder","floor","unary minus","not",     -- 5,6,7,8
                   "and_bits","or_bits","xor_bits","not_bits",  -- 9,10,11,12
                   "power","xor"}                               -- 13,14
--              ,
--       e28ops = {"rand","cos","sin","tan","arctan","log","sqrt"}

--DEV use NTdesc from pglobals.e?: (no, we don't have that here!)
sequence rtndescs = {"type","function", "procedure"}


--/*
    This will definitely never work on RDS Eu!
--*/
--puts(1,"uh1?\n")
--?msg_id
    diaglooping += 1
--maybe:
--  enter_cs()
--DEV
--!/*
    if rbldrqd then
--      rebuild_callback()
        --
        -- invoke pemit2.e/rebuild_callback() if needed, but we can't use a
        -- normal hll call as this is linked into all .exe files, in which 
        -- case :!rbidscb resolves to 0 and this does nothing. Likewise we
        -- use a shadow copy of rbldrqd, and invoke :!rbidscb at most once,
        -- which (if non-0) tests the real rbldrqd before doing anything.
        --
        rbldrqd = 0
        #ilASM{ 
            [32]
--DEV push/pop d/qword[ds+8] (not yet supported by pilasm.e)
                mov eax,[ds+8]
                push eax
                call :!rbidscb  -- (leaves return addr on stack)
                add esp,4       -- (discard "")
                pop eax
                mov [ds+8],eax
            [64]
                mov rax,[ds+8]
                push rax
                call :!rbidscb  -- (leaves return addr on stack)
                add rsp,8       -- (discard "")
                pop rax
                mov [ds+8],rax
            []
              }
    end if
--!*/
--/*
    if rbldrqd then
        #ilASM{ 
--jmp :fin
--    :!rbidscb
            [32]
--              push dword[esp]                         -- (leave the ret addr on stack)
                mov edx,routine_id(rebuild_callback)    -- mov edx,imm32 (sets K_ridt)
                mov ecx,$_Ltot                          -- mov ecx,imm32 (=symtab[rebuild_callback][S_Ltot])
                call :%opFrame
X               mov dword[ebp+16],:rbidsret
                mov dword[ebp+28],:rbidsret
                jmp $_il                                -- jmp code:rebuild_callback
            [64]
--              push qword[rsp]                         -- (leave the ret addr on stack)
--pop al
                mov rdx,routine_id(rebuild_callback)    -- mov edx,imm32 (sets K_ridt)
                mov rcx,$_Ltot                          -- mov ecx,imm32 (=symtab[rebuild_callback][S_Ltot])
                call :%opFrame
X               mov qword[rbp+32],:rbidsret
                mov qword[rbp+56],:rbidsret
                jmp $_il                                -- jmp code:rebuild_callback
            []
          ::rbidsret    
--          ret
--    ::fin
          }
    end if
--*/
--?1
--?9/0
--?2

--  if msg_id=#FFFFFFFF then    -- (-1 as an unsigned 32-bit value!)
----puts(1,"setting batchmode to 1...\n") --DEV temp!
--      batchmode = 1
--      return 0
--  end if
--DEV [may no longer be rqd] [set routines should be in here anyway]
--  crash_msg = ""      -- /necessary/: ensure compiler knows this is string/integer
--  crash_msg = "abc"       -- /necessary/: ensure compiler knows this is string/integer
                        --  (needed since it is only ever set by assembly code)
--  crash_msg = 0   -- get callstack as at enumbset.
--  crashfile = 0
    #ilASM{
        [32]
--11/12/15:
            mov eax,:!opTchkRetAddr
            lea edi,[TchkRetAddr]
            push ebx
            push eax
--          push ebx
--          mov dword[esp],:!opTchkRetAddr
            fild qword[esp]
            add esp,8
            call :%pStoreFlt
            mov eax,:!cb_ret
            lea edi,[cb_ret_addr]
            push ebx
            push eax
            fild qword[esp]
            add esp,8
            call :%pStoreFlt
            mov eax,:!cf_ret
            lea edi,[cf_ret_addr]
            push ebx
            push eax
            fild qword[esp]
            add esp,8
            call :%pStoreFlt
--          mov eax,:!cc_ret
--          lea edi,[cc_ret_addr]
--          push ebx
--          push eax
--          fild qword[esp]
--          add esp,8
--          call :%pStoreFlt
            lea edi,[symtab]
        [64]
            mov rax,:!opTchkRetAddr
            lea rdi,[TchkRetAddr]
            push rax
--          mov qword[rsp],:!opTchkRetAddr
            fild qword[rsp]
            add rsp,8
            call :%pStoreFlt
            mov rax,:!cb_ret
            lea rdi,[cb_ret_addr]
            push rax
            fild qword[rsp]
            add rsp,8
            call :%pStoreFlt
            mov rax,:!cf_ret
            lea rdi,[cf_ret_addr]
            push rax
            fild qword[rsp]
            add rsp,8
            call :%pStoreFlt
--          mov rax,:!cc_ret
--          lea rdi,[cc_ret_addr]
--          push rax
--          fild qword[rsp]
--          add rsp,8
--          call :%pStoreFlt
            lea rdi,[symtab]
        []
            call :%opGetST  -- [e/rdi]:=symtab (see pStack.e)
          }
    lines = 0
--puts(1,"d2\n")
    if symtab[T_EBP]=0 then             -- compiled
        ds4 = floor(symtab[T_ds4]/4)
    else                                -- interpreted
        ds4 = symtab[T_ds4]
    end if

-- 4/8/15:
    if and_bits(symtab[T_cmdlnflg],#04)!=0 then
        batchmode = 1
    end if

--(DEV) this may help you get better ex.err when something crashes in this file (untested)
--/*
    pst4 = symtab[T_EBP][3]     -- (we may want to loop until we hit a 0)
    #ilASM{
        [32]
            mov eax,[pst4]
            shl eax,2
            mov [ds+8],eax
        [64]
            pop al
--          mov rax,[pst4]
        []
          }
--*/

--?1
--?msg_id
--?2
--?msg_id
--puts(1,"d2a-\n")
--DEV (temp)
--DEV these may want to be inside the loop... (is ebp_root overwritten?)
--DEV untried:
--  {N,rtn,from_addr,ret_addr,prev_ebp,ebp_root} = peekNS({or_ebp*4+machine_word(),6},machine_word(),0)
--  {vsb_prev,vsb_next,symtabptr,gvarptr,vsb_magic} = peekNS({ebp_root,5},machine_word(),0)
    if machine_bits()=32 then
--EXCEPT
--      {N,rtn,from_addr,ret_addr,prev_ebp,ebp_root} = peek4u({or_ebp*4+4,6})
        {N,rtn,from_addr,ehand,prev_ebp,ebp_root,ret_addr} = peek4u({or_ebp*4+4,7})
        {vsb_prev,vsb_next,symtabptr,gvarptr,vsb_magic} = peek4u({ebp_root,5})
    else -- machine_bits()=64
--      {N,rtn,from_addr,ret_addr,prev_ebp,ebp_root} = peek8u({or_ebp*4+8,6})
        {N,rtn,from_addr,ehand,prev_ebp,ebp_root,ret_addr} = peek8u({or_ebp*4+8,7})
        {vsb_prev,vsb_next,symtabptr,gvarptr,vsb_magic} = peek8u({ebp_root,5})
    end if

--  if not batchmode then --DEV
    if show_low_level_diagnostics then
        if diagdiag>0 or (vsb_magic-#40565342) or msg_id<1 or msg_id>length(msgs) then
            printf(1,"N=%d, rtn=%d, from=#%s, ret=#%s, ehand=%s, prevebp=#%s, ebproot=#%s\n",
--DEV (is fn not assigned yet??)
--          put2(sprintf("N=%d, rtn=%d, from=#%s, ret=#%s, ehand=%s, prevebp=#%s, ebproot=#%s\n",
                   {N,rtn,addrS(from_addr),addrS(ret_addr),addrS(ehand),addrS(prev_ebp),addrS(ebp_root)})
            printf(1,"or_eax=#%08x, or_ecx=#%08x, or_edx=#%08x,\nor_esi=#%08x, or_edi=#%08x\n",
--          put2(sprintf("or_eax=#%08x, or_ecx=#%08x, or_edx=#%08x,\nor_esi=#%08x, or_edi=#%08x\n",
                   {or_eax,or_ecx,or_edx,or_esi,or_edi})
--          magicok = "\"@VSB\""
            magicok = `"@VSB"`
--DEV wrong on machine_bits()=64... (possibly one for docs) [I think it may be OK now...]
--          if vsb_magic!=#40565342 then
            if (vsb_magic-#40565342) then
                magicok = "**BAD MAGIC**"
            end if
            printf(1,"vsb_prev=#%s, vsb_next=#%s, vsb_magic=%s (%s)\n",
--          put2(sprintf("vsb_prev=#%s, vsb_next=#%s, vsb_magic=%s (%s)\n",
                   {addrS(vsb_prev),addrS(vsb_next),addrS(vsb_magic),magicok})
--  end if
--
--  if not batchmode then
--      if msg_id<1 or msg_id>length(msgs) then
--          msg = "**BAD MESSAGE ID**"
--      else
--          msg = msgs[msg_id][1..-2] -- (strip trailing \n)
--      end if
--      printf(1,"\ndiag(%d[%s]) called...\n",{msg_id,msg})
--      lines += 1
--end if
        end if
    end if
--puts(1,"d2a\n")
    abortcode = 1
--  if string(crash_msg) then
--      msg = crash_msg
--  els
    if msg_id<1 or msg_id>length(msgs) then
        msg = sprintf("diag(%d) [**BAD MESSAGE ID**] called\n",msg_id)
--      msg_id = 1 --DEV abortcode
--      abortcode = 1
    else
        msg = msgs[msg_id]
        abortcode = msg_id
    end if

--puts(1,"d2b\n")
    --
    -- NB. symtab may contain uninitialised fields (especially S_value). [DEV]
    --
    --    Attempts to modify symtab, which now has a reference count of 2,
    --    will attempt a clone operation and may therefore crash, as well
    --    as being about as likely to succeed as changing the brake pads,
    --    clutch, gearbox, tyres, steering wheel, and engine oil, all at 
    --    the same time, on a juggernaut careering out of control down 
    --    a steep mountain road.
    --
    --    Likewise attempts to (eg) print symtab may also crash.
    --
    --    Lastly, note that \constants\ are not necessarily initialised yet, 
    --    not just the obvious main=create(Window...) but also name="name",
    --    though literal integer constants (eg DEBUG=1) should be fine.
    --
--puts(1,"d2c\n")
--if atom(symtab) then return 0 end if  --DEV??!
--  s8 = symtab[T_callstk]  -- {ep1,ep2,era,etd,ern,or_ebp*4,?vsb_used?,dcount}
    --
    -- NB. s8 is "volatile". Hopefully this is of no concern to you, but in 
    -- order to avoid allocating space on the heap (which may be full), or 
    -- messing with bytesallocated/freed counts, opGetST uses a rather dim
    -- ref-count-agnostic-hack when it updates symtab[T_callstk]. If opGetST 
    -- is invoked again (eg it is used in both routine_id and command_line) 
    -- then this s8, or more specifically any float elements extracted from 
    -- this s8, may also get modified (from afar). An atom z z=s8[5] is not
    -- necessarily safe from a following opGetST either. Of course if you 
    -- create eg a string version of s8, or z, then that won't change.
    --
    -- ep1 and ep2 are error code specific, for example e09slin is
    --  "slice length is negative [%d..%d]\n" and ep1/2 are those idx.
    -- era is a return addr, possibly adjusted to pick up a var address,
    --  which can be converted to a line number via routineno/linetab.
    -- etd is a raw pointer to the threadstack static ref bank.
    --  Note this is non-subscriptable and may contain unassigned items.
    -- ern is a routine no/index into symtab, eg 21 for main file code.
    -- or_ebp is a raw pointer to frame in the callstack (see function retD)
--DEV currently not reported!
    -- dcount is the number of dropped callstack blocks. If the program
    --  runs out of memory all but the first and last two callstack 
    --  blocks are freed, which will hopefully release enough memory
    --  for this routine to complete successfully. This program should 
    --  also limit the entries printed to keep ex.err reasonably sized.
    --
--puts(1,"d2d\n")
--/*
--  if not batchmode then
--      puts(1,"{ep1,ep2,era,etd,ern,or_ebp*4,???,dcount}:\n")
--      printf(1,"s8=#%08x,#%08x,#%08x,#%08x,%d,#%08x,%d,%d\n",s8) lines += 2
--  end if
    --?8
    ep1 = s8[1]             -- error parameter 1
    ep2 = s8[2]             -- error parameter 2
    era = s8[3]             -- return addr (adjusted to be within code emitted for line)
    etd = s8[4]             -- threadstack ptr
    rtn = s8[5]             -- active routine number
    or_ebp*4 = s8[6]            -- frame ptr (at point of failure)
--vsb_used [DEV]
    dcount = s8[8]          -- dropped callstack blocks
--?9
    if not batchmode then
        ppOpt({pp_Ascii,{' ',#7E},pp_Nest,1})
    end if
--  ppOpt({pp_Pause,10})
--ppOpt({pp_Pause,0})

--puts(1,"d3\n")
--?10
--*/
    --
    -- First create a vmap to allow gvar idx/addr to be mapped to symtab
    --
    if vmax=0 then
--      vmap = {}
        vmap = repeat(0,0)
        for i=length(symtab) to T_maintls by -1 do
            si = symtab[i]
            if sequence(si)
            and si[S_NTyp]<=S_GVar2 then
                c = si[S_Slink]
                if c>vmax then
--                  vmap &= repeat(0,c-vmax)
                    if vmax=0 then
                        vmap = repeat(0,c)
                    else
                        vmap &= repeat(0,c-vmax)
                    end if
                    vmax = c
                end if
                vmap[c] = i
            end if
        end for
    end if
    --
    -- initialise pathset [DEV do we want this/call current_dir&'\\' just the once?] [seems ok like this...]
    --
    pathset = symtab[T_pathset]
--  for j=1 to length(pathset) do
--      if length(pathset[j])<2 or pathset[j][2]!=':' then
--          pathset[j] = current_dir()&`\`&pathset[j]
--      end if
--  end for
    if msg_id=106 then      -- e106ioob(edi,edx)
        if find(or_edi,{#40000000,#4000000000000000}) then
--          or_edx = or_ecx
            or_esi = or_ecx
            msg_id = 92
        else
--          msg = sprintf(msg,{or_edi,or_edx})
            msg = sprintf(msg,{or_edi,or_esi})
        end if
    elsif msg_id=30 then    -- e30ume
        -- Map any machine exceptions that occur on add1 (refcount) 
        --  followed by a "helper" cmp eax,<varno>; ==> to e92:
--17/11/16 afaik, we still use cmp eax,imm32, not cmp rax, but I think we got rid of all inc.
--      if machine_bits()=32 then
--4/7/17:
--DEV the rqd test should be do-able - CSvaddr or [ds+?] or symtab[??]... (two valid ranges, when interpreting)
--      if (or_era>=??? and or_era<=???-5)
--      or (or_era>=??? and or_era<=???-5) then
--try
            x6 = peek({or_era,6})
--catch e
----  if e[E_CODE]!=#C0000005 then  -- maybe...
--  ?{"warning: peek failure pDiagN.e line 2655",e,or_era}
--  x6 = "123456"
--end try
    --      --  inc dword[ebx+src*4-8]      377104 2s3 F8
            --  add dword[ebx+src*4-8],1    203104 2s3 F8 01
            if x6[1]=0o203
            and x6[2]=0o104
            and and_bits(x6[3],0o307)=0o203         -- sib(maybe!) of 0o2s3,
            and x6[4]=#F8                           -- displacement is -8
            and x6[5]=#01 then                      -- literal imm8 of 1
    --DEV 64-bit
                if x6[6]=cmp_eax_imm32 then
                    or_esi = peek4u(or_era+6)
                    msg_id = 92
                    msg = msgs[92]
                end if
            end if
--      else
----    add qword[rbx+rcx*4-16],1             ;#0042D0E9: 48:203104213 F0 01         u  00 0A  3   6      
----    cmp eax,662                           ;#0042D0EF: 075 96020000               vu 00 01  1   8      
--          x6 = peek({or_era,7})
--          if x6[1]=#48
--          and x6[2]=0o203
--          and x6[3]=0o104
--          and and_bits(x6[4],0o307)=0o203         -- sib(maybe!) of 0o2s3,
--          and x6[5]=#F0                           -- displacement is -16
--          and x6[6]=#01 then                      -- literal imm8 of 1
--              if x6[7]=cmp_eax_imm32 then
--                  or_esi = peek4u(or_era+7)
--                  msg_id = 92
--                  msg = msgs[92]
--              end if
--          end if
--      end if
        if msg_id=30 then
            if xceptn=#C0000005
            or xceptn=#C0000005-#100000000 then
                wmsg = "[MEMORY VIOLATION]"
            elsif xceptn=#C00000FD
               or xceptn=#C00000FD-#100000000 then
                wmsg = "[STACK OVERFLOW]"
--#80000003/[EXCEPTION_BREAKPOINT(int3)]
            else
                wmsg = sprintf("#%08x",xceptn)
            end if
            msg = sprintf(msg,{wmsg,xcepta})
        end if
    end if
--?msg_id
    if msg_id=1             -- e01tcf(ecx)  -- (ecx is var no)
    or msg_id=110 then      -- e110tce(ecx) -- (ecx is var address)
        if msg_id=1 then
            varno = or_ecx
        else
            varno = varIdx(or_ecx,rtn,symtab)
        end if
        o = "???"
        if varno<1 or varno>length(symtab) then
            name = sprintf("???(varno=%d[or_ecx=#%08x])",{varno,or_ecx})
        else
            si = symtab[varno]
            if atom(si) then
                name = sprintf("???(atom(symtab[%d]))",varno)
            else
                name = si[S_Name]
                if atom(name) then
                    --DEV/SUG unnamed index temps -> ioob??? (see e01tcf)
                    name = sprintf("???(symtab[%d][S_name]=%d)",{varno,si})
                end if
                sNTyp = si[S_NTyp]
                if sNTyp!=S_GVar2
                and sNTyp!=S_TVar3 then
                    o = sprintf("???(symtab[%d] wrong type)",varno)
                else
                    if sNTyp=S_GVar2 then
                        {novalue,o} = getGvarValue(si[S_Slink])
                    elsif sNTyp=S_TVar3 then
                        {novalue,o} = getTvarValue(si[S_Tidx])
                    end if
                    if novalue then
                        if novalue=1 then
                            o = "<novalue>"
                        elsif novalue=2 then
                            o = "*** INVALID REF ***"
                        elsif novalue=3 then
                            o = "*** CORRUPT TYPE BYTE ***"
                        else
                            o = "*** INVALID ??? ***"
                        end if
                    else
                        o = sprint(o,50-length(name))
                    end if
                end if
            end if
        end if
--      o = getValue(or_edi, 50, length(si)+17, 1)
        msg = sprintf(msg,{name,o})         -- "type check failure, %s is %s\n"
    elsif msg_id=91         -- e92vhnbaav(ecx)
       or msg_id=92         -- e92vhnbaav(esi)
       or msg_id=93         -- e93vhnbaav(edi)
       or msg_id=94 then    -- e94vhnbaav(edx,esi)
--      -- varno in or_edx
--      if or_edx<1 or or_edx>length(symtab) then
--          si = sprintf("???(varno=%d)",or_edx)
--      else
--          si = symtab[or_edx]
--          if atom(si) then
--              si = sprintf("???(atom(symtab[%d]))",or_edx)
--          else
--              si = symtab[or_edx][S_Name]
--              if atom(si) then
--                  si = sprintf("???(symtab[%d][S_name]=%d)",{or_edx,si})
--              end if
--          end if
--      end if
        if msg_id=94 then
--DEV if integer(esi) ain't it supposed to become e04atsaa? (done in AddressMapping for one of them...)
--          if integer(or_esi) then
--!             msg_id = 4
--          end if
            or_esi = or_edx
--?{65,or_esi}
        elsif msg_id=91 then
            or_esi = or_ecx
        elsif msg_id=93 then
            or_esi = varIdx(or_edi,rtn,symtab)
        end if
        -- varno in or_esi
        if or_esi<1 or or_esi>length(symtab) then
            si = sprintf("???(varno=%d)",or_esi)
        else
            si = symtab[or_esi]
            if atom(si) then
                si = sprintf("???(atom(symtab[%d]))",or_esi)
            else
                si = si[S_Name]
                if atom(si) then
                    si = sprintf("???(symtab[%d][S_name]=%d)",{or_esi,si})
                end if
            end if
        end if
        msg = sprintf(msg,{si})
--?msg
--?1
    elsif msg_id=32 then        -- e32hc(era,edi)
        msg = sprintf(msg,{or_era,or_edi})
        or_era = or_edi
    elsif msg_id=58 then        -- e58bfn(edi)
        msg = sprintf(msg,or_edi)
    elsif msg_id=119 then       -- e119af(edi)
        #ilASM{
            -- recover (atom)ref ==> ref
            --  eg #40487620 ==> "oops"
            [32]
                mov eax,[or_edi]
                call :%pLoadMint
                mov [msg2],eax
            [64]
                mov rax,[or_edi]
                call :%pLoadMint
                mov [msg2],rax
              }
        if length(msg2) then
            msg2 &= ": "&msg2
        end if
        msg = sprintf(msg,{msg2})
    elsif msg_id=120 then       -- e120fle
--      c = varIdx(ep1)
        if or_esi=1 then
            name = "init"
        elsif or_esi=2 then
            name = "limit"
        elsif or_esi=4 then
            name = "step"
        else
            name = "???"
        end if
        if or_edi<1 or or_edi>length(symtab) then
            o = sprintf("???(varno=%d)",or_edi)
        else
            si = symtab[or_edi]
            if atom(si) then
                o = sprintf("???(atom(symtab[%d]))",or_edi)
            else
                sNTyp = si[S_NTyp]
                if sNTyp!=S_GVar2
                and sNTyp!=S_TVar3 then
                    o = sprintf("???(symtab[%d] wrong type)",or_edi)
                else
                    if sNTyp=S_GVar2 then
                        {novalue,o} = getGvarValue(si[S_Slink])
                    elsif sNTyp=S_TVar3 then
                        {novalue,o} = getTvarValue(si[S_Tidx])
                    end if
                    if novalue then
                        if novalue=1 then
                            o = "<novalue>"
                        elsif novalue=2 then
                            o = "*** INVALID REF ***"
                        elsif novalue=3 then
                            o = "*** CORRUPT TYPE BYTE ***"
                        else
                            o = "*** INVALID ??? ***"
                        end if
                    else
                        o = sprint(o,55-length(name))
                    end if
                end if
            end if
        end if
--      o = "??"--getValue(or_edi, 5, 0, 1)
        msg = sprintf(msg,{name,o}) -- "for loop error, %s is %s\n"
--      msg = sprintf(msg,{ep1})
    elsif msg_id=9 then -- e09slin(edi,ecx)
        msg = sprintf(msg,{or_edi,or_ecx})  -- "slice length is negative [%d..%d]\n"
    elsif msg_id=11 then -- e11sepeos(edi,esi)
        if or_edi<0 then
            msg = sprintf("slice end(%d) less than negative length(%d)\n",{or_edi,-or_esi})
        else
            msg = sprintf(msg,{or_edi,or_esi})  -- "slice ends past end of sequence (%d > %d)\n"
        end if
    elsif msg_id=72 then        -- e72iri(edi)
        msg = sprintf(msg,or_edi)
    elsif msg_id=6              -- e06ioob(edi,esi)
       or msg_id=116 then       -- e116rrnp(edi,esi)
        msg = sprintf(msg,{or_edi,or_esi})
    elsif msg_id=14 then        -- e14soa(edi)
        if or_edi>=1 and or_edi<=length(e14ops) then
            o = e14ops[or_edi]
        else
            o = sprintf("???(%d)",or_edi)
        end if
        msg = sprintf(msg,{o,o})
    elsif msg_id=108 then       -- e108pe(edi)
        msg = sprintf(msg,{or_edi})
    elsif msg_id=80 then        -- e80cbrna(esi)
        rtn = or_esi    -- routine number
        -- (params/locals suppressed below, since they no longer exist)
    elsif msg_id=53 then        -- e53mcat(esi,ecx)
        msg = sprintf(msg,{or_esi,or_ecx*4,or_ecx*4-or_esi})
    elsif msg_id=88 then        -- e88atcfpmbaos(edi)
        msg = sprintf(msg,{iff(or_esi=1?"fun":"pro")})  -- c_func|c_proc
    elsif msg_id=68 then        -- e68crash
        msg = sprintf(msg,{crash_msg})  -- crash(xxxx)
    elsif msg_id=42 then        -- e42a(ecx)
        msg = sprintf(msg,{or_ecx})
    elsif msg_id=55 then
        integer lm = length(msg)
        if string(throwee) and lm<70 then
            integer lt = length(throwee)
            if lt+lm>76 then
                throwee[74-(lt+lm)..$] = "..."
            end if
            msg = sprintf("%s (%s)\n",{msg[1..$-1],throwee})
        end if
    end if
--?2
--/*

    if msg_id=1         -- e01tcf (ep1 is var idx)
    or msg_id=110 then  -- e110tce (ep1 is var addr)
        if msg_id=110 then
            ep1 = varIdx(ep1)
        end if
        si = "???"
        if ep1>0 and ep1<=length(symtab) then
            si = symtab[ep1][S_Name]
            if atom(si) then
                si = sprintf("???(name=%d)",si)
            end if
        end if
        o = getValue(ep1, 50, length(si)+17, 1)
        msg = sprintf(msg,{si,o})       -- "type check failure, %s is %s\n"
    elsif msg_id=10         -- e10sspeos
      and ep1<0 then
        msg = sprintf("slice start(%d) less than negative length(%d)\n",{ep1,-ep2})
    elsif msg_id=11         -- e11sepeos
      and ep1<0 then
        msg = sprintf("slice end(%d) less than negative length(%d)\n",{ep1,-ep2})
    elsif msg_id=6          -- e06ioob
       or msg_id=9          -- e09slin
       or msg_id=10         -- e10sspeos
       or msg_id=11         -- e11sepeos
       or msg_id=31         -- e31mce
       or msg_id=116 then   -- e116rrnp
        msg = sprintf(msg,{ep1,ep2})
--  elsif msg_id=120 then   -- e120fle
----        c = varIdx(ep1)
--      o = getValue(ep1, 5, 0, 1)
--      if ep2=1 then
--          si = "init"
--      elsif ep2=2 then
--          si = "limit"
--      elsif ep2=4 then
--          si = "step"
--      else
--          si = "???"
--      end if
--      msg = sprintf(msg,{si,o})
----        msg = sprintf(msg,{ep1})
    elsif msg_id=121 then   -- e121flelimstep
        c = varIdx(ep1)
        o = getValue(c, 5, 0, 1)
        c = varIdx(ep2)
        o2 = getValue(c, 5, 0, 1)
        msg = sprintf(msg,{o,o2})
    elsif msg_id=28 then    -- e28NNatXmbausq
        if ep1>=1 and ep1<=length(e28ops) then
            o = e28ops[ep1]
        else
            o = "???"
        end if
        msg = sprintf(msg,{o,o})
    elsif msg_id=30 then    -- e30ume
        -- Map any machine exceptions that occur on inc/add1 (refcount) 
        --  followed by a "helper" cmp eax,<varno>; ==> to e92:
        x6 = peek({era,6})
        --  inc dword[ebx+src*4-8]      377104 2s3 F8
        --  add dword[ebx+src*4-8],1    203104 2s3 F8 01
        if x6[2]=0o104
        and and_bits(x6[3],0o307)=0o203         -- sib(maybe!) of 0o2s3,
        and x6[4]=#F8 then                      -- displacement is -8
--DEV 64-bit
            if x6[1]=0o377 -- inc
            and x6[5]=cmp_eax_imm32 then
                ep1 = peek4u(era+5)
                msg_id = 92
            elsif x6[1]=0o203 -- add
              and x6[5]=#01                     -- ie [ebx+idx*4-8],1
              and x6[6]=cmp_eax_imm32 then
                ep1 = peek4u(era+6)
                msg_id = 92
            end if
        end if
        if msg_id=92 then -- ie e92vhnbaav, aka "variable %s has not been assigned a value"
            msg = msgs[92]
            if ep1>0 and ep1<=length(symtab) then
                si = symtab[ep1]
                msg = sprintf(msg,si[S_Name..S_Name])
            else
                printf(1,"diag.e: oops(4), var no[=%d] out of range\n",ep1) -- See note at top
            end if
        else
            if ep1=#C0000005-#100000000 then
                wmsg = "[MEMORY VIOLATION]"
            elsif ep1=#C00000FD-#100000000 then
                wmsg = "[STACK OVERFLOW]"
            else
                wmsg = sprintf("#%08x",ep1)
            end if
            msg = sprintf(msg,{wmsg,era})
        end if
    elsif msg_id=92 then    -- e92vhnbaav
        c = varIdx(ep1)
        si = symtab[c][S_Name]
        if atom(si) then
            si = sprintf("???(name=%d)",si)
        end if
        msg = sprintf(msg,{si})
    elsif msg_id=97 then    -- e97he
--      if ep1=112 then
--          o = {"112(ERROR_DISK_FULL)"}
--      else
            o = {sprintf("%d",ep1)}
--      end if
        msg = sprintf(msg,o)
    elsif msg_id=98 then    -- e98fiofe
        if ep1=112 then
            o = {"112(ERROR_DISK_FULL)"}
        else
            o = {sprintf("%d",ep1)}
        end if
        msg = sprintf(msg,o)
        crashfile = "NUL"
    elsif msg_id>=120 and msg_id<=122 then  -- for loop errors:
        c = varIdx(ep1)
        o = getValue(c, 50, 18, 1)
        msg = sprintf(msg,{o})
    elsif msg_id=108 then       -- e108pe
        if ep1=87 then
            wmsg = "ERROR_INVALID_PARAMETER"
        else
            wmsg = sprintf("#%08x",ep1)
        end if
        msg = sprintf(msg,{wmsg})
    end if
--puts(1,"d4\n")
--*/
--?3
--?msg
--?4

--EXCEPT
--(need to get the int3 tests done first!) [DONE]
bool error_handler
    #ilASM{
        [32]
            xor eax,eax
            cmp [ebp+16],ebx
            setne al
            mov [error_handler],eax
        [64]
            xor rax,rax
            cmp [rbp+32],rbx
            setne al
            mov [error_handler],rax
          }
    if error_handler 
--  and msg_id!=12 then -- not e12pa ('!' keyed in trace window)
    and msg_id!=12      -- not e12pa ('!' keyed in trace window)
    and msg_id!=56 then -- not e56rocow (also non-catchable)
        msg = trim(msg)
        sr = symtab[rtn]
        lineno = convert_offset(or_era,sr)
        diaglooping -= 1
        throw({msg_id,or_era,lineno,rtn,-1,-1,-1,msg})
    end if

    if not batchmode then
        puts(1,"\n")
    end if

--  if equal(crashfile,"") then return batchmode end if
    if equal(crashfile,"") then return end if
    if find(crashfile,{"NUL","NULL","/dev/null"}) then
        fn = -1
    elsif not atom(crashfile) then
        fn = open(crashfile,"w")
    else
        fn = open("ex.err","w")
    end if
--puts(1,"ex.err open\n")
--?{"rtn",rtn}
    msg2 = ""
    while 1 do
--?rtn
        if rtn<1 or rtn>length(symtab) then -- See note at top
            printf(1,"pDiagN.e line 3064: oops, rtn[=%d] out of range[1..%d]\n",{rtn,length(symtab)})
--          exit
--      end if
            rtype = 0   -- (added 15/4/16, at the time we had the wrong symtab... then again it was a bug in pTrace.e)
else
        sr = symtab[rtn]
--?sr
        sNTyp = sr[S_NTyp]
        if sNTyp>=S_Type
        and (swod or and_bits(sr[S_State],K_wdb)) then -- skip without debug items
--          sequence sr = symtab[rtn]
            lineno = convert_offset(or_era,sr)
--/*
            lineno = sr[S_1stl]     -- line no of "procedure"/"function"/"type" keyword
            linetab = sr[S_ltab]
            lastline = linetab[$]

--          returnoffset = era-sr[S_il]
            returnoffset = or_era-sr[S_il]
            --
            -- Convert the offset to a line number.
            -- A raw line table, as built in pilx86.e, is {skip|offset} where
            --  skip is <0, meaning -n lines emitted no code, and
            --  offset is >=0, a start offset of the next code-emitting line.
            --  There is also a dummy max entry added at the end of ilxlate().
            --  A line table should never have two (or more) skips in a row.
            -- When interpreting, we can just use the raw table directly: skip down the
            --  table until the offset is (b)reached, though we only know that when we 
            --  hit the next entry, as detailed in the following.
            --  Example1: a raw linetab of {-2,0,24,36} means offsets 0..23 are S_1st+2,
            --            24..35 are S_1st+3, and <0 or >=36 are out-of-bounds (so leave 
            --            lineno as -1). We only know that we should have stopped for an 
            --            offset of 17 when we hit the 24, and the lineno we want is that 
            --            before the +1 triggered by the 0 (or whatever line adjustment 
            --            we made on [2] when we decide to stop on [3]).
            --  Example2: for a linetab of {-14,#14,-3,#47...} and offset of #22, we only 
            --            know that #14 (S_1st+14) is the right entry when we hit the #47
            --            (S_1st+18), so there is a +1 and -(-3) that we must ignore.
            --            Note that while an exception at offset #47 means S_1st+18, a 
            --            return address of #47 would be the last call made by S_1st+14;
            --            hence add -1 when using a return address to report an error.
            -- When compiled, the linetab is packed: the offsets are converted to deltas
            --  (so most will be <=127) with #81,#80 used as lead-ins for word,dword to
            --  hold values that will not fit in a byte, and lastly stored as a "string"
            --  of binary (#00..#FF) bytes, making it just over 25% of the size, on 32
            --  bit systems, and just over 12.5% on 64 bit systems (ie worth having).
            --  A fairly straightforward decode of the next raw value (into lti) is
            --  followed by the same logic as above, except that when we start with a
            --  raw table we can test lastline directly, but not when unpacking it.
            -- Lastly note that [era] is incredibly fiddly to set, especially for
            --  low-level routines isolated several calls deep from user code. If
            --  the following yields -1, first suspect [era] rather than this code,
            --  except for e30ume, in which case suspect that "Address Mapping" in
            --  :!fehDiag (below) is missing a test/label.
            --
            if string(linetab) then -- compiled (deltas packed to string)
                lineno = -1
                if returnoffset>=0 then
                    thisline = sr[S_1stl]
                    linenxt = thisline
                    skip = 0
                    base = 0
                    for i=1 to length(linetab) do
                        if skip then
                            skip -= 1
                        else
                            lti = linetab[i]
                            if lti>#7F then
                                if lti>#81 then
                                    lti = lti-#100
                                elsif lti=#81 then
                                    lti = linetab[i+1]*#100+linetab[i+2]
                                    if lti>#7FFF then
                                        lti -= #10000   -- (skip < -128)
                                    end if
                                    skip = 2
                                elsif lti=#80 then
                                    lti = linetab[i+1]*#1000000+linetab[i+2]*#10000+linetab[i+3]*#100+linetab[i+4]
                                    if lti>#7FFFFFFF then
                                        lti -= #100000000 -- (skip < -32,768?? [very rare, if ever])
                                    end if
                                    skip = 4
                                else
                                    ?9/0    -- (sanity check, should never happen)
                                end if
                            end if
                            if lti<0 then       -- -n lines emitted no code
                                linenxt -= lti
                            else                -- start offset of next line
                                tmp = base
                                base += lti
                                lti = tmp
--                              if returnoffset<=lti then exit end if   -- all done
                                if returnoffset<lti then
                                    lineno = thisline
                                    exit
                                end if  -- all done
                                thisline = linenxt
                                linenxt += 1
                            end if
                        end if
                    end for
                end if
            else -- interpreted (raw linetab, a dword-sequence, not converted to deltas/packed)
                lineno = sr[S_1stl]
                lastline = linetab[$]
                if returnoffset<0 or returnoffset>=lastline then
                    lineno = -1
                else
                    linenxt = lineno
                    for i=1 to length(linetab) do
                        lti = linetab[i]
                        if lti<0 then       -- -n lines emitted no code
                            linenxt -= lti
                        else                -- start offset of next line
--                          if returnoffset<=lti then exit end if   -- all done
                            if returnoffset<lti then exit end if    -- all done
                            lineno = linenxt
                            linenxt += 1
                        end if
                    end for
                end if
            end if
--*/
--          if lineno=-1 and find(msg_id,{92,30}) then
            if lineno=-1 and find(msg_id,{92,30}) and length(msg) and length(msg2)=0 then
--?-1
--              --
--              -- If you have opFrame / mov a,b / mov c,d / opCall, where b or d is
--              --  unassigned (a,c are parameters of the routine about to be called),
--              --  then the e92 will try to locate the failure address within the 
--              --  routine about to be called; pop (at most one not yet active) frame 
--              --  and try again:
--              --
--DEV done above, I think: [needs testing in terror.exw!]
--              if msg_id=30 then
----DEV:
----                    if c_func(xIsBadReadPtr,{era,1}) then
----                        printf(1,"diag.e: oops, invalid <era>[%08x]\n",era)
----                    els
--                  if peek({era,2})=incd_sib then
----DEV:
----                        if c_func(xIsBadReadPtr,{era-5,1}) then
----                            printf(1,"diag.e: oops, invalid <era-5>[%08x]\n",era)
----                        els
--                      if peek(era-5)=cmp_eax_imm32 then
--                          msg = msgs[92]  -- ie e92vhnbaav, aka
--                                  -- "variable %s has not been assigned a value"
--                          ep1 = peek4u(era-4)
--                          if ep1>0 and ep1<=length(symtab) then
--                              si = symtab[ep1]
--                              msg = sprintf(msg,si[S_Name..S_Name])
--                          else
--                              printf(1,"diag.e: oops(2), var no[=%d] out of range\n",ep1) -- See note at top
--                          end if
--                      end if
--                  end if
--              end if
--              msg_id = 0
--              if not retN() then  -- See note at top
--                  printf(1,"diag.e: oops, lineno=-1/e92/not retD(), era=#%08x\n",era)
--                  exit
--              end if
--DEV removed 13/5/15 (for test/terror...)
                if show_bad_era then
                    msg2 = sprintf("  (warning: lineno of -1 for era of #%08x)\n",{or_era})
                end if
                rtype = 1   -- 1 skip (lineno=-1)
            else
--?lineno
--?sr
--?sr[S_FPno]
--?9997
--?symtab[T_fileset]
--?9998
--if 0 then
--              filename = symtab[T_fileset][sr[S_FPno]][1..2]&lineno
----?filename
--              filename[1] = pathset[filename[1]]
----?filename
--              put2(sprintf("%s%s:%d",filename))
--else -- new code
--20/12/19:
--              filename = symtab[T_fileset][sr[S_FPno]][1..2]
                sequence sfs = symtab[T_fileset]
                integer srfn = sr[S_FPno]
--printf(1,"pDiagN.e line 3294, srfn=%d, length(symtab[T_fileset])=%d\n",{srfn,length(sfs)})
                if srfn>length(sfs) then
--DEV triggered on ::retaddr in pcallfunc.e line 251 - needs something akin to AddressMapping (see below)...[??]
--?pathset
--printf(1,"cc_ret:%08x\n",{cc_ret_addr})
                    filename = {"<unknown file>",sprintf("(%d)",srfn)}
                    -- (drat, cannot be sure, so just take a leap of faith...)
--                  filename = {pathset[2],"pcallfunc.e",":cc_retaddr"}
                    sr[S_Name] = -1
                else
--                  filename = sfs[srfn][1..2]
                    filename = deep_copy(sfs[srfn][1..2])
                    filename[1] = pathset[filename[1]]
                end if
                if lineno=-1 then
                    filename = append(filename,sprintf("-1 (era=#%s, from_addr=#%s, ret_addr=#%s)",
                                                       {addrS(or_era),addrS(from_addr),addrS(ret_addr)}))
                else
                    filename = append(filename,sprintf("%d",lineno))
                end if
                put2(sprintf("%s%s:%s",filename))
--4/2/21
cdi_filename = filename[2]
--end if
                if sr[S_Name]=-1 then
--              if sr[S_Name]=-1 or sr[S_NTyp]=S_Rsvd then
                    put2("\n")
                else
                    put2(sprintf(" in %s %s()\n",{rtndescs[sr[S_NTyp]-S_Type+1],sr[S_Name]}))
--?3
                end if
                if length(msg) then     -- first time only
                    if length(msg2) then
--                      put2(msg2)
                        puts(1,msg2)
                        msg2 = ""
                    end if
                    put2(msg)
                    msg = ""
                end if
if msg_id=80 then   -- e80cbrna
    -- "call_back returned non-atom",
    -- note this occurs after the callback has returned, hence
    -- parameters/locals suppressed, since they no longer exist
    msg_id = 0
else
--?sr
                p = sr[S_Parm1]         -- (0 for all top-level subs)
--?sr
                tidx = 0    --DEV can we not use [S_Tidx]??
                while p do
                    if p<1 or p>length(symtab) then
                        printf(1,"oops, p(%d) out of bounds(1..%d), pDiagN.e line 3281\n",{p,length(symtab)})
                        exit
                    end if
                    sp = symtab[p]
--?{sp}
--SUG: or si[S_NTyp]!=S_TVar3
                    if atom(sp) then
                        printf(1,"oops, atom(symtab[%d]), pDiagN.e line 3288\n",p)
                        exit
                    end if
                    name = sp[S_Name]
                    if sequence(name) then
--                      o = "??"--getValue(p, maxlen, length(name), 0)
                        {novalue,o} = getTvarValue(tidx)
--                      if showellipse or lc<maxlen-50 then
--                          -- we need a (much) shorter version for on-screen display:
--                          o2 = getValue(p,50,length(name), 1)
--                          o = sprintf("    %s = %s\n",{name,o})
--                          o2 = sprintf("    %s = %s\n",{name,o2})
--                          put2x(o2,o)
--                      else
--                          put2(sprintf("    %s = %s\n",{name,o}))
--                      end if
                        putz(name,o,novalue)
                    elsif name!=-1 then -- should not happen!
--                      put2(sprintf("    %d[!!] = ???\n",name))
                        name = sprintf("%d[!!]",name)
--                      o = "??"--getValue(p, maxlen, length(name), 0)
                        {novalue,o} = getTvarValue(tidx)
--                      put2(sprintf("    %s = %s\n",{name,o}))
                        putz(name,o,novalue)
                    end if
                    tidx -= 1
                    p = sp[S_Slink]
                end while
end if
--              if not retN() then exit end if
                rtype = 2       -- 2 normal
            end if  -- lineno!=-1
        else -- K_wdb
--removed 4/2/21...
--          if sNTyp<S_Type then
--              put2(sprintf("pDiagN.e line 3322: symtab[%d] bad S_NTyp[%d]\n",{rtn,sNTyp}))
--?sr
--?"sleep(5)..."
--sleep(5)
----            else
----                put2(sprintf("diag.e: symtab[%d] skipped (no debug)\n",{rtn}))
--          end if
--          msg_id = 0
--          if not retN() then exit end if
            rtype = 3           -- 3 without debug
        end if  -- K_wdb
        msg_id = 0
end if
        while 1 do
            or_ebp = floor(prev_ebp/4)
--11/12/15:
--          if ret_addr=TchkRetAddr then
--29/10/17:
            if ret_addr=TchkRetAddr
            or ret_addr=TchkRetAddr-1   -- 22/12/19
            or ret_addr=cf_ret_addr-1 then
                or_era = from_addr-1
            else
                or_era = ret_addr-1
            end if
--?{ret_addr,cf_ret_addr,from_addr,or_era}
--put2(sprint({ret_addr,cf_ret_addr,from_addr,or_era}))
--put2(sprintf("ret_addr=#%s, cf_ret_addr=#%s, from_addr=%s, or_era=#%s\n",
--  {addrS(ret_addr),addrS(cf_ret_addr),addrS(from_addr),addrS(or_era)}))

            if or_ebp=0 then exit end if
--DEV (untried)
--constant W = machine_word()
--  --      {N,rtn,from_addr,ret_addr,prev_ebp,ebp_root} = peekNS({or_ebp*4+W,6},W,0)
--          {N,rtn,from_addr,?,prev_ebp,ebp_root,ret_addr} = peekNS({or_ebp*4+W,7},W,0)
            if machine_bits()=32 then
--EXCEPT
--              {N,rtn,from_addr,ret_addr,prev_ebp,ebp_root} = peek4u({or_ebp*4+4,6})
                {N,rtn,from_addr,?,prev_ebp,ebp_root,ret_addr} = peek4u({or_ebp*4+4,7})
            else -- machine_bits()=64
--              {N,rtn,from_addr,ret_addr,prev_ebp,ebp_root} = peek8u({or_ebp*4+8,6})
                {N,rtn,from_addr,?,prev_ebp,ebp_root,ret_addr} = peek8u({or_ebp*4+8,7})
            end if
if 0 then -- DEV 29/10/17/TEMP
    if show_low_level_diagnostics then
--      if diagdiag>0 or (vsb_magic-#40565342) or msg_id<1 or msg_id>length(msgs) then
        if diagdiag>0 then
            put2(sprintf("N=%d, rtn=%d, from=#%s, ret=#%s, ehand=%s, prevebp=#%s, ebproot=#%s\n",
                   {N,rtn,addrS(from_addr),addrS(ret_addr),addrS(ehand),addrS(prev_ebp),addrS(ebp_root)}))
            put2(sprintf("or_eax=#%08x, or_ecx=#%08x, or_edx=#%08x,\nor_esi=#%08x, or_edi=#%08x\n",
                   {or_eax,or_ecx,or_edx,or_esi,or_edi}))
--          magicok = "\"@VSB\""
            magicok = `"@VSB"`
--DEV wrong on machine_bits()=64... (possibly one for docs) [I think it may be OK now...]
--          if vsb_magic!=#40565342 then
            if (vsb_magic-#40565342) then
                magicok = "**BAD MAGIC**"
            end if
            put2(sprintf("vSb_prev=#%s, vsb_next=#%s, vsb_magic=%s (%s)\n",
                   {addrS(vsb_prev),addrS(vsb_next),addrS(vsb_magic),magicok}))
        end if
    end if
end if
--if diagdiag then
--          printf(1,"N=%d, rtn=%d, from=#%s, ret=#%s, prevebp=#%s, ebproot=#%s\n",
--                 {N,rtn,addrS(from_addr),addrS(ret_addr),addrS(prev_ebp),addrS(ebp_root)})
--end if
-- (untried [might cause problems with test after loop, which might go away if moved (back) above??])
            if prev_ebp=0 then exit end if
--          if prev_ebp=0 then put2("prev_ebp=0\n") exit end if
--if ret_addr=cf_ret_addr-1 then ret_addr = ?? end if
            if ret_addr!=0 
--          and ret_addr!=cb_ret_addr then
            and or_era!=cb_ret_addr-1 then
--          and or_era!=cf_ret_addr-2 then
--          and ret_addr!=cf_ret_addr-1 then
--put2("QUIT\n")
                ret_addr -= 1
                exit
            end if
--if prev_ebp!=0 then
--          if platform()=WINDOWS then
--              put2("(^^^) call_back from Windows/dll/asm\n")
--          else
            put2(" (^^^) call_back from OperatingSystem/sharedlib/asm\n")
--          end if
--end if
        end while
        if or_ebp=0 then
            if length(msg2) then
                put2(msg2)
                msg2 = ""
            end if
            if rtype=1 then -- skip (lineno=-1)
                printf(1,"pDiagN.e line 3413: oops, lineno=-1/e92/not retD(), era=#%08x\n",or_era)
            end if
            exit
        end if
        if rtype = 2 then       -- 2 normal
            put2("... called from ")
        end if
    end while
--erm??
--  if not batchmode then
    if not batchmode and atom(crash_msg) then
        puts(1,"\nGlobal & Local Variables\n")
    end if
    if fn!=-1 then
        puts(fn,"\nGlobal & Local Variables\n")
        fileno = 0

        for i=T_const1+1 to length(symtab) do
            si = symtab[i]
            if sequence(si) then                            -- might not be dumped (unused)
                name = si[S_Name]
--SUG and consts?
                if equal(si[S_NTyp],S_GVar2)
                and (swod or and_bits(si[S_State],K_wdb))   -- skip without debug items
                and sequence(name) then                     -- skip unnamed items
                    fpno = si[S_FPno]
                    if fileno!=fpno then
                        fileno = fpno
                        filename = deep_copy(symtab[T_fileset][fileno][1..2])
                        filename[1] = pathset[filename[1]]
                        printf(fn,"\n %s%s:\n",filename)
--4/2/21:
cdi_filename = filename[2]
                    end if
                    {novalue,o} = getGvarValue(si[S_Slink])
                    if novalue then
                        printf(fn,"    %s = <novalue>\n",{name})
                    else
                        clever_dump(name, o)
                    end if
                end if
            end if
        end for

        if not batchmode then
            puts(1,"\n")
--DEV
--          #isginfo{crash_msg,0b1001,0,0,integer,0} -- (verify compiler is working properly)
--          #isginfo{crash_msg,0b1001,0,0,integer,3} -- (verify compiler is working properly)
--          if atom(crash_msg) and fn!=-1 then
            if fn!=-1 then
                if atom(crashfile) then
--                  puts(1,"--> see "&current_dir()&"\\ex.err\n")
                    printf(1,"--> see %s\n",{join_path({current_dir(),"ex.err"})})
--              elsif not find(crashfile,{"NUL","/dev/null"}) then
                else -- (above not necessary, fn would be -1)
                    puts(1,"--> see "&crashfile&"\n")
                end if
            end if
        end if
--DEV if interperting, leave this open and have p.exw/main() dump Warnings() to it:
        if fn!=-1 then
            close(fn)
        end if
    end if
    close(-9)
    if crash_rtn!=-1 then
--  if 0 then
        for i=length(crash_rtn) to 1 by -1 do
            if call_func(crash_rtn[i],{0})!=0 then exit end if
        end for
--  end if
    elsif not batchmode then
--  if not batchmode then
--?batchmode
        puts(1,"Press Enter...")
        if wait_key() then end if
--      abort(abortcode)
    end if
    abort(abortcode)
--?batchmode
    --
    -- PS if you're looking for puts(1,"\nPress Enter[, or d for diagnostics]..."),
    --    it is not in here but in pmsgs.e
    --
end procedure

--  return batchmode
--end function

--/*
--atom diagcb
--  diagcb = call_back(routine_id("diag"))
--#ilASM{lea edi,[diagcb]
--     call %opCrshRtn} -- save [edi]
--*/

--DEV./SUG:
--/*
    #ilASM{ :%opErrf    -- use the called from address in the frame
                mov esi,[or_ebp*4+12] (except esi is ep2)
            :%opErr     -- called from address in esi
          }
--*/


 -- Low Level Integration
 ------------------------
--
--  1) Exceptions (via pFEH.e/:!fehDiag). For :!iDiag see next.
--      Exceptions are often preferred for performance reasons, for example:
--  --?     cmp esi,h4  -- (not always necessary)
--  --?     jl :doInt   --   "    "        "
--  --?     je :!iDiag  -- (branch misprediction likely!)
--        :!opXXe94vhnbaavesi
--          cmp byte[ebx+esi*4-1],#12   -- float
--          jcc ???     -- (branch misprediction!!)
--      Catching an exception at :!opXXe94vhnbaavesi (when esi==h4 aka unassigned) is 
--       obviously faster (by which I mean when no exception/fatal error occurs) than
--       always performing a conditional jump that is almost never taken, and in many 
--       cases the test that it would otherwise need can also be omitted. Further, on
--       some processors with limited branch prediction granularity, having jumps so
--       close is something that should be avoided if at all possible. However (OTOH)
--       it may need additional setup (eg var no in some other register) that negates 
--       any such gains/omissions, in which case direct call may be better. While a
--       heavily used opcode such as opMove or opJcc can show significant benefits
--       from even a single clock saving, if it is easier on the lesser used opcodes
--       to setup regs/era etc, take the hit for maintainability sake and go direct.
--      Bang labels (:!) are used so that if a particular source (eg pSubse.e) is not
--       loaded, a cmp edx,:!opSubse1ipRe92a just quietly resolves to cmp edx,0.
--       Likewise we use :!fehDiag (and :!iDiag) so things will compile cleanly under 
--       -nodiag (and resolve to jmp 0, ie continue at next instruction, eg an int3).
--      It is up to the programmer to ensure all such label names are unique, with
--       the compiler throwing errors for any possible clash (unlike hll globals,
--       there is no way to use namespaces, so any clashes are always errors).
--      Typically the code below (just above ::alset) converts exception addresses to
--      exception codes, and diag() figures out ep1/2/era etc as needed for that code.
--      However, just as long as it is consistent, it doesn't matter who does it.
--
--  2) Direct call (via :!iDiag)
--      Other times you may as well just call the error handler directly, eg type
--       check errors, divide by zero, badretf, etc. There may even be a small gain
--       from test/extra-setup/jmp, over extra-setup/no-test-but-catch-exceptions.
--       It is obviously easier to setup regs/era near the point of failure rather
--       than in here, hidden amongst possibly hundreds of similar blobs of code.
--      As above, diag() has to figure out ep1/2/era etc for that code, hence it is
--       generally unwise to mix exception and direct handling for the same number.
--       However, it is also perfectly reasonable to apply ad-hoc standards, such 
--       as ep1 in or_ecx, ep2 in or_edi in direct/exception logic below, before
--       invoking diag(), just as long as that also knows the score, and of course
--       any such ad-hoc standards would be entirely exception-code-specific.
--      Before invoking :!iDiag the exception code should be stored in al, and the
--       effective return address in edx, so obviously move eax/edx to other regs 
--       first, if they are going to be needed.
--      A call :!iDiag should be followed by int3 to avoid confusion under -nodiag.
--       (If you don't use -nodiag you can ignore that, if you do, you want an OS
--        level fault rather than stumbling blindly on. Try/catch, if it is ever 
--        implemented, will handle things before they get anywhere near here.)
--
--  3) Testing
--      If should go without saying that there is absolutely nowhere else in Phix,
--       by quite some margin, that needs such diligent and thorough testing as any
--       modifications made here. I know there are hundreds of exception codes, and
--       hundreds of bang labels, but they all need long and hard thought as to how
--       each and every one of them could ever possibly trigger. Without any doubt,
--       some of them will already have been missed, so don't make things worse.
--      Anything that needs fixing should also get a new entry in test/terror.exw,
--       assuming that completely missed the bug and passed with flying colours.
--

--DEV initD issues? (spotted in passing)
constant a32h4 = #40000000,
         a64h4 = #4000000000000000

    #ilASM{ jmp :%opRetf

--/*
procedure :!diagFrame(:%)
end procedure -- (for Edita/CtrlQ)
--*/
    :!diagFrame
---------------
        -- stub provision for pemit2.e, see there for an explanation, which
        --  will also involve you looking over the :!rbidscb calls above,
        --  where you will also find the outermost save/restore of [ds+8].
        [32]
            mov eax,[ds+8]          -- symtab
            mov esi,[eax+84]        -- symtab[T_EBP=22]
            test esi,esi
            jz @f   -- ?? (pemit2.e was setting rbldrqd too early)
            -- in case it helps, the next two lines should work just as well:
--          mov eax,[symtab]
--          mov edx,[ebp+eax*4+84]  -- symtab[T_EBP]
--          mov eax,[ebx+edx*4]     -- symtab[T_EBP][1] = ebp4
--          mov ecx,[ebx+edx*4+4]   -- symtab[T_EBP][2] = esp4
            mov esi,[ebx+esi*4+8]   -- symtab[T_EBP][3] = pst4
--          shl eax,2
--          shl ecx,2
            shl esi,2
--          mov ebp,eax             -- restore ebp
--          mov esp,ecx             -- restore esp
            mov [ds+8],esi          -- restore symtabptr
          @@:
            push dword[esp]                         -- (leave the ret addr on stack)
--          mov [ds+8],eax
        [64]
            mov rax,[ds+8]          -- symtab
            mov rsi,[rax+168]       -- symtab[T_EBP=22]
            test rsi,rsi
            jz @f
            mov rsi,[rbx+rsi*4+16]  -- symtab[T_EBP][3] = pst4
            shl rsi,2
            mov [ds+8],rsi          -- restore symtabptr
          @@:
            push qword[rsp]                         -- (leave the ret addr on stack)
--          mov [ds+8],rax
        []
            call :%opFrame
            ret


--/*
procedure :%pCrashMsg(:%)
end procedure -- (for Edita/CtrlQ)
--*/
    :%pCrashMsg
---------------
        -- note there is a separate hll crash() routine,
        --  see builtins\pCrash.e for details
        [32]
            -- calling convention
            --  mov eax,[msg]       -- (should be a string, opUnassigned)
            --  call :%pCrashMsg    -- (save eax)
            mov edx,[crash_msg]
            cmp eax,h4
            mov [crash_msg],eax
            jl @f
                add dword[ebx+eax*4-8],1
          @@:
            cmp edx,h4
            jle @f
                sub dword[ebx+edx*4-8],1
                jne @f
                push dword[esp]
                call :%pDealloc0
        [64]
            -- calling convention
            --  mov rax,[msg]       -- (opUnassigned)
            --  call :%pCrashMsg    -- (save eax)
            mov r15,h4
            mov rdx,[crash_msg]
            cmp rax,r15
            mov [crash_msg],rax
            jl @f
                add qword[rbx+rax*4-16],1
          @@:
            cmp rdx,r15
            jle @f
                sub qword[rbx+rdx*4-16],1
                jne @f
                push qword[rsp]
                call :%pDealloc0
        []
          @@:
            ret

--DEV dead
----/*
--procedure :%pCrash(:%)
--end procedure -- (for Edita/CtrlQ)
----*/
--  :%pCrash
--------------
--      -- note there is a separate hll crash(fmt,data={}) routine,
--      --  which is not part of the optable.
--      [32]
--          -- calling convention
--          --  mov eax,[msg]           -- (opUnassigned)
--          --  call :%pCrash           -- crash(msg)
--          add dword[ebx+eax*4-8],1    -- incref
--          mov edx,[??]
--          mov [crash_msg],eax
--          cmp edx,h4
--          jle @f
--              sub dword[ebx+edx*4-8],1
--              jnz @f
--              call :%pDealloc
--        @@:
--      [64]
----DEV
----            cmp eax,h4
----            jl @f
----            cmp dword[ebx+eax*4-12],0
----            je :%pCrash1
--      []
--          int3
--          ret
--
----/*
--procedure :%pCrash1(:%)
--end procedure -- (for Edita/CtrlQ)
----*/
--  :%pCrash1   -- dead too
---------------
--      [32]
--          -- calling convention
--          --  mov eax,[fmt]       -- (opUnassigned)
--          --  call :%pCrash1      -- crash(eax)
----            call :%pCrashMsg
--      []
--          int3
--          ret

--/*
procedure :%pCrashFile(:%)
end procedure -- (for Edita/CtrlQ)
--*/
    :%pCrashFile
----------------
        [32]
            -- calling convention
            --  mov eax,[file_path] -- (opUnassigned)
            --  call :%pCrashFile   -- crash_file(eax)
            mov edx,[crashfile]
            cmp eax,h4
            mov [crashfile],eax
            jl @f
                add dword[ebx+eax*4-8],1
          @@:
            cmp edx,h4
            jle @f
                sub dword[ebx+edx*4-8],1
                jne @f
                push dword[esp]
                call :%pDealloc0
        [64]
            -- calling convention
            --  mov rax,[file_path] -- (opUnassigned)
            --  call :%pCrashFile   -- crash_file(rax)
            mov r15,h4
            mov rdx,[crashfile]
            cmp rax,r15
            mov [crashfile],rax
            jl @f
                add qword[rbx+rax*4-16],1
          @@:
            cmp rdx,r15
            jle @f
                sub qword[rbx+rdx*4-16],1
                jne @f
                push qword[rsp]
                call :%pDealloc0
        []
          @@:
            ret

--global procedure crash_file(object file_path)
---- Specify a file path name in place of "ex.err" where you want
---- any diagnostic information to be written.
---- May be called multiple times, at the point of a crash the
---- last value passed to this routine is used.
---- A value of "" turns off diagnostics completely.
---- A value of "NUL" or "/dev/null" displays messages to screen
---- but does not create an ex.err file.
---- A value of 0 restores default handling.
---- 
--  crashfile = file_path
--end procedure

--/*
procedure :%pCrashRtn(:%)
end procedure -- (for Edita/CtrlQ)
--*/
    :%pCrashRtn
---------------
        [32]
            -- calling convention
            --  mov eax,[rid]
            --  call :%pCrashRtn    -- crash_routine(eax)
--DEV
--?         push dword[esp]                         -- (leave the ret addr on stack)
            push eax
            mov edx,routine_id(set_crash_routine)   -- mov edx,imm32 (sets K_ridt)
            mov ecx,$_Ltot                          -- mov ecx,imm32 (=symtab[set_crash_routine][S_Ltot])
            call :%opFrame
--          call :!diagFrame
--          add esp,4
            pop dword[ebp]                          -- rid
--EXCEPT
--          mov dword[ebp+16],:crashrtnret
            mov dword[ebp+28],:crashrtnret
            jmp $_il                                -- jmp code:set_crash_routine
        [64]
--?         push qword[rsp]                         -- (leave the ret addr on stack)
            push rax
            mov rdx,routine_id(set_crash_routine)   -- mov edx,imm32 (sets K_ridt)
            mov rcx,$_Ltot                          -- mov ecx,imm32 (=symtab[set_crash_routine][S_Ltot])
            call :%opFrame
--          call :!diagFrame
--          add rsp,8
            pop qword[rbp]                          -- rid
--EXCEPT
--          mov qword[rbp+32],:crashrtnret
            mov qword[rbp+56],:crashrtnret
            jmp $_il                                -- jmp code:set_crash_routine
        []
          ::crashrtnret
            ret

--global procedure crash_routine(integer rid)
--procedure set_crash_routine(integer rid)
---- specify the routine id of a function to call in the event
---- that your program must be shut down due to an error. The
---- function should accept one parameter (currently always 0)
---- and return 0 to allow any other crash routines to run.
--  if rid=-1 then
--      crash_rtn = -1
--  elsif crash_rtn=-1 then
--      crash_rtn = {rid}
--  else
--      crash_rtn = append(crash_rtn,rid)
--  end if
--end procedure

--/*
procedure :%pThrow(:%)
end procedure -- (for Edita/CtrlQ)
--*/
    :%pThrow
--  :!pThrow
------------
        [32]
            -- calling convention
            --  mov eax,[e]         -- opUnassigned
            --  mov ecx,[user_data] -- (or h4)
            --  call :%pThrow       -- throw(e,user_data)
            --  int3
            cmp eax,h4
            jl @f
                add dword[ebx+eax*4-8],1
          @@:
            cmp ecx,h4
            jle @f
                add dword[ebx+ecx*4-8],1
          @@:
            push ecx                            -- [1] ref user_data
            push eax                            -- [2] ref e
            mov edx,routine_id(throw)           -- mov edx,imm32 (sets K_ridt)
            mov ecx,$_Ltot                      -- mov ecx,imm32 (=symtab[throw][S_Ltot])
            call :%opFrame
--          mov edx,[esp+8]
            pop dword[ebp]                      -- [2] e
            pop dword[ebp-4]                    -- [1] user_data
--EXCEPT
--          mov dword[ebp+16],:throwret
--          mov dword[ebp+28],:throwret         -- return address
            pop edx
            mov dword[ebp+28],edx               -- return address
            mov dword[ebp+12],edx               -- called from address
--          pop dword[ebp+12]                   -- called from address
            jmp $_il                            -- jmp code:convert_offset
--        ::throwret
--          pop edi                             --[1] addr res (an integer)
--          mov [edi],eax
        [64]
            mov r15,h4
            cmp rax,r15
            jl @f
                add qword[rbx+rax*4-16],1
          @@:
            cmp rcx,r15
            jle @f
                add qword[rbx+rcx*4-16],1
          @@:
            push rcx                            -- [1] ref user_data
            push rax                            -- [2] ref e
            mov rdx,routine_id(throw)           -- mov edx,imm32 (sets K_ridt)
            mov rcx,$_Ltot                      -- mov ecx,imm32 (=symtab[throw][S_Ltot])
            call :%opFrame
--          mov rdx,[rsp+16]
            pop qword[rbp]                      -- [2] e
            pop qword[rbp-8]                    -- [2] user_data
--EXCEPT
--          mov qword[rbp+32],:throwret         -- return address
--          mov qword[rbp+56],:throwret         -- return address
            pop rdx
            mov qword[rbp+56],rdx               -- return address
            mov qword[rbp+24],rdx               -- called from address
--          pop qword[rbp+24]                   -- called from address
            jmp $_il                            -- jmp code:convert_offset
--        ::throwret
--          pop rdi                             --[1] addr res (an integer)
--          mov [rdi],rax
        []
--          ret

--DEV DEAD
--  --for throw.e: [DEV, needs to be put into the optable]
--  --/*
--  procedure :%pConvertOffset(:%)
--  end procedure -- (for Edita/CtrlQ)
--  --*/
--  --/*
--      :!ConvertOffset
--  -------------------
--          [32]
--              -- calling convention
--              --  mov eax,[era]
--              --  mov esi,[sr]
--              --  lea edi,[lineno]        -- result (integer)
--              --  call :%pConvertOffset   -- [edi] := convert_offset(eax,ecx)
--              cmp eax,h4
--              jl @f
--                  add dword[ebx+eax*4-8],1
--            @@:
--              cmp esi,h4
--              jl @f
--                  add dword[ebx+esi*4-8],1
--            @@:
--              push dword[esp]                     -- [0] duplicate return address
--              push edi                            -- [1] addr res
--              push esi                            -- [2] ref sr
--              push eax                            -- [3] ref era
--              mov edx,routine_id(convert_offset)  -- mov edx,imm32 (sets K_ridt)
--              mov ecx,$_Ltot                      -- mov ecx,imm32 (=symtab[convert_offset][S_Ltot])
--              call :%opFrame
--              mov edx,[esp+12]
--              pop dword[ebp]                      -- [3] era
--              pop dword[ebp-4]                    -- [2] sr
--  --EXCEPT
--X             mov dword[ebp+16],:coret
--              mov dword[ebp+28],:coret            -- return address
--              mov dword[ebp+12],edx               -- called from address
--              jmp $_il                            -- jmp code:convert_offset
--            ::coret
--              pop edi                             --[1] addr res (an integer)
--              mov [edi],eax
--          [64]
--              mov r15,h4
--              cmp rax,r15
--              jl @f
--                  add qword[rbx+rax*4-16],1
--            @@:
--              cmp rsi,r15
--              jl @f
--                  add qword[rbx+rsi*4-16],1
--            @@:
--              push qword[rsp]                     -- [0] duplicate return address
--              push rdi                            -- [1] addr res
--              push rsi                            -- [2] ref sr
--              push rax                            -- [3] ref era
--              mov rdx,routine_id(convert_offset)  -- mov edx,imm32 (sets K_ridt)
--              mov rcx,$_Ltot                      -- mov ecx,imm32 (=symtab[convert_offset][S_Ltot])
--              call :%opFrame
--              mov rdx,[rsp+24]
--              pop qword[rbp]                      -- [3] era
--              pop qword[rbp-8]                    -- [2] sr
--  --EXCEPT
--X             mov qword[rbp+32],:coret            -- return address
--              mov qword[rbp+56],:coret            -- return address
--              mov qword[rbp+24],rdx               -- called from address
--              jmp $_il                            -- jmp code:convert_offset
--            ::coret
--              pop rdi                             --[1] addr res (an integer)
--              mov [rdi],rax
--          []
--              ret
--  --*/

--/*
procedure :!SetBatchMode(:%)
end procedure -- (for Edita/CtrlQ)
--*/
    :!SetBatchMode
        [32]
            mov eax,[esp]
            mov [batchmode],1
            jmp eax
        [64]
            mov rax,[rsp]
            mov [batchmode],1
            jmp rax
        []

--DEV/SUG (in the optable). Do we want called from or era or both?: callfunc:cf, cfunc:era(?), fileio:cf, palloc:cf
      :!fatalN
        -- calling convention
        --  mov ecx,imm32       -- no of frames to pop to obtain an era (>=1)
        --  mov al,imm          -- error code [1..length(msgs)-1, currently 122]
        -- (note: 64bit **//can//** use ecx/al above, but needs rdi/rsi below)
        --  mov edi,ep1         -- [optional] (opUnassigned)
        --  mov esi,ep2         -- [optional] (opUnassigned) [used for 110/ecx]
        --  jmp :!fatalN        -- fatalN(level,errcode,ep1,ep2)
        [32]
          @@:
--X         mov edx,[ebp+16]    -- era
--          mov edx,[ebp+28]    -- era
            mov edx,[ebp+12]    -- called from address
            mov ebp,[ebp+20]    -- (nb no local vars after this!)
            sub ecx,1
            jg @b
            sub edx,1
            mov ecx,esi
--          jmp :!iDiag         -- fatal error (see pdiagN.e)
--          int3
        [64]
          @@:
--X         mov rdx,[rbp+32]    -- era
--          mov rdx,[rbp+56]    -- era
            mov rdx,[rbp+24]    -- called from address
            mov rbp,[rbp+40]    -- (nb no local vars after this!)
--          sub rcx,1
            sub ecx,1
            jg @b
            sub rdx,1
            mov rcx,rsi
--          jmp :!iDiag         -- fatal error (see pdiagN.e)
--          int3
        []

--/*
procedure :!iDiag(:%)
end procedure -- (for Edita/CtrlQ)
--*/
    :!iDiag
-----------
        -- Invoked directly from point of error, al set to msg_id. 
        -- Instead of ecx,edx being the exception code and address,
        -- edx contains the era (effective return address), and 
        -- ep1,ep2 (if used) are error code dependent, but will be 
        -- in the other registers (so save them all).
        [32]
            and eax,#FF         -- error code (1..255)
            mov [msg_id],eax
            mov eax,edi         -- (store edi before we trash it!)
            lea edi,[or_edi]
            call :%pStoreMint   -- [or_edi]:=edi, as float if rqd
            mov eax,[msg_id]
            lea edi,[or_eax]
            call :%pStoreMint   -- [or_eax]:=eax (not useful here)
            mov eax,ecx
            lea edi,[or_ecx]
            call :%pStoreMint   -- [or_ecx]:=ecx
            xor eax,eax         -- edx is not available, unless first moved!
            lea edi,[or_edx]
            call :%pStoreMint   -- [or_edx]:=0
            mov eax,edx         -- era
            lea edi,[or_era]
            call :%pStoreMint   -- [or_era]:=edx
            mov eax,ebp
            shr eax,2
            mov [or_ebp],eax    -- [or_ebp]:=ebp/4
            mov eax,esp
            lea edi,[or_esp]
            call :%pStoreMint   -- [or_esp]:=esp
            mov eax,esi
            lea edi,[or_esi]
            call :%pStoreMint   -- [or_esi]:=esi
--?
            xor eax,eax
            lea edi,[xceptn]
            call :%pStoreMint   -- [xceptn]:=0
            lea edi,[xcepta]
            call :%pStoreMint   -- [xcepta]:=0
        [64]
            and rax,#FF         -- error code (1..255)
            mov [msg_id],rax
            mov rax,rdi         -- (store rdi before we trash it!)
            lea rdi,[or_edi]
            call :%pStoreMint   -- [or_edi]:=rdi, as float if rqd
            mov rax,[msg_id]
            lea rdi,[or_eax]
            call :%pStoreMint   -- [or_eax]:=rax (not useful here)
            mov rax,rcx
            lea rdi,[or_ecx]
            call :%pStoreMint   -- [or_ecx]:=rcx
            xor rax,rax         -- rdx is not available, unless first moved!
            lea rdi,[or_edx]
            call :%pStoreMint   -- [or_edx]:=0
            mov rax,rdx         -- era
            lea rdi,[or_era]
            call :%pStoreMint   -- [or_era]:=rdx
            mov rax,rbp
            shr rax,2
            mov [or_ebp],rax    -- [or_ebp]:=rbp/4
            mov rax,rsp
            lea rdi,[or_esp]
            call :%pStoreMint   -- [or_esp]:=rsp
            mov rax,rsi
            lea rdi,[or_esi]
            call :%pStoreMint   -- [or_esi]:=rsi
--?
            xor rax,rax
            lea rdi,[xceptn]
            call :%pStoreMint   -- [xceptn]:=0
            lea rdi,[xcepta]
            call :%pStoreMint   -- [xcepta]:=0
        []
            cmp [diaglooping],0
            je @f
                [32]
                    mov edi,[ecode]             -- "diag looping, error code is "
                    call :%puts1
                    mov edx,[msg_id]
                    push 0
--                  call :%puthex32a
                    call :%putsint
                    mov edi,[erais]             -- ", erais #"
                    call :%puts1
                    mov edx,[or_era]
                    push 1
                    call :%puthex32
                    mov eax,1
                [64]
                    mov rdi,[ecode]             -- "diag looping, error code is "
                    call :%puts1
                    mov rdx,[msg_id]
                    push 0
--                  call :%puthex32a
                    call :%putsint
                    mov rdi,[erais]             -- ", erais #"
                    call :%puts1
                    mov rdx,[or_era]
                    push 1
                    call :%puthex32
                    mov rax,1
                []
--                  add [diaglooping],1
--                  cmp [diaglooping],2
                    mov ecx,[diaglooping]
                    add ecx,1
                    mov [diaglooping],ecx
                    cmp ecx,2
                    je @f
                        jmp :%opAbort
          @@:               
            jmp :alset


    ::DiagLooping -- (because jne does not support memory operand)
        [32]
            jmp dword[esp]
        [64]
            jmp qword[rsp]
        []

--/*
procedure :!fehDiag(:%)
end procedure -- (for Edita/CtrlQ)
--*/
    :!fehDiag
----------
            -- from pFEH.e (and nowhere else), msg_id not yet known
            -- Aside: the context record offsets really belong in pFEH.e, but
            --        the (local hll variables) or_ecx, etc belong in here.
            cmp [diaglooping],2
            jge :DiagLooping
        [PE32]
            --  esi is context record (an annoted copy can be found in pFEH.e)
            --  edx is exception address
            --  ecx is exception code
            -- (ebp already reset, and ebx zeroed)
            mov eax,ecx         -- exception code
            lea edi,[xceptn]
            call :%pStoreMint
            mov eax,edx         -- exception address
            lea edi,[xcepta]
            call :%pStoreMint
            lea edi,[or_era]    -- (may get replaced)
            call :%pStoreMint
            mov eax,[esi+176]   -- eax
            lea edi,[or_eax]
            call :%pStoreMint
            mov eax,[esi+172]   -- ecx
            lea edi,[or_ecx]
            call :%pStoreMint
            mov eax,[esi+168]   -- edx
            lea edi,[or_edx]
            call :%pStoreMint
            mov eax,[esi+156]   -- edi
            lea edi,[or_edi]
            call :%pStoreMint
            mov eax,[esi+180]   -- ebp
            shr eax,2
            mov [or_ebp],eax
            mov eax,[esi+196]   -- esp
            lea edi,[or_esp]
            call :%pStoreMint
            mov eax,[esi+160]   -- esi
            lea edi,[or_esi]
            call :%pStoreMint
--156 or_edi            dd ?
--160 or_esi            dd ?
--164 or_ebx            dd ?
--168 or_edx            dd ?
--172 or_ecx            dd ?
--176 or_eax            dd ?
--180 or_ebp            dd ?
--184 or_eip            dd ?
--196 or_esp            dd ?
--          mov esp,[esi+196]   -- (restore) [already done in pFEH.e]
--          mov ebp,[esi+180]   -- (restore) [already done in pFEH.e]
            xor eax,eax
        [PE64]
            --  rsi is context record (an annoted copy can be found in pFEH.e)
            --  rdx is exception address
            --  rcx is exception code
            -- (rbp already reset, and rbx zeroed)
            mov rax,rcx         -- exception code
            lea rdi,[xceptn]
            call :%pStoreMint
            mov rax,rdx         -- exception address
            lea rdi,[xcepta]
            call :%pStoreMint
            lea rdi,[or_era]    -- (may get replaced)
            call :%pStoreMint
            mov rax,[rsi+120]   -- rax
            lea rdi,[or_eax]
            call :%pStoreMint
            mov rax,[rsi+128]   -- rcx
            lea rdi,[or_ecx]
            call :%pStoreMint
            mov rax,[rsi+136]   -- rdx
            lea rdi,[or_edx]
            call :%pStoreMint
--/*
120 DWORD64 Rax;
128 DWORD64 Rcx;
136 DWORD64 Rdx;
144 DWORD64 Rbx;
152 DWORD64 Rsp;
160 DWORD64 Rbp;
168 DWORD64 Rsi;
176 DWORD64 Rdi;
184 DWORD64 R8;
192 DWORD64 R9;
200 DWORD64 R10;
208 DWORD64 R11;
216 DWORD64 R12;
224 DWORD64 R13;
232 DWORD64 R14;
240 DWORD64 R15;
248 DWORD64 Rip;
--*/
            mov rax,[rsi+176]   -- rdi
            lea rdi,[or_edi]
            call :%pStoreMint
            mov rax,[rsi+160]   -- rbp
            shr rax,2
            mov [or_ebp],rax
            mov rax,[rsi+152]   -- rsp
            lea rdi,[or_esp]
            call :%pStoreMint
            mov rax,[rsi+168]   -- rsi
            lea rdi,[or_esi]
            call :%pStoreMint
--          mov rsp,[rsi+152]   -- (already done in pFEH.e)
--          mov rbp,[rsi+160]   -- (already done in pFEH.e)
            xor rax,rax
            mov r15,h4
        [ELF32]
            --  esi is context record (an annoted copy can be found in pFEH.e)
            --  edx is exception address
--          --  ecx is exception code (would always be SIGSEGV)
            -- (ebp already reset, and ebx zeroed)
--          mov eax,ecx         -- exception code
            xor eax,eax
            lea edi,[xceptn]
            call :%pStoreMint
            mov eax,edx         -- exception address
            lea edi,[xcepta]
            call :%pStoreMint
            lea edi,[or_era]    -- (may get replaced)
            call :%pStoreMint
            mov eax,[esi+64]    -- eax
            lea edi,[or_eax]
            call :%pStoreMint
            mov eax,[esi+60]    -- ecx
            lea edi,[or_ecx]
            call :%pStoreMint
            mov eax,[esi+56]    -- edx
            lea edi,[or_edx]
            call :%pStoreMint
            mov eax,[esi+36]    -- edi
            lea edi,[or_edi]
            call :%pStoreMint
            mov eax,[esi+44]    -- ebp
            shr eax,2
            mov [or_ebp],eax
            mov eax,[esi+48]    -- esp
            lea edi,[or_esp]
            call :%pStoreMint
            mov eax,[esi+40]    -- esi
            lea edi,[or_esi]
            call :%pStoreMint
--      .edi           rd 1     ;36
--      .esi           rd 1     ;40
--      .ebp           rd 1     ;44
--      .esp           rd 1     ;48
--      .ebx           rd 1     ;52
--      .edx           rd 1     ;56
--      .ecx           rd 1     ;60
--      .eax           rd 1     ;64
--      .trapno        rd 1     ;68
--      .err           rd 1     ;72
--      .eip           rd 1     ;76 (correct)
--      .cs            rw 1     ;80
--      .__csh         rw 1     ;82
--      .eflags        rd 1     ;84
--      .esp_at_signal rd 1     ;88
            xor eax,eax
--          pop al
        [ELF64]
            --  rsi is context record (an annoted copy can be found in pFEH.e)
            --  rdx is exception address
--          --  rcx is exception code (would always be SIGSEGV)
            -- (rbp already reset, and rbx zeroed)
--          mov rax,rcx         -- exception code
            xor rax,rax
            lea rdi,[xceptn]
            call :%pStoreMint
            mov rax,rdx         -- exception address
            lea rdi,[xcepta]
            call :%pStoreMint
            lea rdi,[or_era]    -- (may get replaced)
            call :%pStoreMint

            mov rax,[rsi+0x90]  -- rax
            lea rdi,[or_eax]
            call :%pStoreMint
            mov rax,[rsi+0x98]  -- rcx
            lea rdi,[or_ecx]
            call :%pStoreMint
            mov rax,[rsi+0x88]  -- rdx
            lea rdi,[or_edx]
            call :%pStoreMint
            mov rax,[rsi+0x68]  -- rdi
            lea rdi,[or_edi]
            call :%pStoreMint
            mov rax,[rsi+0x78]  -- rbp
            shr rax,2
            mov [or_ebp],rax
            mov rax,[rsi+0xA0]  -- rsp
            lea rdi,[or_esp]
            call :%pStoreMint
            mov rax,[rsi+0x70]  -- rsi
            lea rdi,[or_esi]
            call :%pStoreMint
--          mov rsp,[rsi+0xA0]  -- (already done in pFEH.e)
--          mov rbp,[rsi+0x78]  -- (already done in pFEH.e)
            xor rax,rax
            mov r15,h4
        []

--/*
procedure AddressMapping()
end procedure -- (for Edita/CtrlQ)
--*/
 -- Address Mapping
 ------------------
            -- (all label addresses are expected to be <1G)
            -- (DEV factor out common code once it all works,
            --      and once terror.exw is up and running.)
            mov al,1
--          mov al,2    -- now via :!iDiag
--          cmp edx,:%e02atdb0
--          je :alset
            cmp edx,:!Jccp2Intp3Ref
            jne @f
              [32]
                mov eax,[esp+4]
                lea edi,[or_era]
                sub eax,1
              [64]
                mov rax,[rsp+8]
                lea rdi,[or_era]
                sub rax,1
              []
                call :%pStoreMint
                mov al,92           -- e92vhnbaav(esi)
                jmp :setal
          @@:
            cmp edx,:!opXore92a
            jne @f
              [32]
                mov eax,[esp+4]
                lea edi,[or_era]
                sub eax,1
              [64]
                mov rax,[rsp+8]
                lea rdi,[or_era]
                sub rax,1
              []
                call :%pStoreMint
                mov al,91           -- e91vhnbaav(ecx) (ecx is var no)
                jmp :setal
          @@:
            cmp edx,:%opPpndSA
            jne @f
              [32]
                mov eax,[esp+20]
                lea edi,[or_era]
                sub eax,1
              [64]
                mov rax,[rsp+40]
                lea rdi,[or_era]
                sub rax,1
              []
                call :%pStoreMint
                mov al,93           -- e93vhnbaav(edi) (edi is var no)
                jmp :setal
          @@:

            cmp edx,:!opLene36or92
            jne @f
              [32]
                mov edi,[a32h4]
                mov eax,[or_esi]
              [64]
                mov rdi,[a64h4]
                mov rax,[or_esi]
              []
                call :%opJccE
                je :e94vhnbaavedx
              [32]
                mov eax,[esp+4]
                lea edi,[or_era]
                sub eax,1
                add esp,8
              [64]
                mov rax,[rsp+8]
                lea rdi,[or_era]
                sub rax,1
                add rsp,16
              []
                call :%pStoreMint
                mov al,36           -- e36loaaind
                jmp :setal
          @@:
            cmp edx,:!pSubsse94
            jne @f
              [32]
                mov ecx,[or_ecx]
                lea esp,[esp+ecx*4+4]
              [64]
                mov rcx,[or_ecx]
                lea rsp,[rsp+rcx*8*8]
              []
                jmp :e94vhnbaavedx
          @@:
            cmp edx,:!opSubse1e04or92
            je :e94_or_e04
            cmp edx,:!opSubse1ipRe92a
            je :e94vhnbaavedx
            cmp edx,:!Jccp2NotInt
            je :e94vhnbaavedx
            cmp edx,:!Jife92
            je :e94vhnbaavedx
            cmp edx,:!opJnotxe92a
            je :e94vhnbaavedx
            cmp edx,:!opXore92b
            je :e94vhnbaavedx
            cmp edx,:%pSubsss
            je :e94vhnbaavedx
            cmp edx,:!opSubse1iRe92a
            je :e94_or_e04
            cmp edx,:!opSubse1isRe92a
            jne @f
              ::e94_or_e04
              [32]
                mov edi,[a32h4]
                mov eax,[or_esi]
              [64]
                mov rdi,[a64h4]
                mov rax,[or_esi]
              []
                call :%opJccE
                je :e94vhnbaavedx
              [32]
                mov eax,[esp+4]
                lea edi,[or_era]
                sub eax,1
--31/7/17 (*4)
                add esp,8
              [64]
                mov rax,[rsp+8]
                lea rdi,[or_era]
                sub rax,1
                add rsp,16
              []
                call :%pStoreMint
                mov al,04           -- e04atsaa
                jmp :setal

          ::e94vhnbaavedx
              [32]
                mov eax,[esp+4]
                lea edi,[or_era]
                sub eax,1
                add esp,8
              [64]
                mov rax,[rsp+8]
                lea rdi,[or_era]
                sub rax,1
                add rsp,16
              []
                call :%pStoreMint
                mov al,94           -- e94vhnbaav(edx)
                jmp :setal
          @@:
--/*
--;  2352           mov ecx,[esp+8]         -- pTCB/4
--                  mov [edi+ecx*4+20],esi  ;#004D2FEE: 211164217 14               uv 00 C2  1 651      
--
--;  2378           mov rcx,[rsp+64]        -- pTCB/4
--;  2391           mov [rcx*4+rdi+32],rsi  -- pTCB[idx]:=this.pNext
--
--;  1876       mov eax,[esi*4+edi+20]  -- pTCB.pFree[idx]
--;  1878       jnz :!blockfound
--*/
            cmp edx,:!blockfound
            je :e32hcblockfound
--cmp edx,:!blockfound4
--je :e32hcblockfound
            cmp edx,:!blockfoundC0000005
            je :e32hcblockfound
            cmp edx,:!bf_midchain
            jne @f
          ::e32hcblockfound
                -- memory corruption problem
              [32]
--              mov eax,[esp+12]    -- return address (of :%pGetPool call)
--              mov eax,[esp+12]
                mov eax,[esp+16]
                lea edi,[or_edi]
                sub eax,1
              [64]
--              mov rax,[rsp+8*11]  -- return address (of :%pGetPool call)
--              mov rax,[rsp+80]
                mov rax,[rsp+88]
                lea rdi,[or_edi]
                sub rax,1
              []
--              cmp edx,
--pAllocStr
--pAllocSeq
--pStoreFlt
--pAlloc
--newVSB
--allocate()
                call :%pStoreMint
--              mov [or_ecx],1
                mov al,32           -- e32hc(era,edi)
                jmp :setal
          @@:
            cmp edx,:!GetPoolnotTCBa
            jne @f
              [32]
--              mov eax,[esp+8]
                mov eax,[esp+12]
                lea edi,[or_edi]
                sub eax,1
              [64]
--              mov rax,[rsp+80]
                mov rax,[rsp+88]
                lea rdi,[or_edi]
                sub rax,1
              []
                call :%pStoreMint
                mov al,32           -- e32hc(era,edi)
                jmp :setal
          @@:
            cmp edx,:!FreePoole32a
            jne @f
              [32]
                mov eax,[esp+4]
                lea edi,[or_edi]
                sub eax,1
              [64]
                mov rax,[rsp+8]
                lea rdi,[or_edi]
                sub rax,1
              []
                call :%pStoreMint
                mov al,32           -- e32hc(era,edi)
                jmp :setal
          @@:
            [32]
            cmp edx,:!opPeek1xMLE
            je :e99atom
            cmp edx,:!opPeek2xMLE
            je :e99atom
            cmp edx,:!opPeek4xMLE
            je :e99atom
            cmp edx,:!opPeek8xsMLE
            je :e99atom
            cmp edx,:!opPeek8xuMLE
            jne @f
              ::e99atom
--              mov eax,[esp+8]
                mov eax,[esp+12]
                lea edi,[or_era]
                sub eax,1
                call :%pStoreMint
                mov al,99           -- e99ipma
                jmp :setal
          @@:
            cmp edx,:!opPeek1xsMLE
            je :e99seq
            cmp edx,:!opPeek2xsMLE
            je :e99seq
            cmp edx,:!opPeek4xsMLE
            je :e99seq
            cmp edx,:!opPeek8xsMLE2
            je :e99seq
            cmp edx,:!opPeek8xuMLE2
            jne @f
              ::e99seq
--              mov eax,[esp+16]
                mov eax,[esp+20]
                lea edi,[or_era]
                sub eax,1
                call :%pStoreMint
                mov al,99           -- e99ipma
                jmp :setal
            [64]
            cmp edx,:!opPeek1xMLE64
            je :e99atom
            cmp edx,:!opPeek2xMLE64
            je :e99atom
            cmp edx,:!opPeek4xMLE64
            je :e99atom
            cmp edx,:!opPeek8xsMLE64
            je :e99atom
            cmp edx,:!opPeek8xuMLE64
            jne @f
              ::e99atom
--              mov rax,[rsp+16]
                mov rax,[rsp+24]
                lea rdi,[or_era]
                sub rax,1
                call :%pStoreMint
                mov al,99           -- e99ipma
                jmp :setal
          @@:
            cmp edx,:!opPeek1xsMLE64
            je :e99seq
            cmp edx,:!opPeek2xsMLE64
            je :e99seq
            cmp edx,:!opPeek4xsMLE64
            je :e99seq
            cmp edx,:!opPeek8xsMLE264
            je :e99seq
            cmp edx,:!opPeek8xuMLE264
            jne @f
              ::e99seq
--              mov rax,[rsp+32]
                mov rax,[rsp+40]
                lea rdi,[or_era]
                sub rax,1
                call :%pStoreMint
                mov al,99           -- e99ipma
                jmp :setal
            []
          @@:
            cmp edx,:!PokeN1E30
            je :e100ipma
            cmp edx,:!PokeN2E30
            je :e100ipma
            cmp edx,:!PokeN4E30
            je :e100ipma
            cmp edx,:!PokeN8E30
            je :e100ipma
            cmp edx,:!Poke1SeqE30
            je :e100ipma
            cmp edx,:!Poke2SeqE30
            je :e100ipma
            cmp edx,:!Poke3SeqE30
            je :e100ipma
            cmp edx,:!Poke8SeqE30
            je :e100ipma
            [32]
            cmp edx,:!Poke1SeqE30a
            je :e100ipma
            []
            cmp edx,:!PokeN1StrE30
            je :e100ipma
            cmp edx,:!PokeN2StrE30
            je :e100ipma
            cmp edx,:!PokeN4StrE30
            je :e100ipma
            cmp edx,:!PokeN8StrE30
            jne @f
              ::e100ipma
            [32]
                mov eax,[esp+4]
                lea edi,[or_era]
                sub eax,1
            [64]
                mov rax,[rsp+8]
                lea rdi,[or_era]
                sub rax,1
            []
                call :%pStoreMint
                mov al,100          -- e100ipma
                jmp :setal
          @@:
            cmp edx,:%pFree_e107ifma
            jne @f
            [32]
                mov eax,[esp+4]
                lea edi,[or_era]
                sub eax,1
            [64]
                mov rax,[rsp+8]
                lea rdi,[or_era]
                sub rax,1
            []
                call :%pStoreMint
                mov al,107          -- e107ifma
                jmp :setal
          @@:
            cmp edx,:!MemCopyIMA
            jne @f
            [32]
                mov eax,[esp+4]
                lea edi,[or_era]
                sub eax,1
            [64]
                mov rax,[rsp+8]
                lea rdi,[or_era]
                sub rax,1
            []
                call :%pStoreMint
                mov al,24           -- e24imcma
                jmp :setal
          @@:
            mov al,30
    ::setal
        [32]
            and eax,#FF
            mov [msg_id],eax
        [64]
            and rax,#FF
            mov [msg_id],rax
        []

    ::alset
        call :%NoCleanup
          }
--puts(1,"uh0?\n")
    if diagdiag>0 
    and show_low_level_diagnostics then
        if xceptn=0 then
--DEV Equivalent changes should probably be applied elsewhere (in this particular case 
--      heap corruption meant it crashed in printf before it could display anything).
--      A related idea could be that pHeap.e invokes :!diagHC (new) to set a flag to
--      use low-level displays instead of (more readable) printfs, and/or displays the
--      call stack without any (corrupted) variables that would make pDiagN.e crash.
--      [or maybe "" if find(msg_id,{31,32,33,53,77})!=0...]
--          printf(1,"error code %d, or_era=#%08x, or_ebp=#%08x, or_esp=#%08x\n",
--                 {msg_id,or_era,or_ebp*4,or_esp})
            puts1("error code ")
            putsint(msg_id,0)
            puts1(", or_era=#")
            puthex32(or_era,0)
            puts1(", or_ebp=#")
            puthex32(or_ebp*4,0)
            puts1(", or_esp=#")
            puthex32(or_esp,1)
        else
--          show_corruption(CTB)
            printf(1,"exception #%08x at #%08x, or_era=#%08x, or_ebp=#%08x, or_esp=#%08x\n",
                   {xceptn,xcepta,or_era,or_ebp*4,or_esp})
        end if
-->++ (regs)
        printf(1,"stack(#%08x): %08x, %08x, %08x, %08x\n",or_esp&peek4u({or_esp,4}))
        printf(1,"stack(#%08x): %08x, %08x, %08x, %08x\n",or_esp+16&peek4u({or_esp+16,4}))
        printf(1,"stack(#%08x): %08x, %08x, %08x, %08x\n",or_esp+32&peek4u({or_esp+32,4}))
        printf(1,"stack(#%08x): %08x, %08x, %08x, %08x\n",or_esp+48&peek4u({or_esp+48,4}))
    end if

    diag()
    -- control does not return... [DEV]
puts(1,"uh? (pdiagN.e line 4791)\n")
--DEV standard problem... must fix this one day...
    msg_id += 1
    batchmode = 1
--  xceptn += 1
--  #ilASM{
--      [32]
--          mov eax,[esp]
----            mov edx,[batchmode]
--          jmp eax
--      [64]
--          mov rax,[rsp]
----            mov rdx,[batchmode]
--          jmp rax
--      []
--        }


--end of new code
--===============
---- "without debug" stops the run-time update of line and file info, and
---- this program from dumping local vars, which in this case would be [DEV?]
---- just crash_rtn, crash_msg, crashfile, and stoploop. While it remains
---- quite sensible to specify this here, it should be perfectly OK to 
---- turn it back on for a while rather than play completely in the dark.
---- If you get any problems, comment this line out, suffer the 4 extra 
---- lines in your .err, some pretty wierd and wacky misleading line nos
---- (eg the line in this source rather than where the user app actually 
----  crashed), and hopefully/maybe get a better clue where it 
---- (this, ie pdiag.e) went wrong ;-)):
--
----/**/without debug -- Phix: disables all debug/diagnostic stuff!
--------/**/with debug
--without type_check
------with trace
--
--
----without trace   -- NB no effect under "without debug"
----with trace      -- NB no effect under "without debug"
--
--
---- TODO: 
---- "Traced lines leading up to the failure:", eg:
----
----C:\Program Files\Phix\test.exw:6    procedure d(sequence s)
----C:\Program Files\Phix\test.exw:8        d(s[2..length(s)-1])
----C:\Program Files\Phix\test.exw:6    procedure d(sequence s)
----C:\Program Files\Phix\test.exw:8        d(s[2..length(s)-1])
----
---- (Personally, though, I've never found that much use)
--
--constant binderrs = {
--"infile is not string",                   --1 --*
--"error opening infile (p.exe)",           --2 -- in use? hard disk problems?
--"error seeking to infile eof",                --3 --          ""
--"error allocating sizeof(p.exe|new.exe)", --4 -- insufficient memory? (2MB should be plenty!)
--"error reading p.exe",                        --5 -- as 2,3
--"MZ header not found",                        --6 -- p.exe corrupt? hard disk problems?
--"PE header not found",                        --7 -- ""               ""
--"subvers not atom",                       --8 --*
--"length(optable)!=length(opNames)",       --9 -- [details already shown]
--"calcsize!=dumpsize",                     --10 -- serious! will need soure to reproduce
--"incorrect image size",                   --11    ""
--"outfile not string",                     --12 --*
--"error writing new.exe",                  --13 -- in use? hard disk problems?
--"sig not sequence"}                       --14 --*
---- items marked --* should not happen (unless p.exw sources badly hacked)
--
--
--constant rtndescs = {"type","function", "procedure"}
--
--object crashfile crashfile = -1
--
--integer stoploop      -- this is independently tested for in the backend...
--      stoploop = 0        --  (but it does not hurt any to re-test it here)
--
--function diag(integer msg_id)
-- removed from e01:
    -- Note: since the diag routine uses some of the builtins,
    --  then eg object o o="fred" getc(o) will not generate 
    -- 'type check error, fn is "fred"', but instead
    -- 'type check error, getc parameter fn is wrong'. [?DEV I may have fixed this since?]
    --  [as opposed to getc("fred"), which causes compile-time error]
    -- When you see "builtin parameter", look up the routine
    --  in the documentation to find out exactly which 
    --  parameter it is referring to.
    -- For more details, also see variable builtinparamwrong.
--integer builtinparamwrong
--integer k, rtn, callee, fileno, pathno, noofparams, i2, i3, i4, km1, km2, km3, kp1
--integer lineno
--object si, codeseg, o, o2, name
--integer tidx, idx
--integer pn
--object linetab
--integer line
--object ugh --DEV!
--
------  puts(1,"diag called...\n")
--------/**/if getc(0) then end if
--  if stoploop then
--      -- this is independently tested for in the backend...
--      --  (flag is intended to catch errors in pdiag.e itself)
----        puts(1,"diag looping!\n")
--      puts(1,"diag looping!\n")
----        stoploop = 2
----    elsif stoploop=2 then
----        puts(1,"diag re-looping!\n")
--      return 0
----        if getc(0) then end if
----        abort(0)
----    else
--  end if
--  stoploop=1
--
------  puts(1,"diag called2...\n")
--
--  if equal(crashfile,"") then return 0 end if
----    if equal(crashfile,"") then abort(1) end if
--
--  builtinparamwrong = 0
--  callcount = 0
--  if find(crashfile,{"NUL","/dev/null"}) then
--      pn = -1
----DEV: if we can't open .err, display to screen anyway...
----        [I think we're OK, but not tested, let me know if it works as it should]
--  elsif crashfile!=-1 then
--      pn = open(crashfile,"w")
--  else
--      pn = open("p.err","w")
--  end if
--
--  while 1 do
--      while 1 do
--              callcount += 1
--                  if msg_id<=length(msgs) then
--                      i2 = ep1  i3 = ep2
--                      elsif msg_id = 108 then
--                          -- position error [%08x]
--                          o = {i2}
--                      elsif msg_id = 98 then
--                          -- flush error [%08x]
--                          -- add human-readables for common ones as follows:
--                          -- (see eg arwen/Constants.ew, ERROR_INVALID_HANDLE etc)
----                            if i2=? then
----                                o = {i2," desc"}
----                            elsif i2=? then
----                                o = {i2," desc"}
----                            else
--                              o = {i2,""}
----                            end if
--                      elsif msg_id = 62
--                         or msg_id = 58
--                         or msg_id = 7 then
--                          -- file number %d is not open
--                          -- invalid file number (%d)
--                          -- slice start is less than 1 (%d)
--                          o = {i2}
--                      elsif msg_id = 6
--                         or msg_id = 106 then
--                          -- index %d out of bounds, assigning to sequence length %d
--                          -- index %d out of bounds, reading from sequence length %d
--                          o = {i2,i3}
--                      elsif msg_id >= 9 
--                        and msg_id <= 11 then
--                          -- slice length is negative (%d..%d)
--                          -- slice starts past end of sequence (%d > %d)
--                          -- slice ends past end of sequence (%d > %d)
--                          o = {i2,i3}
--                      elsif msg_id = 116 then
--                          -- routine requires %d parameters, not %d
--                          o = {i2,i3}
--                      elsif msg_id = 1
--                      or msg_id = 110 then
--                          if symtab[i2][S_NTyp] = S_TVar3 then
--                              i4 = -symtab[i2][S_Tidx]
--                          else
--                              o = symtab[i2][S_value] (DEV)
--                          end if
--                          if i3=0 then
--                              o = {"<diag.e: oops, i3 is zero>",o}
--                          elsif builtinparamwrong then
--                              o = {symtab[builtinparamwrong][S_Name]&" parameter "&
----DEV: (testme!)
----                                     symtab[i3][S_Name],"wrong"}
--                                   symtab[i3][S_Name],o}
--                          else
--                              o = {symtab[i3][S_Name],o}
--                          end if
--
--                      elsif msg_id = 93 then
--                          -- bind error %d (%s)
--                          o = {i2,binderrs[i2]}
--                      else
--                          o = 0
--                      end if
--                      put2(pn,msgs[msg_id],o)
--                  else
--                      put2(pn,"unknown error code %d\n",{msg_id})
--                  end if
--
--          elsif msg_id=1 then
--              -- Instead of 'type check error, fn is "fred"', for
--              -- the builtins, which we are likely to use all the
--              -- time and hence trash any "current value", output
--              -- 'type check error, builtin parameter fn is wrong'
--              --  (user is expected to lookup "fn" in the docs)
--              -- FWIW, RDS Eu tends to output routine-specific
--              -- messages, eg "first parameter to match must be 
--              -- a sequence", "file number is not an integer",
--              -- and likwise not show any "current value".
--              builtinparamwrong=rtn
--          end if
--
--flush(pn)
--      end while
--  end while
--flush(pn)
--
--  if pn!=-1 and callcount>100 then
--      printf(pn,"  (skipping %d levels)\n",callcount-100)
--  end if
------  puts(1,"hey7!\n")
--
--  fileno = 0
--
--  if pn!=-1 then
--      puts(pn,"\nGlobal & Local Variables\n")
--
--      for i=1 to length(symtab) do
--      end for
--      puts(1,"\n")
--      close(pn)
--  end if
--  if crash_rtn!=-1 then
--      for i=length(crash_rtn) to 1 by -1 do
--          if call_func(crash_rtn[i],{0})!=0 then exit end if
--      end for
--  end if
----puts(1,"done!\n")
----abort(1)
----if getc(0) then end if
----if stoploop then abort(1) end if
--  stoploop = 0
--  return 0
--end function
--
--
--
--procedure setup()
--  diagcb = routine_id("diag") --  if diagcb <= 0 then puts(1,"diagcb <= 0\n") abort(1) end if
----?diagcb
--  diagcb = call_back(diagcb)  --  if diagcb = -1 then puts(1,"callback = -1\n") abort(1) end if
----?diagcb
--
--  #ilASM{mov edi,[diagcb]
--         call %opCrshRtn}
--end procedure
--if 01 then
--setup()
--end if
--

--          #ilASM{ jmp :%pRetf     -- (there's one of these at every eof)
--                  :%somelabel
--                }
--          puts(1,"hello from somelabel\n")  -- [keep this short/simple]
--          #ilASM{ ret }
--          <EOF>


--  #ilASM{ jmp :fin
----        [32]
----DEV:
--          :%e01tcfAddiii
----            :%e02atdb0  -- (DEV moved to pUnassigned for now)
----            :%pUnassigned   -- aka e92vhnbaavesiesp
--              -- var no(?) in esi
----            :%pRTErn
----?       :%pRTErf
--              -- errorcode in al
--              -- ep1 in e/rdi
--              -- ep2 in e/rsi
----?           -- use the called from address ([e/rbp+12/24])[? just let without debug take care of it... ?]
----?           -- note that e/rbp may need to be specially set by the callee (eg to ebp_prev),
----?           --  since there is no point in say pfileioN.e reporting errors in iofatal().
----                mov e??,[ebp+12]    -- called from address
----                mov r??,[rbp+24]    -- called from address
--          int3
----        [64]
----            pop al
----        []
--          ::fin
--        }
