
Phix readme.txt
===============

The windows installer (phix.setup.exe) defaults to C:\Program Files\Phix, though 
you can change that. Alternatively you can use 7zip, and probably most other file 
compression utilities, to extract the contents of phix.setup.exe, or equivalently
phix1.zip .. phix3.zip, to any directory of your choice.

**DEV**: this has changed with pdemo.exw and needs rewriting:
Installation does not modify anything in the system registry, nor does it create 
any start menu or desktop shortcuts, or set the PATH environment variable. 
The recommended post-installation setup is to manually associate *.exw files with 
pw.exe using the "open with" option in Windows Explorer. Personally I never set 
PATH, EUINC, or EUDIR, though they can be used if you want.

If using the windows installer, it should create pw.exe automatically for you.

However, you must manually inspect/run ppw.bat if extracting files by hand - it
is currently just 3 lines and the last line (p -c pdemo) is technically optional.

If you run "p -c p", to recompile the compiler from the supplied sources, then
it will create both a new p.exe and a new pw.exe, again automatically for you.

Please note the following entries are probably more for my benefit than yours.

Version 1.0.0
=============
03/02/2021: Bugfix: IupMessage crashed if msg was NULL. Added bWrap option to
            both IupMessage() and IupMessageError().
04/02/2021: Bugfix: User defined types within classes not working. Given say
            class Element Ihandle handle, the "Ihandle" was being pushed via
            addUnnamedConstant(), when it obviously needed addRoutineId().
04/02/2021: Bugfix: delete_routine(x,0) was not removing the routine.
04/02/2021: Icallback() was creating multiple instances of a callback, even
            though it was keeping (3) tables to avoid precisely that...
            Not a big deal, since call_back(), which obviously Icallback()
            wraps, already checked for and re-used prior instances anyway.
07/02/2021: Bugfix: unique(x,STABLE) was not using the dictionary tid, and
            hence returning the entire original list... Belated test added.
16/02/2021: Fixed trace on linux
17/02/2021: Enhancement: Added a hll goto statement, and documented it.
18/02/2021: Created builtins\pipeio.e from guts of demo\capture_console.exw
23/02/2021: Moved && and || up the precedence table above relops, matching
            Python and now disagreeing with C/JavaScript. For example,
            a && b != c was a && (b != c), but is now (a && b) != c. Note
            a and b != c is however still a and (b != c). t66 tweaked.
03/03/2021: vslice() can now accept a pair of column indices, so it can now
            take an actual slice as it always should have been able to.
            (Needed/wanted for a rewrite of demo/rosetta/viewppm.exw)
23/05/2021: Removed the pointless restriction that rid() only worked when
            rid was specifically of type integer, which means you can now
            do things like s[FUNC](1,2,3) - and of course we can rely on
            call_func/proc() to crash when s[FUNC] ain't an integer/rid, 
            so it is not about to "do weird things" as initially feared.


Version 0.8.3
=============
22/12/2020: Bugfix: "class x string name procedure name()" now produces the
            error "invalid type". Note that all methods are "virtual" in the
            sense they can be overidden, ideally it would check that it was
            only overriding something from an "extends" clause, but that is
            not particularly simple. Instead it is saying it cannot override 
            a string with a procedure. Likewise it will trigger an error if
            you try to override a procedure with a function and vice versa.
            I would have liked to put signature checks in there, but alas
            that information is not held (think "pure virtual"), or rather 
            it is quite a bit more difficult to dig out.
24/12/2020: BUGFIX: in something (well, anything!) like the following
                sequence x = repeat(0,rand(50))
                if <condition> then
                    x = {} -- (or any other [fixed] length)
                end if -- <<== **bad mergeBlocks()/mergeTypes() here !!!**
                if length(x)=0 then ?9/0 end if -- <<== uses if 0=0 then!!
            pltype.e was replacing/merging the lslen[prev] of -1 (aka
            "no idea yet") with the lslen[curr] of 0, which meant it would
            think x was length 0 (or whatever) even after condition failed.
            It now leaves it alone (the last if now /looks/ at x's length).
            Amazed this was not caught years ago, and even more amazed that 
            p -test didn't start /any/ bitchin...
02/01/2021: Replaced sys_exit with sys_exit_group, for proper linux s/down.
08/01/2021: Moved initialisation in sha256.e to permit forward calls.
10/01/2021: Added hideScope/restoreScope for nested functions, which hide
            anything and everything that would otherwise require a closure.
            Note that some work remains outstanding in this area.
21/01/2021: Tightened up on builtin overrides, to prevent such nonsense as
            integer integer, atom atom, string string, and so on. constant
            SLASH was a bit of an innocent bystander, though you/I should
            probably be using the builtin constant anyway.
21/01/2021: BUGFIX: Ctrl [] in Edita/Edix now correctly skip backtick strs.
21/01/2021: Edita: easinst.ew now uses GetWindowText, should be better.
21/01/2021: BUGFIX: Edix now uses an ini file rather than IupConfig for the
            list of open files, which was always corrupting itself. Close
            file now works properly, not mangling the table of tab handles.
25/01/2021: IupCloseOnEscape() is now applied by default to all IupDialog,
            and the latter has a new bEsc parameter to turn that off.
25/01/2021: get_routine_info() now has a bName parameter, which will avoid
            unnecessarily populating the symbol table with actual names.
25/01/2021: ENH: IupScintilla() added to pGUI, along with a demo.
29/01/2021: BUGFIX: deleting a dictionary entry was moving the key but not
            the data(!!). Many thanks to irv for finding this nasty bug.
29/01/2021: New still_has_delete_routine() function. Used by the internal 
            type checking routines of builtins\structs.e to identify any 
            now-invalid struct/class instances, and prevent accidentally 
            clobbering a slot now occupied by something else.
30/01/2021: BUGFIX: tagset was using ceil() not floor() for some unknown
            reason, and thus overruning the limit (when step!=1).
01/02/2021: ENH: allow dot notation on "struct" variables (previously it
            only allowed that on actual struct/class definitions). Eg/ie
                -- procedure test(Widget x)  -- (was ok)
                procedure test(struct x)     -- (now ok too)
                    x.field = 1
                end procedure
01/02/2021: Allowed delete_routine(x,0) to remove any prior association.
            The docs said you could, but the front-end disallowed it.

Version 0.8.2
=============
22/05/2020: You can now say pMem = free(pMem), setting pMem to NULL. While
            free() is still a procedure, there is a new function ffree()
            which returns appropriate NULLs and pmain.e now quietly maps 
            free() calls to ffree() instead of issuing an error.
06/06/2020: Removed backtick from Delimiter section of FASM.syn - I have
            no idea why/when it got put in there. Improves viewing of
            list.asm files, and hopefully does not break something else.
08/06/2020: Added a find_all() builtin (documented alongside find()).
            Make builtins\ordinal.e an auto-include, why not.
18/06/2020: Added a simple assert() builtin, see crash() docs.
20/06/2020: Made serialize() return a binary string, rather than a
            dword-sequence.
20/06/2020: Nested constant declarations, eg instead of
                constant WSAEINTR   = 10004,
                         WSAEACCES  = 10013
                constant {ERROR_NO, ERROR_NAME, ERROR_SHORT} = columnize(
                    {{WSAEINTR,         "WSAEINTR",  "Interrupted function call."},
                     {WSAEACCES,        "WSAEACCES", "Permission denied."}}
            (which gets more and more error prone and tedious as the table 
             grows in size) you can now define them all together like this:
                constant {ERROR_NO, ERROR_NAME, ERROR_SHORT} = columnize(
                    {{WSAEINTR:=10004,  "WSAEINTR",  "Interrupted function call."},
                     {WSAEACCES:=10013, "WSAEACCES", "Permission denied."},
            ie on the second line before the first ',' we see both a definition 
            of WSAEINTR and a reference to it.
25/06/2020: Added pp_IntCh of -1 handling to show eg 65 as just 'A'.
26/06/2020: HACKFIX. An "if atom(data) then" which was being optimised away
            was invoking an ltAdd(TEST+flippable,..) in pilx86/opJtyp which
            was never getting undone/flipped. For now, I simply removed it,
            which means it will now properly test a few things that it was
            previously optimising away. This may need to be revisited.
            (For some failing example code see end of test/t37misc.exw.)
28/06/2020: New faster version of is_prime(), now moved to pfactors.e.
28/06/2020: Added apply() and filter() routines, plus get_routine_info().
28/06/2020: BUGFIX: Named parameter handling error on auto-includes, eg
                ?shorten(s,ml:=2)
                               ^ incompatible type for routine signature
            In this particular case/call it now effectively/temporarily
            overrides the psym.e signature from "FPSI" to "FPOO". It may
            prove better to replace that sig = T_object just added to
            pmain.e/ParamList() with an outright rejection along the
            lines of Aborp("named params require explicit include"),
            which you can find commented-out/untested in ParamList().
            [Or, of course, shove a gazillion and one tons of code into
             psym.e to handle named args on autoincludes, and builtins..
             but I think I'd rather start a brand new Phix 2.0.........!]
27/08/2020: BUGFIX: eax trashed by belayed opScmp: Expr() now has an extra 
            saveFunctionResultVars(opsidx,INTSTOO) call (see t37misc).
27/08/2020: BUGFIX: a literal -1073741824 was getting T_N not T_Integer.
08/10/2020: BUGFIX: printf(1,"x%04dx\n", -1) was yiedling "x00-1x". It
            now yields "x  -1x", whereas Euphoria yields "x-001x", which
            I suspect is down to the underlying C implementation and may
            not even be constent between platforms. I may be persuaded 
            to attempt futher tweaks to improve compatability should 
            anyone kick up a big enough fuss about it..
10/10/2020: BUGFIX: without debug {include} with debug was not properly
            re-enabling top level diagnostics on the current file, so
            effectively without debug would continue to apply to all
            top-level code after the with debug statement. The flag is 
            binary, btw, you can't turn it off for half the top-level
            code, but obvs you shd be able to turn it back on for the
            current file, after hiding some builtins from the ex.err.
            Subroutines after the with debug were however always fine.
01/11/2020: BUGFIX: Nested constants were not properly marked as such,
            eg SPREAD in p2js_basics.e - triggering the compilation
            error "cannot create jump table" on a switch statement
            (since to do so all targets must be known at compile-time,
             and that will be forced by any "fallthrough" statements).
05/11/2020: BUGFIX: Embedded classes were using the stids rather than
            the srids in struct_add_field(). This meant that several
            test cases worked fine interpreted, but crashed with an
            invlaid routine_id when compiled.
15/11/2020: BUGFIX: switch length(r) do was suppressing the store of
            tmp in tmp=length(r), leaving it in a register, then in
            "if tmp=1, elsif tmp=2..." the first tmp was correctly
            getting the register, but second (etc) "reloading" tmp.
            Quite probbly, the real bug was that reginfo was not
            being correctly maintained, but instead I simply killed
            pilx86.e/transtmpfer() [maybe I could have limited that
            damage to IF], and luckily there didn't appear to be any
            significant performance hit, but time may yet tell...
16/11/2020: Enhancement: added %q and %Q to (s)printf(), which are
            like %s but add quotes. %q can use backticks, and favours
            them if the new "prefer_backtick" option is set, whereas
            %Q forces doublequotes. Moved (s)print() from their old
            builtins\VM\psprintN.e home to builtins\VM\pprntfN.e, 
            so that the internal allascii() could be shared/tweaked.
17/11/2020: Added Edix\syn\css.syn to syntax-colour .css files, obvs.
            Be warned it thinks \\ is a css line comment (none such).
            Feel free to copy it to Edita\syn, but I'm not going to
            bother with maintaining and shipping two copies of it.

Version 0.8.1
=============
01/05/2019: Made mpfr.e check for existence of msvcp100.dll/msvcr100.dll.
            Also made mpir_get_versions() 64-bit compatible and work when
            mpir_only is in force, fixed mpz_mod_ui() [obviously it had
            never been tested], added mpz_fdiv_qr() and fixed a bug in 
            mpz_get_str() whereby it crashed with a stack overflow when
            printing >~100K digits (it now prints in chunks of 20K digits
            and stitches them together).
01/05/2019: Minor bugfix in timedate.e for length(td)<DT_TZ.
01/05/2019: Removed thousands of valign=top from the docs since it made
            no difference to the chm but messed up the online docs.
08/05/2019: Added get_file_date() and set_file_date(), and added an optional
            date_type parameter to dir().
09/05/2019: BUGFIX: [sequence] tasks = extract_tasks(tasks) was not checking
            for tasks being unassigned properly (==> [MEMORY VIOLATION]), if
            the pass-by-reference optimisation was being applied in opFrst.
            Arguably on declaration the front-end should spot such and issue 
            a fatal compilation-time error [DEV]. It would also benefit from
            popping an opFrame before triggering the fatal runtime error, to
            prevent the spurious "(warning: lineno of -1 for era of #HHHH)".
22/05/2019: BUGFIX: The internal routine timedate.e/timedate_to_julian_day()
            was returning the same day for 28/2/2034 and 1/3/2034. There was
            already a different algorithm from wikipedia commented out which
            claimed to return the same results: it didn't, and has now been
            adopted as the better algorithm, fixing the above problem.
25/05/2019: ENH: throw("could not open "&dll_name) was reporting the error as
            "unhandled exception", it now checks for the "plain string" case
            and says eg "unhandled exception (could not open kerbnel32.dll)"
            (that is, when you have accidentally spelt kernel32 with a 'b').
03/06/2019: More thread-safety mods to VM\prntfN.e (see OEforum).
17/06/2019: Added sprintf() functionality to throw().
19/06/2019: Added several dozen more routines to mpfr.e. Added try/catch to
            the core types. Added mpz_add_si, mpz_sub_si, mpz_odd, mpz_even, 
            mpz_sign, mpz_remove, mpz_prime_factors, mpz_factorstring, and 
            mpz_re_compose.
            Removed mpz_fits_s[/u]long_p, added mpz_fits_integer[/atom].
            Removed mpz_get_s[/u]i, added mpz_get_integer[/atom].
            Obviously the replacements are geared towards the phix limits,
            ie 31/53/63/64 bits, rather than the machine word / C limits.
            Allowed mpfr_set_default_prec and mpfr_set_prec to accept a -ve 
            decimal precision (as well as the existing +ve binary precision).
21/06/2019: Bugfix: emitHex10sdi() did not have any proper handling for large
            64-bit integer literals, eg anything > #FFFFFFFF.
22/06/2019: Added pp_IntCh to ppp.e and deprecated several pp_StrFmt settings:
             -2 => use {pp_IntCh,false} instead
             -3 => use {pp_StrFmt,-1,pp_IntCh,false} instead
              1 => use {pp_StrFmt,3,pp_IntCh,false} instead
            (I went the extra yard to give properly helpful error messages.)
            pp_IntCh means display integers as eg 65'A', default true, and
            obviously set it to false when you only want the 65 part.
            [This change adds no functionality, just makes things clearer.]
22/06/2019: Make ppp.e use backticks rather than doublequotes + escapes, when
            that seems to be the sensible thing to do. Ditto ex.err files.
23/06/2019: Bugfix: looks like I was mapping TCHAR the wrong way round in
            cffi.e, and ansi was getting treated as UTF-16. Oops.
08/07/2019: Bugfix in Edita/edx: Pascal block comments ({"{","}"}) were not
            working. Traced to chidx2 -= 1 before the first scanForUrls() 
            call in [ea]synclr.e which has been removed. Obviously it seems 
            ok, but there may be some unexpected knock-on effects.
16/07/2019: Added pipes parameter to system_exec(), and demo/capture_console
            to demonstrate use. Needs a bit more work on linux.
22/07/2019: Enhanced scanf() to properly handle %b/o/x formats. Previously
            it could scan "#DEADBEEF" but not "DEADBEEF", now it can do both.
            At the same time, to_number() was given an optional inbase param.
31/07/2019: BUGFIX: scanf() was not parsing floats/checking for '.' correctly.
02/08/2019: Added and_bitsu(), ditto not/or/xor, which give unsigned results.
07/08/2019: ENH/BUGFIX Invoking free() on a value with delete_routine() in force 
            now behaves as delete(). It is assumed it will re-invoke free(), but 
            of course with the routine field as just zeroed. Previously, calling
            free() on such would often result in a cryptic fatal error, as the 
            subsequent delete() would re-invoke free() with the same address.
12/09/2019: ?`"abc"` now prints `"abc"` rather than "\"abc\"". If a string has
            any of "\r\n\t\0\e" then doublequotes\backslash must still be used, 
            but if not, yet some of "\\\"\'" (aka `\"'`) /are/ present, then it 
            uses backticks instead of dblquotes. See builtins\VM\psprintN.e
12/09/2019: Upgraded pGUI libs to 3.27, which went without hitch.
19/09/2019: BUGFIX: /0 on pltype.e line 678, procedure ltAdd(). Caused by the
            (udt) "boolean wild_str = find('?', str) or find('*', str)" in 
            demo\search.exw, dltyp[=976]!=ptyp[=1]. It now checks that both
            are <=T_object before complaining.
21/09/2019: BUGFIX: peek({mem,0}) on 64-bit was doing a sub rsp,8 which should
            have been an add rsp,8 (to match the 32-bit add esp,4). That little
            beastie of a bug was apparently introduced on 23/5/15!
27/09/2019: BUGFIX: unconditional exit from for loop: reversed the changes of 
            25/10/2017 (see below), which resurfaced. It occured to me (and I
            have no idea why not 2 years ago) that the backpatch could probably
            also be done in jskip() [as well as opEndFor], which it now is.
            Note there may be some uses of jskip() which do not set "mergeSet"
            properly, which it now relies on.
25/10/2019: BUGFIX: A statement such as board = repeat('+'&"x",2) incorrectly
            changed opRepeat to opRepCh (or more accurately routineNo from 
            T_repeat to T_repch in pmain.e/ParamList()), leading to strange
            and random output, as it brutally coerced parameter 1 to char using 
            the asm equivalent of and_bits(<ref of string "xx">,#FF), and also
            a fatal crash on line 3370 in pilx86.e/StoreDest() on finding a 
            string with string elements/characters. It now checks not just for 
            a toktype of SQUOTE, but also a following Ch of ','.
27/10/2019: ENH: Made routine_ids first class, ie [also] callable directly. Eg:
                procedure show(string s) ?s end procedure
                constant r_show = routine_id("show")
                -- show("40")   (normal non-routine_id call [still valid!])
                call_proc(r_show,{"41"})                -- old style [""!]
                r_show("42")                            -- ***new***

                function f(integer i) return i end function
                constant r_f = routine_id("f")
                --?f(40)        (normal non-routine_id call [still valid!])
                ?call_func(r_f,{41})                    -- old style [""!]
                ?r_f(42)                                -- ***new***
            Any integer followed by '(' is treated as a call_func() or a 
            call_proc(), depending on the context, and obviously as above
            the normal/old styles remain perfectly valid and fully supported.
            However I decided against supporting this sort of thing (at least 
            not before someone comes up with a compelling reason to):
                --?r_f(r_f)(43)
                --r_f(r_show)(44)
                --sequence rids = {r_show}
                --rids[1](45)
            Instead you have to code integer rid=r_f(r_f) then ?rid(43), etc.
            (Some wag might say that makes them second class, but it is not 
             like you could ever do anything like that with "show" anyway.)
            Note that i(5) where i is not a routine_id will fail just as badly
            and in exactly the same way as call_func/proc(i,{5}) always has.
            Likewise the additional compile-time validation of argument types
            is not performed/deferred until run-time, in exactly the same way
            as it always was for explicit/old-style call_func/proc() calls.
            These changes were made to complement the syntax enhancements for
            the new struct/class handling, specifically class methods.
22/11/2019: Added builtins\ordinal.e (not an autoinclude):
                ord(i) returns "st", "nd", "rd", or "th".
                ordinal(n) returns "first", "second", etc.
                ordinal(n, bJustSpell:=true) - returns "one", "two", etc.
23/11/2019: Added an explicit string(cl) check to ppp.e, since I simply never 
            want to see {"this",{9,10}} as {"this","\t\n"} ever again... if 
            it causes any grief, I guess I'll have to make it optional.
25/11/2019: [EXPERIMENTAL] added simple lambda function support. You can now
                integer rid = function(integer i) return i+1 end function
                ?rid(3) -- (as per 27/10/2019 above, nowt changed today.)
            Not entirely sure where this is heading. The "integer rid =" is
            not the interesting part, the inline function on the rhs is.
            Such functions would need an explicit self_rid to be recursive.
            A single-statement equivalent might also be possible, such as:
                integer rid = =>(integer i) return i+1 -- (or "==>"?)
            The body could also be any single statement (if/for/while/try as 
            well as return, but not an unwrapped block), with caveat emptor
            should it exit instead of an inner [explicit] return. However I
            should probably wait until I find an actual use for the longhand 
            form before I start designing a shorthand form!
            **NB** the expression does not yet have any access to the nested
            scope, I just realised, making it rather somewhat less useful...
            Left as is for now, need to get demo\rosetta\Nested_function.exw
            working before anything else can be done here.
            Also [DEV] this remains undocumented, apart from here.
28/11/2019: BUGFIX (bitbucket issue #27, rmatch length 1 not working.) There
            was a completely spurious "ln -= 1" on line 164, such that if you 
            sought "xyz" it would actually find "xy", and in the length of 1 
            case, the ensuing for j=1 to 0 loop never successfully returned.
05/12/2019: BUGFIX: power(i,3) emitted mov ecx,3; mov eax,ecx; call %opPow;
            so everything came out as 27. It now calls clearReg(ecx) which
            makes it emit the more correct and useful mov eax,[i] instead.
11/12/2019: Added %t to (s)printf(), to print "true"/"false" from a boolean.
11/02/2020: To complement first class routine_ids, the absence of a trailing
            '(' makes a routine reference equivalent to its routine_id, eg
            atom eThread = create_thread(routine_id("mythread"),{1}) is old,
            atom eThread = create_thread(mythread,{1}) is now the same. Nice.
            Note that some builtins such as length still have no routine_id,
            and instead you must provide you own mini-shim for such things.
            [update: fixed in 0.8.2 via autoinclude builtins/hll_stubs.e]
12/01/2020: day_of_week() changed to return 1..7 (Mon..Sun) [was Sun..Sat]
            (bringing it into line with ISO 8601 and the earlier ISO 2015)
13/02/2020: Added include_file[s]() to match include_path[s](), see docs.

Version 0.8.0
=============
17/08/2018: BUGFIX: delete_routine() was not setting refcounts or even storing 
            the result correctly. res = delete_routine(res,rid) would work, but
            tmp = {a,b}; res = delete_routine(tmp,rid) (and the same when tmp 
            was an unnamed temporary variable due to inlining the {a,b}) would
            leave res either completely unassigned or with its prior value.
24/08/2018: BUGFIX: DoForwardDef() was declaring function returns as Private
            rather than FuncRes, leading to saveFunctionResultVars() not being
            called for routines (such as ba_sub) that had an explicit forward
            definition, producing garbage results. Trivial fix, once found.
02/12/2018: BUGFIX: sprintf("%f",2e-77*1e100) yeilded "1:000...". The ':' is
            from '9'+1, it is kind of trying to print 19999... (which is fine,
            if we end up with >5 left over we run back down, rounding up), but
            instead of a 9 for the 2nd digit it got a 10. Clearly such values 
            are quite rare, eg the apparently equivalent 2e23 does not exhibit
            the same problem. Added a second test "if digit=10 then exit" and 
            an "or digit=10" to round_str().
12/01/2019: Added %v to [s]printf(), which is %s with a sprint(), and can
            therefore be used to print [almost] anything. Note however that
            bigatoms will show the bcd-internals, and bigints the base-65536 
            internals, etc, so it ain't perfect [yet...].
29/01/2019: "and board[i+direction][j+move] == iff(move=0?'.':opponent) then"
            failed in pmain.e/DoIff() as exprBP was not zero, as already noted 
            in the comment which read "we may yet have a problem..." next to
            it. Trivial fix, just save/restore exprBP (and scBP, why not).
15/02/2019: BUG: the type text_point() in builtins/pscreen.e was checking for
            {300,500}, inherited from a {200,500} as found in OE's image.e,
            however I had at some point resized my terminal to {500,271}. It
            now checks for {65535,65535}. However, the problem was originally
            triggered via a trace(1) in pilx86.e when running "p p -d e01",
            ie typecheck within pTrace.e as compiled into p.exe, which simply
            confused the bejesus out of pDiagN.e, as none of the addresses 
            corresponded to the symtab of p.exw... and it still would ...
            What I think it needs to do is check for addresses in the region 
            of symtab[20=optable] or something like that, and (temporarily) 
            use a "lower-level" symtab from symtab[T_EBP][3] (as set in p.exw,
            potentially recursively), or somesuch. [DEV/TODO]
25/02/2019: BUGFIX: the try statement had absolutely no localtype handling, as illustrated 
            by the following code which compares said against an if construct:

                integer q, q11, q22, q12, r, r11, r22, r12
                if rand(2)=1 then
                    q = 1
                    q11 = q
                else
                    q = 2
                    q22 = q
                end if
                q12 = q
                ?q

                try
                    r = 1
                    r11 = r
                catch e
                    r = 2
                    r22 = r
                end try
                r12 = r
                ?r

                #isginfo{q,integer,1,2,object,-1}
                #isginfo{q11,integer,1,1,object,-1}
                #isginfo{q22,integer,2,2,object,-1}
                #isginfo{q12,integer,1,2,object,-1}     -- good!

                #isginfo{r,integer,1,2,object,-1}
                #isginfo{r11,integer,1,1,object,-1}
                #isginfo{r22,integer,2,2,object,-1}
                --#isginfo{r12,integer,1,2,object,-1}
                #isginfo{r12,integer,2,2,object,-1}     -- oops!

            If you ran that code, r would always be shown as 2, despite no exception ever occuring
            (and in fact an ex.err would contain 1, but the compiler still insists it must be 2).
            The above code is now permanently part of test/t49ginfo.exw, with "if r12=2 then ?9/0".

            The localtype information (which includes min/max as well as type) was not being properly
            reset at "catch" and merged at "end try", in the same way it is at "else" and "end if",
            and obviously similar mechanisms also exist for "for/while" .. "end for/while", which
            turned out to be the more suitable to repurpose when fixing this particular issue.

            Note that the il now generated re-uses opLoopTop and opCtrl,END+LOOP, something like:

                try
                --  opLoopTop,lmask,gmask,elnk
                --  opTry,tmp,tgt
                    ...
                --  opCtrl,END+LOOP,link,emitline
                --  opTryend,0,149,0,96,
                catch
                --  opLoopTop,lmask,gmask,elnk
                --  opCatch,tlnk,e
                    ...
                --  opCtrl,END+LOOP,link,emitline
                end try
                --  opLabel,mergeSet,0/x86loc,link

            (obviously I have used poetic licence and moved "catch" down two lines for clarity)
            (and obviously them not loops, just blocks that need the same reset/merge handling)
            Apart from the "that's always 2", the actual machine code is otherwise unchanged.
            There is an untried and commented-out TRY constant in pltype.e, but replicating
            opLoopTop as, I dunno, say, OpTryBlock seems like far too much effort for the 
            rather scant reward of slightly easier to read ildump.txt files.
            I also added clearIchain() to DoTry(), to reset any "known to be initialised" flags.

08/03/2019  BUGFIX: Crash in pmain.e/DoIff() compiling the following:
                integer i = rand(10)
                bool doit = iff( false ? i>=5 and i<7 : i>5 and i<=7 )
            Turns out it had never handled any and/or inside an iff() expression properly.
            Simple fix was just Expr(0,0) ==>> Expr(0,asBool), times two, in DoIff().

20/04/2019: Other changes in the 0.8.0 release:
            Installation now in three parts, phix.0.8.0.setup.exe, phix.0.8.0.1.zip, and
            phix.0.8.0.2.zip. The setup.exe automatically downloads (unless it can find them) 
            and extracts the two .zip files (via demo/pGUI/pdemo/installation.e).
            NEWGSCAN (disabled for now)
            GetLastAccessTime() renamed as GetFileTime() in builtins/timestamp.ew
            get_text() now supports GT_KEEP_BOM, and GT_WHOLE_FILE now only adds a trailing \n 
            when the file has been opened in text mode (ptok.e changed to do that to match).
            log2() added
            de/encode_base64() documented and builtins/base64.e made an auto-include.
            custom_sort() enhanced to support inline tagsorts w/out custom comparison routine, 
            and now also supports both ascending and descending modes.
            sort_columns() added
            extract() and reinstate() added
            include_path() added (without an s) to complement/filter include_paths()
            set_file_size() added, and get_file_size() can now return a KB/GB/TB string result.
            sq_min(), sq_max(), and sq_log2() added
            allow_novalue() added to bigatom.e
            ba_sprintf() can now comma-separate the integer part of the result, like sprintf().
            New aliases ba_div, ba_idiv, ba_mul for ba_divide, ba_idivide, and ba_multiply.
            New routines ba_factorial, ba_gcd, ba_lcm, ba_mod, ba_mod_exp, ba_sign, and ba_uminus.
            pretty_print() moved to builtins/pretty.e and documented (was in misc.e)
            new_dict() can now make a fast copy of an existing dictionary
            factorial() now iterative (and hence slightly faster, was recursive)
            lcm() added to complement gcd()
            builtins\xml.e added, for converting xml text <==> nested structure, and documented.
            builtins\mpfr.e, a wrapper for gmp, for fast [very fast] arbitrary precision maths.
             (^note the dlls still need to be downloaded manually for now, but I have a plan..)
            builtins\pSQLite.e added, see (extensive) new documentation under Other Libraries.
            regex now supports h/v/z for horizontal whitespace, vertical whitespace, and eof.
            [s]printf() now has text centering, v for sprint(), unicode alignment now optional.
            new error "attempt to get square root of negative number" in the sqrt() builtin.
            builtins\prnd.e is not yet used and should be ignored for now.
            rmatch() added to complement match(), as per OE, matches from the other end.
            proper() added to builtins/pcase.e - not yet documented [now done, 1/1/20].
            builtins\complex.e added for complex number handling - not yet documented.
            builtins\pfrac.e added for rational fraction handling - not yet documented.
            builtins\pqueue.e added for priority queue handling - not yet documented.
            OE compatibility routines lookup(), keyvalues(), text_format() and begins() added 
            to builtins/pvlookup.e, not yet documented.
            builtins now has ripemd160.e and sha256.e, not documented, not likely to ever be.
            ditto builtins/unicode_console.e


Version 0.7.8
=============
10/01/2018: BUGFIX: when the file/directory is not found, apply get_proper_path() 
            recursively to the parent directory.
21/01/2018: BUGFIX: rand() effectively preserved the high bit for ranges over
            #7FFFFFFF. Changed a jge (signed jump) to jae (unsigned). 
            Also changed the 64-bit store to deal with 64-bit results above
            #7FFF_FFFF_FFFF_FFFF correctly.
03/02/2018: Corrected definition of xGetExitCodeThread.
15/02/2018: New LiteZip wrapper and documentation added.
15/02/2018: BUGFIX: Slice replacement was out-by-one, eg
                s = { 1,2,3,4,5,6 } 
                s[1..1] = 9
            was incorrectly {9,1,2,3,4,5}, now yields {9,2,3,4,5,6}.
            Thanks to Tom for finding this. 
17/02/2018: Bugfix: parse_json() did not cope with negative numbers.
            Thanks to Chris for finding this. 
17/02/2018: Bugfix: setup.ew crashed for registry keys of length 0.
            Thanks to Kat for finding this. 
20/02/2018: Removed spurious sanity check in change_timezone().
            Thanks to Chris for finding this. 
24/02/2018: =$ can now be used anywhere except the first in an enum.
            Previously =$ only worked for delta==+1, but DoEnum() 
            now has a prev var, so that any "by delta" now works.
28/02/2018: Added peek_wstring() and poke_wstring().
04/03/2018: adjust_timedate() no longer clobbers DT_MSEC aka DT_DOW.
05/03/2018: timedate.e: added "ms" to extract/print milliseconds.
            (not thoroughly tested, possible ambiguity issue noticed)
06/03/2018: constant integer {a,b} = <expr> style syntax now supported.
06/03/2018: date(DT_GMT) now returns the GMT (==UTC) time, irrespective 
            of location, with milliseconds. 
14/03/2018: Fixed potential thread safety issue in sprint().
14/03/2018: Upgraded to IUP 3.24. Three routines have been removed:
            IupColorbar, IupColorBrowser, and IupDial.



Version 0.7.5
=============
22/02/2017: BUGFIX: task_yield() was incorrectly defined as E_other in psym.e,
            it is now correctly defined as E_all. Correspondingly, the #ilASM
            in the routine itself was missing an e_all directive.
07/03/2017: BUGFIX: ltAdd() was reusing a SET entry, causing problems:

                integer froot
                procedure digital_root()
                integer root = 0
                    while 1 do
                        root += 1
                        if root<froot then exit end if
                        root = 0
                    end while
                    froot = root
                end procedure

                froot = 10
                digital_root()

            The root=0 SET was overwriting the +=1 SET, causing it to think
            that root must be 0 after the end while. I just rudely deleted 
            the whole attempt to reuse SET entries... (fingers crossed)
16/03/2017: BUGFIX: s[i][$] = 'x' completely broken when s[i] was a string.
            :%opRepe was jumping to :RepeStr /before/ calling :%fixupIndex
17/03/2017: Renamed the "unicode" library as "utfconv" in the manual, as
            more logical, and to match the name of the (auto)include file.
17/03/2017: Bugfix: adjust_timedate() crashed if delta was not a whole number
            of seconds. Also, timedate_diff() was ignoring milliseconds, and
            date() now optionally returns milliseconds.
19/03/2017: Now supports the following Orac idioms:
                s.i as shorthand for s[i]
                s.i.j as shorthand for s[i][j]
                [i to j] as an alternative to [i..j]
                ~s as shorthand for length(s)
                int, seq as shorthand for integer, sequence
            (disable if required by setting constant ORAC in p.exw to 0)
            Edix\tools\reindent has been updated to support these, but I
            don't plan to backport that to edita (feel free, just look on
            bitbucket for a diff around this date to edix\src\rein.e and
            try to make the equivalent changes to edita\src\earein.e).
21/07/2017: Bugfix: pilx86/opPow could emit overwrite edi==tmpr; in this
            particular case res = power(x,length(d)) was effectively the 
            same as res = power(x,<raw addr of res>), obviously leading 
            to power overflow errors. (60 second fix)
29/03/2017: Bugfix: cdx = find(cemi,"CEeMm")-(cemi='m') was invoking find(), leaving 
            the result in eax, then invoking opSeq to set eax (as cemi=='m'), then 
            subtracting eax from eax, and therefore always setting cdx to 0.
            It now invokes saveFunctionResultVars() in pmain.e before trashing eax.
03/01/2017: Enhanced integer powers. Previously, (on both 32 and 64 bit) it would
            calculate power(-177..+177,1..4) in eax/rdx. It now uses two ranges:
            32-bit: +/-181^4, and +/-10^9, and 64-bit: +/-46340^4, and +/-10^17.
            See builtins/VM/pPower for the precise details.
            Added a note to phix.chm/core/atom/floats are not exact which explains
            some of the reasons for needing/wanting to do this (on 64 bit).
            (Specifically, rc/truncateable primes was broken on 64-bit)
            This change also fixed several problems in t28prntf.e (on 64 bit).
07/06/2017: Bugfix: Crash in pilx86.e/opFor2. Change may be suspect.
            Running an entirely incomplete source along the lines of:
                for i=1 to length(board) do
                    if board[i]='1' then
                        ?i
                        for j=1 to 0/*[=length(moves)]*/ do
                            if 0 then
                            end if
                        end for
                    end if
                end for
            The above crash occurred. Temporarily changing the ?9/0 to ?"9/0" and 
            running it caused all 1..50 to be printed, instead of the 14 expected.
            Changed the offending code, inside the branch clearly commented with
            "-- (we've deduced loop will iterate 0 times)", from:
                if s5[pc]=opLabel then
                    if s5[pc+1]!=exitMerge then ?9/0 end if -- more investigation rqd?
                    pc += 4             
                end if
            to
                if s5[pc]=opLabel
                and s5[pc+1]=exitMerge then
                    pc += 4             
                end if
            Hopefully it is just the case that optimising away an entire for loop
            has never happened in such close proximity to end if(s) before...
            All tests pass with this change, but it is a "quick fix" that may need
            to be revisited.
20/06/2017: You can now also declare variables as part of multiple assignment, eg
                {string name, integer id} = lookup()
            As well as being a generally useful enhancement/feature to have, this
            also brings the language more in line with how I want to document it.
            Note however sub-types do /NOT/ propagate as you might expect, eg:
                {a, string b, c} = lookup()
            will terminate in error if b already exists, or if a /or c/
            does not already exist. While string {a, b, c} propagates 
            the type, and declares three new variables of type string, 
            that does /not/ happen for types inside the {}, except when
            the type immediately precedes an opening (/nested) '{'.
            Admittedly this is a simple practical choice/implementation
            detail (see pmain.e/GetMultiAssignSet()) that it may be
            possible to improve upon, but there are four use cases for
            that routine, hence the simplest possible solution won.
02/09/2017  Bugfix: sprintf("%f",-9.999999999) yielded ".0" because round()
            was not accounting for a leading '-'. (Amazingly, not seen b4)
25/09/2017  BUGFIX: ?sort({{0},{-3.8}}) gave completely the wrong results.
            In compare({a},{b}) the nested compare of a,b was not handling
            integers vs floats correctly; if {a,b} was {int,float} or
            {float,int} it would simply assume int<float. The same bug was
            present for infix <, <=, >, >=, though = and != were fine.
            Quite amazing how that one lain undetected for 4..10 years!
            Also, a=compare(a,b) looked dodgy - deallocating tgt before 
            the comparison - now does it after (no new tests though).
25/10/2017  An "illegal/unsupported construct" error now occurs if an end 
            for statement is immediately preceded by an unconditional exit.
            Where possible, use a simple if construct instead. This proved 
            necessary because the opEndFor was being skipped, but that sets 
            the zero iterations jump. Example:
                for i=1 to length(s) do
                    ?s[i]
--                  if 1 then   -- (or constant DEBUG etc)
                        exit
--                  end if
                end for
            (s[1] errored on length(s)=0, because the <1 jump was wrong.)
            Note the /*unconditional*/ inner "if" makes no difference here.
            However and of course, this sort of thing is (still) fine:
                for i=1 to length(s) do
                    ?s[i]
                    if "abc"="def" then
                        ?"what??"
                    else
                        exit
                    end if
                end for
            (assuming the compiler does not optimise that test away too)
            Update: Problem resurfaced. There are now fixes for this in 
            both pmain.e/DoFor() and pilx86.e/jskip().
29/10/2017  Bugfix. A call_func that triggered a type check was displaying
            an incorrect return address/line number of -1. As part of this
            fix, the ex.err no longer contains the stack entry arising from 
            pcallfunc.e/call_common(), somewhat unintended, but perhaps not
            a bad thing. [Critical addition: !cf_ret now in the optable.]
01/11/2017  Bugfix. Multiple assignment style declarations were not always
            correctly declaring/defining local variables, eg:

                string str = "1"

                procedure test()
                    -- this /should/ declare a new private variable,
                    string {str} = {"22"}
                    -- whereas this we /would/ expect to do damage!
                    --  {str} = {"22"}
                    if str="33" then ?9/0 end if --(suppress warning)
                end procedure

                test()
                if length(str)=2 then ?9/0 end if
            
            Previously, because there already was a variable named str, 
            it did not declare a new, and private, one inside test() - 
            but now it always does. Trivial change, that took far longer
            to find then test and then write this, than actually code.

Version 0.7.2
=============
02/01/2017: Fixed one of the rarer and more confusing errors: if b.exw was
                constant PINF = 1E308*1000
                include m.e
            and m.e was:
                global constant PINF = 1E308*1000,
                                MINF = - PINF
            Then we had an error in m.e of PINF not defined! The problem was
            that DoConstant() was spotting the need to link together on the
            S_Clink chain, but was doing so using an old copy of symtab[O],
            from before the addSymEntryAt(), thereby messing up the S_Nlink
            chain. It now fetches a fresh copy of symtab[O] -- simples.
03/01/2017: Finally added trace(3). However, it has exposed a glitch in the
            file i/o routines: a missing line every 106/107 lines, which is
            approx 7738/7811, suspiciously close to 8192-6*73(=7754) buffer
            limits in VM/pfileioN.e - which will hopefully be trivial to 
            fix once better reproduced. [DEV/challenge]
05/01/2017: Bugfix: Unary minus incorrectly applied to routine_ids, eg:
                constant r_x = routine_id("x")
                ?{r_x,-r_x,routine_id("x"),-routine_id("x")}
            prints say {947,-947,947,-1755}. The problem was Factor() not
            testing for K_rtn properly, it now does a proper opUminus.
            [There is also a PushFactor(k,1,T_integer) after resolveRoutineId
             that I suspect should really set isLiteral to 0, as another way
             to solve the same problem, but that broke "p -test" badly... At
             least, I think that is why -r_x works but the longer does not.]
            Should only have been an issue compiling, not interpreting.
15/01/2017: Bugfix: fatal error in lineinfo() when trying to update LineTab
            for "procedure text_mode() end procedure" (no \n). Similar fix
            to that of 07/12/2015 for a completely empty source file.
23/01/2017: Bugfix: running IDE.exw crashed in Or_K_ridt() when calling 
            routine_id("x") mid-procedure x(with >=2 params). At that stage,
            the S_Slink chain is "backwards" so must be scanned differently.

Version 0.7.1
=============
17/11/2016  You can now use platform(), machine_bits(), and machine_word() as
            parameter defaults (optimised to literal integers in pmain.e/
            getOneDefault). Also, a switch with no case statements now triggers
            an error (because pilx86.e cannot cope, and besides it is bit like 
            having an if/elsif/else/end if with no if, no elsif, and maybe no
            else). New builtins to_integer() and to_number(). Files are now
            properly flushed/closed when an app terminates. Several x64 asm
            fixes (far too many to list, though many were just minor glitches
            in the list.asm files). Discovered rand() was really pants on x64,
            rax is now initialised with seed<<32|seed, iyswim. puts1.e was
            mutely displaying nothing, so it now does an AllocConsole first.

Version 0.7.0
=============
30/05/2016  Added bool as a simple alias of integer. It was about time.
            (one line change to psym.e, plus syntax colour and help link.)
11/06/2016  BUGFIX: the break statement was only being permitted at the
            "top level" inside DoSwitch(). Added DoBreak/breakBP/breakMerge
            modelled after DoExit/exitBP/exitMerge. Took about 30 mins.
19/06/2016  Added "forward call assumed" warnings to Phix. Initially I was a
            bit hesitant, but now that it is in place... it's a good thing.
            Part of me is certain that some people will think it is a crime 
            against humanity that to get rid of the warnings you have to add 
            an explicit forward declaration, but... tough. Well, if it really
            bothers you that much, change FWARN in p.exw to 0 and rebuild.
            There already was a warning, been there for quite some time now, 
            when an implicit local got resolved to a global, which has not 
            caused me any trouble at all, in fact quite the opposite. 
            (In case you haven't guessed, it is very difficult for me to
             properly justify any of this beyond a simple gut feeling.)
            When you get a warning, and it is not something that would just
            obviously be better off earlier on anyway, simply add an explicit 
            forward routine definition.
            The only mention of (explicit) forward declarations in the manual 
            is in Core/Declarations/Scope. [DEV: updates to manual still rqd]
23/06/2016: Removed unused parameter warnings for routines which are the target
            of routine_id (as long as that is known at compile-time). Callbacks
            for win32lib etc demand a fixed set of parameters which got a bit
            too much in one of the demos I had a play with. At the same time,
            constants which are assigned the result of a function with side
            effects other than E_none or E_other are also exhonorated from 
            unused warnings, such as constant TextLabel = create(Label,...).
            [E_other stems, I believe, from the 09/02/2016 bugfix. It replaces
             E-none in any routine of said that has any #ilASM in it.]

Version 0.6.7
=============
15/08/2015  Got the parlour trick ("p p p p p p p p -cp") all working again.
19/08/2015  Removed find_from() and match_from(), as they were messing up the
            listing files. From now on use find() and match() instead.
09/09/2015  Finished adding bigatom, including documentation to dist. In an
            amusing twist, I carefully shifted loads of mid-routine variable
            declarations to the tops of the routines, because I knew there
            was something amiss with them, before at the very very very last
            hurdle running slap into that 18-month old niggle, and fixing it:
09/09/2015  BUGFIX: scoped variable declarations at the toplevel were always
            being created as tvars, with rather drastic consequences! It now
            invokes TopDecls() instead of Locals() from Block(), as needed.
18/09/2015  It is now possible to declare forward types. Needed if I ever
            want to make builtins\timedate.e an auto-include, or anything 
            else with user defined types.
28/09/2015  BUGFIX: switch lower(ch) do needed a saveFunctionResultVars().
20/10/2015  BUGFIX: sprintf("%g",1e-14) was yielding "1e-15"!! Trivial fix, 
            once spotted. The problem was that it starts printing 9.9999e-15,
            as we expect, then round() does what it should, namely convert
            that "9.9999" to 10!0000 and trim, except the replaced '!' was 
            also discarded, so the caller (sprintf2 in pprntfN.e) did not 
            know to exp += 1. It now keeps the trailing '!', which sprintf2 
            was already getting rid of properly anyway.
26/11/2015  BUGFIX: code such as while <condition> do integer c=0 was using 
            onDeclaration semantics and not resetting c on every iteration.
            New parameter AllowOnDeclaration added to distinguish calls from
            block() and elsewhere. (Applies to both Locals and TopDecls.)
26/11/2015  Added routine x(string name, rid=routine_id(name)) support, for
            IUP. Very basic, with strict limitations: the routine_id MUST be
            the first defaulted parameter, and it MUST refer to the previous 
            parameter. Calls MUST specify a literal string for the name and
            it MUST be resolved at compile-time. Also note that it is not a
            proper defaulted parameter, but substituted for a real routine
            id by the front end. Further improvements may be possible, and
            necessary, when similar things refuse to compile. At some point
            a complete overhaul/rewrite of parameter defaulting, to allow
            general expressions, is likely to be required anyway. See the
            routine getOneDefault in pmain.e for the full set of horrors.
07/12/2015  BUGFIX: Finally fixed the "if save_modified_tabs() then" in 
            wee11. The problem was that save_modified_tabs() was assumed 
            to be a forward local routine, but was actually being mapped
            to the global routine in pilx86.e/unused_cleanup(), however
            opJif was not applying forwardretarget to relocate the result
            of the function call. I expect there are several similar bugs
            yet to be found. The actual symptom was e04atssaa in GetSrc(),
            I added a quick note which will hopefully assist in future.
07/12/2015  BUGFIX: ?9/0 in lineinfo() (major guff) when compiling an
            empty program. Solved fairly easily by adding
                if lastline=-1 then
--                  emitline = -1
                    lastline = 0
                    emitline = 1
                end if
            to opRetf handling (in pilx86.e). Changed as shown above to 
            handle the empty program listing (under -d!) a bit better.
12/12/2015  Finally made '!' in pTrace.e abort properly (I needed to pop a
            couple of stack entries, save return addresses in both opTchk
            and opLnt, and replace opTchkRetAddr with called from in pDiag).
12/12/2015  Finally vanquished that "oops2 in ReconstructIds" message when
            producing some listing files (esp "-d!"). Also added UnAlias.
09/02/2016  BUGFIX: a procedure with atom prev=c_func() was being flagged as
            having no side effects. Moved SideEffects = None above the call
            to Locals() in pmain.e/DoRoutineDef(). Then moved it even further
            up, as parameter types and defaults could equally be missed.
15/02/2016  BUGFIX: preserved rbx/rbp/rdi/rsi/r12/r13/r14/r15 over callbacks;
            fileopen/pcom now appear to work on 64-bit!! (YAY!)

Version 0.6.6
=============
13/07/2011  Fixed the problem with call() in pcfunc.e, and implemented that
            save/restore of ebp I was thinking of (without hiccup or murmor).
01/08/2011  Finally sorted out ebp save/restore in builtins/pcfunc.e, at least
            theoretically. Now to get all the errors back working.
01/08/2011  Stopped the compiler from incorrectly resolving builtin types, so
                constant r1 = routine_id("normal"),
                         r2 = routine_id("integer")
                function normal() return 4 end function
                function integer() return 5 end function
                constant r3 = routine_id("normal"),
                         r4 = routine_id("integer")
                ?{r1,r2,r3,r4}
                ?call_func(r4,{})
                ?call_func(r3,{})
                ?call_func(r2,{})
                ?call_func(r1,{})
            now prints {523,525,523,525} 5 4 5 4, whereas before it would
            print {523,1,523,525} and crash on the call_func(r2,{}).
            No real reason for fixing this, just tidying up pcfunc.e
            -There really is no builtin "routine" integer, adding one just
             for orthogonality would make some things ten times slower, as
             you can easily prove by "-d"ing a test program, though some
             intrepid soul might be able to "fudge" one with some clever
             handling of Q_Routine [which may currently be wrongly tested 
             against T_object rather than T_Bin] in pmain.e
            Update: Not properly available yet, but eventually I expect it will
            be possible to code something like this (untested/uncompiled):
                global type integer(object o)
                -- NB You must use a namespace to force this to be called
                --    instead of %opInt directly, and keep any moaning
                --    about any performance hit to yourself. This exists
                --    to /allow/ routine_id("integer"), not make it fast.
                integer res
                    #ilASM{ mov eax,[o]
                            call %opInt
                            mov [res],eax }
                  return res
                end type
            or even (this is very speculative) [DEV test this]
                return eu:integer(o)
            You may need to prod me if you think it is actually worth having.
07/01/2012  Stack frame and #ilasm changes mean Phix >=0.6.3 is incompatible 
            with files in the copy of the builtins directory as shipped with 
            Edita <= 0.3.5  (which only exists so you can get going just by 
            installing Edita, without needing to install Phix as well). A
              "***RENAMING THE EDITA\\BUILTINS DIRECTORY MAY FIX THIS***"
            message is shown by Phix if an error occurs in a file whose path
            contains "edita\\builtins\\", hopefully minimising any confusion.
            Update: ilasm has been superceded (with ilASM) this release anyway.
07/01/2012  (further details of the above mentioned changes:)
            I perhaps should not have done this, but I cleaned up some of the
            #ilasm constants, typically m_reg => mreg, xxx_d8 => xxxd8, etc.
            However, the new stack handling (needed for threads) will break
            almost all existing #ilasm, so it may actually be a good thing.
            The new stack handling puts all parameters and local variables
            at [ebp-nn] instead of (less sensibly) at a fixed [#NNNNNNNN]
            which was being saved&h4/restored on every call/return. (by h4 
            I mean set to unassigned aka "<no value>" ie #40000000.)
            Update: ilasm has been superceded this release anyway.
20/01/2012  Finally got the new "-thread" handling to work on all tests,
            and even edita, but there is still a problem with self-host.
            (from what I can see, "pnew2.exe -pmkr3" is returning 0, but
             the caller is somehow getting #C0000005 back instead. I am
             going to press on with error handling updates (terror.exw)
             and getting demo handling into pgui.exw for now.)
            [Update: "-thread" was temporary & has now been removed]
28/01/2012  Towards the end of Call() in pmain.e, you can still find:
                --if newEBP then
                -- DEV: this might be overkill!! (it sure is!)
                --          saveFunctionResultVars(opsidx)
                --end if
            This caused a problem in sanity.exw as 
                sort(-500 + rand(repeat(1000, 1000)))
            gave an incompatible parameter error; the '+' was correctly
            (with the expected warning) being promoted to sq_add(), but
            calling the above too early caused opstype[opsidx] to get
            changed from T_sequence to T_object, or something like that.
            After commenting this out, I had to insert the same at quite
            a few different places throughout pmain.e. I should warn you
            that I have quite probably missed out a few, but all tests 
            and "p -c -thread edita" seem to work, though self-hosting
            and "p -thread edita" are still broken. As above, I'll just 
            press on with error handling and more demos.
31/01/2012  Finally!! Phix now self-hosts with new stack frame handling!
            The last bit was the new opRetf setting ebp = ebp_prev even
            when ebp_prev was 0, which meant the "debug shutdown code"
            fell over in a heap (even though it had nothing to do).
31/01/2012  Fixed a bug in opGets whereby 123\r\r\n would get read as
            12323\r\n. Sure I've seen that before, though it might have
            been in the experimental hll version. Update: this may be
            the thing I reported 21/08/2010, see below.
19/02/2012  Improved constant folding in the front end, so that eg
                constant inf = 1e300*1e300
                constant nan = -(inf/inf)
            are handled there rather than at run-time (and the same for 
            any other "*/+-" constant expressions, except for "/0").
            [Update 8/1/16: specific example squished by 64-bit mods]
17/03/2012  Still working on the migration to ebp-based local variables,
            in preparation for full thread support. This has broken many
            things, specifically error reporting and debugging. I will 
            need to spend quite some time on test\terror.exw getting more
            and more cases to work properly before the next release. (Nowt
            really to report, just felt like an update to this file was due.)
06/04/2012  Prevented the creation of a temp in ?9/0.
06/04/2012  Allowed ":=" as well as "=" in assignment. All legacy code will 
            continue to work unaltered, this just allows the programmer to
            explicitly differentiate assignment from comparison ("==").
            Same deal in "for", constant declaration, enums, and defaulted
            parameters, though admittedly the latter few are perhaps less 
            than my ideal stylewise, but this is all optional and a matter 
            of personal choice anyway.
            Note that Edita/Tools/Re-indent and Window Painter currently
            give errors when they find := and ==. [DEV]
06/04/2012  Allow (again optionally, all legacy code can stay the same) eg:
                constant integer Main=create(Window...)
            which typechecks if create returns {}. Also valid, in the name 
            of optionally allowing the programmer to be doubly-explicit:
                constant integer A=1,B=2,C=3, string T="T",U="U",V="V"
            (The type "carries over" any commas, until/unless replaced.)
            Omitting the type, as all legacy code does, is effectively the 
            same as coding "constant object", which is basically pointless
            (as in it is exactly equivalent to the plain "constant"), and
            means "no type check; infer any type info as best you can".
            
            Being explicit about the type may not only catch things that
            go wrong much earlier, but may let the compiler make just one
            typecheck on Main where it is declared rather than scattered
            throughout the code, and hence improve performance that way.
            
            Lastly, a warning was added:
                constant integer=1
                         ^Warning: variable name assumed, not type
            This may of couse affect some legacy code that previously
            compiled without any warnings.          
08/04/2012  Added trim(), trim_head(), trim_tail() auto-include, see
            builtins\ptrim.e. I kept it all nice and simple.
12/04/2012  Completed the rewrite/restructure of test/terror.exw.
04/06/2012  Minor tweak to -dumpil: unused ops were writing a binary
            zero to the output file, it now puts "<spare>".
06/06/2012  Finally finished the replacement opFor (the old one's error 
            handling got mullered by the new ebp handling). Hoorah! Boy I 
            can tell you that was a job and a half.
19/08/2012  Added error handling for duplicate cases in switch statements,
            ie in "switch x do 1: <blah> 1: <blah2>" the compiler does not
            know whether it should jump to blah or blah2; ==> fatal error.
            After some consideration I decided the same error should be
            shown for "if i=1 then <blah> elsif i=1 then <blah2>". Though 
            technically it would be correct to execute blah and skip blah2,
            rather unlikely to be what the programmer actually wanted.

            Internally, the compiler is free to implement a switch (or if)
            construct as either a jump table or standard cmp/jmps, as the
            latter would be much quicker in say "switch flag true: false:",
            plus I see no reason to waste time converting perfectly valid 
            if constructs in legacy code to switch statements, especially
            when doing so could actually make things slower. (Both the new
            "switch x with jump_table" and any fallthroughs will force the
            use of a jump table, btw, should you incorrectly care about it.)
            Hence the front end emits identical il (or as near as possible)
            for both "switch" and "if" statements, which makes it harder to
            emit an error for one and not the other anyway.
            
            Note that the compiler scans the switch/if construct backwards,
            (simply because it takes advantage of an existing linkage) so
            there may be slight confusion over which duplicate it reports.
            Also note that there is no guarantee that an if-construct will
            display all/any duplicate tests (as once any "not switchable" 
            reason is found, the checking loop just quits).
30/09/2012  Added default namespaces to Phix. I suddenly realised how to handle
            errors properly, which is why they didn't go in sooner. I still have
            some reservations, particularly that if boris and ken both want to
            have the default namespace "mayor", you cannot include them both in
            the same file, but you can use wrapper stubs to avoid the issue - eg
            if you get an error from "include boris.e\n include ken.e" then you
            can write kenstub.e just containing "include ken.e", and instead use
            "include boris.e\n include kenstub.e as retired" to get round it.
            [NB: the above will not work as desired on OpenEu, no fix there.]
10/10/2012  Bugfix. If eg opPuts,1,s already had s loaded in edi, it effectively
            became opPuts,1,1. A minor change to pilx86.e now spots this [rare]
            case and moves edi to eax before moving the file number into edi.
27/12/2012  Bugfix. If an include file contains something like
                Window()
                global procedure Window()
                    puts(1,"Window\n")
                end procedure
            then the implicit forward call assumes Window() is a local routine.
            Additional code (ReLinkAsGlobal in psym.e) has been added to move
            the symtab entry off the localscope and into globalscope. Prior to
            this, if the caller attempted:
                include inc1.e  -- (ie code similar to the above)
                Window()
            it would fail to locate Window(), as dropScope (also in psym.e) had
            "hidden" Window(), because it *was* still present in the localscope. 
            The documentation needs [DEV] to be updated to say: 

                Implicit forward calls are assumed to be locally defined. 
                If the actual definition (in the same file) turns out to be global, 
                that assumption is undone. However you cannot make implicit forward 
                calls to as-yet-undefined globals in other files, for example:

                    x = somefunc()
                    include somefunc.e

                will fail, since the first line will assume a local routine, which is
                never found (in this file). Unfortunately, when we do find the routine
                definition in somefunc.e, we cannot "link" the existing outstanding
                local forward call because it would still be legal for the caller to
                define it locally. In such cases either move the include line above
                the call or explicitly declare the routine as forward global:

                    forward global function somefunc()
                    x = somefunc()
                    include somefunc.e

                Aside: the auto-include builtins work by effectively declaring all 
                       known routines as forward global (see syminit() in psym.e).
                [UPDATE: more work has been done, see below (14/02/2014)]
23/02/2013  Added multiple assignments to Phix. Resumed work on Phix.chm where I
            hope the formal documentation will be available by the time you read this.
            Resumed work on stdin redirection, found that builtins/pfileio.e does
            work in that respect, but it all needs benchmarking (and fixing) yet.
            (If you can imagine what the pure asm equivalent of pfileio.e looks like,
             you will understand why I am quite keen to get rid of it!)
25/02/2013  Fixed a bug in opMovti whereby (as previously noted at some time in the
            past but never addressed) it was checking dtype instead of slroot. This
            meant it did not perform a type check storing a float in an integer,
            which later caused a memory leak warning message.
01/03/2013  Bugfix. pttree.e used a terminator of 0 for identifiers, and -1 for strings
            (and -2 for ilasm), which was fine until you defined a string with the
            value "\0\t\r\n" (or similar), at which point the traversal would spot the
            0 and pass it off to pemit.e/ReconstructIds as an identifier. Oops. It
            now uses -2, -1, (-3) terminators (widespread change, but went smoothly)
            As a by-product, I have disabled the #ilasm tt_search in pttree.e and
            reverted to the hll version. It does not seem much slower. (flw)
11/03/2013  Added abs() function as auto-include builtin/pabs.e 
            [Update: moved to builtins\pmaths.e]
20/03/2013  Bugfix. opDivf was (accidentally) truncating to 53 bits when testing to
            see if the result could be stored as a 31-bit integer. If it did not fit
            was when the damage became evident. A knock-on effect was that attempting 
            to print an atom, say PI*power(2,65) or larger using %d gave very cryptic 
            results or even crashed. The exact point at which errors crept in is not 
            known, but PI*power(2,64) was/is apparently fine. opDivf now uses frndint,
            and may therefore be (ever so) slightly slower than it used to be.
25/03/2013  Added multiple var display to pdebug.e (almost a complete rewrite).
            Added test\trace.exw as a home for all trace-related (manual) tests.
27/03/2013  Completed demo\arwendemo\filedump.exw. Next job is to transfer the F3 test
            routine to pemit.e [update: done]
27/03/2013  Bugfix. opSubse1i was making a royal mess of reporting a type check error.
            An appropriate new test case was added to test\terror.exw (opSubse1i01)
10/04/2013  Completed demo\msgbox.exw. The first tests (on pfileio.e) indicate the 
            alternatives (to c_func/c_proc) do not provide the significant performance 
            advantage as hoped for. However these techniques may (yet) be helpful in 
            my long term quest to migrate code from the back end to #ilasm statements.
17/04/2013  Added isConst and isConst8 to allow inlining of 32- and 8-bit constants, eg
                constant BUFFERSIZE = 8192
                #ilasm{ push_imm32,%isConst,BUFFERSIZE} -- 150 00200000
            substitution occurs asap in pemit.e/ilasm(). Note that some constants are
            not known at compile-time (eg "constant Main = create(window..") in which
            case they must be loaded using isVar. Update: superceded by #ilASM.
24/04/2013  Fixed another bug in call_rel32 <offset that fits in a byte>, which was
            claimed to be fixed 17/1/2013, not that I updated this file that time.
01/05/2013  First stab at global labels seems to be working. Global labels (see pops.e
            for more details) have been introduced as part of the on-going drive to
            replace closed source backend assembly with open source hll/#ilasm.
14/05/2013  File i/o has really stopped me dead in my tracks. The experimental hll code
(internal)  is way too slow, so I've fallen down the rabbit-hole of finding better ways
            to implement things in #ilasm to avoid the overhead, AND it needs rather a
            lot of work to make it thread-safe, that is with different threads working 
            on different files but at the same time.
29/05/2013  Missing autoincludes no longer show a distracting and irrelevant line no.
11/06/2013  Made \b a backspace. Previously it was inline binary, which was confusing
            and apparently completely unused. Made \x and \# accept a single digit so
            now \#8<non-hex-digit> and \#08 behave the same.
17/06/2013  Started work on a replacement for ilasm(), using standard intel syntax.
            Work on pfileio.e has finally convinced me this is necessary; running off
            to find the exact format/spelling of every single instruction was getting
            more than a bit tedious. Refactored ilasm() out of pmain.e into pilasm.e,
            added (the temp) "#ilASM{}" and "ilASM()" with the intention of getting
            the new stuff working alongside the old (to simplify testing/migration).
18/06/2013  The migration to pilasm.e triggered one nasty bug, global labels stopped
            working. Tracked the problem down to having three "integer currRtn" vars
            knocking about. Removed two leaving just the one in pglobals.e, seems ok.
22/06/2013  Completed the replacement for ilasm (wow, that was quick Pete), back to
            pfileio3.e to use it in anger.
23/06/2013  With the improved inline assembler, it took just one day to finish the
            (open source) new hll file i/o routines and gain the last 10% needed
            to match the original (closed source, non-thread-safe) assembly. Well,
            some more work is required to completely replace them, and I have no 
            real idea how long that will take. First job is to find something a bit
            simpler and figure out how to autoinclude/link builtins to global labels.
            Update 30/06: rewrote gets to get same speed as the older asm version.
24/06/2013  Undeclared global labels were not being detected/reported properly.
02/07/2013  Permitted with/without immediately after the ')' of a routine definition.
            Specifically I wanted "without warning" there, but I see no reason why
            you could not have with/without trace/profile/debug but didn't perform
            any testing of those. I did deliberately disable gui/console/licence.
            Obviously the previous (file-level) settings are restored at end routine, 
            which is the main reason why I wanted this in the first place.
03/07/2013  Finished the basic/ilASM rewrite of puts to match previous speed, as per
            23/6, I've still no idea how to properly integrate it just yet (all the
            tests are using #ilASM to invoke the global labels, rather than normal-
            looking hll calls; I'm not expecting trouble but its been a murderous
            fortnight that seems to have completely turned my brain to mush).
            Update 24/08: still no movement on this integration thing.
            Update 28/03: Done a test (pmalloc), but /still/ dragging me heels.
04/07/2013  It is now possible to use backtick (`) on include statements. Also
            added a couple of "line+=1" to MultiLineString() in ptok.e, which I
            had previously spotted when I could not spare time to properly test.
05/07/2013  Permitted "mov reg,%glbl" and "cmp reg,%glbl" in #ilASM{}.
05/07/2013  Added version compatibility checking to ptok.e. It now warns you if
            lodaing a pcfunc.e with no version or one less than 0.6.3, similar 
            for pfileio3.e (which should change before next release), and also 
            if loading any file marked newer than phixversion (as defined in
            pglobals.e). This made ptok.e throw its toys out of the pram a bit 
            with any error on an autoinclude forcing a complete reparse of the
            including file. Another one surfaced in pmsg.e when leaving global
            off an initialAutoEntry() target, in that symtab[N][S_FPno] was
            still an index to binftab (in psym.e) since effectively the routine 
            had been declared forward global, and called, but not actually
            defined anywhere, it died trying to Undefined() something without
            either a file or line number. Update 24/07: I now pass N to WarnU
            in pmsgs to explicitly check for <=T_AInc for that last change.
05/07/2013  Finally got "sequence x = {}" to deliver an "x is unused" message.
            A bit of a Hail Mary this one, in pmain.e/Assignment() I put both
            K_aod+K_used (was previously just K_aod) in the flags (statemod)
            when assigning a literal on declaration. Seems to work just fine 
            and does not create a slew of errors (or indeed any) on p -cp,
            p edita, or p -test, so that's more than good enough for me! 
            (I had a few tries at this before but had to reverse out changes.)
07/07/2013  Bugfix. Both local and global labels in ilASM were stored in the
(internal)  same treeplace; if you declare ::local and then call :%local it
            used to get an index into the local label table and happily try
            and subscript the global table with it. It now uses the leading
            % to distinguish globals from locals properly and therefore no
            longer tries to use the wrong index on the wrong table, which
            could lead to all manner of strange symptoms, not just the ioob
            that I got.
09/07/2013  Bugfix. See end of t53switch.exw for example of problematic code.
            "then return else" omits the {opJmp,opCtrl,ELSE} as it should,
            but that meant no opLabel, which triggered a ?9/0 in ilxlate().
            Not entirely convinced by the "return==>no jump table" aspect,
            but since Phix has had the switch statement for two and a half
            years without hitting this little snaglette, it should be ok.
10/07/2013  Bugfix. opAndBits was not checking non-integer results correctly.
            The (frankly shocking) upshot was that the following statement
                ?and_bits(-1,#80000000)
            went into an infinite loop(!!). Similar fixes applied to Or,Xor,
            and NotBits.
18/07/2013  Added resizeLVColumns() to arwen, for horizontally filling a
            ListView by proportionally resizing the column widths, since 
            I found myself once again duplicating that code. Also ran off
            and started playing.. the incomplete results are now in demos\
            arwendemos\resize.exw which looks promising, but I have other
            stuff to do!
24/07/2013  Added some version info to the Phix executable, since I heard
            that might quash a few false positives over on VirusTotal...
            ... but sadly it did not. (No change whatsoever, still 7/46)
30/07/2013  Fixed a problem in arwen/destroy(), not properly removing the
            id from the parent's ObjectChildren. The problem originally
            surfaced in pgdemo/Amend(), probably the most brutal use of
            destroy() that has yet been devised, so this is not likely
            to affect many other (existing) programs.
31/07/2013  Fixed another bug in arwen/destroy(); not all COMBO have a
            hidden edit control (see create) and calling eraseControl()
            with ObjectExtra[id] which was still UNDEFINED (-1) (added
            a specific test for that as well) caused much unhappiness,
            somewhat later on in the day when re-using that -1. Any and
            all manner of mayhem could have occured because of this.
01/08/2013  Fixed a bug in arwen/getClipboardText(CF_UNICODETEXT). The size
            returned from xGlobalSize is in bytes and must be halved before
            being passed to peek2u().
01/08/2013  Phix can now handle UTF8 sources (trivial change; it just skips
            the leading BOM). Unfortunately arwen and win32lib both still use 
            8bit/ansi windows API so the results are not all that impressive 
            (or, erm, "correct") but at least one major hurdle has gone. See
            demo\HelloUTF8.exw for a quick unicode messagebox example.
01/08/2013  Bugfix. The first routine in a list.asm for a source file with no
            top-level code would try and use local/param var info from the
            second routine in that same file, if any. Sometimes things looked
            reasonable until you really examined them, sometimes this resulted 
            in things like "mov edi,[ebp-8] (??? 0 555)". I have now put in a 
            fatal "?9/0" where it was quietly outputting that gibberish.
            Update: the gibberish had to go back for filedump.exw.
01/08/2013  Bugfix. As spotted in passing some time ago, opMove was moving reg 
            to edx before calling dealloc. Now moves prev (not reg) as it should.
            This had made a compiled pgui corrupt the null string ("") constant.
            (Which not surprisingly took me quite some time to track down!)
24/08/2013  Bugfix. builtins\pcmdln.e (aka command_line()) was being a bit over-
            zealous in applying get_proper_path(). Since a file with the exact
            name of "C:\Program Files (x86)\Phix\x" just happens to exist on my
            machine, running 'pw test -isense "x"' got me a commmand_line() of
                {"C:\Program Files (x86)\Phix\pw.exe",
                 "C:\Program Files (x86)\Phix\test.exw",
                 "-isense",
                 "C:\Program Files (x86)\Phix\x"}
            It now only applies get_proper_path() to the 1st & 2nd elements.
            (It was actually the quotes around the x that triggered this, btw.)
20/09/2013  Bugfix. The following code illustrates the problem:
                include builtins\pcfunc.e
                function r(string s) return routine_id(s) end function
                ?r("open_dll")
            which displayed -1. Once there are any unresolved routine-ids all 
            potential targets must be kept. I found some commented out code in 
            pemit.e, then changed a few tests higher up (putting lots more on 
            the S_Slink chain), seems ok now.
21/09/2013  Bugfix. Missing opUnassigned before opMkSq, causing the error message
            "fatal exception [MEMORY VIOLATION] at #hhhhhhhh" to appear instead
            of the desired "variable xxx has not been assigned a value". This
            could only occur when xxx was of a T_integer type.
21/09/2013  Bugfix. Attempt to subscript an atom in ReconstructIds() when some
            unresolved routine_ids triggered that call in the non-bind case at
            the end of pemit.e. The changes from yesterday (20/09) seemed to 
            trigger this but only because I was testing/using more unresolved 
            routine_ids. Possibly/probably introduced 07/01/2012 (date found
            in the source not matching anything logged above).
23/09/2013  Bugfix/rule change. Since multiple assignments are performed right-
            to-left (in order to get any subscripts right), the following code:
        
                sequence name = {1,"name"}
                integer encoding
                    {encoding,name} = name  -- (now produces compilation error)

            was equivalent to

                    name = name[2]
                    encoding = name[1]

            which set (name to "name" and then) encoding to 'n' (not 1), and

                    {name[2],name[1]} = name -- (ditto, compilation error now)

            was equivalent to

                    name[1] = name[2]
                    name[2] = name[1]

            which changed "name" to "aame", when you probably expected "anme".

            The procedure MultipleAssignment() in pmain.e has been altered to
            issue "in {..}=x, can only assign x in the leftmost lhs element"
            to help avoid such problems. The following is still valid:
            
                sequence name = {"name",1}
                integer encoding
                    {name,encoding} = name
            
            since "name" on the lhs is/only occurs in the leftmost position.
            Such problems do not and never did arise if there is any kind of 
            operator, subscript, {}, or function call on the rhs. Technically,
            I might concede that "tmp=name, {...}=tmp" might be a more correct
            solution, if anyone ever finds me a suitably convincing example.
22/10/2013  Allowed (eg) sequence a,b,$ which I thought it already allowed, but
            on closer inspection that handling was only on enum and constant.
22/10/2013  Got rid of the last #ilasm, everything should now use #ilASM.
            [Update: found some more 10/1/14]
29/10/2013  Added a simple auto-include, tagset(N), which returns a sequence
            {1,2,3,...N} for use in tag sorts. Also added demo\tagsort.exw
            [DEV help docs still need updating]
01/11/2013  Bugfix opSubsss/e09slin (slice length is negative) was getting the
            wrong return address off the stack, reporting error on line "-1".
17/11/2013  Spotted an optimisation in {ts,te} = {s,e} which meant I could
            avoid building the temporary sequence and assign directly: being
            when no lhs occurs on the rhs, and no subscripts are involved.
            New flag get_from_stack added to pmain.e/MultipleAssignment().
            Also, fixed a missing tmpI issue with opRepe (in "") and created 
            a suitable test case for it, see test\t57masgn.exw, and also 
            fixed an incorrect error line bug in opRepe.
08/12/2013  Added trunc() (autoinclude) to misc.e, just to test a post which
            appeared on EuForum.
30/12/2013  Minor tweak to database.e: it now returns a string rather than a
            dword-sequence when appropriate.
31/12/2013  Ported win32dib and demos to Arwen (was dependent on win32lib),
            and added them to the distribution in the \demo directory. One of 
            them (feedback) seems nearly twice as fast, but sadly that just 
            means it crashes/hangs twice as often as it does on win32lib...
01/01/2014  Bugfix. A simple file of "include builtins\pgetpath.e" worked 
            fine but the command "p builtins\pgetpath.e" threw a wobbly.
            (Problem was encountered testing intellisense on that file.)
            Traced the root cause to pgetpath.e using pcurrdir.e which uses 
            get_proper_path().. and main.e\checkforbuiltins() not checking 
            for includeFile() returning fileno<0. It ("") now resets fileno.
            May also help with some problems Phix has with OE4 \std files.
            Update: Actually the problem with OE4\std is that implicit forward
            calls are local; it would be quite wrong to automatically elevate
            them to global as the global definition is found, however it might
            be rather sensible to elevate them (& warn under -lint) instead of
            suffering a fatal compilation error that could be avoided...
            Update: see 14/02/2014.
01/01/2014  Finally added block scope. Took me less than an hour. However,
            I did nothing about immediately decrefing the variables.
06/01/2014  Couple of tweaks to psym.e to handle std/pretty.e which contains:
                namespace pretty
                procedure pretty(..)
            1) at first it went ":" expected, 2) then it went "a namespace is
            required, pretty is defined in pretty.e and pretty.e". It now
            skips namespaces (S_Nspc) unless Ch=':' or inWhat=InNS (the latter
            is only used by pmain/resolveRoutineId() when it finds a ':').
            [Update: needed a fix 16/6/14, see below]
10/01/2014  Replaced ilasm with ilASM in test\swtime[2]. Required updates to
            pilasm.e to support "jmp [ecx*4+:jmptbl]" and "jmp_table_entry :l1"
            Also updated test\t97mleak.exw and demo\cpuid.exw, which should be
            the last of them; then again I said that nearly 3 months ago.
13/01/2014  Bugfix. Under profile_time (and most likely profile and trace), when
            processing an unconditional backward jump (ie an "end while") the
            compiler used length(x86) from before emitting the opLn(pt|p|t) to
            calculate the offset, which was therefore off by 10 bytes. Caused
            particularly confusing results in eaedb.e/bulkUpkF(), which basically
            only iterated at most once. See edita/readme.txt 02/01/14, where I
            wrongly claimed there was some database corruption, but presumably it
            was removing profile/trace in preparation for running verify edita.edb
            which made the difference. pilx86/opJmp now explicitly calls lineinfo() 
            before calculating joffset.
23/01/2014  Bugfix (pending). Found that a puts(1,s) could fail silently (with 
            ERROR_NOT_ENOUGH_MEMORY) when passed a very long string. On the machine 
            where this was first noticed, the string had to be over 62,600 bytes long. 
            (It was just a quick debug line, "?s", which surprised me by showing nowt.)
            Further investigation yielded several such limitations documented throughout 
            the windows API, ranging from 35K (pipes in 64-bit/Unicode processes) to 
            1.3MB (the default maximum working set size). Generally speaking these 
            limitations do not apply when working with disk-based files, but may occur 
            when dealing with consoles, pipes, and other file redirections. To avoid the
            issue, modified builtins/pfileio3.e to split console writes into 8K blocks.
            Phix does not automatically use that file yet, but it is planned (soon), 
            and it is thoroughly tested and matches the speed of the (closed source)
            backend it is intended to replace.
24/01/2014  Added checks for zero length replacement slices in strings, so that (eg)
                string s = "123456"
                s[3..5] = {} -- (as opposed to s[3..5] = "", which was always fine)
            no longer causes a type check. Obviously if the replacement length is 0,
            there is no need for opReps to auto-expand s to a dword-sequence, even
            if it could be argued that higher-level type checking should ideally be
            in place.
02/02/2014  Added typechecking to multiple assignments. (Dunno how I missed that.)
02/02/2014  Bugfix. Ctrl-Click on "decode" of "p2asm:decode" completely ignored the
            "p2asm:" part. Added trapns to pglobals.e to cope. Also needed a loop in
            ptok.e to scan all the way down the S_Nlink chain. Lastly, added tests
            ("or chovline!=line or chovfrom!=col") to eaisense.exw which makes the
            intellilink appearance much more responsive - no more need to move the
            mouse over another word before going back to the one actually wanted!
            (This is really a bug in Edita/intellilink which needed mods to Phix.)
06/02/2014  Created builtins\timestamp.ew to handle DateTimeStamp fields. Moved 
            DateTimeStampToString from axtra.ew, added SYSTEMTIMEtoDateTimeStamp 
            and FILETIMEtoDateTimeStamp routines.
13/02/2014  Bug: "for i=1 to length(s) then" compiled cleanly. DoFor() in pmain.e
            clobbers ttidx when creating the symtab entry for "i", after to/by have
            been parsed, so I had (sloppily) replaced Match(T_do) with getToken(). 
            It now saves ttidx (in do_ttidx) for a proper check.
14/02/2014  Moved code from pmain.e/DoRoutineDef to pilx86.e/BackPatchForwardCalls
            so that it could be called from the final unused_cleanup(), instead of
            Undefined() if (and only if) an implicit forward call (assumed to be a
            local) can be resolved as a unique global routine. This should reduce
            the number of places you need explicit forward definitions. This had
            just been niggling me for months, nothing specific needed fixing.
            Also added test\t59mri.exw, which may get extended a bit more yet. For
            one thing there are cases where it just blatently ignores a namespace.
            Update (18/2/14):
            Originally I was only going to issue a warning under -lint, but after
            that initial dithering, realised this is a last chance hail mary with
            limited testing, far better to encourage programmers to add the said
            explicit forward definitions, use the pmain.e code, and keep symtab 
            free from unnecessary/duplicate/forwarded entries. (As well as that 
            ignored namespace issue, which could prove unnecessarily fiddly - as
            obviously if you do it all on the first pass then you have everything
            to hand, but otherwise you have to save namespace idx or 0, and have
            some way to make InTable() use that remembered namespace, etc, etc.)
20/02/2014  Yet more bugs in command_line(). Basically eg "..\p" attempted:
                tmp = get_proper_path("C:\\Program Files\\Phix\\p")
                res = c_func(xSearchPath,{"",tmp,".exe"...})
            which is completely wrong; get_proper_path returns "" (as no such
            file exists) and we don't get much further. It now does:
                tmp = get_proper_path("C:\\Program Files\\Phix\\")&"p"
            and then xSearchPath can apply the extension(s) quite happily.
21/02/2014  Fixed one of the outstanding #isginfo in pemit.e that was broken in
            pth.exw. Boiled things down to a very small program, basically if
                mzpe = repeat(' ',mzpelen)
                mzpe[1] = 'x'
            were analysed in the wrong order, opRepe1 reset the length to -1.
            Not entirely convinced this is right, but it did the trick, added
                if dlen>0 then
                    vlen = dlen
                end if
            to pilx86.e/GetDest().
21/02/2014  For some reaon, psym.e/addRoutineId() was setting S_used, so eg
                function keyHandler()
                    return 0
                end function
                constant r_keyHander = routine_id("keyHandler")
            was failing to issue an unused warning for r_keyHander. I noticed this
            triggered 3 new warnings in the -test set (all now fixed/suppressed).
22/02/2014  Bugfix: commenting out the r_Assignment=routine_id("Assignment") line 
            made opSubse1i in ilxlate() crash with ?9/0 because constant propagation 
            made opPow leave the tmpd (aka tmptransfer) that opRmdr kindly left it, 
            but it does not actually need. In other words, when opPow decides it can
            just "mov [lMask],1", it must remember to set tmpd to 0. Added this as a
            test (opPow001) to terror.exw:
                sequence symtab = {0}
                integer r_Assignment
                integer lMask
                procedure Assignment(integer tidx)
                integer lprev
                    lMask = power(2,remainder(tidx,29))
                    lprev = symtab[1]
                end procedure
                --r_Assignment = routine_id("Assignment")
                Assignment(0)
                r_Assignment=9/0
            (Encountered this while working on that #isginfo problem yesterday.)
06/03/2014  Modified pmain.e/Compile() to process all namespace/format/with/
            without at the top of the main file before including pdiag.e, and
            removed the erroneous 'format "p.fmt"' from pdiag.e.
28/03/2014  Minor tweak to pprntf.e: printf(1,"%,d",1000000) displays "1,000,000",
            and sprintf("%,3.6","123456.654321") returns "123,456.654321", ie a ',' 
            immediately after the '%' (only valid for 'd' and 'f') inserts a comma 
            every third character from the end, or '.' if there is one. Note that
            the comma is instead of ('0'|'-'|'+') ie zerofill/leftjustify/showplus
            and cannot be used at the same time as them. (Just something I needed.)
29/05/2014  Made plist.e attempt to align #ilASM generated assembly with the 
            preceding source code line, based (solely) on the leading mnemonic. 
            Added a couple of equivalents: je/jz, and jne/jnz, more may be required. 
            May cause occasional odd artifacts, if so it can be disabled simply by 
            changing the constant ALIGNASM=1 in plist.e to 0. It certainly makes the 
            listing files a bit easier on these tired old eyes!
02/06/2014  Added columnize() auto-include (builtins/pcolumns.e) from Eu 4.1. Tidied
            up the code a bit, and had to fix a bug in the default parameter handling 
            during testing, whereby it incorrectly reported "missing parameters".
02/06/2014  Added multiple assignment on declaration, so you can now do things like:
                constant {field,desc} = columnize({{"field1", "desc1"},
                                                   {"field2", "desc2"}})
                sequence {group,code} = columnize({{"group1", "code1"},
                                                   {"group2", "code2"},
                                                   {"group3", "code3"}})
            which is equivalent to:
                constant field = {"field1", "field2"},
                         desc = {"desc1", "desc2"}
                sequence group = {"group1", "group2", "group3"},
                         code = {"code1", "code2", "code3"}
            Of course that is not particularly impressive for such tiny tables, but
            if you are setting up two or more tables with hundreds of entries that 
            absolutely must correspond, for expressions like desc[find(name,field)],
            then, trust me, being able to keep all related items together on the one 
            line (eg {"field2", "desc2"}) can definitely make things far less error 
            prone and easier to extend and maintain. Obviously, there is absolutely
            nothing special about columnize(), within reason just about any other 
            function or expression could be used instead. Also quite obviously, you
            should expect a bit of a performance hit if you do this sort of thing
            significantly more than the once-at-startup of the original intention,
            or rather I should just admit that, for once, I have not spent(/wasted)
            any time considering, testing, or improving any performance aspects.
02/06/2014  Bugfix. res = {getEncoding(res),res} was using pbr in the call and then
            failing with res has not been assigned a value. Additional code added
            to Assignment() to suppress the pbr handling if we still need lhsvar.
            It seems I had that code all along, but failed to relocate it properly
            when I migrated the onDeclaration:=2 to after the Expr() call, whenever
            it was that I did that (2,3 years ago?).
06/06/2014  Bugfix. opRepe1 crashed on x[i] = r when x was unassigned. Unable as I
            was to find out exactly what the problem was (neither inspecting the 
            source not running OllyDbg were fruitful) and considering that it is 
            likely to be rewritten soon(ish), I just opUnassigned things, which may
            make things a bit slower (by 1 clock) but then again all my benchmarks
            have gone to pot since this Evo-Gen nonsense started... Life is sad :-(
            Exactly the same problem with opRepe, fixed in the same manner.
                -- 6/1/14 skip namespaces if next char is not ':'
                --else
            [Update: a bunch more of similar things were fixed 27/1/14]
16/06/2014  Bugfix. two (or more) "include <file> as ns" statements were no longer
            triggering a fatal compilation error (duplicate namespace). Introduced
            6/1/14. It now does /not/ skip namespaces when fatal=0. Note this means
            that technically the case solved 6/1/14 (see above) is now only "fixed" 
            when the namespace occurs before the procedure, which it always should.
            (I say "fixed" since namespaces are still in the same identifier-space 
             as everything else, which afaik is different in OpenEuphoria, besides
             which it was always much more of a compatibility hack than a fix.)
20/06/2014  Bugfix. Resource leak in Arwen/setFont. It now calls DeleteObject().
08/07/2014  Added UpDown controls to Arwen. Note they rely on UDS_AUTOBUDDY as
            things stand, and if labels (etc) are logically bigger than the size
            they appear, it may(/will) crop the auto-sized updown control. See
            arwendemo\boids3d.exw (once finished) for an example of use.
16/07/2014  Added demo\ArwenDemo\boids3d.exw, and Win32Demo\boids3d.exw, from 
            which it was ported. The arwen version also has proper resizing.
            Note that the new aextra.ew functions (drawPolygonh etc) are a bit
            of a hack and may not be supported as-is in future releases.
26/07/2014  Bugs in pbinary.e (independant wordaround applied) caused me to drop 
            opRepe1is, as that was being applied incorrectly, when ref/res was a 
            dword-sequence. It may or may not be resurrected when opRepe1 (etc) 
            is ported to #ilASM{}. (I would have dug deeper were I not planning 
            to replace it all.)
27/07/2014  Something has gone badly wrong in the final exception handler. Since
            it is all going to be replaced anyway, resorted to several quick fix
            such as opUnassigned (*4 in pmain.e) and several je e94vhnbaavexx in
            the asm backend. opJcc/opScc/opXor/opRand/opTrace/opProfile/opMemSet
            /opJifX/opJnotx were all affected, and quite possibly a few others.
            At least test/terror.exw now runs through cleanly once again.
23/08/2014  Added more opUnassigned ahead of opAdd/Sub/etc.
01/09/2014  Bugfix. Silent failure should you code something like:
                constant op1 = 1
                for op1=1 to 4 do
            It would have given an error if op1 not integer, but I missed this.
            Internally, it was damaging op1, but using a literal '1' everywhere, 
            including inside the for loop. It now delivers the compilation error
            "already declared as a constant", as it always should have done.
02/10/2014  Added "-repl" command line option, whicih prompts for and executes 
            Phix statements. Technically a repl is read-eval-print-loop, but
            there is no print (so it is really a "rel" rather than a "repl"), 
            you have to enter eg "?3+5" or get an error. Much more work is 
            needed, mainly to stop compilation errors terminating everything,
            and there's at least one ugly "hang" somewhere. Will get back to
            this as 0.6.4 progresses a bit more.
03/10/2014  Added puthex32() to (tmp) puts1.e, to get a start on diagnostics.
            Also completed the work needed for ELF32/64 versions of gets0(),
            but at the moment just testing that it compiles & lists cleanly.
08/10/2014  Bugfix: x = length(s) was not properly decrementing the refcount
            of the previous (non-integer) content of x. Spotted in passing, 
            no symptoms were ever spotted (in probably 6 years!), I can only 
            assume that all my code always stores length() in an integer.
09/01/2015  Bugfix. Localtype was all messed up for if string(s)/else case.
            Caused a whole slew of code to be omitted from the executable.
            Made pltype.e/mergeBlocks restore FLIPPED entries under FULL. 
            See example added at end of t46pltype.exw. 
12/01/2015  Finally completed migration to the new packed (string) linetabs,
            as I'm about to start using them alot (test/terror etc).
14/01/2015  Bug fixes to %g and %e format in printf etc. %e could result in
            "1e+", now prints "1e+0". %g was not removing trailing 0s.
            Also updated the help with some Phix-specific points.
25/04/2015  Added mov edx,routine_id("open"), mov ecx,$_Ltot, jmp $_il to
            #ilASM, for examples see pfileioN.e
18/05/2015  Added dbinit, db_init() to database.e, to make it safe for
            forward calls/autoinclusion.
02/08/2015  Fixed a nasty bug in pttree.e; it was getting the -1 (string) and
            -2 (identifier) terminators the wrong way round. The initial symptom
            was that routine_id("Compare") in edita\src\eacomp.ew was not getting
            resolved at compile-time, along with two others, presumably because
            there were previous "\"Compare\"" constants, eg one in eamenus.ew.
            The actual bugfix was "ttCh<0" ==> "ttCh<-2" in tt_search().

Version 0.6.2
=============
14/08/2010  [Thanks to Les Bothwell] builtins/pcfunc.e (introduced in 0.5.9) 
            was not handling dword-sequences correctly. It was checking each 
            element for atom but then doing nothing with it, resulting in spaces
            being passed to whatever function was called. It now uses the common
            toString() routine. Also, reverse() was always returning a dword-
            sequence, changed to return a string when that was passed to it.
20/08/2010  Discovered much missing error checking introduced by optional/named 
            parameter handling. Tedious, but I think I got everything concerning
            the number of parameters, on both normal and (implicit) forward calls.
            Also spotted, forward-calling a routine as a function but then later
            declaring it as a procedure, or vice versa, had never previously been
            reported as an error, presumably causing any and all manner of run-time 
            failures. However, I suspect more could be done regarding type checking 
            of parameters on forward calls. [DEV]
21/08/2010  ALERT: while adding Export() to test\terror.exw, I noticed a
            probable bug in file i/o. The raw text had embedded "\r\n" in
            it, since that's what the EditText needs. However writing such
            to a file opened in text mode produced very odd/duplicate text.
            For now, I just changed terror.exw/Export() to use binary mode, 
            but I need to take a closer look at this.
            Update: I made a change in this area 31/01/2012, though that was on
            read rather than write and may not be the same thing. I have at
            some point (no idea when) removed the "wb" from Export().
21/08/2010  [Thanks to Les Bothwell] demo\xpm\run.xpm missing from the distro.
            (Much work is still needed on demo\ArwenDemo\FlatToolBar.exw.)
            [Update 11/9 it seems better now, but still something amiss.]
            Also, the demo/Win32Demo/win32lib.ew "stub" said:
                --include ..\win32lib\win32lib.ew
                include ..\win32lib6\win32lib.ew
            which should be the other way round.
21/08/2010  Removed code from pdiag.e/getValue() which fouled up the ex.err.
            (marked with "--DEV 24/6/10 (need to experiment a bit here...)")
21/08/2010  Added F8=Step(OVER) and F9=Step(OUT) to the debugger.
            F7/F8/F9 chosen to match other debuggers I currently use, btw.
            F7 behaves the same as return, ie single step.
            F8 behaves like downarrow in RDS Eu, resuming on the next line,
               ie not stopping on any line in the routine(s) being called.
               (behaviour of downarrow in the Phix debugger is noted below)
            F9 resumes trace after the statement calling the current routine.
               NOTE: pressing F9 when at the toplevel will typically execute
               the entire remainder of the program without stopping again.
               (In OllyDbg, F9 is execute until return, whereas in Borland 
                C++ Builder, F9 is resume execution, like 'q' in Phix.)

            As a reminder, the keys currently honoured in the debugger are:
            [page]up/down/left/right/home/end/tab: navigate the source code.
            +/- increase/decrease variables area. [needs work]
            q quit (resume normal execution)
            Q Quit ("", permanently)
            ! abort (create ex.err)
            ? inspect variable (** more work required here, for >1 var **) [DEV]
            F1 show run screen (unless already showing that) [DEV shd be "help"]
            F2 show debug screen (actually, any key will do after F1 keyed)
            F3 (temp) ?9/0, commented out for 0.6.1 release.
            F4 unused
            F5 unused
            F6 animate (ie trace until key press)
            F7 step into (==return)
            F8 step over (ie execute immediate call(s) without trace)
            F9 step out (ie resume in calling routine, if any)
            F10 unused
            F11 unused
            F12 unused
            Of course the console mode debugger is just a stepping stone to
            a nice modern gui debugger, one day, or perhaps pdebug.e is exactly
            what we want in p.exe, but not at all what we want in pw.exe[?].
22/08/2010  Added "\xhh" to string handling, same as "\#hh" (inline hex byte).
            (Only because I got very bored reading a very tedious C++ book,
             and figured it might one day be one less thing that trips someone 
             up when converting some C++ code snippet to Phix. "\#hh" handling
             has been around for quite a while.)
29/08/2010  Problem with eg:
                if 0 then
                    p(1)
                end if
                procedure p(integer i)
                                    ^ routine previously called with no parameters
                end procedure
            Of course what that message is really trying to say is "there was a p()"
             (or rather it misinterpreted the p(1) call under emitON=0 as that),
             "so this would be alright, if only there were some defaults here".
            The real problem was that it was initialising the (empty) backpatch list
            under emitON=0, or at least that's the bit I just fixed. (ie there may 
            well be some other quite similar problems)
02/09/2010  Bugfix in opFind. If the result var was non-integer, the attempt to 
            decref it was coded wrongly (edx*8 should have been edx*4), and further
            the error trapping code had been accidentally left commented out with 
            a ";;DEV temp: (removed 14/10/09)" (my guess would be I've previously
            stared at that line of code unable to understand what was going wrong).
            The net result was a low-level machine crash reported either on the call 
            to the routine containing the find, if you were lucky, line -1 if not,
            and/or it could also lead to spurious memory leak reports.
            Of course not much real-world code stores a find result in a var that
            does not already contain an integer, but win32lib/CreateForm does, so
            ex05,6,14,22 and maybe other win32lib demos are working now.
05/09/2010  Minor tweak to Arwen. A test program failed on RDS Eu because it did not
            open shell32.dll, with the class "ToolbarWindow32" unregistered. It now
            catches this error (once) and registers it before retrying. This problem
            did not appear to affect Phix, presumably because p[w].exe opens shell32
            automatically anyway, not that it still uses it. Anyway, it paves the
            way to remove the dependency on shell32 (for programs that otherwise do
            not need it), should that ever become a good idea, and of course irons
            out hopefully the last discrepancy between Arwen on Phix and on RDS Eu.
11/09/2010  A builtin with the wrong number of parameters, eg "equal(x)", was not
            being correctly reported as an error. As per 20/08 above.
11/09/2010  Added find_replace() auto-include builtin (see builtins/findrepl.e).
            (Only because I saw it being used on EuForum)
12/09/2010  Added cleanup code to pdebug.e, should stop the spurious reports of a
            memory leak after using trace(). [DEV only partly helped ;-(]
12/09/2010  Added -batch command line option, makes test/terror.exw (test errors)
            much slicker by suppressing all console i/o.
16/09/2010  Migrated call_proc and call_func from the asm backend to pcfunc.e, to
            complete the set. I should note that an out-of-date pcfunc.e (as shipped
            in Edita\builtins) will trigger conflicts, should it load of both the
            C:\Program Files\Edita\builtins version (ie the old one) and the new one 
            in C:\Program Files\Phix\builtins. Overwriting the offending file should
            fix any problems. [DEV I should version it somehow]
23/09/2010  OOPS: undid the 16/09 changes. It somehow introduced a "random" error in
            test\t19find.exw, which afaik does not use them at all, and needs further 
            investigation. I should also note that switching between the hll and asm
            versions is not trivial, there is a right and wrong order to do it, which
            I failed to write down. [update: this is now fixed]
26/09/2010  Added the cursor() builtin, as per RDS Eu.
09/10/2010  Fixed isConstRef[Count] handling when interpreting. Previous versions of
            pilx86.e had "if bind and" before using it. After adding new code to the
            blurph() routine in pemit.e, to fix them up immediately rather than via
            a chain, the big problem was that ReconstructSequence() was being called 
            after blurph(), when it needed to be done before. I should mention that 
            it would seem that logically there is a similar situation with floats, 
            though maybe because they cannot be nested it simply never arose. I did 
            check however, found nothing, and all the tests I have passed just fine.
18/10/2010  Allowed multiple global constants with the same name and the same value 
            to co-exist, instead of issuing a "namespace qualifier is required" 
            message. Changes are in psym.e, routine InTable(): as (each) second
            potentially ambiguous definition is found (hits=1), it checks for both
            constants (S_Const), the same type (S_vtype), the same value (S_value),
            and K_noclr bit set on both. For example TRUE is declared in common.e
            and std/types.e (in RDS Eu 4.0). It may also be necessary to check for 
            the K_lit bit being set on both as well, but probably not. Needless to 
            say all existing tests and demo programs work fine with these changes, 
            but then again none had any conflicting/duplicate globals anyway.
            (Required as part of my on-going efforts to get more 4.0 bits to run,
             though bear in mind the goal is "a few quick edits" to "port" code
             from RDS Eu 4.0 to Phix, or make it run on both, rather than to just
             slavishly copy every single new feature of 4.0.)
22/10/2010  Running say "..\..\p -c t9" cannot open pdiag.e etc. Also command_line
            was returning "C:\\Program Files\\Phix\\p" when it should be returning
            "C:\\Program Files\\Phix\\p.exe" [both fixed]
05/12/2010  Added icons to demo\ArwenDemo\demo_tabs.exw.
11/12/2010  Finally fixed up arwendemo\demo_toolbar.exw, over a year after I got
            imagelists fully working.
28/12/2010  Phix now has a switch statement!
02/01/2011  Added delete_routine and delete, and rewrote opMkSq to push elements
            onto the stack already pre-incref'd, or move/zero tmps and function 
            returns, thus removing a significant cause of "ref count leaks".
11/01/2011  Fixed bug in "p -d -test". plist.e was altering opNames but then not
            resetting things for the next test. (Of course "p -d -test" has no
            real use except as a stress test of the compiler.)
12/02/2011  Added include_paths() routine for compatibility with RDS Eu, see
            builtins/pincpath.e and the corresponding initialAutoEntry entry
            in psym.e
12/02/2011  Decided to remove the "ingore namespace" logic, whereby std/ includes
            quietly skipped the namespace line. Many of the standard 4.0 includes
            need explicit forward declarations to work properly on Phix, eg
--/**/global integer GET_SHORT_ANSWER, GET_LONG_ANSWER
--/**/forward global function value(sequence st, integer start_point=1, integer answer=GET_SHORT_ANSWER)
--/*
namespace stdget
--*/
            This should help prevent problems with using incompatible includes.
            Phix requires explicit (local) definition of namespaces, eg
                include get.e as stdget
            Personally speaking, if after "include get.e" I see "stdget:xxx",
            then I feel it is more trouble to find out where "stdget" is 
            actually defined than this feature gains, and/or just get annoyed
            when the more obvious "get:xxx" does not work... I also feel quite
            strongly that such "global namespaces" are "essentially broken" in
            that they very easily create the very conflict they mean to fix.
            (In fact, "stdget" is probably not "get" for exactly that reason.)
            (To put the quiet ignore back in, search for T_namespace.)
14/02/2011  Added fallthru/fallthrough as standalone statements to switch (ie 
            the opposite of break). Seems I completely forget about them.
06/03/2011  Fixed an index out of bounds in opSubse which was being reported
            (somewhat unhelpfully) as a "MEMORY VIOLATION", when the final
            segment was a string. It was just a simple matter of picking up
            the return address from the wrong place.
09/03/2011  Added compatibility implementations of insert(), splice(), head(),
            tail(), remove(), replace().
27/04/2011  Implemented crash() and fixed crash_file().
27/04/2011  Bugfix: after eg include dll.e as dll, defining a routine parameter
            also named dll gave a ":" expected error.
28/04/2011  Bugfix: defining a third global split() routine when two had already
            been defined gave a namespace required error (attempting to fixup a
            previous forward global declaration). In the particular case this 
            occured (iup\layout.e clashing with std\regex.e and std\sequence.e)
            none of them were forward anyway. Fixed with one caveat: if you
            were to declare (in multiple source files):
                forward global routine split()
                global routine split()
                global routine split()
            then the (first) forward definition would automatically be resolved 
            to the first actual definition, without any warning. I doubt that is
            likely to be a problem for anyone, he says.
29/04/2011  Bugfix: atom subscripts in opSubse were not saving eax, causing all
            manner of unpredictable results. Carefully checked everywhere else
            for similar errors, couldn't find any.
01/05/2011  Bugfix: c_func/c_proc now save/restore esp, to allow for cdecl calls
            even when they are defined as stdcall routines (to match RDS Eu).
            Another couple of EuIup demos now work properly, with just pplot.ex
            and euchat.ex left needing to be fixed.
30/05/2011  Fixed the problems in hll call_proc/func as defined in pcfunc.e and
            switched to using them instead of the previous assembler routines.
            One problem was that the hll routines set their side effects wrong;
            added #ilasm{e_all} to handle it. [DEV] I suppose I should also add
            #ilasm{e_side,<hll_name>} for more fine-grained settings.
30/05/2011  call_proc/func now support optional parameters properly.

Version 0.6.0
=============
06/06/2010  Fixed problem in opOrBits whereby it was testing dtype instead of
            sltype. This fixes the first problem 0.5.9 has with Edita, now it
            is just a matter of finding that memory leak...
08/06/2010  Tracked down the memory leak, see test code added to the end of
            test/t49ginfo.exw: it was using opRepe1ip when it ought not to.
09/06/2010  Random crashes in pdiag.e, traced to a) not skipping the new T_ebp
            and b) setting the new T_ebp before decrypting the data section.
            (T_ebp has since been removed)
09/06/2010  opSubse1 was obtaining addr idx from [esp]-19, when it should be
            [esp]-20 (as was stated in pilx86.e all along), resulting in an
            oops/"variable %s has not been assigned a value" from pdiag.e (ie 
            it could not fill in the %s).
12/06/2010  Modified pdiag.e to dump more of eg "allfiles", when processing a
            sequence rather than using the entire "allotment" (of 500) on the
            first, it now tries for 99 per sub-element. [update: put it back
            to the way it was, for now, as it gibberised some stuff.]
18/06/2010  A statement such as symtab[1] = append(symtab[1],99) performs a
            symtab[1]=0 over the call to avoid copy-on-write oveheads, but in:
                if 0 then -- false
                    symtab[1] = append(symtab[1],99)
                end if
            the append call was correctly omitted, the zeroisation not. It was
            missing an "if emitON then" wrapper in Assignment/fastSubscriptLHS.
            (btw: it can do this because it knows append (and prepend) cannot
             reference symtab, plus it checks that the "99" does not reference
             it in any way either.)
18/06/2010  Removed the oneString constant and corresponding (linewise) code.
            (mainly because I was never going to get defaulted parameters to
             work under oneString=0, there is simply no reason to even try.)
24/06/2010  BUGFIX: A difficult thing to explain, and one I'm not entirely
            happy with, the following code went wrong:

                sequence x86
                --/**/  #isginfo{x86,0b0100,MIN,MAX,integer,-2} -- Dsq of integer (unknown length)

                procedure emitHex5j()
                    x86 &= 0
                end procedure

                integer dbpos

                x86 = {}
                emitHex5j()
                dbpos=length(x86)
                x86[dbpos] = 1
                dbpos=length(x86)   --<< BUG

            The last line just set dbpos to 0, whereas the one two lines up
            emitted the correct code (use "p -d -nodiag test" and examine the
            resulting list.asm). The problem was that the opRepe1 retrieved
            the "new" value of dlen (0, from the x86={}), and re-saved it...
            What seemed pretty straightforward soon turned into a nightmare!
            After several false starts, added vlen to pilx86.e. This caused
            one knock-on effect, divm in pemit.e now has a "final" length of
            -1, which used to be -2 (my other attempts to fix this made many
            more things go wrong!), which is odd rather than badly wrong. (A
            -1 means "we don't [yet] know", -2 means "any length".) At least 
            everything else seems to work now...

25/06/2010  Finally finished defaulted and named parameters.
            [DEV][DOC]:

            Defaulted Parameters
            ====================
            
            Specifying a default for a parameter makes it optional, eg:
            
                procedure p(integer i=37)

            which can be called using p() or p(5). Any non-defaulted parameters
            must be grouped together first (/on the left) and must always be 
            present on every call statement to the routine.
            
            The following default expressions are permitted:
            
                * (signed) literal integers
                * (signed) literal floats
                * literal strings
                * entirely-constant-only sequences
                * any single variable or constant name, including earlier params
                * length(<ditto>)
                * command_line()
            
            No operators of any kind are currently permitted, and the length()
            and command_line() are the only functions currently supported. While
            that may seem a bit harsh, there are sound reasons for keeping it as
            simple <sound of me choking here> and hence as bug-free as possible.

            That list was devised from analysing the std/ directory, and will
            be extended only when necessary. There was just the one case that is
            not supported, though easily worked round: regex.e uses the function 
            get_ovector_size() as a default - it is probably more readable and 
            debuggable after a simple and fairly obvious change.

        <aside>
            There is no way to determine how many parameters some external C or asm code
            has pushed onto the stack before invoking a call_back. Should you specify
            optional parameters on a call_back routine, the compiler will always assume
            the *maximum* number of parameters have been passed. Admittedly this is
            somewhat arbitrary, but either way it must make some kind of fixed assumption. 

            The recommendation (which may one day become enforced) is to provide some
            fixed-number-of-parameters-call-back-wrapper(s) for any routine with optional 
            parameters, eg:

            function somefunc(object a, object b=0)
                ...
            end function

            function CB_somefunc1(object a)
                return somefunc(a)
            end function

            function CB_somefunc2(object a, object b)
                return somefunc(a,b)
            end function

            and use either routine_id("CB_somefunc1") or routine_id("CB_somefunc2") as
            the parameter to call_back(), but never call_back(routine_id("somefunc")).

        </aside>

            If any routine has more than one defaulted parameter, is is usually
            a prime candidate for using named parameters, described next.


            Named Parameters
            ================

            Named Parameters allow values to be passed in any order, as long
            as all those needing to be present are specified somewhere. They
            are particularly useful in combination with multiple defaulted
            parameters.

            In the most verbose form, named parameters operate in a similar
            manner to namespaces. In the following paragraphs I use the term
            "paramspace" instead, to minimise any potential confusion.
            
                procedure someproc(object a, integer b)
                    ...
                end procedure
            
                someproc(someproc:b=5, someproc:a="thing")

            However, you may be pleased to learn/before you panic, there can
            be one and only one valid paramspace, hence we can omit it:
            
                someproc(:b=5, :a="thing")
            
            A minor syntactic tweak allows the more elegant (and recommended):
            
                someproc(b:=5, a:="thing")

            You should read the ":=" operator as "explicit assignment", not
            that it or the corresponding "==" (explicit test for equality)
            are widely implemented. Of course the "=" operator can stand for
            either, depending on context, however someproc(b=5,a="thing") is
            only ever going to pass two boolean 0/1 values, unless it causes
            a compilation error because there are no other a and b in scope,
            or perhaps a type error storing an integer in a sequence, etc.

            Technically, paramspaces are actually the inverse of namespaces; 
            the latter serve to disambiguate between multiple conflicting 
            globals already in scope, whereas the former serve to activate  
            an otherwise unavailable scope where the variable can be found.
            So namespaces solve the "too many" problem whereas paramspaces
            solve the "not any" problem.

            There seemed no particular reason to implement fully qualified
            namespace prefixes on defaulted parameter verbose paramspaces,
            however namespaces are perfectly valid elsewhere, eg/ie
            
                liba:proc(liba:proc:b=libb:func())

            is invalid (you may be relieved to hear!) but
            
                liba:proc(proc:b=libb:func())
                liba:proc(:b=libb:func())
                liba:proc(b:=libb:func())
            
            are fine, and can only be exactly what you meant anyway.

            Likewise there seemed no reason to interfere with the compiler's
            special treatment of routine_id(<literal string>); hence such as
            routine_id(:s="blah") and/or routine_id(routine_id:s="blah") is
            specifically NOT supported; the literal constant string would 
            not be resolved at compile-time as it would in the otherwise 
            equivalent routine_id("blah"). The program would work just fine,
            but the routine_id would be resolved at run-time rather than at
            compile-time.
            
            The use of named parameters on builtins/sources, or when calling 
            third party library code, is not generally advised anyway, since 
            names may change between releases. Unless of course the names are
            expressly part of the published interface.


            Remember:
            =========

            You need the ':' (and the actual parameter name).

            It is, of course, a leading ':' or a ':=' operator which serves
            to distinguish a paramspace / named parameter from a namespace.


            Example:
            ========
            
                function findit(object what, sequence table,
                                integer fromidx=1, integer toidx=length(table), 
                                integer direction=1, integer step=1)

            can be called using
            
                findit(x,xtable)
                findit(x,xtable,step:=2)
                findit(table:=xtable, step:=2, what:=x)
            
            In particular, the middle example is, I hope, much better than say 
            "findit(x,table,,,,2)", which Phix does not support and I have no 
            intention of ever implementing.

30/06/2010  Implemented 3rd (optional) parameter on find(), the start index, 
            and made find_from() an alias of find(). Negative subscripts from 
            -1 to -length(table), and positive subscripts from 1 to length+1
            are supported, along with any floats which round to integers in 
            that range, but any other value produces an error. 
            btw: it turned out that optional parameters on builtin routines 
            have nothing whatsoever to do with the above (25/06/2010) mods.
            Ditto all that for match().

08/08/2010  Distribution now includes a copy of win32lib (0.70.2c).

Version 0.5.9
=============
27/03/2010  '!' in trace window was not producing an ex.err
            (due to a spurious popad in e12pa)
27/03/2010  When s was {} and i was -1, s[i] was reporting:
                index 0 out of bounds, reading sequence length 0
            ie it was giving the index after fixup for negative indexes. 
            It now saves/gives the original (in fixupIndex):
                index -1 out of bounds, reading sequence length 0
            Similar changes were made to fixupSliceStart & fixupSliceEnd
07/04/2010  Fixed a bug in syswait.ew: it was using "command /C" when for
            compatibility with windows 9x/XP/etc it should be %COMSPEC% /C
            Among other things this meant that Edita/Tools/Verify edita.edb
            gave some very wierd messages when running db_compress(), as
            for some strange reason command.com exists on windows XP, but
            does not understand double-quoted arguments. (On Windows XP
            you are supposed to use cmd.exe instead of command.com, btw,
            and that is precisely what %COMSPEC% will do.) In addition, a
08/04/2010  system() call now waits up to 0.4 seconds to allow things like
            rename to take effect, otherwise db_compress() would invoke eg
            system(ren edita.edb edita.t5) then manage to open edita.edb
            before C:\Windows\system\cmd.exe had actually done anything.
09/04/2010  Fixed a nasty bug in the MakeString low-level routine. If you
            (eg) open "fred.txt", ie a string, then it opens that as you
            might expect, but if you use {'f','r','e','d','.','t','x','t'}
            instead, then it quietly creates a string, opens the file, 
            and frees the string before returning. (MakeString is common
            to many things: open, puts, define_c_func, and so on.)
            Problem is, it was 1 byte out, and therefore it could clobber
            one byte of OS-space, which causes any affected program to
            just die rather suddenly. Ooops!
14/04/2010  Stopped all attempts to branch straighten isAddr values.
            Eg (opReps followed by end while):

                ;   105         path[k..k+2] = ""
                    add ecx,2                             ;#0040FCE6: 203301 02                  uv 02 02  1  82      
                    mov [#00419540] (symtab[1057]),ecx    ;#0040FCE9: 211015 40954100            uv 00 02  1  83 02   
                    mov ecx,1                             ;#0040FCEF: 271 01000000               vu 02 00  1  83      
            >>      push #0040FD12                        ;#0040FCF4: 150 12FD4000               uv 00 00  1  84      
                    mov edi,#004197C0 (symtab[449])       ;#0040FCF9: 277 C0974100               vu 80 00  1  84      
                    mov esi,#00419540 (symtab[1057])      ;#0040FCFE: 276 40954100               uv 40 00  1  85      
                    push #0041953C (k)                    ;#0040FD03: 150 3C954100               vu 00 00  1  85      
                    mov eax,#00419534 (path)              ;#0040FD08: 270 34954100               uv 01 00  1  86      
                    jmp #00406362 (opReps)                ;#0040FD0D: 351 5066FFFF               v  00 00  1  86      
                ;   106     end while
                    jmp #0040FCCD                         ;#0040FD12: 353 B9                     v  00 00  1  87      

            Previously, the push (labelled with >>) was branch straightened
            to #0040FCCD, however if an error occurs inside the opReps call,
            it reported the error on the line before the while loop top...
            It is written like that, btw, because path[1][2][3..4]="" is also
            an opReps call, with 2 extra pushes between the >> and the jmp.
            In reality, such code interferes with branch prediction on modern
            CPUs and should probably be replaced with call/add esp,nn style 
            code, but when(/if) opReps is rewritten in #ilasm format.
24/04/2010  Moved current_dir from asm to hll (builtins\pcurrdir.e)
24/04/2010  Added "; ===\n" to list.asm so files appear in Ctrl Q popup.
24/04/2010  Added get_proper_path builtin (see builtins\pgetpath.e), and
            called it from current_dir() and command_line().
24/04/2010  Incorporated get_proper_path into p.exw (and ptok.e). 
            Does everything I hoped it would, even the following, when e9.exw 
            contains just "?command_line()" (bear in mind that just yesterday 
            I was getting all kinds of confusing gobbledegook):

            C:\PROGRA~1\Phix>p -c e9
            ==> {"C:\Program Files\Phix\e9.exe","C:\Program Files\Phix\e9.exe"}

            C:\PROGRA~1\Phix>P -C E9
            ==> {"C:\Program Files\Phix\e9.exe","C:\Program Files\Phix\e9.exe"}

            C:\PROGRA~1\Phix>E9
            ==> {"C:\Program Files\Phix\e9.exe","C:\Program Files\Phix\e9.exe"}

            C:\PROGRA~1\Phix>E9.eXe
            ==> {"C:\Program Files\Phix\e9.exe","C:\Program Files\Phix\e9.exe"}

            C:\PROGRA~1\Phix>c:\progra~1\PHIX\e9.EXE
            ==> {"C:\Program Files\Phix\e9.exe","C:\Program Files\Phix\e9.exe"}

            C:\PROGRA~1\Phix>P E9
            ==> {"C:\Program Files\Phix\p.exe","C:\Program Files\Phix\e9.exw"}

            C:\PROGRA~1\Phix>c:\progra~1\PHIX\P.ExE C:/PRogrA~1/pHIx/E9
            ==> {"C:\Program Files\Phix\p.exe","C:\Program Files\Phix\e9.exw"}

            In fact the only anomly I could find is that when you create a file
            say "Phix.exe" (ie open("Phix.exe","w[b]") to overwrite any existing),
            then if "pHix.eXe" exists, it keeps that odd case. Pretty sure that's 
            just the way the Windows filesystem works.
            
            [[BTW, I got the "C:\PROGRA~1\Phix>" prompt to occur on Windows XP
                   by running command.com from a cmd.exe shell, it stays like
                   that even when you "exit" the command.com back to cmd.exe]]

            (Whether or not there should be a "simple_command_line()" builtin,
             maybe just chopping current_dir() off the front of any returned
             paths, is an altogether different question, btw.)
06/05/2010  Added enums.
11/05/2010  Removed temp. constant useLower and tidied up the code a bit.
26/05/2010  Completed the re-implementation of define_c_func, define_c_proc,
            define_c_var, c_func, c_proc, and call_back in hll/#ilasm (see
            builtins/pcfunc.e). This was done since c_func was not working
            on Windows 7, as reported by Les Bothwell. At this point I doubt it
            will immediately solve the problem, but may make finding it easier.
            NOTE: At this point I noticed 0.5.9 was significantly slower than 0.5.8.
            I restored the asm c_func etc, to no avail, and have no idea where the
            problem is. However, I have copies of both on this box, and will attempt
            to morph a copy of 0.5.8 to 0.5.9 testing for a performance hit at each
            step of the way. This is likely to take several weeks, if not longer...
            (On the plus side, I found the bug with memory corruption checking, which
             was left on in 0.5.8 since turning it off broke self-hosting. There was
             an instruction in AllocFlt which was missing an "if debugmem" wrapper.)
01/06/2010  Added pcache etc to builtins\pgetpath.e, made it much faster.           
04/06/2010  Added 12 files to the "Phix compatible" set in ptok.e, namely database.e,
            dll.e, file.e, get.e, graphics.e, image.e, machine.e, misc.e, msgbox.e, 
            ppp.e, syswait.ew, and wildcard.e. The ones shipped with Phix should work
            fine on both Phix and RDS Eu, whereas the ones that come with RDS Eu make
            Phix jump through the pmach.e compatibility layer.
04/06/2010  Added some pushf(d)/popf(d) to the low-level cpu indentification routines,
            hoefully this will solve the issue on Windows 7.

