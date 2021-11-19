--
-- builtins/unit_test.e
--
--  A simple unit testing framework for Phix (autoinclude).
--
--      test_equal(2+2,4,"2+2 is not 4 !!!!")
--      test_summary()
--
--  If all goes well, no output is shown, and the program carries on normally.
--  You can easily force [summary/verbose] output, crash/prompt on fail, etc. [See docs]

--  Note that I have used the same routine names as Euphoria, but the parameters are
--       all different [esp their order] and therefore they are not compatibile...
--       In particular you have to give every test a name in Euphoria, whereas here
--       such things are optional. Also, Euphoria works by putting tests in files
--       named "t_*" and running eutest, whereas here they are part of the app, and
--       will start failing on live systems (eg) if not properly installed.
--
-- now in psym.e:
--global enum
--  TEST_QUIET          = 0,    -- (summary only when fail)
--  TEST_SUMMARY        = 1,    -- (summary only [/always])
--  TEST_SHOW_FAILED    = 2,    -- (summary + failed tests)
--  TEST_SHOW_ALL       = 3     -- (summary + all tests)
--  TEST_ABORT          = 1     -- (abort on failure, at summary)
-- (TEST_QUIET          = 0)    -- (carry on despite failure)
--  TEST_CRASH          = -1    -- (crash on failure, immediately)
--  TEST_PAUSE          = 1     -- (always pause)
-- (TEST_QUIET          = 0)    -- (never pause)
--  TEST_PAUSE_FAIL     = -1    -- (pause on failure)
--
integer tests_run    = 0,
        tests_passed = 0,
        tests_failed = 0,
        verbosity = TEST_QUIET,
        abort_on_fail = TEST_QUIET,
        pause_summary = TEST_PAUSE_FAIL,
        log_fn = 0

-- (aside: 0 rather than "" avoids need for a test_init(), when autoincluded:)
object module = 0,          -- (set to string by set_test_module)
       prev_module = 0      -- (    ""           show_module)

procedure test_log(string fmt, sequence args={})
    printf(2, fmt, args)
    if log_fn!=0 then
        printf(log_fn, fmt, args)
    end if
end procedure

procedure show_module()
    if prev_module!=module then
        test_log("%s:\n",{module})
        prev_module = module
    end if
end procedure

global procedure set_test_verbosity(integer level)
    verbosity = level
end procedure

global function get_test_verbosity()
    return verbosity
end function

global procedure set_test_abort(integer abort_test)
-- abort_test is TEST_ABORT(at summary)/TEST_QUIET/TEST_CRASH(immediately)
    abort_on_fail = abort_test
end procedure

global function get_test_abort()
    return abort_on_fail
end function

global procedure set_test_pause(integer pause)
-- pause is TEST_PAUSE/TEST_QUIET/TEST_PAUSE_FAIL (default)
    pause_summary = pause
end procedure

global function get_test_pause()
    return pause_summary
end function

global procedure set_test_logfile(string filename)
-- (closed by test_summary([true]))
    if log_fn!=0 then ?9/0 end if
    if platform()!=JS then
        log_fn = open(filename,"w")
        if log_fn=-1 then ?9/0 end if
    end if
end procedure

global function get_test_logfile()
    return log_fn
end function

global procedure test_summary(bool close_log=true)
    if (tests_run>0 and verbosity>=TEST_SUMMARY)
    or tests_failed>0 then
        show_module()
        string passpc = sprintf("%.2f",(tests_passed / tests_run) * 100)
        if passpc[-3..-1]=".00" then passpc = passpc[1..-4] end if
        if tests_failed!=0 and passpc="100" then passpc = "99.99" end if
                -- (the above may be needed when tests_run is > 10,000)
        test_log("\n %d tests run, %d passed, %d failed, %s%% success\n", 
                 {tests_run, tests_passed, tests_failed, passpc})
        if pause_summary=TEST_PAUSE
        or (pause_summary=TEST_PAUSE_FAIL and tests_failed>0) then
            if platform()!=JS then
                puts(1,"Press any key to continue...")
                {} = wait_key()
                puts(1,"\n")
            end if
        end if
    end if
    if close_log and log_fn>0 then
        close(log_fn)
        log_fn = 0
    end if
    if tests_failed>0 
    and abort_on_fail=TEST_ABORT then
        abort(1)
    end if
    tests_run = 0
    tests_passed = 0
    tests_failed = 0
    module = 0
    prev_module = 0
end procedure

global procedure set_test_module(string name)
--
--  The module name is simply a hint to the programmer about where to go to 
--  fix some problem just introduced/detected, eg after
--
--      set_test_module("logical")
--      ...
--      set_test_module("relational")
--      ...
--      set_test_module("regression")
--      ...
--      test_summary()
--
--  if you get (say)
--
--      relational:
--        test12 failed: 2 expected, got 4
--       20 tests run, 19 passed, 1 failed, 95% success
--
--  then you know to look for the "test12" test in the relational section.
--  test_summary() is automatically invoked by set_test_module(), and you
--  have to do it right at the end, or risk getting no output whatsoever.
--  Obviously you are free to use any appropriate section names, and if
--  you never invoke set_test_module() they are all lumped together.
--  Likewise it is perfectly fine if you cannot be bothered to name the
--  individual tests, just a bit more of a hunt should they trigger.
--
    if tests_run!=0 then
        test_summary(false)
    end if
    module = name
end procedure

--Instead of this there is now an Alias() in psym.e:
--global procedure set_test_section(string name)
--  set_test_module(name)
--end procedure

procedure test_result(bool success, sequence args, integer fdx, level)
    string fmt = iff(fdx=1?"  failed: %s: %v should %sequal %v\n"
                          :"  failed: %s\n")
    tests_run += 1
    if success then
        if verbosity=TEST_SHOW_ALL and args[1]!="" then
            show_module()
            test_log("  passed: %s\n", args)
        end if
        tests_passed += 1
    else
        if verbosity>=TEST_SHOW_FAILED then
            show_module()
            test_log(fmt, args)
        end if
        if abort_on_fail=TEST_CRASH then
            crash("unit test failure (%s)",args,level)
        end if
        tests_failed += 1
    end if
end procedure

global procedure test_equal(object a, object b, string name="", bool eq=true)

    bool success
    if a=b then
        success = true
    elsif sq_mul(0,a)=sq_mul(0,b) then
        -- for complicated sequences values (same shape)
        if atom(a) then
            success = abs(a-b)<1e-9
        else
            success = or_all(sq_lt(flatten(sq_abs(sq_sub(a,b))),1e-9))
            -- or maybe:
--/*
            success = true
            a = flatten(a)
            b = flatten(b)
            for i=1 to length(a) do
                atom ai = a[i],
                     bi = b[i]
                if ai!=bi
                and not(abs(ai-bi)<1e9) then
                    success = false
                    exit
                end if
            end for
--*/
        end if
    else
        success = false
    end if
    string ne = iff(eq?"":"not ")
    test_result(success=eq,{name,a,ne,b},1,4-eq)

end procedure

global procedure test_not_equal(object a, object b, string name="")
    test_equal(a,b,name,false)
end procedure

global procedure test_true(bool success, string name="")
    test_result(success,{name},2,3)
end procedure

global procedure test_false(bool success, string name="")
    test_result(not success,{name},2,3)
end procedure

global procedure test_pass(string name="")
    test_result(true,{name},2,3)
end procedure

global procedure test_fail(string name="")
    test_result(false,{name},2,3)
end procedure


