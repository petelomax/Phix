--
-- builtins/unit_test.e
--
--  A simple unit testing framework for Phix (autoinclude).
--
--      --set_test_verbosity(TEST_QUIET) -- (the default)
--      test_not_equal(2+2,5,"2+2 is 4 !!!!")
--      test_summary()
--
--  If all goes well, no output is shown, and the program carries on normally.
--  You can easily force [summary] output, crash/prompt on fail, etc. [See docs]

--  Note that I have used the same routine names as Euphoria, but the parameters are
--       all different [esp their order] and therefore they are not compatibile...
--       In particular you have to give every test a name in Euphoria, whereas here
--       such things are optional. Also, Euphoria works by putting tests in files
--       named "t_*" and running eutest, whereas here they are part of the app, and
--       will start failing on live systems (eg) if not properly installed, which I
--       (very strongly) think is actually far superior...
--       [If you want unit tests to "go away" in production releases, you just need
--        eg "global constant UNIT_TESTS = false" and litter "if UNIT_TESTS then"
--        throughout your code, and maybe something along similar lines to the way
--        that docs/phix/makephix.exw/readtoc() verifies that pglobals.e matches 
--        banner.htm, but in your standard build script (assuming you have one).  ]
--
-- now in psym.e:
global enum
    TEST_QUIET              = 0,    -- (summary only when fail)
    TEST_SUMMARY            = 1,    -- (summary only [/always])
    TEST_SHOW_FAILED_ONLY   = 2,    -- (summary + failed tests)
    TEST_SHOW_ALL           = 3     -- (summary + all tests)
--
integer tests_run    = 0,
        tests_passed = 0,
        tests_failed = 0,
        verbosity = TEST_QUIET,
        wait_on_summary = -1,
        abort_on_fail = 0,
        log_fn = 0

-- (aside: 0 rather than "" avoids need for a test_init(), when autoincluded:)
object module = 0,          -- (set to string by set_test_module)
       prev_module = 0      -- (    ""           show_module)

procedure test_log(string fmt, sequence args={})
    integer fn = 2
    for i=1 to 1+(log_fn!=0) do
        printf(fn, fmt, args)
        fn = log_fn
    end for
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

--global function set_test_abort(integer abort_test)
global procedure set_test_abort(integer abort_test)
--  integer prev = abort_on_fail
-- 1 = abort, 0 = carry on, -1 = crash on fail (default)
    abort_on_fail = abort_test
--  return prev
--end function
end procedure

global procedure set_wait_on_summary(integer to_wait)
-- 1 always, 0 never, -1 on fail (default)
    wait_on_summary = to_wait
end procedure

global procedure set_test_logfile(string filename)
-- (closed by test_summary())
    if log_fn!=0 then ?9/0 end if
    log_fn = open(filename,"w")
    if log_fn=-1 then ?9/0 end if
end procedure

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
        if wait_on_summary=1
        or (wait_on_summary=-1 and tests_failed>0) then
            puts(1,"Press any key to continue...")
            {} = wait_key()
        end if
    end if
    if close_log and log_fn>0 then
        close(log_fn)
        log_fn = 0
    end if
    if tests_failed>0 
    and abort_on_fail=1 then
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
--      --test_summary(false) -- (optional/automatic)
--      set_test_module("relational")
--      ...
--      --test_summary(false) -- (optional/automatic)
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
--  test_summary() is automatically invoked by set_test_module(), however
--  in some cases it may make more sense to explicitly invoke it, and you
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

global procedure test_equal(object a, object b, string name="", bool eq=true)
    tests_run += 1

    integer success
    if a=b then
        success = 1     
    elsif sq_mul(0,a)=sq_mul(0,b) then
        -- for complicated sequences values (same shape)
        if atom(a) then
            success = abs(a-b)<1e-9
        else
            success = max(sq_lt(sq_abs(sq_sub(a,b)),1e-9))
        end if
    else
        success = 0
    end if

    if success=eq then
        if verbosity=TEST_SHOW_ALL and name!="" then
            show_module()
            test_log("  passed: %s\n", {name})
        end if
        tests_passed += 1
    else
        if verbosity>=TEST_SHOW_FAILED_ONLY then
            show_module()
            string fmt = iff(eq?"  failed: %s: %v expected, got %v\n"
                               :"  failed: %s: %v should not equal %v\n")
            test_log(fmt, {name,a,b})
        end if
        if abort_on_fail=-1 then crash("unit test failure") end if
        tests_failed += 1
    end if
end procedure

global procedure test_not_equal(object a, object b, string name="", bool eq=false)
    test_equal(a,b,name,eq)
end procedure

global procedure test_true(bool success, string name="")
    tests_run += 1
    if success then
        if verbosity=TEST_SHOW_ALL and name!="" then
            show_module()
            test_log("  passed: %s\n", {name})
        end if
        tests_passed += 1
    else
        if verbosity>=TEST_SHOW_FAILED_ONLY then
            show_module()
            test_log("  failed: %s\n", {name})
        end if
        if abort_on_fail=-1 then crash("unit test failure") end if
        tests_failed += 1
    end if
end procedure

global procedure test_false(bool success, string name="")
    test_true(not success,name)
end procedure

global procedure test_pass(string name)
    test_true(true,name)
end procedure

global procedure test_fail(string name="")
    test_true(false,name)
end procedure


