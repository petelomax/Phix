--
-- pbreak.e
--
-- Phix implementation of allow_break() and check_break().
--DEV I really might need to recode this in asm to switch it off for debug/trace/error...
-- I may also want to force a console to exist...
--
without trace
--with trace

integer binit binit = 0
integer AllowBreak
integer cc_count

constant CTRL_C_EVENT       = 0,
         CTRL_BREAK_EVENT   = 1

--integer maxonce
--      maxonce = 1
--puts(1,"ooh\n")
function HandlerRoutine(integer event)
--if maxonce then
--  maxonce = 0
--  puts(1,"ooh\n")
--  ?9/0
--end if
    if event=CTRL_C_EVENT
    or event=CTRL_BREAK_EVENT then
        if not AllowBreak then
            cc_count += 1
            return 1        -- handled
        end if
    end if
    return 0                -- not handled
end function

procedure initB()
atom kernel32, xSetConsoleCtrlHandler

    kernel32 = open_dll("kernel32.dll")     -- DEV do something about this?...

--#without reformat

    xSetConsoleCtrlHandler = define_c_func(kernel32,"SetConsoleCtrlHandler",
        {C_POINTER, --  PHANDLER_ROUTINE  pHandlerRoutine,  // address of handler function
         C_INT},    --  BOOL  fAdd  // handler to add or remove
        C_INT)      -- BOOL

--#with reformat

    if not c_func(xSetConsoleCtrlHandler,{call_back(routine_id("HandlerRoutine")),1}) then ?9/0 end if

    AllowBreak = 0

    cc_count = 0

    binit = 1

end procedure

type boolean(integer b)
    return b=0 or b=1
end type

global procedure allow_break(boolean b)
-- If b is TRUE then allow control-c/control-break to terminate the program. 
-- If b is FALSE then don't allow it.
-- Initially they *will* terminate the program, but only when it
-- tries to read input from the keyboard.
    if not binit then initB() end if
    AllowBreak = b
end procedure

global function check_break()
-- returns the number of times that control-c or control-break
-- were pressed since the last time check_break() was called
integer r
    if not binit then initB() end if
    r = cc_count
    cc_count = 0
    return r
end function


