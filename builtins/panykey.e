--
--  panykey.e
--  =========
--      Phix implementation of any_key() and maybe_any_key()
--      Stolen from OpenEuphoria's std/console.e
--

--**
-- Display a prompt to the user and wait for any key.
--
-- Parameters:
--   # ##prompt## : Prompt to display, defaults to "Press Any Key to continue..."
--   # ##con## : Either 1 (stdout), or 2 (stderr). Defaults to 1.
--
-- Comments:
-- This wraps [[:wait_key]] by giving a clue that the user should press a key, and
-- perhaps do some other things as well.
--
-- Example 1:
-- <eucode>
-- any_key() -- "Press Any Key to continue..."
-- </eucode>
--
-- Example 2:
-- <eucode>
-- any_key("Press Any Key to quit")
-- </eucode>
--
-- See Also:
--      [[:wait_key]]

global procedure any_key(string prompt="Press Any Key to continue...", integer con = 1)
    if find(con,{1,2})=0 then
        con = 1
    end if
    puts(con, prompt)
    {} = wait_key()
    puts(con, "\n")
end procedure

--**
-- Description:
--   Display a prompt to the user and wait for any key **only** if the user is
--   running under a GUI environment.
--   
-- Parameters:
--   # ##prompt## : Prompt to display, defaults to "Press Any Key to continue..."
--   # ##con## : Either 1 (stdout), or 2 (stderr). Defaults to 1.
--
-- Comments:
-- This wraps [[:wait_key]] by giving a clue that the user should press a key, and
-- perhaps do some other things as well.
-- Requires Windows XP or later or Windows 2003 or later to work.  Earlier versions of Windows  --DEV
-- or O/S will always pause even when not needed.
-- On the other hand, on Unix like Operating Systems this wont pause even when needed.
--
-- Example 1:
-- <eucode>
-- any_key() -- "Press Any Key to continue..."
-- </eucode>
--
-- Example 2:
-- <eucode>
-- any_key("Press Any Key to quit")
-- </eucode>
--
-- See Also:
--      [[:wait_key]]

constant STD_OUTPUT_HANDLE = -11

global procedure maybe_any_key(string prompt="Press Any Key to continue...", integer con = 1)
    #ilASM{
        [PE32]
            push STD_OUTPUT_HANDLE              -- nStdHandle
            call "kernel32.dll","GetStdHandle"
            add eax,1
            cmp eax,1
            jbe :no_console                     -- NULL or INVALID_HANDLE_VALUE (-1)
        [PE64]
--DEV may need the standard align trick...
            sub rsp,8*4
            mov rcx,STD_OUTPUT_HANDLE           -- nStdHandle
            call "kernel32.dll","GetStdHandle"
            add rax,1
            add rsp,8*4
            cmp rax,1
            jbe :no_console                     -- NULL or INVALID_HANDLE_VALUE (-1)
        [ELF32]
        [ELF64]
        []
          }
    any_key(prompt, con)
    #ilASM{
        [PE32,PE64]
        ::no_console
        [ELF32]
        [ELF64]
        []
          }
end procedure

