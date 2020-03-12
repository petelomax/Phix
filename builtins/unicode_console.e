--
-- builtins/unicode_console.e
--
--  Implements unicode_console()    -- initialises one, and returns true if unicode supported
--
--  While there is some appeal to performing (the equivalent inline assembly of) this automatically in the 
--  back-end/run-time, it would probably break too much legacy code that assumes Windows-1252 or ISO-8859.
--
--  Note this file is not an auto-include, and not documented.
--  Also note that, especially on windows, a unicode-aware font must also be installed/selected, which this
--  routine makes no attempt whatsoever to do. While typically fonts are about 1.5MB, a full unicode font 
--  is likely to be 20 or even 50MB. My searches failed to find one I was impressed with (on windows).
--
include builtins\cffi.e
constant tGSH = """
HANDLE WINAPI GetStdHandle(
  _In_  DWORD nStdHandle
);
""",
tSCOCP = """
BOOL WINAPI SetConsoleOutputCP(
  _In_  UINT wCodePageID
);
""",		   
STD_OUTPUT_HANDLE = -11,
CP_UTF8 = 65001,
envset = {"LANG","LC_ALL","LC_CTYPE"}

atom k32 = NULL, xGetStdHandle, hConsole, xSetConsoleOutputCP

global function unicode_console()
-- initialises the windows console for unicode, and
-- returns true if unicode is supported, else false.
bool res = false
    if platform()=WINDOWS then
        if k32=NULL then
            puts(1,"")  -- force console to exist
            k32 = open_dll("kernel32.dll")
            xGetStdHandle = define_cffi_func(k32,tGSH)
            hConsole = c_func(xGetStdHandle,{STD_OUTPUT_HANDLE})
            xSetConsoleOutputCP = define_cffi_func(k32,tSCOCP)
        end if
        -- following is equivalent to running "chcp 65001":
        res = c_func(xSetConsoleOutputCP,{CP_UTF8})
    else    -- LINUX
        for i=1 to length(envset) do
            if match("UTF",upper(getenv(envset[i])))!=0 then
                res = true
                exit
            end if
        end for
    end if
    return res
end function

