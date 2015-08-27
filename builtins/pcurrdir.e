--
-- pcurrdir.e
-- ==========
--
--  Implements current_dir(). This is automatically included when needed; 
--  there should be no need to manually include this file.
--
--  This was originally coded in asm, however on Windows XP (at least), if
--  you are in C:\Program Files\Phix and run command.com, it twitches the
--  directory to C:\PROGRA~1\Phix (as shown in the command prompt window,
--  nothing whatsoever to do with Phix/c_func, but pure Windows XP stuff).
--  Even typing exit, to quit command.com and return to cmd.exe, leaves it 
--  "twitched". I know you really should not use command.com on XP, but 
--  stuff happens. Anyway, the upshot is that calls to current_dir() should 
--  never return the latter (C:\PROGRA~1\Phix), therefore we ought to use 
--  get_proper_path internally, and hence this was recoded in hll.
--
--/*
NB: this is Phix only; it will not work on and may even crash RDS Eu (an 8-bit string thing)
--*/
--without debug
include builtins\pgetpath.e -- not strictly necessary, but why not.
----/**/include builtins\pcfunc.e -- not strictly necessary, but why not.
--include builtins\VM\pgetpathN.e -- not strictly necessary, but why not. [DEAD]
--include builtins\VM\pcfuncN.e -- not strictly necessary, but why not.

integer init = 0
atom kernel32, xGetCurrentDirectory

global function current_dir()
integer l
--string res
sequence res    --DEV
    if not init then
--DEV requires locking as per pprintf.e?
        kernel32 = open_dll("kernel32.dll")
--#without reformat
        xGetCurrentDirectory = define_c_func(kernel32, "GetCurrentDirectoryA",
            {C_INT,  -- DWORD nBufferLength // size, in characters, of directory buffer 
             C_PTR}, -- LPTSTR lpBuffer     // address of buffer for current directory
            C_INT)   -- DWORD
--#with reformat
        init = 1
    end if
    l = c_func(xGetCurrentDirectory,{0,NULL})
    if l=0 then return "" end if
    res = repeat(' ',l-1)
    l = c_func(xGetCurrentDirectory,{l,res})
    res = get_proper_path(res)
    return res
end function


