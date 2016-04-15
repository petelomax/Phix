--
-- penv.e
-- ======
--
-- The Phix implementation of getenv and setenv.
--
-- This file is fully compatible with RDS Eu, though you will probably get
--  a warning re getenv being redefined.
--
-- Aside: was originally coded in assembler; hll seems more sensible.

--!/**/without debug

--include builtins\pcfunc2.e as pcfunc

integer eInit eInit = 0
atom kernel32
atom xGetEnvironmentVar,
     xSetEnvironmentVar

procedure Einit()
-- (platform()=WINDOWS only)
--DEV locking as per pprntf
    kernel32 = open_dll("kernel32.dll")
--#without reformat
    xGetEnvironmentVar = define_c_func(kernel32,"GetEnvironmentVariableA",
        {C_POINTER, --  LPCTSTR  lpName,    // address of environment variable name
         C_POINTER, --  LPTSTR  lpBuffer,   // address of buffer for variable value
         C_INT},    --  DWORD  nSize    // size of buffer, in characters
        C_INT)      -- DWORD number of chars stored in lpBuffer
    xSetEnvironmentVar = define_c_func(kernel32,"SetEnvironmentVariableA",
        {C_POINTER, --  LPCTSTR  lpszName,  // address of environment variable name
         C_POINTER}, -- LPCTSTR  lpszValue  // address of new value for variable
        C_INT)      -- BOOL 
--#with reformat
    eInit = 1
end procedure

global function getenv(sequence var)
--
-- Return the specified environment variable setting, eg
-- getenv("PATH") might return "C:\WINDOWS;C:\WINDOWS\COMMAND"
-- Returns -1 if the variable is undefined
--
atom pRes
integer len
object res = -1

    if platform()=WINDOWS then
        if eInit=0 then Einit() end if

        -- first get the required size:
        len = c_func(xGetEnvironmentVar,{var,0,0})
        if len!=0 then
            pRes = allocate(len)
            len = c_func(xGetEnvironmentVar,{var,pRes,len})
            res = peek({pRes,len})
            free(pRes)
        end if
    elsif platform()=LINUX then
--      ?9/0
        -- there's a getenv in libc, I think...
        --DEV (temp, for return type)
        if var="abc" then
            res = {}
        end if
--      lea     rdi,[display_name]
--      call    getenv
--      lea     r11,[msg00]
--      or      rax,rax
--      jz      error_exit
    end if
    return res
end function

global function setenv(sequence var, object newValue=0)
--
-- Set the specified environment variable setting, eg
--      setenv("PATH","C:\WINDOWS;C:\WINDOWS\COMMAND")
-- The newValue can be 0(NULL) to delete a variable from the
--  current processes environment, or it can be the address of
--  a previously allocated memory-string.
-- Returns 1(true) on success, 0(false) on failure.
--
integer res
    if platform()=WINDOWS then
        if eInit=0 then Einit() end if
        res = c_func(xSetEnvironmentVar,{var,newValue})
    elsif platform()=LINUX then
        ?9/0
    end if
    return res
end function

