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
    enter_cs()
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
    leave_cs()
end procedure

global function getenv(string var)
--
-- Return the specified environment variable setting, eg
-- getenv("PATH") might return `C:\WINDOWS;C:\WINDOWS\COMMAND`
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
        #ilASM{
            [ELF32]
                mov eax,[var]
                shl eax,2
                push eax
                call "libc.so.6","getenv"
                add esp,4
                lea edi,[pRes]
                call :%pStoreMint
            [ELF64]
                mov rdi,[var]
                shl rdi,2
                call "libc.so.6","getenv"
                lea rdi,[pRes]
                call :%pStoreMint
            []
              }
        if pRes!=NULL then
            res = peek_string(pRes)
        end if
    end if
    return res
end function

--DEV doc/psym:
--global function setenv(sequence var, object newValue=0)
global function setenv(string var, object newValue=NULL, integer overwrite=1)
--
-- Set the specified environment variable setting, eg
--      setenv("PATH",`C:\WINDOWS;C:\WINDOWS\COMMAND`)
-- The newValue can be 0(NULL) to delete a variable from the
--  current processes environment, or it can be the address of
--  a previously allocated memory-string.
-- Returns 1(true) on success, 0(false) on failure, or -1 
--  if overwrite=0 and var already exists.
--
integer res
    if atom(newValue) and newValue!=NULL then
        --DEV (spotted in passing) this might come a cropper as in #FFFFFFFF!=-1...
        if newValue!=and_bits(newValue,-1) then ?9/0 end if -- verify mint
        {} = peek(newValue) -- verify it is a readable memory location/ptr
    end if
    if overwrite=0 then
        if getenv(var)!=-1 then return -1 end if
    end if
    if platform()=WINDOWS then
        if eInit=0 then Einit() end if
        res = c_func(xSetEnvironmentVar,{var,newValue})
    elsif platform()=LINUX then
        if newValue=NULL then
            #ilASM{
                [ELF32]
                    mov eax,[var]
                    shl eax,2
                    push eax                    -- name
                    call "libc.so.6","unsetenv"
                    add eax,1 -- (0=success=>1, -1=failure=>0)
                    add esp,4
                    mov [res],eax
                [ELF64]
                    mov rdi,[var]
                    shl rdi,2                   -- name
                    call "libc.so.6","unsetenv"
                    add rax,1 -- (0=success=>1, -1=failure=>0)
                    mov [res],rax
                []
                  }
        elsif string(newValue) then
            #ilASM{
                [ELF32]
                    mov eax,[var]
                    mov esi,[newValue]
                    shl eax,2
                    shl esi,2
                    push 1                      -- overwrite
                    push esi                    -- value
                    push eax                    -- name
                    call "libc.so.6","setenv"
                    add eax,1 -- (0=success=>1, -1=failure=>0)
                    add esp,12
                    mov [res],eax
                [ELF64]
                    mov rdi,[var]
                    mov rsi,[newValue]
                    shl rdi,2                   -- name
                    shl rsi,2                   -- value
                    mov rdx,1                   -- overwrite
                    call "libc.so.6","setenv"
                    add rax,1 -- (0=success=>1, -1=failure=>0)
                    mov [res],rax
                []
                  }
        elsif atom(newValue) then
            #ilASM{
                [ELF32]
                    mov esi,[var]
                    mov eax,[newValue]
                    shl esi,2
                    call :%pLoadMint
                    push 1                      -- overwrite
                    push eax                    -- value
                    push esi                    -- name
                    call "libc.so.6","setenv"
                    add eax,1 -- (0=success=>1, -1=failure=>0)
                    add esp,12
                    mov [res],eax
                [ELF64]
                    mov rax,[newValue]
                    call :%pLoadMint
                    mov rdi,[var]
                    mov rdx,1                   -- overwrite
                    mov rsi,rax                 -- value
                    shl rdi,2                   -- name
                    call "libc.so.6","setenv"
                    add rax,1 -- (0=success=>1, -1=failure=>0)
                    mov [res],rax
                []
                  }
        else
            ?9/0
        end if
    end if
    return res
end function

global function unsetenv(string var)
    return setenv(var)
end function

