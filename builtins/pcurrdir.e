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
--include builtins\VM\pcfunc.e -- not strictly necessary, but why not.

integer init = 0
atom kernel32, xGetCurrentDirectory

global function current_dir()
integer l
string res
    if not init then
        if platform()=WINDOWS then
            -- added 25/11/16:
            enter_cs()
--#without reformat
            kernel32 = open_dll("kernel32.dll")
            xGetCurrentDirectory = define_c_func(kernel32, "GetCurrentDirectoryA",
                {C_INT,  -- DWORD nBufferLength // size, in characters, of directory buffer 
                 C_PTR}, -- LPTSTR lpBuffer     // address of buffer for current directory
                C_INT)   -- DWORD
--#with reformat
            leave_cs()
        end if
        init = 1
    end if
    if platform()=WINDOWS then
        l = c_func(xGetCurrentDirectory,{0,NULL})
        if l=0 then return "" end if
        res = repeat(' ',l-1)
        l = c_func(xGetCurrentDirectory,{l,res})
        res = get_proper_path(res)
    elsif platform()=LINUX then
        res = repeat(' ',512)   -- (or start with 32, double till it fits, and "find_from" the \0)
        #ilASM {
            [ELF32]
--#     Name                        Registers                                                                                                               Definition
--                                  eax     ebx                     ecx                     edx                     esi                     edi
--183   sys_getcwd                  0xb7    char *buf               unsigned long size      -                       -                       -                       fs/dcache.c:2104
                mov ebx,[res]
                mov eax,183     -- sys_getcwd
                shl ebx,2
                mov ecx,512     -- size
                int 0x80
                xor ebx,ebx
                mov [l],eax
            [ELF64]
--%rax  System call             %rdi                    %rsi                            %rdx                    %rcx                    %r8                     %r9
--79    sys_getcwd              char *buf               unsigned long size
                mov rdi,[res]
                mov rax,79      -- sys_getcwd
                shl rdi,2       -- buf
                mov rsi,512     -- size
                syscall
                mov [l],rax
            []
               }
--not as documented (lscr)
--      if l!=0 then ?9/0 end if
        if l<0 then ?9/0 end if
        res = res[1..find('\0',res)-1]
    else
        ?9/0
    end if
    return res
end function


