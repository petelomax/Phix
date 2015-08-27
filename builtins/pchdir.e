--
-- pchdir.e
-- ========
--
--  Phix implementation of chdir (auto-include)
--
--  This file is loaded automatically by Phix as and when needed. There is no
--  need to manually include this file (unless you want a namespace on it).
--

global function chdir(string newdir)
-- Changes the current directory. Returns 1 - success, 0 - fail.
integer res
    #ilASM{
        [PE32]
            mov eax,[newdir]
            shl eax,2
            push eax                                    -- lpPathName
            call "kernel32.dll","SetCurrentDirectoryA"
            mov [res],eax
        [PE64]
            mov rcx,rsp -- put 2 copies of rsp onto the stack...
            push rsp
            push rcx
            or rsp,8    -- [rsp] is now 1st or 2nd copy:
                        -- if on entry rsp was xxx8: both copies remain on the stack
                        -- if on entry rsp was xxx0: or rsp,8 effectively pops one of them (+8)
                        -- obviously rsp is now xxx8, whatever alignment we started with
            sub rsp,8*5 -- shadow space (min 32 bytes/4 params) and realign stack
            mov rcx,[newdir]
            shl rcx,2
            call "kernel32.dll","SetCurrentDirectoryA"
            mov [res],rax
--          add rsp,8*5
--          pop rsp
            mov rsp,[rsp+8*5]   -- equivalent to the add/pop
        [ELF32]
            pop al  -- DEV not started
--12        sys_chdir                   0x0c    const char *filename    -                       -                       -                       -               fs/open.c:361
        [ELF64]
            pop al  -- ""
--80        sys_chdir               const char *filename
          }
    return res
end function

