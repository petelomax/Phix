--
-- pchdir.e
-- ========
--
--  Phix implementation of chdir (auto-include)
--
--  This file is loaded automatically by Phix as and when needed. There is no
--  need to manually include this file (unless you want a namespace on it).
--

function toString(sequence name)--, integer errcode)
-- Explicitly convert a dword-sequence to an 8-bit string
string res
integer nlen
object ch
    nlen = length(name)
    res = repeat(' ',nlen)
    for i=1 to nlen do
        ch = name[i]
        if atom(ch) then
            ch = and_bits(ch,#FF)
            res[i] = ch
        else
--          fatal(errcode)
            ?9/0
        end if
    end for
    return res
end function
--DEV offs! (or just say(doc) that chdir accepts string only!)
if "abc"="def" then
    {} = toString("abc")
end if

--global function chdir(string newdir)
global function chdir(sequence newdir)
-- Changes the current directory. Returns 1 - success, 0 - fail.
--DEV NO!! (must fix this sometime... [any #ilASM storing in a hll var must set the type to object, or a flag on symtab to that effect])
--integer res = 0
integer res
    if not string(newdir) then newdir = toString(newdir) end if
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
--#     Name                        Registers                                                                                                               Definition
--                                  eax     ebx                     ecx                     edx                     esi                     edi
--12    sys_chdir                   0x0c    const char *filename    -                       -                       -                       -               fs/open.c:361
            mov ebx,[newdir]
            mov eax,12          -- sys_chdir
            shl ebx,2
            int 0x80
            xor ebx,ebx
            test eax,eax
            mov eax,ebx
--DEV tryme: (will need pilasm.e mods, is this even the right syntax?)
--          setz al
            jnz @f
                mov eax,1
          @@:
            mov [res],eax
        [ELF64]
--%rax  System call             %rdi                    %rsi                            %rdx                    %rcx                    %r8                     %r9
--80    sys_chdir               const char *filename
            mov rdi,[newdir]
            mov rax,80          -- sys_chdir
            shl rdi,2
            syscall
            test rax,rax
            mov rax,rbx
            jnz @f
                mov rax,1
          @@:
            mov [res],rax
          }
    return res
end function

