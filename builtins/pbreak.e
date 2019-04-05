--
-- pbreak.e
--
-- Phix implementation of allow_break() and check_break().
--
--without debug
without trace

integer binit = 0
integer AllowBreak = true
--atom cb_handler = NULL    -- (failed attempt)
integer cc_count = 0

constant CTRL_C_EVENT       = 0,
         CTRL_BREAK_EVENT   = 1

constant SIGINT = 2

#ilASM{ jmp :!opCallOnceYeNot

  :%initB
    [PE32]
        push 1                      -- BOOL  fAdd
        push :my_signal_handler     -- address of handler function
        call "kernel32.dll","SetConsoleCtrlHandler"
        ret
    [PE64]
        mov rcx,rsp -- put 2 copies of rsp onto the stack...
        push rsp
        push rcx
        or rsp,8
        sub rsp,8*5
        mov rdx,1                       -- BOOL  fAdd
        mov rcx,:my_signal_handler      -- address of handler function
        call "kernel32.dll","SetConsoleCtrlHandler"
        mov rsp,[rsp+8*5]
        ret
    [ELF32]
--#     Name                        Registers                                                                                                               Definition
--                                  eax     ebx                     ecx                     edx                     esi                     edi
--67    sys_sigaction               0x43    int sig                 const struct old_sigaction *act struct old_sigaction *oact  -           -               arch/mips/kernel/signal.c:300
        push 0
        push 4  -- SA_SIGINFO
        push 0
        push :my_signal_handler
        mov eax, 67 -- SYSCALL_SIGACTION (67==#43)
        mov ebx, SIGINT
        mov ecx,esp
        xor edx, edx 
        int 0x80                -- sigaction(SIGINT, &sigact, 0)
        add esp,16
        xor ebx,ebx             -- (common requirement after int 0x80)
        ret
    [ELF64]
--%rax  System call             %rdi                    %rsi                            %rdx                    %rcx                    %r8                     %r9
--13        sys_rt_sigaction        int sig                 const struct sigaction *act     struct sigaction *oact  size_t sigsetsize
        -- (may yet need a "push 0x04000004 -- SA_SIGINFO or SA_RESTORER"?, see example64.asm)
        push rbx
        push rbx
        push rbx
        push rbx
        push rbx
        push rbx
        push rbx
        push rbx

        push :restorer
        push 0x04000004    -- SA_SIGINFO or SA_RESTORER
        push :my_signal_handler
        mov r10,8
        mov rcx,r10
        xor rdx,rdx
        mov rsi,rsp
        mov rdi,SIGINT
        mov rax,13  -- sys_rt_sigaction
        syscall
        add rsp,88
        ret

      ::restorer
        mov rax,15 -- NR_rt_sigreturn
        syscall
        int3

    []

      ::my_signal_handler

    [PE32]
        xor ebx,ebx                 -- important!
--      mov eax,[cb_handler]
--      cmp eax,ebx
--      jz @f
--          jmp eax
--    @@:
        mov eax,[esp+4]
        push ecx
        push edx
        cmp eax,CTRL_C_EVENT
        je @f
        cmp eax,CTRL_BREAK_EVENT
        jne :retz
          @@:
            cmp [AllowBreak],ebx
            jne :retz
            mov eax,1   -- handled
            add [cc_count],1
            pop edx
            pop ecx
            ret 4
  ::retz
        pop edx
        pop ecx
        xor eax,eax     -- not handled
        ret 4
    [PE64]
        xor rbx,rbx                 -- important!
--      mov rax,[cb_handler]
--      cmp rax,rbx
--      jz @f
--          jmp rax
--    @@:
        cmp rcx,CTRL_C_EVENT
        je @f
        cmp rcx,CTRL_BREAK_EVENT
        jne retz
          @@:
            cmp [AllowBreak],rbx
            jne :retz
            mov rax,1   -- handled
            add [cc_count],rax
            ret
  ::retz
        xor rax,rax     -- not handled
        ret
    [ELF32]
        xor ebx,ebx                 -- important!
--      mov eax,[cb_handler]
--      cmp eax,ebx
--      jz @f
--          jmp eax
--    @@:
        add [cc_count],1
        ret
    [ELF64]
        xor rbx,rbx                 -- important!
--      mov rax,[cb_handler]
--      cmp rax,rbx
--      jz @f
--          jmp rax
--    @@:
        add [cc_count],1
        ret
      }

global procedure allow_break(bool bAllow)
--global procedure allow_break(bool bAllow, atom handler=NULL)
-- Determine whether control-c/control-break terminate the program. 
-- If bAllow is TRUE then allow that, else/FALSE, don't.
-- Initially those keystrokes *will* terminate the program, but
--  only when it actually tries to read input from a console.
    if not binit then
        #ilASM{call :%initB}
    end if
    AllowBreak = bAllow
--  cb_handler = handler
end procedure

global function check_break()
-- returns the number of times that control-c or control-break
-- were pressed since the last time check_break() was called
--integer res = cc_count    -- grr...
integer res
    #ilASM{
        [32]
            mov eax,[cc_count]
            mov [res],eax
        [64]
            mov rax,[cc_count]
            mov [res],rax
          }
    cc_count = 0
    return res
end function

