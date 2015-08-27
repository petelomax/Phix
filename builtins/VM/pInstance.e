--
--  pInstance.e
--  ===========
--
--  Implements instance()
--
include builtins\VM\pHeap.e -- :%pStoreFlt

#ilASM{ jmp :%opRetf

--/*
procedure :%opInstance(:%)
end procedure -- (for Edita/CtrlQ)
--*/
    :%opInstance
----------------
    [32]
        -- calling convention:
        --  lea edi,[res]
        --  call :%opInstance   -- [edi] := instance()
    [PE32]
        push ebx                                -- lpModuleName (NULL)
        call "kernel32.dll","GetModuleHandleA"
        push ebx
        push eax
        fild qword[esp]
        add esp,8
        jmp :%pStoreFlt
    [ELF32]
        mov eax,ebx
        jmp :%pStoreMint
    [64]
        -- calling convention:
        --  lea rdi,[res]
        --  call :%opInstance   -- [rdi] := instance()
    [PE64]
        mov rcx,rsp -- put 2 copies of rsp onto the stack...
        push rsp
        push rcx
        or rsp,8    -- [rsp] is now 1st or 2nd copy:
                    -- if on entry rsp was xxx8: both copies remain on the stack
                    -- if on entry rsp was xxx0: or rsp,8 effectively pops one of them (+8)
                    -- obviously rsp is now xxx8, whatever alignment we started with
        sub rsp,8*5
        mov rcx,rbx                             -- lpModuleName (NULL)
        call "kernel32.dll","GetModuleHandleA"
--      add rsp,8*5
--      pop rsp     -- restore, equivalent to rsp += (either #08 or #10)
        mov rsp,[rsp+8*5]   -- equivalent to the add/pop
        push rax
        fild qword[esp]
        add esp,8
        jmp :%pStoreFlt
    [ELF64]
        mov rax,rbx
        jmp :%pStoreMint
    []
       }
