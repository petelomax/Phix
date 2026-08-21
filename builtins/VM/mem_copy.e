--
-- builtins\VM\mem_copy.e
--
--without debug -- erm, throw() not permitted

global procedure mem_copy(atom dst, src, integer len)
    if len<0 then
        crash("invalid mem_copy length")
    elsif len>0 then
        try
            #ilASM{
                [32]
                    mov eax,[src]
                    call :%pLoadMint    -- eax:=(int32)eax, edx:=hi_dword
                    mov esi,eax
                    mov eax,[dst]
                    call :%pLoadMint    -- eax:=(int32)eax, edx:=hi_dword
                    mov edi,eax
                    mov ecx,[len]
                [64]
                    mov rax,[src]
                    call :%pLoadMint    -- rax:=(int64)rax
                    mov rsi,rax
                    mov rax,[dst]
                    call :%pLoadMint    -- rax:=(int64)rax
                    mov rdi,rax
                    mov rcx,[len]
                []
                    rep movsb
            }
        catch e
            throw("mem_copy exception (%v)",{e})
        end try
    end if
end procedure

