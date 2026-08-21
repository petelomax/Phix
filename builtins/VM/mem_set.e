--
-- builtins\VM\mem_set.e
--
--without debug -- erm, throw() not permitted

global procedure mem_set(atom dst, integer byte, len)
    if len<0 then
        crash("invalid mem_set length")
    elsif len>0 then
        try
            #ilASM{
                [32]
                    mov eax,[dst]
                    call :%pLoadMint    -- eax:=(int32)eax, edx:=hi_dword
                    mov edi,eax
                    mov eax,[byte]
                    mov ecx,[len]
                [64]
                    mov rax,[dst]
                    call :%pLoadMint    -- rax:=(int64)rax
                    mov rdi,rax
                    mov rax,[byte]
                    mov rcx,[len]
                []
                    rep stosb
            }
        catch e
            throw("mem_set exception (%v)",{e})
        end try
    end if
end procedure

