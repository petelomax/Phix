--
-- builtins/assert.e (an autoinclude)
--
--  "assert(test,msg)" is not unlike "if not test then crash(msg) end if".
--
global procedure assert(bool condition, string msg="", sequence args={})
    if not condition then
        crash("assertion failure:"&msg,args,2)
--/* (replaced for pwa/p2js)
        if length(args) then msg = sprintf(msg,args) end if
        -- <similar to crash callstack-1 msg:>
        #ilASM{
            [32]
                mov edi,[msg]
                add dword[ebx+edi*4-8],1    -- (incref, just in case)
--              xor esi,esi                 -- ep2 unused
                mov ecx,1
            [64]
                mov rdi,[msg]
                add qword[rbx+rdi*4-16],1   -- (incref, just in case)
--              xor rsi,rsi                 -- ep2 unused
                mov rcx,1
            []
                mov al,119                  -- e119af(edi), "assertion failure %s"
                jmp :!fatalN
                int3 }
--*/
    end if
end procedure

