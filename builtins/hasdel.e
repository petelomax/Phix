--
-- builtins\hasdel.e
-- =================
--
--  Phix implementation of still_has_delete_routine(). 
--
global function still_has_delete_routine(object x, integer specifically=0)
    if integer(x) then return false end if
    integer rid
    #ilASM{ [32]
                mov edx,[x]
                mov eax,[ebx+edx*4-4] -- (load index & type byte)
                and eax,0x00FFFFFF    -- (keep delete_index only)
                mov [rid],eax
            [64]
                mov rdx,[x]
                mov rax,[rbx+rdx*4-8] -- (load index & type byte)
                shl rax,8
                shr rax,8
                mov [rid],rax
          }
--13/3/24 (ignore the dummy set by pEmit2.e)
--  return rid!=0
    return rid!=0 and rid!=1 and (specifically=0 or specifically=rid)
end function


