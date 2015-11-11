DEAD
--
-- builtins\pfloat.e
-- =================
--  Implements atom_to_float80() and float80_to_atom(). 32 and 64 bit versions to follow.
--
global function atom_to_float80(atom a)
string res = repeat(' ',10)
    #ilASM{
        [32]
            mov esi,[a]
            mov edi,[res]
            fld qword[ebx+esi*4]
            fstp tbyte[ebx+edi*4]
        [64]
            mov rsi,[a]
            mov rdi,[res]
            fld tbyte[rbx+rsi*4]
            fstp tbyte[rbx+rdi*4]
        []
          }
    return res
end function

global function float80_to_atom(sequence s)
atom m10 = allocate(10) -- workspace --DEV use integer and /4 trick?
atom res = m10+0.1 -- (create a random value/space for result) [DEV or use %pStoreFlt?]
    if length(s)!=10 then ?9/0 end if
    poke(m10,s)
    #ilASM{
        [32]
            mov esi,[m10]
            mov edi,[res]
            fld tbyte[esi]
            fstp qword[ebx+edi*4]
        [64]
            mov rsi,[m10]
            mov rdi,[res]
            fld tbyte[rsi]
            fstp tbyte[rbx+rdi*4]
           }
    free(m10)
    return res
end function

