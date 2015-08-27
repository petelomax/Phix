--
--  pFloatN.e
--  =========
--
--  implements: atom_to_float32/64/80() and float32/64/80_to_atom() 
--
--  Note that loss of precision will occur using float80_to_atom() on 32-bit
--   (DEV I should really issue a warning for that at compile-time)
--

procedure loadFloat(atom a)
-- st0:=[a], where a can be an integer or a floating point value
-- (for an alternative method, see pTrig.e/LoadFlt)
    #ilASM{
        [32]
            mov eax,[a]
            cmp eax,h4
            jl @f
                fld qword[ebx+eax*4]
                jmp :%opRetf
          @@:
            fild dword[a]
--          jmp :%opRetf
        [64]
            mov rax,[a]
            mov r15,h4
            cmp rax,r15
            jl @f
                fld tbyte[rbx+rax*4]
                jmp :%opRetf
          @@:
            fild qword[a]
--          jmp :%opRetf
        []
         }
end procedure

global function atom_to_float32(atom a)
string res = repeat(' ',4)
    loadFloat(a)
    #ilASM{
        [32]
            mov edi,[res]
            fstp dword[ebx+edi*4]
        [64]
            mov rdi,[res]
            fstp dword[rbx+rdi*4]
        []
          }
    return res
end function

global function atom_to_float64(atom a)
string res = repeat(' ',8)
    loadFloat(a)
    #ilASM{
        [32]
            mov edi,[res]
            fstp qword[ebx+edi*4]
        [64]
            mov rdi,[res]
            fstp qword[rbx+rdi*4]
        []
          }
    return res
end function

global function atom_to_float80(atom a)
string res = repeat(' ',10)
    loadFloat(a)
    #ilASM{
        [32]
            mov edi,[res]
            fstp tbyte[ebx+edi*4]
        [64]
            mov rdi,[res]
            fstp tbyte[rbx+rdi*4]
        []
          }
    return res
end function

function toBinaryString(sequence s)
-- internal helper function, converts a dword-sequence into a byte-string
--  containing the raw binary of a 32/64/80-bit floating point value.
string res = repeat(' ',length(s))
    for i=1 to length(s) do
        res[i] = and_bits(s[i],#FF)
    end for
    return res
end function
--DEV (xType=0 thing)
{} = toBinaryString("")

global function float32_to_atom(sequence s)
atom res
    if length(s)!=4 then ?9/0 end if --DEV crash()? (and two more below)
    if not string(s) then s = toBinaryString(s) end if
    #ilASM{
        [32]
            mov esi,[s]
            lea edi,[res]
            fld dword[ebx+esi*4]
            call :%pStoreFlt
        [64]
            mov rsi,[s]
            lea rdi,[res]
            fld dword[rbx+rsi*4]
            call :%pStoreFlt
           }
    return res
end function

global function float64_to_atom(sequence s)
atom res
    if length(s)!=8 then ?9/0 end if
    if not string(s) then s = toBinaryString(s) end if
    #ilASM{
        [32]
            mov esi,[s]
            lea edi,[res]
            fld qword[ebx+esi*4]
            call :%pStoreFlt
        [64]
            mov rsi,[s]
            lea rdi,[res]
            fld qword[rbx+rsi*4]
            call :%pStoreFlt
           }
    return res
end function

global function float80_to_atom(sequence s)
atom res
    if length(s)!=10 then ?9/0 end if
    if not string(s) then s = toBinaryString(s) end if
    #ilASM{
        [32]
            mov esi,[s]
            lea edi,[res]
            fld tbyte[ebx+esi*4]
            call :%pStoreFlt
        [64]
            mov rsi,[s]
            lea rdi,[res]
            fld tbyte[rbx+rsi*4]
            call :%pStoreFlt
           }
    return res
end function

