--
-- builtins\infnan.e (an autoinclude)
-- =================
--
-- 64 bit IEEE-754 layout:
-- sign|   mantissa  |   significand   |  (whole thing in hex)  ---- atom_to_float64: ----  ---- meaning ----
--    1|111 1111 1111|0000<52 bits>0000|  #FFF0 0000 0000 0000  x"00 00 00 00 00 00 F0 FF"  negative infinity
--    0|111 1111 1111|0000<aka 6.5>0000|  #7FF0 0000 0000 0000  x"00 00 00 00 00 00 F0 7F"  positive infinity
--    ?|111 1111 1111|1xxx< bytes >xxxx|  #7FF8 xxxx xxxx xxxx  x"00 00 00 00 00 00 F8 7F"  NaN (or #FFF8...)
--
-- 80 bit IEEE-754 layout:
-- sign|     mantissa     |     fraction    |  -- (whole thing in hex) -   atom_to_float80  ---- meaning ----
--    1|111 1111 1111 1111|0000<64 bits>0000|  #FFFF 0000 0000 0000 0000  x"00*7 80 FF FF"  negative infinity
--    0|111 1111 1111 1111|0000< aka 8 >0000|  #7FF0 0000 0000 0000 0000  x"00*7 80 FF 7F"  positive infinity
--    0|111 1111 1111 1111|xxxx< bytes >xxx1|  #7FF0 xxxx xxxx xxxx xxx1  x"00*7 C0 FF 7F"  NaN (or #FFFFC..)
--
-- Note: encodings of NaN are not completely specified in IEEE 754.
--       some sources say inf is [only] /all/ zero fraction, and 
--       /any/ bit set makes it a nan of some flavour, but this relies
--       solely on t'most signicant bit of fraction (aka significand)
--       no attempt is made to distinguish twixt quiet/signalling nans.
--       there is no handling/testing of denormalised numbers either.
--
-- Should you want to try this in #ilASM{}, one thing I did find:
--  https://board.flatassembler.net/topic.php?t=16336
--  (bit of confirmation) https://board.flatassembler.net/topic.php?t=6765
--  https://euroassembler.eu/eadoc/index.htm
--
global function is_nan(object a, bool b64=false)
    -- returns true if a is [+/-]nan
    integer res
    #ilASM{
        [32]
            mov esi,[a]
            mov eax,ebx
            cmp esi,h4
            jl @f
                cmp byte[ebx+esi*4-1],0x12
                jne @f
                    mov cx,[ebx+esi*4+6]
                    and cx,0x7FF8
                    cmp cx,0x7FF8
                    sete al
          @@:
            mov [res],eax
        [64]
            mov rsi,[a]
            mov r15,h4
            mov rax,rbx
            cmp rsi,r15
            jl @f
                cmp byte[rbx+rsi*4-1],0x12
                jne @f
                cmp byte[b64],1
                je :b64way
                    mov ecx,[rbx+rsi*4+6]
                    and ecx,0x7FFFF000
                    cmp ecx,0x7FFFC000
                    sete al
                    jmp @f
              ::b64way
                    sub rsp,8
                    fld tbyte[rbx+rsi*4]
                    fstp qword[rsp]
                    mov cx,[rsp+6]
                    add rsp,8
                    and cx,0x7FF8
                    cmp cx,0x7FF8
                    sete al
          @@:
            mov [res],rax
        []
         }
    return res
--/* -- hll code the above is based on:
    if not atom(a) or integer(a) then return false end if
    if machine_bits()=32 or b64 then
        sequence f8 = atom_to_float64(a)
        return and_bits(f8[8],#7F)=#7F
           and and_bits(f8[7],#F8)=#F8
    else
        sequence f10 = atom_to_float80(a)
        return and_bits(f10[10],#7F)=#7F and f10[9]=#FF
--         and and_bits(f10[8],#C0)=#C0
           and and_bits(f10[8],#FF)!=#80
    end if
--*/
--  return a!=a -- (ah, wouldn't support b64 anyway...)
end function

global function is_inf(object a, integer sgn=0, bool b64=false)
    -- returns true if a is: sgn -1:   -inf
    --                            0: +/-inf
    --                           +1:   +inf
    if sgn=-1 and a>=0 then return false
    elsif sgn=1 and a<0 then return false end if
    integer res
    #ilASM{
        [32]
            mov esi,[a]
            mov eax,ebx
            cmp esi,h4
            jl @f
                cmp byte[ebx+esi*4-1],0x12
                jne @f
                    mov cx,[ebx+esi*4+6]
                    and cx,0x7FFF
                    cmp cx,0x7FF0
                    sete al
          @@:
            mov [res],eax
        [64]
            mov rsi,[a]
            mov r15,h4
            mov rax,rbx
            cmp rsi,r15
            jl @f
                cmp byte[rbx+rsi*4-1],0x12
                jne @f
                cmp byte[b64],1
                je :b64way
                    mov ecx,[rbx+rsi*4+6]
                    and ecx,0x7FFFFF00
                    cmp ecx,0x7FFF8000
                    sete al
                    jmp @f
              ::b64way
                    sub rsp,8
                    fld tbyte[rbx+rsi*4]
                    fstp qword[rsp]
                    mov cx,[rsp+6]
                    add rsp,8
                    and cx,0x7FFF
                    cmp cx,0x7FF0
                    sete al
          @@:
            mov [res],rax
        []
         }
    return res
--/* -- hll code the above is based on:
    if not atom(a) or integer(a) then return false end if
    if machine_bits()=32 or b64 then
        sequence f8 = atom_to_float64(a)
        sgn = iff(sgn=0?true:iff(sgn=-1?and_bits(f8[8],#80)!=0: 
                                        and_bits(f8[8],#80)==0))
        return sgn and and_bits(f8[8],#7F)=#7F
                   and f8[7]=#F0
--                 and f8[1..6] = {0,0,0,0,0,0}
    else
        sequence f10 = atom_to_float80(a)
        sgn = iff(sgn=0?true:iff(sgn=-1?and_bits(f10[10],#80)!=0: 
                                        and_bits(f10[10],#80)==0))
        return sgn and and_bits(f10[10],#7F)=#7F
                   and f10[9]=#FF and f10[8]=#80
--                 and f10[1..7] = {0,0,0,0,0,0,0}
    end if
--*/
end function

