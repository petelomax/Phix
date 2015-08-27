
-- low level functions

global function null_bits(integer n, integer nullbit)
    n = or_bits(n, nullbit)
    n = xor_bits(n, nullbit)
    return n
end function

global function or_all(object s)
-- or together all elements of a sequence
atom result
    if atom(s) then
        return s
    end if
    result = 0
    for i = 1 to length(s) do
        result = or_bits(result, s[i])
    end for
    return result
end function


global function aggregate(integer n, integer d)
-- fill out the integer to the nearest upper multiple of d
-- assumes 1-based convention and the numbers must be +ve
    n -= 1
    n += d - remainder(n, d)
    return n    
end function


-----------------------------------



global function hiWord(atom dWord)
    return floor(dWord/#10000)
end function

global function loWord(atom dWord)
    return and_bits(dWord, #FFFF)
end function


--DEV Make me a builtin * 3:
--/* Not Phix (defined in autoinclude builtins\ppoke2.e)
global procedure poke2(atom addr, object x)
    if integer(x) then
        if x < 0 then
            x += 65536
        end if  
        poke(addr, x) -- note: only the lower 8 bits are stored
        poke(addr+1, floor(x/256))
    else -- sequence
        for i = 1 to length(x) do
?9/0 -- wrong:
            poke2(addr+i-1, x[i])
        end for
    end if
end procedure


global function peek2u(object atomorsqlen2)
atom addr
integer word, len
sequence sres
    if atom(atomorsqlen2) then
        addr = atomorsqlen2
        word = peek(addr) + peek(addr+1) * 256
        return word
    else
        if length(atomorsqlen2)!=2 then ?9/0 end if
        addr = atomorsqlen2[1]
        len = atomorsqlen2[2]
        sres = repeat(0,len)
        for i=1 to len do
            word = peek(addr) + peek(addr+1) * 256
            sres[i] = word
            addr += 2
        end for
        return sres
    end if
end function


global function peek2s(object atomorsqlen2)
atom addr
integer word, len
sequence sres
    if atom(atomorsqlen2) then
        addr = atomorsqlen2
        word = peek(addr) + peek(addr+1) * 256
        if word > 32767 then
            word -= 65536
        end if
        return word
    else
        if length(atomorsqlen2)!=2 then ?9/0 end if
        addr = atomorsqlen2[1]
        len = atomorsqlen2[2]
        sres = repeat(0,len)
        for i=1 to len do
            word = peek(addr) + peek(addr+1) * 256
            if word > 32767 then
                word -= 65536
            end if
            sres[i] = word
            addr += 2
        end for
        return sres
    end if
end function
--*/

--/* Not required for Phix (defined in builtins\peekstr.e)
global
function peek_string(atom addr)
atom last

    last = addr

    -- find the string's null terminator
    while peek(last) do
        last += 1
    end while

    if addr != last then
        return peek( {addr, last - addr} )
    else
        return ""
    end if

end function
--*/

global function peek_string_zz(atom addr)
sequence text, strings

    strings = {}
    
    while peek(addr) do
        text = peek_string(addr)
        strings = append(strings, text)
        addr += length(text) + 1        
    end while

    -- special case where NO strings were found in that, technically, the
    -- first NULL represents an empty string
    if not length(strings) then
        strings = { "" }
    end if

    return strings

end function

--/* -- Phix: use the ones in pcase.e (which also do ég <-> ÉG)

constant CASEDISPLACEMENT = 'a' - 'A'

global
function upper(object s)
    if atom(s) then
        if s >= 'a' then
            if s <= 'z' then
                return s - CASEDISPLACEMENT
            end if
        end if
        return s
    end if
    for i = 1 to length(s) do
        s[i] = upper(s[i])
    end for
    return s
end function

global
function lower(object s)
    if atom(s) then
        if s >= 'A' then
            if s <= 'Z' then
                return s + CASEDISPLACEMENT
            end if
        end if
        return s
    end if
    for i = 1 to length(s) do
        s[i] = lower(s[i])
    end for
    return s
end function

--*/
