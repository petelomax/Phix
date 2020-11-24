--
-- ppoke2.e
--
-- Phix implementations of poke2(), peek2s(), and peek2u().
--
-- This is an auto-include file; there is no need to manually include
--  it, unless you want a namespace.
--

global procedure poke2(atom addr, object x)
atom xi
    if atom(x) then
        poke(addr, and_bits(x,#FF))
        poke(addr+1, floor(and_bits(x,#FF00)/256))
    else -- sequence
        for i=1 to length(x) do
            xi = x[i]   -- helps speedwise, also ensures we do
                        --  not blindly poke nested sequences.
            poke(addr, and_bits(xi,#FF))
            addr += 1
            poke(addr, floor(and_bits(xi,#FF00)/256))
            addr += 1
        end for
--SUG, or WideString builtin? (UTF8->UTF16 would be wrong this way)
--      if string(x) then
--          -- assume we are creating a WideString and want a word terminator
--          poke(addr,0)
--          poke(addr+1,0)
--      end if
    end if
end procedure

--constant SIGNED=1, UNSIGNED=0
--function peek2(object atomorsqlen2, integer signed)
global function peek2s(object atomorsqlen2)
atom addr
integer word, len
sequence sres
    if atom(atomorsqlen2) then
        addr = atomorsqlen2
        word = peek(addr)+peek(addr+1)*256
--      if signed==SIGNED then
        if word>32767 then
            word -= 65536
        end if
--      end if
        return word
    else
        if length(atomorsqlen2)!=2 then ?9/0 end if
        addr = atomorsqlen2[1]
        len = atomorsqlen2[2]
        sres = repeat(0,len)
        for i=1 to len do
            word = peek(addr)+peek(addr+1)*256
--          if signed==SIGNED then
            if word>32767 then
                word -= 65536
            end if
--          end if
            sres[i] = word
            addr += 2
        end for
        return sres
    end if
end function

--DEV/SUG:
--global function peek2s(object atomorsqlen2)
--  return peek2(atomorsqlen2,SIGNED)
--end function

global function peek2u(object atomorsqlen2)
--  return peek2(atomorsqlen2,UNSIGNED)
--end function
atom addr
integer word, len
sequence sres
    if atom(atomorsqlen2) then
        addr = atomorsqlen2
        word = peek(addr)+peek(addr+1)*256
        return word
    else
        if length(atomorsqlen2)!=2 then ?9/0 end if
        addr = atomorsqlen2[1]
        len = atomorsqlen2[2]
        sres = repeat(0,len)
        for i=1 to len do
            word = peek(addr)+peek(addr+1)*256
            sres[i] = word
            addr += 2
        end for
        return sres
    end if
end function

