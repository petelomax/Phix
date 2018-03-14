--
-- pokestr.e
--
-- Phix implementation of poke_string
--
-- This is an auto-include file; there is no need to manually include
--  it, unless you want a namespace.
--

global function poke_string(atom buffaddr, integer buffsize, sequence s)

    if buffaddr<=0 or buffsize<=length(s) then
        return 0
    end if

    if not string(s) then
        for i=1 to length(s) do
            if not atom(s[i]) then return 0 end if
        end for
    end if

    poke(buffaddr, s)
    buffaddr += length(s)
    poke(buffaddr, 0)

    return buffaddr
end function

global function poke_wstring(atom buffaddr, integer buffsize, sequence s)

    if buffaddr<=0 or buffsize<=length(s)*2 then
        return 0
    end if

    if not string(s) then
        for i=1 to length(s) do
            if not atom(s[i]) then return 0 end if
        end for
    end if

    poke2(buffaddr, s)
    buffaddr += length(s)*2
    poke2(buffaddr, 0)

    return buffaddr
end function
