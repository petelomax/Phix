--
-- puts1h.e
-- ========
--
--  Low-level (/temp/test/diag) simplistic console i/o hll wrapper routines.
--
--  This is /NOT/ an autoinclude, and there are no plans to ever make it one, whereas
--  builtins\VM\puts1.e (which this file uses) /IS/ a proper optable'd autoinclude.
--
--  The hll routines below should also serve as all the documentation you could ever possibly
--  need, including precise instructions for/examples of invoking things from inline assembly.
--

include builtins\VM\puts1.e     -- the low-level #ilASM that the following routines call.

global procedure puts1(string s)
-- Equivalent to puts(1,s)
--  if not string(s) then s = toString(s) end if    -- (maybe one day, but probably better off being done in the calling routine)
    #ilASM{ 
        [32]
            mov edi,[s]
            call :%puts1    -- (nb use :%puts1ediesi if edi is not a phix string)
        [64]
            mov rdi,[s]
            call :%puts1    -- (nb use :%puts1rdirsi if rdi is not a phix string)
        []
    }
end procedure

global procedure puthex32(atom a, integer showcr=1)
--
-- Display an unsigned integer on the console in hexadecimal.
-- Equivalent to printf(1,"%08x%s",{a,(""|"\n")}))
-- Note that a is only an atom to cover (32-bit integer) values #3FFFFFFF..#FFFFFFFF, ...
--  a = and_bits(a,#FFFFFFFF) -- ... and it behaves as if this line was not commented out.
--
    #ilASM{ 
        [32]
            mov edx,[a]
        [64]
            mov rdx,[a]
        []
            push [showcr]       -- (0 or 1)
            call :%puthex32a    -- (nb use :%puthex32 (no "a") if edx/rdx is not a phix atom)
                                --      (there is also a #ilASM-only :%puthex64 (no "a").)
    }
end procedure

global procedure putsint(integer i, integer showcr=1)
--
-- Display a signed integer on the console in decimal, optionally with a trailing cr
-- Equivalent to printf(1,"%d%s",{i,iff(showcr,"\n","")})
-- NB it is not meant to display (/work on) anything outside +/-9,999,999,999, if
--    you have any issues, use puthex32 instead and do the conversions yourself.
--
    #ilASM{ 
        [32]
            mov eax,[i]
        [64]
            mov rax,[i]
        []
            push [showcr]       -- 0 or 1, 32 or 64 bit.
            call :%putsint      -- (ps should be ok for any 32-bit integer, including > h4)
                                -- ( - by which I mean in e/rax, rather than in integer i)
          }
end procedure

global procedure getc0()
--
-- Equivalent to {} = getc(0)
--           aka if getc(0) then end if
--
    #ilASM{ 
            call :%getc0
          }
end procedure

