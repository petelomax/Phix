--
-- builtins\hll_stubs.e
-- ====================
--
--  hll verions of builtins implemented in assembly, for use when you need a routine_id.
--  
--  The compiler automatically substitutes routine_id("length") with routine_id("hll_length")
--  and likewise a bare 'length' with no following '(' with the same implied routine_id.
--
--  As noted in the footnotes of the routine_id documentation, using call_func/proc is at
--  least 3 or 4 times slower than a direct call of a hll routine, and likwise a hll routine
--  is at least 2 or 3 times slower than a direct call on asm, and even more so for inlined
--  assembly, as occurs sometimes, especially for length(). Hence these routines should not 
--  be used in the inner loops of production code where performance is in any way critical.
--
--  In some cases, the equivalent sq_xxx routine may prove significantly faster and/or easier.
--
--  Note that if you are dynamically constructing the names passed to routine_id, then you
--  should graft on the "hll_" part yourself, the compiler only does so for inline literals
--  and not for anything like "len"&"gth" or apply({"length","integer"},routine_id).
--  Also I would hope obviously this file is only an auto-include in the cases where the
--- compiler has spotted it is needed, if you're having to graft on the "hll_" yourself,
--  then you'll probably have to "include hll_stubs.e" explicitly as well.
--
--integer result:
global function hll_atom(object o) return atom(o) end function
global function hll_compare(object a,b) return compare(a,b) end function
global function hll_equal(object a,b) return equal(a,b) end function
global function hll_even(atom a) return even(a) end function
global function hll_integer(object o) return integer(o) end function
global function hll_length(sequence s) return length(s) end function
global function hll_object(object o) without warning return object(o) end function
global function hll_odd(atom a) return odd(a) end function
global function hll_sequence(object o) return sequence(o) end function
global function hll_string(object o) return string(o) end function

--atom result:
global function hll_and_bits(atom a,b) return and_bits(a,b) end function
global function hll_not_bits(atom a) return not_bits(a) end function
global function hll_or_bits(atom a,b) return or_bits(a,b) end function
global function hll_xor_bits(atom a,b) return xor_bits(a,b) end function
global function hll_remainder(atom a,b) return remainder(a,b) end function
--global function hll_rmdr(atom a,b) return remainder(a,b) end function -- (now done in psym.e)
global function hll_floor(atom a) return floor(a) end function
global function hll_power(atom a,b) return power(a,b) end function
global function hll_cos(atom a) return cos(a) end function
global function hll_sin(atom a) return sin(a) end function
global function hll_tan(atom a) return tan(a) end function
global function hll_arctan(atom a) return arctan(a) end function
global function hll_ln(atom a) return log(a) end function
global function hll_log(atom a) return log(a) end function
global function hll_sqrt(atom a) return sqrt(a) end function
global function hll_rand(atom a) return rand(a) end function

--sequence/string result:
global function hll_append(object a,b) return append(a,b) end function
global function hll_prepend(object a,b) return prepend(a,b) end function
--global function hll_repeat(object o, integer i) return repeat(o,i) end function

