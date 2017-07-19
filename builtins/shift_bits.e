--
-- builtins/shift_bits.e
-- =====================
--
--  Copied from std/math.e, for compatibility with OpenEuphoria.
--

--**
-- Moves the bits in the input value by the specified distance.
--
-- Parameters:
--   # ##source_number## : object: The value(s) whose bits will be be moved.
--   # ##shift_distance## : integer: number of bits to be moved by. 
-- Comments:
-- * If ##source_number## is a sequence, each element is shifted.
-- * The value(s) in ##source_number## are first truncated to a 32-bit integer.
-- * The output is truncated to a 32-bit integer.
-- * Vacated bits are replaced with zero.
-- * If ##shift_distance## is negative, the bits in ##source_number## are moved left.
-- * If ##shift_distance## is positive, the bits in ##source_number## are moved right.
-- * If ##shift_distance## is zero, the bits in ##source_number## are not moved.
--
-- Returns:
-- Atom(s) containing a 32-bit integer. A single atom in ##source_number## is an atom, or
-- a sequence in the same form as ##source_number## containing 32-bit integers.
--
-- Example 1:
-- <eucode>
-- ? shift_bits((7, -3) --> 56
-- ? shift_bits((0, -9) --> 0
-- ? shift_bits((4, -7) --> 512
-- ? shift_bits((8, -4) --> 128
-- ? shift_bits((0xFE427AAC, -7) --> 0x213D5600
-- ? shift_bits((-7, -3) --> -56  which is 0xFFFFFFC8 
-- ? shift_bits((131, 0) --> 131
-- ? shift_bits((184.464, 0) --> 184
-- ? shift_bits((999_999_999_999_999, 0) --> -1530494977 which is 0xA4C67FFF
-- ? shift_bits((184, 3) -- 23
-- ? shift_bits((48, 2) --> 12
-- ? shift_bits((121, 3) --> 15
-- ? shift_bits((0xFE427AAC, 7) -->  0x01FC84F5
-- ? shift_bits((-7, 3) --> 0x1FFFFFFF
-- ? shift_bits({48, 121}, 2) --> {12, 30}
-- </eucode>
--
-- See Also:
--   [[:rotate_bits]]

global function shift_bits(object source_number, integer shift_distance)
integer lSigned
    if sequence(source_number) then
        for i = 1 to length(source_number) do
            source_number[i] = shift_bits(source_number[i], shift_distance)
        end for
        return source_number
    end if
    source_number = and_bits(source_number, 0xFFFFFFFF)
    if shift_distance = 0 then
        return source_number
    end if
        
    if shift_distance < 0 then
        source_number *= power(2, -shift_distance)
    else
        lSigned = 0
        -- Check for the sign bit so we don't propagate it.
        if and_bits(source_number, 0x80000000) then
            lSigned = 1
            source_number = and_bits(source_number, 0x7FFFFFFF)
        end if
--PL (nb this change is not apparently rqd to OE...)
--      source_number /= power(2, shift_distance)
        source_number = floor(source_number/power(2, shift_distance))
        if lSigned and shift_distance < 32 then
            -- Put back the sign bit now shifted
            source_number = or_bits(source_number, power(2, 31-shift_distance))
        end if
    end if
        
    return and_bits(source_number, 0xFFFFFFFF)
end function

