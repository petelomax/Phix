--
-- builtins/shift_bits.e
-- =====================
--
--  Copied from std/math.e, for compatibility with OpenEuphoria.
--

--**
-- See Also:
--   [[:rotate_bits]]

global function shift_bits(object source_number, integer shift_distance)
    if sequence(source_number) then
        for i=1 to length(source_number) do
            source_number[i] = shift_bits(source_number[i], shift_distance)
        end for
        return source_number
    end if
    if shift_distance!= 0 then
        if shift_distance < 0 then
            source_number *= power(2, -shift_distance)
        else
            integer lSigned = 0
            -- Check for the sign bit so we don't propagate it.
            if and_bits(source_number, 0x80000000) then
                lSigned = 1
                source_number = and_bits(source_number, 0x7FFFFFFF)
            end if
--PL (nb this change is not apparently rqd to OE...)
--          source_number /= power(2, shift_distance)
            source_number = floor(source_number/power(2, shift_distance))
            if lSigned and shift_distance < 32 then
                -- Put back the sign bit now shifted
                source_number = or_bits(source_number, power(2, 31-shift_distance))
            end if
        end if
    end if
    source_number = and_bits(source_number, 0xFFFFFFFF)
    return source_number
end function

