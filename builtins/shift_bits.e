--
-- builtins\shift_bits.e
-- =====================
--
--  Copied from std/math.e, for compatibility with OpenEuphoria.
--

--**
-- See Also:
--   [[:rotate_bits]]

global function shift_bits(object n, integer shift_distance)
    if sequence(n) then
        for i=1 to length(n) do
            n[i] = shift_bits(n[i], shift_distance)
        end for
        return n
    end if
    if shift_distance!= 0 then
        if shift_distance < 0 then
            n *= power(2, -shift_distance)
        else
            integer lSigned = 0
            -- Check for the sign bit so we don't propagate it.
            if and_bits(n, 0x80000000) then
                lSigned = 1
                n = and_bits(n, 0x7FFFFFFF)
            end if
--PL (nb this change is not apparently rqd to OE...)
--          n /= power(2, shift_distance)
            n = floor(n/power(2, shift_distance))
            if lSigned and shift_distance < 32 then
                -- Put back the sign bit now shifted
                n = or_bits(n, power(2, 31-shift_distance))
            end if
        end if
    end if
--6/4/24...
--  n = and_bits(n, 0xFFFFFFFF)
    return n
end function

global function count_bits(object n, integer nFrames=2)
    if sequence(n) then
        for i=1 to length(n) do
            n[i] = count_bits(n[i],nFrames+1)
        end for
        return n
    end if
    integer res = 0
    if n<0 or (not integer(n) and atom_to_float64(n)[$]=127) then -- [+/-]nan/inf
        return -1
    end if
--  if n>0 and n<1 then
--  if n>0 and n!=floor(n) then
    if n!=floor(n) then
        if n>1 then
            res = count_bits(floor(n),nFrames+1)
            n -= floor(n)
        end if
        while n!=0 do
            while n<1 do
                n*=2
            end while
            n -= 1
            res += 1
        end while
    else
--      if n<0 or n!=round(n) then
--          string g = sprintf("%g",n)
--          if not find('.',g) then g &= sprintf("%+g",n-round(n)) end if
--          crash("count_bits(%s): argument must be a non-negative integer",{g},nFrames:=nFrames)
--      end if
--javascript compatibility... (now fixed, but it's a better test anyway)
--      while not integer(n) and n!=0 do
        while n>iff(machine_bits()=32?0x3FFF_FFFF:#3FFF_FFFF_FFFF_FFFF) do
--          res += and_bits(n,1)
            res += odd(n)
            n = floor(n/2)
        end while
        while n do
-- naieve, slower:
--          res += odd(n)
--          n = floor(n/2)
-- Kernigans bit counter:
-- eg n = 0b10101 && n-1=0b10100 => 0b10100
--    n = 0b10100 && n-1=0b10011 => 0b10000
--    n = 0b10000 && n-1=0b01111 => 0b00000
--          n = and_bits(n,n-1)
            n &&= n-1
            res += 1
        end while
    end if
    return res
end function

