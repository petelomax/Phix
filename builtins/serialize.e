--
-- serialize.e
--
-- Serialization of Data Objects
--
-- Based on and providing compatibility for OpenEuphoria's std/serialize.e
--
-- An additional parameter has been addded to deserialize to control whether or
-- not the final pos should be returned when processing a sequence input. (The
-- OpenEuphoria version always returns {value,pos} when given a sequence, but 
-- not a file.)
--

--
-- Internal format
-- ===============
--
-- First byte:
--          0..248    -- immediate small integer, -9 to 239
                      -- since small negative integers -9..-1 might be common
constant I2B = 249,   -- 2-byte signed integer follows
         I3B = 250,   -- 3-byte signed integer follows
         I4B = 251,   -- 4-byte signed integer follows
         F4B = 252,   -- 4-byte f.p. number follows
         F8B = 253,   -- 8-byte f.p. number follows
         S1B = 254,   -- sequence, 1-byte length follows, then elements
         S4B = 255,   -- sequence, 4-byte length follows, then elements
         I8B = 0,     -- 8-byte integer, for 4.1 compatibility
         F10 = 1      -- 10-byte floating point, for 4.1 compatibility

-- Note that I8B and F10 are encoded inside (/after) an S4B with a 
-- length of that value, which would not otherwise legally occur.
-- While that wastes a little bit of space, it ensures that this can 
-- decode objects which were serialized with an older/OE version.

constant MIN1B = -9,
--       MAX1B = 237,
         MAX1B = 239,
--       MIN2B = -power(2, 15),
         MIN2B = -32768,
--       MAX2B =  power(2, 15)-1,
         MAX2B =  32767,
--       MIN3B = -power(2, 23),
         MIN3B = -8388608,
--       MAX3B =  power(2, 23)-1,
         MAX3B =  8388607,
--       MIN4B = -power(2, 31),
         MIN4B = -2147483648,
--       MAX4B =  power(2, 31)-1
         MAX4B =  2147483647
--       MIN8B = -power(2, 63),
--       MAX8B =  power(2, 63)-1

function getonebyte(object sf, integer pos)
    integer onebyte
    if sequence(sf) then
        onebyte = sf[pos]
        pos += 1
    elsif pos<0 then
        onebyte = peek(sf-pos-1)
        pos -= 1
    else
        onebyte = getc(sf)
    end if
    return {onebyte,pos}
end function

function getint(object sf, integer n, integer pos)
-- read an n-byte signed integer from the file or sequence
integer onebyte
atom res = 0, 
     mult = 1 -- then #100, #10000, #1000000, etc
    for i=1 to n do
        {onebyte,pos} = getonebyte(sf,pos)
        res += onebyte*mult
        mult *= #100
    end for
    return {res,pos}
end function

function getflt(object sf, integer n, integer pos)
-- return a 4, 8, or 10 byte sequence, for passing to floatNN_to_atom()
string floatseq = repeat('0',n) -- (binary, not human readable)
integer onebyte
    for i=1 to n do
        {onebyte,pos} = getonebyte(sf,pos)
        floatseq[i] = onebyte
    end for
    return {floatseq,pos}
end function

function deserialize_object(object sf, integer c, integer pos)
-- Read a serialized object from sequence or file.
-- A non-zero c is a prefetched byte.
atom len
object res

    if c=0 then
        {c,pos} = getonebyte(sf,pos)
        if c<I2B then
            res = c+MIN1B
            return {res,pos}
        end if
    end if

    switch c do
        case I2B then
            {res,pos} = getint(sf,2,pos)
            res += MIN2B

        case I3B then
            {res,pos} = getint(sf,3,pos)
            res += MIN3B

        case I4B then
            {res,pos} = getint(sf,4,pos)
            res += MIN4B

        case F4B then
            {res,pos} = getflt(sf,4,pos)
            res = float32_to_atom(res)

        case F8B then
            {res,pos} = getflt(sf,8,pos)
            res = float64_to_atom(res)

-- 30/10/2020 (p2js_parse.e)
--      case else
        default:
            -- sequence
            if c=S1B then
                {len,pos} = getonebyte(sf,pos)
            else
                {len,pos} = getint(sf,4,pos)
            end if
            if len<0 or not integer(len) or len>MAX4B then
                ?9/0
            end if
            if c=S4B and len<256 then
                if len=I8B then
                    {res,pos} = getint(sf,8,pos)

                elsif len=F10 then
                    {res,pos} = getflt(sf,10,pos)
                    res = float80_to_atom(res)

                else
--                  crash("Invalid sequence serialization")
                    ?9/0
                end if
            else
                res = repeat(' ',len)
                for i=1 to len do
                    -- in-line small integer for greater speed on strings
                    {c,pos} = getonebyte(sf,pos)
                    if c<I2B then
                        res[i] = c+MIN1B
                    else
--p2js(?)
--                      {res[i],pos} = deserialize_object(sf, c, pos)
                        sequence rip = deserialize_object(sf, c, pos)
                        res[i] = rip[1]
                        pos = rip[2]
                    end if
                end for
            end if
    end switch
    return {res,pos}
end function

global function deserialize(object sf, integer pos = 1, integer returnpos = 0)
-- read a serialized Euphoria object
-- sf: a sequence or integer file handle or memory address (if pos<0)
-- pos: ignored when sf is an atom, otherwise an index into sequence sf which
--      must point to the start of a serialized object. Defaults to 1.
-- returnpos: 1 = yes, 0 = no

    object res = deserialize_object(sf, 0, pos) -- nb res is really {res,pos}
    if returnpos=0 then
        res = res[1]
    end if
    return res
end function

global function serialize(object x)
-- return the serialized representation of any object, as a sequence of bytes
string x4, s    -- 20/06/2020 now yields a binary string

    if integer(x) then -- and x >= MIN4B and x <= MAX4B then
        if x>=MIN1B and x<=MAX1B then
            x -= MIN1B
            s = "" & x

        elsif x>=MIN2B and x<=MAX2B then
            x -= MIN2B
            s = I2B & and_bits(x, #FF) & floor(x/#100)

        elsif x>=MIN3B and x<=MAX3B then
            x -= MIN3B
            s = I3B & and_bits(x, #FF) & and_bits(floor(x/#100), #FF) & floor(x/#10000)

        elsif x>=MIN4B and x<=MAX4B then
            x -= MIN4B
            s = I4B & int_to_bytes(x)

        else
            s = S4B & I8B & repeat('\0',3) & int_to_bytes(x, 8)

        end if

    elsif atom(x) then
        -- floating point
        x4 = atom_to_float32(x)
        if x=float32_to_atom(x4) then
            -- can represent as 4-byte float
            s = F4B & x4
        else
            x4 = atom_to_float64(x)
            if x=float64_to_atom(x4) then
                s = F8B & atom_to_float64(x)
            else
                s = S4B & F10 & repeat('\0',3) & atom_to_float80(x)
            end if
        end if

    else
        -- sequence
        integer lx = length(x)
        if lx<=255 then
            s = S1B & lx
        else
            s = S4B & int_to_bytes(lx)
        end if
        for i=1 to lx do
            s &= serialize(x[i])
        end for
    end if
    return s
end function

--**
-- saves a Euphoria object to disk in a binary format.
--
-- Parameters:
-- # ##data## : any Euphoria object.
-- # ##filename## : the name of the file to save it to.
--
-- Returns:
-- An **integer**, 0 if the function fails, otherwise the number of bytes in the
-- created file.
-- 
-- Comments:
-- If the named file does not exist it is created, otherwise it is overwritten.
--
-- You can use the [[:load]] function to recover the data from the file.
--
-- Example 1:
-- <eucode>
-- include std/serialize.e
-- integer size = dump(myData, theFileName) 
-- if size = 0 then
--     puts(1, "Failed to save data to file\n")
-- else
--     printf(1, "Saved file is %d bytes long\n", size)
-- end if
-- </eucode>
--
--public function dump(sequence data, sequence filename)
--  integer fh
--  sequence sdata
--  
--  fh = open(filename, "wb")
--  if fh < 0 then
--      return 0
--  end if
--  
--  sdata = serialize(data)
--  puts(fh, sdata)
--  
--  close(fh)
--  
--  return length(sdata) -- Length is always > 0
--end function

--**
-- restores a Euphoria object that has been saved to disk by [[:dump]].
--
-- Parameters:
-- # ##filename## : the name of the file to restore it from.
--
-- Returns:
-- A **sequence**, the first element is the result code. If the result code is 0
-- then it means that the function failed, otherwise the restored data is in the 
-- second element.
-- 
-- Comments:
-- This is used to load back data from a file created by the [[:dump]]
-- function.
--
-- Example 1:
-- <eucode>
-- include std/serialize.e
-- sequence mydata = load(theFileName) 
-- if mydata[1] = 0 then
--     puts(1, "Failed to load data from file\n")
-- else
--     mydata = mydata[2] -- Restored data is in second element.
-- end if
-- </eucode>
--
--public function load(sequence filename)
--  integer fh
--  sequence sdata
--
--  fh = open(filename, "rb")
--  if fh < 0 then
--      return {0}
--  end if
--  
--  sdata = deserialize(fh)
--  
--  close(fh)
--  return {1, sdata}
--end function
