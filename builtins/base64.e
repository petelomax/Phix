--
-- builtins\base64.e
-- =================
--   Base 64 Encoding and Decoding
--
--   Base64 is used to encode binary data into an ASCII string; this allows
--   binary data to be transmitted using media designed to transmit text data only.
--   See http://en.wikipedia.org/wiki/Base64 and the RFC 2045 standard for more
--   information.
--

constant aleph = {'A','B','C','D','E','F','G','H','I','J','K','L','M',
                  'N','O','P','Q','R','S','T','U','V','W','X','Y','Z',
                  'a','b','c','d','e','f','g','h','i','j','k','l','m',
                  'n','o','p','q','r','s','t','u','v','w','x','y','z',
                  '0','1','2','3','4','5','6','7','8','9','+','/'}

--#
--# - see also ccha in which inverted decode table is built.
--#
--# Pad character is '=' (61)
--#
--# encoded lines should be no more than 76 characters long.
--#
--# Base 64 has been chosen (in RFC 2045) since
--#
--#    256^3 = 16777216 = 64^4
--# 
--# hence every 3 input characters can be represented by 4 encoded,
--# and vice versa.
--# If length(s) mod 3 = 0 no padding.
--# If length(s) mod 3 = 1 code with two bytes and pad with two 
--# If length(s) mod 3 = 2 code with three bytes and pad with one
--#
--# For each set of three input characters:
--#    (from writing out the bits longhand)
--#
--# out[1]=                   in[1]/4
--# out[2]=in[1] rem 4 * 16 + in[2]/16
--# out[3]=in[2] rem 16 * 4 + in[3]/64
--# out[4]=in[3] rem 64 * 1 
--#
constant ediv = {4, 16, 64,  0}
constant erem = {0,  4, 16, 64}
constant emul = {0, 16,  4,  1}
--#
--# When decoding the last four characters,
--# if byte(3) = pad then length = 1 
--# else if byte(4) = pad then length = 2 
--# else length = 3
--#
--# For each set of four input characters:
--#    (from writing out the bits longhand)
--#
--# out[1]=in[1]        * 4  + in[2]/16
--# out[2]=in[2] rem 16 * 16 + in[3]/4   
--# out[3]=in[3] rem  4 * 64 + in[4]
--#
constant drem = {64, 16,  4}
constant dmul = { 4, 16, 64}
constant ddiv = {16,  4,  1}

--# Some constants for smart looping

constant nc4   = {2, 3, 4, 1} --# 1234123412341234...
constant next  = {1, 1, 1, 0} --# increment by 3 every 4 iterations
constant nc3   = {3, 1, 2}  --# 321321321321...
constant ldrop = {2, 1, 1}  --# to drop len by 4 every 3 output


global function encode_base64(sequence in, integer wrap_column = 0)
--
-- encodes to base64.
--
-- The in parameter should be a string or sequence of bytes, ie no float/string/sequence elements
-- The result should be broken into lines of no more than 76 characters before transmission.
--
integer len, oidx, prev, case4, tmp, inch
sequence result

    len = length(in)

    --#
    --# start with a full-length sequence of pads;
    --# then decrement oidx to leave rqd pads in place
    --#
    oidx = floor((len+2)/3)*4
    result = repeat('=', oidx)
    if remainder(len,3)!=0 then
        oidx = oidx+remainder(len,3)-3
    end if

    case4 = 1
    inch  = 1
    for i=1 to oidx do
        --#
        --# out[1]=                   in[1]/4
        --# out[2]=in[1] rem 4 * 16 + in[2]/16
        --# out[3]=in[2] rem 16 * 4 + in[3]/64
        --# out[4]=in[3] rem 64 * 1 
        --#
        --# ediv = {4,16,64, 0}
        --# erem = {0, 4,16,64}
        --# emul = {0,16, 4, 1}
        --#
        tmp = 0
        if ediv[case4]>0 and inch<=len then
            tmp = floor(in[inch]/ediv[case4])
        end if
        if erem[case4]>0 then
            tmp += remainder(prev, erem[case4])*emul[case4]
        end if
        result[i] = aleph[tmp+1] --# and encode it
        if inch<=len then
            prev = in[inch]
        end if
        inch += next[case4]
        case4 = nc4[case4]
    end for

    if wrap_column>0
    and length(result)>wrap_column then
        sequence chunks = {}
        while 1 do
            if length(result)<=wrap_column then
                chunks = append(chunks,result)
                exit
            end if
            chunks = append(chunks,result[1..wrap_column])
            result = result[wrap_column+1..$]
        end while
        result = join(chunks, "\r\n")
    end if

    return result
end function


global function decode_base64(sequence in)
--
-- decodes from base64.
--
-- The in parameter should be a string or sequence of characters with a length 
--  which is a multiple of 4 between 4 and 76 (inclusive).
-- Note: As per RFC2045, lines should not exceed 76 characters. Rather than
--  join recieved encoded lines and pass the lot to decode() in one go, you
--  should pass individual lines and join the results from decode(). This
--  routine will not fare well if passed a string with embedded linebreaks.
--
integer len, oidx, case3, tmp
sequence result
sequence ccha

--  in = substitute(in,"\r\n","")
--  in = substitute(in,"\r","")
--  in = substitute(in,"\n","")

    len = length(in)
    if remainder(len, 4)!=0 then
        return -1
    end if

    oidx = (len/4)*3
    case3 = 3

    while in[len]='=' do    --# should only happen 0 1 or 2 times
        oidx -= 1
        case3 = nc3[case3]
        len -= 1
    end while

    --#
    --# invert aleph to a decode table
    --#
    ccha = repeat(0, 256)
    for i=1 to 64 do
        ccha[aleph[i]] = i-1
    end for

    result = repeat('?', oidx)
    for i=oidx to 1 by -1 do
        --#
        --# out[1]=in[1]        * 4  + in[2]/16
        --# out[2]=in[2] rem 16 * 16 + in[3]/4   
        --# out[3]=in[3] rem  4 * 64 + in[4]
        --#
        --# drem = {64,16,4}
        --# dmul = {4,16,64}
        --# ddiv = {16,4,1}
        --#
        tmp = remainder(ccha[in[len-1]], drem[case3])*dmul[case3]
        tmp += floor(ccha[in[len]]/ddiv[case3])
        result[i] = tmp
        len -= ldrop[case3]
        case3 = nc3[case3]
    end for

    return result
end function
