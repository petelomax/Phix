--
-- builtins\base64.e
-- =================
--
--   Base 64 Encoding and Decoding (autoinclude)
--
--   Author: Pete Lomax  Jul 3 2002
--
--   Base64 is used to encode binary data into an ASCII string; this allows
--   binary data to be transmitted using media designed to transmit text data only.
--   See http://en.wikipedia.org/wiki/Base64 and the RFC 2045 standard for more
--   information.
--
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
--# out[1]=                    src[1]/4
--# out[2]=src[1] rem 4 * 16 + src[2]/16
--# out[3]=src[2] rem 16 * 4 + src[3]/64
--# out[4]=src[3] rem 64 * 1 
--#
--#
--# When decoding the last four characters,
--# if byte(3) = pad then length = 1 
--# else if byte(4) = pad then length = 2 
--# else length = 3
--#
--# For each set of four input characters:
--#    (from writing out the bits longhand)
--#
--# out[1]=src[1]        * 4  + src[2]/16
--# out[2]=src[2] rem 16 * 16 + src[3]/4     
--# out[3]=src[3] rem  4 * 64 + src[4]
--#

bool base64_init = false
sequence aleph
sequence ccha   -- inverted decode table


procedure init_base64()
    aleph = {'A','B','C','D','E','F','G','H','I','J','K','L','M',
             'N','O','P','Q','R','S','T','U','V','W','X','Y','Z',
             'a','b','c','d','e','f','g','h','i','j','k','l','m',
             'n','o','p','q','r','s','t','u','v','w','x','y','z',
             '0','1','2','3','4','5','6','7','8','9','+','/'}
    --#
    --# invert aleph to a decode table
    --#
    ccha = repeat(0, 256)
    for i=1 to length(aleph) do
        ccha[aleph[i]] = i-1
    end for
    base64_init = true
end procedure

global function encode_base64(sequence src, integer wrap_column=0)
--
-- encodes to base64.
--
-- The src parameter should be a string or sequence of bytes, ie no float/string/sequence elements
-- The result should be broken into lines of no more than 76 characters before transmission.
--
sequence ediv = {4, 16, 64,  0},
         erem = {0,  4, 16, 64},
         emul = {0, 16,  4,  1},
         nc4  = {2,  3,  4,  1}, --# 1234123412341234...
         next = {1,  1,  1,  0} --# increment by 3 every 4 iterations

    if not base64_init then init_base64() end if

    --#
    --# start with a full-length sequence of pads;
    --# then decrement oidx to leave rqd pads in place
    --#
    integer len = length(src),
            oidx = floor((len+2)/3)*4
    string result = repeat('=', oidx)
    if remainder(len,3)!=0 then
        oidx = oidx+remainder(len,3)-3
    end if

    integer case4 = 1,
            sdx = 1,
            prev, tmp
    for i=1 to oidx do
        --#
        --# out[1]=                    src[1]/4
        --# out[2]=src[1] rem 4 * 16 + src[2]/16
        --# out[3]=src[2] rem 16 * 4 + src[3]/64
        --# out[4]=src[3] rem 64 * 1 
        --#
        --# ediv = {4,16,64, 0}
        --# erem = {0, 4,16,64}
        --# emul = {0,16, 4, 1}
        --#
        tmp = 0
        if ediv[case4]>0 and sdx<=len then
            integer ch = src[sdx]
            if ch<0 or ch>#FF then ?9/0 end if  -- bytes only!
            tmp = floor(ch/ediv[case4])
        end if
        if erem[case4]>0 then
            tmp += remainder(prev, erem[case4])*emul[case4]
        end if
        result[i] = aleph[tmp+1] --# and encode it
        if sdx<=len then
            prev = src[sdx]
        end if
        sdx += next[case4]
        case4 = nc4[case4]
    end for

    if wrap_column>0 then
        len = length(result)
        integer start = 1
        if len>wrap_column then
            sequence chunks = {}
            while 1 do
                if len<=wrap_column then
                    chunks = append(chunks,result[start..$])
                    exit
                end if
                tmp = start
                start += wrap_column
                chunks = append(chunks,result[tmp..start-1])
                len -= wrap_column
            end while
            result = join(chunks, "\r\n")
        end if
    end if

    return result
end function


global function decode_base64(sequence src)
--
-- decodes from base64.
--
-- The src parameter should be a string or sequence of characters with a length 
--  which is a multiple of 4 between 4 and 76 (inclusive).
--
-- The result is a string, but it may contain binary rather than text.
--
-- Note: As per RFC2045, lines should not exceed 76 characters.
--  Joining received encoded lines and passing the lot to decode() in one go
--  should be expected to be a little slower than passing individual lines 
--  and joining the results from decode() - simply because the decoded form 
--  is 3/4 or 75% of the size of the encoded form. However there may be cases
--  such as splitting an already joined line, that tip things the other way.
--
sequence drem = {64, 16,  4},
         dmul = { 4, 16, 64},
         ddiv = {16,  4,  1},
         nc3  = { 3,  1,  2},   --# 321321321321...
         ldrop = {2,  1,  1}    --# to drop len by 4 every 3 output

    if not base64_init then init_base64() end if

    src = substitute_all(src,{"\r\n","\r","\n"},{"","",""})

    integer len = length(src)
    if remainder(len, 4)!=0 then ?9/0 end if

    integer oidx = (len/4)*3,
            case3 = 3, tmp

    while src[len]='=' do   --# should only happen 0 1 or 2 times
        oidx -= 1
        case3 = nc3[case3]
        len -= 1
    end while

    string result = repeat('?', oidx)
    for i=oidx to 1 by -1 do
        --#
        --# out[1]=src[1]        * 4  + src[2]/16
        --# out[2]=src[2] rem 16 * 16 + src[3]/4     
        --# out[3]=src[3] rem  4 * 64 + src[4]
        --#
        --# drem = {64,16, 4}
        --# dmul = { 4,16,64}
        --# ddiv = {16, 4, 1}
        --#
        tmp = remainder(ccha[src[len-1]], drem[case3])*dmul[case3]
        tmp += floor(ccha[src[len]]/ddiv[case3])
        result[i] = tmp
        len -= ldrop[case3]
        case3 = nc3[case3]
    end for

    return result
end function

--/*
just some quick tests:
sequence s, e, d
s = sq_sub(sq_rand(repeat(256,10000)),1)    -- NB: 0..255 only
e = encode_base64(s,76)
d = decode_base64(e)
if d!=s then ?9/0 end if
e = encode_base64(tagset(54))
?e
?length(e)

for i=1 to 57 do
    s = tagset(i)
    e = encode_base64(s)
    d = decode_base64(e)
    if d!=s then ?9/0 end if
end for
--*/
