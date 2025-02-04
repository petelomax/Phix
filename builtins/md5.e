--
-- builtins\md5.e
--
-- Non-optimised. Originally written by Davi Tassinari de Figueiredo.
-- Not documented. Actually taken from demo\rosetta\md5.exw ...
--
--with javascript_semantics -- (fine)

function uxor(atom data1,atom data2)
    atom result = xor_bits(data1,data2)
    if result<0 then result += #100000000 end if
    return result
end function
 
function uor(atom data1,atom data2)
    atom result = or_bits(data1,data2)
    if result<0 then result += #100000000 end if
    return result
end function
 
function r32(atom a)
    return remainder(a,#100000000)
end function
 
function rol(atom word,integer bits)
    -- left rotate the bits of a 32-bit number by the specified number of bits
    return r32(word*power(2,bits))+floor(word/power(2,32-bits))
end function
 
constant K =
{#d76aa478, #e8c7b756, #242070db, #c1bdceee, #f57c0faf, #4787c62a, #a8304613, #fd469501,
 #698098d8, #8b44f7af, #ffff5bb1, #895cd7be, #6b901122, #fd987193, #a679438e, #49b40821,
 #f61e2562, #c040b340, #265e5a51, #e9b6c7aa, #d62f105d, #02441453, #d8a1e681, #e7d3fbc8,
 #21e1cde6, #c33707d6, #f4d50d87, #455a14ed, #a9e3e905, #fcefa3f8, #676f02d9, #8d2a4c8a,
 #fffa3942, #8771f681, #6d9d6122, #fde5380c, #a4beea44, #4bdecfa9, #f6bb4b60, #bebfbc70,
 #289b7ec6, #eaa127fa, #d4ef3085, #04881d05, #d9d4d039, #e6db99e5, #1fa27cf8, #c4ac5665,
 #f4292244, #432aff97, #ab9423a7, #fc93a039, #655b59c3, #8f0ccc92, #ffeff47d, #85845dd1,
 #6fa87e4f, #fe2ce6e0, #a3014314, #4e0811a1, #f7537e82, #bd3af235, #2ad7d2bb, #eb86d391}
 
constant m_block = {1, 2, 3, 4, 5, 6, 7, 8, 9,10,11,12,13,14,15,16,
                    2, 7,12, 1, 6,11,16, 5,10,15, 4, 9,14, 3, 8,13,
                    6, 9,12,15, 2, 5, 8,11,14, 1, 4, 7,10,13,16, 3,
                    1, 8,15, 6,13, 4,11, 2, 9,16, 7,14, 5,12, 3,10}
 
constant c_words = {#67452301,#efcdab89,#98badcfe,#10325476}
 
sequence words
 
function divide_in_words(sequence message)
-- Divides the string into words (32-bit numbers)
    integer l = floor(length(message)/4)
    sequence res = repeat(0,l)
    for word=1 to l do
        res[word] = bytes_to_int(message[word*4-3..word*4])
    end for
    return res
end function
 
procedure process_block(sequence block)
-- Updates the words according to the contents of the block
atom a,b,c,d
 
    block = divide_in_words(block)
 
    a = words[1]
    b = words[2]
    c = words[3]
    d = words[4]
 
    -- Round 1
    for step=1 to 16 by 4 do
        a = r32(b+rol(r32(a+block[m_block[step  ]]+K[step  ]+uor(and_bits(b,c),and_bits(not_bits(b),d))), 7))
        d = r32(a+rol(r32(d+block[m_block[step+1]]+K[step+1]+uor(and_bits(a,b),and_bits(not_bits(a),c))),12))
        c = r32(d+rol(r32(c+block[m_block[step+2]]+K[step+2]+uor(and_bits(d,a),and_bits(not_bits(d),b))),17))
        b = r32(c+rol(r32(b+block[m_block[step+3]]+K[step+3]+uor(and_bits(c,d),and_bits(not_bits(c),a))),22))
    end for
 
    -- Round 2
    for step=17 to 32 by 4 do
        a = r32(b+rol(r32(a+block[m_block[step  ]]+K[step  ]+uor(and_bits(b,d),and_bits(c,not_bits(d)))), 5))
        d = r32(a+rol(r32(d+block[m_block[step+1]]+K[step+1]+uor(and_bits(a,c),and_bits(b,not_bits(c)))), 9))
        c = r32(d+rol(r32(c+block[m_block[step+2]]+K[step+2]+uor(and_bits(d,b),and_bits(a,not_bits(b)))),14))
        b = r32(c+rol(r32(b+block[m_block[step+3]]+K[step+3]+uor(and_bits(c,a),and_bits(d,not_bits(a)))),20))
    end for
 
    -- Round 3
    for step=33 to 48 by 4 do
        a = r32(b+rol(r32(a+block[m_block[step  ]]+K[step  ]+uxor(b,xor_bits(c,d))), 4))
        d = r32(a+rol(r32(d+block[m_block[step+1]]+K[step+1]+uxor(a,xor_bits(b,c))),11))
        c = r32(d+rol(r32(c+block[m_block[step+2]]+K[step+2]+uxor(d,xor_bits(a,b))),16))
        b = r32(c+rol(r32(b+block[m_block[step+3]]+K[step+3]+uxor(c,xor_bits(d,a))),23))
    end for
 
    -- Round 4
    for step=49 to 64 by 4 do
        a = r32(b+rol(r32(a+block[m_block[step  ]]+K[step  ]+uxor(c,or_bits(b,not_bits(d)))), 6))
        d = r32(a+rol(r32(d+block[m_block[step+1]]+K[step+1]+uxor(b,or_bits(a,not_bits(c)))),10))
        c = r32(d+rol(r32(c+block[m_block[step+2]]+K[step+2]+uxor(a,or_bits(d,not_bits(b)))),15))
        b = r32(c+rol(r32(b+block[m_block[step+3]]+K[step+3]+uxor(d,or_bits(c,not_bits(a)))),21))
    end for
 
    -- Update the words
    words = deep_copy(words)
    words[1] = r32(words[1]+a)
    words[2] = r32(words[2]+b)
    words[3] = r32(words[3]+c)
    words[4] = r32(words[4]+d)
end procedure
 
function pad_message(sequence message)
-- Add bytes to the end of the message so it can be divided
-- in an exact number of 64-byte blocks.
    integer l = length(message),
            bytes_to_add = 64-remainder(l+9,64)
    if bytes_to_add=64 then bytes_to_add = 0 end if
 
    message = deep_copy(message)
    message &= #80
    message &= repeat(0,bytes_to_add)
    message &= int_to_bytes(l*8)&{0,0,0,0}
 
    return message
end function
 
 
global function md5(sequence message)
    -- Given a string, returns a 16-byte hash of it.
 
    words = c_words -- Initialize the H words
 
    message = pad_message(message)  -- Add bytes to the message
 
    -- Process each 64-byte block
    for block=1 to length(message) by 64 do
        process_block(message[block..block+63])
    end for
 
    -- Convert hash into bytes
    return int_to_bytes(words[1])&    -- Return the hash
           int_to_bytes(words[2])&
           int_to_bytes(words[3])&
           int_to_bytes(words[4])
 
end function

/*
if platform()=JS or include_file()=1 then
    string fmt = "0x%02X%02X%02X%02X%02X%02X%02X%02X%02X%02X%02X%02X%02X%02X%02X%02X\n"
 
    printf(1,fmt,md5(""))
    printf(1,fmt,md5("a"))
    printf(1,fmt,md5("abc"))
    printf(1,fmt,md5("message digest"))
    printf(1,fmt,md5("abcdefghijklmnopqrstuvwxyz"))
    printf(1,fmt,md5("ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789"))
    printf(1,fmt,md5("12345678901234567890123456789012345678901234567890123456789012345678901234567890"))
    -- output:
    --  0xd41d8cd98f00b204e9800998ecf8427e
    --  0x0cc175b9c0f1b6a831c399e269772661
    --  0x900150983cd24fb0d6963f7d28e17f72
    --  0xf96b697d7cb7938d525a2f31aaf161d0
    --  0xc3fcd3d76192e4007dfb496cca67e13b
    --  0xd174ab98d277d9f5a5611c2c9f419d9f
    --  0x57edf4a22be3c955ac49da2e2107b67a
end if
*/
