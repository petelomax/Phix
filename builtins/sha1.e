--
-- builtins\sha1.e
-- ===============
--
--  NB no longer considered secure. Non-optimised.
--  (copied from demo\rosetta\sha1.exw)
--
--with javascript_semantics -- (no prob)

function uint32(atom v)
    return and_bitsu(v,#FFFFFFFF)
end function 

function sq_uint32(sequence s)
    for i=1 to length(s) do
        s[i] = uint32(s[i])
    end for
    return s
end function
 
function dword(string msg, integer i)
-- get dword as big-endian
    return msg[i]*#1000000+msg[i+1]*#10000+msg[i+2]*#100+msg[i+3]
end function
 
function xor_all(sequence s)
    atom result = 0
    for i=1 to length(s) do
        result = xor_bits(result, s[i])
    end for
    result = uint32(result)
    return result
end function 

function rol(atom word, integer bits)
-- left rotate the bits of a 32-bit number by the specified number of bits
    return uint32(word*power(2,bits))+floor(word/power(2,32-bits))
end function

global function sha1(string msg)
    atom a,b,c,d,e,temp,k
    sequence w = repeat(0,80)
    atom h0 = 0x67452301,
         h1 = 0xefcdab89,
         h2 = 0x98badcfe,
         h3 = 0x10325476,
         h4 = 0xc3d2e1f0

    integer bits = length(msg)*8
    msg &= #80
    while mod(length(msg),64)!=56 do msg &= '\0' end while
    msg &= reverse(int_to_bytes(bits,8))

    for chunk=1 to length(msg) by 64 do
        for i=1 to 16 do
            w[i] = dword(msg,chunk+(i-1)*4)
        end for
        for i=17 to 80 do
            w[i] = rol(xor_all({w[i-3],w[i-8],w[i-14],w[i-16]}),1)
        end for
        {a,b,c,d,e} = {h0,h1,h2,h3,h4}
        for i=1 to 80 do
            if i<=20 then
                temp = or_bits(and_bits(b,c),and_bits(not_bits(b),d))
                k = #5A827999
            elsif i<=40 then
                temp = xor_bits(xor_bits(b,c),d)
                k = #6ED9EBA1
            elsif i<=60 then
                temp = or_bits(or_bits(and_bits(b,c),and_bits(b,d)),and_bits(c,d))
                k = #8F1BBCDC
            else -- i<=80
                temp = xor_bits(xor_bits(b,c),d)
                k = #CA62C1D6
            end if
            temp = uint32(rol(a,5)+temp+e+k+w[i])
            {a,b,c,d,e} = {temp,a,rol(b,30),c,d}
        end for
        {h0,h1,h2,h3,h4} = sq_uint32(sq_add({h0,h1,h2,h3,h4},{a,b,c,d,e}))
    end for
--  sequence res = {h0, h1, h2, h3, h4}
--  for i=1 to length(res) do
--      res[i] = sprintf("%08X",res[i])
--  end for
--  return join(res)
--  atom m = allocate(20)
--  poke4(m,{h0,h1,h2,h3,h4})
--  string res = peek({m,20})
--  free(m)
    sequence s = {h0,h1,h2,h3,h4}
    string res = repeat('\0',20)
--  for i=1 to 5 do
--      atom h = s[i]
--      for j=0 to 3 do
--          res[i*4-j] = and_bits(h,#FF)
--          h = floor(h/#100)
--      end for
--  end for
    for i=1 to 5 do
        atom h = s[i]
        for j=-3 to 0 do
            res[i*4+j] = and_bits(h,#FF)
            h = floor(h/#100)
        end for
    end for
    return res
end function

--/*
string res = sha1("Rosetta Code")
?res

if res!="48c98f7e 5a6e736d 790ab740 dfc3f51a 61abe2b5" then ?9/0 end if
res = sha1("Rosetta code")
if res!="b18c883f 4da75016 4b5af362 ea9b9f27 f90904b4" then ?9/0 end if
res = sha1("abc")
if res!="a9993e36 4706816a ba3e2571 7850c26c 9cd0d89d" then ?9/0 end if
res = sha1("Ars longa, vita brevis")
if res!="e640d285 242886eb 96ab80cb f858389b 3df52f43" then ?9/0 end if
--*/
