--
-- hmac.e - keyed-hash message authentication code routines
--
-- note some of the byte-ordering is a little suspect... (rev4 etc should not be needed)
-- callee should include any of these actually used.. (changed 15/7/21 with the addition of sha1)
--include sha1.e
--include sha256.e
--include sha512.e
--include md5.e     -- nb not working with this (see commented out drivel below)...
--
-- Some possible alternatives (just in case...):
--  http://forums.purebasic.com/english/viewtopic.php?t=47360&p=441076
--  https://www.nayuki.io/page/fast-sha2-hashes-in-x86-assembly
--
-- free online testers:
--  https://www.freeformatter.com/hmac-generator.html#ad-output
--  https://quickhash.com/
--
-- https://en.wikipedia.org/wiki/HMAC
--

--DEV rev4 should probably be done by sha256() and sha512()... [and is in builtins]

global function hmac_digest(string s)
    -- return s as a hex-string
    string res = ""
    for i=1 to length(s)-3 by 4 do
        for j=i+3 to i by -1 do
            res &= sprintf("%02x",s[j])
        end for
    end for
    return res
end function

global
function rev4(string s)
string res = ""
    for i=1 to length(s)-3 by 4 do
        for j=i+3 to i by -1 do
            res &= s[j]
        end for
    end for
    return res
end function

function hmac(string key, message, integer hashrtn, blocksize, outputsize)
--   Inputs:
--    key:        Bytes     array of bytes
--    message:    Bytes     array of bytes to be hashed
--    hashrtn:    Function  the hash function to use (e.g. routine_id("sha256"))
--    blocksize:  Integer   the block size of the underlying hash function (e.g. 64 bytes for SHA-1)
--    outputsize: Integer   the output size of the underlying hash function (e.g. 20 bytes for SHA-1)
            
    -- Keys longer than blocksize are shortened by hashing them
    if length(key)>blocksize then
        key := call_func(hashrtn,{key}) -- Key becomes outputsize bytes long
        if length(key)!=outputsize then ?9/0 end if -- sanity check
    end if
   
    -- Keys shorter than blocksize are padded to blocksize by padding with zeros on the right
    if length(key)<blocksize then
        -- pad key with zeros to make it blocksize bytes long
        key &= repeat('\0',blocksize-length(key))
    end if
    if length(key)!=blocksize then ?9/0 end if
    
    string i_key_pad = sq_xor_bits(key,0x36)    -- Inner padded key
    string o_key_pad = sq_xor_bits(key,0x5c)    -- Outer padded key
    
    string res = call_func(hashrtn,{o_key_pad & rev4(call_func(hashrtn,{i_key_pad & message}))})

    return res
end function

global function hmac_sha256(string key, message)
    return hmac(key,message,routine_id("sha256"),blocksize:=64,outputsize:=32)
end function

global function hmac_sha512(string key, message)
    return hmac(key,message,routine_id("sha512"),blocksize:=128,outputsize:=64)
end function

global function hmac_sha1(string key, message)
    return hmac(key,message,routine_id("sha1"),blocksize:=64,outputsize:=20)
end function


--global function hmac_md5(string key, message)
--  return hmac(key,message,routine_id("md5"),blocksize:=64,outputsize:=16)
--end function
--/* -- failed tests from https://datatracker.ietf.org/doc/html/rfc2104
include md5.e
include hmac.e
--/*
Test Vectors (Trailing '\0' of a character string not included in test):

  key =         0x0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b
  key_len =     16 bytes
  data =        "Hi There"
  data_len =    8  bytes
  digest =      0x9294727a3638bb1c13f48ef8158bfc9d

  key =         "Jefe"
  data =        "what do ya want for nothing?"
  data_len =    28 bytes
  digest =      0x750c783e6ab0b503eaa86e310a5db738

  key =         0xAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA
  key_len       16 bytes
  data =        0xDDDDDDDDDDDDDDDDDDDD...
                ..DDDDDDDDDDDDDDDDDDDD...
                ..DDDDDDDDDDDDDDDDDDDD...
                ..DDDDDDDDDDDDDDDDDDDD...
                ..DDDDDDDDDDDDDDDDDDDD
  data_len =    50 bytes
  digest =      0x56be34521d144c88dbb8c733f0e8b3f6
--*/

string key, msg
key = repeat('\x0b',16)
msg = "Hi There"
--?hmac_digest(hmac_md5(key,msg))
--0x9294727a3638bb1c13f48ef8158bfc9d
-- "574E78208A9D6E6B79FEE6019EEE1687"   -- nope...
--?hmac_digest(hmac_md5(msg,key))
-- "73D3ABE111FD418F0DE5C8CD82700A0E"   -- nope...
key = "Jefe"
msg = "what do ya want for nothing?"
--?hmac_digest(hmac_md5(key,msg))
--0x750c783e6ab0b503eaa86e310a5db738
-- "731EE663C97FC6A2E715889B19355FD0"   -- nope
--?hmac_digest(hmac_md5(msg,key))
-- "445691A84AB8BB43FB1DFED4B2491575"   -- nope
key = repeat('\xAA',16)
msg = repeat('\xDD',50)
--?hmac_digest(hmac_md5(key,msg))
--0x56be34521d144c88dbb8c733f0e8b3f6
-- "2A661C296D34455D0A0C225902E3038C"   -- nope
--?hmac_digest(hmac_md5(msg,key))
-- "25F78367C0E3D15FFA7B9DE0BBF49BBC"   -- nope
--*/


