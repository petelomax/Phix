--
-- builtins\sha256.e
-- =================
--
-- fairly faithful rendition of https://en.wikipedia.org/wiki/SHA-2
--  with slightly improved names (eg s0 -> sigma0) from elsewhere.
--  Using a big block of highly optimised inline assembly (#ilASM).
--  See demo\rosetta\sha-256.exw for a slightly saner hll version.

integer sha_init4 = 0, sha_const4

procedure sha_init()
-- sha-256 initializes the hashing constants with these 8 dwords
--constant integer sha_init4 = floor(allocate(8*4)/4)
    sha_init4 = floor(allocate(8*4)/4)
    poke4(sha_init4*4,{0x6a09e667,0xbb67ae85,0x3c6ef372,0xa54ff53a,
                       0x510e527f,0x9b05688c,0x1f83d9ab,0x5be0cd19})

-- sha-256 uses 64 dwords, one for each round of main hashing routine
--constant integer sha_const4 = floor(allocate(64*4)/4)
    sha_const4 = floor(allocate(64*4)/4)
    poke4(sha_const4*4,{0x428a2f98,0x71374491,0xb5c0fbcf,0xe9b5dba5,
                        0x3956c25b,0x59f111f1,0x923f82a4,0xab1c5ed5,
                        0xd807aa98,0x12835b01,0x243185be,0x550c7dc3,
                        0x72be5d74,0x80deb1fe,0x9bdc06a7,0xc19bf174,
                        0xe49b69c1,0xefbe4786,0x0fc19dc6,0x240ca1cc,
                        0x2de92c6f,0x4a7484aa,0x5cb0a9dc,0x76f988da,
                        0x983e5152,0xa831c66d,0xb00327c8,0xbf597fc7,
                        0xc6e00bf3,0xd5a79147,0x06ca6351,0x14292967,
                        0x27b70a85,0x2e1b2138,0x4d2c6dfc,0x53380d13,
                        0x650a7354,0x766a0abb,0x81c2c92e,0x92722c85,
                        0xa2bfe8a1,0xa81a664b,0xc24b8b70,0xc76c51a3,
                        0xd192e819,0xd6990624,0xf40e3585,0x106aa070,
                        0x19a4c116,0x1e376c08,0x2748774c,0x34b0bcb5,
                        0x391c0cb3,0x4ed8aa4a,0x5b9cca4f,0x682e6ff3,
                        0x748f82ee,0x78a5636f,0x84c87814,0x8cc70208,
                        0x90befffa,0xa4506ceb,0xbef9a3f7,0xc67178f2})
end procedure

function pad64(integer v)
-- round v up to multiple of 64
    return floor((v+63)/64)*64
end function

function rev4(string s)
--string res = ""
string res = repeat(' ',0)
    for i=1 to length(s)-3 by 4 do
        for j=i+3 to i by -1 do
            res &= s[j]
        end for
    end for
    return res
end function

global function sha256(string msg)
integer len = length(msg)+1
integer mem4 = floor(allocate(32)/4)        -- for result
integer block_offset

    -- add the '1' bit and space for size in bits, padded to multiple of 64
    msg &= #80&repeat('\0',pad64(len+8)-len)
    len = (len-1)*8
    for i=length(msg) to 1 by -1 do
        msg[i] = and_bits(len,#FF)
        len = floor(len/#100)
        if len=0 then exit end if
    end for
    integer num_blocks = length(msg)/64
    if sha_init4=0 then sha_init() end if

#ilASM{

    sub esp,296

    -- stack contents: 
    -- [esp]    t2
    -- [esp+4]  t1
    -- [esp+8]  a
    -- [esp+12] b
    -- [esp+16] c
    -- [esp+20] d
    -- [esp+24] e
    -- [esp+28] f
    -- [esp+32] g
    -- [esp+36] h
    -- [esp+40] schedule (64 dwords = 256 bytes)
    -- [esp+296] <existing stack>

    -- set the initial hash values ( hash_h0 -> hash_h7 )

    mov esi,[sha_init4]
    mov edi,[mem4]
    shl esi,2
    shl edi,2
    mov ecx,8
    rep movsd

    -- prepare "global registers" for main algorithm

    cld
    mov edx,[num_blocks]
    mov [block_offset],0

    -- perform hashing one block at a time

  ::mainloop1

    -- load user data into schedule, format to little-endian

    mov esi,[msg]
    shl esi,2
--  add esi,[block_offset]  -- DEV not supported yet (by pilasm.e)
--      DEV: as block_offset is an integer, the above would be fine;
--           the hard part is atom block_offset -> error, but not when
--           inside an if integer(block_offset), etc, ie localtypes...
--      OR: error: not integer /* if this bothers you, may I remind
--          you that mov reg,[var]; add ??,reg is faster anyway, as 
--          long as you put any other instruction between them */
    mov edi,[block_offset]
    add esi,edi
    lea edi,[esp+40]    -- schedule
    mov ecx,16
@@: 
    lodsd
    bswap eax
    stosd
    sub ecx,1
    jnz @b

    -- perform message schedule

    mov ecx,48      -- for i=16 to 63
@@: 
    -- eax(alpha1) := (x ror 17) xor ( x ror 19) xor (x shr 10) where x==w[i-2]
    mov eax,[edi-8]     -- w[i-2]
    mov ebx,eax
    ror eax,17
    ror ebx,19
    xor eax,ebx
    mov ebx,[edi-8]     -- w[i-2]
    shr ebx,10
    xor eax,ebx

    add eax,[edi-28]    -- +w[i-7]

    -- ebx(alpha0) := (x ror 7) xor (x ror 18) xor (x shr 3) where x==w[i-15]
    mov ebx,[edi-60]    -- w[i-15]
    mov esi,ebx
    ror ebx,7
    ror esi,18
    xor ebx,esi
    mov esi,[edi-60]    -- w[i-15]
    shr esi,3
    xor ebx,esi

    add eax, ebx
    add eax, [edi-64]
    stosd               -- w[i] := alpha0 + w[i-16] + alpha1 + w[i-7]
    sub ecx,1
    jnz @b              -- end for (i=16 to 63)

    -- initialize eight working variables with hash value

    mov esi,[mem4]
    lea edi, [esp+8]    -- a
    shl esi,2
    mov ecx, 8
    rep movsd           -- (a..h:=h0..h7)

    -- main computation loop

    --; t1 = h + sigma1(e) + ch(e,f,g) + k(i) + w(i)
    --; t2 = sigma0(a) + maj(a,b,c)
    --; h = g
    --; g = f
    --; f = e
    --; e = d + t1
    --; d = c
    --; c = b
    --; b = a
    --; a = t1 + t2

    xor ecx, ecx        -- for i=0 to 63 [actually 0 to 252 by 4]
@@: 
    -- ebx(sigma1) := (e ror 6) xor (e ror 11) xor (e ror 25)
    mov ebx, [esp+24]   -- e
    mov esi, ebx
    ror ebx,  6
    ror esi, 11
    xor ebx, esi
    mov esi, [esp+24]   -- e
    ror esi, 25
    xor ebx, esi

    add ebx, [esp+36]   -- h

    -- eax(ch) := (e & f) xor (~ex & g)
    mov eax, [esp+24]   -- e
    mov esi, eax
    not esi
    and eax, [esp+28]   -- f
    and esi, [esp+32]   -- g
    xor eax, esi

    add eax, ebx
    mov esi,[sha_const4]
    add eax, dword[ecx+esi*4]   -- +k[i] 
    add eax, [esp+ecx+40]       -- +w[i]
    mov [esp+4], eax            -- t1 := h + sigma1 + ch + k[i] + w[i]

    -- eax(sigma0) := (a ror 2) xor (a ror 13) xor (a ror 22)
    mov eax, [esp+8]    -- a
    mov ebx, eax
    ror eax,  2
    ror ebx, 13
    xor eax, ebx
    mov ebx, [esp+8]    -- a
    ror ebx, 22
    xor eax, ebx

    -- ebx(maj) := (a & b) xor (a & c) xor (b & c)
    mov ebx, [esp+8]    -- a
    mov esi, ebx
    and ebx, [esp+12]   -- b
    and esi, [esp+16]   -- c
    xor ebx, esi
    mov esi, [esp+12]   -- b
    and esi, [esp+16]   -- c
    xor ebx, esi

    add eax, ebx
    mov [esp], eax      -- t2 := sigma0 + maj

    -- main variable shift

    std
    lea esi, [esp+32]   -- g
    lea edi, [esp+36]   -- h

    lodsd
    stosd               -- h:=g
    lodsd
    stosd               -- g:=f
    lodsd
    stosd               -- f:=e

    lodsd
    add eax, [esi-12]

    stosd               -- e:=d+t1
    lodsd
    stosd               -- d:=c
    lodsd
    stosd               -- c:=b
    lodsd
    stosd               -- b:=a
    lodsd

    add eax, [esi]
    stosd               -- a:=t1+t2

    add ecx, 4
    cmp ecx, 256
    jnz @b              -- end for (i=0 to 63) [actually 0 to 252 by 4]

    -- add working variables to hash value

    cld
    lea esi, [esp+8]    -- [a]
    mov edi, [mem4]
    shl edi,2

    xor ecx, ecx
@@: 
    lodsd
    add [edi+ecx], eax  -- h0..h7 += a..h
    add ecx, 4
    cmp ecx, 32
    jnz @b

--  add [block_offset],64   -- (not yet supported, as above)
    mov esi,[block_offset]
    add esi,64
    mov [block_offset],esi
    sub edx,1
    jnz :mainloop1

    -- discard temps (reset stack)
    add esp, 296
    xor ebx, ebx    -- important!
}

    string res = peek({mem4*4,32})
    free(mem4*4)
    return rev4(res)
--  return res
end function
 

