--
-- sha512.e
-- ========
--
-- Translation of https://programmer209.wordpress.com/tag/sha-512/ aka
-- https://programmer209.wordpress.com/2012/09/15/fasm-x86-secure-hash-algorithm/
--   (but just the sha-512 parts, demacroified, optimised, and commented)
--
-- Technical note:
--  This was not chosen for speed, but so that I could get a fasm 
--  version up and running to aid in debugging it (which of course 
--  is a largely matter of comparing //all// the interim results). 
--  That said, I doubt you could improve on this (32bit) very much... 
--  I cleaned it up and optimised it a bit during an internet outage,
--  and made the minimum possible tweaks (see nBlocks1024) for this 
--  32-bit code to run on 64-bit, but of course if you really wanted 
--  to, 64-bit could probably be made >2x as fast, maybe even >4x, 
--  and quite probably in ~1/8th of the number of source code lines!
--  (some alternatives to consider for that can be found in hmac.e)
--
-- Note this only supports messages that are a whole number of bytes.
--
without javascript_semantics

-- sha-512 uses 80 qwords, one for each round of main hashing routine
constant integer SHA_512_Kt = floor(allocate(80*8)/4)
    poke4(SHA_512_Kt*4,{0x428a2f98,0xd728ae22,0x71374491,0x23ef65cd,
                        0xb5c0fbcf,0xec4d3b2f,0xe9b5dba5,0x8189dbbc,
                        0x3956c25b,0xf348b538,0x59f111f1,0xb605d019,
                        0x923f82a4,0xaf194f9b,0xab1c5ed5,0xda6d8118,
                        0xd807aa98,0xa3030242,0x12835b01,0x45706fbe,
                        0x243185be,0x4ee4b28c,0x550c7dc3,0xd5ffb4e2,
                        0x72be5d74,0xf27b896f,0x80deb1fe,0x3b1696b1,
                        0x9bdc06a7,0x25c71235,0xc19bf174,0xcf692694,
                        0xe49b69c1,0x9ef14ad2,0xefbe4786,0x384f25e3,
                        0x0fc19dc6,0x8b8cd5b5,0x240ca1cc,0x77ac9c65,
                        0x2de92c6f,0x592b0275,0x4a7484aa,0x6ea6e483,
                        0x5cb0a9dc,0xbd41fbd4,0x76f988da,0x831153b5,
                        0x983e5152,0xee66dfab,0xa831c66d,0x2db43210,
                        0xb00327c8,0x98fb213f,0xbf597fc7,0xbeef0ee4,
                        0xc6e00bf3,0x3da88fc2,0xd5a79147,0x930aa725,
                        0x06ca6351,0xe003826f,0x14292967,0x0a0e6e70,
                        0x27b70a85,0x46d22ffc,0x2e1b2138,0x5c26c926,
                        0x4d2c6dfc,0x5ac42aed,0x53380d13,0x9d95b3df,
                        0x650a7354,0x8baf63de,0x766a0abb,0x3c77b2a8,
                        0x81c2c92e,0x47edaee6,0x92722c85,0x1482353b,
                        0xa2bfe8a1,0x4cf10364,0xa81a664b,0xbc423001,
                        0xc24b8b70,0xd0f89791,0xc76c51a3,0x0654be30,
                        0xd192e819,0xd6ef5218,0xd6990624,0x5565a910,
                        0xf40e3585,0x5771202a,0x106aa070,0x32bbd1b8,
                        0x19a4c116,0xb8d2d0c8,0x1e376c08,0x5141ab53,
                        0x2748774c,0xdf8eeb99,0x34b0bcb5,0xe19b48a8,
                        0x391c0cb3,0xc5c95a63,0x4ed8aa4a,0xe3418acb,
                        0x5b9cca4f,0x7763e373,0x682e6ff3,0xd6b2b8a3,
                        0x748f82ee,0x5defb2fc,0x78a5636f,0x43172f60,
                        0x84c87814,0xa1f0ab72,0x8cc70208,0x1a6439ec,
                        0x90befffa,0x23631e28,0xa4506ceb,0xde82bde9,
                        0xbef9a3f7,0xb2c67915,0xc67178f2,0xe372532b,
                        0xca273ece,0xea26619c,0xd186b8c7,0x21c0c207,
                        0xeada7dd6,0xcde0eb1e,0xf57d4f7f,0xee6ed178,
                        0x06f067aa,0x72176fba,0x0a637dc5,0xa2c898a6,
                        0x113f9804,0xbef90dae,0x1b710b35,0x131c471b,
                        0x28db77f5,0x23047d84,0x32caab7b,0x40c72493,
                        0x3c9ebe0a,0x15c9bebc,0x431d67c4,0x9c100d4c,
                        0x4cc5d4be,0xcb3e42b6,0x597f299c,0xfc657e2a,
                        0x5fcb6fab,0x3ad6faec,0x6c44198c,0x4a475817})

function pad128(integer v)
-- round v up to multiple of 128
    return floor((v+127)/128)*128
end function

constant integer schedule4 = allocate(80*8)/4

constant integer H_0_512 = allocate(16*4)/4 -- hash result
constant integer Temp_H  = allocate(16*4)/4 -- a,b,c,d,e,f,g,h, as qwords

global function sha512(string msg)
    integer BitLength = length(msg)*8,
            len = length(msg)+1,
            nBlocks1024

    -- add the '1' bit and space for size in bits, padded to multiple of 128
    msg &= #80&repeat('\0',pad128(len+16)-len)
    len = BitLength
    for i=length(msg) to 1 by -1 do
        msg[i] = and_bits(len,#FF)
        len = floor(len/#100)
        if len=0 then exit end if
    end for
-- (sug: calculate nBlocks1024 in hll.. why not poke H_0_512 too:)
--   [the resulting source file would then be ~80 lines shorter]
--/*
    poke4(H_0_512*4,{0x6a09e667,0xf3bcc908,0xbb67ae85,0x84caa73b,
                     0x3c6ef372,0xfe94f82b,0xa54ff53a,0x5f1d36f1,
                     0x510e527f,0xade682d1,0x9b05688c,0x2b3e6c1f,
                     0x1f83d9ab,0xfb41bd6b,0x5be0cd19,0x137e2179})
--*/

#ilASM{

        -- compute the number of 1024 bit blocks in the message

        mov edx,[BitLength]

        shr edx,10

        add edx,1

        [32]
        mov [nBlocks1024],edx
        [64]                                -- (as above, minor tweak 1 of 2)
        mov [nBlocks1024],rdx
        []

        -- get length of last block

        -- if greater than or equal to 896 bits, an extra padding block is needed

        mov edx,[BitLength]

        and edx,0x3ff

        cmp edx,896

        jl @f

            [32]
            add dword [nBlocks1024],1
            [64]                            -- (as above, minor tweak 2 of 2)
            add qword [nBlocks1024],1
            []

    @@:
--sug... (verify this on a few test cases before you jump...)
--mov eax,[nBlocks1024hll]
--mov ebx,[nBlocks1024]
--cmp eax,ebx
--je @f
--  int3
--@@:

        -- set/check the last 128 bits in the padded message to the bit length of the message

        mov edi,[msg]
        shl edi,2

        mov edx,[nBlocks1024]
        shl edx,7

        -- quick check that nBlocks1024*1024 == length(msg):
        [32]
        cmp edx,[edi-12]
        [64]
        cmp rdx,[rdi-24]
        []
        je @f
            int3
        @@:

        add edi,edx

        -- BitLength is a dword (32 bits)

        -- copy to/check the last 32 bits of the padded message

        sub edi,4

        mov eax,[BitLength]

        bswap eax

--      mov [edi],eax
        cmp [edi],eax
        je @f
            int3 
      @@:

        -- initialise H_0_512:

        mov edi,[H_0_512]
        shl edi,2

        mov [edi],   dword 0x6a09e667
        mov [edi+ 4],dword 0xf3bcc908
        mov [edi+ 8],dword 0xbb67ae85
        mov [edi+12],dword 0x84caa73b
        mov [edi+16],dword 0x3c6ef372
        mov [edi+20],dword 0xfe94f82b
        mov [edi+24],dword 0xa54ff53a
        mov [edi+28],dword 0x5f1d36f1
        mov [edi+32],dword 0x510e527f
        mov [edi+36],dword 0xade682d1
        mov [edi+40],dword 0x9b05688c
        mov [edi+44],dword 0x2b3e6c1f
        mov [edi+48],dword 0x1f83d9ab
        mov [edi+52],dword 0xfb41bd6b
        mov [edi+56],dword 0x5be0cd19
        mov [edi+60],dword 0x137e2179

-- (as per suggestion above, this is where ilASM would then start...)

        mov ecx,[nBlocks1024]

        mov esi,[msg]
        shl esi,2

  ::HASH_BLOCK

        mov edi,[schedule4]
        shl edi,2

        mov dx,128

      ::INIT_SCHED

        -- copy the 1024 bit block to the first 1024 bits of the message schedule

        mov eax,[esi]

        -- big endian to little endian

        bswap eax

        mov [edi],eax

        add esi,4
        add edi,4

        sub dx,4
        jnz :INIT_SCHED

        -- iterate the hash value

        push esi
        push ecx
        --
        --      locals: (beware of any call/push, of course!)
        --
        --        T1_U dd 0         -> [esp]
        --        T1_L dd 0         -> [esp+4]
        --
        --        T2_U dd 0         -> [esp+8]
        --        T2_L dd 0         -> [esp+12]
        --
        --        T3_U dd 0         -> [esp+16]
        --        T3_L dd 0         -> [esp+20]
        --
        sub esp,24


        -- prepare the message schedule

        -- the first 32 dwords of the message schedule
        -- already contain the 1024 bit message block
        -- {strangely also true of the first 128 bytes}

        mov esi,[schedule4]
        mov edi,[schedule4]
        shl esi,2
        shl edi,2

        add esi,128
        add edi,128

        xor ecx,ecx

        mov cx,16

  ::PREPARE

        -- W[t] = W[t-7] + W[t-16] + sigma_512_1(W[t-2]) + sigma_512_0(W[t-15])

        -- 64 bit words:

        -- lower 32 bits in EAX
        -- upper 32 bits in EDX

        -- W[t-7]

        mov eax,[esi-52]
        mov edx,[esi-56]

        -- W[t-16]

        add eax,[esi-124]
        adc edx,[esi-128]

        mov [edi+4],eax     -- W[t] = W[t-7] + W[t-16] (more to follow)
        mov [edi],edx

        -- x = W[t-2]
        -- sigma_512_1(x) = ROTR(x,19) xor ROTR(x,61) xor SHR(x,6)
        --  result returned in EAX and EDX, uses T3

        mov eax,[esi-12]    -- W[t-2]
        mov edx,[esi-16]

        -- SHR(x,6)

        shrd eax,edx,6
        shr edx,6

        mov [esp+20],eax    -- T3_L
        mov [esp+16],edx    -- T3_U

        -- ROTR(x,19)

        mov eax,[esi-12]    -- W[t-2]
        mov edx,[esi-16]

        mov ebx,eax

        shrd eax,edx,19
        shr edx,19

        shl ebx,32-19

        or edx,ebx

        -- xor

        xor [esp+20],eax    -- T3_L
        xor [esp+16],edx    -- T3_U

        -- ROTR(x,61) = ROTR(ROTR(x,32),29)
        --  where ROTR(x,32)==load eax:edx as edx:eax

        mov edx,[esi-12]    -- rot32(W[t-2])
        mov eax,[esi-16]

--      ROT_RIGHT 29

        mov ebx,eax

        shrd eax,edx,29
        shr edx,29

        shl ebx,32-29

        or edx,ebx

        -- xor

        xor eax,[esp+20]    -- T3_L
        xor edx,[esp+16]    -- T3_U

        -- add to W[t]

        add [edi+4],eax     -- W[t] += sigma_512_1(W[t-2])
        adc [edi],edx

        -- x = W[t-15]
        -- sigma_512_0(x) = ROTR(x,1) xor ROTR(x,8) xor SHR(x,7)
        --  result left in EAX and EDX, uses T3

        mov eax,[esi-116]   -- W[t-15]
        mov edx,[esi-120]

        -- SHR(x,7)

        shrd eax,edx,7
        shr edx,7

        mov [esp+20],eax    -- T3_L
        mov [esp+16],edx    -- T3_U

        -- ROTR(x,1)

        mov eax,[esi-116]   -- W[t-15]
        mov edx,[esi-120]

        mov ebx,eax

        shrd eax,edx,1
        shr edx,1

        shl ebx,32-1

        or edx,ebx

        -- xor

        xor [esp+20],eax    -- T3_L
        xor [esp+16],edx    -- T3_U

        -- ROTR(x,8)

        mov eax,[esi-116]   -- W[t-15]
        mov edx,[esi-120]

        mov ebx,eax

        shrd eax,edx,8
        shr edx,8

        shl ebx,32-8

        or edx,ebx

        -- xor

        xor eax,[esp+20]    -- T3_L
        xor edx,[esp+16]    -- T3_U

        -- add to W[t]

        add [edi+4],eax     -- W[t] += sigma_512_0(W[t-15])
        adc [edi],edx

        add esi,8
        add edi,8

        add cx,1

        cmp cx,80

        jl :PREPARE

        -- initialise a,b,c,d,e,f,g,h

        mov esi,[H_0_512]
        mov edi,[Temp_H]
        shl esi,2
        shl edi,2

        mov ecx,16
        rep movsd

        -- compute the hash

        xor ecx,ecx

        mov esi,[Temp_H]
        mov edi,[schedule4]
        shl esi,2
        shl edi,2

  ::HASH

        -- 64 bit words:

        -- lower 32 bits in EAX
        -- upper 32 bits in EDX

        -- a = esi + 0
        -- b = esi + 8
        -- c = esi + 16
        -- d = esi + 24
        -- e = esi + 32
        -- f = esi + 40
        -- g = esi + 48
        -- h = esi + 56

        -- T1 = h + SIGMA_512_1(e) + Ch(e,f,g) + K[t] + W[t]

        -- T1 = h

        mov eax,[esi+60]
        mov edx,[esi+56]

        mov [esp+4],eax     -- T1_L
        mov [esp],edx       -- T1_U

        -- T1 += W[t]

        mov ebx,ecx
        shl ebx,3

        mov eax,[edi+ebx+4]
        mov edx,[edi+ebx]

        add [esp+4],eax     -- T1_L
        adc [esp],edx       -- T1_U

        -- T1 += Ch(e,f,g)

        -- Ch(e,f,g) = (e and f) xor (not(e) and g)
        --  result left in EAX and EDX, uses T3

        -- e = esi + 32
        -- f = esi + 40
        -- g = esi + 48

        -- e and f

        mov eax,[esi+36]    -- e
        mov edx,[esi+32]

        and eax,[esi+44]    -- f
        and edx,[esi+40]

        mov [esp+20],eax    -- T3_L
        mov [esp+16],edx    -- T3_U

        -- not(e)

        mov eax,[esi+36]    -- e
        mov edx,[esi+32]

        not eax
        not edx

        -- not(e) and g

        and eax,[esi+52]    -- g
        and edx,[esi+48]

        -- xor

        xor eax,[esp+20]    -- T3_L
        xor edx,[esp+16]    -- T3_U

        add [esp+4],eax     -- T1_L
        adc [esp],edx       -- T1_U

        -- T1 += K[t]

        push esi

        mov esi,[SHA_512_Kt]
        shl esi,2

        mov edx,ecx         -- (t is in ecx)

        shl edx,3

        mov eax,[esi+edx+4] -- K[t]
        mov edx,[esi+edx]

        pop esi

        add [esp+4],eax     -- T1_L
        adc [esp],edx       -- T1_U

        -- T1 += SIGMA_512_1(e)

        -- SIGMA_512_1(e) = ROTR(e,14) xor ROTR(e,18) xor ROTR(e,41)
        -- (result left in EAX and EDX, uses T3)

        -- ROTR(e,14)

        mov eax,[esi+36]    -- e
        mov edx,[esi+32]

        mov ebx,eax

        shrd eax,edx,14
        shr edx,14

        shl ebx,32-14

        or edx,ebx

        mov [esp+20],eax    -- T3_L
        mov [esp+16],edx    -- T3_U

        -- ROTR(e,18)

        mov eax,[esi+36]    -- e
        mov edx,[esi+32]

        mov ebx,eax

        shrd eax,edx,18
        shr edx,18

        shl ebx,32-18

        or edx,ebx

        -- xor

        xor [esp+20],eax    -- T3_L
        xor [esp+16],edx    -- T3_U

        -- ROTR(e,41) = ROTR(ROTR(e,32),9)
        --  where ROTR(e,32)==load eax:edx as edx:eax

        mov edx,[esi+36]    -- rot32(e)
        mov eax,[esi+32]

--      ROT_RIGHT 9

        mov ebx,eax

        shrd eax,edx,9
        shr edx,9

        shl ebx,32-9

        or edx,ebx

        -- xor

        xor eax,[esp+20]    -- T3_L
        xor edx,[esp+16]    -- T3_U


        add [esp+4],eax     -- T1_L
        adc [esp],edx       -- T1_U

        -- T2 = SIGMA_512_0(a) + Maj(a,b,c)

        -- T2 = Maj(a,b,c)

        -- Maj(a,b,c) = (a and b) xor (a and c) xor (b and c)
        --  (result left in EAX and EDX, uses T2)

        -- a = esi + 0
        -- b = esi + 8
        -- c = esi + 16

        -- a and b

        mov eax,[esi+4]     -- a
        mov edx,[esi]

        and eax,[esi+12]    -- b
        and edx,[esi+8]

        mov [esp+12],eax    -- T2_L
        mov [esp+8],edx     -- T2_U

        -- a and c

        mov eax,[esi+4]     -- a
        mov edx,[esi]

        and eax,[esi+20]    -- c
        and edx,[esi+16]

        -- xor

        xor [esp+12],eax    -- T2_L
        xor [esp+8],edx     -- T2_U

        -- b and c

        mov eax,[esi+12]    -- b
        mov edx,[esi+8]

        and eax,[esi+20]    -- c
        and edx,[esi+16]

        -- xor

        xor eax,[esp+12]    -- T2_L
        xor edx,[esp+8]     -- T2_U

        mov [esp+12],eax    -- T2_L
        mov [esp+8],edx     -- T2_U

        -- T2 += SIGMA_512_0(a)

        -- SIGMA_512_0(a) = ROTR(a,28) xor ROTR(a,34) xor ROTR(a,39)
        --  result left in EAX and EDX (uses T3)

        -- a = esi + 0

        -- ROTR(a,28)

        mov eax,[esi+4]     -- a
        mov edx,[esi]

        mov ebx,eax

        shrd eax,edx,28
        shr edx,28

        shl ebx,32-28

        or edx,ebx

        mov [esp+20],eax    -- T3_L
        mov [esp+16],edx    -- T3_U

        -- ROTR(a,34) = ROTR(ROTR(a,32),2)
        --  where ROTR(a,32)==load eax:edx as edx:eax

        mov edx,[esi+4]     -- rot32(a)
        mov eax,[esi]

--      ROT_RIGHT 2

        mov ebx,eax

        shrd eax,edx,2
        shr edx,2

        shl ebx,32-2

        or edx,ebx

        -- xor

        xor [esp+20],eax    -- T3_L
        xor [esp+16],edx    -- T3_U

        -- ROTR(a,39) = ROTR(ROTR(a,32),7)
        --  where ROTR(a,32)==load eax:edx as edx:eax

        mov edx,[esi+4]     -- rot32(a)
        mov eax,[esi]

--      ROT_RIGHT 7

        mov ebx,eax

        shrd eax,edx,7
        shr edx,7

        shl ebx,32-7

        or edx,ebx

        -- xor

        xor eax,[esp+20]    -- T3_L
        xor edx,[esp+16]    -- T3_U

        add [esp+12],eax    -- T2_L
        adc [esp+8],edx     -- T2_U

        -- h = g

        mov eax,[esi+52]
        mov edx,[esi+48]

        mov [esi+60],eax
        mov [esi+56],edx

        -- g = f

        mov eax,[esi+44]
        mov edx,[esi+40]

        mov [esi+52],eax
        mov [esi+48],edx

        -- f = e

        mov eax,[esi+36]
        mov edx,[esi+32]

        mov [esi+44],eax
        mov [esi+40],edx

        -- e = d + T1

        mov eax,[esi+28]
        mov edx,[esi+24]

        add eax,[esp+4]     -- T1_L
        adc edx,[esp]       -- T1_U

        mov [esi+36],eax
        mov [esi+32],edx

        -- d = c

        mov eax,[esi+20]
        mov edx,[esi+16]

        mov [esi+28],eax
        mov [esi+24],edx

        -- c = b

        mov eax,[esi+12]
        mov edx,[esi+8]

        mov [esi+20],eax
        mov [esi+16],edx

        -- b = a

        mov eax,[esi+4]
        mov edx,[esi]

        mov [esi+12],eax
        mov [esi+8],edx

        -- a = T1 + T2

        mov eax,[esp+4]     -- T1_L
        mov edx,[esp]       -- T1_U

        add eax,[esp+12]    -- T2_L
        adc edx,[esp+8]     -- T2_U

        mov [esi+4],eax
        mov [esi],edx

        add ecx,1
        cmp ecx,80

        jl :HASH

        -- add a,b,c,d,e,f,g,h to the hash

        mov esi,[Temp_H]
        mov edi,[H_0_512]
        shl esi,2
        shl edi,2

        mov ecx,8

    @@:

            mov eax,[esi+4]
            mov edx,[esi]

            add [edi+4],eax
            adc [edi],edx

            add esi,8
            add edi,8

            sub ecx, 1
        jnz @b

        add esp,24
        pop ecx
        pop esi

        sub ecx,1
        jnz :HASH_BLOCK

        xor ebx, ebx    -- important!
    }
    string res = peek({H_0_512*4,64})
    return res
end function

