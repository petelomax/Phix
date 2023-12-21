--
-- builtins\hash.e
--
--  Based on the work of Jiri Babor, as modified by Aku,
--  and further by me, to support *any* objects as key.
--
--  auto-include for the hash(key) case, however (at the moment),
--  only the HSIEH30 algo value is used/available via psym.e for 
--  the hash(key,algo) case - others may be added if/when needed, 
--  but for now they are not supported and would require these
--  to be uncommented, plus an explicit include (for no gain):
--
--global constant
--- HSIEH30 = -6,   -- (already in psym.e, just this one)
--  HSIEH32 = -5,
--  ADLER32 = -4,
--  FLETCHER32 = -3,
--  MD5 = -2,
--  SHA256 = -1
--
--  Note this does /not/ yield the same results as Euphoria.
--  Also note this is /not/ thread-safe (due to hidden refcount 
--  updates on hash_t and hash_t_rol_1), and this is provided 
--  only for compatibility with Euphoria, and was not written 
--  for the best possible performance - I would expect to get
--  better performance using standard dictionaries instead.
--  (see builtins\dict.e, or better yet the manual).
--
sequence hash_t
atom hash_t_rol_1
integer hash_init = false

function rol(atom x)
    #ilASM{
        [32]
            mov eax,[x]
            call :%pLoadMint    -- eax:=(int32)eax, edx:=hi_dword
            rol eax,1
            lea edi,[x]
            call :%pStoreMint   -- [edi]:=eax, as float if rqd
        [64]
            mov rax,[x]
            call :%pLoadMint    -- rax:=(int64)rax
            rol eax,1           -- (32 bit on purpose)
            lea rdi,[x]
            call :%pStoreMint   -- [rdi]:=rax, as float if rqd
        []
          }
    return x
end function

procedure init_hash()
    hash_init = true
    --
    -- create an array of 256 32-bit random integer such that any
    -- bit column contains exactly 128 zeros and 128 ones
    --
    hash_t = repeat(0,128) & repeat(1,128)
    sequence hash_s = repeat(0,32)
    set_rand(24341)
    for i=1 to 32 do
        hash_s[i] = shuffle(hash_t)
    end for

    -- exchange rows and columns : turn sequence s 90 degrees clockwise
    hash_t = repeat(repeat(0,32),256) -- init output sequence
    for r=1 to 32 do
        for c=1 to 256 do
            hash_t[c][32-r+1] = hash_s[r][c]
        end for
    end for

    -- turn bit sequences into 32-bit integers
    for i=1 to 256 do
        hash_t[i] = bits_to_int(hash_t[i])
    end for

    hash_t_rol_1 = rol(hash_t[1])
end procedure

function hash_hash(atom h, atom key)
    sequence bytes
    if integer(key) then
        if key<0 then
            key += #1_0000_0000
        end if
        bytes = int_to_bytes(key)
    else
        bytes = atom_to_float64(key)
    end if
    for i=1 to length(bytes) do
        h = xor_bitsu(rol(h), hash_t[bytes[i]+1])
    end for
    return h
end function

global function hash(object key, atom algo=0)
    atom h -- result, 0..4294967295 (a 32-bit uint)
    if not hash_init then init_hash() end if
    if integer(key) then
        if key>=0 and key<=255 then
            h = xor_bitsu(hash_t_rol_1, hash_t[key+1])
        else
            h = hash_hash(hash_t[4],key)
        end if
    elsif atom(key) then
        h = hash_hash(hash_t[8],key)
    else
        h = hash_t[and_bitsu(length(key), #FF)+1]
        for i=1 to length(key) do
            h = hash_hash(h,hash(key[i]))
        end for
    end if
    if algo=HSIEH30 then
--      return (0x3FFFFFFF & (i32 + ((0xC0000000 & i32) >> 30)));
--DEV... (h unassigned!)
--      h = and_bits(#3FFF_FFFF,h+floor(and_bitsu(#C000_0000,h)/0x4000_0000))
        h += floor(and_bitsu(#C000_0000,h)/0x4000_0000)
        h = and_bits(#3FFF_FFFF,h)
    end if
    return h
end function


