--
--  builtins\inflate.e  raw DEFLATE (RFC-1951) decompressor
--  Input  : binary string  (each char = one byte)
--  Output : decompressed binary string
--
--  Benchmarking notes:
--  I've seen this do 35MB/s for worst case/tiny file, which is
--  already miles better than anything I need it for. There are
--  several ways it could be sped up, inline assembly and maybe
--  far more significantly a pre-allocated dst, but my decision
--  is: no more work until/unless someone moans about it.
--
string input
integer bitBuf,         -- bits waiting to be read
        bitLen,         -- how many bits are valid in bitBuf
          indx,         -- 1-based index into input string
           lin          -- length(input)

local procedure refill(integer need)
    -- ensure at least `need` bits in the buffer
    while bitLen<need and indx<=lin do
        bitBuf += input[indx] << bitLen
        bitLen += 8
        indx += 1
    end while
end procedure

local function readBits(integer n)
    -- read `n` bits (LSB-first) and remove from buffer
    if n = 0 then return 0 end if
    refill(n)
    integer p2 = power(2,n),
           res = bitBuf && (p2-1)
    bitBuf = floor(bitBuf/p2)
    bitLen -= n
    return res
end function

---------- Huffman tree builder ----------
local function buildTree(sequence lens) --> lut sequence
    sequence countsMem = repeat(0,16),
            offsetsMem = repeat(0,16),
            sortedSyms = repeat(0,320)
    integer maxBits = 0, off = 1
    for l in lens do
        if l>0 then
            countsMem[l] += 1
            if l>maxBits then maxBits = l end if
        end if
    end for
    -- offsets
    for i=1 to maxBits do
        offsetsMem[i] = off
        off += countsMem[i]
    end for
    -- sort symbols by length
    for i,l in lens do
        if l>0 then
            sortedSyms[offsetsMem[l]] = i
            offsetsMem[l] += 1
        end if
    end for
    -- build the table
    integer rev = 0, sortedIdx = 1,
            limit = 1 << maxBits
    sequence lut = repeat(0,limit)
    for treeLen=1 to maxBits do
        integer step = 1 << treeLen,
                count = countsMem[treeLen]
        for i=1 to count do
            integer sym = sortedSyms[sortedIdx],
                   esym = (treeLen<<9)+sym-1
            sortedIdx += 1
            for j=rev+1 to limit by step do
                lut[j] = esym
            end for
            -- increment rev in bit-reversed order
            integer bit = 1 << (treeLen-1)
            while rev && bit do
                rev = xor_bits(rev,bit)
                bit = floor(bit/2)
            end while
            rev = xor_bits(rev,bit)
        end for
    end for
    return lut
end function

local function decodeSymbol(sequence lut) --> integer
    -- decode a symbol from a lut --
    refill(16)  -- we never need more than 15 bits to index
    -- lut.length is a power of two
    integer mask = length(lut)-1,
             sym = lut[(bitBuf && mask)+1],
             len = sym >> 9
    if len=0 or len>bitLen then
        throw("invalid huffman code / malformed stream")
    end if
    bitBuf = bitBuf >> len
    bitLen -= len
    sym = and_bits(sym,0x1FF)   -- low 9 bits = symbol value
    return sym
end function

-- constants from RFC-1951 -- (except odr has 1-based indexes)
local constant odr = {17,18,19,1,9,8,10,7,11,6,12,5,13,4,14,3,15,2,16},
               l0  = {3,4,5,6,7,8,9,10,11,13,15,17,19,23,27,31,35,
                        43,51,59,67,83,99,115,131,163,195,227,258},
               ex0 = {0,0,0,0,0,0,0,0,1,1,1,1,2,2,2,2,3,3,3,3,4,4,4,4,5,5,5,5,0},
               d1  = {1,2,3,4,5,7,9,13,17,25,33,49,65,97,129,193,257,385,513,
                    769,1025,1537,2049,3073,4097,6145,8193,12289,16385,24577},
               ex1 = {0,0,0,0,1,1,2,2,3,3,4,4,5,5,6,6,7,7,8,8,9,9,10,10,11,11,12,12,13,13}

-- fixed huffman trees (built just the once)
sequence lxTree,    -- 512-entry literal/length tree
         dxTree     -- 32-entry distance tree

--DEV
global integer fbl

global function inflate(string src, integer s=1, e=-1)
    integer ls = length(src)
    if e=-1 then e = ls end if
    if e>ls or s>e or s<1 then throw("src/s/e error") end if
    {input,bitBuf,bitLen,indx,lin} = {src,0,0,s,e}

    string out = ""

    -- main decompression loop --
    do
        -- block header: BFINAL (bit 0), BTYPE (bits 1-2)
        integer bits = readBits(3),
              bfinal = bits && 1,
               btype = bits >> 1

        if btype=0 then
            -- stored (uncompressed) block --
            -- Determine how many whole bytes are still trapped in the
            --  bit buffer and rewind the raw input pointer as needed
            integer unusedBytes = bitLen >> 3
            indx -= unusedBytes
            bitBuf = 0; bitLen = 0  -- byte-align
            -- need at least 4 bytes for LEN+NLEN (else corrupt input)
            indx += 4
            if indx>lin then throw("truncated LEN") end if
            integer len = input[indx-4]+(input[indx-3]<<8),
                   nlen = input[indx-2]+(input[indx-1]<<8),
                   endx = indx+len-1
            if xor_bits(len,nlen)!=#FFFF then throw("bad NLEN") end if
            if endx>lin then throw("truncated data") end if
            -- copy the raw data
            for i=indx to endx do
                out &= input[i]
            end for
            indx += len
--fbl = 0
        else
            -- compressed block --
            sequence lTree, dTree
            if btype = 1 then
                -- Fixed Huffman --
                if not object(lxTree) then -- (ie still unassigned)
                    -- literal/length lengths
                    sequence llls = repeat(8,288)
                    llls[145..256] = 9
                    llls[257..280] = 7
                    lxTree = buildTree(llls)
                    -- distance lengths
                    dxTree = buildTree(repeat(5,32))
                end if
                lTree = lxTree
                dTree = dxTree
            elsif btype = 2 then
                -- Dynamic Huffman --
                integer hlit = readBits(5)+257,
                       hdist = readBits(5)+1,
                       hclen = readBits(4)+4
                -- code-length code lengths (19 values, permuted by odr)
                sequence clens = repeat(0,19)
                for d in odr to hclen do
                    clens[d] = readBits(3)
                end for
                sequence clTree = buildTree(clens), -- code-length tree
                        alldcls = repeat(0,320)
                -- decode the actual literal/length and distance code lengths
                integer idx = 1
                while idx<=hlit+hdist do
                    integer sym = decodeSymbol(clTree)
                    if sym<16 then
                        alldcls[idx] = sym
                        idx += 1
                    else
                        sym -= 15
                        if sym>3 then throw("invalid code length") end if
                        integer rep = iff(sym=3?11:3)+readBits({2,3,7}[sym]), 
                                val = iff(sym=1?alldcls[idx-1]:0)
                        alldcls[idx..idx+rep-1] = val
                        idx += rep
                    end if
                end while
                -- build the final trees
                lTree = buildTree(alldcls[1..hlit])
                dTree = buildTree(alldcls[hlit+1..hlit+hdist])
            else
                throw("unknown BTYPE") --> treat as corrupt
            end if

            -- ----- decode Huffman codes -----
            do
                -- need at least one bit to look for the end-of-block marker
                if bitLen=0 and indx>lin then throw("missing EOB") end if
                integer sym = decodeSymbol(lTree), ldx = sym-256
                if ldx<0 then  -- literal
                    out &= sym
                elsif ldx!=0 then   -- length/distance pair
                    integer matchLen = l0[ldx]+readBits(ex0[ldx]),
                                 dsx = decodeSymbol(dTree)+1,
                           matchDist = d1[dsx]+readBits(ex1[dsx])
                    -- copy matchLen bytes from $-matchDist,
                    -- one at a time since they may overlap
                    s = length(out)-matchDist+1
                    e = s+matchLen-1
                    if s<1 then throw("invalid match distance") end if
                    for i=s to e do
                        out &= out[i]
                    end for
                end if
            until ldx=0 // aka EOB marker found
        end if
    until bfinal
fbl = bitLen
    if indx!=lin+1 or bitLen>7 then
        -- more bytes to process??
        throw("incomplete decode")
    end if 

    return out
end function

