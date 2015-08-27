--
-- pMemChk.e
-- =========
--
--  Not technically part of the VM, just deeply tied to it...
--  Perform a detailed post-interpret analysis of the temp stack, if there
--  is anything left of it.
--
--  This checks for two things: memory leaks, and memory corruptions.
--  Obviously the latter is serious whereas the former is a bit "so what?",
--  unless it is sufficient to have some significant impact on performance.
--
--  *** NOTE *** This code is /NOT/ meant to be "robust".
--  If something else corrupts memory and/or we end up with a rouge pointer 
--  and this goes bang, you really are better off just fixing whatever went
--  wrong first, rather than improving this, for something that ought never 
--  to have happened in the first place, and most likely never happen again.
--  In fact, if anything, this program SHOULD be deliberately fragile so as 
--  to expose unusual problems, that is rather than quietly help hide them.
--  Obviously if tweaking this helps track something down, that's different.
--
--  *** NOTE2 *** This code is /NOT/ meant to be "accurate".
--  For instance it may report a "** leak **" that is actually freed memory,
--  with the real culprit hiding somewhere earlier/later. Granted, I really
--  ought to be able to fix that one, but, y'know. Also, note the following
--  situation: |<-48 bytes free->|<24leak>|<24free>|.. Both the free blocks
--  are going to "pair" with the leak in the middle, and therefore reported
--  gets it twice, yoda. Again, probably easily fixable, but, y'know...
--
-- The report (C:\Program Files (x86)\Phix\memchk.txt) should contain:
--  * The global pool (pTCB)
--  * A list of superblocks (pSBH)
--  * The thread control block(s) (pTCB)
--  * free/nofl chains, which indicate any memory leaks
--
-- I hope it is clear that if we have a 12K superblock, and a single memory
-- leak of a 48 byte block (see the "Revised Sizes" tables in pHeap.e) then
-- we expect an error on pFree[2] followed by "pairs XXX OK" on pFree[3..9].
--
-- Also, some of the entries may be "missing": if we have 4 memory leaks of
-- 48 bytes, we might only get a single pFree of 192 bytes pairing with the
-- first, with three "hidden" nestled against it. If you resolve the first,
-- the others should appear on subsequent runs.

integer silentrun = 1   -- 1: create memchk.txt, nowt on screen
                        -- 0: update screen, leave memchk.txt be

constant verbose = 01

procedure print2(integer fn, string fmt, sequence args={})
    if length(args) then
        fmt = sprintf(fmt,args)
    end if
    if silentrun then
        puts(fn,fmt)
    else
        puts(1,fmt)
    end if
end procedure

integer errorcount = 0

function okdesc(integer ok)
sequence oknot = {"OK","*** NOT OK ***"}
-- basically this is just a simple mapping of true(1)|false(0) to 1|2:
    if not ok then errorcount += 1 end if
    return oknot[2-ok]
end function

constant littleendian = 0   -- 1 = show 257 as 01010000, else as 00000101 

procedure hex_dump(integer fn, atom addr, integer size)
string hex, hex4, text, text4
integer addrMax
integer byte
    if size<0 then ?9/0 end if
    while size do
        hex = ""
        text = ""
        if size<16 then
            addrMax = addr+size-1
        else
            addrMax = addr+16
            addrMax -= and_bits(addrMax,#0F)+1
        end if
        if littleendian then
            for a=addr to addrMax do
                byte = peek(a)
                hex &= sprintf("%02x",byte)
                if remainder(a+5,4)=0 then
                    hex &= ' '
                end if
                if byte<' ' or byte>#7E then byte = '.' end if
                text &= byte
            end for
        else
            hex4 = ""
            text4 = ""
            for a=addr to addrMax do
                byte = peek(a)
                hex4 = sprintf("%02x",byte)&hex4
                if byte<' ' or byte>#7E then byte = '.' end if
                text4 = byte&text4
                if remainder(a+5,4)=0 then
                    hex &= hex4&' '
                    text &= text4
                    hex4 = ""
                    text4 = ""
                end if
            end for
        end if
        print2(fn,"%08x: %s %s\n",{addr,hex,text})
        size -= (addrMax-addr+1)
        addr = addrMax+1
    end while
end procedure

function hex_dump40(atom addr)
string hex, hex4
integer addrMax
integer byte
    hex = ""
    addrMax = addr+40-1
    if littleendian then
        for a=addr to addrMax do
            byte = peek(a)
            hex &= sprintf("%02x",byte)
            if remainder(a+5,4)=0 then
                hex &= ' '
            end if
        end for
    else
        hex4 = ""
        for a=addr to addrMax do
            byte = peek(a)
            hex4 = sprintf("%02x",byte)&hex4
            if remainder(a+5,4)=0 then
                hex &= hex4&' '
                hex4 = ""
            end if
        end for
    end if
    return hex
end function

function getline(atom era)
integer pLink, N
atom addr, a2
integer fno, pathno
string path, file, rtn
--sequence line
sequence symtabN
object linetab
integer lineno, lastline, linenxt, lti
atom returnoffset
object ttidx
    pLink = T_maintls
    N = T_maintls
    addr = symtab[N][S_il]
    while pLink do
        a2 = symtab[pLink][S_il]
        if a2<era and a2>addr then
            addr = a2
            N = pLink
        end if
        pLink = symtab[pLink][S_Slink]
    end while
    symtabN = symtab[N]
    fno = symtabN[S_FPno]
    {pathno,file} = filenames[fno]
    path = filepaths[pathno]
    linetab = symtabN[S_ltab]
    if sequence(linetab) then
        if string(linetab) then ?9/0 end if     -- compiled??
--  line = ppf(symtabN)
            lineno = symtabN[S_1stl]    -- line no of "procedure"/"function"/"type" keyword
            lastline = linetab[$]

--          returnoffset = era-symtabN[S_il]
            returnoffset = era-addr
            --
            -- Convert the offset to a line number.
            -- A raw line table, as built in pilx86.e, is {skip|offset} where
            --  skip is <0, meaning -n lines emitted no code, and
            --  offset is >=0, a start offset of the next code-emitting line.
            --  There is also a dummy max entry added at the end of ilxlate().
            --  A line table should never have two (or more) skips in a row.
            -- When interpreting, we can just use the raw table directly: skip down the
            --  table until the offset is (b)reached, though we only know that when we 
            --  hit the next entry, as detailed in the following.
            --  Example1: a raw linetab of {-2,0,24,36} means offsets 0..23 are S_1st+2,
            --            24..35 are S_1st+3, and <0 or >=36 are out-of-bounds (so leave 
            --            lineno as -1). We only know that we should have stopped for an 
            --            offset of 17 when we hit the 24, and the lineno we want is that 
            --            before the +1 triggered by the 0 (or whatever line adjustment 
            --            we made on [2] when we decide to stop on [3]).
            --  Example2: for a linetab of {-14,#14,-3,#47...} and offset of #22, we only 
            --            know that #14 (S_1st+14) is the right entry when we hit the #47
            --            (S_1st+18), so there is a +1 and -(-3) that we must ignore.
            --            Note that while an exception at offset #47 means S_1st+18, a 
            --            return address of #47 would be the last call made by S_1st+14;
            --            hence add -1 when using a return address to report an error.
            -- When compiled, the linetab is packed: the offsets are converted to deltas
            --  (so most will be <=127) with #81,#80 used as lead-ins for word,dword to
            --  hold values that will not fit in a byte, and lastly stored as a "string"
            --  of binary (#00..#FF) bytes, making it just over 25% of the size, on 32
            --  bit systems, and just over 12.5% on 64 bit systems (ie worth having).
            --  A fairly straightforward decode of the next raw value (into lti) is
            --  followed by the same logic as above, except that when we start with a
            --  raw table we can test lastline directly, but not when unpacking it.
            -- Lastly note that [era] is incredibly fiddly to set, especially for
            --  low-level routines isolated several calls deep from user code. If
            --  the following yields -1, first suspect [era] rather than this code,
            --  except for e30ume, in which case suspect that "Address Mapping" in
            --  :!fehDiag (below) is missing a test/label.
            --
--          if string(linetab) then -- compiled (deltas packed to string)
--              lineno = -1
--              if returnoffset>=0 then
--                  thisline = sr[S_1stl]
--                  linenxt = thisline
--                  skip = 0
--                  base = 0
--                  for i=1 to length(linetab) do
--                      if skip then
--                          skip -= 1
--                      else
--                          lti = linetab[i]
--                          if lti>#7F then
--                              if lti>#81 then
--                                  lti = lti-#100
--                              elsif lti=#81 then
--                                  lti = linetab[i+1]*#100+linetab[i+2]
--                                  if lti>#7FFF then
--                                      lti -= #10000   -- (skip < -128)
--                                  end if
--                                  skip = 2
--                              elsif lti=#80 then
--                                  lti = linetab[i+1]*#1000000+linetab[i+2]*#10000+linetab[i+3]*#100+linetab[i+4]
--                                  if lti>#7FFFFFFF then
--                                      lti -= #100000000 -- (skip < -32,768?? [very rare, if ever])
--                                  end if
--                                  skip = 4
--                              else
--                                  ?9/0    -- (sanity check, should never happen)
--                              end if
--                          end if
--                          if lti<0 then       -- -n lines emitted no code
--                              linenxt -= lti
--                          else                -- start offset of next line
--                              tmp = base
--                              base += lti
--                              lti = tmp
----                                if returnoffset<=lti then exit end if   -- all done
--                              if returnoffset<lti then
--                                  lineno = thisline
--                                  exit
--                              end if  -- all done
--                              thisline = linenxt
--                              linenxt += 1
--                          end if
--                      end if
--                  end for
--              end if
--          else -- interpreted (raw linetab, a dword-sequence, not converted to deltas/packed)
--              lineno = sr[S_1stl]
--              lastline = linetab[$]
                if returnoffset<0 or returnoffset>=lastline then
                    lineno = -1
                else
                    linenxt = lineno
                    for i=1 to length(linetab) do
                        lti = linetab[i]
                        if lti<0 then       -- -n lines emitted no code
                            linenxt -= lti
                        else                -- start offset of next line
--                          if returnoffset<=lti then exit end if   -- all done
                            if returnoffset<lti then exit end if    -- all done
                            lineno = linenxt
                            linenxt += 1
                        end if
                    end for
                end if
--          end if
    else -- (no linetab)
        lineno = -1
    end if
    ttidx = symtabN[S_Name]
    if atom(ttidx) then
        if ttidx=-1 then
            rtn = sprintf("[%08x]",{addr})
        else
            rtn = sprintf("[%s, %08x]",{getname(ttidx,-2),addr})
        end if
    else
        rtn = sprintf("[%s, %08x]",{ttidx,addr})
    end if
    return {era,path,file,lineno,rtn}
end function


-- Entry Padding
-- =============
--  DWORD   era         -- [-8] effective return address, for diagnostics/leak checking
--  DWORD   pRoot       -- [-4] pointer to the SuperBlock Header[+1]
--                      -- 0b01 (aka bit 0): block is free
--                      -- if xor(left,right) not 0|1 then memory corruption has occurred.
--  (DWORD  nSize)      -- [+0] size (permanent for allocate()d blocks)
--  (DWORD  pNext)      -- [+4] (on free items only), next freelist link
--  (DWORD  pPrev)      -- [+8] (on owned free items only), previous freelist link

-- Entry Padding (64-bit)
-- ======================
--  QWORD   era         -- [-16] effective return address, for diagnostics/leak checking
--  QWORD   pRoot       -- [-8] pointer to the SuperBlock Header[+1]
--                      -- 0b01 (aka bit 0): block is free
--                      -- if xor(left,right) not 0|1 then memory corruption has occurred.
--  (QWORD  nSize)      -- [+0] size (permanent for allocate()d blocks)
--  (QWORD  pNext)      -- [+8] (on free items only), next freelist link
--  (QWORD  pPrev)      -- [+16] (on owned free items only), previous freelist link

atom ePrev = 0

--/*
symtab[21]:{T_maintls,S_Proc,1,(K_wdb+K_ran),0,626,P,0,0,0,#00303C50}
symtab[626]:{Abort,S_Proc,1,(S_used+K_used+K_wdb),0,631,{80'P',8},627,1,2,#003042C1}
symtab[631]:{showtype,S_Proc,1,(S_used+S_set+K_used+K_wdb+K_gbl),0,639,{80'P',15,1},632,2,2,#0030430D}
symtab[639]:{F,S_Func,1,(S_used+K_used+K_wdb),0,0,{70'F',15},640,1,1,#00304302}

pFree[ 2] = 002A71D4, pRoot = 002A7188[+1] OK, nSize = 48 OK, pNext = 00000000, pairs 002A71A4 *** NOT OK ***
 ** leak ** 002A71A4, pRoot = 002A7188, hex: 00000000 28000000 04000000 01000000 00000080 01000000 07DD0940 4B550A40 
                      era = 0029CA8E: C:\Program Files (x86)\Phix\test\t03showt.exw:NNNN [procedure/function/type XXX,]
pFree[ 3] = 002A7204, pRoot = 002A7188[+1] OK, nSize = 96 OK, pNext = 00000000, pairs 002A71D4 OK
pFree[ 4] = 002A7264, pRoot = 002A7188[+1] OK, nSize = 192 OK, pNext = 00000000, pairs 002A7204 OK
pFree[ 5] = 002A7324, pRoot = 002A7188[+1] OK, nSize = 384 OK, pNext = 00000000, pairs 002A7264 OK
pFree[ 6] = 002A74A4, pRoot = 002A7188[+1] OK, nSize = 768 OK, pNext = 00000000, pairs 002A7324 OK
pFree[ 7] = 002A77A4, pRoot = 002A7188[+1] OK, nSize = 1536 OK, pNext = 00000000, pairs 002A74A4 OK
pFree[ 8] = 002A7DA4, pRoot = 002A7188[+1] OK, nSize = 3072 OK, pNext = 00000000, pairs 002A77A4 OK
pFree[ 9] = 002A89A4, pRoot = 002A7188[+1] OK, nSize = 6144 OK, pNext = 00000000, pairs 002A7DA4 OK
--*/

sequence done = {}

function dump_link(integer fn, atom pLink, atom eSize, integer nofl, integer nStatus)
atom pRoot, dwMagic, nSize, pNext, pPrev, pPair, pRootp, era, nSizeb
integer ok, k
--xdiff
--integer allok
    if machine_bits()=32 then
--      hex_dump(fn,pLink-8,20)
        pRoot = peek4u(pLink-4)
        ok = and_bits(pRoot,1)
        pRoot -= ok
--      if verbose or not ok then
--      end if
        if ok then
            dwMagic = peek4u(pRoot)
            ok = (dwMagic=#00484253)
        end if
--      allok = ok
        print2(fn,", pRoot = %08x[+1] %s",{pRoot,okdesc(ok)})
        nSize = peek4u(pLink)
        ok = (nSize=eSize)
--      allok = (allok and ok)
        print2(fn,", nSize = %d %s",{nSize,okdesc(ok)})
        pNext = peek4u(pLink+4)
        print2(fn,", pNext = %08x",{pNext})
        if nStatus=-4 and not nofl then -- global pool
            nSizeb = peek4u(pRoot+8)
            ok = (nSize=nSizeb)
            print2(fn," %s\n",{okdesc(ok)})
        else                
            if nofl then
                pPrev = peek4u(pLink+8)
                ok = (pPrev=ePrev and nStatus!=-4)
--              allok = (allok and ok)
                if not ok then
                    print2(fn,", pPrev = %08x",{pPrev,okdesc(ok)})
                end if
            end if
--DEV find the pairing... lists of "already done", replaced when merged...
--          xdiff = pLink-pRoot-28  -- [28=sizeof(SBH)+sizeof(padding)]
--          print2(fn,", xdiff:%d, N:%f",{xdiff,(xdiff)/nSize}) -- temp...
--          print2(fn,", pairs ??? \n") -- temp...
            if and_bits((pLink-pRoot-28)/nSize,1) then
                pPair = pLink-nSize
--              k = find(pPair,done)
--              if k then
--                  done[k] = pPair
--              end if
            else
                pPair = pLink+nSize
--              k = find(pLink,done)
            end if
            k = find(pPair,done)
            pRootp = peek4u(pPair-4)
            ok = (pRootp=pRoot)
--          allok = (allok and ok)
            print2(fn,", pairs %08x %s\n",{pPair,okdesc(ok)})
--          done = append(done,pPair)
            if k=0 then
                era = peek4u(pPair-8)
                print2(fn," ** leak ** %08x, pRoot = %08x, hex:%s\n",{pPair,pRootp,hex_dump40(pPair)})
                print2(fn,"            era = %08x %s%s:%d %s\n",getline(era))
--              done = append(done,min(pLink,pPair))
--              done = append(done,pLink)
                done = append(done,pPair)
                errorcount += 1
            end if
        end if
        done = append(done,pLink)
    else --64
        hex_dump(fn,pLink-16,40)
--      ... [DEV]
    end if
    ePrev = pLink
    return pNext
end function

-- Superblock Header
-- =================
--  DWORD   dwMagic     -- [+0] checked for memory corruption ("SBH\0" = #00484253)
--  DWORD   dwThreadId  -- [+4] owner
--  DWORD   nsbSize     -- [+8] does not include this 20 bytes (should always be >= 10K)
--  DWORD   pTCB        -- [+12] ptr/4 to the owning Thread Control Block (see below)
--  DWORD   pNxtsb      -- [+16] next superblock
--  ==
--  20 bytes (#14)  (see below for 64-bit version)

-- Superblock Header (64-bit)
-- ==========================
--  DWORD   dwMagic     -- [+0] checked for memory corruption ("SBH\0" = #00484253)
--  DWORD   dwThreadId  -- [+4] owner
--  QWORD   nsbSize     -- [+8] does not include this 32 bytes (should always be >= 8K)
--  QWORD   pTCB        -- [+16] ptr/4 to the owning Thread Control Block (see below)
--  QWORD   pNxtsb      -- [+24] next superblock
--  ==
--  32 bytes (#20) - note that magic and threadId are (quite deliberately) still dwords.

function dump_sbh(integer fn, integer pSBH)
integer nSize, ok
atom dwMagic, pTCB, pNxtsb
    print2(fn,"\npSBH = %08x\n",{pSBH})
    dwMagic = peek4u(pSBH)
    ok = (dwMagic=#00484253)
    if verbose or not ok then
        print2(fn,"dwMagic = %08x %s\n",{dwMagic,okdesc(ok)})
    end if
    if verbose then
        print2(fn,"dwThreadId = %08x\n",{peek4u(pSBH+4)})
    end if
    if machine_bits()=32 then
        nSize = peek4u(pSBH+8)
    else --64
        nSize = peek8u(pSBH+8)
    end if
    if verbose then
        print2(fn,"nSize = %08x (%d)\n",{nSize,nSize})
    end if
    if machine_bits()=32 then
        pTCB = peek4u(pSBH+12)
        pNxtsb = peek4u(pSBH+16)
    else --64
        pTCB = peek8u(pSBH+16)
        pNxtsb = peek8u(pSBH+24)
    end if
    if verbose then
        print2(fn,"pTCB = %08x [%08x]\n",{pTCB,pTCB*4})
        print2(fn,"pNxtsb = %08x\n",{pNxtsb})
    end if
-- this is total overkill with 12K superblocks, but if you modify pHeap 
--  to use something much smaller, this may (or may not) be more useful.
--  if verbose then
    if 0 then
        hex_dump(fn,pSBH+20,nSize)
    end if
    return pNxtsb
end function

function safescr(sequence s, atom idx)
    if idx<1 or idx>length(s) then return "***??***" end if
    return s[idx]
end function

function issafescr(sequence s, atom idx)
    return idx>=1 and idx<=length(s)
end function

constant sdesc = {"normal", "orphaned", "free", "global pool"}

-- Thread Control Blocks
-- =====================
--  DWORD       dwMagicT    -- [+0] checked for memory corruption ("TCB\0" = #00424354)
--  DWORD       dwThreadId  -- [+4] owner
--  DWORD       pNxtcb      -- [+8] next thread control block
--  DWORD       pFirst      -- [+12] first superblock
--  DWORD       nStatus     -- [+16] -1: normal, -2: orphaned, -3: free, -4: global pool
--  DWORD[27]   pFree       -- [+20] chains of owned free blocks, by size (not locked)
--  DWORD[27]   pNofl       -- [+128] ""       not-     ""                (need locks)
--  DWORD       dwMagicC    -- [+236] checked for memory corruption ("CCB\0" = #00424343)
--  BYTE[24]    csLock      -- [+240] critical section for locking (a dword futex on Lnx)
--  DWORD       dwMagicE    -- [+264] checked for memory corruption ("ECB\0" = #00424345)
--  ===
--  268 bytes (27*2+7)*4+24  (see below for 64-bit version)

-- Thread Control Block (64-bit)
-- =============================
--  DWORD       dwMagicT    -- [+0] checked for memory corruption ("TCB\0" = #00424354)
--  DWORD       dwThreadId  -- [+4] owner
--  QWORD       pNxtcb      -- [+8] next tcb
--  QWORD       pFirst      -- [+16] first superblock
--  QWORD       nStatus     -- [+24] -1: normal, -2: orphaned, -3: free, -4: global pool
--  QWORD[58]   pFree       -- [+32] chains of owned free blocks, by size (not locked)
--  QWORD[58]   pNofl       -- [+496]  ""    not-      ""                 (need locks)
--  DWORD       dwMagicB    -- [+960] checked for memory corruption ("BCB\0" = #00424342)
--  DWORD       dwMagicC    -- [+964] checked for memory corruption ("CCB\0" = #00424343)
--  BYTE[40]    csLock      -- [+968] critical section for locking (a dword futex on Lnx)
--  DWORD       dwMagicE    -- [+1008] checked for memory corruption ("ECB\0" = #00424345)
--  ===
--  1012 bytes - note that magic and threadId are (quite deliberately) still dwords.
--  dwMagicC can be used to check we located csLock correctly, all pointers should end 0b00

function dump_tcb(integer fn, integer pTCB)
-- note that pTCB is (actualaddr/4).
atom dwMagic, dwThreadId, pNxtcb, pSBH, nStatus, pLink, dwMagicB, dwMagicC, dwMagicE
integer ok, lim
atom nSize
    print2(fn,"\npTCB = %08x = [%08x]\n",{pTCB,pTCB*4})
    dwMagic = peek4u(pTCB*4)
    ok = (dwMagic=#00424354)
    if verbose or not ok then
        print2(fn,"dwMagic = %08x %s\n",{dwMagic,okdesc(ok)})
    end if
    dwThreadId = peek4u(pTCB*4+4)
    if verbose then
        print2(fn,"dwThreadId = %08x\n",{dwThreadId})
    end if
    if machine_bits()=32 then
        pNxtcb = peek4u(pTCB*4+8)
        pSBH = peek4u(pTCB*4+12)
        nStatus = peek4s(pTCB*4+16)
    else -- 64
        pNxtcb = peek8u(pTCB*4+8)
        pSBH = peek8u(pTCB*4+16)
        nStatus = peek8s(pTCB*4+24)
    end if
    ok = (nStatus=-4 or pSBH=0)
    if verbose or not issafescr(sdesc,-nStatus) or not ok then
        print2(fn,"pNxtcb = %08x\n",{pNxtcb})
        print2(fn,"pFirst = %08x %s\n",{pSBH,okdesc(ok)})
        print2(fn,"nStatus = %08x (%d) [%s]\n",{nStatus,nStatus,safescr(sdesc,-nStatus)})
    end if
    if machine_bits()=32 then
        lim = 27
        nSize = 24
    else -- 64
        lim = 58
        nSize = 44
    end if
    for i=1 to lim do
        if machine_bits()=32 then
            pLink = peek4u(pTCB*4+16+i*4)
        else -- 64
            pLink = peek8u(pTCB*4+24+i*8)
        end if
        if pLink!=0 then
            print2(fn,"pFree[%2d] = %08x",{i,pLink})
            while 1 do
                pLink = dump_link(fn,pLink,nSize,0,nStatus)
                if pLink=0 then exit end if
                print2(fn,"            %08x",{pLink})
            end while
--DEV follow chain, check all roots are correct...
        end if
--      if remainder(i,4)=0 then print2(fn,"\n") end if
--  end for
--  print2(fn,"\n")
--  for i=1 to lim do
        if machine_bits()=32 then
            pLink = peek4u(pTCB*4+124+i*4)
        else
            pLink = peek4u(pTCB*4+488+i*8)
        end if
        if pLink!=0 then
            print2(fn,"pNofl[%2d] = %08x",{i,pLink})
            while 1 do
                pLink = dump_link(fn,pLink,nSize,1,nStatus)
                if pLink=0 then exit end if
                print2(fn,"            %08x",{pLink})
            end while
        end if
--      if remainder(i,4)=0 then print2(fn,"\n") end if
        nSize *= 2
    end for
--  print2(fn,"\n")
    if machine_bits()=32 then
        dwMagicC = peek4u(pTCB*4+236)
        dwMagicE = peek4u(pTCB*4+264)
    else --64 (yes, magics are always dwords)
        dwMagicB = peek4u(pTCB*4+960)
        ok = (dwMagicB=#00424342)
        if verbose or not ok then
            print2(fn,"dwMagicB = %08x %s\n",{dwMagicB,okdesc(ok)})
        end if
        dwMagicC = peek4u(pTCB*4+964)
        dwMagicE = peek4u(pTCB*4+1008)
    end if
    ok = (dwMagicC=#00424343)
    if verbose or not ok then
        print2(fn,"dwMagicC = %08x %s\n",{dwMagicC,okdesc(ok)})
    end if
    ok = (dwMagicE=#00424345)
    if verbose or not ok then
        print2(fn,"dwMagicE = %08x %s\n",{dwMagicE,okdesc(ok)})
    end if
if 01 then
--  while pSBH!=0 do    -- (should never iterate, btw)
    while pSBH!=0 do
        pSBH = dump_sbh(fn,pSBH)
    end while
end if
    return pNxtcb
end function

constant day = {"Sun","Mon","Tue","Wed","Thu","Fri","Sat"}

global procedure check_heap(integer ntcb4)--, integer frst4)
string mempath = rootpath&"memchk.txt"
integer fn = open(mempath,"w")
sequence d
integer tcb4 = ntcb4
    if fn=-1 then
        printf(1,"error opening %s; press any key...",{mempath})
        {} = wait_key()
        return
    end if
    errorcount = 0
    silentrun = 1
    while 1 do  -- repeat with silentrun==0 if need be
        if not silentrun then
            printf(1,"checking heap, see %s\n",{mempath})
        end if
        if silentrun then
            d = date()                                              -- {year,month,day,hour,minute,second,dow,doy}
            d = d[4..6]&{day[d[7]]}&d[3]&d[2]&remainder(d[1],100)   -- {hour,minute,second,dow,day,month,year}
            printf(fn,"--\n")
            printf(fn,"-- Phix memory leak check.\n")
            printf(fn,"-- %s%s\n",{filepaths[filenames[1][1]],filenames[1][2]})
            printf(fn,"-- Generated at %d:%02d:%02d on %s %02d/%02d/%02d.\n",d)
            printf(fn,"--\n\n")
        end if
        print2(fn,"check_heap(%08x [%08x])\n",{ntcb4,ntcb4*4})
--      print2(fn,"check_heap(%08x [%08x], %08x [%08x])\n",{ntcb4,ntcb4*4,frst4,frst4*4})

        while tcb4!=0 do
            tcb4 = dump_tcb(fn,tcb4)
        end while

        print2(fn,"%d errors\n",{errorcount})
        if not silentrun then exit end if
        close(fn)
        if errorcount=0 then exit end if
        silentrun = 0
        errorcount = 0
        tcb4 = ntcb4        -- (reset)
    end while
    #ilASM{
        [PE32]
            call "kernel32.dll","GetProcessHeap"
            push ebx                            -- lpMem (NULL)
            push ebx                            -- dwFlags
            push eax                            -- hHeap
            call "kernel32.dll","HeapValidate"
            test eax,eax
            jnz :heapok
        [PE64]
            mov rcx,rsp -- put 2 copies of rsp onto the stack...
            push rsp
            push rcx
            or rsp,8    -- [rsp] is now 1st or 2nd copy:
                        -- if on entry rsp was xxx8: both copies remain on the stack
                        -- if on entry rsp was xxx0: or rsp,8 effectively pops one of them (+8)
                        -- obviously rsp is now xxx8, whatever alignment we started with
            sub rsp,8*5                             -- minimum 4 param shadow space, and align/somewhere to save rax
            mov [rsp+32],rax                        -- save rax (required length)
            call "kernel32.dll","GetProcessHeap"
            mov r8,rbx                          -- lpMem (p3)
            mov rdx,rbx                         -- dwFlags (p2,0)
            mov rcx,rax                         -- hHeap (p1)
            call "kernel32.dll","HeapValidate"
--          add rsp,8*5
--          pop rsp
            mov rsp,[rsp+8*5]   -- equivalent to the add/pop
            test rax,rax
            jnz :heapok
        [ELF32,ELF64]
            -- (nothing doing on linux, the "heap" is just some contiguous memory block, 
            --  logically following the code, with a (new) upper limit as set by sys_brk)
            jmp :heapok
        []
           }
    puts(1,"\n\n *** HeapValidate Error ***\n\n")
    if getc(0) then end if
    #ilASM{
          ::heapok
           }
    if errorcount!=0 then
        printf(1,"%s%s\n",{filepaths[filenames[1][1]],filenames[1][2]})
        printf(1,"report written to %s; press any key...",{mempath})
        {} = wait_key()
    end if
end procedure

