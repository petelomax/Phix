--
-- psched.e
-- ========
--
--  Instruction post-scheduler/peephole optimiser.
--

--  ********    Sadly I got a fair way with this, way back when pmain.e was directly emitting
--  * NOTE *    x86 binary, and before the introduction of loadReg/merge. However the latter
--  ********    utterly spanners this - you simply cannot track register use and then start
--              shuffling things around (over the mentioned/tracked branchpoints).
--
--  I do *NOT* expect this will ever work, it remains because it potentially collects very
--   useful information, just uses it at the wrong time, or something, eg:
--
--      "while k and k<=klast do"
--==>
--  mov eax,[#0041080C] (k)               ;#0040B3E5: 241 0C084100               uv 01 00  1   1      
--  test eax,eax                          ;#0040B3EA: 205300                     uv 00 01  1   2 01   
--  je #0040B57E                          ;#0040B3EC: 017204 8C010000            v  00 00  1   2      
--  mov ecx,[#00410810] (klast)           ;#0040B3F2: 213015 10084100            uv 02 00  1   3      
--  cmp eax,ecx                           ;#0040B3F8: 071310                     uv 00 03  1   4 02   
--  jg #0040B57E                          ;#0040B3FA: 017217 7E010000            v  00 00  1   4      
--
-- The plan was/is to remove the dependencies by moving the load of klast:
--
--  mov eax,[#0041080C] (k)               ;#0040B3E5: 241 0C084100               uv 01 00  1   1      
--  mov ecx,[#00410810] (klast)           ;#0040B3??: 213015 10084100            uv 02 00  1   1      
--  test eax,eax                          ;#0040B3??: 205300                     uv 00 01  1   2
--  je #0040B57E                          ;#0040B3??: 017204 8C0100??            v  00 00  1   2      
--  cmp eax,ecx                           ;#0040B3??: 071310                     uv 00 03  1   3
--  jg #0040B57E                          ;#0040B3??: 017217 7E010000            v  00 00  1   3
--
-- 25% faster. However, at #0040B3EC we track register content, via a merge(mergeSet) call, so
--  if we /later/ move the load of ecx as shown, it'll go wrong. All jumps now have a schend()
--  above them to prevent this, far better would be to decide to move that load /before/ the
--  merge() call... [DEV any suggestions welcome!]
--
-- A very slim chance remains that we might be able to resurrect this by delaying the merge(),
--  and/or "peeking ahead" when we spot a dependency, but classical compiler theory states the
--  (only) correct approach is:
--      il -> dataflow analysis -> SSA (or somesuch) -> register allocation (linear scan or
--      graph colouring) -> x86 binary.
-- Of course adding an SSA form will inevitably make the compiler/interpreter some 50% slower...
--

--with trace
--  Interface (optional):
--
--      schedule(refd, agid, modd, pair, flags, id)
--      <emit an instruction*>
--      schedule(refd, agid, modd, pair, flags, id)
--      <emit an instruction*>
--      -- also, when you know a schend() is imminent, set
--      sch00n = schoon
--      ...
--      if schidx then schend() end if
--      -- or --
--      schend()    -- (if you know schidx!=0)
--
-- The interface is optional in an all-or-nothing sense: after the first call
--  to schedule, calls must be made before every instruction until the schend
--  but otherwise blocks of code (eg an isOpCode calling convention block,
--  especially those with eg "NB p1/2 obtained from [esp]-9/-15 on error")
--  can be left as-is, without any such calls, and often should.
--
-- The general rule of thumb is to code "if schidx then schend() end if" before
--  any non-schedulable block, or at any unconditional branch point/call stmt.
--  Note that "<emit an instruction>" may be eg an emitHex1() and emitHexDword() 
--  pair rather than a single emitHexXx() call. It is also perfectly reasonable 
--  to treat say <cmp/jmp/display_fatal_error> as a single instruction, and in 
--  some cases may even be necessary.
--
--  sch00n just saves a bit of work, for example if you have two back-to-back
--  opRepe1's, some previous statements might "nick" an instruction or two
--  from the first opRepe1 to improve their scheduling, but the ones between 
--  the two opRepe1 (/schend) calls can never gain, so do nowt. If sch00n is 
--  set and you call schedule() when the table is empty, it leaves it empty.
--  (sch00n is automatically reset by the shend you said was imminent, btw)
--  It is no big sin to forget a sch00n=schoon, just suboptimal.
--
-- * <emit an instruction>, ie any emitHexXx() call, also invokes lineinfo() 
--  in pemit.e, which may also call shfixup() to keep track of LineTab entries.
--  Also loadReg() breaks the schedule()/lineinfo()/emitHexXx() ordering, by
--  doing lineinfo() [schedule()/emitHexXx()] instead, and needs a special flag 
--  (skipFixup) to compensate.
--
-- Before each instruction, we call:
--
--  schedule(refd, agid, modd, pair, flags, N)
--
--      eg: schedule(0,edibit+ecxbit,eaxbit,pUV,0,0)
--          Emit(<mov eax,[edi+ecx*4]>)
--          schedule(ecxbit,0,0,pUV,0,N)
--          Emit(<mov [N],ecx>)
--
--  where:
--      refd is referenced registers : \  eaxbit/ecxbit/edxbit/ebxbit/
--      agid is agi-ref'd registers  :  : espbit/ebpbit/esibit/edibit,
--      modd is modified registers   : /  or regbit[reg+1] where reg=loadReg()
--      pair: pNP, pU, pV, or pUV
--      flags: 1 if mods or tests flags, else 0
--      id: the var-id being stored/loaded (to avoid memory dependencies)
--      soon: schoon(=1) if schend is imminent, else 0
--          NB: id is non-zero for mov eax,[N] but 0 for mov eax,N; it is
--              whether memory is accessed rather than isVar is used, ie/eg:
-- correct:
--              schedule(0,0,eaxbit,pUV,0,      src  ,0)
--              emitHex5(mov_eax_mem32,isVar,src)               -- mov eax,[src]
-- correct:
--              schedule(0,0,esibit,pUV,0,      0  ,0)
--              emitHex5(mov_esi_imm32,isVar,p2)                -- mov esi,p2
-- wrong:
--              schedule(0,0,edxbit,pUV,0,      dest  ,0)
--              emitHex5(mov_edx_imm32,isVar,dest)              -- mov edx,dest
-- wrong:
--              schedule(0,0,0,pUV,0,           0  ,0)
--              emitHex6(mov_m32_imm32,isVar,N)                 -- mov [N],imm32
--              emitHexDword(constIntVal)
--
--  [NB: agid is not properly used at the moment, except in a "refd+=agid" way.
--       more work to avoid agi stalls remains outstanding (27/8/8)]
--
-- At branchpoints we simply call:
--
--  if schidx then schend() end if
--
--      (obviously you can omit the test if you know it is always true)
--
--      global integer schfwd can be set to 1 before the schend() call to trash
--      the schedule information; this is used on forward calls which create
--      unusual backpatch links via symtab[N][S_il] (see ParamList), rather than
--      attempt to maintain all backpatch links for all forward called routines,
--      we simply prohibit rescheduling.

--
-- The plan is simple: we keep a table of the above values, at schend()
--  we reorder the instructions to remove dependencies, eg:
--
--      instruction     clock
--      mov eax,[a]       1
--      mov [b],eax       2
--      mov ecx,[c]       2
--      mov [d],ecx       3
-- ==>
--      mov eax,[a]       1
--      mov ecx,[c]       1
--      mov [b],eax       2
--      mov [d],ecx       2
--
-- Pairing adds only a relatively minor complication. We assume the start
--  of each block begins in the u pipe:
--  When in the u pipe, if the next instruction has a pairing of:
--      np: nowt special is needed.
--      v: find a u that could benefit from being shuffled up, do that,
--          before the v, and leave ourselves flagged as being in u.
--      u,uv: flag as being in the v pipe.
--  When in the v pipe, if the next instruction has a pairing of:
--      np: find a v that could benefit from being shuffled up, do that,
--          before the np, and flag as being in the u pipe.
--      u: as np, but leave ourselves flagged as being in the v pipe.
--      v, uv: flag as being in the u pipe.
--
-- The "shuffling up" is in fact the trickiest part. Fairly obviously,
--  we keep a record of length(x86) at each schedule() call, and from
--  that we can figure out which chunks of x86 to move. However we also
--  need to keep an eye on LineTab, and ensure that any shuffle leaves
--  all LineTab entries pointing at the same instruction they were.
--
-- At the end of a block, ie when we issue a conditional jump or hit a
--  branch/label point, the schend(0) call scans through the remainder of
--  the table ordering by pairing as best as possible. Note that in eg
--      mov edx,[N]
--      cmp ecx,edx
--      jne L1
--      mov esi,[N2]
--  Theoretically the mov esi,[N2] could be moved up two instructions,
--   to remove the dependency on edx. However this is dangerous, since
--   reginfo(>0) may hold details of some content of esi, and besides
--   the interface is not designed to allow such. Instead the dependency
--   on edx should have been removed by shuffling up the mov edx,[N] if
--   possible, and if not, tough.
--
-- Lastly, when we freeTmpVar(), if the register/memory location has not
--  been ref'd, it may be possible to isDead the store. This will be done
--  on an individual/ad-hoc basis more likely than a blanket change.
--
-- NB The above represents the original plan and may not adequately
--    document various tweaks made later in the day. As an example,
--    a further enhancement might be that say k=x86[k+1] leaves "k+1" 
--    in ecx, but opSubse1i wants it in edi; as we know a "reginfo=0" 
--    is imminent, we might try to backpatch some ecx to edi.
--
global constant pNP=0, pU=1, pV=2, pUV=3

global constant eaxbit = #01,
                ecxbit = #02,
                edxbit = #04,
                ebxbit = #08,
--              espbit = #10,
--              ebpbit = #20,
                esibit = #40,
                edibit = #80,
                regbit = {#01,#02,#04,#08,#10,#20,#40,#80}
--                        eax,ecx,edx,ebx,esp,ebp,esi,edi
--
--global integer schidx -- master index to the following tables:
--             schidx = 0   -- now in pilx86.e
sequence refs, agis, mods, pairs, flags, ids, posns, lte
if sched then
         refs = {} agis = {} mods = {} pairs = {} flags = {} ids = {} posns = {} lte = {}
end if
--integer pipe
--      pipe = pU
integer wasLtLen
        wasLtLen = -1

global integer schall
               schall = 0

procedure shunt(sequence schedule, integer first, integer last)
integer firstbyte, lastbyte, outbyte, s, istart, iend, k
--, bstart
sequence buffer
integer flte, llte, rescan, prev, prevk, lti
--integer start, term, next, newstart
integer start, newstart

--sequence wasLineTab
--integer exprTerm
--sequence exprTerms
--trace(1)
--  -- find out whether 0 or -1 required for exprBP chain:
--  exprTerm = exprBP
--  while exprTerm>0 do
--      exprTerm = x86[exprTerm]
--  end while
--exprTerms = repeat(-99,length(exprBPs)) --DEV getting silly now....
--  for i=1 to length(exprBPs) do
--      k = exprBPs[i]
--      while k>0 do
--          k = x86[k]
--      end while
--      exprTerms[i] = k
--  end for
--wasLineTab = LineTab
    firstbyte = posns[first]+1
    if last=schidx then
        lastbyte = length(x86)
    else
        lastbyte = posns[last+1]
    end if
    outbyte = firstbyte
    buffer = x86[firstbyte..lastbyte]
    flte = length(LineTab)+1
    llte = 0
    for i=first to last do
        s = schedule[i]
        istart = posns[s]+1
--      istart = posns[s]-firstbyte+2
        if s<last then
            iend = posns[s+1]
--          iend = posns[s+1]-firstbyte+1
        else
            iend = lastbyte
--          iend = lastbyte-firstbyte+1
        end if
        ids[s]=outbyte  --DEV document properly... see schall
        pairs[s]=iend
        istart = istart-firstbyte+1
        iend = iend-firstbyte+1
--      bstart = posns[s]
--      k = lte[i]
        k = lte[s]
        if k then
            LineTab[k] = outbyte-1
            if k<flte then flte = k end if
            if k>llte then llte = k end if
        end if
--      bstart -= firstbyte-2
--      for j = istart-firstbyte+1 to iend-firstbyte+1 do
        for j = istart to iend do
            x86[outbyte] = buffer[j]
            outbyte += 1
        end for
    end for
--{0,-4,56,62,109,126,139,149,190}
--if equal(LineTab,{0,-4,56,62,109,126,150,131,196}) then
--  LineTab = {0,-4,56,62,109,126,131,150,196}
--end if
--  ?LineTab
    --
    -- "unswap" any instructions which have landed out-of-order.
    --  Eg suppose we had something like:
    --      15:     a=b
    --          mov eax,[b]
    --          mov [a],eax
    --      16:     c=d
    --          mov ecx,[d]
    --          mov [c],ecx
    --  and (without worrying about why) we end up with say:
    --      16:     c=d
    --          mov ecx,[d]
    --      15:     a=b
    --          mov eax,[b]
    --          mov [c],ecx
    --          mov [a],eax
    -- to simplify matters, swap the linestarts of lines 15&16.
    -- (of course there is no reason why you would get the above
    --  results on such a simple code stream, but equivalents
    --  can and do happen in more complex situations. Doubtless
    --  a more convincing example could be found...)
    --
    -- This guarantees we can print a listing (see plist.e), at
    -- the risk of slight confusion when reporting errors - but
    -- since the vast majority of error reporting occurs within
    -- VM opcodes, calls to which prevent this sort of overlap,
    -- the chances of confusion are pretty slim (but not zero).
    -- Note also that opLnt/p/pt naturally prevent any overlap,
    -- so this cannot upset trace()/profile() handling.
    --
if llte then
    while 1 do
        rescan = 0
        prev = LineTab[flte]
        prevk = flte
        for i=flte+1 to llte do
            lti = LineTab[i]
            if lti>=0 then
                if lti<prev then
                    LineTab[prevk] = lti
                    LineTab[i] = prev
                    rescan = 1
                end if
                prev = lti
                prevk = i
            end if
        end for
        if not rescan then exit end if
    end while
end if
--?wasLineTab
--?LineTab

    --
    -- fixup any outstanding backpatch chains we have clobbered...
    --
    while schall do
        start = s5[schall-2]
        for j=first to last do
            if start>=posns[j]
            and start<=pairs[j] then
                newstart = start -(posns[j]-ids[j]) -1
                s5[schall-2] = newstart
                exit
            end if
        end for
        schall = s5[schall]
    end while
--DEV bpStack removed 15/1: this didn't work (by that time) anyway...
--?9/0
--  for i=1 to length(bpStack) do
--      start = bpStack[i]
--      if start>0 then
--          term = bpTerms[i]
----            bpStack[i] = fixupBackPatchChain(k,first,last,firstbyte,lastbyte,buffer,bpTerms[i])
----function fixupBackPatchChain(integer start, integer first, integer last, 
----integer firstbyte, integer lastbyte, sequence buffer, integer term)
--  while 1 do
--      if start>=firstbyte and start<=lastbyte then
--          newstart = 0
--          for j=first to last do
--              if start>=posns[j]
--              and start<=pairs[j] then
--                  newstart = start -(posns[j]-ids[j]) -1
--                  exit
--              end if
--          end for
--          next = buffer[start-firstbyte+1]
--          x86[newstart] = term
--          term = newstart
--      else
--          next = x86[start]
--          x86[start] = term
--          term = start
--      end if
----
----DEV the way this currently works is to rebuild the chain "backwards", eg
----                 1 2 3 4 5 6 7 8 9 10 11 12 13 14
----        table = {_,_,_,_,0,_,_,5,_, _, 8, _, _, _}  (before schend(0))
----        start = 11
----     schedule = {1,4,2,5,3,6,8,7,9,11,10,13,12,14}
---- ==>
----        table = {_,_,_,7,_,_,10,_,_,0, _, _, _, _}  (after schend(0) & this)
----        start = 4
---- Now, if we rewrote this routine to preserve order (and return 10 in the 
----        above case/patch to 0-4-7), the following could be an "early exit".
----        Alternately, you could rebuild the chain backwards as above, then
----        when the following condition is hit, rebuild it forwards...
----
----        if next<=firstbyte then exit end if
--      if next<=0 then exit end if
--      start = next
--  end while
----    return term
----end function
--          bpStack[i] = term
--      end if
--  end for

--if getc(0) then end if
end procedure

--global integer skipFixup
--             skipFixup = 0
--
--global procedure shfixup()
----procedure fixuplt()
---- Save the linetab entry if LineTab was extended just after the previous call
----    to schedule(). If a hll statement is converted into multiple assembly
----    instructions, as most are, the LineTab entry becomes "attached" to the
----    address of the first and continues to point at it wherever it gets moved.
----    DEV: LineTab entries should not be moved out of order if we want to avoid
----         confusing plist.e, also possibly pemit.e/nilte. No attempt has yet
----         been made to stop such.
--integer LtLen
--if not skipFixup then
----if skipFixup then
----    skipFixup = 0
----else
--  LtLen = length(LineTab)
--  if schidx and wasLtLen!=LtLen then
--      lte[schidx] = LtLen
--  end if
--  wasLtLen = LtLen
--end if
--end procedure

-- pass this as last parameter to schedule() if a schend() is imminent (and
--  therefore there is no point starting anew), else pass 0:
global constant schoon=1

global integer sch00n  -- set to schoon whenever you know a schend() is imminent.
               sch00n = 0   -- normal state

global procedure schedule(integer refd, integer agid, integer modd, integer pair, integer flag, integer id)
--trace(1)
integer LtLen, posn
    if lastline!=emitline then lineinfo() end if
    if schidx=0 then
        if sch00n then return end if
    end if
    schidx+=1
    refs = append(refs,refd)
    agis = append(agis,agid)
    mods = append(mods,modd)
    pairs = append(pairs,pair)
    flags = append(flags,flag)
    ids = append(ids,id)
    posn = length(x86)
    posns = append(posns,posn)
--  lte = append(lte,0)
--if skipFixup then
--  skipFixup = 0
--  shfixup()
--end if
    LtLen = length(LineTab)
--DEV and LineTab[LtLen]=posn?
    if wasLtLen!=LtLen then
        lte = append(lte,LtLen)
--      lte[schidx] = LtLen
    else
        lte = append(lte,0)
    end if
    wasLtLen = LtLen
--  wasLtLen = length(LineTab)
end procedure
--global procedure schedule(integer refd, integer agid, integer modd, integer pair, integer flag, integer id)
----integer wasPopped
----trace(1)
----    fixuplt()
----too early:
----    if agid or refd then
------      if agid then
------          refd += agid
------          wasPopped = popped
------          popped = 0
------      end if
----        for i=popped+1 to schidx do
------          if and_bits(refd,refs[i]) then
----            if and_bits(refd,mods[i]) then
----                -- btw, I believe the "worst case scenario" here is that
----                --  a "double shuffle" may cause this loop to re-examine
----                --  something we have skipped over.
----                --  There may be a pathological case whereby we should
----                --  not call shuffle if i is <= popped?
------if i>popped and i>wasPopped then
------              shuffle(i,agid)
----                shuffle(i)
------end if
----            end if
----        end for
------if agid then
------  popped += wasPopped???
------end if
----    end if
--  schidx+=1
--  if schidx>length(refs) then
--      refs = append(refs,refd+agid)
--      mods = append(mods,modd)
--      pairs = append(pairs,pair)
--      flags = append(flags,flag)
--      ids = append(ids,id)
--      posns = append(posns,length(x86))
--      lte = append(lte,0)
--  else
--      refs[schidx] = refd+agid
--      mods[schidx] = modd
--      pairs[schidx] = pair
--      flags[schidx] = flag
--      ids[schidx] = id
--      posns[schidx] = length(x86)
--      lte[schidx] = 0
--  end if
--if skipFixup then
--  skipFixup = 0
--  shfixup()
--end if
--  wasLtLen = length(LineTab)
--end procedure

sequence Schedule, done
integer schnext, notthisnext

--with trace
procedure shout(integer j)
integer agid, mk
    agid = agis[j]
    if flags[j] or agid then
        for k=1 to j-1 do
            if not done[k] then
--DEV surely this should have been mods[k]:
--              if flags[k] or and_bits(agid,refs[k]) then
                mk = mods[k]
                if flags[k] or and_bits(agid,refs[k]) or (mk=0 and ids[k]) then
                    Schedule[schnext] = k
                    schnext += 1
                    done[k] = 1
                    notthisnext = 0
                end if
            end if
        end for
    end if
--  if notthisnext = j then
----trace(1)
--?1
--  end if
    Schedule[schnext] = j
    schnext += 1
    done[j] = 1
end procedure

global integer schfwd   -- set to 1 in ParamList, to trash any accumulated schedule info.
               schfwd = 0

global procedure schend()
--
-- Apply the accumulated schedule information to reorder the instructions.
-- Note the algorithm used is somewhat ad-hoc/organic in nature, ie while
--  the information we are likely to need is fairly straightforward, how
--  we actually use it is very much trial-and-error.
--
integer refd, agid, modd, id, mj, any3, ntnid
integer first, last
--,flagged, flag

--trace(1)
    if schidx then
--DEV?? (no longer in use methinks)
        if schfwd or schidx<2 then
            schfwd = 0
        else
--          if skipFixup then
--              skipFixup = 0
--              shfixup()
--          end if
            Schedule = repeat(0,schidx)
            done = repeat(0,schidx)
            schnext = 1
            notthisnext = 0
            for i=1 to schidx do
                refd = refs[i]
                agid = agis[i]
                modd = mods[i]
                id = ids[i]
                if agid then
                    for j=1 to i-1 do
                        if not done[j] then
                            mj = mods[j]
                            if and_bits(agid,mj) then
                                shout(j)
                                -- plant any 3 [?]
                                any3 = 3
                                for k=1 to i-1 do
                                    if not done[k] then
                                        shout(k)
                                        any3 -= 1
                                        if any3=0 then exit end if
                                    end if
                                end for
                            end if
                        end if
                    end for
                end if
                if refd then
                    for j=1 to i-1 do
                        if not done[j] then
                            mj = mods[j]
                            if and_bits(refd,mj) then
                                shout(j)
                                if j=i-1 then notthisnext=i end if
                            end if
                        end if
                    end for
                end if
                if modd then
                    for j=1 to i-1 do
                        if not done[j] then
                            if and_bits(modd,refs[j]) then
                                shout(j)
                            end if
                        end if
                    end for
                end if
                if id then
                    for j=1 to i-1 do
                        if not done[j] then
                            if ids[j]=id then
                                shout(j)
                                notthisnext = i
                            end if
                        end if
                    end for
                end if
            end for
            if notthisnext then
                refd = refs[notthisnext]
                ntnid = ids[notthisnext]
                if ntnid=0 then ntnid=-1 end if
                for i=1 to schidx do
                    if i!=notthisnext
                    and not done[i]
                    and not and_bits(refd,refs[i])
                    and not flags[i]
                    and ids[i]!=ntnid then
                        shout(i)
                        exit
                    end if
                end for
            end if
--DEV tryme, speedwise:
if schnext>1 then
            for i=1 to schidx do
                if not done[i] then
                    Schedule[schnext] = i
                    schnext += 1
                end if
            end for
            --
            -- process moved entries only, in one block
            --
            first = 0
            for i=1 to schidx do
                if Schedule[i]!=i then
                    if first=0 then first = i end if
                    last = i
                end if
            end for
            if first then shunt(Schedule, first, last) end if
end if
        end if -- schfwd

        schidx = 0
        schall = 0
        wasLtLen = -1

        -- use [1..0] rather than {} to keep any space allocated
        --  for each table to hand/available for append(), since
        --  we expect to re-use these tables thousands of times.
--DEV test to see if this obtains any gain
        refs = refs[1..0]
        agis = agis[1..0]
        mods = mods[1..0]
        pairs = pairs[1..0]
        flags = flags[1..0]
        ids = ids[1..0]
        posns = posns[1..0]
        lte = lte[1..0]
    end if -- schidx
    sch00n = 0
    if lastline!=emitline then lineinfo() end if
end procedure

--global procedure schend()
--sequence schedule
--integer mod, ref, modd, refd, nxt, pair, pj, flag, flagged, id, idj, idlast
--sequence done
--integer didx
--integer first, last
----,agid
----if schidx=15 then trace(1) end if
----    fixuplt()
----trace(1)
--if skipFixup then
--  skipFixup = 0
--  shfixup()
--end if
--  --
--  -- reorder the instructions, removing dependencies and agi stalls[DEV]
--  -- and filling U and V pipes as best we can.
--  -- Note: as it stands, the goal here is to remove obvious deficiencies,
--  --       rather than to find an optimal solution...
--  --       The algorithm is relatively simple: for each instruction, see
--  --       if we want to insert an instruction to remove reg/mem dependency,
--  --       and then also see if """ to get the intructions to pair better.
--  --       To prove/disprove whether a problem is in this code (rather than 
--  --       somewhere else), just comment out the shunt() call at the end.
--  --
--  schedule = repeat(0,schidx)     -- desired reordering of instructions
--  didx = 0                        -- index to ""
--  done = repeat(0,schidx)         -- flagged as added to schedule
--  pipe = pU                       -- assume each block starts in the u pipe
--  modd = 0                        -- clear register (modified) dependency
--  refd = 0                        -- clear register (referenced) dependency
--  idlast = 0                      -- clear memory dependency
--  for i=1 to schidx do
--      if not done[i] then
--          --
--          -- we are about to add i to schedule, but we might insert one or two before it.
--          --
--          refd = refs[i]
--          didx += 1
--          nxt = i
--          flagged = flags[i]
--          id = ids[i]
--          idj = 0
--          if and_bits(refd,modd) then             -- register dependency with last instruction
----DEV or memory dependency?
--              --
--              -- hunt upwards for a better instruction to emit first:
--              --
--              modd = or_bits(modd,mods[i])
--              for j=i+1 to schidx do
--                  if not done[j] then
--                      ref = refs[j]
--                      mod = mods[j]
--                      flag = flags[j]
--                      if (not flagged or not flag)    -- not dependent on or interfering with flags
--                      and not and_bits(ref,modd)      -- no register (modified) dependency
----                        and not and_bits(mod,modd)      -- no register (modified) dependency
--                      and not and_bits(mod,refd) then -- no register (referenced) dependency
----                        and (idlast=0 or idlast!=idj)   -- no memory dependency with prev
----                                                        -- DEV could relax "" if fill below...
----                        and (id=0 or id!=idj) then      -- no memory dependency with next
--                          idj = ids[j]
--                          if idj and idj!=idlast then
--                              idj = find(idj,ids)
--                              if idj=j then idj = 0 end if
--                          end if
--                          if idj=0 then
--                              done[j] = 1
--                              ids[j] = 0
--                              nxt = j
--                              modd = or_bits(modd,mod)
--                              exit
--                          end if
--                      end if
--                      idj = 0
--                      modd = or_bits(modd,mod)
--                      refd = or_bits(refd,ref)
--                      flagged = or_bits(flagged,flag)
--                  end if
--              end for
--          else
--              modd = or_bits(modd,mods[i])
--          end if
----            modd = or_bits(modd,mods[i])
--          pair = pairs[nxt]
--          if (pipe=pU and pair=pV)
--          or (pipe=pV and pair<=pU) then
--              --
--              -- find something to fill the spare pipe:
--              --
--              flagged = flags[i]
--              for j=nxt+1 to schidx do
--                  if not done[j] then
--                      ref = refs[j]
--                      pj = pairs[j]
--                      mod = mods[j]
--                      flag = flags[j]
----                        idj2 = ids[j]
--                      if (not flagged or not flag)    -- not dependent on or interfering with flags
--                      and not and_bits(ref,modd)      -- no register (modified) dependency
----                        and not and_bits(mod,modd)      -- not futher mod to prev modded DEV also needed above?
--                      and not and_bits(mod,refd)      -- no register (referenced) dependency
--                      and and_bits(pj,pipe) then      -- can execute in the empty pipe
----                        and (idlast=0 or idlast!=idj2)  -- no memory dependency with last
----                        and (id=0 or id!=idj2)          --    ""                     next DEV nxt=/!=i test missing!
----                        and (idj=0 or idj!=idj2) then   --    ""                     next DEV nxt=/!=i test missing!
--                          idj = ids[j]
--                          if idj and idj!=idlast then
--                              idj = find(idj,ids)
--                              if idj=j then idj = 0 end if
--                          end if
--                          if idj=0 then
--                              done[j] = 1
--                              ids[j] = 0
--                              schedule[didx] = j
--                              didx += 1
--                              exit
--                          end if
--                      end if
--                      modd = or_bits(modd,mod)
--                      refd = or_bits(refd,ref)
--                      flagged = or_bits(flagged,flag)
--                  end if
--              end for
--              if pipe = pV and pair=pNP then
--                  pipe = pU
--              end if
--          elsif pipe=pU then  -- and pair!=pV
--              if pair!=pNP then   -- pU or pUV
--                  pipe = pV
--              end if
--          else -- pipe=pV     -- and pair>=pV
--              pipe = pU
--          end if
--          schedule[didx] = nxt
--          done[nxt] = 1
--          if nxt!=i then
--              didx += 1
--              schedule[didx] = i
--              done[i] = 1
--              pair = pairs[i]
--              if pipe=pU then
--                  if and_bits(pair,pU) then
--                      pipe = pV
--                  end if
--              else -- pipe=pV
--                  if pair!=pU then
--                      pipe = pU
--                  end if
--              end if
--          end if
----if pipe = pU then
----    modd=0
----else
--          modd = mods[i]      -- may as well, in case we got the pairing wrong...
----end if
--          ids[i] = 0
--          idlast = id
--      end if
--  end for
--  --
--  -- process moved entries only, in one block
--  --
--  first = 0
--  for i=1 to schidx do
--      if schedule[i]!=i then
--          if first=0 then first = i end if
--          last = i
--      end if
--  end for
--  if first then shunt(schedule, first, last) end if
--
--  schidx = 0
--  wasLtLen = -1
--end procedure

--Challenge #1:
---- integer a,b,c,d
---- a=1 d=2
---- puts(1,"hello\n")  -- flush reginfo
--b=a c=d
--This currently emits:
--  mov esi,[#0040C358] (a)               ;#0040B024: 213065 58C34000            uv 40 00  1   4      
--  mov [#0040C35C] (b),esi               ;#0040B02A: 211065 5CC34000            uv 00 40  1   5 40   
--  mov ecx,[#0040C364] (d)               ;#0040B030: 213015 64C34000            vu 02 00  1   5      
--  mov [#0040C360] (c),ecx               ;#0040B036: 211015 60C34000            uv 00 02  1   6      
--better:
--  mov esi,[#0040C358] (a)               ;#0040B024: 213065 58C34000            uv 40 00  1   4      
--  mov ecx,[#0040C364] (d)               ;#0040B02A: 213015 64C34000            vu 02 00  1   4      
--  mov [#0040C35C] (b),esi               ;#0040B030: 211065 5CC34000            uv 00 40  1   5
--  mov [#0040C360] (c),ecx               ;#0040B036: 211015 60C34000            vu 00 02  1   5      
-- (Strictly speaking, the puts statement does not achieve anything, I just
--  wanted to be clear this challenge has nothing to do with reginfo, or
--  for that matter knowing the value of a/d, so mov [b],1 mov [c],2 would 
--  be somewhat missing the point, albeit such may have some other merit.)


