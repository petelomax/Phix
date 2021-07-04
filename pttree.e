--
-- pttree.e
-- ========
--

without trace
--with type_check
global sequence tt      -- The ternary tree
--sequence ttchk                -- ttchk[ttidx+CH] is 1 for strings, 2 for atoms, 3 for seq
--              ttchk = {}
--type ttt(sequence x)
--  if length(x)>664 then
--      if ttchk[661]!=2 then return 0 end if
--  end if
--  return 1
--end type
--global ttt tt     -- The ternary tree
                tt = {}

global integer ttidx    -- The main idx/result.
               ttidx = 0
    -- 0-based: tt[ttidx+CH]..tt[ttidx+GT] obtain node elements:

    -- For strings (var_ids and literals), this is:
global constant CH=1    -- char this node represents
       constant LT=2    --                         idx to "<" chs, or 0
global constant EQ=3    -- if ch<=0 then data else idx to next ch, or 0
       constant GT=4    --                         idx to ">" chs, or 0

    -- a terminator (so that "add" and "additional" can be stored in the same 
    -- tree) of -2 (for var_ids) or -1 (for string literals) is used, which
    -- allows for eg string fred="fred" to use the same slots for the ascii
    -- characters but yield different tt[ttidx+EQ] for external use.

--DEV tt_integer??
    -- For atoms (added via tt_atom), tt[ttidx+CH] holds an integer
    --  (in the range -1073741824..1073741823 aka #C0000000..#3FFFFFFF)
    --  or a 64-bit float. atom_base holds the root of this tree, and
    --  tt[ttidx+EQ] is not used internally/available for external use.
    
    -- For sequences (added via tt_sequence), tt[ttidx+CH] is either
    --  an atom element, {} for end of sequence, {-ttidx} for a
    --  substring, or {ttidx} for a subsequence. Strict ordering is
    --  not obeyed, only uniqueness is guaranteed. seq_base holds
    --  the root of this tree, and tt[ttidx+EQ] is available for 
    --  external use on the {} record.


global string identset
    identset = repeat(' ',256)  -- characters valid in an identifier
    -- convert to "string of nulls" (repeat(0,256) makes a dword-sequence)
    for i=1 to 256 do identset[i] = 0 end for
    identset['0'..'9'] = 1
    identset['A'..'Z'] = 1
    identset['_'] = 1
    identset['a'..'z'] = 1
--4/9/19 (nb changes must match charset in ptok.e
--  identset[#80] = 1   -- for rosettacode/unicode
--  identset[#88] = 1   -- for rosettacode/unicode
--  identset[#94] = 1   -- for rosettacode/unicode (as ptok.e is not stored in utf8)
--  identset[#9A] = 1   -- for rosettacode/unicode
--  identset[#A3] = 1   -- for rosettacode/unicode
--  identset[#BB] = 1   -- for rosettacode/unicode
--  identset[#CE] = 1   -- for rosettacode/unicode
--  identset[#CF] = 1   -- for rosettacode/unicode
--  identset[#E2] = 1   -- for rosettacode/unicode
    identset[#80..#BF] = 1
    identset[#C2..#F4] = 1

global string thisline -- copy of text[line]
    thisline = ""

integer ttCh    -- current tt[ttidx+CH] being inspected [speedwise]
integer isCh    -- identset[Ch]

--with trace
global procedure tt_search()
--
-- Search for/Insert a var_id beginning with Ch at thisline[col].
-- Result is ttidx, and Ch must be left correctly set.
--
integer pcurr   -- 1-based ptr idx, for later update of tt[pcurr] if needbe.

    pcurr = 0
--  isCh = identset[Ch]
    isCh = 1    -- must be, charset[Ch[=text[col]]=LETTER to get here.
    if length(tt) then  -- (19/10 not actually needed as tt_atom called first (via psym.e's PI) )
        ttidx = 0
        while 1 do
            ttCh = tt[ttidx+CH]
--          if ttchk[ttidx+CH]!=1 then ttCh=9/0 end if
--2/8/15!!
if 01 then
            if not isCh then
                if ttCh=-2 then return end if
--DEV wtf?  a) I doubt this is ever even triggered. 
--          b) it should clearly be ttCh vs Ch[?]
--          c) is there a way to make it common code?
--              if ttCh<0 then
                if ttCh<-2 then -- (2/8/15, yippee!!)
--              if ttCh>0 then  -- (2/8/15, nope...)
--              if ttCh>Ch then -- (2/8/15, nope...)
--              if ttCh<Ch then -- (2/8/15, nope...)
                    pcurr = ttidx+GT
                else
                    pcurr = ttidx+LT
                end if
            elsif ttCh=Ch then
                col += 1
                Ch = text[col]
                isCh = identset[Ch]
                pcurr = ttidx+EQ
            elsif Ch<ttCh then
                pcurr = ttidx+LT
            else -- Ch>ttCh then
                pcurr = ttidx+GT
            end if
else
            if ttCh=Ch then
                if ttCh=-2 then return end if
                col += 1
                Ch = text[col]
                isCh = identset[Ch]
                pcurr = ttidx+EQ
            elsif Ch<ttCh then
                pcurr = ttidx+LT
            else -- Ch>ttCh then
                pcurr = ttidx+GT
            end if
end if
            ttidx = tt[pcurr]
            if ttidx=0 then exit end if
        end while
    end if
--?{Ch,isCh}
    --
    -- Not a duplicate, so insert remainder of string
    --
    ttidx = length(tt)
    while 1 do
        if pcurr then       -- (19/10 not actually needed as tt_atom called first (via psym.e's PI) )
            tt[pcurr] = ttidx
        end if
--      tt = append(tt,0)   -- CH
--      tt = append(tt,0)   -- LT
--      tt = append(tt,0)   -- EQ
--      tt = append(tt,0)   -- GT
        tt &= {-2,0,0,0}
--ttchk = append(ttchk,1) -- string check
--ttchk = append(ttchk,0)
--ttchk = append(ttchk,0)
--ttchk = append(ttchk,0)
        if not isCh then return end if
        tt[ttidx+CH] = Ch
        col += 1
        Ch = text[col]
        isCh = identset[Ch]
        pcurr = ttidx+EQ
        ttidx += 4
    end while
end procedure

integer rtn_id
--DEV string? (tryme)
sequence key, pkey
--      key = ""
integer kidx,lp,lk,pCh,ch
        ch = 0
        lk = 0
integer term

function charmatch()
    if kidx<=lp then
        pCh = pkey[kidx]
        if ch!=pCh then return 0 end if
    end if
    if term=-2 then -- identifier scan (-1 is literal strings scan)
        if not identset[ch] then return 0 end if
    end if
    if kidx>lk then
        key &= ch
        lk += 1
    else
        key[kidx] = ch
    end if
    return 1
end function

-- aside:
--  The use of rand() to traverse the tree improves encryption
--  slightly, without any significant performance overhead.
--  Adding a bit of randomness also probably helps the test set
--  catch a few more (obscure) problems than it otherwise would.
--  Just as "constant a=1, b=2" and "constant b=2, a=1" cause
--  no measurable difference to any subsequent code, it does
--  not matter what order we write such things to the exe file.
--  While removing this might help some hacker reverse engineer
--  their own code, it is unlikely to help on yours or mine.
--  I have only put this in on the basis that it has absolutely
--  no impact on interpretation whatsoever, and less than 0.01%
--  when compiling, tracing, or reporting an error or warning.
--  Lastly, and obviously, replacing "j = rand(n)" with "j = n"
--  (perhaps controlled by a new parameter) will of course get
--  things back into strict alphabetical/numerical order.
-- /aside
--  
integer ttscramble

function traverse(integer node)
integer nxt, r, j, k
sequence todo
--integer nodech, nodelt, nodeeq, nodegt
--nodech=tt[node+CH]
--nodelt=tt[node+LT]
--nodeeq=tt[node+EQ]
--nodegt=tt[node+GT]
    if ttscramble then
        todo = {GT,EQ,LT}
--DEV/SUG instead (if ttscramble is still in use?):
--      if ttscramble then todo = scramble(todo) end if & then common up this code...
        for n=3 to 1 by -1 do
            j = rand(n)
            k = todo[j]
            nxt = tt[node+k]
            if k=EQ then
                ch = tt[node+CH]
                if ch>0 then
                    if nxt then
                        kidx += 1
                        if charmatch() then
                            if not traverse(nxt) then return 0 end if
                        end if
                        kidx -= 1
                    end if
                elsif ch=term and kidx>=lp then
                    -- a leaf node with key not shorter than partial:
                    if not call_func(rtn_id,{key[1..kidx],node}) then return 0 end if
                end if
            else
                if nxt then
                    if not traverse(nxt) then return 0 end if
                end if
            end if
            if j<n then
                todo[j] = todo[n]
            end if
        end for
        return 1
    end if
    r = 1
--if ttchk[node+CH]!=1 then ttCh=9/0 end if
    nxt = tt[node+LT]               if nxt then r = traverse(nxt) end if    -- recurse(LT)
    if r then
        ch = tt[node+CH]
--31/1/15:
--      if ch>0 then    kidx += 1
        if ch>=0 then   kidx += 1
            if (term!=-2 or ch>0) and charmatch() then
                nxt = tt[node+EQ]   if nxt then r = traverse(nxt) end if    -- recurse(EQ)
            end if      kidx -= 1
        elsif ch=term and kidx>=lp then
            -- a leaf node with key not shorter than partial:
            r = call_func(rtn_id,{key[1..kidx],node})
        end if
        if r then
            nxt = tt[node+GT]       if nxt then r = traverse(nxt) end if    -- recurse(GT)
        end if
    end if
    return r
end function

--global procedure tt_traverse(integer rtnid, sequence partkey, integer terminator)
global procedure tt_traverse(integer rtnid, string partkey, integer terminator, integer carblems=scramble)
--
-- Traverses the var_id (terminator=-2) or string literal(terminator=-1) tree 
--  calling the specified function, which should accept two parameters, the
--  string and a nodeidx, and return 1 to continue traversal, 0 to cease.
-- A partkey of "fish" will match "fishcake", but not vice versa.
-- The last parameter defaults to pglobals\scramble for use in pemit.e, but is
--  explictly set to 0 for use in intellisense (so things come out alphabetically).
--
-- NB this is not meant to be particularly fast, use sparingly.
--
    if length(tt) then
        rtn_id = rtnid
        pkey = partkey
        lp = length(pkey)
        key = ""
        lk = 0 --length(key)
        kidx = 0
        term = terminator
        ttscramble = carblems
        if traverse(0) then end if
    end if
end procedure

integer gnidx
function look_for_gnidx(sequence name, integer node)
    if node=gnidx then
        pkey = name
        return 0
    end if
    return 1
end function
constant r_look_for_gnidx = routine_id("look_for_gnidx")

global function getname(integer getnameidx, integer terminator)
--
-- Perform a tree traversal to obtain the name.
-- warning: only use following a fatal error;
--          unwise use could be awfully slow.
--
-- DEV probably to be replaced with a full traverse/replace of all identifiers...
--      (see pemit.e)
--
    gnidx = getnameidx
    tt_traverse(r_look_for_gnidx,"",terminator,0)
    return pkey
end function


global procedure relink()
-- Compilation has ceased, this is called either from finalfixups or Abort.
-- Locals may have been delinked from the ternary tree, and marked with -2
-- in symtab[i][S_Nlink]: relink them. Suppose for example there are some
-- 18 or so variables/parameters named "desc" in your application. Clearly
-- some/most of those have dropped out of scope at end routine/file so the
-- lookup will get/only examine the right one. By the time this is called,
-- all such "desc" have been replaced with an appropriate varno, and the
-- "desc" sitting in the ternary tree has a pointer of 0. Throw the whole
-- lot back onto that chain, so the exe only needs the one "desc" string,
-- rather than 18 of them...
object si
integer k
--  for i=1 to T_pathset-1 do
--      si = symtab[i]
--      if sequence(si) and si[S_Nlink]=-2 then
--          k = si[S_Name]+EQ
--          si = tt[k]
--          symtab[i][S_Nlink] = si
--          tt[k] = i
--      end if
--  end for
    for i=T_constm1 to length(symtab) do
        si = symtab[i]
        if sequence(si) and si[S_Nlink]=-2 then
            k = si[S_Name]+EQ
            si = tt[k]
            symtab[i][S_Nlink] = si
            tt[k] = i
        end if
    end for
end procedure

--DEV untried, there may be several places (in pEmit2.e) that have/need this:
--/*
global procedure unlink(integer ttidx, integer symidx)
sequence sv = symtab[symidx]
integer node = sv[S_Name],
        slink = tt[node+EQ],
        snxt
    if sv[S_NTyp]=S_Rsvd then ?9/0 end if   -- sanity check
    if slink=symidx then
        tt[node+EQ] = sv[S_Nlink]
    else
        while slink do
            snext = symtab[slink][S_Nlink]
            if snext=symidx then
                symtab[slink][S_Nlink] = sv[S_Nlink]
                exit
            end if
            slink = snext
        end while
    end if
end procedure
--*/

--
-- Routines for constant literal handling:
--

integer atom_base
        atom_base = 0

global procedure tt_atom(atom data)
--
-- Lookup an atom value in the ternary tree.
--      Result is ttidx. Info may be stored in tt[ttidx+EQ], usually an
--      index into symtab. The atom value corresponding to a given ttidx
--      can be retrieved from tt[ttidx+CH(=1)].
--
-- Note The "atom subtree" is totally separate from the main "string tree",
--      controlled by atom_base. The main string tree does not have an
--      equivalent "string_base", just assumes it is zero, ie that the 
--      tt_string calls (below) occur before any tt_atom calls.
--      If at any point you wanted to introduce "string_base" you would 
--      need to change ttidx/pcurr=0 handling to !=-1 or somesuch.
--
atom thisAtom
integer pcurr
    ttidx = atom_base
    pcurr = 0
    if ttidx then
        while 1 do
            thisAtom = tt[ttidx+CH]
--if ttchk[ttidx+CH]!=2 then ttCh=9/0 end if
            if data=thisAtom then return end if
            if data>thisAtom then
--          c = compare(data,thisAtom)
--          if c=0 then return end if
--          if c=1 then -- data>thisAtom
                pcurr = ttidx+GT
            else -- data<thisAtom
                pcurr = ttidx+LT
            end if
            ttidx = tt[pcurr]
            if ttidx=0 then exit end if
        end while
    end if
    ttidx = length(tt)
    if pcurr then
        tt[pcurr] = ttidx
    else
        atom_base = ttidx
    end if
    tt = append(tt,data)    -- CH
    tt = append(tt,0)       -- LT
    tt = append(tt,0)       -- EQ
    tt = append(tt,0)       -- GT
--ttchk = append(ttchk,2) -- atom
--ttchk = append(ttchk,0)
--ttchk = append(ttchk,0)
--ttchk = append(ttchk,0)
end procedure

integer idxstart
        idxstart=1
integer idx, ltxt, ltt
integer tCh
integer pcurr
--global procedure tt_string(sequence text, integer idx, integer terminator)
global procedure tt_string(string text, integer terminator)
--
-- Lookup a string value in the ternary tree.
--
--      Result is ttidx. Info may be stored in tt[ttidx+EQ], usually an
--      index into symtab.
--
--      The terminator should be -2 for var_id use or -1 for string literal use.
--       (A terminator of -3 is also used for ilasm.)
--      This allows eg string fred="fred" to share the same slots for the 
--      ascii characters but yield a different ttidx and hence tt[ttidx+EQ] 
--      for string literal and var_id use.
--      Eg tt_string("global",-2) means that ttidx matches global rather 
--      than "global". While we might validly have tt_string("",-1) and
--      T_nullstr=ttidx, it should be clear that tt_string("",-2) yields a 
--      ttidx that the parser never can, and in fact yields a ttidx which 
--      is very probably of no use to anybody under any circumstances,
--      except perhaps as a special "won't match with anything" value.
--
--      This differs from tt_search in that it does not demand a non-identset 
--      terminator and does not place "The cat sat on the mat" and "The " in 
--      the exact same pigeonhole, as the parser-orientated tt_search would.
--      As mentioned, this routine can also handle "", ie the null string,
--      which tt_search cannot (ioob on Ch = thisline[col] for starters).
--
--      tt_string("keyword",-2) is the preferred mechanism during compiler
--      initialisation, equivalent to tt_lookup("keyword ",1) [nb the ' '],     --DEV pardon?
--      or thisline="keyword "/Ch='k'/col=1/tt_search().
--
--      Unlike the comments in tt_atom and tt_sequence, there should be no
--      reason why this should not be invoked before tt_search, if desired.
--
    pcurr = 0
    ltxt = length(text)
    idx = idxstart
    idxstart = 1
    if idx<=ltxt then
        ch = text[idx]
    else
        ch = terminator
    end if
    ltt = length(tt)
    if ltt then
        ttidx = 0
        while 1 do
            tCh = tt[ttidx+CH]
--if ttchk[ttidx+CH]!=1 then ttCh=9/0 end if
            if ch=tCh then
                if ch=terminator then return end if
                idx += 1
                if idx>ltxt then
                    ch = terminator
                else
                    ch = text[idx]
                    if terminator!=-1 then
                        if not identset[ch] then
                            ch = terminator
                        end if
                    end if
                end if
                pcurr = ttidx+EQ
            elsif ch>tCh then
                pcurr = ttidx+GT
            else -- ch<tCh
                pcurr = ttidx+LT
            end if
            ttidx = tt[pcurr]
            if ttidx=0 then exit end if
        end while
    end if
    ttidx = ltt
    if pcurr then
        tt[pcurr] = ttidx
    end if
    while 1 do
        tt = append(tt,ch)      -- CH
        tt = append(tt,0)       -- LT
        tt = append(tt,0)       -- EQ
        tt = append(tt,0)       -- GT
--ttchk = append(ttchk,1) -- string
--ttchk = append(ttchk,0)
--ttchk = append(ttchk,0)
--ttchk = append(ttchk,0)

        if ch=terminator then return end if
        pcurr = ttidx+EQ
        ttidx += 4
        idx += 1
        if idx>ltxt then
            ch = terminator
        else
            ch = text[idx]
            if terminator!=-1 then
                if not identset[ch] then
                    ch = terminator
                end if
            end if
        end if
        tt[pcurr] = ttidx
    end while
end procedure

integer seq_base
        seq_base = 0

global procedure tt_sequence(sequence data)
--
-- Lookup a sequence value in the ternary tree.
--
--      Result is ttidx. Info may be stored in tt[ttidx+EQ], usually an
--      index into symtab. Integer and float elements are stored as-is.
--      Subsequences must be stored as {ttidx}, and substrings as {-ttidx},
--      ie/eg when parsing say {"string",{1,2},3}, rather than invoking 
--      tt_sequence({"string",{1,2},3}) you might instead expect to
--      tt_string("string")/tt_sequence({1,2})/tt_sequence({-t1},{t2},3).
--      The end of sequence is (internally) indicated by {}. 
--
--      Retrieval of values stored in the sequence tree is non-trivial; see 
--      pemit.e/tt_traverseQ(). The primary goal is to answer the question
--      "is this unique?" as fast as possible: other mechanisms should be
--      used when speed of retrieval is important.
--
-- Note The "sequence subtree" is separate from the main "string tree",
--      controlled by seq_base. The main string tree does not have an
--      equivalent "string_base", just assumes it is zero, ie that the 
--      tt_string calls (below) occur before any tt_sequence calls.
--      If at any point you wanted to introduce "string_base" you would 
--      need to change ttidx/pcurr=0 handling to !=-1 or somesuch.
--
object thisElement, thisKey
integer pcurr, thisIdx, c
--, save
    pcurr = 0
    thisIdx = 1
    if length(data)=0 then
        thisElement = {}
    else
        thisElement = data[1]
    end if
    ttidx = seq_base
    while ttidx do
        thisKey = tt[ttidx+CH]
--if ttchk[ttidx+CH]!=3 then ttCh=9/0 end if
        c = compare(thisElement,thisKey)
        if c=0 then     -- ie thisElement=thisKey
            if thisIdx > length(data) then return end if
            thisIdx += 1
            if thisIdx>length(data) then
                thisElement = {}
            else
                thisElement = data[thisIdx]
            end if
            pcurr = ttidx+EQ
        elsif c=1 then  -- ie thisElement>thisKey
            pcurr = ttidx+GT
        else -- c=-1    -- ie thisElement<thisKey
            pcurr = ttidx+LT
        end if
        ttidx = tt[pcurr]
    end while
    ttidx = length(tt)
    if pcurr then
        tt[pcurr] = ttidx
    else
        seq_base = ttidx
    end if
    while 1 do
        tt = append(tt,thisElement)     -- CH
        tt = append(tt,0)               -- LT
        tt = append(tt,0)               -- EQ
        tt = append(tt,0)               -- GT
--ttchk=append(ttchk,3) --seq
--ttchk=append(ttchk,0)
--ttchk=append(ttchk,0)
--ttchk=append(ttchk,0)
        if thisIdx > length(data) then return end if
        pcurr = ttidx+EQ
        ttidx += 4
        thisIdx += 1
        if thisIdx>length(data) then
            thisElement = {}
        else
            thisElement = data[thisIdx]
        end if
        tt[pcurr] = ttidx
    end while
end procedure

-- version of traverse for items added via tt_atom.
-- in this case the EQ link is not used internally,
-- rtn_id should retrieve tt[node+EQ] itself.
function traverseA(integer node)
integer nxt, r, j, k
sequence todo
--object dbg

    if scramble then
        todo = {GT,EQ,LT}
        for n=3 to 1 by -1 do
            j = rand(n)
            k = todo[j]
            if k=EQ then
                if not call_func(rtn_id,{tt[node+CH],node}) then return 0 end if
            else
                nxt = tt[node+k]
                if nxt then
                    if not traverseA(nxt) then return 0 end if
                end if
            end if
            if j<n then
                todo[j] = todo[n]
            end if
        end for
        return 1
    end if
    r = 1
--if ttchk[node+CH]!=2 then ttCh=9/0 end if
    nxt = tt[node+LT]               if nxt then r = traverseA(nxt) end if   -- recurse(LT)
    if r then
--dbg= tt[node+CH]
        r = call_func(rtn_id,{tt[node+CH],node})
        if r then
            nxt = tt[node+GT]       if nxt then r = traverseA(nxt) end if   -- recurse(GT)
        end if
    end if
    return r
end function

global procedure tt_traverseA(integer rtnid)
--
-- Traverses the tt_atom tree calling the specified function.
-- rtnid should be the routine_id of a funtion which accepts two parameters,
-- atom value and node idx, and returns 1 to continue traversal, 0 to cease.
--
    if atom_base then
        rtn_id = rtnid
        if traverseA(atom_base) then end if
    end if
end procedure

-- version of traverse for items added via tt_sequence.
-- rtn_id should retrieve tt[node+EQ] itself.
function traverseQ(integer node)
object element
integer nxt, r, j, k
sequence todo

    if scramble then
        todo = {GT,EQ,LT}
        for n=3 to 1 by -1 do
            j = rand(n)
            k = todo[j]
            nxt = tt[node+k]
            if k=EQ then
                element = tt[node+CH]
                if equal(element,{}) then
                    if not call_func(rtn_id,{key[1..kidx],node}) then return 0 end if
                else
                    kidx += 1
                    if kidx>lk then
                        key = append(key,element)
                        lk += 1
                    else
                        key[kidx] = element
                    end if
                    if nxt then
                        if not traverseQ(nxt) then return 0 end if
                    end if
                    kidx -= 1
                end if
            else
                if nxt then
                    if not traverseQ(nxt) then return 0 end if
                end if
            end if
            if j<n then
                todo[j] = todo[n]
            end if
        end for
        return 1
    end if
    r = 1
--if ttchk[node+CH]!=3 then ttCh=9/0 end if
    nxt = tt[node+LT]               if nxt then r = traverseQ(nxt) end if   -- recurse(LT)
    if r then
        element = tt[node+CH]
        if equal(element,{}) then
            r = call_func(rtn_id,{key[1..kidx],node})
        else
            kidx += 1
            if kidx>lk then
                key = append(key,element)
                lk += 1
            else
                key[kidx] = element
            end if
            nxt = tt[node+EQ]       if nxt then r = traverseQ(nxt) end if   -- recurse(EQ)
--          if r then
                kidx -= 1
--          end if
        end if
        if r then
            nxt = tt[node+GT]       if nxt then r = traverseQ(nxt) end if   -- recurse(GT)
        end if
    end if
    return r
end function

global procedure tt_traverseQ(integer rtnid)
--
-- Traverses the tt_sequence tree calling the specified function.
-- rtnid should be the routine_id of a funtion which accepts two parameters,
-- the sequence and node idx, and returns 1 to continue traversal, 0 to cease.
--
    if seq_base then
        rtn_id = rtnid
        kidx = 0
        key = {}
        lk = 0 --length(key)
        if traverseQ(seq_base) then end if
    end if
end procedure


--function tt_stringf(sequence text)
--  tt_string(text,-2)
--  return ttidx
--end function

--procedure tt_stringF(sequence text, integer chk, integer term=-2)
--  tt_string(text,term)
procedure tt_stringF(sequence text, integer chk)
    tt_string(text,-2)
    if chk!=ttidx then
        printf(1,"%s should be %d(not %d)\n",{text,ttidx,chk})
        if getc(0) then end if
    end if
end procedure

--procedure tt_stringA(sequence text, integer alias)
--  tt_string(text,-2)
--  tt[pcurr] = alias
--end procedure
--
--procedure tt_glabel(sequence glabel, integer chk)
--  tt_string(glabel,-3)
--  if chk!=ttidx then
--      printf(1,"%s should be %d(not %d)\n",{glabel,ttidx,chk})
--      if getc(0) then end if
--  end if
--end procedure

-- Keywords etc (T_xxx constants)
-- ========
--  These don't really add anything, just define a unique ttidx. 
--  As such they are just numbers, and you could order this lot 
--  in logical grouping, alphabetically, or completely at random, 
--  it would make no difference, except inserting things means 
--  (lots of) extra work, compared to adding at the end, that is.
--
-- Tech note: Coded this way so that eg "=T_global" in say
--      pmain.e can inline the constant 24, as opposed to
--      say constant T_global = tt_stringF("global"), which
--      obviously means T_global is not known until runtime
--      and hence pmain.e etc have to load it. Whether that
--      actually makes for any measurable gain is unknown.
--      Reordering or inserting causes tt_stringF() to walk
--      you through any necessary changes one at a time, so
--      you can just add "new_keyword" of 0 and be told the 
--      number that you actually need to use, the next time
--      you try to compile it (p.exw/pth.exw/pgui.exw).
--
-- TIP: Add things at the end. If you MUST reorder, allow a
--      good 20 mins (depending where you start from) to go
--      through the renumbering, then another 20 mins or so
--      to ponder why you insist on wasting your time. ;-)
--
global constant T_global        = 24    tt_stringF("global",T_global)
global constant T_proc          = 64    tt_stringF("procedure",T_proc)
global constant T_func          = 100   tt_stringF("function",T_func)
global constant T_type          = 120   tt_stringF("type",T_type)
global constant T_not           = 136   tt_stringF("not",T_not)
global constant T_then          = 152   tt_stringF("then",T_then)
global constant T_do            = 164   tt_stringF("do",T_do)
global constant T_and           = 180   tt_stringF("and",T_and)
global constant T_or            = 192   tt_stringF("or",T_or)
global constant T_xor           = 208   tt_stringF("xor",T_xor)
global constant T_end           = 224   tt_stringF("end",T_end)
global constant T_exit          = 240   tt_stringF("exit",T_exit)
global constant T_elsif         = 260   tt_stringF("elsif",T_elsif)
global constant T_else          = 268   tt_stringF("else",T_else)
global constant T_to            = 276   tt_stringF("to",T_to)
global constant T_by            = 288   tt_stringF("by",T_by)
global constant T_if            = 300   tt_stringF("if",T_if)
global constant T_for           = 312   tt_stringF("for",T_for)
global constant T_while         = 336   tt_stringF("while",T_while)
global constant T_return        = 364   tt_stringF("return",T_return)
global constant T_profile       = 384   tt_stringF("profile",T_profile)
global constant T_profile_time  = 408   tt_stringF("profile_time",T_profile_time)
global constant T_trace         = 428   tt_stringF("trace",T_trace)
global constant T_warning       = 456   tt_stringF("warning",T_warning)
global constant T_type_check    = 484   tt_stringF("type_check",T_type_check)
global constant T_debug         = 504   tt_stringF("debug",T_debug)
global constant T_console       = 536   tt_stringF("console",T_console)
global constant T_gui           = 548   tt_stringF("gui",T_gui)
global constant T_constant      = 568   tt_stringF("constant",T_constant)
global constant T_include       = 596   tt_stringF("include",T_include)
global constant T_with          = 612   tt_stringF("with",T_with)
global constant T_without       = 628   tt_stringF("without",T_without)
global constant T_forward       = 648   tt_stringF("forward",T_forward)
--DEV to go... (or better yet move/rename T_ilASM)
global constant T_ilasmX        = 668   tt_stringF("ilasm",T_ilasmX)
global constant T_istype        = 692   tt_stringF("istype",T_istype)
global constant T_isinit        = 712   tt_stringF("isinit",T_isinit)
global constant T_isginfo       = 736   tt_stringF("isginfo",T_isginfo)
global constant T_MIN           = 752   tt_stringF("MIN",T_MIN)
global constant T_MAX           = 764   tt_stringF("MAX",T_MAX)
global constant T_MAXLEN        = 780   tt_stringF("MAXLEN",T_MAXLEN)
global constant T_machine_func  = 832   tt_stringF("machine_func",T_machine_func)
global constant T_machine_proc  = 852   tt_stringF("machine_proc",T_machine_proc)
global constant T_append        = 876   tt_stringF("append",T_append)
global constant T_prepend       = 900   tt_stringF("prepend",T_prepend)
global constant T_licence       = 932   tt_stringF("licence",T_licence)
global constant T_public        = 956   tt_stringF("public",T_public)
global constant T_export        = 976   tt_stringF("export",T_export)
global constant T_enum          = 988   tt_stringF("enum",T_enum)
global constant T_ifdef         = 1004  tt_stringF("ifdef",T_ifdef)
global constant T_elsifdef      = 1020  tt_stringF("elsifdef",T_elsifdef)
global constant T_elsedef       = 1036  tt_stringF("elsedef",T_elsedef)
global constant T_WIN32         = 1060  tt_stringF("WIN32",T_WIN32)
global constant T_WINDOWS       = 1080  tt_stringF("WINDOWS",T_WINDOWS)
global constant T_LINUX         = 1104  tt_stringF("LINUX",T_LINUX)
global constant T_FREEBSD       = 1136  tt_stringF("FREEBSD",T_FREEBSD)
global constant T_SUNOS         = 1160  tt_stringF("SUNOS",T_SUNOS)
global constant T_OPENBSD       = 1192  tt_stringF("OPENBSD",T_OPENBSD)
global constant T_OSX           = 1204  tt_stringF("OSX",T_OSX)
global constant T_UNIX          = 1224  tt_stringF("UNIX",T_UNIX)
global constant T_WIN32_GUI     = 1244  tt_stringF("WIN32_GUI",T_WIN32_GUI)
global constant T_WIN32_CONSOLE = 1276  tt_stringF("WIN32_CONSOLE",T_WIN32_CONSOLE)
global constant T_SAFE          = 1292  tt_stringF("SAFE",T_SAFE)
global constant T_DATA_EXECUTE  = 1344  tt_stringF("DATA_EXECUTE",T_DATA_EXECUTE)
global constant T_UCSTYPE_DEBUG = 1396  tt_stringF("UCSTYPE_DEBUG",T_UCSTYPE_DEBUG)
global constant T_CRASH         = 1420  tt_stringF("CRASH",T_CRASH)
global constant T_switch        = 1448  tt_stringF("switch",T_switch)
global constant T_fallthru      = 1480  tt_stringF("fallthru",T_fallthru)
global constant T_fallthrough   = 1500  tt_stringF("fallthrough",T_fallthrough)
global constant T_jump_table    = 1544  tt_stringF("jump_table",T_jump_table)
global constant T_case          = 1560  tt_stringF("case",T_case)
global constant T_default       = 1584  tt_stringF("default",T_default)
global constant T_break         = 1604  tt_stringF("break",T_break)
global constant T_continue      = 1628  tt_stringF("continue",T_continue)
global constant T_strict        = 1652  tt_stringF("strict",T_strict)
global constant T_namespace     = 1688  tt_stringF("namespace",T_namespace)

--DEV ilASM additions:
global constant T_e_all         = 1708  tt_stringF("e_all",T_e_all)
global constant T_byte          = 1720  tt_stringF("byte",T_byte)
global constant T_word          = 1736  tt_stringF("word",T_word)
global constant T_dword         = 1756  tt_stringF("dword",T_dword) -- see also Z_dword below
global constant T_qword         = 1780  tt_stringF("qword",T_qword)
--global constant T_tbyte       = 1780  tt_stringF("tbyte",T_tbyte) -- I'm with OllyDbg here [== misnamed "tword" of fasm]
global constant T_eax           = 1792  tt_stringF("eax",T_eax)
global constant T_ebx           = 1804  tt_stringF("ebx",T_ebx)
global constant T_ecx           = 1816  tt_stringF("ecx",T_ecx)
global constant T_edx           = 1828  tt_stringF("edx",T_edx)
global constant T_ebp           = 1836  tt_stringF("ebp",T_ebp)
global constant T_esp           = 1848  tt_stringF("esp",T_esp)
global constant T_esi           = 1856  tt_stringF("esi",T_esi)
global constant T_edi           = 1864  tt_stringF("edi",T_edi)
global constant T_r8d           = 1876  tt_stringF("r8d",T_r8d)
global constant T_r9d           = 1888  tt_stringF("r9d",T_r9d)
global constant T_r10d          = 1904  tt_stringF("r10d",T_r10d)
global constant T_r11d          = 1916  tt_stringF("r11d",T_r11d)
global constant T_r12d          = 1928  tt_stringF("r12d",T_r12d)
global constant T_r13d          = 1940  tt_stringF("r13d",T_r13d)
global constant T_r14d          = 1952  tt_stringF("r14d",T_r14d)
global constant T_r15d          = 1964  tt_stringF("r15d",T_r15d)
global constant T_al            = 1972  tt_stringF("al",T_al)
global constant T_cl            = 1980  tt_stringF("cl",T_cl)
global constant T_dl            = 1988  tt_stringF("dl",T_dl)
global constant T_bl            = 1996  tt_stringF("bl",T_bl)
global constant T_ah            = 2004  tt_stringF("ah",T_ah)
global constant T_ch            = 2012  tt_stringF("ch",T_ch)
global constant T_dh            = 2020  tt_stringF("dh",T_dh)
global constant T_bh            = 2028  tt_stringF("bh",T_bh)
global constant T_spl           = 2040  tt_stringF("spl",T_spl)
global constant T_bpl           = 2052  tt_stringF("bpl",T_bpl)
global constant T_sil           = 2064  tt_stringF("sil",T_sil)
global constant T_dil           = 2076  tt_stringF("dil",T_dil)
global constant T_r8l           = 2084  tt_stringF("r8l",T_r8l)
global constant T_r9l           = 2092  tt_stringF("r9l",T_r9l)
global constant T_r10l          = 2100  tt_stringF("r10l",T_r10l)
global constant T_r11l          = 2108  tt_stringF("r11l",T_r11l)
global constant T_r12l          = 2116  tt_stringF("r12l",T_r12l)
global constant T_r13l          = 2124  tt_stringF("r13l",T_r13l)
global constant T_r14l          = 2132  tt_stringF("r14l",T_r14l)
global constant T_r15l          = 2140  tt_stringF("r15l",T_r15l)
-- (r8b..r15b are exactly the same as r8l..r15l)
global constant T_r8b           = 2148  tt_stringF("r8b",T_r8b)
global constant T_r9b           = 2156  tt_stringF("r9b",T_r9b)
global constant T_r10b          = 2164  tt_stringF("r10b",T_r10b)
global constant T_r11b          = 2172  tt_stringF("r11b",T_r11b)
global constant T_r12b          = 2180  tt_stringF("r12b",T_r12b)
global constant T_r13b          = 2188  tt_stringF("r13b",T_r13b)
global constant T_r14b          = 2196  tt_stringF("r14b",T_r14b)
global constant T_r15b          = 2204  tt_stringF("r15b",T_r15b)
global constant T_ax            = 2212  tt_stringF("ax",T_ax)
global constant T_cx            = 2220  tt_stringF("cx",T_cx)
global constant T_dx            = 2228  tt_stringF("dx",T_dx)
global constant T_bx            = 2236  tt_stringF("bx",T_bx)
global constant T_sp            = 2240  tt_stringF("sp",T_sp)
global constant T_bp            = 2244  tt_stringF("bp",T_bp)
global constant T_si            = 2248  tt_stringF("si",T_si)
global constant T_di            = 2252  tt_stringF("di",T_di)
global constant T_r8w           = 2260  tt_stringF("r8w",T_r8w)
global constant T_r9w           = 2268  tt_stringF("r9w",T_r9w)
global constant T_r10w          = 2276  tt_stringF("r10w",T_r10w)
global constant T_r11w          = 2284  tt_stringF("r11w",T_r11w)
global constant T_r12w          = 2292  tt_stringF("r12w",T_r12w)
global constant T_r13w          = 2300  tt_stringF("r13w",T_r13w)
global constant T_r14w          = 2308  tt_stringF("r14w",T_r14w)
global constant T_r15w          = 2316  tt_stringF("r15w",T_r15w)
global constant T_rax           = 2328  tt_stringF("rax",T_rax)
global constant T_rbx           = 2340  tt_stringF("rbx",T_rbx)
global constant T_rcx           = 2352  tt_stringF("rcx",T_rcx)
global constant T_rdx           = 2364  tt_stringF("rdx",T_rdx)
global constant T_rbp           = 2372  tt_stringF("rbp",T_rbp)
global constant T_rsp           = 2384  tt_stringF("rsp",T_rsp)
global constant T_rsi           = 2392  tt_stringF("rsi",T_rsi)
global constant T_rdi           = 2400  tt_stringF("rdi",T_rdi)
global constant T_r8            = 2404  tt_stringF("r8",T_r8)
global constant T_r9            = 2408  tt_stringF("r9",T_r9)
global constant T_r10           = 2412  tt_stringF("r10",T_r10)
global constant T_r11           = 2416  tt_stringF("r11",T_r11)
global constant T_r12           = 2420  tt_stringF("r12",T_r12)
global constant T_r13           = 2424  tt_stringF("r13",T_r13)
global constant T_r14           = 2428  tt_stringF("r14",T_r14)
global constant T_r15           = 2432  tt_stringF("r15",T_r15)

global constant T_lea           = 2444  tt_stringF("lea",T_lea)
global constant T_mov           = 2456  tt_stringF("mov",T_mov)
global constant T_add           = 2468  tt_stringF("add",T_add)
global constant T_adc           = 2476  tt_stringF("adc",T_adc)
global constant T_sbb           = 2488  tt_stringF("sbb",T_sbb)
global constant T_sub           = 2500  tt_stringF("sub",T_sub)
-- (T_and, T_or, T_xor already defined, using the same ttidx is fine)
global constant T_cmp           = 2512  tt_stringF("cmp",T_cmp)
global constant T_test          = 2528  tt_stringF("test",T_test)
global constant T_rol           = 2540  tt_stringF("rol",T_rol)
global constant T_ror           = 2548  tt_stringF("ror",T_ror)
global constant T_rcl           = 2556  tt_stringF("rcl",T_rcl)
global constant T_rcr           = 2564  tt_stringF("rcr",T_rcr)
global constant T_shl           = 2576  tt_stringF("shl",T_shl)
global constant T_shr           = 2584  tt_stringF("shr",T_shr)
global constant T_sar           = 2596  tt_stringF("sar",T_sar)
global constant T_inc           = 2600  tt_stringF("inc",T_inc)
global constant T_dec           = 2608  tt_stringF("dec",T_dec)
global constant T_push          = 2620  tt_stringF("push",T_push)
global constant T_pop           = 2632  tt_stringF("pop",T_pop)
global constant T_pushad        = 2644  tt_stringF("pushad",T_pushad)
global constant T_popad         = 2656  tt_stringF("popad",T_popad)
global constant T_call          = 2668  tt_stringF("call",T_call)
--DEV we need the opXxxx (as per iload)
global constant T_jo            = 2676  tt_stringF("jo",  T_jo)
global constant T_jno           = 2688  tt_stringF("jno", T_jno)
global constant T_jb            = 2696  tt_stringF("jb",  T_jb)
global constant T_jc            = 2704  tt_stringF("jc",  T_jc)
global constant T_jnae          = 2716  tt_stringF("jnae",T_jnae)
global constant T_jnb           = 2724  tt_stringF("jnb", T_jnb)
global constant T_jnc           = 2732  tt_stringF("jnc", T_jnc)
global constant T_jae           = 2744  tt_stringF("jae", T_jae)
global constant T_je            = 2752  tt_stringF("je",  T_je)
global constant T_jz            = 2760  tt_stringF("jz",  T_jz)
global constant T_jne           = 2768  tt_stringF("jne", T_jne)
global constant T_jnz           = 2776  tt_stringF("jnz", T_jnz)
global constant T_jbe           = 2784  tt_stringF("jbe", T_jbe)
global constant T_jna           = 2788  tt_stringF("jna", T_jna)
global constant T_jnbe          = 2796  tt_stringF("jnbe",T_jnbe)
global constant T_ja            = 2800  tt_stringF("ja",  T_ja)
global constant T_js            = 2808  tt_stringF("js",  T_js)
global constant T_jns           = 2816  tt_stringF("jns", T_jns)
global constant T_jp            = 2824  tt_stringF("jp",  T_jp)
global constant T_jpe           = 2832  tt_stringF("jpe", T_jpe)
global constant T_jnp           = 2840  tt_stringF("jnp", T_jnp)
global constant T_jpo           = 2848  tt_stringF("jpo", T_jpo)
global constant T_jl            = 2856  tt_stringF("jl",  T_jl)
global constant T_jnge          = 2868  tt_stringF("jnge",T_jnge)
global constant T_jnl           = 2876  tt_stringF("jnl", T_jnl)
global constant T_jge           = 2888  tt_stringF("jge", T_jge)
global constant T_jle           = 2896  tt_stringF("jle", T_jle)
global constant T_jng           = 2900  tt_stringF("jng", T_jng)
global constant T_jnle          = 2908  tt_stringF("jnle",T_jnle)
global constant T_jg            = 2912  tt_stringF("jg",  T_jg)
global constant T_jmp           = 2924  tt_stringF("jmp", T_jmp)
global constant T_ret           = 2928  tt_stringF("ret", T_ret)

global constant T_seto          = 2944  tt_stringF("seto",  T_seto)
global constant T_setno         = 2956  tt_stringF("setno", T_setno)
global constant T_setb          = 2964  tt_stringF("setb",  T_setb)
global constant T_setc          = 2972  tt_stringF("setc",  T_setc)
global constant T_setnae        = 2984  tt_stringF("setnae",T_setnae)
global constant T_setnb         = 2992  tt_stringF("setnb", T_setnb)
global constant T_setnc         = 3000  tt_stringF("setnc", T_setnc)
global constant T_setae         = 3012  tt_stringF("setae", T_setae)
global constant T_sete          = 3020  tt_stringF("sete",  T_sete)
global constant T_setz          = 3028  tt_stringF("setz",  T_setz)
global constant T_setne         = 3036  tt_stringF("setne", T_setne)
global constant T_setnz         = 3044  tt_stringF("setnz", T_setnz)
global constant T_setbe         = 3052  tt_stringF("setbe", T_setbe)
global constant T_setna         = 3056  tt_stringF("setna", T_setna)
global constant T_setnbe        = 3064  tt_stringF("setnbe",T_setnbe)
global constant T_seta          = 3068  tt_stringF("seta",  T_seta)
global constant T_sets          = 3076  tt_stringF("sets",  T_sets)
global constant T_setns         = 3084  tt_stringF("setns", T_setns)
global constant T_setp          = 3092  tt_stringF("setp",  T_setp)
global constant T_setpe         = 3100  tt_stringF("setpe", T_setpe)
global constant T_setnp         = 3108  tt_stringF("setnp", T_setnp)
global constant T_setpo         = 3116  tt_stringF("setpo", T_setpo)
global constant T_setl          = 3124  tt_stringF("setl",  T_setl)
global constant T_setnge        = 3136  tt_stringF("setnge",T_setnge)
global constant T_setnl         = 3144  tt_stringF("setnl", T_setnl)
global constant T_setge         = 3156  tt_stringF("setge", T_setge)
global constant T_setle         = 3164  tt_stringF("setle", T_setle)
global constant T_setng         = 3168  tt_stringF("setng", T_setng)
global constant T_setnle        = 3176  tt_stringF("setnle",T_setnle)
global constant T_setg          = 3180  tt_stringF("setg",  T_setg)

global constant T_fld           = 3192  tt_stringF("fld",  T_fld)
global constant T_fldpi         = 3204  tt_stringF("fldpi",T_fldpi)
global constant T_fstp          = 3220  tt_stringF("fstp", T_fstp)
global constant T_fild          = 3236  tt_stringF("fild", T_fild)
global constant T_fist          = 3248  tt_stringF("fist", T_fist)
global constant T_fistp         = 3256  tt_stringF("fistp",T_fistp)
global constant T_faddp         = 3272  tt_stringF("faddp",T_faddp)
--global constant T_fcomp       = 3272  tt_stringF("fcomp",T_fcomp)
global constant T_st0           = 3280  tt_stringF("st0",  T_st0)
global constant T_st1           = 3288  tt_stringF("st1",  T_st1)
global constant T_st2           = 3296  tt_stringF("st2",  T_st2)
global constant T_st3           = 3304  tt_stringF("st3",  T_st3)
global constant T_st4           = 3312  tt_stringF("st4",  T_st4)
global constant T_st5           = 3320  tt_stringF("st5",  T_st5)
global constant T_st6           = 3328  tt_stringF("st6",  T_st6)
global constant T_st7           = 3336  tt_stringF("st7",  T_st7)

global constant T_rep           = 3344  tt_stringF("rep", T_rep)
global constant T_movsb         = 3356  tt_stringF("movsb",T_movsb)
global constant T_movsd         = 3364  tt_stringF("movsd",T_movsd)
global constant T_lodsd         = 3384  tt_stringF("lodsd",T_lodsd)
global constant T_stosd         = 3400  tt_stringF("stosd",T_stosd)
global constant T_lodsb         = 3408  tt_stringF("lodsb",T_lodsb)
global constant T_stosb         = 3416  tt_stringF("stosb",T_stosb)
global constant T_cwde          = 3432  tt_stringF("cwde", T_cwde)
global constant T_cdq           = 3444  tt_stringF("cdq", T_cdq)
global constant T_rdtsc         = 3460  tt_stringF("rdtsc", T_rdtsc)
global constant T_cpuid         = 3480  tt_stringF("cpuid", T_cpuid)
global constant T_nop           = 3488  tt_stringF("nop", T_nop)

global constant T_h4            = 3500  tt_stringF("h4",T_h4)

global constant T_fs            = 3504  tt_stringF("fs",T_fs)
-- ("fs" is a segment register occasionally used in exception handling and thread local storage, btw)

global constant T_format        = 3520  tt_stringF("format",T_format)
global constant T_PE32          = 3540  tt_stringF("PE32",T_PE32)
global constant T_PE64          = 3552  tt_stringF("PE64",T_PE64)
global constant T_ELF32         = 3576  tt_stringF("ELF32",T_ELF32)
global constant T_ELF64         = 3588  tt_stringF("ELF64",T_ELF64)
global constant T_DLL           = 3600  tt_stringF("DLL",T_DLL)
global constant T_SO            = 3608  tt_stringF("SO",T_SO)
global constant T_GUI           = 3624  tt_stringF("GUI",T_GUI)
global constant T_CONSOLE       = 3652  tt_stringF("CONSOLE",T_CONSOLE)
global constant T_icons         = 3672  tt_stringF("icons",T_icons)
global constant T_version       = 3704  tt_stringF("version",T_version)
global constant T_manifest      = 3732  tt_stringF("manifest",T_manifest)

--global constant T_at          = 3312  tt_stringF("at",T_at)
--global constant T_on          = 3312  tt_stringF("on",T_on)

global constant T_jmp_table_entry=3784  tt_stringF("jmp_table_entry",T_jmp_table_entry)   -- special for jump table encoding

global constant T_db            = 3792  tt_stringF("db",T_db)
global constant T_dw            = 3796  tt_stringF("dw",T_dw)
global constant T_du            = 3804  tt_stringF("du",T_du)
global constant T_dd            = 3812  tt_stringF("dd",T_dd)
global constant T_dq            = 3820  tt_stringF("dq",T_dq)
global constant T_idiv          = 3836  tt_stringF("idiv",T_idiv)
global constant T_tbyte         = 3856  tt_stringF("tbyte",T_tbyte)             -- I'm with OllyDbg here [== misnamed "tword" of fasm]
global constant T_tword         = 3876  tt_stringF("tword",T_tword)             -- (used for error reporting only)

global constant T_fcomp         = 3896  tt_stringF("fcomp",T_fcomp)
global constant T_fnstsw        = 3920  tt_stringF("fnstsw",T_fnstsw)
global constant T_sahf          = 3932  tt_stringF("sahf",T_sahf)

global constant T_ds            = 3940  tt_stringF("ds",T_ds)
-- ("ds" is the raw data section, used to obtain symtabptr etc. See pemit2.e)
global constant T_std           = 3948  tt_stringF("std",T_std)
global constant T_cld           = 3956  tt_stringF("cld",T_cld)
global constant T_int           = 3964  tt_stringF("int",T_int)
global constant T_int3          = 3972  tt_stringF("int3",T_int3)
global constant T_stosq         = 3980  tt_stringF("stosq",T_stosq)
global constant T_syscall       = 4008  tt_stringF("syscall",T_syscall)
global constant T_ilASM         = 4024  tt_stringF("ilASM",T_ilASM)
global constant T_lock          = 4036  tt_stringF("lock",T_lock)
global constant T_cmpxchg       = 4056  tt_stringF("cmpxchg",T_cmpxchg)
global constant T_fninit        = 4076  tt_stringF("fninit",T_fninit)
global constant T_fstcw         = 4088  tt_stringF("fstcw",T_fstcw)
global constant T_fnstcw        = 4100  tt_stringF("fnstcw",T_fnstcw)
global constant T_fldcw         = 4112  tt_stringF("fldcw",T_fldcw)
global constant T_movsq         = 4120  tt_stringF("movsq",T_movsq)
global constant T_lodsq         = 4128  tt_stringF("lodsq",T_lodsq)
global constant T_xchg          = 4144  tt_stringF("xchg",T_xchg)
global constant T_fcompp        = 4152  tt_stringF("fcompp",T_fcompp)
global constant T_repe          = 4160  tt_stringF("repe",T_repe)
global constant T_repne         = 4172  tt_stringF("repne",T_repne)
global constant T_cmpsb         = 4184  tt_stringF("cmpsb",T_cmpsb)
global constant T_cmpsd         = 4192  tt_stringF("cmpsd",T_cmpsd)
global constant T_shld          = 4200  tt_stringF("shld",T_shld)
global constant T_cdqe          = 4208  tt_stringF("cdqe",T_cdqe)
global constant T_cbw           = 4220  tt_stringF("cbw",T_cbw)
global constant T_cwd           = 4224  tt_stringF("cwd",T_cwd)
global constant T_neg           = 4236  tt_stringF("neg",T_neg)
global constant T_fchs          = 4248  tt_stringF("fchs",T_fchs)
global constant T_fsubp         = 4264  tt_stringF("fsubp",T_fsubp)
global constant T_fdivp         = 4284  tt_stringF("fdivp",T_fdivp)
global constant T_frndint       = 4312  tt_stringF("frndint",T_frndint)
global constant T_imul          = 4328  tt_stringF("imul",T_imul)
global constant T_fmulp         = 4348  tt_stringF("fmulp",T_fmulp)
global constant T_cqo           = 4360  tt_stringF("cqo",T_cqo)
global constant T_div           = 4368  tt_stringF("div",T_div)
global constant T_mul           = 4380  tt_stringF("mul",T_mul)
global constant T_fxch          = 4396  tt_stringF("fxch",T_fxch)
global constant T_fprem         = 4416  tt_stringF("fprem",T_fprem)
global constant T_fldz          = 4424  tt_stringF("fldz",T_fldz)
global constant T_fyl2x         = 4444  tt_stringF("fyl2x",T_fyl2x)
global constant T_fsub          = 4448  tt_stringF("fsub",T_fsub)
global constant T_f2xm1         = 4468  tt_stringF("f2xm1",T_f2xm1)
global constant T_fld1          = 4476  tt_stringF("fld1",T_fld1)
global constant T_fscale        = 4496  tt_stringF("fscale",T_fscale)
global constant T_fabs          = 4508  tt_stringF("fabs",T_fabs)
global constant T_fcom          = 4512  tt_stringF("fcom",T_fcom)
global constant T_fcos          = 4520  tt_stringF("fcos",T_fcos)
global constant T_fsin          = 4532  tt_stringF("fsin",T_fsin)
global constant T_fptan         = 4548  tt_stringF("fptan",T_fptan)
global constant T_fpatan        = 4568  tt_stringF("fpatan",T_fpatan)
global constant T_fldl2e        = 4584  tt_stringF("fldl2e",T_fldl2e)
global constant T_fsqrt         = 4600  tt_stringF("fsqrt",T_fsqrt)
global constant T_fadd          = 4604  tt_stringF("fadd",T_fadd)
global constant T_lodsw         = 4612  tt_stringF("lodsw",T_lodsw)
global constant T_fst           = 4616  tt_stringF("fst",T_fst)
global constant T_stosw         = 4624  tt_stringF("stosw",T_stosw)
global constant T_prefetchnta   = 4660  tt_stringF("prefetchnta",T_prefetchnta)
global constant T_movups        = 4676  tt_stringF("movups",T_movups)
global constant T_xmm0          = 4692  tt_stringF("xmm0",T_xmm0)
global constant T_xmm1          = 4700  tt_stringF("xmm1",T_xmm1)
global constant T_xmm2          = 4708  tt_stringF("xmm2",T_xmm2)
global constant T_xmm3          = 4716  tt_stringF("xmm3",T_xmm3)
global constant T_xmm4          = 4724  tt_stringF("xmm4",T_xmm4)
global constant T_xmm5          = 4732  tt_stringF("xmm5",T_xmm5)
global constant T_xmm6          = 4740  tt_stringF("xmm6",T_xmm6)
global constant T_xmm7          = 4748  tt_stringF("xmm7",T_xmm7)
global constant T_movntps       = 4768  tt_stringF("movntps",T_movntps)
global constant T_movaps        = 4784  tt_stringF("movaps",T_movaps)
global constant T_sfence        = 4808  tt_stringF("sfence",T_sfence)
global constant T_Ltot          = 4824  tt_stringF("Ltot",T_Ltot)
global constant T_il            = 4828  tt_stringF("il",T_il)
global constant T_fmul          = 4832  tt_stringF("fmul",T_fmul)
global constant T_override      = 4864  tt_stringF("override",T_override)   -- banned forever!!
global constant T_iff           = 4872  tt_stringF("iff",T_iff)
global constant T_iif           = 4884  tt_stringF("iif",T_iif)
global constant T_EU4_1         = 4904  tt_stringF("EU4_1",T_EU4_1)
global constant T_fiadd         = 4920  tt_stringF("fiadd",T_fiadd)
global constant T_fimul         = 4936  tt_stringF("fimul",T_fimul)
global constant T_ficom         = 4952  tt_stringF("ficom",T_ficom)
global constant T_ficomp        = 4960  tt_stringF("ficomp",T_ficomp)
global constant T_fisub         = 4972  tt_stringF("fisub",T_fisub)
global constant T_fisubr        = 4980  tt_stringF("fisubr",T_fisubr)
global constant T_fidiv         = 4996  tt_stringF("fidiv",T_fidiv)
global constant T_fidivr        = 5004  tt_stringF("fidivr",T_fidivr)
global constant T_bswap         = 5024  tt_stringF("bswap",T_bswap)
global constant T_BITS32        = 5052  tt_stringF("BITS32",T_BITS32)
global constant T_BITS64        = 5064  tt_stringF("BITS64",T_BITS64)
global constant T_DLLMAIN       = 5092  tt_stringF("DllMain",T_DLLMAIN)
global constant T_movd          = 5100  tt_stringF("movd",T_movd)
global constant T_PHIX          = 5116  tt_stringF("PHIX",T_PHIX)
global constant T_indirect_incs = 5180  tt_stringF("indirect_includes",T_indirect_incs) -- (ignored)
global constant T_inline        = 5200  tt_stringF("inline",T_inline)                   -- (ignored)
global constant T_try           = 5208  tt_stringF("try",T_try)
global constant T_catch         = 5224  tt_stringF("catch",T_catch)
--global constant T_throw       = 5240  tt_stringF("throw",T_throw)     -- does not belong here!
global constant T_bsr           = 5232  tt_stringF("bsr",T_bsr)
global constant T_shrd          = 5240  tt_stringF("shrd",T_shrd)
global constant T_EU4_0         = 5248  tt_stringF("EU4_0",T_EU4_0)
global constant T_bt            = 5256  tt_stringF("bt",T_bt)
global constant T_rdrand        = 5276  tt_stringF("rdrand",T_rdrand)
global constant T_bsf           = 5284  tt_stringF("bsf",T_bsf)
global constant T_struct        = 5300  tt_stringF("struct",T_struct)
global constant T_abstract      = 5332  tt_stringF("abstract",T_abstract)
global constant T_class         = 5348  tt_stringF("class",T_class)
global constant T_extends       = 5372  tt_stringF("extends",T_extends)
global constant T_this          = 5384  tt_stringF("this",T_this)
global constant T_virtual       = 5412  tt_stringF("virtual",T_virtual)
global constant T_final         = 5428  tt_stringF("final",T_final)
global constant T_private       = 5452  tt_stringF("private",T_private)
global constant T_dynamic       = 5480  tt_stringF("dynamic",T_dynamic)
global constant T_nullable      = 5512  tt_stringF("nullable",T_nullable)
global constant T_static        = 5532  tt_stringF("static",T_static)
global constant T_Icallback     = 5572  tt_stringF("Icallback",T_Icallback)
global constant T_Icallbacki    = 5580  tt_stringF("Icallbacki",T_Icallbacki)
global constant T_until         = 5604  tt_stringF("until",T_until)
global constant T_goto          = 5620  tt_stringF("goto",T_goto)
global constant T_javascript    = 5656  tt_stringF("javascript",T_javascript)
global constant T_js_semantics  = 5700  tt_stringF("javascript_semantics",T_js_semantics)

--          elsif ttidx=T_indirect_includes
--             or ttidx=T_inline then

--global constant T_DLLATTACH       = 5098  tt_stringF("DLL_PROCESS_ATTACH",T_DLLATTACH)
--global constant T_movq            = 4912  tt_stringF("movq",T_movq)
--global constant gtiTestM  = T_ilASM+0     tt_glabel("%opTestM", gtiTestM)
--global constant gtiopTestM        = 4020  tt_glabel("%opTestM", gtiopTestM)
--global constant gtiopFrame        = 4044  tt_glabel("%opFrame", gtiopFrame)
--global constant gtiopRetf         = 4064  tt_glabel("%opRetf",  gtiopRetf)
--global constant gtiopCallOnce = 4100  tt_glabel("%opCallOnce", gtiopCallOnce)

--(from rosetta/"REAL" calendar)
--tt_stringA("INCLUDE",T_include)

--,T_dump_listing = tt_stringf("dump_listing")
--,T_f = tt_stringf("f")

sequence ilstuff                -- for ilasm().
         ilstuff = {}

-- ilstuff[tt[ttidx+EQ]] holds:
--  integer for a single output byte, or
--  sequence s: integer(s[1]) for a single output byte, or
--              sequence(s[1]) for multiple bytes, plus optionally
--              integer(s[2]), number of bytes to follow.
--  an iltuff[i] of int is equivalent to {int}, former preffered.

-- condition codes:
--constant cc = { {"o"},                -- 0
--              {"no"},             -- 1
--              {"b","c","nae"},    -- 2
--              {"nb","nc","ae"},   -- 3
--              {"e","z"},          -- 4
--              {"ne","nz"},        -- 5
--              {"be","na"},        -- 6
--              {"nbe","a"},        -- 7
--              {"s"},              -- 8
--              {"ns"},             -- 9
--              {"p","pe"},         -- A
--              {"np","po"},        -- B
--              {"l","nge"},        -- C
--              {"nl","ge"},        -- D
--              {"le","ng"},        -- E
--              {"nle","g"}}        -- F

constant regs = {"eax","ecx","edx","ebx","esp","ebp","esi","edi"}
              --    0     1     2     3     4     5     6     7
--constant r8   = { "al", "cl", "dl", "bl", "ah", "ch", "dh", "bh"}

integer iidx
--integer DIAG
--      DIAG=0
procedure iload(sequence txt, object datum)
--if equal(txt,"ebx_esi4") then trace(1) end if
--if DIAG then
--  printf(1,"%s  ",{txt})
--  ?datum
--end if
    tt_string(txt,-3)
    ilstuff = append(ilstuff,datum)
    iidx = ttidx+EQ
    if tt[iidx] then                -- BUG! (duplicate iload?)
        puts(1,"\n\n** ERROR ** pttree.e/iload():txt already exists\n\n")
        iidx = tt[iidx]
        iidx = ilstuff[iidx]        -- type error will do nicely
        datum = 9/0
    end if
    tt[iidx] = length(ilstuff)
end procedure

--DEV ilASM (and something for jmp)
--procedure ilset(sequence txt, integer cc)
--  tt_string(txt,-2)
--  tt[ttidx+EQ] = cc
--end procedure

--procedure init_iL()
--sequence ci --, cj
--  for i=1 to length(cc) do
--      ci = cc[i]
--      for j=1 to length(ci) do
----            cj = ci[j]
----            ilset("j"&cj,i-1)   -- 0o017 (0o200..0o217) imm32       -- eg jle
--          tt_string("j"&ci[j],-2)
--          tt[ttidx+EQ] = i-1
--      end for
--  end for
--  tt_string("jmp",-2)
--  tt[ttidx+EQ] = #10
--end procedure
--init_iL()

procedure init_il()
-- load ternary tree with #ilasm keywords (terminator of -3)
-- (only invoked when the compiler first hits a #ilasm construct)
--sequence ci, cj, rs, rd
sequence rs
--integer xrm,
--      xrw     -- work/base version of xrm. nb eg xrm = xrw+#40 is better than xrm+=#40
--object name

--/*
    iload("e_all",0)
    -- jcc_rel32 and setcc:
    for i=1 to length(cc) do
        ci = cc[i]
        for j=1 to length(ci) do
            cj = ci[j]
            iload("j"&cj&"_rel32",{{#0F,#7F+i},4})  -- 0o017 (0o200..0o217) imm32       -- eg jle_rel32
            iload("set"&cj,{{#0F,#8F+i},1})         -- 0o017 (0o220..0o237) 0o30r       -- eg setl r8
        end for
    end for
    for src=1 to 8 do   -- eax to edi (as idx to regs/r8)
        rs = regs[src]
        -- two reg opcodes:
        for dst=1 to 8 do   -- eax to edi (as idx to regs/r8)
            rd = regs[dst]
            --   (#B7==0o267==0o300-0o011)
            xrm = #B7+dst*8+src                     -- 0o3rr
            iload("add_"&rs&"_"&rd,{{#01,xrm}})     -- 0o001 3rm    -- eg add_eax_edx   (ie eax+=edx)
            iload( "or_"&rs&"_"&rd,{{#09,xrm}})     -- 0o011 3rm    -- eg  or_ecx_edx
            iload("adc_"&rs&"_"&rd,{{#11,xrm}})     -- 0o021 3rm    -- eg adc_edi_edx
            iload("sbb_"&rs&"_"&rd,{{#19,xrm}})     -- 0o031 3rm    -- eg sbb_esi_ecx
            iload("and_"&rs&"_"&rd,{{#21,xrm}})     -- 0o041 3rm    -- eg and_edx_eax
            iload("sub_"&rs&"_"&rd,{{#29,xrm}})     -- 0o051 3rm    -- eg sub_eax_esi
            iload("xor_"&rs&"_"&rd,{{#31,xrm}})     -- 0o061 3rm    -- eg xor_eax_eax
            iload("cmp_"&rs&"_"&rd,{{#39,xrm}})     -- 0o071 3rm    -- eg cmp_edi_edx
            iload("test_"&rs&"_"&rd,{{#85,xrm}})    -- 0o205 3rm    -- eg test_ecx_ecx
            iload("mov_"&rd&"_"&rs,{{#8B,xrm}})     -- 0o213 3rm    -- eg mov_edi_edx   (ie edi:=edx)
          if src!=5 then    -- not esp (treated as one reg opcode below)
            xrw = dst*8-8+src-1                     -- 0o0ds
            xrm = xrw
           if src!=6 then   -- not [ebp] (use [ebp+imm8=0] instead)
            iload("mov_m"&rs&"_"&rd,{{#89,xrm}})    -- 0o211 0rm    -- eg mov_medx_eax  (for mov [edx],eax)
           end if
           if dst!=6 then   -- not [ebp] (use [ebp+imm8=0] instead)
            iload("cmp_"&rd&"_m"&rs,{{#3B,xrm}})    -- 0o073 0rm    -- eg cmp_edx_meax  (for cmp edx,[eax])
            iload("mov_"&rd&"_m"&rs,{{#8B,xrm}})    -- 0o213 0rm    -- eg mov_edx_meax  (for mov edx,[eax])
           end if

            xrm = xrw+#40                           -- 0o1ds
            iload("mov_"&rs&"d8_"&rd,{{#89,xrm},1}) -- 0o211 0o1ds d8       -- eg mov_ebpd8_edi (for mov [ebp+d8],edi)
           if dst!=5 then   -- not [esp+d8] (treated as one reg opcode below)
            iload("cmp_"&rd&"_"&rs&"d8",{{#3B,xrm},1}) -- 0o073 0o1ds d8    -- eg cmp_eax_esid8 (for cmp eax,[esi+d8])
            iload("mov_"&rd&"_"&rs&"d8",{{#8B,xrm},1}) -- 0o213 0o1ds d8    -- eg mov_ebx_ebxd8 (for mov ebx,[ebx+d8])
           end if
            iload("lea_"&rd&"_"&rs&"d8",{{#8D,xrm},1}) -- 0o215 0o1ds d8    -- eg lea_ebx_ebpd8 (for lea ebx,[ebp+d8])
            xrm = xrw+#80                           -- 0o2ds
            -- nb: the [ebp] versions of these [may] need special treatment in ilasm().
            iload("mov_"&rs&"d32_"&rd,{{#89,xrm},1}) -- 0o211 0o2ds d32 -- eg mov_ebpd32_ebx (for mov [ebp+d32],ebx)
           if dst!=5 then   -- not [esp+d8] (treated as one reg opcode below)
            iload("mov_"&rd&"_"&rs&"d32",{{#8B,xrm},4}) -- 0o213 0o2ds d32  -- eg mov_ecx_ebpd32 (for mov ecx,[ebp+d32])
           end if
            iload("lea_"&rd&"_"&rs&"d32",{{#8D,xrm},4}) -- 0o215 0o2ds d32  -- eg lea_ebx_ebpd32 (for lea ebx,[ebp+d32])
          end if
--DEV some of these are probably not valid... (si5,s4b)
            -- sib bytes:
          if src!=5 then    -- cannot use esp as idx!
            -- nb 0r4 si5 has special meaning, as does xr4 04b... [DEV rename then?]
            xrm = src*8-8+dst-1
            iload(rd&"_"&rs,xrm)                    -- 0o0ib    -- eg esi_ebx  (for [esi+ebx])
            iload(rd&"_"&rs&"4",#80+xrm)            -- 0o2ib    -- eg ebx_eax4 (for [ebx+eax*4])
          end if
            -- 8-bit opcodes:
          if dst<=4 and src<=4 then
            rs = r8[src]
            rd = r8[dst]
            xrm = #B7+dst*8+src                     -- 0o3rr
            iload("mov_"&rd&"_"&rs,{{#8A,xrm}})     -- 0o212 3rm    -- eg mov_cl_dl (ie cl:=dl)

            rs = regs[src]
          end if
        end for
        -- one byte opcodes:
        iload("inc_"&rs,#3F+src)                    -- 0o38r    -- eg inc_ecx
        iload("dec_"&rs,#47+src)                    -- 0o11r    -- eg dec_edi
        iload("push_"&rs,#4F+src)                   -- 0o12r    -- eg push_eax
        iload("pop_"&rs,#57+src)                    -- 0o13r    -- eg pop_esi
        iload("mov_"&rs&"_imm32",{#B7+src,4})       -- 0o27r    -- eg mov_edi_imm32
        -- one reg opcodes:
       if src!=1 then   -- not eax (shorter below)
        iload("add_" &rs&"_imm32",{{#81,#BF+src},4}) -- 0o201 0o30r imm32   -- eg add_ecx_imm32
        iload( "or_" &rs&"_imm32",{{#81,#C7+src},4}) -- 0o201 0o31r imm32   -- eg  or_edx_imm32
        iload("adc_" &rs&"_imm32",{{#81,#CF+src},4}) -- 0o201 0o32r imm32   -- eg adc_edi_imm32
        iload("sbb_" &rs&"_imm32",{{#81,#D7+src},4}) -- 0o201 0o33r imm32   -- eg sbb_esi_imm32
        iload("and_" &rs&"_imm32",{{#81,#DF+src},4}) -- 0o201 0o34r imm32   -- eg and_ecx_imm32
        iload("sub_" &rs&"_imm32",{{#81,#E7+src},4}) -- 0o201 0o35r imm32   -- eg sub_esi_imm32
        iload("xor_" &rs&"_imm32",{{#81,#EF+src},4}) -- 0o201 0o36r imm32   -- eg xor_edx_imm32
        iload("cmp_" &rs&"_imm32",{{#81,#F7+src},4}) -- 0o201 0o37r imm32   -- eg cmp_edi_imm32
        iload("test_"&rs&"_imm32",{{#F7,#BF+src},4}) -- 0o367 0o30r imm32   -- eg test_edi_imm32
       end if

      if src!=5 then    -- not esp
        iload("add_d"&rs&"d8_i8",{{#83,#3F+src},2}) -- 0o203 0o10r d8 i8 -- eg add_dedid8_i8 (=add dword[esi+d8],i8)
        iload( "or_d"&rs&"d8_i8",{{#83,#47+src},2}) -- 0o203 0o11r d8 i8 -- eg  or_deaxd8_i8
        iload("adc_d"&rs&"d8_i8",{{#83,#4F+src},2}) -- 0o203 0o12r d8 i8 -- eg adc_decxd8_i8
        iload("sbb_d"&rs&"d8_i8",{{#83,#57+src},2}) -- 0o203 0o13r d8 i8 -- eg sbb_desid8_i8
        iload("and_d"&rs&"d8_i8",{{#83,#5F+src},2}) -- 0o203 0o14r d8 i8 -- eg and_dedxd8_i8
        iload("sub_d"&rs&"d8_i8",{{#83,#67+src},2}) -- 0o203 0o15r d8 i8 -- eg sub_deaxd8_i8
        iload("xor_d"&rs&"d8_i8",{{#83,#6F+src},2}) -- 0o203 0o16r d8 i8 -- eg xor_desid8_i8
        iload("cmp_d"&rs&"d8_i8",{{#83,#77+src},2}) -- 0o203 0o17r d8 i8 -- eg cmp_dedxd8_i8
      end if

        iload("add_"&rs&"_imm8",{{#83,#BF+src},1})  -- 0o203 0o30r imm8 -- eg add_edi_imm8
        iload( "or_"&rs&"_imm8",{{#83,#C7+src},1})  -- 0o203 0o31r imm8 -- eg  or_eax_imm8
        iload("adc_"&rs&"_imm8",{{#83,#CF+src},1})  -- 0o203 0o32r imm8 -- eg adc_ecx_imm8
        iload("sbb_"&rs&"_imm8",{{#83,#D7+src},1})  -- 0o203 0o33r imm8 -- eg sbb_esi_imm8
        iload("and_"&rs&"_imm8",{{#83,#DF+src},1})  -- 0o203 0o34r imm8 -- eg and_edx_imm8
        iload("sub_"&rs&"_imm8",{{#83,#E7+src},1})  -- 0o203 0o35r imm8 -- eg sub_eax_imm8
        iload("xor_"&rs&"_imm8",{{#83,#EF+src},1})  -- 0o203 0o36r imm8 -- eg xor_esi_imm8
        iload("cmp_"&rs&"_imm8",{{#83,#F7+src},1})  -- 0o203 0o37r imm8 -- eg cmp_edx_imm8

        xrw = #04+src*8-8                                 -- 0o0r4
       if src!=1 then   -- not eax (shorter below)
        xrm = xrw+1                                       -- 0o0r5
        iload("mov_mem32_"&rs,{{#89,xrm},4})        -- 0o211 0o0r5 m32 -- eg mov_mem32_esi  (for mov [m32],esi)
        iload("mov_"&rs&"_mem32",{{#8B,xrm},4})     -- 0o213 0o0r5 m32 -- eg mov_edx_mem32  (for mov edx,[m32])
       end if
        xrm = xrw                                         -- 0o0r4
        iload("mov_sib_"&rs,{{#89,xrm},1})          -- 0o211 0o0r4 0osib -- mov_sib_ecx (for mov [b+i*s],ecx)
        iload("mov_"&rs&"_sib",{{#8B,xrm},1})       -- 0o213 0o0r4 0osib -- mov_ecx_sib (for mov ecx,[b+i*s])
        iload("mov_"&rs&"_mesp",{{#8B,xrm,#24}})    -- 0o213 0o0r4 0o044 -- mov_ebx_mesp for mov ebx,[esp]

        xrm = xrw+#40                                     -- 0o1r4
        iload("add_"&rs&"_sibd8",{{#03,xrm},2})     -- 0o003 0o1r4 sib imm8             (for add ebx,[b+i*s+d8])
        iload( "or_"&rs&"_sibd8",{{#0B,xrm},2})     -- 0o013 0o1r4 sib imm8             (for  or ebx,[b+i*s+d8])
        iload("adc_"&rs&"_sibd8",{{#13,xrm},2})     -- 0o023 0o1r4 sib imm8             (for adc ebx,[b+i*s+d8])
        iload("sbb_"&rs&"_sibd8",{{#1B,xrm},2})     -- 0o033 0o1r4 sib imm8             (for sbb ebx,[b+i*s+d8])
        iload("and_"&rs&"_sibd8",{{#23,xrm},2})     -- 0o043 0o1r4 sib imm8             (for and ebx,[b+i*s+d8])
        iload("sub_"&rs&"_sibd8",{{#2B,xrm},2})     -- 0o053 0o1r4 sib imm8             (for sub ebx,[b+i*s+d8])
        iload("xor_"&rs&"_sibd8",{{#33,xrm},2})     -- 0o063 0o1r4 sib imm8             (for xor ebx,[b+i*s+d8])
        iload("cmp_"&rs&"_sibd8",{{#3B,xrm},2})     -- 0o073 0o1r4 sib imm8             (for cmp ebx,[b+i*s+d8])
        iload("mov_sibd8_"&rs,{{#89,xrm},2})        -- 0o211 0o1r4 sib d8  eg mov_sibd8_edi (for mov [b+i*s+d8],edi)
        iload("mov_"&rs&"_sibd8",{{#8B,xrm},2})     -- 0o213 0o1r4 sib d8  eg mov_edi_sibd8 (for mov edi,[b+i*s+d8])

        iload("mov_espd8_"&rs,{{#89,xrm,#24},1})    -- 0o211 0o1r4 0o044 d8             (for mov [esp+d8],ebx)
        iload("mov_"&rs&"_espd8",{{#8B,xrm,#24},1}) -- 0o213 0o1r4 0o044 d8             (for mov ebx,[esp+d8])
        iload("lea_"&rs&"_sibd8",{{#8D,xrm},2})     -- 0o215 0o1r4 sib d8               (for lea ecx,[b+i*s+d8])
        iload("lea_"&rs&"_espd8",{{#8D,xrm,#24},1}) -- 0o215 0o1r4 0o044 i8             (for lea ebx,[esp+d8])
--DEV d32 versions of above (untried)
--      xrm = xrw+#80                               -- 0o2r4
--      iload("mov_sib_d32_"&rs,{{#89,xrm},5})      -- 0o211 0o2r4 o0sib d32 -- mov_sib_d32_edi (for mov [b+i*s+d32],edi)
--      iload("mov_"&rs&"_sib_d32",{{#8B,xrm},5})   -- o0213 0o2r4 0osib d32 -- mov_edi_sib_d32 (for mov edi,[b+i*s+d32])
--      iload("mov_espd32_"&rs,{{#89,xrm,#24},4})   -- 0o211 0o1r4 0o044 d32            (for mov [esp+d32],ebx)
--      iload("mov_"&rs&"_espd32",{{#8B,xrm,#24},4}) -- o213 0o1r4 0o044 d32            (for mov ebx,[esp+d32])

--done above!
--      xrm = xrw+#41                               -- 0o1r5
--      iload("mov_ebpd8_"&rs,{{#89,xrm},1})        -- 0o211 0o1r5 d8                   (for mov [ebp+d8],eax)
--      xrm = xrw+#81                               -- 0o2r5
--      iload("mov_ebpd32_"&rs,{{#89,xrm},1})       -- 0o211 0o2r5 d32                  (for mov [ebp+d32],eax)

        iload("ror_"&rs&"_imm8",{{#C1,#C7+src},1})  -- 0o301 0o31r imm8 -- ror eax,imm8
        iload("shl_"&rs&"_imm8",{{#C1,#DF+src},1})  -- 0o301 0o34r imm8 -- shl eax,imm8
        iload("shr_"&rs&"_imm8",{{#C1,#E7+src},1})  -- 0o301 0o35r imm8 -- shr eax,imm8
        iload("shl_"&rs&"_1",{{#D1,#DF+src}})       -- 0o321 0o34r      -- shl eax,1

      if src!=5 then    -- not esp
        xrm = src-1                         -- 0o00r
        iload("fild_word_"&rs,{{#DF,xrm}})          -- 0o337 0o00r  -- eg fild_word_esi for fild word[esi]
        iload("fild_dword_"&rs,{{#DB,xrm}})         -- 0o333 0o00r  -- eg fild_dword_esi for fild dword[esi]
       if src!=6 then   -- not ebp (use imm8 of 0)
        iload("mov_word_m"&rs,{{#66,#C7,xrm},2})    -- 0o146 0o307 0o00r imm16      (<word prefix> mov word[esi],imm16)
        iload("mov_dword_m"&rs,{{#C7,xrm},4})       -- 0o307 0o00r imm32                          (mov dword[esi],imm32)
        xrm = #17+src                       -- 0o03r
        iload("fistp_dword_"&rs,{{#DB,xrm}})        -- 0o333 0o03r  -- eg fistp_dword_esi for fisp dword[esi]
        xrm = #37+src                       -- 0o07r
        iload("fistp_qword_"&rs,{{#DF,xrm}})        -- 0o337 0o07r  -- eg fistp_qword_esi for fisp qword[esi]
       end if
        xrm = #3F+src                       -- 0o10r
        iload("mov_"&rs&"d8_imm32",{{#C7,xrm},5})   -- 0o307 0o10r d8 imm32     -- mov [reg+d8],imm32

        xrm = #27+src                       -- 0o05r
        iload("fild_qword_"&rs,{{#DF,xrm}})         -- 0o337 0o05r  -- eg fild_qword_eax for fild dword[eax]
        xrm = #3F+src                       -- 0o10r
        iload("fild_dword_"&rs&"d8",{{#DB,xrm},1})  -- 0o333 0o10r d8   -- eg fild_dword_esid8 for fild dword[esi+d8]
        xrm = #57+src                       -- 0o13r
        iload("fistp_dword_"&rs&"d8",{{#DB,xrm},1}) -- 0o333 0o13r d8   -- eg fistp_dword_esid8 for fild dword[esi+d8]
        xrm = #67+src                       -- 0o15r
        iload("fild_qword_"&rs&"d8",{{#DF,xrm},1})  -- 0o337 0o15r d8   -- eg fild_qword_esid8 for fistp qword[esi+d8]
        xrm = #77+src                       -- 0o17r
        iload("fistp_qword_"&rs&"d8",{{#DF,xrm},1}) -- 0o337 0o17r d8   -- eg fistp_qword_esid8 for fild qword[esi+d8]
      end if

        xrm = #0F+src                       -- 0o02r
        iload("fstp_m64_m_"&rs,{{#DD,xrm}})         -- 00335 0o02r      -- fstp qword[edx]

      if src!=5 then    -- not esp
        xrm = #3F+src                       -- 0o10r
        iload("test_byte_" &rs&"d8",{{#F6,xrm},2})  -- 0o366 0o10r d8 imm8  -- eg test_byte_eaxd8 for test byte[eax+d8],imm8
        iload("test_dword_"&rs&"d8",{{#F7,xrm},5})  -- 0o367 0o10r d8 imm32 -- eg test_dword_esid8 for test dword[esi+d8],imm32 
      end if

        xrm = #CF+src                       -- 0o32r
--DIAG=1
        iload("call_"&rs,{{#FF,xrm}})               -- 0o377 0o32r      -- call reg
--DIAG=0
--      xrm = #E0+src                       -- 0o34r
        xrm = #DF+src                       -- 0o34r
        iload("jmp_"&rs,{{#FF,xrm}})                -- 0o377 0o34r      -- jmp reg

        if src!=6 then  -- not [ebp] (use [ebp+imm8=0] instead)
--bugfix 27/5/2013:
--          xrm = #30+src                   -- 0o06r
            xrm = #2F+src                   -- 0o06r
            iload("push_m"&rs,{{#FF,xrm}})          -- 0o377 0o06r      -- eg push_mesi for push dword[esi]
        end if
--      xrm = #70+src                       -- 0o16r
        xrm = #6F+src                       -- 0o16r
        iload("push_m"&rs&"_d8",{{#FF,xrm},1})      -- 0o377 0o16r imm8 -- eg push_mesi_d8 for push dword[esi+d8]

        -- 8-bit opcodes:
        rs = r8[src]
        iload("mov_"&rs&"_imm8",{#AF+src,1})        -- 0o26r    -- eg mov_al_imm8
        xrm = xrw                                   -- 0o0r4
        iload("mov_"&rs&"_sib",{{#8A,xrm},1})       -- 0o212 0o0r4 sib      -- eg mov_al_sib
        xrm = xrw+#40                               -- 0o1r4
        iload("mov_sibd8_"&rs,   {{#88,xrm},2})     -- 0o210 0o1r4 sib d8   -- eg mov_sibd8_dl
        iload("mov_"&rs&"_sibd8",{{#8A,xrm},2})     -- 0o212 0o1r4 sib d8   -- eg mov_dl_sibd8
        iload("cmp_"&rs&"_imm8",{{#82,#F7+src},1})  -- 0o202 0o37r i8       -- eg cmp_al_imm8
--       mov_sib_al     = {#88,#04},                -- 0o210 0o004 sib          -- mov [b32+i32*s],al
--       mov_sib_eax    = {#89,#04},                -- 0o211 0o004 sib          -- mov [b32+i32*s],eax
--       mov_eax_sib    = {#8B,#04},                -- 0o213 0o004 sib          -- mov eax,[b32+i32*s]
    end for

    iload("add_eax_imm32",{#05,4})                  -- 0o005 imm32              -- add eax,imm32
    iload( "or_eax_imm32",{#0D,4})                  -- 0o015 imm32              --  or eax,imm32
    iload("adc_eax_imm32",{#15,4})                  -- 0o025 imm32              -- adc eax,imm32
    iload("sbb_eax_imm32",{#1D,4})                  -- 0o035 imm32              -- sbb eax,imm32
    iload("and_eax_imm32",{#25,4})                  -- 0o045 imm32              -- and eax,imm32
    iload("sub_eax_imm32",{#2D,4})                  -- 0o055 imm32              -- sub eax,imm32
    iload("xor_eax_imm32",{#35,4})                  -- 0o065 imm32              -- xor eax,imm32
    iload("cmp_eax_imm32",{#3D,4})                  -- 0o075 imm32              -- cmp eax,imm32
    iload("test_eax_imm32",{#A9,4})                 -- 0o251 imm32              -- test eax,imm32

    iload("rdtsc",{{#0F,#31}})                      -- 0o017 0o061              -- rdtsc (EDX:EAX := Time Stamp Counter)
    iload("cpuid",{{#0F,#A2}})                      -- 0o017 0o242              -- cpuid (EAX := Processor id.info etc)
    iload("pushad",#60)                             -- 0o140                    -- pushad
    iload( "popad",#61)                             -- 0o141                    -- popad
    iload("wd_prfx",#66)                            -- 0o146                    -- <word prefix>
    iload("push_imm32",{#68,4})                     -- 0o150 imm32              -- push <dword literal>

    --DEV rest of the full set of 8 as above/below to be inserted here:
    iload("cmpb_sibd8i8",  {{#80,#7C},3})           -- 0o200 0o174 sib d8 i8    -- cmp byte[b32+i32*s+d8],i8
    iload("cmpd_sibd8i32", {{#81,#7C},3})           -- 0o201 0o174 sib d8 imm32 -- cmp dword[b32+i32*s+d8],imm32
    iload("cmpd_mem32_i32",{{#81,#3D},8})           -- 0o201 0o075 m32 imm32    -- cmp dword[m32],imm32
    iload("cmpd_mem32_i8", {{#83,#3D},5})           -- 0o203 0o075 m32 imm8     -- cmp dword[m32],imm8

    iload("addd_sibd8i8",{{#83,#44},3})             -- 0o203 0o104 sib d8 i8    -- add dword[b32+i32*s+d8],i8
    iload( "ord_sibd8i8",{{#83,#4C},3})             -- 0o203 0o114 sib d8 i8    --  or dword[b32+i32*s+d8],i8
    iload("adcd_sibd8i8",{{#83,#54},3})             -- 0o203 0o124 sib d8 i8    -- adc dword[b32+i32*s+d8],i8
    iload("sbbd_sibd8i8",{{#83,#5C},3})             -- 0o203 0o134 sib d8 i8    -- sbb dword[b32+i32*s+d8],i8
    iload("andd_sibd8i8",{{#83,#64},3})             -- 0o203 0o144 sib d8 i8    -- and dword[b32+i32*s+d8],i8
    iload("subd_sibd8i8",{{#83,#6C},3})             -- 0o203 0o154 sib d8 i8    -- sub dword[b32+i32*s+d8],i8
    iload("xord_sibd8i8",{{#83,#74},3})             -- 0o203 0o164 sib d8 i8    -- xor dword[b32+i32*s+d8],i8
    iload("cmpd_sibd8i8",{{#83,#7C},3})             -- 0o203 0o174 sib d8 i8    -- cmp dword[b32+i32*s+d8],i8

-- not implemented:
--CPU Disasm
--Address   Hex dump            Command                                  Comments
--7C97DB8A  #2B #4C #B3 36      SUB ECX,DWORD PTR [ESI*4+EBX+36]
--7C97DB8E  #2B #44 #B3 36      SUB EAX,DWORD PTR [ESI*4+EBX+36]
--7C97DB92  #3B #4C #B3 36      CMP ECX,DWORD PTR [ESI*4+EBX+36]
--7C97DB96  #39 #54 #B3 36      CMP DWORD PTR [ESI*4+EBX+36],EDX
--
--7C97DB8A  0o053 0o114 0o263 36        SUB ECX,DWORD PTR [ESI*4+EBX+36]
--7C97DB8E  0o053 0o104 0o263 36        SUB EAX,DWORD PTR [ESI*4+EBX+36]
--7C97DB92  0o073 0o114 0o263 36        CMP ECX,DWORD PTR [ESI*4+EBX+36]
--7C97DB96  0o071 0o124 0o263 36        CMP DWORD PTR [ESI*4+EBX+36],EDX

    iload("pop_mem32",{{#8F,#05},4})                -- 0o217 0o005 mem32        -- pop dword [mem32]

    iload("nop",#90)                                -- 0o220                    -- nop (no-op)
    iload("cwde",#98)                               -- 0o230                    -- cwde (ax-> eax)

    iload("mov_eax_mem32",{#A1,4})                  -- 0o241 mem32              -- mov eax,[mem32]
    iload("mov_mem32_eax",{#A3,4})                  -- 0o243 mem32              -- mov [mem32],eax
    iload("ret_i16",{#C2,2})                        -- 0o302 imm16              -- ret imm16
    iload("ret",#C3)                                -- 0o303                    -- ret
--  iload("mov_sib_imm32",{{#C7,#04},5})            -- 0o307 0o004 sib i32      -- mov [b32+i32*s],imm32    (unused/untested)
    iload("mov_m32_imm32",{{#C7,#05},8})            -- 0o307 0o005 m32 i32      -- mov [m32],imm32

--DEV this should probably be in the top loop: (moved 21/5/2013)
--  iload("setl",{{#0F,#9C},1})                     -- 0o017 0o234 xrm          -- setl r/m8

    iload("cdq",#99)                                -- 0o231                    -- cdq (eax-> edx:eax)
    iload("fld_pi",{{#D9,#EB}})                     -- 0o331 0o353              -- fldpi
    iload("fild_mem32",{{#DB,#05},4})               -- 0o333 0o005 m32          -- fild dword[mem32]
    iload("fistp_dword_esp",{{#DB,#1C,#24}})        -- 0o333 0o034 0o044        -- fistp dword[esp]
    iload("fistp_qword_esp",{{#DF,#3C,#24}})        -- 0o337 0o074 0o044        -- fistp qword[esp]

    iload("fistp_d_espd8",{{#DB,#5C,#24},1})        -- 0o333 0o134 0o044 d8     -- fistp dword[esp+d8]

    iload("fld_qword_sib",{{#DD,#04},1})            -- 0o335 0o004 sib          -- fld qword[ebx+ecx*4]

    iload("fstp_qword_esp",{{#DD,#1C,#24}})         -- 0o335 0o034 0o044        -- fstp qword[esp]
    iload("fstp_m64_sib",{{#DD,#1C},1})             -- 00335 0o034 sib          -- fstp qword[b32+i32*s]

    iload("fild_dword_esp",{{#DB,#04,#24}})         -- 0o333 0o004 0o044        -- fild dword[esp]
    iload("fild_qword_esp",{{#DF,#2C,#24}})         -- 0o337 0o054 0o044        -- fild qword[esp]

    iload("faddp",{{#DE,#C1}})                      -- 0o336 0o301              -- faddp [st1,st0]

--CPU Disasm
--0040BFB5  DB18            FISTP DWORD PTR [EAX]
--0040BFB7  DB19            FISTP DWORD PTR [ECX]
--0040BFB2  DB1CE4          FISTP DWORD PTR [ESP]   -- overridden with #24 as last byte....

    iload("call_rel32",{#E8,4})                     -- 0o350 offset32
    iload( "jmp_rel32",{#E9,4})                     -- 0o351 offset32
    iload("jump_rel32",{#E9,4})                     -- 0o351 offset32

    iload("rep_movsb",{{#F3,#A4}})                  -- 0o363 0o244              -- rep movsb
    iload("rep_movsd",{{#F3,#A5}})                  -- 0o363 0o245              -- rep movsd
    iload("rep_stosd",{{#F3,#AB}})                  -- 0o363 0o253              -- rep stosd

    iload("stosd",#AB)                              -- 0o253                    -- lodsd (ie [edi]:=eax; edi+=4)
    iload("lodsd",#AD)                              -- 0o255                    -- lodsd (ie eax:=[esi]; esi+=4)

    iload("call_mem32",{{#FF,#15},4})               -- 0o377 0o025 mem32        -- call [mem32]

    iload("push_mem32",{{#FF,#35},4})               -- 0o377 0o065 mem32        -- push dword[mem32]

    iload("jmp_si5_imm32",{{#FF,#24},5})            -- 0o377 0o044 si5 imm32    -- jmp dword[i*s+imm32]
                                                    -- (for si5 use eg ebp_ecx4; ebp(=5) here means "no base")

    iload("inc_sibd8",{{#FF,#44},2})                -- 0o377 0o104 sib d8       -- inc dword[b32+i32*s+d8]
    iload("dec_sibd8",{{#FF,#4C},2})                -- 0o377 0o104 sib d8       -- inc dword[b32+i32*s+d8]

    iload("call_sibd8",{{#FF,#54},2})               -- 0o377 0o124 sib d8       -- call dword[b32+i32*s+d8]

    iload("jmp_sibd8",{{#FF,#64},2})                -- 0o377 0o144 sib d8       -- jmp dword[b+i*s+d8]

    iload("jmp_table_entry",{isAddr,3})     -- special for jump table encoding
                                            -- (*NB* not generally supported, see test\swtime.exw)
--DEV experimental:
--  iload("jmp_table_ennew",{isJmp,3})      -- special for jump table encoding


    -- flags, see pglobals.e (and pilx86.e/pemit.e)
    iload("isOpCode",       isOpCode)
--  iload("isApiFn",        isApiFn)
    iload("isAPIfn",        isAPIfn)
    iload("isVar",          isVar)
    iload("isConst",        isConst)
    iload("isConst8",       isConst8)
if not newEmit then
    iload("isConstRef",     isConstRef)
end if
    iload("isConstRefCount",isConstRefCount)
    iload("isJmpG",         isJmpG)
    iload("isGaddr",        isGaddr)
    iload("isAddr",         isAddr)
    iload("isJmp",          isJmp)
--*/

    -- and finally the opcodes
--  for i=1 to length(opNames) do
--  for i=1 to maxNVop do   -- don't iload() virtual opcodes!
--      name = opNames[i]
--      if string(name) then
--          iload(name,i)
----DEV if replaceBuiltinOps then           -- (untried, just an idea)
----DEV ilopcodes=append(ilopcodes,ttidx)   -- so that eg "::opLen" scores a hit
----DEV end if
--      end if
--  end for

--DEV temporary, until newEBP bedded in (these are virtual opcodes to boot):
--DEV actually, these are probably permanent now...
--  iload("opLoadMem", {opLoadMem +#10000000,2})
--  iload("opLeaMov",  {opLeaMov  +#10000000,2})
--  iload("opStoreMem",{opStoreMem+#10000000,2})
--  iload("opFildMem", {opFildMem +#10000000,1})

--DEV is this still actually used?? (search for il_search)
    for src=1 to 8 do   -- eax to edi (as idx to regs/r8)
        rs = regs[src]
        iload(rs,src-1)
    end for

end procedure

--function tt_stringI(string name)
--  tt_string(name,-3)
--  return ttidx
--end function
--global constant I_malloc = tt_stringI("%opMallocX")   -- (nb must match the label in pmalloc.e)
--global constant I_mfree = tt_stringI("%opMfreeX") -- (nb must match the label in pmalloc.e)

global function il_search(integer label)
    if not length(ilstuff) then init_il() end if    -- [NB ilASM still needs the opcodes, for now...]
    idxstart = col
    tt_string(text,-3)
    col = idx
    Ch = text[col]
    idx = tt[ttidx+EQ]
    if label=':' then return idx end if
--  if idx=0 then Abort("unrecognised") end if
    if idx=0 then return {} end if
    return ilstuff[idx]
end function

sequence ttinit
global procedure tt_save_init()
-- called before testall loop
-- take a copy of tt before it gets loaded with ptrs into symtab.
--24/6/13: (shaves ~0.01s off p -test)
    if not length(ilstuff) then init_il() end if
    ttinit = tt
end procedure
--constant ttinit = tt --,      --DEV if testall?
--       ttiused = used
--       ttiused = length(tt)
--, tchkinit = ttchk

global procedure tt_init()  -- ""
-- called at bottom of testall loop
-- reset ttree, when starting again with a new empty symtab.
    tt = ttinit
--ttchk = tchkinit
--  used = ttiused
    atom_base = 0
    seq_base = 0
--24/6/13:
--  ilstuff = {}                -- force reload under testall, see il() in pmain.e
end procedure

global procedure clear_seq_tree()
--
-- Called by DumpSignaturesAndLineTables() in pemit.e after the existing
-- seq_tree has been fully processed to to start a fresh "signature pool".
-- A "signature pool" is useful in say a win32lib program which contains 
-- hundreds of handlers, all with an "IIP" signature (aka integer self, 
-- integer event, sequence parms). Clearly rather than dump hundreds of 
-- copies of "IIP" (or the post-mapsig equivalent), we would rather dump 
-- one, via tt_traverseQ, and link all matching symtab entries to it.
-- Only used when creating an exe, so speed is not all that critical.
--
    seq_base = 0
end procedure

