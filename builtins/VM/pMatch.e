--
-- pMatch.e
-- ========
--
--  (Temporary) hll implementation of match()
--  There is an equivalent commented-out backend/asm version in pJcc.e, conversion of which is yet to be completed.
--

global function match(object s1, sequence s2, integer start=1, integer case_insensitive=0)
--
-- Try to match s1 against some slice of s2.
-- If successful, return the element number of s2 where the (first) matching slice begins,
-- else return 0.  

-- This is the closest way to express the back-end algorithm in hll.
-- Phix ensures that eg 6.5+6.5 is stored as a short int 13, not
-- a floating point 13.0. Finding a short int is faster than anything
-- else, because there is no dereference, hence when we hit a mismatch
-- and one of the disagreeing values is a shortint, use that to look
-- for a better place to restart the top-level scan.
-- In the asm backend, we actually apply the integer() checks before
-- the deep-equal(), and use quick_equal() in the inner loops.
-- The asm backend also uses slightly different code for the four
-- cases match(string,string), match(string,sequence), match(seq,str)
-- and match(seq,seq) for the obvious reasons of bit size, and not
-- bothering to check for string[i] as integer, and that the first
-- three cases do not ever need to invoke deep_equal().
--
-- Changes to the functionality of match() noted below arose from a 
-- protracted discussion on EuForum in 2002.
--
integer res, s2idx, ls1, ls2
object s1i, s2i
    -- This line, and first parameter being object not sequence, is not RDS compliant.
    --  (RDS gives error "first argument of match() must be a sequence")
    if atom(s1) then return find(s1,s2,start) end if
    res = start
    ls1 = length(s1)
    -- This line is also not RDS compliant
    --  (RDS gives error "first argument of match() must be a non-empty sequence")
    if ls1=0 then return 0 end if
    if start<1 then return 0 end if
    ls2 = length(s2)
--  if ls1 > ls2 then return 0 end if -- see below
    while 1 do
        if res+ls1-1>ls2 then return 0 end if
        for i=1 to ls1 do
            s1i = s1[i]
            if case_insensitive then s1i = lower(s1i) end if
            s2idx = i+res-1
--          if s2idx>ls2 then return 0 end if -- see above
            s2i = s2[s2idx]
            if case_insensitive then s2i = lower(s2i) end if
--          if not quick_equal(sli,s2i) then    -- asm variant
            if not equal(s1i,s2i) then
--              if integer(sli) and not integer(s2i) then -- maybe?
--              if integer(sli) and not equal(s1i,s2i) then -- maybe?
                if integer(s1i) then
                    -- scan for sli later on in s2
                    -- eg s1=13131...,
                    --    s2=1313x31...
                    -- with s1[5]=1, scanning fwd two places to s2[7]=1
                    -- gives the earliest point worth re-starting from.
                    -- if s1i does not occur anywhere later in s2, then
                    -- clearly there will be no match anywhere.
                    for j=s2idx+1 to ls2+1 do
                        res += 1
--                      s2idx = i+res-1
--                      s2idx += 1
--                      if s2idx>ls2 then return 0 end if
                        if j>ls2 then return 0 end if
                        s2i = s2[j]
                        if case_insensitive then s2i = lower(s2i) end if
                        if equal(s1i,s2i) then exit end if
                    end for
                    exit
                elsif integer(s2i) then
                    -- scan for s2i earlier on in s1
                    -- eg s1=131313x...,
                    --    s2=1313131...
                    -- with s2[7]=1, scanning back two places to s1[5]=1
                    -- gives the earliest point worth re-starting from.
                    -- if s2[7] does not occur anywhere earlier in s1,
                    -- then clearly we should restart from s2[8].
                    for j=i-1 to 0 by -1 do
                        res += 1
                        if j=0 then exit end if
                        s1i = s1[j]
                        if case_insensitive then s1i = lower(s1i) end if
                        if equal(s2i,s1i) then exit end if
                    end for
                    exit
--              elsif not equal(s1i,s2i) then   -- deep_equal() here in asm
                else
                    res += 1
                    exit
                end if
            end if
            if i=ls1 then return res end if
        end for
    end while
end function

