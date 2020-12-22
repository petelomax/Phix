--
-- builtins\VM\pMatch.e
-- ====================
--
--  (Temporary) hll implementation of match()
--  There is an equivalent commented-out backend/asm version in pJcc.e, conversion of which is yet to be completed.
--

--global function match(object needle, sequence haystack, integer start=1, bool case_insensitive=false)
global function match(object needle, sequence haystack, integer start=1, bool case_sensitive=true)
--
-- Try to match needle against some slice of haystack.
-- If successful, return the element number of haystack where the (first) matching slice begins,
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
integer res, hdx, ln, lh
object ni, hi
    -- This line, and first parameter being object not sequence, is not RDS compliant.
    --  (RDS gives error "first argument of match() must be a sequence")
    if atom(needle) then
        return find(needle,haystack,start)
    end if
--  if case_insensitive then
    if not case_sensitive then
        needle = lower(needle)
        haystack = lower(haystack)
    end if
    res = start
    ln = length(needle)
    -- This line is also not RDS compliant
    --  (RDS gives error "first argument of match() must be a non-empty sequence")
    if ln=0 then return 0 end if
    if start<1 then return 0 end if
    lh = length(haystack)
--  if ln > lh then return 0 end if -- see below
    while 1 do
        if res+ln-1>lh then return 0 end if
        for i=1 to ln do
            ni = needle[i]
            hdx = i+res-1
--          if hdx>lh then return 0 end if -- see above
            hi = haystack[hdx]
--          if case_insensitive then ni = lower(ni)
--          if not case_sensitive then ni = lower(ni)
--                                     hi = lower(hi) end if
--          if not quick_equal(ni,hi) then  -- asm variant
            if not equal(ni,hi) then
--              if integer(ni) and not integer(hi) then -- maybe?
--              if integer(ni) and not equal(ni,hi) then -- maybe?
                if integer(ni) then
                    -- scan for ni later on in haystack
                    -- eg needle=13131...,
                    --    haystack=1313x31...
                    -- with needle[5]=1, scanning fwd two places to haystack[7]=1
                    -- gives the earliest point worth re-starting from.
                    -- if ni does not occur anywhere later in haystack, then
                    -- clearly there will be no match anywhere.
                    for j=hdx+1 to lh+1 do
                        res += 1
--                      hdx = i+res-1
--                      hdx += 1
--                      if hdx>lh then return 0 end if
                        if j>lh then return 0 end if
                        hi = haystack[j]
--                      if case_insensitive then hi = lower(hi) end if
--                      if not case_sensitive then hi = lower(hi) end if
                        if equal(ni,hi) then exit end if
                    end for
                    exit
                elsif integer(hi) then
                    -- scan for hi earlier on in needle
                    -- eg needle=131313x...,
                    --    haystack=1313131...
                    -- with haystack[7]=1, scanning back two places to needle[5]=1
                    -- gives the earliest point worth re-starting from.
                    -- if haystack[7] does not occur anywhere earlier in needle,
                    -- then clearly we should restart from haystack[8].
                    for j=i-1 to 0 by -1 do
                        res += 1
                        if j=0 then exit end if
                        ni = needle[j]
--                      if case_insensitive then ni = lower(ni) end if
--                      if not case_sensitive then ni = lower(ni) end if
                        if equal(hi,ni) then exit end if
                    end for
                    exit
--              elsif not equal(ni,hi) then -- deep_equal() here in asm
                else
                    res += 1
                    exit
                end if
            end if
            if i=ln then return res end if
        end for
    end while
end function

--**
-- Try to match a needle against some slice of a haystack in reverse order.
--
-- Parameters:
--   # ##needle## : a sequence to search for
--   # ##haystack## : a sequence to search in
--   # ##start## : an integer, the starting index position (defaults to length(##haystack##))
--
-- Returns:
--   An **integer**, either 0 if no slice of ##haystack## starting before 
--   ##start## equals ##needle##, else the highest lower index of such a slice.
--
-- Comments:
--   If ##start## is less than 1, it will be added once to length(##haystack##)
--   to designate a position counted backwards. Thus, if ##start## is -1, the
--   first element to be queried in ##haystack## will be ##haystack##[$-1],
--   then ##haystack##[$-2] and so on.
--
-- Example 1:
-- <eucode>
-- location = rmatch("the", "the dog ate the steak from the table.")
-- -- location is set to 28 (3rd 'the')
-- location = rmatch("the", "the dog ate the steak from the table.", -11)
-- -- location is set to 13 (2nd 'the')
-- </eucode>
--
-- See Also:
--     [[:rfind]], [[:match]]

--global function rmatch(object needle, sequence haystack, integer start=length(haystack), bool case_insensitive=false)
global function rmatch(object needle, sequence haystack, integer start=length(haystack), bool case_sensitive=true)

    -- This line, and first parameter being object not sequence, is not RDS compliant.
    --  (RDS gives error "first argument of match() must be a sequence")
    if atom(needle) then
        return rfind(needle,haystack,start)
    end if

    integer ln = length(needle),
            lh = length(haystack)

    if ln=0
    or start=0
    or start>lh
    or lh+start<1 then
        return 0
    end if

--  if case_insensitive then
    if not case_sensitive then
        needle = lower(needle)
        haystack = lower(haystack)
    end if

    if start<1 then
        start = lh+start
    end if

    if start+ln-1>lh then
        start = lh-ln+1
    end if

--28/11/19 (bitbucket issue #27)
--  ln -= 1

    for i=start to 1 by -1 do
--      if equal(needle, haystack[i..i+ln]) then
--          return i
--      end if
        for j=1 to ln do
--          object nj = needle[j]
--          object hj = haystack[i+j-1]
--          if nj!=hj then exit end if  
            if needle[j]!=haystack[i+j-1] then exit end if  
            if j=ln then return i end if
        end for
    end for

    return 0
end function


