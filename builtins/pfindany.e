--
-- pfindany.e
--
--  Phix implementation of find_any() and match_any()
--
-- This is an auto-include file; there is no need to manually include
--  it, unless you want a namespace.
--
-- NB: as per the docs these do things the "wrong way round", looking
--     for haystack in needles, in order to yeild the first/lowest idx.

global function find_any(sequence needles, haystack, integer start=1, mult=1)
    assert(mult=1 or mult>length(needles))
    for i,h in haystack from start do
        integer j = find(h,needles)
        if j then
            if mult>1 then return i*mult+j end if
            return i
        end if
    end for
    return 0
end function

global function match_any(sequence needles, haystack, integer start=1, mult=1)
    assert(mult=1 or mult>length(needles))
--  for i,h in haystack from start do -- (but h is of no use to us here)
    for i=start to length(haystack) do
        for j,n in needles do
            if begins(n,haystack,i) then
                if mult>1 then return i*mult+j end if
                return i
            end if
        end for
    end for
    return 0
end function

-- shd be same, kept just in case:
--global function find_any(sequence needles, haystack, integer start=1)
--  for i=start to length(haystack) do
--      if find(haystack[i],needles) then
--          return i
--      end if
--  end for
--  return 0
--end function

-- untested, did't like the look of this...
--global function match_any(sequence needles, haystack, integer start=1)
--  integer res = 0
--  for n in needles do
--      r = match(n,haystack,start)
--      if r and r<res then
--          res = r
--          return i
--      end if
--  end for
--  return res
--end function

