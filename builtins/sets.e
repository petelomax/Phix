--
-- builtins\sets.e
-- ===============
--
--  Basic set opertions.
--

type set_type(object s)
    bool is_set = sequence(s)
    if is_set and length(s) then
        -- optimisation for in-order sets:
        object highest = s[1]
        for i=2 to length(s) do
            object si = s[i]
            if si>highest then
                highest = si
            elsif rfind(si,s,i-1) then
                is_set = false
                exit
            end if
        end for
    end if
    return is_set
end type

global function is_empty(set_type s)
    --
    -- Returns true if x is empty.
    -- Symbolically represented with a circle with a diagonal through it (&empty;)
    --
    return length(s)=0
end function

-- Aside: settled on (set_type s, object x) over (object x, set_type s)
--        for the next few, due to parallels with append(), but op2 find.

global function is_member(set_type s, object x)
    --
    -- Returns true if x is a member of s.
    -- Symbolically represented with an E shape (&isin;).   
    --
    return find(x,s)!=0
end function

global function add_member(set_type s, object x)
    --   if not is_member(x,s) then
    if not find(x,s) then
        s = append(s,x)
    end if
    return s
end function

global function remove_member(set_type s, object x)
    integer k = find(x,s)
    if k then
        s[k..k] = {}
    end if
    return s
end function

global function union(sequence s1, object s2=-1)
    --
    -- Returns anything that occurs in any set.
    -- Symbolically represented with a U shape (&cup;).
    -- Can be invoked as union(s1,s2) or union({s1,s2}),
    --  where s1 and s2 are sequences and the second
    --  form permits more than two/any number of sets.
    -- The union of empty sets is the empty set, as is
    --  the union of an empty set of sets. Otherwise 
    --  empty sets have no impact on the result when 
    --  in the presence of other non-empty sets.
    -- An error ocurs when s1 or s2 or elements within
    --  that should be sequences are in fact atoms.
    -- Strings (char sets) are perfectly fine too.
    --
    s1 = deep_copy(s1)
    if s2!=-1 then
        -- assume union(s1,s2) style invocation
        if not set_type(s1) then ?9/0 end if
        if not set_type(s2) then ?9/0 end if
        for i=1 to length(s2) do
            if not find(s2[i],s1) then
                s1 = append(s1,s2[i])
            end if
        end for
    elsif length(s1) then
        -- assume union({s1,s2,...}) style invocation
        s2 = s1[2..$]
        s1 = s1[1]
        if not set_type(s1) then ?9/0 end if
        for i=1 to length(s2) do
            if not set_type(s2[i]) then ?9/0 end if
            s1 = union(s1,s2[i])
        end for
    end if
    return s1
end function

local function set_default(sequence s1, object s2=-1)
    -- (local) return {} or "" as the new set default
    bool bAllString = true
    if s2!=-1 then
        bAllString = string(s1) and string(s2)
    else
        for i=1 to length(s1) do
            if not string(s1[i]) then
                bAllString = false
                exit
            end if
        end for
    end if
    return iff(bAllString?"":{})
end function

global function intersection(sequence s1, object s2=-1)
    --
    -- Returns anything that occurs in every set.
    -- Symbolically represented as upsidedown U (&cap;)
    -- Can be invoked as intersection(s1,s2) or as
    --  intersection({s1,s2}), where s1 and s2 are 
    --  sequences and the second form permits more 
    --  than two/any number of sets.
    -- The intersection of empty sets is the empty 
    --  set, as is the intersection of an empty set 
    --  of sets, or a set of just one set. 
    --  The presence of any empty set means that 
    --  the result will also be the empty set.
    -- An error ocurs when s1 or s2 or elements within
    --  that should be sequences are in fact atoms.
    -- While s1 can be a set of sets, if at the same 
    --  time s2 is a set or set of sets, the result
    --  is then completely undefined and unsupported.
    -- Strings (char sets) are perfectly fine too.
    --
    set_type res = set_default(s1,s2)
    if s2!=-1 then
        -- assume intersection(s1,s2) style invocation
        if not set_type(s1) then ?9/0 end if
        if not set_type(s2) then ?9/0 end if
        for i=1 to length(s1) do
            object si = s1[i]
            if find(si,s2) then
                res = append(res,si)
            end if
        end for
    else
        -- assume intersection({s1,s2,s3...}) style invocation
        if length(s1) then
            set_type s = s1[1]
            for i=1 to length(s) do
                object si = s[i]
                if not find(si,res) then
                    for j=length(s1) to 2 by -1 do
                        set_type sj = s1[j]
                        if not find(si,sj) then exit end if
                        if j=2 then res = append(res,si) end if
                    end for
                end if
            end for
        end if
    end if
    return res
end function

global function difference(sequence s1, object s2=-1, bool symmetric=true)
    --
    -- Returns anything missing from some other set.
    -- if symmetric is false only items from s1 or s1[1] are returned,
    -- otherwise the result contains each and every item in any set 
    -- that is missing from any of the other sets.
    -- Obviously difference(s1,s2,true) is effectively the same as
    --           union(difference(s1,s2,false),difference(s2,s1,false))
    --  [slightly more complicated than that when s1 is a set of sets]
    -- Symbolically represented as: symmetric: a triangle, or sometimes
    --                              as a circle with a horizontal line,
    --                              relative: a backslash
    --                              (dunno any html codes for this one)
    -- 
    set_type res = set_default(s1,s2)
    if s2!=-1 then
        -- assume difference(s1,s2) style invocation
        if not set_type(s1) then ?9/0 end if
        if not set_type(s2) then ?9/0 end if
        for i=1 to length(s1) do
            object si = s1[i]
            if not find(si,res)
            and not find(si,s2) then
                res = append(res,si)
            end if
        end for
        if symmetric then
            for i=1 to length(s2) do
                object si = s2[i]
                if not find(si,res)
                and not find(si,s1) then
                    res = append(res,si)
                end if
            end for
        end if
    else
        -- assume difference({s1,s2,s3...}) style invocation
        if length(s1) then
            for h=1 to iff(symmetric?length(s1):1) do
                set_type s = s1[h]
                for i=1 to length(s) do
                    object si = s[i]
                    if not find(si,res) then
                        for j=1 to length(s1) do
                            if j!=h and not find(si,s1[j]) then
                                res = append(res,si)
                                exit 
                            end if
                        end for
                    end if
                end for
            end for
        end if
    end if
    return res
--/*
    sequence res = {}
   for i=1 to length(s1) do
      object o = s1[i]
      p = find (o, s2)
      if (p < 1) then
         s3 = append (s3, o)
      end if
   end for
    if symmetric then
   for i=1 to length(s2) do
      o = s2[i]
      p = find (o, s1)
      if (p < 1) then
         s3 = append (s3, o)
      end if
   end for
   return s3
--*/
end function

global function is_subset(set_type subset, superset)
    --
    -- return false if any element of subset is not in superset
    -- Symbolically represented as: underlined[?] C shape (&sub;)
    --
    for i=1 to length(subset) do
--      if not is_member(sub[i],super) then
        if not find(subset[i],superset) then
            return false
        end if
    end for
    return true
--DEV uh??
--   sequence intersect = intersection(sub, super),
--              diff = difference(intersect, sub)
--   return length(diff)=0
end function

global function is_superset(set_type superset, subset)
    --
    -- returns true if all elements of subset are in superset
    -- Symbolically represented as: underlined[?] reverse C (&sup;)
    --
   return is_subset(subset, superset)
end function

-- AVAILABLE FUNCTIONS:
--
--    s = add_member(s, x)      -- adds a new member to a set, if it's not already a member
--    s = remove_member(s, x)  -- removes the specified member from the set
--    s = union(s, s)         -- returns the union of two given sets
--    s = intersection(s, s) -- returns the intersection of two given sets
--    s = diff(s, s)        -- returns the difference of the two given sets; i.e.,
--                        the inverse of the union (with the intersection removed)
--


