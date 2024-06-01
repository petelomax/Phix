--
-- builtins\sets3.e
-- ================
--
--  Another verion of sets.e - one day I'll benchmark these...
--
-- 21/5/24: enforced sorted order, use binary search, and
--          I'm going to rely on custom-sets as a test!!! [DONE]
--

type set_type(object s)
    bool is_set = sequence(s)
    if is_set and length(s) then
        -- optimisation for in-order sets:
        object highest = s[1]
        for i=2 to length(s) do
            object si = s[i]
            if si<=highest then return false end if
            highest = si
        end for
    end if
    return is_set
end type

--global 
function my_is_empty(set_type s)
    --
    -- Returns true if x is empty.
    -- Symbolically represented with a circle with a diagonal through it (&empty;)
    --
    return length(s)=0
end function

--global 
function my_is_member(set_type s, object x)
    --
    -- Returns true if x is a member of s.
    -- Symbolically represented with an E shape.    
    -- [arg order matches append(), not find(), binary_earch()]
    --
    return binary_search(x,s)>0
end function

--global 
function my_add_member(set_type s, object x)
    -- [arg order matches append()]
    integer k = binary_search(x,s)
    if k<0 then
        k = -k
        if k>length(s) then
            s = append(s,x)
        else
            s[k..k-1] = {x}
        end if
    end if
    return s
end function

--global 
function my_remove_member(set_type s, object x)
    -- [arg order matches append()]
    integer k = binary_search(x,s)
    if k>0 then
        s[k..k] = {}
    end if
    return s
end function

--global 
function my_union(sequence s1, object s2=-1)
    --
    -- Returns anything that occurs in any set.
    -- Symbolically represented with a U shape.
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
    nested function set_merge(set_type s1, s2)
        integer s1x = 1, s1l = length(s1),
                s2x = 1, s2l = length(s2)
        bool first = true
        sequence res = s1
        while s1x<=s1l
          and s2x<=s2l do
            integer c = compare(s1[s1x],s2[s2x])
            if c<=0 then
                if not first then
                    res = append(res,s1[s1x])
                end if
                s1x += 1
                if c=0 then
                    s2x += 1
                end if
            else
                if first then
                    res = s1[1..s1x-1]
                    first = false
                end if
                res = append(res,s2[s2x])
                s2x += 1
            end if
        end while
        if s1x<=s1l then
            assert(s2x>s2l)
            if not first then
                res &= s1[s1x..$]
            end if
        elsif s2x<=s2l then
            res &= s2[s2x..$]
        end if
        return res
    end nested function

    s1 = deep_copy(s1)
    if s2=-1 then
        -- union({s1,s2,...}) style invocation
        if length(s1) then
            s2 = s1[2..$]
            s1 = s1[1]
            assert(length(s2) or set_type(s1))
            for si in s2 do
                s1 = set_merge(s1,si)
            end for
        end if
        return s1
    end if
    -- assume union(s1,s2) style invocation
    return set_merge(s1,s2)
end function

local function set_default(sequence s1)
    -- return {} or "" as the new set default
    for s in s1 do
        if not string(s) then
            return {}
        end if
    end for
    return ""
end function

--global 
function my_intersection(sequence s1, object s2=-1)
    --
    -- Returns anything that occurs in every set.
    -- Symbolically represented as upsidedown U
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
    if s2!=-1 then s1 = {s1,s2} end if
    set_type res = set_default(s1)
    integer l = length(s1)
    if l>1 then
        s2 = s1[2..$]
        s1 = s1[1]
        integer i1 = 1, il = length(s1)
        l -= 1
        sequence ls = apply(s2,length),
                 ldx = repeat(1,l)
        --
        -- if we can increment all ldx such that all
        -- s2[k][ldx[k]] match an element of s1 then
        -- said element deserves to be added to res.
        --
        while i1<=il do
            object s1i = s1[i1]
            integer k = 1
            while k<=l do
                integer kx = ldx[k]
                if kx>ls[k] then
--                  i1 = il+1
--                  exit
                    return res
                end if
                integer c = compare(s1i,s2[k][kx])
                if c=0 then
                    k += 1
                elsif c=-1 then
                    exit -- nope, try next s1[]
                else -- c == +1
--                  ldx[k] += 1
                    ldx[k] = kx+1
                end if                  
            end while
            if k>l then
                res = append(res,s1[i1])
                ldx = sq_add(ldx,1)
            end if
            i1 += 1
        end while
    end if
    return res
end function

--global 
function my_difference(set_type s1, s2)
    --
    -- Returns items in s1 not present in s2.
    -- Symbolically represented as: a backslash
    --
    set_type res = set_default({s1,s2})
    integer i1 = 1, l1 = length(s1),
            i2 = 1, l2 = length(s2)
    while i1<=l1 do
        if i2>l2 then
            res &= s1[i1..$]
            exit
        end if
        integer c = compare(s1[i1],s2[i2])
        if c=-1 then
            res = append(res,s1[i1])
            i1 += 1
        elsif c=0 then
            i1 += 1
            i2 += 1
        else
            i2 += 1
        end if
    end while
    return res
end function

--global 
function symmetric_difference(set_type s1, s2)
    --
    -- Returns items in s1 not present in s2 or vice versa.
    -- Symbolically represented as: a triangle, or sometimes
    --                    as a circle with a horizontal line
    --
    return my_union(my_difference(s1,s2),my_difference(s2,s1))
end function

--global 
function my_is_subset(set_type subset, superset)
    --
    -- return false if any element of subset is not in superset
    -- Symbolically represented as: underlined C shape
    --
    integer ssx = 1, ssl = length(superset)
    for s in subset do
        do
            if ssx>ssl then return false end if
            integer c = compare(s,superset[ssx])
            if c<0 then return false end if
            ssx += 1
        until c=0
    end for
    return true
end function

--global 
function my_is_superset(set_type superset, subset)
    --
    -- returns true if all elements of subset are in superset
    -- Symbolically represented as: underlined reverse C
    --
   return my_is_subset(subset, superset)
end function

