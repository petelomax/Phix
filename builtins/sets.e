--
-- builtins\sets.e
-- ===============
--
--  Basic set operations (an autoinclude).
--
--  TODO: full testing in a browser (esp sid_id/delete_routine stuff,
--                                   and implicit set_ignore_atom thing, esp +ve tgt)
--
--  See test\t72sets.exw for, well, guess what, some tests.
--  Used in demo\rosetta\Numbrix.exw
--      and demo\rosetta\BronKerbosch.exw
--
-- At some point I wrote this (re sets.e) in the release notes:
--   UPDATE: hardly suprisingly, I've realised there is probably a better way
--           to deal with the sad fact that JavaScript has no delete_routine()
-- but (clips self round t'back of 'is 'ead) can no longer remember what I
-- was on about... unless it was that FinalizationRegistry thing, perhaps?
-- or... it could just be that last paragraph of sets.htm...
--
bool s_init = false
sequence sets
integer free_sets = 0

local string management_option -- = "NORMAL" -- (keep "get" simple)
local integer options = 0b1111
local bool check_order = true,
                jsflag = false
local constant S_SEQ = 0b1000,  -- sequences allowed
               S_STR = 0b0100,  -- strings allowed
               S_SID = 0b0010,  -- set_ids allowed
               S_ARC = 0b0001   -- auto-reclaim (aka delete_routine)

-- (aside: S_ARC without S_SID is meaningless/pointless, ditto 0b0000)

local procedure set_deleter(atom sid)
    assert(platform()!=JS)
    integer id = sid-0.5
    sets[id] = free_sets
    free_sets = id
end procedure

local type set_type(object sid)
    if sequence(sid) then
        integer flag = iff(string(sid)?S_STR:S_SEQ)
        if platform()!=JS and not and_bits(options,flag) then 
            return false
        end if
        if check_order and length(sid) then
            object highest = sid[1]
--          for i,si in sid from 2 do
            for si in sid from 2 do
                if si<=highest then return false end if
                highest = si
            end for
        end if
        return true
    end if
    if platform()=JS then return false end if
    if not integer(sid) then
--      if platform()=JS or (not and_bits(options,S_ARC))
        if (not and_bits(options,S_ARC))
        or not still_has_delete_routine(sid, set_deleter) then
            return false
        end if
        sid -= 0.5
        if not integer(sid) then return false end if
    end if
    return and_bits(options,S_SID) and s_init and sid>0 
       and sid<=length(sets) and not integer(sets[sid])
end type

global procedure set_management(string how, bool bCheckOrder=true)
--  if how="JS" then jsflag = true; return end if
    switch how do
        // all 15 options 0b0001..0b1111 shown:
        case "NORMAL":  options = 0b1111
        case "ATOM":    options = 0b0011
        case "ATOMSTR": options = 0b0111
        case "ATOMSEQ": options = 0b1011
--                                0b1001 -- meaningless/as 0b1000
--                                0b0001 -- meaningless
--                                0b0101 -- meaningless/as 0b0100
--                                0b1101 -- meaningless/as 0b1100
        case "INTEGER": options = 0b0010
        case "INTSTR":  options = 0b0110
        case "INTSEQ":  options = 0b1010
        case "NOTAUTO": options = 0b1110
        case "SEQ":     options = 0b1000
        case "STR":     options = 0b0100
        case "SEQSTR":  options = 0b1100
        default: ?9/0
    end switch
    management_option = how
    check_order = bCheckOrder
end procedure

global function get_management()
    -- (Aliased to get_set_management() in psym.e/syminit().)
    -- (if set_management()/new_set() not yet called)
--  if not object(management_option) then
--      return "NORMAL"
--  end if
--  return management_option
    return iff(not object(management_option)?"NORMAL":management_option)
end function

global function new_set(object items={})
    assert(platform()=JS or and_bits(options,S_SID))
    if not s_init then
        s_init = true
        sets = {}
        -- (if set_management() not already called)
        if not object(management_option) then
            management_option = "NORMAL"
        end if
    end if
    if atom(items) then
        assert(platform()!=JS)
        items = deep_copy(sets[floor(items)])
    else
        items = unique(items)
    end if
    if platform()=JS then return items end if
    integer res = free_sets
    if res then
        free_sets = sets[res]
        sets[res] = items
    else    
        sets = append(sets,items)
        res = length(sets)
    end if
    set_type sid = res
    if platform()!=JS
    and and_bits(options,S_ARC) then
        sid = delete_routine(sid+0.5,set_deleter)
    end if
    return sid
end function

local procedure check_set(sequence s)
    -- helper for when we're kinda forced to use object 
    -- where we would much rather be using the set_type
    -- (this also helps solve some refcount issues too)
    for si in s do
        assert(set_type(si))
    end for
end procedure

global procedure discard_set(object s)
    if platform()=JS then return end if
    assert(and_bits(options,S_SID))
    if sequence(s) then
        check_set(s)
        for si in s do
            discard_set(si)
        end for
    else
        check_set({s})
        if integer(s) then
            set_deleter(s+0.5)
        else
            delete(s)
        end if
    end if
end procedure

global function get_members(set_type s, bool bDestroy=true)
    if sequence(s) then return s end if
    assert(and_bits(options,S_SID))
    integer id = floor(s)
    sequence res = sets[id]
    if bDestroy then
        if not integer(s) then delete(s) end if
        sets[id] = free_sets
        free_sets = id
    else
        res = deep_copy(res)
    end if
    return res
end function

global function set_size(set_type s)
--  if atom(s) then
--      return length(sets[floor(s)])
--  end if
--  return length(s)
    return length(get_members(s,false))
end function

global function is_empty(set_type s)
    --
    -- Returns true if x is empty.
    -- Symbolically represented with a circle with a diagonal through it (&empty;)
    --
    return set_size(s)=0
end function

-- Aside: settled on args of (s,x), to parallel (eg) append(), 
--               rather than (x,s), which parallels (eg) find(),
--        that is for the next few routines.

global function is_member(set_type s, object x)
    --
    -- Returns true if x is a member of s.
    -- Symbolically represented with an E shape (&isin;).   
    --
--  if atom(s) then
--      sequence set = sets[s]
--      bool res = binary_search(x,set)>0
--      return res
--  end if
--  return binary_search(x,s)>0
    return binary_search(x,get_members(s,false))>0
end function

atom lastTgt = 0

global procedure set_ignore_atom(object res)
    -- procedural calls to the next 7 get mapped to this by the compiler
    if platform()=JS or jsflag then
        crash("implicit discard not supported under JS")
    end if
    assert(atom(res),"implicit discard only permitted for atom results")
    assert(lastTgt!=-1,"last tgt of -1 should not be ignored")
end procedure

--DEV/SUG it might be better/faster to do these the other way round...
global function add_member(set_type s, object x)
    lastTgt = 0
    atom sid
    integer id
    bool bSid = atom(s)
    if bSid then
        sid = s
        id = floor(sid)
        s = sets[id]
    end if
    integer k = binary_search(x,s)
    if k<0 then
        if bSid then
            sets[id] = 0
        end if
        k = -k
        if k>length(s) then
            s = append(s,x)
        else
            s[k..k-1] = {x}
        end if
        if bSid then
            sets[id] = s
        end if
    end if
    return iff(bSid?sid:s)
end function

global function add_members(set_type s, sequence items)
    lastTgt = 0
    for x in items do
        s = add_member(s,x)
    end for
    return s
end function

global function remove_member(set_type s, object x)
    lastTgt = 0
    if sequence(s) then
        integer k = binary_search(x,s)
        if k>0 then
            s = deep_copy(s)
            s[k..k] = {}
        end if
        return s
    end if
    integer id = floor(s)
    sequence set = sets[id]
    integer k = binary_search(x,set)
    if k>0 then
        sets[id] = 0
--      set = deep_copy(set)
        set[k..k] = {}
        sets[id] = set
    end if
    return s
end function

global function remove_members(set_type s, sequence items)
    lastTgt = 0
    for si in items do
        s = remove_member(s,si)
    end for
    return s
end function

global function union(object s1, s2=-1, atom tgt=0)
    --
    -- Returns anything that occurs in any set.
    -- Symbolically represented with a U shape (&cup;).
    -- Can be invoked as union(s1,s2) or union({s1,s2}),
    --  where second form permits any number of sets.
    -- By default returns a sequence result, unless tgt is
    --  non_zero: +ve stores in that set and returns tgt,
    --            -1 creates and returns a brand new set.
    -- if tgt==iff(s2=-1?s1[1]:s1) then it performs a
    --  slightly more efficient in-situ update on that.
    -- The union of empty sets is the empty set, as is
    --  the union of an empty set of sets. Otherwise 
    --  empty sets have no impact on the result when 
    --  in the presence of other non-empty sets.
    --
    lastTgt = tgt
    if s2!=-1 then s1 = {s1,s2} end if
    check_set(s1)
    object res = iff(length(s1)?s1[1]:{})
    if atom(res) then
        bool insitu = res=tgt
        integer id = floor(res)
        res = sets[id]
        if insitu then
            sets[id] = 0        
        else
            res = deep_copy(res)
        end if
    end if
    if length(s1) then s1[1] = 0 end if
    for s2 in s1 from 2 do
        if atom(s2) then
            res &= sets[floor(s2)]
        else
            res &= s2
        end if
    end for
    res = unique(res)
    if tgt=-1 then
        res = new_set(res)
    elsif tgt and platform()!=JS then
        sets[floor(tgt)] = res
        res = tgt
    end if
    return res
end function

global function intersection(object s1, s2=-1, atom tgt=0)
    --
    -- Returns anything that occurs in every set.
    -- Symbolically represented as upsidedown U (&cap;)
    -- Can be invoked as intersection(s1,s2) or as
    --  intersection({s1,s2}), where s1 and s2 are 
    --  sids or sequences and the second form permits 
    --  more than two/any number of them.
    -- By default returns a sequence result, unless tgt is
    --  non_zero: +ve stores in that set and returns tgt,
    --            -1 creates and returns a brand new set.
    -- The intersection of empty sets is the empty 
    --  set, as is the intersection of an empty set 
    --  of sets, or a set of just one set. 
    --  The presence of any empty set means that 
    --  the result will also be the empty set.
    -- Aliased to intersect() in psym.e/syminit().
    --
    lastTgt = tgt
    if s2!=-1 then s1 = {s1,s2} end if
    check_set(s1)
    object res = iff(length(s1)?s1[1]:{})
    if atom(res) then
        bool insitu = res=tgt
        integer id = floor(res)
        res = sets[id]
        if insitu then
            sets[id] = 0        
        else
            res = deep_copy(res)
        end if
    end if
    if length(s1) then s1[1] = 0 end if
    for s2 in s1 from 2 do
        if atom(s2) then
            s2 = sets[floor(s2)]
        end if
        for i=length(res) to 1 by -1 do
            if binary_search(res[i],s2)<0 then
                res[i..i] = {}
            end if
        end for
        if length(res)=0 then exit end if
    end for
    if tgt=-1 then
        res = new_set(res)
    elsif tgt and platform()!=JS then
        sets[floor(tgt)] = res
        res = tgt
    end if
    return res
end function

global function difference(set_type s1, s2, bool symmetric=true, atom tgt=0)
    --
    -- Returns anything missing from some other set.
    -- if symmetric is false only items from s1 or s1[1] are returned,
    -- otherwise the result contains each and every item in any set 
    -- that is missing from any of the other sets.
    -- Obviously difference(s1,s2,true) is effectively the same as
    --           union(difference(s1,s2,false),difference(s2,s1,false))
    -- Symbolically represented as: symmetric: a triangle, or sometimes
    --                              as a circle with a horizontal line,
    --                              relative: a backslash (&setminus;)
    -- 
    lastTgt = tgt
    sequence ds2s1
    if symmetric then
        -- (save now lest we clobber sets[floor(s1)])
        ds2s1 = difference(s2,s1,false)
    end if
    object res = s1
    if atom(res) then
        bool insitu = res=tgt
        integer id = floor(res)
        res = sets[id]
        if insitu then
            sets[id] = 0        
        else
            res = deep_copy(res)
        end if
    else
        res = deep_copy(res)
    end if
    if atom(s2) then
        s2 = sets[floor(s2)]
    end if
    for i=length(res) to 1 by -1 do
        if binary_search(res[i],s2)>0 then
            res[i..i] = {}
        end if
    end for
    if symmetric then
        res = add_members(res,ds2s1)
    end if
    if tgt=-1 then
        res = new_set(res)
    elsif tgt and platform()!=JS then
        sets[floor(tgt)] = res
        res = tgt
    end if
    return res
end function

global function is_subset(set_type subset, superset)
    --
    -- return false if any element of subset is not in superset
    -- Symbolically represented as: underlined[?] C shape (&sub;)
    --
    for si in get_members(subset,false) do
        if not is_member(superset,si) then
            return false
        end if
    end for
    return true
end function

global function is_superset(set_type superset, subset)
    --
    -- returns true if all elements of subset are in superset
    -- Symbolically represented as: underlined[?] reverse C (&sup;)
    --
   return is_subset(subset, superset)
end function

