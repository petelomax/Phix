--
-- builtins\sets2.e
-- ================
--
--  Basic set operations.
--
--  Experimental/subject to change.
--  I wrote this as a replacement for sets.e - for no particularly good reason...
--  So I decided to keep both, more testing is required to see which is better
--  and of course the docs would need updating.
--
--  Maybe a set should be {flags,s} [always length s] where (say)
--       flags      s
--      ORDERED     s is kept == unique(s) at all times, binary_search() valid
--     UNORDERED    s is an unordered sequence
--       DICT       s is a dictionary
--
--include dict.e -- (maybe...?)

bool s_init = false
sequence sets

procedure init_s()
    s_init = true
    sets = {}
end procedure

global function is_member(integer sid, object x)
    --
    -- Returns true if x is a member of set.
    -- Symbolically represented with an E shape.    
    -- args are (sid,x), to parallel (eg) append, rather than
    --          (x,sid), which parallels (eg) find
    --
--  object set = sets[sid]
    sequence set = sets[sid]
    bool res = binary_search(x,set)>0
--  bool res = iff(sequence(set)?binary_search(x,set)>0
--                              :getd_index(x,set)!=0)
    return res
end function

global function get_members(integer sid)
--  object set = sets[sid]
--  if integer(set) then
--      set = getd_all_keys(set)
--  end if
--  return set
    return sets[sid]
end function

global procedure add_member(integer sid, object x)
--  object set = sets[sid]
    sequence set = sets[sid]
--  if sequence(set) then
    integer k = binary_search(x,set)
    if k<0 then
        sets[sid] = 0
        k = -k
        if k>length(set) then
            set = append(set,x)
        else
            set = set[1..k-1] & 0 & set[k..$]
            set[k] = x
        end if
        sets[sid] = set
    end if
--  else
--      setd(x,0,set)
--  end if
end procedure

global procedure add_members(integer sid, sequence items)
--  object set = sets[sid]
    sequence set = sets[sid]
    integer l = length(set)
    sets[sid] = 0
--  if sequence(set) then
    if length(set)=0 then
        set = unique(deep_copy(items,1))
    else
        for i=1 to length(items) do
            object x = items[i]
            integer k = binary_search(x,set)
            if k<0 then
                k = -k
                if k>l then
                    set = append(set,x)
                else
--                  set = set[1..k-1] & {x} & set[k..$]
--                  set = set[1..k-1] & 0 & set[k..$]
--                  set[k] = x
                    set[k..k-1] = {x}
                end if
                l += 1
            end if
        end for
    end if
        sets[sid] = set
--  else
--      for i=1 to length(items) do
--          setd(items[i],0,set)
--      end for
--  end if
end procedure

--DEV what about dicts??
function set_default(sequence s1, object s2=-1)
    -- return {} or "" as the new set default
    bool bAllString = true
--???
--  bool bAllString = string(s1)
    if s2!=-1 then
        bAllString = string(s1) and string(s2)
    elsif not string(s1) then
--what about s2??
        for i=1 to length(s1) do
            if not string(s1[i]) then
                bAllString = false
                exit
            end if
        end for
    end if
    return iff(bAllString?"":{})
end function

--DEV to psym.e
--global constant SET_TYPE_DICT=0, SET_TYPE_SEQ=1
--, SET_TYPE_SORTED=3

--global function new_set(object items={}, integer set_type=SET_TYPE_DICT)
global function new_set(object items={})
    if not s_init then init_s() end if
    if integer(items) then
        items = deep_copy(sets[items])
    else
        items = unique(items)
    end if
--  sets = append(sets,iff(set_type=SET_TYPE_DICT?new_dict():items))
--  sets = append(sets,iff(set_type=SET_TYPE_DICT?new_dict()
--                                               :set_default(items)))
--  sets = append(sets,set_default(items))
    sets = append(sets,items)
--  sets = append(sets,iff(set_type=SET_TYPE_DICT?new_dict():
--                     iff(set_type=SET_TYPE_SORTED?items:{})))
    integer sid = length(sets)
--  if length(items) then
----    if set_type!=SET_TYPE_SORTED and length(items) then
--      add_members(sid,items)
--  end if
    return sid
end function

global procedure remove_member(integer sid, object x)
--  object set = sets[sid]
    sequence set = sets[sid]
--  if sequence(set) then
        integer k = binary_search(x,set)
        if k>0 then
            sets[sid] = 0
            set[k..k] = {}
            sets[sid] = set
        end if
--  else
----        if getd_index(x,set)!=0 then
--          deld(x,set)
----        end if
--  end if
end procedure

global procedure remove_members(integer sid, sequence s)
    for i=1 to length(s) do
        remove_member(sid,s[i])
    end for
end procedure

--global function union(object s1, s2=0, integer set_type=SET_TYPE_SEQ)
global function union(object s1, s2=0, tgt=0)
    --
    -- Returns anything that occurs in any set.
    -- Symbolically represented with a U shape.
    -- Can be invoked as union(s1,s2) or union({s1,s2}),
    --  where s1 and s2 are sids or sequences of them and
    --  the second form permits any number (>=0) of them.
    -- By default returns a sequence result, unless tgt is
    --  non_zero: +ve stores in that set and returns tgt,
    --            -1 creates and returns a brand new set.
--X -- By default returns a sequence result, unless set_type
--X --  is SET_TYPE_DICT in which case it returns a new sid.
    -- The union of empty sets is the empty set, as is
    --  the union of an empty set of sets. Otherwise 
    --  empty sets have no impact on the result when 
    --  in the presence of other non-empty sets.
--X -- An error ocurs when s1 or s2 or elements within
--X --  that should be sequences are in fact atoms.
--erm... [done]
    -- Strings (char sets) are perfectly fine too, but
    -- if s1 is a string then s2 must be one too, or 
    -- a sequence of strings, and target must be 0.
    -- Note: you cannot mix sids and strings, or use
    -- literal sequences except via new_set().
    --
    bool in_situ = tgt==s1
--  sequence res
--  s1 = deep_copy(s1)
    if string(s1) then
--      ?9/0 -- TBC...???
        assert(tgt=0)
        if not string(s2) then
            s2 = join(s2,"")
        end if
        assert(string(s2))
        s1 = unique(s1&s2)
--      else
--          s1 = unique(s1&join(s2,""))
--      end if
    elsif s2!=0 then
        -- assume union(s1,s2) style invocation
        --  sid,(any),tgt===sid: in situ
        --  else create a sequence result/temp
--      if sequence(s1) then
--          s1 = deep_copy(s1)
--      elsif tgt!=s1 then
        s1 = sets[s1]
        if in_situ then
            sets[tgt] = 0
        else
            s1 = deep_copy(s1,1)
        end if
--          if integer(s1) then
--              s1 = getd_all_keys(s1)
--          end if
--      end if  
--      if integer(s2) then
            s2 = sets[s2]
--          if integer(s2) then
--              s2 = getd_all_keys(s2)
--          end if
--      end if  
        integer l = length(s1)
        for i=1 to length(s2) do
            object x = s2[i]
--          if sequence(s1) then
--          if not in_situ then
                integer k = binary_search(x,s1)
                if k<0 then
                    k = -k
                    if k>l then
                        s1 = append(s1,x)
                    else
--                      s1 = s1[1..k-1] & 0 & s1[k..$]
--                      s1[k] = x
                        s1[k..k-1] = {x}
                    end if
                    l += 1
                end if
--          elsif getd_index(x,tgt)==0 then
--              setd(x,0,tgt)
--          end if
        end for

--      if in_situ then
        if tgt!=0 then
--          s1 = tgt
--oh bollocks, sequence...
--          tgt = sets[tgt]
            if tgt=-1 then
                tgt = new_set(s1)
--          tgt = new_set(s1)
--      elsif tgt then
----            tgt = 
--          
--          elsif not in_situ then
            else
                sets[tgt] = s1
            end if
--      if tgt!=0 and not in_situ then
--          if tgt=-1 then
--              s1 = new_set(s1)
--          else
--              s2 = sets[tgt]
--              if sequence(s2) then
--                  sets[tgt] = s1
--              else
--                  destroy_dict(tgt,true)
--                  add_members(tgt,s1)
--                  s1 = tgt
--              end if
--          end if
            s1 = tgt
        end if
    elsif length(s1) then
        -- assume union({s1,s2,...}) style invocation
        s2 = s1[2..$]
        s1 = s1[1]
--      if not set_type(s1) then ?9/0 end if
        for i=1 to length(s2) do
--          if not set_type(s2[i]) then ?9/0 end if
            s1 = union(s1,s2[i],tgt)
            if tgt=-1 then tgt=s1 end if
        end for
    end if
--  return iff(set_type=SET_TYPE_DICT?new_set(s1):s1)
    return s1
end function

global function intersection(object s1, s2=-1, integer tgt=0)
    --
    -- Returns anything that occurs in every set.
    -- Symbolically represented as upsidedown U
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
--X -- An error ocurs when s1 or s2 or elements within
--X --  that should be sequences are in fact atoms.
    -- While s1 can be a set of sets, if at the same 
    --  time s2 is a set or set of sets, the result
    --  is then completely undefined and unsupported.
    -- Strings (char sets) are perfectly fine too, but
    -- if s1 is a string then s2 must be one too, or 
    -- a sequence of strings, and target must be 0.
    -- Note: you cannot mix sids and strings, or use
    -- literal sequences except via new_set().
    --
    bool in_situ = tgt==s1
    if string(s1) then
        assert(tgt=0)
        string res = ""
        s1 = unique(s1)
--      if not string(s2) then
--          s2 = join(s2,"")
--      end if
--      assert(string(s2))
--      s1 = unique(s1&s2)
        for i=1 to length(s1) do
            integer ch = s1[i]
--          if not find(ch,res) then
                if string(s2) then
                    if find(ch,s2) then
                        res &= ch
                    end if
                else
                    for j=length(s2) to 1 by -1 do
                        string sj = s2[j]
                        if not find(ch,sj) then exit end if
                        if j=1 then res &= ch end if
                    end for
                end if
--          end if
        end for
        s1 = res
--  elsif 
--  set_type res = set_default(s1,s2)
    else
        sequence res = {}
        if s2!=-1 then
            -- assume intersection(s1,s2) style invocation
            s1 = sets[s1]
            if in_situ then
                sets[tgt] = 0
            else
                s1 = deep_copy(s1,1)
            end if
            s2 = sets[s2]
            if string(s1) then res = "" end if
--          wsequence res = iff(string(s1):"":{})
--      if not set_type(s2) then ?9/0 end if
            for i=1 to length(s1) do
                object si = s1[i]
                if find(si,s2) then
                    res = append(res,si)
                end if
            end for
        else
            -- assume intersection({s1,s2,s3...}) style invocation
            if length(s1) then
                sequence s = s1[1]
                if string(s) then res = "" end if
                for i=1 to length(s) do
                    object si = s[i]
                    if not find(si,res) then
                        for j=length(s1) to 2 by -1 do
--                          set_type sj = s1[j]
                            sequence sj = sets[s1[j]]
                            if not find(si,sj) then exit end if
                            if j=2 then res = append(res,si) end if
                        end for
                    end if
                end for
            end if
        end if
        if tgt!=0 then
            if tgt=-1 then
                tgt = new_set(res)
            else
                sets[tgt] = res
            end if
            s1 = tgt
        else
            s1 = res
        end if
--      s1 = res
    end if
--  return res
    return s1
end function

--global function difference(sequence s1, object s2=-1, bool symmetric=true)
global function difference(object s1, s2=-1, integer tgt=0, bool symmetric=true)
    --
    -- Returns anything missing from some other set.
    -- if symmetric is false only items from s1 or s1[1] are returned,
    -- otherwise the result contains each and every item in any set 
    -- that is missing from any of the other sets.
    -- Obviously difference(s1,s2,0,true) is effectively the same as
    --           union(difference(s1,s2,0,false),difference(s2,s1,0,false))
    --  [slightly more complicated than that when s1 is a set of sets]
    -- Symbolically represented as: symmetric: a triangle, or sometimes
    --                              as a circle with a horizontal line,
    --                              relative: a backslash
    -- Strings (char sets) are perfectly fine too, but
    -- if s1 is a string then s2 must be one too, or 
    -- a sequence of strings, and target must be 0.
    -- Note: you cannot mix sids and strings, or use
    -- literal sequences except via new_set().
    -- 
--  bool in_situ = tgt==s1 -- we must build a new set anyway...
    sequence res
    if string(s1) then
        assert(tgt=0)
        res = ""
        s1 = unique(s1)
        if string(s2) then
            s1 = {s1,s2}
        else
            for i=1 to length(s2) do assert(string(s2[i])) end for
--          s1 = prepend(deep_copy(s2,1),s1)
--          s2 = deep_copy(s2,1)
--          s1 = prepend(s2,s1)
            s1 = {s1}&s2
        end if
        for h=1 to iff(symmetric?length(s1):1) do
            string s = s1[h]
            for i=1 to length(s) do
                integer ch = s[i]
                if not find(ch,res) then
                    for j=1 to length(s1) do
                        if j!=h and not find(ch,s1[j]) then
                            res &= ch
                            exit 
                        end if
                    end for
                end if
            end for
        end for
        s1 = res
--/*
    els
        for i=1 to length(s1) do
            integer ch = s1[i]
            if string(s2) then
--              if not find(ch,res) and -- (handled via unique)
                if not find(ch,s2) then
                    res &= ch
                end if
            else
--              for j=length(s2) to 1 by -1 do
                for j=1 to length(s2) do
                    string sj = s2[j]
--                  if not find(ch,sj) then exit end if
--                  if j=1 then res &= ch end if
                    if not find(ch,sj) then
                        res &= ch
                        exit
                    end if
                end for
            end if
        end for
        if symmetric then
            if string(s2) then
                for i=1 to length(s2) do
                    integer ch = s2[i]
                    if not find(ch,res)
                    and not find(ch,s1) then
                        res = &=ch
                    end if
                end for
            else
                for j=1 to length(s2) do
                    string sj = s2[j]
                    if not find(ch,sj) then
                        res &= ch
                        exit
                    end if
                end for
            end if
        end if

    set_type res = set_default(s1,s2)
--*/
    else
        if s2!=-1 then
            -- assume difference(s1,s2) style invocation
            s1 = {s1,s2}
        end if
--      if not set_type(s1) then ?9/0 end if
--      if not set_type(s2) then ?9/0 end if
--      for i=1 to length(s1) do
--          object si = s1[i]
--          if not find(si,res)
--          and not find(si,s2) then
--              res = append(res,si)
--          end if
--      end for
--      if symmetric then
--          for i=1 to length(s2) do
--              object si = s2[i]
--              if not find(si,res)
--              and not find(si,s1) then
--                  res = append(res,si)
--              end if
--          end for
--      end if
--  else
        -- assume difference({s1,s2,s3...}) style invocation
--      res = {}
        res = {}
        if length(s1) then
            res = ""
            integer sid
            for i=1 to length(s1) do
                sid = s1[i] -- (assertion point)
                if not string(sets[sid]) then
                    res = {}
                    exit
                end if
            end for
            for h=1 to iff(symmetric?length(s1):1) do
                sid = s1[h] -- (assertion point)
                sequence s = sets[sid]
                for i=1 to length(s) do
                    object si = s[i]
                    if not find(si,res) then
                        for j=1 to length(s1) do
                            if j!=h then
                                sid = s1[j] -- (assertion point)
                                sequence sj = sets[sid]
                                if not find(si,sj) then
                                    res = append(res,si)
                                    exit 
                                end if
                            end if
                        end for
                    end if
                end for
            end for
        end if
        if tgt!=0 then
            if tgt=-1 then
                tgt = new_set(res)
            else
                sets[tgt] = res
            end if
            s1 = tgt
        else
            s1 = res
        end if
    end if
--  return res
    return s1
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

global function is_subset(object subset, superset)
    --
    -- return false if any element of subset is not in superset
    -- Symbolically represented as: underlined C shape
    --
    if string(subset) then
        assert(string(superset))
    else
--      assert(integer(subset))
--      assert(integer(superset))
        subset = sets[subset]
        superset = sets[superset]
    end if
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

global function is_superset(object superset, subset)
    --
    -- returns true if all elements of subset are in superset
    -- Symbolically represented as: underlined reverse C
    --
   return is_subset(subset, superset)
end function

global function is_sameset(object set1, set2)
    if string(set1) then
        assert(string(set2))
--      return unique(set1)=unique(set2)
        set1 = unique(set1)
        set2 = unique(set2)
    else
        set1 = sets[set1]
        set2 = sets[set2]
    end if
--  return sets[set1]=sets[set2]
    return set1==set2
end function

--/*
--test code:
include sets2.e
integer set1 = new_set({1,2,3}),
        set2 = new_set({3,4,5}),
        set3 = new_set()
?{set1,get_members(set1)}       -- {1,{1,2,3}}
?{set2,get_members(set2)}       -- {2,{3,4,5}}
?is_member(set1,3)              -- 1 (aka true)
?is_member(set1,4)              -- 0 (aka false)
?union(set1,set2)               -- {1,2,3,4,5}
?union("123","345")             -- "12345"
?get_members(set1)              -- {1,2,3} (ie unchanged)
?{set3,get_members(set3)}       -- {3,{}}
?union(set1,set2,set3)          -- 3 (ie the sid of set3)
?get_members(set3)              -- {1,2,3,4,5}
?"==="
?get_members(set2)              -- {3,4,5}
?intersection(set1,set2)        -- {3}
?intersection("123","345")      -- "3"
?intersection(set1,set2,set3)   -- 3 (ie the sid of set1)
?get_members(set3)              -- {3}
?"==="
?difference(set1,set2)          -- {1,2,4,5}
?difference(set1,set2,0,false)  -- {1,2}
?difference({set1,set2},-1,0,false) -- {1,2}
?difference({set1,set2})        -- {1,2,4,5}
?difference("123","345")        -- "1245"
?difference("123",{"345"},0,false) -- "12"
{} = difference(set1,set2,set3,false)
?get_members(set3)              -- {1,2}
{} = difference(set1,set2,set3)
?get_members(set3)              -- {1,2,4,5}
?is_subset(set1,set2)           -- 0
?union(set1,set2,set2)          -- 2 (ie the sid of set2)
?get_members(set2)              -- {1,2,3,4,5}
?is_subset(set1,set2)           -- 1
?is_sameset(set1,set2)          -- 0
remove_member(set2,4)
--remove_member(set2,'4')
remove_member(set2,5)
--remove_member(set2,'5')
?is_sameset(set1,set2)          -- 1
?get_members(set1)              -- {1,2,3}
--integer set3 = new_set({3,1,2})
--integer set3 = new_set("312")
--add_members(integer sid, sequence items)
remove_members(set3,deep_copy(get_members(set3)))
?"==="
?get_members(set3)              -- {}
add_members(set3, {3,1,2})
?get_members(set3)              -- {}
?is_sameset(set1,set3)          -- 1
?is_sameset("123","312")        -- 1

-- above converted to a unit test...
include sets2.e
integer set1 = new_set({1,2,3}),
        set2 = new_set({3,4,5}),
        set3 = new_set()
assert({set1,get_members(set1)}=={1,{1,2,3}})
assert({set2,get_members(set2)}=={2,{3,4,5}})
assert(is_member(set1,3))
assert(not is_member(set1,4))
assert(union(set1,set2)=={1,2,3,4,5})
assert(union("123","345")=="12345")
assert(get_members(set1)=={1,2,3})
assert({set3,get_members(set3)}=={3,{}})
assert(union(set1,set2,set3)==set3)
assert(get_members(set3)=={1,2,3,4,5})

assert(get_members(set2)=={3,4,5})
assert(intersection(set1,set2)=={3})
assert(intersection("123","345")=="3")
assert(intersection(set1,set2,set3)==set3)
assert(get_members(set3)=={3})

assert(difference(set1,set2)=={1,2,4,5})
assert(difference(set1,set2,0,false)=={1,2})
assert(difference({set1,set2},-1,0,false)=={1,2})
assert(difference({set1,set2})=={1,2,4,5})
assert(difference("123","345")=="1245")
assert(difference("123",{"345"},0,false)=="12")
assert(difference(set1,set2,set3,false)==set3)
assert(get_members(set3)=={1,2})
assert(difference(set1,set2,set3)==set3)
--Xassert(get_members(set3)=={1,2,4,5}) [p2js violation]
sequence s3 = get_members(set3)
assert(s3=={1,2,4,5})
s3 = {}
assert(not is_subset(set1,set2))
assert(union(set1,set2,set2)=set2)
--Xassert(get_members(set2)=={1,2,3,4,5}) [p2js violation]
sequence s2 = get_members(set2)
assert(s2=={1,2,3,4,5})
s2 = {}

assert(is_subset(set1,set2))
assert(not is_sameset(set1,set2))
remove_member(set2,4)
remove_member(set2,5)
assert(is_sameset(set1,set2))
assert(get_members(set1)=={1,2,3})
remove_members(set3,deep_copy(get_members(set3)))

assert(get_members(set3)=={})
add_members(set3, {3,1,2})
assert(get_members(set3)=={1,2,3})
assert(is_sameset(set1,set3))
assert(is_sameset("123","312"))

--*/
