--
-- builtins\dict.e
-- ===============
--
--  The Phix implementation of dictionaries aka associative arrays.
--
-- See demo\rosetta\AVL_tree.exw for a slightly longer but perhaps more readable version.
--  (I stripped the comments here because no way am I maintaining them in parallel...)
-- Unlike the above, this allows multiple dictionaries ([tid] has appeared everwhere).
--
--without debug -- showing a massive "trees" in ex.err files is not normally helpful!

enum KEY = 0,
     DATA,
     LEFT,
     HEIGHT,    -- (NB +/-1 gives RIGHT/LEFT)
     RIGHT

sequence trees,
         treenames,
         roots,
         sizes,
         defaults,
         freelists
integer free_trees = 0,
        init_dict = 0

procedure dict_init()
    trees = {{}}
--  trees = repeat(repeat(0,0),0)
    treenames = {"1"}
    roots = {0}
    sizes = {0}
    defaults = {NULL}
    freelists = {0}
    init_dict = 1
end procedure

--DEV/SUG: (requires forward type [erm, may already be working, just not yet tried in psym/init]) ...needs MARKTYPES...
global type dictionary(integer tid)
    return tid=1 or (init_dict and tid>=1 and tid<=length(roots) and sequence(trees[tid]))
--  return tid=1 or (init_dict and tid>=1 and tid<=length(roots))
end type

global function is_dict(object tid)
    return tid=1 or (init_dict and integer(tid) and tid>=1 and tid<=length(roots) and sequence(trees[tid]))
--  return tid=1 or (init_dict and integer(tid) and tid>=1 and tid<=length(roots))
end function

procedure check_tid(integer tid)
--28/10/20: (even though p2js.js may yet choose a completely different approach for dictionaries...)
    if not is_dict(tid) then crash("invalid dictionary id (%d)",{tid},3) end if
--  if not is_dict(tid) then
--      #ilASM{ mov ecx,2           -- no of frames to pop to obtain an era (>=2)
--              mov al,56           -- e56idi (invalid dictionary id)
--              jmp :!fatalN        -- fatalN(level,errcode,ep1,ep2)
--            }
--  end if
    if not init_dict then dict_init() end if
end procedure

function newNode(object key, object data, integer tid)
    integer node = freelists[tid]
    if node=0 then
--p2js: (NOT QUITE SURE HERE...)
--/*
        node = length(trees[tid])+1
--      trees[tid] &= {key,data,NULL,1,NULL}
        -- much faster:
        trees[tid] = append(trees[tid],key)
        trees[tid] = append(trees[tid],data)
        trees[tid] = append(trees[tid],NULL)
        trees[tid] = append(trees[tid],1)
        trees[tid] = append(trees[tid],NULL)
--*/
        -- no measurable gain:
        sequence treet = trees[tid]
        if length(treet)=0 then
            treet = {key,data,NULL,1,NULL}
            node = 1
        else
            trees[tid] = 0
            treet = append(treet,key)
            node = length(treet)
            treet = append(treet,data)
            treet = append(treet,NULL)
            treet = append(treet,1)
            treet = append(treet,NULL)
        end if
        trees[tid] = treet
--      node = length(treet)
    else
        freelists[tid] = trees[tid][node]
        trees[tid][node+KEY..node+RIGHT] = {key,data,NULL,1,NULL}
    end if
    sizes[tid] += 1
    return node
end function

function height(integer node, integer branch, integer tid)
    node = trees[tid][node+branch]
    return iff(node=NULL ? 0 : trees[tid][node+HEIGHT])
end function

procedure setHeight(integer node, integer tid)
    trees[tid][node+HEIGHT] = max(height(node,LEFT,tid), height(node,RIGHT,tid))+1
end procedure

function rotate(integer node, integer direction, integer tid)
    integer idirection = LEFT+RIGHT-direction,
            pivot = trees[tid][node+idirection],
            temp2 = trees[tid][pivot+direction]
    trees[tid][pivot+direction] = node
    trees[tid][node+idirection] = temp2
    setHeight(node,tid)
    setHeight(pivot,tid)
    return pivot
end function

function getBalance(integer N, integer tid)
    return iff(N==NULL ? 0 : height(N,LEFT,tid)-height(N,RIGHT,tid))
end function

function insertNode(integer node, object key, object data, integer tid)
    if node==NULL then
        return newNode(key,data,tid)
    end if
    integer c = compare(key,trees[tid][node+KEY])
    if c=0 then
        trees[tid][node+DATA] = data
    else
        integer direction = HEIGHT+c    -- LEFT or RIGHT
        trees[tid][node+direction] = insertNode(trees[tid][node+direction], key, data, tid)
        setHeight(node,tid)
        integer balance = trunc(getBalance(node,tid)/2) -- +/-1 (or 0)
        if balance then
            direction = HEIGHT-balance  -- LEFT or RIGHT
            integer child = trees[tid][node+direction]
            c = compare(key,trees[tid][child+KEY])
            if c=balance then
                trees[tid][node+direction] = rotate(child,direction,tid)
            end if
            if c!=0 then
                node = rotate(node,LEFT+RIGHT-direction,tid)
            end if
        end if
    end if
    return node
end function

global procedure setd(object key, object data, integer tid=1)
    check_tid(tid)
    roots[tid] = insertNode(roots[tid], key, data, tid)
end procedure

global procedure putd(object key, object data, integer tid=1)
    setd(key, data, tid=1)
end procedure

-- (old name) now handled via psym.e/syminit()/Alias():
--global procedure putd(object key, object data, integer tid=1)
--  roots[tid] = insertNode(roots[tid], key, data, tid)
--end procedure

global procedure setd_default(object o, integer tid)
    check_tid(tid)
    defaults[tid] = o
end procedure

function getNode(integer node, object key, dflt, integer tid)
    while node!=NULL do
        integer c = compare(key,trees[tid][node+KEY])
        if c=0 then return trees[tid][node+DATA] end if
        integer direction = HEIGHT+c    -- LEFT or RIGHT
        node = trees[tid][node+direction]
    end while
    return dflt
end function

global function getd(object key, integer tid=1)
    check_tid(tid)
    return getNode(roots[tid], key, defaults[tid], tid)
end function

global function getdd(object key, dflt, integer tid=1)
    check_tid(tid)
    return getNode(roots[tid], key, dflt, tid)
end function

function getKey(integer node, object key, integer tid)
    while node!=NULL do
        integer c = compare(key,trees[tid][node+KEY])
        if c=0 then return node end if
        integer direction = HEIGHT+c    -- LEFT or RIGHT
        node = trees[tid][node+direction]
    end while
    return NULL
end function

global function getd_index(object key, integer tid=1)
    check_tid(tid)
    return getKey(roots[tid], key, tid)
end function

global function getd_by_index(integer node, integer tid=1)
    check_tid(tid)
    if node=0 then return 0 end if
    return trees[tid][node+DATA]
end function

function minValueNode(integer node, tid, direction)
    while 1 do
        integer next = trees[tid][node+direction]
        if next=NULL then exit end if
        node = next
    end while
    return node
end function

function deleteNode(integer root, object key, integer tid)
    if root=NULL then return root end if
    integer c = compare(key,trees[tid][root+KEY]),
            left = trees[tid][root+LEFT],
            right = trees[tid][root+RIGHT]
    if c=-1 then
        trees[tid][root+LEFT] = deleteNode(left, key, tid)
    elsif c=+1 then
        trees[tid][root+RIGHT] = deleteNode(right, key, tid)
    elsif left==NULL
       or right==NULL then
        integer temp = iff(left ? left : right)
        if temp==NULL then  -- No child case
            {temp,root} = {root,NULL}
        else                -- One child case
            trees[tid][root+KEY..root+RIGHT] = trees[tid][temp+KEY..temp+RIGHT]
        end if
        trees[tid][temp+KEY] = freelists[tid]
--4/2/21: (no gain, but left in)
        trees[tid][temp+DATA] = NULL
        freelists[tid] = temp
        sizes[tid] -= 1
    else                    -- Two child case
        integer temp = minValueNode(right,tid,LEFT)
        key = trees[tid][temp+KEY]
        trees[tid][root+KEY] = key
--bugfix 29/1/21:
        trees[tid][root+DATA] = trees[tid][temp+DATA]
        trees[tid][root+RIGHT] = deleteNode(right, key, tid)
    end if
    if root=NULL then return root end if
    setHeight(root,tid)
    integer balance = trunc(getBalance(root,tid)/2)
    if balance then
        integer direction = HEIGHT-balance
        c = compare(getBalance(trees[tid][root+direction],tid),0)
        if c=-balance then
            trees[tid][root+direction] = rotate(trees[tid][root+direction],direction,tid)
        end if
        root = rotate(root,LEFT+RIGHT-direction,tid)
    end if
    return root
end function

global procedure deld(object key, integer tid=1)
    check_tid(tid)
    roots[tid] = deleteNode(roots[tid],key,tid)
end procedure

function traverse(integer node, integer rid, object user_data, integer tid, bool rev)
    object tt = trees[tid],
           key = tt[node+KEY],
           data = tt[node+DATA]
    integer left = tt[node+LEFT],
            right = tt[node+RIGHT]
    tt = 0
    if rev then
        {left,right} = {right,left}
    end if
    if left!=NULL then
        if traverse(left,rid,user_data,tid,rev)=0 then return 0 end if
    end if
    if call_func(rid,{key,data,user_data})=0 then return 0 end if
    if right!=NULL then
        if traverse(right,rid,user_data,tid,rev)=0 then return 0 end if
    end if
    return 1
end function

global procedure traverse_dict(integer rid, object user_data=0, integer tid=1, bool rev=false)
    check_tid(tid)
    if roots[tid]!=0 then
        {} = traverse(roots[tid], rid, user_data, tid, rev)
    end if
end procedure

function traverse_key(integer node, integer rid, object pkey, object user_data, integer tid, bool rev)
    object tt = trees[tid],
           key = tt[node+KEY],
           data = tt[node+DATA]
    integer left = tt[node+LEFT],
            right = tt[node+RIGHT],
            c = compare(key,pkey)
    tt = 0
    if rev then
        {left,right} = {right,left}
        c = -c
    end if
    if left!=NULL and c>0 then
        if traverse_key(left,rid,pkey,user_data,tid,rev)=0 then return 0 end if
    end if
    if c>=0 then
        if call_func(rid,{key,data,pkey,user_data})=0 then return 0 end if
    end if
    if right!=NULL then
        if traverse_key(right,rid,pkey,user_data,tid,rev)=0 then return 0 end if
    end if
    return 1
end function

global procedure traverse_dict_partial_key(integer rid, object pkey, object user_data=0, integer tid=1, bool rev=false)
    check_tid(tid)
    if roots[tid]!=0 then
        {} = traverse_key(roots[tid], rid, pkey, user_data, tid, rev)
    end if
end procedure

--object gpk    -- NB not thread safe!

--function gpk_visitor(object key, object /*data*/, object /*pkey*/, object /*user_data=-2*/)
--  gpk = key
--  return 0    -- stop!
--end function
--constant r_gpkv = routine_id("gpk_visitor")

function traverser(sequence res, integer node, bool partial, object pkey, integer tid, bool rev)
    if node!=NULL then
        object tt = trees[tid],
               key = tt[node+KEY],
               data = tt[node+DATA]
        integer left = tt[node+LEFT],
                right = tt[node+RIGHT]
        tt = 0
        integer c = iff(partial?compare(key,pkey):0)
        if rev then
            {left,right} = {right,left}
        end if
        if left!=NULL then
            res = traverser(res, left, partial, pkey, tid, rev)
        end if  
        if c>=0 and (not partial or length(res)=0) then
            res = append(res,key)
        end if
        if right!=NULL and (not partial or length(res)=0) then
            res = traverser(res, right, partial, pkey, tid, rev)
        end if
    end if
    return res
end function

global function getd_partial_key(object pkey, integer tid=1, bool rev=false)
    check_tid(tid)
--if 0 then
--  gpk = defaults[tid]
--  if roots[tid]!=0 then
--      {} = traverse_key(roots[tid], r_gpkv, pkey, NULL, tid, rev)
--  end if
--  return gpk
--end if
    object res = traverser({}, roots[tid], true, pkey, tid, rev)
    if length(res) then
        res = res[1]
    else
        res = defaults[tid]
    end if
    return res
end function

global function getd_all_keys(integer tid=1)
    check_tid(tid)
--p2js:
--  return traverser({}, roots[tid], false, NULL, tid, false)
    sequence res = {}
    res = traverser(res, roots[tid], false, NULL, tid, false)
    return res
end function

global function dict_size(integer tid=1)
    check_tid(tid)
    return sizes[tid]
end function

function peekpop(integer tid, bool rev, bDelete)
    integer node = minValueNode(roots[tid],tid,iff(rev?RIGHT:LEFT))
    if node=0 then return defaults[tid] end if
    object key = trees[tid][node+KEY],
           data = trees[tid][node+DATA]
    if bDelete then
        roots[tid] = deleteNode(roots[tid],key,tid)
    end if
    return {key,data}
end function

global function peep_dict(integer tid=1, bool rev=false)
    return peekpop(tid,rev,false)
end function

global function pop_dict(integer tid=1, bool rev=false)
    return peekpop(tid,rev,true)
end function

global function dict_name(integer tid=1)
    check_tid(tid)
    return treenames[tid]
end function

global function named_dict(string name)
    for tid=1 to length(treenames) do
        if treenames[tid]=name then return tid end if
    end for
    return NULL
end function

----DEV temp: (didn't help...)
--function f(object o) return o end function
--if "abc"="def" then object x=f(1) x=f(1.5); x=f(""); x=f({1,1.5,"",{x}}) end if

global function new_dict(object kd_pairs = {}, integer pool_only=0)
--if "abc"="def" then object x=f(1) x=f(1.5); x=f(""); x=f({1,1.5,"",{x}}) end if
--kd_pairs = f(kd_pairs)
--pool_only = f(pool_only)
    if not init_dict then dict_init() end if
    integer tid = free_trees
    if tid!=0 then
        free_trees = trees[free_trees]
        trees[tid] = {}
        treenames[tid] = ""
        roots[tid] = NULL
        sizes[tid] = 0
        defaults[tid] = NULL
        freelists[tid] = 0
    elsif pool_only=0 then
        trees = append(trees,{})
--      trees = append(trees,repeat(0,0))
        treenames = append(treenames,"")
        roots &= NULL
        sizes &= 0
        defaults &= 0
        freelists &= 0
        tid = length(trees)
    elsif pool_only>1 then
        if length(kd_pairs) then ?9/0 end if
        if kd_pairs!={} then ?9/0 end if
        for i=1 to pool_only do
--25/8/16 (spotted in passing!)
--          trees = append(trees,{})
            trees = append(trees,free_trees)
            treenames = append(treenames,"")
            free_trees = length(trees)
            roots &= NULL
            sizes &= 0
            defaults &= 0
            freelists &= 0
        end for
    end if
    if string(kd_pairs) then
        treenames[tid] = kd_pairs
    elsif sequence(kd_pairs) then
        for i=1 to length(kd_pairs) do
            if length(kd_pairs[i])!=2 then ?9/0 end if
            object {key,data} = kd_pairs[i]
            setd(key,data,tid)
        end for
    else
        integer copy_tid = kd_pairs
        check_tid(copy_tid)     
--      trees[tid] = trees[copy_tid]
        trees[tid] = deep_copy(trees[copy_tid])
        freelists[tid] = freelists[copy_tid]
        -- programming note: the above is not really a copy, but 
        --                   of course the cow-semantics of phix
        --                   leaves one tree/freelist pair as-is
        --                   when the other pair is modified.
        roots[tid] = roots[copy_tid]
        sizes[tid] = sizes[copy_tid]
    end if
    return tid
end function

global procedure destroy_dict(integer tid, bool justclear=false)
--global procedure destroy_dict(dictionary tid, bool justclear=false)
--
-- Note: It is (and should be) perfectly legal to destroy_dict(1) at the very start.
--       In contrast, destroy_dict(5) is fatal when 5 is not a valid dictionary, or
--       (equivalently) was the recent subject of a destroy_dict() call.
--
    check_tid(tid)
    if tid=1 or justclear then
        -- just empty the default, but leave it still available
        -- (this also means that new_dict() can never return 1)
        trees[tid] = {}
--      trees[tid] = repeat(0,0)
        roots[tid] = NULL
        sizes[tid] = 0
        defaults[tid] = NULL
--      freelists[tid] = 0
        if freelists[tid]!=0 then ?9/0 end if
    else
        trees[tid] = free_trees
        roots[tid] = NULL
        sizes[tid] = 0
        defaults[tid] = NULL
        free_trees = tid
        freelists[tid] = 0
    end if
end procedure

