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

enum KEY = 0,
     DATA,
     LEFT,
     HEIGHT,    -- (NB +/-1 gives RIGHT/LEFT)
     RIGHT

sequence trees
sequence roots
sequence sizes
sequence freelists
integer free_trees = 0
integer initd = 0

procedure dinit()
    trees = {{}}
    roots = {0}
    sizes = {0}
    freelists = {0}
    initd = 1
end procedure

--DEV/SUG: (requires forward type [erm, may already be working, just not yet tried in psym/init])
--global type dictionary(integer tid)
--  return tid=1 or (initd and tid>=1 and tid<=length(roots) and sequence(trees[tid]))
--end type

global function is_dict(object tid)
    return initd and integer(tid) and tid>=1 and tid<=length(roots) and sequence(trees[tid])
end function

procedure check(integer tid)
    if tid!=1 and not is_dict(tid) then
        #ilASM{ mov ecx,2           -- no of frames to pop to obtain an era (>=2)
                mov al,56           -- e56idi (invalid dictionary id)
                jmp :!fatalN        -- fatalN(level,errcode,ep1,ep2)
              }
    end if
    if not initd then dinit() end if
end procedure

function newNode(object key, object data, integer tid)
integer node = freelists[tid]
    if node=0 then
        node = length(trees[tid])+1
--      trees[tid] &= {key,data,NULL,1,NULL}
        -- much faster:
        trees[tid] = append(trees[tid],key)
        trees[tid] = append(trees[tid],data)
        trees[tid] = append(trees[tid],NULL)
        trees[tid] = append(trees[tid],1)
        trees[tid] = append(trees[tid],NULL)
        -- no measurable gain:
--      sequence tree=trees[tid]
--      trees[tid] = 0
--      tree = append(tree,key)
--      node = length(tree)
--      tree = append(tree,data)
--      tree = append(tree,NULL)
--      tree = append(tree,1)
--      tree = append(tree,NULL)
--      trees[tid] = tree
    else
        freelists[tid] = trees[tid][node]
        trees[tid][node+KEY..node+RIGHT] = {key,data,NULL,1,NULL}
    end if
    sizes[tid] += 1
    return node
end function

function height(integer node, integer branch, integer tid)
    node = trees[tid][node+branch]
    return iff(node=NULL?0:trees[tid][node+HEIGHT])
end function

procedure setHeight(integer node, integer tid)
    trees[tid][node+HEIGHT] = max(height(node,LEFT,tid), height(node,RIGHT,tid))+1
end procedure

function rotate(integer node, integer direction, integer tid)
integer idirection = LEFT+RIGHT-direction
integer pivot = trees[tid][node+idirection]
integer Temp2 = trees[tid][pivot+direction]
    trees[tid][pivot+direction] = node
    trees[tid][node+idirection] = Temp2
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

global procedure setd(object key, object data, integer tid=1)   -- (aka putd)
    check(tid)
    roots[tid] = insertNode(roots[tid], key, data, tid)
end procedure

-- (old name) now handled via psym.e/syminit()/Alias():
--global procedure putd(object key, object data, integer tid=1)
--  roots[tid] = insertNode(roots[tid], key, data, tid)
--end procedure

function getNode(integer node, object key, integer tid)
    while node!=NULL do
        integer c = compare(key,trees[tid][node+KEY])
        if c=0 then return trees[tid][node+DATA] end if
        integer direction = HEIGHT+c    -- LEFT or RIGHT
        node = trees[tid][node+direction]
    end while
    return NULL
end function

global function getd(object key, integer tid=1)
    check(tid)
    return getNode(roots[tid], key, tid)
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
    check(tid)
    return getKey(roots[tid], key, tid)
end function

global function getd_by_index(integer node, integer tid=1)
    check(tid)
    if node=0 then return 0 end if
    return trees[tid][node+DATA]
end function

function minValueNode(integer node, integer tid)
    while 1 do
        integer next = trees[tid][node+LEFT]
        if next=NULL then exit end if
        node = next
    end while
    return node
end function

function deleteNode(integer root, object key, integer tid)
integer c, left, right
    if root=NULL then return root end if
    c = compare(key,trees[tid][root+KEY])
    left = trees[tid][root+LEFT]
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
        freelists[tid] = temp
        sizes[tid] -= 1
    else                    -- Two child case
        integer temp = minValueNode(right,tid)
        key = trees[tid][temp+KEY]
        trees[tid][root+KEY] = key
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
    check(tid)
    roots[tid] = deleteNode(roots[tid],key,tid)
end procedure

function traverse(integer node, integer rid, object user_data, integer tid, bool rev)
sequence tt = trees[tid]
object key = tt[node+KEY],
       data = tt[node+DATA]
integer left = tt[node+LEFT],
        right = tt[node+RIGHT]
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
    check(tid)
    if roots[tid]!=0 then
        {} = traverse(roots[tid], rid, user_data, tid, rev)
    end if
end procedure

function traverse_key(integer node, integer rid, object pkey, object user_data, integer tid, bool rev)
sequence tt = trees[tid]
object key = tt[node+KEY],
       data = tt[node+DATA]
integer left = tt[node+LEFT],
        right = tt[node+RIGHT]
integer c = compare(key,pkey)
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
    check(tid)
    if roots[tid]!=0 then
        {} = traverse_key(roots[tid], rid, pkey, user_data, tid, rev)
    end if
end procedure

object gpk  -- NB not thread safe!

function gpk_visitor(object key, object /*data*/, object /*pkey*/, object /*user_data=-2*/)
    gpk = key
    return 0    -- stop!
end function
constant r_gpkv = routine_id("gpk_visitor")

global function getd_partial_key(object pkey, integer tid=1, bool rev=false)
    check(tid)
    gpk = 0
    if roots[tid]!=0 then
        {} = traverse_key(roots[tid], r_gpkv, pkey, NULL, tid, rev)
    end if
    return gpk
end function

global function dict_size(integer tid=1)
    check(tid)
    return sizes[tid]
end function

global function new_dict(sequence kd_pairs = {}, integer pool_only=0)
    if not initd then dinit() end if
    integer tid = free_trees
    if tid!=0 then
        free_trees = trees[free_trees]
        trees[tid] = {}
        roots[tid] = NULL
        sizes[tid] = 0
        freelists[tid] = 0
    elsif pool_only=0 then
        trees = append(trees,{})
        roots &= NULL
        sizes &= 0
        freelists &= 0
        tid = length(trees)
    elsif pool_only>1 then
        for i=1 to pool_only do
--25/8/16 (spotted in passing!)
--          trees = append(trees,{})
            trees = append(trees,free_trees)
            free_trees = length(trees)
            roots &= NULL
            sizes &= 0
            freelists &= 0
        end for
    end if
    for i=1 to length(kd_pairs) do
        if length(kd_pairs[i])!=2 then ?9/0 end if
        object {key,data} = kd_pairs[i]
        setd(key,data,tid)
    end for
    return tid
end function

global procedure destroy_dict(integer tid, bool justclear=false)
--global procedure destroy_dict(dictionary tid, bool justclear=false)
--
-- Note: It is (and should be) perfectly legal to destroy_dict(1) at the very start.
--       In contrast, destroy_dict(5) is fatal when 5 is not a valid dictionary, or
--       (equivalently) was the recent subject of a destroy_dict() call.
--
    check(tid)
    if tid=1 or justclear then
        -- just empty the default, but leave it still available
        -- (this also means that new_dict() can never return 1)
        trees[tid] = {}
        roots[tid] = NULL
        sizes[tid] = 0
--      freelists[tid] = 0
        if freelists[tid]!=0 then ?9/0 end if
    else
        trees[tid] = free_trees
        roots[tid] = NULL
        sizes[tid] = 0
        free_trees = tid
        freelists[tid] = 0
    end if
end procedure

