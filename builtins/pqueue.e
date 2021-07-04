--
-- builtins/pqueue.e
-- =================
--
-- Basic implementation of priority queues (an autoinclude).
--
--  A priority queue is kind of fast to-do list, whereby you can add items,
--  as many as you want, in any order, and quickly retrieve the lowest (if 
--  it is a MIN_HEAP) or highest(if it is a MAX_HEAP) element - one or the 
--  other per list only, not both.
--
--  Relatively small lists gain little, however I can attest to seeing this
--  thoroughly trounce an earlier version of a program, that had to process 
--  over 800,000 numbers in a different order to the way they were created, 
--  by a factor of 17.5 (0.8s vs 14s), yet the only real difference was how 
--  those numbers were stored/sorted.
--
--
-- Interface
-- =========
--
--  integer pqid = pq_new(integer t=MIN_HEAP) creates a new priority queue.
--              [optional, omit pqid below to use the default common queue]
--              The argument t must be either MIN_HEAP (ie pop 1 before 2)
--              or MAX_HEAP (ie pop 2 before 1). Note the default queue is
--              always a MIN_HEAP (ie pqid of 1 in the calls below).
--
--  pq_size(integer pqid=1) obtains the number of entries currently in the list.
--
--  pq_empty(integer pqid=1) yields true when the list is empty, false otherwise.
--
--  pq_add(sequence item, integer pqid=1) adds an item to a priority queue, where
--                                        item is {object data, object priority}.
--
--  sequence item = pq_pop(integer pqid=1) obtains highest/lowest priority item.
--                           The item returned is {object data, object priority}.
--
--  object data = pq_pop_data(integer pqid=1) as pq_pop but discards priority.
--                          There would be no way to determine what the priority 
--                          was, unless you had buried it somewhere inside data.
--
--  pq_destroy(integer pqid=1, bool justclear=false) removes/frees a queue once
--                          no longer needed. [pqid of 1 is always just cleared]
--
--  Traditionally the priority is an integer, but here it can also be a float, 
--  a string, or even a complex nested sequence.
--
--  Obviously, using the default queue (of 1) can cause problems if it is used 
--  for different things simultaneously, but also obviously is quite handy for 
--  smaller/simpler (quick 'n dirty) programs.
--
--
-- Technial details
-- ================
--
-- A priority queue is a tree stored in a flat list, as follows
--
--  tree            1
--                2   3
--               4 5 6 7
--
--             -   1   1   2   2   3   3    <-- parent idx (virtual)
--  node       1   2   3   4   5   6   7    <-- this is sequence pq[pqid]
--            2,3 4,5 6,7  -   -   -   -    <-- children idx (virtual)
--
-- The tree root is at index 1, with valid indices 1 through n, and an element 
-- at index i having:
--  * children at indices 2i and 2i+1 - absent if > length(), of course.
--  * a parent at index floor(i/2) - except, obviously, for the root at [1].
--
-- Shape property: The tree is always fully filled, except for the last row.
--                 The last row is always left-filled, without any gaps.
-- Heap property: The key in each node is always >= or <= its childrens keys.
--   (Note there is no guaranteed left/right relation of any child keys.)
--
-- Insertion proceeds as follows (see also flat table form below):
--
--               8                       8                  9
--            5     7       ==>       5     9   ==>      5     8
--          3  4  X(=9)             3  4  7            3  4  7
--
-- ie we add 9 at X (pq[$+1]), then swap it with parent (upheap) as needed.
-- Note that the 9 is initially placed where it is not because of any relation
-- to the 8, 7 or 5, but to satisfy the shape constraint. Likewise both left
-- and right children are swapped with their parent identically, and without 
-- any consideration of the value in the other sibling (if present).
-- Alt: first observe the shape constraint, then enforce the heap constraint.
--
-- Removal (of root, the only type of removal supported) proceeds as follows:
--
--               9                       6                 8
--            8     5       ==>       8     5   ==>     6     5
--          3  6                    3                 3 
--
-- ie root := 6 (ie pq[$]), then swap with larger/smaller child (downheap) as needed.
-- The downheap of 6 stops early here because it is greater than all of its children.
-- Note that shape and heap constraints are satisfied both initially and on completion,
-- even though to the casual onlooker the initial 8 and 5, and the final 6 and 5 might 
-- at first glance appear to be the wrong way round.
--
-- Naturally the code below belays actually planting the 9/6 until the final step.
-- Rewriting the above examples in flat table form, with notional _ markers:
--
--          Insertion: 8 5 7 3 4 _          Removal: 9 8 5 3 6
--                 ==> 8 5 _ 3 4 7               ==> _ 8 5 3
--                 ==> _ 5 8 3 4 7               ==> 8 _ 5 3
--                 ==> 9 5 8 3 4 7               ==> 8 6 5 3
--
-- It should now be clear that those operations are very low cost, and in fact both
-- insertion and removal are O(log2(n)) operations.
--
enum PDATA, PRIORITY

--global enum MIN_HEAP = -1, MAX_HEAP = +1  -- now in psym.e

bool pqinit = false
sequence pq, pqtype, pqcrid
integer freelist = 0

function pq_compare(object priority1, priority2)
    return compare(priority1,priority2)
end function
--constant PQ_COMPARE = routine_id("pq_compare")

procedure pq_init()
    pq = {{}}
    pqtype = {MIN_HEAP}
--26/9/19:
--  pqcrid = {-1}
    pqcrid = {pq_compare}
    pqinit = true
end procedure

--global function pq_new(integer t=MIN_HEAP, crid=pq_compare)
global function pq_new(integer t=MIN_HEAP, crid=-2)
    if t!=MIN_HEAP and t!=MAX_HEAP then ?9/0 end if
    if crid=-2 then crid = pq_compare end if
    if not pqinit then pq_init() end if
    integer pqid
    if freelist=0 then
        pq = append(pq,{})
        pqtype = append(pqtype,t)
        pqcrid = append(pqcrid,crid)
        pqid = length(pq)
    else
        pqid = freelist
        freelist = pqtype[freelist]
        pq[pqid] = {}
        pqtype[pqid] = t
        pqcrid[pqid] = crid
    end if
    return pqid
end function

--global procedure pq_destroy(integer pqid=1, bool justclear=false, integer crid=pq_compare)
global procedure pq_destroy(integer pqid=1, bool justclear=false, integer crid=-2)
    if not pqinit then pq_init() end if
    if not sequence(pq[pqid]) then ?9/0 end if
    if crid=-2 then crid = pq_compare end if
    if pqid=1 or justclear then
        pq[pqid] = {}
        pqcrid[pqid] = crid
    else
        pqtype[pqid] = freelist
        freelist = pqid
        pq[pqid] = 0
    end if
end procedure

global function pq_size(integer pqid=1)
    if not pqinit then pq_init() end if
    return length(pq[pqid])
end function

global function pq_empty(integer pqid=1)
--  if not pqinit then pq_init() end if
    return pq_size(pqid)=0
end function

global procedure pq_add(sequence item, integer pqid=1)
-- item is {object data, object priority}
    if not pqinit then pq_init() end if
    if length(item)!=2 then ?9/0 end if
    integer n = length(pq[pqid])+1,
            m = floor(n/2),
            heap_type = pqtype[pqid],
            crid = pqcrid[pqid]
--  pq[pqid] &= 0
--  pq[pqid] = deep_copy(pq[pqid]) & 0
    sequence pqp = deep_copy(pq[pqid]) & 0
--  pq[pqid] = deep_copy(pq[pqid] & 0)
    -- append at end, then up heap
--  while m>0 and call_func(crid,{item[PRIORITY],pq[pqid][m][PRIORITY]})=heap_type do
    while m>0 and call_func(crid,{item[PRIORITY],pqp[m][PRIORITY]})=heap_type do
--      pq[pqid][n] = pq[pqid][m]
        pqp[n] = pqp[m]
        n = m
        m = floor(m/2)
    end while
--  pq[pqid][n] = item
    pqp[n] = item
    pq[pqid] = pqp
end procedure
 
global function pq_pop(integer pqid=1)
--  if not pqinit then pq_init() end if     -- (would crash next either way)
    sequence result = pq[pqid][1]
 
    integer qn = length(pq[pqid]),
            n = 1,
            m = 2,
            heap_type = pqtype[pqid],
            crid = pqcrid[pqid]
    while m<qn do
        if m+1<qn and call_func(crid,{pq[pqid][m+1][PRIORITY],pq[pqid][m][PRIORITY]})=heap_type then
            m += 1
        end if 
        if call_func(crid,{pq[pqid][m][PRIORITY],pq[pqid][qn][PRIORITY]})!=heap_type then exit end if
        pq[pqid][n] = pq[pqid][m]
        n = m
        m = m * 2
    end while
    pq[pqid][n] = pq[pqid][qn]
    pq[pqid] = pq[pqid][1..$-1]
    return result
end function

global function pq_pop_data(integer pqid=1)
    return pq_pop(pqid)[PDATA]
end function

global function pq_peek(integer pqid=1)
--  if not pqinit then pq_init() end if     -- (would crash next either way)
    sequence result = pq[pqid][1]
    return result
end function

