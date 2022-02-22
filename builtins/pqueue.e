--
-- builtins/pqueue.e
-- =================
--
--  Basic implementation of priority and other queues (an autoinclude).
--
-- Priority queues (see about half way down for traditional queues/stacks)
-- ===============
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
--  Also implements standard (FIFO) queues and (LIFO) stacks.
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
-- Technical details
-- =================
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
--15/2/22 (spotted in passing, gotta be better!)
--  sequence pqp = deep_copy(pq[pqid]) & 0
    sequence pqp = pq[pqid]
    pq[pqid] = 0
    pqp &= 0
--  pq[pqid] = pqp
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
--15/2/22: (erm, not tried...)
    sequence result = pq[pqid][1]
--  sequence result = deep_copy(pq[pqid][1])
    return result
end function

--
-- Traditional queues and stacks
-- =============================
--
--  Fairly obviously a queue is exactly like the one in a shop, you do not expect
--  people arriving after you to be served before you, and a stack is exactly like
--  a pile of plates in a kitchen, the ones on the top are regularly removed and
--  replaced, whereas the ones at the bottom could be there for months even years.
--
-- Interface
-- =========
--
--  integer qid = new_queue(*)              -- create a new FIFO queue
--  integer qid = new_stack()               -- create a new LIFO stack
--  push(integer qid, object item*)         -- add to end/front of queue/stack
--  object item = pop(integer qid*)         -- remove next from queue/stack
--  object item = top(integer qid*)         -- inspect head of queue/stack
--  pushn(integer qid, sequence items*)     --  push() multiple items
--  sequence items = popn(integer qid, n*)  --  pop() multiple items
--  sequence items = topn(integer qid, n*)  --  top() multiple items
--  integer res = queue_size(integer qid)   -- return the size of a queue
--  integer res = stack_size(integer qid)   -- return the size of a stack
--  bool res = queue_empty(integer qid)     -- yields true/false
--  bool res = stack_empty(integer qid)     -- yields true/false
--  destroy_queue(integer qid)              -- release a queue for reuse
--  destroy_stack(integer qid)              -- release a stack for reuse
--
--  The * indicates shared routines with a defaulted field which specifies whether 
--  to apply queue or stack behaviour, which you would not usually be expected to 
--  provide, but are not prohibited from so doing: FIFO_QUEUE and LIFO_QUEUE and
--  the default ANY_QUEUE (as all defined in psym.e) can be used when needed.
--  The other (non-starred) routines have no need or use for such a parameter, 
--  and in fact several stack_XXX() are simply aliased to queue_XXX() in psym.e,
--  obviously really only existing to allow/show clearer indication of intent.
--
--enum ANY_QUEUE = 0, FIFO_QUEUE = 1, LIFO_QUEUE = 2    -- as defined in psym.e/p2js.js

bool q_init = false
sequence q, qtypes
integer qfreelist = 0

--procedure init_q()
--  q = {}
--  qtypes = {}
--  q_init = true
--end procedure

global function new_queue(integer qtype=FIFO_QUEUE) -- create a new FIFO queue
    assert(qtype=FIFO_QUEUE or qtype=LIFO_QUEUE)
--  if not q_init then init_q() end if
    if not q_init then
        q = {}
        qtypes = {}
        q_init = true
    end if
    integer qid
    if qfreelist=0 then
        q = append(q,{})
        qtypes = append(qtypes,qtype)
        qid = length(q)
    else
        qid = qfreelist
        qfreelist = qtypes[qfreelist]
        q[qid] = {}
        qtypes[qid] = qtype
    end if
    return qid
end function

global function new_stack(integer qtype=LIFO_QUEUE) -- create a new FIFO queue
    return new_queue(qtype)
end function

global procedure destroy_queue(integer qid) -- [aliased as destroy_stack() in psym.e]
--  if not q_init then init_q() end if
    if not sequence(q[qid]) then ?9/0 end if
    qtypes[qid] = qfreelist
    qfreelist = qid
    q[qid] = 0
end procedure

global function queue_size(integer qid)     -- [aliased as stack_size() in psym.e]
--  if not q_init then init_q() end if
    return length(q[qid])
end function

global function queue_empty(integer qid)    -- [aliased as stack_empty() in psym.e]
--  if not q_init then init_q() end if
    return queue_size(qid)=0
end function

--[to be] aliased in psym.e *3:
--global procedure destroy_stack(integer qid) destroy_queue(qid) end procedure
--global function stack_size(integer qid) return queue_size(qid) end function
--global function stack_empty(integer qid) return queue_empty(qid) end function

global procedure push(integer qid, object item, integer qtype=ANY_QUEUE, bool bSingle=true)
--  if not q_init then init_q() end if
    assert(bSingle or sequence(item))
    sequence qq = q[qid]
    q[qid] = 0
    if qtype=FIFO_QUEUE
    or (qtype=ANY_QUEUE and qtypes[qid]=FIFO_QUEUE) then
        if bSingle then
            qq = append(qq,item)
        else
            qq &= item
        end if
    else
        if bSingle then
            qq = prepend(pp,item)
        else
            qq = deep_copy(item)&qq
        end if
    end if
    q[qid] = qq
end procedure

global procedure pushn(integer qid, sequence items, integer qtype=ANY_QUEUE)
    push(qid,items,qtype,false)
end procedure
 
global function pop(integer qid, n=-1, qtype=ANY_QUEUE, bool bPop=true)
--  if not q_init then init_q() end if  -- (would crash next either way)
    object result
    sequence qq = q[qid]
    bool bDC = bPop -- deep_copy(top)
    if not bPop then
        integer l = length(qq)
        if l=n or (n=-1 and l=1) then
            q[qid] = {}
            bPop = false
            bDC = false
        else
            q[qid] = 0 -- (kill refcount)
        end if
    end if
    if qtype=FIFO_QUEUE
    or (qtype=ANY_QUEUE and qtypes[qid]=FIFO_QUEUE) then
        if n=-1 then
            result = qq[1]
            if bPop then qq = qq[2..$] end if
        else
            result = qq[1..n]
            if bPop then qq = qq[n+1..$] end if
        end if
    else
        if n=-1 then
            result = qq[$]
            if bPop then qq = qq[1..$-1] end if
        else
            result = qq[-n..$]
            if bPop then qq = qq[1..-n-1] end if
        end if
    end if
    if bPop then
        q[qid] = qq
    elsif bDC then
        result = deep_copy(result)
    end if
    return result
end function

global function popn(integer qid, n, qtype=ANY_QUEUE)
    return pop(qid,n,qtype)
end function

global function top(integer qid, n=-1, qtype=ANY_QUEUE)
    return pop(qid,n,qtype,false)
end function

global function topn(integer qid, n, qtype=ANY_QUEUE)
    return pop(qid,n,qtype,false)
end function

