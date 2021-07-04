--
-- pHeap.e
-- =======
--
-- Practical note: You should never really need to read this. If you are happy (for now)
--  to say "it is a programming language: it /knows/ how to deal with strings, and it 
--  /knows/ how to deal with floating-point numbers", my advice is leave this well alone.
--  Mind you, the "Memory Use At The Lowest Level" stuff is probably worthwhile skimming,
--  but even I do not trouble myself to remember the precise details of all of it.
--
--DEV/SUG:
--  Running an strace on ./phix demo/pGUI/gears.exw creates a report with 140,000 getpid() in it.
--  That is almost certainly from pThread.e/pGetThread; it is probably worth making pStack.e save
--  it in vsb_root+nn (and duplicate it when a new vsb is created), at least temporarily, and in
--  fact no reason not to do the same for kernel32/GetCurrentThreadId calls, and then pGetThread
--  could be inlined/removed (taking the opportunity to alleviate any AGI stalls).
--  There are really only 4 places it is called (6 if you account for 32/64 bit), and while
--  you're at it, you could also (on Windows) try the mov eax,fs:[0x24] thing.
--
-- Technical note
-- ==============
--  Low-level errors in this code are almost always extremely serious and mapping them
--  to some hll source code line is not typically very helpful. For instance a heap
--  corruption is quite likely to be detected on some subsequent and entirely innocent
--  statement, and pointing the programmer at that will often hinder rather than help.
--  [update: as this matures, it seems that showing both might be increasingly useful]
--  These routines are written in a "fast fail" style.
--
--
-- The point of a heap manager is to scale effortlessly when it allocates /billions/ of
--  small blocks of memory, and allow them to be freed and re-used in any order, all in
--  a thread-safe manner, which is a decidedly non-trivial task. I am extremely proud of 
--  the code below, and believe it is very clean and efficient, considering the task it 
--  must do, but can hardly claim it is "simple". On that note, it is highly likely that 
--  the odd bug remains, although by its very nature it is the most extensively tested 
--  part of Phix. Any improvements are always welcome, of course.
--
--
-- Low-level memory access. Except for integers and a few constants, everything in Phix
--  gets stored in the heap, so an efficient heap manager is crucial to performance.
--  While newer versions of Windows (allegedly) come with a start-of-the-art allocator, 
--  previous verions were less than ideal, and Linux does not supply a heap manager at 
--  all. (Reference: http://x86asm.net/articles/memory-allocation-in-linux/index.html )
--  I looked at a few existing heap managers, but of course none are written in Phix, 
--  and besides, what works for (eg) C++ is not necessarily the best fit here. 
--  Further reading:
--   http://gee.cs.oswego.edu/dl/html/malloc.html (found after this was written)
-- As well as dealing with internal/implicit memory requirements, this also implements 
--  thread-safe and cross-platform allocate() and free(). The externally exposed asm 
--  entry points are: :%pAllocStr, :%pAllocSeq, :%pStoreFlt, :%pStoreMint, :%pLoadMint, 
--  :%pAlloc, :%pFree, :%pDealloc[0], :%pInitCS, :%pDeleteCS, :%pEnterCS, :%pLeaveCS,
--  plus :%pGetPool and :%pFreePool, but they should really only be used by pStack.e,
--  and pApnd.e (:%pFreePool only)
--  The trivial :%pGetpGtcb is also available should you need/want it.
--DEV
--  There may also need to be a :%pTerminateThread entry point for orphan handling.
--
-- TODO: * performance tests * [DEV]
--
-- Terminology
-- ===========
--  While this may yet make little sense, there are two types of memory block:
--  a) thread control blocks - used exclusively by these routines, one per thread.
--  b) superblocks - contain externally useable memory (eg "allocate(10)")
--  NB: All talk of thread safety in this program has nothing whatsoever to do with 
--  the thread safety of data /within/ the memory blocks it provides. In particular, 
--  increasing and decreasing reference counts on shared objects by multiple threads 
--  is an application-level issue that requires explicit application-level locking.
--  (eg see enter_cs() which is implemented via :%pEnterCS below)
--
--
-- Memory Use At The Lowest Level.
-- ==============================
--  This is actually utterly irrelevant to the heap manager, but it may be helpful to
--  properly understand exactly what (most) things it deals with are being used for.
--  Besides, a heap manager that does not yield "useful" sizes is no good to anyone.
--  Of course, memory obtained through allocate() can be used for whatever you want.
--
--  All memory blocks/allocations are at least dword-aligned, which means the address 
--  ends in 0b00 and right shifting two places does not cause any loss of information. 
--  Phix stores (31-bit) integers in the range -1073741824..1073741823, ie #C0000000..
--  #3FFFFFFF, ie leading bits 0b11 or 0b00, in a single dword. The special value h4,
--  or #40000000, means "unassigned", and anything above that (beginning 0b01) is a
--  2-bit-shifted raw address. For example, a hll variable containing #00000001 is 
--  just 1, whereas #40120021 is a ref pointing to memory at #00480084 - the shift 
--  effectively discards the leading #4/first two bits and puts back the 0b00 ending.
--  In practice we almost always scale (*4) rather than shift for performance reasons, 
--  but the net effect is exactly the same, including those first and last two bits.
--  Assuming something like "mov reg,[var]; cmp reg,h4; jl :isInt" has just occurred 
--  (the compiler may omit the load if it knows [var] is already in a reg, and the 
--  cmp/jmp if it is not integer, and is assigned, eg after "if not integer(var)"):
--
--  Floats are stored (on 32-bit) as
--      refcount @ dword[reg*4-8]
--      type #12 @ byte[reg*4-1]        plus 3 bytes used by delete_routine
--      f64      @ qword[reg*4]         as a 64-bit float (-1e308..+1e308)
--      total 16 bytes. On 64-bit two qwords and a tbyte(/f80) make it 26 bytes,
--                      which is rounded up to 28 to keep things dword-aligned.
--
--  Strings are stored as
--      maxlen   @ dword[reg*4-16]      actual allocated size in bytes
--      length   @ dword[reg*4-12]      in bytes/chars, not including null
--      refcount @ dword[reg*4-8]
--      type #82 @ byte[reg*4-1]        plus 3 bytes used by delete_routine
--      data     @ byte[reg*4]          ansi, UTF8, or even raw binary/WideString
--      char[i]  @ byte[reg*4+i-1]
--      null     @ byte[reg*4+length]   must always be present
--      header size is 16 bytes. 64-bit header is 32 bytes, but still 8-bit chars.
--
--  Sequences are stored as
--      slack    @ dword[reg*4-20]      bytes spare before header, initially 0
--      maxlen   @ dword[reg*4-16]      actual allocated size in bytes
--      length   @ dword[reg*4-12]      in dwords (qwords on 64-bit)
--      refcount @ dword[reg*4-8]
--      type #80 @ byte[reg*4-1]        plus 3 bytes used by delete_routine
--      elements @ dword[reg*4]         dword ints/refs
--      item[i]  @ dword[reg*4+i*4-4]
--      header size is 20 bytes. On 64-bit header is 40 bytes, with qword elements.
--
--  By making ref*4 locate the raw data, rather than the start of the header, we not only have
--  common/consistent offsets to data/type/refcount/length/maxlen, but also cause the special 
--  "unassigned" value of #40000000 (aka h4) to equate to a raw address of 0, which, quite 
--  apart from probably being used for something else or a "null pointer trap" by the OS, 
--  would make type/refcount etc negative/wrapped, ie it makes h4 patently/doubly invalid.
--  On 64-bit, type/refcount/length/maxlen/slack are [reg*4-1/16/24/32/40] respectively.
--
--  As noted above, a delete_routine index (see pDelete.e) may be stored in the remaining 3 
--  bytes of the type dword [at -4] (or 7 bytes of the type qword [at -8] on 64-bit).
--
--  As you can see, for floats the required size is always 16 bytes, 8 bytes of which are header.
--  Strings and sequences are allocated blocks of 32,64,128,etc, 16/20 bytes of which are header.
--  A zero-length string still needs a null terminator and thus does not fit in a 16 byte block.
--  Likewise, on 64-bit, you cannot fit a 32/40 byte header in 28 bytes, hence for both 32-bit
--  and 64-bit, chain[1]s only contain floats, and small allocate()ions, (which may become a bit 
--  clearer later on, and turns out to have little or no significance, except in the tables).
--  Update: There is a subtle implication for 64-bit here, that means qword-sequences are always 
--  qword-aligned (as long as the superblock was), ditto strings, which suits us just fine.
--
--  When strings/sequences are initially created, any spare space is at the end, and as long as
--  the refcount is 1 and we are doing a=append(a,b) style where b!=a, it may be done in situ.
--  Strings are always stored "hard left", with any spare space [maxlen-length-17] at the end.
--  Slice operations such as x=x[i..j] on dword-sequences with a refcount of 1 are permitted to 
--  move the header in situ, and set slack accordingly. A prepend operation (see VM/pApnd.e)
--  which finds slack of zero asks for a larger size and re-plants the header smack in the middle
--  of the newly allocated space, to cater for both subsequent append and prepend operations. If
--  prepend finds slack non-zero (and the refcount set to 1) it can move the 20-byte header left 
--  by 4 bytes (and reduce slack by 4), hence minimising the amount of data which must be moved. 
--  Append operations on dword-sequences must therefore check the length against (maxlen-slack), 
--  if there is no more space on the rhs it just creates a new "hard left" sequence.
--  Incidentally, any slack value not ending in 0b00 should immediately trigger an error.
--
--  Note that top-notch performance of prepend/slice on long strings has been deliberately 
--  sacrificed for the sake of significant speed gains elsewhere, whereas the performance of 
--  prepend/slice on dword-sequences is not compromised. By slice I specifically mean s=s[i..j]
--  when that could theoretically be done "in situ", rather than all forms of slices. For fast 
--  prepend/slice on strings you would need a "base" pointer in the string header to allow the 
--  text to start on an odd byte boundary, that is rather than move every byte just because you 
--  prepended one or more bytes to the start, but of course that would make all other operations 
--  on strings slower, and prevent passing the raw data to c_func etc. Some applications might 
--  therefore benefit from using dword-sequences instead of strings, by the simple technique of 
--  initialising to {} instead of "", or deliberately appending and then removing (say) a -1,
--  or, recommended if at all possible, reworked to use append in preference to prepend.
--
--  The maxlen is actually the total allocated space expressed in bytes, and is perhaps incorrectly 
--  named, equating to maximum length+17 for strings and maximum length*4+20 for dword-sequences. 
--  (Of course, actlen would have been a better name, but there is little point changing it now.)
--  On 64-bit those become maximum length+33 and maximum length*8+40 respectively.
--  For strings, (which must be dword-aligned) there is never any space at the start, for space at 
--  the end we must test whether (length+17)<maxlen (note the extra 1 for the null).
--  For sequences, checking for space at the start is simply whether slack is non-zero, at 
--  the end test whether (slack+(length*4)+20)<maxlen (again, slack is expressed in bytes).
--
--  Specifically note that when freeing a float, subtract 8, when freeing a string, subtract 16,
--  and when freeing a dword_sequence, subtract [slack]+20, that is, in order to obtain the proper 
--  "block start address" that a [ie this] heap manager has any chance of understanding. Of course
--  those numbers become 16/32/40 in 64-bit code.
--
--  The heap manager may also place additional fields before and after the above, for various 
--  housekeeping and/or corruption detection purposes. Note that the heap manager is heavily
--  biased towards faster performance, rather than absolute minimum memory consumption, not
--  that it chooses to be deliberately wasteful of course, but it is fair to say that using the
--  "doubling" method, described next, means that on average just shy of 25% of all allocated 
--  memory will technically not be in use, which (imho) is perfectly reasonable. By "on average"
--  I mean in the "mid-run" state, obviously at startup and should it go idle, expect high 90s.
--  Of course if anyone finds a better scheme I'd certainly love to hear about it.
--
--
-- Heap Management.
-- ===============
--  Smaller sizes (for now assume 16,32,64,128,256,etc, but we will revise that shortly) are 
--  allocated in per-thread "superblocks", ie we grab (say) an 8K chunk and split it up into 
--  packets of smaller sizes. According to some Intel paper I read, smaller sizes 16..512 
--  account for 98% of all usage, though that was for C++. We maintain simple chains of size 
--  links, 16,32,64,..2GB, and for now assume that means about 28 such lists (60 on 64bit),
--  per thread: not entirely trivial, but nevertheless perfectly manageable.
--  
--  Now, suppose we allocate an 8K "superblock" and logically attach it to sizes[10] (similar
--  processing may occur for larger sizes), then imagine we only want 32 bytes from it. We 
--  repeatedly split the block, detaching it from sizes[10] (if we ever really put it there)
--  and leave half on sizes[9] (4K), half the rest on sizes[8] (2K), and so on right down the
--  line until we're left with the 32 bytes we need. Now, if we want another 32 byte chunk,
--  that "other half" will be immediately available in sizes[2], and if we want another, that
--  is before either becomes free, then we will scan forward, find the sizes[3] (64 bytes) we
--  have left behind, and split that, half onto sizes[2] and the other half satisfying the
--  immediate requirement. Repeated requests for 32 byte chunks might continue this splitting
--  until we have used the whole 8K, at which point there are no more sizes[] to use and we
--  allocate another 8K "superblock" and repeat the process. One small point: whenever we 
--  split a block, we know that all the half-size-freelists down to the required size must be 
--  empty/null, which may (or may not) make things just a tiny little bit easier.
--
--  Releasing memory reverses this process. Simply put, we attempt to merge odd entries with 
--  the next, and even entries with the previous, pushing things back up sizes[] as far as 
--  they will go. "Entry Padding" below explains this in excruciating detail, together with a 
--  nice little diagram. Should we successfully merge an entire block, it is returned to the 
--  global pool (see below). In practice we allocate somewhat more than 8K (see below) to 
--  prevent padding (see below) from messing up the maths.
--
--  A thread must use a global lock to obtain a superblock, but can allocate smaller elements 
--  within any block it owns without locking. To avoid contention, each thread maintains two 
--  independent free lists, one for memory it owns, which it can update without any locking, 
--  and one for non-owned-memory, which must be locked, but with a thread-specific mechanism 
--  to minimise contention. It is of course other threads that put entries onto the non-owned
--  freelist(s), and the owning thread that later comes along and recycles them. No attempt 
--  is made to merge items before they are placed on the "non-owned" list, nor is the free bit
--  set on pRoot (explained below). A key aspect of this two-list scheme is that single threaded 
--  applications need perform virtually no locking as they acquire, release, and recycle memory, 
--  and of course even in a heavily multithreaded program, the vast majority of items are freed 
--  by the same thread that allocated them, and that can also be performed without locking.
--
--  One obvious example where memory allocated by one thread gets freed by another is the main 
--  gui creating and adding to (with appropriate locking) a "to-do" list, that a worker thread 
--  can (also lock and) process/remove entries from.
--
--  When the "owned" list is empty and the "non-owned" is not, we lock, zero the non-owned 
--  pointer, unlock, and then step down the list and merge things (ie everything that has 
--  been put on the "non-owned" freelist by other threads). Such locks (on the "non-owned"
--  lists) are extremely transient, covering only 3 or 4 fast instructions/clocks.
--  
--
-- Thread Termination.
-- ==================
--  By the time a thread reaches a natural conclusion, it may have amassed quite some list
--  of "owned" memory blocks. Some or all of that memory may remain in use, it just no longer
--  has a proper owning parent. Obviously, if the thread terminates abruptly, maybe we could
--  periodically check whether GetExitCodeThread() returns STILL_ACTIVE, but there would no
--  doubt be other issues (eg local variables/refcounts) that would prevent any recovery of
--  that memory short of application shutdown. Whatever, there should be a mechanism for a
--  thread to terminate gracefully, which includes saying to these routines "here, this is
--  all my stuff, you deal with it from now on.", or at least this should not preclude such.
--  NOTE: The first (few) version(s) are unlikely to have /any/ thread termination handling. [DEV]
--
--  In practice, we really want to leave "orphaned" memory exactly where it is, both in terms
--  of where the memory physically resides (obviously) and any and all pointers to it, since
--  any attempt to relink would significantly complicate matters, especially so for locking,
--  at least, that is, until every last byte ceases to be active. As mentioned above, when a
--  block is fully merged it is returned to the global pool, and that is just as valid for 
--  active threads as the orphaned memory previously owned by now-terminated threads. All we
--  need (or can) do is place a status code on the thread control block(s) as follows:
--
--      -1: normal (thread still active, let the owner deal with it)
--      -2: orphaned (thread terminated, lock/scan/merge whenever we need a new superblock)
--              [DEV there is still nothing that ever sets this value... (apart from and
--                   not counting the thing where we decide to leave it set to -2)]
--      -3: free (this thread control block should have all null pointers, and
--                can be reused whenever we need a new thread control block.)
--      -4: the global pool (-1 would probably be just fine, but why risk confusion)
--
--  Negative numbers were chosen simply because they stand out more in diagnostic hex dumps.
--  Normal/active thread control blocks can often end up with all null pointers, but in no 
--  way does that mean they are available for anyone else to nick.
--
--  Some lock contention may occur as things are shunted in and out of the global pool, but
--  it is almost all single-item and very transient.
--
--  There is one point where the memory allocator may appear to stall. Suppose that thread A 
--  allocates 10000 items and puts them on a "to-do" list, and thread B processes and removes
--  (say) 9996 of those entries (obviously with appropriate locking). When thread A allocates 
--  another item it may process all 9996 entries in one hit, which is fine, does not hinder 
--  thread B or any other thread, and besides is likely to be a tiny fraction of the time it
--  spent doing whatever it did to make the list that big in the first place. /However/, if 
--  thread A has terminated, a lock must remain in force for the entire duration of processing 
--  those 9996 entries, which /will/ stall anything attempting to free one of the remaining 4 
--  items, and likewise, for that matter, other threads attempting to obtain a new superblock.
-- DEV:
--  While :%pTerminateThread performs a full clear out/merglist all the "not-owned" lists, in       [DEV*: this has stopped making any sense to me!!]
--  minimal locking fashion, thread A may terminate shortly after creating those 10000 items.     [or did I just mean it's not pTerminateThread's job?]
--  It is of course the single (locked) call :scan_orphans below, of which I speak.
--  (Translation: There is only one fairly rare case where the heap manager stalls any other
--                thread for anything beyond the briefest of fleeting moments.)
--
--
--  OK, so that's the theory, let's put together a practical implementation.
--
-- Superblock Header
-- =================
--  DWORD   dwMagic     -- [+0] checked for memory corruption ("SBH\0" = #00484253)
--  DWORD   dwThreadId  -- [+4] owner
--  DWORD   nsbSize     -- [+8] does not include this 20 bytes (should always be >= 10K)
--  DWORD   pTCB        -- [+12] ptr/4 to the owning Thread Control Block (see below)
--  DWORD   pNxtsb      -- [+16] next superblock
--  ==
--  20 bytes (#14)  (see below for 64-bit version)
--
-- dwThreadId is normally set from kernel32/xGetCurrentThreadId() or sys_getpid()
-- When superblocks are added to the global pool, dwThreadId is set to 0, and pTCB gets replaced.
-- Originally, pNxtsb was to be per-owner for orphaned block handling, but is now just a single chain 
--  linked from pGtcb, and really only of any use for diagnostic heap dumps (external to this code).
-- Strictly, we only really need nsbSize and pTCB here, but that would of course be harder to debug.
--
-- Entry Padding
-- =============
--  DWORD   era         -- [-8] effective return address, for diagnostics/leak checking
--  DWORD   pRoot       -- [-4] pointer to the SuperBlock Header[+1]
--                      -- 0b01 (aka bit 0): block is free
--                      -- if xor(left,right) not 0|1 then memory corruption has occurred.
--  (DWORD  nSize)      -- [+0] size (permanent for allocate()d blocks)
--  (DWORD  pNext)      -- [+4] (on free items only), next freelist link
--  (DWORD  pPrev)      -- [+8] (on owned free items only), previous freelist link
--
--  The pointer to the superblock header, together with the block size, determines whether for
--  merging purposes this is a "leftie" or "rightie". Consider the following possible memory 
--  splittings, in which the right side might be more broken up than the left, or vice versa:
--  (Obviously this represents a tiny chunk of a scheme that scales effortlessly to petabyte
--   sized blocks potentially split into trillions of float-sized fragments, and covering up
--   to 28-ish splitable/mergeable/halveable/doubleable sizes on 32-bit, 60-ish on 64-bit.)
--
--                              s   160 |       L       |
--                              i    80 |   L   |   R   |
--                              z    40 | L | R | L | R |
--                              e    20 |L|R|L|R|L|R|L|R|
--                               offset 0   40  80  120 160
--
--  A block at offset 80 is a "leftie" for sizes 20 and 40, and must look to the right to see
--  if the corresponding block is both free and the same size, whereas a block of size 80 at
--  that address is a "rightie" and must look left to see, again, if the corresponding block 
--  is both free and the same size. Clearly, if (offset/size) is odd, look left, else right. 
--  (ie/eg line 3: 0/40=0(->R), 40/40=1(->L), 80/40=2(->R), 120/40=3(->L), ...)
--
--  Just to be very clear here, consider a block of size 40 (3rd line) at offset 80: we can say 
--  with /absolute certainty/ that on the right is either another block of size 40 or two blocks
--  of size 20, but there will definitely be a valid padding at offset 120. However, should we 
--  try to look left (which we wouldn't want to do if we are trying to put things back together 
--  the same way we took them apart) then at offset 40 we might find the same, /or/ that it is 
--  slap bang in the middle of an 80, and hence might be anything, rather than a pRoot[+1].
--  Whereas a block of size 80 (2nd line) at offset 80 looking left will definitely find a pRoot
--  at offset 0, although of course it may still be in use and/or of size 20, 40, or 80.
--
--  Fairly obviously, superblock headers always lie on a dword boundary, hence we can use the 
--  low-order bit to signify free areas. Note that bit is NOT set when items are placed on the
--  "not-owned" freelists. When merging two blocks, an xor of their pRoots should be 0 or 1,
--  otherwise memory corruption has occurred. The size of a float/string/sequence can be deduced 
--  from the Phix data it contains, whereas allocate()d blocks need an explicit/permanent size.
--  nSize (except as just noted), pNext, and pPrev overlap the live data, and the doubly-linked 
--  freelist allows us to quickly snip an entry out of the sizes[n] freelist as is is merged,
--  before carrying on forwards with similar processing for sizes[n+1]. In practice we only need
--  set pPrev on the owned freelists, it isn't used on the non-owned freelists (which are picked 
--  up and merged wholesale, by the owner). Also, of course, pPrev should be 0 on the first item 
--  in the (owned) list and likewise pNext should be 0 on the last.
--
--  Note: pNext and pPrev can freely point from one superblock to another and back, and a walk
--        down a free chain (which I do not do anywhere) could quite probably re-visit several
--        superblocks several times. I expect all dwThreadId at [pRoot[-1]+4] on any one chain 
--        should match, including all fully merged blocks in the global pool, however (as yet) 
--        this code makes no attempt whatsoever to verify that. [DEV: MCHK/MLEAK/FALLOC]
--
--DEV may be slightly out of date...
--  Minor point: In practice, the nSize value on allocate()'d blocks is 4 less than it is on
--               freed blocks, which is actually (still) 4 less than the useable size, eg:
--pHeapD(??)
--<              in the table below, [1] says 20[#00000014]=4+16; so the block size (20) is
--<              the value stored on the freelists, pGetPool returns edx=16 which is stored 
--<              by pAlloc(<=12) just after pRoot (as nSize, and in exactly the same place  
--<              where pFreePool stores the value 20), before 12 bytes of useable space. 
--<              Obviously on 64-bit those numbers are more like 8<8<useable eg 36/28/20.
--               in the table below, [1] says 24[#00000018]=8+16; so the block size (24) is
--               the value stored on the freelists, pGetPool returns edx=20 which is stored 
--               by pAlloc(<=12) just after pRoot (as nSize, and in exactly the same place  
--               where pFreePool stores the value 24), before 12 bytes of useable space. 
--               Obviously on 64-bit those numbers are more like 8?<8<useable eg 36/28/20.  [DEV]
--               None of this causes any particular problems, other than mild confusion when 
--               debugging this code. Obviously I didn't consciously design it like that, but 
--               that is the way it works, and that is the way it is going to stay, not that
--               two matching tweaks to pAlloc/pFree ought be anything but trivial. [DEV oh, just sort it, once regression tests all up and running...]
--
--
-- Thread Control Blocks
-- =====================
--  DWORD       dwMagicT    -- [+0] checked for memory corruption ("TCB\0" = #00424354)
--  DWORD       dwThreadId  -- [+4] owner
--  DWORD       pNxtcb      -- [+8] next thread control block
--  DWORD       pFirst      -- [+12] first superblock
--  DWORD       nStatus     -- [+16] -1: normal, -2: orphaned, -3: free, -4: global pool
--  DWORD[27]   pFree       -- [+20] chains of owned free blocks, by size (not locked)
--  DWORD[27]   pNofl       -- [+128] ""       not-     ""                (need locks)
--  DWORD       dwMagicC    -- [+236] checked for memory corruption ("CCB\0" = #00424343)
--  BYTE[24]    csLock      -- [+240] critical section for locking (a dword futex on Lnx)
--< DWORD       dwMagicE    -- [+264] checked for memory corruption ("ECB\0" = #00424345)
--> DWORD       SaveEBP     -- [+264]
--> DWORD       pTLS        -- [+268]
--  DWORD       dwMagicE    -- [+272] checked for memory corruption ("ECB\0" = #00424345)
--  ===
--< 268 bytes (27*2+7)*4+24  (see below for 64-bit version)
--  276 bytes (27*2+9)*4+24  (see below for 64-bit version)
--
--  A dwThreadId of 0 indicates the global pool (kept in pGtcb), which needs extra locking, has
--  an nStatus of -4, and the special properties that a) all entries in each pFree[i] chain point 
--  to <superblockheader+24> with a pRoot+0b01 and an nSize that corresponds exactly to [i]; there 
--  is never any need to do any merging within the global pool, and b) all of pNofl[] should forever 
--  be nulls. There should never be any reference [in pGtcb] to any memory that is still in use 
--  (except perhaps when there is a programming error such as an application invoking free(addr), 
--  but then continuing to use addr - you could test for bad pointers with and_bits(peek(addr-8),1), 
--  not that passing proves it is good, as it may have already been reused for something else).
--  [for a 64-bit application that test would, of course, be and_bits(peek(addr-16),1) instead, and
--   obviously any and all such tests would not port at all well to either OpenEuphoria or RDS Eu.]
--
--  As pGtcb is a hll variable and will always be dword-aligned it is stored /4 to ensure it is always 
--  an integer, and for consistency the pTCB values (on superblock headers) and pNxtcb are likewise /4, 
--  however pFirst etc are exact. [Note 64bit changes many *4 to *8, but pTCB handling remains /4|*4.]
--  While you must lock pGtcb->csLock to insert a new tcb, there should be no need for any locking by
--  other currently active threads that are just scanning the pGtcb->pNxtcb chain (there is no reason 
--  for them to be looking for the newest dwThreadId's tcb, or get upset when they cannot find it).
--  (In practice that just means updating pGtcb.pNxtcb last, after new.pNxtcb has got the old value.)
--  (Technically speaking, the creation of pGtcb itself is not thread safe, and we cannot change that,
--   since we haven't got anything we can lock until after we have created it, but it is highly unlikely 
--   that any application could get as far as CreateThread without somehow managing to initialise it.)
--
--  pFirst was originally intended to be per dwThreadId, but now everything is linked to pGtcb and it
--  should be null on all other records, and is only of any use for (external) heap dump diagnostics.
--  DEV: when/if we get Phix dlls, pFirst should probably be used to hold pGtcb...
--  dwMagicC/E can be used to check we located csLock correctly, all pointers should end with 0b00.
--  Obviously, on all superblock headers dwThreadId should match pTCB->dwThreadId (which also means
--  setting pSBH.dwThreadId to zero when transferring it to the global pool [&resetting pSBH.pTCB]).
--  
--  Possible improvements: Currently the pGtcb.pNxtcb chain is scanned to locate a dwThreadId, it may 
--                          improve matters to keep the current thread's tcb in thread local storage, 
--                          or use some other faster lookup.
--                         At startup, it hammers through ~17*2 pFree/pNofl on the thread tcb, and
--                          repeats that trick on the global pool. It might help (tracing, anyway)
--                          to have nMax holding the highest pFree/pNofl that can possibly contain
--                          anything useful. On 64-bit it's ~49*4 such checks that will all fail.
--                          Or: Init pFree with -1's, and when we allocate something, replace any
--                          lower pFree[i]s of -1 with 0s. If a scan stops on a pFree[] of -1, we
--                          know the rest of the table is empty and the same for pNofl (which is 
--                          still initialised to 0s). I'm thinking of a "js :ohnoitsallempty".
--                          To be fair, though, even saving 400 million clocks on a real-world
--                          application is unlikely to be particularly noticeable, these days.
--                         It might also be possible to detect the "startup" case, allocate an
--                          initial superblock and then obtain a tcb from within that. That could 
--                          however mean that we need to merge within the global pool, which I
--                          think would be a bad idea.
--                         We could also mark (in use) allocated blocks by setting bit #02 in pRoot,
--                          so they can be skipped by any memory leak checking code (none such yet).
--                          Obviously that means after xor(pRoots) we cmp 3 not cmp 1, before ja,
--                          and of course we need to mask out the extra bit. (ditto for pStack)
--                          [Update: I am now thinking that constant lf=allocate(sizeof(LOGFONT))
--                           w/o free should be treated as a memory leak that should be fixed.]
--                         Deferred coalescing. I have noticed this performs quite poorly when 
--                          saving 300,000-line/30MB files in Edita. This currently implements 
--                          very aggressive coalescing, even to the point of returning fully 
--                          merged blocks to the global pool, as soon as anything is freed. 
--                          One approach might be to extend pFree/pNofl to pFree/pDefc/pNofl: 
--                          at ::gpchecknextand we would check all 3, nicking a single entry
--                          off pDefc exactly as we do pFree, but on the first iteration only,
--                          ie when pDefc[idx] is 0, we also hammer back down pDefc[idx-1..1]
--                          performing the deferred coalescing, ideally stopping the moment we 
--                          have merged a block of the original required size*. It may also be 
--                          possible to utilise the spare bit on pRoot (#02) to indicate blocks 
--                          which have not been coalesced, instead of a separate pDefc list.
--                          It would clearly be quite critical that #02 blocks are kept grouped 
--                          together at the head of pFree and we can stop scanning the list (on
--                          that first iteration thing) as soon as we hit any non-#02 entry. By
--                          scanning backwards and stopping asap we /should/ avoid trying to 
--                          push any non-#02 entries ahead of #02 entries, if that makes sense..
--                          Use the pHeap.e/pHeapD.e approach (see psym/ptok). Thread termination
--                          and the like may require a temporary reversion to aggressive mode.
--
-- *some further thoughts on deferred coalescing, regarding the #02 grouping just mentioned:
--  imagine, on a clean heap, we have:
--      sequence sinTable = repeat(0,N), 
--               cosTable = repeat(0,N)
--      for i=1 to N do
--          sinTable[i] = sin(theta)
--          cosTable[i] = cos(theta)
--          theta += something
--      end for
--  and then we:
--      sinTable = {}
--  The result? pFree[1] has N odd floats that cannot be coalesced, since cosTable is still
--  using the pairing N even floats. N can quite easily be very large (30,000 or more). We
--  must ensure we do not needlessly re-traverse that 30,000 long chain (once is enough), by
--  not adhering to the rule that all #02 entries must be at the start of the list. If a 
--  pFree[2] is 0 so we deferred_coalesce(1), it must carry on down the list until it hits
--  a non-#02, rather than quit the moment it populates a #02. On the other hand, a call
--  to deferred_coalesce(1) can (and should) push #02 entries onto pFree(2), and we go back
--  up pFree[2..idx-1] doing the same. We can quit between any deferred_coalesce(N) calls,
--  but not during the middle of one. Note that #02 does not mean it /can/ be coalesced,
--  but that we haven't even looked. I am now leaning towards the pDefc list rather than 
--  the #02 bit, because of all of this.
--
-- Memory Leak Checking And Heap Diagnostics
-- =========================================
--  Every entry on the free list has a logical partner that we can examine. While that block 
--  might be further split internally, it should be enough to report on the leftmost chunk.
--
--  This approach does not locate anything in completely full superblocks, but is deemed to be 
--  good enough for our purposes - if that ever proves otherwise it should prove relatively 
--  simple to skip down the full list of superblocks (start with pGtcb.pFirst then follow the
--  pNxtsb chain) and examine offset 0 for any block with a non-0 dwThreadId that we failed to 
--  visit via the freelists. As long as we report at least one of the possibly many items in 
--  error, the assumption is made that, after the programmer has fixed that particular memory 
--  leak or corruption, the next run will show up anything else that also needs to be reported.
--
--  It also strikes me that we will need to process all pNofl at the start of all of this:
--   things like mergelist and scan_orphans may want to be made part of the optable. [DEV?]
--
--  In order to make any sense of the contents of the heap, we need to store an era (effective
--  return address), as shown above in "Entry Padding", that can later be translated into a 
--  source file/line. Note that if we find an "orphaned" block of memory there is little point
--  trying to guess whether it is/was a float/string/sequence/raw memory, especially if we are
--  suspicious that any kind of memory corruption may have happened. What we do know is where
--  the padding/era are/should be, and the block size (from whatever free list entry pairs with 
--  it), and a hex dump of the first 40 bytes (size of sequence header) or so should be enough.
--
--  Also, any such checks are only performed when interpreting, and take significant advantage 
--  of having two (or more) heaps: the initial one that compiles the source code and later on
--  runs the code that analyses the second, and a second one, only used while running the app.
--  Obviously that means we can code the potentially difficult analysis in normal hll code, and
--  not have to worry that day-to-day work vars are messing up the very thing we are looking at.
--  If we are not going to do the analysis, clearly we can stick with using just the one heap.
--  Maybe at some point the required code could be repackaged into a single "check_memory()" 
--  call that can be invoked from the very last line of an application (and when interpreted 
--  it would effectively all get done twice), so that we can (optionally) have some form of
--  this checking in a compiled program, but that is left as an exercise for the reader. [DEV did I say that better somewhere else? opposite/create a new stack to run the analysis in...]
--
--  Because existing libraries such as win32lib and arwen contain non-released memory allocations
--  such as constant lf = allocate(SIZEOF_LOGFONT), and things like call_back() allocate memory 
--  that is never released, the default is to skip all memory obtained via allocate(), which is     [BLUFF]
--  achieved by leaving the era set to 0 (or perhaps some other magic number). Alternatively we
--  could have allocate() set era but have a secondary routine, say not_freed(a), which clears 
--  the era to exclude it from subsequent checking. Checking that all allocate() calls in an 
--  application have an appropriate matching free() is for now something that may or may not be 
--  returned to at a later date. Right now this is concentrating on the quiet stuff that goes on 
--  behind the scenes pretty much all the time.
--
--  The interpret code (in p.exw/main) needs something like:    [DONE]
--
--      #ilASM{
--              ...
--              call :%pNewGtcbChain        -- (if analysing)
--              mov [ntcb4],eax             -- temp save
--              <existing call :%pNewStack still rqd>
--              <existing call(symtab[T_maintls][S_il])>
--              <existing restore ebp/esp/symtabptr>
--              mov eax,[ntcb4]
--              call :%pRestoreGtcbChain    -- (if analysing)
--              mov [ntcb4],edx             -- analyse later, in hll (and free it)
--              ...
--            }
--      check_heap(ntcb4)                   -- (if analysing)
--      <carry on with profile, -test, etc>
--
--copied from above: [DEV/dead]
--  add next/prev to pAlloc/pFree. Need to save/restore/0 pAhead with pGtcb.
--  rework pStack to use :%pAlloc/pFree, then :%pGetPool/FreePool->::pGetPool/FreePool,
--  and remove them from the optable. (AH: but :%pFreePool is used by pApnd.e)
--  code to pFree() everything on pAhead, in pCleanupAll. Create/test say FALLOC=0/1.
--  then code to scan for memory leaks, also in pCleanupAll. Proper reporting may want/
--  need to be done in pdiag; we might want to limit the number of items listed, and
--  common up with existing code therein the conversion of era-1 to file/line, plus
--  hammering down symtab collecting [S_il] addresses and sorting them into order is
--  definitely a job for hll rather than asm. Or we could scan symtab looking for the
--  largest [S_il] less than era, starting with symtab[T_maintls]. Or we could mark 
--  all alloc with a special tag/magic, and simply skip them (optionally) when checking
--  for memory leaks... Or just have an era of 0 in pAlloc.
--  DOH: let's just do memory leak checking when interpreted (only) and save the things
--  from the "opInterp", before restoring the ones used by p.exw; then we can analyse
--  things in hll code at our leisure!!!    <<--***
--</dead>
--
--
--  Note: if profiling then analysing should be turned off, otherwise there could be floats
--  as stored by opLnp[t] etc in the wrong heap/being reported as memory leaks, likewise for
--  tracing.
--
-- Revised Sizes.
-- =============
--  The initial plan of 16/32/64/128 etc byte sizes falls apart once we have entry padding. I wrote a
--  quick ditty (reproduced below) which created the following suggested sizes (for 32-bit):
--
--  idx      superblock size            block size  details             string           sequence
--  [1]                               24[#00000018]=4+4+16              n/a              n/a
--  [2]                               48[#00000030]=4+4+40              x(23)            s[5]
--  [3]                               96[#00000060]=4+4+88              x(71)            s[17]
--  [4]                              192[#000000C0]=4+4+184             x(167)           s[41]
--  [5]                              384[#00000180]=4+4+376             x(359)           s[89]
--  [6]                              768[#00000300]=4+4+760             x(743)           s[185]
--  [7]                             1536[#00000600]=4+4+1528            x(1,511)         s[377]
--  [8]                             3072[#00000C00]=4+4+3064            x(3,047)         s[761]
--  [9]                             6144[#00001800]=4+4+6136            x(6,119)         s[1,529]
-- [10]      12308[#00003014]      12288[#00003000]=4+4+12280 *         x(12,263)        s[3,065]
-- [11]      24596[#00006014]      24576[#00006000]=4+4+24568           x(24,551)        s[6,137]
-- [12]      49172[#0000C014]      49152[#0000C000]=4+4+49144           x(49,127)        s[12,281]
-- [13]      98324[#00018014]      98304[#00018000]=4+4+98296           x(98,279)        s[24,569]
-- [14]     196628[#00030014]     196608[#00030000]=4+4+196600          x(196,583)       s[49,145]
-- [15]     393236[#00060014]     393216[#00060000]=4+4+393208          x(393,191)       s[98,297]
-- [16]     786452[#000C0014]     786432[#000C0000]=4+4+786424          x(786,407)       s[196,601]
-- [17]    1572884[#00180014]    1572864[#00180000]=4+4+1572856         x(1,572,839)     s[393,209]
-- [18]    3145748[#00300014]    3145728[#00300000]=4+4+3145720         x(3,145,703)     s[786,425]
-- [19]    6291476[#00600014]    6291456[#00600000]=4+4+6291448         x(6,291,431)     s[1,572,857]
-- [20]   12582932[#00C00014]   12582912[#00C00000]=4+4+12582904        x(12,582,887)    s[3,145,721]
-- [21]   25165844[#01800014]   25165824[#01800000]=4+4+25165816        x(25,165,799)    s[6,291,449]
-- [22]   50331668[#03000014]   50331648[#03000000]=4+4+50331640        x(50,331,623)    s[12,582,905]
-- [23]  100663316[#06000014]  100663296[#06000000]=4+4+100663288       x(100,663,271)   s[25,165,817]
-- [24]  201326612[#0C000014]  201326592[#0C000000]=4+4+201326584       x(201,326,567)   s[50,331,641]
-- [25]  402653204[#18000014]  402653184[#18000000]=4+4+402653176       x(402,653,159)   s[100,663,289]
-- [26]  805306388[#30000014]  805306368[#30000000]=4+4+805306360       x(805,306,343)   s[201,326,585]
-- [27] 1610612756[#60000014] 1610612736[#60000000]=4+4+1610612728      x(1,610,612,711) s[402,653,177]
--([28] 3221225492[#C0000014] 3221225472[#C0000000]=4+4+3221225464      x(3,221,225,447) s[805,306,361]) [not possible, see below]
--                                      [#C0000000=3.00GB]
-- (The * on item 10 marks the virtual stack block size as used in pStack.e)
--
--  idx indicates the freelist entry (pFree/pNofl) [although this code uses 0,4,8...104(=26*4)]
--  superblock size includes the header and what we request from the OS (minimum 12K+20 rather than 8K)
--  The block size (critically) is the value which doubles in the above table, or, if you prefer to think 
--  the other way round, it is the value that we can halve and therefore split into two smaller blocks.
--  The details column shows padding (pRoot/era) and useable space, and by implication how to "round up".
--  The string and sequence columns show the maximum sizes the block can contain.
--
--  In the above scheme, a standard superblock can contain 512 floats, and if we assume the majority of 
--  strings are less than 71 characters long and the majority of sequences are less than 17 elements long 
--  (which is more likely to be so for table[i] than table) then a standard superblock can contain >= 128 
--  of them. All of which suggests that, on average, locking should be required less than 1% of the time.
--
--  The last entry ([28]) is shown in brackets because it would always fail, since kernel32/HeapAlloc has a 
--  hard limit of #7FFFFFFF. In practice (no doubt real-world apps would manage quite a bit less) it failed 
--  at #734CA1F8, equivalent to x(1,934,401,999) and s[483,600,499], as opposed to the values in [27] above. 
--  In adopting this scheme we are accepting an artificial limit of some 83% (75% of the theoretical limit), 
--  not that even a half-competent programmer should be thinking that hogging all possible available memory 
--  is the best way to achieve anything. Plus it kinda guarantees ~308MB should usually be available to cover 
--  diagnostics and/or any potential differences in available memory between compiled & interpreted apps,
--  at least should it fail when asked for something too big, as opposed to too many of something smaller.
--  Quick experiments on RDS Eu 2.4 and OpenEuphoria 4.1 (32-bit) gave (sequence) limits of s[483,164,865] 
--  and s[468,365,949] respectively, clearly (20%) better than the Phix dword-sequence limit, but a paltry 
--  30% of the Phix string (/byte/file size) limit.
--  
--  On a practical note, repeating the (trivial) size calculation loop on each and every allocation request
--  is actually faster than any dirty bsr-style skullduggery. (Ignore that if you don't know what it means.)
--  Testing will no doubt be limited to "if the smaller sizes all work, we can stop". Lastly, remember that 
--  explicit requests (ie allocate()) need an extra size field, so the limits for that are 12,36,84,180,372, 
--  but (thankfully) a few judiciously placed +/-12/24 allow the exact same code to be used for both).
--
--  As promised, I shall now formally declare the following limitations:
--
--      32-bit Phix does not support memory allocations over 1,610,612,728 bytes and in turn strings longer
--      than 1,610,612,711 characters or dword-sequences longer than 402,653,177. (The manual states these
--      limits as 400 million characters or 100 million elements, triple-able if allocated up front.)
--      64-bit Phix has (or more accurately is expected to have) such ludicrously high limits they are hardly 
--      worth mentioning. Oh go on then, around 5.5EB, by which I mean 5 million TB, which in 2014 is over a 
--      /billion/ quids worth of RAM (based on a 256GB SSD for 75UKP), and presumably a leccy bill that would 
--      put the LHC to shame. Another potential practical issue is the sheer weight of all that RAM (plus 
--      substrate) might tear a hole in the earth's crust, unless spread out a bit. I recently (May 2014) 
--      read that x86-64 is limited to 256TB, and Windows 7, according to wikipedia, is limited to 192GB. 
--
-- The 64-bit version of the above table is:
--  idx      superblock size            block size  details     string          sequence
--  [1]                               44[#0000002C]=8+8+26+2    n/a             n/a
--  [2]                               88[#00000058]=8+8+72      x(39)           s[4]
--  [3]                              176[#000000B0]=8+8+160     x(127)          s[15]
--  [4]                              352[#00000160]=8+8+336     x(303)          s[37]
--  [5]                              704[#000002C0]=8+8+688     x(655)          s[81]
--  [6]                             1408[#00000580]=8+8+1392    x(1,359)        s[169]
--  [7]                             2816[#00000B00]=8+8+2800    x(2,767)        s[345]
--  [8]                             5632[#00001600]=8+8+5616    x(5,583)        s[697]
--  [9]      11296[#00002C20]      11264[#00002C00]=8+8+11248 * x(11,215)       s[1,401]
-- [10]      22560[#00005820]      22528[#00005800]=8+8+22512   x(22,479)       s[2,809]
-- [11]      45088[#0000B020]      45056[#0000B000]=8+8+45040   x(45,007)       s[5,625]
-- [12]      90144[#00016020]      90112[#00016000]=8+8+90096   x(90,063)       s[11,257]
-- [13]     180256[#0002C020]     180224[#0002C000]=8+8+180208  x(180,175)      s[22,521]
-- [14]     360480[#00058020]     360448[#00058000]=8+8+360432  x(360,399)      s[45,049]
-- [15]     720928[#000B0020]     720896[#000B0000]=8+8+720880  x(720,847)      s[90,105]
-- [16]    1441824[#00160020]    1441792[#00160000]=8+8+1441776 x(1,441,743)    s[180,217]
-- [17]    2883616[#002C0020]    2883584[#002C0000]=8+8+2883568 x(2,883,535)    s[360,441]
-- [18]    5767200[#00580020]    5767168[#00580000]=8+8+5767152 x(5,767,119)    s[720,889]
--  <snip>
-- [58] 6341068275337658400[#5800000000000020] 6341068275337658368[#5800000000000000]=16+6341068275337658352  x(6,341,068,275,337,658,319)  s[792,633,534,417,207,289]
--([59]12682136550675316768[#B000000000000020]12682136550675316736[#B000000000000000]=16+12682136550675316720 x(12,682,136,550,675,316,687) s[1,585,267,068,834,414,585])
--                                                                [#B000000000000000=11.00EB]
--
-- The above assumes floats are 16 byte header and a tbyte, so 26 bytes, rounded up to 28 to keep dword-alignment,
--  strings have a 32 byte header but are still ansi/UTF8, sequences have a 40 byte header and each element is 8 
--  bytes, and the superblock header becomes 32 bytes and padding (pRoot) doubles to 16 bytes, as detailed below.
--  The allocate() limits become 20,64,152,328,680,1384, etc, similar to 32-bit, that is +16,*2 instead of *2,+16.
--  The last 10 or so entries are probably beyond any practical use, but there is little gain to removing them.
--
--
-- Just in case anyone wants to play around with these numbers a bit more, here are the quick ditties I used:
--
--  -- 32-bit:
--  atom d = 16
--  atom w
--  integer i = 2
--  while d<2*1024*1024*1024 do
--      w = (d+8)*2
--      d = w-8
--      printf(1,"-- [%d] %10d[#%08x] %10d[#%08x]=4+%d\t\tx(%,d)\ts[%,d]\n",{i,w+20,w+20,w,w,d,d-17,floor((d-20)/4)})
--      i += 1
--  end while
--  printf(1,"[#%08x=%3.2fGB]\n",{w,w/(1024*1024*1024)})
--  if getc(0) then end if
--  abort(0)
--
--  -- 64-bit:
--  -- (note that if this is run on 32-bit, the 53-bit precision of floats loses the trailing #20 [line 54+]
--  --  on the lhs, and the rhs can be out by approx 2K. Lines 58..59 shown above are from a 64-bit run.)
--  atom d = 28
--  atom w
--  integer i = 2
--  while d<8*1024*1024*1024*1024*1024*1024 do
--      w = (d+16)*2
--      d = w-16
--      printf(1,"-- [%d] %10d[#%08x] %10d[#%08x]=8+%d\tx(%,d)\ts[%,d]\n",{i,w+32,w+32,w,w,d,d-33,floor((d-40)/8)})
--      i += 1
--  end while
--  printf(1,"[#%08x=%3.2fEB]\n",{w,w/(1024*1024*1024*1024*1024*1024)})
--  if getc(0) then end if
--  abort(0)
--
-- Superblock Header (64-bit)
-- ==========================
--  DWORD   dwMagic     -- [+0] checked for memory corruption ("SBH\0" = #00484253)
--  DWORD   dwThreadId  -- [+4] owner
--  QWORD   nsbSize     -- [+8] does not include this 32 bytes (should always be >= 8K)
--  QWORD   pTCB        -- [+16] ptr/4 to the owning Thread Control Block (see below)
--  QWORD   pNxtsb      -- [+24] next superblock
--  ==
--  32 bytes (#20) - note that magic and threadId are (quite deliberately) still dwords.
--
-- Entry Padding (64-bit)
-- ======================
--  QWORD   era         -- [-16] effective return address, for diagnostics/leak checking
--  QWORD   pRoot       -- [-8] pointer to the SuperBlock Header[+1]
--                      -- 0b01 (aka bit 0): block is free
--                      -- if xor(left,right) not 0|1 then memory corruption has occurred.
--  (QWORD  nSize)      -- [+0] size (permanent for allocate()d blocks)
--  (QWORD  pNext)      -- [+8] (on free items only), next freelist link
--  (QWORD  pPrev)      -- [+16] (on owned free items only), previous freelist link
--
-- Thread Control Block (64-bit)
-- =============================
--  DWORD       dwMagicT    -- [+0] checked for memory corruption ("TCB\0" = #00424354)
--  DWORD       dwThreadId  -- [+4] owner
--  QWORD       pNxtcb      -- [+8] next tcb
--  QWORD       pFirst      -- [+16] first superblock
--  QWORD       nStatus     -- [+24] -1: normal, -2: orphaned, -3: free, -4: global pool
--  QWORD[58]   pFree       -- [+32] chains of owned free blocks, by size (not locked)
--  QWORD[58]   pNofl       -- [+496]  ""    not-      ""                 (need locks)
--  DWORD       dwMagicB    -- [+960] checked for memory corruption ("BCB\0" = #00424342)
--  DWORD       dwMagicC    -- [+964] checked for memory corruption ("CCB\0" = #00424343)
--  BYTE[40]    csLock      -- [+968] critical section for locking (a dword futex on Lnx)
--< DWORD       dwMagicE    -- [+1008] checked for memory corruption ("ECB\0" = #00424345)
--> QWORD       SaveEBP     -- [+1008]
--> QWORD       pTLS        -- [+1016]
--  DWORD       dwMagicE    -- [+1024] checked for memory corruption ("ECB\0" = #00424345)
--  ===
--< 1012 bytes - note that magic and threadId are (quite deliberately) still dwords.
--  1028 bytes - note that magic and threadId are (quite deliberately) still dwords.
--  dwMagicC can be used to check we located csLock correctly, all pointers should end 0b00
--
--
--DEV this probably belongs in the manual (ilASM), referenced here:
-- Technical note
-- ==============
--  This code contains both 32 and 64 bit versions, as well as PE and ELF differences.
--  In particular I want to point out that any split into equivalent chunks for 32/64 
--  or PE/ELF is quite arbitrary: pilasm.e declares labels etc using exactly the same
--  conditions (emitON) that control binary being emitted. When you see the general 
--  pattern [32] [64] [] label [32] [64] [] label [32] [64] [] label, you should not 
--  assume anything other than that I have used labels as a convenient breakpoint. 
--  So, for example, [32] ::mergeone [64] ::mergeone [] should not cause any problem.
--  Obviously [32] ::somelabel [64] call :somelabel [] causes the predictable error,
--  which can be somewhat confusing, so declaring all labels under [] is recommended.
--  While there is nothing wrong with [32] 800 lines [64] 800 lines [], it is probably 
--  a fair bit easier to maintain when the equivalent blocks both fit on the screen.
--  Of course it largely depends on what you are doing: when debugging (or simply just 
--  reading and understanding) it will probably help to have larger chunks, whereas 
--  smaller chunks make applying equivalent changes to 32 and 64 bit code far easier.
--  Obviously the splits you see were for translation, which is now done and dusted.
--  Feel free to merge/uninterleave or split/interleave chunks as it takes your fancy,
--  or "factor out" common code such as EnterCriticalSection and/or GetCurrentThreadId. [DONE!]
--  Likewise I have tended to use the ordering [PE32]/[ELF32]/[PE64]/[ELF64] but there
--  is no reason that (for instance) [PE32]/[PE64]/[ELF32]/[ELF64] would be wrong.
--  Obviously the code is more than twice as long as it would be if it supported just
--  a single target architecture, not much that can realistically be done about that.
--
--
-- Techical note: this all goes horribly wrong if e/rbx is not 0 on entry, hence to 
--  avoid grief I zero it at all known entry points. Feel free to add a few more.
--

-- OLD STUFF:
-- Inconsequential ramblings that I couldn't quite bring myself to delete
-- ======================================================================
--  Obviously, feel free to skip this section.
--
--  Technically speaking hll variables/values beginning with 0b10 are invalid. 
--  Undefined behaviour occurs if the backend is asked to process such garbage,
--  ie in most cases it would probably be treated as a negative integer, but
--  other times it could bit-shift it and more than likely crash immediately.
--  Of course data (f64/char[]/raw memory) can contain any value, including 
--  dwords that happen to begin with 0b10, it is hll variables, and likewise
--  dword-sequence elements, which cannot. Note however the [new] compiler uses
--  0b10 for pre-relocated references, which get shr 2+0b001'd leaving a 0b01 
--  leadin. Said shift and add can be/is done at compilation-time for normal 
--  executables, but must be done after loading for DLLs [if ever finished].)
--
--DOH: I can/should do ALOT of testing before trying to make this live!!!
--SUG:safe_poke(base,offset,value): [base-8] must not have 0b01, [[base-8]] must be "SBH\0",
-- offset must be >=0 and <(=?)[base-4]-sizeinbytes(value).
--
--  All memory allocations are rounded up to a sensible size for subsequent splitting/reuse.
--  It makes little sense to bother the OS with sizes less than 8K, instead we grab ourselves
--  a "superblock" and farm out little bits of it ourselves. What we need are blocks that can
--  be split (in half) and recombined simply and efficiently. For now, assume powers of 2 (16,
--  32,64,128,256,...2GB) but expect a bit of tweaking in a moment.
--
--  This is deliberately fragile, with plenty of checks that trigger (/crash) when the slightest
--  thing goes wrong, on the basis that if it has not crashed by the time we have self-hosted
--  Phix and run Edita, then it probably never will.
--
--without debug

--include builtins\VM\pFPU.e        -- (:%down53/64, :%near53/64 in pAlloc/Free)
--include builtins\VM\pFEH.e
--DEV
include builtins\VM\pDeleteN.e

-- (suprisingly, these two =0 are all the initialisation we need!)

integer pGtcb = 0       -- the global control block (dwThreadId=0, stored /4)

integer stdcs = 0       -- for very short one-off inits in \builtins (opEnter/LeaveCS).

--integer gt1tcb = 0

constant M_MMAP_THRESHOLD = -3
integer  mmap_threshold = 128*1024  -- linux only, see notes below

--DEV/temp:
constant memory_corruption = "memory corruption at #"
constant pGtcb4eq = ", pGtcb*4=#"
constant diffis = ", diff="

--constant pssebp = "pSetSaveEBP: #"
--constant psslmh = " <- #"
--constant psssob = " ("
--constant pssccr = ")\n"

--/*
integer withjs = 3  -- for with[out] js/javascript[_semantics]:
                    --  0b01: without js
                    --  0b10: with js
                    --  0b11: default/without js/reset
                    -- 1 <==> 2 is error, but 3 <==> 1 | 2 | 3 is fine.
                    --  (uses test[aka bitwise and]!=0 for validity)
                    --  (:%opAlloClone crashes if bit 01 is not set)
--*/
integer withjs = 0  -- with(1) / without(0) js/javascript[_semantics]
                    -- treat 2 (meaning any) as 0 (aka without)

--DEV:
#ilASM{ jmp :!opCallOnceYeNot
--#ilASM{ jmp :fin

--/*
procedure ::pGetMem(::)
end procedure
--*/
    ::pGetMem
-------------
        -- on entry, eax(/rax) is the required length, including any headers
        -- caller is responsible for any required locking
        -- on exit, eax(/rax) is the allocated memory (may want a /4)
        -- can return 0 (which should be fatal for everything except allocate())
        [PE32]
            -- standard (Windows API) calling convention applies: 
            -- eax/ecx/edx are damaged, as are st0..7
            -- ebx/ebp/esi/edi are preserved
            -- result in eax, eax:edx, or st0
            push eax                                -- dwBytes (for HeapAlloc)
            call "kernel32.dll","GetProcessHeap"
--DEV try HEAP_NO_SERIALIZE (=#00000001), when running performance tests, but I somehow doubt it will matter
--      (specifically test calling allocate(8000) until it fails)
            push ebx                                -- dwFlags (0)
            push eax                                -- hHeap
            call "kernel32.dll","HeapAlloc"
        [PE64]
            -- standard (Windows API) calling convention applies: 
            -- first 4 parameters are passed in rcx/rdx/r8/r9 (or xmm0..3),
            -- caller (ie this code) must allocate >=32 bytes of shadow space.
            -- logically: p1/rcx at [rsp], p2/rdx at [rsp+8], p3/r8 at [rsp+16],
            -- p4/r9 at [rsp+24], then: p5 in [rsp+32], p6 in [rsp+40], etc.
            -- rax/rcx/rdx/r8/r9/r10/r11 are damaged, as are xmm0..5 and st0..7
            -- rbx/rbp/rdi/rsi/r12/r13/r14/r15 are preserved (as are xmm6..15)
            -- result in rax or xmm0
            --
            -- Auto-aligning the stack to a 16-byte boundary
            -- =============================================
            --      mov reg,rsp         -- (if you have one to spare, obviously, else a
            --      push rsp            --  push rsp; push qword[rsp] pair will do, but
            --      push reg            --  that way causes an AGI stall, on the [rsp].)
            --      -- (there are now two copies of the original rsp on the stack, doh)
            --      or rsp,8            -- [rsp] is now 1st or 2nd copy:
            --                          -- if on entry rsp was xxx8: both copies remain on the stack
            --                          -- if on entry rsp was xxx0: or rsp,8 effectively pops one of them
            --                          -- obviously rsp is now xxx8, whatever alignment we started with
            --      sub rsp,8*<odd>     -- (realign the stack as you normally would, including shadow space)
            --      ...
            --      <##code##>
            --      ...
            --  --  add rsp,8*<odd>
            --  --  pop rsp             -- restore, equivalent to rsp += (either #08 or #10)
            --      mov rsp,[rsp+8*odd] -- equivalent to the add/pop
            --
            mov rcx,rsp -- put 2 copies of rsp onto the stack...
            push rsp
            push rcx
            or rsp,8    -- [rsp] is now 1st or 2nd copy:
                        -- if on entry rsp was xxx8: both copies remain on the stack
                        -- if on entry rsp was xxx0: or rsp,8 effectively pops one of them (+8)
                        -- obviously rsp is now xxx8, whatever alignment we started with
            sub rsp,8*5                             -- minimum 4 param shadow space, and align/somewhere to save rax
            mov [rsp+32],rax                        -- save rax (required length)
            call "kernel32.dll","GetProcessHeap"
            mov r8,[rsp+32]                         -- dwBytes (p3)
            mov rdx,rbx                             -- dwFlags (p2,0)
            mov rcx,rax                             -- hHeap (p1)
            call "kernel32.dll","HeapAlloc"
--          add rsp,8*5
--          pop rsp
            mov rsp,[rsp+8*5]   -- equivalent to the add/pop
        [ELF32]
            -- standard (kernel) calling convention applies: 
            -- syscall (int 0x80) number in eax (see docs\LinuxSyscallReference.txt), eg:
            --  1 sys_exit
            --  3 sys_read
            --  4 sys_write (stdout is 1)
            --  5 sys_open
            --  6 sys_close
            -- 45 sys_brk
            --140 sys_llseek
            -- arguments in ebx,ecx,edx,esi,edi[,ebp?] (so assume *all* regs damaged)
            -- (may need brandelf -t Linux filename on freeBSD, unless we can figure out how to do that as part of "p -c")
            -- *NB*: ELF32 often requires "xor ebx,ebx" after an int 0x80, whereas PE32/PE64/ELF64 preserve ebx/rbx
            -- In the case of C library calls (eg call "libc.so.6","printf"), eax/ecx/edx are damaged,
            --  but ebx/esi/edi/ebp are preserved. No floating point registers are preserved.
--DEV this lot needs replacing: calls to libiup.so routines completely obliterate memory allocated as below,
--      apparently sys_brk is "old hat" and "mmap with anon mapping" would be better.
--      See http://man7.org/linux/man-pages/man2/mmap.2.html 
--      Also, builtins\syswait.ew has examples of [ELF32/64] inline assembler calling libc funcs directly (call "libc.so.6","fork"),
--      and builtins\pcfunc.e has a few from libdl (call "libdl.so.2", "dlopen").
--      [plain old malloc is also an option, but I read it (sometimes) uses mmap internally anyway, and it does not have the protection
--       flags that might prove useful, then again it does have mcheck/mprobe... It has also occurred to me that the problem might be 
--       that libiup.so uses malloc/mmap and whichever it uses assumes it does all the sys_brks, so that may force our hand.]

--/*
            push eax            -- save size
            -- call sys_brk(0) to find the current location of the program break
            xor ebx,ebx
            mov eax,45          -- sys_brk(0)
            int 0x80
            cmp eax,-4096
            ja :error_brk
            -- call sys_brk(prev+size) to set the new program break
            mov ebx,eax
            add ebx,[esp]       -- size
            push eax
            mov eax,45          -- sys_brk(ebx=long brk)
            int 0x80
            xor ebx,ebx         -- (NB: common requirement after an int 0x80)
            pop ecx             -- prev
            cmp eax,-4096
            jbe @f
          ::error_brk
                xor ebx,ebx     -- (covers the case when that ja triggers)
                xor ecx,ecx
          @@:
            -- return previous program break (or 0)
            mov eax,ecx
            add esp,4           -- discard size
--*/

--void *mmap(void *addr(NULL), size_t length, int prot, int flags, int fd, off_t offset);
-- PROT_EXEC=4 | PROT_READ=1 | PROT_WRITE=2; 7=RWX
--  MAP_SHARED              =       01h             ; Share changes
--  MAP_PRIVATE             =       02h             ; Changes are private
--  MAP_ANONYMOUS           =       20h             ; don't use a file
-- the following broke ./phix -test...
--/*
            push 0                  -- offset (ignored)
            push -1                 -- fd (ignored)
            push #22                -- MAP_ANONYMOUS | MAP_PRIVATE
            push 7                  -- RWX
            push eax                -- size
            push ebx                -- addr (NULL)
            call "libc.so.6","mmap"
            add esp,24
--*/
--void * malloc (size_t size)
            --2/5/17: (not strictly necessary on 32-bit)
            -- On Linux, by default, memory allocations below 128K are serviced from the heap 
            -- and above from mmap. However pemit2 needs optable+data_section+code_section to
            -- all be within a 32-bit offset of each other, which we achieve by forcing every
            -- allocation to be serviced from the heap. Note that since Phix never actually
            -- invokes libc/free, there is no advantage to using mmap anyway (as yet).
            push eax
            cmp eax,[mmap_threshold]
            jle @f
                mov [mmap_threshold],eax
                push eax
                push M_MMAP_THRESHOLD
                call "libc.so.6","mallopt"      -- mallopt(int param, int value);
                add esp,8
          @@:
--          push eax -- (done above)
            call "libc.so.6","malloc"
            add esp,4

        [ELF64]
--pop al    -- calling convention is going to be hopelessly wrong in almost all cases!
            -- standard (kernel) calling convention applies: 
            -- syscall number in rax (see docs\lsct64.txt)
            -- first 6 parameters are passed in rdi/rsi/rdx/rcx(or r10 for system calls)/r8/r9 (or xmm0..7).
            -- rax/rcx/rdx/rsi/rdi/r8/r9/r10/r11 are damaged, as are xmm0..15 and st0..7
            -- rbx/rbp/r12/r13/r14/r15 are preserved
            -- In the case of C library calls (eg call "libc.so.6","printf"), rax/rcx/rdx/rsi/rdi/r8..r11 
            --  are damaged, but rbx/rbp/r12..r15 are preserved. No floating point registers are preserved.
--/*
            push rax                -- save length
--          mov r14,rax             -- save length
            -- call sys_brk(0) to find the current location of the program break
            xor rdi,rdi
            mov rax,12              -- sys_brk(rdi=unsigned long brk)
            syscall
            -- call sys_brk(prev+size) to set the new program break
--          mov rdi,rax
--          add rdi,r14
            pop rdi
            add rdi,rax
            mov rax,12              -- sys_brk(rdi=unsigned long brk)
            syscall
--          pop r14
--  Description                     RAX     RBX     RCX     RDX     RSI     RDI     RBP     RSP     R8      R9      R10     R11     R12     R13     R14     R15     RIP/RA
--  Dwarf2 number                   0       3       2       1       4       5       6       7       8       9       10      11      12      13      14      15      16
--  Preserved across function calls         Yes                                     Yes     Yes                                     Yes     Yes     Yes     Yes     N/A
--  Function parameter                              4th     3rd     2nd     1st                     5th     6th                                                     N/A
--  Return register                 1st                     2nd                                                                                                     N/A
--  Kernel parameter                #NR                     3rd     2nd     1st                     5th     6th     4th                                             N/A
--*/
--/!*
--void *mmap(void *addr, size_t length, int prot, int flags, int fd, off_t offset);
push rdi
push rsi
            mov r9,0                -- offset (ignored)
            mov r8,-1               -- fd (ignored)
--          mov rcx,#22             -- MAP_ANONYMOUS | MAP_PRIVATE
            mov rcx,#62             -- MAP_ANONYMOUS | MAP_PRIVATE | MAP_32BIT
            mov rdx,7               -- RWX
            mov rsi,rax             -- size
            mov rdi,rbx             -- addr (NULL)
            call "libc.so.6","mmap"
--          add rsp,48
pop rsi
pop rdi
--*!/
--void * malloc (size_t size)
--untried:
-- 7/2/17:
--/*
push rdi
push rsi
            --2/5/17:
            -- On Linux, by default, memory allocations below 128K are serviced from the heap 
            -- and above from mmap. However pemit2 needs optable+data_section+code_section to
            -- all be within a 32-bit offset of each other, which we achieve by forcing every
            -- allocation to be serviced from the heap. Note that since Phix never actually
            -- invokes libc/free, there is no advantage to using mmap anyway (as yet).
            push rax
            cmp rax,[mmap_threshold]
            jle @f
                mov [mmap_threshold],rax
                mov rdi,M_MMAP_THRESHOLD
                mov rsi,rax
                call "libc.so.6","mallopt"      -- mallopt(int param, int value);
          @@:
            pop rdi
--          mov rdi,rax
            call "libc.so.6","malloc"
pop rsi
pop rdi
--*/
        []
            ret

--/*
procedure ::pGetThread(::)
end procedure -- (for Edita/CtrlQ)
--*/
    ::pGetThread
----------------
        -- nb most registers are trashed, as per calling conventions outlined above
        --   (but feel free to save/restore them here, if it helps any)
        --  result in eax/rax
        [PE32]
--DEV for the crack, when you've bored, instead try ([also try debugging] then try PE64, then replace everywhere it's called)
--          mov eax,fs:[0x24]
            call "kernel32.dll","GetCurrentThreadId"
        [ELF32]
--DEV libc/getpid()
            mov eax,20              -- sys_getpid()
            int 0x80
            xor ebx,ebx             -- (common requirement after int 0x80)
        [PE64]
            mov rax,rsp -- put 2 copies of rsp onto the stack...
            push rsp
            push rax
            or rsp,8    -- [rsp] is now 1st or 2nd copy:
                        -- if on entry rsp was xxx8: both copies remain on the stack
                        -- if on entry rsp was xxx0: or rsp,8 effectively pops one of them (+8)
                        -- obviously rsp is now xxx8, whatever alignment we started with
            sub rsp,8*5
            call "kernel32.dll","GetCurrentThreadId"
--          add rsp,8*5
--          pop rsp
            mov rsp,[rsp+8*5]   -- equivalent to the add/pop
        [ELF64]
-- 7/2/17:
push rdi
push rsi
push rcx
            mov eax,39              -- sys_getpid()
            syscall
pop rcx
pop rsi
pop rdi
        []
            ret
    
--/*
procedure ::pGetTCB(::)
end procedure -- (for Edita/CtrlQ)
--*/
    :%pGetTCB
--------------
        -- local routine for pSetSaveEBP; could also be used by pGetPool?
        -- (esi:=pTCB/4; most regs trashed)
        -- (also, eax/rax contains the thread-id on exit)
    [32]
        -- <no parameters>
        -- on exit, pTCB/4 in esi
        xor ebx,ebx -- (save some grief)
        call :pGetThread    -- (eax:=dwThreadId; most regs trashed)
        mov esi,[pGtcb]
        test esi,esi
    [64]
        -- <no parameters>
        -- on exit, pTCB/4 in rsi
        xor rbx,rbx     -- (save some grief)
        call :pGetThread    -- (rax:=dwThreadId; most regs trashed)
        mov rsi,[pGtcb]
        test rsi,rsi
    []
        jnz :gettcbloop
            int3
      ::gettcbloop
    [32]
        mov esi,dword[ebx+esi*4+8]          -- tcb:=tcb.pNxtcb
        test esi,esi
        jz :tcbnotfound
--      jnz @f
--SUG return 0...
--          int3
--          ret
--    @@:
        cmp dword[ebx+esi*4],#00424354      -- dwMagicT ("TCB\0")
        je @f
            int3
      @@:
        cmp dword[ebx+esi*4+4],eax          -- dwThreadId
    [64]
        mov rsi,qword[rbx+rsi*4+8]          -- tcb:=tcb.pNxtcb
        test rsi,rsi
        jz :tcbnotfound
--      jnz @f
----            int3
--          ret
--    @@:
        cmp dword[rbx+rsi*4],#00424354      -- dwMagicT ("TCB\0")
        je @f
            int3
      @@:
        cmp dword[rbx+rsi*4+4],eax          -- dwThreadId
    []
        jne :gettcbloop

      ::tcbnotfound
        ret

-- Callback handling
-- =================
--  This is one of those dirt-simple routines that requires an awful lot of explaining...
--  Phix relies on ebp(/rbp) pointing to the current frame on the virtual stack (which is
--  allocated in ~8K blocks on the heap). If we invoke call/c_func/proc, and that invokes
--  a callback, ebp will most likely be meaningless (to Phix). Hence the former trio must
--  save ebp which the latter can restore, and all four must restore everything back to
--  the way they found it, as soon as that becomes appropriate. The single routine below,
--  :%pSetSaveEBP, locates pTCB.SaveEBP, saves edx/rdx therein, and returns the previous 
--  content in eax/rax. Note that arwen\pComN.ew also uses this, however I rather suspect
--  it may be superfluous as the callback handler should cope, but all this has changed
--  beyond recognition since the days when pComN.ew was written... [DEV]
--  A callback is an asynchrochronous event to Phix: when it invokes some external C/asm 
--  code, it has no idea if, when, or in what state things will be in, when control gets 
--  to the callback handler, and it needs some way to restore a bit of sanity. Prior to 
--  multithreading, this used a local static variable in pcfunc.e. The local integer(s)
--  prev_ebp4, in call()/c_func()/c_proc(), use the trick of storing a dword-aligned 
--  value /4 in an integer, and that takes care of any nesting, to any depth. 
--  Logically, I suppose, pSetSaveEBP belongs in pStack.e - but making pGetTCB global is 
--  not really justifiable, especially not just for this trivial little thing.          --DEV done anyway...
--
--  call/c_func/proc should: 
--                              mov edx,ebp
--                              call :%pSetSaveEBP  -- (eax<-pTCB.SaveEBP<-edx, all regs trashed)
--                              shr eax,2
--                              mov [local_ebp4],eax
--  and when control returns:
--                              mov edx,[local_ebp4]        -- see note[1] below
--                              shl edx,2
--                              call :%pSetSaveEBP  -- (eax<-pTCB.SaveEBP<-edx, all regs trashed)
--  callback_handler should:
--                              push ebp                    -- see note[2] below
--                              xor edx,edx                 -- edx:=0
--                              call :%pSetSaveEBP  -- (eax<-pTCB.SaveEBP<-edx, all regs trashed)
--                              test eax,eax                
--                              jz @f
--                                  mov ebp,eax             -- see note[3] below
--                            @@:
--                              push eax
--  and finally:
--                              pop edx
--                              call :%pSetSaveEBP  -- (eax<-pTCB.SaveEBP<-edx, all regs trashed)
--                              pop ebp                     -- see note[2] below
--  whew!
--  note[1]: ebp must be correct for this to work. If you are worried that a c_func/proc
--           could return with a damaged ebp, then just push/pop it, and don't confuse 
--           that completely separate issue with anything that this is trying to do!
--
--  note[2]: The callback handler also has the further responsibility of saving/restoring 
--           ebp for the benefit of the C code, which has nothing to do with SetSaveEBP.
--
--  note[3]: callback_handler zeroes pTCB.SaveEBP: should it be invoked "twice in a row", 
--           without an intervening call/c_func/proc, or directly from #ilASM before any
--           such, the test eax,eax ensures it leaves ebp as-is.
--

--/*
procedure :%pSetSaveEBP(:%)
end procedure
--*/
    :%pSetSaveEBP
--------------
        -- save ebp before c_func (etc), in case of call_back, in a thread safe manner
        -- (eax<-pTCB.SaveEBP<-edx, all regs trashed)
--push esi
        -- (factored out as a prelude to using it elsewhere:)
        call :%pGetTCB          -- (esi:=pTCB/4; most regs trashed)
--pop edi
    [32]
        mov eax,[ebx+esi*4+264]     -- SaveEBP
        mov [ebx+esi*4+264],edx     -- SaveEBP
--DEV temp code:
--pushad
---- pSetSaveEBP: #00000000 <- #00000000 (%d)
--mov edi,[pssebp]
--call :%puts1
--mov edx,[esp+28]  -- eax
--push ebx
--call :%puthex32
--mov edi,[psslmh]
--call :%puts1
--mov edx,[esp+20]  -- edx
--push ebx
--call :%puthex32
--mov edi,[psssob]
--call :%puts1
----mov eax,[esp+4]     -- esi
--mov eax,[esp]     -- edi
--push ebx
--call :%putsint
--mov edi,[pssccr]
--call :%puts1
--popad
    [64]
        mov rax,[rbx+rsi*4+1008]    -- SaveEBP
        mov [rbx+rsi*4+1008],rdx    -- SaveEBP
    []
        ret
        
--/*
procedure :%pGetpGtcb(:%)
end procedure -- (for Edita/CtrlQ)  [I quickly tired of keying CtrlQ/<stare blankly>/AltS]
--*/
    :%pGetpGtcb         -- (for external heap diagnostics, if ever needed/written)
---------------
    [32]
        mov eax,[pGtcb]
    [64]
        mov rax,[pGtcb]
    []
        ret

--/*
procedure :%pNewGtcbChain(:%)
end procedure -- (for Edita/CtrlQ)
--*/
    :%pNewGtcbChain
--------------
        -- eax/rax:=existing [pGtcb], if any, and [pGtcb]:=0 forces the test/newtcb 
        --          at the start of pGetPool to trigger the next time it is called.
        --  see "Memory Leak Checking And Heap Diagnostics" above
    [32]
        mov eax,[pGtcb]
        mov [pGtcb],ebx
    [64]
        mov rax,[pGtcb]
        mov [pGtcb],rdx
    []
        ret

--/*
procedure :%pRestoreGtcbChain(:%)
end procedure -- (for Edita/CtrlQ)
--*/
    :%pRestoreGtcbChain
------------------
        -- restore [pGtcb] from the value in eax/rax, typically saved from the one 
        --  p.exe just used to compile the source, and return the current value in 
        --  edx/rdx, typically one that was specially created to interpret the app
        --  in, deliberately distinct from the p.exe one so that we can analyse it.
        --  see "Memory Leak Checking And Heap Diagnostics" above
    [32]
        mov edx,[pGtcb]
        mov [pGtcb],eax
    [64]
        mov rdx,[pGtcb]
        mov [pGtcb],rax
    []
        ret

--/*
procedure ::EnterCriticalSection(::)
end procedure -- (for Edita/CtrlQ)
--*/
    ::EnterCriticalSection
--------------------------
        -- note this is internal, see also :%opEnterCS
        [PE32]
            -- lpCriticalSection in ecx
            push ecx
            call "kernel32.dll","EnterCriticalSection"
            ret
        [PE64]
            -- lpCriticalSection in rcx
            mov rax,rsp -- put 2 copies of rsp onto the stack...
            push rsp
            push rax
            or rsp,8    -- [rsp] is now 1st or 2nd copy:
                        -- if on entry rsp was xxx8: both copies remain on the stack
                        -- if on entry rsp was xxx0: or rsp,8 effectively pops one of them (+8)
                        -- obviously rsp is now xxx8, whatever alignment we started with
            sub rsp,8*5
            call "kernel32.dll","EnterCriticalSection"
--          add rsp,8*5
--          pop rsp
            mov rsp,[rsp+8*5]   -- equivalent to the add/pop
            ret

--      [ELF]
-- (it may be easier/better to just call "libpthread.so.0","pthread_mutex_lock")
--          --/*
--            -- credit: "futexes are tricky" by Ulrich Drepper (futex3 code)
--            int val = 0;
--            void lock() {
--              int c;
--              if ((c = cmpxchg(val, 0, 1)) != 0) {
--                if (c!=2) {
--                  c = xchg(val, 2);
--                }
--                while (c!=0) {
--                  futex_wait(&val, 2);
--                  c = xchg(val, 2);
--                }
--              }
--            }
--            void unlock() {
--              if (atomic_dec(val) != 1) {
--                val = 0;
--                futex_wake(&val, 1);
--              }
--            }
--            My quick explanation (go and find a copy of futex.pdf for more detail):
--              val has three states:  0 = unlocked, 1 = locked no waiters, 2 = locked with waiters.
--              lock: if we can 0->1 then we're done (fast route, uncontended lock)
--                    else loop: set val to 2; if it was 0 we're done else wait/loop.
--              unlock: set val (which must have been 1 or 2) to 0; if it was 2 wake one waiter up.
--          --*/

        [ELF32]
            -- csLock (address of a dword futex) in ecx
            -- all registers trashed
            mov edx,1
            xor eax,eax
            lock cmpxchg [ecx],edx  -- atomic(<tmp>:=eax; eax:=[ecx]; if <tmp>==eax then [ecx]:=edx end if)
            test eax,eax            -- so if eax is now/still 0, then [ecx] just got set (from 0) to 1.
            jz @f
                push ecx
                cmp eax,2
                je :futex_wait      -- (else [rcx] must have been 1 on entry)
              ::xchg2
                mov eax,2
                lock xchg [ecx],eax
                test eax,eax
                jz :futex_locked    -- if [ecx] just got set from 0 to 2, we're done (else (1|2)->2==>wait)
              ::futex_wait
                    mov eax,240         -- sys_futex(ebx=void *futex, ecx=int op, edx=int val, esi=const struct timespec *timeout[==NULL][, edi=void *futex2, ebp=int val3])
                    mov ebx,ecx         -- *futex
                    mov ecx,#80         -- FUTEX_WAIT(0) or FUTEX_PRIVATE_FLAG(128)
                    mov edx,2           -- val
                    xor esi,esi         -- no timeout
                    int 0x80            -- futex_wait(ecx,2)
                    xor ebx,ebx         -- (common requirement after int 0x80)
                    mov ecx,[esp]
                    jmp :xchg2
              ::futex_locked
                pop ecx
          @@:
            ret

        [ELF64]
            -- csLock (address of a dword futex) in rcx (yes, a futex is still a dword on 64-bit)
            mov edx,1
            xor eax,eax
            lock cmpxchg [rcx],edx  -- atomic(<tmp>:=eax; eax:=[rcx]; if <tmp>==eax then [rcx]:=edx end if)
            test eax,eax            -- so if eax is now/still 0, then [rcx] just got set (from 0) to 1.
            jz @f
                push rcx
                cmp eax,2
                je :futex_wait      -- (else [rcx] must have been 1 on entry)
              ::xchg2
                mov eax,2
                lock xchg [rcx],eax
                test eax,eax
                jz :futex_locked    -- if [rcx] just got set from 0 to 2, we're done (else (1|2)->2==>wait)
              ::futex_wait
-- 7/2/17:
push rdi
push rsi
                    mov eax,202         -- sys_futex(rdi=u32*uaddr, rsi=int op, rdx=u32 val, r10=struct timespec*utime[==NULL] [,r8=u32*uaddr2,r9=u32 val3])
                    mov rdi,rcx         -- *futex
                    mov rsi,#80         -- FUTEX_WAIT(0) or FUTEX_PRIVATE_FLAG(128)
                    mov edx,2           -- val
                    xor r10,r10         -- no timeout
                    syscall             -- futex_wait(rcx,2)
pop rsi
pop rdi
                    mov rcx,[rsp]
                    jmp :xchg2
              ::futex_locked
                pop rcx
          @@:
            ret
        []

--/*
procedure ::TryEnterCriticalSection(::)
end procedure -- (for Edita/CtrlQ)
--*/
    ::TryEnterCriticalSection
-----------------------------
        -- note this is internal, see also :%opTryCS
        [PE32]
            -- lpCriticalSection in ecx
            push ecx
            call "kernel32.dll","TryEnterCriticalSection"
            -- eax==0 on failure [DEV may want a test/mov 1 here]
            ret
        [PE64]
            -- lpCriticalSection in rcx
            mov rax,rsp -- put 2 copies of rsp onto the stack...
            push rsp
            push rax
            or rsp,8    -- [rsp] is now 1st or 2nd copy:
                        -- if on entry rsp was xxx8: both copies remain on the stack
                        -- if on entry rsp was xxx0: or rsp,8 effectively pops one of them (+8)
                        -- obviously rsp is now xxx8, whatever alignment we started with
            sub rsp,8*5
            call "kernel32.dll","TryEnterCriticalSection"
--          add rsp,8*5
--          pop rsp
            mov rsp,[rsp+8*5]   -- equivalent to the add/pop
            -- rax==0 on failure [DEV may want a test/mov 1 here]
            ret

        [ELF32]
            -- csLock (address of a dword futex) in ecx
            -- all registers trashed
            mov edx,1
            xor eax,eax
            lock cmpxchg [ecx],edx  -- atomic(<tmp>:=eax; eax:=[ecx]; if <tmp>==eax then [ecx]:=edx end if)
            test eax,eax            -- so if eax is now/still 0, then [ecx] just got set (from 0) to 1.
            jz @f
                xor eax,eax         -- failure
                ret
          @@:
            mov eax,1
            ret

        [ELF64]
            -- csLock (address of a dword futex) in rcx (yes, a futex is still a dword on 64-bit)
            mov edx,1
            xor rax,rax
            lock cmpxchg [rcx],edx  -- atomic(<tmp>:=eax; eax:=[rcx]; if <tmp>==eax then [rcx]:=edx end if)
            test eax,eax            -- so if eax is now/still 0, then [rcx] just got set (from 0) to 1.
            jz @f
                xor rax,rax         -- failure
                ret
          @@:
            mov rax,1
            ret
        []

--/*
procedure ::LeaveCriticalSection(::)
end procedure -- (for Edita/CtrlQ)
--*/
    ::LeaveCriticalSection
--------------------------
        -- note this is internal, see also :%opLeaveCS
        [PE32]
            -- lpCriticalSection in ecx
            push ecx
            call "kernel32.dll","LeaveCriticalSection"
            ret

        [PE64]
            -- lpCriticalSection in rcx
            mov rax,rsp -- put 2 copies of rsp onto the stack...
            push rsp
            push rax
            or rsp,8    -- [rsp] is now 1st or 2nd copy:
                        -- if on entry rsp was xxx8: both copies remain on the stack
                        -- if on entry rsp was xxx0: or rsp,8 effectively pops one of them (+8)
                        -- obviously rsp is now xxx8, whatever alignment we started with
            sub rsp,8*5
            call "kernel32.dll","LeaveCriticalSection"
--          add rsp,8*5
--          pop rsp
            mov rsp,[rsp+8*5]   -- equivalent to the add/pop
            ret

        [ELF32]
            -- csLock (address of a dword futex) in ecx
            lock sub dword[ecx],1   -- *futex -= 1
            jz @f
                mov [ecx],ebx       -- *futex := 0
                mov eax,240         -- sys_futex(ebx=u32*uaddr, ecx=int op, edx=u32 val [,esi=struct timespec*utime,edi=u32*uaddr2,ebp=u32 val3])
                mov ebx,ecx         -- *futex
                mov ecx,#81         -- FUTEX_WAKE(1) or FUTEX_PRIVATE_FLAG(128)
                mov edx,1           -- val
                int 0x80            -- futex_wake(ecx,1)
                xor ebx,ebx         -- (common requirement after int 0x80)
          @@:
            ret

        [ELF64]
            -- csLock (address of a dword futex) in rcx
            lock sub dword[rcx],1   -- *futex -= 1  (yes, it is still a dword on 64-bit)
            jz @f
-- 7/2/17:
push rdi
push rsi
                mov [rcx],rbx       -- *futex := 0
                mov eax,202         -- sys_futex(rdi=u32*uaddr, rsi=int op, rdx=u32 val [,r10=struct timespec*utime,r8=u32*uaddr2,r9=u32 val3])
                mov rdi,rcx         -- *futex
                mov rsi,#81         -- FUTEX_WAKE(1) or FUTEX_PRIVATE_FLAG(128)
                mov edx,1           -- val
                syscall             -- futex_wake(rcx,1)
pop rsi
pop rdi
          @@:
            ret
        []

--/*
procedure ::newtcb(::)
end procedure -- (for Edita/CtrlQ)
--*/
    ::newtcb
------------
        -- (no parameters)
        -- caller is responsible for any required locking
        -- on exit edi(/rdi) is pTCB/4 (dwThreadId is left set to 0)
        -- esi(/rsi) is preserved, all other registers are trashed [DEV]
        [32]
            mov edi,[pGtcb]     -- (stored /4)
            test edi,edi
            jz @f
                -- (this loop assumes pGtcb.csLock is in force)
              ::scanforfreetcblooptop
                mov edi,[ebx+edi*4+8]           -- pNxtcb
                test edi,edi
                jz @f
                cmp dword[ebx+edi*4+16],-3      -- nStatus (free)
                jne :scanforfreetcblooptop
                jmp :freetcbfound
          @@:
-->         mov eax,268         -- sizeof(TCB)
            mov eax,276         -- sizeof(TCB)
            call :pGetMem
            test eax,eax
--          jz :memoryallocationfailure
            jnz @f
                int3
          @@:
            mov edx,eax
            lea edi,[eax+4]
            shr eax,2
            push eax            -- [1] save result
            mov dword[edx],#00424354                    -- dwMagicT ("TCB\0")
-->         mov dword[edx+264],#00424345                -- dwMagicE ("ECB\0")
            mov dword[edx+272],#00424345                -- dwMagicE ("ECB\0")
            xor eax,eax
-->         mov ecx,65 -- (4+27*2+1+6)  -- (not dwMagicT/E, dwThreadId..csLock[$])
            mov ecx,67 -- (4+27*2+1+6+2)    -- (not dwMagicT/E, dwThreadId..pTLS)
            rep stosd
            mov dword[edx+236],#00424343                -- dwMagicC ("CCB\0")
        [PE32]
            add edx,240
            push edx                                    -- lpCriticalSection
            call "kernel32.dll","InitializeCriticalSection"
--      [ELF32]
            -- (a mutex (int32) of 0, as already set, is enough)
        [32]
            pop edi             -- [1] pTCB/4
          ::freetcbfound
            mov dword[ebx+edi*4+16],-1  -- nStatus (normal)
        [64]
            mov rax,rsp -- put 2 copies of rsp onto the stack...
            push rsp
            push rax
            or rsp,8    -- [rsp] is now 1st or 2nd copy:
                        -- if on entry rsp was xxx8: both copies remain on the stack
                        -- if on entry rsp was xxx0: or rsp,8 effectively pops one of them (+8)
                        -- obviously rsp is now xxx8, whatever alignment we started with
            sub rsp,8*5                                 -- minimum 4 param shadow space, and align/somewhere to save rax
            mov rdi,[pGtcb]                             -- (stored /4)
            test rdi,rdi
            jz @f
                -- (this loop assumes pGtcb.csLock is in force)
              ::scanforfreetcblooptop
                mov rdi,[rbx+rdi*4+8]           -- pNxtcb
                test rdi,rdi
                jz @f
                cmp qword[rbx+rdi*4+24],-3      -- nStatus (free)
                jne :scanforfreetcblooptop
                jmp :freetcbfound
          @@:
-->         mov rax,1012    -- sizeof(TCB)
            mov rax,1028    -- sizeof(TCB)
            call :pGetMem
            test rax,rax
--          jz :memoryallocationfailure
            jnz @f
                int3
          @@:
            mov rdx,rax
            lea rdi,[rax+8]
            shr rax,2
            mov [rsp+32],rax    -- [1] save result
            mov dword[rdx],#00424354                    -- dwMagicT ("TCB\0")
            mov dword[rdx+4],ebx                        -- dwThreadId (:=0)
-->         mov dword[rdx+1008],#00424345               -- dwMagicE ("ECB\0")
            mov dword[rdx+1024],#00424345               -- dwMagicE ("ECB\0")
            xor rax,rax
-->         mov rcx,125 -- (3+58*2+1+5) -- (not dwMagicT/E, not dwThreadId, pNextcb..csLock[$])
            mov rcx,127 -- (3+58*2+1+5+2) -- (not dwMagicT/E, not dwThreadId, pNextcb..pTLS)
            rep stosq
            mov dword[rdx+960],#00424342                -- dwMagicB ("BCB\0")
            mov dword[rdx+964],#00424343                -- dwMagicC ("CCB\0")
        [PE64]
            lea rcx,[rdx+968]                           -- lpCriticalSection (p1)
            call "kernel32.dll","InitializeCriticalSection"
--      [ELF64]
            -- (a mutex (int32) of 0, as already set, is enough)
        [64]
--          mov rax,[rsp+32]    -- [1] pTCB/4
            mov rdi,[rsp+32]    -- [1] pTCB/4
          ::freetcbfound
            mov qword[rbx+rdi*4+24],-1          -- nStatus (normal)
--          add rsp,8*5
--          pop rsp
            mov rsp,[rsp+8*5]   -- equivalent to the add/pop
        []
            ret

--/*
procedure ::mergeone(::)
end procedure -- (for Edita/CtrlQ)
--*/
    ::mergeone
--------------
        -- (should only ever be called by the block owner, or fully locked orphaned block handling)
    [32]
        -- on entry, return address in [esp] (DOH)
        --           block ptr in eax and [esp+4] (cleared by this routine)
        --           block idx(*4) in [esp+8] (cleared by this routine)
        --           block size in [esp+12] (cleared by this routine) 
        --           <return address for mergelist or null at [esp+16]>
        --           tcb/4 in [esp+20]
        --           (obviously, last two entries cleared by calling routine)
        mov esi,[eax-4]     -- pRoot
        and esi,#FFFFFFFE   -- clear free bit (0b01, needed when looping)
        mov ecx,[esp+12]    -- block size
        cmp dword[esi],#00484253    -- dwMagic ("SBH\0")
--      jne :memorycorruption
        je @f
            int3
      @@:
        cmp ecx,[esi+8]     -- pRoot.nsbSize (does not include header)
        jne :mergeonenotfull
            -- block fully merged, return it to the global pool
--DEV :%retruntoglobalpool (do we really care??)
            mov edi,[pGtcb]
            mov [esi+4],ebx             -- dwThreadId(0)
            mov [esi+12],edi            -- pTCB/4
            or esi,1
            mov [eax],ecx               -- nSize (should already be set?)
            mov [eax-4],esi             -- set free bit (0b01) on pRoot
            mov esi,eax
            lea ecx,[ebx+edi*4+240]     -- csLock
            cmp dword[ebx+edi*4+236],#00424343  -- dwMagicC ("CCB\0")
--          jne :notCCB
            je @f
                int3
          @@:
-->         cmp dword[ebx+edi*4+264],#00424345  -- dwMagicE ("ECB\0")
            cmp dword[ebx+edi*4+272],#00424345  -- dwMagicE ("ECB\0")
            jne :notECB
            push ecx                    -- (save lpCriticalSection/csLock for Leave)
            call :EnterCriticalSection
            mov ecx,[esp+12]            -- idx*4 (+12 as lpCriticalSection still on the stack)
            mov eax,[edi*4+ecx+20]      -- tmp:=pGtcb.pFree[idx]
            mov [esi+4],eax             -- this.pNext:=tmp
            mov [esi+8],ebx             -- this.pPrev:=null
--          mov [esi-8],ebx             -- this.era:=null (erm?)
            mov [edi*4+ecx+20],esi      -- pGtcb.pFree[idx]:=this
--DEV we also want to unlink from pFirst???
            pop ecx                     -- lpCriticalSection/csLock
            call :LeaveCriticalSection
        [64]
        -- on entry, return address in [rsp] (DOH)
--DEV r13,14,15?
        --           block ptr in rax and [rsp+8] (cleared by this routine)
        --           block idx(*8) in [rsp+16] (cleared by this routine)
        --           block size in [rsp+24] (cleared by this routine) 
        --           <return address for mergelist or null at [rsp+32]>
        --           tcb/4 in [rsp+40]
        --           (obviously, last two entries cleared by calling routine)
        --           Note that, as normal, stack is 8 mod 16 after all that!
        mov r8,[rax-8]                  -- pRoot
        and r8l, byte #FE               -- clear free bit (0b01, needed when looping, but not first time thru)
        mov rcx,[rsp+24]                -- block size
        cmp dword[r8],#00484253         -- dwMagic ("SBH\0")
--      jne :memorycorruption
        je @f
            int3
      @@:
        cmp rcx,[r8+8]                  -- pRoot.nsbSize (does not include header)
        jne :mergeonenotfull
            -- block fully merged, return it to the global pool
--DEV :%retruntoglobalpool (do we really care??)
            mov rdi,[pGtcb]
            mov [r8+4],ebx              -- dwThreadId(0)
            mov [r8+16],rdi             -- pTCB/4
            or r8l, byte 1
            mov [rax],rcx               -- nSize (should already be set?)
            mov [rax-8],r8              -- set free bit (0b01) on pRoot
            mov rsi,rax
            lea rcx,[rbx+rdi*4+968]     -- csLock
            cmp dword[rbx+rdi*4+960],#00424342  -- dwMagicB ("BCB\0")
--          jne :notCCB
            je @f
                int3
          @@:
            cmp dword[rbx+rdi*4+964],#00424343  -- dwMagicC ("CCB\0")
--          jne :notCCB
            je @f
                int3
          @@:
-->         cmp dword[rcx+40],#00424345         -- dwMagicE ("ECB\0")
            cmp dword[rcx+56],#00424345         -- dwMagicE ("ECB\0")
            jne :notECB
            push rcx                    -- save lpCriticalSection/csLock (for Leave)
            call :EnterCriticalSection
            mov rcx,[rsp+24]            -- idx*8
            mov rax,[rdi*4+rcx+32]      -- tmp:=pGtcb.pFree[idx]
            mov [rsi+8],rax             -- this.pNext:=tmp
            mov [rsi+16],rbx            -- this.pPrev:=null
--          mov [rsi-16],rbx            -- this.era:=null (erm?)
            mov [rdi*4+rcx+32],rsi      -- pGtcb.pFree[idx]:=this
--DEV we also want to unlink from pFirst?
            pop rcx                     -- lpCriticalSection/csLock
            call :LeaveCriticalSection
        []
            jmp :mergeonedone

      ::mergeonenotfull
        [32]
            -- odd or even?
            push eax                    -- [1] in case of no merge
            sub eax,esi
            sub eax,28  -- (superblock header + pRoot + era)
            cmp dword[esi],#00484253    -- dwMagic ("SBH\0")
            jne :notSBH
--          cdq                         -- eax -> edx:eax (sign extend)
            xor edx,edx
            idiv ecx
            test edx,edx
--          jnz :internalerror
            jz @f
                int3
          @@:
            test eax,1
            mov eax,[esp+8]             -- block ptr
            mov ecx,[esp+16]            -- block size
            jnz :mergeleft
                -- check right
                add eax,ecx
                jmp @f
          ::mergeleft
                -- check left
                sub eax,ecx
                mov [esp+8],eax         -- new block ptr, iff we merge and loop
          @@:
            mov edi,[eax-4]             -- pRoot
            xor esi,edi
            cmp esi,1
--          ja :memorycorruption
            jbe @f
--30/5/16:
                jmp :mc53
                int3
          @@:
            test edi,1
            jz :nomerge
            cmp ecx,[eax]               -- nSize
        [64]
            -- odd or even?
-- DEV use r12?
            push rax                    -- [1] in case of no merge
            sub rax,r8
            sub rax,48  -- (superblock header + pRoot + era)
            cmp dword[r8],#00484253     -- dwMagic ("SBH\0")
            jne :notSBH
--          cdq                         -- ?eax -> edx:eax (sign extend)
            xor rdx,rdx
            idiv rcx
            test rdx,rdx
--          jnz :internalerror
            jz @f
                int3
          @@:
            test rax,1
            mov rax,[rsp+16]            -- block ptr
            mov rcx,[rsp+32]            -- block size
            jnz :mergeleft
                -- check right
                add rax,rcx
                jmp @f
          ::mergeleft
                -- check left
                sub rax,rcx
                mov [rsp+16],rax        -- new block ptr, iff we merge and loop
          @@:
            mov rdi,[rax-8]             -- pRoot
-- 10/10/14:
--          xor rsi,rdi
--          cmp rsi,1
            xor r8,rdi
            cmp r8,1
--          ja :memorycorruption
            jbe @f
--test/tmp (will only work for pFreePool/mergeone, not mergelist/mergeone.)
-- (did not help: error occured mid-opRetf, so line number would be meaningless anyway,
--                plus the jmp !iDiag bypassed the natural trapping of int3 in fdbg.)
----                mov rdx,[rsp+48]
--              mov rdx,[rsp+56]
--              mov al,32                   -- e32hc(era,edi)
--              sub rdx,1
--              -- (rdi is supposed to be a pRoot, in this one particular case)
--              jmp :!iDiag
                int3
          @@:
            test rdi,1
            jz :nomerge
            cmp rcx,[rax]               -- nSize
        []
            jne :nomerge
                -- merge blocks - just detach eax from the freelist and loop.
        [32]
                add esp,4                   -- [1] discard 
--temp:
-- blat the middle pRoot (leave some footprints for a hex dump)
--mov esi,[esp+4] -- block ptr
--add esi,ecx
--mov dword[esi-4],-10
                mov esi,[eax+4]             -- pNext
                mov edx,[eax+8]             -- pPrev
                mov edi,[esp+8]             -- idx(*4)
                test edx,edx
                jnz :mergeone_midchain
                    mov edx,[esp+20]        -- tcb/4
                    test esi,esi
                    jz @f
                        mov [esi+8],ebx     -- this.pNext.pPrev:=null(=pPrev)
                  @@:
                    mov [edx*4+edi+20],esi  -- pTCB.pFree[idx]:=this.pNext
                    jmp @f
              ::mergeone_midchain
                    mov [edx+4],esi         -- this.pPrev.pNext:=this.pNext
                    test esi,esi
                    jz @f
                        mov [esi+8],edx     -- this.pNext.pPrev:=this.pPrev
              @@:
                mov eax,[esp+4]
                add edi,4
                add ecx,ecx                 -- (or shl ecx,1)
                mov [esp+8],edi             -- idx(*4)
                mov [esp+12],ecx            -- block size
                mov [eax],ecx               -- nSize
        [64]
--              add esp,8                   -- [1] discard 
                add rsp,8                   -- [1] discard 
--temp:
-- blat the middle pRoot (leave some footprints for a hex dump)
--mov rsi,[rsp+8]   -- block ptr
--add rsi,rcx
--mov qword[rsi-8],-10
                mov rsi,[rax+8]             -- pNext
                mov rdx,[rax+16]            -- pPrev
                mov rdi,[rsp+16]            -- idx(*8)
                test rdx,rdx
                jnz :mergeone_midchain
                    mov rdx,[rsp+40]        -- tcb/4
                    test rsi,rsi
                    jz @f
                        mov [rsi+16],rbx    -- this.pNext.pPrev:=null(=pPrev)
                  @@:
                    mov [rdx*4+rdi+32],rsi  -- pTCB.pFree[idx]:=this.pNext
                    jmp @f
              ::mergeone_midchain
                    mov [rdx+8],rsi         -- this.pPrev.pNext:=this.pNext
                    test rsi,rsi
                    jz @f
--                      mov [rsi+8],rdx     -- this.pNext.pPrev:=this.pPrev
                        mov [rsi+16],rdx    -- this.pNext.pPrev:=this.pPrev
              @@:
                mov rax,[rsp+8]             -- block ptr
                add rdi,8
                add rcx,rcx                 -- (or shl rcx,1)
                mov [rsp+16],rdi            -- idx(*8)
                mov [rsp+24],rcx            -- block size
                mov [rax],rcx               -- nSize
        []
                -- ...and loop back to see what else it can be merged with
                jmp :mergeone

          ::nomerge
                -- so just add the block to the owned freelist
        [32]
                pop eax                         -- [1] restore
                or edi,1                        -- set 0b01 bit...
                mov edx,[esp+20]                -- pTCB/4
                mov [eax-4],edi                 -- ...on pRoot
--              mov [eax-8],ebx                 -- era:=null?
                mov [eax+8],ebx                 -- pPrev(:=0)
                mov edi,[esp+8]                 -- idx(*4)
                cmp dword[ebx+edx*4],#00424354  -- dwMagicT ("TCB\0")
--              jne :notTCB
                je @f
                    int3
              @@:
                mov ecx,[edx*4+edi+20]          -- tmp:=pFree[idx]
                mov [eax+4],ecx                 -- this.pNext:=tmp
                test ecx,ecx
                jz @f
                    cmp dword[ecx+8],0          -- check tmp.pPrev was 0
--                  jnz :pPrevnot0
                    mov [ecx+8],eax             -- tmp.pPrev:=this
                    jz @f
                        pop edx
--                      mov edi,[esp+?]
                        mov edi,edx             -- temp
                        mov al,32               -- e32hc(era,edi)   --DEV show era/line number?
                        jmp :!iDiag
                        int3
              @@:
                mov [edx*4+edi+20],eax          -- pFree[idx]:=this
        [64]
                pop rax                         -- [1] restore
                or rdi,1                        -- set 0b01 bit...
                mov rdx,[rsp+40]                -- pTCB/4
                mov [rax-8],rdi                 -- ...on pRoot
--              mov [rax-16],rbx                -- era:=null?
                mov [rax+16],rbx                -- pPrev(:=0)
                mov rdi,[rsp+16]                -- idx(*8)
                cmp dword[rbx+rdx*4],#00424354  -- dwMagicT ("TCB\0")
--              jne :notTCB
                je @f
                    int3
              @@:
                mov rcx,[rdx*4+rdi+32]          -- tmp:=pFree[idx]
--2/12/14:
--              mov [rax+4],rcx                 -- this.pNext:=tmp
                mov [rax+8],rcx                 -- this.pNext:=tmp
                test rcx,rcx
                jz @f
                    cmp qword[rcx+16],0         -- check tmp.pPrev was 0
--                  jnz :pPrevnot0
                    mov [rcx+16],rax            -- tmp.pPrev:=this
                    jz @f
                        pop rdx
--                      mov rdi,[rsp+?]
                        mov rdi,rdx             -- temp
                        mov al,32               -- e32hc(era,edi)   --DEV show era/line number?
                        jmp :!iDiag
                        int3
              @@:
                mov [rdx*4+rdi+32],rax          -- pFree[idx]:=this
        []
      ::mergeonedone
    [32]
        ret 12
    [64]
        ret 24
    []

--/*
procedure ::mergelist(::)
end procedure -- (for Edita/CtrlQ)
--*/
    ::mergelist
---------------
    [32]
        -- on entry, <return address in [esp]>
        --           tcb/4 in [esp+4].
        --           list head in eax and [esp+8], (shd not be 0)
        --           block size in [esp+12],
        --           block idx(*4) in [esp+16],
        -- calling routine is responsible for removing the above
        -- (no specific result)
        mov ecx,[eax+4]     -- get/set pNext now
        mov edi,[esp+12]    -- block size
        mov edx,[esp+16]    -- block idx
        mov [esp+8],ecx     -- new list head
        -- (the following 3 parameters are cleared by mergeone)
        push edi            -- block size
        push edx            -- block idx
        push eax            -- block ptr
        call :mergeone
        mov eax,[esp+8]     -- pNext
        test eax,eax
    [64]
        -- on entry, <return address in [rsp]>
        --           tcb/4 in [rsp+8].
        --           list head in rax and [rsp+16], (shd not be 0)
        --           block size in [rsp+24],
        --           block idx(*8) in [rsp+32],
        -- calling routine is responsible for removing the above
        -- (no specific result)
        mov rcx,[rax+8]     -- get/set pNext now
        mov rdi,[rsp+24]    -- block size
        mov rdx,[rsp+32]    -- block idx
        mov [rsp+16],rcx    -- new list head
        -- (the following 3 parameters are cleared by mergeone)
        push rdi            -- block size
        push rdx            -- block idx
        push rax            -- block ptr
        call :mergeone
        mov rax,[rsp+16]    -- pNext
        test rax,rax
    []
        jne :mergelist
        ret

--/*
procedure ::scan_orphans(::)
end procedure -- (for Edita/CtrlQ)
--*/
  ::scan_orphans
----------------
    [32]
        -- on entry esi is a locked tcb with nStatus of -2 (orphaned)
        -- esi is preserved, all other registers are trashed.
        push dword -3                       -- [1] new nStatus (free, iff everything merges)
        mov edi,0                           -- idx*4
        mov edx,24                          -- block size
      ::scanorphanlooptop
            mov eax,[esi*4+edi+128]         -- is pNofl[idx]!=0?
            test eax,eax
            jz @f
                mov [esi*4+edi+128],ebx     -- pNofl[idx]:=0
                push edi                    -- [2] idx*4
                push edx                    -- [3] block size
                push eax                    -- [4] list head
                push esi                    -- [5] pTCB/4
                -- recap: mergelist expects
                --   return address in [esp],
                --   tcb/4 in [esp+4],
                --   list head in eax and [esp+8], 
                --   block size in [esp+12],
                --   block idx(*4) in [esp+16],
                -- calling routine is responsible for removing the above
                call :mergelist
                pop esi                     -- [5] restore pTCB/4
                pop eax                     -- [4] list head (discard)
                pop edx                     -- [3] restore block size
                pop edi                     -- [2] restore idx
          @@:
            cmp dword[esi*4+edi+20],0       -- is pFree[idx]!=0?
            je @f
                mov dword[esp],-2           -- leave nStatus as -2 (orphaned)
          @@:
            add edi,4
            add edx,edx
            cmp edi,108 -- (=27*4)
            jl :scanorphanlooptop
        pop dword[ebx+esi*4+16]             -- [1] new nStatus
    [64]
        -- on entry rsi is a locked tcb with nStatus of -2
        -- rsi is preserved, all other registers are trashed.
        push qword -3                       -- [1] new nStatus (free, iff everything merges)
        mov rdi,0                           -- idx*8
        mov rdx,44                          -- block size
      ::scanorphanlooptop
            mov rax,[rsi*4+rdi+496]         -- is pNofl[idx]!=0?
            test rax,rax
            jz @f
--6/11/16:
--              mov [rsi*4+rdi+496],ebx     -- pNofl[idx]:=0
                mov [rsi*4+rdi+496],rbx     -- pNofl[idx]:=0
                push rdi                    -- [2] idx*8
                push rdx                    -- [3] block size
                push rax                    -- [4] list head
                push rsi                    -- [5] pTCB/4
                -- recap: mergelist expects
                --   return address in [rsp],
                --   tcb/4 in [rsp+8],
                --   list head in rax and [rsp+16], 
                --   block size in [rsp+24],
                --   block idx(*8) in [rsp+32],
                -- calling routine is responsible for removing the above
                call :mergelist
                pop rsi                     -- [5] restore pTCB/4
                pop rax                     -- [4] list head (discard)
                pop rdx                     -- [3] restore block size
                pop rdi                     -- [2] restore idx
          @@:
            cmp qword[rsi*4+rdi+32],0       -- is pFree[idx]!=0?
            je @f
                mov qword[rsp],-2           -- leave nStatus as -2 (orphaned)
          @@:
            add rdi,8
            add rdx,rdx
--          cmp edi,108 -- (=27*4)
            cmp rdi,464 -- (=58*8)
            jl :scanorphanlooptop
--      pop qword[rbx+rsi*4+24]             -- [1] new nStatus
        pop rax
--DEV 9/10/14 (spotted in passing)
--      mov [ebx+esi*4+16],rax              -- [1] new nStatus
--6/11/16 (DEV: did I replace the pop because pilasm.e was[/is] not coping with it??)
--      mov [ebx+esi*4+24],rax              -- [1] new nStatus
        mov [rbx+rsi*4+24],rax              -- [1] new nStatus
    []
        ret

--
-- NOTE: it seemed like a clever idea to common up diagnostics, but that
--        just made the diagnostics themselves very much harder to debug!
--        (as needed I expect to gradually undo/inline the following..)
--
--    ::memorycorruption    (..much like I've already done with this)
--      int3
--      nop
--    ::pPrevnot0   -- e32hc(era,edi)
--          -- first, clear that offending tmp.pPrev
--          -- (not ideal, but might reduce diaglooping)
--          -- (may also need to unlock)
--          [32]
--              mov dword[ecx+8],0
--              mov ecx,2
--              pop edx
--              add esp,12
--          [64]
--              mov qword[rcx+16],0
--              mov rcx,2
--              pop rdx
--              add rsp,24
--          []
--              mov al,32
--              jmp :!iDiag
--              int3
--              nop
--    ::internalerror
--      int3
--      nop
--    ::invalidmemoryrequest
--      int3
--      nop
--    ::memoryallocationfailure     -- e77phroom?
--      int3
--      nop
      ::deallocerrunknowntype
        [32]
            mov edx,[esp+4]
            mov al,32
            sub edx,1
            mov edi,edx
        [64]
            mov rdx,[rsp+8]
            mov al,32
            sub rdx,1
            mov rdi,rdx
        []
            jmp :!iDiag
            int3
        nop
--    ::notCCB
--      int3
--      nop
      ::notECB
        int3
        nop
      ::notSBH
        int3
        nop
--    ::notTCB
--      int3
--      nop
--    ::notb01
--      int3
--      nop
      ::sizeErr
        int3
        nop
      ::pfreeidxnz
        int3
        nop
      ::listheadz
        int3
        nop
      ::idxlz
        -- [DEV] improve this...
--      jmp :%e02atdb0
        int3
        nop
--    ::e37atambpi
--      int3
--      nop
      ::e38atfmba   -- argument to free must be an atom
        int3
        nop

--/*
procedure :%pGetPool(:%)
end procedure -- (for Edita/CtrlQ)
--*/
    :%pGetPool
--------------
    [32]
        -- on entry ecx is the required length (in bytes, including any headers)
        --          edx is <era>
        -- on exit, edx is the allocated size and eax is the raw pointer, and
        --          all other registers are trashed.
        -- the returned memory is left filled with whatever garbage was there.
        -- can return 0 (which should be fatal for everything except allocate())
sub edx,1
        push edx    -- [0] era
        push ecx    -- [1] save required size
        xor ebx,ebx -- (save some grief)
        call :pGetThread    -- (eax:=dwThreadId; most regs trashed)
        push eax    -- [2] save thread id
        mov esi,[pGtcb]
        test esi,esi
    [64]
        -- on entry rcx is the required length (in bytes, including any headers)
        --          rdx is <era>
        -- on exit, rdx is the allocated size and rax is the raw pointer, and
        --          all other registers are trashed.
        -- the returned memory is left filled with whatever garbage was there.
        -- can return 0 (which should be fatal for everything except allocate())
        sub rsp,8*11    -- minimum shadow space plus align/temp stores:
                        -- [rsp+0/8/16/24] shadow space <no longer rqd/used>
                        -- [rsp]    temp result     \
                        -- [rsp+8]  unused           \  ( recent
                        -- [rsp+16] asize (>8K)      /   additions)
                        -- [rsp+24] idx*8 for ""    /
                        -- [rsp+32] [1]required size
                        -- [rsp+40] lpCriticalSection
                        -- [rsp+48] [2]dwThreadId
                        -- [rsp+56] idx*8
                        -- [rsp+64] pTCB/4
                        -- [rsp+72] rqd size, rounded up (<2*[1])
                        -- [rsp+80] <era>
        xor rbx,rbx     -- (save some grief)
        mov [rsp+80],rdx    -- [0] era
        mov [rsp+32],rcx    -- [1] save required size
        call :pGetThread    -- (rax:=dwThreadId; most regs trashed)
        mov [rsp+48],eax    -- [2] save thread id
        mov rsi,[pGtcb]
        test rsi,rsi
    []
        jnz @f
            -- create a thread control block for the global pool...
            call :newtcb                -- (edi/rdi := pTCB/4)
    [32]
            mov dword[ebx+edi*4+16],-4  -- nStatus (global pool)
            mov [pGtcb],edi
    [64]
            mov qword[rbx+rdi*4+24],-4  -- nStatus (global pool)
            mov [pGtcb],rdi
    []
          ::newcttcb
            -- ...and one for the current thread
    [32]
            mov ecx,[pGtcb]
            shl ecx,2
            add ecx,240     -- csLock
            cmp dword[ecx-4],#00424343      -- dwMagicC ("CCB\0")
--          jne :notCCB
            je :CCBok
                int3
          ::CCBok
-->         cmp dword[ecx+24],#00424345     -- dwMagicE ("ECB\0")
            cmp dword[ecx+32],#00424345     -- dwMagicE ("ECB\0")
            jne :notECB
            push ecx                        -- lpCriticalSection/csLock (for Leave)
    [64]
            mov rcx,[pGtcb]
            shl rcx,2
            add rcx,968     -- csLock
            cmp dword[rcx-8],#00424342      -- dwMagicB ("BCB\0")
--          jne :notCCB
            je :CCBok
                int3
          ::CCBok
            cmp dword[rcx-4],#00424343      -- dwMagicC ("CCB\0")
--          jne :notCCB
            je :CCBok2
                int3
          ::CCBok2
-->         cmp dword[rcx+40],#00424345     -- dwMagicE ("ECB\0")
            cmp dword[rcx+56],#00424345     -- dwMagicE ("ECB\0")
            jne :notECB
            mov [rsp+40],rcx            -- save lpCriticalSection/csLock (for Leave)
    []
            call :EnterCriticalSection
            call :newtcb                -- (edi/rdi := pTCB/4)
    [32]
            mov esi,edi
            mov edi,[pGtcb]
            mov eax,[esp+4]             -- [2] thread id
            mov edx,[ebx+edi*4+8]       -- edx:=pGtcb.pNxtcb
--          mov [gt1tcb],edx            -- (save for pGetStdCS)
            mov [ebx+esi*4+8],edx       -- new.pNxtcb:=edx
            mov [ebx+esi*4+4],eax       -- new.dwThreadId:=eax
            mov [ebx+edi*4+8],esi       -- pGtcb.pNxtcb:=new
            pop ecx                     -- lpCriticalSection/csLock
    [64]
            mov rsi,rdi
            mov rdi,[pGtcb]
            mov eax,dword[rsp+48]       -- [2] thread id
            mov rdx,[rbx+rdi*4+8]       -- rdx:=pGtcb.pNxtcb
--          mov [gt1tcb],rdx            -- (save for pGetStdCS)
            mov [rbx+rsi*4+8],rdx       -- new.pNxtcb:=rdx
            mov [rbx+rsi*4+4],eax       -- new.dwThreadId:=eax
            mov [rbx+rdi*4+8],rsi       -- pGtcb.pNxtcb:=new
            mov rcx,[rsp+40]            -- lpCriticalSection/csLock
    []  
            call :LeaveCriticalSection
            jmp :tcbfound

      @@:
    [32]
        mov esi,dword[ebx+esi*4+8]          -- tcb:=tcb.pNxtcb
        test esi,esi
        jz :newcttcb
      :!GetPoolnotTCBa  -- era @ [esp+8]
        cmp dword[ebx+esi*4],#00424354      -- dwMagicT ("TCB\0")
--      jne :memorycorruption
        je :tcbmagicok
            int3
      ::tcbmagicok
        cmp dword[ebx+esi*4+4],eax          -- dwThreadId
    [64]
        mov rsi,qword[rbx+rsi*4+8]          -- tcb:=tcb.pNxtcb
        test rsi,rsi
        jz :newcttcb
      :!GetPoolnotTCBa  -- era @ [rsp+80]
        cmp dword[rbx+rsi*4],#00424354      -- dwMagicT ("TCB\0")
--      jne :memorycorruption
        je :tcbmagicok
            int3
      ::tcbmagicok
        cmp dword[rbx+rsi*4+4],eax          -- dwThreadId
    []
        jne @b

      ::tcbfound
    [32]
        pop eax         -- [2] discard the ThreadId
        pop ecx         -- [1] restore rqd size
        -- esi contains pTCB/4, stack is now empty
        push esi        -- [1] pTCB/4
        -- figure out the required size
        cmp ecx,#50000000
--      ja :invalidmemoryrequest
        jb @f
            int3
      @@:
        add ecx,8       -- add space for pRoot + era
        mov edx,24      -- size
        mov edi,0       -- idx*4
      @@:
        cmp edx,ecx
        jge @f
            add edx,edx
            add edi,4
            jmp @b
      @@:
        push edi        -- [2] save idx(*4)
        push edx        -- [3] save rqd size, rounded up
    [64]
        mov rcx,[rsp+32]    -- [1?] restore required size
        mov [rsp+64],rsi    -- pTCB/4
        -- figure out the required size
--      mov rdx,#5000000000000000
        mov rdx,#50000000 --00000000
        shl rdx,32
        cmp rcx,rdx
--      ja :invalidmemoryrequest
        jb @f
            int3
      @@:
        add rcx,16      -- add space for pRoot + era
        mov rdx,44      -- size
        mov rdi,0       -- idx*8
      @@:
        cmp rdx,rcx
        jge @f
            add rdx,rdx
            add rdi,8
            jmp @b
      @@:
        mov [rsp+56],rdi    -- [2?] save idx(*8)
        mov [rsp+72],rdx    -- [3?] save rqd size, rounded up
    []
        -- now look for an available block of >= rqd size
      ::gpchecknext
    [32]
        mov eax,[esi*4+edi+20]  -- pTCB.pFree[idx]
        test eax,eax
        jnz :!blockfound
        mov eax,[esi*4+edi+128] -- pTCB.pNofl[idx]
        test eax,eax
        jnz :retakeownership
        add edx,edx
        add edi,4
        cmp edi,108 -- (=27*4)
        jl :gpchecknext
        -- nothing suitable, get a fresh block.
        -- first: process any orphaned blocks (ie merge and shift what we can into the global pool)
--      pushad -- (not entirely sure what we think we are preserving here...)
                -- eax=0, edi=108, edx=#A0000000, esi is pTCB/4, which is already pushed and not used again, ecx is done with (size+4)
        mov esi,[pGtcb]
      @@:
        mov esi,[ebx+esi*4+8]           -- pNxtcb
        test esi,esi
        jz @f
            cmp dword[ebx+esi*4+16],-2  -- nStatus (orphaned?)
            jne @b
            lea edx,[ebx+esi*4+240]     -- csLock
            push edx            -- [?8]                 -- lpCriticalSection (for Leave)
            mov ecx,edx
            call :EnterCriticalSection
            cmp dword[ebx+esi*4+16],-2  -- nStatus (orphaned?)
            jne :someonelsedidit
                -- (aside: the biggest worry here and what really warrants the above recheck
                --          would be that while we waited it could have been processed /and/ 
                --          then reused for a new thread, which is happily using it *right
                --          now*, without any locking whatsoever. Just saying, in case you 
                --          were thinking "it don't matter if we call scan_orphans twice"...
                --          and you'd be quite right, as long as it wasn't /in/ re-use.
                --          Of course, it /might/ have been scanned, re-used, and by now
                --          re-orphaned (-2), but that's no problem. Anyway, sorry to panic
                --          you over - OMG - 7 bytes and/or 1 clock, but we need it!)
            call :scan_orphans
          ::someonelsedidit
            pop ecx                     -- lpCriticalSection/csLock
            call :LeaveCriticalSection
    [64]
        mov rax,[rsi*4+rdi+32]  -- pTCB.pFree[idx]
        test rax,rax
        jnz :!blockfound
        mov rax,[rsi*4+rdi+496] -- pTCB.pNofl[idx]
        test rax,rax
        jnz :retakeownership
        add rdx,rdx
        add rdi,8
--      cmp edi,108 -- (=27*4)
        cmp rdi,464 -- (=58*8)
        jl :gpchecknext
        -- nothing suitable, get a fresh block.
        -- first: process any orphaned blocks (ie merge and shift what we can into the global pool)
        mov rsi,[pGtcb]
      @@:
        mov rsi,[rbx+rsi*4+8]           -- pNxtcb
        test rsi,rsi
        jz @f
            cmp dword[rbx+rsi*4+24],-2  -- nStatus (orphaned?)
            jne @b
            lea rcx,[rbx+rsi*4+968]     -- csLock
            mov [rsp+40],rcx            -- save lpCriticalSection (for Leave)
            call :EnterCriticalSection
            cmp dword[rbx+rsi*4+24],-2  -- nStatus (orphaned?)
            jne :someonelsedidit
                -- (notes as per 32 bit)
            call :scan_orphans
          ::someonelsedidit
            mov rcx,[rsp+40]            -- lpCriticalSection/csLock
            call :LeaveCriticalSection
    []
            jmp @b
      @@:
        -- now check whether something in the global pool will satify our needs:
    [32]
        mov esi,[pGtcb]
        mov edi,[esp+4]     -- [2] idx*4
        mov edx,[esp]       -- [3] rqd size
    [64]
        mov rsi,[pGtcb]
        mov rdi,[rsp+56]    -- [2?] idx*8
        mov rdx,[rsp+72]    -- [3?] rqd size (rounded up)
    []
      ::gpchecknextgpool
    [32]
        cmp dword[esi*4+edi+20],0   -- pGtb.pFree[idx]
    [64]
        cmp qword[rsi*4+rdi+32],0   -- pGtb.pFree[idx]
    []
        je :gpentryempty
    [32]
            push edx        -- [4] temp save (size)
            push ebx        -- [5] temp result
            lea ecx,[ebx+esi*4+240]             -- pGtcb.csLock
            cmp dword[ebx+esi*4+236],#00424343  -- dwMagicC ("CCB\0")
--          jne :notCCB
            je @f
                int3
          @@:
-->         cmp dword[ecx+24],#00424345         -- dwMagicE ("ECB\0")
            cmp dword[ecx+32],#00424345         -- dwMagicE ("ECB\0")
            jne :notECB
            push ecx                            -- save lpCriticalSection (for Leave)
            call :EnterCriticalSection
    [64]
--DEV use [rsp+16], [rsp] instead:
--          mov r14,rdx     -- temp save (size)
--          mov r13,rbx     -- temp result
            mov [rsp+16],rdx    -- temp save (size)
            mov [rsp],rbx       -- temp result
            lea rcx,[rbx+rsi*4+968]     -- pGtcb.csLock
            cmp dword[rbx+rsi*4+960],#00424342  -- dwMagicB ("BCB\0")
--          jne :notCCB
            je @f
                int3
          @@:
            cmp dword[rbx+rsi*4+964],#00424343  -- dwMagicC ("CCB\0")
--          jne :notCCB
            je @f
                int3
          @@:
-->         cmp dword[rcx+40],#00424345         -- dwMagicE ("ECB\0")
            cmp dword[rcx+56],#00424345         -- dwMagicE ("ECB\0")
            jne :notECB
            mov [rsp+40],rcx                    -- save lpCriticalSection (for Leave)
            call :EnterCriticalSection
    [32]
            mov eax,[esi*4+edi+20]      -- recheck (pGtb.pFree[idx])
            test eax,eax
    [64]
            mov rax,[rsi*4+rdi+32]      -- recheck (pGtb.pFree[idx])
            test rax,rax
    []
            jz :gpentrystolen
    [32]
--DEV this was already +4?
                mov ecx,[eax+4]         -- pNext
                mov [esi*4+edi+20],ecx  -- pGtb.pFree[idx]
                mov [esp+4],eax         -- stash result
    [64]
                mov rcx,[rax+8]         -- pNext
                mov [rsi*4+rdi+32],rcx  -- pGtb.pFree[idx]
--              mov r13,rax             -- stash result
                mov [rsp],rax           -- stash result
    []
          ::gpentrystolen
    [32]
            pop ecx                     -- lpCriticalSection/csLock
            call :LeaveCriticalSection
            pop eax         -- [5] result or null
            pop edx         -- [4] restore (size)
            test eax,eax
            jz :gpentryempty
            -- perform some quick sanity checks:
            lea esi,[eax-28]    -- start of superblock
            mov ecx,[eax-4]     -- pRoot...
            test ecx,0b01       -- .. should have 0b01 set
--          jz :notb01
            jnz @f
                int3
          @@:
            sub ecx,1
            cmp esi,ecx         -- ...but otherwise equal eax-28
--          jnz :notb01
            jz @f
                int3
          @@:
            cmp dword[esi],#00484253 -- dwMagic ("SBH\0")
            jne :notSBH
            cmp dword[esi+8],edx    -- and pSBH.nsbSize should match
            jne :sizeErr
            push edx            -- ([4]), size (as expected at gpgotfromgpool)
    [64]
            mov rcx,[rsp+40]            -- lpCriticalSection/csLock
            call :LeaveCriticalSection
--          mov rax,r13     -- result or null
--          mov rdx,r14     -- restore (size)
            mov rax,[rsp]       -- result or null
            mov rdx,[rsp+16]    -- restore (size)
            test rax,rax
            jz :gpentryempty
            -- perform some quick sanity checks:
            lea rsi,[rax-48]    -- start of superblock
            mov rcx,[rax-8]     -- pRoot...
            test rcx,0b01       -- .. should have 0b01 set
--          jz :notb01
            jnz @f
                int3
          @@:
            sub rcx,1
            cmp rsi,rcx         -- ...but otherwise equal rax-48
--          jnz :notb01
            jz @f
                int3
          @@:
            cmp dword[rsi],#00484253 -- dwMagic ("SBH\0")
            jne :notSBH
            cmp qword[rsi+8],rdx    -- and pSBH.nsbSize should match
            jne :sizeErr
--          -- (gpgotfromgpool expects the size to be in r14)
            -- (gpgotfromgpool expects the size to be in [rsp+16])
--          mov [rsp+16],r14
    []
            jmp :gpgotfromgpool -- (resets dwThreadId and pTCB)

      ::gpentryempty
    [32]
        add edx,edx
        add edi,4
        cmp edi,108 -- (=27*4)
    [64]
        add rdx,rdx
        add rdi,8
--      cmp rdi,264 -- (=58*8)
        cmp rdi,464 -- (=58*8)
    []
        jl :gpchecknextgpool

        -- nowt useful in the global pool, get some new memory then.
    [32]
        mov edi,[esp+4]     -- [2] idx*4
        mov eax,[esp]       -- [3] rqd size
--      push esi            -- [4] save (tcb/4)
        cmp edi,36
        jae @f
            mov edi,36
            mov eax,12288   -- (block size[10])
    [64]
        mov rdi,[rsp+56]    -- [2?] idx*8
        mov rax,[rsp+72]    -- [3?] rqd size
        cmp rdi,64
        jae @f
            mov rdi,64
            mov rax,11264
    []
      @@:
        -- create a new superblock and add it (under locking) to the pGtcb->pNxtcb chain
    [32]
        mov esi,[pGtcb]
        push eax                    -- [4] save size
        push edi                    -- [5] save (idx(*4))
        add eax,20                  -- sizeof(Superblock Header)
        lea ecx,[ebx+esi*4+240]     -- pTCB.csLock
        cmp dword[ebx+esi*4+236],#00424343  -- dwMagicC ("CCB\0")
--      jne :notCCB
        je @f
            int3
      @@:
-->     cmp dword[ecx+24],#00424345         -- dwMagicE ("ECB\0")
        cmp dword[ecx+32],#00424345         -- dwMagicE ("ECB\0")
        jne :notECB
        push eax                    -- [6] size/space for result
        push ecx                    -- save lpCriticalSection (for Leave)
        call :EnterCriticalSection
    [64]
        mov rsi,[pGtcb]
--DEV why not [rsp+56/72]? (oh, I think I know why... but instead why not two extra slots on rsp? 0/8/16/24 are all free!)
--      mov r14,rax                 -- [4?] save size
--      mov r12,rdi                 -- [5?] save (idx(*8))
        mov [rsp+16],rax            -- [4?] save size
        mov [rsp+24],rdi            -- [5?] save (idx(*8))
--      add rax,32                  -- sizeof(Superblock Header)
        lea rcx,[rbx+rsi*4+968]     -- pTCB.csLock
        cmp dword[rbx+rsi*4+960],#00424342  -- dwMagicB ("BCB\0")
--      jne :notCCB
        je @f
            int3
      @@:
        cmp dword[rbx+rsi*4+964],#00424343  -- dwMagicC ("CCB\0")
--      jne :notCCB
        je @f
            int3
      @@:
-->     cmp dword[rcx+40],#00424345         -- dwMagicE ("ECB\0")
        cmp dword[rcx+56],#00424345         -- dwMagicE ("ECB\0")
        jne :notECB
        mov [rsp+40],rcx            -- save lpCriticalSection (for Leave)
        call :EnterCriticalSection
    [32]
        mov eax,[esp+4]             -- [6]
--      add eax,20                  -- sizeof(Superblock Header)
        call :pGetMem
        test eax,eax
        jz @f
--          add eax,4
            mov ecx,[ebx+esi*4+12]  -- tmp:=pGtcb.pFirst
            mov [eax+16],ecx        -- this.pNxtsb:=tmp
            mov [ebx+esi*4+12],eax  -- pGtcb.pFirst:=this
      @@:
        mov [esp+4],eax             -- [6] store result
--      mov esi,eax
        pop ecx                     -- lpCriticalSection/csLock
        call :LeaveCriticalSection
    [64]
--      mov rax,r14
        mov rax,[rsp+16]
        add rax,32                  -- sizeof(Superblock Header)
        call :pGetMem
        test rax,rax
        jz @f
            mov rcx,[rbx+rsi*4+16]  -- tmp:=pGtcb.pFirst
            mov [rax+24],rcx        -- this.pNxtsb:=tmp
            mov [rbx+rsi*4+16],rax  -- pGtcb.pFirst:=this
      @@:
--      mov r13,rax                 -- save result
        mov [rsp],rax               -- save result
        mov rcx,[rsp+40]            -- lpCriticalSection/csLock
        call :LeaveCriticalSection
    [32]
        pop esi                     -- [6] result
        pop edi                     -- [5] restore (idx(*4))
        test esi,esi
        jnz :gpgotfromgpool
            add esp,20              -- (now including era)
            xor eax,eax
            ret
    [64]
--      mov rsi,r13                 -- result
        mov rsi,[rsp]               -- result
--      mov rdi,r12                 -- [5?] restore (idx(*8))
        mov rdi,[rsp+24]            -- [5?] restore (idx(*8))
        test rsi,rsi
        jnz :gpgotfromgpool
--DEV rax:=0?? (added)
            add rsp,8*11
            xor rax,rax
            ret
        -- (size is (still) in r14) [erm, [rsp+16]?]
    []

--/*
procedure ::gpgotfromgpool(::)
end procedure -- (for Edita/CtrlQ)
--*/
      ::gpgotfromgpool
----------------------
        call :pGetThread    -- (eax:=dwThreadId; most regs trashed)
    [32]
        pop edx             -- [4] size
        mov dword[esi],#00484253    -- dwMagic ("SBH\0")
        mov [esi+4],eax             -- dwThreadId
--      mov eax,[esp]
        mov eax,[esp+8]             -- [1] pTCB/4
        mov [esi+8],edx             -- nsbSize
        mov [esi+12],eax            -- pTCB/4
        mov [esi+24],esi            -- set pRoot
--      ; eax,edi,edx should be set
        lea eax,[esi+28] -- hmm...
    [64]
--      mov rdx,r14                 -- [4?] size
        mov rdx,[rsp+16]            -- [4?] size
        mov dword[rsi],#00484253    -- dwMagic ("SBH\0")
        mov [rsi+4],eax             -- dwThreadId
        mov rax,[rsp+64]            -- pTCB/4
        mov [rsi+8],rdx             -- nsbSize
        mov [rsi+16],rax            -- set sbh.pTCB/4
        mov [rsi+40],rsi            -- set pRoot
--      ; rax,rdi,rdx should be set
        lea rax,[rsi+48] -- hmm...
    []
        jmp :blockdetached

      ::retakeownership
    [32]
        push edi            -- [4] save (idx) (may be unnecessary)
        push edx            -- [5] save (block size)
        push ebx            -- [6] create space (list head)
        push esi            -- [7] save (pTCB/4)
        lea ecx,[ebx+esi*4+240]         -- csLock
        cmp dword[ebx+esi*4],#00424354  -- dwMagicT ("TCB\0")
--      jne :notTCB
        je @f
            mov edx,[esp+16]
            mov edi,[esp+12]
            mov al,32                   -- e32hc(era,edi)
            sub edi,1
            jmp :!iDiag
            int3
      @@:
        cmp dword[ecx-4],#00424343      -- dwMagicC ("CCB\0")
--      jne :notCCB
        je @f
            int3
      @@:
-->     cmp dword[ecx+24],#00424345     -- dwMagicE ("ECB\0")
        cmp dword[ecx+32],#00424345     -- dwMagicE ("ECB\0")
        jne :notECB
        push ecx            -- [8]      -- save lpCriticalSection (for Leave)
        call :EnterCriticalSection
    [64]
        mov [rsp],rsi       -- [7] save pTCB/4
        mov [rsp+8],rbx     -- [6] space for list head
        mov [rsp+16],rdx    -- [5] save block size
        mov [rsp+24],rdi    -- [4] save idx*8

        lea rcx,[rbx+rsi*4+968]         -- csLock
        cmp dword[rbx+rsi*4],#00424354  -- dwMagicT ("TCB\0")
--      jne :notTCB
        je @f
            mov rdx,[rsp+88]
            mov rdi,[rsp+80]
            mov al,32                   -- e32hc(era,edi)
            sub rdi,1
            jmp :!iDiag
            int3
      @@:
        cmp dword[rcx-8],#00424342      -- dwMagicB ("BCB\0")
--      jne :notCCB
        je @f
            int3
      @@:
        cmp dword[rcx-4],#00424343      -- dwMagicC ("CCB\0")
--      jne :notCCB
        je @f
            int3
      @@:
-->     cmp dword[rcx+40],#00424345     -- dwMagicE ("ECB\0")
        cmp dword[rcx+56],#00424345     -- dwMagicE ("ECB\0")
        jne :notECB
        mov [rsp+40],rcx            -- save lpCriticalSection (for Leave)
        call :EnterCriticalSection
    [32]
        cmp dword[ebx+esi*4],#00424354      -- magic ("TCB\0")
--      jne :notTCB
        je @f
            int3
      @@:
        mov eax,[esi*4+edi+128]     -- grab latest (pTCB.pNofl[idx])
        mov [esi*4+edi+128],ebx     -- and zero
        mov [esp+8],eax     -- [6] store list head
        pop ecx                     -- lpCriticalSection/csLock
        call :LeaveCriticalSection
    [64]
        cmp dword[rbx+rsi*4],#00424354      -- magic ("TCB\0")
--      jne :notTCB
        je @f
            int3
      @@:
        mov rax,[rsi*4+rdi+496]     -- grab latest (pTCB.pNofl[idx])
        mov [rsi*4+rdi+496],rbx     -- and zero
        mov [rsp+8],rax     -- [6] store list head
        mov rcx,[rsp+40]            -- lpCriticalSection/csLock
        call :LeaveCriticalSection
    [32]    
        mov eax,[esp+4]     -- [6] list head
        test eax,eax
        jz :listheadz       -- (no-one else should have done it!)
        -- recap: mergelist expects
        --   return address in [esp],
        --   tcb/4 in [esp+4],
        --   list head in eax and [esp+8], 
        --   block size in [esp+12],
        --   block idx(*4) in [esp+16],
        -- calling routine is responsible for removing the above
    [64]
        mov rax,[rsp+8]     -- [6] list head
        test rax,rax
        jz :listheadz       -- (no-one else should have done it!)
        -- recap: mergelist expects
        --   return address in [rsp],
        --   tcb/4 in [rsp+8],
        --   list head in rax and [rsp+16], 
        --   block size in [rsp+24],
        --   block idx(*8) in [rsp+32],
        -- calling routine is responsible for removing the above
    []
        call :mergelist
    [32]
        pop esi             -- [7] restore pTCB/4
        pop eax             -- [6] list head (discard)
        pop edx             -- [5] restore block size
        pop edi             -- [4] restore idx
    [64]
        mov rsi,[rsp]       -- [7] restore pTCB/4
        mov rax,[rsp+8]     -- [6] list head (discard)
        mov rdx,[rsp+16]    -- [5] restore block size
        mov rdi,[rsp+24]    -- [4] restore idx
    []
        -- go back and see what mergelist has left us, if anything.
        jmp :gpchecknext

    [32]
      :!blockfound  -- era @ [esp+12]
        -- first detach it from the freelist
        -- (see the recap in blockdetached, [only] esi and ecx are available)
        -- If we get an exception here, and as :!blockfound is only invoked 
        --  from one point above, we immediately zero dword[esi*4+edi+20],
        --  in pFEH.e, to minimise any heap corruption knock-on effects.
--DEV should we not check here that pRoot is sensible first?
        mov esi,[eax+4]             -- pNext
        mov ecx,[eax+8]             -- pPrev
        test ecx,ecx
        jnz :!bf_midchain
            mov ecx,[esp+8]         -- pTCB/4
            test esi,esi
            jz @f
--17/4/16:
--              cmp esi,#10
--              jbe :mc53clear
              :!blockfoundC0000005
                mov [esi+8],ebx     -- this.pNext.pPrev:=null(=pPrev)
          @@:
            cmp dword[ebx+ecx*4],#00424354  -- magic ("TCB\0")
--          jne :notTCB
            je @f
                int3
          @@:
            mov [ecx*4+edi+20],esi  -- pTCB[idx]:=this.pNext
            jmp :blockdetached
      :!bf_midchain
--17/4/16:
--          cmp ecx,2
--          jbe :mc53clear
            mov [ecx+4],esi         -- this.pPrev.pNext:=this.pNext
            test esi,esi
            jz :blockdetached
--17/4/16 trap here (esi is 1)
                cmp esi,#800            -- may need other values, maybe scan pGtcb chain? [DEV temp, either way]
                ja @f
--                  -- zero a few pointers in an attempt to prevent infinite loops reporting the error..
--                  mov [ecx+4],ebx         -- this.pPrev.pNext:=0
--                ::mc53clear
--                  mov ecx,[esp+8]         -- pTCB/4
--                  mov [eax+4],ebx         -- this.pNext:=0
--                  mov [eax+8],ebx         -- this.pPrev:=0
--                  mov [ecx*4+edi+20],ebx  -- pTCB[idx]:=0
                    jmp :mc53
              @@:
                mov [esi+8],ecx     -- this.pNext.pPrev:=this.pPrev
    [64]
      :!blockfound  -- era @ [rsp+80]
        -- first detach it from the freelist
        -- (see the recap in blockdetached, [only] esi and ecx are available)
        -- if we get an exception here, and as :!blockfound is only called 
        --  from one point above, we immediately zero qword[rsi*4+rdi+32],
        --  in pFEH.e, to minimise any heap corruption knock-on effects.
        mov rsi,[rax+8]             -- pNext
        mov rcx,[rax+16]            -- pPrev
        test rcx,rcx
        jnz :!bf_midchain
            mov rcx,[rsp+64]        -- pTCB/4
            test rsi,rsi
            jz @f
              :!blockfoundC0000005
                mov [rsi+16],rbx    -- this.pNext.pPrev:=null(=pPrev)
          @@:
            cmp dword[rbx+rcx*4],#00424354 -- magic ("TCB\0")
--          jne :notTCB
            je @f
                int3
          @@:
-- 2/12/14:
--          mov [rcx*4+rdi+32],esi  -- pTCB[idx]:=this.pNext
            mov [rcx*4+rdi+32],rsi  -- pTCB[idx]:=this.pNext
            jmp :blockdetached
      :!bf_midchain
            mov [rcx+8],rsi         -- this.pPrev.pNext:=this.pNext
            test rsi,rsi
            jz :blockdetached
                mov [rsi+16],rcx    -- this.pNext.pPrev:=this.pPrev
    []
      ::blockdetached
    [32]
        -- recap:
        --      [esp] is rqd block size, rounded up (<=edx)
        --      [esp+4] is rqd idx*4 (<=edi) (not actually needed anymore)
        --      [esp+8] is pTCB/4
--DEV   --      [esp+12] is era
--      --      [esp+12] is <return address>
        --      [esp+16] is <return address>
        --      eax is an available block
        --      edx is size of ""
        --      edi is idx*4 of ""
        --      (obviously we may need to split eax until edx/edi match the stack values)
        mov ecx,[esp+8]             -- pTCB/4
        mov esi,[eax-4]             -- pRoot [+0b01, sometimes]
        and esi,#FFFFFFFE           -- clear 0b01, the free bit
--17/4/16:
        cmp esi,#800
        jbe :mc53
        cmp dword[esi],#00484253    -- dwMagic ("SBH\0")
--      jne :memorycorruption       -- (now checked in splitdone)
        je @f
--17/4/16:
            -- memory corruption at #HHHHHHHH(esi) (pGtcb*4=#HHHHHHHH, diff=#HHHHHHHH)
          ::mc53
--          mov edx,[esp+12]
--          mov al,53               -- e53mcat(esi,ecx)
--          sub edx,1
--          mov ecx,[pGtcb]
--          jmp :!iDiag
            push eax
--          push esi
            mov edi,[memory_corruption]         -- "memory corruption at #"
            call :%puts1
            mov edx,[esp]
            push 0                      -- no cr
            call :%puthex32
            mov edi,[pGtcb4eq]          -- ", pGtcb*4=#"
            call :%puts1
            mov edx,[pGtcb]
            shl edx,2
            push 0
            call :%puthex32
            mov edi,[diffis]            -- ", diff="
            call :%puts1
            mov ecx,[pGtcb]
            mov edx,[esp]
            shl ecx,2
            push 1
            sub edx,ecx
            call :%puthex32
--          pop esi
            pop eax

            int3
      @@:
        cmp edx,[esp]               -- rqd block size
        je :splitdone
        shr edx,1                   -- /2
        or esi,1                    -- set 0b01 free bit
        sub edi,4                   -- decrease (idx*4)
        js :idxlz
        mov [eax+edx-4],esi         -- pRoot, with 0b01 set
--pHeapD [NO!]
--!     mov esi,[esp+12]            -- era
        mov [eax+edx],edx           -- nSize
--!     mov [eax+edx-8],esi         -- era
        lea esi,[eax+edx]           -- esi:=address of block to put on freelist
        cmp [ecx*4+edi+20],ebx      -- check pFree[idx] is zero
        jne :pfreeidxnz             -- (if not, why we split bigger block?)
        mov [eax+edx+4],ebx         -- esi.pNext:=0 (w/o agi stall)
        mov [eax+edx+8],ebx         -- esi.pPrev:=0 ("")
        mov [ecx*4+edi+20],esi      -- pFree[idx](which was null):=2nd half
    [64]
        -- recap:
--afaik, rsp should be the same as just after the:
--      sub rsp,8*11    -- minimum shadow space plus align/temp stores:
--                      -- [rsp+0/8/16/24] shadow space
--                      -- [rsp+32] required size
--                      -- [rsp+40] lpCriticalSection
--                      -- [rsp+48] dwThreadId
--                      -- [rsp+56] idx*8
--                      -- [rsp+64] pTCB/4
--                      -- [rsp+72] rqd size, rounded up
--                      -- [rsp+80] era
        --      rax is an available block
        --      rdx is size of ""
        --      rdi is idx*8 of ""
        --      (obviously we may need to split rax until rdx/rdi match the stack values)
        mov rcx,[rsp+64]            -- pTCB/4
--      mov rsi,[rax-8]             -- pRoot [+0b01, sometimes]
        mov r8,[rax-8]              -- pRoot [+0b01, sometimes]
--DEV (spotted in passing) disassembles as rsi..., should be esi according to FDBG, should really be rsi...
--      and esi,#FFFFFFFE           -- clear 0b01, the free bit
--      and rsi,-2                  -- clear 0b01, the free bit
--DEV needs a #40 rex...
--      and sil,-2                  -- clear 0b01, the free bit
        and r8l, byte #FE           -- clear 0b01, the free bit
--DEV ditto
--      cmp dword[esi],#00484253    -- dwMagic ("SBH\0")
--      cmp dword[rsi],#00484253    -- dwMagic ("SBH\0")
        cmp dword[r8],#00484253     -- dwMagic ("SBH\0")
--      jne :memorycorruption       -- (now checked in splitdone)
        je @f
            int3
      @@:
        cmp rdx,[rsp+72]            -- rqd block size
        je :splitdone
        shr rdx,1                   -- /2
--      or rsi,1                    -- set 0b01 free bit
        or r8l,1                    -- set 0b01 free bit
        sub rdi,8                   -- decrease (idx*8)
        js :idxlz
--      mov [rax+rdx-8],rsi         -- pRoot, with 0b01 set
        mov [rax+rdx-8],r8          -- pRoot, with 0b01 set
--pHeapD: [NO!]
--!     mov r9,[rsp+80]             -- era
        mov [rax+rdx],rdx           -- nSize
--!     mov [rax+rdx-16],r9         -- era
        lea rsi,[rax+rdx]           -- rsi:=address of block to put on freelist
        cmp [rcx*4+rdi+32],rbx      -- check pFree[idx] is zero
        jne :pfreeidxnz             -- (if not, why we split bigger block?)
        mov [rax+rdx+8],rbx         -- rsi.pNext:=0 (w/o agi stall)
        mov [rax+rdx+16],rbx        -- rsi.pPrev:=0 ("")
        mov [rcx*4+rdi+32],rsi      -- pFree[idx](which was null):=2nd half
    []
        -- and carry on splitting as many times as required
        jmp :blockdetached

      ::splitdone
    [32]
--      and esi,#FFFFFFFE           -- clear 0b01, the free bit
--      add esp,12
        cmp dword[esi],#00484253    -- dwMagic ("SBH\0")
--      jne :memorycorruption
        je @f
            int3
      @@:
        mov ecx,[esp+12]            -- era (edi would be fine too, I think)
        add esp,16
        mov [eax-4],esi             -- pRoot (w/out the free bit set)
        mov [eax-8],ecx             -- era
        sub edx,8                   -- subtract the space used by pRoot + era
        -- result is edx bytes at eax
    [64]
--      mov r8,rsi
--      and r8l,byte #FE            -- clear 0b01, the free bit
--      add esp,12
        cmp dword[r8],#00484253     -- dwMagic ("SBH\0")
--      jne :memorycorruption
        je @f
            int3
      @@:
        mov r9,[rsp+80]             -- era
        add rsp,8*11
        mov [rax-8],r8              -- pRoot (w/out the free bit set)
        mov [rax-16],r9             -- era
        sub rdx,16                  -- subtract the space used by pRoot + era
        -- result is rdx bytes at rax
    []
        ret


--/*
procedure :%pFreePool(:%)
end procedure -- (for Edita/CtrlQ)
--*/
    :%pFreePool
---------------
    [32]
        -- release ecx bytes of memory at eax, using edx as <era>.
        -- (no specific result, trashes eax/ecx/edx/esi/edi)
--DEV (temp, see if we can get a consistent diff in edi)
mov edi,[pGtcb]
shl edi,2
sub edi,eax
        push edx
        mov esi,[eax-4]             -- pRoot
        xor ebx,ebx -- (save some grief)
--      and esi,#FFFFFFFE           -- clear free bit (needed when looping)
        test esi,#01
--      jnz :memorycorruption
        jz :!FreePoole32a
--6/8/19: real cause: free() when delete_routine() in force...
--??        jmp :mc53

--          pop edx
--                      mov edi,[esp+?]
            mov edi,edx             -- temp
            mov al,32               -- e32hc(era,edi)   --DEV show era/line number?
            jmp :!iDiag
            int3
      :!FreePoole32a
        cmp dword[esi],#00484253    -- dwMagic ("SBH\0")
--      jne :memorycorruption
        je @f
            int3
      @@:
        add ecx,8                   -- include size of padding
        mov edx,24
        mov edi,0
      @@:
        cmp edx,ecx
--      jle @f
        jge @f
            add edx,edx
            add edi,4
            jmp @b
      @@:
--      cmp edx,ecx
--      jne :memorycorruption
        je @f
            int3
      @@:
        cmp edi,108 -- (=27*4)
--      jae :memorycorruption
        jb @f
            int3
      @@:
    [64]
        -- release rcx bytes of memory at rax, using rdx as <era>.
        -- (no specific result, trashes rax/rcx/rdx/rsi/rdi/r8/r9/r10/r11)
        push rdx
        mov rsi,[rax-8]             -- pRoot
        xor rbx,rbx -- (save some grief)
        test rsi,#01
--      jnz :memorycorruption
        jz :!FreePoole32a
            int3
      :!FreePoole32a
        cmp dword[rsi],#00484253    -- dwMagic ("SBH\0")
--      jne :memorycorruption
        je @f
            int3
      @@:
        add rcx,16                  -- include size of padding
        mov rdx,44                  -- block size
        mov rdi,0                   -- idx*8
      @@:
        cmp rdx,rcx
        jge @f
            add rdx,rdx
            add rdi,8
            jmp @b
      @@:
--      jne :memorycorruption
        je @f
            int3
      @@:
        cmp rdi,464 -- (=58*8)
--      jae :memorycorruption
        jb @f
            int3
      @@:
    [32]
--erm? (added 30/5/14)
        mov [eax],ecx               -- nSize
        push eax                    -- [1] save
push ecx -- (does not actually help) [DEV]
    [64]
        mov [rax],rcx               -- nSize
        mov r12,rax                 -- [1] save
    []
        call :pGetThread            -- (eax:=dwThreadId; most regs trashed)
    [32]
pop ecx
        cmp eax,[esi+4]             -- sbh.dwThreadId
        mov edx,[esi+12]            -- pTCB/4
    [64]
        cmp eax,dword[rsi+4]        -- sbh.dwThreadId (dwMagic and dwThreadId are still dwords, btw)
        mov rdx,[rsi+16]            -- pTCB/4
    []
        jne @f
            -- owned
    [32]
            pop eax                 -- [1] restore
            push edx                -- pTCB/4
            push ebx                -- null (instead of mergelist's return address)
            push ecx                -- block size (cleared by mergeone)
            push edi                -- idx(*4)    (cleared by mergeone)
            push eax                -- block ptr  (cleared by mergeone)
            call :mergeone          --             (ends with a ret 12)
--          add esp,8
            add esp,12
    [64]
            mov rax,r12             -- [1] restore
            push rdx                -- pTCB/4
            push rbx                -- null (instead of mergelist's return address)
            push rcx                -- block size (cleared by mergeone)
            push rdi                -- idx(*8)    (cleared by mergeone)
            push rax                -- block ptr  (cleared by mergeone)
            call :mergeone          --             (ends with a ret 24)
--          add rsp,16
            add rsp,24
    []
            ret
      @@:
        -- not owned
    [32]
        cmp dword[ebx+edx*4],#00424354      -- magic ("TCB\0")
--      jne :memorycorruption
        je @f
            int3
      @@:
        lea esi,[ebx+edx*4+128]     -- locate pNofl (before edx gets damaged!)
        lea ecx,[ebx+edx*4+240]     -- csLock
        cmp dword[ebx+edx*4+236],#00424343  -- dwMagicC ("CCB\0")
--      jne :notCCB
        je @f
            int3
      @@:
-->     cmp dword[ecx+24],#00424345 -- dwMagicE ("ECB\0")
        cmp dword[ecx+32],#00424345 -- dwMagicE ("ECB\0")
        jne :notECB
        push ecx                                    -- lpCriticalSection (for Leave)
        call :EnterCriticalSection
    [64]
        cmp dword[rbx+rdx*4],#00424354      -- magic ("TCB\0")
--      jne :memorycorruption
        je @f
            int3
      @@:
        lea rsi,[rbx+rdx*4+496]     -- locate pNofl (before rdx gets damaged!)
        lea rcx,[rbx+rdx*4+968]     -- csLock
--23/11/16!
--      cmp dword[rbx+rdx*4-960],#00424342  -- dwMagicB ("BCB\0")
        cmp dword[rbx+rdx*4+960],#00424342  -- dwMagicB ("BCB\0")
--      jne :notCCB
        je @f
            int3
      @@:
        cmp dword[rcx-4],#00424343  -- dwMagicC ("CCB\0")
--      jne :notCCB
        je @f
            int3
      @@:
-->     cmp dword[rcx+40],#00424345 -- dwMagicE ("ECB\0")
        cmp dword[rcx+56],#00424345 -- dwMagicE ("ECB\0")
        jne :notECB
        mov [rsp+32],rcx            -- save lpCriticalSection (for Leave)
        call :EnterCriticalSection
    [32]
        mov eax,[esp+4]             -- [1] restore
        mov edx,[esi+edi]           -- pNofl[idx] (previous)
        mov [esi+edi],eax           -- pNofl[idx] (new)
--13/8/15 no!!
--      mov ecx,[eax-4]             -- pRoot
        mov [eax+4],edx             -- pNext
--      or ecx,1
        mov [eax+8],ebx             -- pPrev (null, probably unnecessary)
--      mov [eax-4],ecx             -- pRoot (with bit 0b01 set)
--      mov [eax-8],ebx             -- era?
        pop ecx                     -- lpCriticalSection/csLock
        call :LeaveCriticalSection
        add esp,8                   -- [1] discard (and era)
    [64]
        mov rax,r12                 -- [1] restore
        mov rdx,[rsi+rdi]           -- pNofl[idx] (previous)
        mov [rsi+rdi],rax           -- pNofl[idx] (new)
--13/8/15 no!!
--      mov rcx,[rax-8]             -- pRoot
        mov [rax+8],rdx             -- pNext
--      or rcx,1
        mov [rax+16],rbx            -- pPrev (null, probably unnecessary)
--      mov [rax-8],rcx             -- pRoot (with bit 0b01 set)
--      mov [rax-16],rbx            -- era?
        mov rcx,[rsp+32]            -- lpCriticalSection/csLock
        call :LeaveCriticalSection
        add rsp,8                   -- discard era
    []
        ret

--/*
procedure :%pAllocStr(:%)
end procedure -- (for Edita/CtrlQ)
--*/
    :%pAllocStr
---------------
    [32]
        -- Allocate a string of length ecx.
        -- Sets the length, type/refcount, and returns a ref in eax.
        -- All other registers (except eax) are preserved.
        test ecx,ecx
--      js e101atasonl                    ; attempt to allocate string of negative length
--      js :invalidmemoryrequest
        jns @f
            int3
      @@:
        pushad
        add ecx,17                      -- header plus terminating null
        xor ebx,ebx                     -- (save some grief)
        mov edx,[esp+32]                -- era
        call :%pGetPool                 -- allocate ecx bytes, rounded up
        test eax,eax
--      jz :memoryallocationfailure
        jnz @f
            mov edx,[esp+32]                -- era
            mov al,33   -- e33maf
            sub edx,1
            jmp :!iDiag
            int3
      @@:
        mov ecx,[esp+24]                -- retrieve original length (ecx from the pushad)
        mov [eax],edx                   -- set maxlen
        lea edx,[eax+17]                --      ref: raw plus header & type bit,
        mov [eax+4],ecx                 -- set length
        ror edx,2                       --      rotated,
        mov dword[eax+8],1              -- refcount
        mov [esp+28],edx                --      keep new ref (in eax),
        mov dword[eax+12],0x82000000    -- type
        popad                           --      but restore all other registers
    [64]
        -- Allocate a string of length rcx.
        -- Sets the length, type/refcount, and returns a ref in rax.
        -- All other registers (except rax) are preserved. [DEV/ERM/may need yet more push/pop...]
        test rcx,rcx
--      js e101atasonl                    ; attempt to allocate string of negative length
--      js :invalidmemoryrequest
        jns @f
            int3
      @@:
--      pushad
        sub rsp,8                       -- align stack
        xor rbx,rbx                     -- (save some grief)
        push rsi                        -- [1] save
        push rdi                        -- [2] save
        push rdx                        -- [3] save
        push rcx                        -- [4] save
        add rcx,33                      -- header plus terminating null
        mov rdx,[rsp+40]                -- era
        call :%pGetPool                 -- allocate ecx bytes, rounded up
        test rax,rax
--      jz :memoryallocationfailure
        jnz @f
            int3
      @@:
        mov rcx,[rsp]                   -- [4] retrieve original length
        mov [rax],rdx                   -- set maxlen
--<     lea rdx,[rax+33]                --      ref: raw plus header & type bit,
        mov [rax+8],rcx                 -- set length
--<     ror rdx,2                       --      rotated,
        mov qword[rax+16],1             -- refcount
        mov dword[rax+24],ebx           -- type loword
        mov dword[rax+28],0x82000000    -- type
--      popad                           --      but restore all other registers
--<     mov rax,rdx
        mov rdx,[rsp+8]                 -- [3] restore
        mov rdi,[rsp+16]                -- [2] restore
        mov rsi,[rsp+24]                -- [1] restore
        lea rax,[rax+33]                -- ref: raw plus header & type bit,
        add rsp,5*8
        ror rax,2                       -- rotated,
    []
        ret

    :%pWithJS
-------------
        --
        --  mov e/rax flag  (1 for without js, 2 for with js, 3 to reset)
        --  call :%opWithJS
        --  e/rcx contains prev, should you need it (mov to e/rax and re-call to preserve)
        --  e/rdx trashed
        --
    [32]
--      and eax,1
--      mov ecx,[withjs]
--      mov edx,[esp]
        mov [withjs],eax
--      sub edx,1
--      test eax,ecx
    [64]
--      and rax,1
--      mov rcx,[withjs]
--      mov rdx,[rsp]
        mov [withjs],rax
--      sub rdx,1
--      test rax,rcx
    []
--      jnz @f
--          mov al,29       -- e29wojsc
--          jmp :!iDiag
--          int3
--    @@:
        ret

    :%pAlloClone
----------------
    --
    -- About to clone a sequence because its refcount!=1.
    -- If withjs is in force, terminate in error.
    -- e/rdx should be set to era on entry, all registers preserved
    --  
--DEV ... (fixes needed in pilasm)
--      test [withjs],1
    [32]
        mov eax,[withjs]
        test eax,1
    [64]
        mov rax,[withjs]
        test rax,1
    []
--</dev>
        jz @f
            mov al,56           -- e56rocow, "p2js violation: relies on copy on write semantics"
    [32]
            sub edx,1
            mov [withjs],ebx
    [64]
            sub rdx,1
            mov [withjs],rbx
    []
            jmp :!iDiag
            int3
      @@:       
--*/
        ret


--/*
procedure :%pAllocSeq(:%)
end procedure -- (for Edita/CtrlQ)
--*/
    :%pAllocSeq
---------------
    [32]
        -- Allocate a sequence of length ecx.
        --  on entry, era in edx
        -- Sets the length, type/refcount, and returns a ref in eax.
        -- All other registers (except eax) are preserved.
        test ecx,ecx
--      js e101atasonl                  ; attempt to allocate string of negative length
--      js :invalidmemoryrequest
        jns @f
            int3
      @@:
        pushad
        xor ebx,ebx                     -- (save some grief)
        lea ecx,[ebx+ecx*4+20]          -- 4 bytes per element plus header
--      mov edx,[esp+32]                -- era
        call :%pGetPool                 -- allocate ecx bytes, rounded up
        test eax,eax
--      jz :memoryallocationfailure
        jnz @f
--          mov edx,[esp+32]                -- era
            mov edx,[esp+20]                -- era (was in edx)
            mov al,33   -- e33maf
            sub edx,1
            jmp :!iDiag
            int3
      @@:
        mov [eax],ebx                   -- set slack (=0)
        mov ecx,[esp+24]                -- retrieve original length (ecx from the pushad)
        mov [eax+4],edx                 -- set maxlen (= allocated size)
        lea edi,[eax+21]                --      ref: raw plus header & type bit,
        mov [eax+8],ecx                 -- set length
        ror edi,2                       --      rotated,
        mov dword[eax+12],1             -- refcount
        mov [esp+28],edi                --      keep new ref (in eax),
        mov dword[eax+16],0x80000000    -- type
        popad                           --      but restore all other registers
    [64]
        -- Allocate a sequence of length rcx.
        --  on entry, era in rdx
        -- Sets the length, type/refcount, and returns a ref in rax.
        -- All other registers (except rax) are preserved. [DEV/ERM...]
        test rcx,rcx
--      js e101atasonl                  ; attempt to allocate string of negative length
--      js :invalidmemoryrequest
        jns @f
            int3
      @@:
--      pushad
--28/12/15:
--      sub rsp,8                       -- align stack
        push r9                         -- [0] save
        xor rbx,rbx                     -- (save some grief)
        push rsi                        -- [1] save
        push rdi                        -- [2] save
        push rdx                        -- [3] save
        push rcx                        -- [4] save
--      lea rcx,[rbx+rcx*8+40]          -- 8 bytes per element plus header
        shl rcx,3                       -- 8 bytes per element
--      mov rdx,[rsp+40]                -- era
        add rcx,40                      -- plus header
        call :%pGetPool                 -- allocate rcx bytes, rounded up
        test rax,rax
--      jz :memoryallocationfailure
        jnz @f
            int3
      @@:
        mov [rax],rbx                   -- set slack (=0)
        mov rcx,[rsp]                   -- [4] retrieve original length
        mov [rax+8],rdx                 -- set maxlen (= allocated size)
--      lea rdi,[rax+41]                --      ref: raw plus header & type bit,
        mov [rax+16],rcx                -- set length
--      ror rdi,2                       --      rotated,
        mov qword[rax+24],1             -- refcount
--      mov [esp+28],edi                --      keep new ref (in eax),
        mov dword[rax+32],ebx           -- type loword
        mov dword[rax+36],0x80000000    -- type
--      popad                           --      but restore all other registers
        mov rdx,[rsp+8]                 -- [3] restore
        mov rdi,[rsp+16]                -- [2] restore
        mov rsi,[rsp+24]                -- [1] restore
        mov r9,[rsp+32]                 -- [0] restore
        lea rax,[rax+41]                -- ref: raw plus header & type bit,
        add rsp,5*8
        ror rax,2                       -- rotated,
    []
        ret

--/*
procedure :%pLoadFlt(:%)
end procedure -- (for Edita/CtrlQ)
--*/
    :%pLoadFlt
---------------
    [32]
        -- Load [esi] into st0, whether integer or floating-point.
        -- NB no checks are made to verify [esi] is an atom.
        -- eax is damaged, all other registers are preserved.
        mov eax,[esi]
        cmp eax,h4
        jl @f
            fld qword[ebx+eax*4]
            ret
      @@:
        fild dword[esi]
    [64]
        -- Load [rsi] into st0, whether integer or floating-point.
        -- NB no checks are made to verify [rsi] is an atom.
        -- rax is damaged, r15 set ot h4, all other registers are preserved.
        mov rax,[rsi]
        mov r15,h4
        cmp rax,r15
        jl @f
            fld tbyte[rbx+rax*4]
            ret
      @@:
        fild qword[rsi]
    []
        ret

--/*
procedure :%pStoreFlt(:%)
end procedure -- (for Edita/CtrlQ)
--*/
    :%pStoreFlt
---------------
    [32]
        -- Store st0 in [edi], as integer if possible.
        -- Deallocates previous contents of [edi] if required.
        -- All registers are preserved.
        pushad
        xor ebx,ebx -- (save some grief)
        -- Force round to 64 bits, since that is how we store floats.
        sub esp,8
        fstp qword[esp]     -- store as 64-bit float/pop (2 cycle)
        fld qword[esp]      -- reload                    (1 cycle)
        fist dword[esp]     -- store as 32-bit int
        mov edx,[edi]       -- prev contents
        fild dword[esp]
        mov ecx,[esp]
        add esp,8
        fcomp st1           -- pop one
        fnstsw ax
--DEV Agner sez (look this up!) test ax,0x40 jz StoreFlt64 (nb test inverted)
        sahf
        jne :StoreFlt64     -- not integer
        mov eax,ecx
        shl ecx,1
        jo :StoreFlt64      -- result bigger than 31 bits
        fstp st0            -- discard copy
        mov [edi],eax
        cmp edx,h4
        jle @f
            sub dword[ebx+edx*4-8],1
            jne @f
--          push dword[esp+40]
            push dword[esp+32]  -- era
            call :%pDealloc0
      @@:
        popad
    [64]
        -- Store st0 in [rdi], as integer if possible.
        -- Deallocates previous contents of [rdi] if required.
        -- All registers are preserved. [DEV? what about :%pDealloc? :%pGetPool?]
        -- Preserves rax/rcx/rdx/rsi (see StoreMint) and rdi
        -- NB: is now expected to leave r15 set to h4 (pTime.e)
        push rdx
        push rcx
        push rax
        push rsi
        mov r15,h4
        xor rbx,rbx -- (save some grief)
        sub rsp,16
--      fist qword[rsp]     -- store as 64-bit int (instruction not supported [by the/any hardware!])
        fld st0
        fistp qword[rsp]    -- store as 64-bit int
        mov rdx,[rdi]       -- prev contents
        fild qword[rsp]
        mov rcx,[rsp]
        add rsp,16
        fcomp st1           -- pop one
        fnstsw ax
--DEV Agner sez (look this up!) test ax,0x40 jz StoreFlt64 (nb test inverted)
        sahf
        jne :StoreFlt64     -- not integer
        mov rax,rcx
        shl rcx,1
        jo :StoreFlt64      -- result bigger than 31 bits
        fstp st0            -- discard copy
        mov [rdi],rax
--      cmp rdx,h4
--      mov r15,h4
        cmp rdx,r15
        jle @f
--6/11/16:
--          sub qword[ebx+edx*4-16],1
            sub qword[rbx+rdx*4-16],1
            jne @f
            push rdi            -- save
            push qword[rsp+40]  -- era
            call :%pDealloc0
            pop rdi             -- restore
      @@:
        pop rsi
        pop rax
        pop rcx
        pop rdx
    []
        ret

      ::StoreFlt64
    [32]
        cmp edx,h4
        jle :SFNewFlt
        mov ecx,[ebx+edx*4-8]           -- refcount
        cmp byte[ebx+edx*4-1],0x12
        jne @f
            sub ecx,1
            jne :SFnodealloc                -- reuse if refcount is exactly 1
            fstp qword[ebx+edx*4]
            popad
            ret
      @@:
        sub ecx,1
        jne :SFnodealloc
            push dword[esp+32]
            call :%pDealloc0
            jmp :SFNewFlt
      ::SFnodealloc
        mov [ebx+edx*4-8],ecx           -- store decremented refcount (still>0)
      ::SFNewFlt
        mov ecx,16
        mov edx,[esp+32]                -- era
        call :%pGetPool                 -- allocate ecx bytes
        test eax,eax
--      jz :memoryallocationfailure
        jnz @f
            popad
            pop edx
            mov al,33   -- e33maf
            sub edx,1
            jmp :!iDiag
            int3
      @@:
        mov edi,[esp]                   -- restore original edi (pGetPool trashes all regs)
        lea edx,[eax+9]                 --      ref: raw plus header & type bit,
        mov dword[eax],1                -- refcount
        ror edx,2                       --      rotated,
        mov dword[eax+4],0x12000000     -- type
        mov [edi],edx                   --      store ref in [edi]
        fstp qword[eax+8]               -- value
        popad

    [64]
--      cmp rdx,h4
--      mov r15,h4
        push rdi
        cmp rdx,r15
        jle :SFNewFlt
        mov rcx,[rbx+rdx*4-16]          -- refcount
        cmp byte[rbx+rdx*4-1],0x12
        jne @f
            sub rcx,1
            jne :SFnodealloc                -- reuse if refcount is exactly 1
            fstp tbyte[rbx+rdx*4]
            pop rdi
            jmp @b  -- (pop pop pop ret)

      @@:
        sub rcx,1
        jne :SFnodealloc
            push qword[rsp+40]
            call :%pDealloc0
            jmp :SFNewFlt
      ::SFnodealloc
        mov [rbx+rdx*4-16],rcx          -- store decremented refcount (still>0)
      ::SFNewFlt
--      mov rcx,26
        mov rcx,28
        mov rdx,[rsp+40]                -- era
        call :%pGetPool                 -- allocate rcx bytes
        test rax,rax
--      jz :memoryallocationfailure
        jnz @f
            mov rdx,[rsp+40]
            mov al,33   -- e33maf
            sub rdx,1
            jmp :!iDiag
            int3
      @@:
--      mov rdi,[rsp]                   -- restore original rdi (pGetPool trashes all regs) [DEV]
        pop rdi
        lea rdx,[rax+17]                --      ref: raw plus header & type bit,
        mov qword[rax],1                -- refcount
        ror rdx,2                       --      rotated,
        mov qword[rax+8],rbx            -- type/delete_rtn
        mov [rdi],rdx                   --      store ref in [rdi]
        mov byte[rax+15],0x12           -- type
        fstp tbyte[rax+16]              -- value
        pop rsi
        pop rax
        pop rcx
        pop rdx
    []
        ret

--/*
procedure :%pLoadMint(:%)
end procedure -- (for Edita/CtrlQ)
--*/
    :%pLoadMint -- finish loading a machine-sized (32/64-bit) integer
---------------
--DEV era in edx? error code in ecx?
    [32]
        -- eax:=(int32)eax, edx:=hi_dword
        -- All other registers are preserved (ebx:=0).
        xor ebx,ebx         -- (save some grief)
        cmp eax,h4
        jle @f
            cmp byte[ebx+eax*4-1],#12
            je :LoadMintFlt
                --DEV or should we use the carry flag??
                pop edx
                mov al,48               -- e48atlmmba
                sub edx,1
                jmp :!iDiag
                int3
          ::LoadMintFlt
            sub esp,8
            fld qword[ebx+eax*4]
--DEV try fisttp?
            call :%down53
            fistp qword[esp]
            call :%near53
            mov eax,[esp]
            mov edx,[esp+4]
            add esp,8
      @@:
        ret
    [64]
        -- rax:=(int64)rax
        -- All other registers are preserved (rbx:=0).
        mov r15,h4
        xor rbx,rbx         -- (save some grief)
        cmp rax,r15
        jle @f
            cmp byte[rbx+rax*4-1],#12
            je :LoadMintFlt
                pop rdx
                mov al,48               -- e48atlmmba
                sub rdx,1
                jmp :!iDiag
                int3
          ::LoadMintFlt
--removed 17/1/17:
--          sub rsp,8
            fld tbyte[rbx+rax*4]

--12/01/17:
            -- if uint>#7FFF... then uint-=#1_0000...
            push r15            -- #4000_0000_0000_0000
            fild qword[rsp]
            fadd st0,st0        -- #8000_0000_0000_0000
            fld st1
            fcomp
            fnstsw ax
            sahf
            jb :below80
                fadd st0,st0    -- #1_0000_0000_0000_0000
                fsub st1,st0
          ::below80
            fstp st0            -- discard

--DEV try fisttp?
            call :%down64
            fistp qword[rsp]
            call :%near64
            pop rax
      @@:
        ret
    []

--/*
procedure :%pStoreMint(:%)
end procedure -- (for Edita/CtrlQ)
--*/
    :%pStoreMint    -- store a machine-sized (32/64-bit) integer
----------------
    [32]
        -- Store eax in [edi], as integer if possible, else as float.
        -- Deallocates previous contents of [edi] if required.
        -- All registers are preserved.
        pushad
        mov ecx,eax
        mov edx,[edi]       -- prev contents
        xor ebx,ebx         -- (save some grief)
        shl ecx,1
        jno @f
            fild dword[esp+28]      -- (ie the eax from pushad)
            jmp :StoreFlt64         -- (will dealloc [edi] if rqd)
      @@:
        mov [edi],eax
        cmp edx,h4
        jle @f
            sub dword[ebx+edx*4-8],1
            jne @f
            push dword[esp+32]
            call :%pDealloc0
      @@:
        popad
    [64]
        -- Store rax in [rdi], as integer if possible, else as float.
        -- Deallocates previous contents of [rdi] if required.
        -- Preserves rax/rcx/rdx/rsi (see StoreFlt) and rdi.
        push rdx
        push rcx
        push rax
        push rsi
        xor rbx,rbx -- (save some grief)
        mov rcx,rax
        mov rdx,[rdi]       -- prev contents
        shl rcx,1
        jno @f
            fild qword[rsp+8]   -- (saved rax)
            jmp :StoreFlt64
      @@:
        mov r15,h4
        mov [rdi],rax
        cmp rdx,r15
        jle @f
            sub qword[rbx+rdx*4-16],1
            jne @f
            push rdi
            push qword[rsp+40]
            call :%pDealloc0
            pop rdi
      @@:
        pop rsi
        pop rax
        pop rcx
        pop rdx
    []
        ret

--/*
procedure :%pDealloc[0](:%)
end procedure -- (for Edita/CtrlQ)
--*/
    :%pDealloc          -- (era==[e/rsp])
--------------
        --
        -- called when a refcount [ebx+edx*4-8] drops to zero.
        -- Note the refcount itself need not actually be 0 when this is called,
        --      ie "sub reg,1; jz deallocX; mov[ebx+edx*4-8],reg" is permitted.
        -- Invoke opDealloc iff era==[esp], ie when inlined and all those final
        --  jmp/jz from opcodes, otherwise push the era and invoke opDealloc0.
        -- All registers are trashed (except for esp and ebx[==0 on entry/exit]).
        --
    [32]
        push dword[esp] -- (era, discarded on exit)
    :%pDealloc0         -- (era @ [esp+4])
    -----------
        shl edx,2
        xor ebx,ebx -- (save some grief)
        test dword[edx-4],0x00FFFFFF    -- delete_routine in low 3 bytes of type
        jz @f
            push edx
            lea eax,[edx+1]
            -- put a refcount of 1 back (normally but not necessarily 0 by now)
            mov dword[edx-8],1
--          mov dword[edx-8],2
--          mov eax,[pDelRtn]
            -- reconstruct the ref
--          add edx,1
--      mov eax,[DelRtn]            ; see builtins/pdelete.e/deletef()
--          shl eax,2
--          ror edx,2
            ror eax,2
--      mov [DelRef],edx
--      call eax
--DEV:
--          call :%DelRtn
--          call [pDelRtn]
--          call eax
            call :%opDelete
--        ::pDealloc12
            pop edx
            mov dword[edx-8],ebx        -- and re-zero the refcount
    [64]
        push qword[rsp]
    :%pDealloc0         -- (era @ [rsp+8])
    -----------
        lea rax,[rbx-1]                         -- #FFFFFFFFFFFFFFFF, instead of...
        shl rdx,2
        shr rax,8                               -- #00FFFFFFFFFFFFFF (which, being 56 bits, we struggle to handle on 32-bits) [DEV]
        xor rbx,rbx -- (save some grief)
--      test qword[rdx-8],0x00FFFFFFFFFFFFFF    -- delete_routine in low 7 bytes of type?
        test qword[rdx-8],rax                   -- delete_routine in low 7 bytes of type?
        jz @f
            push rdx
            lea rax,[rdx+1]
            -- put a refcount of 1 back (normally but not necessarily 0 by now)
            mov qword[rdx-16],1
--          mov qword[rdx-16],2
--          mov rax,[pDelRtn]
            -- reconstruct the ref
--          add rdx,1
--      mov eax,[DelRtn]            ; see builtins/pdelete.e/deletef()
--          shl rax,2
            ror rax,2
--      mov [DelRef],edx
--      call eax
--DEV:
--          call :%DelRtn
--          call rax
            call :%opDelete
--        ::pDealloc24
            pop rdx
            mov qword[rdx-16],rbx       -- and re-zero the refcount
    []
      @@:
    [32]
        cmp byte[edx-1],0x80
        jae @f
            -- float
--          if debug
                cmp byte[edx-1],0x12
                jne :deallocerrunknowntype
--          end if
--          mov eax,edx
            lea eax,[edx-8]
            mov ecx,16
            mov edx,[esp+4]         -- era
            call :%pFreePool        -- free ecx bytes at eax
--          ret
            ret 4
    [64]
        cmp byte[rdx-1],0x80
        jae @f
            -- float
--          if debug
                cmp byte[rdx-1],0x12
                jne :deallocerrunknowntype
--          end if
--          mov rax,rdx
            lea rax,[rdx-16]
--          mov rcx,26
            mov rcx,28
--          jmp :%pFreePool     -- free rcx bytes at rax
            mov rdx,[rsp+8]
            call :%pFreePool    -- free rcx bytes at rax
            ret 8
    []
      @@:
        je @f
            -- string
    [32]
--          if debug
                cmp byte[edx-1],0x82
                jne :deallocerrunknowntype
--          end if
            mov ecx,[edx-16]        -- maxlen in bytes
            lea eax,[edx-16]        -- start of allocated memory (address of maxlen)
            mov edx,[esp+4]         -- era
            call :%pFreePool        -- free ecx bytes at eax
          ::dret
--          ret
            ret 4
    [64]
--          if debug
                cmp byte[rdx-1],0x82
                jne :deallocerrunknowntype
--          end if
            mov rcx,[rdx-32]        -- maxlen in bytes
            lea rax,[rdx-32]        -- start of allocated memory (address of maxlen)
--          jmp :%pFreePool         -- free rcx bytes at rax
            mov rdx,[rsp+8]         -- era
            call :%pFreePool        -- free rcx bytes at rax
          ::dret
--          ret
            ret 8
    []
      @@:
            -- sequence
    [32]
--          if debug
--              cmp byte[edx-1],0x80        -- (must be to get here)
--              jne :deallocerrunknowntype
--          end if
            mov dword[edx-8],0          -- clear refcount (just in case/see note above/tested to terminate loop)
         ::dseqloop
            mov ecx,[edx-12]            -- length in dwords
         ::dseqresetbase
            mov esi,edx                 -- start with base ptr, ie s[1]
         ::dseqelementloop
            sub ecx,1
    [64]
--          if debug
--              cmp byte[rdx-1],0x80        -- (must be to get here)
--              jne :deallocerrunknowntype
--          end if
            mov qword[rdx-16],0         -- clear refcount (just in case/see note above/tested to terminate loop)
         ::dseqloop
            mov rcx,[rdx-24]            -- length in qwords
            shl rcx,3                   -- length in bytes
         ::dseqresetbase
            mov rsi,rdx                 -- start with base ptr, ie s[1]
         ::dseqelementloop
            sub rcx,8
    []
            jge @f
    [32]
                mov ecx,[edx-16]        -- maxlen in bytes
                mov esi,[edx-8]         -- restore raw address (parent) from refcount
                mov edi,[edx-4]         -- restore remaining items (parent) from type 
                lea eax,[edx-20]        -- start of allocated memory (address of slack)
                sub eax,[edx-20]        -- account for any slack
                pushad
                mov edx,[esp+36]        -- era
                call :%pFreePool        -- free ecx bytes at eax
                popad
                test esi,esi            -- if no parent quit
                jz :dret
                mov edx,esi             -- raw address
                mov ecx,edi             -- remaining items
                jmp :dseqresetbase
    [64]
                mov rcx,[rdx-32]        -- maxlen in bytes
                mov rsi,[rdx-16]        -- restore raw address (parent) from refcount
                mov rdi,[rdx-8]         -- restore remaining items (parent) from type 
                lea rax,[rdx-40]        -- start of allocated memory (address of slack)
                sub rax,[rdx-40]        -- account for any slack
--              pushad
                push rdx    -- (align)
                push rsi
                push rdi
                mov rdx,[rsp+32]        -- era
                call :%pFreePool        -- free rcx bytes at rax
--              popad
                pop rdi
                pop rsi
                pop rdx
                test rsi,rsi            -- if no parent quit
                jz :dret
                mov rdx,rsi             -- raw address
                mov rcx,rdi             -- remaining items
                jmp :dseqresetbase
    []
          @@:
    [32]
            mov edi,[esi+ecx*4]
            xor eax,eax                 -- for use as base (lessens AGI to boot!)
            cmp edi,h4  
            jl :dseqelementloop
            sub dword[eax+edi*4-8],1    -- decrement refcount
            lea eax,[eax+edi*4]
            jnz :dseqelementloop
            test dword[eax-4],0x00FFFFFF
    [64]
            mov rdi,[rsi+rcx]
            xor rax,rax                 -- for use as base (lessens AGI to boot!)
            lea r14,[rbx-1]             -- #FFFFFFFFFFFFFFFF, instead of...
--          cmp rdi,h4  
            mov r15,h4
            shr r14,8                   -- #00FFFFFFFFFFFFFF (which, being 56 bits, we struggle to handle on 32-bits) [DEV]
            cmp rdi,r15
            jl :dseqelementloop
            sub qword[rax+rdi*4-16],1   -- decrement refcount
            lea rax,[rax+rdi*4]
            jnz :dseqelementloop
--          test dword[eax-4],0x00FFFFFF
--[DEV check the listing:]
-->         test qword[rax-8],-256  -- delete_routine in low 7 bytes of type 
            test qword[rax-8],r14       -- delete_routine in low 7 bytes of type?
    []
            jz :nodeleteroutine
    [32]
                pushad
                mov dword[eax-8],1      -- put a refcount of 1 back
--              mov dword[eax-8],2      -- put a refcount of 2 back
--              mov ecx,[pDelRtn]
                add eax,1               -- reconstruct the ref
--      mov edx,[DelRtn]
--              shl ecx,2
                ror eax,2
--      mov [DelRef],eax
--      call edx
--              mov edx,eax
--DEV
--              call :%DelRtn
--              call ecx
                call :%opDelete
--            ::pDealloc40
                popad
                mov dword[eax-8],0
    [64]
                push rax
                push rcx
                push rdx
                push rsi
                --rsi?
                mov qword[rax-16],1     -- puts a refcount of 1 back
--              mov qword[rax-16],2     -- puts a refcount of 1 back
--              mov rcx,[pDelRtn]
                add rax,1               -- reconstruct the ref
--      mov rdx,[DelRtn]            ; see builtins/pdelete.e/deletef()
--              shl rcx,2
                ror rax,2
--      mov [DelRef],eax
--      call edx
--DEV:
--              call :%DelRtn
--              mov rdx,rax
--              call rcx
                call :%opDelete
--            ::pDealloc24a
                pop rsi
                pop rdx
                pop rcx
                pop rax
--DEV++??
                mov qword[rax-16],rbx       -- and re-zero the refcount
    []
          ::nodeleteroutine
    [32]
            cmp byte[eax-1],0x80
    [64]
            cmp byte[rax-1],0x80
    []
            jnb @f
            -- float
    [32]
--              if debug
                    cmp byte[eax-1],0x12
                    jne :deallocerrunknowntype
--              end if
                pushad
                sub eax,8
                mov ecx,16
                mov edx,[esp+36]        -- era
                call :%pFreePool        -- free ecx bytes at eax
                popad
    [64]
--              if debug
                    cmp byte[rax-1],0x12
                    jne :deallocerrunknowntype
--              end if
--              pushad
                push rcx
                push rdx
                push rsi
                sub rax,16
--              mov rcx,26
                mov rcx,28
                mov rdx,[rsp+32]        -- era
                call :%pFreePool        -- free rcx bytes at rax
--              popad
                pop rsi
                pop rdx
                pop rcx
    []
                jmp :dseqelementloop
          @@:
    [32]
                mov edi,[eax-12]        -- length
    [64]
                mov rdi,[rax-24]        -- length
    []
                je @f
                    -- string
    [32]
--                  if debug
                        cmp byte[eax-1],0x82
                        jne :deallocerrunknowntype
--                  end if
                    pushad
--  push edx
--  push ecx
                    mov ecx,[eax-16]
                    sub eax,16
                    mov edx,[esp+36]    -- era
                    call :%pFreePool    -- free ecx bytes at eax
                    popad
--  pop ecx
--  pop edx
    [64]
--                  if debug
                        cmp byte[rax-1],0x82
                        jne :deallocerrunknowntype
--                  end if
--                  pushad
                    push rcx
                    push rdx
                    push rsi
--17/12/14:
--                  mov ecx,[eax-16]
--                  sub eax,16
                    mov rcx,[rax-32]
                    sub rax,32
                    mov rdx,[rsp+32]    -- era
                    call :%pFreePool    -- free rcx bytes at rax
--                  popad
                    pop rsi
                    pop rdx
                    pop rcx
    []
                    jmp :dseqelementloop
          @@:
                -- sequence
    [32]
--              if debug
--                  cmp byte[eax-1],0x80        -- (must be to get here)
--                  jne :deallocerrunknowntype
--              end if
                mov [eax-8],edx     -- save raw (parent) in refcount (otherwise zero)
                mov [eax-4],ecx     -- save remaining items in type (otherwise seq, ie #80)
                mov edx,eax         -- new raw (child) address
    [64]
--              if debug
--                  cmp byte[rax-1],0x80        -- (must be to get here)
--                  jne :deallocerrunknowntype
--              end if
                mov [rax-16],rdx    -- save raw (parent) in refcount (otherwise zero)
                mov [rax-8],rcx     -- save remaining items in type (otherwise seq, ie #80)
                mov rdx,rax         -- new raw (child) address
    []
                jmp :dseqloop

--if debug
--deallocerrunknowntype:
--  call puthex32
--  mov esi,deallocunknowntype
--  call putstr
--  jmp showerror
--end if

--;;DEV trigger p.err creation...
--if used HeapFail
--HeapFail:
--  mov esi,Heaperror
--  call putstr
--;;int3
--; invoke ExitProcess,1
--end if
--
--;;DEV remove this?:
--showerror:
--  call showcr
--  call showcr
--  mov esi,pressenter
--  call putstr
--  call showcr
--  call wait_key
--  call opFreeCons
--;DEV??? (if close() is now in hll)
--  call opClosem9
--  invoke ExitProcess,1

--!*/

--DEV allocate_data() etc might be an issue..
--DEV/SUG put guards on all allocate and free, reduce limits accordingly. (NO: pRoot should be enough!)
--/*
procedure :%pAlloc(:%)
end procedure -- (for Edita/CtrlQ)
--*/
    :%pAlloc
------------
--DEV linked list: nSize (4/8) -> nSize,pNext,pPrev (12/24), and matching changes to limits (pHeapA.e anyone?).
--  pAlloc needs:
--      enter_cs(0)     -- (if >1 tcb exists?)
--      this.next = pAhead
--      this.prev = null
--      pAhead = this
--      leave_cs(0)     -- ("", saved)
--  pFree needs:
--      enter_cs(0)     -- (if >1 tcb exists?)
--      if this.prev==null then
--          pAhead:=this.next
--      else
--          this.prev.next:=this.next
--      end if
--      if this.next!==null then
--          this.next.prev:=this.prev
--      end if
--      leave_cs(0)     -- ("", saved)
    [32]
        -- size in ecx, result addr in edi
        xor ebx,ebx -- (save some grief)
--/* now the responsibility of builtins/pAlloc.e:
        cmp ecx,h4  --DEV :%pLoadMint?
        jl @f
            cmp byte[ebx+ecx*4-1],0x12
            jne :e37atambpi  -- argument to allocate must be positive integer
            sub esp,8
            fld qword[ebx+ecx*4]
            call :%down53
            fistp qword[esp]
            call :%near53
            mov ecx,[esp]
            add esp,8
      @@:
        test ecx,ecx
--      jle :e37atambpi     -- argument to allocate must be positive integer
--      jg @f
        jge @f              -- 25/10/15 allocate(0) should be allowed
          ::e37atambpi      -- argument to allocate must be positive integer
            pop edx
            mov al,37       -- e37atambpi
            sub edx,1
            jmp :!iDiag
            int3
      @@:
--*/
        -- calling convention
        -- mov ecx,[size]   -- (checked to be integer>=0)
        -- lea edi,[res]
        -- mov edx,[ebp+12] -- era/called from address
        -- call :%pAlloc
        add ecx,4           -- for nSize
--      mov edx,[esp]       -- era[DEV]
        push edi
        call :%pGetPool     -- (trashes all registers)
        -- result is edx bytes at eax, but we use first 4 bytes to save the size, for free().
        test eax,eax
        jz @f
            mov [eax],edx   -- so this is 16 when returning 12 bytes of useable space (at eax+4).
            add eax,4       -- for nSize (see also "Minor point" above)
      @@:
        pop edi
        push ebx
        push eax
        fild qword[esp]
        add esp,8
        jmp :%pStoreFlt     -- [edi]:=st0
    [64]
        -- size in rcx, result addr in rdi
        xor rbx,rbx -- (save some grief)
--/* now the responsibility of builtins/pAlloc.e:
        mov r15,h4
        push rdi
        cmp rcx,r15
        jl @f
            cmp byte[rbx+rcx*4-1],0x12
            jne :e37atambpi  -- argument to allocate must be positive integer
            sub rsp,8
            fld tbyte[rbx+rcx*4]
            call :%down64
            fistp qword[rsp]
            call :%near64
            pop rcx
      @@:
        test rcx,rcx
--      jle :e37atambpi     -- argument to allocate must be positive integer
        jg @f
          ::e37atambpi      -- argument to allocate must be positive integer
            pop rdx
            mov al,37       -- e37atambpi
            sub rdx,1
            jmp :!iDiag
            int3
      @@:
--*/
        add rcx,8           -- for nSize
--      mov rdx,[rsp+8]     -- era
        push rdi
        call :%pGetPool
        -- result is rdx bytes at rax, but we use first 8 bytes to save the size, for free().
        test rax,rax
        jz @f
            mov [rax],rdx   -- so this is 28 when returning 20 bytes of useable space (at rax+8).
            add rax,8       -- for nSize (see also "Minor point" above)
      @@:
        pop rdi
        push rax
        fild qword[rsp]     -- (treat as signed)
        add rsp,8
        jmp :%pStoreFlt     -- [rdi]:=st0
    []

--/*
procedure :%pFree(:%)
end procedure -- (for Edita/CtrlQ)
--*/
    :%pFree
------------
    [32]
        -- addr in eax, era in edx
        xor ebx,ebx -- (save some grief)
        cmp eax,h4  --DEV :%pLoadMint
        jl @f
            cmp byte[ebx+eax*4-1],0x12
            jne :e38atfmba   -- argument to free must be an atom
            sub esp,8
            fld qword[ebx+eax*4]
            call :%down53
            fistp qword[esp]
            call :%near53
            mov eax,[esp]
            add esp,8
      @@:
      :%pFree_e107ifma      -- exception here mapped to e107ifma
        mov ecx,[eax-4]     -- retrieve size
        sub eax,4
--      mov edx,[esp]       -- era
    [64]
        -- addr in rax, era in rdx
        mov r15,h4
        xor rbx,rbx -- (save some grief)
        cmp rax,r15
        jl @f
            cmp byte[rbx+rax*4-1],0x12
            jne :e38atfmba   -- argument to free must be an atom
            sub rsp,8
            fld tbyte[rbx+rax*4]
            call :%down64
            fistp qword[rsp] -- (assume ok, matches pAlloc [signed])
            call :%near64
            pop rax
      @@:
      :%pFree_e107ifma      -- exception here mapped to e107ifma
        mov rcx,[rax-8]     -- retrieve size
        sub rax,8
--      mov rdx,[rsp]       -- era
    []
        call :%pFreePool
        ret

--SUG: (now that I've written them, maybe I'll use them... though admittedly IncRef just looks like pure pointless overhead...)
--     (we might want cmp h4 inside these calls, which may alter the balance in favour of them being useful...)
--/*
    :%opIncRef
--------------
        [32]
            -- on entry edx must be set, all registers preserved
            cmp edx,h4
            jl @f
                add dword[ebx+edx*4-8],1
          @@:
        [64]
            -- on entry rdx must be set, all registers preserved
            mov r15,h4
            cmp edx,r15
            jl @f
                add qword[rbx+rdx*4-16],1
          @@:
        []
        ret
    :%opDecRef
--------------
        [32]
            -- on entry esi and edx must be set, all registers preserved
            cmp edx,h4
            jl @f
                sub dword[ebx+edx*4-8],1
                jnz @f
                    pushad
                    mov dword[esi],ebx
                    push dword[esp+32]
                    call :%pDealloc0
                    popad
        [64]
            -- on entry rsi and rdx must be set, no(?) registers preserved
            mov r15,h4
            cmp rdx,r15
            jl @f
                sub qword[rbx+rdx*4-16],1
                jnz @f
--                  pop al  -- (yet to determine which registers need preserving)
                    mov qword[rsi],rbx
--                  push qword[rsp+40??]
                    push qword[rsp]
                    call :%pDealloc0
        []
          @@:
            ret
--*/

--/*
procedure :%pCleanupAll(:%)
end procedure -- (for Edita/CtrlQ)
--*/
    :%pCleanupAll
-----------------
--          call :%pFreeStack
--      [32]
--      [64]
--      []
            ret

--/*
procedure :%pInitCS(:%)
end procedure -- (for Edita/CtrlQ)
--*/
    :%pInitCS
-------------
        [32]
            -- calling convention:
            -- lea edi,[res]            -- (integer)
            -- call :%pInitCS           -- [edi]:=init_cs()
            -- (trashes all registers)
            mov edx,[esp]               -- era
            mov ecx,4                   -- for nSize
            push edi
        [64]
            -- calling convention:
            -- lea rdi,[res]            -- (integer)
            -- call :%pInitCS           -- [rdi]:=init_cs()
            -- (trashes all registers)
            mov rdx,[rsp]               -- era
            mov rcx,8                   -- for nSize
            push rdi
        [PE32]
            add ecx,24                  -- win32 critical section size
        [PE64]
            add rcx,40                  -- win64 critical section size
        [ELF32]
            add ecx,4                   -- linux: single DWORD futex,
        [ELF64]
            add rcx,4                   --        even on 64-bit
        []
            call :%pGetPool             -- (trashes all registers)
        [32]
            test eax,eax
--          jz :memoryallocationfailure
            jnz @f
                int3
          @@:
            mov [eax],edx               -- save nSize
        [PE32]
            lea ecx,[eax+4]
        [ELF32]
            mov [eax+4],ebx             -- (a mutex (int32) of 0 is enough)
        [32]
            shr eax,2
            pop edi
            call :%pStoreMint           -- [edi]:=eax (all registers preserved)
        [PE32]
            push ecx                                    -- lpCriticalSection
            call "kernel32.dll","InitializeCriticalSection"
        [64]
            test rax,rax
--          jz :memoryallocationfailure
            jnz @f
                int3
          @@:
            mov [rax],rdx               -- save nSize
        [PE64]
            lea rcx,[rax+8]             -- lpCriticalSection (p1)
        [ELF64]
            mov dword[rax+8],ebx        -- (a mutex (int32) of 0 is enough) [yes, on 64-bit]
        [64]
            shr rax,2
            pop rdi
            call :%pStoreMint           -- [rdi]:=rax (preserves rax/rcx/rdx/rsi/rdi)
        [PE64]
            mov rax,rsp -- put 2 copies of rsp onto the stack...
            push rsp
            push rax
            or rsp,8    -- [rsp] is now 1st or 2nd copy:
                        -- if on entry rsp was xxx8: both copies remain on the stack
                        -- if on entry rsp was xxx0: or rsp,8 effectively pops one of them (+8)
                        -- obviously rsp is now xxx8, whatever alignment we started with
            sub rsp,8*5                 -- minimum 4 param shadow space, and align/somewhere to save rax
--          (rcx already set)
            call "kernel32.dll","InitializeCriticalSection"
--          add rsp,8*5
--          pop rsp
            mov rsp,[rsp+8*5]   -- equivalent to the add/pop
        []
            ret

--/*
procedure :%pDeleteCS(:%)
end procedure -- (for Edita/CtrlQ)
--*/
    :%pDeleteCS
---------------
        [32]
            -- calling convention
            --  mov eax,[mycs]
            --  mov esi,mycs        -- var no, if it can be unassigned [DEV]
            --  call :%pDeleteCS    -- delete_cs(eax)
--          test eax,eax
--          jz @f
            shl eax,2
            jz :unassignedcs
--          jnz @f
--              mov eax,[stdcs]
--              mov [stdcs],ebx
--        @@:
        [PE32]
--          lea ecx,[ebx+eax*4+4]
            lea ecx,[eax+4]
            push eax
            push ecx                                -- lpCriticalSection
            call "kernel32","DeleteCriticalSection"
            pop eax
--      [ELF32]
--          pop al          -- (nowt needed)
        [64]
            -- calling convention
            --  mov rax,[mycs]
            --  mov rsi,mycs        -- var no, if it can be unassigned
            --  call :%pDeleteCS    -- delete_cs(rax)
--          test rax,rax
--          jz @f
            shl rax,2
            jz :unassignedcs
--          jnz @f
--              mov rax,[stdcs]
--              mov [stdcs],rbx
--        @@:
        [PE64]
            push rax
            mov rcx,rsp -- put 2 copies of rsp onto the stack...
            push rsp
            push rcx
            or rsp,8    -- [rsp] is now 1st or 2nd copy:
                        -- if on entry rsp was xxx8: both copies remain on the stack
                        -- if on entry rsp was xxx0: or rsp,8 effectively pops one of them (+8)
                        -- obviously rsp is now xxx8, whichever alignment we started with
            sub rsp,8*5 -- minimum 4 param shadow space, and align
--          lea rcx,[rbx+rax*4+8]                   -- lpCriticalSection
            lea rcx,[rax+8]
            call "kernel32","DeleteCriticalSection"
--          add rsp,8*5
--          pop rsp
            mov rsp,[rsp+8*5]   -- equivalent to the add/pop
            pop rax
--      [ELF64]
--          pop al              -- (nowt needed)
        [32]
--          mov ecx,[ebx+eax*2] -- retrieve size
            mov ecx,[eax]       -- retrieve size
--          shl eax,2
            mov edx,[esp]       -- era
        [64]
--          mov rcx,[rbx+rax*2] -- retrieve size
            mov rcx,[rax]       -- retrieve size
--          shl rax,2
            mov rdx,[rsp]       -- era
        []
            call :%pFreePool
--        @@:
            ret


--/*
procedure :%pGetStdCS(:%)
end procedure -- (for Edita/CtrlQ)
--*/
--DEV cold feet?
--/*
    :%pGetStdCS                 -- [edi]:=get_stdcs()
--------------
    [32]
        -- calling convention
        -- lea edi,[res]        -- result
        -- call :%pGetStdCS     -- eax:=get_stdcs(); [edi]:=eax
        -- (trashes all other registers)
        mov eax,[stdcs]
        push edi
        test eax,eax
        jnz @f
            cmp [gt1tcb],ebx
            je @f
                -- (so >1 tcb exists)
                lea edi,[stdcs]
                call :%pInitCS      -- (trashes all registers)
                mov eax,[stdcs]
      @@:
        pop rdi
        jmp :%pStoreMint            -- [edi]:=eax (all registers preserved)
    [64]
        mov rax,[stdcs]
        test rax,rax
        jnz @f
            cmp [gt1tcb],rbx
            je @f
                lea rdi,[stdcs]
                call :%pInitCS      -- (trashes all registers)
                mov rax,[stdcs]
      @@:
        pop rdi
        jmp :%pStoreMint            -- [rdi]:=rax (preserves rax/rcx/rdx/rsi/rdi)
    []
--*/

-- no: compiler automatically inserts NULL
--  :%opEnterSTDCS
--      mov eax,[stdcs]
--      ...
--/*
procedure :%pEnterCS(:%)
end procedure -- (for Edita/CtrlQ)
--*/
    :%pEnterCS
--------------
    [32]
        -- calling convention
        -- mov ecx,[mycs]       -- or xor ecx,ecx
        -- mov esi,mycs         -- var no, if it can be unassigned [DEV]
        -- call :%opEnterCS     -- enter_cs(ecx)
        test ecx,ecx
        jnz @f
            mov ecx,[stdcs]
            test ecx,ecx
            jnz @f
            lea edi,[stdcs]
            call :%pInitCS
            mov ecx,[stdcs]
--          lea edi,[stdcs]
--          call :%pGetStdCS    -- sets eax, trashes rest
--          mov ecx,eax
            xor esi,esi
      @@:
        shl ecx,2
        jnz @f
      ::unassignedcs
            pop edx -- era
            mov al,92           -- e92vhnbaav(esi)
            sub edx,1
            jmp :!iDiag
            int3
      @@:
        add ecx,4
    [64]
        -- calling convention
        -- mov rcx,[mycs]       -- or xor rcx,rcx
        -- mov rsi,mycs         -- var no, if it can be unassigned
        -- call :%opEnterCS     -- enter_cs(rcx)
        test rcx,rcx
        jnz @f
            mov rcx,[stdcs]
            test rcx,rcx
            jnz @f
            lea rdi,[stdcs]
            call :%pInitCS
            mov rcx,[stdcs]
--          lea rdi,[stdcs]
--          call :%pGetStdCS    -- sets rax, trashes rest
--          mov rcx,rax
            xor rsi,rsi
      @@:
        shl rcx,2
        jnz @f
      ::unassignedcs
            pop rdx -- era
            mov al,92           -- e92vhnbaav(esi)
            sub rdx,1
            jmp :!iDiag
            int3
      @@:
        add rcx,8
    []
        call :EnterCriticalSection -- in pHeap.e (cs in ecx) all registers trashed
        ret

--/*
procedure :%pTryCS(:%)
end procedure -- (for Edita/CtrlQ)
--*/
    :%pTryCS
------------
--DEV (if >1 tcb exists)
    [32]
        -- calling convention
        -- mov ecx,[mycs]
        -- mov esi,mycs         -- var no, if it can be unassigned [DEV]
        -- lea edi,[res]
        -- call :%opTryCS       -- [edi]:=try_cs(ecx) (0 means failure)
        shl ecx,2
        jz :unassignedcs
        add ecx,4
        push edi
    [64]
        -- calling convention
        -- mov rcx,[mycs]
        -- mov rsi,mycs         -- var no, if it can be unassigned
        -- lea rdi,[res]
        -- call :%opTryCS       -- [rdi]:=try_cs(rcx) (0 means failure)
        shl rcx,2
        jz :unassignedcs
        add rcx,8
        push rdi
    []
        call :TryEnterCriticalSection
    [32]
        pop edi
    [64]
        pop rdi
    []
        jmp :%pStoreMint

--NO: compiler automatically inserts NULL
--  :%opLeaveSTDCS
--      mov eax,[stdcs]
--      ...
--/*
procedure :%pLeaveCS(:%)
end procedure -- (for Edita/CtrlQ)
--*/
    :%pLeaveCS
--------------
-- DEV (if >1 tcb exists)
    [32]
        -- calling convention
        -- mov ecx,[mycs]       -- or xor ecx,ecx
        -- mov esi,mycs         -- var no, if it can be unassigned [DEV]
        -- call :%opLeaveCS     -- leave_cs(ecx)
        test ecx,ecx
        jnz @f
            mov ecx,[stdcs]
            test ecx,ecx
            jz :%e02atdb0
      @@:
        shl ecx,2
        jz :unassignedcs
        add ecx,4
    [64]
        -- calling convention
        -- mov rcx,[mycs]       -- or xor rcx,rcx
        -- mov rsi,mycs         -- var no, if it can be unassigned
        -- call :%opLeaveCS     -- leave_cs(rcx)
        test rcx,rcx
        jnz @f
            mov rcx,[stdcs]
            test rcx,rcx
            jz :%e02atdb0
      @@:
        shl rcx,2
        jz :unassignedcs
        add rcx,8
    []
        call :LeaveCriticalSection
        ret
    
--  ::fin
    }


