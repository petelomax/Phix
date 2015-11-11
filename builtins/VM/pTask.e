--
-- builtins\VM\pTask.e
--
--  Shamelessly ripped off from OpenEuphoria, and tidied up a bit.
--
--DEV not yet an auto-include

--For the initial idea of controlled lock-step, see demo\rosetta\Synchronous_concurrency.exw, third version
--However, we need the scheduler to get every other go...

Better strategy?

I am trying to devise a lock-step scheduling algorithm, and it seems to help to phrase it in terms of a simple colour card game,
with greedy/naughty children and a bowl of sweets. Just wanted to see if anyone can spot any simplifications/glaring errors.

{{{
Let there be a black card for the teacher, and (pair per child) two red, two orange, two yellow, two green, two blue, etc.
The teacher starts with the black card and all colour2 cards in their hand. There are no cards on the table at the start.
All children start with a colour1 card and wait for the matching colour2 card to be put on the table.
<loop>
The teacher selects a colour2 card and puts it down on the table, let's say red2, and waits for red1.
The child with red1 picks up red2, takes a sweet, puts down red1, and waits for the black card.
The teacher can now pick up red1, put down black, and wait for red2.
The child with the red2 can now swap it for the black card, and wait for red1.
The teacher can now pick up the red2 card, put down red1, and wait for the black.
The child with the black card can pick up red1 and put down the black, and wait for red2.
The teacher can now pick up the black and select another colour2 (goto loop)

Notes:  Only one player is ever waiting for a specific card[1], different to the one they just put down[2].
        Nobody can ever put down a card they are not currently holding[3].
        There may be a simpler way (perhaps with red1<->red2 swapped per round) but I can't see it.
        [1] not particularly important [2] important [3] extremely important!
}}}

Pete

PS This is to implement tasks, so anyone suggesting using tasks will be e-kneecapped :-/

(A busy wait is acceptable to ensure that the teacher does not start until all children are ready.)

I implemented the following, using mutually exclusive locking, as a follow-up to a rosetta code task (if you must know it was Synchronous_concurrency, but reading that would not help here):
-- Simple lock-step strategy:
--  say we have three cards: Red, Green, and Blue (in a children's game).
--  initially both players have one in hand and the other is on the table.
--  (Tip: Deferring the "put down" as late as possible avoids any squabbles over picking a sweet from the sweet bowl)
--  The player with  red  can pick up green (and take a sweet) then put down the  red  they already have
--  The player with green can pick up  blue (and take a sweet) then put down the green they already have
--  The player with  blue can pick up  red  (and take a sweet) then put down the  blue they already have
--  The players are therefore forced to take alternate turns.
--  After picking up  red  and putting down  blue they must wait for green.
--  After picking up green and putting down  red  they must wait for  blue.
--  After picking up  blue and putting down green they must wait for  red.
The scheme can easily be extended, to red->orange->yellow->green->blue[->red] and accomodate more players (#cards-1), who are forced to play sequentially.
I think it helps to phrase the problem in terms of greedy/naughty children and ensuring that they cannot cheat/play out of turn.

With more than 2 players, most of their time is spent simply waiting for a specific card to be laid (/lock to be freed).

It works well, however I now need a new variation (not related to any specific rosetta task): a teacher (/scheduler) is to get every other go, and
can decide who goes next. As above, everyone (including the teacher) must spend most of the time waiting for a card to be laid down on the table, and
no two players should ever be waiting for the same card. If it needs 4 or 5 times as many cards (/locks), so be it (feel free to use red/black, 
hearts/clubs, 1..9, whatever). In all probability, the best solution probably mirrors the notion that children have small hands and can only hold a 
few cards, whereas (as a grown-up) the teacher is perfectly capable of holding a whole classful of cards in their hands, but at any moment all of them 
(including the teacher) should only ever be waiting for one specific card. I also predict there can only ever be one card on the table, but I'm not 
completely certain about that.

To be clear, I am looking for an //**english language**// strategy description (rather than any code) which not just lets but forces the teacher to have 
every other go, and lets them decide who goes next, all based on the premise of everyone spending most of their time waiting for a specific card to be laid.

Of course what I'm attempting is to implement tasks as step-locked threads. My google-fu has failed, perhaps no other language has both threads and tasks?

OK, this is my first attempt. Before I try implementing this, can anyone improve on it?

Let there be black (for the teacher), and (pair per child) two red, two orange, two yellow, two green, two blue, etc.

    teacher         child1          child2          ...
    lock black                                          -- (freely obtained)
    lock *2                                             -- ("", prior to any create/resume_thread)
                    lock red1       lock orange1        -- (freely obtained)
    <busy wait>     init = 1        init = 1
                  loop:
                    lock red2       lock orange2        -- (stall)
    unlock red2
    lock red1                                           -- (stall)
                    <run>
                    unlock red1                         -- (aka task_yield)
                    lock black                          -- (stall)
    unlock black
    lock red2                                           -- (stall)
                    unlock red2
                    lock red1                           -- (stall)
    unlock red1
    lock black                                          -- (stall)
                    unlock black
                    goto loop

    unlock orange2
    lock orange1                                        -- (stall)
                                    <run>
                                    unlock orange1      -- (aka task_yield)
                                    lock black          -- (stall)
    unlock black
    lock orange2                                        -- (stall)
                                    unlock orange2
                                    lock orange1        -- (stall)
    unlock orange1
    lock black                                          -- (stall)
                                    unlock black
                                    goto loop

The teacher starts with the black card and all colour2 cards in their hand.
All children start with a colour1 card and wait for the matching colour2 card to be put on the table.
(A busy wait is acceptable to ensure that the teacher does not start until all children are ready.)
<loop>
The teacher selects a colour2 card and puts it down on the table, let's say red2, and waits for red1.
The child with red1 picks up red2, takes a sweet, puts down red1, and waits for the black card.
The teacher can now pick up red1, put down black, and wait for red2.
The child with the red2 can now swap it for the black card, and wait for red1.
The teacher can now pick up the red2 card, put down red1, and wait for the black.
The child with the black card can pick up red1 and put down the black, and wait for red2.
The teacher can now pick up the black and select another colour2 (goto loop)

Notes:  Only one person is ever waiting for a specific card, different to the one they just put down.
        Nobody can ever put down a card they are not currently holding.
        There may be a simpler way (perhaps with red1<->red2 swapped per round) but I can't see it.

BETTER: (but bollocks... there's no stall in the loop!!) [SLEEP]
******
    teacher         child1          child2          ...
    lock *2                                             -- (before any other create_thread(/task)[/resume_thread])
    r,o = 2
                    lock red1       lock orange1        -- (freely obtained)
                    r = 2           o = 2
                  loop:
                    lock red[r]     lock orange[o]      -- (stall)
    lock black
    unlock red[r]
    r = 3-r
    lock red[r]                                         -- (stall)
    unlock black
                    <run>
                    r = 3-r
                    unlock red[r]                       -- (aka task_yield)
                    lock black                          -- (stall)
                    unlock black
                    goto loop
    unlock orange[o]
    o = 3-o
    lock orange[o]                                      -- (stall)
                                    <run>
                                    o = 3-o
                                    unlock orange[o]    -- (aka task_yield)
                                    goto loop
Notes:  The o,r in the teacher(/scheduler) are independent variables [held in an array] to those in the children.
        The teacher(/scheduler) must wait for the child to set their control var to 2 (non-0) before an unlock. (a spinwait shd be fine)
        We can store a task_id in the thread local storage as provided by pHeap.e

include builtins\VM\pStack.e    -- :%newStack and :%freeStack

pete: let's not mess about trying to create a new threadstack on pre-newEmit!!
      which means, belay this until p p p p p p p p -test works under newEmit!      [DONE!!]
      it might also be slightly helpful to have the debugger working first!!!!

task_create -> create_thread(SUSPENDED)
task_yield -> if next_task!=running_task then resume_thread(next_task) suspend_thread(running_task) end if

--public procedure task_delay(atom delaytime)   -- [DEV, undocumented]
--(SUG: task_save() An error occurs if you call task_save more than twice in any 2-second period.)

sequence tasks = {}         -- (the first call to task_create() adds one for main thread)
integer current_task = 0    -- ""
integer rt_first = 0        -- unsorted list of active real time tasks
integer ts_first = 0        -- unsorted list of active time shared tasks
integer free_tasks = 0

-- status values
constant ST_ACTIVE = 0,
         ST_SUSPENDED = 1,
         ST_DEAD = 2

constant T_REAL_TIME = 1,
         T_TIME_SHARE = 2

constant clock_period = 0.01
constant TASK_NEVER = 1e300

--constant e16cbchop        = 16    -- call_backs cannot have optional parameters
--constant e72iri       = 72    -- invalid routine_id
--constant e73atodmbs   = 73    -- argument to open_dll must be string
--constant e74dcfpe     = 74    -- define_c_func/proc parameter error
--constant e75cbrpmaba  = 75    -- call back routine parameters must all be atoms
--constant e81ipicfp        = 81    -- insufficient parameters in call_func/proc()
--constant e84cbpmbropr = 84    -- call_back parameter must be routine_id or {'+',routine_id}
--constant e88atcfpmbaos    = 88    -- arguments to c_func/proc must be atoms or strings
--constant e93tmpicfp   = 93    -- too many parameters in call_func/proc()
--constant e117rdnrav   = 117   -- routine does not return a value
--constant e118rrav     = 118   -- routine returns a value
--constant T_const1     = 26    -- (must match pglobals.e, but don't include that here)

--DEV use crash?
--procedure fatal(integer errcode, integer ep1=0)
--  #ilASM{ 
--      [32]
--          mov eax,[errcode]
--          mov edi,[ep1]
--          xor esi,esi     -- ep2 unused
--          call :%pRTErn   -- fatal error (see pdiagN.e)
--      [64]
--          mov rax,[errcode]
--          mov rdi,[ep1]
--          xor rsi,rsi     -- ep2 unused
--          call :%pRTErn   -- fatal error (see pdiagN.e)
--      []
--        }
--  ?9/0
--end procedure

type boolean(integer x)
    return x=0 or x=1
end type

boolean clock_stopped = FALSE

procedure scheduler()
-- pick the next task to run
atom earliest_time, start_time, t
boolean ts_found
sequence tp
integer p, earliest_task

    -- first check the real-time tasks

    -- find the task with the earliest MAX_TIME
    earliest_task = rt_first

    if clock_stopped or earliest_task = 0 then
        -- no real-time tasks are active
        start_time = 1
        t = -1

    else
        -- choose a real-time task
        earliest_time = tasks[earliest_task][TASK_MAX_TIME]

        p = tasks[rt_first][TASK_NEXT]
        while p!=0 do
            tp = tasks[p]
            if tp[TASK_MAX_TIME]<earliest_time then
                earliest_task = p
                earliest_time = tp[TASK_MAX_TIME]
            end if
            p = tp[TASK_NEXT]
        end while

        -- when can we start? how many runs?
        t = time()

        start_time = tasks[earliest_task][TASK_MIN_TIME]

        if earliest_task = current_task
        and tasks[current_task][TASK_RUNS_LEFT]>0 then
            -- runs left - continue with the current task
        else
            if tasks[current_task][TASK_TYPE]=T_REAL_TIME then
                tasks[current_task][TASK_RUNS_LEFT] = 0
            end if
            tasks[earliest_task][TASK_RUNS_LEFT] = tasks[earliest_task][TASK_RUNS_MAX]
        end if
    end if

    if start_time>t then
        -- No real-time task is ready to run.
        -- Look for a time-share task.

        ts_found = FALSE
        p = ts_first
        while p != 0 do
            tp = tasks[p]
            if tp[TASK_RUNS_LEFT]>0 then
                  earliest_task = p
                  ts_found = TRUE
                  exit
            end if
            p = tp[TASK_NEXT]
        end while

        if not ts_found then
            -- all time-share tasks are at zero, recharge them all,
            -- and choose one to run
            p = ts_first
            while p!=0 do
                tp = tasks[p]
                earliest_task = p
                tasks[p][TASK_RUNS_LEFT] = tp[TASK_RUNS_MAX]
                p = tp[TASK_NEXT]
            end while
        end if

        if earliest_task=0 then
            -- no tasks are active - no task will ever run again
            -- RTFatal("no task to run") ??
            abort(0)
        end if

        if tasks[earliest_task][TASK_TYPE]=T_REAL_TIME then
            -- no time-sharing tasks, wait and run this real-time task
--          wait(start_time-t)
            sleep(start_time-t)
        end if

    end if

    tasks[earliest_task][TASK_START] = time()

    current_task = earliest_task
--  if earliest_task = current_task then
--      pc += 1  -- continue with current task
--  else
--      -- switch to a new task
--
--      -- save old task state
--      tasks[current_task][TASK_CODE] = Code
--      tasks[current_task][TASK_PC] = pc
--      tasks[current_task][TASK_STACK] = call_stack
--
--      -- load new task state
--      Code = tasks[earliest_task][TASK_CODE]
--      pc = tasks[earliest_task][TASK_PC]
--      call_stack = tasks[earliest_task][TASK_STACK]
--
--      current_task = earliest_task
--
--      if tasks[current_task][TASK_PC] = 0 then
--          -- first time we are running this task
--          -- call its procedure, passing the args from task_create
--          pc = 1
--          val[t_id] = tasks[current_task][TASK_RID]
--          val[t_arglist] = tasks[current_task][TASK_ARGS]
--          new_arg_assign()
--          Code = {CALL_PROC, t_id, t_arglist}
--      else
--          -- resuming after a task_yield()
--          pc += 1
--          restore_privates(call_stack[$])
--      end if
--  end if
end procedure

function task_insert(integer first, integer task_id)
-- add a task to a list of tasks
    tasks[task_id][TASK_PREV] = 0
    tasks[task_id][TASK_NEXT] = first
    return task_id
end function

function task_delete(integer first, integer task_id)
-- remove a task from a list of tasks
integer prev = tasks[task_id][TASK_PREV]
integer next = tasks[task_id][TASK_NEXT]
    if prev!=0 then
        tasks[prev][TASK_NEXT] = next
    else
        first = next
    end if
    if next!=0 then
        tasks[next][TASK_PREV] = prev
    end if
    return first
end function

function add_task(integer rid, object args, integer task_type)
integer task_id
--      -1, 0, T_TIME_SHARE, ST_ACTIVE, 0, 0, 0, 1, 1, 1, 1, 0, {}, 1, {}, {}

sequence task = {rid,           -- TASK_RID
                 task_type,     -- TASK_TYPE
                 ST_SUSPENDED,  -- TASK_STATE
                 0,             -- TASK_START
                 0,             -- TASK_MIN_INC
                 0,             -- TASK_MAX_INC
                 0,             -- TASK_MIN_TIME
                 TASK_NEVER,    -- TASK_MAX_TIME
                 1,             -- TASK_RUNS_LEFT
                 1,             -- TASK_RUNS_MAX
                 0,             -- TASK_NEXT
                 0,             -- TASK_PREV
--!*/
                 args,          -- TASK_ARGS
                 0}             -- TASK_EBP4 (set by task_yield)
    if free_tasks then
        task_id = free_tasks
        free_tasks = tasks[free_tasks]
        tasks[task_id] = task
    else
        tasks = append(tasks,task)
        task_id = length(tasks)
    end if
    return task_id
end function

global function task_create(integer rid, sequence args)
--
-- Creates a new task, given a routine_id and the arguments that should be passed to it, in
--  exactly the same manner/format as call_proc, which is in fact used internally.
--  Note that task_create merely adds an inactive entry to the internal table; before any
--  code is executed there must be a task_schedule to specify when/how it should be run,
--  and a task_yield to suspend itself and start/resume execution of said table entry.
--
-- Returns a task_id which is a positive integer index to the internal table, and is needed 
--  when calling task_schedule/status/suspend. Each active task_id is unique, however (unlike 
--  OpenEuphoria) ids of completed tasks may get reused. [If that causes problems I suggest
--  extending task_status to optionally return args, which can contain an application-provided
--  unique id.]
--<or>
--  No two active tasks ever have the same task_id, however task_ids of completed tasks may get reused.
--
-- Comments:
-- task_create creates a new task, but does not start it executing. You must call [[:task_schedule]] for this purpose.
-- Each task has its own set of private variables and its own call stack. Global and local variables are shared between all tasks.
-- If a run-time error is detected, the traceback will include information on all tasks, with the offending task listed first.
-- Many tasks can be created that all run the same procedure, possibly with different parameters.
-- A task cannot be based on a function, since there would be no way of using the function result.
--  
-- Example: mytask = task_create(routine_id("myproc"), {5, 9, "ABC"})
--  
integer task_id
--sequence symtab
--object si             -- copy of symtab[i], speedwise
--integer minparams,
--      maxparams,
--      noofparams

    if current_task=0 then
        -- create an entry for the main process
--      -1, 0, T_TIME_SHARE, ST_ACTIVE, 0, 0, 0, 1, 1, 1, 1, 0, {}, 1, {}, {}

        current_task = add_task(-1,0,T_TIME_SHARE)
        if ts_first!=0 then ?9/0 end if -- (sanity check)
        ts_first = task_insert(ts_first, current_task)
        tasks[current_task][TASK_STATE] = ST_ACTIVE
        tasks[current_task][TASK_MIN_TIME] = 1
        tasks[current_task][TASK_MAX_TIME] = 1
    end if

--/*
    -- validate rid/args: (or we could just let call_proc issue these errors)

    -- get copy of symtab. NB read only! may contain nuts! (unassigned vars)
    #ilASM{ 
        [32]
            lea edi,[symtab]
        [64]
            lea rdi,[symtab]
        []
            call :%opGetST  -- [e/rdi]:=symtab (see builtins\VM\pStack.e)
          }
    if rid<T_const1
    or rid>length(symtab) then
        crash("invalid routine_id")
    end if
    si = symtab[rid]
    if not sequence(si)
    or si[S_NTyp]!=S_Proc then
        crash("invalid routine_id")
    end if

    minparams = si[S_ParmN]
    maxparams = length(si[S_sig])-1
    noofparams = length(args)
    if noofparams<minparams then crash("insufficient parameters") end if
    if noofparams>maxparams then crash("too many parameters") end if
--*/
    task_id = add_task(rid,args,T_REAL_TIME)
    return task_id
end function
--/*
enum TASK_RID,      -- routine id
--   TASK_TID,      -- external task id
     TASK_TYPE,     -- type of task: T_REAL_TIME or T_TIME_SHARED       (DEV merge these?)
     TASK_STATE,    -- status: ST_ACTIVE, ST_SUSPENDED, ST_DEAD
     TASK_START,    -- start time of current run
     TASK_MIN_INC,  -- time increment for min
     TASK_MAX_INC,  -- time increment for max
     TASK_MIN_TIME, -- minimum activation time
                    -- or number of executions remaining before sharing
     TASK_MAX_TIME, -- maximum activation time (determines task order)
     TASK_RUNS_LEFT,-- number of executions left in this burst
     TASK_RUNS_MAX, -- maximum number of executions in one burst
     TASK_NEXT,     -- points to next task on list
     TASK_PREV,     --   ""      prev       ""
     TASK_ARGS,     -- args to call task procedure with at startup
--   TASK_PC,       -- program counter for this task
--   TASK_CODE,     -- IL code for this task
--   TASK_STACK     -- call stack for this task
     TASK_EBP4      -- (set by task_yield)

procedure opTASK_CREATE()
-- create a new task
symtab_index sub
sequence new_entry
boolean recycle

    a = Code[pc+1] -- routine id
(see call_proc/:%opGetST)
    if val[a] < 0 or val[a] >= length(e_routine) then
            crash("invalid routine id")
    end if
    sub = e_routine[val[a]+1]
    if SymTab[sub][S_TOKEN] != PROC then
            crash("specify the routine id of a procedure, not a function or type")
    end if
    b = Code[pc+2] -- args

    -- initially it's suspended
    new_entry = {val[a],            -- TASK_RID
--               next_task_id,      -- TASK_TID
                 T_REAL_TIME,       -- TASK_TYPE
                 ST_SUSPENDED,      -- TASK_STATE
                 0,                 -- TASK_START
                 0,                 -- TASK_MIN_INC
                 0,                 -- TASK_MAX_INC
                 0,                 -- TASK_MIN_TIME
                 TASK_NEVER,        -- TASK_MAX_TIME
                 1,                 -- TASK_RUNS_LEFT
                 1,                 -- TASK_RUNS_MAX
                 0,                 -- TASK_NEXT
                 0,                 -- TASK_PREV
                 val[b],            -- TASK_ARGS
--               0,                 -- TASK_PC
--               {},                -- TASK_CODE
--               {}}                -- TASK_STACK
                 0}                 -- TASK_EBP4    (set by task_yield)

    recycle = FALSE
    for i = 1 to length(tasks) do
        if tasks[i][TASK_STATE] = ST_DEAD then
            -- this task is dead, recycle its entry
            -- (but not its external task id)
            tasks[i] = new_entry
            recycle = TRUE
            exit
        end if
    end for

    if not recycle then
        -- expand
        tasks = append(tasks, new_entry)
    end if

    target = Code[pc+3]
    val[target] = next_task_id
    if not id_wrap and next_task_id < TASK_ID_MAX then
        next_task_id += 1
    else
        -- extremely rare
        id_wrap = TRUE -- id's have wrapped
        for i = 1 to TASK_ID_MAX do
            next_task_id = i
            for j = 1 to length(tasks) do
                if next_task_id = tasks[j][TASK_TID] then
                    next_task_id = 0
                    exit -- this id is still in use
                end if
            end for
            if next_task_id then
                exit -- found unused id for next time
            end if
        end for
        -- must have found one - couldn't have trillions of non-dead tasks!
    end if
    pc += 4
end procedure
--*/

global procedure task_schedule(integer task_id, object schedule)
--procedure opTASK_SCHEDULE()
-- schedule a task by linking it into the real-time tasks queue,
-- or the time sharing tasks queue

--  integer task
atom t
--  object s
atom min_time,max_time
integer task_type = tasks[task_id][TASK_TYPE]
integer task_state = tasks[task_id][TASK_STATE]

--  a = Code[pc+1]
--  task = which_task(val[a])
--  b = Code[pc+2]

    if atom(schedule) then
        -- time-sharing
        if schedule<=0 then
--          RTFatal("number of executions must be greater than 0")
            crash("number of executions must be greater than 0")
        end if
        --tasks[task_id][TASK_RUNS_LEFT] = schedule  -- current execution count
        tasks[task_id][TASK_RUNS_MAX] = schedule   -- max execution count
        if task_type=T_REAL_TIME
        and task_state!=ST_SUSPENDED then
            rt_first = task_delete(rt_first, task_id)
        end if
        if task_type=T_REAL_TIME
        or task_state=ST_SUSPENDED then
            ts_first = task_insert(ts_first, task_id)
        end if
        task_type = T_TIME_SHARE

    else
        -- real-time
        if length(schedule)!=2 then
            crash("second argument must be {min_time, max_time}")
        end if
        {min_time,max_time} = schedule
        if min_time<0
        or max_time<0 then
            crash("min and max times must be greater than or equal to 0")
        end if
        if min_time>max_time then
            crash("task min time must be <= task max time")
        end if
        tasks[task_id][TASK_MIN_INC] = min_time
        tasks[task_id][TASK_MAX_INC] = max_time

        if min_time < clock_period/2 then
            -- allow multiple runs per clock period
            if min_time > 1.0e-9 then
                tasks[task_id][TASK_RUNS_MAX] = floor(clock_period / min_time)
            else
                -- avoid divide by zero or almost zero
                tasks[task_id][TASK_RUNS_MAX] = 1000000000 -- arbitrary, large
            end if
        else
            tasks[task_id][TASK_RUNS_MAX] = 1
        end if
        t = time()
        tasks[task_id][TASK_MIN_TIME] = t + min_time
        tasks[task_id][TASK_MAX_TIME] = t + max_time

        if task_type=T_TIME_SHARE
        and task_state!=ST_SUSPENDED then
            ts_first = task_delete(ts_first, task_id)
        end if
        if task_type=T_TIME_SHARE
        or task_state=ST_SUSPENDED then
            rt_first = task_insert(rt_first, task_id)
        end if
        task_type = T_REAL_TIME
    end if
    tasks[task_id][TASK_TYPE] = task_type
    tasks[task_id][TASK_STATE] = ST_ACTIVE
--  pc += 3
--end procedure
end procedure

global procedure task_suspend(integer task_id)
    tasks[task_id][TASK_STATE] = ST_SUSPENDED
    tasks[task_id][TASK_MAX_TIME] = TASK_NEVER
    if tasks[task_id][TASK_TYPE]=T_REAL_TIME then
        rt_first = task_delete(rt_first, task_id)
    else
        ts_first = task_delete(ts_first, task_id)
    end if
end procedure

procedure call_current_task()
-- (put into a separate routine as the frame of task_yield is no longer with us)
integer rid = tasks[current_task][TASK_RID]
sequence args = tasks[current_task][TASK_ARGS]
    call_proc(rid,args)
end procedure

global procedure task_yield()
--
-- Temporarily stop running this task, and give the scheduler a chance to pick a new task.
-- Task switching is achieved simply by saving and restoring ebp, which must always be 
--  dword-aligned and hence can safely be stored /4, as an integer. The only slightly
--  tricky part is creation and cleanup of virtual stacks, and looping on task termination.
--
integer ebp4    -- scratch var
integer rid
sequence args
atom t
--sequence task = tasks[current_task]   -- (maybe)

    #ilASM{
        [32]
            mov eax,ebp
            shr eax,2
            mov [epb4],eax
        [64]
            mov rax,rbp
            shl rax,2
            mov [ebp4],rax
        []
          }
    tasks[current_task][TASK_EBP4] = ebp4
    if tasks[current_task][TASK_STATE]=ST_ACTIVE then
        if tasks[current_task][TASK_RUNS_LEFT] > 0 then
            tasks[current_task][TASK_RUNS_LEFT] -= 1
        end if
        if tasks[current_task][TASK_TYPE]=T_REAL_TIME then
            t = time()
            if tasks[current_task][TASK_RUNS_MAX]>1
            and tasks[current_task][TASK_START]=t then
                -- quick run of rapid-cycling task - clock hasn't even ticked
                if tasks[current_task][TASK_RUNS_LEFT]=0 then
                    -- avoid excessive number of runs per clock period
                    t += clock_period
                    tasks[current_task][TASK_RUNS_LEFT] = tasks[current_task][TASK_RUNS_MAX]
                    tasks[current_task][TASK_MIN_TIME] = t + tasks[current_task][TASK_MIN_INC]
                    tasks[current_task][TASK_MAX_TIME] = t + tasks[current_task][TASK_MAX_INC]
--              else
--                  -- let it run multiple times per tick
                end if
            else
                tasks[current_task][TASK_MIN_TIME] = t + tasks[current_task][TASK_MIN_INC]
                tasks[current_task][TASK_MAX_TIME] = t + tasks[current_task][TASK_MAX_INC]
            end if
        end if
    end if

    while 1 do
        scheduler()                                 -- (invokes abort(0) if nothing to run)
        ebp4 = tasks[current_task][TASK_EBP4]
        if ebp4!=0 then exit end if                 -- (set by, erm, task_yield;-)...)
        -- create a new callstack and call_proc. [DEV new routines/tweaks needed in pStack, to manage freelists of vsbs]
        -- Warning: Between :%newStack and :%freeStack there is no proper frame for this task_yield,
        --          or rather there is, but r/ebp is not pointing at it any more, so no parameters or 
        --          local or temporary variables of any kind whatsoever can be accessed. (You could
        --          stash some //values// on the stack, or use file-level hll variables, though.)
        --          Luckily, however, we can safely make a simple parameterless call without problem.
        #ilASM{ push r/ebp
                call :%newStack}
        call_current_task()
        #ilASM{ call :%freeStack
                pop r/ebp}
        -- on return, cleanup and schedule something else (if any)
        task_suspend(current_task)      -- (unlink it)
        tasks[current_task] = ST_DEAD
    end while
    #ilASM{
        [32]
            mov ebp,[ebp4]
            shl ebp,2
        [64]
            mov rbp,[ebp4]
            shl rbp,2
        []
--          jmp :%opRetf
          }
end procedure

global function task_self()
    return current_task
end function

global function task_status(integer task_id)
-- return task status
    return 1-tasks[task_id][TASK_STATE]
    -- {ST_ACTIVE(=0) -> return 1,
    --  ST_SUSPENDED(=1) -> return 0,
    --  ST_DEAD(=2) -> return -1}
end function

atom save_clock = -1

global procedure task_clock_stop()
-- stop the scheduler clock
    if not clock_stopped then
        save_clock = time()
        clock_stopped = TRUE
    end if
end procedure

global procedure task_clock_start()
-- resume the scheduler clock
atom shift
    if clock_stopped then
        if save_clock>=0 and save_clock<time() then
            shift = time() - save_clock
            for i = 1 to length(tasks) do
                tasks[i][TASK_MIN_TIME] += shift
                tasks[i][TASK_MAX_TIME] += shift
            end for
        end if
        clock_stopped = FALSE
    end if
end procedure

global function task_list()
-- return list of active and suspended tasks
sequence list
    list = {}
    for i = 1 to length(tasks) do
        if tasks[i][TASK_STATE]!=ST_DEAD then
            list = append(list, tasks[i][TASK_TID])
        end if
    end for
    return list
end function

--!/*
constant TRUE = 1, FALSE = 0

boolean t1_running, t2_running

procedure task1(sequence message)
    for i=1 to 10 do
        printf(1, " task1 (%d) %s\n", {i, message})
        task_yield()
    end for
    t1_running = FALSE
end procedure

procedure task2(sequence message)
    for i=1 to 10 do
        printf(1, " task2 (%d) %s\n", {i, message})
        task_yield()
    end for
    t2_running = FALSE
end procedure

puts(1, " main task : start \n")

atom t1 , t2

    t1 = task_create(routine_id(" task1 "), {" Hello "})
    t2 = task_create(routine_id(" task2 "), {" Goodbye "})

    task_schedule(t1 , {2.5 , 3})
    task_schedule(t2 , {5, 5.1})

    t1_running = TRUE
    t2_running = TRUE

    while t1_running or t2_running do
        if get_key()='q' then
            exit
        end if
        task_yield()
    end while

    puts(1, " main task : stop \n")
-- program ends when main task is finished
--!*/
