--
-- builtins\VM\pTask.e
-- ===================
--
--  Shamelessly ripped off from OpenEuphoria, and tidied up a bit (with some more tidyup rqd!).
--
--  This is an auto-include file

include builtins\VM\pStack.e    -- :%pNewStack and :%pFreeStack

sequence tasks              -- (the first call to task_create() adds one for main thread)
if "abc"="def" then tasks={} end if
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
--constant e72iri           = 72    -- invalid routine_id
--constant e73atodmbs       = 73    -- argument to open_dll must be string
--constant e74dcfpe         = 74    -- define_c_func/proc parameter error
--constant e75cbrpmaba      = 75    -- call back routine parameters must all be atoms
--constant e81ipicfp        = 81    -- insufficient parameters in call_func/proc()
--constant e84cbpmbropr     = 84    -- call_back parameter must be routine_id or {'+',routine_id}
--constant e88atcfpmbaos    = 88    -- arguments to c_func/proc must be atoms or strings
--constant e93tmpicfp       = 93    -- too many parameters in call_func/proc()
--constant e117rdnrav       = 117   -- routine does not return a value
--constant e118rrav         = 118   -- routine returns a value
constant T_const1       = 26,   -- (must match pglobals.e, but don't include that here)
         S_NTyp         = 2,    -- Const/GVar/TVar/Nspc/Type/Func/Proc
         S_sig          = 7,    -- routine signature, eg {'F',T_integer} (nb S_sig must be = S_vtype)
         S_ParmN        = 9,    -- minimum no of parameters (max is length(S_sig)-1)
         S_Proc         = 8     -- Procedure

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

enum TASK_RID,      -- routine id
--   TASK_TID,      -- external task id
     TASK_TYPE,     -- type of task: T_REAL_TIME(1) or T_TIME_SHARED(2)
     TASK_STATE,    -- status: ST_ACTIVE(0), ST_SUSPENDED(1), ST_DEAD(2)
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
    if first!=0 then
        if tasks[first][TASK_PREV]!=0 then ?9/0 end if
        tasks[first][TASK_PREV] = task_id
    end if
    tasks[task_id][TASK_PREV] = 0
    tasks[task_id][TASK_NEXT] = first
    return task_id
end function

function task_delete(integer first, integer task_id)
-- remove a task from a list of tasks
integer prev = tasks[task_id][TASK_PREV]
integer next = tasks[task_id][TASK_NEXT]
    if prev!=0 then
        if task_id=first then ?9/0 end if
        tasks[prev][TASK_NEXT] = next
    else
        if task_id!=first then ?9/0 end if
        first = next
    end if
    if next!=0 then
        tasks[next][TASK_PREV] = prev
    end if
    return first
end function

--integer next_task_id = 1

function add_task(integer rid, object args, integer task_type)
integer task_id

sequence task = {rid,           -- TASK_RID
--               next_task_id,  -- TASK_TID
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
--  next_task_id += 1
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
sequence symtab
object si = 0           -- copy of symtab[i], speedwise
integer minparams,
        maxparams,
        noofparams

    if current_task=0 then
        -- create an entry for the main process
--      -1, 0, T_TIME_SHARE, ST_ACTIVE, 0, 0, 0, 1, 1, 1, 1, 0, {}, 1, {}, {}

--      if init=0 then
            tasks = {}
--          init = 1
--      end if
        current_task = add_task(-1,0,T_TIME_SHARE)
        if ts_first!=0 then ?9/0 end if -- (sanity check)
        ts_first = task_insert(ts_first, current_task)
        tasks[current_task][TASK_STATE] = ST_ACTIVE
        tasks[current_task][TASK_MIN_TIME] = 1
        tasks[current_task][TASK_MAX_TIME] = 1
        -- (T_EBP set by first call to task_yield [overwriting the 0])
    end if

-- reinstated 17/8/19 (as routine_id now returns 0):
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
    if rid>T_const1
    and rid<=length(symtab) then
        si = symtab[rid]
    end if
    if not sequence(si)
    or si[S_NTyp]!=S_Proc then
        crash("invalid routine_id")
    end if

    minparams = si[S_ParmN]
    maxparams = length(si[S_sig])-1
    noofparams = length(args)
    if noofparams<minparams then crash("insufficient parameters") end if
    if noofparams>maxparams then crash("too many parameters") end if

    task_id = add_task(rid,args,T_REAL_TIME)
    -- (T_EBP set within task_yield [because it is 0])
    return task_id
end function
--/*

--  procedure opTASK_CREATE()
--  -- create a new task
--  symtab_index sub
--  sequence new_entry
--  boolean recycle
--
--      a = Code[pc+1] -- routine id
--  (see call_proc/:%opGetST)
--      if val[a] < 0 or val[a] >= length(e_routine) then
--              crash("invalid routine id")
--      end if
--      sub = e_routine[val[a]+1]
--      if SymTab[sub][S_TOKEN] != PROC then
--              crash("specify the routine id of a procedure, not a function or type")
--      end if
--      b = Code[pc+2] -- args
--
--      -- initially it's suspended
--      new_entry = {val[a],            -- TASK_RID
--  --               next_task_id,      -- TASK_TID
--                   T_REAL_TIME,       -- TASK_TYPE
--                   ST_SUSPENDED,      -- TASK_STATE
--                   0,                 -- TASK_START
--                   0,                 -- TASK_MIN_INC
--                   0,                 -- TASK_MAX_INC
--                   0,                 -- TASK_MIN_TIME
--                   TASK_NEVER,        -- TASK_MAX_TIME
--                   1,                 -- TASK_RUNS_LEFT
--                   1,                 -- TASK_RUNS_MAX
--                   0,                 -- TASK_NEXT
--                   0,                 -- TASK_PREV
--                   val[b],            -- TASK_ARGS
--  --               0,                 -- TASK_PC
--  --               {},                -- TASK_CODE
--  --               {}}                -- TASK_STACK
--                   0}                 -- TASK_EBP4    (set by task_yield)
--
--      recycle = FALSE
--      for i = 1 to length(tasks) do
--          if tasks[i][TASK_STATE] = ST_DEAD then
--              -- this task is dead, recycle its entry
--              -- (but not its external task id)
--              tasks[i] = new_entry
--              recycle = TRUE
--              exit
--          end if
--      end for
--
--      if not recycle then
--          -- expand
--          tasks = append(tasks, new_entry)
--      end if
--
--      target = Code[pc+3]
--      val[target] = next_task_id
--      if not id_wrap and next_task_id < TASK_ID_MAX then
--          next_task_id += 1
--      else
--          -- extremely rare
--          id_wrap = TRUE -- id's have wrapped
--          for i = 1 to TASK_ID_MAX do
--              next_task_id = i
--              for j = 1 to length(tasks) do
--                  if next_task_id = tasks[j][TASK_TID] then
--                      next_task_id = 0
--                      exit -- this id is still in use
--                  end if
--              end for
--              if next_task_id then
--                  exit -- found unused id for next time
--              end if
--          end for
--          -- must have found one - couldn't have trillions of non-dead tasks!
--      end if
--      pc += 4
--  end procedure
--*/

global procedure task_schedule(integer task_id, object schedule)
--
-- schedule a task by linking it into the real-time tasks queue,
-- or the time sharing tasks queue

atom t
atom min_time,max_time
    if task_id=0 then task_id = 1 end if    -- (OE compatibility)
    -- (the same line may be wanted in task_suspend and task_status)
integer task_type = tasks[task_id][TASK_TYPE]
integer task_state = tasks[task_id][TASK_STATE]

--?{"task_schedule",task_id,schedule,tasks}
    if atom(schedule) then
        -- time-sharing
--      if schedule<=0 then
        if not integer(schedule) or schedule<=0 then    -- (27/7/19, in passing)
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
--?{"task_schedule (exit)",task_id,schedule,tasks}
end procedure

global procedure task_suspend(integer task_id)
--?{"task_suspend",task_id,tasks}
    if tasks[task_id][TASK_STATE]!=ST_ACTIVE then ?9/0 end if   -- added 27/7/19
    tasks[task_id][TASK_STATE] = ST_SUSPENDED
    tasks[task_id][TASK_MAX_TIME] = TASK_NEVER
    if tasks[task_id][TASK_TYPE]=T_REAL_TIME then
        rt_first = task_delete(rt_first, task_id)
    else
        ts_first = task_delete(ts_first, task_id)
    end if
--  task_yield()    -- added 27/7/19 [removed same day, as per docs must do itself]
--  ("" erm, absolutely *not*, unless task_self()==task_id..., I now realise)
--?{"task_suspend (end)",task_id,tasks}
end procedure

procedure call_current_task()
-- (Put into a separate routine as the frame of task_yield is no longer with us.)
-- (It proved necessary to keep the system stack balanced, so this uses a trick
--  of push 0; jmp :%opCallProc instead of call :%opCallProc, to signal that it
--  should transfer control directly to :%opRetf rather than do a "ret".)
integer rid = tasks[current_task][TASK_RID]
sequence args = tasks[current_task][TASK_ARGS]
--BUGFIX: This was the plan, but it unbalances the system stack... (taskwire.exw)
--  call_proc(rid,args)
    #ilASM{
        [32]
            mov eax,[rid]       -- (opUnassigned)
            mov esi,[args]      -- (opUnassigned)
            push ebx
            jmp :%opCallProc    -- call_proc(eax,esi)
        [64]
            mov rax,[rid]       -- (opUnassigned)
            mov rsi,[args]      -- (opUnassigned)
            push rbx
            jmp :%opCallProc    -- call_proc(rax,rsi)
        []
          }
    -- (note: any code placed here would never be executed)
end procedure

integer kill_ebp_id = 0     -- (delay till using something else)

procedure kill_ebp()
integer ebp4 = tasks[kill_ebp_id][TASK_EBP4]
    if kill_ebp_id=current_task then ?9/0 end if    -- sanity check
    tasks[kill_ebp_id][TASK_EBP4] = 0
    #ilASM{ 
        [32]
            push ebp
            mov ebp,[ebp4]
            shl ebp,2
            call :%pFreeStack
            pop ebp
        [64]
            push rbp
            mov rbp,[ebp4]
            shl rbp,2
            call :%pFreeStack
            pop rbp
          }
--18/7/17:
    tasks[kill_ebp_id] = free_tasks
    free_tasks = kill_ebp_id

    kill_ebp_id = 0
end procedure

--DEV/SUG:
--procedure validate_stack()
--integer esp4
--  #ilASM{
--      [32]
--          mov eax,esp
--          shr eax,2
--          mov [esp4],eax
--      [64]
--          mov rax,rsp
--          shr rax,2
--          mov [esp4],rax
--      []
--        }
--  if esp4!=tasks[current_task][TASK_ESP4] then ?9/0 end if    -- stack imbalance
--end procedure

integer ebp4    -- scratch var (must be static for task_yield, see below)

global procedure task_yield()
--
-- Temporarily stop running this task, and give the scheduler a chance to pick a new task.
-- Task switching is achieved simply by saving and restoring ebp, which must always be 
--  dword-aligned and hence can safely be stored /4, as an integer. The only slightly
--  tricky part is creation and cleanup of virtual stacks, and looping on task termination.
--
atom t
--sequence task = tasks[current_task]   -- (maybe)

    if current_task=0 then return end if
    #ilASM{
            e_all
        [32]
            mov eax,ebp
            shr eax,2
            mov [ebp4],eax
        [64]
            mov rax,rbp
            shr rax,2
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
        --
        -- create a new callstack and call_proc. [DEV new routines/tweaks needed in pStack, to manage freelists of vsbs]
        --
        -- WARNING: After :%pNewStack there is no proper frame for this task_yield, or rather it is 
        -- *******  saved in tasks[prev_task][TASK_EBP4], but r/ebp is not pointing at it any more, 
        --          so no parameters or local or temporary variables of any kind whatsoever can be 
        --          accessed. Obviously, ebp will be pointing to an empty frame [a fake T_maintls].
        --          You can/must use static file-level hll variables, like ebp4, or maybe the stack,
        --          and luckily we /can/ safely make simple parameterless calls without any problem,
        --          but if (eg) you tried to inline "call_current_task()", such that rid and args 
        --          became locals or temporaries, then it would all fail rather horribly...
        --
        #ilASM{ call :%pNewStack }

        call_current_task()                     -- (start new task)

        --
        -- on return, cleanup and schedule something else (if any)
        --
        -- first, re-save ebp (since that is likely to be set deeper
        --  down the stack from a mid-task invocation of task_yield),
        --    - in this particular case purely for use in kill_ebp().
        #ilASM{
            [32]
                mov eax,ebp
                shr eax,2
                mov [ebp4],eax
            [64]
                mov rax,rbp
                shr rax,2
                mov [ebp4],rax
            []
              }

        tasks[current_task][TASK_EBP4] = ebp4
        task_suspend(current_task)  -- (it has finished, unlink it)
-->
--18/7/17:
        tasks[current_task][TASK_STATE] = ST_DEAD
--NO:! (see kill_ebp...)
--      tasks[current_task] = free_tasks
--      free_tasks = current_task

        if kill_ebp_id!=0 then      -- quickly check for "two in a row"
            -- (can only happen for two new that both die immediately)
            kill_ebp()
        end if
        kill_ebp_id = current_task  -- (kill /after/ ebp:=something else)
    end while
--DEV:
--  validate_stack()    -- (as above, do not try to inline!)
    #ilASM{
        [32]
            mov ebp,[ebp4]
            shl ebp,2
----DEV (temp, verify it is a valid ebp, or 0:)
--mov eax,[ebp+24]              -- ebp_root
--cmp dword[eax+16],#40565342   -- vsb_magic="@VSB"?
--je @f
--  int3
--@@:
        [64]
            mov rbp,[ebp4]
            shl rbp,2
        []
--          jmp :%opRetf
          }
    if kill_ebp_id!=0 then
        kill_ebp()
    end if
end procedure

global procedure task_delay(atom delaytime)
atom t = time()+delaytime
    while 1 do
        task_yield()
        if time()>=t then exit end if
        sleep(0.01)
    end while
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
    if clock_stopped and current_task!=0 then
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
sequence list = {}
    if current_task!=0 then
        for i=1 to length(tasks) do
--18/7/17:
--          if tasks[i][TASK_STATE]!=ST_DEAD then
            if sequence(tasks[i])
            and tasks[i][TASK_STATE]!=ST_DEAD then
--              list = append(list, tasks[i][TASK_TID])
                list = append(list, i)
            end if
        end for
    end if
    return list
end function

--/*
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

    t1 = task_create(routine_id("task1"), {"Hello"})
    t2 = task_create(routine_id("task2"), {"Goodbye"})

--  task_schedule(t1 , {2.5, 3})    -- run every 2.5 to 3 seconds
--  task_schedule(t2 , {5, 5.1})    -- run every 5 to 5.1 seconds

    -- ten times faster:
--  task_schedule(t1 , {0.25, 0.3}) -- run every 0.25 to 0.3 seconds
--  task_schedule(t2 , {0.5, 0.51}) -- run every 0.5 to 0.51 seconds

    -- time shared:
    task_schedule(t1 , 1) -- run twice
    task_schedule(t2 , 2) -- run once


    t1_running = TRUE
    t2_running = TRUE

    while t1_running or t2_running do
        if get_key()='q' then
?9/0
            exit
        end if
        task_yield()
--      sleep(0.2)
    end while

    puts(1, " main task : stop \n")
-- program ends when main task is finished
--*/
