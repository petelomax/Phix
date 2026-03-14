--
-- builtins\eta.e
-- Adaptive ETA estimator (incomplete...)

local enum TOTAL, -- 1: total
            DONE, -- 2: done
           START, -- 3: start_time
        LAST_ETA, -- 4: last_eta_time
   SMOOTHED_RATE, -- 5: smoothed_rate
      ETA_FACTOR, -- 6: eta_factor (multiplicative)
        ETA_BIAS, -- 7: eta_bias   (additive correction)
   ERROR_HISTORY, -- 8: error_history
     MAX_HISTORY, -- 9: max_history
 SMOOTHING_ALPHA, -- 10: smoothing alpha
      ETA_LENGTH = $

sequence etas
bool eta_free = -1

--global type eta_state(object x)
--  return sequence(x) and length(x)=10
--end type

global type eta_state(object i)
    return integer(i) and (i=0 or (i>=1 and i<=length(etas)))
end type

--DEV never actually tested:
atom now = NULL
global procedure set_now(atom a)
    now = a
end procedure

local function get_now()
    return iff(now?now:time())
end function

global function eta_new(integer total_work)
    if eta_free=-1 then
        etas = {}
        eta_free = 0
    end if
    sequence eta = repeat(0,ETA_LENGTH)
    eta[TOTAL] = total_work
    eta[ETA_FACTOR] = 1.0
    eta[ERROR_HISTORY] = {}
    eta[MAX_HISTORY] = 10
    eta[SMOOTHING_ALPHA] = 0.05
    integer res
    if eta_free then
        res = eta_free
        eta_free = etas[eta_free]
        etas[res] = eta
    else
        etas = append(etas,eta)
        res = length(etas)
    end if
    return res
end function

--DEV never actually tested:
global function eta_delete(eta_state s)
    etas[s] = eta_free
    eta_free = s
    return null
end function

global procedure eta_start(eta_state s)
--  atom t = time()
    atom t = get_now()
    etas[s][START] = t
    etas[s][LAST_ETA] = t
end procedure


global function eta_update(eta_state s, integer done)
    sequence eta = etas[s]
    etas[s] = NULL
    
    eta[DONE] = done

    integer total = eta[TOTAL],
        work_left = total - done
--  if done <= 0 then return "&" end if

--  atom now = time()
    atom now = get_now()
--       dt = now - eta[LAST_ETA]
--  if dt < 1 then return "" end if  -- only emit once per second

    atom lapsed = now - eta[START]
--  atom inst_rate = done / lapsed
--  atom inst_rate = done / (now - eta[START])
    atom inst_rate = iff(lapsed ? done/lapsed : 0)

    -- update smoothed rate
    atom a = eta[SMOOTHING_ALPHA],
         r = eta[SMOOTHED_RATE]
    r = iff(r=0 ? inst_rate : a*inst_rate + (1-a)*r)
    eta[SMOOTHED_RATE] = r

    -- raw ETA
--  atom eta_raw = work_left / r
    atom eta_raw = iff(r=0 ? 0 : work_left / r)

    -- corrected ETA
    atom eta_corr = eta_raw * eta[ETA_FACTOR] + eta[ETA_BIAS]

    -- never show <2 seconds
--  if eta_corr < 2 then eta_corr = 2 end if

    -- predict completion time
    atom predicted_finish = now + eta_corr

    eta[LAST_ETA] = now -- update last eta time

    etas[s] = eta

    -- return ETA in seconds as an atom
    return sprintf("%g",eta_corr)
--  return eta_corr
end function


-- Call this when you *actually* finish a predicted segment.
-- It measures error and adjusts internal factors.
global procedure eta_record_completion(eta_state s)
--  atom now = time()
--DEV shit error:
--  atom predicted_finish = s[LAST_ETA] + 2  -- approximate from last emission
--  atom predicted_finish = etas[s][LAST_ETA] + 2  -- approximate from last emission
    atom predicted_finish = etas[s][LAST_ETA] + 0.005  -- approximate from last emission
--  atom error = now - predicted_finish
--  atom error = time() - predicted_finish
    atom error = get_now() - predicted_finish

    -- update history
    sequence history = etas[s][ERROR_HISTORY]
    etas[s][ERROR_HISTORY] = NULL
--  s[ERROR_HISTORY] = append(s[ERROR_HISTORY], error)
    if length(history) >= etas[s][MAX_HISTORY] then
        history = history[2..$]
    end if
    history = append(history,error)
    etas[s][ERROR_HISTORY] = history

    -- update bias (additive)
--  atom bias = sum(history)/length(history)
--  s[ETA_BIAS] = 0
--  for e in s[8] do
--      s[ETA_BIAS] += e
--  end for
--  s[ETA_BIAS] /= length(s[8])
--  etas[s][ETA_BIAS] = bias
    etas[s][ETA_BIAS] = sum(history)/length(history)

    -- update factor (multiplicative)
    if error > 0 then
        etas[s][ETA_FACTOR] *= 1.05     -- optimistic -> increase pessimism
    else
        etas[s][ETA_FACTOR] *= 0.97     -- pessimistic -> relax
    end if
    ? {"Final factor",etas[s][ETA_FACTOR],"bias",etas[s][ETA_BIAS]}
end procedure


--tests:
--include eta.e

--/*
procedure test_fast_then_slow()
    integer N = 200
    eta_state S = eta_new(N)
    eta_start(S)

    for i=1 to N do
        -- fast for first half, slow for second half
        if i < N/2 then
            sleep(0.001)       -- 1 ms
        else
            sleep(0.010)       -- 10 ms
        end if

        object eta = eta_update(S,i)
--      if string(eta) and length(eta) then
        if length(eta) then
            printf(1,"i=%3d ETA=%s sec\n",{i,eta})
        end if
    end for

    eta_record_completion(S)
--  ? {"Final factor",S[6],"bias",S[7]}
end procedure

test_fast_then_slow()
--*/
--/*
procedure test_slow_then_fast()
    integer N = 200
    eta_state S = eta_new(N)
    eta_start(S)

    for i=1 to N do
        if i < N/2 then
            sleep(0.010)
        else
            sleep(0.001)
        end if

        object eta = eta_update(S,i)
--      if string(eta) and length(eta) then
        if length(eta) then
            printf(1,"i=%3d ETA=%s sec\n",{i,eta})
        end if
    end for

    eta_record_completion(S)
--  ? {"Final factor",S[6],"bias",S[7]}
end procedure

test_slow_then_fast()
--*/
--/*
procedure test_random()
    integer N = 200
    eta_state S = eta_new(N)
    eta_start(S)

    for i=1 to N do
        sleep(rand(5)/200)  -- 0–25 ms
        object eta = eta_update(S,i)
--      if string(eta) and length(eta) then
        if length(eta) then
            printf(1,"i=%3d ETA=%s sec\n",{i,eta})
        end if
    end for

    eta_record_completion(S)
--  ? {"Final factor",S[6],"bias",S[7]}
end procedure

test_random()
--*/

--/*
Do you have any suggestions for an enhanced eta routine? I recently added an eta to a
long-running program and noticed that many routines such as gcd() and is_prime() are
much faster for small numbers, hence an outer loop of 1..limit tends to give hopelessly 
optimistic etas whereas limit..1 is pessimistic, which is probably far less annoying.
However the thought struck me that if you kept a mini-history of previous etas and when
they were given, you would know how badly off the mark you were, and be able to adjust
future estimates to be more realistic.
--*/

