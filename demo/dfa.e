-- This code is in the public domain - feel free to do anything you
-- wish with it.
--
-- Eli Bendersky (eliben@gmail.com)
--

enum DFA_TRANS_TABLE, DFA_TRANS_TARGET, DFA_START, DFA_FINAL

global type DFA(object dfa)
    return sequence(dfa)
       and length(dfa)=4
       and sequence(dfa[DFA_TRANS_TABLE])
       and sequence(dfa[DFA_TRANS_TARGET])
       and state(dfa[DFA_START])
       and sequence(dfa[DFA_FINAL])
end type

global function DFA_simulate(DFA this, string in)
    state cur_state = this[DFA_START]

    for i=1 to length(in) do
        integer ch = in[i]
        integer k = find({cur_state,ch},this[DFA_TRANS_TABLE])
        if k=0 then
            return "REJECT"
        end if
        cur_state = this[DFA_TRANS_TARGET][k]
    end for

    if find(cur_state,this[DFA_FINAL]) then
        return "ACCEPT"
    else
        return "REJECT"
    end if
end function

global procedure DFAshow(DFA this)
    printf(1,"DFA start state: %d\n",{this[DFA_START]})
    printf(1,"DFA final state(s): %s\n",{sprint(this[DFA_FINAL])})
    for i=1 to length(this[DFA_TRANS_TABLE]) do
        printf(1,"Trans[%d,%c] = %d\n",{this[DFA_TRANS_TABLE][i][1],this[DFA_TRANS_TABLE][i][2],this[DFA_TRANS_TARGET][i]})
    end for
end procedure

--
-- Builds the epsilon closure of states for the given NFA
--
function build_eps_closure(NFA nfa, sequence states)
    --
    -- push all states onto a stack
    --
    sequence unchecked_stack = states
    --
    -- initialize eps_closure(states) to states
    --
    sequence eps_closure = states

    while length(unchecked_stack) do
        --
        -- pop state t, the top element, off the stack
        --
        state t = unchecked_stack[1]
        unchecked_stack = unchecked_stack[2..$]
        --
        -- for each state u with an edge from t to u labeled EPS
        --
        for i=1 to length(nfa[NFA_TRANS_TABLE][t]) do
            input ch = nfa[NFA_TRANS_TABLE][t][i]
            if ch == EPS then
                state u = i
                --
                -- if u is not already in eps_closure, add it and push it onto stack
                --
                if not find(u,eps_closure) then
                    eps_closure &= u
                    unchecked_stack &= u
                end if
            end if
        end for
    end while

    eps_closure = sort(eps_closure)
    return eps_closure
end function

type state_rep(sequence rep)
    for i=1 to length(rep) do
        if not state(rep[i]) then return false end if
    end for
    return true
end type

type state_rep_seq(sequence srs)
    for i=1 to length(srs) do
        if not state_rep(srs[i]) then return false end if
    end for
    return true
end type

global function subset_construct(NFA nfa)
--
-- Subset construction algorithm. Creates a DFA that recognizes the same
-- language as the given NFA
--
integer k
DFA dfa
    --
    -- state_rep_seq: a set of NFA states which is represented by some DFA state
    --
    state_rep_seq marked_states = {}
    state_rep_seq unmarked_states
    --
    -- gives a number to each state in the DFA
    --
    state_rep_seq dfa_state_num = {}
    --
    -- initially, eps-closure(nfa.initial) is the only state in the DFAs states
    -- and it's unmarked
    --
    state_rep first = build_eps_closure(nfa, {nfa[NFA_INITIAL]})
    unmarked_states = {first}
    --
    -- the initial dfa state
    --
    dfa_state_num = {first}
    dfa = {{},{},1,{}} -- {DFA_TRANS_TABLE,DFA_TRANS_TARGET,DFA_START,DFA_FINAL}

    while length(unmarked_states) do
        --
        -- Take out one unmarked state and mark it (remove from the unmarked set,
        -- insert into the marked set)
        --
        state_rep a_state = unmarked_states[1]
        unmarked_states = unmarked_states[2..$]
        if find(a_state,marked_states) then ?9/0 end if
        marked_states = append(marked_states,a_state)
        --
        -- If this state contains the NFA's final state, add it to the DFA's set
        -- of final states
        --
        if find(nfa[NFA_FINAL],a_state) then
            k = find(a_state,dfa_state_num)
            if k=0 then ?9/0 end if
            dfa[DFA_FINAL] &= k
        end if
        --
        -- for each input symbol the nfa knows
        --
        for i=1 to length(nfa[NFA_INPUTS]) do
            input inp_i = nfa[NFA_INPUTS][i]
            --
            -- next state
            --
            sequence mset = NFA_move(nfa, a_state, inp_i)
            state_rep next = build_eps_closure(nfa, mset)
            --
            -- if we haven't examined this state before, add it to the unmarked
            -- states, and make up a new number for it
            --
            if not find(next,unmarked_states)
            and not find(next,marked_states) then
                unmarked_states = append(unmarked_states,next)
                dfa_state_num = append(dfa_state_num,next)
            end if

            state at = find(a_state,dfa_state_num)
            state n = find(next,dfa_state_num)
            dfa[DFA_TRANS_TABLE] = append(dfa[DFA_TRANS_TABLE],{at,inp_i})
            dfa[DFA_TRANS_TARGET] = append(dfa[DFA_TRANS_TARGET],n)
        end for
    end while
    if 0 then
        pp(dfa_state_num,{pp_Nest,1})
        ?nfa[NFA_FINAL]
    end if
    return dfa
end function

