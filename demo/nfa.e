-- This code is in the public domain - feel free to do anything you
-- wish with it.
--
-- Eli Bendersky (eliben@gmail.com)
--

global type state(integer s)
    return s>=1
end type

global enum EPS = -1, NONE = 0

global type input(integer ch)
    return ch=EPS or ch=and_bits(ch,#FF)
end type

----------------------------------------------------------------
--
-- Implementation of the NFA class
--

--constant empty_row = {size,NONE}

global enum NFA_SIZE, NFA_INITIAL, NFA_FINAL, NFA_TRANS_TABLE, NFA_INPUTS

function is_legal_state(sequence this, state s)
    return s<=this[NFA_SIZE]
end function

global type NFA(object nfa)
    return sequence(nfa)
       and length(nfa)=5
       and integer(nfa[NFA_SIZE])
       and is_legal_state(nfa,nfa[NFA_INITIAL])
--     and integer(nfa[NFA_INITIAL])
--     and nfa[NFA_INITIAL]>=1 and nfa[NFA_INITIAL]<=size
       and is_legal_state(nfa,nfa[NFA_FINAL])
--     and integer(nfa[NFA_FINAL])
--     and nfa[NFA_FINAL]>=1 and nfa[NFA_FINAL]<=size
       and sequence(nfa[NFA_TRANS_TABLE])
       and sequence(nfa[NFA_INPUTS])
end type

function newNFA(integer size, state initial, state final)
    sequence empty_row = repeat(NONE,size)
    sequence trans_table = repeat(empty_row,size)
    sequence inputs = ""
    NFA this = {size,initial,final,trans_table,inputs}
    return this
end function

function add_trans(NFA this, state sfrom, state sto, input in)
    if not is_legal_state(this,sfrom) then ?9/0 end if
    if not is_legal_state(this,sto) then ?9/0 end if

    this[NFA_TRANS_TABLE][sfrom][sto] = in

    if in != EPS
    and not find(in,this[NFA_INPUTS]) then
        this[NFA_INPUTS] &= in
    end if
    return this
end function

function shift_states(NFA this, integer shift)

    if shift>=1 then
        integer new_size = this[NFA_SIZE] + shift
        sequence empty_row = repeat(NONE,new_size)
        sequence new_trans_table = repeat(empty_row,new_size)
        for i=1 to this[NFA_SIZE] do
            for j=1 to this[NFA_SIZE] do
                new_trans_table[i+shift][j+shift] = this[NFA_TRANS_TABLE][i][j]
            end for
        end for
        this[NFA_SIZE] = new_size
        this[NFA_INITIAL] += shift
        this[NFA_FINAL] += shift
        this[NFA_TRANS_TABLE] = new_trans_table
    end if
    return this
end function

function fill_states(NFA this, NFA other)
    if this[NFA_SIZE]<=other[NFA_SIZE] then ?9/0 end if
    for i=1 to other[NFA_SIZE] do
        for j=1 to other[NFA_SIZE] do
            this[NFA_TRANS_TABLE][i][j] = other[NFA_TRANS_TABLE][i][j]
        end for
    end for
    for i=1 to length(other[NFA_INPUTS]) do
        input ch = other[NFA_INPUTS][i]
        if not find(ch,this[NFA_INPUTS]) then
            this[NFA_INPUTS] &= ch
        end if
    end for
    return this
end function

function append_empty_state(NFA this)
    --
    -- append a new row (already with a larger size)
    --
    sequence empty_row = repeat(NONE,this[NFA_SIZE]+1)
    this[NFA_TRANS_TABLE] = append(this[NFA_TRANS_TABLE],empty_row)

    -- append a new column
    --
    for i=1 to this[NFA_SIZE] do
        this[NFA_TRANS_TABLE][i] &= NONE
    end for

    this[NFA_SIZE] += 1
    return this
end function

global procedure show(NFA this)
    printf(1,"This NFA has %d states (1 - %d)\n",this[NFA_SIZE])
    printf(1,"The initial state is %d\n",{this[NFA_INITIAL]})
    printf(1,"The final state is %d\n",{this[NFA_FINAL]})

    for sfrom=1 to this[NFA_SIZE] do
        for sto=1 to this[NFA_SIZE] do
            input in = this[NFA_TRANS_TABLE][sfrom][sto]
            if in!=NONE then
                printf(1,"Transition from %d to %d on input %s\n",{sfrom,sto,iff(in==EPS?"EPS":in&"")})
            end if
        end for
    end for

    printf(1,"\n")
end procedure

global function NFA_move(NFA this, sequence states, input inp)
sequence result = {}
    --
    -- for each state in the set of states
    --
    for i=1 to length(states) do
        --
        -- for each transition from this state
        --
        state si = states[i]
        
        for j=1 to length(this[NFA_TRANS_TABLE][si]) do
            --
            -- if the transition is on input inp, add it to the resulting set
            --
            if this[NFA_TRANS_TABLE][si][j]=inp then
                result &= j
            end if
        end for
    end for

    result = sort(result)
    return result
end function

----------------------------------------------------------------
--
-- NFA building functions
--
-- Using Thompson Construction, build NFAs from basic inputs or
-- compositions of other NFAs.
--


-- Builds a basic, single input NFA
--
global function build_nfa_basic(input in)
    NFA basic = newNFA(2, 1, 2)
    basic = add_trans(basic, 1, 2, in)
    return basic
end function


-- Builds an alternation of nfa1 and nfa2 (nfa1|nfa2)
--
global function build_nfa_alter(NFA nfa1, NFA nfa2)

    -- How this is done: the new nfa must contain all the states in
    -- nfa1 and nfa2, plus a new initial and final states.
    -- First will come the new initial state, then nfa1's states, then
    -- nfa2's states, then the new final state
    --

    -- make room for the new initial state
    nfa1 = shift_states(nfa1,1)

    -- make room for nfa1
    nfa2 = shift_states(nfa2, nfa1[NFA_SIZE])

    -- create a new nfa and initialize it with (the shifted)
    -- nfa2
    --
    NFA new_nfa = nfa2

    -- nfa1's states take their places in new_nfa
    --
    new_nfa = fill_states(new_nfa, nfa1)

    -- Set new initial state and the transitions from it
    --
    new_nfa = add_trans(new_nfa, 1, nfa1[NFA_INITIAL], EPS)
    new_nfa = add_trans(new_nfa, 1, nfa2[NFA_INITIAL], EPS)
    new_nfa[NFA_INITIAL] = 1

    -- Make up space for the new final state
    --
    new_nfa = append_empty_state(new_nfa)

    -- Set new final state
    --
    new_nfa[NFA_FINAL] = new_nfa[NFA_SIZE]
    new_nfa = add_trans(new_nfa, nfa1[NFA_FINAL], new_nfa[NFA_FINAL], EPS)
    new_nfa = add_trans(new_nfa, nfa2[NFA_FINAL], new_nfa[NFA_FINAL], EPS)

    return new_nfa
end function


-- Builds a concatenation of nfa1 and nfa2 (nfa1nfa2)
--
global function build_nfa_concat(NFA nfa1, NFA nfa2)

    -- How this is done: First will come nfa1, then nfa2 (its
    -- initial state replaced with nfa1's final state)
    --
    nfa2 = shift_states(nfa2, nfa1[NFA_SIZE]-1)

    -- create a new nfa and initialize it with (the shifted)
    -- nfa2
    --
    NFA new_nfa = nfa2

    -- nfa1's states take their places in new_nfa
    -- note: nfa1's final state overwrites nfa2's initial state,
    -- thus we get the desired merge automagically (the transition
    -- from nfa2's initial state now transits from nfa1's final state)
    --
    new_nfa = fill_states(new_nfa, nfa1)

    -- set the new initial state (the final state stays nfa2's final state,
    -- and was already copied)
    --
    new_nfa[NFA_INITIAL] = nfa1[NFA_INITIAL]

    return new_nfa
end function


-- Builds a star (kleene closure) of nfa (nfa*)
--
global function build_nfa_star(NFA nfa)

    -- How this is done: First will come the new initial state,
    -- then nfa, then the new final state
    --

    -- make room for the new initial state
    --
    nfa = shift_states(nfa,1)

    -- make room for the new final state
    --
    nfa = append_empty_state(nfa)

    -- add new transitions
    --
    nfa = add_trans(nfa, nfa[NFA_FINAL], nfa[NFA_INITIAL], EPS)
    nfa = add_trans(nfa, 1, nfa[NFA_INITIAL], EPS)
    nfa = add_trans(nfa, nfa[NFA_FINAL], nfa[NFA_SIZE], EPS)
    nfa = add_trans(nfa, 1, nfa[NFA_SIZE], EPS)

    nfa[NFA_INITIAL] = 1
    nfa[NFA_FINAL] = nfa[NFA_SIZE]

    return nfa
end function


