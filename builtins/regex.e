--
-- builtins\regex.e
-- ================
--
-- Routines for handling regular expressions.
--

global constant RE_PIKEVM           = #001, 
                RE_RECURSIVE        = #002, 
                RE_EARLY_EXIT       = #004, 
                RE_BACKREFERENCES   = #008, 
                RE_CASEINSENSITIVE  = #010,
                RE_DOTMATCHESNL     = #020

integer options = RE_PIKEVM -- (RE_EARLY_EXIT etc disabled by default)

integer r_ErrorHandler = NULL

global procedure regex_options(integer opts=RE_PIKEVM, integer rErrHand=NULL)
    if and_bits(opts,RE_PIKEVM) then
        -- prohibit both RE_PIKEVM and RE_RECURSIVE
        if and_bits(opts,RE_RECURSIVE) then ?9/0 end if
    elsif not and_bits(opts,RE_RECURSIVE) then
        -- allow RE_BACKREFERENCES (only/etc) to retain existing engine
        opts = or_bits(opts,and_bits(options,RE_PIKEVM+RE_RECURSIVE))
    end if
    options = opts
    r_ErrorHandler = rErrHand
end procedure

-- slightly faster (but not by alot):
--/*
enum JMP, SPLIT, MATCH, SAVE, BOL, EOL, CHAR, CLASS, WORD_BOUND, BKREF,
     SEQ, REGEX, GROUP, GROUP_END, SPLIT_FIXUP1, SPLIT_FIXUP2, 
     OPT, OPT_MN, OPT_FIXUP
--*/
-- much easier to debug:
--/!*
constant JMP = "JMP",
         SPLIT = "SPLIT",
         MATCH = "MATCH",
         SAVE = "SAVE",
         BOL = "BOL",
         EOL = "EOL",
         CHAR = "CHAR",
         CLASS = "CLASS",
         WORD_BOUND = "WORD_BOUND",
         BKREF = "BKREF",
         SEQ = "SEQ",
         REGEX = "REGEX",
         GROUP = "GROUP",
         GROUP_END = "GROUP_END",
         SPLIT_FIXUP1 = "SPLIT_FIXUP1",
         SPLIT_FIXUP2 = "SPLIT_FIXUP2",
         OPT = "OPT",
         OPT_MN = "OPT_MN",
         OPT_FIXUP = "OPT_FIXUP"
--*!/

constant nil = 0

function inclass(integer ch, bool negated, string pairs)
bool inclass
    for i=1 to length(pairs) by 2 do
        if and_bits(options,RE_CASEINSENSITIVE) then
            ch = upper(ch)
        end if
        inclass = ((ch>=pairs[i]) and (ch<=pairs[i+1]))
        if inclass then exit end if
    end for
    return (inclass!=negated)
end function

--One possibility (for better backreference handling) is to ditch mark[] in addthread, and instead keep all
--distinct {pc,saved}, with a potentially exponential rise in memory overhead. Let me know how you get on.

sequence mark
sequence reactivate

with trace
--DEV this is a candidate for a nested function:
function addthread(sequence q, sequence code, integer gen, integer pc, string input, integer sp, sequence saved)
    if mark[pc]!=gen then
        mark[pc] = gen
        switch code[pc] do
            case JMP:
                q = addthread(q, code, gen, code[pc+1], input, sp, saved)
            case SPLIT:
                q = addthread(q, code, gen, code[pc+1], input, sp, saved)
                q = addthread(q, code, gen, code[pc+2], input, sp, saved)
            case SAVE:
                integer k = code[pc+1]
--p2js:
                saved = deep_copy(saved)
                saved[k] = sp
                q = addthread(q, code, gen, pc+2, input, sp, saved)
            case BOL:
                if sp=1 then
                    q = addthread(q, code, gen, pc+1, input, sp, saved)
                end if
            case EOL:
                if sp=length(input)+1 then
                    q = addthread(q, code, gen, pc+1, input, sp, saved)
                end if
            case WORD_BOUND:
                bool word_bound, negated = code[pc+1]
                if sp=1 then
                    word_bound = inclass(input[sp],0,"azAZ09__")
                else
                    word_bound = inclass(input[sp-1],0,"azAZ09__")
                    if word_bound then
                        word_bound = sp>length(input) or inclass(input[sp],1,"azAZ09__")
                    else
                        word_bound = sp<=length(input) and inclass(input[sp],0,"azAZ09__")
                    end if
                end if
                if word_bound!=negated then
                    q = addthread(q, code, gen, pc+2, input, sp, saved)
                end if
            case BKREF:
                integer bn = code[pc+1]*2+1
                integer {bs,be} = saved[bn..bn+1]
                integer l = be-bs
                if bs>=1 and l>=1
                and sp+l-1<=length(input)
                and input[sp..sp+l-1]=input[bs..be-1] then
                    if length(reactivate)<l then
                        reactivate &= repeat({},l-length(reactivate))
                    end if
--p2js:
                    reactivate = deep_copy(reactivate)
                    reactivate[l] = append(reactivate[l],{pc+2, saved})
                end if
            else
                q = append(q,{pc, saved})
        end switch
    end if
    return q
end function

function pikevm_execute(sequence code, string input, integer strtndx)
    sequence matched = {}
    sequence run = {}
    sequence rdy = {}
    integer pc
    integer gen = 1
    integer sp = strtndx
    sequence saved = repeat(0,code[2]*2)
    reactivate = {}
    mark = repeat(0,length(code))
    run = addthread(run, code, gen, 4, input, sp, saved)
    while length(run) or length(reactivate) do
        gen += 1
        for i=1 to length(run) do
            {pc,saved} = run[i]
            switch code[pc] do
                case CHAR:
                    if sp<=length(input) then
                        integer ch = input[sp]
                        if and_bits(options,RE_CASEINSENSITIVE) then
                            ch = upper(ch)
                        end if
                        if ch==code[pc+1] then
                            rdy = addthread(rdy, code, gen, pc+2, input, sp+1, saved)
                        end if
                    end if
                case CLASS:
                    if sp<=length(input)
                    and inclass(input[sp],code[pc+1],code[pc+2]) then
                        rdy = addthread(rdy, code, gen, pc+3, input, sp+1, saved)
                    end if
                case MATCH:
                    matched = saved
                    exit
                else
                    ?9/0
            end switch
        end for
        if and_bits(options,RE_EARLY_EXIT) then
            if length(matched) then exit end if
        end if
        run = rdy
        if length(reactivate) then
            rdy = {}
            sequence r1 = reactivate[1]
            reactivate = reactivate[2..$]
            for i=1 to length(r1) do
                {pc,saved} = r1[i]
                -- aside: ps+2 is needed, I think, to cover the sp+=1 below
                --        and because we're adding to rdy not run, but yes,
                --        it surprised me at first too.
                rdy = addthread(rdy, code, gen+1, pc, input, sp+2, saved)
            end for
        else
            rdy = {}
        end if
        if sp>length(input) then exit end if
        sp += 1
    end while
    return matched
end function

constant MAXTHREAD = 1000
--constant MAXTHREAD = 20

function backtrackingvm(sequence code, string input, integer strtndx)
    integer pc, sp
    sequence saved = repeat(0,code[2]*2)
    sequence ready = {{4,strtndx,saved}}
    int nready = 1
    
    -- run threads in stack order
    while nready>0 do
        -- pop state for next thread to run
        {pc, sp, saved} = ready[$]
        ready = ready[1..$-1]
        nready -= 1  
        while 1 do
            object opcode = code[pc]
            switch opcode do
                case CHAR:
                    if sp>length(input) then exit end if
                    integer ch = input[sp]
                    if and_bits(options,RE_CASEINSENSITIVE) then
                        ch = upper(ch)
                    end if
                    if ch!=code[pc+1] then exit end if
                    sp += 1
                    pc += 2
                case MATCH:
                    return saved
                case JMP:
                    pc = code[pc+1]
                case SPLIT:
                    if nready >= MAXTHREAD then
                        printf(1, "\nregexp overflow\n\n");
                        return {}
                    end if
                    -- queue new thread
                    ready = append(ready,{code[pc+2],sp,saved})
                    nready += 1
                    pc = code[pc+1] -- continue current thread
                case SAVE:
                    integer k = code[pc+1]
--p2js:
                    saved = deep_copy(saved)
                    saved[k] = sp
                    pc += 2
                case CLASS:
                    if sp>length(input)
                    or not inclass(input[sp],code[pc+1],code[pc+2]) then
                        exit
                    end if
                    sp += 1
                    pc += 3
                case BKREF:
                    integer bn = code[pc+1]*2+1
                    integer {bs,be} = saved[bn..bn+1]
                    integer l = be-bs
                    --RE_CASEINSENSITIVE(NO)
                    if bs<1 or l<1
                    or sp+l-1>length(input)
                    or input[sp..sp+l-1]!=input[bs..be-1] then
                        exit
                    end if
                    sp += l
                    pc += 2
                case BOL:
                    if sp!=1 then exit end if
                    pc += 1
                case EOL:
                    if sp!=length(input)+1 then exit end if
                    pc += 1
                case WORD_BOUND:
                    bool word_bound, negated = code[pc+1]
                    if sp=1 then
                        word_bound = inclass(input[sp],0,"azAZ09__")
                    else
                        word_bound = inclass(input[sp-1],0,"azAZ09__")
                        if word_bound then
                            word_bound = sp>length(input) or inclass(input[sp],1,"azAZ09__")
                        else
                            word_bound = sp<=length(input) and inclass(input[sp],0,"azAZ09__")
                        end if
                    end if
                    if word_bound=negated then
                        exit
                    end if
                    pc += 2
                default:
                    ?9/0
            end switch
        end while
    end while
    return {}
end function

--
-- Parse routines
--

procedure Abort(string msg, string src, integer idx)
-- error parsing the regular expression
    if r_ErrorHandler!=NULL then
        call_proc(r_ErrorHandler,{msg,src,idx})
    else
        printf(1,"%s\n%s^%s\n",{src,repeat(' ',idx-1),msg})
    end if
end procedure

function isnum(integer ch)
    return (ch>='0' and ch<='9')
end function

function nibble(string src, integer idx)
    if idx<=length(src) then
        integer ch = upper(src[idx])
        if ch>='0' and ch<='9' then return {idx,ch-'0'} end if
        if ch>='A' and ch<='F' then return {idx,ch-'A'+10} end if
    end if
    Abort("hex digit expected", src, idx)
    return {0,0}
end function

function hexch(string src, integer idx)
--
-- parse a hex character such as \x1B -> 27 (ESC), 
-- the \x having already been detected.
--
integer n = 0, nib
    for i=1 to 2 do
        idx += 1
        {idx,nib} = nibble(src, idx)
        if idx=0 then exit end if
        n = n*16+nib
    end for
    return {idx,n}
end function

function nextch(string src, integer idx, bool allow_class=true)
    idx += 1
    if idx>length(src) then
        Abort("incomplete character class", src, idx)
        return {0,'x'}
    end if
    integer ch = src[idx]
    if ch='\\' then
--DEV this needs to handle eg "[\s.!;,:?]" as whitespace plus punctuation...
        idx += 1
        -- (btw: you never actually need to escape any of "(){}$+*?|" )
        if idx>length(src) or not find(src[idx],"\\x[](){}^$+-*?|tnr") then 
            if allow_class=true
            and find(lower(src[idx]),"dsw") then
                return {idx,-1}
            end if
            Abort("invalid escape", src, idx)
            return {0,'x'}
        end if
        ch = src[idx]
        if ch='x' then
            {idx,ch} = hexch(src, idx)
        else
            integer k = find(ch,"0nrt")
            if k then
                if k=4 then
                    ch = 9 -- Tab (\t is illegal under p2js)
                else
                    ch = "0\n\r"[k]
                end if
            end if
        end if
    end if
    return {idx, ch}
end function

integer group_number = 0    -- (should only be modified during parsing)

function chr(string src, integer idx)
--
-- parse a single (escaped) character
--
sequence res
integer ch
bool negated

    if idx<=length(src) then
        ch = src[idx]
        if ch='.' then
            if and_bits(options,RE_DOTMATCHESNL) then
                res = {CLASS,0,"\x00\xFF"}
            else
                res = {CLASS,1,"\n\n"}
            end if
            idx += 1
            return {idx, res}
        elsif ch='^' then
            res = {BOL}
            idx += 1
            return {idx, res}
        elsif ch='$' then
            res = {EOL}
            idx += 1
            return {idx, res}
        elsif ch='[' then               -- character class
            negated = false
            {idx, ch} = nextch(src, idx, true)
            if idx=0 then return {0,{}} end if
            if ch='^' then
                negated = true
                {idx, ch} = nextch(src, idx, true)
                if idx=0 then return {0,{}} end if
            end if
            string pairs = ""
            while 1 do
                if ch=-1 then
--trace(1) -- seems OK...
                    ch = src[idx]
                    integer k = find(ch,"dswDSW")
--PL 4/7/18 allow eg [^\s]
-- old (and ugly) code:
--                  if negated!=(k>3) then
--                      Abort("invalid escape (negation mismatch)", src, idx)
--                      return {0,{}}
--                  end if
--                  k -= 3*negated
-- new code:
                    if k>3 then
                        -- eg [^\S] ==> [\s], or [\S] ==> [^\s]
                        -- note that eg [^\s-] is valid, but [^\S-] is not
                        -- (we cannot flip negated if any pre/post exists)
                        -- likewise note that say [\S\D] must be written 
                        -- as [^\s\d], and [^\S\D] as [\s\d].
                        if pairs!=""
                        or idx=length(src)
                        or src[idx+1]!=']' then
                            Abort("invalid escape (negation mismatch)", src, idx)
                            return {0,{}}
                        end if
                        k -= 3
                        negated = not negated
                    end if
-- </new code>
--                  pairs &= {"09","  \n\n\r\r\t\t","azAZ09__"}[k]
                    pairs &= {"09","  \n\n\r\r"&9&9,"azAZ09__"}[k]
--                  idx += 1
                    {idx, ch} = nextch(src, idx, true)
                else
                    pairs &= ch
                    integer ch2
                    {idx, ch2} = nextch(src, idx, true)
                    if idx=0 then return {0,{}} end if
                    if ch2='-'
                    and (idx>=length(src) or -- (prevent crash)
                         src[idx+1]!=']') then
                        {idx, ch} = nextch(src, idx, false)
                        pairs &= ch
                        {idx, ch} = nextch(src, idx, true)
                    else
                        pairs &= ch -- 'a'->'aa' (range of 1)
                        ch = ch2
                    end if
                end if
                if ch=']' then exit end if
            end while
            if and_bits(options,RE_CASEINSENSITIVE) then
--SUG (untested)
--              pairs = upper(substitute(pairs,"azAZ","AZ"))
                pairs = upper(pairs)
            end if
            res = {CLASS, negated, pairs}
--?res
            idx += 1
            return {idx,res}
        elsif ch='\\' then
            {idx,ch} = nextch(src, idx, false)
            if idx=0 then return {0,{}} end if
            if ch='d' or ch='D' then
                negated = ch='D'
                res = {CLASS, negated, "09"}
                idx += 1
                return {idx, res}
            elsif ch='s' or ch='S' then
                negated = ch='S'
--              res = {CLASS, negated, "  \n\n\r\r\t\t"}
                res = {CLASS, negated, "  \n\n\r\r"&9&9}
                idx += 1
                return {idx, res}
            elsif ch='h' or ch='H' then
                negated = ch='H'
                -- Unicode space separator?
--              res = {CLASS, negated, "  \t\t"}
                res = {CLASS, negated, "  "&9&9}
                idx += 1
                return {idx, res}
            elsif ch='v' or ch='V' then
                negated = ch='V'
                -- vertical tab, form feed, paragraph or line separator?
                res = {CLASS, negated, "\n\n\r\r"}
                idx += 1
                return {idx, res}
            -- not (yet) handled: \R - one line break (\r\n pair) + all \v...
            elsif ch='w' or ch='W' then
                negated = ch='W'
                res = {CLASS, negated, "azAZ09__"}
                idx += 1
                return {idx, res}
            elsif ch='z' then
                res = {EOL}
                idx += 1
                return {idx, res}
            elsif ch>='1' and ch<='9' then
                if not and_bits(options,RE_BACKREFERENCES) then
                    Abort("not enabled", src, idx)
                    return {0,{}}
                end if
                integer capture_group = ch-'0'
                if capture_group>=group_number then
                    Abort("no such backreference/capture group", src, idx)
                    return {0,{}}
                end if
                res = {BKREF, capture_group}
                idx += 1
                return {idx, res}
            elsif ch='b' or ch='B' then
                negated = ch='B'
                res = {WORD_BOUND, negated}
                idx += 1
                return {idx, res}
            elsif ch='x' then
                {idx,ch} = hexch(src, idx)
                res = {CHAR, ch}
                idx += 1
                return {idx, res}
            else
                integer k = find(ch,"nrt0")
                if k then
--                  ch = "\n\r\t\0"[k]
                    ch = {'\n','\r',9,'\0'}[k]
                elsif not find(ch,".^$[]()") then
                    Abort("unknown escape", src, idx)
                    return {0,{}}
                elsif and_bits(options,RE_CASEINSENSITIVE) then
                    ch = upper(ch)
                end if
                res = {CHAR, ch}
                idx += 1
                return {idx, res}
            end if
        else
            if and_bits(options,RE_CASEINSENSITIVE) then
                ch = upper(ch)
            end if
            res = {CHAR, ch}
            idx += 1
            return {idx, res}
        end if
    end if
    Abort("alphanumeric expected", src, idx)
    return {0,{}}
end function

forward function expression(string src, integer idx)

function atm(string src, integer idx)
--
-- parse a single element or (nested/capture) group
--
sequence res
    if idx<=length(src) and src[idx]=='(' then
        idx += 1
        if idx<=length(src) and src[idx]=='?' then
            idx += 1
            integer ch=iff(idx>length(src)?'x':src[idx])
            if ch!=':' then
                string msg = iff(ch='>'?"atomic groups not supported":
                             iff(ch='<'?"lookbehind not supported":
                             iff(ch='='
                              or ch='!'?"lookahead not supported":
                             iff(ch='('?"conditional expressions not supported"
                                       :"':' expected"))))
                Abort(msg, src, idx)
                return {0,{}}
            end if
            idx += 1
            res = {GROUP,-1,nil} -- non-capture
        else
            group_number += 1
            res = {GROUP,group_number,nil}
        end if
--p2js:
--      {idx,res[3]} = expression(src, idx)
        object r3
        {idx,r3} = expression(src, idx)
        res[3] = r3
        if idx=0 then return {0,{}} end if
        if idx>length(src) or src[idx]!=')' then
            Abort("')' expected", src, idx)
            return {0,{}}
        end if
        idx += 1
    else
        {idx,res} = chr(src, idx)
    end if
    return {idx, res}
end function

function number(string src, integer idx)
--
-- parse a number (isnum(src[idx]) should have already been tested)
--  - specifically for {n,m} repeat qualifiers.
--
integer n = 0
    if not isnum(src[idx]) then
        Abort("number expected", src, idx)
        return {0,{}}
    end if
    while 1 do
        n = n*10+src[idx]-'0'
        idx += 1
        if idx>=length(src) or not isnum(src[idx]) then exit end if
    end while
    return {idx,n}
end function

function atm_repeat(string src, integer idx)
--
-- parse atm() plus any repeat qualifers (?*+{n,m})
--
sequence res
integer ch, optmin, optmax, greedy
    {idx,res} = atm(src,idx)
    if idx=0 then return {0,{}} end if
    if idx<=length(src) then
        ch = src[idx]                           --         min max
        if ch='?'                               --  '?' ->  0  +1
        or ch='*'                               --  '*' ->  0  -1
        or ch='+' then                          --  '+' ->  1  -1
            greedy = 1
            idx += 1
            if idx<=length(src) and src[idx]='?' then
                greedy = 0
                idx += 1
            end if
            optmin = iff(ch='+'?1, 0)
            optmax = iff(ch='?'?1,-1)
            res = {OPT,optmin,optmax,greedy,res}

        elsif ch='{' then
            --
            -- {n} means exactly n
            -- {n,} means at least n (max is -1)
            -- {n,m} means at least n and at most m
            -- A trailing ? specifies non-greedy (superfluous for exact)
            -- The arbitray limit of 1000 should prevent DDOS attacks.
            --          
            --
            idx += 1
            if idx>length(src) or not isnum(src[idx]) then
                Abort("number expected", src, idx)
                return {0,{}}
            end if
            {idx,optmin} = number(src,idx)
            if optmin>1000 then
                Abort("invalid range", src, idx)
                return {0,{}}
            end if
            if idx=0 then return {0,{}} end if
            if idx<=length(src) and src[idx]=',' then
                idx += 1
                if idx<=length(src) 
                and isnum(src[idx]) then                -- {n,m}
                    {idx,optmax} = number(src,idx)
                    if idx=0 then return {0,{}} end if
                    if optmax<optmin
                    or optmax>1000 then
                        Abort("invalid range", src, idx)
                        return {0,{}}
                    end if
                else                                    -- {n,} == n or more
                    optmax = -1
                end if
            else                                        -- {n} == {n,n}
                optmax = optmin         
            end if
            if idx>length(src) or src[idx]!='}' then
                Abort("'}' expected", src, idx)
                return {0,{}}
            end if
            idx += 1
            greedy = 1
            if idx<=length(src) and src[idx]='?' then
                greedy = 0
                idx += 1
            end if
            res = {OPT_MN,optmin,optmax,greedy,res}     -- expanded in concatenate()
        end if
    end if
    return {idx,res}
end function

function concatenate(string src, integer idx)
--
-- parse sequential elements eg abc -> {SEQ,a,b,c}
--
sequence res = {SEQ}, res2
    while 1 do
        {idx,res2} = atm_repeat(src,idx)
        if idx=0 then return {0,{}} end if
        if res2[1]=OPT_MN then
            integer {?,optmin,optmax,greedy} = res2
            if optmax=-1 then
                res &= repeat(res2[5],optmin-1)
                res = append(res,{OPT,1,-1,greedy,res2[5]})
            else
                res &= repeat(res2[5],optmin)
                res &= repeat({OPT,0,1,greedy,res2[5]},optmax-optmin)
            end if
        else
            res = append(res,res2)      
        end if
        if idx>length(src) or find(src[idx],")|*?") then exit end if
    end while
    return {idx,res}
end function

function expression(string src, integer idx)
--
-- parse a full expression (using recursive descent)
--
sequence res
    {idx,res} = concatenate(src,idx)
    if idx=0 then return {0,{}} end if
    if idx<=length(src) then
        integer ch = src[idx]
        if ch='|' then
            sequence left=res, right
            {idx,right} = expression(src,idx+1)
            if idx=0 then return {0,{}} end if
            --aside: this produces a right tree, but I think that's ok...
            res = {SPLIT, left, right}
        elsif not find(ch,")") then
            Abort(") expected", src, idx)
            return {0,{}}
        end if
    end if
    return {idx,res}
end function

--global 
function regexp_parse(string src)
    integer idx
    sequence res
--  group_number = 0
    group_number = 1
    {idx,res} = expression(src,1)
    if idx=0 then return {} end if
    if idx<=length(src) then
        Abort("incomplete parse", src, idx)
        return {}
    end if
    return res
end function

global function regex_compile(string src)
-- (displays a message and) returns {} if the regular expression could not be successfully parsed
    sequence expr = regexp_parse(src)
    if expr={} then 
        return {} 
    end if
    if expr[1]!=SEQ 
    or expr[2][1]!=BOL then
        -- if not anchored, src := ".*?("&src&")" (with RE_DOTMATCHESNL in force)
        --  (ie prefix the expression with a non-greedy .* and add an outer group)
        expr = {SEQ,{OPT, 0,-1,0, {CLASS, 0, "\x00\xFF"}},
                    {GROUP, 1, expr}}
    else
        -- if anchored, src := "("&src&")"
        --  (ie just add an outer group)
        expr = {GROUP, 1, expr}
    end if
    expr = {expr}
--  if 01 then
--      ppOpt({pp_Nest,1,pp_Pause,0})
--      pp(expr[1])
--  end if
    sequence code = {REGEX, 0, 0} -- magic, length, number of instructions
    integer n = 0, i, j
    integer optmin,optmax,greedy
    while length(expr) do
        sequence node = expr[1]
        expr = expr[2..$]
        switch node[1] do

            case CHAR, BOL, EOL, CLASS, WORD_BOUND, BKREF:
                code &= node
                n += 1

            case SEQ:
                expr = node[2..$] & expr

            case GROUP:
                integer n2 = node[2]
                if n2=-1 then
                    expr = {node[3]} & expr
                else
                    code &= {SAVE, n2*2-1}
                    expr = {node[3], {GROUP_END, n2}} & expr
                end if
                n += 1
            case GROUP_END:
                code &= {SAVE, node[2]*2}
                n += 1

            case SPLIT:
                i = length(code)+1
                code &= {SPLIT, i+3, nil}
                n += 1
                expr = {node[2], {SPLIT_FIXUP1, i, node[3]}} & expr
            case SPLIT_FIXUP1:
                i = node[2]
                j = length(code)+1
                code &= {JMP, nil}
                n += 1
                code[i+2] = j+2
                expr = {node[3], {SPLIT_FIXUP2, j}} & expr
            case SPLIT_FIXUP2:
                i = node[2]
                j = length(code)+1
                code[i+1] = j

            case OPT:
                {?,optmin,optmax,greedy} = node
                i = length(code)+1
                if optmin=0 then                        -- '?' or '*'
                    if greedy then
                        code &= {SPLIT, i+3, nil}
                    else
                        code &= {SPLIT, nil, i+3}
                    end if
                    n += 1
                end if
                expr = {node[5], {OPT_FIXUP, optmin, optmax, greedy, i}} & expr
            case OPT_FIXUP:
                {?,optmin,optmax,greedy,i} = node
                if optmin=1 then                        -- '+'
                    j = length(code)+1
                    if greedy then
                        code &= {SPLIT, i, j+3}
                    else
                        code &= {SPLIT, j+3, i}
                    end if
                else                                    -- '?' or '*'
                    if optmax=-1 then                   -- '*'
                        code &= {JMP,i}
                    end if
                    j = length(code)+1
                    code[i+1+greedy] = j
                end if

            else
                ?9/0  -- unknown opcode
        end switch
    end while
    code &= {MATCH}
    n += 1
    code[2] = group_number
    code[3] = n
    return code
end function

global function regex(sequence re, string s,  integer strtndx=1)
--
-- returns {} if no match could be found, otherwise an even-length sequence
-- of capture indexes, eg regex(`(a)(b)`,"ab") yields {1,3,1,2,2,3}. Note
-- that the goup start is the correct index for the first character but the
-- end is always <idx of last character>+1. res[1..2] is the capture for the
-- entire expression, and subsequent pairs for any () as read left-to-right.
--
sequence code
    if string(re) then
        code = regex_compile(re)
        if length(code)=0 then return {} end if
    else
        if re[1]!=REGEX then ?9/0 end if
        code = re
    end if
    sequence m 
    if and_bits(options,RE_PIKEVM) then
        m = pikevm_execute(code, s, strtndx)
    else
        m = backtrackingvm(code,s, strtndx)
    end if
    for i=2 to length(m) by 2 do
        m[i] -= 1
    end for
    return m
end function

--
-- Draft routines
--

global function gsub(sequence re, string target, string rep)
    sequence code = iff(string(re)?regex_compile(re):re),
             m = regex(code,target)
    if length(m) then
        string res = ""
        integer k = find('&',rep)       -- should that be \0?
        while length(target) do
            integer ai = m[1]
            res &= target[1..ai-1]
            if k then
                res &= rep[1..k-1]&target[ai..m[2]]&rep[k+1..$]
            else
                res &= rep
            end if
            --DEV/SUG use the new strtndx arg instead of slicing?
            target = target[m[2]+1..$]
            if length(target)=0 then exit end if
            m = regex(code,target)
            if length(m)=0 then exit end if         
        end while
        res &= target
        return res
    end if
    return target
end function

global function gmatch(sequence re, string target, string res)
    sequence code = iff(string(re)?regex_compile(re):re),
             m = regex(code,target)
    if length(m) then
        for i=1 to length(m)/2-1 do
            integer k = match(sprintf(`\%d`,{i}),res)
            if k!=0 then
                res[k..k+1] = target[m[i*2+1]..m[i*2+2]]
            end if
        end for
        return res
    end if
    return -1
end function

--
-- Testing routines (see test\t63regex.exw)
--

function escape(string s)
-- helper routine for regxep_list
string res = ""
    for i=1 to length(s) do
        integer ch = s[i],
--              k = find(ch,"\n\r\t\0")
                k = find(ch,{'\n','\r',9,'\0'})
        if k then
--DEV p2js/fixme:
--          res &= `\`&"nrt0"[k]
            res &= '\\'&"nrt0"[k]
        elsif ch<' ' or ch>#7E then
--DEV p2js/fixme:
--          res &= sprintf(`\x%02x`,ch)
            res &= sprintf("\\x%02x",ch)
        else
            res &= ch
        end if
    end for
    return res
end function

global procedure regexp_list(sequence code)
sequence labels = repeat(0,length(code))
integer n = length(code)
integer i = 4
    while i<=n do
        switch code[i] do
            case BOL, EOL, MATCH:
                i += 1
            case CHAR, WORD_BOUND, BKREF, SAVE:
                i += 2
            case CLASS:
                i += 3
            case SPLIT:
                if labels[code[i+1]]=0 then
                    labels[code[i+1]] = code[i+1]
                end if
                if labels[code[i+2]]=0 then
                    labels[code[i+2]] = code[i+2]
                end if
                i += 3
            case JMP:
                if labels[code[i+1]]=0 then
                    labels[code[i+1]] = code[i+1]
                end if
                i += 2
            else
                ?9/0
        end switch
    end while

    integer l = 1
    for ldx=1 to length(labels) do
        if labels[ldx]!=0 then
            labels[ldx] = sprintf("L%d",{l})
            l += 1
        end if
    end for
    printf(1,"%03d %-6s%-8s%d, %d\n",{0, "", ".regex", code[2], code[3]})
    i = 4
    while i<=n do
        string r = iff(labels[i]!=0 ? labels[i] & ":" : "")
        switch code[i] do
            case CHAR:
                string ch = escape(code[i+1]&"")
                printf(1,"%03d %-6s%-8s'%s'\n",{i, r, "char", ch})
                i += 2
            case BOL:
                printf(1,"%03d %-6s%s\n",{i, r, "bol(^)"})
                i += 1
            case EOL:
                printf(1,"%03d %-6s%s\n",{i, r, "eol($)"})
                i += 1
            case CLASS:
                string c = iff(code[i+1]?"^","")&escape(code[i+2])
                printf(1,"%03d %-6s%s [%s]\n",{i, r, "class", c})
                i += 3
            case WORD_BOUND:
--              string bound = iff(code[i+1]?"^\\b","\\b")
                string bound = iff(code[i+1]?`^\b`,`\b`)
                printf(1,"%03d %-6s%s %s\n",{i, r, "bound", bound})
                i += 2
            case BKREF:
                printf(1,"%03d %-6s%s %d\n",{i, r, "bkref", code[i+1]})
                i += 2
            case SAVE:
                printf(1,"%03d %-6s%-8s%d\n",{i, r, "save", code[i+1]})
                i += 2
            case MATCH:
                if i<n then ?9/0 end if -- sanity check(??)
                printf(1,"%03d %-6s%s\n",{i, r, "match"})
                i += 1
            case SPLIT:
                printf(1,"%03d %-6s%-8s%s, %s\n",{i, r, "split", labels[code[i+1]], labels[code[i+2]]})
                i += 3
            case JMP:
                printf(1,"%03d %-6s%-8s%s\n",{i, r, "jmp", labels[code[i+1]]})
                i += 2
            else
                ?9/0
        end switch
    end while
end procedure


