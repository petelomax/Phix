--
-- permute.e
-- Copyright Pete Lomax 2004
-- see demo\permutes.exw
--

global function permute(integer n, sequence s, bool bFast=false)
--
-- return the nth permute of the given set.
-- n should be an integer in the range 1 to factorial(length(s))
-- For bFast=false only:
-- results in lexicographic order, assuming that s is on entry.
--
    integer l = length(s)
    sequence res = repeat(iff(string(s)?' ':0),l)
    n -= 1
    if bFast then
        -- (old and somewhat quirky algorithm, but faster)
        if not string(s) then
            s = deep_copy(s,1)  -- (top level only needed)
        end if
        for i=l to 1 by -1 do 
            integer w = remainder(n,i)+1
--          w = (i+1)-w         -- (slightly better ordering)
            res[i] = s[w]
            s[w] = s[i]
--          s[w..i-1] = s[w+1..i]   -- (ever so "")
            n = floor(n/i)
        end for
    else
        -- (new[1.0.2] lexicographic position order algorithm)
        sequence tags = tagset(l)
        for i=l to 1 by -1 do 
            integer fi = factorial(i-1),
                    w = floor(n/fi)+1,
                    tw = tags[w]
            res[-i] = s[tw]
--          tags[w..w] = {} -- noticeably slower than:
            l -= 1
            for j=w to l do
                tags[j] = tags[j+1]
            end for
            n = remainder(n,fi)
        end for
    end if
    return res
end function

global function permutes(sequence s, integer cb=0, k=length(s))
    -- eg permutes("123") ==> {"123","132","213","231","312","321"}
    integer l = length(s)
--  assert(k<=l)
    sequence res = {}
--  if l then
    if k<=l then
        sequence tags = custom_sort(s,tagset(l)),
                 uses = repeat(1,l) -- duplicates
        integer p = tags[1], high_p = 1, dups = 0,
         maxparam = iff(cb=0?0:get_routine_info(cb,false)[1])
        object sp = s[p] -- (scratch)
        -- unify duplicate elements, eg "abbc" -> 1224
        for i=2 to l do
            integer ti = tags[i]
            if s[ti]==sp then
                tags[i] = p
                uses[p] += 1
                dups += 1
            else
                sp = s[ti]
                p = ti
            end if
        end for
        tags = sort(tags) -- (first instance order)
        atom kth = 1,  -- (this is the kth permute)
             tk = factorial(l)  -- (of tk in total)
        if dups then -- ((and k=l[ength(s)]...?))
            for ui in uses do
                if ui>1 then tk /= factorial(ui) end if
            end for
--      elsif k<length(s) then -- ((and dups=0...?))
        elsif k<l then -- ((and dups=0...?))
--          tk /= factorial(length(s)-k)
            tk /= factorial(l-k)
--      else -- ((...?))
--          tk = -1 -- ((or perhaps full below less the extract bit...?))
        end if
        while true do
            if high_p<=k then
                sequence sk = extract(s,tags[1..k])
                if cb=0 then
                    res = append(res,sk)
                else
                    bool quit = not iff(maxparam=1?cb(sk):
                                    iff(maxparam=2?cb(sk,kth):
                                                   cb(sk,kth,tk)))
                    if quit then return {} end if
                end if
            end if
            -- find a {p,l} to swap, eg 1234 -> 1243
            --   or (see reverse below) 1432 -> 2431
            p = l-1
            while p and tags[p]>=tags[p+1] do
                p -= 1
            end while
            if p<1 then -- (all done, eg 4321)
--              assert(kth==tk or (dups!=0 and k<length(s)))
                assert(kth==tk or (dups!=0 and k<l))
                if cb=0 then exit end if
                return {}
            end if
            integer tp = tags[p]
            while tags[l]<=tp do
                l -= 1
            end while
            assert(p<l)
            high_p = p
            tags[p] = tags[l]
            tags[l] = tp
            -- eg 1432 -> 2431 above -> 2134 (lowest lexico post-p)
--          tags[p+1..$] = reverse(tags[p+1..$]) [, but respecting k]
            p += 1  
            l = length(s)
            while p<l and p<=k do
                tp = tags[p]
                tags[p] = tags[l]
                tags[l] = tp
                p += 1
                l -= 1
            end while
            if high_p<=k then -- ((and tk!=-1...?))
                kth += 1
                assert(kth<=tk)
            end if
            l = length(s)
        end while   
    end if
    return res
end function

global function combinations(sequence s, integer k, at=1, sequence res={}, part="")
    --
    -- eg join(combinations("123",2),',') ==> "12,13,23"
    --
    if k=0 then -- got a full set
        res = append(res,part)
    else
        if res={} then s=unique(s) end if
        if at+k-1<=length(s) then
            -- get all combinations with and without the next item:
            res = combinations(s,k-1,at+1,res,append(deep_copy(part),s[at]))
            res = combinations(s,k,at+1,res,part)
        end if
    end if
    return res
end function 

global function combinations_with_repetitions(sequence s, integer k=length(s), at=1, sequence res={}, part="")
    --
    -- eg join(combinations_with_repetitions("123",2),',') ==> "11,12,13,22,23,33"
    --
    if length(part)=k then
        res = append(res,part)
    else
        if res={} then s=unique(s) end if
        for at,sat in s from at do
            res = combinations_with_repetitions(s,k,at,res,append(deep_copy(part),sat))
        end for
    end if
    return res
end function

--/*
From rc:Permutations with repetitions:
The task is equivalent to simply counting in base=length(set), from 1 to power(base,n).
Asking for the 0th permutation just returns the total number of permutations (ie "").
Results can be generated in any order, hence early termination is quite simply a non-issue.
[AH, so probably not that helpful... what I want is non-descending permutations...]
with javascript_semantics
function permrep(sequence set, integer n, idx=0)
    integer base = length(set),
            nperm = power(base,n)
    if idx=0 then
        -- return the number of permutations
        return nperm
    end if
    -- return the idx'th [1-based] permutation
    if idx<1 or idx>nperm then ?9/0 end if
    idx -= 1    -- make it 0-based
    sequence res = ""
    for i=1 to n do
        res = prepend(res,set[mod(idx,base)+1])
        idx = floor(idx/base)   
    end for
    if idx!=0 then ?9/0 end if -- sanity check
    return res
end function

-- Some slightly excessive testing:

procedure show_all(sequence set, integer n, lo=1, hi=0)
    integer l = permrep(set,n)
    if hi=0 then hi=l end if
    sequence s = repeat(0,l)
    for i=1 to l do
        s[i] = permrep(set,n,i)
    end for
    string mx = iff(hi=l?"":sprintf("/%d",l)),
          pof = sprintf("perms[%d..%d%s] of %v",{lo,hi,mx,set})
    printf(1,"Len %d %-35s: %v\n",{n,pof,shorten(s[lo..hi],"",3)})
end procedure
 
show_all("123",1)
show_all("123",2)
show_all("123",3)
show_all("456",3)
show_all({1,2,3},3)
show_all({"bat","fox","cow"},2)
show_all("XYZ",4,31,36)
 
integer l = permrep("ACKR",5)
for i=1 to l do
    if permrep("ACKR",5,i)="CRACK" then -- 455
        printf(1,"Len 5 perm %d/%d of \"ACKR\" : CRACK\n",{i,l})
        exit
    end if
end for
--The 590th (one-based) permrep is KCARC, ie reverse(CRACK), matching the 589 result of 0-based idx solutions
printf(1,"reverse(permrep(\"ACKR\",5,589+1):%s\n",{reverse(permrep("ACKR",5,590))})
Output:
Len 1 perms[1..3] of "123"               : {"1","2","3"}
Len 2 perms[1..9] of "123"               : {"11","12","13","...","31","32","33"}
Len 3 perms[1..27] of "123"              : {"111","112","113","...","331","332","333"}
Len 3 perms[1..27] of "456"              : {"444","445","446","...","664","665","666"}
Len 3 perms[1..27] of {1,2,3}            : {{1,1,1},{1,1,2},{1,1,3},"...",{3,3,1},{3,3,2},{3,3,3}}
Len 2 perms[1..9] of {"bat","fox","cow"} : {{"bat","bat"},{"bat","fox"},{"bat","cow"},"...",{"cow","bat"},{"cow","fox"},{"cow","cow"}}
Len 4 perms[31..36/81] of "XYZ"          : {"YXYX","YXYY","YXYZ","YXZX","YXZY","YXZZ"}
Len 5 perm 455/1024 of "ACKR" : CRACK
reverse(permrep("ACKR",5,589+1):CRACK
--*/

--probably dead...
--DEV these are non-unique/"impossible" combinations... more thought required<br>
--/*
global function combination(integer k, n, sequence set)
--
-- return the kth combination of length n items from the given set.
-- k should be an integer in the range 1 to power(length(set),n)
--
    integer l = length(set)
    if k<1 or k>power(l,n) then ?9/0 end if
    k -= 1
    sequence res = repeat(' ',n)
    for i=n to 1 by -1 do
        integer m = remainder(k,l)+1
        res[i] = set[m]
        k = floor(k/l)
    end for
    return res
end function

--/!*
sequence set = "123",
         res = repeat(0,power(length(set),3))
for k=1 to length(res) do
    res[k] = combination(k,3,set)
end for
?length(res)
puts(1,join_by(res,1,9))
puts(1,"===\n")
puts(1,join_by(res,3,9))
--*!/

--/*
27
111   112   113   121   122   123   131   132   133
211   212   213   221   222   223   231   232   233
311   312   313   321   322   323   331   332   333
===
111   121   131   211   221   231   311   321   331
112   122   132   212   222   232   312   322   332
113   123   133   213   223   233   313   323   333
"done"

sequence set = "1234",
         res = repeat(0,power(length(set),3))
for k=1 to length(res) do
    res[k] = combination(k,3,set)
end for
?length(res)
puts(1,join_by(res,1,16))
puts(1,"===\n")
puts(1,join_by(res,4,16))
--*/

--/*
sequence set = "1234",
         res = repeat(0,power(length(set),4))
for k=1 to length(res) do
    res[k] = combination(k,4,set)
end for
?length(res)
puts(1,join_by(res,1,16))
puts(1,"===\n")
puts(1,join_by(res,16,16))
--*/

--*/
