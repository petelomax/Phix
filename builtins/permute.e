--
-- permute.e
-- Copyright Pete Lomax 2004
-- see demo\permutes.exw
--

global function permute(integer n, sequence set, bool bFast=false)
--
-- return the nth permute of the given set.
-- n should be an integer in the range 1 to factorial(length(set))
-- For bFast=false only:
-- results in lexicographic order, assuming that s is on entry.
--
    integer l = length(set)
    sequence res = repeat(iff(string(set)?' ':0),l)
    n -= 1
    if bFast then
        -- (old and somewhat quirky algorithm, but faster)
        if not string(set) then
            set = deep_copy(set,1)  -- (top level only needed)
        end if
        for i=l to 1 by -1 do 
            integer w = remainder(n,i)+1
--          w = (i+1)-w         -- (slightly better ordering)
            res[i] = set[w]
            set[w] = set[i]
--          set[w..i-1] = set[w+1..i]   -- (ever so "")
            n = floor(n/i)
        end for
    else
        -- (new[1.0.2] lexicographic position order algorithm)
        sequence tags = tagset(l)
        for i=l to 1 by -1 do 
            integer fi = factorial(i-1),
                    w = floor(n/fi)+1,
                    tw = tags[w]
            res[-i] = set[tw]
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
    integer l = length(s)
    assert(k<=l)
    sequence res = {}
    if l then
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
        if dups then
            for i=1 to length(uses) do
                integer ui = uses[i]
                if ui>1 then tk /= factorial(ui) end if
            end for
        elsif k<length(s) then -- ((and dups=0...?))
            tk /= factorial(length(s)-k)
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
                assert(kth==tk or (dups!=0 and k<length(s)))
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
            if high_p<=k then
                kth += 1
                assert(kth<=tk)
            end if
            l = length(s)
        end while   
    end if
    return res
end function

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
