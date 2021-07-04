--
-- builtins\match.e
-- ================
--
--  hll implementation of match()  (replaces builtins/VM/pMatch.e)
--
global function match(object needle, sequence haystack, integer start=1, bool case_sensitive=true)
--
-- Try to match needle against some slice of haystack.
-- Return the element number of haystack where the (first) matching slice begins,
-- or 0 if there are none.
--
    integer res, hdx, ln, lh
    object ni, hi
    -- This line, and first parameter being object not sequence, is not Euphoria compliant.
    --  (Euphoria gives error "first argument of match() must be a sequence")
    if atom(needle) then
        return find(needle,haystack,start)
    end if
    if not case_sensitive then
        needle = lower(needle)
        haystack = lower(haystack)
    end if
    res = start
    ln = length(needle)
    -- This line is also not RDS compliant
    --  (RDS gives error "first argument of match() must be a non-empty sequence")
    if ln=0 then return 0 end if
    if start<1 then return 0 end if
    lh = length(haystack)
    while 1 do
        if res+ln-1>lh then return 0 end if
        for i=1 to ln do
            ni = needle[i]
            hdx = i+res-1
            hi = haystack[hdx]
            if not equal(ni,hi) then
                if integer(ni) then
                    -- scan for ni later on in haystack
                    -- eg needle=13131...,
                    --    haystack=1313x31...
                    -- with needle[5]=1, scanning fwd two places to haystack[7]=1
                    -- gives the earliest point worth re-starting from.
                    -- if ni does not occur anywhere later in haystack, then
                    -- clearly there will be no match anywhere.
                    for j=hdx+1 to lh+1 do
                        res += 1
                        if j>lh then return 0 end if
                        hi = haystack[j]
                        if equal(ni,hi) then exit end if
                    end for
                    exit
                elsif integer(hi) then
                    -- scan for hi earlier on in needle
                    -- eg needle=131313x...,
                    --    haystack=1313131...
                    -- with haystack[7]=1, scanning back two places to needle[5]=1
                    -- gives the earliest point worth re-starting from.
                    -- if haystack[7] does not occur anywhere earlier in needle,
                    -- then clearly we should restart from haystack[8].
                    for j=i-1 to 0 by -1 do
                        res += 1
                        if j=0 then exit end if
                        ni = needle[j]
                        if equal(hi,ni) then exit end if
                    end for
                    exit
                else
                    res += 1
                    exit
                end if
            end if
            if i=ln then return res end if
        end for
    end while
end function

global function rmatch(object needle, sequence haystack, integer start=length(haystack), bool case_sensitive=true)

    -- This line, and first parameter being object not sequence, is not Euphoria compliant.
    --  (Euphoria gives error "first argument of match() must be a sequence")
    if atom(needle) then
        return rfind(needle,haystack,start)
    end if

    integer ln = length(needle),
            lh = length(haystack)

    if ln=0
    or start=0
    or start>lh
    or lh+start<1 then
        return 0
    end if

    if not case_sensitive then
        needle = lower(needle)
        haystack = lower(haystack)
    end if

    if start<1 then
        start = lh+start
    end if

    if start+ln-1>lh then
        start = lh-ln+1
    end if

--28/11/19 (bitbucket issue #27)
--  ln -= 1

    for i=start to 1 by -1 do
        for j=1 to ln do
            if needle[j]!=haystack[i+j-1] then exit end if  
            if j=ln then return i end if
        end for
    end for

    return 0
end function

global function match_all(object needle, sequence haystack, integer start=1, bool case_sensitive=true, overlap=false)
    if not case_sensitive then
        needle = lower(needle)
        haystack = lower(haystack)
    end if
    if atom(needle) then
        return find_all(needle,haystack,start)
    end if
    sequence res = {}
    while 1 do
        start = match(needle,haystack,start)
        if start=0 then exit end if
        res = append(res,start)
        start += iff(overlap?1:length(needle))
    end while
    return res
end function

