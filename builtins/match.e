--
-- builtins\match.e
-- ================
--
--  hll implementation of match()  (replaces builtins/VM/pMatch.e)
--
--  Changes noted below ("Euphoria") arose from a protracted discussion on EuForum in 2002.
--
without debug

global function match(object needle, sequence haystack, integer start=1, bool case_sensitive=true)
--
-- Try to match needle against some slice of haystack.
-- Return the element number of haystack where the (first) matching slice begins, else 0.  
--
-- Finding a short int is faster than anything else, because there is no dereference, hence 
-- when we hit a mismatch and one of the disagreeing values is a shortint, use that to look
-- for a better place to restart the top-level scan.
--
    -- This line, and first parameter being object not sequence, is not Euphoria compliant.
    --  (Euphoria gives error "first argument of match() must be a sequence")
    if atom(needle) then
        return find(needle,haystack,start)
    end if
    if not case_sensitive then
        needle = lower(needle)
        haystack = lower(haystack)
    end if
    integer res = start,
             nl = length(needle),
             hl = length(haystack)
    -- This line is also not Euphoria compliant
    --  (Euphoria gives error "first argument of match() must be a non-empty sequence")
    if nl=0 then return 0 end if
    if start<1 then return 0 end if
    while res+nl-1<=hl do
        for i=1 to nl do
            integer hdx = i+res-1
            object ni = needle[i],
                   hi = haystack[hdx]
            if not equal(ni,hi) then
                if integer(ni) then
                    -- scan for ni later on in haystack
                    -- eg needle=13131..., and
                    --  haystack=1313x31...
                    -- with needle[5]=1, scanning forward two places to haystack[7]=1
                    -- gives the earliest point worth re-starting from.
                    -- if ni does not occur anywhere later in haystack, then
                    -- clearly there will be no match anywhere.
                    for j=hdx+1 to hl+1 do
                        res += 1
                        if j>hl then return 0 end if
                        hi = haystack[j]
                        if equal(ni,hi) then exit end if
                    end for
                    exit
                elsif integer(hi) then
                    -- scan for hi earlier on in needle
                    -- eg needle=131313x..., and
                    --  haystack=1313131...
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
            if i=nl then return res end if
        end for
    end while
    return 0
end function

global function rmatch(object needle, sequence haystack, integer start=length(haystack), bool case_sensitive=true)
--
-- Try to match a needle against some slice of a haystack in reverse order.
-- Return the element number of haystack where the (last<=start) matching slice begins, else 0.  
--
    -- This line, and first parameter being object not sequence, is not Euphoria compliant.
    --  (Euphoria gives error "first argument of match() must be a sequence")
    if atom(needle) then
        return rfind(needle,haystack,start)
    end if

    integer nl = length(needle),
            hl = length(haystack)

    if nl=0
    or start=0
    or start>hl
    or hl+start<1 then
        return 0
    end if

    if not case_sensitive then
        needle = lower(needle)
        haystack = lower(haystack)
    end if

    if start<1 then
        start = hl+start
    end if

    if start+nl-1>hl then
        start = hl-nl+1
    end if

    for i=start to 1 by -1 do
        for j=1 to nl do
            if needle[j]!=haystack[i+j-1] then exit end if  
            if j=nl then return i end if
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

global function begins(object sub_text, sequence full_text)
    integer lf = length(full_text)
    if lf=0 then return false end if
    if atom(sub_text) then
        -- eg begins('c',"cat") -> true.
        return sub_text==full_text[1]
    end if
    integer ls = length(sub_text)
    return ls<=lf and sub_text==full_text[1..ls]
end function
