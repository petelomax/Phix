--
-- premoveall.e
--
--  Phix implementation of remove_all()
--
-- This is an auto-include file; there is no need to manually include
--  it, unless you want a namespace.
--

global function remove_all(object needle, sequence haystack)
integer ts, te, ss, se

    -- See if we have to do anything at all.
    se = find(needle, haystack)
    if se=0 then
        return haystack
    end if

    -- Now we know there is at least one occurrence and because
    -- it's the first one, we don't have to move anything yet.
    -- So pretend we have and set up the 'end' variables
    -- as if we had moved stuff.
    se -= 1
    te = se

    while 1 do
        -- Calc where the next target start is (1 after the previous end)
        ts = te+1

        -- Calc where to start the next search (2 after the previous end)
        ss = se+2

        -- See if we got another one.
        se = find(needle, haystack, ss)

        -- We have another one, so calculate the source end(1 before the needle)
        se = se-1

        -- Calc the target end (start + length of what we are moving)
        te = ts+se-ss

        if se<=0 then exit end if

        -- Shift elements down the sequence.
        haystack[ts..te] = haystack[ss..se]

    end while

    -- Check to see if there is anything after the final needle and move it.
    if ss<=length(haystack) then
        te = ts+length(haystack)-ss
        haystack[ts..te] = haystack[ss..$]
    else
        -- Need to backtrack one needle.
        te = ts-1
    end if

    -- Return only the stuff we moved.
    return haystack[1..te]
end function
