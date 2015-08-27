--
-- psplit.e
--
--  Phix implementation of split() and split_any()
--
-- This is an auto-include file; there is no need to manually include
--  it, unless you want a namespace.
--
--DEV document this.
--  example?: split("one},{two},{three","},{") is {"one","two","three"}
--  whereas split_any("one},{two},{three","},{") is {"one","","","two","","","three"}

global function split(sequence src, object delim=' ', integer limit=0, integer no_empty = 0)
sequence ret = {}
integer start
integer pos
integer k

    if length(src)!=0 then

        if sequence(delim) then
            -- Handle the simple case of split("123", ""), opposite is join({"1","2","3"}, "") -- "123"
            if length(delim)=0 then
                for i=1 to length(src) do
                    src[i] = src[i..i]
                    limit -= 1
                    if limit=0 then
--                      src = append(src[1..i],src[i+1..$])
                        src[i+1] = src[i+1..$]
                        src = src[1..i+1]
                        exit
                    end if
                end for
                return src
            end if

            start = 1
            while start<=length(src) do
                pos = match(delim, src, start)
                if pos=0 then exit end if
                ret = append(ret, src[start..pos-1])
                start = pos+length(delim)
                limit -= 1
                if limit=0 then exit end if
            end while
        else
            start = 1
            while start<=length(src) do
                pos = find(delim, src, start)
                if pos=0 then exit end if
                ret = append(ret, src[start..pos-1])
                start = pos+1
                limit -= 1
                if limit=0 then exit end if
            end while
        end if

        ret = append(ret, src[start..$])

        k = length(ret)
        if no_empty then
            k = 0
            for i=1 to length(ret) do
                if length(ret[i])!=0 then
                    k += 1
                    if k!=i then
                        ret[k] = ret[i]
                    end if
                end if
            end for
        end if

        if k<length(ret) then
            ret = ret[1..k]
        end if
    end if
    return ret
end function

global function split_any(sequence src, object delim, integer limit=0, integer no_empty=0)
sequence ret = {}
integer start = 1, pos, k

    if atom(delim) then
        delim = {delim}
    end if

    if length(delim)=0 then
--      crash("sequence:split_any(): delimiter length must be greater than 0")
        ?9/0 --DEV
    end if

    while 1 do
        pos = find_any(delim, src, start)
        if pos=0 then exit end if
        ret = append(ret, src[start..pos-1])
        start = pos+1
        limit -= 1
        if limit=0 then exit end if
    end while

    ret = append(ret, src[start..$])

    k = length(ret)
    if no_empty then
        k = 0
        for i=1 to length(ret) do
            if length(ret[i])!=0 then
                k += 1
                if k!=i then
                    ret[k] = ret[i]
                end if
            end if
        end for
    end if

    if k<length(ret) then
        ret = ret[1..k]
    end if
    return ret
end function

