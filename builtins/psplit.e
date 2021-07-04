--
-- builtins\psplit.e
--
--  Phix implementation of split(), split_any(), split_by(), and split_path()
--
--  This is an auto-include file; there is no need to manually include it, unless you want a namespace.
--

global function split(sequence source, object delimiter=' ', bool no_empty=true, integer limit=0)
    sequence ret = {}
    integer start
    integer pos
    bool add_empties = not no_empty

    if length(source)!=0 then

        if sequence(delimiter) then
            -- Handle the simple case of split("123", ""), opposite is join({"1","2","3"}, "") -- "123"
            if length(delimiter)=0 then
                for i=1 to length(source) do
                    source[i] = source[i..i]
                    limit -= 1
                    if limit=0 then
--                      source = append(source[1..i],source[i+1..$])
                        source[i+1] = source[i+1..$]
                        source = source[1..i+1]
                        exit
                    end if
                end for
                return source
            end if

            start = 1
            while start<=length(source) do
                pos = match(delimiter, source, start)
                if pos=0 then exit end if
--              if no_empty=0 or pos-1>=start then
                if add_empties or pos-1>=start then
                    limit -= 1
                    if limit=0 then exit end if
                    ret = append(ret, source[start..pos-1])
                end if
                start = pos+length(delimiter)
            end while
        else
            start = 1
            while start<=length(source) do
                pos = find(delimiter, source, start)
                if pos=0 then exit end if
--              if no_empty=0 or pos-1>=start then
                if add_empties or pos-1>=start then
                    limit -= 1
                    if limit=0 then exit end if
                    ret = append(ret, source[start..pos-1])
                end if
                start = pos+1
            end while
        end if

--      if no_empty=0 or start<=length(source) then
        if add_empties or start<=length(source) then
            ret = append(ret, source[start..$])
        end if
    end if
    return ret
end function

--changed 5/12/2020:
--global function split_any(sequence source, object delimiters=", \t|", bool no_empty=false, integer limit=0)
--global function split_any(sequence source, object delimiters=", \t|", bool no_empty=true, integer limit=0)
--global function split_any(sequence source, object delimiters=", \t|")
global function split_any(sequence source, object delimiters={',',' ','\t','|'})
sequence ret = {}
integer start = 1, pos
--, k

    if atom(delimiters) then
        delimiters = {delimiters}
    elsif length(delimiters)=0 then
--      crash("split_any(): delimiter length must be greater than 0")
        ?9/0 --DEV
    end if

    while 1 do
        pos = find_any(delimiters, source, start)
        if pos=0 then exit end if
--      if no_empty=false or pos-1>=start then
        if pos-1>=start then
            ret = append(ret, source[start..pos-1])
        end if
        start = pos+1
--      limit -= 1
--      if limit=0 then exit end if
    end while

--  if no_empty=false or start<=length(source) then
    if start<=length(source) then
        ret = append(ret, source[start..$])
    end if

    return ret
end function

global function split_by(sequence s, integer n)
-- Split a sequence into chucks of at most length n each, eg split_by(tagset(9)) => {{1,2,3},{4,5,6},{7,8,9}}.
    sequence res = {}
    integer j = 1, k = n
    while k<length(s) do
        res = append(res,s[j..k])
        j += n
        k += n
    end while
    if j<=length(s) then
        res = append(res,s[j..$])
    end if
    return res
end function

global function split_path(string path, bool preservetrailsep=false)
sequence res = {}
string chunk
integer start = 1, ch
    -- split, preserving any leading separator
    for i=2 to length(path) do
        ch = path[i]
        if ch='\\' 
        or ch='/' then
            chunk = path[start..i-1]
            if length(chunk) then
                res = append(res,chunk)
            end if
            start = i+1
        end if
    end for
    if start<=length(path) then
        chunk = path[start..$]
        res = append(res,chunk)
    elsif preservetrailsep
      and length(path)>=2 then
        -- (Last char must be a separator, otherwise
        --  we would have had to add a final chunk.)
        if length(res)=0 then
            res = path[$..$]
        elsif not find(res[$][$],`\/`) then
            res[$] = append(res[$],ch)
        end if
    end if
    return res
end function

