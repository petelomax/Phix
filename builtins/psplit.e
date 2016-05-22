--
-- psplit.e
--
--  Phix implementation of split() and split_any()
--
-- This is an auto-include file; there is no need to manually include
--  it, unless you want a namespace.
--

global function split(sequence source, object delimiter=' ', integer limit=0, integer no_empty=0)
sequence ret = {}
integer start
integer pos
--integer k

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
                if no_empty=0 or pos-1>=start then
                    ret = append(ret, source[start..pos-1])
                end if
                start = pos+length(delimiter)
                limit -= 1
                if limit=0 then exit end if
            end while
        else
            start = 1
            while start<=length(source) do
                pos = find(delimiter, source, start)
                if pos=0 then exit end if
                if no_empty=0 or pos-1>=start then
                    ret = append(ret, source[start..pos-1])
                end if
                start = pos+1
                limit -= 1
                if limit=0 then exit end if
            end while
        end if

        if no_empty=0 or start<=length(source) then
            ret = append(ret, source[start..$])
        end if

--      k = length(ret)
--      if no_empty then
--          k = 0
--          for i=1 to length(ret) do
--              if length(ret[i])!=0 then
--                  k += 1
--                  if k!=i then
--                      ret[k] = ret[i]
--                  end if
--              end if
--          end for
--      end if
--
--      if k<length(ret) then
--          ret = ret[1..k]
--      end if
    end if
    return ret
end function

global function split_any(sequence source, object delimiters=", \t|", integer limit=0, integer no_empty=0)
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
        if no_empty=0 or pos-1>=start then
            ret = append(ret, source[start..pos-1])
        end if
        start = pos+1
        limit -= 1
        if limit=0 then exit end if
    end while

    if no_empty=0 or start<=length(source) then
        ret = append(ret, source[start..$])
    end if

--  k = length(ret)
--  if no_empty then
--      k = 0
--      for i=1 to length(ret) do
--          if length(ret[i])!=0 then
--              k += 1
--              if k!=i then
--                  ret[k] = ret[i]
--              end if
--          end if
--      end for
--  end if
--
--  if k<length(ret) then
--      ret = ret[1..k]
--  end if
    return ret
end function

