--
-- builtins/punique.e
--
-- See also/similar to Eu's include/std/sequence.e/remove_dups()...
--
--include dict.e

global function unique(sequence s, string options="SORT")
    sequence res = {}
--  if length(s)>1 then
    if length(s)>0 then
--      integer outdx = 1
        if options="STABLE"
        or options="INPLACE" then
            integer d = new_dict()
            setd(s[1],0,d)
            res = {s[1]}
            for i=2 to length(s) do
                object si = s[i]
--7/2/21
--              if getd_index(si)=NULL then
                if getd_index(si,d)=NULL then
                    setd(si,0,d)
--                  outdx += 1
--                  s[outdx] = si
                    res = append(res,si)
                end if
            end for
            destroy_dict(d)
--/*
--DEV untested, just an idea, that sorta fell apart before I tried testing it...
-- (I was aiming for a stable sort, just got a bit flummoxed, and don't really need it right now anyway)
        elsif options="TAGSORT" then
            sequence tags = custom_sort(s, tagset(length(s))) 
            object prev = s[tags[1]]
--          sequence res = {prev}
            for i=2 to length(tags) do
                object next = s[tags[i]]
                if next!=prev then
--                  res = append(res,next)
                    prev = next
                end if
            end for
            return res
--*/
        else
            if options!="PRESORTED" then
                if options!="SORT" then
                    ?9/0    -- unknown
                end if
                s = sort(s)
--              s = sort(deep_copy(s))
            end if
            object prev = s[1]
--5/8/21:
--          res = {prev}
            res = repeat(prev,1)
            for i=2 to length(s) do
                object si = s[i]
                if si!=prev then
                    prev = si
--                  outdx += 1
--                  s[outdx] = prev
                    res = append(res,prev)
                end if
            end for
        end if
--      s = s[1..outdx]
    end if
--  return s
    return res
end function

-- Euphoria compatibility (not autoincluded)
global enum RD_INPLACE, RD_PRESORTED, RD_SORT
constant ops = {"INPLACE","PRESORTED","SORT"}
global function remove_dups(sequence source_data, integer proc_option = RD_PRESORTED)
    return unique(source_data, ops[proc_option])
end function

