--
-- ptagset.e
--
--  Phix implementation of tagset (auto include)
--

global function tagset(integer len)
--
-- Returns a sequence of integers: {1,..,len}.
-- Typically for use in tag sorts.
-- See demo\tagsort.exw for an example of use.
--
sequence res = repeat(0,len)
    for i=1 to len do
        res[i] = i
    end for
    return res
end function

