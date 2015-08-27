--
-- substitute.e
--
--  Phix implementation of substitute (auto-include)
--

global function substitute(string text, string s, string r)
-- replace all instances of s in text with r
integer k = 1, 
        l = length(s),
        startidx = 1
sequence chunks = {}
    while 1 do
        k = match(s,text,k)
        if k=0 then exit end if
        chunks = append(chunks,text[startidx..k-1])
        k += l
        startidx = k
    end while
    if length(chunks) then
        chunks = append(chunks,text[startidx..$])
        text = chunks[1]
        for i=2 to length(chunks) do
            text &= r
            text &= chunks[i]
        end for
    end if
    return text
end function

