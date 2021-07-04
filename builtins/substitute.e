--
-- substitute.e
--
--  Phix implementation of substitute[_all] (auto-include)
--
global function substitute(sequence text, object s, r, integer limit=-1)
-- replace all instances of s in text with r
    integer k = 1, 
            l = iff(atom(s)?1:length(s)),
            startidx = 1
    sequence chunks = {}
    while limit=-1 or length(chunks)<limit do
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

global function substitute_all(sequence text, strings, replacements)
    for i=1 to length(strings) do
        if string(strings) then
            if string(replacements) then
                -- in this case a naive in situ approach is faster:
                integer ch = strings[i], repch = replacements[i]
                for j=1 to length(text) do
                    if text[j]=ch then text[j] = repch end if
                end for
            else
                text = substitute(text,strings[i..i],replacements[i])
            end if
        else
            text = substitute(text,strings[i],replacements[i])
        end if
    end for
    return text
end function


