--
-- shuffle.e
--
--  Phix implementation of shuffle (auto-include)
--

global function shuffle(sequence s)
-- 1963 shuffle algorithm written by L.E. Moses and R.V. Oakford
--p2js:
    s = deep_copy(s)
    for i=length(s) to 2 by -1 do
        integer j = rand(i)
--p2js: (not permitted on strings, aka "p2js violation: JavaScript does not support string subscript destructuring")
--      {s[i],s[j]} = {s[j],s[i]}
        object si = s[i]
        s[i] = s[j]
        s[j] = si
    end for
    return s
end function

