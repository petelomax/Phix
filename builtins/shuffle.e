--
-- shuffle.e
--
--  Phix implementation of shuffle (auto-include)
--

global function shuffle(sequence s)
-- 1963 shuffle algorithm written by L.E. Moses and R.V. Oakford
integer j
    for i=length(s) to 2 by -1 do
        j = rand(i)
        {s[i],s[j]} = {s[j],s[i]}
    end for
    return s
end function

