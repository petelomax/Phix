--
-- log10.e
--

global function log10(atom n)
    return log(n) * INVLN10
end function

global function log2(atom n)
--  return log(n) * INVLN2
    return log(n) * 1.44269504088896340739
end function

