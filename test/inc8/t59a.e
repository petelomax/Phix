-- Main feature of this test is not needing these:
--  (you get warnings, though)
--forward global function A()
--forward global function A1(integer a1)
--forward global function A2(string a, string b)
--forward global function A3(string a3="a3\n")
include t59b.e as b

global function A()
    return "this is t59a\n"
end function

global function A1(integer a1)
    return a1+'A'
end function

global function A2(string a, string b)
    return a & b
end function

global function A3(string a3="a3\n")
    return a3
end function

--  puts(1,b:B())
--  puts(1,B1(' ')&"\n") -- "b"
--  puts(1,B2("abc","def\n")) -- "abcdef"
--  puts(1,B2(b:="def\n",a:="abc")) -- "abcdef"
--  puts(1,B3()) -- "b3\n"
    
    if b:B()!="this is t59b\n" then ?9/0 end if
    if B1(' ')!="b" then ?9/0 end if
    if B2("abc","def\n")!="abcdef\n" then ?9/0 end if
    if B2(b:="def\n",a:="abc")!="abcdef\n" then ?9/0 end if
    if B3()!="b3\n" then ?9/0 end if
