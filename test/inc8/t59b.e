include t59a.e as a

global function B()
    return "this is t59b\n"
end function

global function B1(integer b1)
    return {b1+'B'}
end function

global function B2(string a, string b)
    return a & b
end function

global function B3(string b3="b3\n")
    return b3
end function

--  puts(1,a:A())
--  puts(1,A1(' ')&"\n")    
--  puts(1,A2("abc","def\n")) -- "abcdef"
--  puts(1,A2(b:="def\n",a:="abc")) -- "abcdef"
--  puts(1,A3()) -- "a3\n"

    if a:A()!="this is t59a\n" then ?9/0 end if
    if A1(' ')!='a' then ?9/0 end if
    if A2("abc","def\n")!="abcdef\n" then ?9/0 end if
    if A2(b:="def\n",a:="abc")!="abcdef\n" then ?9/0 end if
    if A3()!="a3\n" then ?9/0 end if
--DEV:
--global function A() -- technically not really wrong...
--  return "this is WRONG\n"
--end function

