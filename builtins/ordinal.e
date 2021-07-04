--
-- builtins\ordinal.e
-- ==================
--
--  A collection of routines for ordinal number handling. (autoinclude)
--
--      ord(integer n) yields "st", "nd", "rd", or "th".
--      ordinal(atom n) yields "first", "second", etc.
--      ordinal(atom n, bool bJustSpell:=true) yields "one", "two", etc.
--

--DEV 10/12/20 (docs not updated)
--global function ord(integer n)
global function ord(atom n)
integer r = remainder(n,10)+1
    if r<=0 or r>4 or remainder(n,100)=r+9 then r = 1 end if
    string res = {"th","st","nd","rd"}[r]
    return res
end function

integer oinit = false
sequence twenties, decades, orders, irregs, ordinals

procedure inito()
    twenties = {"zero","one","two","three","four","five",
                "six","seven","eight","nine","ten","eleven",
                "twelve","thirteen","fourteen","fifteen",
                "sixteen","seventeen","eighteen","nineteen"}    
    decades = {"twenty","thirty","forty","fifty",
               "sixty","seventy","eighty","ninety"}
    orders = {{power(10,15),"quadrillion"},
              {power(10,12),"trillion"},
              {power(10,9),"billion"},
              {power(10,6),"million"},
              {power(10,3),"thousand"}}
    {irregs,ordinals} = columnize({{"one","first"},
                                   {"two","second"},
                                   {"three","third"},
                                   {"five","fifth"},
                                   {"eight","eighth"},
                                   {"nine","ninth"},
                                   {"twelve","twelfth"}})
    oinit = true
end procedure

function twenty(integer n)
    return twenties[mod(n,20)+1]
end function
 
function decade(integer n)
    return decades[mod(n,10)-1]
end function
 
function hundred(integer n)
    if n<20 then
        return twenty(n)
    elsif mod(n,10)=0 then
        return decade(mod(floor(n/10),10))
    end if
    return decade(floor(n/10)) & '-' & twenty(mod(n,10))
end function
 
function thousand(integer n, string withand)
    -- (aside: p2js.exw/insert_dollars() 
    --  must not hundred -> $hundred this:)
    constant sphun = " hundred"
    if n<100 then
        return withand & hundred(n)
    elsif mod(n,100)=0 then
        return withand & twenty(floor(n/100)) & sphun
    end if
    return twenty(floor(n/100)) & sphun & " and " & hundred(mod(n,100))
end function
 
function triplet(atom n)
atom order, high, low
string name, res = ""
    for i=1 to length(orders) do
        {order,name} = orders[i]
        high = floor(n/order)
        low = mod(n,order)
        if high!=0 then
            res &= thousand(high,"")&' '&name
        end if
        n = low
        if low=0 then exit end if
        if length(res) and high!=0 then
            res &= ", "
        end if
    end for
    if n!=0 or res="" then
        res &= thousand(floor(n),iff(res=""?"":"and "))
        n = abs(mod(n,1))
        if n>1e-6 then
            res &= " point"
            --
            -- Ah: inherited from rosettacode, I suspect *10+1e-7 
            --  is there to trigger <1e-6 sooner rather than later,
            --  by rounding up to the nearest six decimal places.
            -- (This sort of maths never was quite my strong suit.)
            --
            for i=1 to 10 do
                integer d = floor(n*10.0000001)
                res &= ' '&twenties[d+1]
                n = n*10-d
                if abs(n)<1e-6 then exit end if
            end for
        end if
    end if
    return res
end function
 
function spell(atom n)
string res = ""
    if n<0 then
        res = "minus "
        n = -n
    end if
    res &= triplet(n)
    return res
end function

global function ordinal(atom n, bool bJustSpell=false)
    if not oinit then inito() end if
    string s = spell(n)
    if not bJustSpell then -- default: "one" => "first", etc
        integer i=length(s)
        while i do
            integer ch = s[i]
            if ch=' ' or ch='-' then exit end if
            i -= 1
        end while
        integer k = find(s[i+1..$],irregs)
        if k then
            s = s[1..i]&ordinals[k]
        elsif s[$]='y' then
            s[$..$] = "ieth"
        else
            s &= "th"
        end if
    end if
    return s
end function
 
