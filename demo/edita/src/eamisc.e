--
-- eamisc.e
--
-- UpperCase, LowerCase, min, max, curb, despace
--
without trace

sequence charMapUp, charMapDown
    charMapUp=repeat(0,256)
    for i=1 to 256 do
        charMapUp[i]=i-1
    end for
    charMapDown=charMapUp
    for i='a' to 'z' do
        charMapUp[i+1]=i-32
        charMapDown[i-31]=i
    end for

global function UpperCase(object text)
    if sequence(text) then
        for i=1 to length(text) do
            text[i]=UpperCase(text[i])
        end for
    else
        if integer(text) and text>=0 and text <=255 then
            return charMapUp[text+1]
        end if
    end if
    return text
end function

global function LowerCase(object text)
    if sequence(text) then
        for i=1 to length(text) do
            text[i]=LowerCase(text[i])
        end for
    else
        if integer(text) and text>=0 and text <=255 then
            return charMapDown[text+1]
        end if
    end if
    return text
end function

--global type bool(integer flag)
--  return (flag=0 or flag=1)
--end type

--/*
global function iff(bool condition, object Tval, object Fval)
    if condition then return Tval else return Fval end if
end function
--*/

global function Min(integer a, integer b)
--min
    if a<b then return a else return b end if
end function

global function Max(integer a, integer b)
    if a>b then return a else return b end if
end function

global function Abs(object a)
    if atom(a) then
        if a<0 then a = -a end if
    else
        for i = 1 to length(a) do
            a[i]=Abs(a[i])
        end for
    end if
    return a
end function

global function curb(integer low, integer val, integer high)
-- eg curb(0,y,11) returns y if y>=0 and y<=11, 0 if y<0, or 11 if y>11.
    if val<=low then return low elsif val>=high then return high else return val end if
end function

global function despace(sequence txt)
integer k
    while 1 do
        k=find(' ',txt)
        if k=0 then
            k=find('\t',txt)
            if k=0 then exit end if
        end if
        txt=txt[1..k-1]&txt[k+1..length(txt)]
    end while
    return txt
end function

-- used by eamenus.ew, eacons.ew, and eaedb.e
--DEV put this in arwen (getExtension):
global function getFileExtension(sequence name)
integer len
    len = length(name)
    for i=len to 1 by -1 do
        if name[i] = '.' then
            return LowerCase(name[i+1..len])
        end if
    end for
    --oops!
    return ""
end function


