--------------------NOTICE-------------------------------*
-- Software ID: w32utils.e
-- Version:     0.70.4a
-- Copyright:   (c) 2000 David Cuny and friends
--              All rights reserved.
-- Licence:
-- This software is provided 'as-is', without any express or implied warranty.
-- In no event will the authors be held liable for any damages arising from
-- the use of this software.
--
-- Permission is granted to anyone to use this software for any purpose,
-- including commercial applications, and to alter it and redistribute it
-- freely, subject to the following restrictictions:
-- 1. The origin of this software must not be misrepresented; you must not
--    claim that you wrote the original software.
-- 2. If you use this software in a product, acknowedgement in the product's
--    documenation and binary are required.
-- 3. Altered source versions, and works substantially derived from the it,
--    must...
--   a) be plainly be marked as such,
--   b) not be misrepresented as the original software,
--   c) include this notice, unaltered.
--------------------End of NOTICE------------------------*

without warning
without trace
include wildcard.e
include dll.e
include machine.e

--/topic Support Routines
--/info
--Miscellaneous 'generic' routines used by win32lib
--These routines can be used by many applications as they are
--not specific to the Win32Lib library.

--/topic Constants
--/const w32False
--/desc The value for a boolean FALSE.

--/topic Constants
--/const w32True
--/desc The value for a boolean True

--/topic Constants
--/const w32True
--/desc Pass this value as a parameter to obtain its current value without setting it.
-- Some routines will use a different magic value when this one could be a valid input.

global constant
    w32False   = (1=0),
    w32True    = (1=1),
    w32GetValue = -1


--/topic Types
--/type w32string
--/desc Implements an ASCII string.
--Identifiers declared as type w32string are sequences that can only
-- contain integers in the range 0 to 255 inclusive.
--
--Example:
--/code
--  w32string Name
--  Name = "abc" -- Okay.
--  Name = 12 -- Fails
--  Name = {'a', 'b', 'c'} -- Okay
--  Name = {'a', 'b', 12.3} -- Fails
--  Name = {"abc", "def"} -- Fails.
--/endcode
---------------------------------------
global type w32string(object s)
---------------------------------------
    if not sequence(s) then
        return 0
    end if

    for i=1 to length(s) do
        if not integer(s[i]) then
            return 0
        end if

        if s[i]<0 or s[i]>255 then
            return 0
        end if

    end for
    return w32True
end type

--/topic Support Routines
--/func w32to_string( object pData )
--/desc Converts the parameter to a string repesentation of its value.
--/ret SEQUENCE: A string representation of the parameter.
---------------------------------------
global function w32to_string(object s)
---------------------------------------
sequence lResult

    if atom(s) then
        return sprintf("%g", s)
    end if

    if w32string(s) then
        return s
    end if

    lResult = ""
    for i=1 to length(s) do

        lResult &= "'"
        lResult &= w32to_string(s[i])
        lResult &= "'"

        if i!=length(s) then
            lResult &= ", "
        end if
    end for

    return lResult
end function



--/topic Support Routines
--/func w32get_bits( atom b32 )
--/desc Does the reverse of /w32or_all() in that it gets all the bit values from an atom.
--/ret Sequence: The non-zero bits set in /i b32.
--
-- Typically used to extract which bits have been set on in a flag.
--
-- Example:
--
-- /code
--      -- combine flags
--      integer flags
--      sequence codes
--      codes = w32get_bits( origflag )
--      if find(WS_EX_CLIENTEDGE, codes) then
--          setText(statusarea, "Client Edge has been specified")
--      end if
-- /endcode
constant kBitPosn = {
                     #00000001 , #00000002, #00000004, #00000008,
                     #00000010 , #00000020, #00000040, #00000080,
                     #00000100 , #00000200, #00000400, #00000800,
                     #00001000 , #00002000, #00004000, #00008000,
                     #00010000 , #00020000, #00040000, #00080000,
                     #00100000 , #00200000, #00400000, #00800000,
                     #01000000 , #02000000, #04000000, #08000000,
                     #10000000 , #20000000, #40000000, #80000000
                    }
global function w32get_bits(atom b32)
sequence lResult

    lResult = {}
    for i=1 to length(kBitPosn) do
        if and_bits(b32, kBitPosn[i]) then
            lResult &= kBitPosn[i]
        end if
    end for

    if length(lResult)=0 then
        lResult = {0}
    end if

    return lResult
end function


--/topic Support Routines
--/func w32signed_word( atom a )
--/desc Converts /i a into a signed 16-bit integer.
--/ret INTEGER: Signed 16 bits of a 32 bit word.
-- Typically used to decode Win32 message values when several values
-- are packed into a single number.
--
-- Example:
--
-- /code
--      -- extract the low portion from lParam
--      integer x
--      x = w32signed_word( lParam )
-- /endcode

global function w32signed_word(atom a)
-- return low word
integer x
    x = and_bits(a, #FFFF)
    if x>#7FFF then
        x -= #10000
    end if
    return x
end function



-----------------------------------------------------------------------------

--/topic Support Routines
--/func w32pack_word( integer low, integer high )
--/desc Packs values into word.
--/ret ATOM: 32 bit word with low value in low 16 bits, high value in high 16 bits.
-- Typically used to encode a message parameter into a 32 bit word.
--
-- Example:
--
-- /code
--      -- pack min and max into parameter
--      integer y
--      lParam = w32pack_word( min, max )
-- /endcode

global function w32pack_word(integer low, integer high)
    -- return packed 32 bit value

    return ( and_bits(high, #FFFF)*#10000 )+and_bits(low,  #FFFF)

end function


--/topic Support Routines
--/func w32lo_word( atom pData)
--/desc returns the low-16 bits of /i pData
--/ret INTEGER: Bits 15-0 of the parameter
--
-- Example:
-- /code
--      integer y
--      y = w32lo_word( bigval )
-- /endcode

global function w32lo_word(atom pData)
    return and_bits(pData, #FFFF)
end function


--/topic Support Routines
--/func w32hi_word( atom pData)
--/desc returns the high 16 bits of /i pData
--/ret INTEGER: Bits 31-16 of the parameter as a 16 bit value.
--
-- Example:
-- /code
--      integer y
--      y = w32hi_word( bigval )
-- /endcode

global function w32hi_word(atom pData)
    return and_bits(and_bits(pData, #FFFF0000)/#10000, #FFFF)
end function


--/topic Support Routines
--/func w32abs( object a )
--/desc Absolute value.
--/ret Absolute value of the atom or sequence.
--
--Example
--/code
--       sequence s
--       atom a
--
--       a = w32abs(-1)   -- = 1
--       s = w32abs({-2,-1,{-1,0,1},1,2}) -- = {2,1,{1,0,1},1,2}
--/endcode

global function w32abs(object a)
    if atom(a) then
        if a<0 then
            return -a
        else
            return a
        end if
    else
        for i=1 to length(a) do
            a[i] = w32abs(a[i])
        end for

        return a
    end if

end function


--/topic Support Routines
--/func w32unpack_dword( atom a )
--/desc Converts a number into two 16-bit integers
--/ret (SEQUENCE) The pair {low word,high word}
-- Typically used to decode Win32 message values.
global function w32unpack_dword(atom a)
    if a<0 then
        a += power(2,32)
    end if
--  return floor(remainder({a,a/#10000},#10000))
    return {and_bits(a,#FFFF),floor(a/#10000)}
end function

--/topic Support Routines
--/func w32shortInt( atom a )
--/desc Converts a number into a 16-bit signed integer
--/ret INTEGER: The value of /i a as a short int, in the -32,768..32767 range.
-- Typically used to decode Win32 message values
--
-- Example:
--
-- /code
--      -- Mouse Position
--      sequence hilo, x, y
--      lohi = w32unpack_dword( lParam )
--      x = w32shortInt(lohi[2])
--      y = w32shortInt(lohi[1])
-- /endcode
global function w32shortInt(atom i)

    -- converts numbers ( 4 bytes #0000 to #FFFF)
    -- to signed short ints (2 bytes -32768 to 32767 )

    -- Force the use of only the rightmost 2 bytes.
    i = and_bits(i, #FFFF)

    if i>=0 and i<=#7FFF then
        return i
    else
        return i-#10000
    end if

end function

global constant
    w32RoundDown = 1,
    w32RoundUp = 2,
    w32RoundToZero = 3,
    w32RoundToInf = 4,
    w32RoundScientific = 5,
    w32RoundCommercial = 6

--/topic Support Routines
--/func w32remainder(object a,object b)
--/desc Ensures that the result has the sign of /i b, contrary to remainder().
--/ret (ATOM) The remainder of /i a divided by /i b, the result having the sign of /i b.
-- remainder() returns a result which has the sign of /i a. /n
-- This function extends to sequences.
global function w32remainder(object a,object b)
    if sequence(a) then
        for i=1 to length(a) do
--          a[i]=remainder(a[i],b)
            a[i] = w32remainder(a[i],b)
        end for
        return a
    elsif sequence(b) then
--      for i=1 to length(a) do
        for i=1 to length(b) do
--          b[i]=remainder(a,b[i])
            b[i] = w32remainder(a,b[i])
        end for
        return b
    end if
    if compare(a,0.0)=compare(b,0.0) then
        return remainder(a,b)
    else
        return b+remainder(a,b)
    end if
end function

--/topic Support Routines
--/func w32average(sequence data,object weight)
--/desc Computes the weighted average of /i data using the weight(s) /i weight.
--/ret (OBJECT) The ratio sum_of_data/sum_of_weights.
-- If /i data is not homogeneous, or /i weight is a sequence and has a sequence element, or
-- the supplied weight is 0, or the sum of supplied weights is 0, then
-- an "absurd" value is returned: 0 if data[1] is a sequence, else {}./n
-- Extra data or weights are discarded./n
-- If data or weight is an empty sequence, 0 is returned.
global function w32average(sequence data,object weight)
object pattern,test,sum_data,x,on_error
atom sum_weight

    if sequence(weight) then
        if length(data)>length(weight) then
            data = data[1..length(weight)]
        elsif length(data)<length(weight) then
            weight = weight[1..length(data)]
        end if
    end if
    if equal(weight,{}) then
        return 0
    elsif equal(data,{}) then
        return 0
    elsif length(data)=1 then
        return data[1]
    end if
    pattern = data[1]
    sum_data = pattern
    pattern = 0
--  if atom(pattern) then
    if atom(sum_data) then
        on_error = {}
    else
        on_error = 0
    end if
    if atom(weight) then
        if weight=0 then
            return on_error
        end if
        for i=2 to length(data) do
            x = data[i]
            test = x
            test = 0
            if compare(test,pattern) then --data is not homogeneous
                return on_error
            end if
            sum_data += x
        end for
        return sum_data/weight
    else
        sum_weight = weight[1]
        for i=2 to length(data) do
            x = data[i]
            test = x
            test = 0
            if sequence(weight[i]) or compare(test,pattern) then --data is not homogeneous
                return on_error
            end if
            sum_data += x
            sum_weight += weight[i]
        end for
        if sum_weight=0 then
            return on_error
        else
            return sum_data/sum_weight
        end if
    end if
end function

--/topic Support Routines
--/func w32round(object a,object b,integer flag)
--/desc Rounds /i a, or all the atoms in the sequence /i a, to some multiple of /i b.
--/ret The rounded value(s).
-- This function extends to sequences, except for the /i flag argument. /n
-- The way the rounded value is reckoned from a is defined by /i flag. Supported values are:
--/li w32RoundDown: round to the highest less or equal multiple of /i b
--/li w32RoundUp: round to the lowest greater or equal multiple of /i b
--/li w32RoundToZero: round to the closest multiple of /i b betwaeen /i a and 0.0
--/li w32RoundToInf: round to the closest multiple of /i b betwaeen /i a and the infinity.
--/li w32RoundScientific: round to closest multiple of /i b. If there is a tie, ensure
-- deterministic evenness of choices.
--/li w32RoundCommercial: round to closest multiple of /i b.
global function w32round(object a,object b,integer flag)
atom diff
integer sgn

    if sequence(a) then
        for i=1 to length(a) do
            a[i] = remainder(a[i],b)
        end for
        return a
    elsif sequence(b) then
--      for i=1 to length(a) do
        for i=1 to length(b) do
            b[i] = remainder(a,b[i])
        end for
        return b
    end if

    if b<=0.0 or a=0.0 then
        return a
    end if
    if sequence(a) then
        for i=1 to length(a) do
            a[i] = w32round(a[i],b,flag)
        end for
        return a
    end if

    diff = w32remainder(a,b)
    sgn = compare(a,0.0)
    if diff/(a*sgn)<1e-8 then   -- a is a multiple of b
        return a
    end if
    -- now sgn is guaranteed to be 1 or -1
    if flag=w32RoundUp then
        return a+b-diff
    elsif flag=w32RoundDown then
        return a-diff
    elsif flag=w32RoundToZero then
        if sgn=1 then
            return a-diff
        else
            return a+b-diff
        end if
    elsif flag=w32RoundToInf then
        if sgn=-1 then
            return a-diff
        else
            return a+b-diff
        end if
    else -- any unknown value defaults to w32Scientific
        sgn = compare(diff+diff,b)
        if sgn=1 then
            return a+b-diff
        elsif sgn=-1 then
            return a-diff
        elsif flag=w32RoundCommercial then
            return a
        else
            if remainder(floor(a/b),2) then
                return a+diff -- makes the integer part even
            else
                return a
            end if
        end if
    end if
end function

constant
    vDigits = ".,0123456789ABCDEF",
    vIgnored = find('0',vDigits)
sequence vCurrencySym vCurrencySym = "$£¤¥€"
integer decimal_mark decimal_mark = 1
global constant
    w32DECIMAL_DOT = 1,
    w32DECIMAL_COMMA = 2
--/topic Support Routines
--/func setDecimalMark(integer new_mark)
--/desc Gets and possibly sets the decimal mark w32TextToNumner() is to recognise.
--/ret (INTEGER) The previous or current value.
-- new_mark is either '.', ',', w32DECIMAL_DOT, w32DECIMAL_COMMA or w32GetValue.
-- Negative values are considered to be w32GetValue, positive unknown value are assumed to be w32DECIMAL_DOT.
-- If the new mark is not w32GetValue, the new mark becomes the decimal mark w32TextToNumber()
-- will use. The former value is always returned.
global function setDecimalMark(integer new_mark)
integer old_mark

    old_mark = decimal_mark
    if new_mark>=0 then
        if new_mark=',' then
            new_mark = w32DECIMAL_COMMA
        elsif new_mark!=w32DECIMAL_COMMA then
            new_mark = w32DECIMAL_DOT
        end if
        decimal_mark = new_mark
    end if
    return old_mark
end function

--/topic Support Routines
--/func w32TextToNumber( sequence text )
--/ret Atom: The number represented by the text.
--/desc This converts the text into a number.
-- If the text contains invalid characters, zero is returned.
--
-- /b "Note 1:" You can supply /b Hexadecimal values if the value is preceded by
-- a '#' character, /b Octal values if the value is preceded by a '@' character,
-- and /b Binary values if the value is preceded by a '!' character. With
-- hexadecimal values, the case of the digits 'A' - 'F' is not important. Also,
-- any period character embedded in the number is used with the correct base.
--
-- /b "Note 2:" Any underscore characters or thousands separators, that are embedded in the text
-- number are ignored. These can be used to help visual clarity for long numbers. The thousands 
-- separator is ',' if the decimal mark is '.' (the default), or '.' if it is ','. You
-- inspect and set it using /setDecimalMark().
--
--/b "Note 3:" You can supply a leading or trailing, minus or plus sign.
--
--/b "Note 4:" You can supply trailing percentage sign(s). Each one present causes
-- the resulting value to be divided by 100.
--
--/b "Note 5:" Any single currency symbol to the left of the first digit is ignored.
-- A currency symbol is any character of the string: "$£¤¥€". A currency symbol may
-- have no digit before it, and must be followed by a dot or digit. If there is none,
-- a single trailing currency sign will be ignored, but there must not be any digit 
-- past it.
--
--/b "Note 6:" Leading characters may appear in any order and be mixed. Only the 
-- sign or base symbol further inside is taken into acount.
--
-- This function can optionally return information about invalid numbers. If
-- /i text has the form of {sequence, integer} then if the integer is nonzero,
-- a sequence is returned. The first element is the value converted, and the
-- second is the position in the text where conversion stopped. If no errors
-- were found then this is zero.
--
--/code
--     sequence rc
--     atom   val
--     rc = w32TextToNumber({"12.34a", 1})
--     --  rc ---> {12.34, 6} -- Error at position 6
--     rc = w32TextToNumber({"12.34", 1})
--     --  rc ---> {12.34, 0} -- No errors.
--
--     val = w32TextToNumber("12.34a")
--     --  val ---> 0
--
--      val = w32TextToNumber("#f80c") --> 63500
--      val = w32TextToNumber("#f80c.7aa") --> 63500.47900390625
--      val = w32TextToNumber("@1703") --> 963
--      val = w32TextToNumber("!101101") --> 45
--      val = w32TextToNumber("12_583_891") --> 12583891
--      val = w32TextToNumber("12_583_891%") --> 125838.91
--      val = w32TextToNumber("12,583,891%%") --> 1258.3891
--
--/endcode
global function w32TextToNumber(sequence text)
-- get the numeric value of text
integer dot,sign,tstart,tend, v, note, notify
atom lhs, rhs, lh, rh
integer base, pc
atom value
integer digcnt
integer currency
-- modified CChris <oedoc@free.fr>, Dec 5, 2006
-- the innermost trailing currency sign is taken into account if no leading currency sign
integer nomoredigit

    dot = 0
    lh = 0
    lhs = 0
    rh = 0
    rhs = 1
    sign = 0
    note = 0
    base = 10
    pc = 1
    digcnt = 0
    currency = 0
    nomoredigit = 0

    if  length(text)=2  and
        sequence(text[1]) and
        integer(text[2])
    then
        notify  = text[2]
        text = text[1]
    else
        notify = 0
    end if

    -- convert the value of the text
    text = upper(text)
    tstart = 1
    tend = length(text)
    -- Ignore leading whitespace
    while tstart<=tend do
        if equal(text[tstart], '-') and sign=0 then
            sign = -1
        elsif equal(text[tstart],'+') and sign=0 then
            sign = 1
        elsif equal(text[tstart],'#') then
            base = 16
        elsif equal(text[tstart],'@') then
            base = 8
        elsif equal(text[tstart],'!') then
            base = 2
        elsif find(text[tstart], {'\t', ' ', #A0})=0 then
            exit
        end if
        tstart += 1
    end while

    -- Ignore trailing whitespace
    while tstart<=tend do
        if equal(text[tend], '-') and sign=0 then
            sign = -1
        elsif equal(text[tend],'+') and sign=0 then
            sign = 1
        elsif equal(text[tend],'%') then
            pc *= 100
        elsif find(text[tend], {'\t', ' ', #A0, #A2})=0 then
            exit
        end if
        tend -= 1
    end while

    -- Set the default sign.
    if sign=0 then
        sign = 1
    end if

    for i=tstart to tend do
        if lhs>0 and find(text[i],"_")>0 then
            -- ignore an embedded grouping characters.
        elsif digcnt=0 and currency=0 and find(text[i],vCurrencySym)>0 then
            -- ignore a single leading currency symbol.
            currency = i
        else
            v = find(text[i], vDigits)
            if v>=vIgnored+base then
                v = 0
            end if
            if v<vIgnored then
                if v=decimal_mark then -- A dot found, or an ignored character.
                    if dot=0 then
                        dot = 1
                    else
                        v = 0 -- this period is illegal
                    end if
                -- else ignore
                end if
                if v=0 then
                -- Invalid char so force a zero return.
                    note = i
                    if notify=0 then
                        sign = 0
                    end if
                    exit
                end if
            else
                v -= vIgnored
                if v<0 or v>base then
                    if not currency and find(text[i],vCurrencySym) then
                        currency = 1
                        nomoredigit = 1
                    else
                        note = i
                    -- Illegal char found.
                        if notify=0 then
                            sign = 0
                        end if
                        exit
                    end if
                elsif nomoredigit then  -- trailing currency sign, shouldn't be any more digits
                    note = i
                    if notify=0 then
                        sign = 0
                    end if
                    exit
                else
                    if dot=1 then
                        rhs *= base
                        rh = (rh*base)+v
                        digcnt += 1
                    else
                        lhs += 1
                        lh = (lh*base)+v
                        digcnt += 1
                    end if
                end if
            end if
        end if

        -- I got to the end without error!
        if i=tend then
            note = 0
        end if

    end for

    if rh=0 and pc=1 then
        -- Common situation optimised for speed.
        value = lh*sign
    else
        value = ((lh+(rh/rhs))*sign)/pc
    end if

    if notify=0 then
        return value
    else
        -- Error if no actual digits where converted.
        if note=0 and digcnt=0 and value=0 then
            note = 1
        end if
        return {value, note}
    end if
end function

--/topic Support Routines
--/func w32or_all( object pData )
--/desc Calculates a binary OR against each element in /pData
--/ret ATOM: The OR'd value
--
--example:
--/code
--      atom flags
--      flags = w32or_all({WS_CHILD, WS_VISIBLE, WS_BORDER})
--/endcode

global function w32or_all(object pData)
atom lResult

    if atom(pData) then
        return pData
    end if

    if length(pData)=0 then
        return 0
    end if

    if length(pData)=1 then
        return w32or_all(pData[1])
    end if

    lResult = or_bits(pData[1], pData[2])
    for i=3 to length(pData) do
        lResult = or_bits(lResult, pData[i])
    end for

    return lResult
end function

--/topic Support Routines
--/func w32findKeyEx( object key, sequence list, object element )
--/desc Find key in list using the depth indicated in element.
--/ret INTEGER: The index for the item that contains /i key. Zero if not found.
--The /i element parameter specifies which sub-elements are to be inspected. 
--
-- Example:
--/code
--      -- find a value from a list
--      constant aList = { {"red", {17, 1, "ap"}}, {"blue", {10,3,"ef"}}, 
--                         {"orange", {299, 9, "op"}}, {"black", {0,4, "yz"} } }
--      integer at
--      at = w32findKey( "op", aList, {2,3} )
--      -- 'at' should now equal 3.
--      at = w32findKey( 10, aList, {2,1} )
--      -- 'at' should now equal 2.
--      at = w32findKey( "gh", aList, {2,3} )
--      -- 'at' should now equal 0.
--/endcode


global function w32findKeyEx(object pKey, object pList, object pElement)
integer lPos
    if atom(pList) then
        return equal(pKey,pList)
    end if

    if sequence(pElement) and length(pElement)=1 then
        pElement = pElement[1]
    end if

    if sequence(pElement) then
        lPos = 0
        for i=1 to length(pList) do
            lPos = w32findKeyEx(pKey, pList[i][pElement[1]], pElement[2..length(pElement)])
            if lPos!=0 then
                return i
            end if
        end for
        return 0
    else
        if pElement<1 then
            for i=1 to length(pList) do
                if sequence(pList[i]) and find(pKey, pList[i]) then
                    return i
                elsif atom(pList[i]) and  equal(pKey, pList[i]) then
                    return i
                end if
            end for
        else
            for i=1 to length(pList) do
                if sequence(pList[i]) and length(pList[i])>=pElement and equal(pList[i][pElement], pKey) then
                    return i
                elsif i=pElement and equal(pList[i], pKey) then
                    return i
                end if
            end for
        end if
    end if
    return 0

end function

--/topic Support Routines
--/func w32findKey( object key, sequence list )
--/desc Find the element in /i list that has /i key as its first element.
--/ret INTEGER: The index for the item that contains /i key. Zero if not found.
--The /i list must be a sequence of sequences. That is, each element in /i list must
--be a sequence containing at least one element. The /i key is the first element in
-- each sub-sequence of /i list.
--
-- Example:
--
--/code
--      -- find a value from a list
--      constant aList = { {"red", 17}, {"blue", 10}, {"orange", 299}, {"black", 0} }
--      integer at
--      at = w32findKey( "orange", aList )
--      -- 'at' should now equal 3.
--      at = w32findKey( "white", aList )
--      -- 'at' should now equal 0.
--/endcode

global function w32findKey(object pKey, sequence pList)
    return w32findKeyEx(pKey, pList, 1)
end function

--/topic Support Routines
--/func w32removeIndex( integer index, sequence list )
--/desc Remove the element at position /i index from the sequence /i list
--/ret The /i list without the element.
--
--If /i index is greater than the list length, the list is returned intact.
-- If /i index is less than 1, it represents the end of the list PLUS
-- the index. So an index of '-1' means the second-last element.
--
-- Example:
--
--/code
--      -- Remove the 4th element from the Name List
--      nameList = w32removeIndex( 4, nameList )
--      -- Remove the last element.
--      nameList = w32removeIndex( 0, nameList )
--/endcode

global function w32removeIndex(integer index, sequence list)
    -- Sanity check for non-existant elements.
    if index>length(list) then
        return list
    end if

    if index<1 then
        index = length(list)+index
        if index<1 then
            return list
        end if
    end if

    return list[1..index-1] & list[index+1..length(list)]
end function

--/topic Support Routines
--/func w32lookup(object pItem, sequence pSource, sequence pTarget)
--/desc Returns the corresponding element.
--/ret OBJECT: The corresponding element or 0/{} if not found.
--This searches /i pSource for /i pItem and if found, it returns the
-- corresponding element from /i pTarget. /n
--If /i pItem is not found in /i pSource, then what is returned depends
-- on a few things. /n
--If /i pTarget is longer than /i pSource then the last element in
-- /i pTarget is returned. This implements a way to return a default
-- value of your choosing. /n
--If /i pTarget is not longer than /i pSource then if the first element
-- of /i pTarget is an atom then zero is returned; otherwise an empty
-- sequence is returned. If pTarget is empty, then 0 is returned.
--
--Examples:
--
--/code
--       x = w32lookup('a', "cat", "dog") --> 'o'
--       x = w32lookup('d', "cat", "dogx") --> 'x'
--       x = w32lookup('d', "cat", "dog") --> 0
--       x = w32lookup("ape", {"ant","bear","cat"}, {"spider","seal","dog"}) --> ""
--       x = w32lookup("ant", {"ant","bear","cat"}, {"spider","seal","dog"}) --> "spider"
--/endcode
global function w32lookup(object pItem, sequence pSource, sequence pTarget)
integer lPosn

-- invalid pTarget
    if not length(pTarget) then
        return 0
    end if

    lPosn = find(pItem, pSource)
    if lPosn>0 then
-- added CChris <oedoc@free.fr> Dec 5, 2006
-- avoided crash when pTarget is too short
        if length(pTarget)>=lPosn then
            return pTarget[lPosn]
        end if
    elsif length(pTarget)>length(pSource) then
        return pTarget[length(pTarget)]
    end if

    -- pTarget is too short, return a guess of an invalid, non crashing value
    if atom(pTarget[1]) then
        return 0
    else
        return ""
    end if

end function

--/topic Support Routines
--/func w32iff (atom test, object ifTrue, object ifFalse)
--/desc Used to embed an 'if' test inside an expression.
--/ret If /i test is /b true then /i ifTrue is returned otherwise /i ifFalse is returned.
--
--Example
--/code
--         msg = sprintf("%s: %s", {
--                      w32iff(ErrType = 'E', "Fatal error", "Warning"),
--                      errortext } )
--/endcode

----------------------------------------------------------------------------
global function w32iff(atom test, object ifTrue, object ifFalse)
    -- returns ifTrue if flag is true, else returns ifFalse
    -- ex:  ? w32iff( 1, "this", "that" )
    --      --> "this"
    if test then
        return ifTrue
    else
        return ifFalse
    end if

end function


--/topic Support Routines
--/func w32removeItem( object item, sequence list)
--/desc Removes /i item from the /i list, if it is in the list.
--/ret The /i list without the item.
--
--Example
--/code
--     -- Remove the name 'fred' from the list.
--     nameList = w32removeItem("fred", nameList)
--/endcode
----------------------------------------------------------------------------
global function w32removeItem(object item, sequence list)
    -- Removes the item from the list if item is in the list.
    item = find(item,  list)
    if item!=0 then
        return list[1..item-1] & list[item+1..length(list)]
    else
        return list
    end if
end function

--/topic Support Routines
--/func w32insertElement( sequence list, integer pos, object item )
--/desc Inserts the /i object into the /i list at position /i pos
--/ret The /i list with the /i item inserted.
--The new element becomes the /i"pos"'th element in the list. /n
--
--This routine also handles positions less than 1. If pos < 1 then it will
-- be changed to length(list)+pos+1, meaning that a value of 0 will add to the
-- end of the list, -1 just before the end, etc... If the recalculated /i pos
-- is less than 1 then the element is added to the front of the /i list.
--
--Example
--/code
--     -- Insert the name 'fred' before the 3rd element in the list.
--     nameList = w32insertElement(nameList, 3 , "fred")
--/endcode

----------------------------------------------------------------------------
global function w32insertElement(sequence list, integer pos, object item)
    if pos=0 then
        pos = length(list)+1
    elsif pos<0 then
        pos = length(list)+1+pos
        if pos<1 then
            pos = 1
        end if
    end if
    if sequence(item) then
        item = {item}
    end if
    return list[1..pos-1] & item & list[pos..length(list)]
end function

--/topic Support Routines
--/func w32replaceItem(sequence pList, object pOld, object pNew)
--/desc Replaces all occurances of /i pOld with /i pNew
--/ret SEQUENCE: Updated /i pList.
--Note this routine will not do recursive replacements. That is, if pNew contains pOld, it
--doesn't go into a never-ending loop.
--
--Example: /n
--/code
--  sequence lRes
--  lRes = w32replaceItem("a;b;c;d;e", ";", " ; ")
--  -- gives "a ; b ; c ; d ; e"
--  lRes = w32replaceItem("The ^^ word is correct", "^^", "third")
--  -- gives "The third word is correct"
--/endcode

global function w32replaceItem(sequence pList, object pOld, object pNew)
integer lPos
integer lFrom
integer lLenOld
integer lLenNew

    if atom(pOld) then
        pOld = {pOld}
    end if
    if atom(pNew) then
        pNew = {pNew}
    end if

    lLenOld = length(pOld)
    lLenNew = length(pNew)

    lFrom = 1
    lPos = match(pOld, pList[lFrom..length(pList)])
    while lPos>0 do
        lPos += lFrom-1
        pList = pList[1..lPos-1] & pNew & pList[lPos+lLenOld..length(pList)]
        lFrom += lLenNew
        lPos = match(pOld, pList[lFrom..length(pList)])
    end while

    return pList

end function

----------------- Sorting ------------------------------------------
--/topic Support routines
--/func w32ordered_find(object x,sequence s,integer verif)
--/desc Uses a binary tree search to find /i x in /i s, possibly
-- checking if /i s is well ordered.
--/ret (INTEGER) The position of /i x in /i s, if any, or the opposite of the
-- position it would have if inserted.
-- /i verif is nonzero if it is desired to check whether /i s is increasing or decreasing.
-- A value of 0 skips the check. 0 is returned if the check fails.
-- If /i x would be inserted at the end of /i s, -1-length(/i s) is returned.
-- If /i s is {}, 0 is returned. If /i s has length 1, it is assumed to be increasing.
global function w32ordered_find(object x,sequence s,integer verif)
integer flag,p,q,r
    if not length(s) then
        return 0
    elsif length(s)=1 then
        return w32lookup(compare(x,s[1]),{-1,0,1},{-1,0,-2})
    end if
    flag = compare(s[2],s[1])
    if flag=0 then
        return 0
    elsif verif then
        for i=3 to length(s) do
            if compare(s[i],s[i-1])!=flag then
                return 0
            end if
        end for
    end if
    p = compare(s[1],x)
    if p=flag then
        return -1
    elsif p=0 then
        return 1
    end if
    p = compare(x,s[length(s)])
    if p=flag then
        return -1-length(s)
    elsif p=0 then
        return length(s)
    end if
    if flag=1 then
        p = 1
        q = length(s)
    else
        q = -1
        p = length(s)
    end if
    while w32abs(q-p)>1 do
        r = floor((p+q)/2)
        verif = compare(x,s[r])
        if verif=0 then
            return r
        elsif verif=flag then
            q = r
        else
            p = r
        end if
    end while
    return w32iff(flag=1,-q,-p)
end function

include sort.e

--/topic Support routines
--/func w32index(sequence s)
--/desc Returns the list of positions of items in /i s.
--/ret (SEQUENCE) A sequence s1 such that s1[i] is the position of the i-th item of sort(s).
global function w32index(sequence s)
sequence s1,result,rep
integer p

    s1 = sort(s)
    result = repeat(0,length(s))
    rep = result
    for i=1 to length(s) do
        p = w32ordered_find(s[i],s1,1)
        result[p+rep[p]] = i
        rep[p] += 1
    end for
    return result
end function

--/topic Support routines
--/func w32index_by(sequence s,integer field)
--/desc Indexes /i s using the field number /i field.
--/ret (SEQUENCE) A sequence of indexes like /w32index().
global function w32index_by(sequence s,integer field)
    for i=1 to length(s) do
        s[i] = s[i][field]
    end for
    return w32index(s)
end function

----------------- Predefined Whitespace Routines --------------------

sequence whitespace
whitespace = ' ' & 9 & 10 & 11 & 12 & 13

--/topic Support Routines
--/func w32trim(object pSource)
--/desc Removes any whitespace chars from both ends of /i pSource
--/ret The original sequence with the matching characters removed.
--/i pSource is the sequence from which to remove whitespace. /n
-- If /i pSource is enclosed in quotes (single or double) they are
-- removed. This is a way to force leading and/or trailing whitespace
--Example:
--/code
--       sequence result
--       result = w32trim("  abc def  ")
--       ? result  -- Should display "abc def"
--       result = w32trim("\"  abc def  \"")
--       ? result  -- Should display "  abc def  "
--/endcode
------------------------------------------------------
global function w32trim(object pSource)
------------------------------------------------------
integer lStart, lEnd

    if atom(pSource) then
        return pSource
    end if
    lEnd = length(pSource)
    if lEnd=0 then
        return pSource
    end if

    -- Check for quote-enclosed string.
    if length(pSource)>=2 and find(pSource[1], "'\"") and pSource[lEnd]=pSource[1] then
        return pSource[2..length(pSource)-1]
    end if

    -- Common check.
    if not find(pSource[1], whitespace) and not find(pSource[lEnd], whitespace) then
        return pSource
    end if


    lStart = 1
    while lStart<=lEnd do
        if find(pSource[lStart], whitespace)=0 then
            exit
        end if
        lStart += 1
    end while

    while lEnd>lStart do
        if find(pSource[lEnd], whitespace)=0 then
            exit
        end if
        lEnd -= 1
    end while

    pSource = pSource[lStart..lEnd]
    -- Check for quote-enclosed string.
    if length(pSource)>=2 and find(pSource[1], "'\"") and pSource[length(pSource)]=pSource[1] then
        return pSource[2..length(pSource)-1]
    else
        return pSource
    end if

end function

--/topic Support Routines
--/func w32trim_right(sequence pSource)
--/desc Removes any whitespace chars from the end of /i pSource
--/ret The original sequence with the matching characters removed.
--/i pSource is the sequence from which to remove whitespace. /n
--Example:
--/code
--       sequence result
--       result = w32trim_right("  abc def  ")
--       ? result  -- Should display "  abc def"
--/endcode

------------------------------------------------------
global function w32trim_right(sequence pSource)
------------------------------------------------------
integer lStart, lEnd

    lEnd = length(pSource)
    if lEnd=0 then
        return pSource
    end if

    -- Common check first.
    if not find(pSource[lEnd], whitespace) then
        return pSource
    end if


    lStart = 1

    while lEnd>=lStart do
        if find(pSource[lEnd], whitespace)=0 then
            exit
        end if
        lEnd -= 1
    end while

    return pSource[lStart..lEnd]

end function

--/topic Support Routines
--/func w32trim_left(sequence pSource)
--/desc Removes any whitespace chars from the beginning of /i pSource
--/ret The original sequence with the matching characters removed.
--/i pSource is the sequence from which to remove whitespace. /n
--Example:
--/code
--       sequence result
--       result = w32trim_left("  abc def  ")
--       ? result  -- Should display "abc def  "
--/endcode

------------------------------------------------------
global function w32trim_left(sequence pSource)
------------------------------------------------------
integer lStart, lEnd

    lEnd = length(pSource)
    if lEnd=0 then
        return pSource
    end if
    lStart = 1

    while lStart<=lEnd do
        if find(pSource[lStart], whitespace)=0 then
            exit
        end if
        lStart += 1
    end while

    return pSource[lStart..lEnd]

end function

--/topic Support Routines
--/func w32split(sequence pSource, object pDelim)
--/desc Returns the undelimited substrings
--/ret SEQUENCE: A set of sequences, one per delimited substring in /i pSource
--/i pSource is the sequence from which to extract the substrings. /n
--/i pDelim is the delimiter character, delimiter string, or a set of
-- single character delimiters. /n
--
--/b "Note 1" - if /i pDelim is an atom and not equal to '{' then sections
--of text enclosed in matching '{' and '}' are not scanned. /n
--/b "Note 2" - if /i pDelim is a single string inside a sequence, then
-- it is considered to be a set of delimiter characters and any one of
-- them can delimiter the /i pSource. The results in this case are that
-- each delimitered text is followed by the sequence containing the
-- character that caused the delimitation. A final empty sequence means
-- that no delimiter was found to end the /i pSource text.
--
--Example:
--/code
--  sequence result
--  result = w32split("if abc = def then xyz()", ' ')
--  ? result  -- Should be {"if","abc","=","def","then","xyz()"}
--
--  -- Example of embedded delimiter...
--  result = w32split("event=Click,font={Courier,12}", ',')
--  ? result  -- Should be {"event=Click","font={Courier,12}"}
--
--  -- Example of delimiter set...
--  result = w32split("event=Click,font={Courier,12}", {"=,{}"})
--  ? result  -- Should be {"event","=","Click",",","font","=","","{","Courier",",","12","}"}
--
--/endcode 

------------------------------------------------------
global function w32split(sequence pSource,  object pDelim)
------------------------------------------------------
sequence lResult
integer l,r,e
integer lDepth
integer lP, lS

    lResult = {}
    lDepth = 0
    l = 1
    r = 1
    if atom(pDelim) then
        -- Single char delimiter.
        while r<=length(pSource) do
            if lDepth=0 then
                if pSource[r]=pDelim then
                    lResult = append(lResult, pSource[l..r-1])
                    l = r+1
                    r = l
                else
                    if find(pSource[r],"({") then
                        lDepth += 1
                        lP = pSource[r]
                        if lP='{' then
                            lS = '}'
                        else
                            lS = ')'
                        end if
                    end if
                    r += 1
                end if
            else
                if pSource[r]=lP then
                    lDepth += 1
                elsif pSource[r]=lS then
                    lDepth -= 1
                end if
                r += 1
            end if
        end while
        lResult = append(lResult, pSource[l..r-1])

    elsif length(pDelim)>0 then
        if atom(pDelim[1]) then
            -- Delimiter is assumed to be a simple string.
            e = r+length(pDelim)-1
            while e<=length(pSource) do
                if equal(pSource[r..e], pDelim) then
                    lResult = append(lResult, pSource[l..r-1])
                    l = e+1
                    r = l
                    e = r+length(pDelim)-1
                else
                    r += 1
                    e += 1
                end if
            end while

            lResult = append(lResult, pSource[l..length(pSource)])
        else
            -- In this case, the format is a string inside a sequence
            -- and the string is a list of single char delimiters.
            pDelim = pDelim[1]
            while r<=length(pSource) do
                if find(pSource[r], pDelim) then
                    lResult = append(lResult, pSource[l..r-1])
                    lResult = append(lResult, {pSource[r]})
                    l = r+1
                    r = l
                else
                    r += 1
                end if
            end while
            lResult = append(lResult, pSource[l..r-1])
            lResult = append(lResult, {})
        end if
    else
        return pSource
    end if
    return lResult
end function

global constant
    w32DELIM_FIRST = 1,
    w32DELIM_LAST = 2
--/topic Support routines
--/func w32join(sequence substrings,object delimiter,integer flag)
--/desc Builds a string by concatenating itmes in substrings, separated by delimiters.
--/ret (SEQUENCE) a whole sequence made from all substrings stitched together.
-- If the w32DELIM_FIRST flag is set, the returned string will start by the delimiter.
-- If the w32DELIM_LAST flag is set, the returned string will end with the delimiter.
global function w32join(sequence substrings,object delimiter,integer flags)
sequence result,item

    if and_bits(flags,w32DELIM_FIRST) then
        result = delimiter & substrings[1]
    else
        result = substrings[1]
    end if
    for i=2 to length(substrings) do
        item = delimiter & substrings[i]
        result &= item
    end for
    if and_bits(flags,w32DELIM_LAST) then
        return result & delimiter
    else
        return result
    end if
end function

--/topic Miscellaneous
--/func w32MinSequenceSize(sequence pList, integer pMinSize, object pNewData)
--/desc Pads /i pList to the right until its length reaches /i pMinSize using /i pNewData as filler.
--/ret The padded sequence, unchanged if its size was not less than /i pMinSize on input.
global function w32MinSequenceSize(sequence pList, integer pMinSize, object pNewData)

    if length(pList)<pMinSize then
        pList &= repeat(pNewData, pMinSize-length(pList))
    end if

    return pList
end function

global constant
    w32BOUNDS_MIN = 0,
    w32BOUNDS_MAX = 1,
    w32BOUNDS_INDEX = 0,
    w32BOUNDS_VALUE = 2,
    w32BOUNDS_PAIR = 4,
    w32BOUNDS_INDEX_FIRST = w32BOUNDS_INDEX,
    w32BOUNDS_VALUE_FIRST = w32BOUNDS_VALUE
--/topic Utilities
--/func w32bounds(sequence items,integer flags)
--/desc Returns index(es) or value(s) of the minimal/maximal element in a sequence
--/ret (OBJECT) One or two indices, or one or two values, depending on flags.
-- /i flags is the sum of zero or more of:
--/li w32BOUNDS_MIN=0: return a minimum index or value;
--/li w32BOUNDS_MAX=1: return a maximum index or value;
--/li w32BOUNDS_INDEX=0,
--/li w32BOUNDS_VALUE=2,
--/li w32BOUNDS_PAIR=4,
--/li w32BOUNDS_INDEX_FIRST=w32BOUNDS_INDEX
--/li w32BOUNDS_VALUE_FIRST=w32BOUNDS_VALUE
-- Obviously, only some combinations make sense.
global function w32bounds(sequence items,integer flags,integer from)
object x,y
integer cmp

    if from<=0 or from>length(items) then
        if and_bits(flags,w32BOUNDS_VALUE+w32BOUNDS_PAIR) then
            return {}
        else
            return 0
        end if
    end if
    x = items[from]
    cmp = w32iff(and_bits(flags,w32BOUNDS_MAX),-1,1)
    for i=from+1 to length(items) do
        y = items[i]
        if compare(x,y)=cmp then
            x = y
            from = i
        end if
    end for
    if and_bits(flags,w32BOUNDS_PAIR) then
        if and_bits(flags,w32BOUNDS_VALUE_FIRST) then
            return {x,from}
        else
            return {from,x}
        end if
    elsif and_bits(flags,w32BOUNDS_VALUE) then
        return x
    else
        return from
    end if
end function

-- Character Type Tests

global constant
    Control_CT      = #0001,
    Alpha_CT        = #0002,
    Digit_CT        = #0004,
    Punct_CT        = #0008,
    Symbol_CT       = #0010,
    White_CT        = #0020,
    Lowercase_CT    = #0040,
    Uppercase_CT    = #0080,
    Printable_CT    = #0100,
    NameChar_CT     = #0200,
    User1_CT        = #0400,
    User2_CT        = #0800,
    User3_CT        = #1000,
    User4_CT        = #2000,
    User5_CT        = #4000,
    User6_CT        = #8000,
    AlphaNumeric_CT = #0006

sequence vCharAttr

vCharAttr = repeat(0, 256)

integer ctype_range_lo,ctype_range_hi,ctype_mode
sequence ctype_string
constant pChar_VALID = 1, pChar_ATOM = 2,pChar_RANGE = 4
procedure sanity_CType(object pChar)
-- sets the vars above so as to allow faster/more versatile operation of *CType routines.
integer p
object c1,c2

    ctype_mode = 0
    if atom(pChar) then
        if pChar>=0 and pChar<=#FFFF then
            ctype_mode = pChar_VALID+pChar_ATOM
            ctype_range_lo = pChar+1
            vCharAttr = w32MinSequenceSize(vCharAttr,ctype_range_lo,0)
        end if
    elsif atom(pChar[1]) then -- plain list
        p = 0
        for i=1 to length(pChar) do
            c1 = pChar[i]
            if not integer(c1) or c1<0 or c1>#FFFF then
                if p=0 then
                    p = i
                end if
            elsif p then
                pChar[p] = c1
                p += 1
            end if
        end for
        if p=0 then
            ctype_mode = pChar_VALID
--/**/      ctype_string = sq_add(pChar,1)          --/* -- Phix
            ctype_string = pChar+1                  --*/ -- RDS
        elsif p>1 then
            ctype_mode = pChar_VALID
--/**/      ctype_string = sq_add(pChar[1..p-1],1)  --/* -- Phix
            ctype_string = pChar[1..p-1]+1          --*/ -- RDS
        end if
        if ctype_mode then
            vCharAttr = w32MinSequenceSize(vCharAttr,w32bounds(ctype_string,w32BOUNDS_MAX+w32BOUNDS_VALUE,1),0)
        end if
    elsif length(pChar)=2 and sequence(pChar[2]) and length(pChar[1]) and length(pChar[2]) then
        c1 = pChar[1][1]
        c2 = pChar[2][1]
        if integer(c1) and integer(c2) and c1<=c2 and c1<=#FFFF and c2>=0 then
            if c1<0 then
                c1 = 0
            end if
            if c2>#FFFF then
                c2 = #FFFF
            end if
            ctype_range_lo = c1+1
            ctype_range_hi = c2+1
            ctype_mode = pChar_VALID+pChar_RANGE
            vCharAttr = w32MinSequenceSize(vCharAttr,ctype_range_hi,0)
        end if
    end if
end procedure

--/topic Support Routines
--/func w32CType( object pChar, object pSet)
--/desc Tests a character to see if it 'belongs' to a specified CharType set.
--/ret OBJECT: w32True or w32False if pChar is an atom, a sequence of these otherwise..
--/i pChar is the character(s) to check. This can take the form of a single
-- character such as 'A', a list of characters such as "1234567890", or
-- a range of characters such as {"a", "z"}. /n
--/i pSet is either a single CharType code or a set of codes. /n
--See /w32SetCType for a complete list of valid CharType codes. /n
-- w32True is returned only if the character belongs to all sets /i pSet refers to.
-- Only single byte characters are supported.
--
--Example:
--/code
--      if w32CType( text[i], Alpha_CT) then
--          ... process an alphabetic char
--      elsif w32CType( text[i], Digit_CT) then
--          ... process an digit char
--      end if
--/endcode
global function w32CType(integer pChar, object pSet)
sequence sResult
atom lValue
integer shift

    sanity_CType(pChar)

    if not ctype_mode then -- invalid input
--      if atom(pChar) then
        return w32False
--      else
--          return {}
--      end if
    end if

    if and_bits(ctype_mode,pChar_ATOM) then
        lValue = vCharAttr[ctype_range_lo]

        if atom(pSet) then
            pSet = {pSet}
        end if

        for i=1 to length(pSet) do
            if not and_bits(lValue,pSet[i]) then
                return w32False
            end if
        end for
        return w32True
    elsif and_bits(ctype_mode,pChar_RANGE) then
        shift = ctype_range_lo-1
        sResult = repeat(w32True,ctype_range_hi-shift)
        for i=ctype_range_lo to ctype_range_hi do
            lValue = vCharAttr[i]
            for j=1 to length(pSet) do
                if not and_bits(lValue,pSet[j]) then
                    sResult[i-shift] = w32False
                    exit
                end if
            end for
        end for
    else
        sResult = repeat(w32True,length(ctype_string))
        for i=1 to length(ctype_string) do
            lValue = vCharAttr[ctype_string[i]]
            for j=1 to length(pSet) do
                if not and_bits(lValue,pSet[j]) then
                    sResult[i] = w32False
                    exit
                end if
            end for
        end for
    end if
    return sResult
end function

--/topic Support Routines
--/func w32GetCType( object pChar)
--/desc Gets the CharType for the specified character(s)
--/ret Same datatype as /i pChar: The CharType code for the character(s)
--/i pChar is the character(s) to check. This can take the form of a single
-- character such as 'A', a list of characters such as "1234567890", or
-- a range of characters such as {"a", "z"}. /n
--
--Example:
--/code
--      object lRC
--      -- Returns an integer containing the CharType Codes for 'a'
--      lRC = w32GetCType('a')
--      -- Returns a sequence containing the CharType Codes for 'a', '1' and '$'
--      lRC = w32GetCType("a1$")
--/endcode
global function w32GetCType(object pChar)
    -- Only support single byte characters.

    sanity_CType(pChar)
    if not ctype_mode then -- invalid input
        if atom(pChar) then
            return w32False
        else
            return {}
        end if
    end if

    if and_bits(ctype_mode,pChar_ATOM) then
        return vCharAttr[ctype_range_lo]
    elsif and_bits(ctype_mode,pChar_RANGE) then
        return vCharAttr[ctype_range_lo..ctype_range_hi]
    else
        for i=1 to length(pChar) do
            pChar[i] = vCharAttr[ctype_string[i]]
        end for
        return pChar
    end if

end function

--/topic Support Routines
--/proc w32SetCType( object pChar, object pSet)
--/desc Sets one or more character types used by /w32CType and /w32GetCType
--/i pChar is the character(s) to set. This can take the form of a single
-- character such as 'A', a list of characters such as "1234567890", or
-- a range of characters such as {"a", "z"}. /n
--/i pSet is either a single CharType code or a list of CharType codes. /n
--The valid CharType codes are ... /n
--/li Control_CT
--/li Alpha_CT
--/li Digit_CT
--/li Punct_CT
--/li Symbol_CT
--/li White_CT
--/li Lowercase_CT
--/li Uppercase_CT
--/li Printable_CT
--/li NameChar_CT
--/li User1_CT
--/li User2_CT
--/li User3_CT
--/li User4_CT
--/li User5_CT
--/li User6_CT
--/li AlphaNumeric_CT (a combination of Alpha_CT and Digit_CT)
--
-- You may define new character sets: the associated value must be a power of 2
-- greater than #8000 and less than #20000000.
--
--Example:
--/code
--      w32SetCType( 255, User1_CT) -- Single char
--      w32SetCType( {"n","z"}, User3_CT ) -- Range of chars
--      w32SetCType( "13579", User2_CT ) -- List of chars
--/endcode
global procedure w32SetCType(object pChar, object pSet)

atom lSet

    sanity_CType(pChar)
    if not ctype_mode then -- invalid input
        return
    end if

    if atom(pSet) then
        lSet = pSet
    else
        lSet = w32or_all(pSet)
    end if

    if integer(pChar) then
        vCharAttr[ctype_range_lo] = lSet
    elsif and_bits(ctype_mode,pChar_RANGE) then
        vCharAttr[ctype_range_lo..ctype_range_hi] = lSet
    else
        for i=1 to length(ctype_string) do
            vCharAttr[ctype_string[i]] = lSet
        end for
    end if
end procedure

--/topic Support routines
--/proc addToCType(object pChar,object pSet)
--/desc Adds characters to one or more sets.
--/i pChar is the character(s) to set. This can take the form of a single
-- character such as 'A', a list of characters such as "1234567890", or
-- a range of characters such as {"a", "z"}. /n
--/i pSet is either a single CharType code or a list of CharType codes. /n
global procedure addToCType(object pChar, object pSet)
atom lSet

    sanity_CType(pChar)
    if not ctype_mode then -- invalid input
        return
    end if

    if sequence(pSet) then
        lSet = w32or_all(pSet)
    else
        lSet = pSet
    end if
    if and_bits(ctype_mode,pChar_ATOM) then
        vCharAttr[ctype_range_lo] = or_bits(vCharAttr[ctype_range_lo],lSet)
    elsif and_bits(ctype_mode,pChar_RANGE) then
--/**/  vCharAttr[ctype_range_lo..ctype_range_hi] = sq_or_bits(vCharAttr[ctype_range_lo..ctype_range_hi],lSet)  --/* -- Phix
        vCharAttr[ctype_range_lo..ctype_range_hi]=or_bits(vCharAttr[ctype_range_lo..ctype_range_hi],lSet)       --*/ -- RDS
    else
        for i=1 to length(ctype_string) do
            vCharAttr[ctype_string[i]] = or_bits(vCharAttr[ctype_string[i]],lSet)
        end for
    end if
end procedure

--/topic Support routines
--/proc removeFromCType(object pChar,object pSet)
--/desc Removes characters from one or more sets.
--/i pChar is the character(s) to set. This can take the form of a single
-- character such as 'A', a list of characters such as "1234567890", or
-- a range of characters such as {"a", "z"}. /n
--/i pSet is either a single CharType code or a list of CharType codes. /n
global procedure removeFromCType(object pChar, object pSet)
atom lSet

    sanity_CType(pChar)
    if not ctype_mode then -- invalid input
        return
    end if

    if sequence(pSet) then
        lSet = w32or_all(pSet)
    else
        lSet = pSet
    end if
    if and_bits(ctype_mode,pChar_ATOM) then
        vCharAttr[ctype_range_lo] -= and_bits(vCharAttr[ctype_range_lo],lSet)
    elsif and_bits(ctype_mode,pChar_RANGE) then
--/**/  vCharAttr[ctype_range_lo..ctype_range_hi] = sq_sub(vCharAttr[ctype_range_lo..ctype_range_hi],sq_and_bits(vCharAttr[ctype_range_lo..ctype_range_hi],lSet)) --/* -- Phix
        vCharAttr[ctype_range_lo..ctype_range_hi]-=and_bits(vCharAttr[ctype_range_lo..ctype_range_hi],lSet)                                                     --*/ -- RDS
    else
        for i=1 to length(ctype_string) do
            vCharAttr[ctype_string[i]] -= and_bits(vCharAttr[ctype_string[i]],lSet)
        end for
    end if
end procedure

w32SetCType({{097},{122}}, Alpha_CT+Lowercase_CT+Printable_CT+NameChar_CT)
w32SetCType({{065},{090}}, Alpha_CT+Uppercase_CT+Printable_CT+NameChar_CT)
w32SetCType({{009},{013}}, Control_CT+White_CT)
w32SetCType({{000},{008}}, Control_CT)
w32SetCType({{014},{031}}, Control_CT)
w32SetCType({{048},{057}}, Digit_CT+Printable_CT+NameChar_CT)
w32SetCType({{033},{047}}, Punct_CT+Printable_CT)
w32SetCType({{058},{064}}, Punct_CT+Printable_CT)
w32SetCType({{091},{096}}, Punct_CT+Printable_CT)
w32SetCType({{123},{126}}, Punct_CT+Printable_CT)
w32SetCType({{127},{159}}, Symbol_CT)
w32SetCType({{161},{255}}, Symbol_CT)
w32SetCType({32,160}, White_CT+Printable_CT)
w32SetCType('_', Punct_CT+Printable_CT+NameChar_CT)


--/topic Support Routines
--/func w32Encode(sequence PlainText, sequence Mask, integer Size)
--/desc This routine transforms a string into an encoded form
--/ret SEQUENCE: A set of /i Size ascii digits.
--This creates a one way hash based on the values of /i PlainText, /i Mask and /i Size.
--/i PlainText is usually a password entered in by a user. /n
--/i Mask is anything your application wishes to use to ensure that two identical
-- /i PlainText values will generate different outputs. For example if this is
-- used for passwords, the /i Mask could be the user's ID. Thus two people that
-- just happen to have the same password cannot know that just by looking at the
-- output of this function. /n
--/i Size is the number of encoded characters you wish returned. This can be
-- any positive integer. The larger the /i Size the less chance that two different
-- /i"PlainText" - /i"Mask" combinations will generate the same output. Values
-- from /b 16 to /b 64 would be sufficient in nearly all cases.
--
--Example:
--/code
--      sequence lCode
--      lCode = w32Encode(PasswordText, UserID, 16)
--/endcode
global function w32Encode(sequence pPlainText, sequence pMask, integer pSize)
sequence lResult
atom lValue
sequence lText
sequence lMask
sequence lDigits
integer j
atom lRound

    lResult = ""
    lText = sprintf("%d%d%s%s%d", {
                                   pSize,
                                   length(pPlainText),
                                   pPlainText,
                                   pMask,
                                   length(pMask)
                                  })
    lText &= reverse(lText)
    lMask = sprintf("%s%d%s%d%d%d", {
                                     pMask,
                                     length(lText),
                                     pPlainText,
                                     length(pMask)+49,
                                     length(pPlainText)+49,
                                     pSize+49
                                    })
    j = 0
    for i=1 to length(lText) do
        j += 1
        if j>length(lMask) then
            j = 1
        end if
        if and_bits(lMask[length(lMask)-j+1], 1) then
            lText[i] = (lText[i]*lMask[j])-(lText[i]/(lMask[j]))
        else
            lText[i] = (lText[i]*lText[i])+(lMask[j]*lMask[j])
        end if
        if i>1 then
            lText[i] += lText[i-1]
        end if
    end for
    lText[1] += lMask[length(lMask)]

    lValue = length(pPlainText)*length(pMask)*(11.76943+(pSize+length(lText)*length(lMask)))
    lRound = length(pPlainText)+length(pMask)
    while length(lResult)<pSize do
        for i=1 to length(lText) do
            lValue = (lValue*723.11251)+lText[i]-lRound

            if lText[1+length(lText)-i]!=0 then
                if lValue<1E-10 or lValue>1E+10 then
                    lValue = (length(pPlainText)-length(lResult))*length(pMask)*(1.76943+pSize)
                else
                    lValue = (lValue/(lText[1+length(lText)-i]) )
                end if
            end if
        end for
        lDigits = sprintf("%.15g", lValue)
        if length(lDigits)>2 then
            lDigits = lDigits[2..length(lDigits)-1]
        end if
        j = length(lDigits)
        while j>=1 do
            if lDigits[j]!='0' then
                lDigits = lDigits[1..j]
                exit
            end if
            j -= 1
        end while
        j = 1
        while j<=length(lDigits) do
            if find(lDigits[j], "0123456789")=0 then
                lDigits = lDigits[1..j-1] & lDigits[j+1..length(lDigits)]
            else
                j += 1
            end if
        end while
        if length(lDigits)>3 then
            lResult &= lDigits[3..length(lDigits)-2]
        end if
        lValue /= 691971
        lRound *= (length(lResult)-5.4321)
        lRound = -lRound
    end while

    return lResult[1..pSize]
end function

-------------------------------------------
--/topic Miscellaneous
--/func w32MaxWidth(object pData)
--/desc Computes the maximum width of elements of /i pData.
--/ret The maximum width.
-- The width of a string is its length. The width of an atom is the length of its
-- string representation using the %15.15g format specifier.
global function w32MaxWidth(object pData)
-------------------------------------------
integer w
integer n
    if atom(pData) then
        return length(w32trim(sprintf("%15.15g", pData)))
    end if

    if w32string(pData) then
        return length(pData)
    end if

    w = 0
    for i=1 to length(pData) do
        n = w32MaxWidth(pData[i])
        if n>w then
            w = n
        end if
    end for

    return w
end function

-------------------------------------------
--/topic Miscellaneous
--/func w32ToString(object pData)
--/desc Assembles a sequence from the substrings in /i pData, separated by commas.
--/ret A sequence concatenating all substrings of /i pdata, with a comma as separator.
-- The returned sequence is enclosed between '(' and ')'.
global function w32ToString(object pData)
-------------------------------------------
sequence ld

    if atom(pData) then
        return w32trim(sprintf("%15.15g", pData))
    end if

    if w32string(pData) then
        return pData
    end if

    ld = "("
    for i=1 to length(pData) do
        ld &= w32ToString(pData[i])
        if i!=length(pData) then
            ld &= ','
        else
            ld &= ")"
        end if
    end for
    return ld
end function

--/topic Support routines
--/func w32extract_formats(sequence s)
--/desc Determines all substrings of i s which are format specifiers
--/ret (SEQUENCE) A triple of lists: starting points, status, end points.
-- The returned lists have the same length:
--/li the first list holds the indexes of the starting '%' of the assumed format specifiers;
--/li the second list holds a 1 to mark an actual specifier, else a 0.
--/li the third list holds the terminating indexes of the assumesd format specifiers;
-- When a format specifier is succesfully parsed, its terminating index is the index of its last character.
-- Otherwise, it is the index of the first illegal character found past it starting index.
global function w32extract_formats(sequence s)
integer pos,nextstart,fallover,stage
sequence r1,r2,r3

    r1 = {} -- starting points
    r2 = {} -- status
    r3 = {} -- end points
    pos = find('%',s)
    fallover = length(s)+1
    while pos>0 and pos<length(s) do
        if s[pos+1]='%' then -- an actual percent sign, skip over it
            nextstart = pos+2
        else
            nextstart = fallover -- assume an error condition
            stage = 0 -- expect +0-,1-9,valid letter
            r1 &= pos
            for i=pos+1 to length(s) do
                if find(s[i],"defgosx") then -- specifier was parsed succesfully
                    r3 &= i
                    r2 &= 1
                    nextstart = i+1
                    exit
                elsif find(s[i],"-0+") then -- must be the first format char, if there
                    if stage=0 then
                        stage = 1
                    else -- invalid at this point
                        r3 &= i
                        r2 &= 0
                        nextstart = i+1
                        exit
                    end if
                elsif s[i]>='1' and s[i]<='9' then
                    if stage<2 then -- if not seen a nonzero digit, that's our stage
                        stage = 2
                    elsif stage=3 then -- there was a period, second figure (group)
                        stage = 4
                    end if
                elsif s[i]='.' then
                    if stage<=2 then -- not valid after a previous period
                        stage = 3
                    else
                        r3 &= i
                        r2 &= 0
                        nextstart = i+1
                        exit
                    end if
                else -- illegal char
                    r3 &= i
                    r2 &= 0
                    nextstart = i+1
                    exit
                end if
            end for
            -- if we didn't use one of the exit statements, nextstart is past the end of the string.
        end if
        if nextstart>=fallover then
        -- the end, because either a format started but didn't terminate, or a terminating char was the last in string
            exit
        end if
        -- move to next
--/**/  pos = find('%',s,nextstart)         --/* -- Phix
        pos=find_from('%',s,nextstart)      --*/ -- RDS
    end while
    if pos=length(s) then -- string ends in '%' which is not part of a specifier
        r1 &= pos
        r3 &= pos
        r2 &= 0
    end if
    return {r1,r2,r3}
end function

object vOldSeed vOldSeed = #69F5C10D

-------------------------------------------
--/topic Miscellaneous
--/func w32rand32(atom N)
--/ret A random integer between 1 and /i N. N may be any atom representing a 32-bit integer.
global function w32rand32(atom N)
-------------------------------------------
integer a, b
sequence d
atom X

    d = date()
--/**/  d = sq_mul(vOldSeed,d)  --/* Phix 
        d = vOldSeed * d        --  RDS */
    vOldSeed = d[1]+d[2]+d[3]
    vOldSeed += time()*(time()+17)
    vOldSeed = floor(remainder(vOldSeed*(vOldSeed-3), #3FFFFFFF))
    set_rand(vOldSeed)
    a = rand(#3FFFFFFF)
    vOldSeed += d[4]+d[5]+d[6]
    vOldSeed += (time()+3.1427)*(time()+619)
    vOldSeed = floor(remainder((vOldSeed+1)*(vOldSeed-1)+1, #3FFFFFFF))
    set_rand(vOldSeed)
    b = rand(#3FFFFFFF)
    X =  and_bits(a, #FFFF)*#10000+and_bits(b, #FFFF)
    return floor(X-floor(X/N)*N)+1

end function

--/topic Support routines
--/func w32rectangle_delta(object r1,object r2, integer typecheck)
--/desc Determines the difference and intersection between two rectangles
--/ret (SEQUENCE) {} on failure, else {r1-r2, r1 inter r2, r2-r1}.
-- /i r1 or /i r2 may be atoms, in hich case they  are RAM addresses to peek4s() a rectangle from. /n
-- If /i r1 or /i r2 is not a sequence of four signed integers, nor an atom, then {} is returned. /n
-- If the left position is not less than the right position, or the top position is not less
-- than the bottom position, for any of the rectangles, {} is returned if typechecking was requested. /n
-- Otherwise, the returned sequence has length 3:
--/li the first element is a possibly empty list of non overlapping rectangles, whose union is
-- the part of /i r1 which lies outside /i r2.
--/li the last element is a possibly empty list of non overlapping rectangles, whose union is
-- the part of /i r2 which lies outside /i r1.
--/li the middle element is a rectangle, the intersection of /i r1 and /i r2. If the rectangles
-- don't meet, the middle element is {} (an empty rectangle).
--
-- Note that the whole type checking is performed if /i typecheck is not zero, and skipped for speed if it is zero.
--
-- All rectangles are given {left,top,right,bottom}.
global function w32rectangle_delta(object r1,object r2,integer typecheck)
sequence result,inter,rect
integer other_edge

    if typecheck then
        -- type check r1
        if sequence(r1) then
            if length(r1)!=4 then
                return {}
            end if
            for i=1 to 4 do
                if not integer(r1[i]) then
                    return {}
                end if
            end for
        else
            r1 = peek4s({r1,4})
        end if
        if r1[1]>=r1[3] or r1[2]>=r1[4] then
            return {}
        end if

        -- type check r2
        if sequence(r2) then
            if length(r2)!=4 then
                return {}
            end if
            for i=1 to 4 do
                if not integer(r2[i]) then
                    return {}
                end if
            end for
        else
            r2 = peek4u({r2,4})
        end if
        if r2[1]>=r2[3] or r2[2]>=r2[4] then
            return {}
        end if

    else -- no type checking, we know wat we're doing, by gum!
        if atom(r1) then
            r1 = peek4s({r1,4})
        end if
        if atom(r2) then
            r2 = peek4s({r2,4})
        end if
    end if

    -- intersection is empty, early return
    if r1[1]>r2[3] or r1[3]<r2[1] or r1[2]>r2[4] or r1[4]<r2[2] then
        return {{r1},{},{r2}}
    end if

    -- initialise
    result = {{},0,{}}
    inter = r1

    if r1[1]<r2[1] then
    -- r1 contributes to the r1-r2 part if its left edge is left of r2's
        rect = r1
        other_edge = r2[1]
        rect[3] = other_edge
        result[1] = append(result[1],rect)
        r1[1] = other_edge
        inter[1] = other_edge
    elsif r1[1]>r2[1] then
    -- r2 contributes to the r2-r1 part if its left edge is left of r1's
        rect = r2
        other_edge = r1[1]
        rect[3] = other_edge
        result[3] = append(result[3],rect)
        r2[1] = other_edge
    end if

    -- and so on for the other three edges
    if r1[2]<r2[2] then
        rect = r1
        other_edge = r2[2]
        rect[4] = other_edge
        result[1] = append(result[1],rect)
        r1[2] = other_edge
        inter[2] = other_edge
    elsif r1[2]>r2[2] then
        rect = r2
        other_edge = r1[2]
        rect[4] = other_edge
        result[3] = append(result[3],rect)
        r2[2] = other_edge
    end if

    if r1[3]>r2[3] then
        rect = r1
        other_edge = r2[3]
        rect[1] = other_edge
        result[1] = append(result[1],rect)
        r1[3] = other_edge
        inter[3] = other_edge
    elsif r1[3]<r2[3] then
        rect = r2
        other_edge = r1[3]
        rect[1] = other_edge
        result[3] = append(result[3],rect)
        r2[3] = other_edge
    end if

    -- no need to change r1 or r2 here, as they are not used in the sequel
    if r1[4]>r2[4] then
        rect = r1
        other_edge = r2[4]
        rect[2] = other_edge
        result[1] = append(result[1],rect)
        inter[4] = other_edge
    elsif r1[4]<r2[4] then
        rect = r2
        other_edge = r1[4]
        rect[2] = other_edge
        result[3] = append(result[3],rect)
    end if

    -- inter has gathered the innermost values for all four edges
    result[2] = inter
    return result
end function
