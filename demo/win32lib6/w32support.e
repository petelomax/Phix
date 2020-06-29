--------------------NOTICE-------------------------------*
-- Software ID: w32support.e
-- Version:     0.60
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

--without warning
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

global constant
    w32False   = (1 = 0),
    w32True    = (1 = 1)



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

    for i = 1 to length(s) do
        if not integer(s[i]) then
            return 0
        end if

        if s[i] < 0 or s[i] > 255 then
            return 0
        end if

    end for
    return w32True
end type



--/topic Support Routines
--/func w32get_bits(atom b32)
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
--      codes = w32get_bits(origflag)
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
        for i = 1 to length(kBitPosn) do
            if and_bits(b32, kBitPosn[i]) then
                lResult &= kBitPosn[i]
            end if
        end for

        if length(lResult) = 0 then
            lResult = {0}
        end if

        return lResult
end function


--/topic Support Routines
--/func w32signed_word(atom a)
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
--      x = w32signed_word(lParam)
-- /endcode

global function w32signed_word(atom a)
-- return low word
integer x
        x = and_bits(a, #FFFF)
        if x > #7FFF then
            x -= #10000
        end if
        return x
end function



-----------------------------------------------------------------------------

--/topic Support Routines
--/func w32pack_word(integer low, integer high)
--/desc Packs values into word.
--/ret ATOM: 32 bit word with low value in low 16 bits, high value in high 16 bits.
-- Typically used to encode a message parameter into a 32 bit word.
--
-- Example:
--
-- /code
--      -- pack min and max into parameter
--      integer y
--      lParam = w32pack_word(min, max)
-- /endcode

global function w32pack_word(integer low, integer high)
-- return packed 32 bit value

        return (and_bits(high, #FFFF) * #10000) + and_bits(low,#FFFF)

end function


--/topic Support Routines
--/func w32lo_word(atom pData)
--/desc returns the low-16 bits of /i pData
--/ret INTEGER: Bits 15-0 of the parameter
--
-- Example:
-- /code
--      integer y
--      y = w32lo_word(bigval)
-- /endcode

global function w32lo_word(atom pData)
        return and_bits(pData, #FFFF)
end function


--/topic Support Routines
--/func w32hi_word(atom pData)
--/desc returns the high 16 bits of /i pData
--/ret INTEGER: Bits 31-16 of the parameter as a 16 bit value.
--
-- Example:
-- /code
--      integer y
--      y = w32hi_word(bigval)
-- /endcode

global function w32hi_word(atom pData)
        return and_bits(and_bits(pData, #FFFF0000) / #10000, #FFFF)
end function


--/topic Support Routines
--/func w32abs(object a)
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
            if a < 0 then
                return -a
            else
                return a
            end if
        else
            for i = 1 to length(a) do
                a[i] = w32abs(a[i])
            end for

            return a
        end if

end function


--/topic Support Routines
--/func w32shortInt(atom a)
--/desc Converts a number into a 16-bit signed integer
--/ret INTEGER: The value of /i a as a short int.
-- Typically used to decode Win32 message values
--
-- Example:
--
-- /code
--      -- Mouse Position
--      sequence hilo, x, y
--      hilo = upack_word(lParam)
--      x = w32shortInt(hilo[2])
--      y = w32shortInt(hilo[1])
-- /endcode
global function w32shortInt(atom i)
-- converts numbers (4 bytes #0000 to #FFFF)
-- to signed short ints (2 bytes -32768 to 32767)

        -- Force the use of only the rightmost 2 bytes.
        i = and_bits(i, #FFFF)

        if i >= 0 and i <= #7FFF then
            return i
        else
            return i - #10000
        end if

end function


--/topic Support Routines
--/func w32TextToNumber(sequence text)
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
-- /b "Note 2:" Any underscore or comma characters, that are embedded in the text
-- number are ignored. These can be used to help visual clarity for long numbers.
--
--/b "Note 3:" You can supply a leading or trailing, minus or plus sign.
--
--/b "Note 4:" You can supply trailing percentage sign(s). Each one present causes
-- the resulting value to be divided by 100.
--
--/b "Note 5:" Any single currency symbol to the left of the first digit is ignored.
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
constant vDigits = ".0123456789ABCDEF"
sequence vCurrencySym vCurrencySym = "$£¤¥€"
global function w32TextToNumber(sequence text)
-- get the numeric value of text
integer dot,sign,tstart,tend, v, note, notify
atom lhs, rhs, lh, rh
integer base, pc
atom value
integer digcnt
integer currency

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

        if length(text) = 2
        and sequence(text[1])
        and integer(text[2]) then
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
        while tstart <= tend do
            if equal(text[tstart], '-') and sign = 0 then
                sign = -1
            elsif equal (text[tstart],'+') and sign = 0 then
                sign = 1
            elsif equal (text[tstart],'#') then
                base = 16
            elsif equal (text[tstart],'@') then
                base = 8
            elsif equal (text[tstart],'!') then
                base = 2
            elsif find(text[tstart], {'\t', ' ', #A0}) = 0 then
                exit
            end if
            tstart += 1
        end while

        -- Ignore trailing whitespace
        while tstart <= tend do
            if equal(text[tend], '-') and sign = 0 then
                sign = -1
            elsif equal(text[tend],'+') and sign = 0 then
                sign = 1
            elsif equal(text[tend],'%') then
                pc *= 100
            elsif find(text[tend], {'\t', ' ', #A0, #A2}) = 0 then
                exit
            end if
            tend -= 1
        end while

        -- Set the default sign.
        if sign = 0 then
            sign = 1
        end if

        for i = tstart to tend do
            if lhs > 0 and find(text[i],"_") > 0 then
                -- ignore an embedded grouping characters.
            elsif digcnt = 0 and currency = 0 and find(text[i],vCurrencySym) > 0 then
                -- ignore a single leading currency symbol.
                currency = i
            else
                v =  find(text[i], vDigits)
                -- Invalid char so force a zero return.
                if v = 1 then -- A dot found.
                    if dot = 0 then
                        dot = 1
                    else
                        note = i
                        if notify = 0 then
                            sign = 0
                        end if
                        exit
                    end if
                else
                    v -= 1
                    if v < 0 or v > base then
                        -- Illegal char found.
                        note = i
                        if notify = 0 then
                            sign = 0
                        end if
                        exit
                    else
                        if dot = 1 then
                            rhs *= base
                            rh = (rh * base) + v - 1
                            digcnt += 1
                        else
                            lhs += 1
                            lh = (lh * base) + v - 1
                            digcnt += 1
                        end if
                    end if
                end if
            end if

            -- I got to the end without error!
            if i = tend then
                note = 0
            end if

        end for

        if rh = 0 and pc = 1 then
            -- Common situation optimised for speed.
            value = lh * sign
        else
            value = ((lh + (rh / rhs)) * sign) / pc
        end if

        if notify = 0 then
            return value
        else
            -- Error if no actual digits where converted.
            if note = 0 and digcnt = 0 and value = 0 then
                note = 1
            end if
            return {value, note}
        end if
end function

--/topic Support Routines
--/func w32or_all(object pData)
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

        if length(pData) = 0 then
            return 0
        end if

        if length(pData) = 1 then
            return w32or_all(pData[1])
        end if

        lResult = or_bits(pData[1], pData[2])
        for i = 3 to length(pData) do
            lResult = or_bits(lResult, pData[i])
        end for

        return lResult
end function

--/topic Support Routines
--/func w32findKeyEx(object key, sequence list, object element)
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
--      at = w32findKey("op", aList, {2,3})
--      -- 'at' should now equal 3.
--      at = w32findKey(10, aList, {2,1})
--      -- 'at' should now equal 2.
--      at = w32findKey("gh", aList, {2,3})
--      -- 'at' should now equal 0.
--/endcode


global function w32findKeyEx(object pKey, object pList, object pElement)
integer lPos 
        if atom(pList) then
            return 0
        end if

        if sequence(pElement) and length(pElement) = 1 then
            pElement = pElement[1]
        end if

        if sequence(pElement) then
            lPos = 0
            for i = 1 to length(pList) do
                lPos = w32findKeyEx(pKey, pList[i][pElement[1]], pElement[2..length(pElement)])
                if lPos != 0 then
                    return i
                end if
            end for
            return 0
        else
            if pElement < 1 then
                for i = 1 to length(pList) do
                    if sequence(pList[i]) and find(pKey, pList[i]) then
                        return i
                    elsif atom(pList[i]) and  equal(pKey, pList[i]) then
                        return i
                    end if
                end for
            else
                for i = 1 to length(pList) do
                    if sequence(pList[i]) and length(pList[i]) >= pElement and equal(pList[i][pElement], pKey) then
                        return i
                    elsif i = pElement and equal(pList[i], pKey) then
                        return i
                    end if
                end for
            end if        
        end if        
        return 0

end function

--/topic Support Routines
--/func w32findKey(object key, sequence list)
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
--      at = w32findKey("orange", aList)
--      -- 'at' should now equal 3.
--      at = w32findKey("white", aList)
--      -- 'at' should now equal 0.
--/endcode

global function w32findKey(object pKey, sequence pList)
        return w32findKeyEx(pKey, pList, 1)
end function

--/topic Support Routines
--/func w32removeIndex(integer index, sequence list)
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
--      nameList = w32removeIndex(4, nameList)
--      -- Remove the last element.
--      nameList = w32removeIndex(0, nameList)
--/endcode

global function w32removeIndex(integer index, sequence list)
-- Sanity check for non-existant elements.
        if index > length(list) then
            return list
        end if

        if index < 1 then
            index = length(list) + index
            if index < 1 then
                return list
            end if
        end if

        return list[1..index-1] & list[index+1..length(list)]
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
--                      errortext })
--/endcode

----------------------------------------------------------------------------
global function w32iff(atom test, object ifTrue, object ifFalse)
-- returns ifTrue if flag is true, else returns ifFalse
-- ex:  ? w32iff(1,"this","that")
--      --> "this"
        if test then
            return ifTrue
        else
            return ifFalse
        end if
end function


--/topic Support Routines
--/func w32removeItem(object item, sequence list)
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
        item = find(item,list)
        if item != 0 then
            return list[1..item-1] & list[item+1..length(list)]
        else
            return list
        end if
end function

--/topic Support Routines
--/func w32insertElement(sequence list, integer pos, object item)
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
        if pos = 0 then
            pos = length(list)+1
        elsif pos < 0 then
            pos = length(list)+1 + pos
            if pos < 1 then
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
        while lPos > 0 do
            lPos += lFrom - 1
            pList = pList[1..lPos-1] & pNew & pList[lPos + lLenOld..length(pList)]
            lFrom += lLenNew
            lPos = match(pOld, pList[lFrom..length(pList)])
        end while

        return pList

end function

--/topic Memory management routines
--/info
--Routines to allocate, use, and release of Random Access Memory (RAM).
--Normally, a Euphoria program doesn't have to worry about this as it is handled quietly
-- in the background. However, when working with external systems, such as the Windows API,
-- the coder often needs to explicitly manage RAM as resource, primarily when sharing data
-- between your application and Windows.

--without warning
without trace
without profile

constant
kernel32 = open_dll("kernel32.dll"),
xIsBadWritePtr = define_c_func(kernel32, "IsBadWritePtr", {C_LONG, C_LONG}, C_LONG),
xIsBadReadPtr = define_c_func(kernel32, "IsBadReadPtr", {C_LONG, C_LONG}, C_LONG),
--xIsBadCodePtr = define_c_func(kernel32, "IsBadReadPtr", {C_LONG, C_LONG}, C_LONG),
xlstrlen = define_c_func(kernel32,"lstrlen",{C_POINTER},C_INT)


-- stores abort handler's routine id
integer  vAbortRtn  vAbortRtn  = -1

-- The number of allocations
integer vAllocations vAllocations = 0
sequence vExtraCare vExtraCare = {}

-- List of memset ids
sequence vOwners vOwners = {}

-- List of address sets. Each set has a list of addresses.
sequence vSets   vSets = {}

-- stores the accumulated size of a structure as it is being defined.
integer  vAllotted  vAllotted  = 0

-- This is added to calls to alloc.
integer vSafetyBuffer vSafetyBuffer = 4

--/topic Memory Management Routines
--/Var w32UsingSafeCode
--/desc Determines whether or not the 'safe' versions of peek and poke are used.
--This is primarily as debugging aid. You only need to set this if you suspect
--that your program is causing memory corruptions or accessing strange locations.
--
-- Set this to zero to turn off the safe versions..
--
-- The initial setting is 0. That is, the safe versions are not being used.
--
--Example:
--/code
--     --Ensure I can change RAM safely
--     w32UsingSafeCode = 1
--     l_SafePoke4(addr, 0)
--/endcode

global integer w32UsingSafeCode w32UsingSafeCode = 0

--/topic Memory Management Routines
--/func w32llSetAbort(i)
--/desc Sets the routine id of an Abort routine.
--/ret The previous value set.
--
-- Used to indicate if an error routine needs to be
-- called in the event of a catastophic error.
-- The error routine is assumed to be a procedure
-- that accepts a single sequence (typically an
-- message string).
--
-- Example:
--
-- /code
--      integer RtnID, OldID
--      RtnID = routine_id("abortErr")
--      OldID = w32llSetAbort(RtnID)
-- /endcode

global function w32llSetAbort(integer i)
-- Set the abort handler id. The routine must take two parameters,
-- a sequence (msg) and a integer (mode ==> 1=warning, 2=fatal)
integer lOldRtn

        lOldRtn = vAbortRtn

        vAbortRtn = i

        return lOldRtn
end function


procedure l_MyFreeMem(atom pAddress)
object VOID
integer lPosn

        if c_func(xIsBadWritePtr, {pAddress, 1}) then
            return
        end if


        -- Check if this addr needs extra care checking.
        lPosn = find(pAddress, vExtraCare)
        if lPosn != 0 then
            pAddress -= 16
            if peek4u(pAddress) != pAddress
            or peek4u(pAddress+4) != pAddress + 4
            or peek4u(pAddress+8) != #C0D0E0F0
            or peek4u(pAddress+12) != #8899AABB then
               -- The area prior to the users RAM was corrupted.
                if vAbortRtn>=0 then
                    call_proc(vAbortRtn,{sprintf("Corrupted RAM at address %d.", pAddress)})
                else
                    crash_message(sprintf("Corrupted RAM at address %d.", pAddress))
                    VOID = 0/0 -- Force a crash
                end if
            end if
            -- Remove it from the list.
            vExtraCare = vExtraCare[1..lPosn-1] & vExtraCare[lPosn+1..length(vExtraCare)]
        end if

        pAddress -= 4

        vAllocations -= 1

        free(pAddress)
        return

end procedure

function l_MyAllocate(integer pSize)
atom lAddr
atom lPreSize
integer lMemSize

        if w32UsingSafeCode != 0 then
            lPreSize = 16
        else
            lPreSize = 0
        end if

        -- Add some bytes for a safety buffer
        lMemSize = lPreSize + pSize + 4 + vSafetyBuffer
        lAddr = allocate(lMemSize)
--?9/0
--printf(1,"%08x\n",{lAddr+4})
        if lAddr != 0 then
            mem_set(lAddr, 0, lMemSize)

            vAllocations += 1


            -- Save total size for later reuse.
            poke4(lAddr, lMemSize)
            lAddr += 4

            -- If being safe, add this address to the extra care list,
            -- so that the free() can see if nothing was messed up.
            if lPreSize != 0 then
                lPreSize = lAddr
                lAddr += 16
                vExtraCare &= lAddr
                poke4(lPreSize, {lPreSize, lPreSize+4, #C0D0E0F0, #8899AABB})
            end if
        end if

        return lAddr
end function


--/func l_SafePeek(object addr)
--/desc This is an enhanced version of Euphoria's peek() function
--/ret see peek() for details.
--If /i w32UsingSafeCode is not zero, this does a memory access check first.

function l_SafePeek(object addr)
atom a, l
        if w32UsingSafeCode = 0 then
            return peek(addr)
        end if

        if sequence(addr) and length(addr) = 2 and atom(addr[1]) and atom(addr[2]) then
            l = addr[2]
            a = addr[1]
        else
            a = addr
            l = 1
        end if

        if c_func(xIsBadReadPtr, {a,l}) = 0 then
            return peek(addr)
        else
            if vAbortRtn>=0 then
                call_proc(vAbortRtn,{sprintf("Peek at address %d for length %d failed.",{a,l})})
                return 0 -- Just in case the abort does not crash program.
            else
--PL 29/4/09:
--              if object(addr) then
                return {}
--              else
--                  return 0
--              end if
            end if
        end if
end function


--/func l_SafePeek4s(object addr)
--/desc This is an enhanced version of Euphoria's peek4s() function
--/ret see peek4s() for details.
--If /i w32UsingSafeCode is not zero, this does a memory access check first.
function l_SafePeek4s(object addr)
atom a, l

        if w32UsingSafeCode = 0 then
            return peek4s(addr)
        end if

        if sequence(addr) and length(addr) = 2 and atom(addr[1]) and atom(addr[2]) then
            l = addr[2] * 4
            a = addr[1]
        else
            a = addr
            l = 4
        end if

        if c_func(xIsBadReadPtr, {a,l}) = 0 then
            return peek4s(addr)
        else
            if vAbortRtn>=0 then
                call_proc(vAbortRtn,{sprintf("Peek4s at address %d for count of %d failed.",{a,l/4})})
                return 0 -- Just in case the abort does not crash program.
            else
-- PL 29/4/09:
--              if object(addr) then
                return {}
--              else
--                  return 0
--              end if
            end if
        end if
end function

--/func l_SafePeek4u(object addr)
--/desc This is an enhanced version of Euphoria's peek4u() function
--/ret see peek4u() for details.
--If /i w32UsingSafeCode is not zero, this does a memory access check first.
function l_SafePeek4u(object addr)
atom a, l

        if w32UsingSafeCode = 0 then
            return peek4u(addr)
        end if


        if sequence(addr) and length(addr) = 2 and atom(addr[1]) and atom(addr[2]) then
            l = addr[2] * 4
            a = addr[1]
        else
            a = addr
            l = 4
        end if

        if c_func(xIsBadReadPtr, {a,l}) = 0 then
            return peek4u(addr)
        else
            if vAbortRtn>=0 then
                call_proc(vAbortRtn,{sprintf("Peek4u at address %d for count of %d failed.",{a,l/4})})
                return 0 -- Just in case the abort does not crash program.
            else
-- PL 29/4/09:
--              if object(addr) then
                return {}
--              else
--                  return 0
--              end if
            end if
        end if
end function

--/func l_SafePoke(object addr)
--/desc This is an enhanced version of Euphoria's poke() function
--/ret see poke() for details.
--If /i w32UsingSafeCode is not zero, this does a memory access check first.
procedure l_SafePoke(atom addr, object thedata)
atom l

        if w32UsingSafeCode = 0 then
            poke(addr, thedata)
            return
        end if

        if sequence(thedata) then
            l = length(thedata)
        else
            l = 1
        end if

        if c_func(xIsBadWritePtr, {addr,l}) = 0 then
            poke(addr, thedata)
        else
            if vAbortRtn>=0 then
                call_proc(vAbortRtn,{sprintf("Poke at address %d for length of %d failed.",{addr,l})})
                return -- Just in case the abort does not crash program.
            end if
        end if
end procedure

--/func l_SafePoke4(object addr)
--/desc This is an enhanced version of Euphoria's poke4() function
--/ret see poke4() for details.
--If /i w32UsingSafeCode is not zero, this does a memory access check first.
procedure l_SafePoke4(atom addr, object thedata)
atom l

        if w32UsingSafeCode = 0 then
            poke4(addr, thedata)
            return
        end if


        if sequence(thedata) then
            l = length(thedata) * 4
        else
            l = 4
        end if

        if c_func(xIsBadWritePtr, {addr,l}) = 0 then
            poke4(addr, thedata)
        else
            if vAbortRtn>=0 then
                call_proc(vAbortRtn,{sprintf("Poke4 at address %d for count of %d failed.",{addr,l/4})})
                return -- Just in case the abort does not crash program.
            end if
        end if
end procedure

-----------------------------------------------------------------------------
-----------------------------------------------------------------------------
-- Memory Management memory management Routines
-----------------------------------------------------------------------------
-----------------------------------------------------------------------------


global constant
    Byte    = -1,
    Int8    = Byte,
    Word    = -2,
    Integer = Word,
    Int16   = Word,
    Long    = -3,
    DWord   = Long,
    Int32   = Long,
    Lpsz    = -4,
    Hndl    = -5,
    HndlAddr = -6,
    Strz    = -7,
    UInt    = -8,
    Ptr     = UInt,
    ULong   = UInt,
    Single  = -9,
    Float   = Single,
    Double  = -10

constant vSizeNames = {Byte, Word, Long, Lpsz, Hndl, HndlAddr, Strz, UInt, Single, Double}
constant vSizeLengs = {   1,    2,    4,    4,    4,        4,    1,    4,      4,      8}


--/topic Memory Management Routines
--/func w32manage_mem(atom Owner, atom Address)
--/desc Records an acquired memory for garbage collection.
-- Normally this is handled automatically by /w32acquire_mem() but if you
-- are expected to manage some memory acquired by another means, such as
-- a Windows call or a 'C' routine, you can use this to record the memory for
-- subsequent release by /w32release_mem().
--
-- Example:
-- /code
--      atom mset, pt, pstr
--
--      -- Establish a new memory set.
--      mset = /new_memset()
--      -- calls a routine which returns a structure address.
--      pt = c_func(xyz, {abc})
--      -- register this memory
--      w32manage_mem(mset, pt)
--      . . .
--      give all the memory area in 'mset' back
--      w32release_mem(mset)
-- /endcode

global procedure w32manage_mem(atom pOwner, atom pAddr)
-- save location for garbage collection
-- This also is used to move a block from one memset to another.
integer lOwnerSub, lAddrSub

        -- Take the address out of any existing memset.
        lAddrSub = 0
        for i = 1 to length(vSets) do
            lAddrSub = find(pAddr, vSets[i])
            if lAddrSub != 0 then
                vSets[i] = w32removeIndex(lAddrSub, vSets[i])
                exit
            end if
        end for

        -- Create a new memset if required.
        lOwnerSub = find(pOwner, vOwners)
        if lOwnerSub = 0 then
            vOwners &= pOwner
            lOwnerSub = length(vOwners)
            vSets = append(vSets, {})
        end if

        -- Store the address into the specified memset
        vSets[lOwnerSub] &= pAddr
end procedure

-----------------------------------------------------------------------------

--/topic Memory Management Routines
--/func w32acquire_mem(atom Owner, object structure)
--/desc Allocate memory for structure, and initialize to zero.
-- The memory allocated is linked to the /i Owner and all the
-- owner's memory can be released by one call. /n
-- If /i structure is a string, it is copied to the memory
-- location along with a zero byte.
--
-- If /i structure is an atom, it specifies that amount of memory
-- to acquire (a minimum of 4 bytes will always be acquired) and
-- the memory is set to all zeros.
--
--/ret Address of allocated memory.
--
-- Example:
-- /code
--      atom mset, pt, pstr
--
--      -- Establish a new memory set.
--      mset = /new_memset()
--      -- get enough memory to hold a UInt datatype
--      xy = w32acquire_mem(UInt)
--      -- allocate a point structure
--      pt = w32acquire_mem(mset, SIZEOF_POINT)
--      -- copy a Euphoria string to a 'C' string area.
--      pstr = /w32acquire_mem(mset, "My String Data")
--      . . .
--      give all the memory area in 'mset' back
--      w32release_mem(mset)
-- /endcode

global function w32acquire_mem(atom pOwner, object pData)
--
-- allocate space for a structure (mininum of 4 bytes)
-- and initialize to zero
--
atom at

        if sequence(pData) then
            -- place string in memory
            at = l_MyAllocate(1 + length(pData))
            if at != 0 then
                poke(at, pData)
                poke(at + length(pData) , 0)
            end if
        else
            -- Check for special datatype "names"
            if pData < 0 then
                pData = find(pData, vSizeNames)
                if pData != 0 then
                    pData = vSizeLengs[pData]
                end if
            end if

            if pData < 4 then
                pData = 4
            end if

            at = l_MyAllocate(pData)

        end if

        if at=0 then
            if vAbortRtn>=0 then
                call_proc(vAbortRtn,{"Unable to allocate space."})
                return 0 -- Just in case the abort does not crash program.
            end if
        else
            w32manage_mem(pOwner, at)
        end if

        return at

end function

--/topic Memory Management Routines
--/proc w32release_mem(atom structure)
--/desc Returns the memory allocated by /w32acquire_mem() back to the system.
-- If /i structure is a memory set id, as returned by /new_memset(), then
-- all the memory owned in the memory set is returned and the memory set id
-- is released. That is, it cannot be reused. /n
-- If /i structure is a memory address returned by /w32acquire_mem(), then just
-- that memory is released. The memory set it belonged to is still usable.
--
-- Example:
-- /code
--      atom mset, pt, pstr
--
--      -- Establish a new memory set.
--      mset = /new_memset()
--      -- get enough memory to hold a UInt datatype
--      xy = /w32acquire_mem(UInt)
--      -- allocate a point structure
--      pt = /w32acquire_mem(mset, SIZEOF_POINT)
--      -- copy a Euphoria string to a 'C' string area.
--      pstr = /w32acquire_mem(mset, "My String Data")
--      . . .
--      give all the memory area in 'mset' back
--      w32release_mem(mset)
-- /endcode

global procedure w32release_mem(atom pData)
integer lOwnerSub
integer lAddrSub, lAddrList
integer ls, ss, Phase1
sequence sets

        -- Check for "special" uninitialized memset value.
        if pData = -1 then
            return
        end if

        -- See if this is a memset owner.
        lOwnerSub = find(pData, vOwners)

        if lOwnerSub = 0 then
            -- If not a memset, see which memset it belongs to.
            lAddrSub = 0
            for i = 1 to length(vSets) do
                lAddrSub = find(pData, vSets[i])
                if lAddrSub != 0 then
                    lAddrList = i
                    exit
                end if
            end for

            if lAddrSub = 0 then
                -- Not in any memset!
                if vAbortRtn >= 0 then
                    call_proc(vAbortRtn, {"Trying to release unacquired memory"})
                    return
                end if

            else
                -- Remove it from the memset
                vSets[lAddrList] = w32removeIndex(lAddrSub,
                                                  vSets[lAddrList])
                -- Give the memory back to the system
                l_MyFreeMem(pData)
            end if

            return
        end if

        --
        sets = {pData}
        ss   = 1

        -- Phase 1: Identified the complete hierarchy of owned memory sets.
        Phase1 = 1
        while Phase1 do
            ls = length(sets)
            for i = ss to ls do
                lOwnerSub = find(sets[i], vOwners)
                for j = 1 to length(vSets[lOwnerSub]) do
                    lAddrSub = find(vSets[lOwnerSub][j],
                                    vOwners)
                    if lAddrSub != 0 then
                        sets &= vOwners[lAddrSub]
                    end if
                end for
            end for
            ss = ls + 1
            Phase1 = (ls != length(sets))
        end while

        -- Phase 2: Free the memory addresses.
        for i = length(sets) to 1 by -1 do
            lOwnerSub = find(sets[i], vOwners)
            for j = 1 to length(vSets[lOwnerSub]) do
                if vSets[lOwnerSub][j] != 0 then
                    l_MyFreeMem(vSets[lOwnerSub][j])

                end if
            end for
        end for
        if sets[1] != 0 then
            l_MyFreeMem(sets[1])

        end if

        -- Phase 3: - clean up the allocation array
        for i = 1 to length(sets) do
            lOwnerSub = find(sets[i], vOwners)
            if lOwnerSub > 0 then
                vSets = w32removeIndex(lOwnerSub, vSets)
                vOwners = w32removeIndex(lOwnerSub, vOwners)
            end if
            for j = 1 to length(vSets) do
                lOwnerSub = find(sets[i], vSets[j])
                if lOwnerSub != 0 then
                    vSets[j] = w32removeIndex(lOwnerSub,
                                              vSets[j])
                    exit
                end if
            end for
        end for

end procedure

--/proc w32release_all_mem()
--/desc Free all the acquired memory
-- This gives back to the system, all the memory acquired by calling
-- /w32acquire_mem(). You must not use any previously acquired memory blocks
-- after this has been called. /n
--
-- /b"NOTE:" When using the Win32Lib library, it is /b not required to call
-- this function as that is done automatically when WinMain() completes.
--
-- /b"WARNING:" Calling this before WinMain() has ended will probably cause the
-- the /i Win32Lib routines to crash.
--
-- Example:
--
--/code
--      -- Return all the memory areas
--      w32release_all_mem()
--
--/endcode

--global procedure w32release_all_mem()
---- No longer needed.
--end procedure



-----------------------------------------------------------------------------

--/topic Memory Management Routines
--/func w32allot(object FldDefn)
--/desc Allocate space in structure for a new field.
--/ret SEQUENCE: Definition to allotted memory.
-- /i FldDefn is either a number of bytes to allocate, one of the predefined
-- datatypes (listed below), or a 2-element sequence containing a repeat count
-- and a datatype or length.
--
-- If a number of bytes is supplied, the field is aligned to the next 32-bit
-- boundry before allocation.
--
-- The returned allotment definition is used by /store and /fetch. It has the
-- following structure. /n
-- The definition has four items: /n
-- An /i offset, a /i datatype, a /i"repeat length", and a /i "unit bytesize" /n
-- Allowable types are: /n
-- /li /b Byte: 8 bit value
-- /li /b Int8: Signed 8 bit integer, same as /b Byte.
-- /li /b Word: 16 bit value
-- /li /b Integer: 16 bit value, same as /b Word
-- /li /b Int16: 16 bit value, same as /b Word
-- /li /b Long: Signed 32 bit value
-- /li /b DWord: 32 bit value, same as /b Long
-- /li /b Int32: 32 bit value, same as /b Long
-- /li /b UInt: Unsigned 32 bit value.
-- /li /b Ptr: 32 bit value, same as /b UInt
-- /li /b ULong: 32 bit value, same as /b UInt
-- /li /b Hndl: 32 bit value, a pointer to a pointer
-- /li /b HndlAddr: 32 bit value,
-- /li /b Lpsz: Long pointer (32 bits) to zero delimited string
-- /li /b Strz: Fixed size buffer that holds a zero-delim string
-- /li /b Single: 32-bit IEEE floating point value
-- /li /b Float: Same as Single
-- /li /b Double: 64-bit IEEE floating point value
--
-- Example:
--
-- /code
-- constant
--    msLeft           = w32allot(Long),
--    msTop            = w32allot(Long),
--    msRight          = w32allot(Long),
--    msBottom         = w32allot(Long),
--    msVelocity       = w32allot(Single),
--    msXYZ            = w32allot({4,DWord}),
--    msReserved       = w32allot(5),
--    msName           = w32allot(Lpsz),
--    msBuffer         = w32allot({128,Strz}),
--    SIZEOF_MYSTRUCT  = /w32allotted_size()
-- /endcode

global function w32allot(object pDataType)

integer soFar, diff, size, i, lCnt

        if sequence(pDataType) then
            lCnt = pDataType[1]
            i =   pDataType[2]
        else
            lCnt = 1
            i   = pDataType
        end if

        -- save position
        soFar = vAllotted

        -- if not a pre-defined type, make sure it gets a word boundary
        size = find(i, vSizeNames)
        if size = 0 then
            if i > 0 then
                diff = remainder(soFar,4)
                if diff then
                    -- word align
                    soFar = soFar + 4 - diff
                end if

                -- size is actual size
                size = i
            end if
        else
            size = vSizeLengs[size]
        end if

        -- w32allot space
        vAllotted += (size * lCnt)


        -- return offset, data type, count, and unit-size
        return { soFar, i, lCnt, size }

end function

--/topic Memory Management Routines
--/func w32allotted_handle(hDefn)
--/desc Returns the address of the supplied handle, but with fetch instructions
-- to get the address from the handle, rather than the handle itself. /n
-- An empty sequence is returned if the parameter was invalid.
--/ret SEQUENCE: Handle's "offset" into a structure.
--
-- Example:
--
-- /code
-- constant
--    hDemo            = /w32allot(Hndl),
--    pDemo            = /w32allotted_handle(hDemo),
--    SIZEOF_DEMO      = /w32allotted_size()
--     . . .
--    x = allocate_struct(SIZEOF_DEMO)
--    initDEMO(x)
--
--    h = w32fetch(x, hDemo)
--    a = w32fetch(x, pDemo)
--    -- 'h' will contain the handle, and 'a' the address from the handle.
-- /endcode


global function w32allotted_handle(sequence pHandle)
    if length(pHandle) = 4
    and
    pHandle[2] = Hndl
       then
        return {pHandle[1], HndlAddr, pHandle[3], pHandle[4]}
    else
        return {}
    end if
end function

--/topic Memory Management Routines
--/func w32allotted_sofar()
--/desc Return size allotted sofar to the structure
--/ret INTEGER: Allotted size of structure.
--
-- Example:
--
-- /code
-- constant
--    rectLeft            = /w32allot(Long),
--    rectTop             = /w32allot(Long),
--    SIZEOF_LT           = /w32allotted_sofar()
--    rectRight           = /w32allot(Long),
--    rectBottom          = /w32allot(Long),
--    SIZEOF_RECT         = /w32allotted_size()
-- /endcode

global function w32allotted_sofar()
-- returns allotted size

        return vAllotted

end function


-----------------------------------------------------------------------------
--/topic Memory Management Routines
--/func w32allotted_size()
--/desc Return allocate size of structure, and reset for new structure.
--/ret INTEGER: Allotted size of structure.
--
-- Example:
--
-- /code
-- constant
--    rectLeft            = /w32allot(Long),
--    rectTop             = /w32allot(Long),
--    rectRight           = /w32allot(Long),
--    rectBottom          = /w32allot(Long),
--    SIZEOF_RECT         = /w32allotted_size()
-- /endcode

global function w32allotted_size()
-- returns allotted size, and clears size
integer soFar

        soFar = w32allotted_sofar()

        vAllotted = 0

        return soFar

end function

-----------------------------------------------------------------------------

--/topic Memory Management Routines
--/proc w32ElemAddr(atom structure, sequence field)
--/desc Calculate the RAM address of a structure element.
--/ret ATOM: The RAM address of /i field within the /i struct
--
--Example:
--/code
--      atom Addr
--      Addr = w32ElemAddr(lFR,FORMATRANGE_rcLeft)
--/endcode

global function w32ElemAddr(atom pStruct, sequence s)
atom w

        if length(s) > 4 and integer(s[5]) and s[5] > 0 and s[5] <= s[3] then
            s[5] = (s[5]-1) * s[4]
        else
            s &= 0
        end if
        w = pStruct + s[1] + s[5]
        return w
end function

--/topic Memory Management Routines
--/proc w32store(atom pStruct, sequence field, object value)
--/desc Store a value into a structure.
-- Type conversion is automatic. For example, if an /b Lpsz field is
-- used, the value is automatically converted from a sequence to a
-- C-style string, and the address of that string is stored in the
-- structure.
--
-- Example:
--
--/code
--      -- allocate RECT structure, and populate it
--      atom rect
--
--      -- allocate the structure
--      rect = /allocate_struct(SIZEOF_RECT)
--
--      -- store values into the structure
--      /w32store(rect,rectLeft,   x1)
--      /w32store(rect,rectTop,    y1)
--      /w32store(rect,rectRight,  x2)
--      /w32store(rect,rectBottom, y2)
--
--      -- Here we store individual elements to an /w32allot array.
--      constant bCoords = w32allot({4,Long})
--      . . .
--      /w32store(rect, bCoords & 1, Col)
--      /w32store(rect, bCoords & 2, Row)
--/endcode

global procedure w32store(atom pStruct, sequence s, object o)
-- Store the data based on its type
atom w  -- where
integer datatype
integer lCnt
atom    at
sequence bytes

        w = w32ElemAddr(pStruct, s)
        datatype = s[2]
        lCnt     = s[3]
        -- For sequences, make sure no more than lCnt elements are stored.
        if sequence(o) then
            if length(o) < lCnt then
                lCnt = length(o)
            end if
        end if

        -- read, based on datatype
        if datatype = Byte then
            -- poke a byte
            if atom(o) then
                l_SafePoke(w,o)
            else
                l_SafePoke(w,o[1..lCnt])
            end if

        elsif datatype = Word then
            -- poke a word
            bytes = int_to_bytes(o)
            l_SafePoke(w,bytes[1..2])

        elsif datatype = Long then
            -- poke long(s)
            if atom(o) then
                l_SafePoke4(w,o)
            else
                l_SafePoke4(w,o[1..lCnt])
            end if

        elsif datatype = UInt then
            -- poke long(s)
            if atom(o) then
                l_SafePoke4(w,o)
            else
                l_SafePoke4(w,o[1..lCnt])
            end if

        elsif datatype = Lpsz then
            -- if atom, treat as long
            if atom(o) then
                l_SafePoke4(w,o)
            else
                -- poke the address of the allotted string
                l_SafePoke4(w,w32acquire_mem(pStruct,o))
            end if

        elsif datatype = Hndl then
            -- poke handle(s)
            if atom(o) then
                l_SafePoke4(w,o)
            else
                l_SafePoke4(w,o[1..lCnt])
            end if

        elsif datatype = HndlAddr then
            -- poke a handle's address
            at = w32acquire_mem(pStruct, 4)
            l_SafePoke4(at,o)
            l_SafePoke4(w,at)

        elsif datatype = Strz then
            -- poke a zero-terminated string
            bytes = o & 0
            if lCnt < s[3] then
                lCnt += 1
            end if
            l_SafePoke(w,bytes[1..lCnt])

        elsif datatype = Single then
            if atom(o) then
                o = {o}
            end if
            if lCnt > length(o) then
                lCnt = length(o)
            end if
            for i = 1 to lCnt do
                l_SafePoke(w, atom_to_float32(o[i]))
                w += 4
            end for

        elsif  datatype = Double then
            if atom(o) then
                o = {o}
            end if
            if lCnt > length(o) then
                lCnt = length(o)
            end if
            for i = 1 to length(o) do
                l_SafePoke(w, atom_to_float64(o[i]))
                w += 8
            end for

        else
            -- poke a string
            l_SafePoke(w,o[1..lCnt])
        end if

end procedure



--/topic Memory Management Routines
--/func w32peek_string(atom address)
--/desc Get sequence from address holding C-style string.
--/ret SEQUENCE: containing the C-style string.
-- This is typically done automatically by the /fetch
-- function.
--
-- Example:
--
--/code
--      -- get a C-string from address
--      sequence s
--
--      s = /w32peek_string(address)
--/endcode
global function w32peek_string(atom a)
-- V0.56 Al Getz
integer l
sequence s

        -- Only deal with non-zero addresses
        if a then
            l = c_func(xlstrlen,{a})
            s = l_SafePeek({a, l})
        else
            s = {}
        end if

        -- send back all the bytes found.
        return s

end function

--/topic Memory Management Routines
--/func w32fetch(atom pStruct, sequence field)
--/desc Fetch field from structure.
--/ret OBJECT: Field from a structure.
-- Data conversion is automatic. For example, if the field is
-- an /b Lpsz, a sequence containing the string will automatically
-- be returned.
--
-- Example:
-- /code
--      -- fetch the average character width from the text metrics structure
--      width = /w32fetch(tm,tmAveCharWidth)
--
--      -- Here we get individual elements from an /w32allot array.
--      constant bCoords = w32allot({4,Long})
--      . . .
--      x = /w32fetch(rect, bCoords & 1)
--      y = /w32fetch(rect, bCoords & 2)
--
--      -- To get all the values at once
--      sequence coords
--      coords = /w32fetch(rect, bCoords)
--
-- /endcode

global function w32fetch(atom pStruct, sequence s)
-- fetch the data based on the type
integer size, cnt
atom at
object result
sequence sbytes

        -- get data type
        size = s[2]
        -- get repeation
        cnt = s[3]
        if length(s) > 4 and integer(s[5]) and s[5] > 0 and s[5] <= cnt then
            s[5] = (s[5]-1) * s[4]
            cnt  = 1 -- Force a fetch of a single element.
        else
            s   &= 0
        end if
        -- address is pStruct + offset
        at = s[1] + pStruct + s[5]

        -- read, based on size
        if size = Byte then
            -- return byte
            if cnt > 1 then
                return l_SafePeek({at,cnt})
            else
                return l_SafePeek(at)
            end if

        elsif size = Word then
            if cnt = 1 then
                -- return word
                return bytes_to_int(l_SafePeek({at,2}) & {0,0})
            else
                s = {}
                for i = 1 to cnt do
                    s &= bytes_to_int(l_SafePeek({at,2}) & {0,0})
                    at += 2
                end for
                return s
            end if

        elsif size = Long then
            if cnt = 1 then
                -- return signed long
                return l_SafePeek4s(at)
            else
                return l_SafePeek4s({at,cnt})
            end if

        elsif size = UInt then
            if cnt = 1 then
                -- return unsigned long
                return l_SafePeek4u(at)
            else
                return l_SafePeek4u({at,cnt})
            end if

        elsif size = Lpsz then
            -- get the pointer
            at = l_SafePeek4u(at)
            if not c_func(xIsBadReadPtr,{at,4})then
                -- return the string
                return w32peek_string(at)
            else
                return at
            end if

        elsif size = Hndl then
            if cnt = 1 then
                -- return handle
                return l_SafePeek4u(at)
            else
                return l_SafePeek4u({at,cnt})
            end if

        elsif size = HndlAddr then
            -- return a handle's address
            return l_SafePeek4u(l_SafePeek4u(at))

        elsif size = Strz then

            return w32peek_string(at)

        elsif size = Single then
            result = {}
            for i = 1 to cnt do
                sbytes = l_SafePeek({at,4})
                at += 4
                result &= float32_to_atom(sbytes)
            end for
            if cnt = 1 then
                result = result[1]
            end if
            return result

        elsif size = Double then
            result = {}
            for i = 1 to cnt do
                sbytes = l_SafePeek({at,8})
                at += 8
                result &= float64_to_atom(sbytes)
            end for
            if cnt = 1 then
                result = result[1]
            end if
            return result

        elsif size > 0 then
            -- return the string
            return(l_SafePeek({at,size}))
        else
            return {}
        end if

end function

-----------------------------------------------------------------------------
--/topic Memory Management Routines
--/func w32address(atom structure, object field)
--/desc Get address of /i field in structure.
--/ret ATOM: Address of the field in the structure.
-- This is typically used if the structure contains an array.
--
-- In this snippet, the /b memBitmapInfo structure contains an array
-- of /b RGBQUAD colors. The array is populated with the values in
-- the pal:
--
-- /code
--    -- get the start of the rgbQuad array
--    rgbQuad = w32address(memBitmapInfo,bmiColors)
--
--    -- copy the pal to memory
--    for i = 1 to colors do
--
--        -- store values
--        /w32store(rgbQuad,rgbRed,      pal[i][1])
--        /w32store(rgbQuad,rgbGreen,    pal[i][2])
--        /w32store(rgbQuad,rgbBlue,     pal[i][3])
--        /w32store(rgbQuad,rgbReserved, 0)
--
--        -- move to next quad
--        rgbQuad += SIZEOF_RGBQUAD
--
--    end for
--
-- /endcode

global function w32address(atom addr, object offset)
-- return address in structure
        if atom(offset) then
            return addr + offset
        elsif length(offset) >= 3 then
            if length(offset) > 4 and integer(offset[5]) and offset[5] > 0 and offset[5] <= offset[3] then
                offset[5] = (offset[5]-1) * offset[4]
            else
                offset   &= 0
            end if
            -- address is struct + offset
            return offset[1] + addr + offset[5]
        else
            return 0
        end if
end function


--/topic Memory Management Routines
--/func w32new_memset()
--/desc Allocates a unique id for a memory set.
--/ret ATOM: An id for a new memory set (memset).
-- A memset id is actually a machine address of a 4-bytes location. You can
-- use this 4-byte area for anything you like, until you call /w32release_mem()
--
-- Example:
--
--/code
--      atom ss
--
--              ss = w32new_memset()
--      b = w32acquire_mem(ss, "All you need is love")
--              ...
--      w32release_mem(ss)  -- Let go of set 'ss'
--/endcode
global function w32new_memset()
        return w32acquire_mem(0,UInt)
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
        if lEnd = 0 then
            return pSource
        end if

        -- Check for quote-enclosed string.
        if length(pSource) >= 2 and find(pSource[1], "'\"") and pSource[lEnd] = pSource[1] then
            return pSource[2..length(pSource)-1]
        end if

        -- Common check.
        if not find(pSource[1], whitespace) and not find(pSource[lEnd], whitespace) then
            return pSource
        end if


        lStart = 1
        while lStart <= lEnd do
            if find(pSource[lStart], whitespace) = 0 then
                exit
            end if
            lStart += 1
        end while

        while lEnd > lStart do
            if find(pSource[lEnd], whitespace) = 0 then
                exit
            end if
            lEnd -= 1
        end while

        pSource = pSource[lStart..lEnd]
        -- Check for quote-enclosed string.
        if length(pSource) >= 2 and find(pSource[1], "'\"") and pSource[length(pSource)] = pSource[1] then
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
        if lEnd = 0 then
            return pSource
        end if

        -- Common check first.
        if not find(pSource[lEnd], whitespace) then
            return pSource
        end if


        lStart = 1

        while lEnd >= lStart do
            if find(pSource[lEnd], whitespace) = 0 then
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
        if lEnd = 0 then
            return pSource
        end if
        lStart = 1

        while lStart <= lEnd do
            if find(pSource[lStart], whitespace) = 0 then
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
            while r <= length(pSource) do
                if lDepth = 0 then
                    if pSource[r] = pDelim then
                        lResult = append(lResult, pSource[l..r-1])
                        l = r+1
                        r = l
                    else
                        if find(pSource[r],"({") then
                            lDepth += 1
                            lP = pSource[r]
                            if lP = '{' then
                                lS = '}'
                            else
                                lS = ')'
                            end if
                        end if
                        r += 1
                    end if
                else
                    if pSource[r] = lP then
                        lDepth += 1
                    elsif pSource[r] = lS then
                        lDepth -= 1
                    end if
                    r += 1
                end if
            end while
            lResult = append(lResult, pSource[l..r-1])

        elsif length(pDelim) > 0 then
            if atom(pDelim[1]) then
                -- Delimiter is assumed to be a simple string.
                e = r+length(pDelim)-1
                while e <= length(pSource) do
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
                while r <= length(pSource) do
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
-- of /i pTarget is an atom then zero is returned otherwise an empty
-- sequence is returned.
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

        lPosn = find(pItem, pSource)
        if lPosn > 0 then
            return pTarget[lPosn]
        elsif length(pTarget) > length(pSource) then
            return pTarget[length(pTarget)]
        elsif atom(pTarget[1]) then
            return 0
        else
            return ""
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


--/topic Support Routines
--/func w32CType(integer pChar, object pSet)
--/desc Tests a character to see if it 'belongs' to a specified CharType set.
--/ret INTEGER: True / False
--/i pChar is a single character. /n
--/i pSet is either a single CharType code or a set of codes. /n
--See /w32SetCType for a complete list of valid CharType codes. /n
--
--Example:
--/code
--      if w32CType(text[i], Alpha_CT) then
--          ... process an alphabetic char
--      elsif w32CType(text[i], Digit_CT) then
--          ... process an digit char
--      end if
--/endcode
global function w32CType(integer pChar, object pSet)
integer lResult
atom lValue

    lResult = #FFFF

    -- Only support single byte characters.
    if pChar < 0 or pChar > 255 then
        return 0
    end if

    lValue = vCharAttr[pChar+1]

    if atom(pSet) then
        pSet = {pSet}
    end if

    for i = 1 to length(pSet) do
        lResult = and_bits(and_bits(lValue,pSet[i]), lResult)
    end for

    return lResult
end function

--/topic Support Routines
--/func w32GetCType(object pChar)
--/desc Gets the CharType for the specified character(s)
--/ret Same datatype as /i pChar: The CharType code for the character(s)
--/i pChar can either be a single character or a sequence of characters.
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
        if integer(pChar) then
            if pChar < 0 or pChar > 255 then
                return 0
            else
                return vCharAttr[pChar+1]
            end if
        elsif sequence(pChar) then
            for i = 1 to length(pChar) do
                pChar[i] = w32GetCType(pChar[i])
            end for
            return pChar
        end if

        return 0

end function

--/topic Support Routines
--/proc w32SetCType(object pChar, object pSet)
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
--Example:
--/code
--      w32SetCType(255,User1_CT) -- Single char
--      w32SetCType({"n","z"},User3_CT) -- Range of chars
--      w32SetCType("13579",User2_CT) -- List of chars
--/endcode
global procedure w32SetCType(object pChar, object pSet)
atom lSet

        if atom(pSet) then
            lSet = pSet
        else
            lSet = 0
            for i = 1 to length(pSet) do
                lSet += pSet[i]
            end for
        end if

        if integer(pChar) then
            if pChar >= 0 and pChar <= 255 then
                vCharAttr[pChar+1] = lSet
            end if
        elsif sequence(pChar) then
            if length(pChar) = 2 and sequence(pChar[1]) and sequence(pChar[2]) then
                for i = floor(pChar[1][1]) to floor(pChar[2][1]) do
                    if i >= 0 and i <= 255 then
                        vCharAttr[i+1] = lSet
                    end if
                end for
            else
                for i = 1 to length(pChar) do
                    if integer(pChar[i]) then
                        if pChar[i] >= 0 and pChar[i] <= 255 then
                            vCharAttr[pChar[i]+1] = lSet
                        end if
                    end if
                end for
            end if
        end if
end procedure

w32SetCType({{097},{122}}, Alpha_CT + Lowercase_CT + Printable_CT + NameChar_CT)
w32SetCType({{065},{090}}, Alpha_CT + Uppercase_CT + Printable_CT + NameChar_CT)
w32SetCType({{009},{013}}, Control_CT + White_CT)
w32SetCType({{000},{008}}, Control_CT)
w32SetCType({{014},{031}}, Control_CT)
w32SetCType({{048},{057}}, Digit_CT + Printable_CT + NameChar_CT)
w32SetCType({{033},{047}}, Punct_CT + Printable_CT)
w32SetCType({{058},{064}}, Punct_CT + Printable_CT)
w32SetCType({{091},{096}}, Punct_CT + Printable_CT)
w32SetCType({{123},{126}}, Punct_CT + Printable_CT)
w32SetCType({{127},{159}}, Symbol_CT)
w32SetCType({{161},{255}}, Symbol_CT)
w32SetCType({32,160}, White_CT + Printable_CT)
w32SetCType('_', Punct_CT + Printable_CT + NameChar_CT)


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
        lText = sprintf("%d%d%s%s%d", {pSize,
                                       length(pPlainText),
                                       pPlainText,
                                       pMask,
                                       length(pMask)
                                      })
        lText &= reverse(lText)
        lMask = sprintf("%s%d%s%d%d%d", {pMask,
                                         length(lText),
                                         pPlainText,
                                         length(pMask)+49,
                                         length(pPlainText)+49,
                                         pSize+49
                                        })
        j = 0
        for i = 1 to length(lText) do
            j += 1
            if j > length(lMask) then
                j = 1
            end if
            if and_bits(lMask[length(lMask)-j+1], 1) then
                lText[i] = (lText[i] * lMask[j]) - (lText[i] / (lMask[j]))
            else
                lText[i] = (lText[i] * lText[i]) + (lMask[j] * lMask[j])
            end if
            if i > 1 then
                lText[i] += lText[i-1]
            end if
        end for
        lText[1] += lMask[length(lMask)]

        lValue = length(pPlainText) * length(pMask) * (11.76943 + (pSize + length(lText) * length(lMask)))
        lRound = length(pPlainText) + length(pMask)
        while length(lResult) < pSize do
            for i = 1 to length(lText) do
                lValue = (lValue * 723.11251) + lText[i] - lRound

                if lText[1+length(lText)-i] != 0 then
                    if lValue < 1E-10 or lValue > 1E+10 then
                        lValue = (length(pPlainText) - length(lResult)) * length(pMask) * (1.76943 + pSize)
                    else
                        lValue = (lValue / (lText[1+length(lText)-i]))
                    end if
                end if
            end for
            lDigits = sprintf("%.15g", lValue)
            if length(lDigits) > 2 then
                lDigits = lDigits[2..length(lDigits)-1]
            end if
            j = length(lDigits)
            while j >= 1 do
                if lDigits[j] != '0' then
                    lDigits = lDigits[1..j]
                    exit
                end if
                j -= 1
            end while
            j = 1
            while j <= length(lDigits) do
                if find(lDigits[j], "0123456789") = 0 then
                    lDigits = lDigits[1..j-1] & lDigits[j+1..length(lDigits)]
                else
                    j += 1
                end if
            end while
            if length(lDigits) > 3 then
                lResult &= lDigits[3..length(lDigits)-2]
            end if
            lValue /= 691971
            lRound *= (length(lResult) - 5.4321)
            lRound = -lRound
        end while

        return lResult[1..pSize]
end function


global constant w32AKey = 1, w32AValue = 2
--------------------------------------
global function w32MakeArgs(sequence pData, integer pArgDelim, integer pKeyDelim)
--------------------------------------
sequence lArgs
sequence lNumData
integer lPos
sequence lKey
sequence lValue
sequence lNumConv
sequence lCurArg

        -- Remove any enclosing whitespace.
        lArgs = w32trim(pData)

        -- Check for enclosing brackets.
        if length(lArgs) > 1
        and (lArgs[1] = '(' and lArgs[length(lArgs)] = ')') then
            -- Strip off the brackets and split it into separate args.
            lArgs = w32split(lArgs[2..length(lArgs)-1], pArgDelim)
            lNumConv = repeat(1, length(lArgs))
        else
            -- Split it into separate args and trim each argument.
            lArgs = w32split(lArgs, pArgDelim)
            lNumConv = repeat(1, length(lArgs))
            for i = 1 to length(lArgs) do
                if length(lArgs[i]) > 0 and find(lArgs[i][1], "'\"") then
                    lNumConv[i] = 0
                end if
                lArgs[i] = w32trim(lArgs[i])
            end for

        end if

        -- Check for numeric args and for keyword/value args.
        for i = 1 to length(lArgs) do
            lCurArg = lArgs[i]
            -- Try to convert it to a number.
            if lNumConv[i] then
                lNumData = w32TextToNumber({lCurArg, w32True})
            else
                lNumData = {0,-1}
            end if
            if lNumData[2] = 0 then
                -- Successfully converted.
                lCurArg = {{},lNumData[1]}
            elsif lNumData[1] != 0 then
                -- Partial conversion.
                lCurArg = {{},{lNumData[1], upper(w32trim(lCurArg[lNumData[2]..length(lCurArg)]))}}

            elsif pKeyDelim > 0  then -- Check for a keyword/value pair.
                lPos = find(pKeyDelim, lCurArg)
                if lPos > 0 then
                    lKey = upper(w32trim(lCurArg[1..lPos-1]))
                    lValue = lCurArg[lPos+1..length(lCurArg)]
                    -- Check for 'escaped' text; ie. no sub fields.
                    if length(lValue) > 1
                    and (lValue[1] = '{' and lValue[length(lValue)] = '}') then
                        -- Strip off the braces
                        lValue = {lValue[2..length(lValue)-1]}
                    else
                        lValue = w32MakeArgs(lValue, pArgDelim, pKeyDelim)
                    end if
                    lCurArg = {lKey, lValue}
                else
                    lCurArg = {{},w32trim(lCurArg)}
                end if
            end if

            lArgs[i] = lCurArg
        end for

        return lArgs
end function

-------------------------------------------
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
        for i = 1 to length(pData) do
            n = w32MaxWidth(pData[i])
            if n > w then
                w = n
            end if
        end for

        return w
end function

-------------------------------------------
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
        for i = 1 to length(pData) do
            ld &= w32ToString(pData[i])
            if i != length(pData) then
                ld &= ','
            else
                ld &= ")"
            end if
        end for
        return ld
end function


object vOldSeed vOldSeed = #69F5C10D

-------------------------------------------
global function w32rand32(atom N)
-------------------------------------------
integer a, b
sequence d
atom X

        d = date()
--/**/  d = sq_mul(vOldSeed,d)  --/* Phix
        d = vOldSeed * d        --  RDS */
        vOldSeed = d[1] + d[2] + d[3]
        vOldSeed += time() * (time() + 17)
        vOldSeed = floor(remainder(vOldSeed * (vOldSeed - 3), #3FFFFFFF))
        set_rand(vOldSeed)
        a = rand(#3FFFFFFF)
        vOldSeed += d[4] + d[5] + d[6]
        vOldSeed += (time() + 3.1427) * (time() + 619)
        vOldSeed = floor(remainder((vOldSeed + 1) * (vOldSeed - 1) + 1, #3FFFFFFF))
        set_rand(vOldSeed)
        b = rand(#3FFFFFFF)
        X = and_bits(a, #FFFF) * #10000 + and_bits(b, #FFFF)
        return floor(X - floor(X/N)*N) + 1
end function

global function w32MinSequenceSize(sequence pList, integer pMinSize, object pNewData)

        if length(pList) < pMinSize then
            pList &= repeat(pNewData, pMinSize - length(pList))
        end if
    
        return pList
end function    
