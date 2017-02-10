include options.e as opt
include w32utils.e as util

sequence vUserLang vUserLang = "english"
sequence vContainer vContainer = "msgs.ini"

--/topic Miscellaneous
--/func setUserLanguage(object pNew)
--/desc Defines or returns the language category that is used by /UseText
--/ret SEQUENCE: The previous setting.
--Initially the language is set to "english". You would use this if the messages file contained
-- other language catgories.
--
--If /i pNew is a sequence, it is the category that will be searched by /UseText in the messages file.
--Otherwise, the current language string will be returned.
--
--Example:
--/code
--  -- Get current setting
--  sequence lCurrent
--  lCurrent = setUserLanguage( 0 ) -- Any non-sequence parameter value will do.
--
--  sequence lOld
--  lOld = setUserLanguage( "thai" )
--  -- Note that the category can be anything.
--  lOld = setUserLanguage( "geek jargon" )
--/endcode

------------------------------------------------
global function setUserLanguage(object pNew)
------------------------------------------------
sequence lOld

    lOld = vUserLang
    if sequence(pNew) then
        vUserLang = pNew
    end if
    return lOld
end function

--/topic Miscellaneous
--/func setContainer(object pNew)
--/desc Defines the message file that is used by /Usetext
--/ret SEQUENCE: The previous setting.
--Initially the file name is set to "msgs.ini". You would use this if the messages file containing
-- the text has a different name or is on a different path.
--
--If /i pNew is a sequence, it is the name and path of the file that will be searched by /UseText.
-- Otherwise, the current file name and path will be returned.
--
--Example:
--/code
--  -- Get current value
--  sequence lCurrent
--  lCurrent = setContainer( 0 ) -- Any non-sequence parameter value will do.
-- 
--  sequence lOld
--  lOld = setContainer( "Application\\Data\\Message.Text" )
--/endcode

------------------------------------------------
global function setContainer(object pNew)
------------------------------------------------
sequence lOld

    lOld = vContainer
    if sequence(pNew) then
        vContainer = pNew
    end if
    return lOld
end function

--/topic Miscellaneous
--/func UseText(object pTextCode, sequence pDefault)
--/desc Fetches the text associated with /i pTextCode
--/ret SEQUENCE: The text.
--This searches the current messages file (see /setContainer ) for the text code
-- in the current language category (see /setUserLanguage ) and when found returns the
-- associated text string.
--
-- If no associated text could be found and no default was supplied, it returns the value of /i pTextCode enclosed
-- with /i MSG[ and /i ]. 
--
--Example: /n
--Assume you have the following messages file ...
--/code
--[english]
--0 = Okay
--1 = File not found
--2 = Record not found
--3 = Record already exists
--9 = Internal error.
--UM = Unit of Measure
--KG = Kilogram
--/endcode
--
--Then we can call this function thus ...
--/code
--      sequence lText
--      lText = UseText(0, "") -- returns "Okay"
--      lText = UseText(9, "") -- returns "Internal error."
--      lText = UseText("KG", "kilo") -- returns "Kilogram"
--      lText = UseText("KM", "") -- returns "MSG[KM]"
--/endcode

------------------------------------------------
global function UseText(object pTextCode, sequence pDefault)
------------------------------------------------
sequence lTextCode
sequence lFoundText

    if atom(pTextCode) then
        lTextCode = sprintf("%g", pTextCode)
    else
        lTextCode = pTextCode
    end if

    lFoundText = opt:getOpt(vContainer, vUserLang, lTextCode, pDefault)
    if length(lFoundText)=0 then
        lFoundText = "MSG[" & lTextCode & "]"
    end if

    return lFoundText
end function

--/topic Miscellaneous
--/func FormatMsg(object pTextCode, sequence pData)
--/desc Fetches the format template associated with /i pTextCode and applies /i pData to it.
--/ret SEQUENCE: The formatted text.
--This searches the current messages file (see /setContainer ) for the text code
-- in the current language category (see /setUserLanguage ) and when found
-- the function uses it as a template with formatting codes in it.
--
--/i pTextCode is either the code (an integer) to search for templates in the messages file, or
-- a message template itself. /n
--/i pData is either a single string or a sequence of strings. /n
--
--A message template can contain zero or more place-holder tokens. Each token is replaced by its
-- respective value from the /i pData to form the final output. A token is either a two-character
-- one in the form of '%N' where 'N' is a digit from 1 to 9, or a token in the form '%{X}' where
-- 'X' 
--
--Example: /n
--Assume you have the following messages file ...
--/code
--[english]
--1 = File '%{NAME}' not found in folder '%{PATH}'
--2 = Record ID '%1' not found
--3 = Record ID '%1' already exists
--9 = Internal error: Code '%1'.
--5001 = Unknown error code: '%1'
--/endcode
--
--Then we can call this function thus ...
--/code
--      sequence lText
--      lText = FormatMsg(1, {"PATH=c:\\temp\\", "NAME=link.opt"} ) -- returns "File 'link.opt' not found in folder 'c:\temp\'"
--      lText = FormatMsg(3, "djp") -- returns "Record ID 'djp' already exists"
--      lText = FormatMsg(17, "Some Text") -- returns "Unknown error code: Msg#17, Some Text"
--      lText = FormatMsg("%1 %3, %2", {"Mr", "Derek", "Parnell"}) -- returns "Mr Parnell, Derek"
--/endcode

------------------------------------------------
global function FormatMsg(object pTextCode, sequence pData)
------------------------------------------------
sequence lMsg
sequence lToken
integer  lPos
sequence lKey
sequence lVal

    if atom(pTextCode) then
        lMsg = UseText(pTextCode, {""})
        if equal(lMsg, {""}) then
            lMsg = UseText(5001, "Msg#" & sprintf("%g", pTextCode) & ", " & util:w32to_string(pData))
        end if
    else
        lMsg = pTextCode
    end if

    if util:w32string(pData) then
        pData = {pData}
    end if

    for i=1 to length(pData) do
        lPos = find('=', pData[i])
        if lPos>0 then
            lKey = w32trim(pData[i][1..lPos-1])
            lVal = w32trim(pData[i][lPos+1..$])
            lToken = "%{" & lKey & "}"
            if match(lToken, lMsg)>0 then
                lMsg = util:w32replaceItem(lMsg, lToken, lVal)
                pData[i] = 0 -- Mark as used.
            end if
        end if
    end for


    for i=1 to length(pData) do
        if i<10 and sequence(pData[i]) then
            lToken = sprintf("%%%d", i)
            if match(lToken, lMsg)>0 then
                lMsg = util:w32replaceItem(lMsg, lToken, pData[i])
                pData[i] = 0 -- Mark as used.
            end if
        end if

        if i>=10 or sequence(pData[i]) then
            lToken = sprintf("%%{%d}", i)
            if match(lToken, lMsg)>0 then
                lMsg = util:w32replaceItem(lMsg, lToken, pData[i])
                pData[i] = 0
            end if
        end if
    end for


    return lMsg
end function
