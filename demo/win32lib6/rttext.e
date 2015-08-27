include options.e
sequence vUserLang vUserLang = "english"
sequence vContainer vContainer = "msgs.ini"

--/topic Miscellaneous
--/func setUserLanguage(sequence pNew)
--/desc Defines the language category that is used by /Usetext
--/ret SEQUENCE: The previous setting.
--Initially the language is set to "english". You would use this if the messages file contained
-- other language catgories.
--
--/i pNew is the category that will be searched by /UseText in the messages file.
--
--Example:
--/code
--  sequence lOld
--  lOld = setUserLanguage( "thai" )
--  -- Note that the category can be anything.
--  lOld = setUserLanguage( "geek jargon" )
--/endcode

------------------------------------------------
global function setUserLanguage(sequence pNew)
------------------------------------------------
    sequence lOld

    lOld = vUserLang
    vUserLang = pNew
    return lOld
end function

--/topic Miscellaneous
--/func setContainer(sequence pNew)
--/desc Defines the message file that is used by /Usetext
--/ret SEQUENCE: The previous setting.
--Initially the language is set to "msgs.ini". You would use this if the messages file containing
-- the text has a different name or is on a different path.
--
--/i pNew is the name and path of the file that will be searched by /UseText.
--
--Example:
--/code
--  sequence lOld
--  lOld = setContainer( "Application\\Data\\Message.Text" )
--/endcode

------------------------------------------------
global function setContainer(sequence pNew)
------------------------------------------------
    sequence lOld

    lOld = vContainer
    vContainer = pNew
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

    lFoundText = getOpt(vContainer, vUserLang, lTextCode, pDefault)
    if length(lFoundText) = 0 then
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
--/i pTextCode is either the code (an integer) to search for in the messages file, or
-- a message template. /n
--/i pData is either a single string or a sequence of strings. /n
--
--Example: /n
--Assume you have the following messages file ...
--/code
--[english]
--1 = File '%1' not found in folder '%2'
--2 = Record ID '%1' not found
--3 = Record ID '%1' already exists
--9 = Internal error: Code '%1'.
--/endcode
--
--Then we can call this function thus ...
--/code
--      sequence lText
--      lText = FormatMsg(1, {"link.opt", "c:\\temp\\"} ) -- returns "File 'link.opt' not found in folder 'c:\temp\'"
--      lText = FormatMsg(3, "djp") -- returns "Record ID 'djp' already exists"
--      lText = FormatMsg("%1 %3, %2", {"Mr", "Derek", "Parnell"}) -- returns "Mr Parnell, Derek"
--/endcode

------------------------------------------------
global function FormatMsg(object pTextCode, sequence pData)
------------------------------------------------
    sequence lMsg

    if atom(pTextCode) then
        lMsg = UseText(pTextCode,"")
        if length(lMsg) = 0 then
            lMsg = UseText(5001,"Msg#") & sprintf("%g", pTextCode)
        end if
    else
        lMsg = pTextCode
    end if

    if w32string(pData) then
        pData = { pData }
    end if

    for i = 1 to length(pData) do
        lMsg = w32replaceItem( lMsg, sprintf("%%%d", i), pData[i])
    end for
    return lMsg
end function

