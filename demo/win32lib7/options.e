include wildcard.e
include w32utils.e

--/topic Miscellaneous
--/func getOpt(sequence pSource, sequence pCategory, sequence pKey, object pDefault)
--/desc Retrieves the data associated with /i pKey in the file /i pSource under the group /i pCategory
--/ret OBJECT: The associtated data.
--/i pSource is the name of a text file in which the data can be found. /n
--/i pCategory is the name of a category, or grouping, found in /i pSource. The categories are coded
-- in the file in the form /b("[" category "]") as the only thing in a line. /n
--/i pKey is the key that will be searched for in the category.  If found the associated string
-- will be returned otherwise /i pDefault is returned. /n
--/i pDefault is a string to be returned if the key is not found in the file.
--
--/b Note that the category and key are not case sensitive.
--
--Any text that begins with "->" is ignored to the end of line. This is how you add comments to the source file.
--
--If the associated data can be converted to a Euphoria atom it will be, otherwise it
-- will be returned as a text string. With the exception that the strings "yes", "true", "on" 
-- will be returned as the integer 1 (one), and the strings "no", "false", "off" will be returned 
-- as the integer 0 (zero). If you actually need any of these strings to be returned verbatim
-- then enclose them in quotes in the file.
--
--If the associated text contains "\n" or "\t" these are replaced with 10 (newline) and 9 (tab) respectively.
--
--The associated data can span multiple lines. This done by simply having the continuation lines
-- begin in column 2 or greater.
-- 
--Example: /n
-- Assume we have an options file, App.Ini, containing the following lines ...
--/code
--[COMMS]
--Baud=9600
--Stop=1
--Bits=7
--Parity=Odd
--AutoConnect=yes
--ACKResponse="yes"
--Welcome= "Application V1.0\n"
--         "Welcome to my application.\n"
--         "(c) 2004 HardMacro"
--
--[COLOR]
--Background = White
--Text = Black
--Highlight = Blue
--Selection = Pink
--
--[LIMITS]
--Files = 10
--DBSize = 1024 -> MB
--/endcode
--
-- then we could call this routine ...
--
--/code
--      object lRes
--      lRes = getOpt("App.Ini", "color", "text", "Gray") -- Returns "Black"
--      lRes = getOpt("App.Ini", "comms", "autoconnect", "no") -- Returns 1
--      lRes = getOpt("App.Ini", "comms", "ACKResponse", "none") -- Returns "yes"
--      lRes = getOpt("App.Ini", "comms", "Parity", "even") -- Returns "Odd"
--      lRes = getOpt("App.Ini", "comms", "CRC", "yes") -- Returns "yes"
--      lRes = getOpt("App.Ini", "User", "ID", -1) -- Returns -1
--      lRes = getOpt("App.Ini", "limits", "files", -1) -- Returns 10
--      lRes = getOpt("App.Ini", "limits", "connections", 4) -- Returns 4
--      lRes = getOpt("App.Ini", "limits", "DBSize", 16) -- Returns 1024
--      lRes = getOpt("App.Ini", "comms", "welcome", "") -- Returns ...
--Application V1.0
--Welcome to my application.
--(c) 2004 HardMacro
--/endcode

global function getOpt(sequence pSource, sequence pCategory, sequence pKey, object pDefault)
    integer lFH 
    object lData 
    sequence lKey                 
    sequence lOptData
    integer lInCategory
    integer lPos


    lInCategory = 0
                
    lData = pDefault
    lFH = open(pSource, "r")
    if lFH = -1 then
        if match(".ini", lower(pSource)) = 0 then
            pSource &= ".ini"
        end if
        lFH = open(pSource, "r")
    end if

    if lFH = -1 then
        return lData
    end if
           
    pCategory = '[' & lower(w32trim(pCategory)) & ']'                
    pKey = lower(w32trim(pKey))
    while w32True do
        lData = gets(lFH)
        if atom(lData) then
            close(lFH)
            return pDefault
        end if

        lData = w32trim(lData)
        -- Strip off any comment text
        lPos = match("->", lData)
        if lPos > 0 then
            if lPos = 1 or find(lData[lPos-1], " \n\t") != 0 then
                lData = w32trim(lData[1..lPos-1])
            end if
        end if
        if length(lData) > 0 then
            if lInCategory then
                if lData[1] = '[' then
                    close(lFH)
                    return pDefault
                end if

                lPos = find('=', lData)
                if lPos then
                    lKey = lower(w32trim(lData[1..lPos-1]))
                    if equal(lKey, pKey) then
                        lOptData = lData[lPos+1..length(lData)]
                        if length(lOptData) < 2 or lOptData[1] != '"' or lOptData[$] != '"' then
                            lOptData = w32trim(lOptData)
                        end if
                        -- Check for continuation lines...

                        lData = gets(lFH)
                        while sequence(lData) do
                            if find(lData[1], " \t\n") = 0 then
                                exit
                            end if
                            -- Check for comments on the line.
                            lPos = match("->", lData)
                            if lPos = 1 then
                                -- Just ignore the line
                            else
                                if lPos > 0 then
                                    if find(lData[lPos-1], " \t") != 0 then
                                        -- strip off comment.
                                        lData = w32trim(lData[1..lPos-2])
                                    end if
                                end if
                                lData = w32trim(lData)
                                if length(lData) = 0 then
                                    -- A totally blank line is end of continuation too.
                                    exit
                                end if

                                -- join this to previous text for this key.
                                lOptData &= lData
                            end if
                            lData = gets(lFH)
                        end while
                        exit
                    end if
                end if
            else
                if match(pCategory, lower(lData)) = 1 then
                   lInCategory = 1
                end if
            end if
        end if

    end while

    close(lFH)

    if length(lOptData) >= 2 and lOptData[1] = '"' and lOptData[$] = '"' then
        lData = lOptData[2..length(lOptData)-1]
    else        
        lData = w32TextToNumber({lOptData, w32True})
        if lData[2] = 0 then
            lData = lData[1]
        else
            lData = lOptData
            lOptData = lower(lData)
            if find(lOptData,{"true","yes","on"}) then
                lData = w32True
            elsif find(lOptData,{"false","no","off"}) then
                lData = w32False
            end if
        end if
    end if
            
    if sequence(lData) then
        lData = w32replaceItem(lData, "\\n", '\n')
        lData = w32replaceItem(lData, "\\t", '\t')
    end if
    return lData
end function
