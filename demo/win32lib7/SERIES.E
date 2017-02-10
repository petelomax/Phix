-- series.e  v1.0 17/July/2002, Derek Parnell
-- A set of routines to manage sets of sequential unique numbers.
--
-- Contains the following routines:
--      next_number(name)              Returns the next number in a 
--                                        named series.
--      current_number(name)           Returns the number returned
--                                        by the most recent call to 
--                                        next_number()
--      define_series(name, attrvals)  Used to initialise or reset
--                                        a series' attributes.
--      get_series(name)               Returns the current attributes
--                                        of a series


-----------------------------------------------
-- FUNCTION: next_number(object Name)
--  RETURNS: The next number in the series.
--  PARAMETERS:
--    Name      Can be any Euphoria object. Each series has a unique 
--              name of your choosing.
--  ACTIONS:
--  *If the series named does not exist, this creates a new series with
--   the default attributes. And if for some reason it cannot create a 
--   new series, it returns immediately with an empty sequence.
--  *Calculates the next number.
--  *If the series is a 'wrapping' type, and the next number is beyond 
--   the last number for the series, the next number is set to the
--   series' first number.
--
--  EXAMPLE:                     
--      -- Define the record layout for Customer.    
--      constant CustID    = next_number("CustRecord"),
--               CustName  = next_number("CustRecord"),
--               CustAddr  = next_number("CustRecord"),
--               CustPhone = next_number("CustRecord"),
--               SIZEOF_Cust = current_number("CustRecord")    
--
--      lNewRecord = repeat(0, SIZEOF_Cust)
--      lNewRecord[CustID] = next_number("custid")

-----------------------------------------------
-- PROCEDURE: define_series(object Name, sequence Values)
--  PARAMETERS:
--    Name      Can be any Euphoria object. Each series has a unique name
--              of your choosing.
--    Values    Is a list of zero or more attribute-value pairs.
--  ACTIONS:
--  *If the series named does not exist, this creates a new series with
--   the default attributes.
--  *Then for each attribute-value pair, it sets the series' attribute
--   to the value supplied. Invalid attributes and values are ignored.
--  The attributes of a series are:
--      SValue      ATOM: The next number to be returned. Default: 1.
--      SIncr       ATOM: The increment used to calculate the new next 
--                        number. Can be negative. Default: 1.
--      SWrap       INTEGER: If the series is a wrapping type, this
--                        should be True. Default: False
--      SFirst      ATOM: The first number to be used in the series.
--                        Default: 1
--      SLast       ATOM: The last number to be used in the series.
--                        Default: 0
--      SRtnId      INTEGER: A routine_id of a callback routine or
--                        -1. Default: -1
--      SUserData   OBJECT: Not used by these routines. It is passed
--                        unchanged to the callback routine. Default: 0
--
--  Note, when you use this routine to set the Value attribute, the 
--  value is not actually reset until the next call to next_number().
--
--  Callback Routine.
--      If defined for a series, the callback routine is invoked just
--      prior to returning from a next_number() or current_number() call.
--      The callback routine receives five parameters: 
--       object: The series name
--       object: The type of callback. Either SCB_NextNum or SCB_CurrentNum
--       atom: For SCB_NextNum this is the next value in the series, 
--             for SCB_CurrentNum this is the previous value returned.
--       integer: Wrapped flag. Only used for SCB_NextNum. If True, 
--             it means that the value has come about because the
--             series has wrapped around from Last to First values.
--       object: The user data. This is passed unaltered from the 
--             define_series() call that set it.
--  The callback routine must return something. Whatever it returns
--  is passed back unaltered to the application in place of the normal
--  value.
--      
--    
--  EXAMPLE:                            
--      -- Customer IDs start at 60001. Use a checkdigit routine to 
--      -- adjust the returned value.    
--      define_series("cust", { {SValue, 60001}, 
--                              {SRtnId, routine_id(GenCheckDigit)},
--                              {SUserData, 11} } )    
--      NextCustID = next_number("cust")
--
--      define_series("row", { {SValue,1}, {SWrap,True}, 
--                             {SFirst,1}, {SLast,24} } )
--      define_series("col", { {SValue,1}, {SWrap,True}, 
--                             {SFirst,1}, {SLast,80} } )
--      . . .
--      while True do
--          k = GetKeyCode()
--          if k != prevcode then
--              if k = Up then
--                defines_series("row", { {SIncr, -1} } )
--                defines_series("col", { {SIncr, 0} } )
--              elsif k = Down then
--                defines_series("row", { {SIncr, 1} } )
--                defines_series("col", { {SIncr, 0} } )
--              elsif k = Left then
--                defines_series("col", { {SIncr, -1} } )
--                defines_series("row", { {SIncr, 0} } )
--              elsif k = Right then
--                defines_series("col", { {SIncr, 1} } )
--                defines_series("row", { {SIncr, 0} } )
--              end if
--              prevcode = k
--          end if
--
--          c = next_number("col")
--          r = next_number("row")
--          . . .
--      end while
--
--      define_series("angle", { {SValue,0}, {SFirst,0}, {SLast,2*PI}, 
--                               {sIncr,0.1}, {SWrap,True} } )
--      . . .
--      Plot (next_number("angle"))

-----------------------------------------------
-- FUNCTION: get_series(object Name)
--  RETURNS: A list of attribute-value pairs. If the series doesn't 
--           exist, an empty sequence is returned.
--  PARAMETERS:
--    Name      Can be any Euphoria object. Each series has a unique
--              name of your choosing.
--  ACTIONS:
--      If the named series doesn't exist, return an empty sequence.
--      Build a list of attributes and their values.
--      Return the attribute-value pair list.    

-----------------------------------------------
-- FUNCTION: current_number(object Name)
--  RETURNS: The previous number returned in the series.
--  PARAMETERS:
--    Name      Can be any Euphoria object. Each series has a unique
--              name of your choosing.
--  ACTIONS:
--  If the series named does not exist, this returns an empty sequence..
--  Returns the current value.
--
--  EXAMPLE:
--       procedure AddNewCustomer()
--           NextID = next_number("cust")
--            . . .
--       end procedure    
--       . . . 
--      AddNewCustomer()
--      -- Get the ID that was just used.
--      CustID = current_number("cust")


--/topic Series
--/info
--Some utility routines to manange series of numbers.
--
--A Series is just a set of numbers that you use when you need a set of sequential
-- values.
--
--Example:
--/code
--    constant fldA = next_number("My Series")
--    constant fldB = next_number("My Series")
--    constant fldC = next_number("My Series")
--    constant fldD = next_number("My Series")
--    constant fldE = next_number("My Series")
--    constant NumFlds = current_number("My Series")
--/endcode
-- This would give the constants fldA - fldE the values 1 to 5 respectively, and the NumFlds is set to
-- 5 as well.
--
--By default, a Series starts at 1 and increments by 1 with each call to /next_number().
-- However, you may define a special series to suit particular needs. You can change the
-- increment amount, the starting value, and whether the series wraps around when getting
-- to the end.
--
--Example:
--/code
--      -- Define a set of angles from 0 to 2PI, incrementing by 0.1.
--      -- And when the last angle is reached, start again at zero.
--      define_series("angle", { {SValue,0}, {SFirst,0}, {SLast,2*PI}, 
--                               {sIncr,0.1}, {SWrap,True} } )
--/endcode
--
--If you have very special requirements, you can even define a routine_id that
-- will be called when the user calls next_number and current_number. You can then
-- make decisions about the value returned to the user; for example you may need to
-- update a database whenever the user gets a new series value, or maybe you need to
-- encrypt it before the user gets it, or convert it text, etc...
--
--/code
--      define_series("special", { {sRtnId,routine_id("SeriesChecker")}, {SUserData,Tolerance} } )
--/endcode
--
--The routine mentioned here will be called with five (5) parameters:
--/li /i "object pName" : The name of the series.
--/li /i "integer pRequest" : Either SCB_CurrentNum or SCB_NextNum which is the type of request that
-- the user is asking for.
--/li /i "atom pValue" : The series' current value.
--/li /i "integer pHasWrapped" : Either 0 (false) or 1 (true) indicating whether or not the series /b just
-- wrapped around to the start again.
--/li /i "object pUserData" : The user data stored in the series by the last define_series() call to it.
--
-- The value returned by this routine will be passed directly to the user.

without trace                                                      
without warning
constant
        True            = (1=1),
        False           = (not True)
global constant
        SValue          = 'v',
        SIncr           = 'i',
        SWrap           = 'w',
        SFirst          = 'f',
        SLast           = 'l',
        SRtnId          = 'r',
        SUserData       = 'u',
        SCB_NextNum     = 'N',
        SCB_CurrentNum  = 'C'

constant
        kSFlds       = {SIncr,SWrap,SFirst,SLast,SValue,SRtnId,SUserData},
        kEmptySeries = {0,    1,    0,     1,    0,    {},    -1,        0},
        kSValue      = 1, -- Always the first field
        kSResetValue = find(SValue,   kSFlds)+1,
        kSIncr       = find(SIncr,    kSFlds)+1,
        kSWrap       = find(SWrap,    kSFlds)+1,
        kSFirst      = find(SFirst,   kSFlds)+1,
        kSLast       = find(SLast,    kSFlds)+1,
        kSRtnId      = find(SRtnId,   kSFlds)+1,
        kSUserData   = find(SUserData,kSFlds)+1

sequence vDefnSeries        -- Names of series
sequence vSeriesData        -- Attribute-set per series
vDefnSeries = {}
vSeriesData = {}

--/topic Series
--/proc define_series(object pName, sequence pAttributes)
--/desc Defines a new series.
--Normally one doesn't need to define a series as a default series is created
-- one the first call of /next_number(). However, if you have special requirements
-- this routine will help customize a series for you.
--
--/i pName is the user-defined name for this series.
--/i pAttributes is a set of zero or more attribute/value pairs to apply to this series.
--
--Valid attributes are : /n
--/li /b SValue : Only used when resetting a series to a defined value.
--/li /b SIncr : The amount to increment the series by. Default is 1.
--/li /b SWrap : Indicator that the series can wrap from last to first. Default is 0 (no wrap).
--/li /b SFirst : The first value in the series. Default is 1.
--/li /b SLast : The last value in the series. Default is 0. If wrapping, this is the value that triggers the wrap.
--/li /b SRtnId : A routine_id that is called when /next_number() and /current_number() are just about to
-- return a value to the user.
--/li /b SUserData : Any user data you wish to be passed back to the routine_id. Default is {}.
--Example:
--/code
--      -- Define a set of angles from 0 to 2PI, incrementing by 0.1.
--      -- And when the last angle is reached, start again at zero.
--      define_series("angle", { {SValue,0}, {SFirst,0}, {SLast,2*PI}, 
--                               {sIncr,0.1}, {SWrap,True} } )
--/endcode

-----------------------------------------------
global procedure define_series(object pName, sequence pValues)
-----------------------------------------------
integer lID
integer lFld

-- Find the series asked for. If it doesn't exist, create it.    
    lID = find(pName, vDefnSeries)
    if lID=0 then
        vSeriesData = append(vSeriesData, kEmptySeries)
        vDefnSeries  = append(vDefnSeries, pName)
        lID = length(vDefnSeries)
    end if

    -- Apply the attribute values, ignoring any invalid ones.
    for i=1 to length(pValues) do

        -- Only accept attr-value pairs: ie. must be a 2-element sequence.
        if sequence(pValues[i]) and length(pValues[i])=2 then

            -- Convert the attribute code into an offset into the series' data.
            -- I need to add one to skip over the Current Value field.
            lFld = find(pValues[i][1], kSFlds)+1

            -- Only apply if valid attr type and the datatype is suitable.
            if lFld>1 and (atom(pValues[i][2]) or (lFld=kSUserData)) then

                vSeriesData[lID][lFld] = pValues[i][2]
            end if
        end if
    end for

end procedure

--/topic Series
--/func next_number(object pName)
--/desc Increments the series and returns the next value in it.
--/ret ATOM: The next value in the series.
--
--Example:
--/code
--    constant CUSTREC      = next_number("Record Layouts")
--    constant C_Id         = next_number(CUSTREC)
--    constant C_GivenName  = next_number(CUSTREC)
--    constant C_FamilyName = next_number(CUSTREC)
--    constant C_Address    = next_number(CUSTREC)
--    constant C_Email      = next_number(CUSTREC)
--    constant CUSTREC_SIZEOF = current_number(CUSTREC)
--
--    constant INVOICE      = next_number("Record Layouts")
--    constant I_Id         = next_number(INVOICE)
--    constant I_Date       = next_number(INVOICE)
--    constant I_CustId     = next_number(INVOICE)
--    constant I_Terms      = next_number(INVOICE)
--    constant I_Address    = next_number(INVOICE)
--    constant INVOICE_SIZEOF = current_number(INVOICE)
--/endcode
-----------------------------------------------
global function next_number(object pName)
-----------------------------------------------
integer lID
atom lNextNum
--  sequence lRC
integer lWrapped


    -- Find the series asked for. If it doesn't exist, create it.    
    lID = find(pName, vDefnSeries)
    if lID=0 then
        define_series(pName,{})
        lID = find(pName, vDefnSeries)
        -- It should exist now. If not, then bail out.
        if lID=0 then
            return {}
        end if
    end if

    lWrapped = False

    -- Has the next value been reset by a call to define_series()?
    if sequence(vSeriesData[lID][kSResetValue]) then
        -- No, do the normal case.
        lNextNum = vSeriesData[lID][kSValue]+vSeriesData[lID][kSIncr]
        -- Handle those series that wrap from last back to first.
        if vSeriesData[lID][kSWrap] then
            if vSeriesData[lID][kSIncr]>0 then
                if lNextNum>vSeriesData[lID][kSLast] then
                    lNextNum = vSeriesData[lID][kSFirst]
                end if
            else
                if lNextNum<vSeriesData[lID][kSLast] then
                    lNextNum = vSeriesData[lID][kSFirst]
                end if
            end if
        end if

    else
        -- Yes, use the reset value instead of calculating it.
        lNextNum = vSeriesData[lID][kSResetValue]
        vSeriesData[lID][kSResetValue] = {}
        -- Range-check those series that wrap.
        if vSeriesData[lID][kSWrap] then
            if vSeriesData[lID][kSIncr]>0  then
                if lNextNum>vSeriesData[lID][kSLast] or
                   lNextNum<vSeriesData[lID][kSFirst] then

                    lNextNum = vSeriesData[lID][kSFirst]
                    lWrapped = True
                end if
            else
                if lNextNum<vSeriesData[lID][kSLast] or
                   lNextNum>vSeriesData[lID][kSFirst] then

                    lNextNum = vSeriesData[lID][kSFirst]
                    lWrapped = True
                end if
            end if
        end if
    end if


    -- Save the value for the next call
    vSeriesData[lID][kSValue] = lNextNum

    -- Check for any callback routine.
    if vSeriesData[lID][kSRtnId]> -1 then
        return call_func(vSeriesData[lID][kSRtnId],
                         {pName, SCB_NextNum, lNextNum, lWrapped,
                          vSeriesData[lID][kSUserData]})
    else
        return lNextNum
    end if

end function

--/topic Series
--/func current_number(object pName)
--/desc Returns the current value in the series but does /b not increment it.
--/ret ATOM: The current value in the series.
--
--Example:
--/code
--    constant CUSTREC      = next_number("Record Layouts")
--    constant C_Id         = next_number(CUSTREC)
--    constant C_GivenName  = next_number(CUSTREC)
--    constant C_FamilyName = next_number(CUSTREC)
--    constant C_Address    = next_number(CUSTREC)
--    constant C_Email      = next_number(CUSTREC)
--    constant CUSTREC_SIZEOF = current_number(CUSTREC)
--
--    constant INVOICE      = next_number("Record Layouts")
--    constant I_Id         = next_number(INVOICE)
--    constant I_Date       = next_number(INVOICE)
--    constant I_CustId     = next_number(INVOICE)
--    constant I_Terms      = next_number(INVOICE)
--    constant I_Address    = next_number(INVOICE)
--    constant INVOICE_SIZEOF = current_number(INVOICE)
--/endcode
-----------------------------------------------
global function current_number(object pName)
-----------------------------------------------
integer lID
--  atom lNextNum

    -- Find the series asked for. If it doesn't exist, bail out.    
    lID = find(pName, vDefnSeries)
    if lID=0 then
        return {}
    end if

    -- Check for any callback routine.
    if vSeriesData[lID][kSRtnId]> -1 then
        return call_func(vSeriesData[lID][kSRtnId],
                         {pName, SCB_CurrentNum,
                          vSeriesData[lID][kSValue], False,
                          vSeriesData[lID][kSUserData]})
    else
        return vSeriesData[lID][kSValue]
    end if
end function

--/topic Series
--/func get_series(object pName)
--/desc Fetches an entire definition for a series.
--/ret SEQUENCE: A Series definition.
--
--The returned value could be used as input to /define_series() if you wish.
--
--The attributes are returned in this order as a set of attribute/value pairs: /n
--/li /b SValue : The current value of the series.
--/li /b SIncr : The amount to increment the series by.
--/li /b SWrap : Indicator that the series can wrap from last to first.
--/li /b SFirst : The first value in the series.
--/li /b SLast : The last value in the series.
--/li /b SRtnId : The stored routine_id, if any
--/li /b SUserData : Any user data you wish to be passed back to the routine_id.
--
--Example:
--/code
--      sequence lDef
--      lDef = get_series("Record Layouts")
--/endcode
-----------------------------------------------
global function get_series(object pName)
-----------------------------------------------
-- Returns a series definition.
integer lID
integer lX

    -- Find the series asked for. If it doesn't exist, bail out.    
    lID = find(pName, vDefnSeries)
    if lID=0 then
        return {}
    end if

    -- Check for a pending value reset.
    if sequence(vSeriesData[lID][kSResetValue]) then
        lX = kSValue
    else
        lX = kSResetValue
    end if

    -- Build the attribute-pair list. The attribute order is not significant.
    return
         {
          {SValue, vSeriesData[lID][lX]},
          {SIncr, vSeriesData[lID][kSIncr]},
          {SWrap, vSeriesData[lID][kSWrap]},
          {SFirst, vSeriesData[lID][kSFirst]},
          {SLast, vSeriesData[lID][kSLast]},
          {SRtnId, vSeriesData[lID][kSRtnId]},
          {SUserData, vSeriesData[lID][kSUserData]}
         }

end function
