-- w32msgs.e
include msgbox.e

constant True = (1=1)

integer
    showWarnings            -- if false, suppresses warning messages
object
    xUserCleanUp            -- the user's "clean up" routine(s)

    xUserCleanUp = -1       -- defined by user
    showWarnings = True     -- display warning messages


sequence
     vMsgsToIgnore vMsgsToIgnore = {}

sequence vAppName,
         vVersion
vAppName = ""
vVersion = ""

------------------------------------------------
procedure fCleanUp(integer pErrCode, sequence pDispMsg)
------------------------------------------------
integer lRC, lNumUserIds
object lUserCleanUp

    -- user clean up
    if sequence(xUserCleanUp) then
        lUserCleanUp = xUserCleanUp
        xUserCleanUp = -1
        lNumUserIds = length(lUserCleanUp)
        for i = 1 to length(lUserCleanUp) do
            lRC = call_func( lUserCleanUp[i],
	           {pErrCode, pDispMsg, i, (i = lNumUserIds)} )
            if lRC = -1 then
                exit
            end if
        end for
        xUserCleanUp = lUserCleanUp
    end if

end procedure

procedure fShowError( sequence pParams, integer pMode )
    integer lErrCode
    integer lResponse
    integer lStyle

    sequence s
    sequence lDispMsg
    sequence lTitle


    if length(pParams) = 0 and pMode = 2 then
        -- This causes the attached cleanup routines to run and nothing else to happen.
        pMode = 3
        lErrCode = 0

    elsif length(pParams) >= 2 and
        sequence(pParams[1]) then
        s = pParams[1]
        if integer(pParams[2]) then
            lErrCode = pParams[2]
        else
            lErrCode = 0
        end if
        if length(pParams) >= 3 then
            s = sprintf(s, pParams[3])
        end if
        lDispMsg = sprintf("Error code %d\n%s", {lErrCode, s})
    else
        s = pParams
        lErrCode = 0
        lDispMsg = s
    end if

    lTitle = vAppName & " - "
    if pMode = 1 then
        lTitle &= "Error Warning"
        lDispMsg &= "\n\n" &
            "Press\n" &
            "  [ YES ]  to continue,\n" &
            "  [ NO ] to ignore repeats, or\n" &
            "  [ Cancel ]  to quit."
        lStyle = MB_ICONEXCLAMATION + MB_YESNOCANCEL
    elsif pMode = 2 then
        lTitle &= "Fatal Error"
        lStyle = MB_ICONHAND
    else -- pMode = 3
        lDispMsg = ""
        lTitle = ""
        lStyle = 0
    end if

    -- give message
    if pMode != 3 and find(lErrCode, vMsgsToIgnore) = 0 then
        lDispMsg &= "\n\n" & vVersion
        lResponse = message_box( lDispMsg, lTitle, lStyle+MB_TASKMODAL )
    else
        lResponse = IDYES
    end if

    if pMode = 2 or pMode = 3 or lResponse = IDCANCEL then
        fCleanUp(lErrCode, lDispMsg)

        if pMode = 2 then
            -- abort
            crash_message(lDispMsg)
        end if
        if pMode != 3 then
            abort(1/0) -- Force the ex.err to be created.
        end if

        if lResponse = IDNO  then
            vMsgsToIgnore &= lErrCode
        end if
    end if

end procedure



-----------------------------------------------------------------------------
--/topic Errors
--/proc abortErr( sequence errorparam )
--/desc Display an error message, run any user-defined cleanup routines, and then abort.
-- /i errorparam can be either a text message or a two-element sequence in which the
-- first is a text message and the second is an error code (integer).
--
-- If any user-defined clean up routine have been attached, they are invoked prior to
-- win32lib's own cleanup routine, then the application is aborted. See /attachCleanUp()
-- for details.
--
-- Example:
--
--/code
--      abortErr( {"The tape drive is not responding.", /w32MsgNum + 17} )
--
--      abortErr(  "Fatal error. Bummer, dude." )
--/endcode


global procedure abortErr( sequence pParams )
   fShowError(pParams, 2)
end procedure


-----------------------------------------------------------------------------
--/topic Errors
--/proc warnErr( sequence errorparam )
--/desc Display an error message, with option to abort.
-- /i errorparam can be either a text message or a two-element sequence in which the
-- first is a text message and the second is an error code (integer).
--
-- This routine will display a dialog window with three buttons. /n
-- /i"[ YES ]" : If selected, the program will continue. /n
-- /i"[ NO  ]" : if pressed, the program will continue, but future warning about the /b same
-- error will be ignored. /n
-- /i"[CANCEL]": if pressed, will stop the program running immediately.
--
-- If any user-defined clean up routine have been attached, they are invoked prior to
-- win32lib's own cleanup routine, if the application is aborted. See /attachCleanUp()
-- for details.
-- Example:
--
--/code
--      warnErr( "Bad data. Abort program?" )
--/endcode

global procedure warnErr( sequence pParams )

   if showWarnings != 0 then
      fShowError(pParams, showWarnings)
   end if

end procedure

-----------------------------------------------------------------------------
--/topic Errors
--/proc setWarning( integer flag )
--/desc Shows or Hides warning messages from user.
-- Since 'warning' messages (triggered by /warnErr) are only warnings and
-- not fatal, it may be advantageous to suppress them in an application.
--
-- The /i flag maybe one of ... /n
--/li 0 To hide warning messages
--/li 1 To show warning messages, giving the option to continue or quit.
--/li 2 To turn all warning messages into fatal errors instead.
--
-- By default, the setting is /b 1
-- Example:
--
--/code
--      -- suppress warning messages
--      setWarning( 0 )
--/endcode

global procedure setWarning( integer flag )
sequence
     lRealFlag

   lRealFlag = {1,0,1,2}
   -- show or suppress warnErr messages
   showWarnings = lRealFlag[ find(flag, {0,1,2}) + 1 ]

end procedure

-----------------------------------------------------------------------------
--/topic Errors
--/proc attachCleanUp( routine_id )
--/desc Establishes a user defined clean up routine which is invoked just prior to win32lib ending or aborting.
--/ret (INTEGER) The number of clean up routines attached.
-- Allows the application to clean up when win32lib application is ending or detects an abort situation. 
-- It is possible to attach multiple clean up routines. If this is done, they are invoked by win32lib in
-- order of most-recently-attached to first-attached, that is in reverse order that they were attached in.
--
-- The clean up routine, when invoked by win32lib, is passed four parameters...
--/li /b ErrorCode (integer) Zero if this is a normal end, or the code number for the 
-- error that is causing win32lib to abort.
--/li /b ErrorText (sequence) The text displayed to the user.
--/li /b ControlId (integer) The id of the current control (0 => no control is current).
--/li /b LastCleanup (integer) A flag which is 1 if this is the last user defined cleanup
-- routine to be invoked, and 0 if there are others still to be invoked.
--
-- The clean up routine must return an integer /i flag. If the /i flag is -1, then no further attached
-- clean up routines will be invoked before win32lib aborts, otherwise any other routines will
-- be invoked. You only return -1 if you really do know what the side-effects will be.
--
-- The clean up routines are run before the crash_routines the program may have defined 
-- if using Euphoria v2.5 or higher.
--
-- Example:
--
--/code
--   function AppCleanUp(integer ErrCode, sequence ErrText, integer ControlId, integer LastCleanUp)
--      . . .
--      return 0 -- Continue with other clean up routines.
--   end function
--
--   -- Link in my clean-up routine
--   cnt = attachCleanUp( routine_id("AppCleanUp" ))
--/endcode

global function attachCleanUp( integer pId )

    if pId >= 0 then
        if atom(xUserCleanUp) then -- this is the first defined routine
            xUserCleanUp = {pId}
        else -- not the first defined routine
            xUserCleanUp = prepend(xUserCleanUp, pId)
        end if

        return length(xUserCleanUp)
    else
        return pId
    end if

end function


-----------------------------------------------------------------------------
--/topic Errors
--/proc detachCleanUp( routine_id )
--/desc Removes a user defined clean up routine from the list of attached ones.
--/ret If unsuccessful this returns -1, else the number of clean up routines still attached.
--
-- Example:
--
--/code
--   integer cnt, CU_id
--   . . .
--   function AppCleanUp(integer ErrCode, sequence ErrText, integer ControlId, integer LastCleanUp)
--      . . .
--      return 0 -- Continue with other clean up routines.
--   end function
--
--   -- Link in my clean-up routine
--   CU_id = routine_id("AppCleanUp" )
--   cnt = attachCleanUp( CU_id )
--   . . .
--   cnt = detachCleanUp( CU_id )
--/endcode

global function detachCleanUp( integer pId )
integer lRc
   if atom(xUserCleanUp)
   then
      return -1
   end if

   lRc = find( pId, xUserCleanUp)

    if lRc > 0 then
        if length(xUserCleanUp) = 1 then
            xUserCleanUp = -1
            return 0
        else
            xUserCleanUp = xUserCleanUp[1 .. lRc - 1] & xUserCleanUp[lRc + 1 .. length(xUserCleanUp)]
            return length(xUserCleanUp)
        end if
    else
        return -1
    end if

end function

--/topic Constants
--/const w32MsgNum
--/desc This is the recommended starting point for user defined error codes.
-- There are no win32lib error codes from this number on. /n
-- Example:
--/code
--   constant BadResponse  = w32MsgNum + 0,
--            FileNotFound = w32MsgNum + 1,
--            ReadOnlyFile = w32MsgNum + 2
--   . . .
--   /abortErr( {The requested file could not be located.", FileNotFound})
--
--/endcode

global constant
   w32MsgNum = 500   -- All win32lib error codes are less than this.

global procedure Init_UI_Msgs(sequence pKeyVals)

    for i = 1 to length(pKeyVals) do
        if equal(upper(pKeyVals[i][1]), "APPNAME") then
            vAppName = pKeyVals[i][2]

        elsif equal(upper(pKeyVals[i][1]), "APPVERSION") then
            vVersion = pKeyVals[i][2]
            if match("Beta", vVersion) then
                puts(1, vVersion)
                puts(1, '\n')
            end if

        end if
    end for
end procedure
