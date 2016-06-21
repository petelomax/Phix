--
--  msgbox.e
--  ========
--
--      Windows message_box() function (Phix compatible)
--
--  17/02/2016: Removed GetActiveWindow (as per Raymond Chen, this can do all manner
--                      of rude and nasty things, including disabling Task Manager.)
--                      New optional parameter for a hwnd if you have one/want your
--                      message box to be modal, and set_mb_hwnd() to default it.
--                      Also made msg/title string, style integer, added constants
--                      to the documentation, and made this source Phix-only.

constant P = C_POINTER, I = C_INT

atom mWnd = NULL,
     user32 = 0,
     xMessageBoxA

global procedure set_mb_hwnd(atom hWnd)
    mWnd = hWnd
end procedure

global function message_box(sequence msg, sequence title, integer style=MB_OK, atom hWnd=mWnd)
integer res = 0
atom pMsg = allocate_string(msg)
atom pTitle = allocate_string(title)

    if user32=0 then
        if platform()!=WIN32 then ?9/0 end if   -- (must use eg IupMessage instead)
        user32 = open_dll("user32.dll")
        if user32=0 then ?9/0 end if
        xMessageBoxA = define_c_func(user32, "MessageBoxA",{P,P,P,I},I)
        if xMessageBoxA=-1 then ?9/0 end if
    end if
    if hWnd=NULL then
        style = or_bits(style,MB_TASKMODAL)
    end if
--  res = c_func(xMessageBoxA, {hWnd,msg,title,style})
    res = c_func(xMessageBoxA, {hWnd,pMsg,pTitle,style})
    free(pMsg)
    free(pTitle)
    return res
end function

