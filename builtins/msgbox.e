-- Phix compatible version, amended to permit forward referencing.
-- Euphoria 2.4
-- Windows message_box() function

without warning

--/* Not required for Phix:
include dll.e
include machine.e
include misc.e
--include porall.e
--*/

--/* Not required for Phix (see psym.e)
-- Possible style values for message_box() style sequence
global constant 
    MB_ABORTRETRYIGNORE = #02, --  Abort, Retry, Ignore
    MB_APPLMODAL = #00,       -- User must respond before doing something else
    MB_DEFAULT_DESKTOP_ONLY = #20000,    
    MB_DEFBUTTON1 = #00,      -- First button is default button
    MB_DEFBUTTON2 = #100,      -- Second button is default button
    MB_DEFBUTTON3 = #200,      -- Third button is default button
    MB_DEFBUTTON4 = #300,   -- Fourth button is default button
    MB_HELP = #4000,            -- Windows 95: Help button generates help event
    MB_ICONASTERISK = #40,
    MB_ICONERROR = #10, 
    MB_ICONEXCLAMATION = #30, -- Exclamation-point appears in the box
    MB_ICONHAND = MB_ICONERROR,        -- A hand appears
    MB_ICONINFORMATION = MB_ICONASTERISK,-- Lowercase letter i in a circle appears
    MB_ICONQUESTION = #20,    -- A question-mark icon appears
    MB_ICONSTOP = MB_ICONHAND,
    MB_ICONWARNING = MB_ICONEXCLAMATION,
    MB_OK = #00,              -- Message box contains one push button: OK
    MB_OKCANCEL = #01,        -- Message box contains OK and Cancel
    MB_RETRYCANCEL = #05,     -- Message box contains Retry and Cancel
    MB_RIGHT = #80000,        -- Windows 95: The text is right-justified
    MB_RTLREADING = #100000,   -- Windows 95: For Hebrew and Arabic systems
    MB_SERVICE_NOTIFICATION = #40000, -- Windows NT: The caller is a service 
    MB_SETFOREGROUND = #10000,   -- Message box becomes the foreground window 
    MB_SYSTEMMODAL  = #1000,    -- All applications suspended until user responds
    MB_TASKMODAL = #2000,       -- Similar to MB_APPLMODAL 
    MB_YESNO = #04,           -- Message box contains Yes and No
    MB_YESNOCANCEL = #03      -- Message box contains Yes, No, and Cancel

-- possible values returned by MessageBox() 
-- 0 means failure
global constant IDABORT = 3,  -- Abort button was selected.
        IDCANCEL = 2, -- Cancel button was selected.
        IDIGNORE = 5, -- Ignore button was selected.
        IDNO = 7,     -- No button was selected.
        IDOK = 1,     -- OK button was selected.
        IDRETRY = 4,  -- Retry button was selected.
        IDYES = 6    -- Yes button was selected.
--*/

atom user32,
     xMessageBoxA, 
     xGetActiveWindow

integer initM
        initM = 0
procedure init()
    if platform()!=WIN32 then ?9/0 end if
--DEV lock as per pprntf.e:
        user32 = open_dll("user32.dll")
--#without reformat
        xMessageBoxA = define_c_func(user32, "MessageBoxA",
            {C_POINTER, --  HWND  hWnd, // handle of owner window
             C_POINTER, --  LPCTSTR  lpText,    // address of text in message box
             C_POINTER, --  LPCTSTR  lpCaption, // address of title of message box
             C_INT},    --  UINT  uType         // style of message box
            C_INT)      -- int 
        if xMessageBoxA=-1 then ?9/0 end if
        xGetActiveWindow = define_c_func(user32, "GetActiveWindow",
            {},
            C_LONG)     -- HWND
        if xGetActiveWindow=-1 then ?9/0 end if
--#with reformat
--  end if
    initM = 1
end procedure

global function message_box(sequence msg, sequence title, object style)
atom pText, pTitle, hWnd
integer res
    res = 0

    if not initM then init() end if

    pText = allocate_string(msg)
    if pText!=0 then
        pTitle = allocate_string(title)
        if pTitle!=0 then
            if sequence(style) then
                style = or_all(style)
            end if
            hWnd = c_func(xGetActiveWindow, {})
            res = c_func(xMessageBoxA, {hWnd,pText,pTitle,style})
            free(pTitle)
        end if
        free(pText)
    end if
    return res
end function



