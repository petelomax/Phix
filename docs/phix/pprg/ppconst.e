--
-- Some constants ripped from win32lib and other include files.
-- (these should only be needed when trying to shroud the source)
-- Hopefully these are unlikely to change.


global function pprgb( integer r, integer g, integer b )
    return r + ( g * 256 ) + ( b * 256 * 256 ) 
end function


global constant 
--	default 		= #80000000,

    wS_CHILD        = #40000000,
    wS_POPUP        = #80000000,
    wS_VISIBLE      = #10000000,
    wS_BORDER       = #00800000,
    wS_DLGFRAME     = #00400000,
    wS_SYSMENU      = #00080000,

    wS_EX_TOOLWINDOW= 128,
    
    bS_DEFPUSHBUTTON= #0001,       -- default pushbutton (heavy border)
    bS_PUSHBUTTON   = #0000,

    eS_LEFT         = #0000,        -- left justified text
    eS_MULTILINE    = #0004,        -- multiline edit
    eS_AUTOHSCROLL  = #0080,        -- automatic horizontal scroll
    eS_AUTOVSCROLL  = #0040,        -- automatic vertical scroll
    eS_READONLY     = #0800,

    wM_GETMINMAXINFO    = #24,

    black           = pprgb(   0,   0,   0 ),
    blue            = pprgb(   0,   0, 128 ),
    skyBlue         = pprgb(   0, 128, 255 ),
    green           = pprgb(   0, 128,   0 ),
    cyan            = pprgb(   0, 128, 128 ),
    red             = pprgb( 128,   0,   0 ),
    magenta         = pprgb( 128,   0, 128 ),
    darkBrown       = pprgb(  64,  64,   0 ),
    brown           = pprgb( 128, 128,   0 ),
    darkGray        = pprgb(  64,  64,  64 ),
    gray            = pprgb( 128, 128, 128 ),
    lightGray       = pprgb( 192, 192, 192 ),
    brightBlue      = pprgb(   0,   0, 255 ),
    brightGreen     = pprgb(   0, 255,   0 ),
    brightCyan      = pprgb(   0, 255, 255 ),
    brightRed       = pprgb( 255,   0,   0 ),
    pink            = pprgb( 255, 176, 176 ),
    brightMagenta   = pprgb( 255,   0, 255 ),
    purple          = pprgb( 208, 128, 208 ),
    yellow          = pprgb( 255, 255,   0 ),
    white           = pprgb( 224, 224, 224 ),
    parchment       = pprgb( 255, 255, 224 ),
    brightWhite     = pprgb( 255, 255, 255 ),

    true            = (1 = 1),
    false           = (1 = 0),

    srcCopy         = #CC0020, -- (DWORD) dest = source
    srcPaint        = #EE0086, -- (DWORD) dest = source OR dest
    srcAnd          = #8800C6, -- (DWORD) dest = source AND dest
    srcInvert       = #660046, -- (DWORD) dest = source XOR dest
    srcErase        = #440328, -- (DWORD) dest = source AND (NOT dest )
    notSrcCopy      = #330008, -- (DWORD) dest = (NOT source)
    notSrcErase     = #1100A6, -- (DWORD) dest = (NOT src) AND (NOT dest)
    mergeCopy       = #C000CA, -- (DWORD) dest = (source AND pattern)
    mergePaint      = #BB0226, -- (DWORD) dest = (NOT source) OR dest
    patCopy         = #F00021, -- (DWORD) dest = pattern
    patPaint        = #FB0A09, -- (DWORD) dest = DPSnoo
    patInvert       = #5A0049, -- (DWORD) dest = pattern XOR dest
    dstInvert       = #550009, -- (DWORD) dest = (NOT dest)
    blackness       = #000042, -- (DWORD) dest = BLACK
    whiteness       = #FF0062, -- (DWORD) dest = WHITE

--	w32hBreak		= 1,
--	w32hPause		= 2,
    w32hMouse       = 3,
    w32hClick       = 4,
    w32hKeyPress    = 5,
--	w32hKeyUp		= 6,
    w32hKeyDown     = 7,
    w32hResize      = 8,
    w32hChange      = 9,
    w32hGotFocus    = 10,
    w32hLostFocus   = 11,
    w32hScroll      = 12,
    w32hOpen        = 13,
    w32hClose       = 14,
--	w32hDestroy 	= 15,
    w32hTimer       = 16,
    w32hPaint       = 17,
--	w32hDragAndDrop = 18,
    w32hEvent       = 19,
    w32hActivate    = 20,
--	w32hAfterEvent	= 21,
--	w32hDropDown	= 22,
--	w32hCloseUp 	= 23,
--	w32hIdle		= 24,

--	w32Edge = {-1},

-- ShowWindow() Commands
--	sW_HIDE = 0,
--	sW_SHOWNORMAL = 1,
    sW_NORMAL = 1,
--	sW_SHOWMINIMIZED = 2,
    sW_SHOWMAXIMIZED = 3,
--	sW_MAXIMIZE = 3,
--	sW_SHOWNOACTIVATE = 4,
--	sW_SHOW = 5,
--	sW_MINIMIZE = 6,
--	sW_SHOWMINNOACTIVE = 7,
--	sW_SHOWNA = 8,
--	sW_RESTORE = 9,
--	sW_SHOWDEFAULT = 10,
--	sW_MAX = 10,

-- attributes for EZ_FONTS
    normal          = 0,
    bold            = 1,
    italic          = 2,
    underline       = 4,
    strikeout       = 8,
    
-- attributes for showWindow
--  normal          = 0,
    modal           = -1,
--	dialog			= -2,

--	minimize		= sW_SHOWMINIMIZED,
--	maximize		= sW_SHOWMAXIMIZED,
--	minimized		= sW_SHOWMINIMIZED,
--	maximized		= sW_SHOWMAXIMIZED,

-- Window Messages
--	wM_NULL 		= #0,
--	wM_CREATE		= #1,
--	wM_DESTROY		= #2,
    wM_MOVE         = #3,
    wM_SIZE         = #5,
--	wM_ACTIVATE 	= #6,

    vK_TAB          = #09,
    vK_RETURN       = #0D,
    vK_ENTER        = vK_RETURN,
    vK_ESCAPE       = #1B,
    vK_PRIOR        = #21,
    vK_PAGEUP       = vK_PRIOR,
    vK_NEXT         = #22,
    vK_PAGEDOWN     = vK_NEXT,
    vK_END          = #23,
    vK_HOME         = #24,
    vK_LEFT         = #25,
    vK_UP           = #26,
    vK_RIGHT        = #27,
    vK_DOWN         = #28,
    vK_APPS         = #5D,
    vK_F1           = #70,
    vK_F2           = #71,
    vK_F3           = #72,
    vK_F4           = #73,
    vK_F5           = #74,
    vK_F6           = #75,
    vK_F12          = #7B,
    vK_F21          = #84,

--	aNSI_CHARSET	= 0,
    dEFAULT_CHARSET = 1,

-- attributes for shift keys
    shiftMask       = #01,           -- shift key is held down
    controlMask     = #02,           -- control key is held down
--	altMask 		= #04,			 -- alt key is held down
--	keyMask 		= #0F,
--	leftBtnMask 	= #10,			 -- Left Button down
    rightBtnMask    = #20,           -- Right Button Down
--	middleBtnMask	= #40,			 -- Middle Button down
--	btnMask 		= #F0,

    -- mouse events
    wM_LBUTTONDBLCLK= #203, -- mouse button double clicked
--	wM_RBUTTONDBLCLK= #206, -- mouse right button double clicked
    wM_LBUTTONDOWN  = #201, -- mouse button down
    wM_RBUTTONDOWN  = #204, -- right button down 
--	wM_MBUTTONDOWN	= #207, -- middle button down
    wM_MOUSEMOVE    = #200, -- mouse moved
    wM_LBUTTONUP    = #202, -- left button released
--	wM_RBUTTONUP	= #205, -- right button released
--	wM_MBUTTONUP	= #208, -- middle button released
--	wM_MOUSEWHEEL	= #20A, -- mouse wheel moved
    
-- From msgbox.e:
    mB_OK           = #00,      -- Message box contains one push button: OK
--	mB_ABORTRETRYIGNORE = #02,	--  Abort, Retry, Ignore
    mB_DEFBUTTON2   = #100,    -- Second button is default button
    mB_OKCANCEL     = #01,      -- Message box contains OK and Cancel
--	mB_YESNOCANCEL	= #03,		-- Message box contains Yes, No, and Cancel
--	mB_YESNO		= #04,		-- Message box contains Yes and No
--	mB_RETRYCANCEL	= #05,		-- Message box contains Retry and Cancel
    mB_ICONHAND     = #10,      -- A hand appears
--	mB_ICONQUESTION = #20,		-- A question-mark icon appears
--	mB_ICONWARNING	= #30,		-- Exclamation-point appears in the box
--	mB_ICONASTERISK = #40,		-- Lowercase letter i in a circle appears
--	mB_ICONERROR = mB_ICONHAND,
--	mB_ICONSTOP = mB_ICONHAND,
--	mB_ICONEXCLAMATION = mB_ICONWARNING,
--	mB_ICONINFORMATION = mB_ICONASTERISK,

    iDOK            = 1,        -- OK button was selected.
    iDCANCEL        = 2,        -- Cancel button was selected.
--	iDABORT 		= 3,		-- Abort button was selected.
--	iDRETRY 		= 4,		-- Retry button was selected.
--	iDIGNORE		= 5,		-- Ignore button was selected.
--	iDYES			= 6,		-- Yes button was selected.
--	iDNO			= 7,		-- No button was selected.

-- From database.e:
-- ERROR STATUS
    dB_OK           = 0,
    dB_OPEN_FAIL    = -1, 
--	dB_EXISTS_ALREADY= -2,
--	dB_LOCK_FAIL	= -3,
    dB_LOCK_NO      = 0,        -- don't bother with file locking 
--	dB_LOCK_SHARED	= 1,		-- read the database
    dB_LOCK_EXCLUSIVE= 2,       -- read and write the database

-- From dll.e:
-- C types for .dll arguments and return value:
--	c_CHAR	  = #01000001,
--	c_UCHAR   = #02000001,
--	c_SHORT   = #01000002,
--	c_USHORT  = #02000002,
    c_INT     = #01000004,
    c_UINT    = #02000004,
    c_LONG    = c_INT,
    c_ULONG   = c_UINT,
    c_POINTER = c_ULONG,
--	c_FLOAT   = #03000004,
--	c_DOUBLE  = #03000008,

--	null = 0,		 -- NULL pointer

--from tk_mem.e:
    byte    = -1,  
--	int8	= byte,
--	word	= -2,
--	int16	= word,
    long    = -3,
--	dWord	= long,
--	int32	= long,
--	lpsz	= -4,
--	hndl	= -5,
--	hndlAddr = -6,
    strz    = -7,
--	uInt	= -8,
--	ptr 	= uInt,
--	uLong	= uInt,
--	single	= -9,
--	float	= single,
--	double	= -10,

--  Pen Styles
    solid           = 0,
    dash            = 1,        --  -------
    dot             = 2,        --  .......
    dashDot         = 3,        --  _._._._
    dashDotDot      = 4         --  _.._.._
-- from get.e:
--	gET_SUCCESS 	= 0,
--	gET_EOF 		= -1,
--	gET_FAIL		= 1