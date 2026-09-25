--
-- demo\theGUI\theGUI.WINAPI.e
--
--  Simple wrapper for WinAPI proof-of-concept (poc) test files, intended
--  solely to make C ==> Phix much easier, rather than any long-term use.
--  Largely this is just a set of tediously trivial mini-shims to make
--  Phix code look more like C code, built in an ad-hoc and on demand
--  fashion, and never remotely intended to be in any sense "complete".
--  This is a very thin shim to the underlying API with almost zero effort
--  towards making anything any easier or even slightly simpler to use,
--  notablke exceptions being RegisterClassEx, ExtCreatePen, GetClientRect, 
--  GetTextExtentPoint32, and TextOut.
--
--  NB contains much useless gdiplus stuff, please ignore all that..
--      (I should probably move it all to theGUI.gdiplus.e) [DEV]
--
include cffi.e
set_unicode(0)
--set_unicode(1) -- DEV should really... then again it's a fair bit of work for near-0 gain
--                                       (I've marked 25 things below with set_unicode...)

global constant 
        WM_USER = #400, -- (1024)
--      AC_SRC_OVER = 0,
--      AC_SRC_ALPHA = 1,
        BI_RGB = 0,
        BI_BITFIELDS = 3,
        BLACK_BRUSH = 4,
        BS_PUSHBUTTON = 0,
        BS_DEFPUSHBUTTON = 1,
        CF_TEXT         = 1,
--      CF_BITMAP       = 2,
        CF_DIB          = 8,
        CF_UNICODETEXT  = 13,
--      CF_DIBV5        = 17,
        CLIP_DEFAULT_PRECIS = 0x00000000, -- Specifies that default clipping MUST be used.
--      CLIP_CHARACTER_PRECIS = 0x00000001, -- This value SHOULD NOT be used.
--      CLIP_STROKE_PRECIS = 0x00000002,    -- This value MAY be returned when enumerating rasterized, TrueType and vector fonts.<31>
--      CLIP_LH_ANGLES = 0x00000010,        -- This value is used to control font rotation, as follows:
--If set, the rotation for all fonts SHOULD be determined by the orientation of the coordinate system; that is, whether the orientation is left-handed or right-handed.
--If clear, device fonts SHOULD rotate counterclockwise, but the rotation of other fonts SHOULD be determined by the orientation of the coordinate system.
--      CLIP_TT_ALWAYS = 0x00000020,        -- This value SHOULD NOT<32> be used.
--      CLIP_DFA_DISABLE = 0x00000040,      -- This value specifies that font association SHOULD<33> be turned off.
--      CLIP_EMBEDDED = 0x00000080,
        CS_VREDRAW = 1,
        CS_HREDRAW = 2,
        COLOR_BTNFACE = 15,
        COLOR_BTNFACE1 = COLOR_BTNFACE+1,
        CREATE_ALWAYS = 2,
        CW_USEDEFAULT = #80000000,
        DashStyleDash = 1,
        DashStyleDot    = 2,
        DashStyleDashDot  = 3,
        DashStyleDashDotDot = 4,
        DEFAULT_CHARSET = 1,
        DEFAULT_GUI_FONT = 17,
        DEFAULT_QUALITY = 0x00,
--      DRAFT_QUALITY = 0x01,
--      PROOF_QUALITY = 0x02,
--      NONANTIALIASED_QUALITY = 0x03,
--      ANTIALIASED_QUALITY = 0x04,
        CLEARTYPE_QUALITY = 0x05,
        DEFAULT_PITCH = 0,
--      FIXED_PITCH = 1,
--      VARIABLE_PITCH = 2,
        DIB_RGB_COLORS = 0,
        ES_MULTILINE = 4,
        ES_READONLY =  #800,
        FF_SWISS = 0x20,
        FILE_ATTRIBUTE_NORMAL = #80,
        FW_NORMAL = 400,
        FW_BOLD = 700,
        GENERIC_WRITE =  #40000000,
--#define GL_RED               0x1903
--#define GL_RGB               0x1907
--#define GL_RGBA          0x1908
--#define GL_BGR               0x80E0
        GL_BGRA = 0x80E1,
--#define GL_DEPTH_COMPONENT 0x1902
--              GL_RGB4                         = #804F,
--              GL_RGB5                         = #8050,
--              GL_RGB8                         = #8051,
--              GL_RGB10                        = #8052,
--              GL_RGB12                        = #8053,
--              GL_RGB16                        = #8054,
--              GL_RGBA2                        = #8055,
--              GL_RGBA4                        = #8056,
--              GL_RGB5_A1                      = #8057,
                GL_RGBA8                        = #8058,
--              GL_RGB10_A2                     = #8059,
--              GL_RGBA12                       = #805A,
--              GL_RGBA16                       = #805B,

        GMEM_MOVEABLE = #2,
        GMEM_ZEROINIT = #40,
        GHND = or_all({GMEM_MOVEABLE, GMEM_ZEROINIT}),
        GWL_WNDPROC = -4,
        HALFTONE = 4,
        HWND_TOPMOST = -1,
        HWND_NOTOPMOST = -2,
        IDI_APPLICATION = 32512, -- icon signifying plain window
        IDC_ARROW = 32512,
        IDC_SIZEWE = 32644, -- Double-pointed arrow pointing west and east
        IMAGE_BITMAP = 0,
        LR_LOADFROMFILE = #10,
        LOGPIXELSY = 90,
--      MA_ACTIVATE = 1,
--      MA_ACTIVATEANDEAT = 2,
        MA_NOACTIVATE = 3,
--      MA_NOACTIVATEANDEAT = 4,
--      MAX_PATH = 260,
        MOD_ALT = 0x0001,
        MOD_CONTROL = 0x0002,
--      MONITOR_DEFAULTTONULL = 0x00000000, -- Returns NULL.
--      MONITOR_DEFAULTTOPRIMARY = 0x00000001, -- Returns a handle to the primary display monitor.
        MONITOR_DEFAULTTONEAREST = 0x00000002, -- Returns a handle to the display monitor that is nearest to the point.
--      OFN_PATHMUSTEXIST = #00000800,
--      OFN_FILEMUSTEXIST = #00001000,
--      OFN_ENABLEHOOK    = #00000020,
--      OFN_EXPLORER      = #00080000,
        OUT_DEFAULT_PRECIS = 0x00000000,
--      OUT_STRING_PRECIS  = 0x00000001,
--      OUT_STROKE_PRECIS  = 0x00000003,
--      OUT_TT_PRECIS      = 0x00000004,
--      OUT_DEVICE_PRECIS  = 0x00000005,
--      OUT_RASTER_PRECIS  = 0x00000006,
--      OUT_TT_ONLY_PRECIS = 0x00000007,
--      OUT_OUTLINE_PRECIS = 0x00000008,
--      OUT_SCREEN_OUTLINE_PRECIS = 0x00000009,
--      OUT_PS_ONLY_PRECIS = 0x0000000A,
        PS_COSMETIC = #00000000,
        PS_DOT = #00000002,
        PS_SOLID = #00000000,
        PS_USERSTYLE = #00000007,
        SM_CXSCREEN = 0,
        SM_CYSCREEN = 1,
        SPI_GETNONCLIENTMETRICS = #29,
        SRCCOPY = #CC0020,
        SS_CENTER = 1,
        STDERR = 2,
        SW_HIDE        = 0,
        SW_SHOWNORMAL  = 1,
        SW_SHOW_NORMAL = 1,
        SW_SHOW        = 5,
        SW_SHOWNA      = 8,
        SWP_NOSIZE     = #0001,
        SWP_NOMOVE     = #0002,
        SWP_NOZORDER   = #0004,
        SWP_NOACTIVATE = #0010,
        SWP_SHOWWINDOW = #0040,
--      SYSTEM_FONT = 13,
        TME_LEAVE = 0x00000002,
--set_unicode
        TOOLTIPS_CLASSA = `tooltips_class32`,
--      TOOLTIPS_CLASSW = allocate_wstring(`tooltips_class32`),
        TOOLTIPS_CLASS = TOOLTIPS_CLASSA,
--      TOOLTIPS_CLASS = TOOLTIPS_CLASSW,
        TRANSPARENT = 1,
        TTDT_INITIAL = 3,
        TTF_SUBCLASS = #10,
        TTF_TRACK    = #20,
--set_unicode
        TTM_ADDTOOLA = WM_USER + 4,
--      TTM_ADDTOOLW = WM_USER + 50,
        TTM_ADDTOOL = TTM_ADDTOOLA,
--      TTM_ADDTOOL = TTM_ADDTOOLW,
        TTM_NEWTOOLRECT = WM_USER + 52,
        TTM_SETDELAYTIME = WM_USER + 3,
        TTM_TRACKACTIVATE = WM_USER + 17,
        TTM_TRACKPOSITION = WM_USER + 18,
--set_unicode
        TTM_UPDATETIPTEXT = WM_USER + 12,
--      TTM_UPDATETIPTEXT = WM_USER + 57,
        TTS_ALWAYSTIP = #01,
        TTS_NOPREFIX  = #02,
        TTS_BALLOON   = #40,
        VK_RETURN = 13,
        VK_ESCAPE = #1B,
        VK_SPACE = #20, -- aka ' '
        VK_END = 35,
        VK_HOME = 36, -- #24
        VK_LEFT = 37,
--temp removed for gVidCut (which should no longer be including this file [/and/ theGUI.e]):
        VK_UP = 38,
        VK_RIGHT = 39,
        VK_DOWN = 40,
--</temp>
        VK_ADD = #6B, -- 107 ('k')??!!
        VK_SUBTRACT = #6D, -- 109 ('m')??!!
        VK_OEM_PLUS = #BB, -- 187 (same as VK_F11 in theGUI)
        VK_OEM_MINUS = #BD, -- 189
        WHITE_BRUSH = 0,
        WA_INACTIVE = 0,
        WM_CREATE = 1,
        WM_DESTROY = 2,
        WM_MOVE = 3,
        WM_SIZE = 5,
        WM_ACTIVATE = 6,
        WM_SETFOCUS = 7,
        WM_KILLFOCUS = 8,
        WM_PAINT = 15,
        WM_CLOSE = 16,
        WM_MOUSEACTIVATE = 33,
        WM_NCCREATE = 129,
        WM_NCCALCSIZE = 131,
        WM_NCACTIVATE = 134,
        WM_KEYDOWN = 256,
        WM_CHAR = 258,
        WM_COMMAND =273,
        WM_SYSCOMMAND = 274,
        WM_TIMER = 275,
        WM_MOUSEMOVE = 512,
        WM_LBUTTONDOWN = 513,
        WM_LBUTTONUP = 514,
        WM_RBUTTONDOWN = 516,
        WM_MOUSEWHEEL = 522,
        WM_MOUSEHWHEEL = 526,
        WM_SIZING = 532,
        WM_CAPTURECHANGED = 533,
        WM_MOVING = 534,
        WM_ENTERSIZEMOVE = 561,
        WM_DROPFILES = 563,
        WM_MOUSELEAVE = 675,
        WM_HOTKEY = 786,
        WM_CLIPBOARDUPDATE = 797,
        WS_CAPTION          = #00C00000,
        WS_CHILD            = #40000000,
        WS_CLIPCHILDREN     = #02000000,
        WS_EX_CLIENTEDGE    = #00000200,
        WS_EX_NOACTIVATE    = #08000000,
        WS_EX_TOPMOST       = #00000008,
        WS_OVERLAPPEDWINDOW = #00CF0000,    --= WS_BORDER+WS_DLGFRAME+WS_SYSMENU+WS_SIZEBOX+WS_MINIMISEBOX+WS_MAXIMISEBOX
        WS_POPUP            = #80000000,
        WS_SYSMENU          = #00080000,
        WS_VISIBLE          = #10000000,
        WS_VSCROLL          = #00200000,

        idPOINT = define_struct(`typedef struct tagPOINT {
                                   LONG x;
                                   LONG y;
                                } POINT, *PPOINT;`),
        idMSG = define_struct(`typedef struct tagMSG {
                                 HWND  hwnd;
                                 UINT  message;
                                 WPARAM wParam;
                                 LPARAM lParam;
                                 DWORD  time;
                                 POINT  pt;
                              } MSG, *PMSG, *LPMSG;`),
        idRECT = define_struct(`typedef struct _RECT {
                                  LONG left;
                                  LONG top;
                                  LONG right;
                                  LONG bottom;
                               } RECT, *PRECT;`),
        idSIZE = define_struct(`typedef struct tagSIZE {
                                  LONG cx;
                                  LONG cy;
                                } SIZE, *PSIZE;`),
        idTOOLINFO = define_struct(`typedef struct {
                                      UINT      cbSize;
                                      UINT      uFlags;
                                      HWND      hWnd;
                                      UINT_PTR  uId;
                                      RECT      rect;
                                      HINSTANCE hinst;
                                      LPTSTR    lpszText;
                                      LPARAM    lParam;
                                      void*     lpReserved;
                                    } TOOLINFO, *PTOOLINFO, *LPTOOLINFO;`),
        idTRACKMOUSEEVENT = define_struct(`typedef struct tagTRACKMOUSEEVENT {
                                             DWORD cbSize;
                                             DWORD dwFlags;
                                             HWND  hWndTrack;
                                             DWORD dwHoverTime;
                                           } TRACKMOUSEEVENT, *LPTRACKMOUSEEVENT;`),
        idLOGFONT = define_struct(`typedef struct {
                                     LONG   lfHeight;
                                     LONG   lfWidth;
                                     LONG   lfEscapement;
                                     LONG   lfOrientation;
                                     LONG   lfWeight;
                                     BYTE   lfItalic;
                                     BYTE   lfUnderline;
                                     BYTE   lfStrikeOut;
                                     BYTE   lfCharSet;
                                     BYTE   lfOutPrecision;
                                     BYTE   lfClipPrecision;
                                     BYTE   lfQuality;
                                     BYTE   lfPitchAndFamily;
                                     TCHAR  lfFaceName[LF_FACESIZE];
                                   } LOGFONT, *PLOGFONT;`),
--DEV not sure if above should be TCHAR...
--/*
typedef struct tagLOGFONTW {
  LONG  lfHeight;
  LONG  lfWidth;
  LONG  lfEscapement;
  LONG  lfOrientation;
  LONG  lfWeight;
  BYTE  lfItalic;
  BYTE  lfUnderline;
  BYTE  lfStrikeOut;
  BYTE  lfCharSet;
  BYTE  lfOutPrecision;
  BYTE  lfClipPrecision;
  BYTE  lfQuality;
  BYTE  lfPitchAndFamily;
  WCHAR lfFaceName[LF_FACESIZE];
} LOGFONTW, *PLOGFONTW, *NPLOGFONTW, *LPLOGFONTW;
--*/
--/!*
        idNONCLIENTMETRICS = define_struct(`#pragma pack(1)
                                            typedef struct tagNONCLIENTMETRICS {
                                              UINT    cbSize;
                                              int     iBorderWidth;
                                              int     iScrollWidth;
                                              int     iScrollHeight;
                                              int     iCaptionWidth;
                                              int     iCaptionHeight;
                                              LOGFONT lfCaptionFont;
                                              int     iSmCaptionWidth;
                                              int     iSmCaptionHeight;
                                              LOGFONT lfSmCaptionFont;
                                              int     iMenuWidth;
                                              int     iMenuHeight;
                                              LOGFONT lfMenuFont;
                                              LOGFONT lfStatusFont;
                                              LOGFONT lfMessageFont;
                                              int     iPaddedBorderWidth;
                                            } NONCLIENTMETRICS, *PNONCLIENTMETRICS, *LPNONCLIENTMETRICS;`),
--*!/
--/*
typedef struct tagNONCLIENTMETRICSW {
  UINT     cbSize;
  int      iBorderWidth;
  int      iScrollWidth;
  int      iScrollHeight;
  int      iCaptionWidth;
  int      iCaptionHeight;
  LOGFONTW lfCaptionFont;
  int      iSmCaptionWidth;
  int      iSmCaptionHeight;
  LOGFONTW lfSmCaptionFont;
  int      iMenuWidth;
  int      iMenuHeight;
  LOGFONTW lfMenuFont;
  LOGFONTW lfStatusFont;
  LOGFONTW lfMessageFont;
  int      iPaddedBorderWidth;
} NONCLIENTMETRICSW, *PNONCLIENTMETRICSW, *LPNONCLIENTMETRICSW;
--*/
        idBITMAP = define_struct(`typedef struct tagBITMAP {
                                    LONG    bmType;
                                    LONG    bmWidth;
                                    LONG    bmHeight;
                                    LONG    bmWidthBytes;
                                    WORD    bmPlanes;
                                    WORD    bmBitsPixel;
                                    LPVOID  bmBits;
                                 } BITMAP, *PBITMAP;`),
        idBITMAPFILEHEADER = define_struct(`#pragma pack(1)
                                            typedef struct tagBITMAPFILEHEADER {
                                              WORD  bfType;
                                              DWORD bfSize;
                                              WORD  bfReserved1;
                                              WORD  bfReserved2;
                                              DWORD bfOffBits;
                                            } BITMAPFILEHEADER, *PBITMAPFILEHEADER;`),
        idBITMAPINFOHEADER = define_struct(`typedef struct tagBITMAPINFOHEADER {
                                              DWORD biSize;
                                              LONG  biWidth;
                                              LONG  biHeight;
                                              WORD  biPlanes;
                                              WORD  biBitCount;
                                              DWORD biCompression;
                                              DWORD biSizeImage;
                                              LONG  biXPelsPerMeter;
                                              LONG  biYPelsPerMeter;
                                              DWORD biClrUsed;
                                              DWORD biClrImportant;
                                            } BITMAPINFOHEADER, *PBITMAPINFOHEADER;`),
        idRGBQUAD = define_struct(`typedef struct tagRGBQUAD {
                                     BYTE rgbBlue;
                                     BYTE rgbGreen;
                                     BYTE rgbRed;
                                     BYTE rgbReserved;
                                   } RGBQUAD;`),
        idBITMAPINFO = define_struct(`typedef struct tagBITMAPINFO {
                                        BITMAPINFOHEADER bmiHeader;
                                        RGBQUAD        bmiColors[1];
                                      } BITMAPINFO, *PBITMAPINFO;`),
        idCIEXYZ = define_struct(`typedef struct tagCIEXYZ {
                                    FXPT2DOT30 ciexyzX;
                                    FXPT2DOT30 ciexyzY;
                                    FXPT2DOT30 ciexyzZ;
                                 } CIEXYZ;`),
        idCIEXYZTRIPLE = define_struct(`typedef struct tagICEXYZTRIPLE {
                                          CIEXYZ ciexyzRed;
                                          CIEXYZ ciexyzGreen;
                                          CIEXYZ ciexyzBlue;
                                        } CIEXYZTRIPLE;`),
        idBITMAPV5HEADER = define_struct(`typedef struct {
                                            DWORD bV5Size;
                                            LONG bV5Width;
                                            LONG bV5Height;
                                            WORD bV5Planes;
                                            WORD bV5BitCount;
                                            DWORD bV5Compression;
                                            DWORD bV5SizeImage;
                                            LONG bV5XPelsPerMeter;
                                            LONG bV5YPelsPerMeter;
                                            DWORD bV5ClrUsed;
                                            DWORD bV5ClrImportant;
                                            DWORD bV5RedMask;
                                            DWORD bV5GreenMask;
                                            DWORD bV5BlueMask;
                                            DWORD bV5AlphaMask;
                                            DWORD bV5CSType;
                                            CIEXYZTRIPLE bV5Endpoints;
                                            DWORD bV5GammaRed;
                                            DWORD bV5GammaGreen;
                                            DWORD bV5GammaBlue;
                                            DWORD bV5Intent;
                                            DWORD bV5ProfileData;
                                            DWORD bV5ProfileSize;
                                            DWORD bV5Reserved;
                                          } BITMAPV5HEADER, *PBITMAPV5HEADER;`),
                                            
        idTEXTMETRIC = define_struct(`typedef struct tagTEXTMETRIC {
                                        LONG    tmHeight;
                                        LONG    tmAscent;
                                        LONG    tmDescent;
                                        LONG    tmInternalLeading;
                                        LONG    tmExternalLeading;
                                        LONG    tmAveCharWidth;
                                        LONG    tmMaxCharWidth;
                                        LONG    tmWeight;
                                        LONG    tmOverhang;
                                        LONG    tmDigitizedAspectX;
                                        LONG    tmDigitizedAspectY;
                                        TCHAR   tmFirstChar;
                                        TCHAR   tmLastChar;
                                        TCHAR   tmDefaultChar;
                                        TCHAR   tmBreakChar;
                                        BYTE    tmItalic;
                                        BYTE    tmUnderlined;
                                        BYTE    tmStruckOut;
                                        BYTE    tmPitchAndFamily;
                                        BYTE    tmCharSet;
                                      } TEXTMETRIC, *PTEXTMETRIC;`),
        pTEXTMETRIC = allocate_struct(idTEXTMETRIC,false),
        idMONITORINFO = define_struct(`typedef struct tagMONITORINFO {
                                         DWORD cbSize;
                                         RECT   rcMonitor;
                                         RECT   rcWork;
                                         DWORD dwFlags;
                                      } MONITORINFO, *LPMONITORINFO;`)

constant USER32 = open_dll(`user32.dll`),
         GDI32   = open_dll(`gdi32.dll`),
         KERNEL32 = open_dll(`kernel32.dll`),
         SHELL32 = open_dll(`shell32.dll`),
--       COMDLG32 = open_dll(`comdlg32.dll`),
         COMCTL32 = open_dll(`comctl32.dll`),
         MSIMG32  = open_dll(`msimg32.dll`),
         MB = machine_bits()

global atom
--          xAlphaBlend,
            xAddClipboardFormatListener,           
            xBeginPaint,
            xBitBlt,
            xClientToScreen,
            xCloseClipboard,
            xCloseHandle,
            xCreateBitmap,
            xCreateCompatibleBitmap,
            xCreateCompatibleDC,
            xCreateDIBSection,
            xCreateFont,
            xCreateFontIndirect,
            xCreateFile,
            xCreatePen,
            xCreateRectRgn,
            xCreateSolidBrush,
            xCreateWindowEx,
            xDefWindowProc,
            xDeleteDC,
            xDeleteObject,
            xDestroyWindow,
            xDispatchMessage,
            xDragAcceptFiles,
            xDragFinish,
            xDragQueryFile,
            xEmptyClipboard,
            xEnableWindow,
            xEndPaint,
            xEnumClipboardFormats,     
            xExtCreatePen,     
            xFillRect,
            xFrameRect,
            xGdiFlush,
            xGetClientRect,
            xGetClipboardData,
            xGetCursorPos,
            xGetDC,
            xGetDeviceCaps,
            xGetDIBits,        
            xGetLastError,
            xGetMessage,
            xGetMessagePos,
            xGetMonitorInfo,
--          xGetOpenFileName,
            xGetObject,
            xGetParent,
            xGetStockObject,
            xGetSystemMetrics,
            xGetTextExtentPoint32W,
            xGetTextMetrics,
            xGetUpdateRect,
            xGetWindowRect,
            xGlobalAlloc,
            xGlobalFree,
            xGlobalLock,
            xGlobalUnlock,
            xInitCommonControls,
            xInvalidateRect,
            xIsClipboardFormatAvailable,
            xIsWindow,
            xIsWindowVisible,
            xKillTimer,
            xLineTo,
            xLoadCursor,
            xLoadImage,
            xLoadIcon,
            xMessageBeep,
            xMonitorFromRect,          
            xMoveToEx,
            xMoveWindow,
            xOpenClipboard,
            xPostMessage,
            xPostQuitMessage,
--          xPtInRect,
            xRegisterClassEx,
            xRegisterHotKey,
            xReleaseCapture,
            xReleaseDC,
            xScreenToClient,
            xSendMessage,
            xSelectClipRgn,        
            xSelectObject,
            xSetActiveWindow,          
            xSetBkMode,
            xSetCapture,
            xSetClipboardData,
            xSetCursor,
            xSetFocus,
            xSetForegroundWindow,
            xSetParent,        
            xSetStretchBltMode,        
            xSetTextColor,
            xSetTimer,
            xSetWindowLong,
            xSetWindowPos,
            xSetWindowTextW,
            xShowWindow,
            xSleep,
            xStretchBlt,           
            xSystemParametersInfoA,
            xTextOut,
            xTrackMouseEvent,
            xTranslateMessage,
            xTransparentBlt,
            xUpdateWindow,
            xValidateRect,
            xWriteFile,
--          idBLENDFUNCTION,
            idLOGBRUSH,
            idPAINTSTRUCT,
            idWNDCLASSEX,
--          idOPENFILENAME,
--          pBLENDFUNCTION,
            pLOGBRUSH,
            pWNDCLASSEX
--          pOPENFILENAME,
--          pszFile

--      xAlphaBlend = define_c_func(MSIMG32,`AlphaBlend`,
--          {C_PTR,     --  _In_    HDC hdcDest,
--           C_INT,     --  _In_    int xoriginDest,
--           C_INT,     --  _In_    int yoriginDest,
--           C_INT,     --  _In_    int wDest,
--           C_INT,     --  _In_    int hDest,
--           C_PTR,     --  _In_    HDC hdcSrc,
--           C_INT,     --  _In_    int xoriginSrc,
--           C_INT,     --  _In_    int yoriginSrc,
--           C_INT,     --  _In_    int wSrc,
--           C_INT,     --  _In_    int hSrc,
--           C_INT},    --  _In_    BLENDFUNCTION ftn   -- (#00FF0000 or #00800000 or similar)
--          C_INT)      -- BOOL 

        xAddClipboardFormatListener = define_c_func(USER32,`AddClipboardFormatListener`,
            {C_PTR},    --  HWND hwnd
            C_BOOL)     -- BOOL
       
        xBeginPaint = define_c_func(USER32,`BeginPaint`,
            {C_PTR,     --  HWND  hwnd              // handle of window
             C_PTR},    --  LPPAINTSTRUCT  lpPaint  // address of structure for paint information
            C_PTR)      -- HDC

        xBitBlt = define_c_func(GDI32, `BitBlt`,
            {C_PTR,     --  HDC hdcDest
             C_INT,     --  int nXDest
             C_INT,     --  int nYDest
             C_INT,     --  int nWidth
             C_INT,     --  int nHeight
             C_PTR,     --  HDC hdcSrc
             C_INT,     --  int nXSrc
             C_INT,     --  int nYSrc
             C_LONG},   --  DWORD dwRop
            C_BOOL)     -- BOOL

        xClientToScreen = define_c_func(USER32,`ClientToScreen`,
            {C_PTR,     --  HWND hWnd
             C_PTR},    --  LPPOINT lpPoint
            C_BOOL)     -- BOOL

        xCloseClipboard = define_c_proc(USER32,`CloseClipboard`,
            {})         --  (void)
--          C_BOOL)     -- BOOL (0 on failure)

        xCloseHandle = define_c_func(KERNEL32, "CloseHandle",
            {C_PTR},    --  HANDLE  hObject // handle of object to close
            C_INT)      -- BOOL

        xCreateBitmap = define_c_func(GDI32,`CreateBitmap`,
            {C_INT,     --  int nWidth
             C_INT,     --  int nHeight
             C_UINT,    --  UINT cPlanes
             C_UINT,    --  UINT cBitsPerPel
             C_PTR},    --  const VOID *lpvBits
            C_PTR)      -- HBITMAP

        xCreateCompatibleBitmap = define_c_func(GDI32,`CreateCompatibleBitmap`,
            {C_PTR,     --  HDC hDC
             C_INT,     --  int nWidth
             C_INT},    --  int nHeight
            C_PTR)      -- HBITMAP

        xCreateCompatibleDC = define_c_func(GDI32,`CreateCompatibleDC`,
            {C_PTR},    --  HDC hDC // handle of memory device context
            C_PTR)      -- HDC

        xCreateDIBSection = define_c_func(GDI32,`CreateDIBSection`,
            {C_PTR,     --  HDC hDC
             C_PTR,     --  BITMAPINFO *pbmi
             C_UINT,    --  UINT iUsage
             C_PTR,     --  VOID **ppvBits
             C_PTR,     --  HANDLE hSection
             C_DWORD},  --  DWORD dwOffset
            C_PTR)      -- HBITMAP

--set_unicode
        xCreateFont = define_c_func(GDI32,`CreateFontA`,
            {C_INT,     --  int nHeight
             C_INT,     --  int nWidth
             C_INT,     --  int nEscapement
             C_INT,     --  int nOrientation
             C_INT,     --  int fnWeight
             C_DWORD,   --  DWORD fdwItalic
             C_DWORD,   --  DWORD fdwUnderline
             C_DWORD,   --  DWORD fdwStrikeOut
             C_DWORD,   --  DWORD fdwCharSet
             C_DWORD,   --  DWORD fdwOutputPrecision
             C_DWORD,   --  DWORD fdwClipPrecision
             C_DWORD,   --  DWORD fdwQuality
             C_DWORD,   --  DWORD fdwPitchAndFamily
             C_PTR},    --  LPCTSTR lpszFace
            C_LONG)     -- HFONT (handle of a logical font)

--set_unicode
        xCreateFontIndirect = define_c_func(GDI32,`CreateFontIndirectA`,
            {C_PTR},    --  CONST LOGFONT  *lplf        // address of logical font structure
            C_LONG)     -- HFONT (handle of a logical font)

--set_unicode
        xCreateFile = define_c_func(KERNEL32,`CreateFileA`,
            {C_PTR,     --  LPCTSTR  lpFileName,    // address of name of the file
             C_LONG,    --  DWORD  dwDesiredAccess, // access (read-write) mode
             C_LONG,    --  DWORD  dwShareMode, // share mode
             C_PTR,     --  LPSECURITY_ATTRIBUTES  lpSecurityAttributes,    // address of security descriptor
             C_LONG,    --  DWORD  dwCreationDistribution,  // how to create
             C_LONG,    --  DWORD  dwFlagsAndAttributes,    // file attributes
             C_PTR},    --  HANDLE  hTemplateFile   // handle of file with attributes to copy
            C_PTR)      -- HANDLE

        xCreatePen = define_c_func(GDI32,`CreatePen`,
            {C_INT,     --  int fnPenStyle
             C_INT,     --  int nWidth
             C_INT},    --  COLORREF crColor
            C_PTR)      -- HPEN handle to pen

        xCreateRectRgn = define_c_func(GDI32,`CreateRectRgn`,
            {C_INT,     --  int nLeftRect
             C_INT,     --  int nTopRect
             C_INT,     --  int nRightRect
             C_INT},    --  int nBottomRect
            C_PTR)      -- HRGN

        xCreateSolidBrush = define_c_func(GDI32, `CreateSolidBrush`,
            {C_UINT},   --  COLORREF  crColor   // brush color value
            C_PTR)      -- HBRUSH

        xCreateWindowEx = define_c_func(USER32,`CreateWindowExW`,
            {C_LONG,    --  DWORD  dwExStyle,   // extended window style
             C_PTR,     --  LPCTSTR  lpClassName,       // address of registered class name
             C_PTR,     --  LPCTSTR  lpWindowName,      // address of window name
             C_LONG,    --  DWORD  dwStyle,     // window style
             C_INT,     --  int  x,     // horizontal position of window
             C_INT,     --  int  y,     // vertical position of window
             C_INT,     --  int  nWidth,        // window width
             C_INT,     --  int  nHeight,       // window height
             C_PTR,     --  HWND  hWndParent,   // handle of parent or owner window
             C_PTR,     --  HMENU  hMenu,       // handle of menu or child-window identifier
             C_PTR,     --  HANDLE  hInstance,  // handle of application instance
             C_PTR},    --  LPVOID  lpParam     // address of window-creation data
            C_PTR)      -- HWND

        xDefWindowProc = define_c_func(USER32,`DefWindowProcW`,
            {C_PTR,     --  HWND  hWnd  // handle of window
             C_UINT,    --  UINT  Msg   // message identifier
             C_UINT,    --  WPARAM  wParam  // first message parameter
             C_UINT},   --  LPARAM  lParam  // second message parameter
            C_PTR)      -- LRESULT

        xDeleteDC = define_c_func(GDI32,`DeleteDC`,
            {C_PTR},    --  HDC hDC // handle of device context 
            C_BOOL)     -- BOOL

        xDeleteObject = define_c_proc(GDI32,`DeleteObject`,
            {C_PTR})    --  HGDIOBJ  hObject    // handle of graphic object
--          C_BOOL)     -- BOOL

        xDestroyWindow = define_c_func(USER32,`DestroyWindow`,
            {C_PTR},    --  HWND hWnd
            C_BOOL)     -- BOOL
           
        xDispatchMessage = define_c_proc(USER32,`DispatchMessageW`,
            {C_PTR})    --  CONST MSG  * lpmsg  // address of structure with message
--          C_LONG)     -- LONG (generally ignored)

        xDragAcceptFiles = define_c_proc(SHELL32,`DragAcceptFiles`,
            {C_PTR,     --  HWND  hWnd, // handle of the registering window
             C_INT})    --  BOOL  fAccept // acceptance option

        xDragFinish = define_c_proc(SHELL32,`DragFinish`,
            {C_PTR})    -- HDROP  hDrop // handle of memory to free

        xDragQueryFile = define_c_func(SHELL32,`DragQueryFile`,
            {C_PTR,     --  HDROP  hDrop, // handle of structure for dropped files
             C_INT,     --  UINT iFile, // index of file to query
             C_PTR,     --  LPTSTR lpszFile, // buffer for returned filename
             C_INT},    --  UINT  cch // size of buffer for filename
            C_INT)      -- UINT

        xEmptyClipboard = define_c_func(USER32,`EmptyClipboard`,
            {},         --  (void)
            C_BOOL)     -- BOOL

        xEnableWindow = define_c_proc(USER32,`EnableWindow`,
            {C_PTR,     --  HWND hWnd
             C_BOOL})   --  BOOL bEnable
--          C_BOOL)     -- BOOL (was enabled)

        xEndPaint = define_c_proc(USER32,`EndPaint`,
            {C_PTR,     --  HWND  hWnd                  // handle of window
             C_PTR})    -- CONST PAINTSTRUCT  *lpPaint  // address of structure for paint data
--          C_BOOL)     -- BOOL (function always returns true so linked as c_proc)

        xEnumClipboardFormats = define_c_func(USER32,`EnumClipboardFormats`,
            {C_INT},    --  UINT format
            C_INT)      -- UINT
           
        xExtCreatePen = define_c_func(GDI32,`ExtCreatePen`,
            {C_INT,     --  DWORD dwPenStyle
             C_INT,     --  DWORD dwWidth
             C_PTR,     --  const LOGBRUSH *lplb
             C_INT,     --  DWORD dwStyleCount
             C_PTR},    --  const DWORD *lpStyle
            C_PTR)      -- HPEN
           
        xFillRect = define_c_func(USER32,`FillRect`,
            {C_PTR,     --  HDC hDC
             C_PTR,     --  const RECT *lprc
             C_PTR},    --  HBRUSH hbr
            C_LONG)     -- int (0 on failure)

        xFrameRect  = define_c_func(USER32,`FrameRect`,
            {C_PTR,     --  HDC  hDC,   // handle of device context
             C_PTR,     --  CONST RECT  * lprc, // address of rectangle coordinates
             C_PTR},    --  HBRUSH  hbr     // handle of brush
            C_INT)      -- int

        xGdiFlush = define_c_func(GDI32,`GdiFlush`,
            {},
            C_INT)      -- BOOL

        xGetClientRect = define_c_func(USER32,`GetClientRect`,
            {C_PTR,     --  HWND hWnd
             C_PTR},    --  LPRECT lpRect
            C_BOOL)     -- BOOL

        xGetClipboardData = define_c_func(USER32,`GetClipboardData`,
            {C_UINT},   --  UINT uFormat
            C_PTR)      -- HANDLE

        xGetCursorPos = define_c_func(USER32,`GetCursorPos`,
            {C_PTR},    --  LPPOINT lpPoint
            C_BOOL)     -- BOOL

        xGetDC = define_c_func(USER32,`GetDC`,
            {C_PTR},    --  HWND  hWnd  // handle of window
            C_PTR)      -- HDC

        xGetDeviceCaps = define_c_func(GDI32,`GetDeviceCaps`,
            {C_PTR,     --  HDC hDC,    // device-context handle
             C_INT},    --  int nIndex // index of capability to query
            C_INT)      -- int

        xGetDIBits = define_c_func(GDI32,`GetDIBits`,
            {C_PTR,     --  HDC hDC
             C_PTR,     --  HBITMAP hbmp
             C_INT,     --  UINT uStartScan
             C_INT,     --  UINT cScanLines
             C_PTR,     --  LPVOID lpvBits
             C_PTR,     --  LPBITMAPINFO lpbi
             C_INT},    --  UINT uUsage
            C_INT)      -- int
           
        xGetLastError = define_c_func(KERNEL32,`GetLastError`,
            {},         -- (void)
            C_INT)      -- DWORD

        xGetMessage = define_c_func(USER32,`GetMessageW`,
            {C_PTR,     --  LPMSG  lpMsg    // address of structure with message
             C_PTR,     --  HWND  hWnd      // handle of window
             C_UINT,    --  UINT  wMsgFilterMin  // first message
             C_UINT},   --  UINT  wMsgFilterMax  // last message
            C_BOOL)     -- BOOL

        xGetMessagePos = define_c_func(USER32,`GetMessagePos`,
            {},
            C_UINT)     -- DWORD

--set_unicode
        xGetMonitorInfo = define_c_func(USER32,`GetMonitorInfoA`,
            {C_PTR,     --  HMONITOR hMonitor
             C_PTR},    --  LPMONITORINFO lpmi
             C_BOOL)    -- BOOL

--      xGetOpenFileName = define_c_func(COMDLG32,`GetOpenFileNameA`,
--          {C_PTR},    --  _Inout_  LPOPENFILENAME lpofn
--           C_BOOL)    -- BOOL

        xGetObject = define_c_func(GDI32,`GetObjectW`,
            {C_PTR,     --  HGDIOBJ  hgdiobj,   // handle to graphics object of interest
             C_LONG,    --  int  cbBuffer,      // size of buffer for object information 
             C_PTR},    --  LPVOID  lpvObject   // pointer to buffer for object information  
            C_INT)      -- int

        xGetParent = define_c_func(USER32,`GetParent`,
            {C_PTR},    --  HWND  hWnd  // handle of child window
            C_PTR)      -- HWND

        xGetStockObject = define_c_func(GDI32,`GetStockObject`,
            {C_INT},    --  int  fnObject   // type of stock object
            C_PTR)      -- HGDIOBJ GetStockObject(

        xGetSystemMetrics = define_c_func(USER32,`GetSystemMetrics`,
            {C_INT},    --  int nIndex
            C_INT)      -- int

        xGetTextExtentPoint32W = define_c_func(GDI32,`GetTextExtentPoint32W`,
            {C_PTR,     --  HDC hDC,    // handle of device context
             C_PTR,     --  LPCTSTR  lpString,  // address of text string
             C_INT,     --  int  cbString,  // number of characters in string
             C_PTR},    --  LPSIZE  lpSize  // address of structure for string size
            C_BOOL)     -- BOOL

        xGetTextMetrics = define_c_func(GDI32,`GetTextMetricsW`,
            {C_PTR,     --  HDC hDC,    // handle of device context
             C_PTR},    --  LPTEXTMETRIC lptm
            C_BOOL)     -- BOOL

        xGetUpdateRect = define_c_func(USER32,`GetUpdateRect`,
            {C_PTR,     --  _In_  HWND hWnd
             C_PTR,     --  _Out_ LPRECT lpRect
             C_BOOL},   --  _In_  BOOL bErase
            C_BOOL)     -- BOOL

        xGetWindowRect = define_c_func(USER32,`GetWindowRect`,
            {C_PTR,     --  HWND hWnd
             C_PTR},    --  LPRECT lpRect
            C_BOOL)     -- BOOL

        xGlobalAlloc = define_c_func(KERNEL32,`GlobalAlloc`,
            {C_UINT,    --  UINT uFlags
             C_UINT},   --  SIZE_T dwBytes
            C_PTR)      -- HGLOBAL

        xGlobalFree = define_c_func(KERNEL32,`GlobalFree`,
            {C_PTR},    --  HGLOBAL hMem
            C_PTR)      -- HGLOBAL (null on success)

        xGlobalLock = define_c_func(KERNEL32,`GlobalLock`,
            {C_PTR},    --  HGLOBAL hMem
            C_PTR)      -- LPVOID

        xGlobalUnlock = define_c_proc(KERNEL32,`GlobalUnlock`,
            {C_PTR})    --  HGLOBAL hMem
--          C_BOOL)     -- BOOL (non-0: success but still locked, 0: check success/failure via GetLastError)

        xInitCommonControls = define_c_proc(COMCTL32,`InitCommonControls`,{})

        xInvalidateRect = define_c_func(USER32,`InvalidateRect`,
            {C_PTR,     --  HWND hWnd
             C_PTR,     --  const RECT *lpRect
             C_BOOL},   --  BOOL bErase
            C_BOOL)     -- BOOL

        xIsClipboardFormatAvailable = define_c_func(USER32,`IsClipboardFormatAvailable`,
            {C_UINT},   --  UINT format
            C_BOOL)     -- BOOL

        xIsWindow = define_c_func(USER32,`IsWindow`,
            {C_PTR},    --  HWND  hWnd         // handle of window
            C_INT)      -- BOOL

        xIsWindowVisible = define_c_func(USER32,`IsWindowVisible`,
            {C_PTR},    --  HWND hWnd
            C_BOOL)     -- BOOL WINAPI

        xKillTimer = define_c_func(USER32,`KillTimer`,
            {C_PTR,     --  HWND hWnd (NULL here)
             C_UINT},   --  UINT_PTR uIDEvent
            C_BOOL)     -- BOOL

        xLineTo = define_c_func(GDI32,`LineTo`,
            {C_PTR,     --  HDC hDC,    // device context handle
             C_INT,     --  int nXEnd, // x-coordinate of line's ending point
             C_INT},    --  int nYEnd   // y-coordinate of line's ending point
            C_BOOL)     -- BOOL

        xLoadCursor = define_c_func(USER32,`LoadCursorW`,
            {C_PTR,     --  HINSTANCE hInstance
             C_PTR},    --  LPCTSTR lpCursorName
            C_PTR)      -- HCURSOR

--set_unicode
        xLoadImage = define_c_func(USER32,`LoadImageA`,
            {C_PTR,     --  HINSTANCE hInstance
             C_PTR,     --  LPCTSTR lpszName
             C_UINT,    --  UINT uType
             C_INT,     --  int cxDesired
             C_INT,     --  int cyDesired
             C_UINT},   --  UINT fuLoad
            C_PTR)      -- HANDLE

        xLoadIcon = define_c_func(USER32,`LoadIconW`,
            {C_PTR,     --  HINSTANCE hInstance
             C_PTR},    --  LPCTSTR lpIconName
            C_PTR)      -- HICON

        xMessageBeep = define_c_func(USER32,`MessageBeep`,
            {C_UINT},   --  UINT uType
            C_BOOL)     -- BOOL
           
        xMonitorFromRect = define_c_func(USER32,`MonitorFromRect`,
            {C_PTR,     --  LPCRECT lprc
             C_INT},    --  DWORD dwFlags
            C_PTR)      -- HMONITOR
           
        xMoveToEx = define_c_func(GDI32,`MoveToEx`,
            {C_PTR,     --  HDC hDC,    // handle of device context
             C_INT,     --  int X, // x-coordinate of new current position
             C_INT,     --  int Y, // y-coordinate of new current position
             C_PTR},    --  LPPOINT lpPoint // address of old current position
            C_BOOL)     -- BOOL

        xMoveWindow = define_c_proc(USER32,`MoveWindow`,
            {C_PTR,     --  HWND hWnd
             C_INT,     --  int X
             C_INT,     --  int Y
             C_INT,     --  int nWidth
             C_INT,     --  int nHeight
             C_BOOL})   --  BOOL bRepaint
--          C_BOOL)     -- BOOL

        xOpenClipboard = define_c_func(USER32,`OpenClipboard`,
            {C_PTR},    --  HWND hWndNewOwner
            C_BOOL)     -- BOOL

        xPostMessage = define_c_func(USER32,`PostMessageW`,
            {C_PTR,     --  _In_opt_  HWND hWnd
             C_INT,     --  _In_      UINT Msg
             C_PTR,     --  _In_      WPARAM wParam
             C_PTR},    --  _In_      LPARAM lParam
            C_INT)      -- BOOL

        xPostQuitMessage = define_c_proc(USER32,`PostQuitMessage`,
            {C_INT})    --  int  nExitCode      // exit code

--      xPtInRect = define_c_func(USER32,`PtInRect`,
--          {C_PTR,     --  _In_  const RECT *lprc
--           C_PTR},    --  _In_  POINT pt
----            C_BOOL)     -- BOOL
--          C_INT)      -- BOOL

        xRegisterClassEx = define_c_func(USER32,`RegisterClassExW`,
            {C_PTR},    --  CONST WNDCLASSEX FAR *lpwcx // address of structure with class data
            C_PTR)      -- ATOM

        xRegisterHotKey = define_c_func(USER32,"RegisterHotKey",
            {C_PTR,     --  HWND  hwnd, // window to receive hot-key notification
             C_INT,     --  int  idHotKey,      // identifier of hot key
             C_INT,     --  UINT  fuModifiers,  // key-modifier flags
             C_INT},    --  UINT  uVirtKey         // virtual-key code
            C_INT)      -- BOOL

        xReleaseCapture = define_c_proc(USER32,`ReleaseCapture`,
            {})         --  (void)
--          C_BOOL)     -- BOOL (ignored)

        xReleaseDC = define_c_func(USER32,`ReleaseDC`,
            {C_PTR,     --  HWND hwnd, // handle of window
             C_PTR},    --  HDC hDC // handle of device context
            C_BOOL)     -- BOOL

        xScreenToClient = define_c_func(USER32,`ScreenToClient`,
            {C_PTR,     --  HWND hWnd
             C_PTR},    --  LPPOINT lpPoint
            C_BOOL)     -- BOOL

        xSendMessage = define_c_func(USER32,`SendMessageW`,
            {C_PTR,     --  HWND  hwnd  // handle of destination window
             C_UINT,    --  UINT  uMsg  // message to send
             C_UINT,    --  WPARAM  wParam  // first message parameter
             C_UINT},   --  LPARAM  lParam  // second message parameter
            C_LONG)     -- LRESULT

        xSelectClipRgn = define_c_func(GDI32,`SelectClipRgn`,
            {C_PTR,     --  HDC hDC,    // handle of device context
             C_PTR},    --  HRGN hrgn
            C_PTR)      -- int complexity

        xSelectObject = define_c_func(GDI32,`SelectObject`,
            {C_PTR,     --  HDC hDC,    // handle of device context
             C_PTR},    --  HGDIOBJ  hgdiobj    // handle of object
            C_PTR)      -- HGDIOBJ

        xSetActiveWindow = define_c_func(USER32,`SetActiveWindow`,
            {C_PTR},    --  HWND hWnd
            C_PTR)      -- HWND
           
        xSetBkMode = define_c_func(GDI32,`SetBkMode`,
            {C_PTR,     --  HDC hDC,    // handle of device context
             C_INT},    --  int iBkMode // flag specifying background mode
            C_INT)      -- int

        xSetCapture = define_c_proc(USER32,`SetCapture`,
            {C_PTR})    --  HWND hWnd
--          C_PTR)      -- HWND (ignored)

        xSetClipboardData = define_c_func(USER32,`SetClipboardData`,
            {C_UINT,    --  UINT uFormat
             C_PTR},    --  HANDLE hMem
            C_INT)      -- HANDLE (NULL means failure)

        xSetCursor = define_c_proc(USER32,`SetCursor`,
            {C_PTR})    --  HCURSOR hCursor
--          C_PTR)      -- HCURSOR (ignored)

        xSetFocus = define_c_func(USER32,`SetFocus`,
            {C_PTR},    --  HWND hWnd
            C_PTR)      -- HWND (previous focus)

        xSetForegroundWindow = define_c_proc(USER32,`SetForegroundWindow`,
            {C_PTR})    --  HWND  hwnd  // handle of window to bring to foreground

        xSetParent = define_c_func(USER32,`SetParent`,
            {C_PTR,     --  HWND hWndChild
             C_PTR},    --  HWND hWndNewParent
            C_PTR)      -- HWND (previous)
           
        xSetStretchBltMode = define_c_func(GDI32, `SetStretchBltMode`,
            {C_PTR,     --  HDC hDC
             C_INT},    --  int iStretchMode
            C_INT)      -- int
           
        xSetTextColor = define_c_func(GDI32,`SetTextColor`,
            {C_PTR,     --  HDC hDC, // handle of device context
             C_PTR},    --  COLORREF crColor // text color
            C_PTR)      -- COLORREF

        xSetTimer = define_c_func(USER32,`SetTimer`,
            {C_PTR,     --  HWND hWnd (NULL here)
             C_UINT,    --  UINT_PTR nIDEvent (0 here)
             C_UINT,    --  UINT uElapse
             C_PTR},    --  TIMERPROC lpTimerFunc
            C_PTR)      -- UINT_PTR

--set_unicode
        xSetWindowLong = define_c_func(USER32,iff(MB=32?`SetWindowLongA`
                                                       :`SetWindowLongPtrA`),
            {C_PTR,     --  HWND  hWnd              // handle of window
             C_UINT,    --  int  nIndex             // offset of value to store
             C_LONG},   --  LONG/LONG_PTR dwNewLong // value to store
            C_LONG)     -- LONG/LONG_PTR            // previous value

        xSetWindowPos = define_c_func(USER32,`SetWindowPos`,
            {C_PTR,     --  HWND hWnd               // handle of window
             C_PTR,     --  HWND hWndInsertAfter    // placement-order handle
             C_INT,     --  int x       // horizontal position
             C_INT,     --  int y       // vertical position
             C_INT,     --  int w       // width
             C_INT,     --  int h       // height
             C_UINT},   --  UINT uFlags // window-positioning flags (SWP_xxx)
            C_BOOL)     -- BOOL

--set_unicode
        xSetWindowTextW = define_c_proc(USER32,`SetWindowTextW`,
            {C_PTR,     --  HWND hWnd
             C_PTR})    --  LPCTSTR lpString

        xShowWindow = define_c_proc(USER32,`ShowWindow`,
            {C_PTR,     --  HWND  hWnd, // handle of window
             C_INT})    --  int  nCmdShow   // show state of window

        xSleep = define_c_proc(KERNEL32,"Sleep",
            {C_INT})    -- DWORD  cMilliseconds     // sleep time in milliseconds 

        xStretchBlt = define_c_func(GDI32,`StretchBlt`,
            {C_PTR,     --  HDC hdcDest
             C_INT,     --  int nXOriginDest
             C_INT,     --  int nYOriginDest
             C_INT,     --  int nWidthDest
             C_INT,     --  int nHeightDest
             C_PTR,     --  HDC hdcSrc
             C_INT,     --  int nXOriginSrc
             C_INT,     --  int nYOriginSrc
             C_INT,     --  int nWidthSrc
             C_INT,     --  int nHeightSrc
             C_LONG},   --  DWORD dwRop
            C_INT)      -- BOOL
           
--set_unicode
        xSystemParametersInfoA = define_c_func(USER32,`SystemParametersInfoA`,
            {C_UINT,    --  UINT  uiAction
             C_UINT,    --  UINT  uiParam,
             C_PTR,     --  PVOID pvParam,
             C_UINT},   --  UINT  fWinIni
            C_INT)      -- BOOL

        xTextOut = define_c_func(GDI32,`TextOutW`,
            {C_PTR,     --  HDC hDC,    // handle of device context
             C_INT,     --  int nXStart,        // x-coordinate of starting position
             C_INT,     --  int nYStart,        // y-coordinate of starting position
             C_PTR,     --  LPCTSTR lpString,   // address of string
             C_INT},    --  int cbString        // number of characters in string
            C_BOOL)     -- BOOL success

        xTrackMouseEvent = define_c_func(USER32,`TrackMouseEvent`,
            {C_PTR},    --  LPTRACKMOUSEEVENT lpEventTrack
            C_BOOL)     -- BOOL

        xTranslateMessage = define_c_proc(USER32,`TranslateMessage`,
            {C_PTR})    --  CONST MSG  *lpmsg   // address of structure with message
--          C_BOOL)     -- BOOL (true if was translated...)

        xTransparentBlt = define_c_func(MSIMG32,`TransparentBlt`,
            {C_PTR,     --  HDC hdcDest
             C_INT,     --  int xoriginDest
             C_INT,     --  int yoriginDest
             C_INT,     --  int wDest
             C_INT,     --  int hDest
             C_PTR,     --  HDC hdcSrc
             C_INT,     --  int xoriginSrc
             C_INT,     --  int yoriginSrc
             C_INT,     --  int wSrc
             C_INT,     --  int hSrc
             C_UINT},   --  UINT crTransparent
            C_BOOL)     -- BOOL

        xUpdateWindow = define_c_proc(USER32,`UpdateWindow`,
            {C_PTR})    --  HWND hWnd
--          C_BOOL)     -- BOOL

        xValidateRect = define_c_func(USER32,`ValidateRect`,
            {C_PTR,     --  HWND hWnd
             C_PTR},    --  const RECT* lpRect (null==all here)
            C_BOOL)     -- BOOL

        xWriteFile = define_c_func(KERNEL32,`WriteFile`,
            {C_PTR,     --  HANDLE hFile
             C_PTR,     --  LPCVOID lpBuffer
             C_INT,     --  DWORD nNumberOfBytesToWrite
             C_PTR,     --  LPDWORD lpNumberOfBytesWritten
             C_PTR},    --  LPOVERLAPPED lpOverlapped
            C_BOOL)     -- BOOL

--      idBLENDFUNCTION = define_struct(`typedef struct _BLENDFUNCTION {
--                                        BYTE BlendOp;
--                                        BYTE BlendFlags;
--                                        BYTE SourceConstantAlpha;
--                                        BYTE AlphaFormat;
--                                      } BLENDFUNCTION, *PBLENDFUNCTION, *LPBLENDFUNCTION;`)
           
        idLOGBRUSH = define_struct(`typedef struct tagLOGBRUSH {
                                      UINT      lbStyle;
                                      COLORREF  lbColor;
                                      ULONG_PTR lbHatch;
                                    } LOGBRUSH, *PLOGBRUSH;`)
           
        idPAINTSTRUCT = define_struct(`typedef struct tagPAINTSTRUCT {
                                         HDC hDC;
                                         BOOL fErase;
                                         RECT rcPaint;
                                         BOOL fRestore;
                                         BOOL fIncUpdate;
                                         BYTE rgbReserved[32];
                                       } PAINTSTRUCT, *PPAINTSTRUCT;`)

        idWNDCLASSEX = define_struct(`typedef struct tagWNDCLASSEX {
                                        UINT    cbSize;
                                        UINT    style;
                                        WNDPROC lpfnWndProc;
                                        int     cbClsExtra;
                                        int     cbWndExtra;
                                        HINSTANCE hInstance;
                                        HICON   hIcon;
                                        HCURSOR hCursor;
                                        HBRUSH  hbrBackground;
                                        LPCTSTR lpszMenuName;
                                        LPCTSTR lpszClassName;
                                        HICON   hIconSm;
                                      } WNDCLASSEX, *PWNDCLASSEX;`)

--      idOPENFILENAME = define_struct(`typedef struct tagOFN {
--                                        DWORD         lStructSize;
--                                        HWND          hwndOwner;
--                                        HINSTANCE     hInstance;
--                                        LPCTSTR       lpstrFilter;
--                                        LPTSTR        lpstrCustomFilter;
--                                        DWORD         nMaxCustFilter;
--                                        DWORD         nFilterIndex;
--                                        LPTSTR        lpstrFile;
--                                        DWORD         nMaxFile;
--                                        LPTSTR        lpstrFileTitle;
--                                        DWORD         nMaxFileTitle;
--                                        LPCTSTR       lpstrInitialDir;
--                                        LPCTSTR       lpstrTitle;
--                                        DWORD         Flags;
--                                        WORD          nFileOffset;
--                                        WORD          nFileExtension;
--                                        LPCTSTR       lpstrDefExt;
--                                        LPARAM        lCustData;
--                                        LPOFNHOOKPROC lpfnHook;
--                                        LPCTSTR       lpTemplateName;
--                                        void          *pvReserved;
--                                        DWORD         dwReserved;
--                                        DWORD         FlagsEx;
--                                      } OPENFILENAME, *LPOPENFILENAME;`)
----                    (nb: #if (_WIN32_WINNT >= 0x0500) above pvReserved removed)

--      pBLENDFUNCTION = allocate_struct(idBLENDFUNCTION)
        pLOGBRUSH = allocate_struct(idLOGBRUSH)
        pWNDCLASSEX = allocate_struct(idWNDCLASSEX)
--      pOPENFILENAME = allocate_struct(idOPENFILENAME)
--      pszFile = allocate(MAX_PATH*2,1)

global constant
        pPAINTSTRUCT = allocate_struct(idPAINTSTRUCT,false),
        pPOINT = allocate_struct(idPOINT,false),
        pRECT = allocate_struct(idRECT,false),
        pSIZE = allocate_struct(idSIZE,false),
        pTOOLINFO = allocate_struct(idTOOLINFO,false),
        pTRACKMOUSEEVENT = allocate_struct(idTRACKMOUSEEVENT,false),
        pNONCLIENTMETRICS = allocate_struct(idNONCLIENTMETRICS,false),
--      pMENUFONT = pNONCLIENTMETRICS+get_field_details(idNONCLIENTMETRICS,`lfMenuFont.lfHeight`)[1],
--      pMENUFONT = get_struct_field_addr(idNONCLIENTMETRICS,pNONCLIENTMETRICS,`lfMenuFont.lfHeight`),
        pBITMAP = allocate_struct(idBITMAP,false),
        pBITMAPINFOHEADER = allocate_struct(idBITMAPINFOHEADER,false),
--      pBITMAPINFO = allocate_struct(idBITMAPINFO,false)
        pBITMAPINFO = allocate(get_struct_size(idBITMAPINFO)+4)
--?{pMENUFONT,qMENUFONT}

set_struct_field(idTRACKMOUSEEVENT,pTRACKMOUSEEVENT,`cbSize`,get_struct_size(idTRACKMOUSEEVENT))
set_struct_field(idNONCLIENTMETRICS,pNONCLIENTMETRICS,`cbSize`,get_struct_size(idNONCLIENTMETRICS))
set_struct_field(idTOOLINFO,pTOOLINFO,`cbSize`,get_struct_size(idTOOLINFO))

global type HWND(atom /*h*/)
    return true
end type

global type HANDLE(atom /*h*/)
    return true
end type


local function get_raw_string_ptr(string s)
--
-- Returns a raw string pointer for s, somewhat like allocate_string(s) but using the existing memory.
-- NOTE: The return is only valid as long as the value passed as the parameter remains in existence.
--       In particular, callbacks must make a semi-permanent copy somewhere other than locals/temps.
--       (one example in theGUI where that /still/ applies would be in setting say tvItem.pszText)
--
    atom res
    #ilASM{
        [32]
            mov eax,[s]
            lea edi,[res]
            shl eax,2
        [64]
            mov rax,[s]
            lea rdi,[res]
            shl rax,2
        []
            call :%pStoreMint
          }
    return res
end function

global function LOWORD(atom dWord)
    integer res = and_bits(dWord, #FFFF)
    if and_bits(res,#8000) then res -= #10000 end if
    return res
end function

global function HIWORD(atom dWord)
    integer res = floor(dWord/#10000)
    if and_bits(res,#8000) then res -= #10000 end if
    return res
end function

--Aside: methinks one of these is wrong! (but if it works...)
global function RGB(atom red, green, blue)
    atom colour = and_bits(red,  #FF)*#10000 + 
                  and_bits(green,#FF)*#100 + 
                  and_bits(blue, #FF)
    return colour
end function

global function Color(atom alpha, red, green, blue)
    return and_bits(red,  #FF) + 
           and_bits(green,#FF) * #100 + 
           and_bits(blue, #FF) * #10000 +
           and_bits(alpha,#FF) * #1000000
end function

global function GetLastError()
    atom res = c_func(xGetLastError,{})
    return res
end function

-- couldn't get this to work:
--global procedure AlphaBlend(atom destDC, integer x,y,w,h, atom srcDC, integer sx,sy,sw,sh, object blend_fn)
--  if sequence(blend_fn) then
--      -- eg {AC_SRC_OVER,0,255,AC_SRC_ALPHA}
--      poke(pBLENDFUNCTION,blend_fn)
--      blend_fn = peek4u(pBLENDFUNCTION)
--  end if
--  bool res = c_func(xAlphaBlend,{destDC, x,y,w,h, srcDC, sx,sy,sw,sh, blend_fn})
--  if not res then
----ERROR_INVALID_PARAMETER = 87
--      ?{`AlphaBlend`,res,GetLastError(),{destDC, x,y,w,h, srcDC, sx,sy,sw,sh, blend_fn}}
--  end if
----    assert(res)
--end procedure

global procedure AddClipboardFormatListener(atom hWnd)
    bool res = c_func(xAddClipboardFormatListener,{hWnd})
    assert(res)
end procedure

global function BeginPaint(atom hWnd, pRECT)
    atom hDC = c_func(xBeginPaint,{hWnd,pRECT})
    return hDC
end function

global procedure BitBlt(atom hdcDest, nXDest, nYDest, nWidth, nHeight, hdcSrc, nXSrc, nYSrc, dwRop)
    integer res = c_func(xBitBlt,{hdcDest, nXDest, nYDest, nWidth, nHeight, hdcSrc, nXSrc, nYSrc, dwRop})
    assert(res!=0)
end procedure

global procedure ClientToScreen(atom hWnd, pPoint)
    bool res = c_func(xClientToScreen,{hWnd,pPoint})
    assert(res!=0)
end procedure

global procedure CloseClipboard()
    c_proc(xCloseClipboard,{})
end procedure

global procedure CloseHandle(atom hObject)
    bool res = c_func(xCloseHandle,{hObject})
    assert(res)
end procedure

global function CreateBitmap(integer w, h, planes, bpp, atom pPixels)
    atom hBitmap = c_func(xCreateBitmap,{w,h,planes,bpp,pPixels})
    return hBitmap
end function

global function CreateCompatibleBitmap(atom hDC, integer w, h)
    atom hBitmap = c_func(xCreateCompatibleBitmap,{hDC,w,h})
    assert(hBitmap!=NULL)
    return hBitmap
end function

local procedure crashee(string msg)
    integer e = GetLastError()
    crash(`%s failed (%d [%08x])`,{msg,e,e})
end procedure

global function CreateCompatibleDC(atom hDC)
    atom res = c_func(xCreateCompatibleDC,{hDC})
    if res=NULL then crashee(`CreateCompatibleDC`) end if
    return res
end function

global function CreateDIBSection(atom hDC, pBMI, integer usage, atom pBits, hSection, integer offset)
    atom hBitmap = c_func(xCreateDIBSection,{hDC, pBMI, usage, pBits, hSection, offset})
    if hBitmap=NULL then crashee(`CreateDIBSection`) end if
    return hBitmap
end function

global function CreateFont(integer nHeight, nWidth, nEscapement, nOrientation, fnWeight,
                                   fdwItalic, fdwUnderline, fdwStrikeOut, fdwCharSet,
                                   fdwOutputPrecision, fdwClipPrecision, fdwQuality,
                                   fdwPitchAndFamily, string lpszFace)
    atom hFont = c_func(xCreateFont,{nHeight,nWidth,nEscapement,nOrientation,fnWeight,
                                     fdwItalic,fdwUnderline,fdwStrikeOut,fdwCharSet,
                                     fdwOutputPrecision,fdwClipPrecision, fdwQuality,
                                     fdwPitchAndFamily,lpszFace})
    assert(hFont!=NULL)
    return hFont
end function

global function CreateFontIndirect(atom lplf)
    return c_func(xCreateFontIndirect,{lplf})
end function

global function CreateFile(string lpFileName, atom dwDesiredAccess, dwShareMode, lpSecurityAttributes,  dwCreationDistribution, dwFlagsAndAttributes, hTemplateFile)
    atom handle = c_func(xCreateFile,{lpFileName,dwDesiredAccess,dwShareMode,lpSecurityAttributes,dwCreationDistribution,dwFlagsAndAttributes,hTemplateFile})
    return handle
end function

global function CreatePen(atom fnPenStyle, nWidth, crColor)
    atom pen = c_func(xCreatePen,{fnPenStyle, nWidth, crColor})
    return pen
end function

global function CreateRectRgn(atom l, t, r, b)
    atom hClip = c_func(xCreateRectRgn,{l,t,r,b})
    assert(hClip!=NULL)
    return hClip
end function

global function CreateSolidBrush(atom colour)
    atom brush = c_func(xCreateSolidBrush,{colour})
    return brush
end function

global function CreateWindowEx(atom dwExStyle, string classname, object lbl, atom dwStyle, x, y, w, h, pHwnd, menu, inst, lParam)

    atom pClassName = allocate_wstring(classname),
         pLblString = allocate_wstring(lbl)

    sequence cwp = {dwExStyle,      -- extended style
                    pClassName,     -- window class name
                    pLblString,     -- window caption or Button text etc..
                    dwStyle,        -- window style
                    x,              -- initial x position
                    y,              -- initial y position
                    w,              -- initial x size
                    h,              -- initial y size
                    pHwnd,          -- parent window handle
                    menu,           -- window menu handle OR user id for child windows
                    inst,           -- program instance handle - Legacy of Win16 apps. 0 will work too.
                    lParam}         -- creation parameters
    atom hWnd = c_func(xCreateWindowEx, cwp)
    if hWnd=NULL then crashee(`CreateWindowEx`) end if
    free({pClassName,pLblString})
    return hWnd
end function

global function DefWindowProc(atom hWnd, Msg, wParam, lParam)
    atom res = c_func(xDefWindowProc,{hWnd, Msg, wParam, lParam})
    return res
end function

global procedure DeleteDC(atom hDC)
    integer res = c_func(xDeleteDC,{hDC})
    assert(res!=0)
end procedure

global procedure DeleteObject(atom hObject)
    c_proc(xDeleteObject,{hObject})
end procedure

global procedure DestroyWindow(atom hWnd)
    integer res = c_func(xDestroyWindow,{hWnd})
    if res=0 then crash(`DestroyWindow failed`) end if
end procedure

global procedure DispatchMessage(atom lpmsg)
    c_proc(xDispatchMessage,{lpmsg})
end procedure

global procedure DragAcceptFiles(atom hWnd, bool bAccept)
    c_proc(xDragAcceptFiles,{hWnd,bAccept})
end procedure

global procedure DragFinish(atom hDrop)
    c_proc(xDragFinish,{hDrop})
end procedure

global function DragQueryFile(atom hDrop, integer iFile, atom lpszFile, integer len)
    integer res = c_func(xDragQueryFile,{hDrop,iFile,lpszFile,len})
    return res
end function

global procedure EmptyClipboard()
    integer res = c_func(xEmptyClipboard,{})
    assert(res!=0)
end procedure

global procedure EnableWindow(atom hWnd, bool enable)
    c_proc(xEnableWindow,{hWnd, enable})
end procedure

global procedure EndPaint(atom hWnd, pPAINTSTRUCT)
    c_proc(xEndPaint,{hWnd, pPAINTSTRUCT})
end procedure

global function EnumClipboardFormats(integer uFormat)
    integer res = c_func(xEnumClipboardFormats,{uFormat})
    return res
end function

global function ExtCreatePen(atom dwWidth, colour, sequence style)
    -- NB: PS_COSMETIC, PS_SOLID brush, PS_USERSTYLE currently fixed here...
    set_struct_field(idLOGBRUSH,pLOGBRUSH,`lbStyle`,PS_SOLID)
    set_struct_field(idLOGBRUSH,pLOGBRUSH,`lbColor`,colour)
    integer l = length(style)
    atom pStyle = allocate(4*l)
    poke4(pStyle,style)
    atom hPen = c_func(xExtCreatePen,{PS_COSMETIC+PS_USERSTYLE,dwWidth,pLOGBRUSH,l,pStyle})
    free(pStyle)
    return hPen
end function

global procedure FillRect(atom hDC, object rect, atom hBrush)
    if sequence(rect) then
        atom {l,t,r,b} = rect
        set_struct_field(idRECT,pRECT,`left`,l)
        set_struct_field(idRECT,pRECT,`top`,t)
        set_struct_field(idRECT,pRECT,`right`,r)
        set_struct_field(idRECT,pRECT,`bottom`,b)
        rect = pRECT
    end if
    atom res = c_func(xFillRect,{hDC,rect,hBrush})
    assert(res!=0)
end procedure

global procedure FrameRect(atom hDC, pRect, hBrush)
    integer res = c_func(xFrameRect,{hDC, pRect, hBrush})
    assert(res!=0)
end procedure

global procedure GdiFlush()
    bool res = c_func(xGdiFlush,{})
    assert(res!=0)
end procedure

global function GetClientRect(atom hWnd)
    bool res = c_func(xGetClientRect,{hWnd,pRECT})
    assert(res)
    integer l = get_struct_field(idRECT,pRECT,`left`),
            t = get_struct_field(idRECT,pRECT,`top`),
            r = get_struct_field(idRECT,pRECT,`right`),
            b = get_struct_field(idRECT,pRECT,`bottom`)
    return {l,t,r,b}
end function

global function GetClipboardData(integer uFormat)
    atom handle = c_func(xGetClipboardData,{uFormat})
    return handle
end function

global procedure GetCursorPos(atom pPOINT)
    bool bOK = c_func(xGetCursorPos,{pPOINT})
    assert(bOK)
end procedure

global function GetDC(atom hWnd)
    atom hDC = c_func(xGetDC,{hWnd})
    assert(hDC!=NULL)
    return hDC
end function

global function GetDeviceCaps(atom hDC, integer nIndex)
    integer res = c_func(xGetDeviceCaps,{hDC,nIndex})
    return res
end function

global procedure GetDIBits(atom hDC, hBmp, integer uStartScan, cScanLines, atom lpvBits, lpbi, integer uUsage)
    integer res = c_func(xGetDIBits,{hDC, hBmp, uStartScan, cScanLines, lpvBits, lpbi, uUsage})
    assert(res!=0)
end procedure

global function GetMessage(atom lpMsg, hWnd, wMsgFilterMin, wMsgFilterMax)
    bool res = c_func(xGetMessage,{lpMsg, hWnd, wMsgFilterMin, wMsgFilterMax})
    return res
end function

global function GetMessagePos()
    atom res = c_func(xGetMessagePos,{})
    return res
end function

global procedure GetMonitorInfo(atom hMon, pMI)
    bool res = c_func(xGetMonitorInfo,{hMon,pMI})
    assert(res!=0)
end procedure

--global function GetOpenFileName(atom hWnd, OFNHookProc, string szFilter)
--  atom pszFilter = allocate_string(szFilter)
--  poke4(pszFile,0)
--  set_struct_field(idOPENFILENAME,pOPENFILENAME,`lStructSize`,get_struct_size(idOPENFILENAME))
--  set_struct_field(idOPENFILENAME,pOPENFILENAME,`hwndOwner`,hWnd)
--  set_struct_field(idOPENFILENAME,pOPENFILENAME,`lpstrFile`,pszFile)
--  set_struct_field(idOPENFILENAME,pOPENFILENAME,`nMaxFile`,MAX_PATH)
--  set_struct_field(idOPENFILENAME,pOPENFILENAME,`lpstrFilter`,pszFilter)
--  atom flags = or_all({OFN_PATHMUSTEXIST,OFN_FILEMUSTEXIST,OFN_ENABLEHOOK,OFN_EXPLORER})
--  set_struct_field(idOPENFILENAME,pOPENFILENAME,`Flags`,flags)
--  set_struct_field(idOPENFILENAME,pOPENFILENAME,`lpfnHook`,OFNHookProc)
----    {} = poke_wstring(szFile,MAX_PATH*2,"")
----    poke8(ofn+8, hWnd)      -- hwndOwner
----    poke8(ofn+16, szFile)   -- lpstrFile
----    poke4(ofn+24, MAX_PATH) -- nMaxFile
----    poke8(ofn+32, `All Files\0*.*\0`) -- lpstrFilter
----    poke4(ofn+72, OFN_PATHMUSTEXIST||OFN_FILEMUSTEXIST||OFN_ENABLEHOOK||OFN_EXPLORER)
----    poke8(ofn+80, call_back({'+',OFNHookProc}))
----    poke8(ofn+80, OFNHookProc)
--
----/*
--      pLOGBRUSH = allocate_struct(idLOGBRUSH)
--      pWNDCLASSEX = allocate_struct(idWNDCLASSEX)
--      pOPENFILENAME = allocate_struct(idOPENFILENAME)
----*/
--  string res = ""
--  bool bOK = c_func(xGetOpenFileName,{pOPENFILENAME})
--  if bOK then
--      res = peek_string(pszFile)
--  end if
--  free(pszFilter)
--  return res
--end function

global procedure GetObject(atom hgdiobj, integer size, atom lpvObject)
    integer r = c_func(xGetObject,{hgdiobj,size,lpvObject})
    assert(r!=0)
end procedure

--global procedure GetBitmap(atom hClip) 
--  integer size = get_struct_size(idBITMAP),
--          r = c_func(xGetObject,{hClip,size,pBITMAP})
--  assert(r!=0)
--end procedure

global function GetParent(atom hWnd)
    return c_func(xGetParent,{hWnd})
end function

global function GetStockObject(integer fnObject)
    atom hGdiObj = c_func(xGetStockObject,{fnObject})
    return hGdiObj
end function

global function GetSystemMetrics(integer nIndex)
    integer res = c_func(xGetSystemMetrics,{nIndex})
    return res
end function

global function GetTextExtentPoint32(atom hDC, string s)
    sequence utf16 = utf8_to_utf16(s)
    integer l = length(utf16)
    atom pUTF16 = allocate(2*l)
    poke2(pUTF16,utf16)
    integer res = c_func(xGetTextExtentPoint32W,{hDC,pUTF16,l,pSIZE})
    assert(res!=0)
    free(pUTF16)
    integer cx = get_struct_field(idSIZE,pSIZE,`cx`),
            cy = get_struct_field(idSIZE,pSIZE,`cy`)
    return {cx,cy}
end function

global procedure GetTextMetrics(atom hDC, lptm)
    bool res = c_func(xGetTextMetrics,{hDC,lptm})
    assert(res!=0)
end procedure

global function GetUpdateRect(atom hWnd, lpRect, bool bErase)
    bool res = c_func(xGetUpdateRect,{hWnd, lpRect, bErase})
    return res
end function

global procedure GetWindowRect(atom hWnd, pRECT)
    bool res = c_func(xGetWindowRect,{hWnd,pRECT})
    assert(res!=0)
end procedure

global function GlobalAlloc(integer uFlags, dwBytes)
    atom hGlobal = c_func(xGlobalAlloc,{uFlags, dwBytes})
    return hGlobal
end function

global procedure GlobalFree(atom hMem)
    atom hGlobal = c_func(xGlobalFree,{hMem})
    assert(hGlobal==NULL)
end procedure

global function GlobalLock(atom hMem)
    atom res = c_func(xGlobalLock,{hMem})
    return res
end function

global procedure GlobalUnlock(atom hMem)
    c_proc(xGlobalUnlock,{hMem})
end procedure

global procedure InitCommonControls()
    c_proc(xInitCommonControls,{})
end procedure

global procedure InvalidateRect(atom hWnd, pRect, bool bErase)
    bool res = c_func(xInvalidateRect,{hWnd,pRect,bErase})
    assert(res)
end procedure

global function IsClipboardFormatAvailable(atom fmt)
    bool res = c_func(xIsClipboardFormatAvailable,{fmt})
    return res
end function

global function IsWindow(atom hWnd)
    bool res = c_func(xIsWindow,{hWnd})
    return res
end function

global function IsWindowVisible(atom hWnd)
    bool res = c_func(xIsWindowVisible,{hWnd})
    return res
end function

global procedure KillTimer(atom hWnd, integer uIDEvent)
    bool res = c_func(xKillTimer,{hWnd,uIDEvent})
    assert(res)
end procedure

global procedure LineTo(atom hDC, x, y)
    bool res = c_func(xLineTo,{hDC,x,y})
    assert(res)
end procedure

--DEV force this use instead:
--global function LoadCursor(atom hInstance, name)
global function LoadCursor(atom name)
    atom hCursor = c_func(xLoadCursor,{NULL,name})
    return hCursor
end function

global function LoadImage(atom hInstance, string name, integer uType, cxDesired, cyDesired, fuLoad)
?"LoadImageA"
    atom handle = c_func(xLoadImage,{hInstance,name,uType,cxDesired,cyDesired,fuLoad})
    return handle
end function

global function LoadIcon(atom hInstance, lpIconName)
    atom icon = c_func(xLoadIcon,{hInstance,lpIconName})
    return icon
end function

global procedure MessageBeep(integer uType)
    bool res = c_func(xMessageBeep,{uType})
    assert(res)
end procedure

global function MonitorFromRect(atom pRECT, dwFlags)
    atom hMon = c_func(xMonitorFromRect,{pRECT,dwFlags})
    return hMon
end function

global procedure MoveToEx(atom hDC, x, y, pPOINT)
    bool res = c_func(xMoveToEx,{hDC,x,y,pPOINT})
    assert(res)
end procedure

global procedure MoveWindow(atom hWnd, x, y, w, h, bool bRepaint)
    c_proc(xMoveWindow,{hWnd, x, y, w, h, bRepaint})
end procedure

global function OpenClipboard(atom hWnd=0)
    -- a hWnd of 0 associates it to the current task
    bool res = c_func(xOpenClipboard,{hWnd})
    return res
end function

global procedure PostMessage(atom hWnd, integer msg, atom wParam, lParam)
    bool res = c_func(xPostMessage,{hWnd,msg,wParam,lParam})
    if res=0 then ?9/0 end if
end procedure

global procedure PostQuitMessage(integer nExitCode=0)
    c_proc(xPostQuitMessage,{nExitCode})
end procedure

--global function PtInRect(atom pRect, pPoint)
--  bool res = c_func(xPtInRect,{pRect, pPoint})
--?{`PtInRect`,res}
--  return res
--end function

global procedure RegisterClassEx(string classname, atom wndproc, class_style=NULL, icon_handle=NULL, cursor_handle=NULL, brush=COLOR_BTNFACE1)
--set_unicode
    atom szAppName = allocate_wstring(classname)
    if cursor_handle=NULL then
        cursor_handle = LoadCursor(IDC_ARROW)
    end if
    if icon_handle=NULL then
--      icon_handle = LoadCursor(IDI_APPLICATION)
        icon_handle = LoadIcon(instance(),IDI_APPLICATION)
    end if
    set_struct_field(idWNDCLASSEX,pWNDCLASSEX,`cbSize`,get_struct_size(idWNDCLASSEX))
    set_struct_field(idWNDCLASSEX,pWNDCLASSEX,`style`,class_style)
    set_struct_field(idWNDCLASSEX,pWNDCLASSEX,`lpfnWndProc`,wndproc) -- default message handler
    set_struct_field(idWNDCLASSEX,pWNDCLASSEX,`cbClsExtra`,0)   -- no more than 40 bytes for win95
    set_struct_field(idWNDCLASSEX,pWNDCLASSEX,`cbWndExtra`,0)   -- "		"      "		"        "
    set_struct_field(idWNDCLASSEX,pWNDCLASSEX,`hInstance`,instance())
    set_struct_field(idWNDCLASSEX,pWNDCLASSEX,`hIcon`,icon_handle)  -- (32 x 32)
    set_struct_field(idWNDCLASSEX,pWNDCLASSEX,`hIconSm`,icon_handle) -- (16 x 16)
    set_struct_field(idWNDCLASSEX,pWNDCLASSEX,`hCursor`,cursor_handle)
    set_struct_field(idWNDCLASSEX,pWNDCLASSEX,`hbrBackground`,brush)
    set_struct_field(idWNDCLASSEX,pWNDCLASSEX,`lpszMenuName`,NULL)
    set_struct_field(idWNDCLASSEX,pWNDCLASSEX,`lpszClassName`,szAppName)
    atom regdclass = c_func(xRegisterClassEx,{pWNDCLASSEX})
    if not regdclass then
        crash(`Registration of new window class: `& classname &` failed.`)
    end if
    free(szAppName)
end procedure

global procedure RegisterHotKey(atom hWnd, idHotKey,fuModifiers, uVirtKey)
    bool res = c_func(xRegisterHotKey,{hWnd,idHotKey,fuModifiers,uVirtKey})
    assert(res)
end procedure

global procedure ReleaseCapture()
    c_proc(xReleaseCapture,{})
end procedure

global procedure ReleaseDC(atom hWnd, hDC)
    bool bOK = c_func(xReleaseDC,{hWnd,hDC})
    assert(bOK)
--  if not bOK then ?{`ReleaseDC?`,bOK} end if
end procedure

global procedure ScreenToClient(atom hWnd, pPt)
    bool res = c_func(xScreenToClient,{hWnd,pPt})
    assert(res) 
end procedure

global function SelectObject(atom hDC, hgdiobj)
    atom old = c_func(xSelectObject,{hDC, hgdiobj})
    return old
end function

--type atom_str(object o)
--  return atom(o) or string(o)
--end type

--global function SendMessage(atom hWnd, uMsg, wParam, atom_str lParam)
--global function SendMessage(atom hWnd, uMsg, wParam, atom_string lParam)
--DEV               typecheck failure, lParam is 11197640.0 ^^^???>?>>
global function SendMessage(atom hWnd, uMsg, wParam, object lParam)
    atom res = c_func(xSendMessage,{hWnd, uMsg, wParam, lParam})
    return res
end function

global procedure SetActiveWindow(atom hWnd)
    atom prev = c_func(xSetActiveWindow,{hWnd})
end procedure

global procedure SetBkMode(atom hDC, integer iBkMode)
    integer res = c_func(xSetBkMode,{hDC, iBkMode})
    assert (res!=0)
end procedure

global procedure SetCapture(atom hWnd)
    c_proc(xSetCapture,{hWnd})
end procedure

constant SIMPLEREGION = 2

global procedure SelectClipRgn(atom hDC, hClip)
    integer complexity = c_func(xSelectClipRgn,{hDC,hClip})
--?{`SelectClipRgn`,hClip,complexity}
    assert(complexity=SIMPLEREGION) -- ? not NULLREGION ?
--#define ERROR             0
--#define NULLREGION            1
--#define SIMPLEREGION      2
--#define COMPLEXREGION     3
--#define RGN_ERROR ERROR
--NULLREGION
--SIMPLEREGION
end procedure

global procedure SetClipboardData(integer uFormat, atom hMem)
    atom res = c_func(xSetClipboardData,{uFormat,hMem})
    assert(res!=NULL)
--  if res=NULL then ?{`SetClipboardData failed`,GetLastError()} end if
end procedure

global procedure SetCursor(atom hCursor)
    c_proc(xSetCursor,{hCursor})
end procedure

global procedure SetFocus(atom hWnd)
    atom hPrev = c_func(xSetFocus,{hWnd})
end procedure

global procedure SetForegroundWindow(atom hWnd)
    c_proc(xSetForegroundWindow,{hWnd})
end procedure

global procedure SetParent(atom child, newparent)
    {} = c_func(xSetParent,{child,newparent})
end procedure

global procedure SetStretchBltMode(atom hDC, integer iStretchMode)
    integer res = c_func(xSetStretchBltMode,{hDC,iStretchMode})
    assert(res!=0)
end procedure

global procedure SetTextColor(atom hDC, crColor)
    atom prev = c_func(xSetTextColor,{hDC,crColor})
end procedure

global function SetTimer(atom hWnd, nIDEvent, uElapse, lpTimerFunc)
    atom hTimer = c_func(xSetTimer,{hWnd, nIDEvent, uElapse, lpTimerFunc})
    return hTimer
end function

global function SetWindowLong(atom hWnd, nIndex, dwNewLong)
    atom res = c_func(xSetWindowLong,{hWnd, nIndex, dwNewLong})
    return res
end function
global constant SetWindowLongPtr = SetWindowLong

global procedure SetWindowPos(atom hWnd, hWndInsertAfter, integer x, y, w, h, uFlags)
    bool bOK = c_func(xSetWindowPos,{hWnd,hWndInsertAfter,x,y,w,h,uFlags})
    assert(bOK)
end procedure

global procedure SetWindowText(atom hWnd, string text)
--  c_proc(xSetWindowTextA,{hWnd,text})
    sequence utf16 = utf8_to_utf16(text)
    atom pText = allocate_wstring(utf16)
    c_proc(xSetWindowTextW,{hWnd,pText})
    free(pText)
end procedure

global procedure ShowWindow(atom hWnd, integer nShow = SW_SHOW_NORMAL)
    c_proc(xShowWindow,{hWnd,nShow})
end procedure

global procedure Sleep(atom milliseconds)
    c_proc(xSleep,{milliseconds})
end procedure

global procedure StretchBlt(atom hdcDest,dx,dy,dw,dh,hdcSrc,sx,sy,sw,sh,dwRop)
    integer res= c_func(xStretchBlt,{hdcDest,dx,dy,dw,dh,hdcSrc,sx,sy,sw,sh,dwRop})
    assert(res!=0)
end procedure

global procedure SystemParametersInfo(atom uiAction,uiParam,pvParam,fWinIni)
    integer res = c_func(xSystemParametersInfoA,{uiAction,uiParam,pvParam,fWinIni})
    assert(res!=0)
end procedure

global procedure TextOut(atom hDC, integer x, y, string s, integer l=length(s))
    sequence utf16 = utf8_to_utf16(s)
    atom lpString = allocate_wstring(utf16)
    l = length(utf16)
    bool bOK = c_func(xTextOut,{hDC,x,y,lpString,l})
    assert(bOK)
    free(lpString)
end procedure

global procedure TrackMouseEvent(atom lpEventTrack)
    bool bOK = c_func(xTrackMouseEvent,{lpEventTrack})
    assert(bOK)
end procedure

global procedure TranslateMessage(atom lpmsg)
    c_proc(xTranslateMessage,{lpmsg})
end procedure

global procedure TransparentBlt(atom destDC, x,y,w,h, srcDC, sx,sy,sw,sh, cTrans)
    bool res = c_func(xTransparentBlt,{destDC, x,y,w,h, srcDC, sx,sy,sw,sh, cTrans})
    assert(res)
end procedure

global procedure UpdateWindow(atom hWnd)
    c_proc(xUpdateWindow,{hWnd})
end procedure

global procedure ValidateRect(atom hWnd, pRECT=NULL)
    bool res = c_func(xValidateRect,{hWnd,pRECT})
    assert(res)
end procedure

global procedure WriteFile(atom hFile, lpBuffer, nNumberOfBytesToWrite, lpNumberOfBytesWritten, lpOverlapped)
    bool res = c_func(xWriteFile,{hFile,lpBuffer,nNumberOfBytesToWrite,lpNumberOfBytesWritten,lpOverlapped})
    assert(res)
end procedure

