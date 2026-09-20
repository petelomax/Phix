--
-- demo\theGUI\theGUI.gdiplus.e
--
--  Some useless gdiplus stuff, moved out of theGUI.WINAPI.e for obvious reasons.
--  NB In my experiments I saw absolutely no benefit whatsoever of gdiplus over
--     the older plain gdi. Maybe the outrageously overcomplicated Direct Draw 
--     garbage /might/ be better, but without a flat API I'm not going there.
--  Update: now used in win_video.exw (since ChatGPT blatted it back at me...)
--
global constant 
        GDI_OK = 0,
        GDI_STATUS_OK = 0,
--/*
Ok=0,
GenericError=1,
InvalidParameter=2,
OutOfMemory=3,
ObjectBusy=4,
InsufficientBuffer=5,
NotImplemented=6,
Win32Error=7,
WrongState=8,
Aborted=9,
FileNotFound=10,
ValueOverflow=11,
AccessDenied=12,
UnknownImageFormat=13,
FontFamilyNotFound=14,
FontStyleNotFound=15,
NotTrueTypeFont=16,
UnsupportedGdiplusVersion=17,
GdiplusNotInitialized=18,
PropertyNotFound=19,
PropertyNotSupported=20,
--*/
        ImageLockModeRead = 0x0001,
--      ImageLockModeWrite = 0x0002,
--      ImageLockModeUserInputBuf = 0x0004,
--#define    PixelFormatIndexed    0x00010000 // Indexes into a palette
        PixelFormatGDI =       0x00020000, // Is a GDI-supported format
        PixelFormatAlpha =     0x00040000, // Has an alpha component
        PixelFormatPAlpha =    0x00080000, // Pre-multiplied alpha
--#define    PixelFormatExtended       0x00100000 // Extended color 16 bits/channel
        PixelFormatCanonical = 0x00200000,

--#define    PixelFormatUndefined     0
--#define    PixelFormatDontCare          0

--#define    PixelFormat1bppIndexed   (1 | ( 1 << 8) | PixelFormatIndexed | PixelFormatGDI)
--#define    PixelFormat4bppIndexed   (2 | ( 4 << 8) | PixelFormatIndexed | PixelFormatGDI)
--#define    PixelFormat8bppIndexed   (3 | ( 8 << 8) | PixelFormatIndexed | PixelFormatGDI)
--#define    PixelFormat16bppGrayScale  (4 | (16 << 8) | PixelFormatExtended)
--#define    PixelFormat16bppRGB555   (5 | (16 << 8) | PixelFormatGDI)
--#define    PixelFormat16bppRGB565   (6 | (16 << 8) | PixelFormatGDI)
--#define    PixelFormat16bppARGB1555   (7 | (16 << 8) | PixelFormatAlpha | PixelFormatGDI)
--#define    PixelFormat24bppRGB          (8 | (24 << 8) | PixelFormatGDI)
--#define    PixelFormat32bppRGB          (9 | (32 << 8) | PixelFormatGDI)
        PixelFormat32bppARGB  =       (10 + (32 << 8) + PixelFormatAlpha + PixelFormatGDI + PixelFormatCanonical),
        PixelFormat32bppPARGB =       (11 + (32 << 8) + PixelFormatAlpha + PixelFormatPAlpha + PixelFormatGDI),
--#define    PixelFormat48bppRGB          (12 | (48 << 8) | PixelFormatExtended)
--#define    PixelFormat64bppARGB     (13 | (64 << 8) | PixelFormatAlpha  | PixelFormatCanonical | PixelFormatExtended)
--#define    PixelFormat64bppPARGB    (14 | (64 << 8) | PixelFormatAlpha  | PixelFormatPAlpha | PixelFormatExtended)
--#define    PixelFormatMax           15
        UnsupportedGdiplusVersion = 17,
        SmoothingModeAntiAlias = 1,
        UnitPixel = 2,
        idGdiplusStartupInput = define_struct("""typedef struct {
                                                  UINT32         GdiplusVersion;
                                                  DebugEventProc DebugEventCallback;
                                                  BOOL           SuppressBackgroundThread;
                                                  BOOL           SuppressExternalCodecs;
                                                } GdiplusStartupInput;"""),
        pGdiplusStartupInput = allocate_struct(idGdiplusStartupInput),
-- warning: this may be quite wrong:
        idImageCodecInfo = define_struct(`typedef struct ImageCodecInfo {
                                           CLSID ClassID;
                                           GUID FormatID;
                                           WCHAR * CodecName;
                                           WCHAR * DllName;
                                           WCHAR * FormatDescription;
                                           WCHAR * FilenameExtension;
                                           WCHAR * MimeType;
                                           DWORD Flags;
                                           DWORD Version;
                                           DWORD SigCount;
                                           DWORD SigSize;
                                           BYTE * SigPattern;
                                           BYTE * SigMask;
                                          };`)

set_struct_field(idGdiplusStartupInput,pGdiplusStartupInput,"GdiplusVersion",1)

atom 
     xGdipBitmapLockBits,
     xGdipBitmapUnlockBits,
--   xGdipCreateBitmapFromGraphics,
--   xGdipCreateBitmapFromHBITMAP,
     xGdipCreateBitmapFromScan0,
     xGdipCreateHBITMAPFromBitmap,
     xGdipCreateFromHDC,
     xGdipCreatePen1,
     xGdipCreateSolidFill,
     xGdipDeleteBrush,
     xGdipDeleteGraphics,
     xGdipDeletePen,
     xGdipDisposeImage,
     xGdipDrawImageI,
     xGdipDrawLine,
     xGdipFillRectangleI,
--   xGdipGetImageEncoders,
--   xGdipGetImageEncodersSize,
     xGdipGetImageWidth,
     xGdipGetImageHeight,
     xGdipGraphicsClear,
     xGdipLoadImageFromFile,
     xGdiplusShutdown,
     xGdiplusStartup,
--   xGdipSaveImageToFile,
     xGdipSetPenColor,
     xGdipSetPenDashArray,
     xGdipSetPenDashStyle,
     xGdipSetSmoothingMode,
$

constant GDIPLUS  = open_dll("GdiPlus.dll")

        xGdipBitmapLockBits = define_c_func(GDIPLUS,"GdipBitmapLockBits",
            {C_PTR,     --  GpBitmap* bitmap
             C_PTR,     --  GDIPCONST GpRect *rect
             C_PTR,     --  UINT flags
             C_PTR,     --  PixelFormat format
             C_PTR},    --  BitmapData *lockeddata
            C_INT)      -- GpStatus

        xGdipBitmapUnlockBits = define_c_func(GDIPLUS,"GdipBitmapUnlockBits",
            {C_PTR,     --  GpBitmap* bitmap
             C_PTR},    --  BitmapData *lockedBitmapData
            C_INT)      -- GpStatus

--      xGdipCreateBitmapFromGraphics = define_c_func(GDIPLUS,"GdipCreateBitmapFromGraphics",
--          {C_INT,     --  INT width
--           C_INT,     --  INT height
--           C_PTR,     --  GpGraphics *target
--           C_PTR},    --  GpGraphics **bitmap
--          C_INT)      -- GpStatus

--      xGdipCreateBitmapFromHBITMAP = define_c_func(GDIPLUS,"GdipCreateBitmapFromHBITMAP",
--          {C_PTR,     --  HBITMAP hbm
--           C_PTR,     --  HPALETTE hpal
--           C_PTR},    --  GpBitmap** bitmap
--          C_INT)      -- GpStatus

        xGdipCreateBitmapFromScan0 = define_c_func(GDIPLUS,"GdipCreateBitmapFromScan0",
            {C_INT,     --  INT width
             C_INT,     --  INT height
             C_INT,     --  INT stride
             C_INT,     --  PixelFormat format
             C_PTR,     --  BYTE* scan0
             C_PTR},    --  GpBitmap** bitmap
            C_INT)      -- GpStatus

        xGdipCreateHBITMAPFromBitmap = define_c_func(GDIPLUS,"GdipCreateHBITMAPFromBitmap",
            {C_PTR,     --  GpBitmap* bitmap
             C_PTR,     --  HBITMAP* hbmReturn
             C_PTR},    --  ARGB background
            C_INT)      -- GpStatus

        xGdipCreateFromHDC = define_c_func(GDIPLUS,"GdipCreateFromHDC",
            {C_UINT,    --  HDC hdc
             C_PTR},    --  GpGraphics **graphics
            C_INT)      -- GpStatus

        xGdipCreatePen1 = define_c_func(GDIPLUS,"GdipCreatePen1",
            {C_UINT,    --  [in, ref]  const Color &color
             C_FLOAT,   --  [in]       REAL width
             C_INT,     --             GpUnit unit  
             C_PTR},    --             GpPen **pen
            C_INT)      -- GpStatus

        xGdipCreateSolidFill = define_c_func(GDIPLUS,"GdipCreateSolidFill",
            {C_INT,     --  ARGB color
             C_PTR},    --  GpSolidFill **brush
            C_INT)      -- GpStatus

        xGdipDeleteBrush = define_c_func(GDIPLUS,"GdipDeleteBrush",
            {C_PTR},    --  GpBrush *brush
            C_LONG)     -- GpStatus

        xGdipDeleteGraphics = define_c_func(GDIPLUS,"GdipDeleteGraphics",
            {C_PTR},    --  GpGraphics *graphics
            C_LONG)     -- GpStatus

        xGdipDeletePen = define_c_func(GDIPLUS,"GdipDeletePen",
            {C_PTR},    --  GpPen *pen
            C_INT)      -- GpStatus

        xGdipDisposeImage = define_c_func(GDIPLUS,"GdipDisposeImage",
            {C_PTR},    --  pImage *image
            C_LONG)     -- Status

        xGdipDrawImageI = define_c_func(GDIPLUS,"GdipDrawImageI",
            {C_PTR,     --  GpGraphics *graphics 
             C_PTR,     --  GpImage *image
             C_INT,     --  INT x
             C_INT},    --  INT y
            C_INT)      -- GpStatus

--DEV should probably use the int version...
        xGdipDrawLine = define_c_func(GDIPLUS,"GdipDrawLine",
            {C_PTR,     --  GpGraphics *graphics 
             C_PTR,     --  GpPen *pen 
             C_FLOAT,   --  REAL x1 
             C_FLOAT,   --  REAL y1
             C_FLOAT,   --  REAL x2
             C_FLOAT},  --  REAL y2
            C_INT)      -- GpStatus

        xGdipFillRectangleI = define_c_func(GDIPLUS,"GdipFillRectangleI",
            {C_PTR,     --  GpGraphics *graphics 
             C_PTR,     --  GpBrush *brush
             C_INT,     --  INT x
             C_INT,     --  INT y 
             C_INT,     --  INT width
             C_INT},    --  INT height
            C_INT)      -- GpStatus

--      xGdipGetImageEncoders = define_c_func(GDIPLUS,"GdipGetImageEncoders",
--          {C_UINT,    --  UINT numEncoders
--           C_UINT,    --  UINT size
--           C_PTR},    --  ImageCodecInfo* encoders
--          C_INT)      -- GpStatus
--
--      xGdipGetImageEncodersSize = define_c_func(GDIPLUS,"GdipGetImageEncodersSize",
--          {C_PTR,     --  UINT* numEncoders
--           C_PTR},    --  UINT* size
--          C_INT)      -- GpStatus

        xGdipGetImageWidth = define_c_func(GDIPLUS,"GdipGetImageWidth",
            {C_PTR,     --  pImage *image
             C_PTR},    --  UNIT *width
            C_LONG)     -- Status

        xGdipGetImageHeight = define_c_func(GDIPLUS,"GdipGetImageHeight",
            {C_PTR,     --  pImage *image
             C_PTR},    --  UNIT *height
            C_LONG)     -- Status

        xGdipGraphicsClear = define_c_func(GDIPLUS,"GdipGraphicsClear",
            {C_PTR,     --  GpGraphics *graphics 
             C_INT},    --  ARGB color
            C_INT)      -- GpStatus

        xGdipLoadImageFromFile = define_c_func(GDIPLUS,"GdipLoadImageFromFile",
            {C_PTR,     --  WCHAR* filename
             C_PTR},    --  ppImage **image
            C_LONG)     -- Status

        xGdiplusShutdown = define_c_proc(GDIPLUS,"GdiplusShutdown",
            {C_PTR})    --  pToken

        xGdiplusStartup = define_c_func(GDIPLUS,"GdiplusStartup",
            {C_PTR,     --  __out  ULONG_PTR token *token
             C_PTR,     --  __in   const GdiplusStartupInput *input
             C_PTR},    --  __out  GdiplusStartupOutput *output
            C_INT)      -- Status
           
--      xGdipSaveImageToFile = define_c_func(GDIPLUS,"GdipSaveImageToFile",
--          {C_PTR,     --  GpImage* image
--           C_PTR,     --  GDIPCONST WCHAR* filename
--           C_PTR,     --  GDIPCONST CLSID* clsidEncoder
--           C_PTR},    --  GDIPCONST EncoderParameters* encoderParams
--          C_INT)      -- GpStatus

        xGdipSetPenColor = define_c_func(GDIPLUS,"GdipSetPenColor",
            {C_PTR,     --  GpPen *pen
             C_INT},    --  ARGB argb
            C_INT)      -- GpStatus

        xGdipSetPenDashArray = define_c_func(GDIPLUS,"GdipSetPenDashArray",
            {C_PTR,     --  GpPen *pen
             C_PTR,     --  REAL *dash
             C_INT},    --  INT count
            C_INT)      -- GpStatus

        xGdipSetPenDashStyle = define_c_func(GDIPLUS,"GdipSetPenDashStyle",
            {C_PTR,     --  GpPen *pen
             C_INT},    --  GpDashStyle dashstyle
            C_INT)      -- GpStatus

        xGdipSetSmoothingMode = define_c_func(GDIPLUS,"GdipSetSmoothingMode",
            {C_PTR,     --  GpGraphics *graphics
             C_INT},    --  SmoothingMode smoothingMode
            C_INT)      -- GpStatus


--global function GdipCreateBitmapFromGraphics(atom w, h, pGraphics)
--  atom ppBitmap = allocate_word()
--  integer res = c_func(xGdipCreateBitmapFromGraphics,{w,h,pGraphics,ppBitmap})
--  assert(res==GDI_STATUS_OK)
--  atom pBitmap = peeknu(ppBitmap)
--  free(ppBitmap)
--  return pBitmap
--end function

global procedure GdipBitmapLockBits(atom bitmap, rect, flags, pixelfmt, pData)
    integer res = c_func(xGdipBitmapLockBits,{bitmap, rect, flags, pixelfmt, pData})
    assert(res==GDI_STATUS_OK)
end procedure

global procedure GdipBitmapUnlockBits(atom bitmap, pData)
    integer res = c_func(xGdipBitmapUnlockBits,{bitmap, pData})
    assert(res==GDI_STATUS_OK)
end procedure

--global function GdipCreateBitmapFromHBITMAP(atom hbm, hpal, gbBipmap)
--  integer status = c_func(xGdipCreateBitmapFromHBITMAP,{hbm,hpal,gbBipmap})
--  return status
--end function

--global function GdipCreateBitmapFromScan0(integer width, height, stride, pixel_format, atom scan0, pBitmap)
global function GdipCreateBitmapFromScan0(integer width, height, stride, pixel_format, string scan0, atom pBitmap)
    integer status = c_func(xGdipCreateBitmapFromScan0,{width, height, stride, pixel_format, scan0, pBitmap})
    return status
end function

global function GdipCreateHBITMAPFromBitmap(atom bitmap, hbmReturn, background)
    integer status = c_func(xGdipCreateHBITMAPFromBitmap,{bitmap, hbmReturn, background})
    return status
end function


global function GdipCreateFromHDC(atom hdc)
    atom ppGraphics = allocate_word()
    integer res = c_func(xGdipCreateFromHDC,{hdc,ppGraphics})
    assert(res==GDI_STATUS_OK)
    atom pGraphics = peeknu(ppGraphics)
    free(ppGraphics)
    return pGraphics
end function

global function GdipCreatePen1(atom colour, width, integer unit=UnitPixel)
    atom ppPen = allocate_word()
    integer res = c_func(xGdipCreatePen1,{colour,width,unit,ppPen})
    assert(res==GDI_STATUS_OK)
    atom pPen = peeknu(ppPen)
    free(ppPen)
    return pPen
end function

global function GdipCreateSolidFill(atom colour)
    atom ppBrush = allocate_word()
    integer res = c_func(xGdipCreateSolidFill,{colour,ppBrush})
    assert(res==GDI_STATUS_OK)
    atom pBrush = peeknu(ppBrush)
    free(ppBrush)
    return pBrush
end function

global procedure GdipDeleteBrush(atom pBrush)
    integer res = c_func(xGdipDeleteBrush,{pBrush})
    assert(res==GDI_STATUS_OK)
end procedure

global procedure GdipDeleteGraphics(atom pGraphics)
    integer res = c_func(xGdipDeleteGraphics,{pGraphics})
    assert(res==GDI_STATUS_OK)
end procedure

global procedure GdipDeletePen(atom pPen)
    integer res = c_func(xGdipDeletePen,{pPen})
    assert(res==GDI_STATUS_OK)
end procedure

global procedure GdipDisposeImage(atom pImage)
    integer res = c_func(xGdipDisposeImage,{pImage})
    assert(res==GDI_STATUS_OK)
end procedure

global procedure GdipDrawImage(atom pGraphics, pImage, x, y)
    integer res = c_func(xGdipDrawImageI,{pGraphics, pImage, x, y})
    assert(res==GDI_STATUS_OK)
end procedure

global procedure GdipDrawLine(atom pGraphics, pPen, x1, y1, x2, y2)
    integer res = c_func(xGdipDrawLine,{pGraphics, pPen, x1, y1, x2, y2})
    assert(res==GDI_STATUS_OK)
end procedure

global procedure GdipFillRectangle(atom pGraphis, pBrush, x, y, width, height)
    integer res = c_func(xGdipFillRectangleI,{pGraphis, pBrush, x, y, width, height})
    assert(res==GDI_STATUS_OK)
end procedure

--global procedure GdipGetImageEncoders(integer numEncoders, size, atom pEncoders)
--  integer status = c_func(xGdipGetImageEncoders,{numEncoders, size, pEncoders})
--  assert(status==GDI_STATUS_OK)
--end procedure
--
--global procedure GdipGetImageEncodersSize(atom pNum, pSize)
--  integer status = c_func(xGdipGetImageEncodersSize,{pNum, pSize})
--  assert(status==GDI_STATUS_OK)
--end procedure

constant pWord = allocate(4)

global function GdipGetImageWidth(atom pImage)
    integer status = c_func(xGdipGetImageWidth,{pImage,pWord})
    assert(status=GDI_OK)
    integer width = peek4u(pWord)
    return width
end function

global function GdipGetImageHeight(atom pImage)
    integer status = c_func(xGdipGetImageHeight,{pImage,pWord})
    assert(status=GDI_OK)
    integer height = peek4u(pWord)
    return height
end function

global procedure GdipGraphicsClear(atom pGraphics, color)
    integer res = c_func(xGdipGraphicsClear,{pGraphics,color})
    assert(res==GDI_STATUS_OK)
end procedure
    
global function GdipLoadImageFromFile(string filepath)
    atom lpFilePathW = allocate_wstring(utf8_to_utf16(filepath))
    integer status = c_func(xGdipLoadImageFromFile,{lpFilePathW,pWord})
    assert(status=GDI_OK)
    free(lpFilePathW)
    atom pImage = peek4u(pWord)
    return pImage
end function

global procedure GdiplusShutdown(atom pToken)
    c_proc(xGdiplusShutdown,{pToken})
end procedure

global procedure GdiplusStartup(atom pToken, pGdiplusStartupInput, pGdiplusStartupOutput)
    integer res = c_func(xGdiplusStartup,{pToken, pGdiplusStartupInput, pGdiplusStartupOutput})
    if res=UnsupportedGdiplusVersion then ?9/0 end if
    assert(res=GDI_STATUS_OK)
end procedure

--global function GdipSaveImageToFile(atom image, string filename, atom clsidEncoder, encoderParams)
--  integer status = c_func(xGdipSaveImageToFile,{image,filename,clsidEncoder,encoderParams})
--  return status
--end function

global procedure GdipSetPenColor(atom pPen, colour)
    integer res = c_func(xGdipSetPenColor,{pPen, colour})
    assert(res=GDI_STATUS_OK)
end procedure

global procedure GdipSetPenDashArray(atom pPen, sequence dash)
    integer l = length(dash)
    atom pDash = allocate(l*4)
    for i=1 to l do
        poke(pDash+(i-1)*4,atom_to_float32(dash[i]))
    end for
    integer res = c_func(xGdipSetPenDashArray,{pPen,pDash,l})
    assert(res==GDI_STATUS_OK)
    free(pDash)
end procedure
global constant GdipSetDashPattern = GdipSetPenDashArray

global procedure GdipSetPenDashStyle(atom pPen, integer penstyle)
    integer res = c_func(xGdipSetPenDashStyle,{pPen, penstyle})
    assert(res=GDI_STATUS_OK)
end procedure

global procedure GdipSetSmoothingMode(atom p_graphics, smoothingMode)
    integer res = c_func(xGdipSetSmoothingMode,{p_graphics,smoothingMode})
    assert(res==GDI_STATUS_OK)  
end procedure

