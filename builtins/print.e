--
-- builtins\print.e
-- ================
--
--  [Draft] initial notes regarding [physical] printing.
--
--  H:\CFULL\HDD (C)\AAAAAA\bigfoot\edita\pp\pptest.exw is semi-working (windows only)
--  Edita's scan is getting stuck somewhere so it is no longer building a project tree...
--
--  Julia runs "print test.png"
--  Go runs "mspaint /pt filename" or "lp filename"
--  others try "/dev/lp0" or "lpr" or "PRN:" or "LPT3" or "lpt1"

--/*
Microsoft Windows [Version 10.0.19045.3324]
(c) Microsoft Corporation. All rights reserved.

C:\Users\Pete>lp
'lp' is not recognized as an internal or external command,
operable program or batch file.

C:\Users\Pete>lpr
'lpr' is not recognized as an internal or external command,
operable program or batch file.

C:\Users\Pete>print /?
Prints a text file.

PRINT [/D:device] [[drive:][path]filename[...]]

   /D:device   Specifies a print device.


C:\Users\Pete>

Phix:
If you have not got something appropriate attached, this will just hang. Other values you can try, on windows: "AUX", "COM1", "COM2", "LPT1"

integer fn = open(iff(platform()=WIN32?"PRN":"/dev/lp0"),"w")
if fn=-1 then
    puts(1,"some error")
else
    puts(fn,"Hello World!")
    close(fn)
    puts(1,"success!")
end if
{} = wait_key()

PostScript:
Technically not really correct, as this has to be sent to the printer directly. It will output Hello world, then, though.

<</PageSize [595 842]>> setpagedevice  % set page size to DIN A4
/Courier findfont                      % use Courier
12 scalefont setfont                   % 12 pt
28 802 moveto                          % 1 cm from the top and left edges
(Hello world) show                     % draw the string


BBC BASIC:
    PD_RETURNDC = 256
    _LOGPIXELSY = 90
    
    DIM pd{lStructSize%, hwndOwner%, hDevMode%, hDevNames%, \
    \    hdc%, flags%, nFromPage{l&,h&}, nToPage{l&,h&}, \
    \    nMinPage{l&,h&}, nMaxPage{l&,h&}, nCopies{l&,h&}, \
    \    hInstance%, lCustData%, lpfnPrintHook%, lpfnSetupHook%, \
    \    lpPrintTemplateName%, lpSetupTemplateName%, \
    \    hPrintTemplate%, hSetupTemplate%}
    pd.lStructSize% = DIM(pd{})
    pd.hwndOwner% = @hwnd%
    pd.flags% = PD_RETURNDC
    
    SYS "PrintDlg", pd{} TO ok%
    IF ok%=0 THEN QUIT
    SYS "DeleteDC", @prthdc%
    @prthdc% = pd.hdc%
    *MARGINS 0,0,0,0
    
    dx% = @vdu%!236-@vdu%!232
    dy% = @vdu%!244-@vdu%!240
    SYS "GetDeviceCaps", @prthdc%, _LOGPIXELSY TO dpi%
    
    DIM rc{l%,t%,r%,b%}
    SYS "CreateSolidBrush", 0 TO brush%
    
    VDU 2,1,32,3
    pitch% = 2
    FOR y% = 0 TO dy% STEP dpi%
        FOR x% = 0 TO dx%-pitch% STEP pitch%
          rc.l% = x% : rc.r% = x% + pitch%/2
          rc.t% = y% : rc.b% = y% + dpi%
          SYS "FillRect", @prthdc%, rc{}, brush%
        NEXT
        pitch% += 2
    NEXT y%
    VDU 2,1,12,3

( in Comdlg32.dll [A/W])
BOOL WINAPI PrintDlg(
  _Inout_  LPPRINTDLG lppd
);
Known issue: If PD_RETURNDC is set but PD_USEDEVMODECOPIESANDCOLLATE flag is not set, 
the PrintDlgEx and PrintDlg functions return incorrect number of copies. 
To get the correct number of copies, ensure that the calling application always uses 
PD_USEDEVMODECOPIESANDCOLLATE with PD_RETURNDC. 


typedef struct tagPD {
  DWORD           lStructSize;
  HWND            hwndOwner;
  HGLOBAL         hDevMode;
  HGLOBAL         hDevNames;
  HDC             hDC;
  DWORD           Flags;
  WORD            nFromPage;
  WORD            nToPage;
  WORD            nMinPage;
  WORD            nMaxPage;
  WORD            nCopies;
  HINSTANCE       hInstance;
  LPARAM          lCustData;
  LPPRINTHOOKPROC lpfnPrintHook;
  LPSETUPHOOKPROC lpfnSetupHook;
  LPCTSTR         lpPrintTemplateName;
  LPCTSTR         lpSetupTemplateName;
  HGLOBAL         hPrintTemplate;
  HGLOBAL         hSetupTemplate;
} PRINTDLG, *LPPRINTDLG;


HRESULT WINAPI PrintDlgEx(
  _Inout_  LPPRINTDLGEX lppd
);

typedef struct tagPDEX {
  DWORD            lStructSize;
  HWND             hwndOwner;
  HGLOBAL          hDevMode;
  HGLOBAL          hDevNames;
  HDC              hDC;
  DWORD            Flags;
  DWORD            Flags2;
  DWORD            ExclusionFlags;
  DWORD            nPageRanges;
  DWORD            nMaxPageRanges;
  LPPRINTPAGERANGE lpPageRanges;
  DWORD            nMinPage;
  DWORD            nMaxPage;
  DWORD            nCopies;
  HINSTANCE        hInstance;
  LPCTSTR          lpPrintTemplateName;
  LPUNKNOWN        lpCallback;
  DWORD            nPropertyPages;
  HPROPSHEETPAGE   *lphPropertyPages;
  DWORD            nStartPage;
  DWORD            dwResultAction;
} PRINTDLGEX, *LPPRINTDLGEX;


typedef struct _devicemode {
  TCHAR dmDeviceName[CCHDEVICENAME];
  WORD  dmSpecVersion;
  WORD  dmDriverVersion;
  WORD  dmSize;
  WORD  dmDriverExtra;
  DWORD dmFields;
  union {
    struct {
      short dmOrientation;
      short dmPaperSize;
      short dmPaperLength;
      short dmPaperWidth;
      short dmScale;
      short dmCopies;
      short dmDefaultSource;
      short dmPrintQuality;
    };
    struct {
      POINTL dmPosition;
      DWORD  dmDisplayOrientation;
      DWORD  dmDisplayFixedOutput;
    };
  };
  short dmColor;
  short dmDuplex;
  short dmYResolution;
  short dmTTOption;
  short dmCollate;
  TCHAR dmFormName[CCHFORMNAME];
  WORD  dmLogPixels;
  DWORD dmBitsPerPel;
  DWORD dmPelsWidth;
  DWORD dmPelsHeight;
  union {
    DWORD dmDisplayFlags;
    DWORD dmNup;
  };
  DWORD dmDisplayFrequency;
#if (WINVER >= 0x0400)
  DWORD dmICMMethod;
  DWORD dmICMIntent;
  DWORD dmMediaType;
  DWORD dmDitherType;
  DWORD dmReserved1;
  DWORD dmReserved2;
#if (WINVER >= 0x0500) || (_WIN32_WINNT >= 0x0400)
  DWORD dmPanningWidth;
  DWORD dmPanningHeight;
#endif 
#endif 
} DEVMODE, *PDEVMODE, *LPDEVMODE;


typedef struct tagDEVNAMES {
  WORD wDriverOffset;
  WORD wDeviceOffset;
  WORD wOutputOffset;
  WORD wDefault;
} DEVNAMES, *LPDEVNAMES;
           
        
DWORD WINAPI CommDlgExtendedError(void);

winspool.drv:
============

BOOL OpenPrinter(
  _In_   LPTSTR pPrinterName,
  _Out_  LPHANDLE phPrinter,
  _In_   LPPRINTER_DEFAULTS pDefault
);


typedef struct _PRINTER_DEFAULTS {
  LPTSTR      pDatatype;
  LPDEVMODE   pDevMode;
  ACCESS_MASK DesiredAccess;
} PRINTER_DEFAULTS, *PPRINTER_DEFAULTS;
           

DWORD StartDocPrinter(
  _In_  HANDLE hPrinter,
  _In_  DWORD Level,
  _In_  LPBYTE pDocInfo
);


typedef struct _DOC_INFO_1 {
  LPTSTR pDocName;
  LPTSTR pOutputFile;
  LPTSTR pDatatype;
} DOC_INFO_1;


BOOL StartPagePrinter(
  _In_  HANDLE hPrinter
);

( Spoolss.dll )
BOOL WritePrinter(
  _In_   HANDLE hPrinter,
  _In_   LPVOID pBuf,
  _In_   DWORD cbBuf,
  _Out_  LPDWORD pcWritten
);


BOOL EndPagePrinter(
  _In_  HANDLE hPrinter
);


BOOL EndDocPrinter(
  _In_  HANDLE hPrinter
);


BOOL ClosePrinter(
  _In_  HANDLE hPrinter
);


BOOL GetDefaultPrinter(
  _In_     LPTSTR pszBuffer,
  _Inout_  LPDWORD pcchBuffer
);

GtkPrintOperation*
gtk_print_operation_new (
  void
)

GtkPrintOperationResult
gtk_print_operation_run (
  GtkPrintOperation* op,
  GtkPrintOperationAction action,
  GtkWindow* parent,
  GError** error
)

if (settings != NULL) { gtk_print_operation_set_print_settings (print, settings); }
if (page_setup != NULL) { gtk_print_operation_set_default_page_setup (print, page_setup); }
--g_signal_connect (print, "begin-print", G_CALLBACK (begin_print), &data);
g_signal_connect (print, "draw-page", G_CALLBACK (draw_page), &data);
res = gtk_print_operation_run (print,
                               GTK_PRINT_OPERATION_ACTION_PRINT_DIALOG,
                               parent,
                               &error);
if (res == GTK_PRINT_OPERATION_RESULT_ERROR) {
--   error_dialog = gtk_message_dialog_new (GTK_WINDOW (parent),
--                               GTK_DIALOG_DESTROY_WITH_PARENT,
--                       GTK_MESSAGE_ERROR,
--                       GTK_BUTTONS_CLOSE,
--                       "Error printing file:\n%s",
--                       error->message);
--   g_signal_connect (error_dialog, "response",
--                   G_CALLBACK (gtk_widget_destroy), NULL);
--   gtk_widget_show (error_dialog);
--   g_error_free (error);
} else if (res == GTK_PRINT_OPERATION_RESULT_APPLY) {
   if (settings != NULL) { g_object_unref (settings); }
   settings = g_object_ref (gtk_print_operation_get_print_settings (print));
}

enum Gtk.PrintOperationAction
The action parameter to gtk_print_operation_run() determines what action the print operation should perform.
Name                                        Description
GTK_PRINT_OPERATION_ACTION_PRINT_DIALOG     Show the print dialog.
GTK_PRINT_OPERATION_ACTION_PRINT            Start to print without showing the print dialog, based on the current print settings.
GTK_PRINT_OPERATION_ACTION_PREVIEW          Show the print preview.
GTK_PRINT_OPERATION_ACTION_EXPORT           Export to a file. This requires the export-filename property to be set.
typedef enum {
  GTK_PRINT_OPERATION_ACTION_PRINT_DIALOG,
  GTK_PRINT_OPERATION_ACTION_PRINT,
  GTK_PRINT_OPERATION_ACTION_PREVIEW,
  GTK_PRINT_OPERATION_ACTION_EXPORT
} GtkPrintOperationAction;

typedef enum {
  GTK_PRINT_OPERATION_RESULT_ERROR,
  GTK_PRINT_OPERATION_RESULT_APPLY,
  GTK_PRINT_OPERATION_RESULT_CANCEL,
  GTK_PRINT_OPERATION_RESULT_IN_PROGRESS
} GtkPrintOperationResult;


--void begin_print (
--  GtkPrintOperation* self,
--  GtkPrintContext* context,
--  gpointer user_data
--)
--
--A typical use for ::begin-print is to use the parameters from the GtkPrintContext and paginate the document accordingly, 
--and then set the number of pages with gtk_print_operation_set_n_pages().

void draw_page (
  GtkPrintOperation* self,
  GtkPrintContext* context,
  gint page_nr,
  gpointer user_data
)

Emitted for every page that is printed. The signal handler must render the page_nr‘s page onto the cairo context obtained from context using gtk_print_context_get_cairo_context().

static void
draw_page (GtkPrintOperation *operation,
           GtkPrintContext   *context,
           gint               page_nr,
           gpointer           user_data) {
  cairo_t *cr = gtk_print_context_get_cairo_context (context);
  gdouble width = gtk_print_context_get_width (context);
  
  cairo_rectangle (cr, 0, 0, width, HEADER_HEIGHT);
  
  cairo_set_source_rgb (cr, 0.8, 0.8, 0.8);
  cairo_fill (cr);
  
  PangoLayout *layout = gtk_print_context_create_pango_layout (context);
  
  PangoFontDescription *desc = pango_font_description_from_string ("sans 14");
  pango_layout_set_font_description (layout, desc);
  pango_font_description_free (desc);
  
  pango_layout_set_text (layout, "some text", -1);
  pango_layout_set_width (layout, width * PANGO_SCALE);
  pango_layout_set_alignment (layout, PANGO_ALIGN_CENTER);
                  
  gint layout_height;
  pango_layout_get_size (layout, NULL, &layout_height);
  gdouble text_height = (gdouble)layout_height / PANGO_SCALE;
  
  cairo_move_to (cr, width / 2,  (HEADER_HEIGHT - text_height) / 2);
  pango_cairo_show_layout (cr, layout);
  
  g_object_unref (layout);
}

--*/
