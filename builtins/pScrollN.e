--
-- pScrollN.e
--
--  implements the scroll() routine.
--

constant
    STD_INPUT_HANDLE         = -10,
    STD_OUTPUT_HANDLE        = -11,
    STD_ERROR_HANDLE         = -12,

--  ENABLE_PROCESSED_INPUT   = 1,

    C_PTR = C_POINTER

object void

atom kernel32,
    xGetStdHandle,
    xAllocConsole,
    xSetConsoleMode,
    xGetConsoleScreenBufferInfo,
    xFillConsoleOutputCharacter,
    xFillConsoleOutputAttribute,
    xScrollConsoleScreenBuffer

-- CONSOLE_SCREEN_BUFFER_INFO structure (pCSBI):
constant
    CSBI_SIZEX  = 0,    --  COORD      dwSize; 
--  CSBI_SIZEY  = 2,
--  CSBI_CPOSX  = 4,    --  COORD      dwCursorPosition; 
--  CSBI_CPOSY  = 6,
    CSBI_ATTR   = 8,    --  WORD       wAttributes; 
--  CSBI_WINX1  = 10,   --  SMALL_RECT srWindow; 
--  CSBI_WINY1  = 12,
--  CSBI_WINX2  = 14,
--  CSBI_WINY2  = 16,
--  CSBI_MAXX   = 18,   --  COORD      dwMaximumWindowSize; 
--  CSBI_MAXY   = 20,
    sizeof_CSBI = 22

-- SMALL_RECT structure (pSMALLRECT):
constant
--  SR_Left = 0,
--  SR_Top = 2,
--  SR_Right = 4,
--  SR_Bottom = 6,
    sizeof_SMALL_RECT = 8

-- CHAR_INFO structure (pCHARINFO):
constant
--  CI_UnicodeChar = 0,     -- Unicode or ANSI character
--  CI_Attributes = 2,      -- text and background colors 
    sizeof_CHAR_INFO = 4


atom stdin, stdout, stderr

integer Sinit
        Sinit = 0
--integer stdin_redirected
--      stdin_redirected = 0

procedure initScroll()
    if platform()=WINDOWS then
        kernel32 = open_dll("kernel32.dll")
--#without reformat
        xGetStdHandle = define_c_func(kernel32,"GetStdHandle",
            {C_UINT},   --  DWORD  nStdHandle   // input, output, or error device
            C_PTR)      -- HANDLE
        xAllocConsole = define_c_func(kernel32,"AllocConsole",
            {},         --  no parameters
            C_INT)      -- BOOL
        xSetConsoleMode = define_c_func(kernel32,"SetConsoleMode",
            {C_PTR,     --  HANDLE  hConsole,   // handle of console input or screen buffer
             C_LONG},   --  DWORD  fdwMode      // input or output mode to set 
            C_INT)      -- BOOL
        xGetConsoleScreenBufferInfo = define_c_func(kernel32,"GetConsoleScreenBufferInfo",
            {C_PTR,     --  HANDLE  hConsoleOutput, // handle of console screen buffer
             C_PTR},    --  PCONSOLE_SCREEN_BUFFER_INFO  // address of screen buffer info
            C_INT)      -- BOOL
        xScrollConsoleScreenBuffer = define_c_func(kernel32,"ScrollConsoleScreenBufferA",
            {C_PTR,     --  HANDLE  hConsoleOutput, // handle of console screen buffer
             C_PTR,     --  PSMALL_RECT  psrctSourceRect, // address of screen buffer rect. to move
             C_PTR,     --  PSMALL_RECT  psrctClipRect, // address of affected screen buffer rect. 
             C_INT,     --  COORD  coordDestOrigin, // new location of screen buffer rect.
             C_PTR},    --  PCHAR_INFO  pchiFill    // address of fill character and color 
            C_INT)      -- BOOL
        xFillConsoleOutputCharacter = define_c_func(kernel32,"FillConsoleOutputCharacterA",
            {C_PTR,     --  HANDLE  hConsoleOutput, // handle of screen buffer
             C_USHORT,  --  TCHAR  cCharacter,  // character to write
             C_UINT,    --  DWORD  nLength, // number of character cells to write to
             C_UINT,    --  COORD  dwWriteCoord,    // x- and y-coordinates of first cell
             C_PTR},    --  LPDWORD  lpNumberOfCharsWritten     // address of number of cells written to
            C_INT)      -- BOOL
        xFillConsoleOutputAttribute = define_c_func(kernel32,"FillConsoleOutputAttribute",
            {C_PTR,     --  HANDLE  hConsoleOutput, // handle of screen buffer
             C_USHORT,  --  WORD  wAttribute,   // color attribute to write
             C_UINT,    --  DWORD  nLength, // number of character cells to write to
             C_UINT,    --  COORD  dwWriteCoord,    // x- and y-coordinates of first cell
             C_PTR},    --  LPDWORD  lpNumberOfAttrsWritten     // address of number of cells written to
            C_INT)      -- BOOL
--#without reformat
        void = c_func(xAllocConsole,{})
        stdin = c_func(xGetStdHandle,{STD_INPUT_HANDLE})
        stdout = c_func(xGetStdHandle,{STD_OUTPUT_HANDLE})
        stderr = c_func(xGetStdHandle,{STD_ERROR_HANDLE})
--  -- nb following is not ENABLE_LINE_INPUT and not ENABLE_ECHO_INPUT
--      if not c_func(xSetConsoleMode,{stdin,ENABLE_PROCESSED_INPUT}) then
--          stdin_redirected = 1
--      end if
    elsif platform()=LINUX then
        stdin = 0
        stdout = 1
        stderr = 2
    end if
    Sinit = 1
end procedure

--DEV not quite right... (fix below, but this still may not cater for amount<-1 or >+1))
global procedure scroll(integer amount, integer top, integer bottom)
integer right
integer attributes
atom dest
atom origin
atom pDword, pCSBI, pSMALLRECT, pCHARINFO

#ilASM{
    [ELF32]
        pop al
    [ELF64]
        pop al
    []
      }
    if not Sinit then initScroll() end if
    pDword = allocate(4)
    pCSBI = allocate(sizeof_CSBI)
    pSMALLRECT = allocate(sizeof_SMALL_RECT)
    pCHARINFO = allocate(sizeof_CHAR_INFO)

    if not c_func(xGetConsoleScreenBufferInfo,{stdout, pCSBI}) then ?9/0 end if
    right = peek2u(pCSBI+CSBI_SIZEX)-1
--  poke2(pSMALLRECT,{0,top-1,right,bottom-1})
    poke2(pSMALLRECT,{0,top-1,right,bottom-1-(amount<0)})
    attributes = peek2u(pCSBI+CSBI_ATTR)
    poke2(pCHARINFO,{' ',attributes})
    if abs(amount)>abs(bottom-top) then
        for i=top to bottom do
            origin = (i-1)*#10000   -- a COORD of {0,i-1}
            if not c_func(xFillConsoleOutputCharacter,{stdout,' ',right,origin,pDword}) then ?9/0 end if
            if not c_func(xFillConsoleOutputAttribute,{stdout,attributes,right,origin,pDword}) then ?9/0 end if
        end for
    else
        dest = (top-1-amount)*#10000    -- a COORD of {0,top-1-amount}
        if not c_func(xScrollConsoleScreenBuffer,{stdout,pSMALLRECT,NULL,dest,pCHARINFO}) then ?9/0 end if
    end if
    free(pCHARINFO)
    free(pSMALLRECT)
    free(pCSBI)
    free(pDword)
end procedure

--/*
from be_machine.c: (honestly, not worth it) [DEV/doc]
#ifdef EUNIX
        short c1;
        short r1;
        int t;
        int i;
        int j;
        int b;
        int prev_t;
        int prev_b;
        int fg;
        int newl;
        int bg;
        char c;
        char linebuff[200 + 1];
        int lbi;
        // save the current position
        r1 = screen_line;
        c1 = screen_col;
        fg = current_fg_color;
        bg = current_bg_color;
        prev_t = -1;
        prev_b = -1;
        if (abs(amount) > abs(bottom - top)) {
                // clear the window
                blank_lines(top-1, bottom-top+1);
        }
        else if (amount > 0) {
                // copy some lines up
                for (i = top; i <= bottom - amount; i++) {
                        SetPosition(i, 1);
                        newl = i - 1;
                        lbi = 0;
                        for (j = 0; j < col_max; j++) {
                                screen_image[newl][j] = screen_image[newl+amount][j];
                                t = screen_image[newl][j].fg_color;
                                b = screen_image[newl][j].bg_color;
                                if (t != prev_t) {
                                        if (lbi) {
                                          linebuff[lbi] = 0;
                                          iputs(linebuff , stdout);
                                          iflush(stdout);
                                          lbi = 0;
                                        }
                                        SetTColor(t);
                                        prev_t = t;
                                }
                                if (b != prev_b) {
                                        if (lbi) {
                                          linebuff[lbi] = 0;
                                          iputs(linebuff , stdout);
                                          iflush(stdout);
                                          lbi = 0;
                                        }
                                        SetBColor(b);
                                        prev_b = b;
                                }
                                c = screen_image[newl][j].ascii;
                                if (c == 0)
                                  c = ' ';
                                linebuff[lbi] = c;
                                lbi++;
                        }
                        linebuff[lbi] = 0;
                        iputs(linebuff , stdout);
                        iflush(stdout);
                }
                // put blank lines at bottom
                SetBColor(bg);
                blank_lines(bottom-amount, amount);
                iflush(stdout);
        }
        else if (amount < 0) {
                // copy some lines down
                for (i = bottom; i >= top-amount; i--) {
                        SetPosition(i, 1);
                        newl = i - 1;
                        lbi = 0;
                        for (j = 0; j < col_max; j++) {
                                screen_image[newl][j] = screen_image[newl+amount][j];
                                t = screen_image[newl][j].fg_color;
                                b = screen_image[newl][j].bg_color;
                                if (t != prev_t) {
                                        if (lbi) {
                                          linebuff[lbi] = 0;
                                          iputs(linebuff , stdout);
                                          iflush(stdout);
                                          lbi = 0;
                                        }
                                        SetTColor(t);
                                        prev_t = t;
                                }
                                if (b != prev_b) {
                                        if (lbi) {
                                          linebuff[lbi] = 0;
                                          iputs(linebuff , stdout);
                                          iflush(stdout);
                                          lbi = 0;
                                        }
                                        SetBColor(b);
                                        prev_b = b;
                                }
                                c = screen_image[newl][j].ascii;
                                if (c == 0)
                                  c = ' ';
                                linebuff[lbi] = c;
                                lbi++;
                        }
                        linebuff[lbi] = 0;
                        iputs(linebuff , stdout);
                        iflush(stdout);
                }
                // put blanks lines at top
                SetBColor(bg);
                blank_lines(top-1, -amount);
                iflush(stdout);
        }
        // restore the current position
        SetPosition(r1, c1);
        SetTColor(fg); // bg will be restored already
        current_fg_color = fg;
        current_bg_color = bg;
#endif
--*/
