--
-- pscreen.e
--
--  Phix implementations of get_screen_char, put_screen_char,   \  from
--                       display_text_image, save_text_image,   / image.e
--                       cursor, and video_config.              - graphics.e
--
-- *NB* in Phix, you need not "include graphics.e" before using
--      video_config, NOR do you ever need to "include pscreen.e";
--      it finds and loads this file for you automagically. Same
--      deal for the other four routines listed above.
--
--  Moved from image.e/graphics.e for the benefit of pmach.e
--
--  In both Phix and RDS Eu you can (if you want) still code eg:
--      include image.e as image
--      image:put_screen_char(i1,i2,s)
--
--  a) The Phix versions of image/graphics.e contain the line
--      "--/**/ include builtins\pscreen.e"
--      Obviously, RDS Eu ignores that line/treats it as a comment
--      whereas Phix includes the file. Also, unlike RDS Eu, in Phix
--      namespaces apply equally to sub-includes.
--
--  b) RDS Eu will, of course, run the versions of the above five
--      as found in image/graphics.e (inside "--/*" "--*/" pairs),
--      whereas Phix will, of course, execute the ones below.
--
--  c) In Phix, calls to eg machine_func(M_PUT_SCREEN_CHAR) are not 
--      supposed to occur, however if an application includes an old 
--      version (say RDS Eu 2.4) of image.e, then pmach.e maps it to
--      here. Now, if we left this in image.e, and pmach.e (naively) 
--      mapped M_PUT_SCREEN_CHAR to image:put_screen_char(), it would 
--      result in an infinite loop. Hence I moved any such affected 
--      routines, but kept everything as compatible as possible.
--

integer iinit iinit = 0

atom xKernel32,
xAllocConsole,xGetStdHandle,
xReadConsoleOutput,xWriteConsoleOutput,
xGetConsoleScreenBufferInfo,xGetLastError,
xSetConsoleCursorInfo


--object void

atom stdout
--, stdin, stderr

constant STD_OUTPUT_HANDLE  = -11   -- #FFFFFFF5
--       STD_INPUT_HANDLE   = -10,  -- #FFFFFFF6
--       STD_ERROR_HANDLE   = -12

-- CONSOLE_CURSOR_INFO structure (xCCI):
constant
    CCI_dwSize   = 0,   --  DWORD  dwSize; 
    CCI_bVisible = 4,   --  BOOL   bVisible; 
    sizeof_CCI   = 8

-- CONSOLE_SCREEN_BUFFER_INFO structure (xCSBI):
constant
    CSBI_SIZEX  = 0,    --  COORD      dwSize; 
    CSBI_SIZEY  = 2,
--  CSBI_CPOSX  = 4,    --  COORD      dwCursorPosition; 
--  CSBI_CPOSY  = 6,
--  CSBI_ATTR   = 8,    --  WORD       wAttributes; 
    CSBI_WINX1  = 10,   --  SMALL_RECT srWindow; 
    CSBI_WINY1  = 12,
    CSBI_WINX2  = 14,
    CSBI_WINY2  = 16,
--  CSBI_MAXX   = 18,   --  COORD      dwMaximumWindowSize; 
--  CSBI_MAXY   = 20,
    sizeof_CSBI = 22

-- Linux (DEV)
--constant MAX_LINES = 100
--constant MAX_COLS  = 200  -- 300? (see text_point() below)


procedure initI()
--atom C_PTR
--  C_PTR = C_POINTER
--DEV locking as per pprntf.e
    if platform()=WINDOWS then
        xKernel32 = open_dll("kernel32.dll")

--#without reformat
        xAllocConsole = define_c_func(xKernel32,"AllocConsole",
            {},         --  no parameters
            C_INT)      -- BOOL

        xGetStdHandle = define_c_func(xKernel32,"GetStdHandle",
            {C_UINT},   --  DWORD  nStdHandle   // input, output, or error device
            C_INT)      -- HANDLE

        xReadConsoleOutput = define_c_func(xKernel32,"ReadConsoleOutputA",
            {C_PTR,     --  HANDLE  hConsoleOutput, // handle of a console screen buffer
             C_PTR,     --  PCHAR_INFO  pchiDestBuffer, // address of buffer that receives data
             C_UINT,    --  COORD  coordDestBufferSize, // column-row size of destination buffer
             C_UINT,    --  COORD  coordDestBufferCoord, // upper-left cell to write to
             C_PTR},    --  PSMALL_RECT  psrctSourceRect // address of rectangle to read from
            C_INT)      -- BOOL

        xWriteConsoleOutput = define_c_func(xKernel32,"WriteConsoleOutputA",
            {C_PTR,     --  HANDLE  hConsoleOutput, // handle of a console screen buffer
             C_PTR,     --  PCHAR_INFO  pchiSrcBuffer,  // address of buffer with data to write
             C_UINT,    --  COORD  coordSrcBufferSize,  // column-row size of source buffer
             C_UINT,    --  COORD  coordSrcBufferCoord, // upper-left cell to write from
             C_PTR},    --  PSMALL_RECT  psrctDestRect  // address of rectangle to write to
            C_INT)      -- BOOL

        xGetConsoleScreenBufferInfo = define_c_func(xKernel32,"GetConsoleScreenBufferInfo",
            {C_PTR,     --  HANDLE  hConsoleOutput, // handle of console screen buffer
             C_PTR},    --  PCONSOLE_SCREEN_BUFFER_INFO  // address of screen buffer info
            C_INT)      -- BOOL

        xSetConsoleCursorInfo = define_c_func(xKernel32,"SetConsoleCursorInfo",
            {C_PTR,     --  HANDLE  hConsoleOutput, // handle of console screen buffer
             C_PTR},    --  PCONSOLE_CURSOR_INFO  // address of cusrsor information
            C_INT)      -- BOOL

        xGetLastError = define_c_func(xKernel32,"GetLastError",
            {},
            C_INT)      -- DWORD
--#with reformat

        {} = c_func(xAllocConsole,{}) -- if we already have one, fail, we don't care.
--      stdin = c_func(xGetStdHandle,{STD_INPUT_HANDLE})
        stdout = c_func(xGetStdHandle,{STD_OUTPUT_HANDLE})
--      stderr = c_func(xGetStdHandle,{STD_ERROR_HANDLE})
--      ftable[1] = {stdin,#01}  -- read access
--      ftable[2] = {stdout,#06} -- write+binary
--      ftable[3] = {stderr,#06} -- write+binary
--      -- set initial foreground and background colours
--      getConsoleScreenBufferInfo()
--      fg_colour = peek2u(!xCSBI+CSBI_ATTR)
--      bg_colour = and_bits(fg_colour,#F0)/#10
--      fg_colour = and_bits(fg_colour,#F)

    elsif platform()=LINUX then
        stdout = 1
        ?9/0    -- incomplete
    else
        ?9/0
    end if
    iinit = 1
end procedure

-- copied from image.e:
type positive_atom(atom x)
    return x>=1
end type

global function get_screen_char(positive_atom line, positive_atom column)
-- returns {character, attributes} of the single character
-- at the given (line, column) position on the screen
atom lpBuffer
object res
atom xRect
    if not iinit then initI() end if
    if platform()=WINDOWS then
        xRect = allocate(8)
        poke2(xRect,{column-1,line-1,column-1,line-1})

        lpBuffer = allocate(4)
        if not c_func(xReadConsoleOutput,{stdout,lpBuffer,#10001,0,xRect}) then
--          dbg = c_func(xGetLastError,{})
            -- error --
            res = -1
        else
            res = peek2u({lpBuffer,2})
        end if

        free(lpBuffer)
        free(xRect)
    elsif platform()=LINUX then
        ?9/0    --DEV
    else
        ?9/0
    end if
    return res
end function

global procedure put_screen_char(positive_atom line, positive_atom column, sequence char_attr)
-- stores {character, attributes, character, attributes, ...} 
-- of 1 or more characters at position (line, column) on the screen
-- (nb: all characters get shown on the same line, see also display_text_image)
atom lpBuffer
integer x
atom xRect

    if not iinit then initI() end if
    if platform()=WINDOWS then
        xRect = allocate(8)

        x = length(char_attr)

        poke2(xRect,{column-1,line-1,column+x-2,line-1})

        lpBuffer = allocate(x*2)
        poke2(lpBuffer,char_attr)

        if not c_func(xWriteConsoleOutput,{stdout,lpBuffer,#10000+floor(x/2),0,xRect}) then
--          dbg = c_func(xGetLastError,{})
            -- error --
        end if

        free(lpBuffer)
        free(xRect)
    elsif platform()=LINUX then
        ?9/0 -- DEV
    else
        ?9/0
    end if
end procedure

-- copied from image.e:
type text_point(sequence p)
--object w
    if length(p)!=2 then return false end if
--  w = p[1]
--9/5/15:
--  if not integer(w) or w<1 or w>200 then return 0 end if
--  if not integer(w) or w<1 or w>300 then return 0 end if
--  if not integer(w) or w<1 or w>500 then return 0 end if
--  w = p[2]
--  return integer(w) and w>=1 and w<=500
--15/2/19: (see readme.txt)
-- 65535 based on the x+y*#10000 thing/must be 16-bit words...
    object {x,y} = p
    return integer(x) and x>=1 and x<=65535
       and integer(y) and y>=1 and y<=65535
end type

global function save_text_image(text_point top_left, text_point bottom_right)
--
--  The two parameters are {line,column}, aka {y,x}.
--
integer leftcol, rightcol, topline, lastline, x, y
atom lpBuffer
object res
atom xRect
    if not iinit then initI() end if

    if platform()=WINDOWS then
        xRect = allocate(8)
        topline = top_left[1]-1
        lastline = bottom_right[1]-1
        leftcol = top_left[2]-1
        rightcol = bottom_right[2]-1
        poke2(xRect,{leftcol,topline,rightcol,lastline})

        x = rightcol-leftcol+1
        y = lastline-topline+1

        lpBuffer = allocate(x*y*4)
        if not c_func(xReadConsoleOutput,{stdout,lpBuffer,x+y*#10000,0,xRect}) then
--          dbg = c_func(xGetLastError,{})
            -- error --
            res = -1
        else
            res = repeat(0,y)
            for i=1 to y do
                res[i] = peek2u({lpBuffer+(i-1)*x*4,x*2})
            end for
        end if

        free(lpBuffer)
        free(xRect)
    elsif platform()=LINUX then
        ?9/0 --DEV
    else
        ?9/0
    end if
    return res
end function

global procedure display_text_image(text_point top_left, sequence image)
integer topline, lastline, leftcol, rightcol, x, y
atom lpBuffer
atom xRect
    if not iinit then initI() end if
    if platform()=WINDOWS then
        xRect = allocate(8)
        x = length(image[1])/2
        y = length(image)
        topline = top_left[1]-1
        lastline = topline+y-1
        leftcol = top_left[2]-1
        rightcol = leftcol+x-1
        poke2(xRect,{leftcol,topline,rightcol,lastline})

        lpBuffer = allocate(x*y*4)
        for i=1 to y do
            poke2(lpBuffer+(i-1)*x*4,image[i])
        end for
--dbg = peek({lpBuffer,x*y*4})
--trace(1)
--dbg = peek({xRect,8})

        if not c_func(xWriteConsoleOutput,{stdout,lpBuffer,x+y*#10000,0,xRect}) then
--          dbg = c_func(xGetLastError,{})
            -- error --
        end if
--dbg = peek({xRect,8})

        free(lpBuffer)
        free(xRect)
    elsif platform()=LINUX then
        ?9/0 -- DEV
    else
        ?9/0
    end if
end procedure

global procedure cursor(integer style)
-- choose a cursor style
    if platform()=WINDOWS then
        if not iinit then initI() end if
        atom xCCI = allocate(sizeof_CCI)
        poke4(xCCI+CCI_bVisible,(style!=0x02000))
        if    style=#0607 then style = 12
        elsif style=#0507 then style = 25
        elsif style=#0407 then style = 50
        else                   style = 100
        end if
        poke4(xCCI+CCI_dwSize,style)
        {} = c_func(xSetConsoleCursorInfo,{stdout,xCCI})
        free(xCCI)
    elsif platform()=LINUX then
        -- Documented as doing nothing on Linux (==OE)
    else
        ?9/0
    end if
end procedure


global function video_config()
-- (supported for VC_LINES/VC_COLUMNS and VC_SCRNLINES/VC_SCRNCOLS use only.)
integer x,y,mx,my
sequence res
atom xCSBI
    puts(1,"")
    if not iinit then initI() end if
    if platform()=WINDOWS then
        xCSBI = allocate(sizeof_CSBI)
        if c_func(xGetConsoleScreenBufferInfo,{stdout,xCSBI}) then

            -- nb this is the display size; the buffer size is CSBI_SIZEX,CSBI_SIZEY
            x = peek2u(xCSBI+CSBI_WINX2)-peek2u(xCSBI+CSBI_WINX1)+1
            y = peek2u(xCSBI+CSBI_WINY2)-peek2u(xCSBI+CSBI_WINY1)+1
            mx = peek2u(xCSBI+CSBI_SIZEX)
            my = peek2u(xCSBI+CSBI_SIZEY)
            res = {1,       -- VC_COLOR, always assumed true
                   3,       -- VC_MODE, text mode always assued
                   my,      -- VC_LINES     \ (these are the only reason why
                   mx,      -- VC_COLUMNS   /  I bothered to support this)
                   0,       -- VC_XPIXELS, text mode always assumed
                   0,       -- VC_YPIXELS, text mode always assumed
                   32,      -- VC_NCOLORS, ("" [??])
                   1,       -- VC_PAGES, assume 1. Although more can be present, 
                            --  the only way to switch is by passing the handles 
                            --  returned from CreateConsoleScreenBuffer to 
                            --  SetConsoleActiveScreenBuffer, as opposed to say
                            --  the DOS-only set_display_page(n).
                   y,       -- VC_SCRNLINES
                   x}       -- VC_SCRNCOLS
        else
            res = {c_func(xGetLastError,{})}
        end if
        free(xCSBI)
    elsif platform()=LINUX then
        ?9/0 --DEV
--      res = {line_max,col_max}
    else
        ?9/0
    end if
    return res
end function


