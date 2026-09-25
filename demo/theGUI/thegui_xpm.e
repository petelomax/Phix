--
-- theGUI\thegui_xpm.e
-- ===================
--
--  xpm handling, in other words small icons defined in a textual format. 
--    """ is baked right into GTK, so that part's a whole lot simpler.
--
--  To avoid making anything global, logs itself via gSetGlobal(),
--           and uses a slightly fiddly custom tg_xpm_callback().
--
-- (Aside: trailing _ used several times in this file 
--         to avoid clashing with theGUI.e under p2js)
local constant BI_RGB_ = 0  

local integer idBITMAPINFOHEADER_ = define_struct(`typedef struct tagBITMAPINFOHEADER {
                                                    DWORD   biSize;
                                                    LONG    biWidth;
                                                    LONG    biHeight;
                                                    WORD    biPlanes;
                                                    WORD    biBitCount;
                                                    DWORD   biCompression;
                                                    DWORD   biSizeImage;
                                                    LONG    biXPelsPerMeter;
                                                    LONG    biYPelsPerMeter;
                                                    DWORD   biClrUsed;
                                                    DWORD   biClrImportant;
                                                  } BITMAPINFOHEADER, *PBITMAPINFOHEADER;`),

              idRGBQUAD = define_struct(`typedef struct tagRGBQUAD {
                                           BYTE rgbBlue;
                                           BYTE rgbGreen;
                                           BYTE rgbRed;
                                           BYTE rgbReserved;
                                         } RGBQUAD;`)

local function tg_hexstr_to_int(string c2)
    -- convert eg "1F" to 31
    integer res = 0
    for c in c2 do
        c -= iff(c<='9'?'0':iff(c<='Z'?'A':'a')-10)
        res = res*16+c
    end for
    return res
end function    
--assert(tg_hexstr_to_int("1F")==31)

local function tg_get_colour_table_(sequence xpm, integer nFrames=1)
--bool bGreyScale = false
    integer {width,height,colours,codeWide} = apply(split(xpm[1]),to_integer),
            nTrans = 0
    -- create a palatte and data:
    -- cc is colour codes, eg 'x' ==> palette[7] (ie cc[7] would be 'x').
    -- cc[i] may be char (if codeWide=1) or string (if >1).
    -- colour code map of <charsWide> chars --> palette index
    sequence cc = repeat(0,colours),
          cused = {0}

    -- read the colour data (number of colours determined from line 1, 3rd number)
    for i=1 to colours do
        string data = xpm[i+1]
        -- extract the colour code          (eg "c" in "c None")
        if codeWide=1 then
            integer cci = data[codeWide]
            assert(cci>' ',"no spaces or tabs",nFrames:=nFrames)
            cc[i] = cci
        else
            string cci = data[1..codeWide]
            assert(min(cci)>' ',"no spaces or tabs",nFrames:=nFrames)
            cc[i] = cci
        end if
        assert(find(cc[i],cc)=i,"duplicate colour code!") -- (GTK don't like that)
        -- and the colour itself            (eg "None" in "c None")
        bool bOK = false
        for j=codeWide+2 to length(data) do
            if data[j]='c' and data[j+1]=' ' then
                for k=j+2 to length(data) do
                    if data[k]>' ' then
                        bOK = true
                        data = lower(data[k..length(data)])
                        for l=1 to length(data) do
                            if data[l]<=' ' then
                                if l>=length(data)-1
                                or data[l+1]<=' '
                                or data[l+2]<=' ' then
                                    data = data[1..l-1]
                                    exit --j
                                end if
                            end if
                        end for
                        exit --j
                    end if
                end for
                exit
            end if
        end for
        assert(bOK)
        -- convert to an {r,g,b} code
        if data[1]='#' then
            if length(data)=7 then
                -- hex tuple: #rrggbb
                integer rr = tg_hexstr_to_int(data[2..3]),
                        gg = tg_hexstr_to_int(data[4..5]),
                        bb = tg_hexstr_to_int(data[6..7])
--              if bGreyScale then 
----                rr = floor((rr+gg+bb)/3)
--                  rr = floor(sqrt((rr+gg+bb)/765)*255)
--                  gg = rr
--                  bb = rr
--              end if
                cused &= rr*#10000+gg*#100+bb
            elsif length(data)=9 then
                -- #aarrggbb
                integer aa = tg_hexstr_to_int(data[2..3]),
                        rr = tg_hexstr_to_int(data[4..5]),
                        gg = tg_hexstr_to_int(data[6..7]),
                        bb = tg_hexstr_to_int(data[8..9])
                    cused &= aa*#1000000+rr*#10000+gg*#100+bb
            else
                ?9/0
            end if
        elsif data="none" then
            assert(nTrans=0)
            assert(i=1) -- (added 16/11/23, for )
            nTrans = i
        else
            crash("hex colours only!",nFrames)
        end if
    end for
    return {cc, cused, nTrans}
end function

local function tg_winAPI_create_DIB_from_xpm_(sequence xpm, integer callback, cTrans=#FFFFFF, bSection=false, nFrames=1)
if not bSection then ?9/0 end if -- a previous daft idea for space saving, removed when I realised just how badly 
-- CLIPIMG needs it gone, and should be purged, especially if we ever want any of the Arwen32dibdemo code to work!
--bool bGreyScale = false
-- (WinAPI only, GTK uses gdk_pixbuf_new_from_xpm_data() instead)
--  ... whereas demo\rosetta\virtunome.exw uses IupImageRGBA() ... [DEV]
--
-- xpm is a [local constant] sequence of the form:
--      constant xpm = {"5 5 2 1",
--                      ". c None",
--                      "x c #000000",
--                      ".....",
--                      ".x.x.",
--                      "..x..",
--                      ".x.x.",
--                      "....."}
--  Obviously the one passed can be any size and number of colours, but
--  the one shown describes a 5x5 bitmap with 2 colours, one char per pixel,
--  and as you can hopefully see above is a small 'X'.
--
-- returns the dibHandle, or crashes.
--
    assert(platform()=WINDOWS)

    -- extract the values: "<wide> <tall> <colours> <chars per pixel> [<hotspot x> <y>]"
    integer {width,height,colours,codeWide} = apply(split(xpm[1]),to_integer)
--  assert(rmdr(width,codeWide)==0)

    -- palette and packing size
--/!*
--  integer --palSize,  -- colours rounded up to supported sizes
    integer bpp         -- bits per pixel (1, 4, 8)
    if bSection then                    /*palsize = 0*/     bpp = 32
    elsif colours<=2 then               /*palSize = 2*/     bpp = 1
    elsif colours<=16 then              /*palSize = 16*/    bpp = 4
    elsif colours<=256 then             /*palSize = 256*/   bpp = 8
--  elsif colours<=65536 then   ??      /*palSize = 65536*/ bpp = 16
--*!/
--  integer palSize = 256,  -- colours rounded up to supported sizes
--          bpp = 8         -- bits per pixel (1, 4, 8)
--  if colours>256 then
    else
        crash("too many colours",nFrames)
    end if
--?{"palSize",2,"bpp",bpp,"colours",colours}

    -- calculate the size of the BITMAPINFO header
--  if idBITMAPINFOHEADER_=0 then set_idBITMAPINFOHEADER() end if
    integer headerSize = get_struct_size(idBITMAPINFOHEADER_) + 
--DEV tryme... (change made 12/12/24 w/o testing, see what happens... remove palSize if ok)
--                      (get_struct_size(idRGBQUAD) * palSize)
--                      (get_struct_size(idRGBQUAD) * colours)
                        iff(bSection?0:get_struct_size(idRGBQUAD)*colours)
    --
    -- Calculate the memory needed for the DIB,
    -- by first calculating the length of a packed and padded line,
    -- which we may as well create now for later use.
    --
    -- setup packed input storage:
    sequence pi,            -- one line as colour indexes
             packed         -- "" packed as per bpp and padded to dword boundary
    pi = repeat(0,width)    -- an array of palette indexes, one per pixel in the line.

    -- setup packed output storage:
    integer bytes           -- loop limit for packing scanlines, [ *1 | /2 | /8 ] see code
    if bpp = 32 then        -- no palette
        bytes = width*4
    elsif bpp = 8 then      -- 1:1, 1 byte per pixel
        bytes = width
    elsif bpp = 4 then      -- 2:1, 2 pixels per byte
--      if remainder(width,2) then pi = append(pi,0) end if             -- pad to multiple of 2
        if rmdr(width,2) then pi &= 0 end if                            -- pad to multiple of 2
--      bytes = floor(length(pi) / 2)
        bytes = length(pi)/2
    elsif bpp = 1 then      -- 8:1, 8 pixels per byte (monochrome)
--      while remainder(length(pi),8) do pi = append(pi,0) end while    -- pad to multiple of 8
        while rmdr(length(pi),8) do pi &= 0 end while                   -- pad to multiple of 8
--      bytes = floor(length(pi) / 8)
        bytes = length(pi)/8
    end if

    -- scanline needs to fall on a dword boundary (4 bytes)
    if remainder(bytes, 4) then
        packed = repeat(0, bytes + 4-remainder(bytes, 4))
    else
        packed = repeat(0, bytes)
    end if

    integer pwidth = length(packed),
--      bitMapSize = height*pwidth
        bitMapSize = iff(bSection?0:height*pwidth)

    -- Allocate memory for header and DIB in one block
    atom pBIH = allocate(headerSize+bitMapSize)

    -- build the bitmap info header
    set_struct_field(idBITMAPINFOHEADER_,pBIH,"biSize",get_struct_size(idBITMAPINFOHEADER_))
    set_struct_field(idBITMAPINFOHEADER_,pBIH,"biWidth",width)          -- Width in pixels.
--  set_struct_field(idBITMAPINFOHEADER_,pBIH,"biHeight",-height)       -- Height in pixels (-ve=topdown).
    set_struct_field(idBITMAPINFOHEADER_,pBIH,"biHeight",height)    -- Height in pixels (-ve=topdown).
    set_struct_field(idBITMAPINFOHEADER_,pBIH,"biPlanes",1)         -- 1 colour plane. (always)
    set_struct_field(idBITMAPINFOHEADER_,pBIH,"biBitCount",bpp)     -- no. of bits per pixel (0, 1, 4, 8, 16, 24, 32)
    set_struct_field(idBITMAPINFOHEADER_,pBIH,"biCompression",BI_RGB_)  -- compression format - none in this case
    set_struct_field(idBITMAPINFOHEADER_,pBIH,"biSizeImage",0)          -- size in bytes of image, set to 0 (unneeded) if no compression
    set_struct_field(idBITMAPINFOHEADER_,pBIH,"biXPelsPerMeter",0)      -- Not needed
    set_struct_field(idBITMAPINFOHEADER_,pBIH,"biYPelsPerMeter",0)      -- Not needed
    set_struct_field(idBITMAPINFOHEADER_,pBIH,"biClrUsed",colours)      -- colours actually used by the bitmap
    set_struct_field(idBITMAPINFOHEADER_,pBIH,"biClrImportant",0)       -- no. colours needed to display image, 0=all

    -- get the address of the first rgb tuple
    atom mbPtr = pBIH+get_struct_size(idBITMAPINFOHEADER_)

    {sequence cc, sequence cused, integer nTrans} = tg_get_colour_table_(xpm, nFrames+1)

    if nTrans=0 then
        cTrans = #000000
    else
        -- find an unused colour, any of the 16,777,215
        --    which are not TG_BLACK aka NULL will do:
        while find(cTrans,cused,2) do cTrans -= 1 end while
--SUG/ALT: (untested, would mean we can ditch the parameter...)
--      cTrans = TG_MAGENTA
--      while find(cTrans,cused) do
--          cTrans = iff(cTrans=TG_MAGENTA?TG_WHITE:rand(#FFFFFFFF))
--      end while
--  or:
--      **MUST** be non-existent, to deal with imagelists correctly...
--          (even if this specific image does not have a 'None')
--          (and the default on gImage() should be TG_MAGENTA)
--  or: (hmmm...)
--      integer k = find(cTrans,cused), nearest
--      if k then
--          for i=1 to length(cused)+3 do -- [it'll always suffice to check for >n missing colours]
--              nearest = cTrans-i;  if nearest>#000000 and not find nearest,cused) then exit end if
--              nearest = cTrans+i;  if nearest<#FFFFFF and not find nearest,cused) then exit end if
--          end for
--          cused[k] = nearest
--      end if

        cused[1] = cTrans
    end if
    if not bSection then
?9/0 -- dead code, I think:
        for c in cused from iff(nTrans?1:2) do
            integer {rr,gg,bb,aa} = to_rgba(c)
            poke(mbPtr,{bb,gg,rr,aa})
            mbPtr += 4
        end for
    end if
    assert(length(xpm)=height+colours+1,"length(xpm)!=height",nFrames:=nFrames)

    mbPtr = pBIH+headerSize

--  atom hDIB, pMem=NULL
    atom hDIB, pMem=mbPtr
--  if bSection then
    {hDIB,pMem} = callback({"NEWSECT",{pBIH,headerSize}})
--  {hDIB,pMem,res} = callback({"NEWSECT",{pBIH,headerSize}})
        mbPtr = pMem
--      pwidth *= 4
--  end if

    -- convert the text into indexes
--  for i=1 to height do
    for i=height to 1 by -1 do
        string data = xpm[i+1+colours]  -- get a line
        assert(length(data)=width*codeWide)
        --
        -- convert XPM chars (or pairs/sets of chars if charsWide>1)
        -- into palette indexes, eg 'X' --> 7, if X was the 7th colour
        -- listed in the colours section:
        --  
        integer k = 1
        for j=1 to width do
            object code = iff(codeWide=1?data[k]:data[k..k+codeWide-1])
            k += codeWide
            integer pij = find(code, cc)
            assert(pij!=0,`invalid/undefined colour code "%s"`,{code&""},nFrames)
            if bSection then
                pi[j] = cused[pij]
            else
                pi[j] = pij-1 -- (0-based)
            end if
        end for
        --
        -- Now convert this array of colour indices into a packed scanline:
        --
        if bpp=32 then -- (bSection==true)
            poke4(mbPtr, pi)
        elsif bpp=8 then -- 1:1
-- removed 3/1/25... (why was it ever so?? -- ah, just poke pi!)
--          packed = pi
--          packed = deep_copy(pi)
            poke(mbPtr, pi)
        else
            integer oidx = 1
            if bpp=4 then -- 4:1
                -- 4 bits per; 2 numbers per byte
                for j=1 to bytes*2 by 2 do
                    packed[oidx] = (pi[j]   * 16) +
                                   (pi[j+1] * 1)
                    oidx += 1
                end for
            elsif bpp=1 then -- 8:1
                -- 1 bits per; 8 numbers per byte
                for j=1 to bytes*8 by 8 do
                    packed[oidx] = (pi[j]   * 128) +
                                   (pi[j+1] * 64) +
                                   (pi[j+2] * 32) +
                                   (pi[j+3] * 16) +
                                   (pi[j+4] * 8) +
                                   (pi[j+5] * 4) +
                                   (pi[j+6] * 2) +
                                   (pi[j+7] * 1)
                    oidx += 1
                end for
            end if
            poke(mbPtr, packed)
        end if
        mbPtr += pwidth
    end for
    free(pBIH)
    return {hDIB,pMem,width,height,cTrans}
end function

local function tg_winAPI_create_DIB_(integer width, height)
    --
    -- Creates a new 32-bit bitmap of the desired width and height.
    --
    integer headerSize = get_struct_size(idBITMAPINFOHEADER_)
    atom pBIH = allocate(headerSize+width*height*4)
    set_struct_field(idBITMAPINFOHEADER_,pBIH,"biSize",headerSize)
    set_struct_field(idBITMAPINFOHEADER_,pBIH,"biWidth",width)          -- Width in pixels.
    set_struct_field(idBITMAPINFOHEADER_,pBIH,"biHeight",-height)       -- Height in pixels (-ve=topdown).
    set_struct_field(idBITMAPINFOHEADER_,pBIH,"biPlanes",1)             -- 1 colour plane. (always)
    set_struct_field(idBITMAPINFOHEADER_,pBIH,"biBitCount",32)          -- no. of bits per pixel (0, 1, 4, 8, 16, 24, 32)
    set_struct_field(idBITMAPINFOHEADER_,pBIH,"biCompression",BI_RGB_)  -- compression format - none in this case
    set_struct_field(idBITMAPINFOHEADER_,pBIH,"biSizeImage",0)          -- size in bytes of image, set to 0 (unneeded) if no compression
    set_struct_field(idBITMAPINFOHEADER_,pBIH,"biXPelsPerMeter",0)      -- Not needed
    set_struct_field(idBITMAPINFOHEADER_,pBIH,"biYPelsPerMeter",0)      -- Not needed
    set_struct_field(idBITMAPINFOHEADER_,pBIH,"biClrUsed",0)            -- colours actually used by the bitmap
    set_struct_field(idBITMAPINFOHEADER_,pBIH,"biClrImportant",0)       -- no. colours needed to display image, 0=all

    return {pBIH,headerSize} -- (NOT automatically freed)
end function

-- for gDatePick:
local constant dp_xpm_txt = """
22 14 11 1
. c None
d c #4D6185
a c #AFA8A6
b c #776B68
e c #FFFFFF
f c #EFF2F9
g c #EAEFF7
i c #F7F7F7
p c #C8C8C8
t c #958E8C
u c #5D5452
......................
.abbbbbbbbba..........
.bepppppppfb..........
.beeeeeeeegb..........
.bepppppppgb..........
.bepgpgpgpgb..ddddddd.
.bepppppppgb...ddddd..
.bepgpgpgpgb....ddd...
.bepppppppgb.....d....
.bepgpgpgpgb..........
.bfppppppubb..........
.bfffffffbt...........
.abbbbbbbb............
......................
"""

-- for gTabs:
local constant xtxt = """
9 9 2 1
. c None
x c #000000
.........
.........
..x...x..
...x.x...
....x....
...x.x...
..x...x..
.........
.........
""",		   ltxt = """
7 9 2 1
. c None
x c #000000
.......
.......
....x..
...xx..
..xxx..
...xx..
....x..
.......
.......
""",		   rtxt = """
7 9 2 1
. c None
x c #000000
.......
.......
..x....
..xx...
..xxx..
..xx...
..x....
.......
.......
"""


-- W11 scrollbar uparrow and a bold/hover version of it
local constant ua = """
10 10 9 1
. c None
a c #dcdcdc
b c #8f8f8f
d c #ececec
e c #959595
f c #858585
g c #b2b2b2
h c #d2d2d2
i c #919191
..........
..........
...abba...
..deffed..
..gffffg..
.hffffffh.
.iffffffi.
.effffffe.
..........
.........."""
local constant ub = """
10 10 10 1
. c None
a c #efefef
b c #c7c7c7
d c #a7a7a7
e c #666666
f c #cecece
g c #767676
h c #989898
i c #aaaaaa
j c #bcbcbc
..........
...abba...
...deed...
..feeeef..
.ageeeega.
.heeeeeeh.
beeeeeeeeb
ieeeeeeeei
ajjjjjjjja
.........."""

local function tg_create_scroll_arrows()
--/*
    sequence xpma = split(ua,'\n'),
             xpmb = split(ub,'\n'),
             res = {}
    for i=1 to 4 do
        if i>1 then -- create left/down/right by rotating the uparrows.
            xpma = deep_copy(xpma)
            xpma[-10..-1] = reverse(columnize(xpma[-10..-1],defval:=' '))
            xpmb = deep_copy(xpmb)
            xpmb[-10..-1] = reverse(columnize(xpmb[-10..-1],defval:=' '))
        end if
        res = append(res,xpma)
        res = append(res,xpmb)
    end for
--*/
--SUG, untried:
    sequence res = {split(ua,'\n'),     -- uparrow
                    split(ub,'\n')}     --    ""  , bold
    -- create left/down/right [norm|bold] by repeated rotations:
    for i=1 to 6 do
        sequence xpm = deep_copy(res[i])
        xpm[-10..-1] = reverse(columnize(xpm[-10..-1],defval:=' '))
        res = append(res,xpm)
    end for
    -- aside: all 10x10 simplifies both rotation and drawing
    return res -- ua,ub,la,lb,da,db,ra,rb
end function

local constant ucb_ = """
13 13 7 1
. c None
a c #40a9a9a9
b c #6a6a6a
c c #626262
d c #999999
e c #eaeaea
_ c #f3f3f3
.abcccccccba.
ade_______eda
be_________eb
c___________c
c___________c
c___________c
c___________c
c___________c
c___________c
c___________c
be_________eb
ade_______eda
.abcccccccba."""

local constant ccb_ = """
13 13 12 1
. c None
a c #40699fd1
b c #0f68bc
c c #5f9bd3
d c #a0c3e5
e c #5f9bd2
f c #5091ce
g c #609bd3
h c #9fc3e4
i c #dfebf6
j c #1069bc
_ c #005fb8
.ab_______ba.
a___________a
b___________b
_____________
_________c___
________de___
___f___de____
___gh_de_____
____gie______
_____j_______
b___________b
a___________a
.ab_______ba."""

-- unchecked radio button
local constant urb_ = """
13 13 11 1
. c None
a c #20cccccc
b c #8f8f8f
c c #6a6a6a
d c #626262
e c #40a0a0a0
f c #747474
g c #b4b4b4
h c #e0e0e0
i c #999999
_ c #ffffff
...abcdcba...
..efgh_hgfe..
.eih_____hie.
afh_______hfa
bg_________gb
ch_________hc
d___________d
ch_________hc
bg_________gb
afh_______hfa
.eih_____hie.
..efgh_hgfe..
...abcdcba..."""

-- checked radio button (active)
local constant crb_ = """
13 13 8 1
. c None
a c #20b4cce2
b c #4b8cca
c c #0060b0
d c #40689ed0
e c #2068b0
f c #c8d8f0
_ c #ffffff
...abcccba...
..dcccccccd..
.dcccccccccd.
accccccccccca
bcccef_fecccb
ccccf___fcccc
cccc_____cccc
ccccf___fcccc
bcccef_fecccb
accccccccccca
.dcccccccccd.
..dcccccccd..
...abcccba..."""

-- checked radio button (inactive)
local constant irb_ = """
13 13 6 1
. c None
a c #40b4cce2
b c #40689ed0
c c #989898
d c #c8d8f0
_ c #ffffff
...accccca...
..bcccccccb..
.bcccccccccb.
accccccccccca
cccccd_dccccc
ccccd___dcccc
cccc_____cccc
ccccd___dcccc
cccccd_dccccc
accccccccccca
.bcccccccccb.
..bcccccccb..
...accccca..."""

-- note these are subtly different to those on a gTabs (ltxt,rtxt)
local constant cal_ltxt = `
6 9 2 1
. c None
x c #313131
......
....x.
...xx.
..xxx.
.xxxx.
..xxx.
...xx.
....x.
......`
local constant cal_rtxt = `
6 9 2 1
. c None
x c #313131
......
.x....
.xx...
.xxx..
.xxxx.
.xxx..
.xx...
.x....
......`

-- for gSlider:
local constant slider_thumb_txt = """
11 19 3 1
. c None
# c #0078D7
+ c #78B4E4
###########
###########
###########
###########
###########
###########
###########
###########
###########
###########
###########
###########
###########
###########
+#########+
.+#######+.
..+#####+..
...+###+...
....+#+....
"""

local constant slider_vthumb_txt = """
19 11 3 1
. c None
# c #0078D7
+ c #78B4E4
##############+....
###############+...
################+..
#################+.
##################+
###################
##################+
#################+.
################+..
###############+...
##############+....
"""

--DEV bug: if no transparent colour, hProgress gets black stripes!!
-- for gProgressBar:
local constant stripes_txt = """
15 15 3 1
. c None
# c #3184fd
+ c #0d6efd
########+++++++
+########++++++
++########+++++
+++########++++
++++########+++
+++++########++
++++++########+
+++++++########
#+++++++#######
##+++++++######
###+++++++#####
####+++++++####
#####+++++++###
######+++++++##
#######+++++++#
"""

-- for gTree:
-- first line is columns rows colors chars-per-pixel [hotx hoty]?
-- The next "colors" lines are, well colours ('c').
-- The next "rows" lines must be "columns" wide.
local constant T_txt = `
15 17 2 1
. c None
0 c #6d6d6d
....U..........
...............
....U..........
...............
....U..........
...............
....U..........
...............
....U..........
...............
....U..........
...............
....0.R.R.R.R.R
...............
....D..........
...............
....D..........
`

constant MP_txt = `
15 17 7 1
. c None
0 c #6d6d6d
g c #babbbc
h c #919191
i c #fcfcfc
j c #294272
m c #4b63a7
....U..........
...............
....U..........
...............
....U..........
...............
....U..........
...............
ghhhhhhhg......
hiiiiiiih......
hiiikiiih......
hiiikiiih......
himmmmmih.0.0.0
hiiikiiih......
hiiikiiih......
hiiiiiiih......
ghhhhhhhg......
`

local constant dir_closed_xpm = `
16 16 8 1
. c None
_ c #000000
| c #9C9C00
a c #CECE63
b c #FFCE9C
c c #FFFF9C
d c #FFFFCE
e c #F7F7F7
................
...||||_........
..|eddc|_.......
.|aaaaaa||||||..
.|ddddddddddca_.
.|dcccccccccba_.
.|dccccccbcbca_.
.|dcccccccbcba_.
.|dccccbcbcbca_.
.|dcccccbcbcba_.
.|dccccbcbcbba_.
.|dbcbbcbcbbba_.
.|aaaaaaaaaaaa_.
.._____________.
................
................`

local constant dir_open_xpm = `
16 16 6 1
. c None
_ c #000000
| c #9C9C00
a c #CECE63
b c #FFCE9C
c c #FFFF9C
................
...||||.........
..|....|........
.|.cccc.||||||..
.|.cccccc....a_.
.|.cccccccccca_.
|||||||||||aca_.
|.cccccbcba_aa_.
|.ccccccbca_aa_.
.|.ccccbcbb|_|_.
.|.ccbbcbbba_|_.
..|.bccbbbba|__.
..|||||||||||__.
...____________.
................
................
`

local constant dot_xpm = `
16 16 7 1
. c None
a c #808080
b c #A1A1A1
c c #C0C0C0
d c #DEDEDE
e c #636363
f c #383838
................
................
................
................
......aabbb.....
.....abbcdcb....
....eaabbcdcb...
....eaaabbccb...
....feaaabbba...
....feeaaabaa...
.....feeaaaa....
......ffeee.....
................
................
................
................`


local function tg_XPM_from_shorthand_(string name)
    if name="dp_xpm" then return dp_xpm_txt end if -- for gDatePick
    if name="xlr_xpm" then return {xtxt,ltxt,rtxt} end if -- for gTabs
    if name="scroll_arrows" then return tg_create_scroll_arrows() end if -- for scrollbars
    if name="chk_xpm" then return {ucb_,ccb_,urb_,crb_,irb_} end if -- for gCheckbox/radio
    if name="calendar_arrows" then return {cal_ltxt,cal_rtxt} end if 
    if name="slider_thumb" then return {slider_thumb_txt,slider_vthumb_txt} end if
    if name="stripes" then return stripes_txt end if
    if name="tree_images" then return {T_txt,MP_txt} end if
    if name="treeOCD" then return {dir_closed_xpm,dir_open_xpm,dot_xpm} end if
    ?9/0
end function

-- undocumented use of gSetGlobal, avoids unnecessary globals between this file and theGUI.e
gSetGlobal("xpm_init",{tg_XPM_from_shorthand_,
                       tg_winAPI_create_DIB_,
                       tg_winAPI_create_DIB_from_xpm_,
                       tg_get_colour_table_,
                       idBITMAPINFOHEADER_})

-- I found this...:
--;(function($){
--
--
--  $.fn.xpm2canvas = function(obj){
--
--      /**
--      * Configuration Object
--      */
--      var o = $.extend({
--          xpm: [],
--          background: "#ffffff",
--          afterEnd: function(instObject){ 
--              
--              if(o.accessibilityEnabled)
--                  o.handleAccessibility(instObject);
--          },
--          beforeStart:function(instObject){
--              
--          },
--          handleAccessibility: function(instObject){
--          
--          },
--          accessibilityEnabled: false,
--          debug: true
--      }, obj || {}), c, imageData, col;
--      
--      /**
--      * Function Debuger to show console logs
--      */
--      function debuger(){
--          if(o.debug)
--              try{console.log.apply('',arguments);} catch(e) {}
--      }
--      
--      function setPixel(imageData, x, y, r, g, b, a) {
--          index = (x + y * imageData.width) * 4;
--          index = ((y - 1) * (imageData.width * 4)) + ((x - 1) * 4);
--          imageData.data[index+0] = r;
--          imageData.data[index+1] = g;
--          imageData.data[index+2] = b;
--          imageData.data[index+3] = a;
--      }
--      
--      function cutHex(h) {return (h.charAt(0)=="#") ? h.substring(1,7):h}
--      function HexToR(h) {return parseInt((cutHex(h)).substring(0,2),16)}
--      function HexToG(h) {return parseInt((cutHex(h)).substring(2,4),16)}
--      function HexToB(h) {return parseInt((cutHex(h)).substring(4,6),16)}
--      
--      
--      return this.each(function() {
--          debuger(o.xpm);
--      
--          var curIns = $(this), setImg = o.xpm[0].split(" "), objColors = [];
--          //current Instance
--          //curIns.width(setImg[0]).height(setImg[1]);
--          //setImg[0] = setImg[1] = 100;
--          qCols = parseInt(setImg[2]);
--          
--          //change this
--          width = setImg[0];
--          height = setImg[1];
--          
--          c = curIns[0].getContext("2d");
--          imageData = c.getImageData(0,0, width, height);
--          //o.xpm[2] => number of colors
--          
--          //get colors
--          for(var i = 1; i <= qCols; i++){
--              var key = o.xpm[i].substr(0,1), value = o.xpm[i].match("#.*$");
--              if(value==null){value=o.background;}
--              objColors[key]=value;
--          }
--          
--          //alert(objColors[o.xpm[7].charAt(20)])
--                          
--          //change pixels
--          for(var r = 0, j=(qCols+1); r < height; r++){
--              //console.log((j+r),o.xpm[(j+r)]);
--              //setPixel(imageData, r, r, 0, 0, 200, 0xff);
--              for(var n = 0; n < width; n++){
--                  col = objColors[o.xpm[(j+r)].charAt(n)][0];
--                  setPixel(imageData, n, r, HexToR(col), HexToG(col), HexToB(col), 0xff);
--                  //if(n==30&&r==19){
--                  //    console.log(o.xpm[(j+r)].charAt(n),col/*,HexToR(col), HexToG(col), HexToB(col)*/)
--                  //}
--              }
--          }
--          c.putImageData(imageData, 0, 0);
--
--          o.afterEnd(curIns);
--      });//close return
--  }
--})(jQuery);
--  var pseudoXMP = [
--          /* <Values> */
--          /* <width/cols> <height/rows> <colors> <char on pixel>*/
--          "40 40 6 1",
--          /* <Colors> */
--          "     c none",
--          ".    c #ffffff",
--          "X    c #dadab6",
--          "o    c #6c91b6",
--          "O    c #476c6c",
--          "+    c #000000",
--          /* <Pixels> */
--          "                                        ",
--          "                                        ",
--          "                                        ",
--          "        .      .X..XX.XX      X         ",
--          "        ..   .....X.XXXXXX   XX         ",
--          "        ... ....X..XX.XXXXX XXX         ",
--          "   ..   ..........X.XXXXXXXXXXX   XX    ",
--          "   .... ........X..XX.XXXXXXXXX XXXX    ",
--          "   .... ..........X.XXXXXXXXXXX XXXX    ",
--          "   ooOOO..ooooooOooOOoOOOOOOOXX+++OO++  ",
--          "   ooOOO..ooooooooOoOOOOOOOOOXX+++OO++  ",
--          "   ....O..ooooooOooOOoOOOOOOOXX+XXXX++  ",
--          "   ....O..ooooooooOoOOOOOOOOOXX+XXXX++  ",
--          "   ..OOO..ooooooOooOOoOOOOOOOXX+++XX++  ",
--          "    ++++..ooooooooOoOOOOOOOOOXX+++ +++  ",
--          "     +++..ooooooOooOOoOOOOOOOXX+++  +   ",
--          "      ++..ooooooooOoOOOOOOOOOXX+++      ",
--          "        ..ooooooOooOOoOOOOOOOXX+++      ",
--          "        ..ooooooooOoOOOOOOOOOXX+++      ",
--          "        ..ooooooOooOOoOOOOOOOXX+++      ",
--          "        ..ooooooooOoOOOOOOOOOXX+++      ",
--          "         ..oooooOooOOoOOOOOOXX+++       ",
--          "         ..oooooooOoOOOOOOOOXX+++       ",
--          "          ..ooooOooOOoOOOOOXX+++        ",
--          "          ..ooooooOoOOOOOOOXX++++       ",
--          "        ..o..oooOooOOoOOOOXX+XX+++      ",
--          "       ...o..oooooOoOOOOOXX++XXX++      ",
--          "      ....OO..ooOooOOoOOXX+++XXXX++     ",
--          "     ...oo..+..oooOoOOOXX++XXooXXX++    ",
--          "    ...ooo..++..OooOOoXX+++XXooOXXX+    ",
--          "   ..oooOOXX+++....XXXX++++XXOOoOOXX+   ",
--          "   ..oooOOXX+++ ...XXX+++++XXOOooOXX++  ",
--          "   ..oooOXXX+++  ..XX+++  +XXOOooOXX++  ",
--          "   .....XXX++++             XXXXXXX++   ",
--          "    ....XX++++              XXXXXXX+    ",
--          "     ...XX+++                XXXXX++    ",
--          "                                        ",
--          "                                        ",
--          "                                        ",
--          "                                        "];
--      
-- $(document).ready(function(){
--          $('#xmp2canvas').xpm2canvas({xpm:pseudoXMP});
--      });

-- quick ditty I wrote to "widen" the xpm used in the docs:
--/*
string xpm = """
...............*...
.............**....
............**.....
...........**......
..........**.......
.****....**.....***
*...**..**...****..
*...***********....
.****...****.......
.......**..........
......***..........
.....*..*..........
....*...*..........
....*..*...........
....***............
"""
for line in split(xpm,'\n') do
    for ch in line do
        puts(1,repeat(ch,3))
    end for
    puts(1,"\n")
end for
--*/
