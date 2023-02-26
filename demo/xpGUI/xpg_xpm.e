--
-- xpGUI\xpg_xpm.e
-- ===============
--
--  xpm handling, in other words small icons defined in a textual format. 
--    """ is baked right into GTK, so that part's a whole lot simpler.
--
--  To avoid making anything global, logs itself via gSetGlobal(),
--           and uses a slightly fiddly custom xpg_xpm_callback().
--
--  No plans as yet to make this generally available.
--
local constant BI_RGB = 0

local integer idBITMAPINFOHEADER,
              idRGBQUAD

local constant dir_closed_xpm = {
/* columns rows colors chars-per-pixel */
"16 16 8 1 ",
"  c #000000",
". c #9C9C00",
"X c #CECE63",
"o c #FFCE9C",
"O c #FFFF9C",
"+ c #FFFFCE",
"@ c #F7F7F7",
"# c #FFFFFF",
/* pixels */
"################",
"###.... ########",
"##.@++O. #######",
"#.XXXXXX......##",
"#.++++++++++OX #",
"#.+OOOOOOOOOoX #",
"#.+OOOOOOoOoOX #",
"#.+OOOOOOOoOoX #",
"#.+OOOOoOoOoOX #",
"#.+OOOOOoOoOoX #",
"#.+OOOOoOoOooX #",
"#.+oOooOoOoooX #",
"#.XXXXXXXXXXXX #",
"##             #",
"################",
"################"},
dir_open_xpm = {
/* columns rows colors chars-per-pixel */
"16 16 6 1 ",
"  c #000000",
". c #9C9C00",
"X c #CECE63",
"o c #FFCE9C",
"O c #FFFF9C",
"+ c #FFFFFF",
/* pixels */
"++++++++++++++++",
"+++....+++++++++",
"++.++++.++++++++",
"+.+OOOO+......++",
"+.+OOOOOO++++X +",
"+.+OOOOOOOOOOX +",
"...........XOX +",
".+OOOOOoOoX XX +",
".+OOOOOOoOX XX +",
"+.+OOOOoOoo. . +",
"+.+OOooOoooX . +",
"++.+oOOooooX.  +",
"++...........  +",
"+++            +",
"++++++++++++++++",
"++++++++++++++++"},
dot_xpm = {
/* columns rows colors chars-per-pixel */
"16 16 7 1 ",
"  c #383838",
". c #636363",
"X c #808080",
"o c #A1A1A1",
"O c #C0C0C0",
"+ c #DEDEDE",
"@ c #FFFFFF",
/* pixels */
"@@@@@@@@@@@@@@@@",
"@@@@@@@@@@@@@@@@",
"@@@@@@@@@@@@@@@@",
"@@@@@@@@@@@@@@@@",
"@@@@@@XXooo@@@@@",
"@@@@@XooO+Oo@@@@",
"@@@@.XXooO+Oo@@@",
"@@@@.XXXooOOo@@@",
"@@@@ .XXXoooX@@@",
"@@@@ ..XXXoXX@@@",
"@@@@@ ..XXXX@@@@",
"@@@@@@  ...@@@@@",
"@@@@@@@@@@@@@@@@",
"@@@@@@@@@@@@@@@@",
"@@@@@@@@@@@@@@@@",
"@@@@@@@@@@@@@@@@"}

local function xpg_hexstr_to_int(string c2)
    -- convert eg "1F" to 31
    integer res = 0
    for c in c2 do
        c -= iff(c<='9'?'0':iff(c<='Z'?'A':'a')-10)
        res = res*16+c
    end for
    return res
end function    
--assert(xpg_hexstr_to_int("1F")==31)

local function xpg_winAPI_create_DIB_from_xpm(sequence xpm, integer callback)
-- (WinAPI only, GTK uses gdk_pixbuf_new_from_xpm_data() instead)
--  ... whereas demo\rosetta\virtunome.exw uses IupImageRGBA() ... [DEV]
--
-- xpm is a [local constant] sequence of the form:
--      constant xpm = {"5 5 2 1",
--                      "  c None",
--                      "x c #000000",
--                      "     ",
--                      " x x ",
--                      "  x  ",
--                      " x x ",
--                      "     "}
--  Obviously the one passed can be any size and number of colours, but
--  the one shown describes a 5x5 bitmap with 2 colours, one char per pixel,
--  and as you can hopefully see above is a small 'X'.
--
-- returns the dibHandle, or crashes.
--
    assert(platform()=WINDOWS)

    -- extract the values: "<wide> <tall> <colours> <chars per pixel>"
    string data = xpm[1]
    sequence dnn = {}
    integer N = 0
    for i=1 to length(data) do
        integer ch = data[i]
        if ch<=' ' then
            if N then
                dnn &= N
                N = 0
            end if
        else
            assert(ch>='0' and ch<='9')
            N = N*10 + (ch-'0')
        end if
    end for
    if N then
        dnn &= N
    end if
    -- ensure 4 words (or more) were found
--  if nn<5 then crash("unable to decode") end if
--  if length(dnn)<4 then crash("unable to decode") end if

    integer {width,height,colours,codeWide} = dnn
    assert(codeWide=1)

    -- palette and packing size
    integer palSize,    -- colours rounded up to supported sizes
            bpp         -- bits per pixel (1, 4, 8)
    if    colours<=2 then               palSize = 2     bpp = 1
    elsif colours<=16 then              palSize = 16    bpp = 4
    elsif colours<=256 then             palSize = 256   bpp = 8
    else
        crash("too many colours")
    end if

    -- calculate the size of the BITMAPINFO header
    integer headerSize = get_struct_size(idBITMAPINFOHEADER) + 
                        (get_struct_size(idRGBQUAD) * palSize)
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
    if bpp = 8 then         -- 1:1, 1 byte per pixel
        bytes = width
    elsif bpp = 4 then      -- 2:1, 2 pixels per byte
        if remainder(width,2) then pi = append(pi,0) end if             -- pad to multiple of 2
        bytes = floor(length(pi) / 2)
    elsif bpp = 1 then      -- 8:1, 8 pixels per byte (monochrome)
        while remainder(length(pi),8) do pi = append(pi,0) end while    -- pad to multiple of 8
        bytes = floor(length(pi) / 8)
    end if

    -- scanline needs to fall on a dword boundary (4 bytes)
    if remainder(bytes, 4) then
        packed = repeat(0, bytes + 4-remainder(bytes, 4))
    else
        packed = repeat(0, bytes)
    end if

    integer pwidth = length(packed),
        bitMapSize = height*pwidth

    -- Allocate memory for header and DIB in one block
    atom memBitmapInfo = allocate(headerSize+bitMapSize)

    -- build the bitmap info header
    set_struct_field(idBITMAPINFOHEADER,memBitmapInfo,"biSize",get_struct_size(idBITMAPINFOHEADER))
    set_struct_field(idBITMAPINFOHEADER,memBitmapInfo,"biWidth",width)          -- Width in pixels.
    set_struct_field(idBITMAPINFOHEADER,memBitmapInfo,"biHeight",-height)       -- Height in pixels (-ve=topdown).
    set_struct_field(idBITMAPINFOHEADER,memBitmapInfo,"biPlanes",1)             -- 1 colour plane. (always)
    set_struct_field(idBITMAPINFOHEADER,memBitmapInfo,"biBitCount",bpp)         -- no. of bits per pixel (0, 1, 4, 8, 16, 24, 32)
    set_struct_field(idBITMAPINFOHEADER,memBitmapInfo,"biCompression",BI_RGB)   -- compression format - none in this case
    set_struct_field(idBITMAPINFOHEADER,memBitmapInfo,"biSizeImage",0)          -- size in bytes of image, set to 0 (unneeded) if no compression
    set_struct_field(idBITMAPINFOHEADER,memBitmapInfo,"biXPelsPerMeter",0)      -- Not needed
    set_struct_field(idBITMAPINFOHEADER,memBitmapInfo,"biYPelsPerMeter",0)      -- Not needed
    set_struct_field(idBITMAPINFOHEADER,memBitmapInfo,"biClrUsed",colours)      -- colours actually used by the bitmap
    set_struct_field(idBITMAPINFOHEADER,memBitmapInfo,"biClrImportant",0)       -- no. colours needed to display image, 0=all

    -- get the address of the first rgb tuple
    atom mbPtr = memBitmapInfo+get_struct_size(idBITMAPINFOHEADER)

    -- create a palatte and data
    sequence cc             -- colour code, eg 'x' ==> palette[7] (ie cc[7] would be 'x').
                            -- cc[i] may be char (if codeWide=1) or string (if >1).
    cc = repeat(0,colours)  -- colour code map of <charsWide> chars --> palette index

    xpm = xpm[2..length(xpm)]   -- discard header

    -- read the colour data (number of colours determined from line 1, 3rd number)
    for i=1 to colours do
        data = xpm[i]
        -- extract the colour code          (eg "c" in "c None")
        cc[i] = data[codeWide]
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
        if bOK then
            -- convert to an {r,g,b} code
            if data[1]='#' then
                assert(length(data)=7)
                -- hex tuple: #rrggbb
                integer rr = xpg_hexstr_to_int(data[2..3]),
                        gg = xpg_hexstr_to_int(data[4..5]),
                        bb = xpg_hexstr_to_int(data[6..7])
                poke(mbPtr,{bb,gg,rr,0})
            else
                crash("hex colours only!")
            end if
        end if
        mbPtr += 4

    end for

    xpm = xpm[colours+1..length(xpm)]   -- discard colour table
    assert(length(xpm)=height)

    mbPtr = memBitmapInfo+headerSize

    -- convert the text into indexes
    for i=1 to height do
        data = xpm[i]   -- get a line
        assert(length(data)=width*codeWide)
        --
        -- convert XPM chars (or pairs/sets of chars if charsWide>1)
        -- into palette indexes, eg 'X' --> 7, if X was the 7th colour
        -- listed in the colours section:
        --  
        for j=1 to length(data) do
            pi[j] = find(data[j], cc) - 1
        end for
        --
        -- Now convert this array of colour indexes into a packed scanline:
        --
        if bpp=8 then -- 1:1
            packed = pi
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
        end if
        poke(mbPtr, packed)
        mbPtr += pwidth
    end for

    atom hDIB = callback({"NEWDIB",{memBitmapInfo,headerSize}})
    free(memBitmapInfo)
    return hDIB
end function

local function xpg_create_image_list(bool bUseGTK, integer callback)
    -- called from xpg_Init() [only]
    if bUseGTK then
        sequence tree_images = repeat(0,3)
        for i,xpm in {dir_closed_xpm,dir_open_xpm,dot_xpm} do
            tree_images[i] = callback(xpm)
        end for
        return tree_images
    else
        idBITMAPINFOHEADER = define_struct("""typedef struct tagBITMAPINFOHEADER {
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
                                               } BITMAPINFOHEADER, *PBITMAPINFOHEADER;""")

        idRGBQUAD = define_struct("""typedef struct tagRGBQUAD {
                                      BYTE rgbBlue;
                                      BYTE rgbGreen;
                                      BYTE rgbRed;
                                      BYTE rgbReserved;
                                     } RGBQUAD;""")

        atom tree_himl = callback({"NEWLIST"})
        for i,xpm in {dir_closed_xpm,dir_open_xpm,dot_xpm} do
            atom icon = xpg_winAPI_create_DIB_from_xpm(xpm,callback)
            {} = callback({"ADDICON",tree_himl,icon})           
        end for
        return tree_himl
    end if
end function

gSetGlobal("XPM_INIT",xpg_create_image_list)

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

