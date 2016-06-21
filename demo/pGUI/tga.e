--
-- tga.e
--
--include file.e
--include get.e
--include misc.e

constant
        TGA_idLength = 1,
        TGA_colorMapType = 2,
        TGA_imageType = 3,
        TGA_colorMapSpec = 4,
        TGA_xOrigin = 5,
        TGA_yOrigin = 6,
        TGA_width = 7,
        TGA_height = 8,
        TGA_bpp = 9,
        TGA_imageDesc = 10

constant
--      TGA_NODATA = 0,
--      TGA_INDEXEDED = 1,
        TGA_RGB = 2,
        TGA_GRAYSCALE = 3
--      TGA_INDEXED_RLE = 9,
--      TGA_RGB_RLE = 10,
--      TGA_GRAYSCALE_RLE = 11


global function LoadTGA(sequence filename)
sequence tga
integer fn,colorMode,imageSize
atom data

    -- open the TGA file
    fn = open(filename, "rb")
    if fn=-1 then
        return {}
    end if

    -- read in the image type
    tga = repeat(0,10)
    tga[TGA_idLength] = getc(fn)
    tga[TGA_colorMapType] = getc(fn)
    tga[TGA_imageType] = getc(fn)
    tga[TGA_colorMapSpec] = get_bytes(fn,5)
    tga[TGA_xOrigin] = getc(fn)
    tga[TGA_xOrigin] += getc(fn)*#100
    tga[TGA_yOrigin] = getc(fn)
    tga[TGA_yOrigin] += getc(fn)*#100
    tga[TGA_width] = getc(fn)
    tga[TGA_width] += getc(fn)*#100
    tga[TGA_height] = getc(fn)
    tga[TGA_height] += getc(fn)*#100
    tga[TGA_bpp] = getc(fn)
    tga[TGA_imageDesc] = getc(fn)

    -- see if the type is one that we support
    if ((tga[TGA_imageType]!=TGA_RGB) and (tga[TGA_imageType]!=TGA_GRAYSCALE)) then
        close(fn)
        return {}
    end if

    -- if colorMode is 3, there is no alpha channel
    colorMode = floor(tga[TGA_bpp]/8)
    imageSize = tga[TGA_width]*tga[TGA_height]*colorMode

    -- allocate memory for TGA image data
    data = allocate(imageSize)
    if data<=0 then
        close(fn)
        return {}
    end if

    for i=0 to imageSize-1 do
        poke(data+i,getc(fn))
    end for

    close(fn)

    return {data,tga[TGA_width],tga[TGA_height],tga[TGA_bpp],colorMode}
end function


