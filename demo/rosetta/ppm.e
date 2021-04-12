--
-- demo/rosetta/ppm.e
-- ==================
--  Common code for several bitmap tasks
--
-- Some colour constants:
global constant black  = #000000,
                blue   = #0000FF,
                green  = #00FF00,
--              red    = #ff2020,
                red    = #FF0000,
                yellow = #FFDF20,
                white  = #FFFFFF

global function new_image(integer width, height, fill_colour=black)
    -- Create new image filled with some colour
    return repeat(repeat(fill_colour,height),width)
end function

function is_digit(integer ch)
    return ch>='0' and ch<='9'
end function

function get_number(string ppm, integer pdx)
    integer n = 0
    while is_digit(ppm[pdx]) do n = n*10+ppm[pdx]-'0' pdx += 1 end while
    pdx += 1
    return {pdx,n}
end function

global function read_ppm(string filename, bool bFlat=false, bText=false)
    string ppm = iff(bText?filename:get_text(filename))
    if ppm[1..3]!="P6\n" then ?9/0 end if
    integer pdx = 4, width, height, maxcolor
    while ppm[pdx]='#' do
        while ppm[pdx]!='\n' do pdx += 1 end while
        pdx += 1
    end while
    {pdx,width} = get_number(ppm,pdx)
    {pdx,height} = get_number(ppm,pdx)
    {pdx,maxcolor} = get_number(ppm,pdx)
    if bFlat then
        ppm = ppm[pdx..$-1]
        return {width,height,ppm}
    end if
    sequence image = new_image(width,height)
--  image = unpack_pixels(image,width,height,ppm,pdx)
--function unpack_pixels(
    for y=1 to height do
        for x=1 to width do
            integer p = 0
            for i=1 to 3 do
                p = p*#100 + ppm[pdx]
                pdx += 1
            end for
            image[x][y] = p
        end for
    end for
    return image
end function

global procedure write_ppm(object file, sequence image)
    integer fn = iff(string(file)?open(file,"wb"):file),
            dimx = length(image),
            dimy = length(image[1])
    printf(fn, "P6\n%d %d\n255\n", {dimx,dimy})
    for y=1 to dimy do
        for x=1 to dimx do
            integer pixel = image[x][y]          -- red,green,blue
            sequence r_g_b = sq_and_bits(pixel,{#FF0000,#FF00,#FF})
                    r_g_b = sq_floor_div(r_g_b,{#010000,#0100,#01})
            puts(fn,r_g_b)
        end for
    end for
    close(fn)
end procedure

global function to_grey(sequence image)
    integer dimx = length(image),
            dimy = length(image[1])
    for x=1 to dimx do
        for y=1 to dimy do
            integer pixel = image[x][y]          -- red,green,blue
            sequence r_g_b  =  sq_and_bits(pixel,{#FF0000,#FF00,#FF})
            integer {r,g,b} = sq_floor_div(r_g_b,{#010000,#0100,#01})
            image[x][y] = floor(0.2126*r + 0.7152*g + 0.0722*b)*#010101
        end for
    end for
    return image
end function

global function bresLine(sequence image, integer x0, y0, x1, y1, colour)
    -- The line algorithm
    integer dimx = length(image),
            dimy = length(image[1]),
            deltaX = abs(x1-x0),
            deltaY = abs(y1-y0),
            stepX = iff(x0<x1,1,-1),
            stepY = iff(y0<y1,1,-1),
            lineError = iff(deltaX>deltaY,deltaX,-deltaY),
            prevle
    lineError = round(lineError/2, 1)
    while true do
        if  x0>=1 and x0<=dimx
        and y0>=1 and y0<=dimy then
            image[x0][y0] = colour
        end if
        if x0=x1 and y0=y1 then exit end if
        prevle = lineError
        if prevle>-deltaX then
            lineError -= deltaY
            x0 += stepX
        end if
        if prevle<deltaY then
            lineError += deltaX
            y0 += stepY
        end if
    end while
    return image
end function

