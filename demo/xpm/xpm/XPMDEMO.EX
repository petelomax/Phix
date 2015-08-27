-- demo.ex
-- 
-- demonstrate display of XPMs
--

include xpm.e
include graphics.e      -- graphic support
include file.e          -- directory searching
include wildcard.e      -- upper/lower case


integer
    wide,       -- width of screen
    x,          -- display x position
    y           -- display y position
sequence 
    theDir,     -- directory listing
    theName,    -- name of file in directory
    theBmp,     -- bitmap read from xpm file
    theList,    -- sequence of bitmap data
    theVideo    -- video attributes

-- initialize
    x = 0
    y = 0
                      

------------------------------------------------------------------------------
function abs( integer i )
    if i < 0 then
        return -i
    else
        return i
    end if
end function
                      
------------------------------------------------------------------------------
function matchPalette( sequence sourcePal, sequence pixels )

    -- return the pixels matched to the current palette instead
    -- of the source palette
    
    atom err, diff, best
    sequence destPal, colorIndex
       
    -- get palette
    destPal = get_all_palette()
    
    -- adjust source
    sourcePal = floor( sourcePal/4 )
    
    -- build holder for results
    colorIndex = repeat( 0, length( sourcePal ) )

    -- look at each entry    
    for i = 1 to length( sourcePal ) do
        -- set the last best match as impossibly bad
        best = #FFFFFF
        -- look through the palette
        for entry = 1 to length( destPal ) do
            -- error amount for this entry
            err = 0                       
            -- for each {rgb}
            for rgb = 1 to 3 do
                -- error is difference between the two
                diff = abs( sourcePal[i][rgb] - destPal[entry][rgb] )
                err = err + diff
            end for

            -- better match than prior best match?
            if err < best then
                -- save error amount
                best = err               
                -- store the palette index
                colorIndex[i] = entry
            end if
        end for
    end for

    -- replace values with references to new palette
    for i = 1 to length( pixels ) do
        for j = 1 to length( pixels[i] ) do
            -- replace
            pixels[i][j] = colorIndex[ pixels[i][j]+1 ]-1
        end for
    end for                       

    return pixels
                                      
end function
                      
                      
-- read all the xpm files in the current directory
    theDir = dir( current_dir() )

-- clear the list
    theList = {}

-- read all xpms as bitmaps
    for i = 1 to length( theDir ) do  
        -- get a name
        theName = lower( theDir[i][D_NAME] )
        
        -- xpm?
        if match( ".xpm", theName ) then
        
            -- read the xpm into a euphoria bitmap
            theBmp = readXpm( theName, {0,0,0} )
            
            -- convert the palette to current palette
            theBmp = matchPalette( theBmp[1], theBmp[2] )
    
            -- add to the list
            theList = append( theList, theBmp )
            
        end if
        
    end for

-- set the graphics mode
    if graphics_mode( 19 ) then
        puts( 1, "Unable to set graphics mode.\n" )
        abort(0)
    end if


-- get the video attributes
    theVideo = video_config()    
    wide = theVideo[VC_XPIXELS]

-- draw all the bitmaps    
    for i = 1 to length( theList ) do

        -- display a bitmap        
        display_image( {x,y}, theList[i] )    

        -- move to next position on screen
        x = x + 24
        
        -- need to wrap?
        if x+24 > wide then
            x = 0            
            y = y + 24
        end if
    
    end for


-- wait for a key
while get_key() = -1 do
end while

-- restore graphics mode
if graphics_mode( -1 ) then
end if

