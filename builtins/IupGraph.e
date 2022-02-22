--
-- builtins/IupGraph.e
--
-- todo: bar graphs[stacked or side], legend, grid style, line style/mode, marks, piechart, ...
--

function redraw_graph_cb(Ihandle graph)

    integer drid = IupGetInt(graph,"DRID"),
            grid = IupGetInt(graph,"GRID"),
            {width, height} = IupGetIntInt(graph, "DRAWSIZE"),
            dsdx = 1
--?IupGetIntInt(graph, "DRAWSIZE")
    sequence datasets = drid(graph)
    // nb: "XTICK" etc may be/get set in the above call.
    atom xtick = IupGetDouble(graph,"XTICK"),
         xmin = IupGetDouble(graph,"XMIN"),
         xmax = IupGetDouble(graph,"XMAX"),
         xmargin = IupGetDouble(graph,"XMARGIN",10),
         xyshift = IupGetDouble(graph,"XYSHIFT"),
         xangle = IupGetDouble(graph,"XANGLE"),
         ytick = IupGetDouble(graph,"YTICK"),
         ymin = IupGetDouble(graph,"YMIN"),
         ymax = IupGetDouble(graph,"YMAX"),
         ymargin = IupGetDouble(graph,"YMARGIN",10),
         yxshift = IupGetDouble(graph,"YXSHIFT"),
         yangle = IupGetDouble(graph,"YANGLE")
--IupSetAttributes(graph,`TITLE="Yellowstone Names"`)
--IupSetAttributes(graph,`XLABEL="n", YLABEL="a(n)"`)

    string title = IupGetAttribute(graph,"GTITLE",""),
           xname = IupGetAttribute(graph,"XNAME",""),
           yname = IupGetAttribute(graph,"YNAME",""),
           xfmt = IupGetAttribute(graph,"XTICKFMT","%g"),
           yfmt = IupGetAttribute(graph,"YTICKFMT","%g"),
           barmode = IupGetAttribute(graph,"BARMODE",""),
           fontface = "Helvetica"
    integer fontstyle = CD_PLAIN,
            fontsize = 9,
            bgclr = IupGetAttributePtr(graph,"BGCOLOR"),
            titlestyle = IupGetInt(graph,"TITLESTYLE",CD_PLAIN)
    cdCanvas cd_canvas = IupGetAttributePtr(graph,"CD_CANVAS")
    integer xcross = IupGetInt(graph,"XCROSSORIGIN"),
            ycross = IupGetInt(graph,"YCROSSORIGIN"),
            xrid = IupGetInt(graph,"XRID"),
            yrid = IupGetInt(graph,"YRID")
    while true do
        sequence ds = datasets[dsdx],
                 s = ds[1]
        if not string(s) then exit end if
        dsdx += 1
        if s="BCOLOR" then
            bgclr = ds[2]
        elsif s="XRID" then
            xrid = ds[2]
        elsif s="YRID" then
            yrid = ds[2]
        elsif s="BARMODE" then
            barmode = ds[2]
        else
            {fontface,fontstyle,fontsize} = ds
        end if
    end while
    cdCanvasSetBackground(cd_canvas, bgclr)
    cdCanvasActivate(cd_canvas)
    cdCanvasClear(cd_canvas)

    -- draw title and axis names
    cdCanvasSetForeground(cd_canvas,CD_BLACK)
    cdCanvasFont(cd_canvas,fontface,titlestyle,fontsize)
    if title!="" then
        cdCanvasSetTextAlignment(cd_canvas,CD_NORTH)
        cdCanvasSetTextOrientation(cd_canvas,0)
        cdCanvasText(cd_canvas,width/2,height-6,title)
    end if
    if yname!="" then
--      cdCanvasSetTextAlignment(cd_canvas,CD_EAST)
        cdCanvasSetTextAlignment(cd_canvas,CD_SOUTH)
        cdCanvasSetTextOrientation(cd_canvas,90)
        cdCanvasText(cd_canvas,14,height/2,yname)
    end if
    if xname!="" then
        cdCanvasSetTextAlignment(cd_canvas,CD_SOUTH)
        cdCanvasSetTextOrientation(cd_canvas,0)
        cdCanvasText(cd_canvas,width/2,4,xname)
    end if
    cdCanvasFont(cd_canvas,fontface,fontstyle,fontsize)
    
    -- draw the x/y-axis labels and vertical gridlines
    xcross = iff(xcross?round((0-xmin)/xtick)+1:1)
    ycross = iff(ycross?round((0-ymin)/ytick)+1:1)

    integer bv = (barmode="VERTICAL"),
            bh = (barmode="HORIZONTAL"),
            nx = round((xmax-xmin)/xtick)+bv,
            ny = round((ymax-ymin)/ytick)+bh
    atom dx = (width-60-xmargin)/nx,
         dy = (height-60-ymargin)/ny,
         vx = 30+xmargin,
--       vy = 30+ymargin+dy*bh/2,
         vy = 30+ymargin,
         x = xmin,
         y = ymin
--  for i=1 to nx+1 do  -- the vertical lines
    for i=1 to nx+1-bv do   -- the vertical lines
        if (grid and not bv) or i=xcross then
            cdCanvasSetForeground(cd_canvas,iff(i=xcross?CD_BLACK:CD_GRAY))
            cdCanvasLine(cd_canvas,vx,30+ymargin,vx,height-30)
        end if
        cdCanvasSetForeground(cd_canvas,CD_BLACK)
        cdCanvasSetTextAlignment(cd_canvas,iff(xangle=0?CD_NORTH:
                                           iff(xangle=90?CD_EAST:
                                           iff(xangle=-90?CD_WEST:9/0))))
        cdCanvasSetTextOrientation(cd_canvas,xangle)
        string xtext = iff(xrid?xrid(x):sprintf(xfmt,x))
        atom ty = 25+ymargin+xyshift+(ycross-1)*dy
--      cdCanvasText(cd_canvas,vx,ty,xtext)
        cdCanvasText(cd_canvas,vx+dx*bv/2,ty,xtext)
        vx += dx
        x += xtick
    end for
    for i=1 to ny+1-bh do   -- the horizontal lines
        if (grid and not bh) or i=ycross then
            cdCanvasSetForeground(cd_canvas,iff(i=ycross?CD_BLACK:CD_GRAY))
            cdCanvasLine(cd_canvas,31+xmargin,vy,width-30,vy)
        end if
        cdCanvasSetForeground(cd_canvas,CD_BLACK)
        cdCanvasSetTextAlignment(cd_canvas,iff(yangle=0?CD_EAST:
                                           iff(yangle=90?CD_NORTH:
                                           iff(yangle=-90?CD_SOUTH:9/0))))
        cdCanvasSetTextOrientation(cd_canvas,yangle)
        string ytext = iff(yrid?yrid(y):sprintf(yfmt,y))
--?{yrid,ytext,yfmt,y}
        atom tx = 25+xmargin+yxshift+(xcross-1)*dx
--      atom tx = 25+xmargin+yxshift+(xcross-1+bh)*dx
--      cdCanvasText(cd_canvas,tx,vy,ytext)
        cdCanvasText(cd_canvas,tx,vy+dy*bh/2,ytext)
        vy += dy
        y += ytick
    end for 
--  cdCanvasSetTextOrientation(cd_canvas,0) -- maybe? [certainly if we are planning on a legend]

    -- and finally draw/plot the points!
    atom w = dx/xtick,
         h = dy/ytick
    vx = 30+xmargin + (xcross-1)*dx
    vy = 30+ymargin + (ycross-1)*dy
    for d=dsdx to length(datasets) do
        sequence dd = datasets[d],
                 {px,py} = dd
        if length(px) then
            integer clr = iff(length(dd)>=3?dd[3]:CD_BLACK)
            cdCanvasSetForeground(cd_canvas,clr)
            atom x1 = 30+xmargin+(px[1]-xmin)*w, 
                 y1 = 30+ymargin+(py[1]-ymin)*h
            for i=2-(bv or bh) to length(px) do
                atom x2 = 30+xmargin+(px[i]-xmin)*w+dx*bv/2,
                     y2 = 30+ymargin+(py[i]-ymin)*h+dy*bh/2
--if remainder(i,1000)=0 then
--?{i,x1,y1,x2,y2,xmin,xmax,w,xtick,dx}
--end if
--              if barmode="VERTICAL" then
                if bv then
--DEV
--                  cdCanvasLine(cd_canvas,x2,vy,x2,y2)
                    cdCanvasBox(cd_canvas,x2-dx/2+1,x2+dx/2-1,vy,y2)
--              elsif barmode="HORIZONTAL" then
                elsif bh then
--DEV
--                  cdCanvasLine(cd_canvas,vx,y2,x2,y2)
                    cdCanvasBox(cd_canvas,vx,x2,y2-dy/2+1,y2+dy/2-1)
                else
                    cdCanvasLine(cd_canvas,x1,y1,x2,y2)
                end if
                x1 = x2
                y1 = y2
            end for
        end if
    end for

    cdCanvasFlush(cd_canvas)
    IupSetAttribute(graph, "RASTERSIZE", NULL) -- release minimum limitation
    return IUP_DEFAULT
end function

function map_graph_cb(Ihandle graph)
    cdCanvas cd_canvas
    IupGLMakeCurrent(graph)
    if platform()=JS then
        cd_canvas = cdCreateCanvas(CD_IUP, graph)
    else
        atom res = IupGetDouble(NULL, "SCREENDPI")/25.4
        cd_canvas = cdCreateCanvas(CD_GL, "10x10 %g", {res})
    end if
    IupSetAttributePtr(graph,"CD_CANVAS",cd_canvas)
--  IupSetAttribute(graph, "RASTERSIZE", NULL) -- release minimum limitation
    return IUP_DEFAULT
end function

function resize_graph_cb(Ihandle graph)
    integer {width, height} = IupGetIntInt(graph, "DRAWSIZE")
    atom res = IupGetDouble(NULL, "SCREENDPI")/25.4
    cdCanvas cd_canvas = IupGetAttributePtr(graph,"CD_CANVAS")
    cdCanvasSetAttribute(cd_canvas, "SIZE", "%dx%d %g", {width, height, res})
    return IUP_DEFAULT
end function

global function IupGraph(integer drid, string attr="", sequence args={})
    Ihandle graph = IupGLCanvas(attr,args)
    IupSetAttributePtr(graph,"BGCOLOR",CD_WHITE) -- (set a default)
    IupSetAttribute(graph,"BARMODE","") -- (avoid an error in pGUI.js)
    IupSetCallbacks(graph, {"MAP_CB", Icallback("map_graph_cb"),
                            "ACTION", Icallback("redraw_graph_cb"),
                            "RESIZE_CB", Icallback("resize_graph_cb")})
    IupSetInt(graph,"DRID",drid)
    IupSetInt(graph,"GRID",true) -- (show the grid by default)
    return graph    
end function

