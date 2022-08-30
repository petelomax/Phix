--
-- builtins/IupGraph.e
--
-- todo: bar graphs[stacked or side], grid style, piechart, ...
--

function redraw_graph_cb(Ihandle graph)

    integer drid = IupGetInt(graph,"DRID"),
            grid = IupGetInt(graph,"GRID"),
            gridcolour = IupGetInt(graph,"GRIDCOLOR"),
            {width, height} = IupGetIntInt(graph, "DRAWSIZE"),
            dsdx = 1
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

    string title = IupGetAttribute(graph,"GTITLE",""),
           xname = IupGetAttribute(graph,"XNAME",""),
           yname = IupGetAttribute(graph,"YNAME",""),
           xfmt = IupGetAttribute(graph,"XTICKFMT","%g"),
           yfmt = IupGetAttribute(graph,"YTICKFMT","%g"),
           mode = IupGetAttribute(graph,"MODE",""),
           barmode = IupGetAttribute(graph,"BARMODE",""),
           markstyle = IupGetAttribute(graph,"MARKSTYLE",""),
           fontface = "Helvetica"
    integer fontstyle = CD_PLAIN,
            fontsize = 9,
            bgclr = IupGetAttributePtr(graph,"BGCOLOR"),
            titlestyle = IupGetInt(graph,"TITLESTYLE",CD_PLAIN),
            legend = 0, -- (idx to datasets)
            lx, ly -- (CD_EAST of the first legend text)
    cdCanvas cd_canvas = IupGetAttributePtr(graph,"CD_CANVAS")
    integer xcross = IupGetInt(graph,"XCROSSORIGIN"),
            ycross = IupGetInt(graph,"YCROSSORIGIN"),
            xrid = IupGetInt(graph,"XRID"),
            yrid = IupGetInt(graph,"YRID")
    while true do
        sequence ds = datasets[dsdx],
                 s = ds[1]
        if not string(s) then exit end if
        if s="BCOLOR" then
            bgclr = ds[2]
        elsif s="XRID" then
            xrid = ds[2]
        elsif s="YRID" then
            yrid = ds[2]
        elsif s="BARMODE" then
            barmode = ds[2]
        elsif s="NAMES" then
            legend = dsdx
        else
            {fontface,fontstyle,fontsize} = ds
        end if
        dsdx += 1
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
    if barmode!="" then
        assert(bv or bh,"invalid BARMODE")
        assert(mode="" or mode="BAR","invalid MODE for BARMODE")
        mode = "BAR"
    elsif mode="BAR" then
        bv = true
    end if
    if markstyle!="" and mode!="MARK" and mode!="MARKLINE" then
        assert(mode="","invalid MODE for MARKSTYLE")
        mode = "MARK"
    end if
    atom dx = (width-60-xmargin)/nx,
         dy = (height-60-ymargin)/ny,
         vx = 30+xmargin,
         vy = 30+ymargin,
         x = xmin,
         y = ymin
    for i=1 to nx+1-bv do   -- the vertical lines
        if (grid and not bv) or i=xcross then
            cdCanvasSetForeground(cd_canvas,iff(i=xcross?CD_BLACK:gridcolour))
            cdCanvasLine(cd_canvas,vx,30+ymargin,vx,height-30)
        end if
        cdCanvasSetForeground(cd_canvas,CD_BLACK)
        cdCanvasSetTextAlignment(cd_canvas,iff(xangle=0?CD_NORTH:
                                           iff(xangle=90?CD_EAST:
                                           iff(xangle=-90?CD_WEST:9/0))))
        cdCanvasSetTextOrientation(cd_canvas,xangle)
        string xtext = iff(xrid?xrid(x):sprintf(xfmt,x))
        atom ty = 25+ymargin+xyshift+(ycross-1)*dy
        cdCanvasText(cd_canvas,vx+dx*bv/2,ty,xtext)
        vx += dx
        x += xtick
    end for
    for i=1 to ny+1-bh do   -- the horizontal lines
        if (grid and not bh) or i=ycross then
            cdCanvasSetForeground(cd_canvas,iff(i=ycross?CD_BLACK:gridcolour))
            cdCanvasLine(cd_canvas,31+xmargin,vy,width-30,vy)
        end if
        cdCanvasSetForeground(cd_canvas,CD_BLACK)
        cdCanvasSetTextAlignment(cd_canvas,iff(yangle=0?CD_EAST:
                                           iff(yangle=90?CD_NORTH:
                                           iff(yangle=-90?CD_SOUTH:9/0))))
        cdCanvasSetTextOrientation(cd_canvas,yangle)
        string ytext = iff(yrid?yrid(y):sprintf(yfmt,y))
        atom tx = 25+xmargin+yxshift+(xcross-1)*dx
        cdCanvasText(cd_canvas,tx,vy+dy*bh/2,ytext)
        vy += dy
        y += ytick
    end for 

    integer lh -- (legend text height, per line)
    if legend then
        sequence legendnames = datasets[legend][2]
        cdCanvasSetTextOrientation(cd_canvas,0)
        cdCanvasSetTextAlignment(cd_canvas,CD_EAST)
        integer ll = length(legendnames), lw=0, lwi
        for i=1 to ll do
            {lwi, lh} = cdCanvasGetTextSize(cd_canvas, legendnames[i])
            lw = max(lw,lwi)
        end for
        string legendpos = IupGetAttribute(graph,"LEGENDPOS","TOPRIGHT")
        if legendpos="XY" then
            {lx,ly} = IupGetIntInt(graph,"LEGENDPOSXY") -- (untested)
        else
            if legendpos[1]='T' then
                assert(legendpos[1..3]="TOP")
                legendpos = legendpos[4..$]
                ly = 10
            else
                assert(legendpos[1..6]="BOTTOM")
                legendpos = legendpos[7..$]
                ly = height-50-ll*lh
            end if
            if legendpos="LEFT" then
                lx = 30+xmargin+lw
            elsif legendpos="CENTER" then
                lx = floor((xmargin+width+lw)/2)
            else
                assert(legendpos="RIGHT")   
                lx = width-30
            end if
        end if
        if IupGetInt(graph,"LEGENDBOX") then
            cdCanvasSetForeground(cd_canvas, bgclr)
            integer lxl = lx-lw-25, lxr = lx+10,
                    lyt = height-(ly+15), lyb = height-(ly+ll*lh+25)
            cdCanvasBox(cd_canvas,lxl,lxr,lyt,lyb)
            cdCanvasSetForeground(cd_canvas, CD_BLACK)
            cdCanvasRect(cd_canvas,lxl,lxr,lyt,lyb)
        end if
    end if

    -- and finally draw/plot the points!
    atom w = dx/xtick,
         h = dy/ytick
    vx = 30+xmargin + (xcross-1)*dx
    vy = 30+ymargin + (ycross-1)*dy
    integer lm1 = dsdx-1
    for d=dsdx to length(datasets) do
        sequence dd = datasets[d],
                 {px,py} = dd
        integer ldd = length(dd),
                 mm = (mode="MARK")
        string dms = markstyle,
               dmm = mode
        if ldd>=4 then
            mm = true
            dms = dd[4]
            if ldd>=5 then
                assert(dd[5]="MARKLINE")
                dmm = "MARKLINE"
            end if
        end if          
        cdCanvasSetForeground(cd_canvas,iff(ldd>=3?dd[3]:CD_BLACK))
        if length(px) then
            atom x1 = 30+xmargin+(px[1]-xmin)*w, 
                 y1 = 30+ymargin+(py[1]-ymin)*h
            for i=2-(bv or bh or mm) to length(px) do
                atom x2 = 30+xmargin+(px[i]-xmin)*w+dx*bv/2,
                     y2 = 30+ymargin+(py[i]-ymin)*h+dy*bh/2
                if mode="BAR" then
                    if bv then
                        cdCanvasBox(cd_canvas,x2-dx/2+1,x2+dx/2-1,vy,y2)
                    elsif bh then
                        cdCanvasBox(cd_canvas,vx,x2,y2-dy/2+1,y2+dy/2-1)
                    end if
                else
                    if mm then
-- (from IupPlot:) mark style of the current dataset. 
--        Can be: "HOLLOW_CIRCLE", "PLUS", "X", [DONE]
--          "STAR", "CIRCLE", "BOX", "DIAMOND",
--          "HOLLOW_BOX", "HOLLOW_DIAMOND". Default "X". 
--        (rest to be implemented as and when needed)
                        if dms="HOLLOW_CIRCLE" then
                            cdCanvasCircle(cd_canvas,x2,y2,8)
                        elsif dms="PLUS" then
                            cdCanvasLine(cd_canvas,x2,y2-3,x2,y2+3)
                            cdCanvasLine(cd_canvas,x2-3,y2,x2+3,y2)
                        else --default/x
                            cdCanvasLine(cd_canvas,x2-3,y2-3,x2+3,y2+3)
                            cdCanvasLine(cd_canvas,x2-3,y2+3,x2+3,y2-3)
                        end if
                    end if
                    if not mm or (dmm="MARKLINE" and i>=2) then
                        cdCanvasLine(cd_canvas,x1,y1,x2,y2)
                    end if
                end if
                x1 = x2
                y1 = y2
            end for
        end if
        if legend then
            integer lX = lx-20,
                    lY = height-ly-25
            if mode="BAR" then -- (untested)
                cdCanvasBox(cd_canvas,lX,lX+10,lY-5,lY+5)
            else
                if mm then
                    if dms="HOLLOW_CIRCLE" then
                        cdCanvasCircle(cd_canvas,lX+15,lY,8)
                    elsif dms="PLUS" then
                        cdCanvasLine(cd_canvas,lX+15,lY-5,lX+15,lY-5)
                        cdCanvasLine(cd_canvas,lX+10,lY,lX+20,lY)
                    else --default/x
                        cdCanvasLine(cd_canvas,lX+10,lY+5,lX+20,lY-5)
                        cdCanvasLine(cd_canvas,lX+10,lY-5,lX+20,lY+5)
                    end if
                end if
                if not mm or dmm="MARKLINE" then
                    cdCanvasLine(cd_canvas,lX+5,lY,lX+25,lY)
                end if
            end if
            cdCanvasSetForeground(cd_canvas,CD_BLACK)
            cdCanvasText(cd_canvas,lX,lY,datasets[legend][2][d-lm1])
            ly += lh
        end if
    end for

    cdCanvasFlush(cd_canvas)
    IupSetAttribute(graph, "RASTERSIZE", NULL) -- release the minimum limitation
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
    Ihandle graph = IupGLCanvas()
    IupSetAttributePtr(graph,"BGCOLOR",CD_WHITE) -- (set a default)
    IupSetAttribute(graph,"BARMODE","") -- (avoid an error in pGUI.js)
    IupSetCallbacks(graph, {"MAP_CB", Icallback("map_graph_cb"),
                            "ACTION", Icallback("redraw_graph_cb"),
                            "RESIZE_CB", Icallback("resize_graph_cb")})
    IupSetInt(graph,"DRID",drid)
    IupSetInt(graph,"GRID",true) -- (show the grid by default)
    IupSetInt(graph,"LEGENDBOX",true) -- (ditto box around legend)
    IupSetInt(graph,"GRIDCOLOR",CD_GRAY)
    IupSetDouble(graph,"XTICK",1)
    IupSetDouble(graph,"YTICK",1)
    IupSetAttributes(graph,attr,args)
    return graph    
end function

