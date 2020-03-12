--
-- builtins\librsvg.e
--
--  Experimental / Not Documented / Windows-only (mainly due to ".dll")
--
-- Sadly the version info in librsvg is implemented as C macros, iow the traditional great big *u to anyone not using a C compiler...

--Unfortunately "develop on desktop" has a somewhat heavy footprint: 13 dlls at ~15MB, just to support SVG display (via librsvg).
--Note: some ".svg" files are pure-defs, no actual svg at all, eg fontawesome-webfont.svg (in case, like me, testing them confuses you..)
--One thing I have spotted is that librsvg does not like eg <font .. /> at all, but "prefers" <font ..></font>, I can live with that.
--https://zzz.dog/ (animated svg, 28K js)

string librsvg_dir = ""
global procedure set_librsvg_dir(string d)
    if get_file_type(d)==FILETYPE_DIRECTORY then
        librsvg_dir = d
    end if
end procedure
constant W = machine_word()

function link_c(object dll, string name, sequence args, atom result=NULL)
    atom lib = iff(string(dll)?open_dll(dll):dll)
    if lib=0 then throw("could not open "&dll) end if
    --(following is == define_c_proc if result==NULL)
    integer res = define_c_func(lib,name,args,result)
    if res=-1 then throw("could not link "&name) end if
    return res
end function

integer xrsvg_handle_new_from_data = NULL,
        xg_type_init = NULL, 
        xg_object_unref, xg_clear_error, xrsvg_handle_close,
        xrsvg_handle_get_pixbuf, xgdk_pixbuf_save, 
        xgdk_pixbuf_get_width, xgdk_pixbuf_get_height, 
        xgdk_pixbuf_get_rowstride, xgdk_pixbuf_get_colorspace,
        xgdk_pixbuf_get_n_channels, xgdk_pixbuf_get_bits_per_sample,
        xgdk_pixbuf_get_has_alpha, xgdk_pixbuf_get_pixels,
        pError,
        xSetDllDirectory

--deprecated
--      xrsvg_pixbuf_from_file_at_size, 
--  xrsvg_pixbuf_from_file_at_size = link_c("librsvg-2-2.dll","rsvg_pixbuf_from_file_at_size",{C_PTR,C_INT,C_INT,C_PTR},C_PTR)

--future?? (since 2.32??)
--      xgdk_pixbuf_read_pixels,
-- const guint8* gdk_pixbuf_read_pixels(const GdkPixbuf *pixbuf);
--  xgdk_pixbuf_read_pixels = link_c("librsvg-2-2.dll","gdk_pixbuf_read_pixels",{C_PTR},C_PTR)          -- nope...
-- GdkPixbuf* rsvg_handle_get_pixbuf(RsvgHandle *handle);
--  xrsvg_handle_get_pixbuf = link_c("librsvg-2-2.dll","rsvg_handle_get_pixbuf",{C_PTR},C_PTR)          -- nope...

procedure svg_error(string error="null_error")
-- process pError, throw it as an exception
-- if pError is NULL, ie librsvg did not provide something better,
-- then (the default) error is just throw as-is.
    atom e = peekNS(pError,W,false)
    if e!=0 then
--      integer code = peek4u(e+4)  -- (C int)
        error = peek_string(peekNS(e+8,W,false))
        c_proc(xg_clear_error,{pError})
--      pokeN(pError,0,W)   -- (probably unhelpful [DEV check...])
    end if
    throw(error)
end procedure

procedure init_svg()
    if platform()=WINDOWS then
        xSetDllDirectory = link_c("kernel32.dll","SetDllDirectoryA",{C_PTR},C_BOOL)
        if librsvg_dir!="" 
        and not c_func(xSetDllDirectory,{librsvg_dir}) then
            throw("Could not set DLL directory")
        end if
    end if
    -- void g_type_init(void); -- no longer rqd? (since glib 2.36, but I guess this is 2.00)
    -- void rsvg_term(void); -- maybe... ("")
    -- void g_object_unref(gpointer object);
    -- void g_clear_error(GError **err);
    -- RsvgHandle* rsvg_handle_new_from_data(const guint8 *data, gsize data_len, GError **error);
    -- GdkPixbuf* rsvg_handle_get_pixbuf(RsvgHandle *handle);
    -- gboolean rsvg_handle_close(RsvgHandle *handle, GError **error);
    -- gboolean gdk_pixbuf_save(GdkPixbuf *pixbuf, const char *filename, const char *type, GError **error, ...);
    -- int gdk_pixbuf_get_width(const GdkPixbuf *pixbuf);
    -- bool gdk_pixbuf_get_has_alpha(const GdkPixbuf *pixbuf);
    -- int gdk_pixbuf_get_colorspace(const GdkPixbuf *pixbuf);
    -- int gdk_pixbuf_get_n_channels(const GdkPixbuf *pixbuf);
    -- int gdk_pixbuf_get_bits_per_sample(const GdkPixbuf *pixbuf);
    -- guchar* gdk_pixbuf_get_pixels(const GdkPixbuf *pixbuf);
    xg_type_init = link_c("libgobject-2.0-0.dll","g_type_init",{},NULL)
    xg_object_unref = link_c("libgobject-2.0-0.dll","g_object_unref",{C_PTR},NULL)
    xg_clear_error = link_c("libglib-2.0-0.dll","g_clear_error",{C_PTR},NULL)
    xrsvg_handle_new_from_data = link_c("librsvg-2-2.dll","rsvg_handle_new_from_data",{C_PTR,C_INT,C_PTR},C_PTR)
    xrsvg_handle_get_pixbuf = link_c("librsvg-2-2.dll","rsvg_handle_get_pixbuf",{C_PTR},C_PTR)
    xrsvg_handle_close = link_c("librsvg-2-2.dll","rsvg_handle_close",{C_PTR,C_PTR},C_BOOL)
    xgdk_pixbuf_save = link_c("libgdk_pixbuf-2.0-0.dll","+gdk_pixbuf_save",{C_PTR,C_PTR,C_PTR,C_PTR,C_PTR},C_BOOL)
    xgdk_pixbuf_get_width = link_c("libgdk_pixbuf-2.0-0.dll","+gdk_pixbuf_get_width",{C_PTR},C_INT)
    xgdk_pixbuf_get_height = link_c("libgdk_pixbuf-2.0-0.dll","+gdk_pixbuf_get_height",{C_PTR},C_INT)
    xgdk_pixbuf_get_rowstride = link_c("libgdk_pixbuf-2.0-0.dll","+gdk_pixbuf_get_rowstride",{C_PTR},C_INT)
    xgdk_pixbuf_get_has_alpha = link_c("libgdk_pixbuf-2.0-0.dll","+gdk_pixbuf_get_has_alpha",{C_PTR},C_INT)
    xgdk_pixbuf_get_colorspace = link_c("libgdk_pixbuf-2.0-0.dll","+gdk_pixbuf_get_colorspace",{C_PTR},C_INT)
    xgdk_pixbuf_get_n_channels = link_c("libgdk_pixbuf-2.0-0.dll","+gdk_pixbuf_get_n_channels",{C_PTR},C_INT)
    xgdk_pixbuf_get_bits_per_sample = link_c("libgdk_pixbuf-2.0-0.dll","+gdk_pixbuf_get_bits_per_sample",{C_PTR},C_INT)
    xgdk_pixbuf_get_pixels = link_c("libgdk_pixbuf-2.0-0.dll","gdk_pixbuf_get_pixels",{C_PTR},C_PTR)
    if platform()=WINDOWS then
        if not c_func(xSetDllDirectory,{NULL}) then
            throw("Could not restore default DLL search order")
        end if
    end if
    pError = allocate(W)
    pokeN(pError,0,W)
    c_proc(xg_type_init,{}) -- (or every time??)
end procedure

--  mysvg = rsvg_handle_new()
--  ok = rsvg_handle_write(mysvg, svg$, LEN(svg$), 0)
--  ok = rsvg_handle_close(mysvg, 0)
--  mypix = rsvg_handle_get_pixbuf(mysvg)
--  g_object_unref(mysvg)
--  rsvg_term()
--  ' create a new gtk image object
--  gtk_image_set_from_pixbuf(image, mypix)     -- [ERM...]
--  g_object_unref(mypix)


global procedure g_object_unref(atom o)
--  if xg_type_init=NULL then init_svg() end if
    if xg_type_init=NULL then ?9/0 end if -- (o be what then??)
    c_proc(xg_object_unref,{o})
end procedure

global function gdk_pixbuf_get_width(atom pGdkPixbuf)
    if xg_type_init=NULL then ?9/0 end if -- ("" be what then??)
    return c_func(xgdk_pixbuf_get_width,{pGdkPixbuf})
end function

global function gdk_pixbuf_get_height(atom pGdkPixbuf)
    if xg_type_init=NULL then ?9/0 end if -- ("" be what then??)
    return c_func(xgdk_pixbuf_get_height,{pGdkPixbuf})
end function

global function gdk_pixbuf_get_rowstride(atom pGdkPixbuf)
    if xg_type_init=NULL then ?9/0 end if -- ("" be what then??)
    return c_func(xgdk_pixbuf_get_rowstride,{pGdkPixbuf})
end function

global function gdk_pixbuf_get_has_alpha(atom pGdkPixbuf)
    if xg_type_init=NULL then ?9/0 end if -- ("" be what then??)
    return c_func(xgdk_pixbuf_get_has_alpha,{pGdkPixbuf})
end function

global constant GDK_COLORSPACE_RGB = 0

global function gdk_pixbuf_get_colorspace(atom pGdkPixbuf)
    if xg_type_init=NULL then ?9/0 end if -- ("" be what then??)
    return c_func(xgdk_pixbuf_get_colorspace,{pGdkPixbuf})
end function

global function gdk_pixbuf_get_n_channels(atom pGdkPixbuf)
    if xg_type_init=NULL then ?9/0 end if -- ("" be what then??)
    return c_func(xgdk_pixbuf_get_n_channels,{pGdkPixbuf})
end function

global function gdk_pixbuf_get_bits_per_sample(atom pGdkPixbuf)
    if xg_type_init=NULL then ?9/0 end if -- ("" be what then??)
    return c_func(xgdk_pixbuf_get_bits_per_sample,{pGdkPixbuf})
end function

global function gdk_pixbuf_get_pixels(atom pGdkPixbuf)
    if xg_type_init=NULL then ?9/0 end if -- ("" be what then??)
    return c_func(xgdk_pixbuf_get_pixels,{pGdkPixbuf})
end function

global function rasterize_svg_pixbuf(string text)
    if xg_type_init=NULL then init_svg() end if
    atom rsvg_handle = c_func(xrsvg_handle_new_from_data,{text,length(text),pError})
    if rsvg_handle=NULL then svg_error("bad svg data?") end if
    if not c_func(xrsvg_handle_close,{rsvg_handle, pError})
    or peekNS(pError,W,false)!=0 then
        svg_error("rsvg_handle_close?")
    end if
    atom pGdkPixbuf = c_func(xrsvg_handle_get_pixbuf,{rsvg_handle})
--  c_proc(xg_object_unref,{rsvg_handle})
    g_object_unref(rsvg_handle)
    if pGdkPixbuf=NULL then svg_error("null pixbuff (defs-only svg?)") end if
    return pGdkPixbuf -- nb: needs g_object_unref(pGdkPixbuf) at some point...
end function

global procedure rasterize_svg_text(string text, outputfilename)
    atom result = rasterize_svg_pixbuf(text)
    if get_file_extension(outputfilename)!="png" then
        throw("get_file_extension(%s)!=png",{outputfilename})
    end if
    bool success = c_func(xgdk_pixbuf_save,{result, outputfilename, "png", pError, NULL})
    if not success then svg_error("cannot save png file") end if
    g_object_unref(result)
--  c_proc(xg_object_unref,{result})
--  return NULL
--end function
end procedure

--/*
static struct imv_bitmap *to_imv_bitmap(GdkPixbuf *bitmap)
{
  struct imv_bitmap *bmp = malloc(sizeof *bmp);
  bmp->width = gdk_pixbuf_get_width(bitmap);
  bmp->height = gdk_pixbuf_get_height(bitmap);
  bmp->format = IMV_ABGR;
  size_t len = bmp->width * bmp->height * 4;
  bmp->data = malloc(len);
  memcpy(bmp->data, gdk_pixbuf_get_pixels(bitmap), len);
  return bmp;
}
--*/

global function rasterize_svg_file(string inputfilename, outputfilename="")
    if xg_type_init=NULL then init_svg() end if
    object text = get_text(inputfilename)
    if text=-1 then throw("cannot read "&inputfilename) end if
    if outputfilename="" then
        outputfilename = substitute(inputfilename,"svg","png")
    end if
    if outputfilename==inputfilename then throw("oops: inputfile==outputfile") end if
--  {} = rasterize_svg_text(text, outputfilename)
    rasterize_svg_text(text, outputfilename)
    return outputfilename
end function

