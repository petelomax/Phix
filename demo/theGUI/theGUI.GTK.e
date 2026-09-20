--
-- demo\theGUI\theGUI.GTK.e
--
--  Simple wrapper for GTK proof-of-concept (poc) test files, intended
--  solely to make C ==> Phix much easier, rather than any long-term use.
--  Largely this is just a set of tediously trivial mini-shims to make
--  Phix code look more like C code, built in an ad-hoc and on demand
--  fashion, and never remotely intended to be in any sense "complete".
--  This and owt that uses it is expected to be GTK2 and GTK3 compatible.
--  In some cases that means "polyfills" for GTK2, that mimic GTK3 bits.
--
requires(WINDOWS) -- (LINUX incomplete/untested)
include cffi.e

constant currdir = current_dir(),
         m = machine_bits(),
         L = platform()=LINUX,
         gtkdir = sprintf("win_gtk%d",m)
--printf(1,"begin(GTK %d bits)\n",m)
assert(chdir(gtkdir))
constant gtk = iff(m=32?"libgtk-win32-2.0-0.dll":"libgtk-3-0.dll"),
         gdk = iff(m=32?"libgdk-win32-2.0-0.dll":"libgdk-3-0.dll"),
--       gdk = substitute(gtk,"gtk","gdk"),
         gto = iff(L?`libgobject-2.0.so.0`:`libgobject-2.0-0.dll`),
         gpx = iff(L?`libgdk_pixbuf-2.0.so.0`:`libgdk_pixbuf-2.0-0.dll`),
         pan = iff(L?`libpango-1.0.so.0`:`libpango-1.0-0.dll`),
         pnc = iff(L?`libpangocairo-1.0.so.0`:`libpangocairo-1.0-0.dll`),
         glx = iff(L?`libgdkglext-1.0-0.so.0`:`libgdkglext-win32-1.0-0.dll`),
         GTKLIB = open_dll(gtk), 
         GDKLIB = open_dll(gdk),
         GTKGDO = open_dll(gto),
         GDKPIX = open_dll(gpx),
         CAIRO  = open_dll("libcairo-2.dll"),
         LBGLIB = open_dll("libglib-2.0-0.dll"),
         PANGO  = open_dll(pan),
         PANCAR = open_dll(pnc),
         LIBGLX = iff(m=32?open_dll(glx):NULL),
         C_DBL = C_DOUBLE,
         GDK_GRAB_SUCCESS = 0,
         gtk_disable_setlocale = define_c_proc(GTKLIB, "gtk_disable_setlocale", {}),
         gtk_init_check = define_c_func(GTKLIB,"gtk_init_check",
            {C_PTR,     --  int* argc
             C_PTR},    --  char*** argv
            C_INT)      -- gboolean
         c_proc(gtk_disable_setlocale,{})
if gtk_init_check < 1 or c_func(gtk_init_check,{0,0})=0 then 
  crash("Failed to initialize GTK library!") 
end if 
assert(chdir(currdir))

global constant
         bGTK2 = m==32,
         bGTK3 = m==64,
--       CAIRO_ANTIALIAS_DEFAULT = 0,
         CAIRO_ANTIALIAS_NONE = 1,
--       CAIRO_ANTIALIAS_GRAY = 2,
--       CAIRO_ANTIALIAS_SUBPIXEL = 3,
--       CAIRO_ANTIALIAS_FAST = 4,
--       CAIRO_ANTIALIAS_GOOD = 5,
--       CAIRO_ANTIALIAS_BEST = 6,
         CAIRO_FORMAT_ARGB32 = 0,
         CAIRO_OPERATOR_CLEAR = 0,
--       CAIRO_OPERATOR_SOURCE = 1,
         CAIRO_OPERATOR_OVER = 2,
--typedef enum _cairo_font_slant {
         CAIRO_FONT_SLANT_NORMAL = 0,
--  CAIRO_FONT_SLANT_ITALIC,
--  CAIRO_FONT_SLANT_OBLIQUE
--} cairo_font_slant_t;
--typedef enum _cairo_font_weight {
         CAIRO_FONT_WEIGHT_NORMAL = 0,
--  CAIRO_FONT_WEIGHT_BOLD
--} cairo_font_weight_t;

         GDK_ACTION_COPY = 1 << 1,
         GDK_BUTTON1_MASK = 1 << 8,
         GDK_CURRENT_TIME = 0,
         GDK_KEY_ESCAPE = #FF1B,
         GDK_KEY_Escape = GDK_KEY_ESCAPE,   -- (as that's wot ChatGPT uses) [VK_ESC]
         GDK_KEY_Return = #FF0D,            --                              [VK_CR]
         GDK_KEY_Left = #FF51,              --                              [VK_LEFT]
         GDK_KEY_Up = #FF52,                --                              [VK_UP]
         GDK_KEY_Right = #FF53,             --                              [VK_RIGHT]
         GDK_KEY_Down = #FF54,              --                              [VK_DOWN]
         GDK_KEY_End = #FF57,               --                              [VK_END]
         GDK_KEY_Home = #FF50,              --                              [VK_HOME]
         GDK_KEY_space = ' ',
         GDK_SELECTION_CLIPBOARD = 69,
         GDK_WINDOW_TYPE_HINT_UTILITY = 5,
         GDK_WINDOW_TYPE_HINT_DOCK = 6,
         GDK_WINDOW_TYPE_HINT_POPUP_MENU = 9,   // A popup menu (from right-click)
--       GDK_WINDOW_STATE_WITHDRAWN  = 1 << 0,
         GDK_WINDOW_STATE_ICONIFIED  = 1 << 1,
--       GDK_WINDOW_STATE_MAXIMIZED  = 1 << 2,
--       GDK_WINDOW_STATE_STICKY     = 1 << 3,
--       GDK_WINDOW_STATE_FULLSCREEN = 1 << 4,
--       GDK_WINDOW_STATE_ABOVE      = 1 << 5,
--       GDK_WINDOW_STATE_BELOW      = 1 << 6,
         GDK_WINDOW_STATE_FOCUSED    = 1 << 7,
         GTK_DEST_DEFAULT_ALL = 0x07,
         GTK_ESC = #FF1B,
         GTK_FILE_CHOOSER_ACTION_OPEN = 0,
         GTK_FILE_CHOOSER_ACTION_SAVE = 1,
         GTK_FILE_CHOOSER_ACTION_SELECT_FOLDER = 2,
         GTK_FILE_CHOOSER_ACTION_CREATE_FOLDER = 3,
         GTK_WINDOW_TOPLEVEL = 0,
         GTK_WINDOW_POPUP = 1,
         GTK_ORIENTATION_HORIZONTAL = 0,
         GTK_ORIENTATION_VERTICAL = 1,
         GTK_RESPONSE_NONE = -1,
         GTK_RESPONSE_REJECT = -2,
         GTK_RESPONSE_ACCEPT = -3,
         GTK_RESPONSE_DELETE_EVENT = -4,
         GTK_RESPONSE_OK     = -5,
         GTK_RESPONSE_CANCEL = -6,
         GTK_RESPONSE_CLOSE  = -7,
         GTK_RESPONSE_YES    = -8,
         GTK_RESPONSE_NO     = -9,
         GTK_RESPONSE_APPLY  = -10,
         GTK_RESPONSE_HELP   = -11,
         GDK_SB_H_DOUBLE_ARROW = 108,
         GDK_SB_V_DOUBLE_ARROW = 116,

--              GDK_EXPOSURE_MASK               = 0x000002, -- 1 << 1,
                GDK_POINTER_MOTION_MASK         = 0x000004, -- 1 << 2,
--              GDK_POINTER_MOTION_HINT_MASK    = 0x000008, -- 1 << 3,
--              GDK_BUTTON_MOTION_MASK          = 0x000010, -- 1 << 4,
--              GDK_BUTTON1_MOTION_MASK         = 0x000020, -- 1 << 5,
--              GDK_BUTTON2_MOTION_MASK         = 0x000040, -- 1 << 6,
--              GDK_BUTTON3_MOTION_MASK         = 0x000080, -- 1 << 7,
                GDK_BUTTON_PRESS_MASK           = 0x000100, -- 1 << 8,
                GDK_BUTTON_RELEASE_MASK         = 0x000200, -- 1 << 9,
--              GDK_KEY_PRESS_MASK              = 0x000400, -- 1 << 10,
--              GDK_KEY_RELEASE_MASK            = 0x000800, -- 1 << 11,
                GDK_ENTER_NOTIFY_MASK           = 0x001000, -- 1 << 12,
                GDK_LEAVE_NOTIFY_MASK           = 0x002000, -- 1 << 13,
--              GDK_FOCUS_CHANGE_MASK           = 0x004000, -- 1 << 14,
--              GDK_STRUCTURE_MASK              = 0x008000, -- 1 << 15,
--              GDK_PROPERTY_CHANGE_MASK        = 0x010000, -- 1 << 16,
--              GDK_VISIBILITY_NOTIFY_MASK      = 0x020000, -- 1 << 17,
--              GDK_PROXIMITY_IN_MASK           = 0x040000, -- 1 << 18,
--              GDK_PROXIMITY_OUT_MASK          = 0x080000, -- 1 << 19,
--              GDK_SUBSTRUCTURE_MASK           = 0x100000, -- 1 << 20,
                GDK_SCROLL_MASK                 = 0x200000, -- 1 << 21,
--              GDK_ALL_EVENTS_MASK             = 0x3FFFFE,
--              } GdkEventMask;
--              GDK_BKE_MASK = 0b0101_0000_0010,
--              GDK_BKE_MASK = 0x0502,
                GDK_HAND_MASK = or_all({GDK_BUTTON_PRESS_MASK,
                                        GDK_BUTTON_RELEASE_MASK,
                                        GDK_POINTER_MOTION_MASK,
                                        GDK_ENTER_NOTIFY_MASK,
                                        GDK_LEAVE_NOTIFY_MASK})

global atom 
    -- cairo
    x_cairo_clip,
    x_cairo_create,
    x_cairo_destroy,
    x_cairo_fill,
    x_cairo_font_face_destroy,
    x_cairo_line_to,
    x_cairo_image_surface_create,
    x_cairo_image_surface_get_data,
    x_cairo_image_surface_get_stride,
    x_cairo_move_to,
    x_cairo_set_font_face,
    x_cairo_set_font_size,
    x_cairo_set_operator,
    x_cairo_surface_destroy,
    x_cairo_toy_font_face_create,
    x_cairo_paint,
    x_cairo_rectangle,
    x_cairo_set_antialias,
    x_cairo_set_dash,
    x_cairo_set_line_width,
    x_cairo_set_source_rgb,
    x_cairo_set_source_rgba,
    x_cairo_show_text,
    x_cairo_stroke,
    x_cairo_text_extents,
    -- gobject
    x_g_free,
    x_g_object_unref,
    x_g_slist_free,
    x_g_signal_connect_data,
    -- gdk
    x_gdk_atom_intern,
    x_gdk_cairo_create,
    x_gdk_cairo_set_source_pixbuf,
    x_gdk_cursor_new_for_display,
    x_gdk_cursor_new_from_name,
    x_gdk_keyboard_grab,
    x_gdk_keyboard_ungrab,
    x_gdk_pixbuf_copy,
    x_gdk_pixbuf_get_height,
    x_gdk_pixbuf_get_n_channels,
    x_gdk_pixbuf_get_pixels,
    x_gdk_pixbuf_get_rowstride,
    x_gdk_pixbuf_get_width,
    x_gdk_pixbuf_new_from_file,
--  x_gdk_pixbuf_save, -- no such thing! (in my 64-bit dll, though is in my 32-bit dll)
    x_gdk_pixbuf_savev,
    x_gdk_pixbuf_save_to_buffer,
    x_gdk_pointer_grab,
    x_gdk_pointer_ungrab,
    x_gdk_screen_get_monitor_at_window,
    x_gdk_screen_get_monitor_geometry,
    x_gdk_win32_drawable_get_handle,
    x_gdk_win32_window_get_handle,
    x_gdk_win32_window_get_impl_hwnd,
    x_gdk_window_get_display,
    x_gdk_window_get_state,
    x_gdk_window_set_cursor,
    -- gtk
-->
    x_gtk_box_new,
    x_gtk_box_pack_start,
    x_gtk_button_new_with_label,
    x_gtk_clipboard_clear,
    x_gtk_clipboard_get,
--  x_gtk_clipboard_set_can_store,
    x_gtk_clipboard_set_image,
    x_gtk_clipboard_set_with_data,
--  x_gtk_clipboard_store,
    x_gtk_clipboard_wait_for_image,
    x_gtk_container_add,
    x_gtk_dialog_run,
    x_gtk_drag_dest_set,
--  x_gtk_drag_get_data,
--  x_gtk_drag_source_set,
    x_gtk_drawing_area_new,
    x_gtk_hbox_new,
    x_gtk_vbox_new,
    x_gtk_file_chooser_dialog_new,
    x_gtk_file_chooser_add_filter,
    x_gtk_file_chooser_get_filename,
    x_gtk_file_chooser_get_filenames,
    x_gtk_file_chooser_set_select_multiple,
    x_gtk_file_filter_new,
    x_gtk_file_filter_add_pattern,
    x_gtk_file_filter_set_name,
    x_gtk_fixed_put,
    x_gtk_gl_area_new,
    x_gtk_gl_area_make_current,
    x_gtk_gl_area_set_required_version,
    x_gtk_gl_area_set_has_depth_buffer,
    x_gtk_gl_area_set_has_stencil_buffer,
--  x_gtk_gl_area_swap_buffers,
    x_gtk_grab_add,
    x_gtk_grab_remove,
    x_gtk_init,
    x_gtk_label_set_text,
    x_gtk_main,
    x_gtk_selection_data_get_data,
    x_gtk_selection_data_get_target,
    x_gtk_selection_data_set,
--  x_gtk_selection_data_set_pixbuf,
--  x_gtk_target_entry_new,
    x_gtk_target_list_add,
    x_gtk_target_list_new,
    x_gtk_target_table_new_from_list,
    x_gtk_tooltip_set_text,
    x_gtk_tooltip_trigger_tooltip_query,
    x_gtk_widget_add_events,
    x_gtk_widget_destroy,
-- oh, wer'e not using this anyway... (would need a GTK2 shim, btw)
--  gtk_widget_get_allocated_width,
    x_gtk_widget_get_allocation, 
    x_gtk_widget_get_display,
    x_gtk_widget_get_parent,
--  gtk_widget_get_parent_window,
    x_gtk_widget_get_visible,
    x_gtk_widget_get_window,
    x_gtk_widget_grab_focus,
    x_gtk_widget_hide,
    x_gtk_widget_queue_resize,
    x_gtk_widget_set_can_focus,
    x_gtk_widget_set_has_tooltip,
    x_gtk_widget_set_has_window,
    x_gtk_widget_set_size_request,
    x_gtk_widget_queue_draw,
    x_gtk_widget_show,
    x_gtk_widget_show_all,
    x_gtk_widget_size_allocate,
    x_gtk_window_deiconify,
    x_gtk_window_get_position,
    x_gtk_window_get_screen,
    x_gtk_window_get_size,
    x_gtk_window_move,
    x_gtk_window_new,
    x_gtk_window_present,
    x_gtk_window_resize,
    x_gtk_window_set_decorated,
    x_gtk_window_set_default_size,
    x_gtk_window_set_modal,
    x_gtk_window_set_resizable,
    x_gtk_window_set_skip_pager_hint,
    x_gtk_window_set_skip_taskbar_hint,
    x_gtk_window_set_title,
    x_gtk_window_set_transient_for,
    x_gtk_window_set_type_hint,
    x_pango_cairo_create_layout,
    x_pango_cairo_font_map_get_default,
    x_pango_cairo_show_layout,
    x_pango_font_description_from_string,
    x_pango_font_description_free,
    x_pango_font_map_create_context,
    x_pango_layout_get_pixel_size,
    x_pango_layout_new,
    x_pango_layout_set_font_description,
    x_pango_layout_set_text

global atom
    x_gtk_main_quit,
    -- structures
    id_cairo_text_extents_t,
    p_cairo_text_extents_t,
    idGdkEventButton,
    idGdkEventConfigure,
    idGdkEventExpose,
    idGdkEventKey,
    idGdkEventMotion,
    idGdkEventWindowState,
    idGdkRectangle, 
    pRECT, gtkRECT

--local atom 
--  idGtkTargetEntry

        x_cairo_clip = define_c_proc(CAIRO,"cairo_clip",
            {C_PTR})    --  cairo_t* cr
        x_cairo_create = define_c_func(CAIRO,`cairo_create`,
            {C_PTR},    --  cairo_surface_t *target
            C_PTR)      -- cairo_public cairo_t *
        x_cairo_destroy = define_c_proc(CAIRO,"cairo_destroy",
            {C_PTR})    --  cairo_t* cr
        x_cairo_fill = define_c_proc(CAIRO,"cairo_fill",
            {C_PTR})    --  cairo_t* cr
        x_cairo_font_face_destroy = define_c_proc(CAIRO,"cairo_font_face_destroy",
            {C_PTR})    --  cairo_font_face_t* font_face
        x_cairo_image_surface_create = define_c_func(CAIRO,"cairo_image_surface_create",
            {C_PTR,     --  cairo_format_t format
             C_INT,     --  int width
             C_INT},    --  int height
            C_PTR)      -- cairo_public cairo_surface_t*
        x_cairo_image_surface_get_data = define_c_func(CAIRO,"cairo_image_surface_get_data",
            {C_PTR},    --  cairo_surface_t* surface
            C_PTR)      -- unsigned char*
        x_cairo_image_surface_get_stride = define_c_func(CAIRO,"cairo_image_surface_get_stride",
            {C_PTR},    --  cairo_surface_t* surface
            C_INT)      -- int
        x_cairo_line_to  = define_c_proc(CAIRO,"cairo_line_to",
            {C_PTR,     --  cairo_t* cr
             C_DBL,     --  double x
             C_DBL})    --  double y
        x_cairo_move_to  = define_c_proc(CAIRO,"cairo_move_to",
            {C_PTR,     --  cairo_t* cr
             C_DBL,     --  double x
             C_DBL})    --  double y
        x_cairo_set_font_face  = define_c_proc(CAIRO,"cairo_set_font_face",
            {C_PTR,     --  cairo_t* cr
             C_PTR})    --  cairo_font_face_t* font_face
        x_cairo_set_font_size  = define_c_proc(CAIRO,"cairo_set_font_size",
            {C_PTR,     --  cairo_t* cr
             C_DBL})    --  double size
        x_cairo_set_operator  = define_c_proc(CAIRO,"cairo_set_operator",
            {C_PTR,     --  cairo_t* cr
             C_INT})    --  cairo_operator_t op
        x_cairo_surface_destroy = define_c_proc(CAIRO,`cairo_surface_destroy`,
            {C_PTR})    --  cairo_surface_t *surface
        x_cairo_toy_font_face_create = define_c_func(CAIRO,"cairo_toy_font_face_create",
            {C_PTR,     --  const char* family
             C_INT,     --  cairo_font_slant_t slant
             C_INT},    --  cairo_font_weight_t weight
            C_PTR)      -- cairo_font_face_t*
        x_cairo_paint = define_c_proc(CAIRO,`cairo_paint`,
            {C_PTR})    --  cairo_t* cr
        x_cairo_rectangle = define_c_proc(CAIRO,"cairo_rectangle",
            {C_PTR,     --  cairo_t* cr
             C_DBL,     --  double x
             C_DBL,     --  double y
             C_DBL,     --  double width
             C_DBL})    --  double height
        x_cairo_set_antialias = define_c_proc(CAIRO,"cairo_set_antialias",
            {C_PTR,     --  cairo_t* cr
             C_INT})    --  cairo_antialias_t antialias
        x_cairo_set_dash = define_c_proc(CAIRO,"cairo_set_dash",
            {C_PTR,     --  cairo_t* cr
             C_PTR,     --  const double* dashes
             C_INT,     --  int num_dashes
             C_DBL})    --  double offset (always 0 here)
        x_cairo_set_line_width = define_c_proc(CAIRO,"cairo_set_line_width",
            {C_PTR,     --  cairo_t* cr
             C_DBL})    --  double width
        x_cairo_set_source_rgb = define_c_proc(CAIRO,"cairo_set_source_rgb",
            {C_PTR,     --  cairo_t* cr
             C_DBL,     --  double red
             C_DBL,     --  double green
             C_DBL})    --  double blue
        x_cairo_set_source_rgba = define_c_proc(CAIRO,"cairo_set_source_rgba",
            {C_PTR,     --  cairo_t* cr
             C_DBL,     --  double red
             C_DBL,     --  double green
             C_DBL,     --  double blue
             C_DBL})    --  double alpha
        x_cairo_show_text = define_c_proc(CAIRO,"cairo_show_text",
            {C_PTR,     --  cairo_t* cr
             C_PTR})    --  const char* utf8
        x_cairo_stroke = define_c_proc(CAIRO,"cairo_stroke",
            {C_PTR})    --  cairo_t* cr
        x_cairo_text_extents = define_c_proc(CAIRO,"cairo_text_extents",
            {C_PTR,     --  cairo_t* cairo
             C_PTR,     --  const char* utf8
             C_PTR})    --  cairo_text_extents_t* extents
        x_g_free = define_c_proc(LBGLIB,"g_free",
            {C_PTR})    --  gpointer mem
        x_g_object_unref = define_c_proc(GTKGDO,`g_object_unref`,{C_PTR})
        x_g_slist_free = define_c_proc(LBGLIB,"g_slist_free",
            {C_PTR})    --  GSList* list

        -- note that g_signal_connect is defined in the GTK sources as a #define of
        -- g_signal_connect_data(....,NULL,0), and is not exported from the dll/so.
        x_g_signal_connect_data = define_c_func(GTKGDO,"g_signal_connect_data",
            {C_PTR,     --  GObject* instance,              // aka handle
             C_PTR,     --  const gchar* detailed_signal,   // a string
             C_PTR,     --  GCallback c_handler,            // a callback
             C_PTR,     --  gpointer data,                  // data for ""
             C_PTR,     --  GClosureNotify destroy_data,    // (NULL here)
             C_INT},    --  GConnectFlags connect_flags     //     ""
            C_INT)      -- gulong // handler id (>0 for success)

-->
        x_gdk_atom_intern = define_c_func(GDKLIB,"gdk_atom_intern",
            {C_PTR,     --  const gchar* atom_name
             C_BOOL},   --  gboolean only_if_exists
            C_PTR)      -- GdkAtom
        x_gdk_cairo_create = define_c_func(GDKLIB,"gdk_cairo_create",
            {C_PTR},    --  GdkDrawable* drawable
            C_PTR)      -- cairo_t*
        x_gdk_cairo_set_source_pixbuf = define_c_proc(GDKLIB,`gdk_cairo_set_source_pixbuf`,
            {C_PTR,     --  cairo_t* cr
             C_PTR,     --  const GdkPixbuf* pixbuf
             C_DBL,     --  double pixbuf_x
             C_DBL})    --  double pixbuf_y
        x_gdk_cursor_new_for_display = define_c_func(GDKLIB,"gdk_cursor_new_for_display",
            {C_PTR,     --  GdkDisplay* display
             C_INT},    --  GdkCursorType cursor_type
            C_PTR)      -- GdkCursor*
        x_gdk_cursor_new_from_name = define_c_func(GDKLIB,"gdk_cursor_new_from_name",
            {C_PTR,     --  GdkDisplay* display
             C_PTR},    --  const gchar* name
            C_PTR)      -- GdkCursor*
-- GDK_DEPRECATED_IN_3_0_FOR(gdk_device_grab)*2
        x_gdk_keyboard_grab = define_c_func(GDKLIB,"gdk_keyboard_grab",
            {C_PTR,     --  GdkWindow* window
             C_BOOL,    --  gboolean owner_events
             C_UINT},   --  guint32 time_
            C_INT)      -- GdkGrabStatus
--GDK_DEPRECATED_IN_3_0_FOR(gdk_device_ungrab)*2
        x_gdk_keyboard_ungrab = define_c_proc(GDKLIB,"gdk_keyboard_ungrab",
            {C_UINT})   --  guint32 time_
        x_gdk_pixbuf_copy = define_c_func(GDKPIX,`gdk_pixbuf_copy`,
            {C_PTR},    --  const GdkPixbuf* pixbuf
            C_PTR)      -- GdkPixbuf*
        x_gdk_pixbuf_get_height = define_c_func(GDKPIX,`gdk_pixbuf_get_height`,
            {C_PTR},    --  const GdkPixbuf* pixbuf
            C_INT)      -- int
        x_gdk_pixbuf_get_n_channels = define_c_func(GDKPIX,`gdk_pixbuf_get_n_channels`,
            {C_PTR},    --  const GdkPixbuf* pixbuf
            C_PTR)      -- int
        x_gdk_pixbuf_get_pixels = define_c_func(GDKPIX,`gdk_pixbuf_get_pixels`,
            {C_PTR},    --  const GdkPixbuf* pixbuf
            C_PTR)      -- guchar*
        x_gdk_pixbuf_get_rowstride = define_c_func(GDKPIX,`gdk_pixbuf_get_rowstride`,
            {C_PTR},    --  const GdkPixbuf* pixbuf
            C_INT)      -- int
        x_gdk_pixbuf_get_width = define_c_func(GDKPIX,`gdk_pixbuf_get_width`,
            {C_PTR},    --  const GdkPixbuf* pixbuf
            C_INT)      -- int
        x_gdk_pixbuf_new_from_file = define_c_func(GDKPIX,`gdk_pixbuf_new_from_file`,
            {C_PTR,     --  const char* filename
             C_PTR},    --  ??
            C_PTR)      -- GdkPixbuf*
--  gdk_pixbuf_save(pixbuf, tmp, "png", NULL)
-- no such thing! (in my \win_gtk64\, though there is one in \win_gtk32\)
--      x_gdk_pixbuf_save = define_c_func(GDKPIX,`gdk_pixbuf_save`,
--          {C_PTR,     --  GdkPixbuf* pixbuf
--           C_PTR,     --  const char* filename
--           C_PTR,     --  const char* type
--           C_PTR},    --  GError** error
--          C_BOOL)     -- gboolean
--/*
gboolean gdk_pixbuf_save           (GdkPixbuf* pixbuf, 
                                    const char* filename, 
                                    const char* type, 
                                    GError** error,
                                    ...) G_GNUC_NULL_TERMINATED;
--*/
        x_gdk_pixbuf_savev = define_c_func(GDKPIX,`gdk_pixbuf_savev`,
            {C_PTR,     --  GdkPixbuf* pixbuf
             C_PTR,     --  const char* filename
             C_PTR,     --  const char* type
             C_PTR,     --  char** option_keys,
             C_PTR,     --  char** option_keys,
             C_PTR},    --  GError** option_values
            C_BOOL)     -- gboolean
        x_gdk_pixbuf_save_to_buffer = define_c_func(GDKPIX,`gdk_pixbuf_save_to_buffer`,
            {C_PTR,     --  GdkPixbuf* pixbuf
             C_PTR,     --  gchar** buffer
             C_PTR,     --  gsize* buffer_size
             C_PTR,     --  const char* type
             C_PTR,     --  GError** error
             C_PTR},    --  (NULL terminator)
            C_BOOL)     -- gboolean
--/*
gboolean gdk_pixbuf_save_to_buffer      (GdkPixbuf* pixbuf,
                     gchar** buffer,
                     gsize* buffer_size,
                     const char* type, 
                     GError** error,
                     ...) G_GNUC_NULL_TERMINATED;

gboolean gdk_pixbuf_save_to_bufferv     (GdkPixbuf* pixbuf,
                     gchar** buffer,
                     gsize* buffer_size,
                     const char* type, 
                     char** option_keys,
                     char** option_values,
                     GError** error);

gboolean gdk_pixbuf_save_to_buffer (
    GdkPixbuf* pixbuf,
    gchar** buffer,
    gsize* buffer_size,
    const char* type,
    GError** error,
    ...
);
gboolean gdk_pixbuf_save_to_bufferv     (GdkPixbuf* pixbuf,
                     gchar** buffer,
                     gsize* buffer_size,
                     const char* type, 
                     char** option_keys,
                     char** option_values,
                     GError** error);

--*/
        x_gdk_pointer_grab = define_c_func(GDKLIB,"gdk_pointer_grab",
            {C_PTR,     --  GdkWindow* window
             C_BOOL,    --  gboolean owner_events
             C_INT,     --  GdkEventMask event_mask
             C_PTR,     --  GdkWindow* confine_to
             C_PTR,     --  GdkCursor* cursor
             C_UINT},   --  guint32 time_
            C_INT)      -- GdkGrabStatus
        x_gdk_pointer_ungrab = define_c_proc(GDKLIB,"gdk_pointer_ungrab",
            {C_UINT})   --  guint32 time_
        x_gdk_screen_get_monitor_at_window = define_c_func(GDKLIB,"gdk_screen_get_monitor_at_window",
            {C_PTR,     --  GdkScreen* screen
             C_PTR},    --  GdkWindow* window
            C_INT)      -- gint
        x_gdk_screen_get_monitor_geometry = define_c_proc(GDKLIB,"gdk_screen_get_monitor_geometry",
            {C_PTR,     --  GdkScreen* screen
             C_INT,     --  gint monitor_num
             C_PTR})    --  GdkRectangle* dest
        x_gdk_window_set_cursor = define_c_proc(GDKLIB,"gdk_window_set_cursor",
            {C_PTR,     --  GdkWindow* window
             C_PTR})    --  GdkCursor* cursor
    if bGTK2 then
        x_gdk_win32_drawable_get_handle = define_c_func(GDKLIB,"gdk_win32_drawable_get_handle",
            {C_PTR},    --  GdkDrawable* drawable
            C_PTR)      -- HGDIOBJ
    else
        x_gdk_win32_window_get_handle = define_c_func(GDKLIB,"gdk_win32_window_get_handle",
            {C_PTR},    --  GdkWindow* window
            C_PTR)      -- HGDIOBJ
    end if
        x_gdk_win32_window_get_impl_hwnd = define_c_func(GDKLIB,"gdk_win32_window_get_impl_hwnd",
            {C_PTR},    --  GdkWindow* window
            C_PTR)      -- HWND
        x_gdk_window_get_display = define_c_func(GDKLIB,"gdk_window_get_display",
            {C_PTR},    --  GdkWindow* window
            C_PTR)      -- GdkDisplay*
        x_gdk_window_get_state = define_c_func(GDKLIB,"gdk_window_get_state",
            {C_PTR},    --  GdkWindow* window
            C_INT)      -- GdkWindowState
if bGTK3 then
        x_gtk_box_new = define_c_func(GTKLIB,"gtk_box_new",
            {C_INT,     --  GtkOrientation orientation
             C_BOOL,    --  gboolean homogeneous
             C_INT},    --  gint spacing
            C_PTR)      -- GtkWidget*
end if
        x_gtk_box_pack_start = define_c_proc(GTKLIB,"gtk_box_pack_start",
            {C_PTR,     --  GtkBox* box
             C_PTR,     --  GtkWidget* child
             C_INT,     --  gboolean expand
             C_INT,     --  gboolean fill
             C_INT})    --  guint padding
        x_gtk_button_new_with_label = define_c_func(GTKLIB,`gtk_button_new_with_label`,
            {C_PTR},    --  const gchar* label
            C_PTR)      -- GtkWidget*
        x_gtk_clipboard_clear = define_c_proc(GTKLIB,`gtk_clipboard_clear`,
            {C_PTR})    --  GtkClipboard* clipboard
        x_gtk_clipboard_get = define_c_func(GTKLIB,`gtk_clipboard_get`,
            {C_PTR},    --  GdkAtom selection
            C_PTR)      -- GtkClipboard*
--      x_gtk_clipboard_set_can_store = define_c_proc(GTKLIB,"gtk_clipboard_set_can_store",
--          {C_PTR,     --  GtkClipboard* clipboard
--           C_PTR,     --  const GtkTargetEntry* targets
--           C_INT})    --  gint n_targets
--      x_gtk_clipboard_store = define_c_proc(GTKLIB,"gtk_clipboard_store",
--          {C_PTR})    --  GtkClipboard* clipboard
        x_gtk_clipboard_set_image = define_c_proc(GTKLIB,"gtk_clipboard_set_image",
            {C_PTR,     --  GtkClipboard* clipboard
             C_PTR})    --  GdkPixbuf* pixbuf
        x_gtk_clipboard_set_with_data = define_c_func(GTKLIB,`gtk_clipboard_set_with_data`,
            {C_PTR,     --  GtkClipboard* clipboard
             C_PTR,     --  const GtkTargetEntry* targets
             C_UINT,    --  guint n_targets
             C_PTR,     --  GtkClipboardGetFunc get_func
             C_PTR,     --  GtkClipboardClearFunc clear_func
             C_PTR},    --  gpointer user_data
            C_BOOL)     -- gboolean
        x_gtk_clipboard_wait_for_image = define_c_func(GTKLIB,`gtk_clipboard_wait_for_image`,
            {C_PTR},    --  GtkClipboard* clipboard
            C_PTR)      -- GdkPixbuf*
        x_gtk_container_add = define_c_proc(GTKLIB,"gtk_container_add",
            {C_PTR,     --  GtkContainer* container
             C_PTR})    --  GtkWidget* widget
        x_gtk_dialog_run = define_c_func(GTKLIB,"gtk_dialog_run",
            {C_PTR},    --  GtkDialog* dialog
            C_INT)      -- gint
        x_gtk_drag_dest_set = define_c_proc(GTKLIB,"gtk_drag_dest_set",
            {C_PTR,     --  GtkWidget* widget
             C_INT,     --  GtkDestDefaults flags
             C_PTR,     --  const GtkTargetEntry* targets
             C_INT,     --  gint n_targets
             C_INT})    --  GdkDragAction actions
--      x_gtk_drag_get_data = define_c_proc(GTKLIB,"gtk_drag_get_data",
--          {C_PTR,     --  GtkWidget* widget
--           C_PTR,     --  GdkDragContext* context
--           C_PTR,     --  GdkAtom target
--           C_INT})    --  guint32 time_
--      x_gtk_drag_source_set = define_c_proc(GTKLIB,"gtk_drag_get_data",
--          {C_PTR,     --  GtkWidget* widget
--           C_INT,     --  GdkModifierType start_button_mask
--           C_PTR,     --  const GtkTargetEntry* targets
--           C_INT,     --  gint n_targets
--           C_INT})    --  GdkDragAction actions
        x_gtk_drawing_area_new = define_c_func(GTKLIB,"gtk_drawing_area_new",
            {},         --  void
            C_PTR)      -- GtkWidget*
        x_gtk_hbox_new = define_c_func(GTKLIB,"gtk_hbox_new",
            {C_BOOL,    --  gboolean homogeneous
             C_INT},    --  gint spacing
            C_PTR)      -- GtkWidget*
        x_gtk_vbox_new = define_c_func(GTKLIB,"gtk_vbox_new",
            {C_BOOL,    --  gboolean homogeneous
             C_INT},    --  gint spacing
            C_PTR)      -- GtkWidget*
        x_gtk_init = define_c_proc(GTKLIB,"gtk_init",
            {})         --  void
        x_gtk_window_deiconify = define_c_proc(GTKLIB,`gtk_window_deiconify`,
            {C_PTR})    --  GtkWindow* window
        x_gtk_window_get_position = define_c_proc(GTKLIB,`gtk_window_get_position`,
            {C_PTR,     --  GtkWindow* window
             C_PTR,     --  gint* root_x
             C_PTR})    --  gint* root_y
        x_gtk_window_get_screen = define_c_func(GTKLIB,"gtk_window_get_screen",
            {C_PTR},    --  GtkWindow* window
            C_PTR)      -- GdkScreen*
        x_gtk_window_get_size = define_c_proc(GTKLIB,`gtk_window_get_size`,
            {C_PTR,     --  GtkWindow* window
             C_PTR,     --  gint* width
             C_PTR})    --  gint* height
        x_gtk_window_move = define_c_proc(GTKLIB,`gtk_window_move`,
            {C_PTR,     --  GtkWindow* window
             C_INT,     --  gint x
             C_INT})    --  gint y
        x_gtk_window_new = define_c_func(GTKLIB,"gtk_window_new",
            {C_INT},    --  GtkWindowType type // usually GTK_WINDOW_TOPLEVEL (nb gone in GTK4)
            C_PTR)      -- GtkWidget*
        x_gtk_window_set_decorated = define_c_proc(GTKLIB,`gtk_window_set_decorated`,
            {C_PTR,     --  GtkWindow* window
             C_BOOL})   --  gboolean setting
        x_gtk_window_set_default_size = define_c_proc(GTKLIB,"gtk_window_set_default_size",
            {C_PTR,     --  GtkWindow* window
             C_INT,     --  gint width
             C_INT})    --  gint height
        x_gtk_label_set_text = define_c_proc(GTKLIB,"gtk_label_set_text",
            {C_PTR,     --  GtkLabel* label
             C_PTR})    --  const gchar* str
        x_gtk_main = define_c_proc(GTKLIB,"gtk_main",{})
        x_gtk_main_quit = define_c_proc(GTKLIB,"gtk_main_quit",{})
        x_gtk_widget_destroy = define_c_proc(GTKLIB,"gtk_widget_destroy",
            {C_PTR})    --  GtkWidget* widget
        x_gtk_widget_get_allocation = define_c_proc(GTKLIB,"gtk_widget_get_allocation",
            {C_PTR,     --  GtkWidget* widget
             C_PTR})    --  GtkAllocation* allocation (aka a GtkRectange)
        x_gtk_widget_get_display = define_c_func(GTKLIB,"gtk_widget_get_display",
            {C_PTR},    --  GtkWidget* widget
            C_PTR)      -- GdkDisplay*
        x_gtk_widget_queue_draw = define_c_proc(GTKLIB,"gtk_widget_queue_draw",
            {C_PTR})    --  GtkWidget* widget
        x_gtk_widget_show = define_c_proc(GTKLIB,`gtk_widget_show`,
            {C_PTR})    --  GtkWindow* window,  // aka handle
        x_gtk_widget_show_all = define_c_proc(GTKLIB,"gtk_widget_show_all",
            {C_PTR})    --  GtkWindow* window,  // aka handle
        x_gtk_widget_size_allocate = define_c_proc(GTKLIB,"gtk_widget_size_allocate",
            {C_PTR,     --  GtkWidget* widget
             C_PTR})    --  GtkAllocation* allocation
        x_gtk_widget_get_visible = define_c_func(GTKLIB,"gtk_widget_get_visible",
            {C_PTR},    --  GtkWidget* widget
            C_BOOL)     -- gboolean
        x_gtk_widget_get_window = define_c_func(GTKLIB,"gtk_widget_get_window",
            {C_PTR},    --  GtkWidget* widget
            C_PTR)      -- GdkWindow*
        x_gtk_widget_grab_focus = define_c_func(GTKLIB,"gtk_widget_grab_focus",
            {C_PTR},    --  GtkWidget* widget
            C_BOOL)     -- gboolean
        x_gtk_widget_hide = define_c_proc(GTKLIB,`gtk_widget_hide`,
            {C_PTR})    --  GtkWindow* window,  // aka handle
        x_gtk_widget_set_has_tooltip = define_c_proc(GTKLIB,`gtk_widget_set_has_tooltip`,
            {C_PTR,     --  GtkWidget* widget
             C_BOOL})   --  gboolean has_tooltip
        x_gtk_widget_set_has_window = define_c_proc(GTKLIB,`gtk_widget_set_has_window`,
            {C_PTR,     --  GtkWidget* widget
             C_BOOL})   --  gboolean has_window
        x_gtk_selection_data_get_data = define_c_func(GTKLIB,"gtk_selection_data_get_data",
            {C_PTR},    --  const GtkSelectionData* selection_data
            C_PTR)      -- const guchar*
        x_gtk_selection_data_get_target = define_c_func(GTKLIB,"gtk_selection_data_get_target",
            {C_PTR},    --  const GtkSelectionData* selection_data
            C_PTR)      -- GdkAtom
        x_gtk_selection_data_set = define_c_proc(GTKLIB,"gtk_selection_data_set",
            {C_PTR,     --  GtkSelectionData* selection_data
             C_PTR,     --  GdkAtom type
             C_INT,     --  gint format
             C_PTR,     --  const guchar* data
             C_INT})    --  gint length
--      x_gtk_selection_data_set_pixbuf = define_c_func(GTKLIB,"gtk_selection_data_set_pixbuf",
--          {C_PTR,     --  GtkSelectionData* selection_data
--           C_INT},    --  GdkPixbuf* pixbuf
--          C_PTR)      -- gboolean
--if bGTK3 then
--      x_gtk_target_entry_new = define_c_func(GTKLIB,"gtk_target_entry_new",
--          {C_PTR,     --  const gchar* target
--           C_INT,     --  guint flags
--           C_INT},    --  guint info
--          C_PTR)      -- GtkTargetEntry*
--end if
        x_gtk_target_list_add = define_c_proc(GTKLIB,"gtk_target_list_add",
            {C_PTR,     --  GtkTargetList* list
             C_PTR,     --  GdkAtom target
             C_UINT,    --  guint flags
             C_UINT})   --  guint info
        x_gtk_target_list_new = define_c_func(GTKLIB,"gtk_target_list_new",
            {C_PTR,     --  const GtkTargetEntry* targets
             C_UINT},   --  guint ntargets
            C_PTR)      -- GtkTargetList*
        x_gtk_target_table_new_from_list = define_c_func(GTKLIB,"gtk_target_table_new_from_list",
            {C_PTR,     --  GtkTargetList* list
             C_PTR},    --  gint* n_targets
            C_PTR)      -- GtkTargetEntry*
        x_gtk_tooltip_set_text = define_c_proc(GTKLIB,"gtk_tooltip_set_text",
            {C_PTR,     --  GtkTooltip* tooltip
             C_PTR})    --  const gchar* text
        x_gtk_tooltip_trigger_tooltip_query = define_c_proc(GTKLIB,"gtk_tooltip_trigger_tooltip_query",
            {C_PTR})    --  GdkDisplay* display
        x_gtk_widget_add_events = define_c_proc(GTKLIB,"gtk_widget_add_events",
            {C_PTR,     --  GtkWidget* widget
             C_INT})    --  gint events
        x_gtk_widget_get_parent = define_c_func(GTKLIB,"gtk_widget_get_parent",
            {C_PTR},    --  GtkWidget* widget
            C_PTR)      -- GtkWidget*
        x_gtk_widget_queue_resize = define_c_proc(GTKLIB,"gtk_widget_queue_resize",
            {C_PTR})    --  GtkWidget* widget
        x_gtk_file_chooser_add_filter = define_c_proc(GTKLIB,"gtk_file_chooser_add_filter",
            {C_PTR,     --  GtkFileChooser* chooser
             C_PTR})    --  GtkFileFilter* filter
        x_gtk_file_chooser_get_filename = define_c_func(GTKLIB,"gtk_file_chooser_get_filename",
            {C_PTR},    --  GtkFileChooser* chooser
            C_PTR)      -- gchar*
        x_gtk_file_chooser_get_filenames = define_c_func(GTKLIB,"gtk_file_chooser_get_filenames",
            {C_PTR},    --  GtkFileChooser* chooser
            C_PTR)      -- GSList*
        x_gtk_file_chooser_set_select_multiple = define_c_proc(GTKLIB,"gtk_file_chooser_set_select_multiple",
            {C_PTR,     --  GtkFileChooser* chooser
             C_BOOL})   --  gboolean select_multiple
        x_gtk_file_filter_new = define_c_func(GTKLIB,"gtk_file_filter_new",
            {},         --  (void)
            C_PTR)      -- GtkFileFilter*
        x_gtk_file_filter_add_pattern = define_c_proc(GTKLIB,"gtk_file_filter_add_pattern",
            {C_PTR,     --  GtkFileFilter* filter
             C_PTR})    --  const gchar* pattern
        x_gtk_file_filter_set_name = define_c_proc(GTKLIB,"gtk_file_filter_set_name",
            {C_PTR,     --  GtkFileFilter* filter
             C_PTR})    --  const gchar* name
        x_gtk_fixed_put = define_c_proc(GTKLIB,"gtk_fixed_put",
            {C_PTR,     --  GtkFixed* fixed
             C_PTR,     --  GtkWidget* widget
             C_INT,     --  gint x
             C_INT})    --  gint y
--DEV move up...
        x_gtk_file_chooser_dialog_new = define_c_func(GTKLIB,"gtk_file_chooser_dialog_new",
            {C_PTR,     --  const gchar* title
             C_PTR,     --  GtkWindow* parent
             C_PTR,     --  GtkFileChooserAction action
             C_PTR,     --  const gchar* first_button_text
             C_INT,     --  Type: response id for 1st button
             C_PTR,     --  const gchar* second_button_text
             C_INT,     --  Type: response id for 2nd button
             C_INT,     --  NULL
             C_INT},    --  NULL
            C_PTR)      -- GtkWidget*
if bGTK3 then
        x_gtk_gl_area_new = define_c_func(GTKLIB,"gtk_gl_area_new",
            {},         --  void
            C_PTR)      -- GtkWidget*
        x_gtk_gl_area_make_current = define_c_proc(GTKLIB,"gtk_gl_area_make_current",
            {C_PTR})    --  GtkGLArea* area
        x_gtk_gl_area_set_required_version = define_c_proc(GTKLIB,"gtk_gl_area_set_required_version",
            {C_PTR,     --  GtkGLArea* area
             C_INT,     --  int major
             C_INT})    --  int minor
        x_gtk_gl_area_set_has_depth_buffer = define_c_proc(GTKLIB,"gtk_gl_area_set_has_depth_buffer",
            {C_PTR,     --  GtkGLArea* area
             C_BOOL})   --  gboolean has_depth_buffer
        x_gtk_gl_area_set_has_stencil_buffer = define_c_proc(GTKLIB,"gtk_gl_area_set_has_stencil_buffer",
            {C_PTR,     --  GtkGLArea* area
             C_BOOL})   --  gboolean has_stencil_buffer
--      x_gtk_gl_area_swap_buffers = define_c_proc(GTKLIB,"gtk_gl_area_swap_buffers",
--          {C_PTR})    --  GtkGLArea* area
end if
        x_gtk_grab_add = define_c_proc(GTKLIB,"gtk_grab_add",
            {C_PTR})    --  GtkWidget* widget
        x_gtk_grab_remove = define_c_proc(GTKLIB,"gtk_grab_remove",
            {C_PTR})    --  GtkWidget* widget
        x_gtk_widget_set_can_focus = define_c_proc(GTKLIB,`gtk_widget_set_can_focus`,
            {C_PTR,     --  GtkWidget* widget
             C_BOOL})   --  gboolean can_focus
        x_gtk_widget_set_size_request = define_c_proc(GTKLIB,"gtk_widget_set_size_request",
            {C_PTR,     --  GtkWidget* widget   // aka handle
             C_INT,     --  gint width,
             C_INT})    --  gint height
        x_gtk_window_present = define_c_proc(GTKLIB,"gtk_window_present",
            {C_PTR})    --  GtkWindow* window
        x_gtk_window_resize = define_c_proc(GTKLIB,"gtk_window_resize",
            {C_PTR,     --  GtkWindow* window
             C_INT,     --  gint width
             C_INT})    --  gint height
--      gtk_widget_get_parent_window = define_c_func(GTKLIB,"gtk_widget_get_parent_window",
--          {C_PTR},    --  GtkWidget* widget
--          C_PTR)      -- GdkWindow*
--      gtk_widget_get_allocated_width = define_c_func(GTKLIB,"gtk_widget_get_allocated_width",
--          {C_PTR},    --  GtkWidget* widget
--          C_INT)      -- int
----            C_INT,false)    -- int
        x_gtk_window_set_modal = define_c_proc(GTKLIB,`gtk_window_set_modal`,
            {C_PTR,     --  GtkWindow* window
             C_BOOL})   --  gboolean modal
        x_gtk_window_set_resizable = define_c_proc(GTKLIB,`gtk_window_set_resizable`,
            {C_PTR,     --  GtkWindow* window,  // aka handle
             C_BOOL})   --  gboolean resizable
        x_gtk_window_set_skip_pager_hint = define_c_proc(GTKLIB,`gtk_window_set_skip_pager_hint`,
            {C_PTR,     --  GtkWindow* window,  // aka handle
             C_BOOL})   --  gboolean setting
        x_gtk_window_set_skip_taskbar_hint = define_c_proc(GTKLIB,`gtk_window_set_skip_taskbar_hint`,
            {C_PTR,     --  GtkWindow* window,  // aka handle
             C_BOOL})   --  gboolean setting
        x_gtk_window_set_title = define_c_proc(GTKLIB,"gtk_window_set_title",
            {C_PTR,     --  GtkWindow* window,  // aka handle
             C_PTR})    --  const gchar* title  // a string
        x_gtk_window_set_transient_for = define_c_proc(GTKLIB,`gtk_window_set_transient_for`,
            {C_PTR,     --  GtkWindow* window,  // aka handle
             C_PTR})    --  GtkWindow* parent   // ""
        x_gtk_window_set_type_hint = define_c_proc(GTKLIB,`gtk_window_set_type_hint`,
            {C_PTR,     --  GtkWindow* window
             C_INT})    --  GdkWindowTypeHint hint
        x_pango_cairo_create_layout = define_c_func(PANCAR,`pango_cairo_create_layout`,
            {C_PTR},    --  cairo_t* cr
            C_PTR)      -- PangoLayout*
        x_pango_cairo_font_map_get_default = define_c_func(PANCAR,`pango_cairo_font_map_get_default`,
            {},         --  (void)
            C_PTR)      -- PangoFontMap* 
        x_pango_cairo_show_layout = define_c_proc(PANCAR,`pango_cairo_show_layout`,
            {C_PTR,     --  cairo_t* cr
             C_PTR})    --  PangoLayout* layout
        x_pango_font_description_from_string = define_c_func(PANGO,`pango_font_description_from_string`,
            {C_PTR},    --  const char* str
            C_PTR)      -- PangoFontDescription* 
        x_pango_font_description_free = define_c_proc(PANGO,`pango_font_description_free`,
            {C_PTR})    --  PangoFontDescription* desc
        x_pango_font_map_create_context = define_c_func(PANGO,`pango_font_map_create_context`,
            {C_PTR},    --  PangoFontMap* fontmap
            C_PTR)      -- PangoContext*
        x_pango_layout_get_pixel_size = define_c_proc(PANGO,`pango_layout_get_pixel_size`,
            {C_PTR,     --  PangoLayout* layout
             C_PTR,     --  int* width
             C_PTR})    --  int* height
        x_pango_layout_new = define_c_func(PANGO,`pango_layout_new`,
            {C_PTR},    --  PangoContext* context
            C_PTR)      -- PangoLayout*
        x_pango_layout_set_font_description = define_c_proc(PANGO,`pango_layout_set_font_description`,
            {C_PTR,     --  PangoLayout* layout
             C_PTR})    --  const PangoFontDescription* desc
        x_pango_layout_set_text = define_c_proc(PANGO,`pango_layout_set_text`,
            {C_PTR,     --  PangoLayout* layout
             C_PTR,     --  const char* text
             C_INT})    --  int length

        id_cairo_text_extents_t = define_struct(`typedef struct {
                                                   double x_bearing;
                                                   double y_bearing;
                                                   double width;
                                                   double height;
                                                   double x_advance;
                                                   double y_advance;
                                                 } cairo_text_extents_t;`)
        p_cairo_text_extents_t = allocate_struct(id_cairo_text_extents_t,false)

        idGdkEventButton = define_struct("""typedef struct GdkEventButton {
                                              GdkEventType event_type;
                                              GdkWindow* window;
                                              gint8 send_event;
                                              guint32 time;
                                              gdouble x;
                                              gdouble y;
                                              gdouble* axes;
                                              ModifierType state;
                                              guint button;
                                              GdkDevice* device;
                                              gdouble x_root, y_root;
                                            };""")

        string tGdkEventConfigure = `typedef struct GdkEventConfigure {
                                       GdkEventType event_type;
                                       GdkWindow *window;
                                       gint8 send_event;
                                       gint x, y;
                                       gint width;
                                       gint height;
                                     };`
        idGdkEventConfigure = define_struct(tGdkEventConfigure)

        idGdkEventKey = define_struct(`typedef struct GdkEventKey {
                                        GdkEventType event_type;
                                        GdkWindow* window;
                                        byte sendEvent;
                                        uint time;
                                        ModifierType state;
                                        uint keyval;
                                        int length;
                                        char* string_;
                                        ushort hardwareKeycode;
                                        ubyte group;
                                       }`)
        string tGdkEventMotion = `typedef struct GdkEventMotion {
                                    GdkEventType event_type;
                                    GdkWindow* window;
                                    gint8 send_event;
                                    guint32 time;
                                    gdouble x;
                                    gdouble y;
                                    gdouble* axes;
                                    GdkModifierType* state;
                                    gint16 is_hint;
                                    GdkDevice* device;
                                    gdouble x_root;
                                    gdouble y_root;
                                  };`
        idGdkEventMotion = define_struct(tGdkEventMotion)

        idGdkEventWindowState = define_struct(`typedef struct GdkEventWindowState {
                                                 GdkEventType event_type;
                                                 GdkWindow* window;
                                                 gint8 send_event;
                                                 GdkWindowState changed_mask;
                                                 GdkWindowState new_window_state;
                                               };`)

        idGdkRectangle = define_struct("""typedef struct GdkRectangle {
                                          int x;
                                          int y;
                                          int width;
                                          int height;
                                        }""")

        pRECT = allocate_struct(idGdkRectangle,false)
        gtkRECT = pRECT
--      idGtkTargetEntry = define_struct("""typedef struct GtkTargetEntry {
--                                            gchar* target;
--                                            guint flags;
--                                            guint info;
--                                          };""")
        string tGdkEventExpose = `typedef struct _GdkEventExpose {
                                  GdkEventType type;
                                  GdkWindow *window;
                                  gint8 send_event;
                                  GdkRectangle area;
                                  GdkRegion *region;
                                  gint count; /* If non-zero, how many more events follow. */
                                 };`
        idGdkEventExpose = define_struct(tGdkEventExpose)

local constant pX = get_struct_field_addr(idGdkRectangle,pRECT,"x"),
               pY = get_struct_field_addr(idGdkRectangle,pRECT,"y"),
               pW = get_struct_field_addr(idGdkRectangle,pRECT,"width"),
               pH = get_struct_field_addr(idGdkRectangle,pRECT,"height")

local function tg_gtk_check_escape(atom winmain, event, /*data*/) -- (GTK only)
    integer keyval = get_struct_field(idGdkEventKey,event,"keyval")
    if keyval=GTK_ESC then
        c_proc(x_gtk_main_quit)
    end if
    return true
end function
global constant escape_key_cb = call_back({'+',tg_gtk_check_escape})

local function tg_double_array(sequence doubles)
    atom ptr = allocate(8*length(doubles))
    for i,d in doubles do
        poke(ptr+8*(i-1),atom_to_float64(d))
    end for
    return ptr
end function

global procedure cairo_clip(atom cairo)
    c_proc(x_cairo_clip,{cairo})
end procedure

global function cairo_create(atom surface)
    atom cairo = c_func(x_cairo_create,{surface})
    assert(cairo!=NULL)
    return cairo
end function

global procedure cairo_destroy(atom cairo)
    c_proc(x_cairo_destroy,{cairo})
end procedure

global procedure cairo_fill(atom cairo)
    c_proc(x_cairo_fill,{cairo})
end procedure

global procedure cairo_font_face_destroy(atom face)
    c_proc(x_cairo_font_face_destroy,{face})
end procedure

global function cairo_image_surface_create(integer fmt, width, height)
    atom cairo_surface = c_func(x_cairo_image_surface_create,{fmt,width,height})
    assert(cairo_surface!=NULL)
    return cairo_surface
end function

global function cairo_image_surface_get_data(atom surface)
    atom res = c_func(x_cairo_image_surface_get_data,{surface})
    assert(res!=NULL)
    return res
end function

global function cairo_image_surface_get_stride(atom surface)
    integer stride = c_func(x_cairo_image_surface_get_stride,{surface})
    assert(stride!=0)
    return stride
end function

global procedure cairo_line_to(atom cairo, x, y)
    c_proc(x_cairo_line_to,{cairo,x+0.5,y+0.5})
end procedure

global procedure cairo_move_to(atom cairo,x,y)
    c_proc(x_cairo_move_to,{cairo,x+0.5,y+0.5})
end procedure

global procedure cairo_set_font_face(atom cairo, face)
    c_proc(x_cairo_set_font_face,{cairo,face})
end procedure

global procedure cairo_set_font_size(atom cairo, size)
    c_proc(x_cairo_set_font_size,{cairo,size})
end procedure

global procedure cairo_set_operator(atom cairo, integer op)
    c_proc(x_cairo_set_operator,{cairo,op})
end procedure

global procedure cairo_surface_destroy(atom surface)
    c_proc(x_cairo_surface_destroy,{surface})
end procedure

global function cairo_toy_font_face_create(string family, integer slant, weight)
    atom font_face = c_func(x_cairo_toy_font_face_create,{family, slant, weight})
    assert(font_face!=NULL)
    return font_face
end function

global procedure cairo_paint(atom cairo)
    c_proc(x_cairo_paint,{cairo})
end procedure

global procedure cairo_rectangle(atom cairo, x, y, w, h)
    c_proc(x_cairo_rectangle,{cairo,x,y,w,h})
end procedure

global procedure cairo_set_antialias(atom cairo, integer antialias)
    c_proc(x_cairo_set_antialias,{cairo,antialias})
end procedure

global procedure cairo_set_dash(atom cairo, sequence dashes)
    atom pDash = tg_double_array(dashes)
    c_proc(x_cairo_set_dash,{cairo,pDash,length(dashes),0})
    free(pDash)
end procedure

global procedure cairo_set_line_width(atom cairo, width)
    c_proc(x_cairo_set_line_width,{cairo,width})
end procedure

global procedure cairo_set_source_rgb(atom cairo, r,g,b)
    c_proc(x_cairo_set_source_rgb,{cairo,r,g,b})
end procedure

global procedure cairo_set_source_rgba(atom cairo, r,g,b,a)
    c_proc(x_cairo_set_source_rgba,{cairo,r,g,b,a})
end procedure

global procedure cairo_show_text(atom cairo, string text)
    c_proc(x_cairo_show_text,{cairo,text})
end procedure

global procedure cairo_stroke(atom cairo)
    c_proc(x_cairo_stroke,{cairo})
end procedure

global function cairo_text_extents(atom cairo, string text)
    c_proc(x_cairo_text_extents,{cairo,text,p_cairo_text_extents_t})
    atom x_bearing = get_struct_field(id_cairo_text_extents_t,p_cairo_text_extents_t,"x_bearing"),
         y_bearing = get_struct_field(id_cairo_text_extents_t,p_cairo_text_extents_t,"y_bearing"),
             width = get_struct_field(id_cairo_text_extents_t,p_cairo_text_extents_t,"width"),
            height = get_struct_field(id_cairo_text_extents_t,p_cairo_text_extents_t,"height"),
         x_advance = get_struct_field(id_cairo_text_extents_t,p_cairo_text_extents_t,"x_advance"),
         y_advance = get_struct_field(id_cairo_text_extents_t,p_cairo_text_extents_t,"y_advance")
    -- bearing is offset between the origin, ie cairo_move_to(x,y), and ink,
    -- with y_bearing usually being negative, advance is how much that x,y
    -- would move, with y_advance usually being zero.
    return {x_bearing,y_bearing,width,height,x_advance,y_advance}
end function

global procedure g_free(atom pMem)
    c_proc(x_g_free,{pMem})
end procedure

global procedure g_object_unref(atom o)
    c_proc(x_g_object_unref,{o})
end procedure

global procedure g_slist_free(atom pList)
    c_proc(x_g_slist_free,{pList})
end procedure

global procedure g_signal_connect_data(atom handle, string signal, atom callback, data=NULL)
    if not is_call_back(callback) then ?9/0 end if
    -- ^ eg passing gtk_main_quit instead of gtk_main_quit_cb (as defined below)
    atom r = c_func(x_g_signal_connect_data,{handle,signal,callback,data,NULL,0})
    assert(r>0)
end procedure

global function gdk_atom_intern(string atom_name, bool only_if_exists)
    atom res = c_func(x_gdk_atom_intern,{atom_name,only_if_exists})
    return res
end function

global function gdk_cairo_create(atom drawable)
    atom cairo = c_func(x_gdk_cairo_create,{drawable})
    return cairo
end function

global procedure gdk_cairo_set_source_pixbuf(atom cairo, pixbuf, x, y)
    c_proc(x_gdk_cairo_set_source_pixbuf,{cairo, pixbuf, x, y})
end procedure

local atom gtk_col_resize_cursor = NULL,
           gtk_row_resize_cursor = NULL

global function gdk_cursor_new_for_display(atom display, integer cursor_type)
    atom csr
    if cursor_type=GDK_SB_H_DOUBLE_ARROW then
        csr = gtk_col_resize_cursor
    elsif cursor_type=GDK_SB_V_DOUBLE_ARROW then
        csr = gtk_row_resize_cursor
    else
        ?9/0
    end if
    if csr=NULL then
--      atom display = c_func(gdk_window_get_display,{window})
        csr = c_func(x_gdk_cursor_new_for_display,{display,cursor_type})
        if cursor_type=GDK_SB_H_DOUBLE_ARROW then
            gtk_col_resize_cursor = csr
        elsif cursor_type=GDK_SB_V_DOUBLE_ARROW then
            gtk_row_resize_cursor = csr
        end if
    end if
    return csr
end function

global function gdk_cursor_new_from_name(atom display, string name)
    atom csr
    if bGTK2 then
        integer what
        if name="col-resize" then
            what = GDK_SB_H_DOUBLE_ARROW
            csr = gtk_col_resize_cursor
        elsif name="row-resize" then
            what = GDK_SB_V_DOUBLE_ARROW
            csr = gtk_row_resize_cursor
        else
            ?9/0
        end if
        if csr=NULL then
            csr = gdk_cursor_new_for_display(display,what)
        end if
--GdkCursor* cursor = gdk_cursor_new(GDK_SB_H_DOUBLE_ARROW); // standard resize WE
--gdk_window_set_cursor(gdk_window, cursor);
--gdk_cursor_unref(cursor); // optional in GTK2 (not strictly needed)
    elsif bGTK3 then
        csr = c_func(x_gdk_cursor_new_from_name,{display,name})
    else
        ?9/0
    end if
    return csr
end function

global constant -- GdkGLConfigMode enum:
--              GDK_GL_MODE_RGB       = 0,
                GDK_GL_MODE_RGBA      = 0,       /* same as RGB */
--              GDK_GL_MODE_INDEX     = 1 << 0,
--              GDK_GL_MODE_SINGLE    = 0,
                GDK_GL_MODE_DOUBLE    = 1 << 1,
--              GDK_GL_MODE_STEREO    = 1 << 2,
--              GDK_GL_MODE_ALPHA     = 1 << 3,
                GDK_GL_MODE_DEPTH     = 1 << 4
--              GDK_GL_MODE_STENCIL   = 1 << 5,
--              GDK_GL_MODE_ACCUM     = 1 << 6,
--              GDK_GL_MODE_MULTISAMPLE = 1 << 7     /* not supported yet */

integer x_gdk_gl_config_new_by_mode = 0

global function gdk_gl_config_new_by_mode(integer mode)
    -- GTK2 only...
    if not bGTK2 then ?9/0 end if
    if x_gdk_gl_config_new_by_mode=0 then
        x_gdk_gl_config_new_by_mode = define_c_func(LIBGLX,"gdk_gl_config_new_by_mode",
            {C_INT},    --  GdkGLConfigMode mode
            C_PTR)      -- GdkGLConfig* glconfig
    end if
    atom glconfig = c_func(x_gdk_gl_config_new_by_mode,{mode})
    if glconfig=0 then crash("") end if
    return glconfig
end function

global procedure gdk_keyboard_grab(atom window, bool owner_events, integer time_)
    integer res = c_func(x_gdk_keyboard_grab,{window, owner_events, time_})
    assert(res=GDK_GRAB_SUCCESS)
end procedure

global procedure gdk_keyboard_ungrab(integer time_)
    c_proc(x_gdk_keyboard_ungrab,{time_})
end procedure

global function gdk_pixbuf_copy(atom pixbuf)
    return c_func(x_gdk_pixbuf_copy,{pixbuf})
end function

global function gdk_pixbuf_get_height(atom pixbuf)
    integer height = c_func(x_gdk_pixbuf_get_height,{pixbuf})
    return height
end function

global function gdk_pixbuf_get_n_channels(atom pixbuf)
    integer n_channels = c_func(x_gdk_pixbuf_get_n_channels,{pixbuf})
    return n_channels
end function

global function gdk_pixbuf_get_pixels(atom pixbuf)
    atom pixels = c_func(x_gdk_pixbuf_get_pixels,{pixbuf})
    return pixels
end function

global function gdk_pixbuf_get_rowstride(atom pixbuf)
    integer rowstride = c_func(x_gdk_pixbuf_get_rowstride,{pixbuf})
    return rowstride
end function

global function gdk_pixbuf_get_width(atom pixbuf)
    integer width = c_func(x_gdk_pixbuf_get_width,{pixbuf})
    return width
end function

global function gdk_pixbuf_new_from_file(string filename, atom ppError)
    atom pixbuf = c_func(x_gdk_pixbuf_new_from_file,{filename,ppError})
    return pixbuf
end function

--  gdk_pixbuf_save(pixbuf, tmp, "png", NULL)
global procedure gdk_pixbuf_save(atom pixbuf, string filename, filetype, atom pError)
    integer res = c_func(x_gdk_pixbuf_savev,{pixbuf, filename, filetype, NULL, NULL, pError})
    assert(res)
end procedure

global function gdk_pixbuf_save_to_buffer(atom pixbuf, pBuffer, integer buffer_size, string typ, atom pError)
    bool res = c_func(x_gdk_pixbuf_save_to_buffer,{pixbuf,pBuffer,buffer_size,typ,pError,NULL})
    return res
end function

global procedure gdk_pointer_grab(atom window, bool owner_events, integer mask, atom confine_to, cursor_, integer time_)
    integer res = c_func(x_gdk_pointer_grab,{window, owner_events, mask, confine_to, cursor_, time_})
    assert(res=GDK_GRAB_SUCCESS)
end procedure

global procedure gdk_pointer_ungrab(integer time_)
    c_proc(x_gdk_pointer_ungrab,{time_})
end procedure

global function gdk_screen_get_monitor_at_window(atom screen, window)
    integer monitor = c_func(x_gdk_screen_get_monitor_at_window,{screen,window})
    return monitor
end function

global procedure gdk_screen_get_monitor_geometry(atom screen, monitor_num, pRECT)
    c_proc(x_gdk_screen_get_monitor_geometry,{screen, monitor_num, pRECT})
end procedure

--gdk_x11_window_get_xid(window)??
global function gdk_win32_drawable_get_handle(atom drawable)
    atom res = iff(bGTK2?c_func(x_gdk_win32_drawable_get_handle,{drawable})
                        :c_func(x_gdk_win32_window_get_handle,{drawable}))
    return res
end function
global constant gdk_win32_window_get_handle = gdk_win32_drawable_get_handle;
--/*
--GTK2:
C:\GTK\include\gtk-2.0\gdk\gdkwin32.h:46 #define GDK_WINDOW_HWND(win)          (GDK_DRAWABLE_IMPL_WIN32(((GdkWindowObject*)win)->impl)->handle)
C:\GTK\include\gtk-2.0\gdk\gdkwin32.h:49 #define GDK_DRAWABLE_HANDLE(win)      (GDK_IS_WINDOW (win) ? GDK_WINDOW_HWND (win) : (GDK_IS_PIXMAP (win) ? GDK_PIXMAP_HBITMAP (win)
C:\GTK\include\gtk-2.0\gdk\gdkwin32.h:52 #define GDK_WINDOW_HWND(d) (gdk_win32_drawable_get_handle (d))C:\GTK\include\ex.err:5 C\GTK\include\gtk-2.0\gdk\gdkwin32.h52 #define GDK_WINDOW_HWND(d) (gdk_win32_drawable_get_handle (d))
C:\GTK\include\gtk-2.0\gdk\gdkwin32.h:52 #define GDK_WINDOW_HWND(d) (gdk_win32_drawable_get_handle (d))
C:\GTK\include\gtk-2.0\gdk\gdkwin32.h:86 HGDIOBJ       gdk_win32_drawable_get_handle (GdkDrawable* drawable);
--GTK3:
C:\gtkX\include\gtk-3.0\gdk\gdkwin32.h:54 #define GDK_WINDOW_HWND(win)          (GDK_WINDOW_IMPL_WIN32(win->impl)->handle)
C:\gtkX\include\gtk-3.0\gdk\gdkwin32.h:57 #define GDK_WINDOW_HWND(d) (gdk_win32_window_get_handle (d))
C:\gtkX\include\gtk-3.0\gdk\gdkwin32.h:57 #define GDK_WINDOW_HWND(d) (gdk_win32_window_get_handle (d))
C:\gtkX\include\gtk-3.0\gdk\gdkwin32.h:86 HGDIOBJ       gdk_win32_window_get_handle (GdkWindow* window);
--*/
global function gdk_win32_window_get_impl_hwnd(atom window)
    atom hWnd = c_func(x_gdk_win32_window_get_impl_hwnd,{window})
    return hWnd
end function

global function gdk_window_get_display(atom window)
    atom display = c_func(x_gdk_window_get_display,{window})
    return display
end function

global function gdk_window_get_state(atom window)
    atom state = c_func(x_gdk_window_get_state,{window})
    return state
end function

global procedure gdk_window_set_cursor(atom window, crsr)
    c_proc(x_gdk_window_set_cursor,{window,crsr})
end procedure

global function gtk_hbox_new(bool homogeneous=false, integer spacing=0)
    atom widget = c_func(x_gtk_hbox_new,{homogeneous, spacing})
    return widget
end function

global function gtk_vbox_new(bool homogeneous=false, integer spacing=0)
    atom widget = c_func(x_gtk_vbox_new,{homogeneous, spacing})
    return widget
end function

global function gtk_box_new(integer orientation=GTK_ORIENTATION_HORIZONTAL, bool homogeneous=false, integer spacing=0)
    atom widget
    if bGTK2 then
        if orientation=GTK_ORIENTATION_HORIZONTAL then
            widget = gtk_hbox_new(homogeneous, spacing)
        elsif orientation=GTK_ORIENTATION_VERTICAL then
            widget = gtk_vbox_new(homogeneous, spacing)
        else
            ?9/0
        end if
    elsif bGTK3 then
        widget = c_func(x_gtk_box_new,{orientation, homogeneous, spacing})
    else
        ?9/0
    end if
    return widget
end function

global procedure gtk_box_pack_start(atom box, child, bool expand, fill, integer padding)
    c_proc(x_gtk_box_pack_start,{box,child,expand,fill,padding})
end procedure

global function gtk_button_new_with_label(string label)
    atom button = c_func(x_gtk_button_new_with_label,{label})
    return button
end function

global procedure gtk_clipboard_clear(atom clipboard)
    c_proc(x_gtk_clipboard_clear,{clipboard})
end procedure

global function gtk_clipboard_get(atom selection)
    atom clipboard = c_func(x_gtk_clipboard_get,{selection})
    return clipboard
end function

--global procedure gtk_clipboard_set_can_store(atom clipboard, targets, integer n_targets)
--(simply unnecessary and unhelpful?)
--  c_proc(x_gtk_clipboard_set_can_store,{clipboard, targets, n_targets})
--end procedure

--global procedure gtk_clipboard_store(atom clipboard)
-- as next
--  c_proc(x_gtk_clipboard_store,{clipboard})
--end procedure

--global procedure gtk_clipboard_set_image(atom clipboard, pixbuf)
---- flurry of transmute warnings on GTK3, does //not// prevent clipboard clear on shutdown in GTK2 either.
--  c_proc(x_gtk_clipboard_set_image,{clipboard, pixbuf})
--end procedure

global procedure gtk_clipboard_set_with_data(atom clipboard, targets, n_targets, get_func, clear_func,user_data)
    bool res = c_func(x_gtk_clipboard_set_with_data,{clipboard,targets,n_targets,get_func,clear_func,user_data})
    assert(res)
end procedure

global function gtk_clipboard_wait_for_image(atom clipboard)
    atom pixbuf = c_func(x_gtk_clipboard_wait_for_image,{clipboard})
    return pixbuf
end function

global procedure gtk_container_add(atom container, widget)
    c_proc(x_gtk_container_add,{container,widget})
end procedure

global function gtk_dialog_run(atom dialog)
    return c_func(x_gtk_dialog_run,{dialog})
end function

global procedure gtk_drag_dest_set(atom widget, flags, targets, n_targets, actions)
    c_proc(x_gtk_drag_dest_set,{widget, flags, targets, n_targets, actions})
end procedure

--global procedure gtk_drag_get_data(atom widget, context, target, time_)
--  c_proc(x_gtk_drag_get_data,{widget,context,target,time_})
--end procedure

--global procedure gtk_drag_source_set(atom widget, integer start_button_mask, atom targets, integer n_targets, actions)
--?"gtk_drag_source_set"
--  c_proc(x_gtk_drag_source_set,{widget,start_button_mask,targets,n_targets,actions})
--end procedure

global function gtk_drawing_area_new()
    atom widget = c_func(x_gtk_drawing_area_new,{})
    return widget
end function

global function gtk_file_chooser_dialog_new(nullable_string title, atom parent, action, 
                                            nullable_string first_btn, integer first_response_id,
                                            nullable_string second_btn, integer second_response_id)
--?{title,parent,action,first_btn,first_response_id,second_btn,second_response_id,NULL}
    return c_func(x_gtk_file_chooser_dialog_new,{title,parent,action,first_btn,first_response_id,second_btn,second_response_id,NULL,NULL})
--  return c_func(x_gtk_file_chooser_dialog_new,{title,parent,action,first_btn,first_response_id,NULL,second_response_id,NULL})
end function

global procedure gtk_file_chooser_add_filter(atom chooser, fltr)
    c_proc(x_gtk_file_chooser_add_filter,{chooser,fltr})
end procedure

global function gtk_file_chooser_get_filename(atom chooser)
    atom pRes = c_func(x_gtk_file_chooser_get_filename,{chooser})
    return peek_string(pRes)
end function

local function tg_gtk_gslist_to_strings(atom pList)
    -- Convert GSList* result from (eg) gtk_file_chooser_get_filenames() to a sequence of strings
    sequence files = {}
    atom node = pList
    integer mw = machine_word()

    while node!=NULL do
        -- GSList struct is [data, next]
        atom data = peekns(node),     -- gchar* filename
             next = peekns(node+mw)

        files = append(files, peek_string(data))

        -- Free the gchar* (the API docs say caller owns them)
        g_free(data)

        node = next
    end while

    -- Free the GSList nodes themselves
    g_slist_free(pList)

    return files
end function

global function gtk_file_chooser_get_filenames(atom chooser)
    atom pList = c_func(x_gtk_file_chooser_get_filenames,{chooser})
    sequence res = tg_gtk_gslist_to_strings(pList)
-- temp: gtk_file_open.exw only copes with a single string return:
if length(res) then ?res res = res[1] end if
    return res
end function

global procedure gtk_file_chooser_set_select_multiple(atom chooser, bool bMulti)
    c_proc(x_gtk_file_chooser_set_select_multiple,{chooser,bMulti})
end procedure

global function gtk_file_filter_new()
    return c_func(x_gtk_file_filter_new,{})
end function

global procedure gtk_file_filter_add_pattern(atom fltr, string pattern)
    c_proc(x_gtk_file_filter_add_pattern,{fltr,pattern})
end procedure

global procedure gtk_file_filter_set_name(atom fltr, string name)
    c_proc(x_gtk_file_filter_set_name,{fltr,name})
end procedure

global procedure gtk_fixed_put(atom fixed, widget, integer x, y)
    c_proc(x_gtk_fixed_put,{fixed,widget,x,y})
end procedure

global function gtk_gl_area_new()
    -- GTK3 only...
    if not bGTK3 then ?9/0 end if
    atom widget = c_func(x_gtk_gl_area_new,{})
    return widget
end function

global procedure gtk_gl_area_make_current(atom area)
    -- GTK3 only...
    if not bGTK3 then ?9/0 end if
    c_proc(x_gtk_gl_area_make_current,{area})
end procedure

global procedure gtk_gl_area_set_required_version(atom area, integer major, minor)
    -- GTK3 only...
    if not bGTK3 then ?9/0 end if
    c_proc(x_gtk_gl_area_set_required_version,{area,major,minor})
end procedure

global procedure gtk_gl_area_set_has_depth_buffer(atom area, bool has_depth_buffer)
    -- GTK3 only...
    if not bGTK3 then ?9/0 end if
    c_proc(x_gtk_gl_area_set_has_depth_buffer,{area,has_depth_buffer})
end procedure

global procedure gtk_gl_area_set_has_stencil_buffer(atom area, bool has_stencil_buffer)
    -- GTK3 only...
    if not bGTK3 then ?9/0 end if
    c_proc(x_gtk_gl_area_set_has_stencil_buffer,{area,has_stencil_buffer})
end procedure

--global procedure gtk_gl_area_swap_buffers(atom area)
--  -- GTK3 only...
--  if not bGTK3 then ?9/0 end if
--  c_proc(x_gtk_gl_area_swap_buffers,{area})
--end procedure

global procedure gtk_grab_add(atom widget)
    c_proc(x_gtk_grab_add,{widget})
end procedure

global procedure gtk_grab_remove(atom widget)
    c_proc(x_gtk_grab_remove,{widget})
end procedure

global procedure gtk_init()
    c_proc(x_gtk_init,{})
end procedure

global procedure gtk_label_set_text(atom label, string text)
    c_proc(x_gtk_label_set_text,{label,text})
end procedure

global procedure gtk_main()
    c_proc(x_gtk_main,{})
end procedure

global procedure gtk_main_quit()
    c_proc(x_gtk_main_quit)
end procedure

global function tg_gtk_quit(atom winmain, /*user_data*/) -- (GTK only)
    c_proc(x_gtk_main_quit)
    return 0 
end function 
global constant gtk_main_quit_cb = call_back({'+',tg_gtk_quit})

global function gtk_selection_data_get_data(atom data)
    atom res = c_func(x_gtk_selection_data_get_data,{data})
    return res
end function

global function gtk_selection_data_get_target(atom data)
    atom res = c_func(x_gtk_selection_data_get_target,{data})
    return res
end function

global procedure gtk_selection_data_set(atom selection_data, typ, fmt, atom_string data, integer len)
    c_proc(x_gtk_selection_data_set,{selection_data,typ,fmt,data,len})
end procedure

--global procedure gtk_selection_data_set_pixbuf(atom selection_data, pixbuf)
--  bool res = c_func(x_gtk_selection_data_set_pixbuf,{selection_data,pixbuf})
----    assert(res)
--  ?{"gtk_selection_data_set_pixbuf",res}
--end procedure


--global function gtk_target_entry_new(string target, integer flags, info)
--  -- aside: result assumed permanent constant, with no need to ever free
--  atom pTarget = c_func(x_gtk_target_entry_new,{target,flags,info})
--  return pTarget
--end function

global procedure gtk_target_list_add(atom list, target, integer flags, info)
    c_proc(x_gtk_target_list_add,{list,target,flags,info})
end procedure

global function gtk_target_list_new(atom targets, integer ntargets)
    atom target_list = c_func(x_gtk_target_list_new,{targets,ntargets})
    return target_list
end function

global function gtk_target_table_new_from_list(atom list, n_targets)
    atom table = c_func(x_gtk_target_table_new_from_list,{list,n_targets})
    return table
end function

--/*
--global function gtk_target_table(sequence targets)
--  -- aside: result assumed permanent constant, with no need to ever free
--  integer l = length(targets),
--        tel = get_struct_size(idGtkTargetEntry)
----    sequence ptrs = repeat(0,l)
--  atom pTargets = allocate(tel*l), pT = pTargets
--  for i,t in targets do
--      {string target, integer flags, integer info} = t
--      t = gtk_target_entry_new(target, flags, info)
--      mem_copy(pT,t,tel)
--      pT += tel
--  end for
--  return pTargets
--end function
--*/
--/*
--global function gtk_target_table(sequence targets)
--
--  integer l = length(targets),
--          tel = get_struct_size(idGtkTargetEntry)
--
--  atom pTargets = allocate(tel*l),
--       pT = pTargets
--
--  for i=1 to l do
--      {string target, integer flags, integer info} = targets[i]
--
--      atom pStr = allocate_string(target)
--
--      set_struct_field(idGtkTargetEntry, pT, "target", pStr)
--      set_struct_field(idGtkTargetEntry, pT, "flags",  flags)
--      set_struct_field(idGtkTargetEntry, pT, "info",   info)
--
--      pT += tel
--  end for
--
--  return pTargets
--end function
--*/

constant pN_Targets = allocate(machine_word())
global function gtk_target_table(sequence targets)
    atom target_list = gtk_target_list_new(NULL, 0)
    for t in targets do
        {string target, integer flags, integer info} = t
        gtk_target_list_add(target_list,gdk_atom_intern(target,FALSE),flags,info)
    end for
    atom table = gtk_target_table_new_from_list(target_list, pN_Targets)
    return table
end function

global procedure gtk_tooltip_set_text(atom tooltip, string text)
    c_proc(x_gtk_tooltip_set_text,{tooltip,text})
end procedure

global procedure gtk_tooltip_trigger_tooltip_query(atom display)
    c_proc(x_gtk_tooltip_trigger_tooltip_query,{display})
end procedure

global procedure gtk_widget_add_events(atom widget, integer events)
    c_proc(x_gtk_widget_add_events,{widget,events})
end procedure

global procedure gtk_widget_destroy(atom widget)
    c_proc(x_gtk_widget_destroy,{widget})
end procedure

global procedure gtk_widget_get_allocation(atom widget, pRECT)
    c_proc(x_gtk_widget_get_allocation,{widget, pRECT});
end procedure

global function gtk_widget_get_display(atom widget)
    return c_func(x_gtk_widget_get_display,{widget})
end function

global function gtk_widget_get_parent(atom widget)
    return c_func(x_gtk_widget_get_parent,{widget})
end function

global function gtk_widget_get_visible(atom widget)
    bool res = c_func(x_gtk_widget_get_visible,{widget})
    return res
end function

global function gtk_widget_get_window(atom widget)
    atom res = c_func(x_gtk_widget_get_window,{widget})
    return res
end function

global procedure gtk_widget_grab_focus(atom widget)
    bool bOK = c_func(x_gtk_widget_grab_focus,{widget})
    assert(bOK)
end procedure

global procedure gtk_widget_hide(atom window)
    c_proc(x_gtk_widget_hide,{window})
end procedure

global procedure gtk_widget_queue_resize(atom widget)
    c_proc(x_gtk_widget_queue_resize,{widget})
end procedure

global procedure gtk_widget_set_can_focus(atom widget, bool can_focus)
    c_proc(x_gtk_widget_set_can_focus,{widget,can_focus})
end procedure

global procedure gtk_widget_set_has_tooltip(atom widget, bool has_tooltip)
    c_proc(x_gtk_widget_set_has_tooltip,{widget,has_tooltip})
end procedure

global procedure gtk_widget_set_has_window(atom widget, bool has_window)
    c_proc(x_gtk_widget_set_has_window,{widget,has_window})
end procedure

--global procedure gtk_widget_set_size_request(atom window, atom width, height, bool bWarn=true)
--  if bWarn then -- (came out wrong size in gtk_menu.exw, so I added bWarn to shut it up)
--      -- (underlying issues/motivation for this seem to have been addressed in hGUI.e)
--      ?"stop using gtk_widget_set_size_request, use gtk_widget_size_allocate instead"
--  end if
global procedure gtk_widget_set_size_request(atom window, atom width, height)
    c_proc(x_gtk_widget_set_size_request,{window,width,height})
end procedure

global procedure gtk_widget_queue_draw(atom widget)
    c_proc(x_gtk_widget_queue_draw,{widget})
end procedure

global procedure gtk_widget_show(atom handle)
    c_proc(x_gtk_widget_show,{handle})
end procedure

global procedure gtk_widget_show_all(atom window)
    c_proc(x_gtk_widget_show_all,{window})
end procedure

global procedure gtk_widget_size_allocate(atom widget, pRECT)
    c_proc(x_gtk_widget_size_allocate,{widget,pRECT})
end procedure

global procedure gtk_window_deiconify(atom window)
    c_proc(x_gtk_window_deiconify,{window})
end procedure

global function gtk_window_get_position(atom handle)
    c_proc(x_gtk_window_get_position,{handle,pX,pY})
    integer x = get_struct_field(idGdkRectangle,pRECT,"x"),
            y = get_struct_field(idGdkRectangle,pRECT,"y")
    return {x,y}
end function

global function gtk_window_get_screen(atom window)
    atom screen = c_func(x_gtk_window_get_screen,{window})
    return screen
end function

global function gtk_window_get_size(atom handle)
    c_proc(x_gtk_window_get_size,{handle,pW,pH})
    integer w = get_struct_field(idGdkRectangle,pRECT,"width"),
            h = get_struct_field(idGdkRectangle,pRECT,"height")
    return {w,h}
end function

global procedure gtk_window_move(atom window, integer x, y)
    c_proc(x_gtk_window_move,{window,x,y})
end procedure

global function gtk_window_new(integer window_type=GTK_WINDOW_TOPLEVEL)
    atom widget = c_func(x_gtk_window_new,{window_type})
    return widget
end function

global procedure gtk_window_present(atom window)
    c_proc(x_gtk_window_present,{window})
end procedure

global procedure gtk_window_resize(atom window, integer width, height)
    c_proc(x_gtk_window_resize,{window,width,height})
end procedure

global procedure gtk_window_set_decorated(atom window, bool setting)
    c_proc(x_gtk_window_set_decorated,{window,setting})
end procedure

global procedure gtk_window_set_default_size(atom window, integer w, h)
    c_proc(x_gtk_window_set_default_size,{window,w,h})
end procedure

global procedure gtk_window_set_modal(atom window, bool modal)
    c_proc(x_gtk_window_set_modal,{window,modal})
end procedure

global procedure gtk_window_set_resizable(atom window, bool resizable)
    c_proc(x_gtk_window_set_resizable,{window,resizable})
end procedure

global procedure gtk_window_set_skip_pager_hint(atom window, bool setting)
    c_proc(x_gtk_window_set_skip_pager_hint,{window,setting})
end procedure

global procedure gtk_window_set_skip_taskbar_hint(atom window, bool setting)
    c_proc(x_gtk_window_set_skip_taskbar_hint,{window,setting})
end procedure

global procedure gtk_window_set_title(atom window, string title)
    c_proc(x_gtk_window_set_title,{window,title})
end procedure

global procedure gtk_window_set_transient_for(atom window, parent)
    c_proc(x_gtk_window_set_transient_for,{window,parent})
end procedure

global procedure gtk_window_set_type_hint(atom window, hint)
    c_proc(x_gtk_window_set_type_hint,{window,hint})
end procedure

global function pango_cairo_create_layout(atom cairo)
    atom layout = c_func(x_pango_cairo_create_layout,{cairo})
    return layout
end function

global function pango_cairo_font_map_get_default()
    atom fontmap = c_func(x_pango_cairo_font_map_get_default,{})
    return fontmap
end function

global procedure pango_cairo_show_layout(atom cairo, layout)
    c_proc(x_pango_cairo_show_layout,{cairo,layout})
end procedure

global function pango_font_description_from_string(string font)
    atom desc = c_func(x_pango_font_description_from_string,{font})
    return desc
end function

global procedure pango_font_description_free(atom desc)
    c_proc(x_pango_font_description_free,{desc})
end procedure

global function pango_font_map_create_context(atom fontmap)
    atom context = c_func(x_pango_font_map_create_context,{fontmap})
    return context
end function

global function pango_layout_get_pixel_size(atom layout)
    c_proc(x_pango_layout_get_pixel_size,{layout,pW,pH})
    integer w = get_struct_field(idGdkRectangle,pRECT,"width"),
            h = get_struct_field(idGdkRectangle,pRECT,"height")
    return {w,h}
end function

global function pango_layout_new(atom context)
    atom layout = c_func(x_pango_layout_new,{context})
    return layout
end function

global procedure pango_layout_set_font_description(atom handle,fontdesc)
    c_proc(x_pango_layout_set_font_description,{handle,fontdesc})
end procedure

global procedure pango_layout_set_text(atom layout, string text, integer l=length(text))
    c_proc(x_pango_layout_set_text,{layout,text,l})
end procedure

global function GTK_GL_AREA(atom area)
    -- dummy casting function
    return area
end function

global function GTK_CONTAINER(atom widget)
    -- dummy casting function
    return widget
end function

--</theGUI.GTK.e>
--/*
GTK has its own philosophy on setting the size of windows and widgets. 
You should set geometry hints before you set the default size.

gtk_window_set_default_size(pw,800,600); 
These hints could be:

GdkGeometry hints;
hints.min_width = 800;
hints.max_width = 800;
hints.min_height = 600;
hints.max_height = 600;
gtk_window_set_geometry_hints( pw, GTK_WIDGET(pw), &hints, (GdkWindowHints)(GDK_HINT_MIN_SIZE | GDK_HINT_MAX_SIZE));
gtk_window_set_default_size (pw, 800, 600);

-- made slightly simpler than the raw API:
global constant escape_key_cb = call_back({'+',tg_gtk_check_escape})
global procedure cairo_set_dash(atom cairo, sequence dashes)
global function gdk_cursor_new_for_display(atom display, integer cursor_type)
global function gdk_cursor_new_from_name(atom display, string name)
global function gtk_box_new(integer orientation=GTK_ORIENTATION_HORIZONTAL, bool homogeneous=false, integer spacing=0)
global function gtk_file_chooser_get_filenames(atom chooser)
global constant gtk_main_quit_cb = call_back({'+',tg_gtk_quit})
global function gtk_target_table(sequence targets)
global function pango_layout_get_pixel_size(atom layout)
--*/

