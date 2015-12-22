#ifdef WIN32            
#include <windows.h>    /* necessary because of the Microsoft OpenGL headers dependency */
#endif
#include <GL/gl.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <iup.h>
#include <iup_config.h>
#include <iupgl.h>
#include <im.h>
#include <im_image.h>
#include <im_convert.h>
#include <iupim.h>


/********************************** Utilities *****************************************/


const char* str_filetitle(const char *filename)
{
  /* Start at the last character */
  int len = (int)strlen(filename);
  int offset = len - 1;
  while (offset != 0)
  {
    if (filename[offset] == '\\' || filename[offset] == '/')
    {
      offset++;
      break;
    }
    offset--;
  }
  return filename + offset;
}

const char* str_fileext(const char *filename)
{
  /* Start at the last character */
  int len = (int)strlen(filename);
  int offset = len - 1;
  while (offset != 0)
  {
    if (filename[offset] == '\\' || filename[offset] == '/')
      break;

    if (filename[offset] == '.')
    {
      offset++;
      return filename + offset;
    }
    offset--;
  }
  return NULL;
}

int str_compare(const char *l, const char *r, int casesensitive)
{
  if (!l || !r)
    return 0;

  while (*l && *r)
  {
    int diff;
    char l_char = *l,
      r_char = *r;

    /* compute the difference of both characters */
    if (casesensitive)
      diff = l_char - r_char;
    else
      diff = tolower((int)l_char) - tolower((int)r_char);

    /* if they differ we have a result */
    if (diff != 0)
      return 0;

    /* otherwise process the next characters */
    ++l;
    ++r;
  }

  /* check also for terminator */
  if (*l == *r)
    return 1;

  if (*r == 0)
    return 1;  /* if second string is at terminator, then it is partially equal */

  return 0;
}

void show_error(const char* message, int is_error)
{
  Ihandle* dlg = IupMessageDlg();
  IupSetStrAttribute(dlg, "PARENTDIALOG", IupGetGlobal("PARENTDIALOG"));
  IupSetAttribute(dlg, "DIALOGTYPE", is_error ? "ERROR" : "WARNING");
  IupSetAttribute(dlg, "BUTTONS", "OK");
  IupSetStrAttribute(dlg, "TITLE", is_error ? "Error" : "Warning");
  IupSetStrAttribute(dlg, "VALUE", message);
  IupPopup(dlg, IUP_CENTERPARENT, IUP_CENTERPARENT);
  IupDestroy(dlg);
}

void show_file_error(int error)
{
  switch (error)
  {
  case IM_ERR_OPEN:
    show_error("Error Opening File.",  1);
    break;
  case IM_ERR_MEM:
    show_error("Insufficient memory.",  1);
    break;
  case IM_ERR_ACCESS:
    show_error("Error Accessing File.",  1);
    break;
  case IM_ERR_DATA:
    show_error("Image type not Supported.",  1);
    break;
  case IM_ERR_FORMAT:
    show_error("Invalid Format.",  1);
    break;
  case IM_ERR_COMPRESS:
    show_error("Invalid or unsupported compression.",  1);
    break;
  default:
    show_error("Unknown Error.",  1);
  }
}

imImage* read_file(const char* filename)
{
  int error;
  imImage* image = imFileImageLoadBitmap(filename, 0, &error);
  if (error) 
    show_file_error(error);
  else
  {
    /* we are going to support only RGB images with no alpha */
    imImageRemoveAlpha(image);
    if (image->color_space != IM_RGB)
    {
      imImage* new_image = imImageCreateBased(image, -1, -1, IM_RGB, -1);
      imConvertColorSpace(image, new_image);
      imImageDestroy(image);

      image = new_image;
    }

    /* create OpenGL compatible data */
    imImageGetOpenGLData(image, NULL);
  }
  return image;
}

int write_file(const char* filename, const imImage* image)
{
  const char* format = imImageGetAttribString(image, "FileFormat");
  int error = imFileImageSave(filename, format, image);
  if (error)
  {
    show_file_error(error);
    return 0;
  }
  return 1;
}

void new_file(Ihandle* ih, imImage* image)
{
  Ihandle* dlg = IupGetDialog(ih);
  Ihandle* canvas = IupGetDialogChild(dlg, "CANVAS");
  imImage* old_image = (imImage*)IupGetAttribute(canvas, "IMAGE");

  IupSetAttribute(dlg, "TITLE", "Untitled - Simple Paint");
  IupSetAttribute(canvas, "FILENAME", NULL);
  IupSetAttribute(canvas, "DIRTY", "NO");

  IupSetAttribute(canvas, "IMAGE", (char*)image);

  /* create OpenGL compatible data */
  imImageGetOpenGLData(image, NULL);

  IupUpdate(canvas);

  if (old_image)
    imImageDestroy(old_image);
}

void check_new_file(Ihandle* dlg)
{
  Ihandle* canvas = IupGetDialogChild(dlg, "CANVAS");
  imImage* image = (imImage*)IupGetAttribute(canvas, "IMAGE");
  if (!image)
  {
    Ihandle* config = (Ihandle*)IupGetAttribute(canvas, "CONFIG");
    int width = IupConfigGetVariableIntDef(config, "NewImage", "Width", 640);
    int height = IupConfigGetVariableIntDef(config, "NewImage", "Height", 480);

    image = imImageCreate(width, height, IM_RGB, IM_BYTE);

    new_file(dlg, image);
  }
}

void open_file(Ihandle* ih, const char* filename)
{
  imImage* image = read_file(filename);
  if (image)
  {
    Ihandle* dlg = IupGetDialog(ih);
    Ihandle* canvas = IupGetDialogChild(dlg, "CANVAS");
    Ihandle* config = (Ihandle*)IupGetAttribute(canvas, "CONFIG");
    imImage* old_image = (imImage*)IupGetAttribute(canvas, "IMAGE");

    IupSetfAttribute(dlg, "TITLE", "%s - Simple Paint", str_filetitle(filename));
    IupSetStrAttribute(canvas, "FILENAME", filename);
    IupSetAttribute(canvas, "DIRTY", "NO");
    IupSetAttribute(canvas, "IMAGE", (char*)image);

    IupUpdate(canvas);

    if (old_image)
      imImageDestroy(old_image);

    IupConfigRecentUpdate(config, filename);
  }
}

void save_file(Ihandle* canvas)
{
  char* filename = IupGetAttribute(canvas, "FILENAME");
  imImage* image = (imImage*)IupGetAttribute(canvas, "IMAGE");
  if (write_file(filename, image))
    IupSetAttribute(canvas, "DIRTY", "NO");
}

void set_file_format(imImage* image, const char* filename)
{
  const char* ext = str_fileext(filename);
  const char* format = "JPEG";
  if (str_compare(ext, "jpg", 0) || str_compare(ext, "jpeg", 0))
    format = "JPEG";
  else if (str_compare(ext, "bmp", 0))
    format = "BMP";
  else if (str_compare(ext, "png", 0))
    format = "PNG";
  else if (str_compare(ext, "tga", 0))
    format = "TGA";
  else if (str_compare(ext, "tif", 0) || str_compare(ext, "tiff", 0))
    format = "TIFF";
  imImageSetAttribString(image, "FileFormat", format);
}

void saveas_file(Ihandle* canvas, const char* filename)
{
  imImage* image = (imImage*)IupGetAttribute(canvas, "IMAGE");

  set_file_format(image, filename);

  if (write_file(filename, image))
  {
    Ihandle* config = (Ihandle*)IupGetAttribute(canvas, "CONFIG");
  
    IupSetfAttribute(IupGetDialog(canvas), "TITLE", "%s - Simple Paint", str_filetitle(filename));
    IupSetStrAttribute(canvas, "FILENAME", filename);
    IupSetAttribute(canvas, "DIRTY", "NO");

    IupConfigRecentUpdate(config, filename);
  }
}

int save_check(Ihandle* ih)
{
  Ihandle* canvas = IupGetDialogChild(ih, "CANVAS");
  if (IupGetInt(canvas, "DIRTY"))
  {
    switch (IupAlarm("Warning", "File not saved! Save it now?", "Yes", "No", "Cancel"))
    {
    case 1:  /* save the changes and continue */
      save_file(canvas);
      break;
    case 2:  /* ignore the changes and continue */
      break;
    case 3:  /* cancel */
      return 0;  
    }
  }
  return 1;
}

void toggle_bar_visibility(Ihandle* item, Ihandle* ih)
{
  if (IupGetInt(item, "VALUE"))
  {
    IupSetAttribute(ih, "FLOATING", "YES");
    IupSetAttribute(ih, "VISIBLE", "NO");
    IupSetAttribute(item, "VALUE", "OFF");
  }
  else
  {
    IupSetAttribute(ih, "FLOATING", "NO");
    IupSetAttribute(ih, "VISIBLE", "YES");
    IupSetAttribute(item, "VALUE", "ON");
  }

  IupRefresh(ih);  /* refresh the dialog layout */
}


/********************************** Callbacks *****************************************/


int canvas_action_cb(Ihandle* canvas)
{
  int x, y, canvas_width, canvas_height;
  void* gldata;
  unsigned int ri, gi, bi;
  imImage* image;
  Ihandle* config = (Ihandle*)IupGetAttribute(canvas, "CONFIG");
  const char* background = IupConfigGetVariableStrDef(config, "Canvas", "Background", "255 255 255");

  IupGetIntInt(canvas, "DRAWSIZE", &canvas_width, &canvas_height);

  IupGLMakeCurrent(canvas);

  /* OpenGL configuration */
  glPixelStorei(GL_UNPACK_ALIGNMENT, 1);           /* image data alignment is 1 */

  glViewport(0, 0, canvas_width, canvas_height);

  glMatrixMode(GL_PROJECTION);
  glLoadIdentity();
  glOrtho(0, canvas_width, 0, canvas_height, -1, 1);

  glMatrixMode(GL_MODELVIEW);
  glLoadIdentity();

  /* draw the background */
  sscanf(background, "%u %u %u", &ri, &gi, &bi);
  glClearColor(ri / 255.f, gi / 255.f, bi / 255.f, 1);
  glClear(GL_COLOR_BUFFER_BIT);

  /* draw the image at the center of the canvas */
  image = (imImage*)IupGetAttribute(canvas, "IMAGE");
  if (image)
  {
    x = (canvas_width - image->width) / 2;
    y = (canvas_height - image->height) / 2;
    gldata = (void*)imImageGetAttribute(image, "GLDATA", NULL, NULL);
    glRasterPos2i(x, y);  /* this will not work for negative values, OpenGL limitation */
    glDrawPixels(image->width, image->height, GL_RGB, GL_UNSIGNED_BYTE, gldata);  /* no zoom support, must use texture */
  }

  IupGLSwapBuffers(canvas);
  return IUP_DEFAULT;
}

int dropfiles_cb(Ihandle* ih, const char* filename)
{
  if (save_check(ih))
    open_file(ih, filename);

  return IUP_DEFAULT;
}

int file_menu_open_cb(Ihandle* ih)
{
  Ihandle* item_revert = IupGetDialogChild(ih, "ITEM_REVERT");
  Ihandle* item_save = IupGetDialogChild(ih, "ITEM_SAVE");
  Ihandle* canvas = IupGetDialogChild(ih, "CANVAS");
  char* filename = IupGetAttribute(canvas, "FILENAME");
  int dirty = IupGetInt(canvas, "DIRTY");

  if (dirty)
    IupSetAttribute(item_save, "ACTIVE", "YES");
  else
    IupSetAttribute(item_save, "ACTIVE", "NO");

  if (dirty && filename)
    IupSetAttribute(item_revert, "ACTIVE", "YES");
  else
    IupSetAttribute(item_revert, "ACTIVE", "NO");
  return IUP_DEFAULT;
}

int edit_menu_open_cb(Ihandle* ih)
{
  Ihandle *clipboard = IupClipboard(); 

  Ihandle *item_paste = IupGetDialogChild(ih, "ITEM_PASTE");

  if (!IupGetInt(clipboard, "IMAGEAVAILABLE"))
    IupSetAttribute(item_paste, "ACTIVE", "NO");
  else
    IupSetAttribute(item_paste, "ACTIVE", "YES");

  IupDestroy(clipboard);
  return IUP_DEFAULT;
}

int config_recent_cb(Ihandle* ih)
{
  if (save_check(ih))
  {
    char* filename = IupGetAttribute(ih, "TITLE");
    open_file(ih, filename);
  }
  return IUP_DEFAULT;
}

int item_new_action_cb(Ihandle* item_new)
{
  if (save_check(item_new))
  {
    Ihandle* canvas = IupGetDialogChild(item_new, "CANVAS");
    Ihandle* config = (Ihandle*)IupGetAttribute(canvas, "CONFIG");
    int width = IupConfigGetVariableIntDef(config, "NewImage", "Width", 640);
    int height = IupConfigGetVariableIntDef(config, "NewImage", "Height", 480);

    if (IupGetParam("New Image", NULL, NULL, "Width: %i[1,]\nHeight: %i[1,]\n", &width, &height, NULL))
    {
      imImage* image = imImageCreate(width, height, IM_RGB, IM_BYTE);

      IupConfigSetVariableInt(config, "NewImage", "Width", width);
      IupConfigSetVariableInt(config, "NewImage", "Height", height);

      new_file(item_new, image);
    }
  }

  return IUP_DEFAULT;
}

int select_file(Ihandle* parent_dlg, int is_open)
{
  Ihandle* config = (Ihandle*)IupGetAttribute(parent_dlg, "CONFIG");
  Ihandle* canvas = IupGetDialogChild(parent_dlg, "CANVAS");
  const char* dir = IupConfigGetVariableStr(config, "MainWindow", "LastDirectory");

  Ihandle* filedlg = IupFileDlg();
  if (is_open)
    IupSetAttribute(filedlg, "DIALOGTYPE", "OPEN");
  else
  {
    IupSetAttribute(filedlg, "DIALOGTYPE", "SAVE");
    IupSetStrAttribute(filedlg, "FILE", IupGetAttribute(canvas, "FILENAME"));
  }
  IupSetAttribute(filedlg, "EXTFILTER", "Image Files|*.bmp;*.jpg;*.png;*.tif;*.tga|All Files|*.*|");
  IupSetStrAttribute(filedlg, "DIRECTORY", dir);
  IupSetAttributeHandle(filedlg, "PARENTDIALOG", parent_dlg);

  IupPopup(filedlg, IUP_CENTERPARENT, IUP_CENTERPARENT);
  if (IupGetInt(filedlg, "STATUS") != -1)
  {
    char* filename = IupGetAttribute(filedlg, "VALUE");
    if (is_open)
      open_file(parent_dlg, filename);
    else
      saveas_file(canvas, filename);

    dir = IupGetAttribute(filedlg, "DIRECTORY");
    IupConfigSetVariableStr(config, "MainWindow", "LastDirectory", dir);
  }

  IupDestroy(filedlg);
  return IUP_DEFAULT;
}

int item_open_action_cb(Ihandle* item_open)
{
  if (!save_check(item_open))
    return IUP_DEFAULT;

  return select_file(IupGetDialog(item_open), 1);
}

int item_saveas_action_cb(Ihandle* item_saveas)
{
  return select_file(IupGetDialog(item_saveas), 0);
}

int item_save_action_cb(Ihandle* item_save)
{
  Ihandle* canvas = IupGetDialogChild(item_save, "CANVAS");
  char* filename = IupGetAttribute(canvas, "FILENAME");
  if (!filename)
    item_saveas_action_cb(item_save);
  else   
  {
    /* test again because in can be called using the hot key */
    int dirty = IupGetInt(canvas, "DIRTY");
    if (dirty)
      save_file(canvas);
  }
  return IUP_DEFAULT;
}

int item_revert_action_cb(Ihandle* item_revert)
{
  Ihandle* canvas = IupGetDialogChild(item_revert, "CANVAS");
  char* filename = IupGetAttribute(canvas, "FILENAME");
  open_file(item_revert, filename);
  return IUP_DEFAULT;
}

int item_exit_action_cb(Ihandle* item_exit)
{
  Ihandle* dlg = IupGetDialog(item_exit);
  Ihandle* config = (Ihandle*)IupGetAttribute(dlg, "CONFIG");
  Ihandle* canvas = IupGetDialogChild(dlg, "CANVAS");
  imImage* image = (imImage*)IupGetAttribute(canvas, "IMAGE");

  if (!save_check(item_exit))
    return IUP_IGNORE;  /* to abort the CLOSE_CB callback */

  if (image)
    imImageDestroy(image);

  IupConfigDialogClosed(config, dlg, "MainWindow");
  IupConfigSave(config);
  IupDestroy(config);
  return IUP_CLOSE;
}

int item_copy_action_cb(Ihandle* item_copy) 
{
  Ihandle* canvas = IupGetDialogChild(item_copy, "CANVAS");
  imImage* image = (imImage*)IupGetAttribute(canvas, "IMAGE");
  Ihandle *clipboard = IupClipboard();
  IupSetAttribute(clipboard, "NATIVEIMAGE", (char*)IupGetImageNativeHandle(image));
  IupDestroy(clipboard);
  return IUP_DEFAULT;
}

int item_paste_action_cb(Ihandle* item_paste) 
{
  if (save_check(item_paste))
  {
    Ihandle* canvas = IupGetDialogChild(item_paste, "CANVAS");
    imImage* old_image = (imImage*)IupGetAttribute(canvas, "IMAGE");

    Ihandle *clipboard = IupClipboard();
    imImage* image = IupGetNativeHandleImage(IupGetAttribute(clipboard, "NATIVEIMAGE"));
    IupDestroy(clipboard);

    if (!image)
    {
      show_error("Invalid Clipboard Data", 1);
      return IUP_DEFAULT;
    }

    /* we are going to support only RGB images with no alpha */
    imImageRemoveAlpha(image);
    if (image->color_space != IM_RGB)
    {
      imImage* new_image = imImageCreateBased(image, -1, -1, IM_RGB, -1);
      imConvertColorSpace(image, new_image);
      imImageDestroy(image);

      image = new_image;
    }

    /* create OpenGL compatible data */
    imImageGetOpenGLData(image, NULL);

    imImageSetAttribString(image, "FileFormat", "JPEG");

    IupSetAttribute(canvas, "DIRTY", "Yes");
    IupSetAttribute(canvas, "IMAGE", (char*)image);
    IupSetAttribute(canvas, "FILENAME", NULL);
    IupSetAttribute(IupGetDialog(canvas), "TITLE", "Untitled - Simple Paint");

    IupUpdate(canvas);

    if (old_image)
      imImageDestroy(old_image);
  }
  return IUP_DEFAULT;
}

int item_background_action_cb(Ihandle* item_background)
{
  Ihandle* canvas = IupGetDialogChild(item_background, "CANVAS");
  Ihandle* config = (Ihandle*)IupGetAttribute(canvas, "CONFIG");
  Ihandle* colordlg = IupColorDlg();
  const char* background = IupConfigGetVariableStrDef(config, "Canvas", "Background", "255 255 255");
  IupSetStrAttribute(colordlg, "VALUE", background);
  IupSetAttributeHandle(colordlg, "PARENTDIALOG", IupGetDialog(item_background));

  IupPopup(colordlg, IUP_CENTERPARENT, IUP_CENTERPARENT);

  if (IupGetInt(colordlg, "STATUS") == 1)
  {
    background = IupGetAttribute(colordlg, "VALUE");
    IupConfigSetVariableStr(config, "Canvas", "Background", background);

    IupUpdate(canvas);
  }

  IupDestroy(colordlg);
  return IUP_DEFAULT;
}

int item_toolbar_action_cb(Ihandle* item_toolbar)
{
  Ihandle* canvas = IupGetDialogChild(item_toolbar, "CANVAS");
  Ihandle* toolbar = IupGetChild(IupGetParent(canvas), 0);
  Ihandle* config = (Ihandle*)IupGetAttribute(canvas, "CONFIG");

  toggle_bar_visibility(item_toolbar, toolbar);

  IupConfigSetVariableStr(config, "MainWindow", "Toolbar", IupGetAttribute(item_toolbar, "VALUE"));
  return IUP_DEFAULT;
}

int item_statusbar_action_cb(Ihandle* item_statusbar)
{
  Ihandle* canvas = IupGetDialogChild(item_statusbar, "CANVAS");
  Ihandle* statusbar = IupGetBrother(canvas);
  Ihandle* config = (Ihandle*)IupGetAttribute(canvas, "CONFIG");

  toggle_bar_visibility(item_statusbar, statusbar);

  IupConfigSetVariableStr(config, "MainWindow", "Statusbar", IupGetAttribute(item_statusbar, "VALUE"));
  return IUP_DEFAULT;
}

int item_help_action_cb(void)
{
  IupHelp("http://www.tecgraf.puc-rio.br/iup");
  return IUP_DEFAULT;
}

int item_about_action_cb(void)
{
  IupMessage("About", "   Simple Paint\n\nAutors:\n   Gustavo Lyrio\n   Antonio Scuri");
  return IUP_DEFAULT;
}


/********************************** Main *****************************************/


Ihandle* create_main_dialog(Ihandle *config)
{
  Ihandle *dlg, *vbox, *canvas, *menu;
  Ihandle *sub_menu_file, *file_menu, *item_exit, *item_new, *item_open, *item_save, *item_saveas, *item_revert;
  Ihandle *sub_menu_edit, *edit_menu, *item_copy, *item_paste;
  Ihandle *btn_copy, *btn_paste, *btn_new, *btn_open, *btn_save;
  Ihandle *sub_menu_help, *help_menu, *item_help, *item_about;
  Ihandle *sub_menu_view, *view_menu, *item_toolbar, *item_statusbar;
  Ihandle *statusbar, *toolbar, *recent_menu, *item_background;

  canvas = IupGLCanvas(NULL);
  IupSetAttribute(canvas, "NAME", "CANVAS");
  IupSetAttribute(canvas, "DIRTY", "NO");
  IupSetAttribute(canvas, "BUFFER", "DOUBLE");
  IupSetCallback(canvas, "ACTION", (Icallback)canvas_action_cb);
  IupSetCallback(canvas, "DROPFILES_CB", (Icallback)dropfiles_cb);

  statusbar = IupLabel("(0, 0) = [0   0   0]");
  IupSetAttribute(statusbar, "NAME", "STATUSBAR");
  IupSetAttribute(statusbar, "EXPAND", "HORIZONTAL");
  IupSetAttribute(statusbar, "PADDING", "10x5");

  item_new = IupItem("&New\tCtrl+N", NULL);
  IupSetAttribute(item_new, "IMAGE", "IUP_FileNew");
  IupSetCallback(item_new, "ACTION", (Icallback)item_new_action_cb);
  btn_new = IupButton(NULL, NULL);
  IupSetAttribute(btn_new, "IMAGE", "IUP_FileNew");
  IupSetAttribute(btn_new, "FLAT", "Yes");
  IupSetCallback(btn_new, "ACTION", (Icallback)item_new_action_cb);
  IupSetAttribute(btn_new, "TIP", "New (Ctrl+N)");
  IupSetAttribute(btn_new, "CANFOCUS", "No");

  item_open = IupItem("&Open...\tCtrl+O", NULL);
  IupSetAttribute(item_open, "IMAGE", "IUP_FileOpen");
  IupSetCallback(item_open, "ACTION", (Icallback)item_open_action_cb);
  btn_open = IupButton(NULL, NULL);
  IupSetAttribute(btn_open, "IMAGE", "IUP_FileOpen");
  IupSetAttribute(btn_open, "FLAT", "Yes");
  IupSetCallback(btn_open, "ACTION", (Icallback)item_open_action_cb);
  IupSetAttribute(btn_open, "TIP", "Open (Ctrl+O)");
  IupSetAttribute(btn_open, "CANFOCUS", "No");

  item_save = IupItem("&Save\tCtrl+S", NULL);
  IupSetAttribute(item_save, "NAME", "ITEM_SAVE");
  IupSetAttribute(item_save, "IMAGE", "IUP_FileSave");
  IupSetCallback(item_save, "ACTION", (Icallback)item_save_action_cb);
  btn_save = IupButton(NULL, NULL);
  IupSetAttribute(btn_save, "IMAGE", "IUP_FileSave");
  IupSetAttribute(btn_save, "FLAT", "Yes");
  IupSetCallback(btn_save, "ACTION", (Icallback)item_save_action_cb);
  IupSetAttribute(btn_save, "TIP", "Save (Ctrl+S)");
  IupSetAttribute(btn_save, "CANFOCUS", "No");

  item_saveas = IupItem("Save &As...", NULL);
  IupSetAttribute(item_saveas, "NAME", "ITEM_SAVEAS");
  IupSetCallback(item_saveas, "ACTION", (Icallback)item_saveas_action_cb);

  item_revert = IupItem("&Revert", NULL);
  IupSetAttribute(item_revert, "NAME", "ITEM_REVERT");
  IupSetCallback(item_revert, "ACTION", (Icallback)item_revert_action_cb);

  item_exit = IupItem("E&xit", NULL);
  IupSetCallback(item_exit, "ACTION", (Icallback)item_exit_action_cb);

  item_copy = IupItem("&Copy\tCtrl+C", NULL);
  IupSetAttribute(item_copy, "NAME", "ITEM_COPY");
  IupSetAttribute(item_copy, "IMAGE", "IUP_EditCopy");
  IupSetCallback(item_copy, "ACTION", (Icallback)item_copy_action_cb);
  btn_copy = IupButton(NULL, NULL);
  IupSetAttribute(btn_copy, "IMAGE", "IUP_EditCopy");
  IupSetAttribute(btn_copy, "FLAT", "Yes");
  IupSetCallback(btn_copy, "ACTION", (Icallback)item_copy_action_cb);
  IupSetAttribute(btn_copy, "TIP", "Copy (Ctrl+C)");
  IupSetAttribute(btn_copy, "CANFOCUS", "No");

  item_paste = IupItem("&Paste\tCtrl+V", NULL);
  IupSetAttribute(item_paste, "NAME", "ITEM_PASTE");
  IupSetAttribute(item_paste, "IMAGE", "IUP_EditPaste");
  IupSetCallback(item_paste, "ACTION", (Icallback)item_paste_action_cb);
  btn_paste = IupButton(NULL, NULL);
  IupSetAttribute(btn_paste, "IMAGE", "IUP_EditPaste");
  IupSetAttribute(btn_paste, "FLAT", "Yes");
  IupSetCallback(btn_paste, "ACTION", (Icallback)item_paste_action_cb);
  IupSetAttribute(btn_paste, "TIP", "Paste (Ctrl+V)");
  IupSetAttribute(btn_paste, "CANFOCUS", "No");

  item_background = IupItem("&Background...", NULL);
  IupSetCallback(item_background, "ACTION", (Icallback)item_background_action_cb);

  item_toolbar = IupItem("&Toobar", NULL);
  IupSetCallback(item_toolbar, "ACTION", (Icallback)item_toolbar_action_cb);
  IupSetAttribute(item_toolbar, "VALUE", "ON");

  item_statusbar = IupItem("&Statusbar", NULL);
  IupSetCallback(item_statusbar, "ACTION", (Icallback)item_statusbar_action_cb);
  IupSetAttribute(item_statusbar, "VALUE", "ON");

  item_help = IupItem("&Help...", NULL);
  IupSetCallback(item_help, "ACTION", (Icallback)item_help_action_cb);

  item_about = IupItem("&About...", NULL);
  IupSetCallback(item_about, "ACTION", (Icallback)item_about_action_cb);

  recent_menu = IupMenu(NULL);

  file_menu = IupMenu(
    item_new,
    item_open,
    item_save,
    item_saveas,
    item_revert,
    IupSeparator(),
    IupSubmenu("Recent &Files", recent_menu),
    item_exit,
    NULL);
  edit_menu = IupMenu(
    item_copy,
    item_paste,
    NULL);
  view_menu = IupMenu(
    item_background,
    IupSeparator(),
    item_toolbar,
    item_statusbar,
    NULL);
  help_menu = IupMenu(
    item_help,
    item_about,
    NULL);

  IupSetCallback(file_menu, "OPEN_CB", (Icallback)file_menu_open_cb);
  IupSetCallback(edit_menu, "OPEN_CB", (Icallback)edit_menu_open_cb);

  sub_menu_file = IupSubmenu("&File", file_menu);
  sub_menu_edit = IupSubmenu("&Edit", edit_menu);
  sub_menu_view = IupSubmenu("&View", view_menu);
  sub_menu_help = IupSubmenu("&Help", help_menu);

  menu = IupMenu(
    sub_menu_file,
    sub_menu_edit,
    sub_menu_view,
    sub_menu_help,
    NULL);

  toolbar = IupHbox(
    btn_new,
    btn_open,
    btn_save,
    IupSetAttributes(IupLabel(NULL), "SEPARATOR=VERTICAL"),
    btn_copy,
    btn_paste,
    NULL);
  IupSetAttribute(toolbar, "MARGIN", "5x5");
  IupSetAttribute(toolbar, "GAP", "2");

  vbox = IupVbox(
    toolbar,
    canvas,
    statusbar,
    NULL);

  dlg = IupDialog(vbox);
  IupSetAttributeHandle(dlg, "MENU", menu);
  IupSetAttribute(dlg, "SIZE", "HALFxHALF");
  IupSetCallback(dlg, "CLOSE_CB", (Icallback)item_exit_action_cb);
  IupSetCallback(dlg, "DROPFILES_CB", (Icallback)dropfiles_cb);

  IupSetCallback(dlg, "K_cN", (Icallback)item_new_action_cb);
  IupSetCallback(dlg, "K_cO", (Icallback)item_open_action_cb);
  IupSetCallback(dlg, "K_cS", (Icallback)item_save_action_cb);
  IupSetCallback(dlg, "K_cV", (Icallback)item_paste_action_cb);
  IupSetCallback(dlg, "K_cC", (Icallback)item_copy_action_cb);

  /* parent for pre-defined dialogs in closed functions (IupMessage and IupAlarm) */
  IupSetAttributeHandle(NULL, "PARENTDIALOG", dlg);

  /* Initialize variables from the configuration file */

  IupConfigRecentInit(config, recent_menu, config_recent_cb, 10);

  if (!IupConfigGetVariableIntDef(config, "MainWindow", "Toolbar", 1))
  {
    IupSetAttribute(item_toolbar, "VALUE", "OFF");

    IupSetAttribute(toolbar, "FLOATING", "YES");
    IupSetAttribute(toolbar, "VISIBLE", "NO");
  }

  if (!IupConfigGetVariableIntDef(config, "MainWindow", "Statusbar", 1))
  {
    IupSetAttribute(item_statusbar, "VALUE", "OFF");

    IupSetAttribute(statusbar, "FLOATING", "YES");
    IupSetAttribute(statusbar, "VISIBLE", "NO");
  }

  IupSetAttribute(dlg, "CONFIG", (char*)config);

  return dlg;
}

int main(int argc, char **argv)
{
  Ihandle *dlg;
  Ihandle *config;

  IupOpen(&argc, &argv);
  IupGLCanvasOpen();
  IupImageLibOpen();

  config = IupConfig();
  IupSetAttribute(config, "APP_NAME", "simple_paint");
  IupConfigLoad(config);

  dlg = create_main_dialog(config);

  /* show the dialog at the last position, with the last size */
  IupConfigDialogShow(config, dlg, "MainWindow");

  /* open a file from the command line (allow file association in Windows) */
  if (argc > 1 && argv[1])
  {
    const char* filename = argv[1];
    open_file(dlg, filename);
  }

  /* initialize the current file, if not already loaded */
  check_new_file(dlg);

  IupMainLoop();

  IupClose();
  return EXIT_SUCCESS;
}
