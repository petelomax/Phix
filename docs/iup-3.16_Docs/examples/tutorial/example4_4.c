#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <math.h>
#include <iup.h>
#include <iup_config.h>
#include <iupgl.h>
#include <cd.h>
#include <cdprint.h>
#include <cdiup.h>
#include <im.h>
#include <im_image.h>
#include <im_convert.h>
#include <iupim.h>


#if _MSC_VER < 1800 /* vc12 (2013) */
#define DEFINE_ROUND
#endif

#ifdef DEFINE_ROUND
double round(double x)
{
  return (int)(x>0 ? x + 0.5 : x - 0.5);
}
#endif


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

/* extracted from the SCROLLBAR attribute documentation */
void scroll_update(Ihandle* ih, int view_width, int view_height)
{
  /* view_width and view_height is the virtual space size */
  /* here we assume XMIN=0, XMAX=1, YMIN=0, YMAX=1 */
  int elem_width, elem_height;
  int canvas_width, canvas_height;
  int scrollbar_size = IupGetInt(NULL, "SCROLLBARSIZE");
  int border = IupGetInt(ih, "BORDER");

  IupGetIntInt(ih, "RASTERSIZE", &elem_width, &elem_height);

  /* if view is greater than canvas in one direction,
  then it has scrollbars,
  but this affects the opposite direction */
  elem_width -= 2 * border;  /* remove BORDER (always size=1) */
  elem_height -= 2 * border;
  canvas_width = elem_width;
  canvas_height = elem_height;
  if (view_width > elem_width)  /* check for horizontal scrollbar */
    canvas_height -= scrollbar_size;  /* affect vertical size */
  if (view_height > elem_height)
    canvas_width -= scrollbar_size;
  if (view_width <= elem_width && view_width > canvas_width)  /* check if still has horizontal scrollbar */
    canvas_height -= scrollbar_size;
  if (view_height <= elem_height && view_height > canvas_height)
    canvas_width -= scrollbar_size;
  if (canvas_width < 0) canvas_width = 0;
  if (canvas_height < 0) canvas_height = 0;

  IupSetFloat(ih, "DX", (float)canvas_width / (float)view_width);
  IupSetFloat(ih, "DY", (float)canvas_height / (float)view_height);
}

void scroll_calc_center(Ihandle* ih, float *x, float *y)
{
  *x = IupGetFloat(ih, "POSX") + IupGetFloat(ih, "DX") / 2.0f;
  *y = IupGetFloat(ih, "POSY") + IupGetFloat(ih, "DY") / 2.0f;
}

void scroll_center(Ihandle* ih, float old_center_x, float old_center_y)
{
  /* always update the scroll position
     keeping it proportional to the old position
     relative to the center of the ih. */

  float dx = IupGetFloat(ih, "DX");
  float dy = IupGetFloat(ih, "DY");

  float posx = old_center_x - dx / 2.0f;
  float posy = old_center_y - dy / 2.0f;

  if (posx < 0) posx = 0;
  if (posx > 1 - dx) posx = 1 - dx;

  if (posy < 0) posy = 0;
  if (posy > 1 - dy) posy = 1 - dy;

  IupSetFloat(ih, "POSX", posx);
  IupSetFloat(ih, "POSY", posy);
}

void zoom_update(Ihandle* ih, double zoom_index)
{
  Ihandle* zoom_lbl = IupGetDialogChild(ih, "ZOOMLABEL");
  Ihandle* canvas = IupGetDialogChild(ih, "CANVAS");
  imImage* image = (imImage*)IupGetAttribute(canvas, "IMAGE");
  double zoom_factor = pow(2, zoom_index);
  IupSetStrf(zoom_lbl, "TITLE", "%.0f%%", floor(zoom_factor * 100));
  if (image)
  {
    float old_center_x, old_center_y;
    int view_width = (int)(zoom_factor * image->width);
    int view_height = (int)(zoom_factor * image->height);

    scroll_calc_center(canvas, &old_center_x, &old_center_y);

    scroll_update(canvas, view_width, view_height);

    scroll_center(canvas, old_center_x, old_center_y);
  }
  IupUpdate(canvas);
}

void set_new_image(Ihandle* canvas, imImage* image, const char* filename, int dirty)
{
  imImage* old_image = (imImage*)IupGetAttribute(canvas, "IMAGE");
  Ihandle* size_lbl = IupGetDialogChild(canvas, "SIZELABEL");
  Ihandle* zoom_val = IupGetDialogChild(canvas, "ZOOMVAL");

  if (filename)
  {
    IupSetStrAttribute(canvas, "FILENAME", filename);
    IupSetfAttribute(IupGetDialog(canvas), "TITLE", "%s - Simple Paint", str_filetitle(filename));
  }
  else
  {
    IupSetAttribute(canvas, "FILENAME", NULL);
    IupSetAttribute(IupGetDialog(canvas), "TITLE", "Untitled - Simple Paint");
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

  /* default file format */
  const char* format = imImageGetAttribString(image, "FileFormat");
  if (!format)
    imImageSetAttribString(image, "FileFormat", "JPEG");
    
  IupSetAttribute(canvas, "DIRTY", dirty? "Yes": "No");
  IupSetAttribute(canvas, "IMAGE", (char*)image);

  IupSetfAttribute(size_lbl, "TITLE", "%d x %d px", image->width, image->height);

  if (old_image)
    imImageDestroy(old_image);

  IupSetDouble(zoom_val, "VALUE", 0);
  zoom_update(canvas, 0);
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

    set_new_image(canvas, image, NULL, 0);
  }
}

void open_file(Ihandle* ih, const char* filename)
{
  imImage* image = read_file(filename);
  if (image)
  {
    Ihandle* canvas = IupGetDialogChild(ih, "CANVAS");
    Ihandle* config = (Ihandle*)IupGetAttribute(canvas, "CONFIG");

    set_new_image(canvas, image, filename, 0);

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

void view_fit_rect(int canvas_width, int canvas_height, int image_width, int image_height, int *view_width, int *view_height)
{
  *view_width = canvas_width;
  *view_height = (canvas_width * image_height) / image_width;

  if (*view_height > canvas_height)
  {
    *view_height = canvas_height;
    *view_width = (canvas_height * image_width) / image_height;
  }
}



/********************************** Callbacks *****************************************/


int canvas_action_cb(Ihandle* canvas)
{
  int x, y, canvas_width, canvas_height;
  unsigned int ri, gi, bi;
  imImage* image;
  cdCanvas* cd_canvas = (cdCanvas*)IupGetAttribute(canvas, "cdCanvas");
  Ihandle* config = (Ihandle*)IupGetAttribute(canvas, "CONFIG");
  const char* background = IupConfigGetVariableStrDef(config, "Canvas", "Background", "255 255 255");

  IupGetIntInt(canvas, "DRAWSIZE", &canvas_width, &canvas_height);

  cdCanvasActivate(cd_canvas);

  /* draw the background */
  sscanf(background, "%u %u %u", &ri, &gi, &bi);
  cdCanvasBackground(cd_canvas, cdEncodeColor((unsigned char)ri, (unsigned char)gi, (unsigned char)bi));
  cdCanvasClear(cd_canvas);

  /* draw the image at the center of the canvas */
  image = (imImage*)IupGetAttribute(canvas, "IMAGE");
  if (image)
  {
    int view_width, view_height;
    Ihandle* zoom_val = IupGetDialogChild(canvas, "ZOOMVAL");
    double zoom_index = IupGetDouble(zoom_val, "VALUE");
    double zoom_factor = pow(2, zoom_index);

    float posy = IupGetFloat(canvas, "POSY");
    float posx = IupGetFloat(canvas, "POSX");

    view_width = (int)(zoom_factor * image->width);
    view_height = (int)(zoom_factor * image->height);

    if (canvas_width < view_width)
      x = (int)floor(-posx*view_width);
    else
      x = (canvas_width - view_width) / 2;

    if (canvas_height < view_height)
    {
      /* posy is top-bottom, CD is bottom-top.
         invert posy reference (YMAX-DY - POSY) */
      float dy = IupGetFloat(canvas, "DY");
      posy = 1.0f - dy - posy;
      y = (int)floor(-posy*view_height);
    }
    else
      y = (canvas_height - view_height) / 2;

    /* black line around the image */
    cdCanvasForeground(cd_canvas, CD_BLACK);
    cdCanvasRect(cd_canvas, x - 1, x + view_width, y - 1, y + view_height);

    imcdCanvasPutImage(cd_canvas, image, x, y, view_width, view_height, 0, 0, 0, 0);
  }

  cdCanvasFlush(cd_canvas);
  return IUP_DEFAULT;
}

int canvas_map_cb(Ihandle* canvas)
{
  cdCanvas* cd_canvas = cdCreateCanvas(CD_IUPDBUFFER, canvas);
  IupSetAttribute(canvas, "cdCanvas", (char*)cd_canvas);
  return IUP_DEFAULT;
}

int canvas_unmap_cb(Ihandle* canvas)
{
  cdCanvas* cd_canvas = (cdCanvas*)IupGetAttribute(canvas, "cdCanvas");
  cdKillCanvas(cd_canvas);
  return IUP_DEFAULT;
}

int zoomout_action_cb(Ihandle* ih)
{
  Ihandle* zoom_val = IupGetDialogChild(ih, "ZOOMVAL");
  double zoom_index = IupGetDouble(zoom_val, "VALUE");
  zoom_index--;
  if (zoom_index < -6)
    zoom_index = -6;
  IupSetDouble(zoom_val, "VALUE", round(zoom_index));  /* fixed increments when using buttons */

  zoom_update(ih, zoom_index);
  return IUP_DEFAULT;
}

int zoomin_action_cb(Ihandle* ih)
{
  Ihandle* zoom_val = IupGetDialogChild(ih, "ZOOMVAL");
  double zoom_index = IupGetDouble(zoom_val, "VALUE");
  zoom_index++;
  if (zoom_index > 6)
    zoom_index = 6;
  IupSetDouble(zoom_val, "VALUE", round(zoom_index));  /* fixed increments when using buttons */

  zoom_update(ih, zoom_index);
  return IUP_DEFAULT;
}

int actualsize_action_cb(Ihandle* ih)
{
  Ihandle* zoom_val = IupGetDialogChild(ih, "ZOOMVAL");
  IupSetDouble(zoom_val, "VALUE", 0);
  zoom_update(ih, 0);
  return IUP_DEFAULT;
}

int canvas_resize_cb(Ihandle* canvas)
{
  imImage* image = (imImage*)IupGetAttribute(canvas, "IMAGE");
  if (image)
  {
    Ihandle* zoom_val = IupGetDialogChild(canvas, "ZOOMVAL");
    double zoom_index = IupGetDouble(zoom_val, "VALUE");
    double zoom_factor = pow(2, zoom_index);
    float old_center_x, old_center_y;

    int view_width = (int)(zoom_factor * image->width);
    int view_height = (int)(zoom_factor * image->height);

    scroll_calc_center(canvas, &old_center_x, &old_center_y);

    scroll_update(canvas, view_width, view_height);

    scroll_center(canvas, old_center_x, old_center_y);
  }
  return IUP_DEFAULT;
}

int canvas_wheel_cb(Ihandle* canvas, float delta)
{
  if (IupGetInt(NULL, "CONTROLKEY"))
  {
    if (delta < 0)
      zoomout_action_cb(canvas);
    else
      zoomin_action_cb(canvas);
  }
  else
  {
    float posy = IupGetFloat(canvas, "POSY");
    posy -= delta * IupGetFloat(canvas, "DY") / 10.0f;
    IupSetFloat(canvas, "POSY", posy);
    IupUpdate(canvas);
  }
  return IUP_DEFAULT;
}

int zoom_valuechanged_cb(Ihandle* val)
{
  double zoom_index = IupGetDouble(val, "VALUE");
  zoom_update(val, zoom_index);
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

      set_new_image(canvas, image, NULL, 0);
    }
  }

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

int item_pagesetup_action_cb(Ihandle* item_pagesetup)
{
  Ihandle* canvas = IupGetDialogChild(item_pagesetup, "CANVAS");
  Ihandle* config = (Ihandle*)IupGetAttribute(canvas, "CONFIG");
  int margin_width = IupConfigGetVariableIntDef(config, "Print", "MarginWidth", 20);
  int margin_height = IupConfigGetVariableIntDef(config, "Print", "MarginHeight", 20);

  if (IupGetParam("Page Setup", NULL, NULL, "Margin Width (mm): %i[1,]\nMargin Height (mm): %i[1,]\n", &margin_width, &margin_height, NULL))
  {
    IupConfigSetVariableInt(config, "Print", "MarginWidth", margin_width);
    IupConfigSetVariableInt(config, "Print", "MarginHeight", margin_height);
  }

  return IUP_DEFAULT;
}

int item_print_action_cb(Ihandle* item_print)
{
  Ihandle* canvas = IupGetDialogChild(item_print, "CANVAS");
  unsigned int ri, gi, bi;
  imImage* image;
  char* title = IupGetAttribute(IupGetDialog(item_print), "TITLE");
  Ihandle* config = (Ihandle*)IupGetAttribute(canvas, "CONFIG");
  const char* background = IupConfigGetVariableStrDef(config, "Canvas", "Background", "255 255 255");

  cdCanvas* cd_canvas = cdCreateCanvasf(CD_PRINTER, "%s -d", title);
  if (!cd_canvas)
    return IUP_DEFAULT;

  /* draw the background */
  sscanf(background, "%u %u %u", &ri, &gi, &bi);
  cdCanvasBackground(cd_canvas, cdEncodeColor((unsigned char)ri, (unsigned char)gi, (unsigned char)bi));
  cdCanvasClear(cd_canvas);

  /* draw the image at the center of the canvas */
  image = (imImage*)IupGetAttribute(canvas, "IMAGE");
  if (image)
  {
    int x, y, canvas_width, canvas_height, view_width, view_height;
    double canvas_width_mm, canvas_height_mm;
    Ihandle* config = (Ihandle*)IupGetAttribute(canvas, "CONFIG");
    int margin_width = IupConfigGetVariableIntDef(config, "Print", "MarginWidth", 20);
    int margin_height = IupConfigGetVariableIntDef(config, "Print", "MarginHeight", 20);

    cdCanvasGetSize(cd_canvas, &canvas_width, &canvas_height, &canvas_width_mm, &canvas_height_mm);

    /* convert to pixels */
    margin_width = (int)((margin_width * canvas_width) / canvas_width_mm);
    margin_height = (int)((margin_height * canvas_height) / canvas_height_mm);

    view_fit_rect(canvas_width - 2 * margin_width, canvas_height - 2 * margin_height, 
                  image->width, image->height, 
                  &view_width, &view_height);

    x = (canvas_width - view_width) / 2;
    y = (canvas_height - view_height) / 2;

    imcdCanvasPutImage(cd_canvas, image, x, y, view_width, view_height, 0, 0, 0, 0);
  }

  cdKillCanvas(cd_canvas);
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

    Ihandle *clipboard = IupClipboard();
    imImage* image = IupGetNativeHandleImage(IupGetAttribute(clipboard, "NATIVEIMAGE"));
    IupDestroy(clipboard);

    if (!image)
    {
      show_error("Invalid Clipboard Data", 1);
      return IUP_DEFAULT;
    }

    set_new_image(canvas, image, NULL, 1);  /* set dirty */
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
  Ihandle *sub_menu_edit, *edit_menu, *item_copy, *item_paste, *item_print, *item_pagesetup;
  Ihandle *btn_copy, *btn_paste, *btn_new, *btn_open, *btn_save;
  Ihandle *sub_menu_help, *help_menu, *item_help, *item_about;
  Ihandle *sub_menu_view, *view_menu, *item_toolbar, *item_statusbar;
  Ihandle *item_zoomin, *item_zoomout, *item_actualsize;
  Ihandle *statusbar, *toolbar, *recent_menu, *item_background;

  canvas = IupCanvas(NULL);
  IupSetAttribute(canvas, "NAME", "CANVAS");
  IupSetAttribute(canvas, "SCROLLBAR", "Yes");
  IupSetAttribute(canvas, "DIRTY", "NO");  /* custom attribute */
  IupSetCallback(canvas, "ACTION", (Icallback)canvas_action_cb);
  IupSetCallback(canvas, "DROPFILES_CB", (Icallback)dropfiles_cb);
  IupSetCallback(canvas, "MAP_CB", (Icallback)canvas_map_cb);
  IupSetCallback(canvas, "UNMAP_CB", (Icallback)canvas_unmap_cb);
  IupSetCallback(canvas, "WHEEL_CB", (Icallback)canvas_wheel_cb);
  IupSetCallback(canvas, "RESIZE_CB", (Icallback)canvas_resize_cb);

  statusbar = IupHbox(
    IupSetAttributes(IupLabel("(0, 0) = 0   0   0"), "EXPAND=HORIZONTAL, PADDING=10x5"),
    IupSetAttributes(IupLabel(NULL), "SEPARATOR=VERTICAL"),
    IupSetAttributes(IupLabel("0 x 0"), "SIZE=70x, PADDING=10x5, NAME=SIZELABEL, ALIGNMENT=ACENTER"),
    IupSetAttributes(IupLabel(NULL), "SEPARATOR=VERTICAL"),
    IupSetAttributes(IupLabel("100%"), "SIZE=30x, PADDING=10x5, NAME=ZOOMLABEL, ALIGNMENT=ARIGHT"),
    IupSetCallbacks(IupSetAttributes(IupButton(NULL, NULL), "IMAGE=IUP_ZoomOut, FLAT=Yes, TIP=\"Zoom Out (Ctrl+-)\""), "ACTION", zoomout_action_cb, NULL),
    IupSetCallbacks(IupSetAttributes(IupVal(NULL), "VALUE=0, MIN=-6, MAX=6, RASTERSIZE=150x25, NAME=ZOOMVAL"), "VALUECHANGED_CB", zoom_valuechanged_cb, NULL),
    IupSetCallbacks(IupSetAttributes(IupButton(NULL, NULL), "IMAGE=IUP_ZoomIn, FLAT=Yes, TIP=\"Zoom In (Ctrl++)\""), "ACTION", zoomin_action_cb, NULL),
    IupSetCallbacks(IupSetAttributes(IupButton(NULL, NULL), "IMAGE=IUP_ZoomActualSize, FLAT=Yes, TIP=\"Actual Size (Ctrl+0)\""), "ACTION", actualsize_action_cb, NULL),
    NULL);
  IupSetAttribute(statusbar, "NAME", "STATUSBAR");
  IupSetAttribute(statusbar, "ALIGNMENT", "ACENTER");

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

  item_pagesetup = IupItem("Page Set&up...", NULL);
  IupSetCallback(item_pagesetup, "ACTION", (Icallback)item_pagesetup_action_cb);

  item_print = IupItem("&Print...\tCtrl+P", NULL);
  IupSetCallback(item_print, "ACTION", (Icallback)item_print_action_cb);

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

  item_zoomin = IupItem("Zoom &In\tCtrl++", NULL);
  IupSetAttribute(item_zoomin, "IMAGE", "IUP_ZoomIn");
  IupSetCallback(item_zoomin, "ACTION", (Icallback)zoomin_action_cb);

  item_zoomout = IupItem("Zoom &Out\tCtrl+-", NULL);
  IupSetAttribute(item_zoomout, "IMAGE", "IUP_ZoomOut");
  IupSetCallback(item_zoomout, "ACTION", (Icallback)zoomout_action_cb);

  item_actualsize = IupItem("&Actual Size\tCtrl+0", NULL);
  IupSetAttribute(item_actualsize, "IMAGE", "IUP_ZoomActualSize");
  IupSetCallback(item_actualsize, "ACTION", (Icallback)actualsize_action_cb);

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
    item_pagesetup,
    item_print,
    IupSeparator(),
    IupSubmenu("Recent &Files", recent_menu),
    IupSeparator(),
    item_exit,
    NULL);
  edit_menu = IupMenu(
    item_copy,
    item_paste,
    NULL);
  view_menu = IupMenu(
    item_zoomin, 
    item_zoomout, 
    item_actualsize,
    IupSeparator(),
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
  IupSetCallback(dlg, "K_cP", (Icallback)item_print_action_cb);
  IupSetCallback(dlg, "K_cMinus", (Icallback)zoomout_action_cb);
  IupSetCallback(dlg, "K_cPlus", (Icallback)zoomin_action_cb);
  IupSetCallback(dlg, "K_cEqual", (Icallback)zoomin_action_cb);
  IupSetCallback(dlg, "K_c0", (Icallback)actualsize_action_cb);

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
