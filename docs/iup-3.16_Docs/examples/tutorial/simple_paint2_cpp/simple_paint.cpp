
#include "simple_paint.h"
#include "simple_paint_util.h"

#include <stdio.h>
#include <stdlib.h>

#include <im_process.h>
#include <iup_config.h>
#include <iupim.h>
#include <im_lib.h>
#include <cd.h>



/********************************** Images *****************************************/


static Ihandle* load_image_PaintZoomGrid(void)
{
  unsigned char imgdata[] = {
    255, 255, 255, 0, 255, 255, 255, 0, 255, 255, 255, 0, 255, 255, 255, 0, 255, 255, 255, 0, 255, 255, 255, 0, 255, 255, 255, 0, 255, 255, 255, 0, 255, 255, 255, 0, 255, 255, 255, 0, 255, 255, 255, 0, 255, 255, 255, 0, 255, 255, 255, 0, 255, 255, 255, 0, 255, 255, 255, 0, 255, 255, 255, 0,
    255, 255, 255, 0, 255, 255, 255, 0, 255, 255, 255, 0, 110, 155, 223, 255, 255, 255, 255, 0, 255, 255, 255, 0, 106, 151, 219, 255, 255, 255, 255, 0, 255, 255, 255, 0, 95, 142, 210, 255, 255, 255, 255, 0, 255, 255, 255, 0, 84, 129, 201, 255, 255, 255, 255, 0, 255, 255, 255, 0, 255, 255, 255, 0,
    255, 255, 255, 0, 255, 255, 255, 0, 255, 255, 255, 0, 110, 155, 223, 255, 255, 255, 255, 0, 255, 255, 255, 0, 102, 147, 217, 255, 255, 255, 255, 0, 255, 255, 255, 0, 92, 137, 207, 255, 255, 255, 255, 0, 255, 255, 255, 0, 80, 125, 197, 255, 255, 255, 255, 0, 255, 255, 255, 0, 255, 255, 255, 0,
    255, 255, 255, 0, 110, 155, 223, 255, 110, 155, 223, 255, 107, 152, 222, 255, 104, 151, 219, 255, 103, 148, 216, 255, 100, 145, 213, 255, 97, 142, 210, 255, 93, 138, 206, 255, 88, 133, 203, 255, 84, 129, 201, 255, 80, 125, 197, 255, 76, 121, 193, 255, 72, 117, 189, 255, 68, 113, 183, 255, 255, 255, 255, 0,
    255, 255, 255, 0, 255, 255, 255, 0, 255, 255, 255, 0, 106, 149, 219, 255, 255, 255, 255, 0, 255, 255, 255, 0, 97, 142, 210, 255, 255, 255, 255, 0, 255, 255, 255, 0, 84, 131, 199, 255, 255, 255, 255, 0, 255, 255, 255, 0, 72, 117, 187, 255, 255, 255, 255, 0, 255, 255, 255, 0, 255, 255, 255, 0,
    255, 255, 255, 0, 255, 255, 255, 0, 255, 255, 255, 0, 103, 148, 216, 255, 255, 255, 255, 0, 255, 255, 255, 0, 92, 137, 207, 255, 255, 255, 255, 0, 255, 255, 255, 0, 81, 125, 196, 255, 255, 255, 255, 0, 255, 255, 255, 0, 68, 113, 185, 255, 255, 255, 255, 0, 255, 255, 255, 0, 255, 255, 255, 0,
    255, 255, 255, 0, 104, 151, 219, 255, 104, 148, 216, 255, 100, 145, 213, 255, 97, 142, 210, 255, 92, 137, 207, 255, 88, 135, 203, 255, 84, 129, 199, 255, 80, 125, 195, 255, 76, 121, 193, 255, 70, 117, 189, 255, 68, 112, 183, 255, 62, 109, 181, 255, 60, 105, 177, 255, 57, 102, 174, 255, 255, 255, 255, 0,
    255, 255, 255, 0, 255, 255, 255, 0, 255, 255, 255, 0, 97, 142, 210, 255, 255, 255, 255, 0, 255, 255, 255, 0, 84, 129, 199, 255, 255, 255, 255, 0, 255, 255, 255, 0, 72, 117, 187, 255, 255, 255, 255, 0, 255, 255, 255, 0, 60, 105, 177, 255, 255, 255, 255, 0, 255, 255, 255, 0, 255, 255, 255, 0,
    255, 255, 255, 0, 255, 255, 255, 0, 255, 255, 255, 0, 93, 138, 206, 255, 255, 255, 255, 0, 255, 255, 255, 0, 80, 125, 195, 255, 255, 255, 255, 0, 255, 255, 255, 0, 66, 113, 185, 255, 255, 255, 255, 0, 255, 255, 255, 0, 56, 103, 173, 255, 255, 255, 255, 0, 255, 255, 255, 0, 255, 255, 255, 0,
    255, 255, 255, 0, 95, 140, 212, 255, 92, 137, 207, 255, 88, 133, 203, 255, 84, 129, 199, 255, 80, 125, 195, 255, 76, 121, 193, 255, 72, 117, 187, 255, 68, 113, 185, 255, 64, 107, 181, 255, 59, 105, 178, 255, 55, 101, 174, 255, 53, 98, 170, 255, 49, 95, 168, 255, 48, 91, 165, 255, 255, 255, 255, 0,
    255, 255, 255, 0, 255, 255, 255, 0, 255, 255, 255, 0, 84, 131, 199, 255, 255, 255, 255, 0, 255, 255, 255, 0, 70, 117, 189, 255, 255, 255, 255, 0, 255, 255, 255, 0, 60, 105, 177, 255, 255, 255, 255, 0, 255, 255, 255, 0, 49, 95, 168, 255, 255, 255, 255, 0, 255, 255, 255, 0, 255, 255, 255, 0,
    255, 255, 255, 0, 255, 255, 255, 0, 255, 255, 255, 0, 82, 125, 195, 255, 255, 255, 255, 0, 255, 255, 255, 0, 68, 113, 185, 255, 255, 255, 255, 0, 255, 255, 255, 0, 57, 102, 174, 255, 255, 255, 255, 0, 255, 255, 255, 0, 46, 92, 165, 255, 255, 255, 255, 0, 255, 255, 255, 0, 255, 255, 255, 0,
    255, 255, 255, 0, 84, 129, 199, 255, 80, 125, 195, 255, 76, 121, 191, 255, 72, 117, 189, 255, 66, 113, 185, 255, 64, 109, 181, 255, 60, 105, 177, 255, 55, 102, 174, 255, 52, 98, 171, 255, 49, 95, 168, 255, 46, 91, 166, 255, 44, 90, 163, 255, 42, 88, 161, 255, 42, 88, 161, 255, 255, 255, 255, 0,
    255, 255, 255, 0, 255, 255, 255, 0, 255, 255, 255, 0, 72, 117, 187, 255, 255, 255, 255, 0, 255, 255, 255, 0, 60, 107, 177, 255, 255, 255, 255, 0, 255, 255, 255, 0, 50, 95, 167, 255, 255, 255, 255, 0, 255, 255, 255, 0, 42, 88, 161, 255, 255, 255, 255, 0, 255, 255, 255, 0, 255, 255, 255, 0,
    255, 255, 255, 0, 255, 255, 255, 0, 255, 255, 255, 0, 68, 113, 185, 255, 255, 255, 255, 0, 255, 255, 255, 0, 56, 101, 173, 255, 255, 255, 255, 0, 255, 255, 255, 0, 46, 92, 165, 255, 255, 255, 255, 0, 255, 255, 255, 0, 42, 88, 161, 255, 255, 255, 255, 0, 255, 255, 255, 0, 255, 255, 255, 0,
    255, 255, 255, 0, 255, 255, 255, 0, 255, 255, 255, 0, 255, 255, 255, 0, 255, 255, 255, 0, 255, 255, 255, 0, 255, 255, 255, 0, 255, 255, 255, 0, 255, 255, 255, 0, 255, 255, 255, 0, 255, 255, 255, 0, 255, 255, 255, 0, 255, 255, 255, 0, 255, 255, 255, 0, 255, 255, 255, 0, 255, 255, 255, 0 };

  Ihandle* image = IupImageRGBA(16, 16, imgdata);
  return image;
}


/*********************************** Utilities Methods **************************************/

                     
void SimplePaint::UpdateImage(imImage* new_image, bool update_size)
{
  file.SetImage(new_image);

  if (update_size)
  {
    Ihandle* size_lbl = IupGetDialogChild(dlg, "SIZELABEL");
    IupSetfAttribute(size_lbl, "TITLE", "%d x %d px", file.GetImage()->width, file.GetImage()->height);

    canvas.ScrollUpdate();
  }

  canvas.Update();
}

void SimplePaint::UpdateFile()
{
  Ihandle* size_lbl = IupGetDialogChild(dlg, "SIZELABEL");

  if (file.GetFilename())
    IupSetfAttribute(dlg, "TITLE", "%s - Simple Paint", str_filetitle(file.GetFilename()));
  else
    IupSetAttribute(dlg, "TITLE", "Untitled - Simple Paint");

  IupSetfAttribute(size_lbl, "TITLE", "%d x %d px", file.GetImage()->width, file.GetImage()->height);

  /* reset zoom to 100% */
  canvas.SetZoom(0);
}

void SimplePaint::CheckNewFile()
{
  if (!file.GetImage())
  {
    int width = IupConfigGetVariableIntDef(config, "NewImage", "Width", 640);
    int height = IupConfigGetVariableIntDef(config, "NewImage", "Height", 480);

    if (file.New(width, height))
      UpdateFile();
  }
}

void SimplePaint::OpenFile(const char* filename)
{
  if (file.Open(filename))
  {
    UpdateFile();

    IupConfigRecentUpdate(config, filename);
  }
}

void SimplePaint::ToggleBarVisibility(Ihandle* item, Ihandle* bar)
{
  if (IupGetInt(bar, "VISIBLE"))
  {
    IupSetAttribute(bar, "FLOATING", "YES");
    IupSetAttribute(bar, "VISIBLE", "NO");
    IupSetAttribute(item, "VALUE", "OFF");
  }
  else
  {
    IupSetAttribute(bar, "FLOATING", "NO");
    IupSetAttribute(bar, "VISIBLE", "YES");
    IupSetAttribute(item, "VALUE", "ON");
  }

  IupRefresh(bar);  /* refresh the dialog layout */
}

void SimplePaint::SelectFile(bool is_open)
{
  const char* dir = IupConfigGetVariableStr(config, "MainWindow", "LastDirectory");

  Ihandle* filedlg = IupFileDlg();
  if (is_open)
    IupSetAttribute(filedlg, "DIALOGTYPE", "OPEN");
  else
  {
    IupSetAttribute(filedlg, "DIALOGTYPE", "SAVE");
    IupSetStrAttribute(filedlg, "FILE", file.GetFilename());
  }
  IupSetAttribute(filedlg, "EXTFILTER", "Image Files|*.bmp;*.jpg;*.png;*.tif;*.tga|All Files|*.*|");
  IupSetStrAttribute(filedlg, "DIRECTORY", dir);
  IupSetAttributeHandle(filedlg, "PARENTDIALOG", dlg);

  IupPopup(filedlg, IUP_CENTERPARENT, IUP_CENTERPARENT);
  if (IupGetInt(filedlg, "STATUS") != -1)
  {
    char* filename = IupGetAttribute(filedlg, "VALUE");
    if (is_open)
      OpenFile(filename);
    else
    {
      if (file.SaveAsFile(filename))
      {
        IupSetfAttribute(dlg, "TITLE", "%s - Simple Paint", str_filetitle(filename));
        IupConfigRecentUpdate(config, filename);
      }
    }

    dir = IupGetAttribute(filedlg, "DIRECTORY");
    IupConfigSetVariableStr(config, "MainWindow", "LastDirectory", dir);
  }

  IupDestroy(filedlg);
}


/********************************** Callbacks Methods *****************************************/


int SimplePaint::ZoomValueChangedCallback(Ihandle* val)
{
  double zoom_index = IupGetDouble(val, "VALUE");
  canvas.SetZoom(zoom_index);
  return IUP_DEFAULT;
}

int SimplePaint::DialogDropfilesCallback(Ihandle*, char* filename)
{
  if (file.SaveCheck())
    OpenFile(filename);

  return IUP_DEFAULT;
}

int SimplePaint::FileMenuOpenCallback(Ihandle*)
{
  Ihandle* item_save = IupGetDialogChild(dlg, "ITEM_SAVE");
  if (file.GetImageChanged())
    IupSetAttribute(item_save, "ACTIVE", "YES");
  else
    IupSetAttribute(item_save, "ACTIVE", "NO");

  Ihandle* item_revert = IupGetDialogChild(dlg, "ITEM_REVERT");
  if (file.GetImageChanged() && file.GetFilename())
    IupSetAttribute(item_revert, "ACTIVE", "YES");
  else
    IupSetAttribute(item_revert, "ACTIVE", "NO");
  return IUP_DEFAULT;
}

int SimplePaint::EditMenuOpenCallback(Ihandle*)
{
  Ihandle *clipboard = IupClipboard(); 

  Ihandle *item_paste = IupGetDialogChild(dlg, "ITEM_PASTE");

  if (!IupGetInt(clipboard, "IMAGEAVAILABLE"))
    IupSetAttribute(item_paste, "ACTIVE", "NO");
  else
    IupSetAttribute(item_paste, "ACTIVE", "YES");

  IupDestroy(clipboard);
  return IUP_DEFAULT;
}

int SimplePaint::ViewMenuOpenCallback(Ihandle*)
{
  Ihandle* item_zoomgrid = IupGetDialogChild(dlg, "ITEM_ZOOMGRID");
  if (canvas.GetZoomGrid())
    IupSetAttribute(item_zoomgrid, "VALUE", "ON");
  else
    IupSetAttribute(item_zoomgrid, "VALUE", "OFF");

  Ihandle* item_toolbox = IupGetDialogChild(dlg, "ITEM_TOOLBOX");
  if (toolbox.Visible())
    IupSetAttribute(item_toolbox, "VALUE", "ON");
  else
    IupSetAttribute(item_toolbox, "VALUE", "OFF");

  Ihandle* item_toolbar = IupGetDialogChild(dlg, "ITEM_TOOLBAR");
  Ihandle* toolbar = IupGetDialogChild(dlg, "TOOLBAR");
  if (IupGetInt(toolbar, "VISIBLE"))
    IupSetAttribute(item_toolbar, "VALUE", "ON");
  else
    IupSetAttribute(item_toolbar, "VALUE", "OFF");

  Ihandle* item_statusbar = IupGetDialogChild(dlg, "ITEM_STATUSBAR");
  Ihandle* statusbar = IupGetDialogChild(dlg, "STATUSBAR");
  if (IupGetInt(statusbar, "VISIBLE"))
    IupSetAttribute(item_statusbar, "VALUE", "ON");
  else
    IupSetAttribute(item_statusbar, "VALUE", "OFF");

  return IUP_DEFAULT;
}

int SimplePaint::ConfigRecentCallback(Ihandle* ih)
{
  if (file.SaveCheck())
  {
    char* filename = IupGetAttribute(ih, "TITLE");
    OpenFile(filename);
  }
  return IUP_DEFAULT;
}

int SimplePaint::DialogMoveCallback(Ihandle* dlg, int x, int y)
{
  int old_x = IupGetInt(dlg, "_OLD_X");
  int old_y = IupGetInt(dlg, "_OLD_Y");

  if (old_x == x && old_y == y)
    return IUP_DEFAULT;

  toolbox.MoveDialog(x - old_x, y - old_y);

  IupSetInt(dlg, "_OLD_X", x);
  IupSetInt(dlg, "_OLD_Y", y);

  return IUP_DEFAULT;
}


/********************************* SimplePaint Menu Item Callbacks Methods **************************/


int SimplePaint::ItemNewActionCallback(Ihandle*)
{
  if (file.SaveCheck())
  {
    int width = IupConfigGetVariableIntDef(config, "NewImage", "Width", 640);
    int height = IupConfigGetVariableIntDef(config, "NewImage", "Height", 480);

    if (IupGetParam("New Image", NULL, NULL, "Width: %i[1,]\nHeight: %i[1,]\n", &width, &height, NULL))
    {
      IupConfigSetVariableInt(config, "NewImage", "Width", width);
      IupConfigSetVariableInt(config, "NewImage", "Height", height);

      file.New(width, height);

      UpdateFile();
    }
  }

  return IUP_DEFAULT;
}

int SimplePaint::ItemOpenActionCallback(Ihandle*)
{
  if (!file.SaveCheck())
    return IUP_DEFAULT;

  SelectFile(true);
  return IUP_DEFAULT;
}

int SimplePaint::ItemSaveasActionCallback(Ihandle*)
{
  SelectFile(false);
  return IUP_DEFAULT;
}

int SimplePaint::ItemSaveActionCallback(Ihandle* item_save)
{
  if (!file.GetFilename())
    ItemSaveasActionCallback(item_save);
  else   
  {
    /* test again because in can be called using the hot key */
    if (file.GetImageChanged())
      file.SaveFile();
  }
  return IUP_DEFAULT;
}

int SimplePaint::ItemRevertActionCallback(Ihandle*)
{
  OpenFile(file.GetFilename());
  return IUP_DEFAULT;
}

int SimplePaint::ItemPagesetupActionCallback(Ihandle*)
{
  int margin_width = IupConfigGetVariableIntDef(config, "Print", "MarginWidth", 20);
  int margin_height = IupConfigGetVariableIntDef(config, "Print", "MarginHeight", 20);

  if (IupGetParam("Page Setup", NULL, NULL, "Margin Width (mm): %i[1,]\nMargin Height (mm): %i[1,]\n", &margin_width, &margin_height, NULL))
  {
    IupConfigSetVariableInt(config, "Print", "MarginWidth", margin_width);
    IupConfigSetVariableInt(config, "Print", "MarginHeight", margin_height);
  }

  return IUP_DEFAULT;
}

int SimplePaint::ItemPrintActionCallback(Ihandle*)
{
  char* title = IupGetAttribute(dlg, "TITLE");
  int margin_width = IupConfigGetVariableIntDef(config, "Print", "MarginWidth", 20);
  int margin_height = IupConfigGetVariableIntDef(config, "Print", "MarginHeight", 20);
  canvas.Print(title, margin_width, margin_height);
  return IUP_DEFAULT;
}

int SimplePaint::ItemExitActionCallback(Ihandle*)
{
  if (!file.SaveCheck())
    return IUP_IGNORE;  /* to abort the CLOSE_CB callback normal processing */

  IupConfigSetVariableStr(config, "MainWindow", "Toolbox", toolbox.Visible() ? "ON" : "OFF");
  if (toolbox.Visible())
    toolbox.HideDialog();  /* manually hide it before the main dialog, because it is a child dialog */

  file.Close();

  IupConfigDialogClosed(config, dlg, "MainWindow");
  IupConfigSave(config);
  IupDestroy(config);
  return IUP_CLOSE;
}

int SimplePaint::ItemCopyActionCallback(Ihandle*)
{
  Ihandle *clipboard = IupClipboard();
  IupSetAttribute(clipboard, "NATIVEIMAGE", NULL); /* clear clipboard first */
  IupSetAttribute(clipboard, "NATIVEIMAGE", (char*)IupGetImageNativeHandle(file.GetImage()));
  IupDestroy(clipboard);
  return IUP_DEFAULT;
}

int SimplePaint::ItemPasteActionCallback(Ihandle*)
{
  if (file.SaveCheck())
  {
    Ihandle *clipboard = IupClipboard();
    imImage* new_image = IupGetNativeHandleImage(IupGetAttribute(clipboard, "NATIVEIMAGE"));
    IupDestroy(clipboard);

    if (!new_image)
    {
      show_error("Invalid Clipboard Data", 1);
      return IUP_DEFAULT;
    }

    file.New(new_image);

    UpdateFile();
  }
  return IUP_DEFAULT;
}

int SimplePaint::ItemBackgroundActionCallback(Ihandle*)
{
  Ihandle* colordlg = IupColorDlg();
  long background = canvas.GetBackground();
  IupSetRGB(colordlg, "VALUE", cdRed(background), cdGreen(background), cdBlue(background));
  IupSetAttributeHandle(colordlg, "PARENTDIALOG", dlg);

  IupPopup(colordlg, IUP_CENTERPARENT, IUP_CENTERPARENT);

  if (IupGetInt(colordlg, "STATUS") == 1)
  {
    const char* background_str = IupGetAttribute(colordlg, "VALUE");
    IupConfigSetVariableStr(config, "Canvas", "Background", background_str);

    unsigned int ri, gi, bi;
    sscanf(background_str, "%u %u %u", &ri, &gi, &bi);
    canvas.SetBackground(cdEncodeColor((unsigned char)ri, (unsigned char)gi, (unsigned char)bi));
  }

  IupDestroy(colordlg);
  return IUP_DEFAULT;
}

int SimplePaint::ItemZoomoutActionCallback(Ihandle*)
{
  canvas.ZoomOut();
  return IUP_DEFAULT;
}

int SimplePaint::ItemZoominActionCallback(Ihandle*)
{
  canvas.ZoomIn();
  return IUP_DEFAULT;
}

int SimplePaint::ItemActualsizeActionCallback(Ihandle*)
{
  canvas.SetZoom(0);
  return IUP_DEFAULT;
}

int SimplePaint::ItemZoomgridActionCallback(Ihandle*)
{
  canvas.SetZoomGrid(canvas.GetZoomGrid()? false: true);

  IupConfigSetVariableStr(config, "Canvas", "ZoomGrid", canvas.GetZoomGrid()? "ON": "OFF");

  return IUP_DEFAULT;
}

int SimplePaint::ItemToolbarActionCallback(Ihandle* item_toolbar)
{
  Ihandle* toolbar = IupGetDialogChild(dlg, "TOOLBAR");

  ToggleBarVisibility(item_toolbar, toolbar);

  IupConfigSetVariableStr(config, "MainWindow", "Toolbar", IupGetAttribute(item_toolbar, "VALUE"));
  return IUP_DEFAULT;
}

int SimplePaint::ItemToolboxActionCallback(Ihandle*)
{
  if (toolbox.Visible())
    toolbox.HideDialog();
  else
    toolbox.ShowDialog();
  return IUP_DEFAULT;
}

int SimplePaint::ItemStatusbarActionCallback(Ihandle* item_statusbar)
{
  Ihandle* statusbar = IupGetDialogChild(dlg, "STATUSBAR");

  ToggleBarVisibility(item_statusbar, statusbar);

  IupConfigSetVariableStr(config, "MainWindow", "Statusbar", IupGetAttribute(item_statusbar, "VALUE"));
  return IUP_DEFAULT;
}

int SimplePaint::ItemResizeActionCallback(Ihandle*)
{
  int height = file.GetImage()->height,
    width = file.GetImage()->width;
  int quality = IupConfigGetVariableIntDef(config, "Image", "ResizeQuality", 1);  /* medium default */

  if (!IupGetParam("Resize", NULL, NULL,
    "Width: %i[1,]\n"
    "Height: %i[1,]\n"
    "Quality: %l|low|medium|high|\n",
    &width, &height, &quality, NULL))
    return IUP_DEFAULT;

  IupConfigSetVariableInt(config, "Image", "ResizeQuality", quality);

  imImage* new_image = imImageCreateBased(file.GetImage(), width, height, -1, -1);
  if (!new_image)
  {
    show_file_error(IM_ERR_MEM);
    return IUP_DEFAULT;
  }

  if (quality == 2)
    quality = 3; /* interpolation order can be 0, 1, and 3 */

  imProcessResize(file.GetImage(), new_image, quality);

  UpdateImage(new_image, true);   /* update size */

  return IUP_DEFAULT;
}

int SimplePaint::ItemMirrorActionCallback(Ihandle*)
{
  imImage* new_image = imImageClone(file.GetImage());
  if (!new_image)
  {
    show_file_error(IM_ERR_MEM);
    return IUP_DEFAULT;
  }

  imProcessMirror(file.GetImage(), new_image);

  UpdateImage(new_image);

  return IUP_DEFAULT;
}

int SimplePaint::ItemFlipActionCallback(Ihandle*)
{
  imImage* new_image = imImageClone(file.GetImage());
  if (!new_image)
  {
    show_file_error(IM_ERR_MEM);
    return IUP_DEFAULT;
  }

  imProcessFlip(file.GetImage(), new_image);

  UpdateImage(new_image);

  return IUP_DEFAULT;
}

int SimplePaint::ItemRotate180ActionCallback(Ihandle*)
{
  imImage* new_image = imImageClone(file.GetImage());
  if (!new_image)
  {
    show_file_error(IM_ERR_MEM);
    return IUP_DEFAULT;
  }

  imProcessRotate180(file.GetImage(), new_image);

  UpdateImage(new_image);

  return IUP_DEFAULT;
}

int SimplePaint::ItemRotate90cwActionCallback(Ihandle*)
{
  imImage* new_image = imImageCreateBased(file.GetImage(), file.GetImage()->height, file.GetImage()->width, -1, -1);
  if (!new_image)
  {
    show_file_error(IM_ERR_MEM);
    return IUP_DEFAULT;
  }

  imProcessRotate90(file.GetImage(), new_image, 1);

  UpdateImage(new_image, true);   /* update size */

  return IUP_DEFAULT;
}

int SimplePaint::ItemRotate90ccwActionCallback(Ihandle*)
{
  imImage* new_image = imImageCreateBased(file.GetImage(), file.GetImage()->height, file.GetImage()->width, -1, -1);
  if (!new_image)
  {
    show_file_error(IM_ERR_MEM);
    return IUP_DEFAULT;
  }

  imProcessRotate90(file.GetImage(), new_image, -1);

  UpdateImage(new_image, true);   /* update size */

  return IUP_DEFAULT;
}

int SimplePaint::ItemNegativeActionCallback(Ihandle*)
{
  imImage* new_image = imImageClone(file.GetImage());
  if (!new_image)
  {
    show_file_error(IM_ERR_MEM);
    return IUP_DEFAULT;
  }

  imProcessNegative(file.GetImage(), new_image);

  UpdateImage(new_image);

  return IUP_DEFAULT;
}

static int brightcont_param_cb(Ihandle* dialog, int param_index, void* user_data)
{
  Ihandle* dlg = (Ihandle*)user_data;

  if (param_index == 0 || param_index == 1)
  {
    imImage* image = (imImage*)IupGetAttribute(dlg, "ORIGINAL_IMAGE");
    imImage* new_image = (imImage*)IupGetAttribute(dlg, "NEW_IMAGE");
    SimplePaintFile* file = (SimplePaintFile*)IupGetAttribute(dlg, "PAINT_FILE");
    SimplePaintCanvas* canvas = (SimplePaintCanvas*)IupGetAttribute(dlg, "PAINT_CANVAS");
    Ihandle* brightness_shift_param = (Ihandle*)IupGetAttribute(dialog, "PARAM0");
    Ihandle* contrast_factor_param = (Ihandle*)IupGetAttribute(dialog, "PARAM1");

    float param[2] = { 0, 0 };
    param[0] = IupGetFloat(brightness_shift_param, "VALUE");
    param[1] = IupGetFloat(contrast_factor_param, "VALUE");

    imProcessToneGamut(image, new_image, IM_GAMUT_BRIGHTCONT, param);

    file->SetImage(new_image, false);

    canvas->Update();
  }
  else if (param_index != IUP_GETPARAM_INIT)
  {
    /* restore original configuration */
    SimplePaintFile* file = (SimplePaintFile*)IupGetAttribute(dlg, "PAINT_FILE");
    SimplePaintCanvas* canvas = (SimplePaintCanvas*)IupGetAttribute(dlg, "PAINT_CANVAS");
    imImage* image = (imImage*)IupGetAttribute(dlg, "ORIGINAL_IMAGE");

    file->SetImage(image, false);

    IupSetAttribute(dlg, "ORIGINAL_IMAGE", NULL);
    IupSetAttribute(dlg, "NEW_IMAGE", NULL);
    IupSetAttribute(dlg, "PAINT_FILE", NULL);
    IupSetAttribute(dlg, "PAINT_CANVAS", NULL);

    if (param_index == IUP_GETPARAM_BUTTON2)  /* cancel */
      canvas->Update();
  }

  return 1;
}

int SimplePaint::ItemBrightcontActionCallback(Ihandle*)
{
  imImage* new_image = imImageClone(file.GetImage());
  if (!new_image)
  {
    show_file_error(IM_ERR_MEM);
    return IUP_DEFAULT;
  }

  IupSetAttribute(dlg, "ORIGINAL_IMAGE", (char*)file.GetImage());
  IupSetAttribute(dlg, "NEW_IMAGE", (char*)new_image);
  IupSetAttribute(dlg, "PAINT_FILE", (char*)&file);
  IupSetAttribute(dlg, "PAINT_CANVAS", (char*)&canvas);

  float param[2] = { 0, 0 };

  if (!IupGetParam("Brightness and Contrast", brightcont_param_cb, dlg,
                   "Brightness Shift: %r[-100,100]\n"
                   "Contrast Factor: %r[-100,100]\n",
                   &param[0], &param[1], NULL))
  {
    imImageDestroy(new_image);
    return IUP_DEFAULT;
  }

  /* since the image was already processed in preview we don't need to process it again,
     but leave it here to illustrate the logic */
  imProcessToneGamut(file.GetImage(), new_image, IM_GAMUT_BRIGHTCONT, param);

  UpdateImage(new_image);

  return IUP_DEFAULT;
}

int SimplePaint::ItemHelpActionCallback(Ihandle*)
{
  IupHelp("http://www.tecgraf.puc-rio.br/iup");
  return IUP_DEFAULT;
}

static int bt_ok_action_cb(Ihandle*)
{
  return IUP_CLOSE;
}

static int bt_sysinfo_action_cb(Ihandle*)
{
  char sysinfo[10240];
  int o = 0;

  o += sprintf(sysinfo+o, "-------  System Information  -------\n\n");

  o += sprintf(sysinfo+o, "IUP %s %s\n\n", IupVersion(), IUP_COPYRIGHT);

  o += sprintf(sysinfo+o, "  System: %s\n", IupGetGlobal("SYSTEM"));
  o += sprintf(sysinfo+o, "  System Version: %s\n", IupGetGlobal("SYSTEMVERSION"));

  char* motif = IupGetGlobal("MOTIFVERSION");
  if (motif)
    o += sprintf(sysinfo+o, "  Motif Version: %s\n", motif);

  char* gtk = IupGetGlobal("GTKVERSION");
  if (gtk)
    o += sprintf(sysinfo+o, "  GTK Version: %s\n", gtk);

  o += sprintf(sysinfo+o, "\n  Screen Size: %s\n", IupGetGlobal("SCREENSIZE"));
  o += sprintf(sysinfo+o, "  Screen Depth: %s\n", IupGetGlobal("SCREENDEPTH"));

  char* opengl = IupGetGlobal("GL_VERSION");
  if (opengl)
  {
    o += sprintf(sysinfo+o, "\n  OpenGL Version: %s\n", opengl);
    o += sprintf(sysinfo+o, "  OpenGL Vendor: %s\n", IupGetGlobal("GL_VENDOR"));
    o += sprintf(sysinfo+o, "  OpenGL Renderer: %s\n", IupGetGlobal("GL_RENDERER"));
  }

  o += sprintf(sysinfo+o, "\nIM %s %s\n", imVersion(), IM_COPYRIGHT);
  o += sprintf(sysinfo+o, "\nCD %s %s\n", cdVersion(), CD_COPYRIGHT);

  IupGetText("System Information", sysinfo);
  return IUP_DEFAULT;
}

int SimplePaint::ItemAboutActionCallback(Ihandle* ih)
{
  Ihandle *bt_ok, *vbox, *dlg_about, *lbl, *bt_sysinfo, *txt;

  lbl = IupLabel(NULL);
  IupSetAttribute(lbl, "IMAGE", "SPLASH");

  bt_ok = IupButton("OK", NULL);
  IupSetAttribute(bt_ok, "PADDING", "6X3");
  IupSetCallback(bt_ok, "ACTION", bt_ok_action_cb);

  bt_sysinfo = IupButton("SysInfo", NULL);
  IupSetAttribute(bt_sysinfo, "PADDING", "6X3");
  IupSetCallback(bt_sysinfo, "ACTION", bt_sysinfo_action_cb);

  vbox = IupVbox(
    lbl,
    IupSetAttributes(IupLabel("Simple Paint 1.2"), "FONT=\"Helvetica, Bold 14\""),
    IupLabel("Copyright © 1995-2015 Tecgraf/PUC-Rio"),
    txt = IupSetAttributes(IupText(NULL), "READONLY=Yes, BORDER=NO, VALUE=\"iup@tecgraf.puc-rio.br\", VISIBLECOLUMNS=12, ALIGNMENT=ACENTER"),
    IupSetAttributes(IupLabel(NULL), "SEPARATOR=Horizontal"),
    IupSetAttributes(IupHbox(bt_sysinfo, IupFill(), bt_ok, NULL), "MARGIN=10x10"),
    NULL);

  IupSetAttribute(vbox, "ALIGNMENT", "ACENTER");
  IupSetAttribute(vbox, "MARGIN", "5x5");
  IupSetAttribute(vbox, "GAP", "5");
  IupSetAttribute(vbox, "FONT", "Helvetica, 12");

  dlg_about = IupDialog(vbox);
  IupSetAttribute(dlg_about, "TITLE", "About");
  IupSetAttribute(dlg_about, "RESIZE", "NO");
  IupSetAttribute(dlg_about, "MINBOX", "NO");
  IupSetAttribute(dlg_about, "MAXBOX", "NO");
  IupSetAttribute(dlg_about, "MENUBOX", "NO");
  IupSetAttributeHandle(dlg_about, "DEFAULTENTER", bt_ok);
  IupSetAttributeHandle(dlg_about, "DEFAULTESC", bt_ok);
  IupSetAttributeHandle(dlg_about, "PARENTDIALOG", IupGetDialog(ih));

  IupSetStrAttribute(txt, "BGCOLOR", IupGetAttribute(dlg_about, "BGCOLOR"));

  IupPopup(dlg_about, IUP_CENTERPARENT, IUP_CENTERPARENT);
  IupDestroy(dlg_about);
  return IUP_DEFAULT;
}


/********************************** Main *****************************************/


Ihandle* SimplePaint::CreateMainMenu()
{
  Ihandle *menu, *sub_menu_file;
  Ihandle *file_menu, *item_exit, *item_new, *item_open, *item_save, *item_saveas, *item_revert;
  Ihandle *sub_menu_edit, *edit_menu, *item_copy, *item_paste, *item_print, *item_pagesetup;
  Ihandle *sub_menu_help, *help_menu, *item_help, *item_about;
  Ihandle *sub_menu_view, *view_menu, *item_toolbar, *item_statusbar;
  Ihandle *item_zoomin, *item_zoomout, *item_actualsize;
  Ihandle *recent_menu, *item_background, *item_toolbox, *item_zoomgrid;
  Ihandle *sub_menu_image, *image_menu;

  item_new = IupItem("&New\tCtrl+N", NULL);
  IupSetAttribute(item_new, "IMAGE", "IUP_FileNew");
  IUP_CLASS_SETCALLBACK(item_new, "ACTION", ItemNewActionCallback);

  item_open = IupItem("&Open...\tCtrl+O", NULL);
  IupSetAttribute(item_open, "IMAGE", "IUP_FileOpen");
  IUP_CLASS_SETCALLBACK(item_open, "ACTION", ItemOpenActionCallback);

  item_save = IupItem("&Save\tCtrl+S", NULL);
  IupSetAttribute(item_save, "NAME", "ITEM_SAVE");
  IupSetAttribute(item_save, "IMAGE", "IUP_FileSave");
  IUP_CLASS_SETCALLBACK(item_save, "ACTION", ItemSaveActionCallback);

  item_saveas = IupItem("Save &As...", NULL);
  IupSetAttribute(item_saveas, "NAME", "ITEM_SAVEAS");
  IUP_CLASS_SETCALLBACK(item_saveas, "ACTION", ItemSaveasActionCallback);

  item_revert = IupItem("&Revert", NULL);
  IupSetAttribute(item_revert, "NAME", "ITEM_REVERT");
  IUP_CLASS_SETCALLBACK(item_revert, "ACTION", ItemRevertActionCallback);

  item_pagesetup = IupItem("Page Set&up...", NULL);
  IUP_CLASS_SETCALLBACK(item_pagesetup, "ACTION", ItemPagesetupActionCallback);

  item_print = IupItem("&Print...\tCtrl+P", NULL);
  IUP_CLASS_SETCALLBACK(item_print, "ACTION", ItemPrintActionCallback);

  item_exit = IupItem("E&xit", NULL);
  IUP_CLASS_SETCALLBACK(item_exit, "ACTION", ItemExitActionCallback);

  item_copy = IupItem("&Copy\tCtrl+C", NULL);
  IupSetAttribute(item_copy, "NAME", "ITEM_COPY");
  IupSetAttribute(item_copy, "IMAGE", "IUP_EditCopy");
  IUP_CLASS_SETCALLBACK(item_copy, "ACTION", ItemCopyActionCallback);

  item_paste = IupItem("&Paste\tCtrl+V", NULL);
  IupSetAttribute(item_paste, "NAME", "ITEM_PASTE");
  IupSetAttribute(item_paste, "IMAGE", "IUP_EditPaste");
  IUP_CLASS_SETCALLBACK(item_paste, "ACTION", ItemPasteActionCallback);

  item_zoomin = IupItem("Zoom &In\tCtrl++", NULL);
  IupSetAttribute(item_zoomin, "IMAGE", "IUP_ZoomIn");
  IUP_CLASS_SETCALLBACK(item_zoomin, "ACTION", ItemZoominActionCallback);

  item_zoomout = IupItem("Zoom &Out\tCtrl+-", NULL);
  IupSetAttribute(item_zoomout, "IMAGE", "IUP_ZoomOut");
  IUP_CLASS_SETCALLBACK(item_zoomout, "ACTION", ItemZoomoutActionCallback);

  item_actualsize = IupItem("&Actual Size\tCtrl+0", NULL);
  IupSetAttribute(item_actualsize, "IMAGE", "IUP_ZoomActualSize");
  IUP_CLASS_SETCALLBACK(item_actualsize, "ACTION", ItemActualsizeActionCallback);

  item_zoomgrid = IupItem("&Zoom Grid", NULL);
  IUP_CLASS_SETCALLBACK(item_zoomgrid, "ACTION", ItemZoomgridActionCallback);
  IupSetAttribute(item_zoomgrid, "NAME", "ITEM_ZOOMGRID");
  IupSetAttribute(item_zoomgrid, "VALUE", "ON");  /* default is ON */

  item_background = IupItem("&Background...", NULL);
  IUP_CLASS_SETCALLBACK(item_background, "ACTION", ItemBackgroundActionCallback);

  item_toolbar = IupItem("&Toobar", NULL);
  IUP_CLASS_SETCALLBACK(item_toolbar, "ACTION", ItemToolbarActionCallback);
  IupSetAttribute(item_toolbar, "NAME", "ITEM_TOOLBAR");
  IupSetAttribute(item_toolbar, "VALUE", "ON");   /* default is ON */

  item_toolbox = IupItem("&Toobox", NULL);
  IUP_CLASS_SETCALLBACK(item_toolbox, "ACTION", ItemToolboxActionCallback);
  IupSetAttribute(item_toolbox, "NAME", "ITEM_TOOLBOX");
  IupSetAttribute(item_toolbox, "VALUE", "ON");   /* default is ON */

  item_statusbar = IupItem("&Statusbar", NULL);
  IUP_CLASS_SETCALLBACK(item_statusbar, "ACTION", ItemStatusbarActionCallback);
  IupSetAttribute(item_statusbar, "NAME", "ITEM_STATUSBAR");
  IupSetAttribute(item_statusbar, "VALUE", "ON");  /* default is ON */

  item_help = IupItem("&Help...", NULL);
  IUP_CLASS_SETCALLBACK(item_help, "ACTION", ItemHelpActionCallback);

  item_about = IupItem("&About...", NULL);
  IUP_CLASS_SETCALLBACK(item_about, "ACTION", ItemAboutActionCallback);

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
    item_zoomgrid,
    IupSeparator(),
    item_background,
    IupSeparator(),
    item_toolbar,
    item_toolbox,
    item_statusbar,
    NULL);
  image_menu = IupMenu(
    IupSetCallbacks(IupItem("&Resize...", NULL), "ACTION", CB_ItemResizeActionCallback, NULL),
    IupSetCallbacks(IupItem("&Mirror", NULL), "ACTION", CB_ItemMirrorActionCallback, NULL),
    IupSetCallbacks(IupItem("&Flip", NULL), "ACTION", CB_ItemFlipActionCallback, NULL),
    IupSetCallbacks(IupItem("&Rotate 180º", NULL), "ACTION", CB_ItemRotate180ActionCallback, NULL),
    IupSetCallbacks(IupItem("&Rotate +90º (clock-wise)", NULL), "ACTION", CB_ItemRotate90cwActionCallback, NULL),
    IupSetCallbacks(IupItem("&Rotate -90º (counter-clock)", NULL), "ACTION", CB_ItemRotate90ccwActionCallback, NULL),
    IupSeparator(),
    IupSetCallbacks(IupItem("&Negative", NULL), "ACTION", CB_ItemNegativeActionCallback, NULL),
    IupSetCallbacks(IupItem("&Brightness and Contrast...", NULL), "ACTION", CB_ItemBrightcontActionCallback, NULL),
    NULL);
  help_menu = IupMenu(
    item_help,
    item_about,
    NULL);
  
  IUP_CLASS_SETCALLBACK(file_menu, "OPEN_CB", FileMenuOpenCallback);
  IUP_CLASS_SETCALLBACK(edit_menu, "OPEN_CB", EditMenuOpenCallback);
  IUP_CLASS_SETCALLBACK(view_menu, "OPEN_CB", ViewMenuOpenCallback);

  sub_menu_file = IupSubmenu("&File", file_menu);
  sub_menu_edit = IupSubmenu("&Edit", edit_menu);
  sub_menu_view = IupSubmenu("&View", view_menu);
  sub_menu_image = IupSubmenu("&Image", image_menu);
  sub_menu_help = IupSubmenu("&Help", help_menu);

  menu = IupMenu(
    sub_menu_file,
    sub_menu_edit,
    sub_menu_view,
    sub_menu_image,
    sub_menu_help,
    NULL);

  IupConfigRecentInit(config, recent_menu, CB_ConfigRecentCallback, 10);

  return menu;
}

Ihandle* SimplePaint::CreateToolbar()
{
  Ihandle *toolbar;
  Ihandle *btn_copy, *btn_paste, *btn_new, *btn_open, *btn_save, *btn_zoomgrid;

  IupSetHandle("PaintZoomGrid", load_image_PaintZoomGrid());

  btn_new = IupButton(NULL, NULL);
  IupSetAttribute(btn_new, "IMAGE", "IUP_FileNew");
  IupSetAttribute(btn_new, "FLAT", "Yes");
  IUP_CLASS_SETCALLBACK(btn_new, "ACTION", ItemNewActionCallback);
  IupSetAttribute(btn_new, "TIP", "New (Ctrl+N)");
  IupSetAttribute(btn_new, "CANFOCUS", "No");

  btn_open = IupButton(NULL, NULL);
  IupSetAttribute(btn_open, "IMAGE", "IUP_FileOpen");
  IupSetAttribute(btn_open, "FLAT", "Yes");
  IUP_CLASS_SETCALLBACK(btn_open, "ACTION", ItemOpenActionCallback);
  IupSetAttribute(btn_open, "TIP", "Open (Ctrl+O)");
  IupSetAttribute(btn_open, "CANFOCUS", "No");

  btn_save = IupButton(NULL, NULL);
  IupSetAttribute(btn_save, "IMAGE", "IUP_FileSave");
  IupSetAttribute(btn_save, "FLAT", "Yes");
  IUP_CLASS_SETCALLBACK(btn_save, "ACTION", ItemSaveActionCallback);
  IupSetAttribute(btn_save, "TIP", "Save (Ctrl+S)");
  IupSetAttribute(btn_save, "CANFOCUS", "No");

  btn_copy = IupButton(NULL, NULL);
  IupSetAttribute(btn_copy, "IMAGE", "IUP_EditCopy");
  IupSetAttribute(btn_copy, "FLAT", "Yes");
  IUP_CLASS_SETCALLBACK(btn_copy, "ACTION", ItemCopyActionCallback);
  IupSetAttribute(btn_copy, "TIP", "Copy (Ctrl+C)");
  IupSetAttribute(btn_copy, "CANFOCUS", "No");

  btn_paste = IupButton(NULL, NULL);
  IupSetAttribute(btn_paste, "IMAGE", "IUP_EditPaste");
  IupSetAttribute(btn_paste, "FLAT", "Yes");
  IUP_CLASS_SETCALLBACK(btn_paste, "ACTION", ItemPasteActionCallback);
  IupSetAttribute(btn_paste, "TIP", "Paste (Ctrl+V)");
  IupSetAttribute(btn_paste, "CANFOCUS", "No");

  btn_zoomgrid = IupButton(NULL, NULL);
  IupSetAttribute(btn_zoomgrid, "IMAGE", "PaintZoomGrid");
  IupSetAttribute(btn_zoomgrid, "FLAT", "Yes");
  IUP_CLASS_SETCALLBACK(btn_zoomgrid, "ACTION", ItemZoomgridActionCallback);
  IupSetAttribute(btn_zoomgrid, "TIP", "Zoom Grid");
  IupSetAttribute(btn_zoomgrid, "CANFOCUS", "No");

  toolbar = IupHbox(
    btn_new,
    btn_open,
    btn_save,
    IupSetAttributes(IupLabel(NULL), "SEPARATOR=VERTICAL"),
    btn_copy,
    btn_paste,
    IupSetAttributes(IupLabel(NULL), "SEPARATOR=VERTICAL"),
    btn_zoomgrid,
    NULL);
  IupSetAttribute(toolbar, "MARGIN", "5x5");
  IupSetAttribute(toolbar, "GAP", "2");
  IupSetAttribute(toolbar, "NAME", "TOOLBAR");

  /* Initialize variables from the configuration file */

  if (!IupConfigGetVariableIntDef(config, "MainWindow", "Toolbar", 1))
  {
    IupSetAttribute(toolbar, "FLOATING", "YES");
    IupSetAttribute(toolbar, "VISIBLE", "NO");
  }

  return toolbar;
}

Ihandle* SimplePaint::CreateStatusbar()
{
  Ihandle *statusbar;

  statusbar = IupHbox(
    IupSetAttributes(IupLabel("(0, 0) = 0   0   0"), "EXPAND=HORIZONTAL, PADDING=10x5, NAME=STATUSLABEL"),
    IupSetAttributes(IupLabel(NULL), "SEPARATOR=VERTICAL"),
    IupSetAttributes(IupLabel("0 x 0"), "SIZE=70x, PADDING=10x5, NAME=SIZELABEL, ALIGNMENT=ACENTER"),
    IupSetAttributes(IupLabel(NULL), "SEPARATOR=VERTICAL"),
    IupSetAttributes(IupLabel("100%"), "SIZE=30x, PADDING=10x5, NAME=ZOOMLABEL, ALIGNMENT=ARIGHT"),
    IupSetCallbacks(IupSetAttributes(IupButton(NULL, NULL), "IMAGE=IUP_ZoomOut, FLAT=Yes, TIP=\"Zoom Out (Ctrl+-)\", CANFOCUS=No"), "ACTION", CB_ItemZoomoutActionCallback, NULL),
    IupSetCallbacks(IupSetAttributes(IupVal(NULL), "VALUE=0, MIN=-6, MAX=6, SIZE=50x10, NAME=ZOOMVAL, CANFOCUS=No"), "VALUECHANGED_CB", CB_ZoomValueChangedCallback, NULL),
    IupSetCallbacks(IupSetAttributes(IupButton(NULL, NULL), "IMAGE=IUP_ZoomIn, FLAT=Yes, TIP=\"Zoom In (Ctrl++)\", CANFOCUS=No"), "ACTION", CB_ItemZoominActionCallback, NULL),
    IupSetCallbacks(IupSetAttributes(IupButton(NULL, NULL), "IMAGE=IUP_ZoomActualSize, FLAT=Yes, TIP=\"Actual Size (Ctrl+0)\", CANFOCUS=No"), "ACTION", CB_ItemActualsizeActionCallback, NULL),
    NULL);
  IupSetAttribute(statusbar, "NAME", "STATUSBAR");
  IupSetAttribute(statusbar, "ALIGNMENT", "ACENTER");

  /* Initialize variables from the configuration file */

  if (!IupConfigGetVariableIntDef(config, "MainWindow", "Statusbar", 1))
  {
    IupSetAttribute(statusbar, "FLOATING", "YES");
    IupSetAttribute(statusbar, "VISIBLE", "NO");
  }

  return statusbar;
}

void SimplePaint::CreateMainDialog(Ihandle* iup_canvas)
{
  Ihandle *vbox;

  vbox = IupVbox(
    CreateToolbar(),
    iup_canvas,
    CreateStatusbar(),
    NULL);

  dlg = IupDialog(vbox);
  IupSetAttributeHandle(dlg, "MENU", CreateMainMenu());
  IupSetAttribute(dlg, "SIZE", "HALFxHALF");
  IUP_CLASS_SETCALLBACK(dlg, "CLOSE_CB", ItemExitActionCallback);
  IUP_CLASS_SETCALLBACK(dlg, "DROPFILES_CB", DialogDropfilesCallback);
  IUP_CLASS_SETCALLBACK(dlg, "MOVE_CB", DialogMoveCallback);

  IUP_CLASS_SETCALLBACK(dlg, "K_cN", ItemNewActionCallback);
  IUP_CLASS_SETCALLBACK(dlg, "K_cO", ItemOpenActionCallback);
  IUP_CLASS_SETCALLBACK(dlg, "K_cS", ItemSaveActionCallback);
  IUP_CLASS_SETCALLBACK(dlg, "K_cV", ItemPasteActionCallback);
  IUP_CLASS_SETCALLBACK(dlg, "K_cC", ItemCopyActionCallback);
  IUP_CLASS_SETCALLBACK(dlg, "K_cP", ItemPrintActionCallback);
  IUP_CLASS_SETCALLBACK(dlg, "K_cMinus", ItemZoomoutActionCallback);
  IUP_CLASS_SETCALLBACK(dlg, "K_cPlus", ItemZoominActionCallback);
  IUP_CLASS_SETCALLBACK(dlg, "K_cEqual", ItemZoominActionCallback);
  IUP_CLASS_SETCALLBACK(dlg, "K_c0", ItemActualsizeActionCallback);

  /* parent for pre-defined dialogs in closed functions (IupMessage and IupAlarm) */
  IupSetAttributeHandle(NULL, "PARENTDIALOG", dlg);
}

SimplePaint::SimplePaint()
  :dlg(NULL), config(NULL), file(), toolbox(), canvas()
{

  /***************  Config ***********/
  config = IupConfig();
  IupSetAttribute(config, "APP_NAME", "simple_paint");
  IupConfigLoad(config);


  /***************  Canvas ***********/
  Ihandle* iup_canvas = canvas.CreateCanvas(&file, &toolbox);

  /* Initialize variables from the configuration file */
  const char* background_str = IupConfigGetVariableStr(config, "Canvas", "Background");
  if (background_str)
  {
    unsigned int ri, gi, bi;
    sscanf(background_str, "%u %u %u", &ri, &gi, &bi);
    canvas.SetBackground(cdEncodeColor((unsigned char)ri, (unsigned char)gi, (unsigned char)bi));
  }

  if (!IupConfigGetVariableIntDef(config, "Canvas", "ZoomGrid", 1))
    canvas.SetZoomGrid(false);


  /***************  Dialog ***********/
  CreateMainDialog(iup_canvas);

  IUP_CLASS_INITCALLBACK(dlg, SimplePaint);

  /* show the dialog at the last position, with the last size */
  IupConfigDialogShow(config, dlg, "MainWindow");


  /***************  Toolbox ***********/
  toolbox.CreateDialog(dlg, config);

  /* Initialize variables from the configuration file */
  if (IupConfigGetVariableIntDef(config, "MainWindow", "Toolbox", 1))
  {
    /* configure the very first time to be aligned with the canvas top left corner */
    if (!IupConfigGetVariableStr(config, "Toolbox", "X"))
    {
      int x = IupGetInt(iup_canvas, "X");
      int y = IupGetInt(iup_canvas, "Y");
      IupConfigSetVariableInt(config, "Toolbox", "X", x);
      IupConfigSetVariableInt(config, "Toolbox", "Y", y);
    }

    toolbox.ShowDialog();
  }
}
