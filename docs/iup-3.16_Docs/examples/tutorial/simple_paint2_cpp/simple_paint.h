
/* avoid multiple inclusions */
#ifndef SIMPLE_PAINT_H
#define SIMPLE_PAINT_H

#include "simple_paint_canvas.h"


class SimplePaint
{
  Ihandle *dlg, *config;

  SimplePaintFile file;
  SimplePaintToolbox toolbox;
  SimplePaintCanvas canvas;

public:

  SimplePaint();

  void CheckNewFile();

  void OpenFile(const char* filename);

protected:

  void CreateMainDialog(Ihandle* iup_canvas);
  Ihandle* CreateStatusbar();
  Ihandle* CreateToolbar();
  Ihandle* CreateMainMenu();

  void SelectFile(bool is_open);

  void UpdateFile();
  void UpdateImage(imImage* new_image, bool update_size = false);

  void ToggleBarVisibility(Ihandle* item, Ihandle* bar);

  IUP_CLASS_DECLARECALLBACK_IFnii(SimplePaint, DialogMoveCallback);
  IUP_CLASS_DECLARECALLBACK_IFns(SimplePaint, DialogDropfilesCallback);

  IUP_CLASS_DECLARECALLBACK_IFn(SimplePaint, ConfigRecentCallback);

  IUP_CLASS_DECLARECALLBACK_IFn(SimplePaint, ItemNewActionCallback);
  IUP_CLASS_DECLARECALLBACK_IFn(SimplePaint, ItemOpenActionCallback);
  IUP_CLASS_DECLARECALLBACK_IFn(SimplePaint, ItemSaveActionCallback);
  IUP_CLASS_DECLARECALLBACK_IFn(SimplePaint, ItemSaveasActionCallback);
  IUP_CLASS_DECLARECALLBACK_IFn(SimplePaint, ItemRevertActionCallback);
  IUP_CLASS_DECLARECALLBACK_IFn(SimplePaint, ItemPagesetupActionCallback);
  IUP_CLASS_DECLARECALLBACK_IFn(SimplePaint, ItemPrintActionCallback);
  IUP_CLASS_DECLARECALLBACK_IFn(SimplePaint, ItemExitActionCallback);
  IUP_CLASS_DECLARECALLBACK_IFn(SimplePaint, ItemCopyActionCallback);
  IUP_CLASS_DECLARECALLBACK_IFn(SimplePaint, ItemPasteActionCallback);
  IUP_CLASS_DECLARECALLBACK_IFn(SimplePaint, ItemZoominActionCallback);
  IUP_CLASS_DECLARECALLBACK_IFn(SimplePaint, ItemZoomoutActionCallback);
  IUP_CLASS_DECLARECALLBACK_IFn(SimplePaint, ItemActualsizeActionCallback);
  IUP_CLASS_DECLARECALLBACK_IFn(SimplePaint, ItemZoomgridActionCallback);
  IUP_CLASS_DECLARECALLBACK_IFn(SimplePaint, ItemBackgroundActionCallback);
  IUP_CLASS_DECLARECALLBACK_IFn(SimplePaint, ItemToolbarActionCallback);
  IUP_CLASS_DECLARECALLBACK_IFn(SimplePaint, ItemToolboxActionCallback);
  IUP_CLASS_DECLARECALLBACK_IFn(SimplePaint, ItemHelpActionCallback);
  IUP_CLASS_DECLARECALLBACK_IFn(SimplePaint, ItemAboutActionCallback);
  IUP_CLASS_DECLARECALLBACK_IFn(SimplePaint, ItemStatusbarActionCallback);
  IUP_CLASS_DECLARECALLBACK_IFn(SimplePaint, ItemResizeActionCallback);
  IUP_CLASS_DECLARECALLBACK_IFn(SimplePaint, ItemMirrorActionCallback);
  IUP_CLASS_DECLARECALLBACK_IFn(SimplePaint, ItemFlipActionCallback);
  IUP_CLASS_DECLARECALLBACK_IFn(SimplePaint, ItemRotate180ActionCallback);
  IUP_CLASS_DECLARECALLBACK_IFn(SimplePaint, ItemRotate90cwActionCallback);
  IUP_CLASS_DECLARECALLBACK_IFn(SimplePaint, ItemRotate90ccwActionCallback);
  IUP_CLASS_DECLARECALLBACK_IFn(SimplePaint, ItemNegativeActionCallback);
  IUP_CLASS_DECLARECALLBACK_IFn(SimplePaint, ItemBrightcontActionCallback);

  IUP_CLASS_DECLARECALLBACK_IFn(SimplePaint, FileMenuOpenCallback);
  IUP_CLASS_DECLARECALLBACK_IFn(SimplePaint, EditMenuOpenCallback);
  IUP_CLASS_DECLARECALLBACK_IFn(SimplePaint, ViewMenuOpenCallback);

  IUP_CLASS_DECLARECALLBACK_IFn(SimplePaint, ZoomValueChangedCallback);
};

#endif
