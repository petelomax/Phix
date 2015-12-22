
/* avoid multiple inclusions */
#ifndef SIMPLE_PAINT_CANVAS_H
#define SIMPLE_PAINT_CANVAS_H

#include "simple_paint_file.h"
#include "simple_paint_toolbox.h"

#include <cd.h>


class SimplePaintCanvas
{
  Ihandle *canvas;

  cdCanvas* cd_canvas;
  SimplePaintFile *file;
  SimplePaintToolbox *toolbox;

  long background;
  bool show_zoomgrid;

  struct
  {
    bool overlay;
    bool start;
    int start_x, start_y;
    int end_x, end_y;
    int start_cursor_x, start_cursor_y;
  } interact;

public:

  SimplePaintCanvas()
    :canvas(0), cd_canvas(0), file(0), toolbox(0),
     background(cdEncodeColor(208, 208, 208)), show_zoomgrid(true)
  {
    interact.overlay = false;
    interact.start = false;
  }
  
  Ihandle* CreateCanvas(SimplePaintFile* ref_file, SimplePaintToolbox* ref_toolbox);

  void Update() { IupUpdate(canvas); }

  void Print(const char* title, int margin_width, int margin_height);

  void SetZoom(double zoom_index);
  void ZoomOut();
  void ZoomIn();

  void ScrollUpdate();

  void SetBackground(long bg)
  {
    background = bg;
    Update();
  }
  long GetBackground() const { return background; }

  void SetZoomGrid(bool show)
  {
    show_zoomgrid = show;
    Update();
  }
  bool GetZoomGrid() const { return show_zoomgrid; }

protected:

  void DrawToolOverlay(cdCanvas* cnv, int start_x, int start_y, int end_x, int end_y);
  void DrawPencil(int start_x, int start_y, int end_x, int end_y);

  void UpdateZoom(double zoom_index);

  double ViewZoomRect(int *_x, int *_y, int *_view_width, int *_view_height);

  IUP_CLASS_DECLARECALLBACK_IFn(SimplePaintCanvas, CanvasActionCallback);
  IUP_CLASS_DECLARECALLBACK_IFn(SimplePaintCanvas, CanvasMapCallback);
  IUP_CLASS_DECLARECALLBACK_IFn(SimplePaintCanvas, CanvasUnmapCallback);
  IUP_CLASS_DECLARECALLBACK_IFnfiis(SimplePaintCanvas, CanvasWheelCallback);
  IUP_CLASS_DECLARECALLBACK_IFn(SimplePaintCanvas, CanvasResizeCallback);
  IUP_CLASS_DECLARECALLBACK_IFniis(SimplePaintCanvas, CanvasMotionCallback);
  IUP_CLASS_DECLARECALLBACK_IFniiii(SimplePaintCanvas, CanvasButtonCallback);
};

#endif
