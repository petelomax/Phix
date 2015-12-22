
#include "simple_paint_canvas.h"
#include "simple_paint_util.h"

#include <string.h>
#include <math.h>

#include <cdirgb.h>
#include <cdiup.h>
#include <cdprint.h>
#include <cdim.h>


//#define USE_OPENGL 1
//#define USE_CONTEXTPLUS 1

#ifdef USE_OPENGL
#include <iupgl.h>
#include <cdgl.h>
#endif

static double iround(double x)
{
  return (int)(x>0 ? x + 0.5 : x - 0.5);
}

static int iabs(int x)
{
  return x<0? -x: x;
}


/*********************************** Utilities **************************************/


/* extracted from the SCROLLBAR attribute documentation */
static void scroll_update(Ihandle* ih, int view_width, int view_height)
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

static void scroll_calc_center(Ihandle* ih, float *x, float *y)
{
  *x = IupGetFloat(ih, "POSX") + IupGetFloat(ih, "DX") / 2.0f;
  *y = IupGetFloat(ih, "POSY") + IupGetFloat(ih, "DY") / 2.0f;
}

static void scroll_center(Ihandle* ih, float old_center_x, float old_center_y)
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

static void scroll_move(Ihandle* ih, int canvas_width, int canvas_height, int move_x, int move_y, int view_width, int view_height)
{
  float posy = 0;
  float posx = 0;

  if (move_x == 0 && move_y == 0)
    return;

  if (canvas_height < view_height)
  {
    posy = IupGetFloat(ih, "POSY");
    posy -= (float)move_y / (float)view_height;
  }

  if (canvas_width < view_width)
  {
    posx = IupGetFloat(ih, "POSX");
    posx -= (float)move_x / (float)view_width;
  }

  if (posx != 0 || posy != 0)
  {
    IupSetFloat(ih, "POSX", posx);
    IupSetFloat(ih, "POSY", posy);
    IupUpdate(ih);
  }
}

static void view_fit_rect(int canvas_width, int canvas_height, int image_width, int image_height, int *view_width, int *view_height)
{
  *view_width = canvas_width;
  *view_height = (canvas_width * image_height) / image_width;

  if (*view_height > canvas_height)
  {
    *view_height = canvas_height;
    *view_width = (canvas_height * image_width) / image_height;
  }
}

static void view_zoom_offset(int view_x, int view_y, int image_width, int image_height, double zoom_factor, int *x, int *y)
{
  *x -= view_x;
  *y -= view_y;

  *x = (int)(*x / zoom_factor);
  *y = (int)(*y / zoom_factor);

  if (*x < 0) *x = 0;
  if (*y < 0) *y = 0;
  if (*x > image_width - 1) *x = image_width - 1;
  if (*y > image_height - 1) *y = image_height - 1;
}


/*********************************** Utilities Methods **************************************/

void SimplePaintCanvas::ZoomIn()
{
  Ihandle* zoom_val = IupGetDialogChild(canvas, "ZOOMVAL");
  double zoom_index = IupGetDouble(zoom_val, "VALUE");
  zoom_index++;
  if (zoom_index > 6)
    zoom_index = 6;
  IupSetDouble(zoom_val, "VALUE", iround(zoom_index));  /* fixed increments when using buttons */

  UpdateZoom(zoom_index);
}

void SimplePaintCanvas::ZoomOut()
{
  Ihandle* zoom_val = IupGetDialogChild(canvas, "ZOOMVAL");
  double zoom_index = IupGetDouble(zoom_val, "VALUE");
  zoom_index--;
  if (zoom_index < -6)
    zoom_index = -6;
  IupSetDouble(zoom_val, "VALUE", iround(zoom_index));  /* fixed increments when using buttons */

  UpdateZoom(zoom_index);
}

void SimplePaintCanvas::SetZoom(double zoom_index)
{
  Ihandle* zoom_val = IupGetDialogChild(canvas, "ZOOMVAL");
  IupSetDouble(zoom_val, "VALUE", zoom_index);

  UpdateZoom(zoom_index);
}

void SimplePaintCanvas::UpdateZoom(double zoom_index)
{
  double zoom_factor = pow(2, zoom_index);
  Ihandle* zoom_lbl = IupGetDialogChild(canvas, "ZOOMLABEL");
  IupSetStrf(zoom_lbl, "TITLE", "%.0f%%", floor(zoom_factor * 100));

  if (file->GetImage())
    ScrollUpdate();

  Update();
}

double SimplePaintCanvas::ViewZoomRect(int *_x, int *_y, int *_view_width, int *_view_height)
{
  int x, y, canvas_width, canvas_height;
  int view_width, view_height;
  Ihandle* zoom_val = IupGetDialogChild(canvas, "ZOOMVAL");
  double zoom_index = IupGetDouble(zoom_val, "VALUE");
  double zoom_factor = pow(2, zoom_index);

  float posy = IupGetFloat(canvas, "POSY");
  float posx = IupGetFloat(canvas, "POSX");

  IupGetIntInt(canvas, "DRAWSIZE", &canvas_width, &canvas_height);

  view_width = (int)(zoom_factor * file->GetImage()->width);
  view_height = (int)(zoom_factor * file->GetImage()->height);

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

  *_x = x;
  *_y = y;
  *_view_width = view_width;
  *_view_height = view_height;

  return zoom_factor;
}

void SimplePaintCanvas::DrawPencil(int start_x, int start_y, int end_x, int end_y)
{
  double res = IupGetDouble(NULL, "SCREENDPI") / 25.4;

  int line_width = toolbox->LineWidth();
  long color = toolbox->Color();

  /* do not use line style here */
  cdCanvas* rgb_canvas = cdCreateCanvas(CD_IMIMAGE, (imImage*)file->GetImage());
  cdCanvasSetfAttribute(rgb_canvas, "RESOLUTION", "%g", res);
  cdCanvasForeground(rgb_canvas, color);
  cdCanvasLineWidth(rgb_canvas, line_width);
  cdCanvasLine(rgb_canvas, start_x, start_y, end_x, end_y);
  cdKillCanvas(rgb_canvas);
}

void SimplePaintCanvas::DrawToolOverlay(cdCanvas* cnv, int start_x, int start_y, int end_x, int end_y)
{
  SimplePaintToolbox::Tool tool_index = toolbox->ToolIndex();
  int line_width = toolbox->LineWidth();
  int line_style = toolbox->LineStyle();
  long color = toolbox->Color();

  cdCanvasForeground(cnv, color);
  cdCanvasLineWidth(cnv, line_width);
  if (line_width == 1)
    cdCanvasLineStyle(cnv, line_style);

  if (tool_index == SimplePaintToolbox::TOOL_LINE)
    cdCanvasLine(cnv, start_x, start_y, end_x, end_y);
  else if (tool_index == SimplePaintToolbox::TOOL_RECT)
    cdCanvasRect(cnv, start_x, end_x, start_y, end_y);
  else if (tool_index == SimplePaintToolbox::TOOL_BOX)
    cdCanvasBox(cnv, start_x, end_x, start_y, end_y);
  else if (tool_index == SimplePaintToolbox::TOOL_ELLIPSE)
    cdCanvasArc(cnv, (end_x + start_x) / 2, (end_y + start_y) / 2, iabs(end_x - start_x), iabs(end_y - start_y), 0, 360);
  else if (tool_index == SimplePaintToolbox::TOOL_OVAL)
    cdCanvasSector(cnv, (end_x + start_x) / 2, (end_y + start_y) / 2, iabs(end_x - start_x), iabs(end_y - start_y), 0, 360);
  else if (tool_index == SimplePaintToolbox::TOOL_TEXT)
  {
    cdCanvasTextAlignment(cnv, CD_SOUTH_WEST);
    cdCanvasNativeFont(cnv, toolbox->Font());
    cdCanvasText(cnv, end_x, end_y, toolbox->Text());
  }
}

void SimplePaintCanvas::ScrollUpdate()
{
  Ihandle* zoom_val = IupGetDialogChild(canvas, "ZOOMVAL");
  double zoom_index = IupGetDouble(zoom_val, "VALUE");
  double zoom_factor = pow(2, zoom_index);

  float old_center_x, old_center_y;
  int view_width = (int)(zoom_factor * file->GetImage()->width);
  int view_height = (int)(zoom_factor * file->GetImage()->height);

  scroll_calc_center(canvas, &old_center_x, &old_center_y);

  scroll_update(canvas, view_width, view_height);

  scroll_center(canvas, old_center_x, old_center_y);
}

void SimplePaintCanvas::Print(const char* title, int margin_width, int margin_height)
{
  cdCanvas* print_canvas = cdCreateCanvasf(CD_PRINTER, "%s -d", title);
  if (!print_canvas)
    return;

  /* do NOT draw the background, use the paper color */

  /* draw the image at the center of the canvas */
  if (file->GetImage())
  {
    int x, y, canvas_width, canvas_height, view_width, view_height;
    double canvas_width_mm, canvas_height_mm;

    cdCanvasGetSize(print_canvas, &canvas_width, &canvas_height, &canvas_width_mm, &canvas_height_mm);

    /* convert to pixels */
    margin_width = (int)((margin_width * canvas_width) / canvas_width_mm);
    margin_height = (int)((margin_height * canvas_height) / canvas_height_mm);

    view_fit_rect(canvas_width - 2 * margin_width, canvas_height - 2 * margin_height,
                  file->GetImage()->width, file->GetImage()->height,
                  &view_width, &view_height);

    x = (canvas_width - view_width) / 2;
    y = (canvas_height - view_height) / 2;

    cdCanvasPutImImage(print_canvas, file->GetImage(), x, y, view_width, view_height);
  }

  cdKillCanvas(print_canvas);
}


/********************************** Callbacks Methods *****************************************/


int SimplePaintCanvas::CanvasActionCallback(Ihandle*)
{
#ifdef USE_OPENGL
  IupGLMakeCurrent(canvas);
#endif
  cdCanvasActivate(cd_canvas);

  /* draw the background */
  cdCanvasBackground(cd_canvas, background);
  cdCanvasClear(cd_canvas);

  /* draw the image at the center of the canvas */
  if (file->GetImage())
  {
    int x, y, view_width, view_height;
    ViewZoomRect(&x, &y, &view_width, &view_height);

    /* black line around the image */
    cdCanvasForeground(cd_canvas, CD_BLACK);
    cdCanvasLineWidth(cd_canvas, 1);
    cdCanvasLineStyle(cd_canvas, CD_CONTINUOUS);
    cdCanvasRect(cd_canvas, x - 1, x + view_width, y - 1, y + view_height);

    /* Some CD drivers have interpolation options for image zoom */
    /* we force NEAREST so we can see the pixel boundary in zoom in */
    /* an alternative would be to set BILINEAR when zoom out */
    cdCanvasSetAttribute(cd_canvas, "IMGINTERP", (char*)"NEAREST");  /* affects only drivers that have this attribute */
    cdCanvasPutImImage(cd_canvas, file->GetImage(), x, y, view_width, view_height);

    if (show_zoomgrid)
    {
      Ihandle* zoom_val = IupGetDialogChild(canvas, "ZOOMVAL");
      double zoom_index = IupGetDouble(zoom_val, "VALUE");
      if (zoom_index > 1)
      {
        int ix, iy;
        double zoom_factor = pow(2, zoom_index);

        cdCanvasForeground(cd_canvas, CD_GRAY);

        for (ix = 0; ix < file->GetImage()->width; ix++)
        {
          int gx = (int)(ix * zoom_factor);
          cdCanvasLine(cd_canvas, gx + x, y, gx + x, y + view_height);
        }
        for (iy = 0; iy < file->GetImage()->height; iy++)
        {
          int gy = (int)(iy * zoom_factor);
          cdCanvasLine(cd_canvas, x, gy + y, x + view_width, gy + y);
        }
      }
    }

    if (interact.overlay)
    {
      double scale_x = (double)view_width / (double)file->GetImage()->width;
      double scale_y = (double)view_height / (double)file->GetImage()->height;

      /* offset and scale drawing in screen to match the image */
      if (scale_x > 1 || scale_y > 1)
        cdCanvasTransformTranslate(cd_canvas, x + scale_x / 2, y + scale_y / 2);  /* also draw at the center of the pixel when zoom in */
      else
        cdCanvasTransformTranslate(cd_canvas, x, y);
      cdCanvasTransformScale(cd_canvas, scale_x, scale_y);

      DrawToolOverlay(cd_canvas, interact.start_x, interact.start_y, interact.end_x, interact.end_y);

      cdCanvasTransform(cd_canvas, NULL);
    }
  }

  cdCanvasFlush(cd_canvas);

#ifdef USE_OPENGL
  IupGLSwapBuffers(canvas);
#endif
  return IUP_DEFAULT;
}

int SimplePaintCanvas::CanvasMapCallback(Ihandle* canvas)
{
#ifdef USE_OPENGL
  double res = IupGetDouble(NULL, "SCREENDPI") / 25.4;
  IupGLMakeCurrent(canvas);
  cd_canvas = cdCreateCanvasf(CD_GL, "10x10 %g", res);
#else
#ifdef USE_CONTEXTPLUS
  cdUseContextPlus(1);
#endif
  cd_canvas = cdCreateCanvas(CD_IUPDBUFFER, canvas);
#ifdef USE_CONTEXTPLUS
  cdUseContextPlus(0);
#endif
#endif
  return IUP_DEFAULT;
}

int SimplePaintCanvas::CanvasUnmapCallback(Ihandle*)
{
  cdKillCanvas(cd_canvas);
  return IUP_DEFAULT;
}

int SimplePaintCanvas::CanvasResizeCallback(Ihandle*)
{
  if (file->GetImage())
    ScrollUpdate();

#ifdef USE_OPENGL
  {
    int canvas_width, canvas_height;
    double res = IupGetDouble(NULL, "SCREENDPI") / 25.4;
    IupGetIntInt(canvas, "DRAWSIZE", &canvas_width, &canvas_height);

    IupGLMakeCurrent(canvas);
    cdCanvasSetfAttribute(cd_canvas, "SIZE", "%dx%d %g", canvas_width, canvas_height, res);
  }
#endif
  return IUP_DEFAULT;
}

int SimplePaintCanvas::CanvasWheelCallback(Ihandle* canvas, float delta, int, int, char*)
{
  if (IupGetInt(NULL, "CONTROLKEY"))
  {
    if (delta < 0)
      ZoomOut();
    else
      ZoomIn();
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

int SimplePaintCanvas::CanvasButtonCallback(Ihandle* canvas, int button, int pressed, int x, int y)
{
  if (file->GetImage())
  {
    int cursor_x = x, cursor_y = y;
    int view_x, view_y, view_width, view_height;
    double zoom_factor = ViewZoomRect(&view_x, &view_y, &view_width, &view_height);

    /* y is top-down in IUP */
    int canvas_height = IupGetInt2(canvas, "DRAWSIZE");
    y = canvas_height - y - 1;

    /* inside image area */
    if (x > view_x && y > view_y && x < view_x + view_width && y < view_y + view_height)
    {
      view_zoom_offset(view_x, view_y, file->GetImage()->width, file->GetImage()->height, zoom_factor, &x, &y);

      if (button == IUP_BUTTON1)
      {
        if (pressed)
        {
          interact.start = true;
          interact.start_x = x;
          interact.start_y = y;
          interact.start_cursor_x = cursor_x;
          interact.start_cursor_y = cursor_y;
        }
        else if (interact.start)
        {
          SimplePaintToolbox::Tool tool_index = toolbox->ToolIndex();

          if (tool_index == SimplePaintToolbox::TOOL_COLORPICKER)
          {
            // must click and release on the same pixel
            if (interact.start_x == x && interact.start_y == y)
            {
              unsigned char** data = (unsigned char**)file->GetImage()->data;
              unsigned char r, g, b;
              int offset;

              offset = y * file->GetImage()->width + x;
              r = data[0][offset];
              g = data[1][offset];
              b = data[2][offset];

              toolbox->SetColor(cdEncodeColor(r, g, b));
            }
          }
          else if (tool_index == SimplePaintToolbox::TOOL_PENCIL)
          {
            DrawPencil(interact.start_x, interact.start_y, x, y);

            file->SetImageChanged();
            IupUpdate(canvas);

            interact.start_x = x;
            interact.start_y = y;
          }
          else if (tool_index >= SimplePaintToolbox::TOOL_LINE && tool_index <= SimplePaintToolbox::TOOL_TEXT)  /* All Shapes */
          {
            if (interact.overlay)
            {
              double res = IupGetDouble(NULL, "SCREENDPI") / 25.4;
  
              cdCanvas* rgb_canvas = cdCreateCanvas(CD_IMIMAGE, (imImage*)file->GetImage());
              cdCanvasSetfAttribute(rgb_canvas, "RESOLUTION", "%g", res);

              DrawToolOverlay(rgb_canvas, interact.start_x, interact.start_y, x, y);

              cdKillCanvas(rgb_canvas);

              interact.overlay = false;

              file->SetImageChanged();
              IupUpdate(canvas);
            }
          }
          else if (tool_index == SimplePaintToolbox::TOOL_FILLCOLOR)
          {
            // must click and release on the same pixel
            if (interact.start_x == x && interact.start_y == y)
            {
              double tol_percent = toolbox->FillTol();
              long color = toolbox->Color();

              image_flood_fill((imImage*)file->GetImage(), x, y, color, tol_percent);

              file->SetImageChanged();
              IupUpdate(canvas);
            }
          }

          interact.start = false;
        }
      }
      else if (button == IUP_BUTTON3)  /* right click */
      {
        // attention: can not use start_* here because they are initialized only for button1
        if (!pressed)
        {
          SimplePaintToolbox::Tool tool_index = toolbox->ToolIndex();
          if (tool_index == SimplePaintToolbox::TOOL_TEXT)
            toolbox->ToolGetText();
        }
      }
    }
  }

  return IUP_DEFAULT;
}

int SimplePaintCanvas::CanvasMotionCallback(Ihandle* canvas, int x, int y, char *status)
{
  SimplePaintToolbox::Tool tool_index = toolbox->ToolIndex();
  if (tool_index == SimplePaintToolbox::TOOL_POINTER)
  {
    char* cursor = IupGetAttribute(canvas, "CURSOR");
    if (strcmp(cursor, "ARROW") != 0)
      IupSetAttribute(canvas, "CURSOR", "ARROW");
  }
  else
  {
    char* cursor = IupGetAttribute(canvas, "CURSOR");
    if (strcmp(cursor, "CROSS") != 0)
      IupSetAttribute(canvas, "CURSOR", "CROSS");
  }

  if (file->GetImage())
  {
    int cursor_x = x, cursor_y = y;
    int view_x, view_y, view_width, view_height;
    double zoom_factor = ViewZoomRect(&view_x, &view_y, &view_width, &view_height);

    /* y is top-down in IUP */
    int canvas_height = IupGetInt2(canvas, "DRAWSIZE");
    y = canvas_height - y - 1;

    /* inside image area */
    if (x > view_x && y > view_y && x < view_x + view_width && y < view_y + view_height)
    {
      Ihandle* status_lbl = IupGetDialogChild(canvas, "STATUSLABEL");
      unsigned char** data = (unsigned char**)file->GetImage()->data;
      unsigned char r, g, b;
      int offset;

      view_zoom_offset(view_x, view_y, file->GetImage()->width, file->GetImage()->height, zoom_factor, &x, &y);

      offset = y * file->GetImage()->width + x; 
      r = data[0][offset];
      g = data[1][offset];
      b = data[2][offset];

      IupSetStrf(status_lbl, "TITLE", "(%4d, %4d) = %3d %3d %3d", x, y, (int)r, (int)g, (int)b);

      if (iup_isbutton1(status) && interact.start) /* button1 is pressed */
      {
        if (tool_index == SimplePaintToolbox::TOOL_POINTER)
        {
          int canvas_width = IupGetInt(canvas, "DRAWSIZE");

          scroll_move(canvas, canvas_width, canvas_height, cursor_x - interact.start_cursor_x, cursor_y - interact.start_cursor_y, view_width, view_height);

          interact.start_cursor_x = cursor_x;
          interact.start_cursor_y = cursor_y;
        }
        else if (tool_index == SimplePaintToolbox::TOOL_PENCIL)
        {
          DrawPencil(interact.start_x, interact.start_y, x, y);

          file->SetImageChanged();
          IupUpdate(canvas);

          interact.start_x = x;
          interact.start_y = y;
        }
        else if (tool_index >= SimplePaintToolbox::TOOL_LINE && tool_index <= SimplePaintToolbox::TOOL_TEXT)  /* All Shapes */
        {
          interact.end_x = x;
          interact.end_y = y;
          interact.overlay = true;

          IupUpdate(canvas);
        }
      }
    }
  }

  return IUP_DEFAULT;
}

Ihandle* SimplePaintCanvas::CreateCanvas(SimplePaintFile* ref_file, SimplePaintToolbox* ref_toolbox)
{
  file = ref_file;
  toolbox = ref_toolbox;

#ifdef USE_OPENGL
  IupGLCanvasOpen();
#endif
#ifdef USE_CONTEXTPLUS
  cdInitContextPlus();
#endif

#ifdef USE_OPENGL
  canvas = IupGLCanvas(NULL);
  IupSetAttribute(canvas, "BUFFER", "DOUBLE");
#else
  canvas = IupCanvas(NULL);
#endif
  IupSetAttribute(canvas, "SCROLLBAR", "Yes");
  IUP_CLASS_SETCALLBACK(canvas, "ACTION", CanvasActionCallback);
  IUP_CLASS_SETCALLBACK(canvas, "MAP_CB", CanvasMapCallback);
  IUP_CLASS_SETCALLBACK(canvas, "UNMAP_CB", CanvasUnmapCallback);
  IUP_CLASS_SETCALLBACK(canvas, "WHEEL_CB", CanvasWheelCallback);
  IUP_CLASS_SETCALLBACK(canvas, "RESIZE_CB", CanvasResizeCallback);
  IUP_CLASS_SETCALLBACK(canvas, "MOTION_CB", CanvasMotionCallback);
  IUP_CLASS_SETCALLBACK(canvas, "BUTTON_CB", CanvasButtonCallback);

  IUP_CLASS_INITCALLBACK(canvas, SimplePaintCanvas);

  return canvas;
}

