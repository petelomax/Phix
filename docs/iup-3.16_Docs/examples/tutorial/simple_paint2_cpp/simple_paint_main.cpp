
#include "simple_paint.h"
#include "simple_paint_util.h"

#include <stdio.h>
#include <stdlib.h>

void SimplePaintSplash(const char* argv0);

int main(int argc, char **argv)
{
  IupOpen(&argc, &argv);
  IupImageLibOpen();

  IupSetGlobal("IMAGEAUTOSCALE", "DPI");
  IupSetGlobal("IMAGESDPI", "96"); /* not necessary, this is already the default value */

  SimplePaintSplash(argv[0]);

  SimplePaint app;

  /* open a file from the command line (allow file association in Windows) */
  if (argc > 1 && argv[1])
  {
    const char* filename = argv[1];
    app.OpenFile(filename);
  }

  /* initialize the current file, if not already loaded */
  app.CheckNewFile();

  IupMainLoop();

  IupClose();
  return EXIT_SUCCESS;
}

/* Possible Enhancements:
- Save last used toolbox options in configuration file?
- Hide/show toolbox options according to selected tool
- Capture image from Camera using IM
- Undo/Redo
- Secondary color for drawing of shapes with both outline and filled at the same time
- Alpha for colors
- Area Selection
*/
