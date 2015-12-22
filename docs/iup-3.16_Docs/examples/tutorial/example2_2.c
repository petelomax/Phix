#include <stdlib.h>
#include <iup.h>

int main(int argc, char **argv)
{
  Ihandle *dlg, *label;

  IupOpen(&argc, &argv);

  label =  IupLabel("Hello world from IUP.");
  dlg = IupDialog(IupVbox(label, NULL));
  IupSetAttribute(dlg, "TITLE", "Hello World 2");

  IupShowXY(dlg, IUP_CENTER, IUP_CENTER);

  IupMainLoop();

  IupClose();
  return EXIT_SUCCESS;
}
