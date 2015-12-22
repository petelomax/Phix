#include <stdlib.h>
#include <iup.h>

int btn_exit_cb( Ihandle *self )
{
  /* Exits the main loop */
  return IUP_CLOSE;
}

int main(int argc, char **argv)
{
  Ihandle *dlg, *button, *label, *vbox;

  IupOpen(&argc, &argv);
  
  label =  IupLabel("Hello world from IUP.");
  IupSetAttribute(label, "PADDING", "10x20");
  button = IupButton("OK", NULL);
  IupSetAttribute(button, "PADDING", "30x2");
  
  vbox = IupVbox(
    label,
    button,
    NULL);
  IupSetAttribute(vbox, "ALIGNMENT", "ARIGHT");
  IupSetAttribute(vbox, "GAP", "10");
  IupSetAttribute(vbox, "MARGIN", "10x10");
  
  dlg = IupDialog(vbox);
  IupSetAttribute(dlg, "TITLE", "Hello World 5a");
  IupSetAttribute(dlg, "MAXBOX", "No");
  IupSetAttribute(dlg, "MINBOX", "No");

  /* Registers callbacks */
  IupSetCallback(button, "ACTION", (Icallback) btn_exit_cb);

  IupShowXY(dlg, IUP_CENTER, IUP_CENTER);

  IupMainLoop();

  IupClose();
  return EXIT_SUCCESS;
}
