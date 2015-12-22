#include <stdlib.h>
#include <iup.h>

int exit_cb(void)
{
  return IUP_CLOSE;
}

int main(int argc, char **argv)
{
  Ihandle *dlg, *multitext, *vbox;
  Ihandle *file_menu, *item_exit, *item_open, *item_saveas;
  Ihandle *sub1_menu, *menu;

  IupOpen(&argc, &argv);

  multitext = IupText(NULL);
  IupSetAttribute(multitext, "MULTILINE", "YES");
  IupSetAttribute(multitext, "EXPAND", "YES");

  item_open = IupItem("Open", NULL);
  item_saveas = IupItem("Save As", NULL);
  item_exit = IupItem("Exit", NULL);
  IupSetCallback(item_exit, "ACTION", (Icallback)exit_cb);

  file_menu = IupMenu(
    item_open,
    item_saveas,
    IupSeparator(),
    item_exit,
    NULL);

  sub1_menu = IupSubmenu("File", file_menu);

  menu = IupMenu(sub1_menu, NULL);

  vbox = IupVbox(
    multitext,
    NULL);

  dlg = IupDialog(vbox);
  IupSetAttributeHandle(dlg, "MENU", menu);
  IupSetAttribute(dlg, "TITLE", "Simple Notepad");
  IupSetAttribute(dlg, "SIZE", "QUARTERxQUARTER");

  IupShowXY(dlg, IUP_CENTER, IUP_CENTER);
  IupSetAttribute(dlg, "USERSIZE", NULL);

  IupMainLoop();

  IupClose();
  return EXIT_SUCCESS;
}
