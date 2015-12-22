#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <iup.h>
#include <iup_config.h>


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

int str_find(const char *str, const char *str_to_find, int casesensitive)
{
  int i, str_len, str_to_find_len, count;

  if (!str || str[0] == 0 || !str_to_find || str_to_find[0] == 0)
    return -1;

  str_len = (int)strlen(str);
  str_to_find_len = (int)strlen(str_to_find);
  count = str_len - str_to_find_len;
  if (count < 0)
    return -1;

  count++;

  for (i = 0; i<count; i++)
  {
    if (str_compare(str, str_to_find, casesensitive))
      return i;

    str++;
  }

  return -1;
}

char* read_file(const char* filename)
{
  int size;
  char* str;
  FILE* file = fopen(filename, "rb");
  if (!file)
  {
    IupMessagef("Error", "Can't open file: %s", filename);
    return NULL;
  }

  /* calculate file size */
  fseek(file, 0, SEEK_END);
  size = ftell(file);
  fseek(file, 0, SEEK_SET);

  /* allocate memory for the file contents + nul terminator */
  str = malloc(size + 1);
  /* read all data at once */
  fread(str, size, 1, file);
  /* set the nul terminator */
  str[size] = 0;

  if (ferror(file))
    IupMessagef("Error", "Fail when reading from file: %s", filename);

  fclose(file);
  return str;
}

int write_file(const char* filename, const char* str, int count)
{
  FILE* file = fopen(filename, "w");
  if (!file)
  {
    IupMessagef("Error", "Can't open file: %s", filename);
    return 0;
  }

  fwrite(str, 1, count, file);

  if (ferror(file))
    IupMessagef("Error", "Fail when writing to file: %s", filename);

  fclose(file);
  return 1;
}

void new_file(Ihandle* ih)
{
  Ihandle* dlg = IupGetDialog(ih);
  Ihandle* multitext = IupGetDialogChild(dlg, "MULTITEXT");

  IupSetAttribute(dlg, "TITLE", "Untitled - Simple Notepad");
  IupSetAttribute(multitext, "FILENAME", NULL);
  IupSetAttribute(multitext, "DIRTY", "NO");
  IupSetAttribute(multitext, "VALUE", "");
}

void open_file(Ihandle* ih, const char* filename)
{
  char* str = read_file(filename);
  if (str)
  {
    Ihandle* dlg = IupGetDialog(ih);
    Ihandle* multitext = IupGetDialogChild(dlg, "MULTITEXT");
    Ihandle* config = (Ihandle*)IupGetAttribute(multitext, "CONFIG");

    IupSetfAttribute(dlg, "TITLE", "%s - Simple Notepad", str_filetitle(filename));
    IupSetStrAttribute(multitext, "FILENAME", filename);
    IupSetAttribute(multitext, "DIRTY", "NO");
    IupSetStrAttribute(multitext, "VALUE", str);

    IupConfigRecentUpdate(config, filename);

    free(str);
  }
}

void save_file(Ihandle* multitext)
{
  char* filename = IupGetAttribute(multitext, "FILENAME");
  char* str = IupGetAttribute(multitext, "VALUE");
  int count = IupGetInt(multitext, "COUNT");
  if (write_file(filename, str, count))
    IupSetAttribute(multitext, "DIRTY", "NO");
}

void saveas_file(Ihandle* multitext, const char* filename)
{
  char* str = IupGetAttribute(multitext, "VALUE");
  int count = IupGetInt(multitext, "COUNT");
  if (write_file(filename, str, count))
  {
    Ihandle* config = (Ihandle*)IupGetAttribute(multitext, "CONFIG");
  
    IupSetfAttribute(IupGetDialog(multitext), "TITLE", "%s - Simple Notepad", str_filetitle(filename));
    IupSetStrAttribute(multitext, "FILENAME", filename);
    IupSetAttribute(multitext, "DIRTY", "NO");

    IupConfigRecentUpdate(config, filename);
  }
}

int save_check(Ihandle* ih)
{
  Ihandle* multitext = IupGetDialogChild(ih, "MULTITEXT");
  if (IupGetInt(multitext, "DIRTY"))
  {
    switch (IupAlarm("Warning", "File not saved! Save it now?", "Yes", "No", "Cancel"))
    {
    case 1:  /* save the changes and continue */
      save_file(multitext);
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


int dropfiles_cb(Ihandle* ih, const char* filename)
{
  if (save_check(ih))
    open_file(ih, filename);

  return IUP_DEFAULT;
}

int multitext_valuechanged_cb(Ihandle* multitext)
{
  IupSetAttribute(multitext, "DIRTY", "YES");
  return IUP_DEFAULT;
}

int file_menu_open_cb(Ihandle* ih)
{
  Ihandle* item_revert = IupGetDialogChild(ih, "ITEM_REVERT");
  Ihandle* item_save = IupGetDialogChild(ih, "ITEM_SAVE");
  Ihandle* multitext = IupGetDialogChild(ih, "MULTITEXT");
  char* filename = IupGetAttribute(multitext, "FILENAME");
  int dirty = IupGetInt(multitext, "DIRTY");

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
  Ihandle *item_cut = IupGetDialogChild(ih, "ITEM_CUT");
  Ihandle *item_delete = IupGetDialogChild(ih, "ITEM_DELETE");
  Ihandle *item_copy = IupGetDialogChild(ih, "ITEM_COPY");
  Ihandle* multitext = IupGetDialogChild(ih, "MULTITEXT");

  if (!IupGetInt(clipboard, "TEXTAVAILABLE"))
    IupSetAttribute(item_paste, "ACTIVE", "NO");
  else
    IupSetAttribute(item_paste, "ACTIVE", "YES");

  if (!IupGetAttribute(multitext, "SELECTEDTEXT")) 
  {
    IupSetAttribute(item_cut, "ACTIVE", "NO");
    IupSetAttribute(item_delete, "ACTIVE", "NO");
    IupSetAttribute(item_copy, "ACTIVE", "NO");
  }
  else 
  {
    IupSetAttribute(item_cut, "ACTIVE", "YES");
    IupSetAttribute(item_delete, "ACTIVE", "YES");
    IupSetAttribute(item_copy, "ACTIVE", "YES");
  }

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

int multitext_caret_cb(Ihandle *ih, int lin, int col)
{
  Ihandle *lbl_statusbar = IupGetDialogChild(ih, "STATUSBAR");
  IupSetfAttribute(lbl_statusbar, "TITLE", "Lin %d, Col %d", lin, col);
  return IUP_DEFAULT;
}

int item_new_action_cb(Ihandle* item_new)
{
  if (save_check(item_new))
    new_file(item_new);

  return IUP_DEFAULT;
}

int item_open_action_cb(Ihandle* item_open)
{
  Ihandle *filedlg;

  if (!save_check(item_open))
    return IUP_DEFAULT;

  filedlg = IupFileDlg();
  IupSetAttribute(filedlg, "DIALOGTYPE", "OPEN");
  IupSetAttribute(filedlg, "EXTFILTER", "Text Files|*.txt|All Files|*.*|");
  IupSetAttributeHandle(filedlg, "PARENTDIALOG", IupGetDialog(item_open));

  IupPopup(filedlg, IUP_CENTERPARENT, IUP_CENTERPARENT);
  if (IupGetInt(filedlg, "STATUS") != -1)
  {
    char* filename = IupGetAttribute(filedlg, "VALUE");
    open_file(item_open, filename);
  }

  IupDestroy(filedlg);
  return IUP_DEFAULT;
}

int item_saveas_action_cb(Ihandle* item_saveas)
{
  Ihandle* multitext = IupGetDialogChild(item_saveas, "MULTITEXT");
  Ihandle *filedlg = IupFileDlg();
  IupSetAttribute(filedlg, "DIALOGTYPE", "SAVE");
  IupSetAttribute(filedlg, "EXTFILTER", "Text Files|*.txt|All Files|*.*|");
  IupSetAttributeHandle(filedlg, "PARENTDIALOG", IupGetDialog(item_saveas));
  IupSetStrAttribute(filedlg, "FILE", IupGetAttribute(multitext, "FILENAME"));

  IupPopup(filedlg, IUP_CENTERPARENT, IUP_CENTERPARENT);

  if (IupGetInt(filedlg, "STATUS") != -1)
  {
    char* filename = IupGetAttribute(filedlg, "VALUE");
    saveas_file(multitext, filename);
  }

  IupDestroy(filedlg);
  return IUP_DEFAULT;
}

int item_save_action_cb(Ihandle* item_save)
{
  Ihandle* multitext = IupGetDialogChild(item_save, "MULTITEXT");
  char* filename = IupGetAttribute(multitext, "FILENAME");
  if (!filename)
    item_saveas_action_cb(item_save);
  else
    save_file(multitext);
  return IUP_DEFAULT;
}

int item_revert_action_cb(Ihandle* item_revert)
{
  Ihandle* multitext = IupGetDialogChild(item_revert, "MULTITEXT");
  char* filename = IupGetAttribute(multitext, "FILENAME");
  open_file(item_revert, filename);
  return IUP_DEFAULT;
}

int item_exit_action_cb(Ihandle* item_exit)
{
  Ihandle* dlg = IupGetDialog(item_exit);
  Ihandle* config = (Ihandle*)IupGetAttribute(dlg, "CONFIG");

  if (!save_check(item_exit))
    return IUP_IGNORE;  /* to abort the CLOSE_CB callback */

  IupConfigDialogClosed(config, dlg, "MainWindow");
  IupConfigSave(config);
  IupDestroy(config);
  return IUP_CLOSE;
}

int goto_ok_action_cb(Ihandle* bt_ok)
{
  int line_count = IupGetInt(bt_ok, "TEXT_LINECOUNT");
  Ihandle* txt = IupGetDialogChild(bt_ok, "LINE_TEXT");
  int line = IupGetInt(txt, "VALUE");
  if (line < 1 || line >= line_count)
  {
    IupMessage("Error", "Invalid line number.");
    return IUP_DEFAULT;
  }

  IupSetAttribute(IupGetDialog(bt_ok), "STATUS", "1");
  return IUP_CLOSE;
}

int goto_cancel_action_cb(Ihandle* bt_ok)
{
  IupSetAttribute(IupGetDialog(bt_ok), "STATUS", "0");
  return IUP_CLOSE;
}

int item_goto_action_cb(Ihandle* item_goto)
{
  Ihandle* multitext = IupGetDialogChild(item_goto, "MULTITEXT");
  Ihandle *dlg, *box, *bt_ok, *bt_cancel, *txt, *lbl;
  int line_count = IupGetInt(multitext, "LINECOUNT");

  lbl = IupLabel(NULL);
  IupSetfAttribute(lbl, "TITLE", "Line Number [1-%d]:", line_count);
  txt = IupText(NULL);
  IupSetAttribute(txt, "MASK", IUP_MASK_UINT);  /* unsigned integer numbers only */
  IupSetAttribute(txt, "NAME", "LINE_TEXT");
  IupSetAttribute(txt, "VISIBLECOLUMNS", "20");
  bt_ok = IupButton("OK", NULL);
  IupSetInt(bt_ok, "TEXT_LINECOUNT", line_count);
  IupSetAttribute(bt_ok, "PADDING", "10x2");
  IupSetCallback(bt_ok, "ACTION", (Icallback)goto_ok_action_cb);
  bt_cancel = IupButton("Cancel", NULL);
  IupSetCallback(bt_cancel, "ACTION", (Icallback)goto_cancel_action_cb);
  IupSetAttribute(bt_cancel, "PADDING", "10x2");

  box = IupVbox(
    lbl,
    txt,
    IupSetAttributes(IupHbox(
      IupFill(),
      bt_ok,
      bt_cancel,
      NULL), "NORMALIZESIZE=HORIZONTAL"),
    NULL);
  IupSetAttribute(box, "MARGIN", "10x10");
  IupSetAttribute(box, "GAP", "5");

  dlg = IupDialog(box);
  IupSetAttribute(dlg, "TITLE", "Go To Line");
  IupSetAttribute(dlg, "DIALOGFRAME", "Yes");
  IupSetAttributeHandle(dlg, "DEFAULTENTER", bt_ok);
  IupSetAttributeHandle(dlg, "DEFAULTESC", bt_cancel);
  IupSetAttributeHandle(dlg, "PARENTDIALOG", IupGetDialog(item_goto));

  IupPopup(dlg, IUP_CENTERPARENT, IUP_CENTERPARENT);

  if (IupGetInt(dlg, "STATUS") == 1)
  {
    int line = IupGetInt(txt, "VALUE");
    int pos;
    IupTextConvertLinColToPos(multitext, line, 0, &pos);
    IupSetInt(multitext, "CARETPOS", pos);
    IupSetInt(multitext, "SCROLLTOPOS", pos);
  }

  IupDestroy(dlg);

  return IUP_DEFAULT;
}

int find_next_action_cb(Ihandle* bt_next)
{
  Ihandle* multitext = (Ihandle*)IupGetAttribute(bt_next, "MULTITEXT");
  char* str = IupGetAttribute(multitext, "VALUE");
  int find_pos = IupGetInt(multitext, "FIND_POS");

  Ihandle* txt = IupGetDialogChild(bt_next, "FIND_TEXT");
  char* str_to_find = IupGetAttribute(txt, "VALUE");

  Ihandle* find_case = IupGetDialogChild(bt_next, "FIND_CASE");
  int casesensitive = IupGetInt(find_case, "VALUE");

  int pos = str_find(str + find_pos, str_to_find, casesensitive);
  if (pos >= 0)
    pos += find_pos;
  else if (find_pos > 0)
    pos = str_find(str, str_to_find, casesensitive);  /* try again from the start */

  if (pos >= 0)
  {
    int lin, col, 
      end_pos = pos + (int)strlen(str_to_find);

    IupSetInt(multitext, "FIND_POS", end_pos);

    IupSetFocus(multitext);
    IupSetfAttribute(multitext, "SELECTIONPOS", "%d:%d", pos, end_pos);

    IupTextConvertPosToLinCol(multitext, pos, &lin, &col);
    IupTextConvertLinColToPos(multitext, lin, 0, &pos);  /* position at col=0, just scroll lines */
    IupSetInt(multitext, "SCROLLTOPOS", pos);
  }
  else
    IupMessage("Warning", "Text not found.");

  return IUP_DEFAULT;
}

int find_close_action_cb(Ihandle* bt_close)
{
  IupHide(IupGetDialog(bt_close));
  return IUP_DEFAULT;
}

int item_find_action_cb(Ihandle* item_find)
{
  Ihandle* dlg = (Ihandle*)IupGetAttribute(item_find, "FIND_DIALOG");
  if (!dlg)
  {
    Ihandle* multitext = IupGetDialogChild(item_find, "MULTITEXT");
    Ihandle *box, *bt_next, *bt_close, *txt, *find_case;

    txt = IupText(NULL);
    IupSetAttribute(txt, "NAME", "FIND_TEXT");
    IupSetAttribute(txt, "VISIBLECOLUMNS", "20");
    find_case = IupToggle("Case Sensitive", NULL);
    IupSetAttribute(find_case, "NAME", "FIND_CASE");
    bt_next = IupButton("Find Next", NULL);
    IupSetAttribute(bt_next, "PADDING", "10x2");
    IupSetCallback(bt_next, "ACTION", (Icallback)find_next_action_cb);
    bt_close = IupButton("Close", NULL);
    IupSetCallback(bt_close, "ACTION", (Icallback)find_close_action_cb);
    IupSetAttribute(bt_close, "PADDING", "10x2");

    box = IupVbox(
      IupLabel("Find What:"),
      txt,
      find_case,
      IupSetAttributes(IupHbox(
        IupFill(),
        bt_next,
        bt_close,
        NULL), "NORMALIZESIZE=HORIZONTAL"),
      NULL);
    IupSetAttribute(box, "MARGIN", "10x10");
    IupSetAttribute(box, "GAP", "5");

    dlg = IupDialog(box);
    IupSetAttribute(dlg, "TITLE", "Find");
    IupSetAttribute(dlg, "DIALOGFRAME", "Yes");
    IupSetAttributeHandle(dlg, "DEFAULTENTER", bt_next);
    IupSetAttributeHandle(dlg, "DEFAULTESC", bt_close);
    IupSetAttributeHandle(dlg, "PARENTDIALOG", IupGetDialog(item_find));

    /* Save the multiline to acess it from the callbacks */
    IupSetAttribute(dlg, "MULTITEXT", (char*)multitext);

    /* Save the dialog to reuse it */
    IupSetAttribute(item_find, "FIND_DIALOG", (char*)dlg);
  }

  /* centerparent first time, next time reuse the last position */
  IupShowXY(dlg, IUP_CURRENT, IUP_CURRENT);

  return IUP_DEFAULT;
}

int item_copy_action_cb(Ihandle* item_copy) 
{
  Ihandle* multitext = IupGetDialogChild(item_copy, "MULTITEXT");
  Ihandle *clipboard = IupClipboard();
  IupSetAttribute(clipboard, "TEXT", IupGetAttribute(multitext, "SELECTEDTEXT"));
  IupDestroy(clipboard);
  return IUP_DEFAULT;
}

int item_paste_action_cb(Ihandle* item_paste) 
{
  Ihandle* multitext = IupGetDialogChild(item_paste, "MULTITEXT");
  Ihandle *clipboard = IupClipboard();
  IupSetAttribute(multitext, "INSERT", IupGetAttribute(clipboard, "TEXT"));
  IupDestroy(clipboard);
  return IUP_DEFAULT;
}

int item_cut_action_cb(Ihandle* item_cut) 
{
  Ihandle* multitext = IupGetDialogChild(item_cut, "MULTITEXT");
  Ihandle *clipboard = IupClipboard();
  IupSetAttribute(clipboard, "TEXT", IupGetAttribute(multitext, "SELECTEDTEXT"));
  IupSetAttribute(multitext, "SELECTEDTEXT", "");
  IupDestroy(clipboard);
  return IUP_DEFAULT;
}

int item_delete_action_cb(Ihandle* item_delete) 
{
  Ihandle* multitext = IupGetDialogChild(item_delete, "MULTITEXT");
  IupSetAttribute(multitext, "SELECTEDTEXT", "");
  return IUP_DEFAULT;
}

int item_select_all_action_cb(Ihandle* item_select_all) 
{
  Ihandle* multitext = IupGetDialogChild(item_select_all, "MULTITEXT");
  IupSetFocus(multitext);
  IupSetAttribute(multitext, "SELECTION", "ALL");
  return IUP_DEFAULT;
}

int item_font_action_cb(Ihandle* item_font)
{
  Ihandle* multitext = IupGetDialogChild(item_font, "MULTITEXT");
  Ihandle* fontdlg = IupFontDlg();
  char* font = IupGetAttribute(multitext, "FONT");
  IupSetStrAttribute(fontdlg, "VALUE", font);
  IupSetAttributeHandle(fontdlg, "PARENTDIALOG", IupGetDialog(item_font));

  IupPopup(fontdlg, IUP_CENTERPARENT, IUP_CENTERPARENT);

  if (IupGetInt(fontdlg, "STATUS") == 1)
  {
    Ihandle* config = (Ihandle*)IupGetAttribute(multitext, "CONFIG");
    font = IupGetAttribute(fontdlg, "VALUE");
    IupSetStrAttribute(multitext, "FONT", font);

    IupConfigSetVariableStr(config, "MainWindow", "Font", font);
  }

  IupDestroy(fontdlg);
  return IUP_DEFAULT;
}

int item_toolbar_action_cb(Ihandle* item_toolbar)
{
  Ihandle* multitext = IupGetDialogChild(item_toolbar, "MULTITEXT");
  Ihandle* toolbar = IupGetChild(IupGetParent(multitext), 0);
  Ihandle* config = (Ihandle*)IupGetAttribute(multitext, "CONFIG");

  toggle_bar_visibility(item_toolbar, toolbar);

  IupConfigSetVariableStr(config, "MainWindow", "Toolbar", IupGetAttribute(item_toolbar, "VALUE"));
  return IUP_DEFAULT;
}

int item_statusbar_action_cb(Ihandle* item_statusbar)
{
  Ihandle* multitext = IupGetDialogChild(item_statusbar, "MULTITEXT");
  Ihandle* statusbar = IupGetBrother(multitext);
  Ihandle* config = (Ihandle*)IupGetAttribute(multitext, "CONFIG");

  toggle_bar_visibility(item_statusbar, statusbar);

  IupConfigSetVariableStr(config, "MainWindow", "Statusbar", IupGetAttribute(item_statusbar, "VALUE"));
  return IUP_DEFAULT;
}

int item_about_action_cb(void)
{
  IupMessage("About", "   Simple Notepad\n\nAutors:\n   Gustavo Lyrio\n   Antonio Scuri");
  return IUP_DEFAULT;
}


/********************************** Main *****************************************/


int main(int argc, char **argv)
{
  Ihandle *dlg, *vbox, *multitext, *menu;
  Ihandle *sub_menu_file, *file_menu, *item_exit, *item_new, *item_open, *item_save, *item_saveas, *item_revert;
  Ihandle *sub_menu_edit, *edit_menu, *item_find, *item_goto, *item_copy, *item_paste, *item_cut, *item_delete, *item_select_all;
  Ihandle *btn_cut, *btn_copy, *btn_paste, *btn_find, *btn_new, *btn_open, *btn_save;
  Ihandle *sub_menu_format, *format_menu, *item_font;
  Ihandle *sub_menu_help, *help_menu, *item_about;
  Ihandle *sub_menu_view, *view_menu, *item_toolbar, *item_statusbar;
  Ihandle *lbl_statusbar, *toolbar_hb, *recent_menu;
  Ihandle *config;
  const char* font;

  IupOpen(&argc, &argv);
  IupImageLibOpen();

  config = IupConfig();
  IupSetAttribute(config, "APP_NAME", "simple_notepad");
  IupConfigLoad(config);

  multitext = IupText(NULL);
    IupSetAttribute(multitext, "MULTILINE", "YES");
    IupSetAttribute(multitext, "EXPAND", "YES");
    IupSetAttribute(multitext, "NAME", "MULTITEXT");
    IupSetAttribute(multitext, "DIRTY", "NO");
    IupSetCallback(multitext, "CARET_CB", (Icallback)multitext_caret_cb);
    IupSetCallback(multitext, "VALUECHANGED_CB", (Icallback)multitext_valuechanged_cb);
    IupSetCallback(multitext, "DROPFILES_CB", (Icallback)dropfiles_cb);

  font = IupConfigGetVariableStr(config, "MainWindow", "Font");
  if (font)
    IupSetStrAttribute(multitext, "FONT", font);

  lbl_statusbar = IupLabel("Lin 1, Col 1");
    IupSetAttribute(lbl_statusbar, "NAME", "STATUSBAR");  
    IupSetAttribute(lbl_statusbar, "EXPAND", "HORIZONTAL");
    IupSetAttribute(lbl_statusbar, "PADDING", "10x5");

  item_new = IupItem("New\tCtrl+N", NULL);
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

  item_save = IupItem("Save\tCtrl+S", NULL);
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

  item_revert = IupItem("Revert", NULL);
    IupSetAttribute(item_revert, "NAME", "ITEM_REVERT");
    IupSetCallback(item_revert, "ACTION", (Icallback)item_revert_action_cb);
    
  item_exit = IupItem("E&xit", NULL);
    IupSetCallback(item_exit, "ACTION", (Icallback)item_exit_action_cb);

  item_find = IupItem("&Find...\tCtrl+F", NULL);
    IupSetAttribute(item_find, "IMAGE", "IUP_EditFind");
    IupSetCallback(item_find, "ACTION", (Icallback)item_find_action_cb);
  btn_find = IupButton(NULL, NULL);
    IupSetAttribute(btn_find, "IMAGE", "IUP_EditFind");
    IupSetAttribute(btn_find, "FLAT", "Yes");
    IupSetCallback(btn_find, "ACTION", (Icallback)item_find_action_cb);
    IupSetAttribute(btn_find, "TIP", "Find (Ctrl+F)");
    IupSetAttribute(btn_find, "CANFOCUS", "No");

  item_cut = IupItem("Cut\tCtrl+X", NULL);
    IupSetAttribute(item_cut, "NAME", "ITEM_CUT");
    IupSetAttribute(item_cut, "IMAGE", "IUP_EditCut");
    IupSetCallback(item_cut, "ACTION", (Icallback)item_cut_action_cb);
  item_copy = IupItem("Copy\tCtrl+C", NULL);
    IupSetAttribute(item_copy, "NAME", "ITEM_COPY");  
    IupSetAttribute(item_copy, "IMAGE", "IUP_EditCopy");
    IupSetCallback(item_copy, "ACTION", (Icallback)item_copy_action_cb);
  item_paste = IupItem("Paste\tCtrl+V", NULL);
    IupSetAttribute(item_paste, "NAME", "ITEM_PASTE");
    IupSetAttribute(item_paste, "IMAGE", "IUP_EditPaste");
    IupSetCallback(item_paste, "ACTION", (Icallback)item_paste_action_cb);
  item_delete = IupItem("Delete\tDel", NULL);
    IupSetAttribute(item_delete, "IMAGE", "IUP_EditErase");  
    IupSetAttribute(item_delete, "NAME", "ITEM_DELETE");
    IupSetCallback(item_delete, "ACTION", (Icallback)item_delete_action_cb);
  item_select_all = IupItem("Select All\tCtrl+A", NULL);
    IupSetCallback(item_select_all, "ACTION", (Icallback)item_select_all_action_cb);

  btn_cut = IupButton(NULL, NULL);
    IupSetAttribute(btn_cut, "IMAGE", "IUP_EditCut");
    IupSetAttribute(btn_cut, "FLAT", "Yes");
    IupSetCallback(btn_cut, "ACTION", (Icallback)item_cut_action_cb);
  btn_copy = IupButton(NULL, NULL);
    IupSetAttribute(btn_copy, "IMAGE", "IUP_EditCopy");
    IupSetAttribute(btn_copy, "FLAT", "Yes");
    IupSetCallback(btn_copy, "ACTION", (Icallback)item_copy_action_cb);
  btn_paste = IupButton(NULL, NULL);
    IupSetAttribute(btn_paste, "IMAGE", "IUP_EditPaste");
    IupSetAttribute(btn_paste, "FLAT", "Yes");
    IupSetCallback(btn_paste, "ACTION", (Icallback)item_paste_action_cb);

  toolbar_hb = IupHbox(
    btn_new,
    btn_open,
    btn_save,
    IupSetAttributes(IupLabel(NULL), "SEPARATOR=VERTICAL"),
    btn_cut,
    btn_copy,
    btn_paste,
    IupSetAttributes(IupLabel(NULL), "SEPARATOR=VERTICAL"),
    btn_find,
    NULL);
  IupSetAttribute(toolbar_hb, "MARGIN", "5x5");
  IupSetAttribute(toolbar_hb, "GAP", "2");

  item_toolbar = IupItem("&Toobar", NULL);
  IupSetCallback(item_toolbar, "ACTION", (Icallback)item_toolbar_action_cb);
  IupSetAttribute(item_toolbar, "VALUE", "ON");
  item_statusbar = IupItem("&Statusbar", NULL);
  IupSetCallback(item_statusbar, "ACTION", (Icallback)item_statusbar_action_cb);
  IupSetAttribute(item_statusbar, "VALUE", "ON");

  if (!IupConfigGetVariableIntDef(config, "MainWindow", "Toolbar", 1))
  {
    IupSetAttribute(item_toolbar, "VALUE", "OFF");

    IupSetAttribute(toolbar_hb, "FLOATING", "YES");
    IupSetAttribute(toolbar_hb, "VISIBLE", "NO");
  }

  if (!IupConfigGetVariableIntDef(config, "MainWindow", "Statusbar", 1))
  {
    IupSetAttribute(item_statusbar, "VALUE", "OFF");

    IupSetAttribute(lbl_statusbar, "FLOATING", "YES");
    IupSetAttribute(lbl_statusbar, "VISIBLE", "NO");
  }

  item_goto = IupItem("&Go To...\tCtrl+G", NULL);
    IupSetCallback(item_goto, "ACTION", (Icallback)item_goto_action_cb);

  item_font = IupItem("&Font...", NULL);
    IupSetCallback(item_font, "ACTION", (Icallback)item_font_action_cb);
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
    item_cut,
    item_copy,
    item_paste,
    item_delete,
    IupSeparator(),
    item_find,
    item_goto,
    IupSeparator(),
    item_select_all,
    NULL);
  format_menu = IupMenu(
    item_font,
    NULL);
  view_menu = IupMenu(
    item_toolbar,
    item_statusbar,
    NULL);
  help_menu = IupMenu(
    item_about,
    NULL);

  IupSetCallback(file_menu, "OPEN_CB", (Icallback)file_menu_open_cb);
  IupSetCallback(edit_menu, "OPEN_CB", (Icallback)edit_menu_open_cb);

  sub_menu_file = IupSubmenu("&File", file_menu);
  sub_menu_edit = IupSubmenu("&Edit", edit_menu);
  sub_menu_format = IupSubmenu("F&ormat", format_menu);
  sub_menu_view = IupSubmenu("&View", view_menu);
  sub_menu_help = IupSubmenu("&Help", help_menu);

  menu = IupMenu(
    sub_menu_file,
    sub_menu_edit,
    sub_menu_format,
    sub_menu_view,
    sub_menu_help,
    NULL);

  vbox = IupVbox(
    toolbar_hb,
    multitext,
    lbl_statusbar,
    NULL);

  dlg = IupDialog(vbox);
  IupSetAttributeHandle(dlg, "MENU", menu);
  IupSetAttribute(dlg, "SIZE", "HALFxHALF");
  IupSetCallback(dlg, "CLOSE_CB", (Icallback)item_exit_action_cb);
  IupSetCallback(dlg, "DROPFILES_CB", (Icallback)dropfiles_cb);

  IupSetAttribute(dlg, "CONFIG", (char*)config);

  /* parent for pre-defined dialogs in closed functions (IupMessage) */
  IupSetAttributeHandle(NULL, "PARENTDIALOG", dlg);

  IupSetCallback(dlg, "K_cN", (Icallback)item_new_action_cb);
  IupSetCallback(dlg, "K_cO", (Icallback)item_open_action_cb);
  IupSetCallback(dlg, "K_cS", (Icallback)item_save_action_cb);
  IupSetCallback(dlg, "K_cF", (Icallback)item_find_action_cb);
  IupSetCallback(dlg, "K_cG", (Icallback)item_goto_action_cb);
  
  IupConfigRecentInit(config, recent_menu, config_recent_cb, 10);

  IupConfigDialogShow(config, dlg, "MainWindow");

  /* initialize the current file */
  new_file(dlg);

  /* open a file from the command line (allow file association in Windows) */
  if (argc > 1 && argv[1])
  {
    const char* filename = argv[1];
    open_file(dlg, filename);
  }

  IupMainLoop();

  IupClose();
  return EXIT_SUCCESS;
}
