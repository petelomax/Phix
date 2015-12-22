
#include "simple_paint_util.h"

#include <string.h>
#include <ctype.h>

#include <iup.h>
#include <cd.h>
#include <im_process.h>


char* str_duplicate(const char* str)
{
  if (!str)
    return NULL;

  int size = (int)strlen(str);
  char* new_str = new char[size+1];
  memcpy(new_str, str, size + 1);
  return new_str;
}

char* str_filepath(const char *filename)
{
  /* Start at the last character */
  int len = (int)strlen(filename);
  int offset = len - 1;
  while (offset != 0)
  {
    if (filename[offset] == '\\' || filename[offset] == '/')
      break;

    offset--;
  }

  if (offset == 0)
    return NULL;
  else
  {
    char* file_path = str_duplicate(filename);
    file_path[offset] = 0;
    return file_path;
  }
}

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


/********************************************************************/


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

void image_flood_fill(imImage* image, int start_x, int start_y, long replace_color, double tol_percent)
{
  float color[3];
  double tol;

  color[0] = (float)cdRed(replace_color);
  color[1] = (float)cdGreen(replace_color);
  color[2] = (float)cdBlue(replace_color);

  /* max value = 255*255*3 = 195075 */
  /* sqrt(195075) = 441 */
  tol = (441 * tol_percent) / 100;

  /* still too high */
  tol = tol / 5;  /* empirical reduce. TODO: What is the best formula? */

  imProcessRenderFloodFill(image, start_x, start_y, color, (float)tol);
}
