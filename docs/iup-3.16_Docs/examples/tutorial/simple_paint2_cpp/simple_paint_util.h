
/* avoid multiple inclusions */
#ifndef SIMPLE_PAINT_UTIL_H
#define SIMPLE_PAINT_UTIL_H

#include <im.h>
#include <im_image.h>


char* str_duplicate(const char* str);  /* must be deleted */
int str_compare(const char *l, const char *r, int casesensitive);

const char* str_filetitle(const char *filename);
const char* str_fileext(const char *filename);
char* str_filepath(const char *filename);  /* must be deleted */

void show_error(const char* message, int is_error);
void show_file_error(int error);

void image_flood_fill(imImage* image, int start_x, int start_y, long replace_color, double tol_percent);

#endif
