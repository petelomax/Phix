
#include "simple_paint_file.h"
#include "simple_paint_util.h"

#include <iup.h>
#include <im_convert.h>
#include <im_process.h>


void SimplePaintFile::Close()
{
  if (filename) 
    delete[] filename;

  if (image) 
    imImageDestroy(image);
}

void SimplePaintFile::SetFilename(const char* new_filename)
{
  if (filename != new_filename)
  {
    if (filename) 
      delete[] filename;

    filename = str_duplicate(new_filename);
  }
}

imImage* SimplePaintFile::Read(const char* new_filename) const
{
  int error;
  imImage* image = imFileImageLoadBitmap(new_filename, 0, &error);
  if (error)
    show_file_error(error);
  return image;
}

bool SimplePaintFile::Write(const char* new_filename) const
{
  const char* format = imImageGetAttribString(image, "FileFormat");
  int error = imFileImageSave(new_filename, format, image);
  if (error)
  {
    show_file_error(error);
    return false;
  }
  return true;
}

void SimplePaintFile::SetFormat(const char* new_filename)
{
  const char* ext = str_fileext(new_filename);
  const char* format = "JPEG";
  if (str_compare(ext, "jpg", 0) || str_compare(ext, "jpeg", 0))
    format = "JPEG";
  else if (str_compare(ext, "bmp", 0))
    format = "BMP";
  else if (str_compare(ext, "png", 0))
    format = "PNG";
  else if (str_compare(ext, "tga", 0))
    format = "TGA";
  else if (str_compare(ext, "tif", 0) || str_compare(ext, "tiff", 0))
    format = "TIFF";
  imImageSetAttribString(image, "FileFormat", format);
}

bool SimplePaintFile::SaveCheck()
{
  if (dirty)
  {
    switch (IupAlarm("Warning", "File not saved! Save it now?", "Yes", "No", "Cancel"))
    {
    case 1:  /* save the changes and continue */
      SaveFile();
      break;
    case 2:  /* ignore the changes and continue */
      break;
    case 3:  /* cancel */
      return false;
    }
  }
  return true;
}

void SimplePaintFile::SaveFile()
{
  if (Write(filename))
    dirty = false;
}

bool SimplePaintFile::SaveAsFile(const char* new_filename)
{
  SetFormat(new_filename);

  if (Write(new_filename))
  {
    SetFilename(new_filename);
    dirty = false;

    return true;
  }

  return false;
}

static void image_fill_white(imImage* image)
{
  float color[3];

  color[0] = 255;
  color[1] = 255;
  color[2] = 255;

  imProcessRenderConstant(image, color);
}

bool SimplePaintFile::New(int width, int height)
{
  imImage* new_image = imImageCreate(width, height, IM_RGB, IM_BYTE);
  if (!new_image)
  {
    show_file_error(IM_ERR_MEM);
    return false;
  }

  /* new image default contents */
  image_fill_white(new_image);

  /* default file format */
  imImageSetAttribString(new_image, "FileFormat", "JPEG");

  SetImage(new_image);

  /* set properties */
  SetFilename(0);
  dirty = false;

  return true;
}

void SimplePaintFile::SetImage(imImage* new_image, bool release)
{
  /* remove previous one if any */
  if (release && image)
    imImageDestroy(image);

  /* set properties (leave filename as it is) */
  dirty = true;
  image = new_image;
}

void SimplePaintFile::New(imImage* new_image)
{
  /* this tests are necessary only for open and paste */

  /* we are going to support only RGB images with no alpha */
  imImageRemoveAlpha(new_image);
  if (new_image->color_space != IM_RGB)
  {
    imImage* rgb_image = imImageCreateBased(new_image, -1, -1, IM_RGB, -1);
    imConvertColorSpace(new_image, rgb_image);
    imImageDestroy(new_image);

    new_image = rgb_image;
  }

  /* default file format */
  const char* format = imImageGetAttribString(new_image, "FileFormat");
  if (!format)
    imImageSetAttribString(new_image, "FileFormat", "JPEG");

  SetImage(new_image);
}

bool SimplePaintFile::Open(const char* new_filename)
{
  imImage* new_image = Read(new_filename);
  if (new_image)
  {
    New(new_image);

    /* set properties */
    dirty = false;
    SetFilename(new_filename);

    return true;
  }

  return false;
}
