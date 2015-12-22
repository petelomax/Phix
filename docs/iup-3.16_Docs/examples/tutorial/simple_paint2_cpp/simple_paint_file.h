
/* avoid multiple inclusions */
#ifndef SIMPLE_PAINT_FILE_H
#define SIMPLE_PAINT_FILE_H

#include <im.h>
#include <im_image.h>


class SimplePaintFile
{
  char* filename;
  imImage* image;
  bool dirty;

public:

  SimplePaintFile()
    :filename(0), image(0), dirty(false)
  {
  }

  void SetFilename(const char* new_filename);
  const char* GetFilename() const { return filename; }

  void SetImage(imImage* new_image, bool release = true);
  const imImage* GetImage() const { return image; }

  void SetImageChanged() { dirty = true; }
  bool GetImageChanged() const { return dirty; }

  imImage* Read(const char* new_filename) const;
  bool Write(const char* new_filename) const;
  void SetFormat(const char* new_filename);

  bool New(int width, int height);
  void New(imImage* new_image);
  bool Open(const char* new_filename);
  bool SaveAsFile(const char* new_filename);
  bool SaveCheck();
  void SaveFile();
  void Close();
};


#endif
