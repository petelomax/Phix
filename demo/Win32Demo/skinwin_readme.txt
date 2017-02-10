------------------------------------------------
Readme for Skin Window Demo and Library (Updated 22 June 2005)
©2005 Dave Probert
email: probert.dave@gmail.com
website: www.purpletiger.com
------------------------------------------------

Simple library for applying a 'skin' to a Windows window.
** Now features the ability to save and load regions from disc - much faster than the scanning process
**  Usually would be done by a tool to scan the bitmap skin and save it - then the main program just uses the file

The library doesn't actually apply the image - that is left to the Paint handler function to blit the image to the window, but the shape of the window will come from the image itself.

There isn't much to say here, except that the code is commented quite a lot (never enough I suppose!), but you should be able to follow it easily enough.  The Demo exw file has also got code to handle dragging the window, applying icons over the skin and handling some aspects of the 'missing' system icons (Close and Minimise).

** Read the Demo code to see how to use the save/load features

There are lots of areas where it can all be improved I'm sure - but that is left up to the skill of the user ;)  If you make any changes/improvements and want them to go in the main library then please drop me an email to probert.dave@gmail.com with the changes and I'll resubmit it with full credits.  Also leave me notes/bugs/praise/rants/raves/etc on the EuForum (www.listfilter.com/EUforum).

Enjoy,

Dave

------------------------------------------------
