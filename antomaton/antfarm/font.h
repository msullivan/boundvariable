
#ifndef __FONT_H
#define __FONT_H

#include <string>
using namespace std;

/* XXX move some of this to fontutil */
struct font {

  int width, height, styles, overlap;
  
  /* give the first n characters of
     a string, doing the right thing
     for color codes */
  static string prefix(string, unsigned int);

  /* similarly, pad a string out to n displayable
     characters, doing the right thing for
     color codes. If n is negative, pad with spaces
     on the left side instead of right.

     precondition: |n| >= 3 */
  static string pad(string s, int n);

  /* truncate to n chars if too long */
  static string truncate(string s, unsigned int);

  /* length of string in actual chars.
     length(s) * (width-overlap) gives
     the screen width. */
  static unsigned int length(string);

  static font * create(string file,
                       string charmap,
                       int width,
                       int height,
                       int styles=1,
                       int overlap=0,
		       int dims=2);
  
  /* return the size in pixels of the string.
     this ignores formatting characters. 
     sizey is always font.height.
  */
  virtual int sizex(const string &) = 0;
  virtual int sizex_plain(const string &) = 0;

  /* returns the number of lines in the string,
     which is always at least 1 unless the
     string is empty. */
  static int lines(const string &);

  /* specify the top-left pixel. draws to the
     screen. */
  virtual void draw(int x, int y, string s) = 0;
  virtual void draw_plain(int x, int y, string s) = 0;
  virtual void drawto(SDL_Surface *, int x, int y, string s) = 0;
  virtual void drawto_plain(SDL_Surface *, int x, int y, string s) = 0;

  /* draw a multi-line string.
     the height in pixels of the area drawn is returned. */
  virtual int drawlines(int x, int y, string s) = 0;

  /* same, but centers each line horizontally about the x position */
  virtual int drawcenter(int x, int y, string s) = 0;

  virtual void destroy() = 0;
  virtual ~font();
};

#endif
