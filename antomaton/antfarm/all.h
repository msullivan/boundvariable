
#ifndef __ALL_H
#define __ALL_H

#include "SDL.h"
#include "SDL_image.h"
#include <math.h>

#include <string>
#include "font.h"
#include "sdlutil.h"
#include "drawable.h"

using namespace std;

#define STARTW 800
#define STARTH 600

/* build this many zoomed versions of
   the graphics, each at 1/2 the size
   of the previous (including the 1:1
   originals) 

   careful setting this too hight, or
   else shrink50 will eventually fail
   because the images are too small
   (could fix this and have it return
   an empty or near-empty surface) */
#define DRAW_NSIZES 4

extern SDL_Surface * screen;

/* name of this executable */
extern string self;

/* is the network enabled? */
extern int network;
/* is the audio subsystem started? */
extern int audio;

#include "version.h"


#ifndef PLATFORM
#  ifdef WIN32
#    define PLATFORM "win32"
#  else 
#    ifdef OSX
#      define PLATFORM "osx"
#    else /* assume linux */
#      define PLATFORM "linux"
#      define LINUX
#    endif
#  endif
#endif

#ifdef WIN32
/* totally moronic "performance" warning in VC++ */
#   pragma warning(disable: 4800)
#endif

#ifndef WIN32
/* new g++ needs ULL suffix for 64-bit constants, ugh */
#  define L64(i) (i ## ULL)
#else
#  define L64(i) i
#endif

#endif
