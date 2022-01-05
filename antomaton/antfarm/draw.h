
#ifndef __DRAW_H
#define __DRAW_H


#include "SDL.h"
#include "SDL_image.h"
#include "font.h"
#include <math.h>
#include <string>

using namespace std;

/* size of zoom 0 tiles 
   we assume that these are
   evenly divisible by 
   2^(DRAW_NSIZES - 1) */
#define TILEW 32
#define TILEH 32

#define GUY_OVERLAPY 5
#define DALEK_OVERLAPY 5
#define HUGBOT_OVERLAPY 5
#define BROKEN_OVERLAPY 0
#define BOMB_OVERLAPY 5
#define MAX_OVERLAPY 5

#define BGCOLOR 0

/* width of source graphic in tiles */
#define SRCTILESW 16

/* utility tiles */
enum { TU_TARGET, TU_DISABLED, TU_WARNING,
       TU_LAYERNORMAL, TU_LAYERALT,
       TU_TILESUD,
       TU_SAVE, TU_SAVEAS, TU_LOAD,
       TU_TITLE, TU_AUTHOR, TU_SIZE, TU_PLAYERSTART,
       TU_CLEAR, TU_PLAY, TU_RANDOM, TU_RANDTYPE, 
       TU_CHANGED, 

       /* menu items */
       TU_X, TU_N, TU_I,
       TU_T, TU_1, TU_2, TU_3, TU_4, TU_P,
       
       TU_EXITOPEN,

       TU_ERASE_BOT, TU_FIRST_BOT,
       TU_DALEK, TU_BROKEN, TU_HUGBOT,

       TU_PLAYBUTTON, TU_PAUSEBUTTON,
       TU_FREVBUTTON, TU_FFWDBUTTON, 
       TU_FWDBUTTON, TU_REVBUTTON,

       TU_SLEEPWAKE, TU_PREFAB,

       TU_SAVESTATE, TU_RESTORESTATE,
       TU_BOOKMARKS,
       TU_UNDO, 

       TU_BOMB, TU_BOMBTIMER,
};

enum tile {
  T_FLOOR, T_RED, T_BLUE, T_GREY, T_GREEN, T_EXIT, T_HOLE, T_GOLD, 
  T_LASER, T_PANEL, T_STOP, T_RIGHT, T_LEFT, T_UP, T_DOWN, T_ROUGH,
  T_ELECTRIC, T_ON, T_OFF, T_TRANSPORT, T_BROKEN, T_LR, T_UD, T_0,
  T_1, T_NS, T_NE, T_NW, T_SE, T_SW, T_WE, T_BUTTON, T_BLIGHT, 
  T_RLIGHT, T_GLIGHT, T_BLACK, T_BUP, T_BDOWN,
  T_RUP, T_RDOWN, T_GUP, T_GDOWN, 
  T_BSPHERE, T_RSPHERE, T_GSPHERE, T_SPHERE,
  T_TRAP2, T_TRAP1,
  
  T_BPANEL, T_RPANEL, T_GPANEL,
  
  T_STEEL, T_BSTEEL, T_RSTEEL, T_GSTEEL, 

  T_HEARTFRAMER, T_SLEEPINGDOOR, 

  T_TRANSPONDER, T_NSWE,

  /* twelve useless tiles */
  TT_1, TT_2, TT_3, TT_4, TT_5, TT_6, TT_7, TT_8, TT_9,
  TT_10, TT_11, TT_12,

  T_DIRRIGHT, T_DIRUP, T_DIRDOWN, T_DIRLEFT,


  NUM_TILES,
};

extern font * fon;
extern font * fonsmall;

struct drawing {

  /* initialized by loadimages:
     there are DRAW_NSIZES of these 
     in exponential backoff */
  static SDL_Surface ** tiles;
  static SDL_Surface ** guy;
  static SDL_Surface ** tilesdim;
  static SDL_Surface ** tileutil;
  static SDL_Surface ** tileutildim;

  /* call this once in the program. true on success */
  static bool loadimages();

  static void destroyimages();

  /* must be in range 0..(DRAW_NSIZES - 1) */
  int zoomfactor;

  string message;

  /* must set at least width, height, lev */
  drawing () {}

  /* Drawing functions */

  /* if surface isn't supplied, then draw to screen. */
  static void drawtile(int px, int py, int tl, int zfactor = 0, 
		       SDL_Surface * surf = 0, bool dim = false);
  static void drawtileu(int px, int py, int tl, int zf = 0,
			SDL_Surface * surf = 0, bool dim = false);

};

#endif
