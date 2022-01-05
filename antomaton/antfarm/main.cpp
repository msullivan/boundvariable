#include "all.h"
#include "sdlutil.h"

#include "util.h"
#include "font.h"
#include "extent.h"
#include "md5.h"
#include "prompt.h"
#include "draw.h"
#include "message.h"

#include "antfarm.h"


#define DEFAULT_DIR "."
#define SPLASH_FILE "splash.png"

SDL_Surface * screen;
string self;

/* for debugging, turn on noparachute */
// #define DEBUG_PARACHUTE 0
#define DEBUG_PARACHUTE SDL_INIT_NOPARACHUTE



int main (int argc, char ** argv) {

  /* change to location of binary. */
  if (argc > 0) {
    string wd = util::pathof(argv[0]);
    util::changedir(wd);

#   if WIN32
    /* on win32, the ".exe" may or may not
       be present. Also, the file may have
       been invoked in any CaSe. */

    self = util::lcase(util::fileof(argv[0]));

		    self = util::ensureext(self, ".exe");

#   else
		    self = util::fileof(argv[0]);
#   endif

  }

  /* set up md5 early */
  md5::init();

  if (SDL_Init (SDL_INIT_VIDEO | 
                SDL_INIT_TIMER | 
		DEBUG_PARACHUTE) < 0) {
    printf("Unable to initialize SDL. (%s)\n", SDL_GetError());
    return 1;
  }
  
  SDL_EnableKeyRepeat(SDL_DEFAULT_REPEAT_DELAY, 
                      SDL_DEFAULT_REPEAT_INTERVAL);

  SDL_EnableUNICODE(1);

  /* set caption and icon */
  {
    SDL_WM_SetCaption("antfarm " VERSION, "");
    SDL_Surface * icon = IMG_Load("icon.png");
    if (icon) SDL_WM_SetIcon(icon, 0);
    /* XXX free icon? It's not clear where we
       can safely do this. */
  }

  screen = sdlutil::makescreen(STARTW, STARTH);

  if (!screen) {
    printf("Failed to create screen.\n");
    goto no_drawings;
  }

  /* draw splash while loading images. animation init
     takes some time! */

  {
    SDL_Surface * splash = sdlutil::imgload(SPLASH_FILE);
    if (splash) {
      SDL_Rect dst;
      dst.x = 2;
      dst.y = screen->h - (splash->h + 2);
      SDL_BlitSurface(splash, 0, screen, &dst);
      SDL_Flip(screen);

      SDL_FreeSurface(splash);
      
    }

  }

  /* XXX callback progress for ainit */
  if (!drawing::loadimages()) {
    if (fon) message::bug(0, "Error loading graphics!");
    printf("Failed to load graphics.\n");
    goto no_drawings;
  }

  /* before we go to the player database, get rid of bad
     mousemotion events that the queue starts with. */
  sdlutil::eatevents(30, SDL_EVENTMASK(SDL_MOUSEMOTION));
  /* this may or may not be a good idea, now */
  SDL_WarpMouse((Uint16)(screen->w * 0.75f), 8);

  /* current arg looking at */
  {
    int carg = 1;

    while (carg < argc) {
      if ((string)argv[carg] == "-movie") {
	DO_MOVIE = 1;
	carg ++;
      } 
      /* else if... */
      else break;
    }

    {
      string f = "default.ant";
      if (carg < argc) f = argv[carg++];

      /* do it */
      antfarm (f);
    }


    drawing::destroyimages();
  }

 no_drawings: ;
  SDL_Quit();

  return 0;
}
