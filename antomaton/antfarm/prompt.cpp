
#include "prompt.h"
#include "font.h"
#include "sdlutil.h"
#include "draw.h"
#include "chars.h"
#include "extent.h"

/* XXX implement in terms of menu class?
   textinput is meant to be the same */

prompt * prompt::create() {
  prompt * pp = new prompt();
  pp->below = 0;
  pp->alpharect = 0;
  return pp;
}

void prompt::destroy() {
  if (alpharect) SDL_FreeSurface(alpharect);
  delete this;
}

prompt::~prompt() {}

string prompt::ask(drawable * b, string t, string d) {
  prompt * pp = prompt::create();
  extent<prompt> ep(pp);
  pp->title = t;
  pp->below = b;
  pp->input = d;

  /* ? */
  pp->posx = 30;
  pp->posy = 120;
		
  return pp->select();
}

string prompt::select() {

  /* use posx to derive width such
     that the box is centered. */
  
  int w = screen->w - (posx * 2);
  int h = 3 * fon->height;

  /* XXX make these values setable */
  /* create alpha rectangle */
  alpharect = sdlutil::makealpharect(w, h, 90, 90, 90, 180);

  sdlutil::outline(alpharect, 2, 36, 36, 36, 200);

  return loop();
}

void prompt::screenresize() {
  if (below) below->screenresize();
}

void prompt::draw() {

  /* clear back */
  if (!below) {
    sdlutil::clearsurface(screen, BGCOLOR);
  } else {
    below->draw();
  }

  /* draw alpha-transparent box */
  SDL_Rect dest;

  dest.x = posx;
  dest.y = posy;

  SDL_BlitSurface(alpharect, 0, screen, &dest);

  string i = input;

  /* allow string to end with ^ */
  if (i.length() > 0 && 
      i[i.length() - 1] == '^') i += '^';

  fon->draw(posx + fon->width, 
	    posy + fon->height, 
	    title + i + (string)YELLOW "_");

}

string prompt::loop() {

  draw();
  SDL_Flip(screen);

  SDL_Event e;

  while ( SDL_WaitEvent(&e) >= 0 ) {

    int key;

    switch(e.type) {
    case SDL_QUIT:
      return ""; /* XXX ? */
    case SDL_KEYDOWN:
      key = e.key.keysym.sym;
      switch(key) {
      case SDLK_ESCAPE:
	return "";

      case SDLK_BACKSPACE:
	input = input.substr(0, input.length() - 1);
	draw();
	SDL_Flip(screen);
	break;

      case SDLK_RETURN:
	return input;

      default:
	if (e.key.keysym.sym == SDLK_u &&
	    e.key.keysym.mod & KMOD_CTRL) {
	 
	  input = "";
	  draw();
	  SDL_Flip(screen);
	} else {
	  int uc = e.key.keysym.unicode;
	  if ((uc & ~0x7F) == 0 && uc >= ' ') {
	    input += (char)(uc);
	    draw();
	    SDL_Flip(screen);
	  }
	}
      }
      break;
    case SDL_VIDEORESIZE: {
      SDL_ResizeEvent * eventp = (SDL_ResizeEvent*)&e;
      screen = sdlutil::makescreen(eventp->w, 
				   eventp->h);
      screenresize();
      draw();
      SDL_Flip(screen);
      break;
    }
    case SDL_VIDEOEXPOSE:
      draw();
      SDL_Flip(screen);
      break;
    default: break;
    }
  }
  return ""; /* XXX ??? */
}

