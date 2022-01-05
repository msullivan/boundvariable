
/* drawing a level */

#include "draw.h"
#include "sdlutil.h"
#include "chars.h"
#include "util.h"
#include "message.h"
#include <time.h>
#include "extent.h"
 
#define TILEUTIL_FILE "tileutil.png"
#define TILES_FILE "tiles.png"

font * fon;
font * fonsmall;

SDL_Surface ** drawing::tiles = 0;
SDL_Surface ** drawing::tilesdim = 0;
SDL_Surface ** drawing::tileutil = 0;
SDL_Surface ** drawing::tileutildim = 0;

/* the bottom of the load screen gives a preview of each level. This
   indicates the maximum width and height (in tiles) shown. */
/* XXX these should perhaps be parameters to drawsmall? */
#define PREVIEWHEIGHT (12 * (TILEH >> 2))
#define PREVIEWWIDTH (26 * (TILEW >> 2))

#define SHOWDESTCOLOR 0xAA, 0xAA, 0x33

bool drawing::loadimages() {
  /* PERF could be alpha=false. but the alphadim and shrink50
     routines rely on this being a 32 bit graphic. */
  SDL_Surface * tt = sdlutil::imgload(TILES_FILE);
  if (!tt) return 0;
  
  SDL_Surface * uu = sdlutil::imgload(TILEUTIL_FILE);
  if (!uu) return 0;

  /* XXX make dim levels for font too (pass in argument) */
  fon = font::create("font.png",
		     FONTCHARS,
 		     9, 16, FONTSTYLES, 1, 3);

  fonsmall = font::create("fontsmall.png",
			  FONTCHARS,
			  6, 6, FONTSTYLES, 0, 3);

  tiles = (SDL_Surface **)malloc(sizeof (SDL_Surface *) * DRAW_NSIZES);
  tilesdim = (SDL_Surface **)malloc(sizeof (SDL_Surface *) * DRAW_NSIZES);
  tileutil = (SDL_Surface **)malloc(sizeof (SDL_Surface *) * DRAW_NSIZES);
  tileutildim = (SDL_Surface **)malloc(sizeof (SDL_Surface *) * DRAW_NSIZES);

  if (!(tiles && tilesdim && tileutil && tileutildim)) return false;

  for(int z = 0; z < DRAW_NSIZES; z++) {
    tiles[z] = 0;
    tilesdim[z] = 0;
    tileutil[z] = 0;
  }

  tileutil[0] = uu;
  tiles[0] = tt;
  /* sdlutil::printsurfaceinfo(tiles[0]); */
  tilesdim[0] = sdlutil::alphadim(tiles[0]);
  tileutildim[0] = sdlutil::alphadim(tileutil[0]);
  if (!tilesdim[0]) return false;
  if (!tileutildim[0]) return false;

  /* XXX call sdlutil::make_mipmaps */
  int last = 0;
  while(last < (DRAW_NSIZES - 1)) {
    last ++;
    tiles[last] = sdlutil::shrink50(tiles[last - 1]);
    if (!tiles[last]) return false;
    tilesdim[last] = sdlutil::alphadim(tiles[last]);
    if (!tilesdim[last]) return false;
    tileutil[last] = sdlutil::shrink50(tileutil[last - 1]);
    if (!tileutil[last]) return false;
    tileutildim[last] = sdlutil::alphadim(tileutil[last]);
    if (!tileutildim[last]) return false;
  }

  if (!fon) return false;
  return true;
}

void drawing::destroyimages() {

  for(int i = 0; i < DRAW_NSIZES; i ++) {
    if (tiles && tiles[i]) SDL_FreeSurface(tiles[i]);
    if (tilesdim && tilesdim[i]) SDL_FreeSurface(tilesdim[i]);
    if (tileutil && tileutil[i]) SDL_FreeSurface(tileutil[i]);
    if (tileutildim && tileutildim[i]) SDL_FreeSurface(tileutildim[i]);
  }
  
  free(tiles);
  free(tilesdim);
  free(tileutil);
  free(tileutildim);

  if (fon) fon->destroy();
  if (fonsmall) fonsmall->destroy ();

}

void drawing::drawtile(int px, int py, int tl, int zf, 
		       SDL_Surface * surf, bool dim) {
  
  if (!surf) surf = screen;

  SDL_Rect src, dst;
  dst.x = px;
  dst.y = py;

  src.x = (TILEW >> zf) * (tl % SRCTILESW);
  src.y = (TILEH >> zf) * (tl / SRCTILESW);

  src.w = (TILEW >> zf);
  src.h = (TILEH >> zf);

  SDL_BlitSurface((dim?tilesdim:tiles)[zf], &src, surf, &dst);

}

void drawing::drawtileu(int px, int py, int tl, int zf, 
			SDL_Surface * surf, bool dim) {
  
  if (!surf) surf = screen;

  SDL_Rect src, dst;
  dst.x = px;
  dst.y = py;

  src.x = (TILEW >> zf) * tl;
  src.y = 0;

  src.w = (TILEW >> zf);
  src.h = (TILEH >> zf);

  SDL_BlitSurface((dim?tileutildim:tileutil)[zf], &src, surf, &dst);

}
