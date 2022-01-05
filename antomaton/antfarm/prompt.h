
#ifndef __PROMPT_H
#define __PROMPT_H

#include "all.h"
#include "drawable.h"

struct prompt : public drawable {
  
  string title;
  string input;

  drawable * below;

  int posx;
  int posy;

  static string ask(drawable * b, string title, string def="");

  static prompt * create();
  void destroy();

  string select();

  virtual void draw();
  virtual void screenresize();
  virtual ~prompt();

  private:
  string loop();

  SDL_Surface * alpharect;

};

#endif
