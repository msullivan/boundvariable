
#include <iostream>
#include <fstream>

#include "all.h"
#include "sdlutil.h"

#include "time.h"
#include "util.h"
#include "font.h"
#include "extent.h"
#include "md5.h"
#include "prompt.h"
#include "draw.h"
#include "message.h"

#ifdef WIN32
#define random rand
#define srandom srand
#endif

// #define ARROWS

enum contents {
  OPEN=0, HOLE, WALL, GOAL, WILD,
};

enum dir {
  NONE, UP, DOWN, LEFT, RIGHT,
};

struct cell {
  contents c;
  dir ant;
#ifdef ARROWS
  dir arrow;
#endif
  char pgm;
};

int SITERSTODO = 150;
int DO_MOVIE = 0;
int movieframe = 1;
string moviebase = "movie";

cell * board = 0;
cell * oboard = 0;

cell * pattern = 0;

#ifdef WIN32
typedef int llong;
#else
#ifdef LINUX
/* gcc 32-bit */
typedef long long llong;
#else
/* gcc 64-bit */
typedef long llong llong;
#endif
#endif

/* how many times has an ant been in a cell? */
llong * reached = 0;


/* solvestuff */
bool solving = false;
int sitersleft = 0;
int solves = 0;
cell * sboard = 0;


/* size of board, oboard, pattern. */
static int WIDTH = 32;
static int HEIGHT = 32;

int zoomfactor = 1;

int highlightx = -1, highlighty = -1;

/* programs allowed (< 10) */
int progpatterns = 0;
/* might contain NONE, meaning wild */
dir progpat[10][7];
/* may not contain wildcards */
dir program[10][7];

static string dtos(dir d) {
  switch(d) {
  case NONE: return "?";
  case UP: return "N";
  case DOWN: return "S";
  case LEFT: return "W";
  case RIGHT: return "E";
  }
  return "?";
}

static dir ctod(char c) {
  switch(c) {
  case 'N': return UP;
  case 'S': return DOWN;
  case 'W': return LEFT;
  case 'E': return RIGHT;
  default: return NONE;
  }
}

static string ptos(dir * p) {
  string o = "";
  for(int i = 0; i < 7; i ++) {
    o += dtos(p[i]);
  }
  return o;
}

static void setprogram(int j, string p) {
  for(unsigned int i = 0; i < 7; i ++) {
    if (i >= p.length () ||
	ctod(p[i]) == NONE) {
      message :: no(0, "Bad program: " + (string)RED + p);
      return ;
    }
    program[j][i] = ctod(p[i]);
  }
}

static void setprogrampattern(int j, string p) {
  for(unsigned int i = 0; i < 7; i ++) {
    if (i >= p.length () ||
	(p[i] != '*' &&
	 ctod(p[i]) == NONE)) {
      message :: no(0, "Bad program pattern: " + (string)RED + p);
      return ;
    }
    if (p[i] == '*') progpat[j][i] = NONE;
    else progpat[j][i] = ctod(p[i]);
  }
}


static int tilecell(int x, int y) {
  int i = y * WIDTH + x;
  switch(board[i].c) {
  case WILD: return T_PANEL;
  case OPEN:
    switch(board[i].ant) {
    case NONE:
#ifdef ARROWS
      switch(board[i].arrow) {
      case NONE: return T_FLOOR;
      case UP: return T_DIRUP;
      case DOWN: return T_DIRDOWN;
      case LEFT: return T_DIRLEFT;
      case RIGHT: return T_DIRRIGHT; 
      }
      break;
#else
      return T_FLOOR;
#endif
    case UP: return T_UP;
    case DOWN: return T_DOWN;
    case LEFT: return T_LEFT;
    case RIGHT: return T_RIGHT;
    }
    break;
  case HOLE: return T_HOLE;
  case WALL: return T_BLUE;
  case GOAL: return T_EXIT;
  }

  return T_STOP;
}

static bool paused;
static bool fog;

/* ??? */
int dummy = 0;
static llong iter = 0;
static int goals = 0;
static int deaths = 0;

static void drawboard () {
  for(int y = 0; y < HEIGHT; y ++) {
    for(int x = 0; x < WIDTH; x ++) {
      int t = tilecell(x, y);

      drawing::drawtile(x * (TILEW >> zoomfactor),
			y * (TILEH >> zoomfactor),
			t, zoomfactor);
      
      int i = y * WIDTH + x;
      if (board[i].c == OPEN &&
	  board[i].ant != NONE) {

	if (board[i].pgm >= 0 &&
	    board[i].pgm <= 9)
	  (zoomfactor?fonsmall:fon)
	  ->draw(x * (TILEW >> zoomfactor),
			 y * (TILEH >> zoomfactor),
			 BLUE + itos(board[i].pgm));
	else
	  fon->draw(x * (TILEW >> zoomfactor),
		    y * (TILEH >> zoomfactor),
		    RED + itos(board[i].pgm));
      }

      /* FOG OF WAR */
      if (fog && (reached[i] * 1000 < iter) &&
	  board[i].c == OPEN) {

	/* never reached! */
	if (!reached[i]) {
	  /* place X */
	  drawing::drawtileu(x * (TILEW >> zoomfactor),
			     y * (TILEH >> zoomfactor),
			     TU_X, zoomfactor,
			     screen, true);
	}
	  /* less dim, depending on level... */

	  float pct = (reached[i] * 1000) / (float)iter;

	  
	  if (pct < 0.10)
	    drawing::drawtileu(x * (TILEW >> zoomfactor),
			       y * (TILEH >> zoomfactor),
			       TU_DISABLED, zoomfactor,
			       screen, 
			       true);

	  if (pct < 0.33)
	    drawing::drawtileu(x * (TILEW >> zoomfactor),
			       y * (TILEH >> zoomfactor),
			       TU_DISABLED, zoomfactor,
			       screen, 
			       true);

	  if (pct < 0.75)
	    drawing::drawtileu(x * (TILEW >> zoomfactor),
			       y * (TILEH >> zoomfactor),
			       TU_DISABLED, zoomfactor,
			       screen, 
			       true);

	  /* Always ... */
	    drawing::drawtileu(x * (TILEW >> zoomfactor),
			       y * (TILEH >> zoomfactor),
			       TU_DISABLED, zoomfactor,
			       screen, 
			       true);

      }
      /* highlight the last "for the first time" tile */
      if (fog && highlightx == x && highlighty == y) {
	drawing::drawtileu(x * (TILEW >> zoomfactor),
			   y * (TILEH >> zoomfactor),
			   TU_TARGET, zoomfactor, screen, true);
      }
    }
  }
}

inline dir dir_cw(dir d) {
  switch (d) {
  case UP: return RIGHT;
  case RIGHT: return DOWN;
  case DOWN: return LEFT;
  case LEFT: return UP;
  default:;
  }
  return NONE;
}

static cell boardat(int x, int y) {
  int i = y * WIDTH + x;
  if (y < 0 || x < 0 || x >= WIDTH || y >= HEIGHT) {
    cell w;
    w.c = WALL;
    w.ant = NONE;
#ifdef ARROWS
    w.arrow = NONE;
#endif
    w.pgm = 0;
    return w;
  } else return board[i];
}

static cell tilein(int x, int y, dir d) {

  /* XXX bounds */
  switch(d) {
  case NONE: abort();
  case UP: return boardat(x, y - 1);
  case DOWN: return boardat(x, y + 1);
  case LEFT: return boardat(x - 1, y);
  case RIGHT: return boardat(x + 1, y);
  }
  /* ??? */
  abort();
  cell w;
  return w;
}

/* return the direction that we should face,
   assuming that the dir 'rel' is what we do
   if we're facing north */
static dir relative(dir current, dir rel) {
  // printf("relative (%s / %s)\n", dtos(current).c_str(),
  //        dtos(rel).c_str());
  switch(current) {
  case UP: return rel;
  case RIGHT: return dir_cw(rel);
    /* PERF */
  case DOWN: return dir_cw(dir_cw(rel));
  case LEFT: return dir_cw(dir_cw(dir_cw(rel)));
  default:;
  }
 return NONE;
}

/* the inverse of 'relative'. When we're facing
   in dir 'current' and see an ant facing dir 'seen',
   what direction is that relative to me if I were
   facing north? */
static dir reltome(dir current, dir seen) {
  switch(current) {
  case UP: return seen;
    /* PERF */
  case RIGHT: return dir_cw(dir_cw(dir_cw(seen)));
  case DOWN: return dir_cw(dir_cw(seen));
  case LEFT: return dir_cw(seen);
  default:;
  }
  return NONE;
}

static void pascii(FILE * what, dir prog[10][7], int w, int h, cell * b) {
  fprintf(what, "This is the title\n");
  for(int i = 0; i < 10; i ++) {
    fprintf(what, "%s\n", ptos(prog[i]).c_str());
  }

  fprintf(what, "%d %d\n", w, h);

  for(int y = 0; y < h; y ++) {
    for(int x = 0; x < w; x ++) {
      switch(b[y * w + x].c) {
      case OPEN:
	if (b[y * w + x].ant == NONE) {
	  if (x == highlightx && y == highlighty) {
	    fprintf(what, " .");
	  } else fprintf(what, " -");
	} else {
	  fprintf(what, "%c", '0' + b[y * w + x].pgm);
	  switch(b[y * w + x].ant) {
	  case NONE:  abort ();
	  case UP:    fprintf(what, "^"); break;
	  case DOWN:  fprintf(what, "v"); break;
	  case LEFT:  fprintf(what, "<"); break;
	  case RIGHT: fprintf(what, ">"); break;
	  }
	}
	break;
      case WALL: fprintf(what, " #"); break;
      case HOLE: fprintf(what, " o"); break;
      case GOAL: fprintf(what, " $"); break;
      case WILD: fprintf(what, " *"); break;
      }
    }
    fprintf(what, "\n");
  }
}

static void advance () {
  
  iter ++;

  for(int y = 0; y < HEIGHT; y ++) {
    for(int x = 0; x < WIDTH; x ++) {
      int i = y * WIDTH + x;
      // oboard[i] = board[i];
      switch (board[i].c) {
      case OPEN:
	
	/* if open, stays open. */
	oboard[i].c = OPEN;
	/* keeps arrow (though arrows are not
	   implemented yet) */
#ifdef ARROWS
	oboard[i].arrow = board[i].arrow;
#endif
	//oboard[i].ant = NONE;
	// oboard[i].pgm = 0;

	switch(board[i].ant) {
	case NONE: {
	  
	  /* XXX look for surrounding ants
	     and move them here */
	  int count = 0;

	  /* assumes invt that if ant != NONE,
	     c = OPEN */
	  cell up    = tilein(x, y, UP);
	  cell down  = tilein(x, y, DOWN);
	  cell left  = tilein(x, y, LEFT);
	  cell right = tilein(x, y, RIGHT);

	  if (up.ant == DOWN) count ++;
	  if (left.ant == RIGHT) count ++;
	  if (right.ant == LEFT) count ++;
	  if (down.ant == UP) count ++;

	  switch(count) {
	  case 4: /* FALLTHROUGH */
	  case 0:
	    oboard[i].ant = NONE;
	    break;
	  case 1:
	    /* ant enters this space, turns according to program,
	       and keeps its current program. */
	    
	    if (up.ant == DOWN) { 
	      oboard[i].ant = relative(DOWN, program[up.pgm][0]);
	      oboard[i].pgm = up.pgm; 
	    } else if (left.ant == RIGHT) {
	      oboard[i].ant = relative(RIGHT, program[left.pgm][0]);
	      oboard[i].pgm = left.pgm;
	    } else if (right.ant == LEFT) {
	      oboard[i].ant = relative(LEFT, program[right.pgm][0]);
	      oboard[i].pgm = right.pgm;
	    } else { 
	      oboard[i].ant = relative(UP, program[down.pgm][0]);
	      oboard[i].pgm = down.pgm; 
	    }

	    break;
	  case 2:
	    /* ants must be at a right angle */
	    if (up.ant == DOWN && left.ant == RIGHT) {
	      oboard[i].ant = relative(RIGHT, program[left.pgm][1]);
	      oboard[i].pgm = left.pgm;
	    } else if (up.ant == DOWN && right.ant == LEFT) {
	      oboard[i].ant = relative(DOWN, program[up.pgm][1]);
	      oboard[i].pgm = up.pgm;
	    } else if (down.ant == UP && left.ant == RIGHT) {
	      oboard[i].ant = relative(UP, program[down.pgm][1]);
	      oboard[i].pgm = down.pgm;
	    } else if (down.ant == UP && right.ant == LEFT) {
	      oboard[i].ant = relative(LEFT, program[right.pgm][1]);
	      oboard[i].pgm = right.pgm;
	    } else {
	      /* otherwise they are not at right angles, so die */
	      oboard[i].ant = NONE;
	    }
	    break;
	  case 3:
	    /* look for the open square */
	    if (up.ant != DOWN) {
	      oboard[i].ant = relative(UP, program[down.pgm][2]);
	      oboard[i].pgm = down.pgm;
	    } else if (right.ant != LEFT) {
	      oboard[i].ant = relative(RIGHT, program[left.pgm][2]);
	      oboard[i].pgm = left.pgm;
	    } else if (down.ant != UP) {
	      oboard[i].ant = relative(DOWN, program[up.pgm][2]);
	      oboard[i].pgm = up.pgm;
	    } else {
	      oboard[i].ant = relative(LEFT, program[right.pgm][2]);
	      oboard[i].pgm = right.pgm;
	    }
	  break;
	  }
	  break;
	}
	  /* otherwise... */
	default:

	  if (iter > 1000000 && !reached[i]) {
	    printf("reached %d/%d for the first time...\n", x, y);
	    highlightx = x;
	    highlighty = y;
	    pascii(stdout, program, WIDTH, HEIGHT, sboard);
	  }

	  reached[i] ++;
	  
	  cell c = tilein(x, y, board[i].ant);
	  switch(c.c) {
	  case WILD:
	    message::no(0, "Tried to advance a WILD pattern!");
	    abort();
	  case GOAL:
	    /* ant goes into goal */
	    // message::no(0, "GOAL!");
	    goals ++;
	    oboard[i].ant = NONE;
	    break;
	  case WALL:
	    /* turn right, keep program */
	    oboard[i].ant = dir_cw(board[i].ant);
	    oboard[i].pgm = board[i].pgm;
	    break;
	  case HOLE:
	    /* ant dies */
	    deaths ++;
	    oboard[i].ant = NONE;
	    break;
	  case OPEN:

	    /* save program */
	    oboard[i].pgm = board[i].pgm;

	    /* check what direction the ant we see is facing,
	       relative to our own direction (nb this preserves
	       NONE-ness */
	    dir opponent = reltome(board[i].ant, c.ant);
	    if (0)
	      printf("@ %d/%d (facing %s): opponent's dir is abs: %s rel: %s\n", 
		     x, y,
		     dtos(board[i].ant).c_str(),
		     dtos(c.ant).c_str(),
		     dtos(opponent).c_str());
	    switch(opponent) {
	      /* will walk out of cell */
	    case NONE: oboard[i].ant = NONE;
	      break;
	      /* otherwise, turn according to program */
	    case UP:    oboard[i].ant = relative(board[i].ant, program[board[i].pgm][3]); break;
	    case RIGHT: oboard[i].ant = relative(board[i].ant, program[board[i].pgm][4]); break;
	    case DOWN:  oboard[i].ant = relative(board[i].ant, program[board[i].pgm][5]); break;
	    case LEFT:  oboard[i].ant = relative(board[i].ant, program[board[i].pgm][6]); break;
	    }
	    break;
	  }
	}
	break;
      default:
	oboard[i].c = board[i].c;
	oboard[i].ant = board[i].ant;
	oboard[i].pgm = board[i].pgm;
#ifdef ARROWS
	oboard[i].arrow = board[i].arrow;
#endif
      }
    }
  }
  
  /* swap the two */
  cell * tmp = board;
  board = oboard;
  oboard = tmp;
}

static dir cdir;
static char cpgm;
static void set_ant(int x, int y) {
  int i = y * WIDTH + x;

  board[i].c = OPEN;
  if (cdir == NONE) {
    dir d = (dir)(1 + (random() & 3));

    board[i].ant = d;
    board[i].pgm = random() % 10;
  } else {
    board[i].c = OPEN;
    board[i].ant = cdir;
    board[i].pgm = cpgm;
  }
}

static contents cother;
static void set_other(int x, int y) {
  board[y * WIDTH + x].c = cother;
  board[y * WIDTH + x].ant = NONE;
}

static bool inboard(int x, int y, int & xx, int & yy) {
  x /= (TILEW >> zoomfactor);
  y /= (TILEH >> zoomfactor);

  if (x >= 0 && y >= 0 &&
      x < WIDTH && y < HEIGHT) {
    xx = x;
    yy = y;
    return true;
  } else return false;
}

static void clear_reachable() {
  for(int i = 0; i < (WIDTH * HEIGHT); i ++) {
    reached[i] = 0;
  }
}

static void copyboard(cell * src, cell * dest, int w, int h) {
  int x = w * h;
  for(int i = 0; i < x; i ++) { dest[i] = src[i]; }
}

/* randomize all program slots */
static void randomize_program() {
  for(int i = 0; i < 10; i ++) {
    for(int j = 0; j < 7; j ++) {
      program[i][j] = (dir)(1 + (random() & 3));
    }
  }
}

static void randomize_to_pattern(bool reset_program = true) {

  /* first the programs */
  if (reset_program) {
  for(int i = 0; i < progpatterns; i ++) {
    for(int j = 0; j < 7; j ++) {
      if (progpat[i][j] == NONE)
	program[i][j] = (dir)(1 + (random() & 3));
      else program[i][j] = progpat[i][j];
    }
  }
  }

  if (!progpatterns) {
    
    message::no (0, "There are no programs at all in this "
		 "pattern!\n");

  } else {
    for(int i = 0; i < WIDTH * HEIGHT; i ++) {
      board[i] = pattern[i];
      if (board[i].c == WILD) {
	board[i].c = OPEN;
	/* XXX could place other stuff too (walls) */
	if (random() & 3) {
	  dir d = (dir)(1 + (random() & 3));
	  board[i].ant = d;
	  board[i].pgm = random() % progpatterns;
	} else board[i].ant = NONE;
      } 
    }
  }
}

static void clear_ants() {
  goals = 0;
  deaths = 0;
  for(int i = 0; i < WIDTH * HEIGHT; i ++) {
    board[i].ant = NONE;
    board[i].pgm = 0;
  }
}

static void place_block () {
  for(int y = 2; y < 10; y ++) {
    for(int x = 2; x < 10; x ++) {
      if (random() & 2) set_ant(x, y);
      else board[y * WIDTH + x].ant = NONE;
    }
  }
}

string title;

string readline(istream & s) {
  string o;
  char c;
  while (s.get(c) && c != '\n') {
    // printf("readchar '%c'\n", c);
    if (c != '\r') o += c;
  }
  return o;
}


/* initialize the board to the pattern,
   filling in wildcards where necessary */
static void default_to_pattern() {
  int x = WIDTH * HEIGHT;
  {
    for(int i = 0; i < x; i ++) { 
      board[i] = pattern[i]; 
      if (board[i].c == WILD) {
	board[i].c = OPEN;
	board[i].ant = NONE;
#ifdef ARROWS
	board[i].arrow = NONE;
#endif
	board[i].pgm = 0;
      }
    }
  }

#if 0
  { /* debugging */
    for(int i = 0; i < x; i ++) {
      oboard[i].c = (contents) random ();
      oboard[i].ant = (dir) random ();
      oboard[i].pgm = (char) random ();
#ifdef ARROWS
      oboard[i].arrow = (dir) random ();
#endif
    }
  }
#endif
  
  /* then programs .. */
  {
    for(int i = 0; i < progpatterns; i ++) {
      for(int j = 0; j < 7; j ++) {
	program[i][j] = progpat[i][j];
	if (program[i][j] == NONE) {
	  /* default to NNNWWWW */
	  if (j < 3) program[i][j] = UP;
	  else program[i][j] = LEFT;
	}
      }
    }
  }
}

/* load the board (in antomaton.um format) from
   the specified file, or use the default if the
   file does not exist. */
static void load_pattern(string s) {

  /* always set up the default program,
     since when we load a pattern it may
     not specify all patterns. */
  {
    for(int i = 0; i < 10; i ++) {
      program[i][0] = UP;
      program[i][1] = UP;
      program[i][2] = UP;

      program[i][3] = LEFT;
      program[i][4] = LEFT;
      program[i][5] = LEFT;
      program[i][6] = LEFT;
    }

    program[2][0] = RIGHT;
    program[2][1] = RIGHT;
    program[2][2] = RIGHT;


    program[1][0] = LEFT;
    program[1][1] = LEFT;
    program[1][2] = LEFT;
    program[1][3] = RIGHT;
    program[1][4] = RIGHT;
    program[1][5] = RIGHT;
    program[1][6] = RIGHT;
  }

  
  ifstream f(s.c_str());

  if (f) {
    title = readline(f);
    printf("title: %s\n", title.c_str());
    string p;
    int i = 0;
    while (f >> p) {
      printf("program or width: %s\n", p.c_str());
      if(p.length () != 7) break;
      if(i >= 10) {
	message :: no (0, "too many programs");
	return ;
      }
      setprogrampattern(i, p);
      i ++;
      progpatterns = i;
    }

    /* p holds width */
    WIDTH = atoi(p.c_str());
    f >> p;
    HEIGHT = atoi(p.c_str());
    
    if (WIDTH <= 0 || HEIGHT <= 0) {
      message :: no (0, "bad height/width: " + itos(HEIGHT) + " x " + itos(WIDTH));
      WIDTH = 1;
      HEIGHT = 1;
      return ;
    }

    if (pattern) free(pattern);
    if (board) free(board);
    if (oboard) free(oboard);
    if (sboard) free(sboard);

    pattern  = (cell*) malloc(WIDTH * HEIGHT * sizeof (cell));
    board    = (cell*) malloc(WIDTH * HEIGHT * sizeof (cell));
    oboard   = (cell*) malloc(WIDTH * HEIGHT * sizeof (cell));
    sboard   = (cell*) malloc(WIDTH * HEIGHT * sizeof (cell));
    reached  = (llong*)  calloc(1, WIDTH * HEIGHT * sizeof (llong));

    /* skip rest of line */
    (void) readline(f);

    for(int y = 0; y < HEIGHT; y ++) {
      string l = readline (f);
      printf("THIS LINE: '%s'\n", l.c_str());
      for(int x = 0; x < WIDTH; x ++) {
	int j = y * WIDTH + x;
	pattern[j].ant = NONE;
#ifdef ARROWS
	pattern[j].arrow = NONE;
#endif
	pattern[j].pgm = 0;
	if (l.length () <= (unsigned)(x * 2 + 1)) {
	  message :: no (0, "line #" +
			 itos(y) + " too short! (" + 
			 itos(l.length()) + " chars)");
	  abort ();
	}
	switch(l[x * 2 + 1]) {
	case '*':
	  pattern[j].c = WILD;
	  break;

	case '-':
	case '.':
	case ' ':
	  pattern[j].c = OPEN;
	  break;
	  
	case 'o':
	case 'O':
	  pattern[j].c = HOLE;
	  break;
	    
	case '#':
	  pattern[j].c = WALL;
	  break;

	case '$':
	  pattern[j].c = GOAL;
	  break;

	case '^':
	  pattern[j].c = OPEN;
	  pattern[j].ant = UP;
	  pattern[j].pgm = l[x * 2] - '0';
	  break;

	case '>':
	  pattern[j].c = OPEN;
	  pattern[j].ant = RIGHT;
	  pattern[j].pgm = l[x * 2] - '0';
	  break;

	case 'v':
	  pattern[j].c = OPEN;
	  pattern[j].ant = DOWN;
	  pattern[j].pgm = l[x * 2] - '0';
	  break;

	case '<':
	  pattern[j].c = OPEN;
	  pattern[j].ant = LEFT;
	  pattern[j].pgm = l[x * 2] - '0';
	  break;

	default: {
	  string cs = " ";
	  cs[0] = l[x * 2 + 1];
	  message::no(0, "unexpected char in board?? " RED + cs);
	  abort ();
	}
	}

	if (pattern[j].ant != NONE &&
	    (pattern[j].pgm < 0 ||
	     pattern[j].pgm > 9)) {
	  message :: no (0, "bad program char");
	  pattern[j].pgm = 0;
	}
	
      }
    }

    /* ok, now initialize board to the default instantiation
       of the pattern */

    default_to_pattern();

    f.close();

  } else {
    printf("Warning: Couldn't load %s\n",
	   s.c_str());

    WIDTH = 32;
    HEIGHT = 32;

    if (pattern) free(pattern);
    if (board) free(board);
    if (oboard) free(oboard);
    if (sboard) free(sboard);
    if (reached) free(reached);

    pattern  = (cell*) malloc(WIDTH * HEIGHT * sizeof (cell));
    board    = (cell*) malloc(WIDTH * HEIGHT * sizeof (cell));
    oboard   = (cell*) malloc(WIDTH * HEIGHT * sizeof (cell));
    sboard   = (cell*) malloc(WIDTH * HEIGHT * sizeof (cell));
    reached  = (llong*)  calloc(1, WIDTH * HEIGHT * sizeof (llong));

    {
      for(int i = 0; i < WIDTH * HEIGHT; i ++) {
	pattern[i].c = OPEN;
	pattern[i].ant = NONE;
#ifdef ARROWS
	pattern[i].arrow = NONE;
#endif
	pattern[i].pgm = 0;
      }
    }

    pattern[10].ant = UP;

    default_to_pattern ();
  }


  /* either way, set the zoomfactor */
  /* XXX crappy; use screen size */
  if (WIDTH <= 16 && HEIGHT <= 24) zoomfactor = 0;
  else zoomfactor = 1;

}

struct farm : public drawable {

  virtual void draw () {
    sdlutil::clearsurface(screen, 0);
    drawboard ();

    /* XXX should be option? */
    if (!DO_MOVIE) {
      fon->draw(5, screen->h - (fon->height + 5), itos(iter)
		+ ((solving)?(" / " YELLOW + itos(solves)):((string)"")));
      fon->draw(5, screen->h - (fon->height * 2 + 5), GREEN + itos(goals));
      fon->draw(5, screen->h - (fon->height * 3 + 5), RED + itos(deaths));

      int lmb = T_STOP;
      switch(cdir) {
      case NONE: lmb = T_HEARTFRAMER; break;
      case UP: lmb = T_UP; break;
      case DOWN: lmb = T_DOWN; break;
      case LEFT: lmb = T_LEFT; break;
      case RIGHT: lmb = T_RIGHT; break;
      }

      int rmb = T_STOP;
      switch(cother) {
      case WALL: rmb = T_BLUE; break;
      case HOLE: rmb = T_HOLE; break;
      case GOAL: rmb = T_EXIT; break;
      case OPEN: rmb = T_FLOOR; break;
      default: rmb = T_STOP; break;
      }

      drawing::drawtile(5, screen->h - (fon->height * 3 + 5 + (TILEH >> zoomfactor)),
			lmb, zoomfactor);

      drawing::drawtile(((TILEW >> zoomfactor) + 5) + 5,
			screen->h - (fon->height * 3 + 5 + (TILEH >> zoomfactor)),
			rmb, zoomfactor);
    }

    /* draw programs */
    for(int i = 0; i < 10; i ++) {
      string s = BLUE;
      if ((9 - i) == cpgm) s = YELLOW;

      fon->draw(WIDTH * (TILEW >> zoomfactor) + 5, 
		screen->h - (fon->height * (i + 1) + 5), 
		s + itos(9 - i) + BLUE ": " WHITE + ptos(program[9 - i]));
    }
  }

  void redraw () {
    draw ();

    /* then save as bmp... */
    if (DO_MOVIE) {
      string s = moviebase + itos(movieframe) + ".bmp";
      SDL_SaveBMP(screen, s.c_str());
      movieframe ++;
      /* stop once we hit goal */
      if (goals > 0) DO_MOVIE = 0;
    }
    
    SDL_Flip(screen);
  }

  virtual void screenresize () {
    redraw ();
  }

  void loop (string def) {
    
    srandom(time(0));
    
    cother = WALL;
    cdir = UP;
    cpgm = 0;

    load_pattern(def);
    
    paused = false;
    fog = true;

    int ox = -1;
    int oy = -1;
    for(;;) {
      SDL_Event e;

      while ( SDL_PollEvent(&e) ) {
      
	int key;

	switch(e.type) {
	case SDL_QUIT:
	  return ; 
	  
	case SDL_MOUSEBUTTONDOWN: {
	  SDL_MouseButtonEvent * em = (SDL_MouseButtonEvent*)&e;
	  
	  /* are we clicking on a menu item? */
	    
	  int x = em->x;
	  int y = em->y;

	  int xx, yy;
	  if (inboard(x, y, xx, yy)) {
	    if (em->button == SDL_BUTTON_LEFT) {
	      set_ant(xx, yy);
	      redraw();
	    } else if (em->button == SDL_BUTTON_RIGHT) {
	      set_other(xx, yy);
	      redraw();
	    }
	    ox = xx;
	    oy = yy;
	  }

	  break;
	}

	case SDL_MOUSEMOTION: {
	  SDL_MouseMotionEvent * em = (SDL_MouseMotionEvent*)&e;
	  if ((em->state & SDL_BUTTON(SDL_BUTTON_RIGHT)) ||
	      (em->state & SDL_BUTTON(SDL_BUTTON_LEFT))) {
	    
	    int mousex = em->x;
	    int mousey = em->y;
	    int xx, yy;

	    if (inboard(mousex, mousey, xx, yy) &&
		(xx != ox || yy != oy)) {

	      if (em->state & SDL_BUTTON(SDL_BUTTON_RIGHT)) {
		set_other(xx, yy);
	      } else if (em->state & SDL_BUTTON(SDL_BUTTON_LEFT)) {
		set_ant(xx, yy);
	      }

	      ox = xx;
	      oy = yy;
	      redraw();
	    }
	  }
	  break;
	}
	case SDL_KEYDOWN: {
	  key = e.key.keysym.sym;
	  switch(key) {

	  case SDLK_f: {
	    fog = !fog;
	    redraw ();
	    break;
	  }

	  case SDLK_l: {
	    paused = true;
	    solving = false;
	    clear_reachable ();
	    solves = 0;
	    sitersleft = 0;
	    iter = 0;
	    deaths = 0;
	    string s = prompt::ask(this, "load antfile:");
	    load_pattern(s);
	    redraw();
	    break;
	  }
	  case SDLK_KP_MINUS:
	  case SDLK_MINUS:
	  case SDLK_PAGEUP:
	    zoomfactor ++;
	    if (zoomfactor > 3) zoomfactor = 3;
	    redraw();
	    break;
	    
	  case SDLK_KP_PLUS:
	  case SDLK_PLUS:
	  case SDLK_EQUALS:
	  case SDLK_PAGEDOWN:
	    zoomfactor --;
	    if (zoomfactor < 0) zoomfactor = 0;
	    redraw();
	    break;

	  case SDLK_p:
	    pascii(stdout, program, WIDTH, HEIGHT, board);
	    break;

	  case SDLK_s:
	    clear_reachable ();
	    sitersleft = 0;
	    solves = 0;
	    solving = !solving;
	    /* random direction */
	    cdir = NONE;
	    break;
	    
	  case SDLK_a:{
	    randomize_to_pattern(false);
	    // place_block ();
	    redraw();
	    break;
	  }

	  case SDLK_z: {
	    /* save current drawing as pattern */
	    string tf = "tmp.ant";
	    FILE * tmp = fopen(tf.c_str(), "w");
	    if (tmp) {
	      pascii(tmp, program, WIDTH, HEIGHT, board);
	      fclose(tmp);
	      load_pattern(tf);
	    } else {
	      message::no(this, "couldn't open temp file");
	    }
	    redraw();
	    break;
	  }

	  case SDLK_q:
	    goals = 0;
	    deaths = 0;
	    iter = 0;
	    default_to_pattern();
	    clear_reachable();
	    printf("----\n");
	    redraw();
	    break;

	  case SDLK_e: {
	    randomize_program();
	    redraw();
	    break;
	  }
	  case SDLK_r:
	    /* random */
	    cdir = NONE;
	    redraw();
	    break;

	  case SDLK_h:
	    cother = HOLE;
	    redraw();
	    break;
	  case SDLK_w:
	    cother = WALL;
	    redraw();
	    break;
	  case SDLK_g:
	    cother = GOAL;
	    redraw();
	    break;
	  case SDLK_o:
	    cother = OPEN;
	    redraw();
	    break;

	  case SDLK_u: {
	    string s = prompt::ask(this, "iters per guess: ");
	    SITERSTODO = atoi(s.c_str());
	    if (SITERSTODO == 0) SITERSTODO = 150;
	    printf("For each ant pattern, try %d iterations\n",
		   SITERSTODO);
	    redraw ();
	    break;
	  }

	  case SDLK_RETURN:
	    paused = true;
	    advance ();
	    redraw();
	    break;

	  case SDLK_m: {
	    string m = prompt::ask(this, "movie base name: ");
	    redraw();
	    if (m != "") {
	      moviebase = m;
	      movieframe = 1;
	      DO_MOVIE = 1;
	      paused = false;
	      goals = 0;
	      /* first frame... */
	      redraw ();
	    }
	    break;
	  }

	  case SDLK_SPACE:
	    paused = !paused;
	    redraw();
	    break;
	  case SDLK_ESCAPE:
	    return;

	  case SDLK_UP:    cdir = UP;   redraw(); break;
	  case SDLK_DOWN:  cdir = DOWN; redraw(); break;
	  case SDLK_LEFT:  cdir = LEFT; redraw(); break;
	  case SDLK_RIGHT: cdir = RIGHT; redraw(); break;

	  default: {
	    int uc = e.key.keysym.unicode;

	    /* allow 0-9 to set program */
	    if ((uc & ~0x7F) == 0) {
	      uc &= 0x7F;
	      
	      if (uc >= '0' &&
		  uc <= '9') {

		if (! (e.key.keysym.mod & KMOD_ALT)) {
		  /* select program */
		  // message::no(this, "YES " + itos (uc - '0'));
		  cpgm = (uc - '0');
		} else {
		  /* modify program */
		  int i = uc - '0';
		  prompt p;
		  
		  string np = 
		    prompt::ask(this, 
				"Enter program #" + itos(i) + " " YELLOW,
				ptos(program[(char)i]));
		  setprogram(i, np);
		}
		redraw();
	      }
	    }
	  }
	  }
	  break;
	}
	case SDL_VIDEORESIZE: {
	  SDL_ResizeEvent * eventp = (SDL_ResizeEvent*)&e;
	  screen = sdlutil::makescreen(eventp->w, 
				       eventp->h);
	  screenresize();
	  redraw();
	  break;
	}
	case SDL_VIDEOEXPOSE:
	  redraw();
	  break;
	default: break;
	}
      }

      /* once done with events, loop */

      if (solving) {
	if (goals > 0) {
	  message::quick(this, "Solve success!", "OK", "OK");
	  pascii(stdout, program, WIDTH, HEIGHT, sboard);
	  
	  copyboard(sboard, board, WIDTH, HEIGHT);
	  solving = false;
	  paused = true;
	  redraw();
	}
	
	if (!sitersleft) {
	  solves ++;
	  clear_ants();
	  // randomize_program();
	  // place_block();

	  randomize_to_pattern();

	  copyboard(board, sboard, WIDTH, HEIGHT);

	  goals = 0;
	  deaths = 0;
	  
	  sitersleft = SITERSTODO;
	} else {
	  advance ();
	  if ((iter & 0x3FFF) == 0) redraw ();
	  sitersleft --;
	}

      } else if (!paused) {
	
	advance();
	redraw();
	
      }

    }
  }
};

void antfarm (string def) {

  farm f;
  f.loop (def);

}
