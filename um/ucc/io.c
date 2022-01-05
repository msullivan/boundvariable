/* Fairly complete implementation of CSO-7767
   "An Input-Output Library for C7sus4"
*/

void print (char *s) {
  for (; *s; ++s)
    {
      putc (*s);
    }
}

int EOF = -1;

int readint () {
  int c = getc ();
  c = (c << 8) | getc ();
  c = (c << 8) | getc ();
  c = (c << 8) | getc ();
  return c;
}


char * itoa (int v, char * str) {
  char * n = str;
  int l = 0;
  
  if (v < 0) {
    *(n++) = 45; /* - */
    v = -v;
  }
  
  {
    int x = v;
    while (x >= 10) {
      ++l;
      x /= 10;
    }
  }
  
  n[l+1] = 0;
  
  while (l >= 0) {
    n[l--] = (v % 10) + 48; /* 0 */
    v /= 10;
  }
  
  return str;
}

