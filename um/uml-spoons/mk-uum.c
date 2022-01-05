#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/stat.h>

int main (int argc, char ** argv) {

  FILE * fin = fopen(argv[1], "rb");
  if (!fin) {
    fprintf (stderr, "can't open %s\n", argv[1]);
    return -1;
  }

  FILE * fout = fopen(argv[2], "wb");
  if (!fout) {
    fprintf (stderr, "can't open %s\n", argv[2]);
    return -1;
  }

  {
    /* assume if we opened it, we can stat it */
    struct stat buf;
    int length;

    if (stat(argv[1], &buf)) return -1;
    else {
      length = buf.st_size >> 2;
      printf ("um is %d inst long\n", length);
      fputc ((length >> 24), fout);
      fputc (((length >> 16) & 0xFF), fout);
      fputc (((length >> 8) & 0xFF), fout);
      fputc ((length & 0xFF), fout);
    }
  }

  {
    int a;
    while(EOF != (a = fgetc(fin))) {
      fputc(a, fout);
    }
  }

  return 0;
}
