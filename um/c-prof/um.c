#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/stat.h>
typedef unsigned int uint;

static uint * ulloc(uint size) {
  uint * r = (uint*)calloc((1 + size), sizeof(uint));
  *r = size;
  return (r + 1);
}

static void ufree(uint * p) {
  free(p - 1);
}

int main (int argc, char ** argv) {
  uint * zero; uint ip = 0; uint reg[8] = {0,0,0,0,0,0,0,0};
  uint * shadow; uint size;

  if (argc < 2) {
    fprintf (stderr, "\nusage:\num program.um\n");
    return -1;
  }
  FILE * f = fopen(argv[1], "rb");
  if (!f) {
    fprintf (stderr, "can't open %s\n", argv[1]);
    return -1;
  }

  {
    /* assume if we opened it, we can stat it */
    struct stat buf;
    if (stat(argv[1], &buf)) return -1;
    else { 
			size = buf.st_size >> 2;
			zero = ulloc(size);
			shadow = ulloc(size);
		}
  }

  /* initialize */
  { 
    int a;
    int n = 4;
    int i = 0;
    while(EOF != (a = fgetc(f))) {
      if (!n--) {
	// test bad endianness:
	// zero[i] = ntohl(zero[i]);
	i++; n = 3; }
      zero[i] <<= 8;
      zero[i]  |= a;
    }
  }

#  define arr(m) (m?(uint*)m:zero)

#  define c w & 7
#  define b (w >> 3) & 7
#  define a (w >> 6) & 7

  /* spin cycle */
  for(;;) {
		uint w;
		shadow[ip]++;
		w = zero[ip++];

    /*
    int c = w & 7;
    int b = (w >> 3) & 7;
    int a = (w >> 6) & 7;
    */

    switch(w >> 28) {
    case 0: if (reg[c]) reg[a] = reg[b]; break;
    case 1: reg[a] = arr(reg[b])[reg[c]]; break;
    case 2: arr(reg[a])[reg[b]] = reg[c]; break;
    case 3: reg[a] = reg[b] + reg[c]; break; 
    case 4: reg[a] = reg[b] * reg[c]; break;
    case 5: reg[a] = reg[b] / reg[c]; break;
    case 6: reg[a] = ~(reg[b] & reg[c]); break;
    case 7: goto END;
    case 8: reg[b] = (uint)ulloc(reg[c]); break;
    case 9: ufree((uint*)reg[c]); break;
    case 10: putchar(reg[c]); fflush(stdout); break;
    case 11: reg[c] = getchar(); break;
    case 12:
      if (reg[b]) {
	ufree(zero);
	size = ((uint*)reg[b])[-1];
	zero = ulloc(size);
	// XXX throwing out profiling information!!!!
	ufree(shadow);
	shadow = ulloc(size);
	memcpy(zero, (uint*)reg[b], size * 4);
      }
      ip = reg[c]; 
      break;
    case 13: reg[7 & (w >> 25)] = w & 0177777777; break;
    }
  }
 END:

	// debrief
	{
		uint i;
		for (i = 0; i < size; i++) {
			fputc (shadow[i] >> 24, stderr);
			fputc ((shadow[i] >> 16) & 0xFF, stderr);
			fputc ((shadow[i] >> 8) & 0xFF, stderr);
			fputc (shadow[i] & 0xFF, stderr);
		}
	}

	return 0;
}
