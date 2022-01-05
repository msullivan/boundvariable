#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/stat.h>
typedef unsigned int uint;

float inst_weights [] =
	{ 1.0F, /* CMOV */
	  1.5F, /* UPD */
	  1.5F, /* ASUB */
	  1.0F, /* ADD */
	  1.0F, /* MUL */
	  3.0F, /* DIV */
	  1.0F, /* NAND */
	  1.0F, /* HALT */
	  6.0F, /* ALLOC */
	  6.0F, /* FREE */
	  1.0F, /* WRITE */ /* XXX ? */
	  1.0F, /* READ */ /* XXX ? */
	  2.0F, /* LOADPROG */
	  1.0F, /* LITERAL */
	  999.0F, 999.0F /* ILLEGAL */
	};

uint getword (FILE * f) {
  uint w = 0;
  w = fgetc(f) << 24;
  w |= (fgetc(f) << 16);
  w |= (fgetc(f) << 8);
  w |= (fgetc(f));
  return w;
}

int main (int argc, char ** argv) {
  uint size;
  char **symbols;
  float *res;
  uint bins, i;

  if (argc < 5) {
    fprintf (stderr, "\nusage:\nrunprof program.sym program.um program.prof program.bins \n");
    return -1;
  }

  FILE * sym_f = fopen(argv[1], "r");
  if (!sym_f) {
    fprintf (stderr, "can't open %s\n", argv[1]);
    return -1;
  }

  FILE * um_f = fopen(argv[2], "rb");
  {
    // assume if we opened it, we can stat it 
    struct stat buf;
    if (stat(argv[2], &buf)) return -1;
    size = buf.st_size >> 2;
  }

  fprintf(stderr, "size = %d\n", size);

  symbols = malloc (size * sizeof(char *));
  for (bins = 0; bins < size; bins++) {
    if (!fscanf (sym_f, "%72as", &symbols[bins])) {
      fprintf(stderr, "couldn't scan\n");
      // FIXME this doesn't seem to work
      break;
    } else {
      // printf("sym: %s\n", symbols[bins]);
    }
	  
    if (strlen(symbols[bins]) == 0) {
      break;
    }
  }

  fprintf (stderr, "number of symbols : %d\n", bins);

  res = malloc(bins * sizeof(float));
  
  for(i = 0; i < bins ; i ++) {
    res[i] = 0.0;
  }

  FILE * prof_f = fopen(argv[3], "rb"),
       * bins_f = fopen(argv[4], "rb");

  for (i = 0; i < size; i++) {
    uint count, inst, index;
    float val;

    // read count
    count = getword(prof_f);

    // read inst
    inst = 15 & (getword(um_f) >> 24);
    val = inst_weights[inst];

    // mult by weight
    val *= (float)count;

    // read index
    index = getword(bins_f);
    if (index > bins) {
      fprintf (stderr, "unknown bin index: %u at %d\n", index, i);
    } else {
      // printf("val: %f\n", val);
      res[index] += val;
      // printf("idx %d = bin %ud (%s), value now %f\n", i, index, symbols[index], res[index]);
      // update hist
    }
  }

  /* normalize ... should have commandline flag to turn this on/off */
  {
    float total = 0.0;
    for(i = 0; i < bins; i ++) {
      total += res[i];
    }
    for(i = 0; i < bins; i ++) {
      res[i] /= total;
    }
  }

  /* XXX sort */
  for (i = 0; i < bins; i++) {
    /* filtering less than epsilon */
    if (res[i] > 0.0001) {
      /* report as a percentage */
      printf("%.2f %s\n", res[i] * 100.0, symbols[i]);
    }
  }

  return 0;
}
