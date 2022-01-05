/*
  Universal Machine Emulator

  This is a sample implementation of a UM-like machine, but certainly not a
  reference implementation.  It was written to demonstrate my new language,
  C7SUS4, and its more intuitive syntax.

  Due to the Spartan nature of C7SUS4, some improvements were required.  This
  version differs from standard UM implementations in that it first reads the
  length of the UM binary (in terms of the number of instructions) from the
  console.  It then reads the binary itself (also from the console).  Any
  following input is treated as input to the application program.
*/

/* requires std.c and io.c */

int * ulloc(int size) {
  /* Assume malloc clears data */
  int * r = (int*)malloc((1 + size) * sizeof(int));
  *r = size;
  return (r + 1);
};

void ufree(int * p) {
  free(p - 1);
};

(* should be an option?! *)
int MAX_HEAP = 1024 * 64;

int main () {
  int ip = 0; 
  int * reg = malloc (8 * sizeof (int));
  int ** mem = malloc (MAX_HEAP * sizeof (int *));
  int * freequeue = malloc (MAX_HEAP * sizeof (int));
  int * freeend = freequeue + MAX_HEAP;
  
  {
    /* setup free queue */
    int i;
    for (i = 0; i < MAX_HEAP; ++i) {
      freequeue[i] = i;
    }
  }
  /* skip the zero array */
  ++freequeue;
  
  {
    /* read size */
    int size = readint();
    int i = 0;
    int * zero;
    
    if (size == EOF) return -1;
    else mem[0] = ulloc(size);
    
    /* initialize */
    zero = mem[0];
    while(i < size) {
      zero[i++] = readint();
    }
  }

  print("loaded.\n");

  /* spin cycle */
  for(;;) {
    int w = (mem[0])[ip++];

    int c = w & 7;
    int b = (w >> 3) & 7;
    int a = (w >> 6) & 7;

    switch(w >> 28) {
    case 0: if (reg[c]) reg[a] = reg[b]; break;
    case 1: reg[a] = (mem[reg[b]])[reg[c]]; break;
    case 2: (mem[reg[a]])[reg[b]] = reg[c]; break;
    case 3: reg[a] = reg[b] + reg[c]; break; 
    case 4: reg[a] = reg[b] * reg[c]; break;
    case 5: reg[a] = reg[b] / reg[c]; break;
    case 6: reg[a] = ~(reg[b] & reg[c]); break;
    case 7: print ("halted.\n"); return 0;
    case 8: 
      {
        int i;
        if (freequeue == freeend) {
          print ("heap exhausted.\n");
          return -1;
        }
        i = *(freequeue++);
        mem[i] = ulloc(reg[c]);
        reg[b] = i;
        break;
      }
    case 9: 
      {
        int i = reg[c];
        /* free the memory */
        ufree(mem[i]);
        /* record the empty slot */
        *(--freequeue) = i;
        break;
      }
    case 10: putc(reg[c]); break;
    case 11: reg[c] = getc(); break;
    case 12:
      {
        if (reg[b]) {
          int *m = mem[reg[b]];
          int size = m[-1];
          ufree(mem[0]);
          mem[0] = ulloc(size);
          memcpy(mem[0], m, size);
        }
        ip = reg[c]; 
        break;
      }
    case 13: reg[7 & (w >> 25)] = w & 33554431; break;
    }
  }
}

