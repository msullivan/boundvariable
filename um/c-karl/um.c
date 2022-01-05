
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define RFILE_SIZE 8

typedef unsigned long int32;

typedef struct 
{
   int32 size;
   int32 data[];
} array;


int32 rfile[RFILE_SIZE];
array *code;
int32 pc;


FILE *upload_file;


#define INITIAL_CODEBUF_SIZE 3100000   /* at least 1 */

array *load_program(char *fname)
{
   FILE *file;
   array *arr;

   int32 size, bufsize;
   int32 *m, *curr, x;
   int ch;


   file = fopen(fname, "r");
   if (file == NULL)
      {
      printf("Error reading program file.\n");
      exit(-1);
      }

   size = 1;  /* leave room for size field */
   bufsize = INITIAL_CODEBUF_SIZE;
   m = (int32 *)malloc(bufsize * sizeof(int32));
   curr = m+size;

   while (1)
      {
      ch = getc(file);
      if (ch == EOF)
	 break;

      if (size >= bufsize)
	 {
	 bufsize *= 2;
	 m = (int32 *)realloc(m, bufsize * sizeof(int32));
	 curr = m+size;
	 }

      x = (int32)ch << 24;
      ch = getc(file);
      if (ch == EOF)
	 {
	 printf("Misformed program file.\n");
	 exit(-1);
	 }
      x = x | ((int32)ch << 16);
      ch = getc(file);
      if (ch == EOF)
	 {
	 printf("Misformed program file.\n");
	 exit(-1);
	 }
      x = x | ((int32)ch << 8);
      ch = getc(file);
      if (ch == EOF)
	 {
	 printf("Misformed program file.\n");
	 exit(-1);
	 }
      x = x | ((int32)ch);

      *curr = x;
      curr++;
      size++;
      }

   fclose(file);
   
   arr = (array *)m;
   arr->size = size - 1;

   printf("Program loaded.\n");

   return(arr);
}


void initialize(char *fname)
{
   int32 i;

   /* register file */
   for (i = 0; i < RFILE_SIZE; i++)
      rfile[i] = 0;

   /* memory */
   code = load_program(fname);
   pc = 0;

   upload_file = NULL;

}


int inputline(char *buf, int size)
/* size >= 1 */
{
   int i = 0;
   int ch;

   while (1)
      {
      ch = getchar();
      
      if (ch == EOF)
	 return(0);

      if (ch == '\n')
	 break;

      if (i < size-1)
	 {
	 buf[i] = (char)ch;
	 i++;
	 }
      }
   /* i <= size-1 */
   
   buf[i] = 0;
   return(1);
}



#define INPUT_BUFFER_SIZE 80

void console(void)
{
   char buf[INPUT_BUFFER_SIZE];

   while (1)
      {
      printf("> ");
      if (!inputline(buf, INPUT_BUFFER_SIZE))
	 break;
      
      if (strcmp(buf, "exit") == 0)
	 break;

      if (strcmp(buf, "upload") == 0)
	 {
	 printf("upload file: ");
	 inputline(buf, INPUT_BUFFER_SIZE);

	 upload_file = fopen(buf, "r");
	 if (upload_file == NULL)
	    {
	    printf("Unable to open file for reading.\n");
	    continue;
	    }

	 break;
	 }

      }
   
}



#define MOVE  0x00000000
#define LOAD  0x10000000
#define STORE 0x20000000
#define ADD   0x30000000
#define MULT  0x40000000
#define DIV   0x50000000
#define NAND  0x60000000
#define HALT  0x70000000
#define ALLOC 0x80000000
#define FREE  0x90000000
#define OUT   0xA0000000
#define IN    0xB0000000
#define JUMP  0xC0000000
#define IMM   0xD0000000

#define OPMASK 0xF0000000
#define AMASK  0x000001C0
#define BMASK  0x00000038
#define CMASK  0x00000007

#define IMMDESTMASK 0x0E000000
#define IMMMASK     0x01FFFFFF


#define findarray(ptr) (ptr == 0 ? code : (array *)ptr)


void execute(void)
{
   int32 inst;
   int32 a, b, c, x, y;
   array *arr;

   while (1)
      {
      inst = code->data[pc];
      pc++;
      
      switch (inst & OPMASK)
	 {
      case MOVE:

	 a = (inst & AMASK) >> 6;
	 b = (inst & BMASK) >> 3;
	 c = inst & CMASK;
	 
	 if (rfile[c] == 0)
	    break;

	 rfile[a] = rfile[b];

	 break;

      case LOAD:

	 a = (inst & AMASK) >> 6;
	 b = (inst & BMASK) >> 3;
	 c = inst & CMASK;
	 
	 x = rfile[b];
	 arr = findarray(x);
	 y = rfile[c];
	 
	 rfile[a] = arr->data[y];

	 break;

      case STORE:
	 
	 a = (inst & AMASK) >> 6;
	 b = (inst & BMASK) >> 3;
	 c = inst & CMASK;
	 
	 x = rfile[a];
	 arr = findarray(x);
	 y = rfile[b];

	 arr->data[y] = rfile[c];

	 break;

      case ADD:
	 
	 a = (inst & AMASK) >> 6;
	 b = (inst & BMASK) >> 3;
	 c = inst & CMASK;
	 
	 rfile[a] = rfile[b] + rfile[c];

	 break;

      case MULT:
	 
	 a = (inst & AMASK) >> 6;
	 b = (inst & BMASK) >> 3;
	 c = inst & CMASK;
	 
	 rfile[a] = rfile[b] * rfile[c];

	 break;

      case DIV:
	 
	 a = (inst & AMASK) >> 6;
	 b = (inst & BMASK) >> 3;
	 c = inst & CMASK;

	 x = rfile[c];
	 
	 rfile[a] = rfile[b] / x;

	 break;

      case NAND:

	 a = (inst & AMASK) >> 6;
	 b = (inst & BMASK) >> 3;
	 c = inst & CMASK;
	 
	 rfile[a] = ~(rfile[b] & rfile[c]);
	 break;

      case HALT:

         exit(0);

      case ALLOC:

	 b = (inst & BMASK) >> 3;
	 c = inst & CMASK;

	 x = rfile[c];
	 arr = (array *)malloc((x+1) * sizeof(int32));
	 arr->size = x;
	 memset(arr->data, 0, x * sizeof(int32));

	 rfile[b] = (int32)arr;

	 break;

      case FREE:

	 c = inst & CMASK;

	 x = rfile[c];

	 free((array *)x);

	 break;

      case OUT:

	 c = inst & CMASK;
	 
	 x = rfile[c];
	 putchar(x);

	 break;

      case IN:

	 c = inst & CMASK;

	 while (1)
	    {

	    if (upload_file != NULL)
	       {
	       /* uploading a file */
	       x = getc(upload_file);
	    
	       if (x != EOF)
		  break;

	       fclose(upload_file);
	       }

	    x = getchar();
	    
	    if (x != 27)
	       break;

	    console();

	    }

	 rfile[c] = (x == EOF ? -1 : x);

	 break;

      case JUMP:

	 b = (inst & BMASK) >> 3;
	 c = inst & CMASK;
	 
	 x = rfile[b];

	 if (x == 0)
	    {
	    pc = rfile[c];
	    break;
	    }

	 free(code);

	 arr = (array *)x;
	 y = arr->size;
	 code = (array *)malloc((y+1) * sizeof(int32));
	 code->size = y;
	 memcpy(code->data, arr->data, y*sizeof(int32));

	 pc = rfile[c];

	 break;

      case IMM:

	 a = (inst & IMMDESTMASK) >> 25;
	 x = inst & IMMMASK;
	 rfile[a] = x;

	 break;

      default:

	 printf("Illegal instruction.\n");
	 exit(1);

	 }
      }
}


int main(int argc, char *argv[])
{
   if (argc != 2)
      {
      printf("Usage: um <program name>\n");
      exit(-1);
      }

   initialize(argv[1]);
   execute();
}
