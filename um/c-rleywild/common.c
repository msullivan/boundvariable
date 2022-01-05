#include "common.h"

platter_t reg[8];
console_t con;
index_t platter, offset;

void
dprintf (const char * format, ...)
{
  if (0)
  {
    va_list arg_ptr;

    va_start (arg_ptr, format);
    vfprintf (stdout, format, arg_ptr);
    va_end (arg_ptr);
    fflush (stdout);
  }
}

void
fail (const char * file, const int line, const char * fn, const char * s)
{
  printf ("%s:%d:%s:(assert: %s) platter = %d, offset = %d\n", file, line, fn, s, platter, offset);
  exit (-1);
}
