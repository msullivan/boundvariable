#ifndef COMMON_H
#define COMMON_H

#include <byteswap.h>
#include <stdarg.h>
#include <stdint.h>
#include <stdio.h>
#include <unistd.h>

#define SUB(r, o, l) (((r)>>(o)) & ((1<<l) - 1))
#define SUB_OP(r) SUB(r, 28, 4)
#define SUB_A(r) SUB(r, 6, 3)
#define SUB_B(r) SUB(r, 3, 3)
#define SUB_C(r) SUB(r, 0, 3)
#define SUB_A2(r) SUB(r, 25, 3)
#define SUB_IMM(r) SUB(r, 0, 25)

typedef unsigned int index_t;
typedef unsigned int platter_t;
typedef unsigned char console_t;
typedef struct _array_t {
  size_t sz;
  platter_t * p;
} array_t;

extern platter_t reg[8];
extern console_t con;
extern index_t platter, offset;

void fail (const char * file, int line, const char * fn, const char * s);
void dprintf (const char * format, ...);

#define assert(cond) \
  do \
  { \
    if ((cond) == 0) \
    { \
      fail (__FILE__, __LINE__, __PRETTY_FUNCTION__, __STRING (cond)); \
    } \
  } \
  while (0)

#endif
