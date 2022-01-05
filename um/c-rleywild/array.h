#ifndef ARRAY_H
#define ARRAY_H

#include "common.h"

void array_init ();
index_t array_alloc (size_t sz);
void array_free (index_t i);
void array_copy (index_t platter);
platter_t array_get (index_t platter, index_t offset);
void array_set (index_t platter, index_t offset, platter_t value);

#endif
