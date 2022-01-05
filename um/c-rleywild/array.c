#include "array.h"

/*
platter_t ** arrays = NULL;
index_t narrays = 0;
*/

#define MAXARRAYS 1000000
array_t arrays[MAXARRAYS];
index_t narrays = MAXARRAYS;

/* a NULL */
void
array_alloc_array (array_t * a, size_t sz)
{
  /*
  assert (a);
  assert (a->p == NULL);
  */

  a->sz = sz;
  assert ((a->p = calloc (sizeof (platter_t), sz)) != NULL);
}

/* a non-NULL */
void
array_free_array (array_t * a)
{
  /*
  assert (a);
  assert (a->p != NULL);
  */

  a->sz = 0;
  free (a->p);
  a->p = NULL;
}

/* d NULL, s non-NULL */
void
array_copy_array (array_t * d, array_t * s)
{
  /*
  assert (d);
  assert (d->p == NULL);
  assert (s);
  assert (s->p != NULL);
  */

  array_alloc_array (d, s->sz);
  memcpy (d->p, s->p, sizeof (platter_t) * s->sz);
}

void
array_init ()
{
  index_t i;
  for (i = 0; i < MAXARRAYS; i ++)
  {
    arrays[i].sz = 0;
    arrays[i].p = NULL;
  }
}

index_t
array_alloc (size_t sz)
{
  index_t i;

dprintf ("array_alloc 0\n");
  for (i = 0; i < narrays; i ++)
  {
    if (arrays[i].p == NULL)
    {
      break;
    }
  }

dprintf ("array_alloc 1\n");
  assert (i != narrays);
/*
  if (i == 0)
  {
    i = narrays ++;
dprintf ("arrays pre = %x\n", arrays);
    assert ((arrays = (platter_t **) realloc (arrays, sizeof (platter_t **) * narrays)) != NULL);
dprintf ("arrays pos = %x\n", arrays);
  }
*/

dprintf ("array_alloc 2\n");
dprintf ("arrays = %x, i = %d, sz = %d\n", arrays, i, sz);
  array_alloc_array (&arrays[i], sz);

dprintf ("array_alloc 3\n");
  return i;
}

void
array_free (index_t i)
{
  assert (i != 0 && i < narrays && arrays[i].p != NULL);
  array_free_array (&arrays[i]);
}

void
array_copy (index_t p)
{
  if (p == 0)
  {
    return;
  }
  else
  {
    assert (arrays[0].p != NULL && p < narrays && arrays[p].p != NULL);
    array_free_array (&arrays[0]);
    array_copy_array (&arrays[0], &arrays[p]);
  }
}

platter_t
array_get (index_t p, index_t o)
{
  assert (p < narrays && arrays[p].p != NULL && o < arrays[p].sz);
  return arrays[p].p[o];
}

void
array_set (index_t p, index_t o, platter_t v)
{
  assert (p < narrays && arrays[p].p != NULL && o < arrays[p].sz);
  arrays[p].p[o] = v;
}
