void memcpy (int * dst, int * src, int len) {
  int * end = dst + len;
  for (; dst != end; ++dst, ++src) {
    *dst = *src;
  }
}

int TRUE = 1;
int FALSE = 0;
