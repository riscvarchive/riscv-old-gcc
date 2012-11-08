#include <string.h>
#include <stdint.h>
#include <limits.h>

#define DETECTNULL(x, c) (~(((((x) & (c)) + (c)) | (x)) | (c)))

/* work around crappy 64b constant generation */
static inline unsigned long getDetectNullConst()
{
  unsigned long y = 0x7F7F7F7F;
#if LONG_MAX == 2147483647L
#elif LONG_MAX == 9223372036854775807L
  asm volatile ("" : "+r"(y));
  y = y << 32 | y;
#else
#error long int is not a 32bit or 64bit type.
#endif
  return y;
}

int strcmp(const char* s1, const char* s2)
{
#if !defined(PREFER_SIZE_OVER_SPEED) && !defined(__OPTIMIZE_SIZE__)
  unsigned long c = getDetectNullConst();
  int misaligned = ((uintptr_t)s1 | (uintptr_t)s2) & (sizeof(long)-1);
  if (__builtin_expect(!misaligned, 1))
  {
      const unsigned long* a1 = (const unsigned long*)s1;
      const unsigned long* a2 = (const unsigned long*)s2;
      while (*a1++ == *a2++)
      {
        /* To get here, *a1 == *a2, thus if we find a null in *a1,
           then the strings must be equal, so return zero.  */
        if (__builtin_expect(DETECTNULL (*(a1-1), c), 1))
          return 0;
      }
      asm volatile("" : "+r"(a1), "+r"(a2)); /* prevent "optimization" */
      a1--;
      a2--;

      /* A difference was detected in last few bytes of s1, so search bytewise */
      s1 = (char*)a1;
      s2 = (char*)a2;
  }
#endif /* not PREFER_SIZE_OVER_SPEED */

  unsigned char c1, c2;
  const unsigned char* us1 = (const unsigned char*)s1;
  const unsigned char* us2 = (const unsigned char*)s2;
  do
  {
    c1 = *us1++;
    c2 = *us2++;
  }
  while (c1 == c2 && c1 != 0);
  return c1 - c2;
}
