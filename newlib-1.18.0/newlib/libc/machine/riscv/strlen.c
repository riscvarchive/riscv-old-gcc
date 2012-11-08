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

size_t strlen(const char* str)
{
  const char* start = str;

#if !defined(PREFER_SIZE_OVER_SPEED) && !defined(__OPTIMIZE_SIZE__)
  if (__builtin_expect((uintptr_t)str & (sizeof(long)-1), 0))
  {
    while ((uintptr_t)str & (sizeof(long)-1))
    {
      char ch = *str;
      str++;
      if (!ch)
        goto out;
    }
  }

  unsigned long* ls = (unsigned long*)str;
  unsigned long c = getDetectNullConst();
  while (!DETECTNULL(*ls, c))
    ls++;
  str = (char*)ls;
#endif /* not PREFER_SIZE_OVER_SPEED */

  while (*str++)
    ;

out:
  return str - start - 1;
}
