#include <string.h>
#include <stdint.h>
#include <limits.h>

#if LONG_MAX == 2147483647L
# define MASK 0x7F7F7F7F
#elif LONG_MAX == 9223372036854775807L
# define MASK 0x7F7F7F7F7F7F7F7F
#endif
#define DETECTNULL(x) (~(((((x) & MASK) + MASK) | (x)) | MASK))

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
  while (!DETECTNULL(*ls))
    ls++;
  str = (char*)ls;
#endif /* not PREFER_SIZE_OVER_SPEED */

  while (*str++)
    ;

out:
  return str - start - 1;
}
