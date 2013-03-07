#include <string.h>
#include <stdint.h>
#include <limits.h>

#if LONG_MAX == 2147483647L
# define MASK 0x7F7F7F7F
#elif LONG_MAX == 9223372036854775807L
# define MASK 0x7F7F7F7F7F7F7F7F
#endif
#define DETECTNULL(x) (~(((((x) & MASK) + MASK) | (x)) | MASK))

char* strcpy(char* dst, const char* src)
{
  const char* dst0 = dst;

#if !defined(PREFER_SIZE_OVER_SPEED) && !defined(__OPTIMIZE_SIZE__)
  int misaligned = ((uintptr_t)dst | (uintptr_t)src) & (sizeof(long)-1);
  if (__builtin_expect(!misaligned, 1))
  {
    long* ldst = (long*)dst;
    const long* lsrc = (const long*)src;

    while (!DETECTNULL(*lsrc))
      *ldst++ = *lsrc++;

    dst = (char*)ldst;
    src = (const char*)lsrc;
  }
#endif /* not PREFER_SIZE_OVER_SPEED */

  char ch;
  do
  {
    ch = *src;
    src++;
    dst++;
    *(dst-1) = ch;
  } while(ch);

  return dst0;
}
