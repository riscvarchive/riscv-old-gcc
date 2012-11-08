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

char* strcpy(char* dst, const char* src)
{
  const char* dst0 = dst;

#if !defined(PREFER_SIZE_OVER_SPEED) && !defined(__OPTIMIZE_SIZE__)
  unsigned long c = getDetectNullConst();
  int misaligned = ((uintptr_t)dst | (uintptr_t)src) & (sizeof(long)-1);
  if (__builtin_expect(!misaligned, 1))
  {
    long* ldst = (long*)dst;
    const long* lsrc = (const long*)src;

    while (!DETECTNULL(*lsrc, c))
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
