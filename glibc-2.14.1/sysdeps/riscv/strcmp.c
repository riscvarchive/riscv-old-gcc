#include <string.h>
#include <stdint.h>

#define STRCMP2B(s1, s2, last) do { \
    char c10 = *(s1), c11 = *((s1)+1), c20 = *(s2), c21 = *((s2)+1); \
    if (__builtin_expect(c10 != c20 || c10 == 0, 0)) \
      return c10 - c20; \
    if (last || __builtin_expect(c11 != c21 || c11 == 0, 0)) \
      return c11 - c21; \
  } while(0)

#undef strcmp
int strcmp(const char* s1, const char* s2)
{
  int misaligned = ((uintptr_t)s1 | (uintptr_t)s2) & (sizeof(long)-1);
  if (__builtin_expect(!misaligned, 1))
  {
      const unsigned long* a1 = (const unsigned long*)s1;
      const unsigned long* a2 = (const unsigned long*)s2;

      while (*a1++ == *a2++)
      {
        /* To get here, *a1 == *a2, thus if we find a null in *a1,
           then the strings must be equal, so return zero.  */
        if (__builtin_expect(__libc_detect_null(*(a1-1)), 1))
          return 0;
      }
      asm volatile ("" : "+r"(a1), "+r"(a2)); /* prevent "optimization" */

      s1 = (const char*)a1, s2 = (const char*)a2;

      if (sizeof(long) == 8)
      {
	STRCMP2B(s1-8, s2-8, 0);
	STRCMP2B(s1-6, s2-6, 0);
      }
      STRCMP2B(s1-4, s2-4, 0);
      STRCMP2B(s1-2, s2-2, 1);
  }

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
libc_hidden_def(strcmp)
