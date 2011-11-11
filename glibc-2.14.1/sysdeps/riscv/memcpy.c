#include <string.h>

#undef memcpy

void* memcpy(void* dst, const void* src, size_t n)
{
  void* dst0 = dst;
  void* end = dst + n;

  /* are dst and src word-aligned? */
  if ((((long)dst | (long)src) & (sizeof(long)-1)) == 0)
  {
    /* copy 10 words at a time */
    for ( ; dst <= end - 10*sizeof(long); dst += 10*sizeof(long), src += 10*sizeof(long))
    {
      long t0 = *(const long*)(src+0*sizeof(long));
      long t1 = *(const long*)(src+1*sizeof(long));
      long t2 = *(const long*)(src+2*sizeof(long));
      long t3 = *(const long*)(src+3*sizeof(long));
      long t4 = *(const long*)(src+4*sizeof(long));
      long t5 = *(const long*)(src+5*sizeof(long));
      long t6 = *(const long*)(src+6*sizeof(long));
      long t7 = *(const long*)(src+7*sizeof(long));
      long t8 = *(const long*)(src+8*sizeof(long));
      long t9 = *(const long*)(src+9*sizeof(long));
      *(long*)(dst+0*sizeof(long)) = t0;
      *(long*)(dst+1*sizeof(long)) = t1;
      *(long*)(dst+2*sizeof(long)) = t2;
      *(long*)(dst+3*sizeof(long)) = t3;
      *(long*)(dst+4*sizeof(long)) = t4;
      *(long*)(dst+5*sizeof(long)) = t5;
      *(long*)(dst+6*sizeof(long)) = t6;
      *(long*)(dst+7*sizeof(long)) = t7;
      *(long*)(dst+8*sizeof(long)) = t8;
      *(long*)(dst+9*sizeof(long)) = t9;
    }

    /* copy a word at a time */
    for ( ; dst <= end - sizeof(long); dst += sizeof(long), src += sizeof(long))
      *(long*)dst = *(const long*)src;
  }

  /* copy a byte at a time */
  for ( ; dst < end; dst++, src++)
    *(char*)dst = *(const char*)src;

  return dst0;
}

libc_hidden_builtin_def (memcpy)
