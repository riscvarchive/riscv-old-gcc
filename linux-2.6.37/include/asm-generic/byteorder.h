#ifndef _ASM_BYTEORDER_H
#define _ASM_BYTEORDER_H

#include <asm/types.h>

#if defined(__GNUC__) && !defined(__STRICT_ANSI__) || defined(__KERNEL__)
#  define __BYTEORDER_HAS_U64__
#  define __SWAB_64_THRU_32__
#endif

#if defined(__LITTLE_ENDIAN)
#  include <linux/byteorder/little_endian.h>
#elif defined(__BIG_ENDIAN)
#  include <linux/byteorder/big_endian.h>
#else
#  error need to know endianess
#endif

#endif /* _ASM_BYTEORDER_H */
