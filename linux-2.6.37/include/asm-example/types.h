#ifndef __TYPES_H
#define __TYPES_H

// YUNSUP
#define __BITS_PER_LONG 64

#include <asm-generic/types.h>

// YUNSUP
#ifdef __KERNEL__
# ifndef __ASSEMBLY__
#  include <asm-generic/bitops/__fls.h>
# endif
#endif

#endif // __TYPES_H
