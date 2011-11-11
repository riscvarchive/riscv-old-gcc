#ifndef __ASM_GENERIC_TYPES_H
#define __ASM_GENERIC_TYPES_H

/*
 * int-ll64 is used on all 32 bit architectures and on x86-64,
 * so use it as a reasonable default.
 */
#include <asm-generic/int-ll64.h>

#ifndef __ASSEMBLY__

typedef unsigned short umode_t;

#endif /* __ASSEMBLY__ */

/*
 * There seems to be no way of detecting this automatically from user
 * space, so 64 bit architectures should override this in their types.h.
 */
#ifndef __BITS_PER_LONG
#define __BITS_PER_LONG 32
#endif

/*
 * These aren't exported outside the kernel to avoid name space clashes
 */
#ifdef __KERNEL__

#ifdef CONFIG_64BIT
#define BITS_PER_LONG 64
#else
#define BITS_PER_LONG 32
#endif /* CONFIG_64BIT */

#if BITS_PER_LONG != __BITS_PER_LONG
#error cannot determine word size
#endif

#ifndef __ASSEMBLY__

/*
 * DMA addresses may be larger than pointers, but not smaller.
 * Do not define the dma64_addr_t type, which never really
 * worked.
 */
#if defined(CONFIG_64BIT) || defined(CONFIG_PHYS_64BIT)
typedef u64 dma_addr_t;
#else
typedef u32 dma_addr_t;
#endif /* 64 bit DMA pointer */

#endif /* __ASSEMBLY__ */

#endif /* __KERNEL__ */

#endif /* _ASM_GENERIC_TYPES_H */
