/* Low-level functions for atomic operations. Mips version.
   Copyright (C) 2005 Free Software Foundation, Inc.
   This file is part of the GNU C Library.

   The GNU C Library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public
   License as published by the Free Software Foundation; either
   version 2.1 of the License, or (at your option) any later version.

   The GNU C Library is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Lesser General Public License for more details.

   You should have received a copy of the GNU Lesser General Public
   License along with the GNU C Library; if not, write to the Free
   Software Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
   02111-1307 USA.  */

#ifndef _MIPS_BITS_ATOMIC_H
#define _MIPS_BITS_ATOMIC_H 1

#include <inttypes.h>
#include <sgidefs.h>

typedef int32_t atomic32_t;
typedef uint32_t uatomic32_t;
typedef int_fast32_t atomic_fast32_t;
typedef uint_fast32_t uatomic_fast32_t;

typedef int64_t atomic64_t;
typedef uint64_t uatomic64_t;
typedef int_fast64_t atomic_fast64_t;
typedef uint_fast64_t uatomic_fast64_t;

typedef intptr_t atomicptr_t;
typedef uintptr_t uatomicptr_t;
typedef intmax_t atomic_max_t;
typedef uintmax_t uatomic_max_t;

#define asm_amo(which, res, mem, value) \
  asm volatile (which "\t%0, %1, 0(%2)" : "=r"(res) : "r"(value), "r"(mem))

/* Atomic compare and exchange. */

#define atomic_compare_and_exchange_val_acq(mem, newval, oldval)         \
  ({ __sync_synchronize();                      \
     __sync_val_compare_and_swap(mem, oldval, newval); })

#define atomic_compare_and_exchange_val_rel(mem, newval, oldval)         \
  ({ typeof(*mem) __prev;                       \
     __prev = __sync_val_compare_and_swap(mem, value);  \
     __sync_synchronize();                      \
     __prev; })

/* Atomic exchange (without compare).  */

#define atomic_exchange_acq(mem, value)         \
  ({ typeof(*mem) __prev;                       \
      if (sizeof(*mem) == 4)				\
	asm_amo("amoswap.w.aq", __prev, mem, value);	\
      else if(sizeof(*mem) == 8)			\
	asm_amo("amoswap.d.aq", __prev, mem, value);	\
      else						\
	abort();					\
     __prev; })

#define atomic_exchange_rel(mem, value)         \
  ({ typeof(*mem) __prev;                       \
      if (sizeof(*mem) == 4)				\
	asm_amo("amoswap.w.rl", __prev, mem, value);	\
      else if(sizeof(*mem) == 8)			\
	asm_amo("amoswap.d.rl", __prev, mem, value);	\
      else						\
	abort();					\
     __prev; })


/* Atomically add value and return the previous (unincremented) value.  */

#define atomic_exchange_and_add(mem, value)             \
  ({ typeof(*mem) __prev;                               \
      if (sizeof(*mem) == 4)				\
	asm_amo("amoadd.w", __prev, mem, value);	\
      else if(sizeof(*mem) == 8)			\
	asm_amo("amoadd.d", __prev, mem, value);	\
      else						\
	abort();					\
     __prev; })

#define catomic_exchange_and_add(mem, value)		\
  atomic_exchange_and_add(mem, value)

#define atomic_bit_test_set(mem, bit)                   \
  ({ typeof(*mem) __prev;                               \
     typeof(*mem) __mask = (typeof(*mem))1 << (bit);    \
      if (sizeof(*mem) == 4)				\
	asm_amo("amoor.w", __prev, mem, __mask);	\
      else if(sizeof(*mem) == 8)			\
	asm_amo("amoor.d", __prev, mem, __mask);	\
      else						\
	abort();					\
     __prev & __mask; })

#define atomic_max(mem, value)		        	\
  ({  typeof(*mem) __prev;                    		\
      if (sizeof(*mem) == 4)				\
	asm_amo("amomaxu.w", __prev, mem, value);	\
      else if(sizeof(*mem) == 8)			\
	asm_amo("amomaxu.d", __prev, mem, value);	\
      else						\
	abort();					\
     __prev; })

#define catomic_max(mem, value) atomic_max(mem, value)

#define atomic_min(mem, value)		        	\
  ({  typeof(*mem) __prev;                    		\
      if (sizeof(*mem) == 4)				\
	asm_amo("amominu.w", __prev, mem, value);	\
      else if(sizeof(*mem) == 8)			\
	asm_amo("amominu.d", __prev, mem, value);	\
      else						\
	abort();					\
     __prev; })

#define atomic_full_barrier() __sync_synchronize()

#endif /* bits/atomic.h */
