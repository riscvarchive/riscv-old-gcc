/* Generic system definitions, based on MN10300 definitions.
 *
 * It should be possible to use these on really simple architectures,
 * but it serves more as a starting point for new ports.
 *
 * Copyright (C) 2007 Red Hat, Inc. All Rights Reserved.
 * Written by David Howells (dhowells@redhat.com)
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public Licence
 * as published by the Free Software Foundation; either version
 * 2 of the Licence, or (at your option) any later version.
 */
#ifndef __ASM_GENERIC_SYSTEM_H
#define __ASM_GENERIC_SYSTEM_H

#ifdef __KERNEL__
#ifndef __ASSEMBLY__

#include <linux/irqflags.h>

struct task_struct;
struct thread_struct;

struct task_struct *__switch_to(struct thread_struct *prev,
				struct thread_struct *next,
				struct task_struct *prev_task);

/* context switching is now performed out-of-line in switch_to.S */
#define switch_to(prev, next, last)					\
do {									\
	current->thread.wchan = (u_long) __builtin_return_address(0);	\
	(last) = __switch_to(&(prev)->thread, &(next)->thread, (prev));	\
	mb();								\
	current->thread.wchan = 0;					\
} while (0)

#define arch_align_stack(x) (x)

#define nop() asm volatile ("nop")

#endif /* !__ASSEMBLY__ */

/*
 * Force strict CPU ordering.
 * And yes, this is required on UP too when we're talking
 * to devices.
 *
 * This implementation only contains a compiler barrier.
 */

#define mb()	asm volatile ("": : :"memory")
#define rmb()	mb()
#define wmb()	asm volatile ("": : :"memory")

#ifdef CONFIG_SMP
#define smp_mb()	mb()
#define smp_rmb()	rmb()
#define smp_wmb()	wmb()
#else
#define smp_mb()	barrier()
#define smp_rmb()	barrier()
#define smp_wmb()	barrier()
#endif

#define set_mb(var, value)  do { var = value;  mb(); } while (0)
#define set_wmb(var, value) do { var = value; wmb(); } while (0)

#define read_barrier_depends()		do {} while (0)
#define smp_read_barrier_depends()	do {} while (0)

/*
 * we make sure local_irq_enable() doesn't cause priority inversion
 */
#ifndef __ASSEMBLY__

#ifndef CONFIG_TRACE_IRQFLAGS_SUPPORT

extern unsigned long __local_save_flags(void);
#define local_save_flags(x)			\
do {						\
	(x) = __local_save_flags();		\
} while (0)

extern void local_irq_enable(void);
extern void local_irq_disable(void);
extern void local_irq_restore(unsigned long flags);
extern unsigned long irqs_disabled(void);

#define local_irq_save(x)			\
do {						\
	local_save_flags(x);			\
	local_irq_disable();			\
} while (0)

#endif

static inline
unsigned long __xchg(volatile unsigned long *m, unsigned long val)
{
	unsigned long retval;
	unsigned long flags;

	local_irq_save(flags);
	retval = *m;
	*m = val;
	local_irq_restore(flags);
	return retval;
}

#define xchg(ptr, v)						\
	((__typeof__(*(ptr))) __xchg((unsigned long *)(ptr),	\
				     (unsigned long)(v)))

static inline unsigned long __cmpxchg(volatile unsigned long *m,
				      unsigned long old, unsigned long new)
{
	unsigned long retval;
	unsigned long flags;

	local_irq_save(flags);
	retval = *m;
	if (retval == old)
		*m = new;
	local_irq_restore(flags);
	return retval;
}

#define cmpxchg(ptr, o, n)					\
	((__typeof__(*(ptr))) __cmpxchg((unsigned long *)(ptr), \
					(unsigned long)(o),	\
					(unsigned long)(n)))

#endif /* !__ASSEMBLY__ */

#endif /* __KERNEL__ */
#endif /* __ASM_GENERIC_SYSTEM_H */
