/* Save current context.
   Copyright (C) 2009 Free Software Foundation, Inc.
   This file is part of the GNU C Library.
   Contributed by Maciej W. Rozycki <macro@codesourcery.com>.

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
   Software Foundation, 51 Franklin Street - Fifth Floor, Boston, MA
   02110-1301, USA.  */

#include <sysdep.h>
#include <sys/asm.h>

#include "ucontext_i.h"

/* int getcontext (ucontext_t *ucp) */

	.text
LEAF (__getcontext)
	/* Store a magic flag.	*/
	li	v1, 1
	REG_S	v1, ( 0 * SZREG + MCONTEXT_GREGS)(a0)	/* zero */

	REG_S	ra, MCONTEXT_PC(a0)
	REG_S	ra, ( 1 * SZREG + MCONTEXT_GREGS)(a0)
	REG_S	s0, (20 * SZREG + MCONTEXT_GREGS)(a0)
	REG_S	s1, (21 * SZREG + MCONTEXT_GREGS)(a0)
	REG_S	s2, (22 * SZREG + MCONTEXT_GREGS)(a0)
	REG_S	s3, (23 * SZREG + MCONTEXT_GREGS)(a0)
	REG_S	s4, (24 * SZREG + MCONTEXT_GREGS)(a0)
	REG_S	s5, (25 * SZREG + MCONTEXT_GREGS)(a0)
	REG_S	s6, (26 * SZREG + MCONTEXT_GREGS)(a0)
	REG_S	s7, (27 * SZREG + MCONTEXT_GREGS)(a0)
	REG_S	s8, (28 * SZREG + MCONTEXT_GREGS)(a0)
	REG_S	s9, (29 * SZREG + MCONTEXT_GREGS)(a0)
	REG_S	sp, (30 * SZREG + MCONTEXT_GREGS)(a0)
	REG_S	tp, (31 * SZREG + MCONTEXT_GREGS)(a0)

#ifdef __riscv_hard_float
  mffsr v1

	fsd	fs0, (20 * 8 + MCONTEXT_FPREGS)(a0)
	fsd	fs1, (21 * 8 + MCONTEXT_FPREGS)(a0)
	fsd	fs2, (22 * 8 + MCONTEXT_FPREGS)(a0)
	fsd	fs3, (23 * 8 + MCONTEXT_FPREGS)(a0)
	fsd	fs4, (24 * 8 + MCONTEXT_FPREGS)(a0)
	fsd	fs5, (25 * 8 + MCONTEXT_FPREGS)(a0)
	fsd	fs6, (26 * 8 + MCONTEXT_FPREGS)(a0)
	fsd	fs7, (27 * 8 + MCONTEXT_FPREGS)(a0)
	fsd	fs8, (28 * 8 + MCONTEXT_FPREGS)(a0)
	fsd	fs9, (29 * 8 + MCONTEXT_FPREGS)(a0)

	sw	v1, MCONTEXT_FSR(a0)
#endif /* __mips_hard_float */

/* rt_sigprocmask (SIG_BLOCK, NULL, &ucp->uc_sigmask, _NSIG8) */
	li	a3, _NSIG8
	add     a2, a0, UCONTEXT_SIGMASK
	move	a1, zero
	li	a0, SIG_BLOCK

	li	v0, SYS_ify (rt_sigprocmask)
	syscall
	bnez	a3, 99f

	move	v0, zero
	ret

99:	PIC_J(__syscall_error)

PSEUDO_END (__getcontext)

weak_alias (__getcontext, getcontext)
