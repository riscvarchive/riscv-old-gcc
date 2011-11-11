/* Set current context.
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

/* int setcontext (const ucontext_t *ucp) */

	.text
LEAF (__setcontext)

	addi    sp, sp, -SZREG
	REG_S	a0, 0(sp)

	/* Check for the magic flag.  */
	li	v0, 1
	REG_L	v1, (0 * SZREG + MCONTEXT_GREGS)(a0)	/* zero */
	bne	v0, v1, 98f

/* rt_sigprocmask (SIG_SETMASK, &ucp->uc_sigmask, NULL, _NSIG8) */
	li	a3, _NSIG8
	move	a2, zero
	add     a1, a0, UCONTEXT_SIGMASK
	li	a0, SIG_SETMASK

	li	v0, SYS_ify (rt_sigprocmask)
	syscall

	REG_L   v0, 0(sp)
	addi    sp, sp, SZREG
	bnez	a3, 99f

#ifdef __riscv_hard_float
	lw	v1, MCONTEXT_FSR(v0)

	fld	fs0, (20 * 8 + MCONTEXT_FPREGS)(v0)
	fld	fs1, (21 * 8 + MCONTEXT_FPREGS)(v0)
	fld	fs2, (22 * 8 + MCONTEXT_FPREGS)(v0)
	fld	fs3, (23 * 8 + MCONTEXT_FPREGS)(v0)
	fld	fs4, (24 * 8 + MCONTEXT_FPREGS)(v0)
	fld	fs5, (25 * 8 + MCONTEXT_FPREGS)(v0)
	fld	fs6, (26 * 8 + MCONTEXT_FPREGS)(v0)
	fld	fs7, (27 * 8 + MCONTEXT_FPREGS)(v0)
	fld	fs8, (28 * 8 + MCONTEXT_FPREGS)(v0)
	fld	fs9, (29 * 8 + MCONTEXT_FPREGS)(v0)

	mtfsr	v1
#endif /* __mips_hard_float */

	/* Note the contents of argument registers will be random
	   unless makecontext() has been called.  */
	REG_L	ra, (1 * SZREG + MCONTEXT_GREGS)(v0)
	REG_L	a0, (4 * SZREG + MCONTEXT_GREGS)(v0)
	REG_L	a1, (5 * SZREG + MCONTEXT_GREGS)(v0)
	REG_L	a2, (6 * SZREG + MCONTEXT_GREGS)(v0)
	REG_L	a3, (7 * SZREG + MCONTEXT_GREGS)(v0)
	REG_L	a4, (8 * SZREG + MCONTEXT_GREGS)(v0)
	REG_L	a5, (9 * SZREG + MCONTEXT_GREGS)(v0)
	REG_L	a6, (10 * SZREG + MCONTEXT_GREGS)(v0)
	REG_L	a7, (11 * SZREG + MCONTEXT_GREGS)(v0)
	REG_L	s0, (20 * SZREG + MCONTEXT_GREGS)(v0)
	REG_L	s1, (21 * SZREG + MCONTEXT_GREGS)(v0)
	REG_L	s2, (22 * SZREG + MCONTEXT_GREGS)(v0)
	REG_L	s3, (23 * SZREG + MCONTEXT_GREGS)(v0)
	REG_L	s4, (24 * SZREG + MCONTEXT_GREGS)(v0)
	REG_L	s5, (25 * SZREG + MCONTEXT_GREGS)(v0)
	REG_L	s6, (26 * SZREG + MCONTEXT_GREGS)(v0)
	REG_L	s7, (27 * SZREG + MCONTEXT_GREGS)(v0)
	REG_L	s8, (28 * SZREG + MCONTEXT_GREGS)(v0)
	REG_L	s9, (29 * SZREG + MCONTEXT_GREGS)(v0)
	REG_L	sp, (30 * SZREG + MCONTEXT_GREGS)(v0)
	REG_L	tp, (31 * SZREG + MCONTEXT_GREGS)(v0)
	REG_L	t7, MCONTEXT_PC(v0)
	addi    sp, sp, SZREG

	move	v0, zero
	jr	t7

98:
	/* This is a context obtained from a signal handler.
	   Perform a full restore by pushing the context
	   passed onto a simulated signal frame on the stack
	   and call the signal return syscall as if a signal
	   handler exited normally.  */
	addi    sp, sp, -((RT_SIGFRAME_SIZE + ALSZ) & ALMASK)

	/* Only ucontext is referred to from rt_sigreturn,
	   copy it.  */
	addi    t1, sp, RT_SIGFRAME_UCONTEXT
	li	t3, ((UCONTEXT_SIZE + SZREG - 1) / SZREG) - 1
0:
	REG_L	t2, (a0)
	addi	t3, t3, -1
	addi    t1, t1, SZREG
	addi    a0, a0, SZREG
	REG_S	t2, -SZREG(t1)
	bgez	t3, 0b

/* rt_sigreturn () -- no arguments, sp points to struct rt_sigframe.  */
	li	v0, SYS_ify (rt_sigreturn)
	syscall

	/* Restore the stack and fall through to the error
	   path.  Successful rt_sigreturn never returns to
	   its calling place.  */
	addi    sp, sp, ((RT_SIGFRAME_SIZE + ALSZ) & ALMASK)

99:	PIC_J(__syscall_error)

PSEUDO_END (__setcontext)

weak_alias (__setcontext, setcontext)
