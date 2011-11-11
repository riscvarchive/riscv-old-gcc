/* Copyright (C) 2005, 2006 Free Software Foundation, Inc.
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

#include <tls.h>
#include "tcb-offsets.h"

/* Save the PID value.  */
#define SAVE_PID \
	lui	t0, %hi(PID_OFFSET);                                   	\
	add	t0, t0, tp;                                            	\
	lw	a2, %lo(PID_OFFSET)(t0);/* Load the saved PID.  */	\
	neg	a2, a2;		        /* Negate it.  */		\
	sw	a2, %lo(PID_OFFSET)(t0);/* Store the temporary PID.  */

/* Restore the old PID value in the parent.  */
#define RESTORE_PID \
	beqz	v0, 1f;			/* If we are the parent... */	\
	lui	t0, %hi(PID_OFFSET);                                   	\
	add	t0, t0, tp;                                            	\
	lw	a2, %lo(PID_OFFSET)(t0);/* Load the saved PID.  */	\
	neg	a2, a2;		        /* Re-negate it.  */		\
	sw	a2, %lo(PID_OFFSET)(t0);/* Restore the PID.  */		\
1:

#include <sysdeps/unix/sysv/linux/riscv/vfork.S>
