/* Copyright (C) 1996, 1997, 2000, 2002, 2003, 2004
	Free Software Foundation, Inc.
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

#include <sysdep.h>
#include <sys/asm.h>

ENTRY (__longjmp)
	REG_L ra,  0*SZREG(a0)
	REG_L sp,  1*SZREG(a0)
	REG_L tp,  2*SZREG(a0)
	REG_L s0,  3*SZREG(a0)
	REG_L s1,  4*SZREG(a0)
	REG_L s2,  5*SZREG(a0)
	REG_L s3,  6*SZREG(a0)
	REG_L s4,  7*SZREG(a0)
	REG_L s5,  8*SZREG(a0)
	REG_L s6,  9*SZREG(a0)
	REG_L s7, 10*SZREG(a0)
	REG_L s8, 11*SZREG(a0)
	REG_L s9, 12*SZREG(a0)

#ifdef __riscv_hard_float
	REG_L t0, 13*SZREG(a0)

	fld  fs0, 8*0 + 14*SZREG(a0)
	fld  fs1, 8*1 + 14*SZREG(a0)
	fld  fs2, 8*2 + 14*SZREG(a0)
	fld  fs3, 8*3 + 14*SZREG(a0)
	fld  fs4, 8*4 + 14*SZREG(a0)
	fld  fs5, 8*5 + 14*SZREG(a0)
	fld  fs6, 8*6 + 14*SZREG(a0)
	fld  fs7, 8*7 + 14*SZREG(a0)
	fld  fs8, 8*8 + 14*SZREG(a0)
	fld  fs9, 8*9 + 14*SZREG(a0)

	mtfsr t0
#endif

	sltu t0, a1, 1    # t0 = (a1 == 0)
	add  v0, t0, a1   # v0 = (a1 == 0 ? 1 : a1)
	ret

END(__longjmp)
