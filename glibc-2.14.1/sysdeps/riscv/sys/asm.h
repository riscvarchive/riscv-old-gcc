/* copyright (c) 1997, 1998, 2002, 2003, 2004, 2005
   Free Software Foundation, Inc.
   This file is part of the GNU C Library.
   Contributed by Ralf Baechle <ralf@gnu.org>.

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

#ifndef _SYS_ASM_H
#define _SYS_ASM_H

#ifndef CAT
# ifdef __STDC__
#  define __CAT(str1,str2) str1##str2
# else
#  define __CAT(str1,str2) str1/**/str2
# endif
# define CAT(str1,str2) __CAT(str1,str2)
#endif

/*
 * Macros to handle different pointer/register sizes for 32/64-bit code
 *
 * 64 bit address space isn't used yet, so we may use the R3000 32 bit
 * defines for now.
 */
#ifdef __riscv64
# define PTR .dword
# define PTRSIZE 8
# define PTRLOG 3
#else
# define PTR .word
# define PTRSIZE 4
# define PTRLOG 2
#endif

/*
 * LEAF - declare leaf routine
 */
#define	LEAF(symbol)	\
		.globl	symbol;                         \
		.align	2;                              \
		.type	symbol,@function;               \
symbol:

/*
 * NESTED - declare nested routine entry point
 */
#define	NESTED(symbol, framesize, rpc) LEAF(symbol)

/*
 * END - mark end of function
 */
#ifndef END
# define END(function)                                   \
		.size	function,.-function
#endif

/*
 * EXPORT - export definition of symbol
 */
#define	EXPORT(symbol)                                  \
		.globl	symbol;                         \
symbol:

/*
 * ABS - export absolute symbol
 */
#define	ABS(symbol,value)                               \
		.globl	symbol;                         \
symbol		=	value

#define	PANIC(msg)                                      \
		.set	push;				\
		.set	reorder;                        \
		la	a0,8f;                          \
		jal	panic;                          \
9:		b	9b;                             \
		.set	pop;				\
		TEXT(msg)

/*
 * Print formated string
 */
#define PRINT(string)                                   \
		.set	push;				\
		.set	reorder;                        \
		la	a0,8f;                          \
		jal	printk;                         \
		.set	pop;				\
		TEXT(string)

#define	TEXT(msg)                                       \
		.data;                                  \
8:		.asciiz	msg;                            \
		.previous;

/*
 * Build text tables
 */
#define TTABLE(string)                                  \
		.text;                                  \
		.word	1f;                             \
		.previous;                              \
		.data;                                  \
1:		.asciz	string;                         \
		.previous

/*
 * Stack alignment
 */
#define ALSZ	15
#define ALMASK	~15

/*
 * Size of a register
 */
#ifdef __riscv64
# define SZREG	8
#else
# define SZREG	4
#endif

/*
 * Use the following macros in assemblercode to load/store registers,
 * pointers etc.
 */
#if (SZREG == 4)
# define REG_S sw
# define REG_L lw
#else
# define REG_S sd
# define REG_L ld
#endif

#endif /* sys/asm.h */
