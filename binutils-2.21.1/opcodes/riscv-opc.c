/* mips-opc.c -- MIPS opcode list.
   Copyright 1993, 1994, 1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002
   2003, 2004, 2005, 2007, 2008, 2009  Free Software Foundation, Inc.
   Contributed by Ralph Campbell and OSF
   Commented and modified by Ian Lance Taylor, Cygnus Support
   Extended for MIPS32 support by Anders Norlander, and by SiByte, Inc.
   MIPS-3D, MDMX, and MIPS32 Release 2 support added by Broadcom
   Corporation (SiByte).

   This file is part of the GNU opcodes library.

   This library is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 3, or (at your option)
   any later version.

   It is distributed in the hope that it will be useful, but WITHOUT
   ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
   or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public
   License for more details.

   You should have received a copy of the GNU General Public License
   along with this file; see the file COPYING.  If not, write to the
   Free Software Foundation, 51 Franklin Street - Fifth Floor, Boston,
   MA 02110-1301, USA.  */

#include <stdio.h>
#include "sysdep.h"
#include "opcode/riscv.h"

/* Short hand so the lines aren't too long.  */

/* The order of overloaded instructions matters.  Label arguments and
   register arguments look the same. Instructions that can have either
   for arguments must apear in the correct order in this table for the
   assembler to pick the right one. In other words, entries with
   immediate operands must apear after the same instruction with
   registers.

   Because of the lookup algorithm used, entries with the same opcode
   name must be contiguous.
 
   Many instructions are short hand for other instructions (i.e., The
   jal <register> instruction is short for jalr <register>).  */

#define WR_xd INSN_WRITE_GPR_D
#define WR_ra INSN_WRITE_GPR_RA
#define WR_fd INSN_WRITE_FPR_D
#define RD_xs1 INSN_READ_GPR_S
#define RD_xs2 INSN_READ_GPR_T
#define RD_fs1 INSN_READ_FPR_S
#define RD_fs2 INSN_READ_FPR_T
#define RD_fs3 INSN_READ_FPR_R

#define MASK_RS (OP_MASK_RS << OP_SH_RS)
#define MASK_RT (OP_MASK_RT << OP_SH_RT)
#define MASK_RD (OP_MASK_RD << OP_SH_RD)
#define MASK_IMM (OP_MASK_IMMEDIATE << OP_SH_IMMEDIATE)
#define MASK_RM (OP_MASK_RM << OP_SH_RM)

const struct riscv_opcode riscv_builtin_opcodes[] =
{
/* These instructions appear first so that the disassembler will find
   them first.  The assemblers uses a hash table based on the
   instruction name anyhow.  */
/* name,    args,	match,	    mask,	pinfo,          	pinfo2,		membership */
{"unimp",   "",         0, 0xffffffff,  0 },
{"nop",     "",         MATCH_ADDI, MASK_ADDI | MASK_RD | MASK_RS | MASK_IMM,  0 },
{"li",     "d,j",      MATCH_ADDI, MASK_ADDI | MASK_RS,  WR_xd }, /* addi */
{"li",     "d,I",	0,    (int) M_LI,	INSN_MACRO },
{"move",    "d,s",	MATCH_ADDI, MASK_ADDI | MASK_IMM,	 WR_xd|RD_xs1 },
{"b",       "p",	MATCH_BEQ, MASK_BEQ | MASK_RS | MASK_RT,	 0 },/* beq 0,0 */

{"addw",     "d,s,t",	MATCH_ADDW, MASK_ADDW,	 WR_xd|RD_xs1|RD_xs2 },
{"addw",    "d,s,j",	MATCH_ADDIW, MASK_ADDIW,	 WR_xd|RD_xs1 },
{"addiw",    "d,s,j",	MATCH_ADDIW, MASK_ADDIW,	 WR_xd|RD_xs1 },
{"fadd.s",   "D,S,T",	MATCH_FADD_S | MASK_RM, MASK_FADD_S | MASK_RM,	 WR_fd|RD_fs1|RD_fs2 },
{"fadd.s",   "D,S,T,m",	MATCH_FADD_S, MASK_FADD_S,	 WR_fd|RD_fs1|RD_fs2 },
{"fadd.d",   "D,S,T",	MATCH_FADD_D | MASK_RM, MASK_FADD_D | MASK_RM,	 WR_fd|RD_fs1|RD_fs2 },
{"fadd.d",   "D,S,T,m",	MATCH_FADD_D, MASK_FADD_D,	 WR_fd|RD_fs1|RD_fs2 },
{"fsub.d",   "D,S,T",	MATCH_FSUB_D | MASK_RM, MASK_FSUB_D | MASK_RM,	 WR_fd|RD_fs1|RD_fs2 },
{"fsub.d",   "D,S,T,m",	MATCH_FSUB_D, MASK_FSUB_D,	 WR_fd|RD_fs1|RD_fs2 },
{"fsub.s",   "D,S,T",	MATCH_FSUB_S | MASK_RM, MASK_FSUB_S | MASK_RM,	 WR_fd|RD_fs1|RD_fs2 },
{"fsub.s",   "D,S,T,m",	MATCH_FSUB_S, MASK_FSUB_S,	 WR_fd|RD_fs1|RD_fs2 },
{"fmul.d",   "D,S,T",	MATCH_FMUL_D | MASK_RM, MASK_FMUL_D | MASK_RM,	 WR_fd|RD_fs1|RD_fs2 },
{"fmul.d",   "D,S,T,m",	MATCH_FMUL_D, MASK_FMUL_D,	 WR_fd|RD_fs1|RD_fs2 },
{"fmul.s",   "D,S,T",	MATCH_FMUL_S | MASK_RM, MASK_FMUL_S | MASK_RM,	 WR_fd|RD_fs1|RD_fs2 },
{"fmul.s",   "D,S,T,m",	MATCH_FMUL_S, MASK_FMUL_S,	 WR_fd|RD_fs1|RD_fs2 },
{"fdiv.d",   "D,S,T",	MATCH_FDIV_D | MASK_RM, MASK_FDIV_D | MASK_RM,	 WR_fd|RD_fs1|RD_fs2 },
{"fdiv.d",   "D,S,T,m",	MATCH_FDIV_D, MASK_FDIV_D,	 WR_fd|RD_fs1|RD_fs2 },
{"fdiv.s",   "D,S,T",	MATCH_FDIV_S | MASK_RM, MASK_FDIV_S | MASK_RM,	 WR_fd|RD_fs1|RD_fs2 },
{"fdiv.s",   "D,S,T,m",	MATCH_FDIV_S, MASK_FDIV_S,	 WR_fd|RD_fs1|RD_fs2 },
{"fsqrt.d",  "D,S",	MATCH_FSQRT_D | MASK_RM, MASK_FSQRT_D | MASK_RM,  WR_fd|RD_fs1 },
{"fsqrt.d",  "D,S,m",	MATCH_FSQRT_D, MASK_FSQRT_D,  WR_fd|RD_fs1 },
{"fsqrt.s",  "D,S",	MATCH_FSQRT_S | MASK_RM, MASK_FSQRT_S | MASK_RM,  WR_fd|RD_fs1 },
{"fsqrt.s",  "D,S,m",	MATCH_FSQRT_S, MASK_FSQRT_S,  WR_fd|RD_fs1 },
{"fmadd.s",   "D,S,T,R",	MATCH_FMADD_S | MASK_RM, MASK_FMADD_S | MASK_RM,	 WR_fd|RD_fs1|RD_fs2|RD_fs3 },
{"fmadd.s",  "D,S,T,R,m",	MATCH_FMADD_S, MASK_FMADD_S,	 WR_fd|RD_fs1|RD_fs2|RD_fs3 },
{"fmadd.d",   "D,S,T,R",	MATCH_FMADD_D | MASK_RM, MASK_FMADD_D | MASK_RM,	 WR_fd|RD_fs1|RD_fs2|RD_fs3 },
{"fmadd.d",  "D,S,T,R,m",	MATCH_FMADD_D, MASK_FMADD_D,	 WR_fd|RD_fs1|RD_fs2|RD_fs3 },
{"fnmadd.s",   "D,S,T,R",	MATCH_FNMADD_S | MASK_RM, MASK_FNMADD_S | MASK_RM,	 WR_fd|RD_fs1|RD_fs2|RD_fs3 },
{"fnmadd.s", "D,S,T,R,m",	MATCH_FNMADD_S, MASK_FNMADD_S,	 WR_fd|RD_fs1|RD_fs2|RD_fs3 },
{"fnmadd.d",   "D,S,T,R",	MATCH_FNMADD_D | MASK_RM, MASK_FNMADD_D | MASK_RM,	 WR_fd|RD_fs1|RD_fs2|RD_fs3 },
{"fnmadd.d", "D,S,T,R,m",	MATCH_FNMADD_D, MASK_FNMADD_D,	 WR_fd|RD_fs1|RD_fs2|RD_fs3 },
{"fmsub.s",   "D,S,T,R",	MATCH_FMSUB_S | MASK_RM, MASK_FMSUB_S | MASK_RM,	 WR_fd|RD_fs1|RD_fs2|RD_fs3 },
{"fmsub.s",  "D,S,T,R,m",	MATCH_FMSUB_S, MASK_FMSUB_S,	 WR_fd|RD_fs1|RD_fs2|RD_fs3 },
{"fmsub.d",   "D,S,T,R",	MATCH_FMSUB_D | MASK_RM, MASK_FMSUB_D | MASK_RM,	 WR_fd|RD_fs1|RD_fs2|RD_fs3 },
{"fmsub.d",  "D,S,T,R,m",	MATCH_FMSUB_D, MASK_FMSUB_D,	 WR_fd|RD_fs1|RD_fs2|RD_fs3 },
{"fnmsub.s",   "D,S,T,R",	MATCH_FNMSUB_S | MASK_RM, MASK_FNMSUB_S | MASK_RM,	 WR_fd|RD_fs1|RD_fs2|RD_fs3 },
{"fnmsub.s", "D,S,T,R,m",	MATCH_FNMSUB_S, MASK_FNMSUB_S,	 WR_fd|RD_fs1|RD_fs2|RD_fs3 },
{"fnmsub.d",   "D,S,T,R",	MATCH_FNMSUB_D | MASK_RM, MASK_FNMSUB_D | MASK_RM,	 WR_fd|RD_fs1|RD_fs2|RD_fs3 },
{"fnmsub.d", "D,S,T,R,m",	MATCH_FNMSUB_D, MASK_FNMSUB_D,	 WR_fd|RD_fs1|RD_fs2|RD_fs3 },
{"amoadd.d",		"d,t,0(b)",	MATCH_AMOADD_D, MASK_AMOADD_D,	 WR_xd|RD_xs1|RD_xs2 },
{"amoswap.d",		"d,t,0(b)",	MATCH_AMOSWAP_D, MASK_AMOSWAP_D,	 WR_xd|RD_xs1|RD_xs2 },
{"amoand.d",		"d,t,0(b)",	MATCH_AMOAND_D, MASK_AMOAND_D,	 WR_xd|RD_xs1|RD_xs2 },
{"amoor.d",		"d,t,0(b)",	MATCH_AMOOR_D, MASK_AMOOR_D,	 WR_xd|RD_xs1|RD_xs2 },
{"amomax.d",		"d,t,0(b)",	MATCH_AMOMAX_D, MASK_AMOMAX_D,	 WR_xd|RD_xs1|RD_xs2 },
{"amomaxu.d",		"d,t,0(b)",	MATCH_AMOMAXU_D, MASK_AMOMAXU_D,	 WR_xd|RD_xs1|RD_xs2 },
{"amomin.d",		"d,t,0(b)",	MATCH_AMOMIN_D, MASK_AMOMIN_D,	 WR_xd|RD_xs1|RD_xs2 },
{"amominu.d",		"d,t,0(b)",	MATCH_AMOMINU_D, MASK_AMOMINU_D,	 WR_xd|RD_xs1|RD_xs2 },
{"amoadd.w",		"d,t,0(b)",	MATCH_AMOADD_W, MASK_AMOADD_W,	 WR_xd|RD_xs1|RD_xs2 },
{"amoswap.w",		"d,t,0(b)",	MATCH_AMOSWAP_W, MASK_AMOSWAP_W,	 WR_xd|RD_xs1|RD_xs2 },
{"amoand.w",		"d,t,0(b)",	MATCH_AMOAND_W, MASK_AMOAND_W,	 WR_xd|RD_xs1|RD_xs2 },
{"amoor.w",		"d,t,0(b)",	MATCH_AMOOR_W, MASK_AMOOR_W,	 WR_xd|RD_xs1|RD_xs2 },
{"amomax.w",		"d,t,0(b)",	MATCH_AMOMAX_W, MASK_AMOMAX_W,	 WR_xd|RD_xs1|RD_xs2 },
{"amomaxu.w",		"d,t,0(b)",	MATCH_AMOMAXU_W, MASK_AMOMAXU_W,	 WR_xd|RD_xs1|RD_xs2 },
{"amomin.w",		"d,t,0(b)",	MATCH_AMOMIN_W, MASK_AMOMIN_W,	 WR_xd|RD_xs1|RD_xs2 },
{"amominu.w",		"d,t,0(b)",	MATCH_AMOMINU_W, MASK_AMOMINU_W,	 WR_xd|RD_xs1|RD_xs2 },
{"and",     "d,s,t",	MATCH_AND, MASK_AND,	 WR_xd|RD_xs1|RD_xs2 },
{"and",    "d,s,j",	MATCH_ANDI, MASK_ANDI,	 WR_xd|RD_xs1 },
{"andi",    "d,s,j",	MATCH_ANDI, MASK_ANDI,	 WR_xd|RD_xs1 },
/* b is at the top of the table.  */
/* bal is at the top of the table.  */
{"beqz",    "s,p",	MATCH_BEQ, MASK_BEQ | MASK_RT,	 RD_xs1 },
{"beq",     "s,t,p",	MATCH_BEQ, MASK_BEQ,	 RD_xs1|RD_xs2 },
{"blez",    "t,p",	MATCH_BGE, MASK_BGE | MASK_RS,	 RD_xs2 },
{"bgez",    "s,p",	MATCH_BGE, MASK_BGE | MASK_RT,	 RD_xs1 },
{"ble",     "t,s,p",	MATCH_BGE, MASK_BGE,	 RD_xs1|RD_xs2 },
{"bleu",     "t,s,p",	MATCH_BGEU, MASK_BGEU,	 RD_xs1|RD_xs2 },
{"bge",     "s,t,p",	MATCH_BGE, MASK_BGE,	 RD_xs1|RD_xs2 },
{"bgeu",     "s,t,p",	MATCH_BGEU, MASK_BGEU,	 RD_xs1|RD_xs2 },
{"bltz",    "s,p",	MATCH_BLT, MASK_BLT | MASK_RT,	 RD_xs1 },
{"bgtz",    "t,p",	MATCH_BLT, MASK_BLT | MASK_RS,	 RD_xs2 },
{"blt",     "s,t,p",	MATCH_BLT, MASK_BLT,	 RD_xs1|RD_xs2 },
{"bltu",     "s,t,p",	MATCH_BLTU, MASK_BLTU,	 RD_xs1|RD_xs2 },
{"bgt",     "t,s,p",	MATCH_BLT, MASK_BLT,	 RD_xs1|RD_xs2 },
{"bgtu",     "t,s,p",	MATCH_BLTU, MASK_BLTU,	 RD_xs1|RD_xs2 },
{"bnez",    "s,p",	MATCH_BNE, MASK_BNE | MASK_RT,	 RD_xs1 },
{"bne",     "s,t,p",	MATCH_BNE, MASK_BNE,	 RD_xs1|RD_xs2 },
{"feq.d",   "d,S,T",    MATCH_FEQ_D, MASK_FEQ_D,  WR_xd|RD_fs1|RD_fs2 },
{"feq.s",   "d,S,T",    MATCH_FEQ_S, MASK_FEQ_S,  WR_xd|RD_fs1|RD_fs2 },
{"flt.d",   "d,S,T",    MATCH_FLT_D, MASK_FLT_D,  WR_xd|RD_fs1|RD_fs2 },
{"flt.s",   "d,S,T",    MATCH_FLT_S, MASK_FLT_S,  WR_xd|RD_fs1|RD_fs2 },
{"fle.d",   "d,S,T",    MATCH_FLE_D, MASK_FLE_D,  WR_xd|RD_fs1|RD_fs2 },
{"fle.s",   "d,S,T",    MATCH_FLE_S, MASK_FLE_S,  WR_xd|RD_fs1|RD_fs2 },
/* CW4010 instructions which are aliases for the cache instruction.  */
{"fcvt.d.l", "D,s",	MATCH_FCVT_D_L | MASK_RM, MASK_FCVT_D_L | MASK_RM,	 WR_fd|RD_xs1 },
{"fcvt.d.l", "D,s,m",	MATCH_FCVT_D_L, MASK_FCVT_D_L,	 WR_fd|RD_xs1 },
{"fcvt.d.s", "D,S",	MATCH_FCVT_D_S, MASK_FCVT_D_S | MASK_RM,	 WR_fd|RD_fs1 },
{"fcvt.d.w", "D,s",	MATCH_FCVT_D_W, MASK_FCVT_D_W | MASK_RM,	 WR_fd|RD_xs1 },
{"fcvt.s.l", "D,s",	MATCH_FCVT_S_L | MASK_RM, MASK_FCVT_S_L | MASK_RM,	 WR_fd|RD_xs1 },
{"fcvt.s.l", "D,s,m",	MATCH_FCVT_S_L, MASK_FCVT_S_L,	 WR_fd|RD_xs1 },
{"fcvt.s.d", "D,S",	MATCH_FCVT_S_D | MASK_RM, MASK_FCVT_S_D | MASK_RM,	 WR_fd|RD_fs1 },
{"fcvt.s.d", "D,S,m",	MATCH_FCVT_S_D, MASK_FCVT_S_D,	 WR_fd|RD_fs1 },
{"fcvt.s.w", "D,s",	MATCH_FCVT_S_W | MASK_RM, MASK_FCVT_S_W | MASK_RM,	 WR_fd|RD_xs1 },
{"fcvt.s.w", "D,s,m",	MATCH_FCVT_S_W, MASK_FCVT_S_W,	 WR_fd|RD_xs1 },
{"fcvt.d.lu", "D,s",	MATCH_FCVT_D_LU | MASK_RM, MASK_FCVT_D_L | MASK_RM,	 WR_fd|RD_xs1 },
{"fcvt.d.lu", "D,s,m",	MATCH_FCVT_D_LU, MASK_FCVT_D_LU,	 WR_fd|RD_xs1 },
{"fcvt.d.wu", "D,s",	MATCH_FCVT_D_WU, MASK_FCVT_D_WU | MASK_RM,	 WR_fd|RD_xs1 },
{"fcvt.s.lu", "D,s",	MATCH_FCVT_S_LU | MASK_RM, MASK_FCVT_S_L | MASK_RM,	 WR_fd|RD_xs1 },
{"fcvt.s.lu", "D,s,m",	MATCH_FCVT_S_LU, MASK_FCVT_S_LU,	 WR_fd|RD_xs1 },
{"fcvt.s.wu", "D,s",	MATCH_FCVT_S_WU | MASK_RM, MASK_FCVT_S_W | MASK_RM,	 WR_fd|RD_xs1 },
{"fcvt.s.wu", "D,s,m",	MATCH_FCVT_S_WU, MASK_FCVT_S_WU,	 WR_fd|RD_xs1 },
{"add",    "d,s,t",	MATCH_ADD, MASK_ADD,  WR_xd|RD_xs1|RD_xs2 },
{"add",   "d,s,j",	MATCH_ADDI, MASK_ADDI,  WR_xd|RD_xs1 },
{"addi",   "d,s,j",	MATCH_ADDI, MASK_ADDI,  WR_xd|RD_xs1 },
{"di",      "",		MATCH_DI, MASK_DI | MASK_RD,	 0 },
{"di",      "d",	MATCH_DI, MASK_DI,	 WR_xd },
{"div",    "d,s,t",	MATCH_DIV, MASK_DIV,  WR_xd|RD_xs1|RD_xs2 },
{"divw",    "d,s,t",	MATCH_DIVW, MASK_DIVW,  WR_xd|RD_xs1|RD_xs2 },
{"divu",    "d,s,t",	MATCH_DIVU, MASK_DIVU,  WR_xd|RD_xs1|RD_xs2 },
{"divuw",    "d,s,t",	MATCH_DIVUW, MASK_DIVUW,  WR_xd|RD_xs1|RD_xs2 },
{"la",     "d,A(b)",	0,    (int) M_LA_AB,	INSN_MACRO },
{"mffsr",   "d",	MATCH_MFFSR, MASK_MFFSR,  WR_xd },
{"mtfsr",   "s",	MATCH_MTFSR, MASK_MTFSR | MASK_RD,  RD_xs1 },
{"mtfsr",   "d,s",	MATCH_MTFSR, MASK_MTFSR,  WR_xd|RD_xs1 },
{"mfpcr",   "d,E",	MATCH_MFPCR, MASK_MFPCR,  WR_xd },
{"mtpcr",   "s,E",	MATCH_MTPCR, MASK_MTPCR,  RD_xs1 },
{"mftx.s",   "d,S",	MATCH_MFTX_S, MASK_MFTX_S,  WR_xd|RD_fs1 },
{"mftx.s",   "d,s",	MATCH_MFTX_S, MASK_MFTX_S,  WR_xd|RD_fs1 },
{"mxtf.s",   "D,s",	MATCH_MXTF_S, MASK_MXTF_S,  WR_fd|RD_xs1 },
{"mxtf.s",   "d,s",	MATCH_MXTF_S, MASK_MXTF_S,  WR_fd|RD_xs1 },
{"mftx.d",   "d,S",	MATCH_MFTX_D, MASK_MFTX_D,  WR_xd|RD_fs1 },
{"mftx.d",   "d,s",	MATCH_MFTX_D, MASK_MFTX_D,  WR_xd|RD_fs1 },
{"mxtf.d",   "D,s",	MATCH_MXTF_D, MASK_MXTF_D,  WR_fd|RD_xs1 },
{"mxtf.d",   "d,s",	MATCH_MXTF_D, MASK_MXTF_D,  WR_fd|RD_xs1 },
{"mul",    "d,s,t",	MATCH_MUL, MASK_MUL,  WR_xd|RD_xs1|RD_xs2 },
{"mulh",   "d,s,t",	MATCH_MULH, MASK_MULH,  WR_xd|RD_xs1|RD_xs2 },
{"mulhu",  "d,s,t",	MATCH_MULHU, MASK_MULHU,  WR_xd|RD_xs1|RD_xs2 },
{"mulhsu", "d,s,t",	MATCH_MULHSU, MASK_MULHSU,  WR_xd|RD_xs1|RD_xs2 },
{"neg",    "d,t",	MATCH_SUB, MASK_SUB | MASK_RS,	 WR_xd|RD_xs2 }, /* sub 0 */
{"neg",    "d,t",	MATCH_SUBW, MASK_SUBW | MASK_RS,	 WR_xd|RD_xs2 }, /* subw 0 */
{"sll",    "d,s,t",	 MATCH_SLL, MASK_SLL,	 WR_xd|RD_xs1|RD_xs2 },
{"sll",    "d,s,>",	 MATCH_SLLI, MASK_SLLI,	 WR_xd|RD_xs1 },
{"slli",    "d,s,>",	 MATCH_SLLI, MASK_SLLI,	 WR_xd|RD_xs1 },
{"srl",    "d,s,t",	 MATCH_SRL, MASK_SRL,	 WR_xd|RD_xs1|RD_xs2 },
{"srl",    "d,s,>",	 MATCH_SRLI, MASK_SRLI,	 WR_xd|RD_xs1 },
{"srli",    "d,s,>",	 MATCH_SRLI, MASK_SRLI,	 WR_xd|RD_xs1 },
{"sra",    "d,s,t",	 MATCH_SRA, MASK_SRA,	 WR_xd|RD_xs1|RD_xs2 },
{"sra",    "d,s,>",	 MATCH_SRAI, MASK_SRAI,	 WR_xd|RD_xs1 },
{"srai",    "d,s,>",	 MATCH_SRAI, MASK_SRAI,	 WR_xd|RD_xs1 },
{"sub",    "d,s,t",	MATCH_SUB, MASK_SUB,	 WR_xd|RD_xs1|RD_xs2 },
{"ei",      "",		MATCH_EI, MASK_EI | MASK_RD,	 0 },
{"ei",      "d",	MATCH_EI, MASK_EI,	 WR_xd },
{"eret",    "",     MATCH_ERET, MASK_ERET,  0 },
{"cflush",  "",	MATCH_CFLUSH, MASK_CFLUSH,	 0 },
{"ret",      "",	MATCH_JALR_R | (LINK_REG << OP_SH_RS), MASK_JALR_R | MASK_RD | MASK_RS | MASK_IMM,	 WR_xd|RD_xs1 },
{"jr.r",      "s",	MATCH_JALR_R, MASK_JALR_R | MASK_RD | MASK_IMM,	 WR_xd|RD_xs1 },
{"jr.r",      "s,j",	MATCH_JALR_R, MASK_JALR_R | MASK_RD,	 WR_xd|RD_xs1 },
{"j.r",      "s",	MATCH_JALR_R, MASK_JALR_R | MASK_RD | MASK_IMM,	 WR_xd|RD_xs1 }, /* jr */
{"j.r",      "s,j",	MATCH_JALR_R, MASK_JALR_R | MASK_RD,	 WR_xd|RD_xs1 }, /* jr */
{"jr",      "s",	MATCH_JALR_J, MASK_JALR_J | MASK_RD | MASK_IMM,	 WR_xd|RD_xs1 },
{"jr",      "s,j",	MATCH_JALR_J, MASK_JALR_J | MASK_RD,	 WR_xd|RD_xs1 },
{"j",       "s",	0,    (int) M_J,	INSN_MACRO },
{"j",       "a",	MATCH_J, MASK_J,	 0 },
{"jalr",    "s",	MATCH_JALR_C | (LINK_REG << OP_SH_RD), MASK_JALR_C | MASK_RD | MASK_IMM,	 WR_xd|RD_xs1 },
{"jalr",    "s,j",	MATCH_JALR_C | (LINK_REG << OP_SH_RD), MASK_JALR_C | MASK_RD,	 WR_xd|RD_xs1 },
{"jalr",    "d,s",	MATCH_JALR_C, MASK_JALR_C | MASK_IMM,	 WR_xd|RD_xs1 },
{"jalr",    "d,s,j",	MATCH_JALR_C, MASK_JALR_C,	 WR_xd|RD_xs1 },
{"jalr.j",    "s",	MATCH_JALR_J | (LINK_REG << OP_SH_RD), MASK_JALR_J | MASK_RD | MASK_IMM,	 WR_xd|RD_xs1 },
{"jalr.j",    "s,j",	MATCH_JALR_J | (LINK_REG << OP_SH_RD), MASK_JALR_J | MASK_RD,	 WR_xd|RD_xs1 },
{"jalr.j",    "d,s",	MATCH_JALR_J, MASK_JALR_J | MASK_IMM,	 WR_xd|RD_xs1 },
{"jalr.j",    "d,s,j",	MATCH_JALR_J, MASK_JALR_J,	 WR_xd|RD_xs1 },
{"jalr.r",    "s",	MATCH_JALR_R | (LINK_REG << OP_SH_RD), MASK_JALR_R | MASK_RD | MASK_IMM,	 WR_xd|RD_xs1 },
{"jalr.r",    "s,j",	MATCH_JALR_R | (LINK_REG << OP_SH_RD), MASK_JALR_R | MASK_RD,	 WR_xd|RD_xs1 },
{"jalr.r",    "d,s",	MATCH_JALR_R, MASK_JALR_R | MASK_IMM,	 WR_xd|RD_xs1 },
{"jalr.r",    "d,s,j",	MATCH_JALR_R, MASK_JALR_R,	 WR_xd|RD_xs1 },
/* SVR4 PIC code requires special handling for jal, so it must be a
   macro.  */
{"jal",     "a",	MATCH_JAL, MASK_JAL,	 WR_ra },
{"lb",      "d,o(b)",	MATCH_LB, MASK_LB,	 WR_xd|RD_xs1 },
{"lbu",     "d,o(b)",	MATCH_LBU, MASK_LBU,	 WR_xd|RD_xs1 },
{"lh",      "d,o(b)",	MATCH_LH, MASK_LH,	 WR_xd|RD_xs1 },
{"lhu",     "d,o(b)",	MATCH_LHU, MASK_LHU,	 WR_xd|RD_xs1 },
{"lw",      "d,o(b)",	MATCH_LW, MASK_LW,	 WR_xd|RD_xs1 },
{"lwu",     "d,o(b)",	MATCH_LWU, MASK_LWU,	 WR_xd|RD_xs1 },
{"ld",	    "d,o(b)", MATCH_LD, MASK_LD,  WR_xd|RD_xs1 },
{"flw",    "D,o(b)",	MATCH_FLW, MASK_FLW,	 WR_fd|RD_xs1 },
{"fld",    "D,o(b)",	MATCH_FLD, MASK_FLD,  WR_fd|RD_xs1 },
{"lui",     "d,u",	MATCH_LUI, MASK_LUI,	 WR_xd },
{"mulw",    "d,s,t",	MATCH_MULW, MASK_MULW,  WR_xd|RD_xs1|RD_xs2 },
{"negw",     "d,t",	MATCH_SUBW, MASK_SUBW | MASK_RS,	 WR_xd|RD_xs2 }, /* sub 0 */
/* nop is at the start of the table.  */
{"not",     "d,s",	MATCH_XORI | MASK_IMM, MASK_XORI | MASK_IMM,	 WR_xd|RD_xs1 },/*nor d,s,0*/
{"or",      "d,s,t",	MATCH_OR, MASK_OR,	 WR_xd|RD_xs1|RD_xs2 },
{"or",     "d,s,j",	MATCH_ORI, MASK_ORI,	 WR_xd|RD_xs1 },
{"ori",     "d,s,j",	MATCH_ORI, MASK_ORI,	 WR_xd|RD_xs1 },
  /* pref and prefx are at the start of the table.  */
{"rdnpc",   "d",	MATCH_RDNPC, MASK_RDNPC,  WR_xd },
{"rem",    "d,s,t",	MATCH_REM, MASK_REM,  WR_xd|RD_xs1|RD_xs2 },
{"remw",    "d,s,t",	MATCH_REMW, MASK_REMW,  WR_xd|RD_xs1|RD_xs2 },
{"remu",    "d,s,t",	MATCH_REMU, MASK_REMU,  WR_xd|RD_xs1|RD_xs2 },
{"remuw",    "d,s,t",	MATCH_REMUW, MASK_REMUW,  WR_xd|RD_xs1|RD_xs2 },
{"fsgnj.s",   "D,S,T",	MATCH_FSGNJ_S, MASK_FSGNJ_S,	 WR_fd|RD_fs1|RD_fs2 },
{"fsgnj.d",   "D,S,T",	MATCH_FSGNJ_D, MASK_FSGNJ_D,	 WR_fd|RD_fs1|RD_fs2 },
{"fsgnjn.s",   "D,S,T",	MATCH_FSGNJN_S, MASK_FSGNJN_S,	 WR_fd|RD_fs1|RD_fs2 },
{"fsgnjn.d",   "D,S,T",	MATCH_FSGNJN_D, MASK_FSGNJN_D,	 WR_fd|RD_fs1|RD_fs2 },
{"fsgnjx.s",   "D,S,T",	MATCH_FSGNJX_S, MASK_FSGNJX_S,	 WR_fd|RD_fs1|RD_fs2 },
{"fsgnjx.d",   "D,S,T",	MATCH_FSGNJX_D, MASK_FSGNJX_D,	 WR_fd|RD_fs1|RD_fs2 },
{"fmin.s",   "D,S,T",	MATCH_FMIN_S, MASK_FMIN_S,	 WR_fd|RD_fs1|RD_fs2 },
{"fmin.d",   "D,S,T",	MATCH_FMIN_D, MASK_FMIN_D,	 WR_fd|RD_fs1|RD_fs2 },
{"fmax.s",   "D,S,T",	MATCH_FMAX_S, MASK_FMAX_S,	 WR_fd|RD_fs1|RD_fs2 },
{"fmax.d",   "D,S,T",	MATCH_FMAX_D, MASK_FMAX_D,	 WR_fd|RD_fs1|RD_fs2 },
{"sllw",    "d,s,t",	 MATCH_SLLW, MASK_SLLW,	 WR_xd|RD_xs1|RD_xs2 },
{"sllw",    "d,s,<",	 MATCH_SLLIW, MASK_SLLIW,	 WR_xd|RD_xs1 },
{"slliw",    "d,s,<",	 MATCH_SLLIW, MASK_SLLIW,	 WR_xd|RD_xs1 },
{"srlw",    "d,s,t",	 MATCH_SRLW, MASK_SRLW,	 WR_xd|RD_xs1|RD_xs2 },
{"srlw",    "d,s,<",	 MATCH_SRLIW, MASK_SRLIW,	 WR_xd|RD_xs1 },
{"srliw",    "d,s,<",	 MATCH_SRLIW, MASK_SRLIW,	 WR_xd|RD_xs1 },
{"sraw",    "d,s,t",	 MATCH_SRAW, MASK_SRAW,	 WR_xd|RD_xs1|RD_xs2 },
{"sraw",    "d,s,<",	 MATCH_SRAIW, MASK_SRAIW,	 WR_xd|RD_xs1 },
{"sraiw",    "d,s,<",	 MATCH_SRAIW, MASK_SRAIW,	 WR_xd|RD_xs1 },
{"slt",     "d,s,t",	MATCH_SLT, MASK_SLT,	 WR_xd|RD_xs1|RD_xs2 },
{"slt",    "d,s,j",	MATCH_SLTI, MASK_SLTI,	 WR_xd|RD_xs1 },
{"slti",    "d,s,j",	MATCH_SLTI, MASK_SLTI,	 WR_xd|RD_xs1 },
{"sltu",    "d,s,t",	MATCH_SLTU, MASK_SLTU,	 WR_xd|RD_xs1|RD_xs2 },
{"sltu",   "d,s,j",	MATCH_SLTIU, MASK_SLTIU,	 WR_xd|RD_xs1 },
{"sltiu",   "d,s,j",	MATCH_SLTIU, MASK_SLTIU,	 WR_xd|RD_xs1 },
{"subw",     "d,s,t",	MATCH_SUBW, MASK_SUBW,	 WR_xd|RD_xs1|RD_xs2 },
{"sb",      "t,q(b)",	MATCH_SB, MASK_SB,	 RD_xs1|RD_xs2 },
{"sh",      "t,q(b)",	MATCH_SH, MASK_SH,	 RD_xs1|RD_xs2 },
{"sw",      "t,q(b)",	MATCH_SW, MASK_SW,	 RD_xs1|RD_xs2 },
{"sd",	    "t,q(b)",	MATCH_SD, MASK_SD,	 RD_xs1|RD_xs2 },
{"fsw",    "T,q(b)",	MATCH_FSW, MASK_FSW,	 RD_xs1|RD_fs2 },
{"fsd",    "T,q(b)",	MATCH_FSD, MASK_FSD,  RD_xs1|RD_fs2 },
{"fence",    "",	MATCH_FENCE, MASK_FENCE | MASK_RD | MASK_RS | MASK_IMM,	 0 },
{"fence.i",   "",	MATCH_FENCE_I, MASK_FENCE_I | MASK_RD | MASK_RS | MASK_IMM,	 0 },
{"fence.l.v",   "",	MATCH_FENCE_L_V, MASK_FENCE_L_V | MASK_RD | MASK_RS | MASK_IMM,	 0 },
{"fence.g.v",   "",	MATCH_FENCE_G_V, MASK_FENCE_G_V | MASK_RD | MASK_RS | MASK_IMM,	 0 },
{"fence.l.cv",   "",	MATCH_FENCE_L_CV, MASK_FENCE_L_CV | MASK_RD | MASK_RS | MASK_IMM,	 0 },
{"fence.g.cv",   "",	MATCH_FENCE_G_CV, MASK_FENCE_G_CV | MASK_RD | MASK_RS | MASK_IMM,	 0 },
{"rdcycle",   "d",	MATCH_RDCYCLE, MASK_RDCYCLE,  WR_xd },
{"rdinstret",   "d",	MATCH_RDINSTRET, MASK_RDINSTRET,  WR_xd },
{"rdtime",   "d",	MATCH_RDTIME, MASK_RDTIME,  WR_xd },
{"break", "",		MATCH_BREAK, MASK_BREAK,	 0 },
{"syscall", "",		MATCH_SYSCALL, MASK_SYSCALL,	 0 },
{"stop", "",		MATCH_STOP, MASK_STOP,	 0 },
{"utidx", "d",		MATCH_UTIDX, MASK_UTIDX,	 WR_xd },
{"movz",    "d,s,t",	MATCH_MOVZ, MASK_MOVZ,  WR_xd|RD_xs1|RD_xs2 },
{"movn",    "d,s,t",	MATCH_MOVN, MASK_MOVN,  WR_xd|RD_xs1|RD_xs2 },
{"fmovz",   "D,s,T",	MATCH_FMOVZ, MASK_FMOVZ,  WR_fd|RD_xs1|RD_fs2 },
{"fmovn",   "D,s,T",	MATCH_FMOVN, MASK_FMOVN,  WR_fd|RD_xs1|RD_fs2 },
{"fcvt.l.d", "d,S",	MATCH_FCVT_L_D | MASK_RM, MASK_FCVT_L_D | MASK_RM,  WR_xd|RD_fs1 },
{"fcvt.l.d", "d,S,m",	MATCH_FCVT_L_D, MASK_FCVT_L_D,  WR_xd|RD_fs1 },
{"fcvt.l.s", "d,S",	MATCH_FCVT_L_S | MASK_RM, MASK_FCVT_L_S | MASK_RM,  WR_xd|RD_fs1 },
{"fcvt.l.s", "d,S,m",	MATCH_FCVT_L_S, MASK_FCVT_L_S,  WR_xd|RD_fs1 },
{"fcvt.w.d", "d,S",	MATCH_FCVT_W_D | MASK_RM, MASK_FCVT_W_D | MASK_RM,  WR_xd|RD_fs1 },
{"fcvt.w.d", "d,S,m",	MATCH_FCVT_W_D, MASK_FCVT_W_D,  WR_xd|RD_fs1 },
{"fcvt.w.s", "d,S",	MATCH_FCVT_W_S | MASK_RM, MASK_FCVT_W_S | MASK_RM,  WR_xd|RD_fs1 },
{"fcvt.w.s", "d,S,m",	MATCH_FCVT_W_S, MASK_FCVT_W_S,  WR_xd|RD_fs1 },
{"fcvt.lu.d", "d,S",	MATCH_FCVT_LU_D | MASK_RM, MASK_FCVT_LU_D | MASK_RM,  WR_xd|RD_fs1 },
{"fcvt.lu.d", "d,S,m",	MATCH_FCVT_LU_D, MASK_FCVT_LU_D,  WR_xd|RD_fs1 },
{"fcvt.lu.s", "d,S",	MATCH_FCVT_LU_S | MASK_RM, MASK_FCVT_LU_S | MASK_RM,  WR_xd|RD_fs1 },
{"fcvt.lu.s", "d,S,m",	MATCH_FCVT_LU_S, MASK_FCVT_LU_S,  WR_xd|RD_fs1 },
{"fcvt.wu.d", "d,S",	MATCH_FCVT_WU_D | MASK_RM, MASK_FCVT_WU_D | MASK_RM,  WR_xd|RD_fs1 },
{"fcvt.wu.d", "d,S,m",	MATCH_FCVT_WU_D, MASK_FCVT_WU_D,  WR_xd|RD_fs1 },
{"fcvt.wu.s", "d,S",	MATCH_FCVT_WU_S | MASK_RM, MASK_FCVT_WU_S | MASK_RM,  WR_xd|RD_fs1 },
{"fcvt.wu.s", "d,S,m",	MATCH_FCVT_WU_S, MASK_FCVT_WU_S,  WR_xd|RD_fs1 },
{"xor",     "d,s,t",	MATCH_XOR, MASK_XOR,	 WR_xd|RD_xs1|RD_xs2 },
{"xor",    "d,s,j",	MATCH_XORI, MASK_XORI,	 WR_xd|RD_xs1 },
{"xori",    "d,s,j",	MATCH_XORI, MASK_XORI,	 WR_xd|RD_xs1 },

/* unit stride */
/* xloads */
{"vld",       "#d,s",        MATCH_VLD, MASK_VLD,  0 },
{"vlw",       "#d,s",        MATCH_VLW, MASK_VLW,  0 },
{"vlwu",      "#d,s",        MATCH_VLWU, MASK_VLWU,  0 },
{"vlh",       "#d,s",        MATCH_VLH, MASK_VLH,  0 },
{"vlhu",      "#d,s",        MATCH_VLHU, MASK_VLHU,  0 },
{"vlb",       "#d,s",        MATCH_VLB, MASK_VLB,  0 },
{"vlbu",      "#d,s",        MATCH_VLBU, MASK_VLBU,  0 },
/* floads */
{"vfld",      "#D,s",        MATCH_VFLD, MASK_VFLD,  0 },
{"vflw",      "#D,s",        MATCH_VFLW, MASK_VFLW,  0 },

/* stride */
/* xloads */
{"vlstd",     "#d,s,t",      MATCH_VLSTD, MASK_VLSTD,  0 },
{"vlstw",     "#d,s,t",      MATCH_VLSTW, MASK_VLSTW,  0 },
{"vlstwu",    "#d,s,t",      MATCH_VLSTWU, MASK_VLSTWU,  0 },
{"vlsth",     "#d,s,t",      MATCH_VLSTH, MASK_VLSTH,  0 },
{"vlsthu",    "#d,s,t",      MATCH_VLSTHU, MASK_VLSTHU,  0 },
{"vlstb",     "#d,s,t",      MATCH_VLSTB, MASK_VLSTB,  0 },
{"vlstbu",    "#d,s,t",      MATCH_VLSTBU, MASK_VLSTBU,  0 },
/* floads */
{"vflstd",    "#D,s,t",      MATCH_VFLSTD, MASK_VFLSTD,  0 },
{"vflstw",    "#D,s,t",      MATCH_VFLSTW, MASK_VFLSTW,  0 },

/* segment */
/* xloads */
{"vlsegd",    "#d,s,#n",     MATCH_VLSEGD, MASK_VLSEGD,  0 },
{"vlsegw",    "#d,s,#n",     MATCH_VLSEGW, MASK_VLSEGW,  0 },
{"vlsegwu",   "#d,s,#n",     MATCH_VLSEGWU, MASK_VLSEGWU,  0 },
{"vlsegh",    "#d,s,#n",     MATCH_VLSEGH, MASK_VLSEGH,  0 },
{"vlseghu",   "#d,s,#n",     MATCH_VLSEGHU, MASK_VLSEGHU,  0 },
{"vlsegb",    "#d,s,#n",     MATCH_VLSEGB, MASK_VLSEGB,  0 },
{"vlsegbu",   "#d,s,#n",     MATCH_VLSEGBU, MASK_VLSEGBU,  0 },
/* floads */
{"vflsegd",   "#D,s,#n",     MATCH_VFLSEGD, MASK_VFLSEGD,  0 },
{"vflsegw",   "#D,s,#n",     MATCH_VFLSEGW, MASK_VFLSEGW,  0 },

/* stride segment */
/* xloads */
{"vlsegstd",  "#d,s,t,#m",   MATCH_VLSEGSTD, MASK_VLSEGSTD,  0 },
{"vlsegstw",  "#d,s,t,#m",   MATCH_VLSEGSTW, MASK_VLSEGSTW,  0 },
{"vlsegstwu", "#d,s,t,#m",   MATCH_VLSEGSTWU, MASK_VLSEGSTWU,  0 },
{"vlsegsth",  "#d,s,t,#m",   MATCH_VLSEGSTH, MASK_VLSEGSTH,  0 },
{"vlsegsthu", "#d,s,t,#m",   MATCH_VLSEGSTHU, MASK_VLSEGSTHU,  0 },
{"vlsegstb",  "#d,s,t,#m",   MATCH_VLSEGSTB, MASK_VLSEGSTB,  0 },
{"vlsegstbu", "#d,s,t,#m",   MATCH_VLSEGSTBU, MASK_VLSEGSTBU,  0 },
/* floads */
{"vflsegstd", "#D,s,t,#m",   MATCH_VFLSEGSTD, MASK_VFLSEGSTD,  0 },
{"vflsegstw", "#D,s,t,#m",   MATCH_VFLSEGSTW, MASK_VFLSEGSTW,  0 },

/* unit stride */
/* xstores */
{"vsd",       "#d,s",        MATCH_VSD, MASK_VSD,  0 },
{"vsw",       "#d,s",        MATCH_VSW, MASK_VSW,  0 },
{"vsh",       "#d,s",        MATCH_VSH, MASK_VSH,  0 },
{"vsb",       "#d,s",        MATCH_VSB, MASK_VSB,  0 },
/* fstores */
{"vfsd",      "#D,s",        MATCH_VFSD, MASK_VFSD,  0 },
{"vfsw",      "#D,s",        MATCH_VFSW, MASK_VFSW,  0 },

/* stride */
/* xstores */
{"vsstd",     "#d,s,t",      MATCH_VSSTD, MASK_VSSTD,  0 },
{"vsstw",     "#d,s,t",      MATCH_VSSTW, MASK_VSSTW,  0 },
{"vssth",     "#d,s,t",      MATCH_VSSTH, MASK_VSSTH,  0 },
{"vsstb",     "#d,s,t",      MATCH_VSSTB, MASK_VSSTB,  0 },
/* fstores */
{"vfsstd",    "#D,s,t",      MATCH_VFSSTD, MASK_VFSSTD,  0 },
{"vfsstw",    "#D,s,t",      MATCH_VFSSTW, MASK_VFSSTW,  0 },

/* segment */
/* xstores */
{"vssegd",    "#d,s,#n",     MATCH_VSSEGD, MASK_VSSEGD,  0 },
{"vssegw",    "#d,s,#n",     MATCH_VSSEGW, MASK_VSSEGW,  0 },
{"vssegh",    "#d,s,#n",     MATCH_VSSEGH, MASK_VSSEGH,  0 },
{"vssegb",    "#d,s,#n",     MATCH_VSSEGB, MASK_VSSEGB,  0 },
/* fstores */
{"vfssegd",   "#D,s,#n",     MATCH_VFSSEGD, MASK_VFSSEGD,  0 },
{"vfssegw",   "#D,s,#n",     MATCH_VFSSEGW, MASK_VFSSEGW,  0 },

/* stride segment */
/* xsegstores */
{"vssegstd",  "#d,s,t,#m",   MATCH_VSSEGSTD, MASK_VSSEGSTD,  0 },
{"vssegstw",  "#d,s,t,#m",   MATCH_VSSEGSTW, MASK_VSSEGSTW,  0 },
{"vssegsth",  "#d,s,t,#m",   MATCH_VSSEGSTH, MASK_VSSEGSTH,  0 },
{"vssegstb",  "#d,s,t,#m",   MATCH_VSSEGSTB, MASK_VSSEGSTB,  0 },
/* fsegstores */
{"vfssegstd", "#D,s,t,#m",   MATCH_VFSSEGSTD, MASK_VFSSEGSTD,  0 },
{"vfssegstw", "#D,s,t,#m",   MATCH_VFSSEGSTW, MASK_VFSSEGSTW,  0 },

{"vsetvl",    "d,s",            MATCH_VSETVL, MASK_VSETVL,  WR_xd|RD_xs1 },
{"vmvv",      "#d,#s",          MATCH_VMVV, MASK_VMVV,  0 },
{"vmsv",      "#d,s",           MATCH_VMSV, MASK_VMSV,  0 },
{"vmst",      "#d,s,t",         MATCH_VMST, MASK_VMST,  0 },
{"vmts",      "d,#s,t",         MATCH_VMTS, MASK_VMTS,  0 },
{"vfmvv",     "#D,#S",          MATCH_VFMVV, MASK_VFMVV,  0 },
{"vfmsv",     "#D,S",           MATCH_VFMSV, MASK_VFMSV,  0 },
{"vfmst",     "#D,S,T",         MATCH_VFMST, MASK_VFMST,  0 },
{"vfmts",     "D,#S,T",         MATCH_VFMTS, MASK_VFMTS,  0 },

{"vvcfg",     "s,t",            MATCH_VVCFG, MASK_VVCFG, RD_xs1|RD_xs2 },
{"vtcfg",     "s,t",            MATCH_VTCFG, MASK_VTCFG, RD_xs1|RD_xs2 },

{"vvcfgivl",  "d,s,#g,#f",      MATCH_VVCFGIVL, MASK_VVCFGIVL,  0 },
{"vtcfgivl",  "d,s,#g,#f",      MATCH_VTCFGIVL, MASK_VTCFGIVL,  0 },
{"vf",        "j(b)",           MATCH_VF, MASK_VF,  0 },

{"venqcmd",   "s,t",  MATCH_VENQCMD, MASK_VENQCMD, RD_xs1|RD_xs2 },
{"venqimm1",  "s,t",  MATCH_VENQIMM1, MASK_VENQIMM1, RD_xs1|RD_xs2 },
{"venqimm2",  "s,t",  MATCH_VENQIMM2, MASK_VENQIMM2, RD_xs1|RD_xs2 },
{"venqcnt",   "s,t",  MATCH_VENQCNT, MASK_VENQCNT, RD_xs1|RD_xs2 },

{"vwaitxcpt",   "",	MATCH_VWAITXCPT, MASK_VWAITXCPT | MASK_RD | MASK_RS | MASK_IMM,	 0 },
};

#define RISCV_NUM_OPCODES \
	((sizeof riscv_builtin_opcodes) / (sizeof (riscv_builtin_opcodes[0])))
const int bfd_riscv_num_builtin_opcodes = RISCV_NUM_OPCODES;

/* const removed from the following to allow for dynamic extensions to the
 * built-in instruction set. */
struct riscv_opcode *riscv_opcodes =
  (struct riscv_opcode *) riscv_builtin_opcodes;
int bfd_riscv_num_opcodes = RISCV_NUM_OPCODES;
#undef RISCV_NUM_OPCODES
