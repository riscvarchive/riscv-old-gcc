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
   name must be contiguous. */

#define WR_xd INSN_WRITE_GPR_D
#define WR_fd INSN_WRITE_FPR_D
#define RD_xs1 INSN_READ_GPR_S
#define RD_xs2 INSN_READ_GPR_T
#define RD_fs1 INSN_READ_FPR_S
#define RD_fs2 INSN_READ_FPR_T
#define RD_fs3 INSN_READ_FPR_R

#define MASK_RS1 (OP_MASK_RS1 << OP_SH_RS1)
#define MASK_RS2 (OP_MASK_RS2 << OP_SH_RS2)
#define MASK_RD (OP_MASK_RD << OP_SH_RD)
#define MASK_IMM ENCODE_ITYPE_IMM(-1U)
#define MASK_UIMM ENCODE_UTYPE_IMM(-1U)
#define MASK_RM (OP_MASK_RM << OP_SH_RM)
#define MASK_PRED (OP_MASK_PRED << OP_SH_PRED)
#define MASK_SUCC (OP_MASK_SUCC << OP_SH_SUCC)
#define MASK_AQ (OP_MASK_AQ << OP_SH_AQ)
#define MASK_RL (OP_MASK_RL << OP_SH_RL)
#define MASK_AQRL (MASK_AQ | MASK_RL)

const struct riscv_opcode riscv_builtin_opcodes[] =
{
/* These instructions appear first so that the disassembler will find
   them first.  The assemblers uses a hash table based on the
   instruction name anyhow.  */
/* name,      isa,   operands, match, mask, pinfo */
{"unimp",     "I",   "",         0, 0xffffffff,  0 },
{"nop",       "I",   "",         MATCH_ADDI, MASK_ADDI | MASK_RD | MASK_RS1 | MASK_IMM,  0 },
{"li",        "I",   "d,j",      MATCH_ADDI, MASK_ADDI | MASK_RS1,  WR_xd }, /* addi */
{"li",        "I",   "d,I",  0,    (int) M_LI,  INSN_MACRO },
{"move",      "I",   "d,s",  MATCH_ADDI, MASK_ADDI | MASK_IMM,   WR_xd|RD_xs1 },
{"mv",        "I",   "d,s",  MATCH_ADDI, MASK_ADDI | MASK_IMM,   WR_xd|RD_xs1 },
{"b",         "I",   "p",  MATCH_BEQ, MASK_BEQ | MASK_RS1 | MASK_RS2,   0 },/* beq 0,0 */
{"and",       "I",   "d,s,t",  MATCH_AND, MASK_AND,   WR_xd|RD_xs1|RD_xs2 },
{"and",       "I",   "d,s,j",  MATCH_ANDI, MASK_ANDI,   WR_xd|RD_xs1 },
{"andi",      "I",   "d,s,j",  MATCH_ANDI, MASK_ANDI,   WR_xd|RD_xs1 },
{"beqz",      "I",   "s,p",  MATCH_BEQ, MASK_BEQ | MASK_RS2,   RD_xs1 },
{"beq",       "I",   "s,t,p",  MATCH_BEQ, MASK_BEQ,   RD_xs1|RD_xs2 },
{"blez",      "I",   "t,p",  MATCH_BGE, MASK_BGE | MASK_RS1,   RD_xs2 },
{"bgez",      "I",   "s,p",  MATCH_BGE, MASK_BGE | MASK_RS2,   RD_xs1 },
{"ble",       "I",   "t,s,p",  MATCH_BGE, MASK_BGE,   RD_xs1|RD_xs2 },
{"bleu",      "I",   "t,s,p",  MATCH_BGEU, MASK_BGEU,   RD_xs1|RD_xs2 },
{"bge",       "I",   "s,t,p",  MATCH_BGE, MASK_BGE,   RD_xs1|RD_xs2 },
{"bgeu",      "I",   "s,t,p",  MATCH_BGEU, MASK_BGEU,   RD_xs1|RD_xs2 },
{"bltz",      "I",   "s,p",  MATCH_BLT, MASK_BLT | MASK_RS2,   RD_xs1 },
{"bgtz",      "I",   "t,p",  MATCH_BLT, MASK_BLT | MASK_RS1,   RD_xs2 },
{"blt",       "I",   "s,t,p",  MATCH_BLT, MASK_BLT,   RD_xs1|RD_xs2 },
{"bltu",      "I",   "s,t,p",  MATCH_BLTU, MASK_BLTU,   RD_xs1|RD_xs2 },
{"bgt",       "I",   "t,s,p",  MATCH_BLT, MASK_BLT,   RD_xs1|RD_xs2 },
{"bgtu",      "I",   "t,s,p",  MATCH_BLTU, MASK_BLTU,   RD_xs1|RD_xs2 },
{"bnez",      "I",   "s,p",  MATCH_BNE, MASK_BNE | MASK_RS2,   RD_xs1 },
{"bne",       "I",   "s,t,p",  MATCH_BNE, MASK_BNE,   RD_xs1|RD_xs2 },
{"add",       "I",   "d,s,t",  MATCH_ADD, MASK_ADD,  WR_xd|RD_xs1|RD_xs2 },
{"add",       "I",   "d,s,j",  MATCH_ADDI, MASK_ADDI,  WR_xd|RD_xs1 },
{"addi",      "I",   "d,s,j",  MATCH_ADDI, MASK_ADDI,  WR_xd|RD_xs1 },
{"la",        "I",   "d,A(b)",  0,    (int) M_LA,  INSN_MACRO },
{"lla",       "I",   "d,A(b)",  0,    (int) M_LLA,  INSN_MACRO },
{"la.tls.gd", "I",   "d,A",  0,    (int) M_LA_TLS_GD,  INSN_MACRO },
{"la.tls.ie", "I",   "d,A",  0,    (int) M_LA_TLS_IE,  INSN_MACRO },
{"neg",       "I",   "d,t",  MATCH_SUB, MASK_SUB | MASK_RS1,   WR_xd|RD_xs2 }, /* sub 0 */
{"sll",       "I",   "d,s,t",   MATCH_SLL, MASK_SLL,   WR_xd|RD_xs1|RD_xs2 },
{"sll",       "I",   "d,s,>",   MATCH_SLLI, MASK_SLLI,   WR_xd|RD_xs1 },
{"slli",      "I",   "d,s,>",   MATCH_SLLI, MASK_SLLI,   WR_xd|RD_xs1 },
{"srl",       "I",   "d,s,t",   MATCH_SRL, MASK_SRL,   WR_xd|RD_xs1|RD_xs2 },
{"srl",       "I",   "d,s,>",   MATCH_SRLI, MASK_SRLI,   WR_xd|RD_xs1 },
{"srli",      "I",   "d,s,>",   MATCH_SRLI, MASK_SRLI,   WR_xd|RD_xs1 },
{"sra",       "I",   "d,s,t",   MATCH_SRA, MASK_SRA,   WR_xd|RD_xs1|RD_xs2 },
{"sra",       "I",   "d,s,>",   MATCH_SRAI, MASK_SRAI,   WR_xd|RD_xs1 },
{"srai",      "I",   "d,s,>",   MATCH_SRAI, MASK_SRAI,   WR_xd|RD_xs1 },
{"sub",       "I",   "d,s,t",  MATCH_SUB, MASK_SUB,   WR_xd|RD_xs1|RD_xs2 },
{"ret",       "I",   "",  MATCH_JALR | (LINK_REG << OP_SH_RS1), MASK_JALR | MASK_RD | MASK_RS1 | MASK_IMM,   WR_xd|RD_xs1 },
{"j",         "I",   "a",  MATCH_JAL, MASK_JAL | MASK_RD,   0 },
{"j",         "I",   "a,s",  0,    (int) M_J,  INSN_MACRO },
{"jal",       "I",   "a",  MATCH_JAL | (LINK_REG << OP_SH_RD), MASK_JAL | MASK_RD,   WR_xd },
{"jal",       "I",   "d,a",  MATCH_JAL, MASK_JAL,   WR_xd },
{"jal",       "I",   "a,s",  0,    (int) M_JAL_RA,  INSN_MACRO },
{"jal",       "I",   "d,a,s",  0,    (int) M_JAL,  INSN_MACRO },
{"jr",        "I",   "s",  MATCH_JALR, MASK_JALR | MASK_RD | MASK_IMM,   WR_xd|RD_xs1 },
{"jr",        "I",   "s,j",  MATCH_JALR, MASK_JALR | MASK_RD,   WR_xd|RD_xs1 },
{"jalr",      "I",   "s",  MATCH_JALR | (LINK_REG << OP_SH_RD), MASK_JALR | MASK_RD | MASK_IMM,   WR_xd|RD_xs1 },
{"jalr",      "I",   "s,j",  MATCH_JALR | (LINK_REG << OP_SH_RD), MASK_JALR | MASK_RD,   WR_xd|RD_xs1 },
{"jalr",      "I",   "d,s",  MATCH_JALR, MASK_JALR | MASK_IMM,   WR_xd|RD_xs1 },
{"jalr",      "I",   "d,s,j",  MATCH_JALR, MASK_JALR,   WR_xd|RD_xs1 },
{"lb",        "I",   "d,o(b)",  MATCH_LB, MASK_LB,   WR_xd|RD_xs1 },
{"lbu",       "I",   "d,o(b)",  MATCH_LBU, MASK_LBU,   WR_xd|RD_xs1 },
{"lh",        "I",   "d,o(b)",  MATCH_LH, MASK_LH,   WR_xd|RD_xs1 },
{"lhu",       "I",   "d,o(b)",  MATCH_LHU, MASK_LHU,   WR_xd|RD_xs1 },
{"lw",        "I",   "d,o(b)",  MATCH_LW, MASK_LW,   WR_xd|RD_xs1 },
{"lui",       "I",   "d,u",  MATCH_LUI, MASK_LUI,   WR_xd },
{"not",       "I",   "d,s",  MATCH_XORI | MASK_IMM, MASK_XORI | MASK_IMM,   WR_xd|RD_xs1 },
{"or",        "I",   "d,s,t",  MATCH_OR, MASK_OR,   WR_xd|RD_xs1|RD_xs2 },
{"or",        "I",   "d,s,j",  MATCH_ORI, MASK_ORI,   WR_xd|RD_xs1 },
{"ori",       "I",   "d,s,j",  MATCH_ORI, MASK_ORI,   WR_xd|RD_xs1 },
{"auipc",     "I",   "d,u",  MATCH_AUIPC, MASK_AUIPC,  WR_xd },
{"seqz",      "I",   "d,s",  MATCH_SLTIU | ENCODE_ITYPE_IMM(1), MASK_SLTIU | MASK_IMM,   WR_xd|RD_xs1 },
{"snez",      "I",   "d,t",  MATCH_SLTU, MASK_SLTU | MASK_RS1,   WR_xd|RD_xs2 },
{"sltz",      "I",   "d,s",  MATCH_SLT, MASK_SLT | MASK_RS2,   WR_xd|RD_xs1 },
{"sgtz",      "I",   "d,t",  MATCH_SLT, MASK_SLT | MASK_RS1,   WR_xd|RD_xs2 },
{"slt",       "I",   "d,s,t",  MATCH_SLT, MASK_SLT,   WR_xd|RD_xs1|RD_xs2 },
{"slt",       "I",   "d,s,j",  MATCH_SLTI, MASK_SLTI,   WR_xd|RD_xs1 },
{"slti",      "I",   "d,s,j",  MATCH_SLTI, MASK_SLTI,   WR_xd|RD_xs1 },
{"sltu",      "I",   "d,s,t",  MATCH_SLTU, MASK_SLTU,   WR_xd|RD_xs1|RD_xs2 },
{"sltu",      "I",   "d,s,j",  MATCH_SLTIU, MASK_SLTIU,   WR_xd|RD_xs1 },
{"sltiu",     "I",   "d,s,j",  MATCH_SLTIU, MASK_SLTIU,   WR_xd|RD_xs1 },
{"sgt",       "I",   "d,t,s",  MATCH_SLT, MASK_SLT,   WR_xd|RD_xs1|RD_xs2 },
{"sgtu",      "I",   "d,t,s",  MATCH_SLTU, MASK_SLTU,   WR_xd|RD_xs1|RD_xs2 },
{"sb",        "I",   "t,q(b)",  MATCH_SB, MASK_SB,   RD_xs1|RD_xs2 },
{"sh",        "I",   "t,q(b)",  MATCH_SH, MASK_SH,   RD_xs1|RD_xs2 },
{"sw",        "I",   "t,q(b)",  MATCH_SW, MASK_SW,   RD_xs1|RD_xs2 },
{"fence",     "I",   "",  MATCH_FENCE | MASK_PRED | MASK_SUCC, MASK_FENCE | MASK_RD | MASK_RS1 | MASK_IMM,   0 },
{"fence",     "I",   "P,Q",  MATCH_FENCE, MASK_FENCE | MASK_RD | MASK_RS1 | (MASK_IMM & ~MASK_PRED & ~MASK_SUCC),   0 },
{"fence.i",   "I",   "",  MATCH_FENCE_I, MASK_FENCE | MASK_RD | MASK_RS1 | MASK_IMM,   0 },
{"rdcycle",   "I",   "d",  MATCH_RDCYCLE, MASK_RDCYCLE,  WR_xd },
{"rdinstret", "I",   "d",  MATCH_RDINSTRET, MASK_RDINSTRET,  WR_xd },
{"rdtime",    "I",   "d",  MATCH_RDTIME, MASK_RDTIME,  WR_xd },
{"break",     "I",   "",    MATCH_BREAK, MASK_BREAK,   0 },
{"syscall",   "I",   "",    MATCH_SYSCALL, MASK_SYSCALL,   0 },
{"xor",       "I",   "d,s,t",  MATCH_XOR, MASK_XOR,   WR_xd|RD_xs1|RD_xs2 },
{"xor",       "I",   "d,s,j",  MATCH_XORI, MASK_XORI,   WR_xd|RD_xs1 },
{"xori",      "I",   "d,s,j",  MATCH_XORI, MASK_XORI,   WR_xd|RD_xs1 },
{"lwu",       "64I", "d,o(b)",  MATCH_LWU, MASK_LWU,   WR_xd|RD_xs1 },
{"ld",        "64I", "d,o(b)", MATCH_LD, MASK_LD,  WR_xd|RD_xs1 },
{"sd",        "64I", "t,q(b)",  MATCH_SD, MASK_SD,   RD_xs1|RD_xs2 },
{"sext.w",    "64I", "d,s",  MATCH_ADDIW, MASK_ADDIW | MASK_IMM,   WR_xd|RD_xs1 },
{"addw",      "64I", "d,s,t",  MATCH_ADDW, MASK_ADDW,   WR_xd|RD_xs1|RD_xs2 },
{"addw",      "64I", "d,s,j",  MATCH_ADDIW, MASK_ADDIW,   WR_xd|RD_xs1 },
{"addiw",     "64I", "d,s,j",  MATCH_ADDIW, MASK_ADDIW,   WR_xd|RD_xs1 },
{"negw",      "64I", "d,t",  MATCH_SUBW, MASK_SUBW | MASK_RS1,   WR_xd|RD_xs2 }, /* sub 0 */
{"sllw",      "64I", "d,s,t",   MATCH_SLLW, MASK_SLLW,   WR_xd|RD_xs1|RD_xs2 },
{"sllw",      "64I", "d,s,<",   MATCH_SLLIW, MASK_SLLIW,   WR_xd|RD_xs1 },
{"slliw",     "64I", "d,s,<",   MATCH_SLLIW, MASK_SLLIW,   WR_xd|RD_xs1 },
{"srlw",      "64I", "d,s,t",   MATCH_SRLW, MASK_SRLW,   WR_xd|RD_xs1|RD_xs2 },
{"srlw",      "64I", "d,s,<",   MATCH_SRLIW, MASK_SRLIW,   WR_xd|RD_xs1 },
{"srliw",     "64I", "d,s,<",   MATCH_SRLIW, MASK_SRLIW,   WR_xd|RD_xs1 },
{"sraw",      "64I", "d,s,t",   MATCH_SRAW, MASK_SRAW,   WR_xd|RD_xs1|RD_xs2 },
{"sraw",      "64I", "d,s,<",   MATCH_SRAIW, MASK_SRAIW,   WR_xd|RD_xs1 },
{"sraiw",     "64I", "d,s,<",   MATCH_SRAIW, MASK_SRAIW,   WR_xd|RD_xs1 },
{"subw",      "64I", "d,s,t",  MATCH_SUBW, MASK_SUBW,   WR_xd|RD_xs1|RD_xs2 },

/* Atomic memory operation instruction subset */
{"lr.w",         "A",   "d,0(b)",    MATCH_LR_W, MASK_LR_W | MASK_AQRL,   WR_xd|RD_xs1 },
{"sc.w",         "A",   "d,t,0(b)",  MATCH_SC_W, MASK_SC_W | MASK_AQRL,   WR_xd|RD_xs1|RD_xs2 },
{"amoadd.w",     "A",   "d,t,0(b)",  MATCH_AMOADD_W, MASK_AMOADD_W | MASK_AQRL,   WR_xd|RD_xs1|RD_xs2 },
{"amoswap.w",    "A",   "d,t,0(b)",  MATCH_AMOSWAP_W, MASK_AMOSWAP_W | MASK_AQRL,   WR_xd|RD_xs1|RD_xs2 },
{"amoand.w",     "A",   "d,t,0(b)",  MATCH_AMOAND_W, MASK_AMOAND_W | MASK_AQRL,   WR_xd|RD_xs1|RD_xs2 },
{"amoor.w",      "A",   "d,t,0(b)",  MATCH_AMOOR_W, MASK_AMOOR_W | MASK_AQRL,   WR_xd|RD_xs1|RD_xs2 },
{"amoxor.w",     "A",   "d,t,0(b)",  MATCH_AMOXOR_W, MASK_AMOXOR_W | MASK_AQRL,   WR_xd|RD_xs1|RD_xs2 },
{"amomax.w",     "A",   "d,t,0(b)",  MATCH_AMOMAX_W, MASK_AMOMAX_W | MASK_AQRL,   WR_xd|RD_xs1|RD_xs2 },
{"amomaxu.w",    "A",   "d,t,0(b)",  MATCH_AMOMAXU_W, MASK_AMOMAXU_W | MASK_AQRL,   WR_xd|RD_xs1|RD_xs2 },
{"amomin.w",     "A",   "d,t,0(b)",  MATCH_AMOMIN_W, MASK_AMOMIN_W | MASK_AQRL,   WR_xd|RD_xs1|RD_xs2 },
{"amominu.w",    "A",   "d,t,0(b)",  MATCH_AMOMINU_W, MASK_AMOMINU_W | MASK_AQRL,   WR_xd|RD_xs1|RD_xs2 },
{"lr.w.aq",      "A",   "d,0(b)",    MATCH_LR_W | MASK_AQ, MASK_LR_W | MASK_AQRL,   WR_xd|RD_xs1 },
{"sc.w.aq",      "A",   "d,t,0(b)",  MATCH_SC_W | MASK_AQ, MASK_SC_W | MASK_AQRL,   WR_xd|RD_xs1|RD_xs2 },
{"amoadd.w.aq",  "A",   "d,t,0(b)",  MATCH_AMOADD_W | MASK_AQ, MASK_AMOADD_W | MASK_AQRL,   WR_xd|RD_xs1|RD_xs2 },
{"amoswap.w.aq", "A",   "d,t,0(b)",  MATCH_AMOSWAP_W | MASK_AQ, MASK_AMOSWAP_W | MASK_AQRL,   WR_xd|RD_xs1|RD_xs2 },
{"amoand.w.aq",  "A",   "d,t,0(b)",  MATCH_AMOAND_W | MASK_AQ, MASK_AMOAND_W | MASK_AQRL,   WR_xd|RD_xs1|RD_xs2 },
{"amoor.w.aq",   "A",   "d,t,0(b)",  MATCH_AMOOR_W | MASK_AQ, MASK_AMOOR_W | MASK_AQRL,   WR_xd|RD_xs1|RD_xs2 },
{"amoxor.w.aq",  "A",   "d,t,0(b)",  MATCH_AMOXOR_W | MASK_AQ, MASK_AMOXOR_W | MASK_AQRL,   WR_xd|RD_xs1|RD_xs2 },
{"amomax.w.aq",  "A",   "d,t,0(b)",  MATCH_AMOMAX_W | MASK_AQ, MASK_AMOMAX_W | MASK_AQRL,   WR_xd|RD_xs1|RD_xs2 },
{"amomaxu.w.aq", "A",   "d,t,0(b)",  MATCH_AMOMAXU_W | MASK_AQ, MASK_AMOMAXU_W | MASK_AQRL,   WR_xd|RD_xs1|RD_xs2 },
{"amomin.w.aq",  "A",   "d,t,0(b)",  MATCH_AMOMIN_W | MASK_AQ, MASK_AMOMIN_W | MASK_AQRL,   WR_xd|RD_xs1|RD_xs2 },
{"amominu.w.aq", "A",   "d,t,0(b)",  MATCH_AMOMINU_W | MASK_AQ, MASK_AMOMINU_W | MASK_AQRL,   WR_xd|RD_xs1|RD_xs2 },
{"lr.w.rl",      "A",   "d,0(b)",    MATCH_LR_W | MASK_RL, MASK_LR_W | MASK_AQRL,   WR_xd|RD_xs1 },
{"sc.w.rl",      "A",   "d,t,0(b)",  MATCH_SC_W | MASK_RL, MASK_SC_W | MASK_AQRL,   WR_xd|RD_xs1|RD_xs2 },
{"amoadd.w.rl",  "A",   "d,t,0(b)",  MATCH_AMOADD_W | MASK_RL, MASK_AMOADD_W | MASK_AQRL,   WR_xd|RD_xs1|RD_xs2 },
{"amoswap.w.rl", "A",   "d,t,0(b)",  MATCH_AMOSWAP_W | MASK_RL, MASK_AMOSWAP_W | MASK_AQRL,   WR_xd|RD_xs1|RD_xs2 },
{"amoand.w.rl",  "A",   "d,t,0(b)",  MATCH_AMOAND_W | MASK_RL, MASK_AMOAND_W | MASK_AQRL,   WR_xd|RD_xs1|RD_xs2 },
{"amoor.w.rl",   "A",   "d,t,0(b)",  MATCH_AMOOR_W | MASK_RL, MASK_AMOOR_W | MASK_AQRL,   WR_xd|RD_xs1|RD_xs2 },
{"amoxor.w.rl",  "A",   "d,t,0(b)",  MATCH_AMOXOR_W | MASK_RL, MASK_AMOXOR_W | MASK_AQRL,   WR_xd|RD_xs1|RD_xs2 },
{"amomax.w.rl",  "A",   "d,t,0(b)",  MATCH_AMOMAX_W | MASK_RL, MASK_AMOMAX_W | MASK_AQRL,   WR_xd|RD_xs1|RD_xs2 },
{"amomaxu.w.rl", "A",   "d,t,0(b)",  MATCH_AMOMAXU_W | MASK_RL, MASK_AMOMAXU_W | MASK_AQRL,   WR_xd|RD_xs1|RD_xs2 },
{"amomin.w.rl",  "A",   "d,t,0(b)",  MATCH_AMOMIN_W | MASK_RL, MASK_AMOMIN_W | MASK_AQRL,   WR_xd|RD_xs1|RD_xs2 },
{"amominu.w.rl", "A",   "d,t,0(b)",  MATCH_AMOMINU_W | MASK_RL, MASK_AMOMINU_W | MASK_AQRL,   WR_xd|RD_xs1|RD_xs2 },
{"lr.w.sc",      "A",   "d,0(b)",    MATCH_LR_W | MASK_AQRL, MASK_LR_W | MASK_AQRL,   WR_xd|RD_xs1 },
{"sc.w.sc",      "A",   "d,t,0(b)",  MATCH_SC_W | MASK_AQRL, MASK_SC_W | MASK_AQRL,   WR_xd|RD_xs1|RD_xs2 },
{"amoadd.w.sc",  "A",   "d,t,0(b)",  MATCH_AMOADD_W | MASK_AQRL, MASK_AMOADD_W | MASK_AQRL,   WR_xd|RD_xs1|RD_xs2 },
{"amoswap.w.sc", "A",   "d,t,0(b)",  MATCH_AMOSWAP_W | MASK_AQRL, MASK_AMOSWAP_W | MASK_AQRL,   WR_xd|RD_xs1|RD_xs2 },
{"amoand.w.sc",  "A",   "d,t,0(b)",  MATCH_AMOAND_W | MASK_AQRL, MASK_AMOAND_W | MASK_AQRL,   WR_xd|RD_xs1|RD_xs2 },
{"amoor.w.sc",   "A",   "d,t,0(b)",  MATCH_AMOOR_W | MASK_AQRL, MASK_AMOOR_W | MASK_AQRL,   WR_xd|RD_xs1|RD_xs2 },
{"amoxor.w.sc",  "A",   "d,t,0(b)",  MATCH_AMOXOR_W | MASK_AQRL, MASK_AMOXOR_W | MASK_AQRL,   WR_xd|RD_xs1|RD_xs2 },
{"amomax.w.sc",  "A",   "d,t,0(b)",  MATCH_AMOMAX_W | MASK_AQRL, MASK_AMOMAX_W | MASK_AQRL,   WR_xd|RD_xs1|RD_xs2 },
{"amomaxu.w.sc", "A",   "d,t,0(b)",  MATCH_AMOMAXU_W | MASK_AQRL, MASK_AMOMAXU_W | MASK_AQRL,   WR_xd|RD_xs1|RD_xs2 },
{"amomin.w.sc",  "A",   "d,t,0(b)",  MATCH_AMOMIN_W | MASK_AQRL, MASK_AMOMIN_W | MASK_AQRL,   WR_xd|RD_xs1|RD_xs2 },
{"amominu.w.sc", "A",   "d,t,0(b)",  MATCH_AMOMINU_W | MASK_AQRL, MASK_AMOMINU_W | MASK_AQRL,   WR_xd|RD_xs1|RD_xs2 },
{"lr.d",         "64A", "d,0(b)",    MATCH_LR_D, MASK_LR_D | MASK_AQRL,   WR_xd|RD_xs1 },
{"sc.d",         "64A", "d,t,0(b)",  MATCH_SC_D, MASK_SC_D | MASK_AQRL,   WR_xd|RD_xs1|RD_xs2 },
{"amoadd.d",     "64A", "d,t,0(b)",  MATCH_AMOADD_D, MASK_AMOADD_D | MASK_AQRL,   WR_xd|RD_xs1|RD_xs2 },
{"amoswap.d",    "64A", "d,t,0(b)",  MATCH_AMOSWAP_D, MASK_AMOSWAP_D | MASK_AQRL,   WR_xd|RD_xs1|RD_xs2 },
{"amoand.d",     "64A", "d,t,0(b)",  MATCH_AMOAND_D, MASK_AMOAND_D | MASK_AQRL,   WR_xd|RD_xs1|RD_xs2 },
{"amoor.d",      "64A", "d,t,0(b)",  MATCH_AMOOR_D, MASK_AMOOR_D | MASK_AQRL,   WR_xd|RD_xs1|RD_xs2 },
{"amoxor.d",     "64A", "d,t,0(b)",  MATCH_AMOXOR_D, MASK_AMOXOR_D | MASK_AQRL,   WR_xd|RD_xs1|RD_xs2 },
{"amomax.d",     "64A", "d,t,0(b)",  MATCH_AMOMAX_D, MASK_AMOMAX_D | MASK_AQRL,   WR_xd|RD_xs1|RD_xs2 },
{"amomaxu.d",    "64A", "d,t,0(b)",  MATCH_AMOMAXU_D, MASK_AMOMAXU_D | MASK_AQRL,   WR_xd|RD_xs1|RD_xs2 },
{"amomin.d",     "64A", "d,t,0(b)",  MATCH_AMOMIN_D, MASK_AMOMIN_D | MASK_AQRL,   WR_xd|RD_xs1|RD_xs2 },
{"amominu.d",    "64A", "d,t,0(b)",  MATCH_AMOMINU_D, MASK_AMOMINU_D | MASK_AQRL,   WR_xd|RD_xs1|RD_xs2 },
{"lr.d.aq",      "64A", "d,0(b)",    MATCH_LR_D | MASK_AQ, MASK_LR_D | MASK_AQRL,   WR_xd|RD_xs1 },
{"sc.d.aq",      "64A", "d,t,0(b)",  MATCH_SC_D | MASK_AQ, MASK_SC_D | MASK_AQRL,   WR_xd|RD_xs1|RD_xs2 },
{"amoadd.d.aq",  "64A", "d,t,0(b)",  MATCH_AMOADD_D | MASK_AQ, MASK_AMOADD_D | MASK_AQRL,   WR_xd|RD_xs1|RD_xs2 },
{"amoswap.d.aq", "64A", "d,t,0(b)",  MATCH_AMOSWAP_D | MASK_AQ, MASK_AMOSWAP_D | MASK_AQRL,   WR_xd|RD_xs1|RD_xs2 },
{"amoand.d.aq",  "64A", "d,t,0(b)",  MATCH_AMOAND_D | MASK_AQ, MASK_AMOAND_D | MASK_AQRL,   WR_xd|RD_xs1|RD_xs2 },
{"amoor.d.aq",   "64A", "d,t,0(b)",  MATCH_AMOOR_D | MASK_AQ, MASK_AMOOR_D | MASK_AQRL,   WR_xd|RD_xs1|RD_xs2 },
{"amoxor.d.aq",  "64A", "d,t,0(b)",  MATCH_AMOXOR_D | MASK_AQ, MASK_AMOXOR_D | MASK_AQRL,   WR_xd|RD_xs1|RD_xs2 },
{"amomax.d.aq",  "64A", "d,t,0(b)",  MATCH_AMOMAX_D | MASK_AQ, MASK_AMOMAX_D | MASK_AQRL,   WR_xd|RD_xs1|RD_xs2 },
{"amomaxu.d.aq", "64A", "d,t,0(b)",  MATCH_AMOMAXU_D | MASK_AQ, MASK_AMOMAXU_D | MASK_AQRL,   WR_xd|RD_xs1|RD_xs2 },
{"amomin.d.aq",  "64A", "d,t,0(b)",  MATCH_AMOMIN_D | MASK_AQ, MASK_AMOMIN_D | MASK_AQRL,   WR_xd|RD_xs1|RD_xs2 },
{"amominu.d.aq", "64A", "d,t,0(b)",  MATCH_AMOMINU_D | MASK_AQ, MASK_AMOMINU_D | MASK_AQRL,   WR_xd|RD_xs1|RD_xs2 },
{"lr.d.rl",      "64A", "d,0(b)",    MATCH_LR_D | MASK_RL, MASK_LR_D | MASK_AQRL,   WR_xd|RD_xs1 },
{"sc.d.rl",      "64A", "d,t,0(b)",  MATCH_SC_D | MASK_RL, MASK_SC_D | MASK_AQRL,   WR_xd|RD_xs1|RD_xs2 },
{"amoadd.d.rl",  "64A", "d,t,0(b)",  MATCH_AMOADD_D | MASK_RL, MASK_AMOADD_D | MASK_AQRL,   WR_xd|RD_xs1|RD_xs2 },
{"amoswap.d.rl", "64A", "d,t,0(b)",  MATCH_AMOSWAP_D | MASK_RL, MASK_AMOSWAP_D | MASK_AQRL,   WR_xd|RD_xs1|RD_xs2 },
{"amoand.d.rl",  "64A", "d,t,0(b)",  MATCH_AMOAND_D | MASK_RL, MASK_AMOAND_D | MASK_AQRL,   WR_xd|RD_xs1|RD_xs2 },
{"amoor.d.rl",   "64A", "d,t,0(b)",  MATCH_AMOOR_D | MASK_RL, MASK_AMOOR_D | MASK_AQRL,   WR_xd|RD_xs1|RD_xs2 },
{"amoxor.d.rl",  "64A", "d,t,0(b)",  MATCH_AMOXOR_D | MASK_RL, MASK_AMOXOR_D | MASK_AQRL,   WR_xd|RD_xs1|RD_xs2 },
{"amomax.d.rl",  "64A", "d,t,0(b)",  MATCH_AMOMAX_D | MASK_RL, MASK_AMOMAX_D | MASK_AQRL,   WR_xd|RD_xs1|RD_xs2 },
{"amomaxu.d.rl", "64A", "d,t,0(b)",  MATCH_AMOMAXU_D | MASK_RL, MASK_AMOMAXU_D | MASK_AQRL,   WR_xd|RD_xs1|RD_xs2 },
{"amomin.d.rl",  "64A", "d,t,0(b)",  MATCH_AMOMIN_D | MASK_RL, MASK_AMOMIN_D | MASK_AQRL,   WR_xd|RD_xs1|RD_xs2 },
{"amominu.d.rl", "64A", "d,t,0(b)",  MATCH_AMOMINU_D | MASK_RL, MASK_AMOMINU_D | MASK_AQRL,   WR_xd|RD_xs1|RD_xs2 },
{"lr.d.sc",      "64A", "d,0(b)",    MATCH_LR_D | MASK_AQRL, MASK_LR_D | MASK_AQRL,   WR_xd|RD_xs1 },
{"sc.d.sc",      "64A", "d,t,0(b)",  MATCH_SC_D | MASK_AQRL, MASK_SC_D | MASK_AQRL,   WR_xd|RD_xs1|RD_xs2 },
{"amoadd.d.sc",  "64A", "d,t,0(b)",  MATCH_AMOADD_D | MASK_AQRL, MASK_AMOADD_D | MASK_AQRL,   WR_xd|RD_xs1|RD_xs2 },
{"amoswap.d.sc", "64A", "d,t,0(b)",  MATCH_AMOSWAP_D | MASK_AQRL, MASK_AMOSWAP_D | MASK_AQRL,   WR_xd|RD_xs1|RD_xs2 },
{"amoand.d.sc",  "64A", "d,t,0(b)",  MATCH_AMOAND_D | MASK_AQRL, MASK_AMOAND_D | MASK_AQRL,   WR_xd|RD_xs1|RD_xs2 },
{"amoor.d.sc",   "64A", "d,t,0(b)",  MATCH_AMOOR_D | MASK_AQRL, MASK_AMOOR_D | MASK_AQRL,   WR_xd|RD_xs1|RD_xs2 },
{"amoxor.d.sc",  "64A", "d,t,0(b)",  MATCH_AMOXOR_D | MASK_AQRL, MASK_AMOXOR_D | MASK_AQRL,   WR_xd|RD_xs1|RD_xs2 },
{"amomax.d.sc",  "64A", "d,t,0(b)",  MATCH_AMOMAX_D | MASK_AQRL, MASK_AMOMAX_D | MASK_AQRL,   WR_xd|RD_xs1|RD_xs2 },
{"amomaxu.d.sc", "64A", "d,t,0(b)",  MATCH_AMOMAXU_D | MASK_AQRL, MASK_AMOMAXU_D | MASK_AQRL,   WR_xd|RD_xs1|RD_xs2 },
{"amomin.d.sc",  "64A", "d,t,0(b)",  MATCH_AMOMIN_D | MASK_AQRL, MASK_AMOMIN_D | MASK_AQRL,   WR_xd|RD_xs1|RD_xs2 },
{"amominu.d.sc", "64A", "d,t,0(b)",  MATCH_AMOMINU_D | MASK_AQRL, MASK_AMOMINU_D | MASK_AQRL,   WR_xd|RD_xs1|RD_xs2 },

/* Supervisor instructions */
{"mfpcr",     "I",   "d,E",  MATCH_MFPCR, MASK_MFPCR,  WR_xd },
{"mtpcr",     "I",   "d,t,E",  MATCH_MTPCR, MASK_MTPCR,  WR_xd|RD_xs1 },
{"mtpcr",     "I",   "t,E",  MATCH_MTPCR, MASK_MTPCR | MASK_RD,  RD_xs1 },
{"setpcr",    "I",   "E,j",  MATCH_SETPCR, MASK_SETPCR | MASK_RD, 0 },
{"setpcr",    "I",   "d,E,j",  MATCH_SETPCR, MASK_SETPCR, WR_xd },
{"clearpcr",  "I",   "E,j",  MATCH_CLEARPCR, MASK_CLEARPCR | MASK_RD, 0 },
{"clearpcr",  "I",   "d,E,j",  MATCH_CLEARPCR, MASK_CLEARPCR | MASK_RD, WR_xd },
{"eret",      "I",   "",     MATCH_ERET, MASK_ERET,  0 },

/* Multiply/Divide instruction subset */
{"mul",       "M",   "d,s,t",  MATCH_MUL, MASK_MUL,  WR_xd|RD_xs1|RD_xs2 },
{"mulh",      "M",   "d,s,t",  MATCH_MULH, MASK_MULH,  WR_xd|RD_xs1|RD_xs2 },
{"mulhu",     "M",   "d,s,t",  MATCH_MULHU, MASK_MULHU,  WR_xd|RD_xs1|RD_xs2 },
{"mulhsu",    "M",   "d,s,t",  MATCH_MULHSU, MASK_MULHSU,  WR_xd|RD_xs1|RD_xs2 },
{"div",       "M",   "d,s,t",  MATCH_DIV, MASK_DIV,  WR_xd|RD_xs1|RD_xs2 },
{"divu",      "M",   "d,s,t",  MATCH_DIVU, MASK_DIVU,  WR_xd|RD_xs1|RD_xs2 },
{"rem",       "M",   "d,s,t",  MATCH_REM, MASK_REM,  WR_xd|RD_xs1|RD_xs2 },
{"remu",      "M",   "d,s,t",  MATCH_REMU, MASK_REMU,  WR_xd|RD_xs1|RD_xs2 },
{"mulw",      "64M", "d,s,t",  MATCH_MULW, MASK_MULW,  WR_xd|RD_xs1|RD_xs2 },
{"divw",      "64M", "d,s,t",  MATCH_DIVW, MASK_DIVW,  WR_xd|RD_xs1|RD_xs2 },
{"divuw",     "64M", "d,s,t",  MATCH_DIVUW, MASK_DIVUW,  WR_xd|RD_xs1|RD_xs2 },
{"remw",      "64M", "d,s,t",  MATCH_REMW, MASK_REMW,  WR_xd|RD_xs1|RD_xs2 },
{"remuw",     "64M", "d,s,t",  MATCH_REMUW, MASK_REMUW,  WR_xd|RD_xs1|RD_xs2 },

/* Single-precision floating-point instruction subset */
{"frsr",      "F",   "d",  MATCH_FRSR, MASK_FRSR,  WR_xd },
{"fssr",      "F",   "s",  MATCH_FSSR, MASK_FSSR | MASK_RD,  RD_xs1 },
{"fssr",      "F",   "d,s",  MATCH_FSSR, MASK_FSSR,  WR_xd|RD_xs1 },
{"flw",       "F",   "D,o(b)",  MATCH_FLW, MASK_FLW,   WR_fd|RD_xs1 },
{"fsw",       "F",   "T,q(b)",  MATCH_FSW, MASK_FSW,   RD_xs1|RD_fs2 },
{"fmv.x.s",   "F",   "d,S",  MATCH_FMV_X_S, MASK_FMV_X_S,  WR_xd|RD_fs1 },
{"fmv.s.x",   "F",   "D,s",  MATCH_FMV_S_X, MASK_FMV_S_X,  WR_fd|RD_xs1 },
{"fmv.s",     "F",   "D,S",  0,    (int) M_FMV_S, INSN_MACRO }, /* fsgnj.s */
{"fneg.s",    "F",   "D,S",  0,    (int) M_FNEG_S, INSN_MACRO }, /* fsgnjn.d */
{"fabs.s",    "F",   "D,S",  0,    (int) M_FABS_S, INSN_MACRO }, /* fsgnjx.d */
{"fsgnj.s",   "F",   "D,S,T",  MATCH_FSGNJ_S, MASK_FSGNJ_S,   WR_fd|RD_fs1|RD_fs2 },
{"fsgnjn.s",  "F",   "D,S,T",  MATCH_FSGNJN_S, MASK_FSGNJN_S,   WR_fd|RD_fs1|RD_fs2 },
{"fsgnjx.s",  "F",   "D,S,T",  MATCH_FSGNJX_S, MASK_FSGNJX_S,   WR_fd|RD_fs1|RD_fs2 },
{"fadd.s",    "F",   "D,S,T",  MATCH_FADD_S | MASK_RM, MASK_FADD_S | MASK_RM,   WR_fd|RD_fs1|RD_fs2 },
{"fadd.s",    "F",   "D,S,T,m",  MATCH_FADD_S, MASK_FADD_S,   WR_fd|RD_fs1|RD_fs2 },
{"fsub.s",    "F",   "D,S,T",  MATCH_FSUB_S | MASK_RM, MASK_FSUB_S | MASK_RM,   WR_fd|RD_fs1|RD_fs2 },
{"fsub.s",    "F",   "D,S,T,m",  MATCH_FSUB_S, MASK_FSUB_S,   WR_fd|RD_fs1|RD_fs2 },
{"fmul.s",    "F",   "D,S,T",  MATCH_FMUL_S | MASK_RM, MASK_FMUL_S | MASK_RM,   WR_fd|RD_fs1|RD_fs2 },
{"fmul.s",    "F",   "D,S,T,m",  MATCH_FMUL_S, MASK_FMUL_S,   WR_fd|RD_fs1|RD_fs2 },
{"fdiv.s",    "F",   "D,S,T",  MATCH_FDIV_S | MASK_RM, MASK_FDIV_S | MASK_RM,   WR_fd|RD_fs1|RD_fs2 },
{"fdiv.s",    "F",   "D,S,T,m",  MATCH_FDIV_S, MASK_FDIV_S,   WR_fd|RD_fs1|RD_fs2 },
{"fsqrt.s",   "F",   "D,S",  MATCH_FSQRT_S | MASK_RM, MASK_FSQRT_S | MASK_RM,  WR_fd|RD_fs1 },
{"fsqrt.s",   "F",   "D,S,m",  MATCH_FSQRT_S, MASK_FSQRT_S,  WR_fd|RD_fs1 },
{"fmin.s",    "F",   "D,S,T",  MATCH_FMIN_S, MASK_FMIN_S,   WR_fd|RD_fs1|RD_fs2 },
{"fmax.s",    "F",   "D,S,T",  MATCH_FMAX_S, MASK_FMAX_S,   WR_fd|RD_fs1|RD_fs2 },
{"fmadd.s",   "F",   "D,S,T,R",  MATCH_FMADD_S | MASK_RM, MASK_FMADD_S | MASK_RM,   WR_fd|RD_fs1|RD_fs2|RD_fs3 },
{"fmadd.s",   "F",   "D,S,T,R,m",  MATCH_FMADD_S, MASK_FMADD_S,   WR_fd|RD_fs1|RD_fs2|RD_fs3 },
{"fnmadd.s",  "F",   "D,S,T,R",  MATCH_FNMADD_S | MASK_RM, MASK_FNMADD_S | MASK_RM,   WR_fd|RD_fs1|RD_fs2|RD_fs3 },
{"fnmadd.s",  "F",   "D,S,T,R,m",  MATCH_FNMADD_S, MASK_FNMADD_S,   WR_fd|RD_fs1|RD_fs2|RD_fs3 },
{"fmsub.s",   "F",   "D,S,T,R",  MATCH_FMSUB_S | MASK_RM, MASK_FMSUB_S | MASK_RM,   WR_fd|RD_fs1|RD_fs2|RD_fs3 },
{"fmsub.s",   "F",   "D,S,T,R,m",  MATCH_FMSUB_S, MASK_FMSUB_S,   WR_fd|RD_fs1|RD_fs2|RD_fs3 },
{"fnmsub.s",  "F",   "D,S,T,R",  MATCH_FNMSUB_S | MASK_RM, MASK_FNMSUB_S | MASK_RM,   WR_fd|RD_fs1|RD_fs2|RD_fs3 },
{"fnmsub.s",  "F",   "D,S,T,R,m",  MATCH_FNMSUB_S, MASK_FNMSUB_S,   WR_fd|RD_fs1|RD_fs2|RD_fs3 },
{"fcvt.w.s",  "F",   "d,S",  MATCH_FCVT_W_S | MASK_RM, MASK_FCVT_W_S | MASK_RM,  WR_xd|RD_fs1 },
{"fcvt.w.s",  "F",   "d,S,m",  MATCH_FCVT_W_S, MASK_FCVT_W_S,  WR_xd|RD_fs1 },
{"fcvt.wu.s", "F",   "d,S",  MATCH_FCVT_WU_S | MASK_RM, MASK_FCVT_WU_S | MASK_RM,  WR_xd|RD_fs1 },
{"fcvt.wu.s", "F",   "d,S,m",  MATCH_FCVT_WU_S, MASK_FCVT_WU_S,  WR_xd|RD_fs1 },
{"fcvt.s.w",  "F",   "D,s",  MATCH_FCVT_S_W | MASK_RM, MASK_FCVT_S_W | MASK_RM,   WR_fd|RD_xs1 },
{"fcvt.s.w",  "F",   "D,s,m",  MATCH_FCVT_S_W, MASK_FCVT_S_W,   WR_fd|RD_xs1 },
{"fcvt.s.wu", "F",   "D,s",  MATCH_FCVT_S_WU | MASK_RM, MASK_FCVT_S_W | MASK_RM,   WR_fd|RD_xs1 },
{"fcvt.s.wu", "F",   "D,s,m",  MATCH_FCVT_S_WU, MASK_FCVT_S_WU,   WR_fd|RD_xs1 },
{"feq.s",     "F",   "d,S,T",    MATCH_FEQ_S, MASK_FEQ_S,  WR_xd|RD_fs1|RD_fs2 },
{"flt.s",     "F",   "d,S,T",    MATCH_FLT_S, MASK_FLT_S,  WR_xd|RD_fs1|RD_fs2 },
{"fle.s",     "F",   "d,S,T",    MATCH_FLE_S, MASK_FLE_S,  WR_xd|RD_fs1|RD_fs2 },
{"fcvt.l.s",  "64F", "d,S",  MATCH_FCVT_L_S | MASK_RM, MASK_FCVT_L_S | MASK_RM,  WR_xd|RD_fs1 },
{"fcvt.l.s",  "64F", "d,S,m",  MATCH_FCVT_L_S, MASK_FCVT_L_S,  WR_xd|RD_fs1 },
{"fcvt.lu.s", "64F", "d,S",  MATCH_FCVT_LU_S | MASK_RM, MASK_FCVT_LU_S | MASK_RM,  WR_xd|RD_fs1 },
{"fcvt.lu.s", "64F", "d,S,m",  MATCH_FCVT_LU_S, MASK_FCVT_LU_S,  WR_xd|RD_fs1 },
{"fcvt.s.l",  "64F", "D,s",  MATCH_FCVT_S_L | MASK_RM, MASK_FCVT_S_L | MASK_RM,   WR_fd|RD_xs1 },
{"fcvt.s.l",  "64F", "D,s,m",  MATCH_FCVT_S_L, MASK_FCVT_S_L,   WR_fd|RD_xs1 },
{"fcvt.s.lu", "64F", "D,s",  MATCH_FCVT_S_LU | MASK_RM, MASK_FCVT_S_L | MASK_RM,   WR_fd|RD_xs1 },
{"fcvt.s.lu", "64F", "D,s,m",  MATCH_FCVT_S_LU, MASK_FCVT_S_LU,   WR_fd|RD_xs1 },

/* Double-precision floating-point instruction subset */
{"fld",       "D",   "D,o(b)",  MATCH_FLD, MASK_FLD,  WR_fd|RD_xs1 },
{"fsd",       "D",   "T,q(b)",  MATCH_FSD, MASK_FSD,  RD_xs1|RD_fs2 },
{"fmv.d",     "D",   "D,S",  0,    (int) M_FMV_D, INSN_MACRO }, /* fsgnj.d */
{"fneg.d",    "D",   "D,S",  0,    (int) M_FNEG_D, INSN_MACRO }, /* fsgnjn.d */
{"fabs.d",    "D",   "D,S",  0,    (int) M_FABS_D, INSN_MACRO }, /* fsgnjx.d */
{"fsgnj.d",   "D",   "D,S,T",  MATCH_FSGNJ_D, MASK_FSGNJ_D,   WR_fd|RD_fs1|RD_fs2 },
{"fsgnjn.d",  "D",   "D,S,T",  MATCH_FSGNJN_D, MASK_FSGNJN_D,   WR_fd|RD_fs1|RD_fs2 },
{"fsgnjx.d",  "D",   "D,S,T",  MATCH_FSGNJX_D, MASK_FSGNJX_D,   WR_fd|RD_fs1|RD_fs2 },
{"fadd.d",    "D",   "D,S,T",  MATCH_FADD_D | MASK_RM, MASK_FADD_D | MASK_RM,   WR_fd|RD_fs1|RD_fs2 },
{"fadd.d",    "D",   "D,S,T,m",  MATCH_FADD_D, MASK_FADD_D,   WR_fd|RD_fs1|RD_fs2 },
{"fsub.d",    "D",   "D,S,T",  MATCH_FSUB_D | MASK_RM, MASK_FSUB_D | MASK_RM,   WR_fd|RD_fs1|RD_fs2 },
{"fsub.d",    "D",   "D,S,T,m",  MATCH_FSUB_D, MASK_FSUB_D,   WR_fd|RD_fs1|RD_fs2 },
{"fmul.d",    "D",   "D,S,T",  MATCH_FMUL_D | MASK_RM, MASK_FMUL_D | MASK_RM,   WR_fd|RD_fs1|RD_fs2 },
{"fmul.d",    "D",   "D,S,T,m",  MATCH_FMUL_D, MASK_FMUL_D,   WR_fd|RD_fs1|RD_fs2 },
{"fdiv.d",    "D",   "D,S,T",  MATCH_FDIV_D | MASK_RM, MASK_FDIV_D | MASK_RM,   WR_fd|RD_fs1|RD_fs2 },
{"fdiv.d",    "D",   "D,S,T,m",  MATCH_FDIV_D, MASK_FDIV_D,   WR_fd|RD_fs1|RD_fs2 },
{"fsqrt.d",   "D",   "D,S",  MATCH_FSQRT_D | MASK_RM, MASK_FSQRT_D | MASK_RM,  WR_fd|RD_fs1 },
{"fsqrt.d",   "D",   "D,S,m",  MATCH_FSQRT_D, MASK_FSQRT_D,  WR_fd|RD_fs1 },
{"fmin.d",    "D",   "D,S,T",  MATCH_FMIN_D, MASK_FMIN_D,   WR_fd|RD_fs1|RD_fs2 },
{"fmax.d",    "D",   "D,S,T",  MATCH_FMAX_D, MASK_FMAX_D,   WR_fd|RD_fs1|RD_fs2 },
{"fmadd.d",   "D",   "D,S,T,R",  MATCH_FMADD_D | MASK_RM, MASK_FMADD_D | MASK_RM,   WR_fd|RD_fs1|RD_fs2|RD_fs3 },
{"fmadd.d",   "D",   "D,S,T,R,m",  MATCH_FMADD_D, MASK_FMADD_D,   WR_fd|RD_fs1|RD_fs2|RD_fs3 },
{"fnmadd.d",  "D",   "D,S,T,R",  MATCH_FNMADD_D | MASK_RM, MASK_FNMADD_D | MASK_RM,   WR_fd|RD_fs1|RD_fs2|RD_fs3 },
{"fnmadd.d",  "D",   "D,S,T,R,m",  MATCH_FNMADD_D, MASK_FNMADD_D,   WR_fd|RD_fs1|RD_fs2|RD_fs3 },
{"fmsub.d",   "D",   "D,S,T,R",  MATCH_FMSUB_D | MASK_RM, MASK_FMSUB_D | MASK_RM,   WR_fd|RD_fs1|RD_fs2|RD_fs3 },
{"fmsub.d",   "D",   "D,S,T,R,m",  MATCH_FMSUB_D, MASK_FMSUB_D,   WR_fd|RD_fs1|RD_fs2|RD_fs3 },
{"fnmsub.d",  "D",   "D,S,T,R",  MATCH_FNMSUB_D | MASK_RM, MASK_FNMSUB_D | MASK_RM,   WR_fd|RD_fs1|RD_fs2|RD_fs3 },
{"fnmsub.d",  "D",   "D,S,T,R,m",  MATCH_FNMSUB_D, MASK_FNMSUB_D,   WR_fd|RD_fs1|RD_fs2|RD_fs3 },
{"fcvt.w.d",  "D",   "d,S",  MATCH_FCVT_W_D | MASK_RM, MASK_FCVT_W_D | MASK_RM,  WR_xd|RD_fs1 },
{"fcvt.w.d",  "D",   "d,S,m",  MATCH_FCVT_W_D, MASK_FCVT_W_D,  WR_xd|RD_fs1 },
{"fcvt.wu.d", "D",   "d,S",  MATCH_FCVT_WU_D | MASK_RM, MASK_FCVT_WU_D | MASK_RM,  WR_xd|RD_fs1 },
{"fcvt.wu.d", "D",   "d,S,m",  MATCH_FCVT_WU_D, MASK_FCVT_WU_D,  WR_xd|RD_fs1 },
{"fcvt.d.w",  "D",   "D,s",  MATCH_FCVT_D_W, MASK_FCVT_D_W | MASK_RM,   WR_fd|RD_xs1 },
{"fcvt.d.wu", "D",   "D,s",  MATCH_FCVT_D_WU, MASK_FCVT_D_WU | MASK_RM,   WR_fd|RD_xs1 },
{"fcvt.d.s",  "D",   "D,S",  MATCH_FCVT_D_S, MASK_FCVT_D_S | MASK_RM,   WR_fd|RD_fs1 },
{"fcvt.s.d",  "D",   "D,S",  MATCH_FCVT_S_D | MASK_RM, MASK_FCVT_S_D | MASK_RM,   WR_fd|RD_fs1 },
{"fcvt.s.d",  "D",   "D,S,m",  MATCH_FCVT_S_D, MASK_FCVT_S_D,   WR_fd|RD_fs1 },
{"feq.d",     "D",   "d,S,T",    MATCH_FEQ_D, MASK_FEQ_D,  WR_xd|RD_fs1|RD_fs2 },
{"flt.d",     "D",   "d,S,T",    MATCH_FLT_D, MASK_FLT_D,  WR_xd|RD_fs1|RD_fs2 },
{"fle.d",     "D",   "d,S,T",    MATCH_FLE_D, MASK_FLE_D,  WR_xd|RD_fs1|RD_fs2 },
{"fmv.x.d",   "64D", "d,S",  MATCH_FMV_X_D, MASK_FMV_X_D,  WR_xd|RD_fs1 },
{"fmv.d.x",   "64D", "D,s",  MATCH_FMV_D_X, MASK_FMV_D_X,  WR_fd|RD_xs1 },
{"fcvt.l.d",  "64D", "d,S",  MATCH_FCVT_L_D | MASK_RM, MASK_FCVT_L_D | MASK_RM,  WR_xd|RD_fs1 },
{"fcvt.l.d",  "64D", "d,S,m",  MATCH_FCVT_L_D, MASK_FCVT_L_D,  WR_xd|RD_fs1 },
{"fcvt.lu.d", "64D", "d,S",  MATCH_FCVT_LU_D | MASK_RM, MASK_FCVT_LU_D | MASK_RM,  WR_xd|RD_fs1 },
{"fcvt.lu.d", "64D", "d,S,m",  MATCH_FCVT_LU_D, MASK_FCVT_LU_D,  WR_xd|RD_fs1 },
{"fcvt.d.l",  "64D", "D,s",  MATCH_FCVT_D_L | MASK_RM, MASK_FCVT_D_L | MASK_RM,   WR_fd|RD_xs1 },
{"fcvt.d.l",  "64D", "D,s,m",  MATCH_FCVT_D_L, MASK_FCVT_D_L,   WR_fd|RD_xs1 },
{"fcvt.d.lu", "64D", "D,s",  MATCH_FCVT_D_LU | MASK_RM, MASK_FCVT_D_L | MASK_RM,   WR_fd|RD_xs1 },
{"fcvt.d.lu", "64D", "D,s,m",  MATCH_FCVT_D_LU, MASK_FCVT_D_LU,   WR_fd|RD_xs1 },

/* Half-precision floating-point instruction subset */
{"flh",       "Xhalf",   "D,o(b)",  MATCH_FLH, MASK_FLH,  WR_fd|RD_xs1 },
{"fsh",       "Xhalf",   "T,q(b)",  MATCH_FSH, MASK_FSH,  RD_xs1|RD_fs2 },
{"fsgnj.h",   "Xhalf",   "D,S,T",  MATCH_FSGNJ_H, MASK_FSGNJ_H,   WR_fd|RD_fs1|RD_fs2 },
{"fsgnjn.h",  "Xhalf",   "D,S,T",  MATCH_FSGNJN_H, MASK_FSGNJN_H,   WR_fd|RD_fs1|RD_fs2 },
{"fsgnjx.h",  "Xhalf",   "D,S,T",  MATCH_FSGNJX_H, MASK_FSGNJX_H,   WR_fd|RD_fs1|RD_fs2 },
{"fadd.h",    "Xhalf",   "D,S,T",  MATCH_FADD_H | MASK_RM, MASK_FADD_H | MASK_RM,   WR_fd|RD_fs1|RD_fs2 },
{"fadd.h",    "Xhalf",   "D,S,T,m",  MATCH_FADD_H, MASK_FADD_H,   WR_fd|RD_fs1|RD_fs2 },
{"fsub.h",    "Xhalf",   "D,S,T",  MATCH_FSUB_H | MASK_RM, MASK_FSUB_H | MASK_RM,   WR_fd|RD_fs1|RD_fs2 },
{"fsub.h",    "Xhalf",   "D,S,T,m",  MATCH_FSUB_H, MASK_FSUB_H,   WR_fd|RD_fs1|RD_fs2 },
{"fmul.h",    "Xhalf",   "D,S,T",  MATCH_FMUL_H | MASK_RM, MASK_FMUL_H | MASK_RM,   WR_fd|RD_fs1|RD_fs2 },
{"fmul.h",    "Xhalf",   "D,S,T,m",  MATCH_FMUL_H, MASK_FMUL_H,   WR_fd|RD_fs1|RD_fs2 },
{"fdiv.h",    "Xhalf",   "D,S,T",  MATCH_FDIV_H | MASK_RM, MASK_FDIV_H | MASK_RM,   WR_fd|RD_fs1|RD_fs2 },
{"fdiv.h",    "Xhalf",   "D,S,T,m",  MATCH_FDIV_H, MASK_FDIV_H,   WR_fd|RD_fs1|RD_fs2 },
{"fsqrt.h",   "Xhalf",   "D,S",  MATCH_FSQRT_H | MASK_RM, MASK_FSQRT_H | MASK_RM,  WR_fd|RD_fs1 },
{"fsqrt.h",   "Xhalf",   "D,S,m",  MATCH_FSQRT_H, MASK_FSQRT_H,  WR_fd|RD_fs1 },
{"fmin.h",    "Xhalf",   "D,S,T",  MATCH_FMIN_H, MASK_FMIN_H,   WR_fd|RD_fs1|RD_fs2 },
{"fmax.h",    "Xhalf",   "D,S,T",  MATCH_FMAX_H, MASK_FMAX_H,   WR_fd|RD_fs1|RD_fs2 },
{"fmadd.h",   "Xhalf",   "D,S,T,R",  MATCH_FMADD_H | MASK_RM, MASK_FMADD_H | MASK_RM,   WR_fd|RD_fs1|RD_fs2|RD_fs3 },
{"fmadd.h",   "Xhalf",   "D,S,T,R,m",  MATCH_FMADD_H, MASK_FMADD_H,   WR_fd|RD_fs1|RD_fs2|RD_fs3 },
{"fnmadd.h",  "Xhalf",   "D,S,T,R",  MATCH_FNMADD_H | MASK_RM, MASK_FNMADD_H | MASK_RM,   WR_fd|RD_fs1|RD_fs2|RD_fs3 },
{"fnmadd.h",  "Xhalf",   "D,S,T,R,m",  MATCH_FNMADD_H, MASK_FNMADD_H,   WR_fd|RD_fs1|RD_fs2|RD_fs3 },
{"fmsub.h",   "Xhalf",   "D,S,T,R",  MATCH_FMSUB_H | MASK_RM, MASK_FMSUB_H | MASK_RM,   WR_fd|RD_fs1|RD_fs2|RD_fs3 },
{"fmsub.h",   "Xhalf",   "D,S,T,R,m",  MATCH_FMSUB_H, MASK_FMSUB_H,   WR_fd|RD_fs1|RD_fs2|RD_fs3 },
{"fnmsub.h",  "Xhalf",   "D,S,T,R",  MATCH_FNMSUB_H | MASK_RM, MASK_FNMSUB_H | MASK_RM,   WR_fd|RD_fs1|RD_fs2|RD_fs3 },
{"fnmsub.h",  "Xhalf",   "D,S,T,R,m",  MATCH_FNMSUB_H, MASK_FNMSUB_H,   WR_fd|RD_fs1|RD_fs2|RD_fs3 },
{"fcvt.s.h",  "Xhalf",   "D,S",  MATCH_FCVT_S_H, MASK_FCVT_S_H | MASK_RM,   WR_fd|RD_fs1 },
{"fcvt.h.s",  "Xhalf",   "D,S",  MATCH_FCVT_H_S | MASK_RM, MASK_FCVT_H_S | MASK_RM,   WR_fd|RD_fs1 },
{"fcvt.h.s",  "Xhalf",   "D,S,m",  MATCH_FCVT_H_S, MASK_FCVT_H_S,   WR_fd|RD_fs1 },
{"fcvt.d.h",  "Xhalf",   "D,S",  MATCH_FCVT_D_H, MASK_FCVT_D_H | MASK_RM,   WR_fd|RD_fs1 },
{"fcvt.h.d",  "Xhalf",   "D,S",  MATCH_FCVT_H_D | MASK_RM, MASK_FCVT_H_D | MASK_RM,   WR_fd|RD_fs1 },
{"fcvt.h.d",  "Xhalf",   "D,S,m",  MATCH_FCVT_H_D, MASK_FCVT_H_D,   WR_fd|RD_fs1 },
{"feq.h",     "Xhalf",   "d,S,T",    MATCH_FEQ_H, MASK_FEQ_H,  WR_xd|RD_fs1|RD_fs2 },
{"flt.h",     "Xhalf",   "d,S,T",    MATCH_FLT_H, MASK_FLT_H,  WR_xd|RD_fs1|RD_fs2 },
{"fle.h",     "Xhalf",   "d,S,T",    MATCH_FLE_H, MASK_FLE_H,  WR_xd|RD_fs1|RD_fs2 },
{"fmv.x.h",   "Xhalf",   "d,S",  MATCH_FMV_X_H, MASK_FMV_X_H,  WR_xd|RD_fs1 },
{"fmv.h.x",   "Xhalf",   "D,s",  MATCH_FMV_H_X, MASK_FMV_H_X,  WR_fd|RD_xs1 },
{"fcvt.w.h",  "Xhalf",   "d,S",  MATCH_FCVT_W_H | MASK_RM, MASK_FCVT_W_H | MASK_RM,  WR_xd|RD_fs1 },
{"fcvt.w.h",  "Xhalf",   "d,S,m",  MATCH_FCVT_W_H, MASK_FCVT_W_H,  WR_xd|RD_fs1 },
{"fcvt.wu.h", "Xhalf",   "d,S",  MATCH_FCVT_WU_H | MASK_RM, MASK_FCVT_WU_H | MASK_RM,  WR_xd|RD_fs1 },
{"fcvt.wu.h", "Xhalf",   "d,S,m",  MATCH_FCVT_WU_H, MASK_FCVT_WU_H,  WR_xd|RD_fs1 },
{"fcvt.h.w",  "Xhalf",   "D,s",  MATCH_FCVT_H_W, MASK_FCVT_H_W | MASK_RM,   WR_fd|RD_xs1 },
{"fcvt.h.wu", "Xhalf",   "D,s",  MATCH_FCVT_H_WU, MASK_FCVT_H_WU | MASK_RM,   WR_fd|RD_xs1 },
{"fcvt.l.h",  "Xhalf", "d,S",  MATCH_FCVT_L_H | MASK_RM, MASK_FCVT_L_H | MASK_RM,  WR_xd|RD_fs1 },
{"fcvt.l.h",  "Xhalf", "d,S,m",  MATCH_FCVT_L_H, MASK_FCVT_L_H,  WR_xd|RD_fs1 },
{"fcvt.lu.h", "Xhalf", "d,S",  MATCH_FCVT_LU_H | MASK_RM, MASK_FCVT_LU_H | MASK_RM,  WR_xd|RD_fs1 },
{"fcvt.lu.h", "Xhalf", "d,S,m",  MATCH_FCVT_LU_H, MASK_FCVT_LU_H,  WR_xd|RD_fs1 },
{"fcvt.h.l",  "Xhalf", "D,s",  MATCH_FCVT_H_L | MASK_RM, MASK_FCVT_H_L | MASK_RM,   WR_fd|RD_xs1 },
{"fcvt.h.l",  "Xhalf", "D,s,m",  MATCH_FCVT_H_L, MASK_FCVT_H_L,   WR_fd|RD_xs1 },
{"fcvt.h.lu", "Xhalf", "D,s",  MATCH_FCVT_H_LU | MASK_RM, MASK_FCVT_H_L | MASK_RM,   WR_fd|RD_xs1 },
{"fcvt.h.lu", "Xhalf", "D,s,m",  MATCH_FCVT_H_LU, MASK_FCVT_H_LU,   WR_fd|RD_xs1 },

/* Rocket Custom Coprocessor extension */
{"custom0",   "Xcustom", "d,s,t,^j", MATCH_CUSTOM0_RD_RS1_RS2, MASK_CUSTOM0_RD_RS1_RS2, 0},
{"custom0",   "Xcustom", "d,s,^t,^j", MATCH_CUSTOM0_RD_RS1, MASK_CUSTOM0_RD_RS1, 0},
{"custom0",   "Xcustom", "d,^s,^t,^j", MATCH_CUSTOM0_RD, MASK_CUSTOM0_RD, 0},
{"custom0",   "Xcustom", "^d,s,t,^j", MATCH_CUSTOM0_RS1_RS2, MASK_CUSTOM0_RS1_RS2, 0},
{"custom0",   "Xcustom", "^d,s,^t,^j", MATCH_CUSTOM0_RS1, MASK_CUSTOM0_RS1, 0},
{"custom0",   "Xcustom", "^d,^s,^t,^j", MATCH_CUSTOM0, MASK_CUSTOM0, 0},
{"custom1",   "Xcustom", "d,s,t,^j", MATCH_CUSTOM1_RD_RS1_RS2, MASK_CUSTOM1_RD_RS1_RS2, 0},
{"custom1",   "Xcustom", "d,s,^t,^j", MATCH_CUSTOM1_RD_RS1, MASK_CUSTOM1_RD_RS1, 0},
{"custom1",   "Xcustom", "d,^s,^t,^j", MATCH_CUSTOM1_RD, MASK_CUSTOM1_RD, 0},
{"custom1",   "Xcustom", "^d,s,t,^j", MATCH_CUSTOM1_RS1_RS2, MASK_CUSTOM1_RS1_RS2, 0},
{"custom1",   "Xcustom", "^d,s,^t,^j", MATCH_CUSTOM1_RS1, MASK_CUSTOM1_RS1, 0},
{"custom1",   "Xcustom", "^d,^s,^t,^j", MATCH_CUSTOM1, MASK_CUSTOM1, 0},
{"custom2",   "Xcustom", "d,s,t,^j", MATCH_CUSTOM2_RD_RS1_RS2, MASK_CUSTOM2_RD_RS1_RS2, 0},
{"custom2",   "Xcustom", "d,s,^t,^j", MATCH_CUSTOM2_RD_RS1, MASK_CUSTOM2_RD_RS1, 0},
{"custom2",   "Xcustom", "d,^s,^t,^j", MATCH_CUSTOM2_RD, MASK_CUSTOM2_RD, 0},
{"custom2",   "Xcustom", "^d,s,t,^j", MATCH_CUSTOM2_RS1_RS2, MASK_CUSTOM2_RS1_RS2, 0},
{"custom2",   "Xcustom", "^d,s,^t,^j", MATCH_CUSTOM2_RS1, MASK_CUSTOM2_RS1, 0},
{"custom2",   "Xcustom", "^d,^s,^t,^j", MATCH_CUSTOM2, MASK_CUSTOM2, 0},
{"custom3",   "Xcustom", "d,s,t,^j", MATCH_CUSTOM3_RD_RS1_RS2, MASK_CUSTOM3_RD_RS1_RS2, 0},
{"custom3",   "Xcustom", "d,s,^t,^j", MATCH_CUSTOM3_RD_RS1, MASK_CUSTOM3_RD_RS1, 0},
{"custom3",   "Xcustom", "d,^s,^t,^j", MATCH_CUSTOM3_RD, MASK_CUSTOM3_RD, 0},
{"custom3",   "Xcustom", "^d,s,t,^j", MATCH_CUSTOM3_RS1_RS2, MASK_CUSTOM3_RS1_RS2, 0},
{"custom3",   "Xcustom", "^d,s,^t,^j", MATCH_CUSTOM3_RS1, MASK_CUSTOM3_RS1, 0},
{"custom3",   "Xcustom", "^d,^s,^t,^j", MATCH_CUSTOM3, MASK_CUSTOM3, 0},

/* Xhwacha extension */
{"stop",      "Xhwacha", "", MATCH_STOP, MASK_STOP, 0},
{"utidx",     "Xhwacha", "d", MATCH_UTIDX, MASK_UTIDX, WR_xd},
{"movz",      "Xhwacha", "d,s,t", MATCH_MOVZ, MASK_MOVZ, WR_xd|RD_xs1|RD_xs2},
{"movn",      "Xhwacha", "d,s,t", MATCH_MOVN, MASK_MOVN, WR_xd|RD_xs1|RD_xs2},
{"fmovz",     "Xhwacha", "D,s,T", MATCH_FMOVZ, MASK_FMOVZ, WR_fd|RD_xs1|RD_fs2},
{"fmovn",     "Xhwacha", "D,s,T", MATCH_FMOVN, MASK_FMOVN, WR_fd|RD_xs1|RD_fs2},

/* unit stride */
/* xloads */
{"vld",       "Xhwacha", "#d,s", MATCH_VLD, MASK_VLD, 0},
{"vlw",       "Xhwacha", "#d,s", MATCH_VLW, MASK_VLW, 0},
{"vlwu",      "Xhwacha", "#d,s", MATCH_VLWU, MASK_VLWU, 0},
{"vlh",       "Xhwacha", "#d,s", MATCH_VLH, MASK_VLH, 0},
{"vlhu",      "Xhwacha", "#d,s", MATCH_VLHU, MASK_VLHU, 0},
{"vlb",       "Xhwacha", "#d,s", MATCH_VLB, MASK_VLB, 0},
{"vlbu",      "Xhwacha", "#d,s", MATCH_VLBU, MASK_VLBU, 0},
/* floads */
{"vfld",      "Xhwacha", "#D,s", MATCH_VFLD, MASK_VFLD, 0},
{"vflw",      "Xhwacha", "#D,s", MATCH_VFLW, MASK_VFLW, 0},

/* stride */
/* xloads */
{"vlstd",     "Xhwacha", "#d,s,t", MATCH_VLSTD, MASK_VLSTD, 0},
{"vlstw",     "Xhwacha", "#d,s,t", MATCH_VLSTW, MASK_VLSTW, 0},
{"vlstwu",    "Xhwacha", "#d,s,t", MATCH_VLSTWU, MASK_VLSTWU, 0},
{"vlsth",     "Xhwacha", "#d,s,t", MATCH_VLSTH, MASK_VLSTH, 0},
{"vlsthu",    "Xhwacha", "#d,s,t", MATCH_VLSTHU, MASK_VLSTHU, 0},
{"vlstb",     "Xhwacha", "#d,s,t", MATCH_VLSTB, MASK_VLSTB, 0},
{"vlstbu",    "Xhwacha", "#d,s,t", MATCH_VLSTBU, MASK_VLSTBU, 0},
/* floads */
{"vflstd",    "Xhwacha", "#D,s,t", MATCH_VFLSTD, MASK_VFLSTD, 0},
{"vflstw",    "Xhwacha", "#D,s,t", MATCH_VFLSTW, MASK_VFLSTW, 0},

/* segment */
/* xloads */
{"vlsegd",    "Xhwacha", "#d,s,#n", MATCH_VLSEGD, MASK_VLSEGD, 0},
{"vlsegw",    "Xhwacha", "#d,s,#n", MATCH_VLSEGW, MASK_VLSEGW, 0},
{"vlsegwu",   "Xhwacha", "#d,s,#n", MATCH_VLSEGWU, MASK_VLSEGWU, 0},
{"vlsegh",    "Xhwacha", "#d,s,#n", MATCH_VLSEGH, MASK_VLSEGH, 0},
{"vlseghu",   "Xhwacha", "#d,s,#n", MATCH_VLSEGHU, MASK_VLSEGHU, 0},
{"vlsegb",    "Xhwacha", "#d,s,#n", MATCH_VLSEGB, MASK_VLSEGB, 0},
{"vlsegbu",   "Xhwacha", "#d,s,#n", MATCH_VLSEGBU, MASK_VLSEGBU, 0},
/* floads */
{"vflsegd",   "Xhwacha", "#D,s,#n", MATCH_VFLSEGD, MASK_VFLSEGD, 0},
{"vflsegw",   "Xhwacha", "#D,s,#n", MATCH_VFLSEGW, MASK_VFLSEGW, 0},

/* stride segment */
/* xloads */
{"vlsegstd",  "Xhwacha", "#d,s,t,#n", MATCH_VLSEGSTD, MASK_VLSEGSTD, 0},
{"vlsegstw",  "Xhwacha", "#d,s,t,#n", MATCH_VLSEGSTW, MASK_VLSEGSTW, 0},
{"vlsegstwu", "Xhwacha", "#d,s,t,#n", MATCH_VLSEGSTWU, MASK_VLSEGSTWU, 0},
{"vlsegsth",  "Xhwacha", "#d,s,t,#n", MATCH_VLSEGSTH, MASK_VLSEGSTH, 0},
{"vlsegsthu", "Xhwacha", "#d,s,t,#n", MATCH_VLSEGSTHU, MASK_VLSEGSTHU, 0},
{"vlsegstb",  "Xhwacha", "#d,s,t,#n", MATCH_VLSEGSTB, MASK_VLSEGSTB, 0},
{"vlsegstbu", "Xhwacha", "#d,s,t,#n", MATCH_VLSEGSTBU, MASK_VLSEGSTBU, 0},
/* floads */
{"vflsegstd", "Xhwacha", "#D,s,t,#n", MATCH_VFLSEGSTD, MASK_VFLSEGSTD, 0},
{"vflsegstw", "Xhwacha", "#D,s,t,#n", MATCH_VFLSEGSTW, MASK_VFLSEGSTW, 0},

/* unit stride */
/* xstores */
{"vsd",       "Xhwacha", "#d,s", MATCH_VSD, MASK_VSD, 0},
{"vsw",       "Xhwacha", "#d,s", MATCH_VSW, MASK_VSW, 0},
{"vsh",       "Xhwacha", "#d,s", MATCH_VSH, MASK_VSH, 0},
{"vsb",       "Xhwacha", "#d,s", MATCH_VSB, MASK_VSB, 0},
/* fstores */
{"vfsd",      "Xhwacha", "#D,s", MATCH_VFSD, MASK_VFSD, 0},
{"vfsw",      "Xhwacha", "#D,s", MATCH_VFSW, MASK_VFSW, 0},

/* stride */
/* xstores */
{"vsstd",     "Xhwacha", "#d,s,t", MATCH_VSSTD, MASK_VSSTD, 0},
{"vsstw",     "Xhwacha", "#d,s,t", MATCH_VSSTW, MASK_VSSTW, 0},
{"vssth",     "Xhwacha", "#d,s,t", MATCH_VSSTH, MASK_VSSTH, 0},
{"vsstb",     "Xhwacha", "#d,s,t", MATCH_VSSTB, MASK_VSSTB, 0},
/* fstores */
{"vfsstd",    "Xhwacha", "#D,s,t", MATCH_VFSSTD, MASK_VFSSTD, 0},
{"vfsstw",    "Xhwacha", "#D,s,t", MATCH_VFSSTW, MASK_VFSSTW, 0},

/* segment */
/* xstores */
{"vssegd",    "Xhwacha", "#d,s,#n", MATCH_VSSEGD, MASK_VSSEGD, 0},
{"vssegw",    "Xhwacha", "#d,s,#n", MATCH_VSSEGW, MASK_VSSEGW, 0},
{"vssegh",    "Xhwacha", "#d,s,#n", MATCH_VSSEGH, MASK_VSSEGH, 0},
{"vssegb",    "Xhwacha", "#d,s,#n", MATCH_VSSEGB, MASK_VSSEGB, 0},
/* fstores */
{"vfssegd",   "Xhwacha", "#D,s,#n", MATCH_VFSSEGD, MASK_VFSSEGD, 0},
{"vfssegw",   "Xhwacha", "#D,s,#n", MATCH_VFSSEGW, MASK_VFSSEGW, 0},

/* stride segment */
/* xsegstores */
{"vssegstd",  "Xhwacha", "#d,s,t,#n", MATCH_VSSEGSTD, MASK_VSSEGSTD, 0},
{"vssegstw",  "Xhwacha", "#d,s,t,#n", MATCH_VSSEGSTW, MASK_VSSEGSTW, 0},
{"vssegsth",  "Xhwacha", "#d,s,t,#n", MATCH_VSSEGSTH, MASK_VSSEGSTH, 0},
{"vssegstb",  "Xhwacha", "#d,s,t,#n", MATCH_VSSEGSTB, MASK_VSSEGSTB, 0},
/* fsegstores */
{"vfssegstd", "Xhwacha", "#D,s,t,#n", MATCH_VFSSEGSTD, MASK_VFSSEGSTD, 0},
{"vfssegstw", "Xhwacha", "#D,s,t,#n", MATCH_VFSSEGSTW, MASK_VFSSEGSTW, 0},

{"vsetcfg",   "Xhwacha", "s", MATCH_VSETCFG, MASK_VSETCFG | MASK_IMM, 0},
{"vsetcfg",   "Xhwacha", "#g,#f", MATCH_VSETCFG, MASK_VSETCFG | MASK_RS1, 0},
{"vsetcfg",   "Xhwacha", "s,#g,#f", MATCH_VSETCFG, MASK_VSETCFG, 0},
{"vsetvl",    "Xhwacha", "d,s", MATCH_VSETVL, MASK_VSETVL, 0},
{"vgetcfg",   "Xhwacha", "d", MATCH_VGETCFG, MASK_VGETCFG, 0},
{"vgetvl",    "Xhwacha", "d", MATCH_VGETVL, MASK_VGETVL, 0},

{"vmvv",      "Xhwacha", "#d,#s", MATCH_VMVV, MASK_VMVV, 0},
{"vmsv",      "Xhwacha", "#d,s", MATCH_VMSV, MASK_VMSV, 0},
{"vf",        "Xhwacha", "q(b)", MATCH_VF, MASK_VF, 0},

{"vxcptcause",   "Xhwacha", "d", MATCH_VXCPTCAUSE, MASK_VXCPTCAUSE, 0},
{"vxcptaux",     "Xhwacha", "d", MATCH_VXCPTAUX, MASK_VXCPTAUX, 0},

{"vxcptsave",    "Xhwacha", "s", MATCH_VXCPTSAVE, MASK_VXCPTSAVE, 0},
{"vxcptrestore", "Xhwacha", "s", MATCH_VXCPTRESTORE, MASK_VXCPTRESTORE, 0},
{"vxcptkill",    "Xhwacha", "", MATCH_VXCPTKILL, MASK_VXCPTKILL, 0},

{"vxcptevac",    "Xhwacha", "s", MATCH_VXCPTEVAC, MASK_VXCPTEVAC, 0},
{"vxcpthold",    "Xhwacha", "", MATCH_VXCPTHOLD, MASK_VXCPTHOLD, 0},
{"venqcmd",      "Xhwacha", "s,t", MATCH_VENQCMD, MASK_VENQCMD, 0},
{"venqimm1",     "Xhwacha", "s,t", MATCH_VENQIMM1, MASK_VENQIMM1, 0},
{"venqimm2",     "Xhwacha", "s,t", MATCH_VENQIMM2, MASK_VENQIMM2, 0},
{"venqcnt",      "Xhwacha", "s,t", MATCH_VENQCNT, MASK_VENQCNT, 0},
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
