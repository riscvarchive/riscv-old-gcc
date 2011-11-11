/* riscv.h.  RISC-V opcode list for GDB, the GNU debugger.
   Copyright 2011
   Free Software Foundation, Inc.
   Contributed by Andrew Waterman 

This file is part of GDB, GAS, and the GNU binutils.

GDB, GAS, and the GNU binutils are free software; you can redistribute
them and/or modify them under the terms of the GNU General Public
License as published by the Free Software Foundation; either version
1, or (at your option) any later version.

GDB, GAS, and the GNU binutils are distributed in the hope that they
will be useful, but WITHOUT ANY WARRANTY; without even the implied
warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See
the GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this file; see the file COPYING.  If not, write to the Free
Software Foundation, 51 Franklin Street - Fifth Floor, Boston, MA 02110-1301, USA.  */

#ifndef _RISCV_H_
#define _RISCV_H_

/* RVC fields */

#define OP_MASK_COP		0x1f
#define OP_SH_COP		0
#define OP_MASK_CRD		0x1f
#define OP_SH_CRD		5
#define OP_MASK_CRS2	0x1f
#define OP_SH_CRS2	5
#define OP_MASK_CRS1	0x1f
#define OP_SH_CRS1	10
#define OP_MASK_CRDS		0x7
#define OP_SH_CRDS		13
#define OP_MASK_CRS2S	0x7
#define OP_SH_CRS2S	13
#define OP_MASK_CRS2BS	0x7
#define OP_SH_CRS2BS	5
#define OP_MASK_CRS1S	0x7
#define OP_SH_CRS1S	10
#define OP_MASK_CIMM6	0x3f
#define OP_SH_CIMM6	10
#define OP_MASK_CIMM5	0x1f
#define OP_SH_CIMM5	5
#define OP_MASK_CIMM10	0x3ff
#define OP_SH_CIMM10	5

static const char rvc_rs1_regmap[8] = { 20, 21, 2, 3, 4, 5, 6, 7 };
#define rvc_rd_regmap rvc_rs1_regmap
#define rvc_rs2b_regmap rvc_rs1_regmap
static const char rvc_rs2_regmap[8] = { 20, 21, 2, 3, 4, 5, 6, 0 };

#define RVC_JUMP_BITS 10
#define RVC_JUMP_ALIGN_BITS 1
#define RVC_JUMP_ALIGN (1 << RVC_JUMP_ALIGN_BITS)
#define RVC_JUMP_REACH ((1ULL<<RVC_JUMP_BITS)*RVC_JUMP_ALIGN)

#define RVC_BRANCH_BITS 5
#define RVC_BRANCH_ALIGN_BITS RVC_JUMP_ALIGN_BITS
#define RVC_BRANCH_ALIGN (1 << RVC_BRANCH_ALIGN_BITS)
#define RVC_BRANCH_REACH ((1ULL<<RVC_BRANCH_BITS)*RVC_BRANCH_ALIGN)

#define RISCV_JTYPE(insn, target) \
  ((MATCH_ ## insn) | (((target) & ((1<<RISCV_JUMP_BITS)-1)) << OP_SH_TARGET))
#define RISCV_LTYPE(insn, rd, bigimm) \
  ((MATCH_ ## insn) | ((rd) << OP_SH_RD) | (((bigimm) & ((1<<RISCV_BIGIMM_BITS)-1)) << OP_SH_BIGIMMEDIATE))
#define RISCV_ITYPE(insn, rd, rs1, imm) \
  ((MATCH_ ## insn) | ((rd) << OP_SH_RD) | ((rs1) << OP_SH_RS) | (((imm) & (RISCV_IMM_REACH-1)) << OP_SH_IMMEDIATE))
#define RISCV_RTYPE(insn, rd, rs1, rs2) \
  ((MATCH_ ## insn) | ((rd) << OP_SH_RD) | ((rs1) << OP_SH_RS) | ((rs2) << OP_SH_RT))

#define RISCV_NOP RISCV_ITYPE(ADDI, 0, 0, 0)

#define RISCV_JUMP_TARGET(address) ((address) >> RISCV_JUMP_ALIGN_BITS)
#define RISCV_CONST_HIGH_PART(VALUE) \
  (((VALUE) + (RISCV_IMM_REACH/2)) & ~(RISCV_IMM_REACH-1))
#define RISCV_CONST_LOW_PART(VALUE) ((VALUE) - RISCV_CONST_HIGH_PART (VALUE))
#define RISCV_LUI_HIGH_PART(VALUE) (RISCV_CONST_HIGH_PART(VALUE) >> RISCV_IMM_BITS)

/* RV fields */

#define OP_MASK_OP		0x7f
#define OP_SH_OP		0
#define OP_MASK_RT		0x1f
#define OP_SH_RT		17
#define OP_MASK_FT		0x1f
#define OP_SH_FT		17
#define OP_MASK_RS		0x1f
#define OP_SH_RS		22
#define OP_MASK_FS		0x1f
#define OP_SH_FS		22
#define OP_MASK_FR		0x1f
#define OP_SH_FR		12
#define OP_MASK_RD		0x1f
#define OP_SH_RD		27
#define OP_MASK_FD		0x1f
#define OP_SH_FD		27
#define OP_MASK_SHAMT		0x3f
#define OP_SH_SHAMT		10
#define OP_MASK_SHAMTW		0x1f
#define OP_SH_SHAMTW	10
#define OP_MASK_RM		0x7
#define OP_SH_RM	9

static const char * const riscv_rm[8] =
  { "rne", "rtz", "rdn", "rup", "rmm", 0, 0, "dyn" };

#define OP_MASK_VRD		0x1f
#define OP_SH_VRD		27
#define OP_MASK_VRS		0x1f
#define OP_SH_VRS		22
#define OP_MASK_VRT		0x1f
#define OP_SH_VRT		17
#define OP_MASK_VRR		0x1f
#define OP_SH_VRR		12

#define OP_MASK_VFD		0x1f
#define OP_SH_VFD		27
#define OP_MASK_VFS		0x1f
#define OP_SH_VFS		22
#define OP_MASK_VFT		0x1f
#define OP_SH_VFT		17
#define OP_MASK_VFR		0x1f
#define OP_SH_VFR		12

#define OP_MASK_IMMNGPR         0x3f
#define OP_SH_IMMNGPR           10
#define OP_MASK_IMMNFPR         0x3f
#define OP_SH_IMMNFPR           16
#define OP_MASK_IMMSEGNELM      0x1f
#define OP_SH_IMMSEGNELM        17
#define OP_MASK_IMMSEGSTNELM    0x1f
#define OP_SH_IMMSEGSTNELM      12

#define LINK_REG 1

#define RISCV_JUMP_BITS 25
#define RISCV_JUMP_ALIGN_BITS 1
#define RISCV_JUMP_ALIGN (1 << RISCV_JUMP_ALIGN_BITS)
#define RISCV_JUMP_REACH ((1ULL<<RISCV_JUMP_BITS)*RISCV_JUMP_ALIGN)

#define OP_MASK_TARGET		((1<<RISCV_JUMP_BITS)-1)
#define OP_SH_TARGET		7

#define RISCV_IMM_BITS 12
#define RISCV_IMMLO_BITS 7
#define RISCV_IMMHI_BITS (RISCV_IMM_BITS - RISCV_IMMLO_BITS)
#define RISCV_BIGIMM_BITS (32-RISCV_IMM_BITS)
#define RISCV_IMM_REACH (1LL<<RISCV_IMM_BITS)
#define RISCV_BIGIMM_REACH (1LL<<RISCV_BIGIMM_BITS)
#define RISCV_BRANCH_BITS RISCV_IMM_BITS
#define RISCV_BRANCH_ALIGN_BITS RISCV_JUMP_ALIGN_BITS
#define RISCV_BRANCH_ALIGN (1 << RISCV_BRANCH_ALIGN_BITS)
#define RISCV_BRANCH_REACH (RISCV_IMM_REACH*RISCV_BRANCH_ALIGN)

#define OP_MASK_BIGIMMEDIATE	((1<<RISCV_BIGIMM_BITS)-1)
#define OP_SH_BIGIMMEDIATE		7
#define OP_MASK_IMMEDIATE	((1<<RISCV_IMM_BITS)-1)
#define OP_SH_IMMEDIATE		10
#define OP_MASK_IMMLO ((1<<RISCV_IMMLO_BITS)-1)
#define OP_SH_IMMLO   10
#define OP_MASK_IMMHI ((1<<(RISCV_IMM_BITS-RISCV_IMMLO_BITS))-1)
#define OP_SH_IMMHI   27

#include "riscv-opc.h"

/* This structure holds information for a particular instruction.  */

struct riscv_opcode
{
  /* The name of the instruction.  */
  const char *name;
  /* A string describing the arguments for this instruction.  */
  const char *args;
  /* The basic opcode for the instruction.  When assembling, this
     opcode is modified by the arguments to produce the actual opcode
     that is used.  If pinfo is INSN_MACRO, then this is 0.  */
  unsigned long match;
  /* If pinfo is not INSN_MACRO, then this is a bit mask for the
     relevant portions of the opcode when disassembling.  If the
     actual opcode anded with the match field equals the opcode field,
     then we have found the correct instruction.  If pinfo is
     INSN_MACRO, then this field is the macro identifier.  */
  unsigned long mask;
  /* For a macro, this is INSN_MACRO.  Otherwise, it is a collection
     of bits describing the instruction, notably any relevant hazard
     information.  */
  unsigned long pinfo;
};

#define INSN_WRITE_GPR_D            0x00000001
#define INSN_WRITE_GPR_RA           0x00000004
#define INSN_WRITE_FPR_D            0x00000008
#define INSN_READ_GPR_S             0x00000040
#define INSN_READ_GPR_T             0x00000080
#define INSN_READ_FPR_S             0x00000100
#define INSN_READ_FPR_T             0x00000200
#define INSN_READ_FPR_R        	    0x00000400
/* Instruction is a simple alias (I.E. "move" for daddu/addu/or) */
#define	INSN_ALIAS		    0x00001000
/* Instruction is actually a macro.  It should be ignored by the
   disassembler, and requires special treatment by the assembler.  */
#define INSN_MACRO                  0xffffffff

/* These are the bits which may be set in the pinfo2 field of an
   instruction. */

/* MIPS ISA defines, use instead of hardcoding ISA level.  */

#define       ISA_UNKNOWN     0               /* Gas internal use.  */
#define       ISA_RV32        1
#define       ISA_RV64        2

#define CPU_UNKNOWN    0
#define CPU_ROCKET32 132
#define CPU_ROCKET64 164

/* This is a list of macro expanded instructions.

   _I appended means immediate
   _A appended means address
   _AB appended means address with base register
   _D appended means 64 bit floating point constant
   _S appended means 32 bit floating point constant.  */

enum
{
  M_LA_AB,
  M_J,
  M_LI,
  M_NUM_MACROS
};


/* The order of overloaded instructions matters.  Label arguments and
   register arguments look the same. Instructions that can have either
   for arguments must apear in the correct order in this table for the
   assembler to pick the right one. In other words, entries with
   immediate operands must apear after the same instruction with
   registers.

   Many instructions are short hand for other instructions (i.e., The
   jal <register> instruction is short for jalr <register>).  */

extern const struct riscv_opcode riscv_builtin_opcodes[];
extern const int bfd_riscv_num_builtin_opcodes;
extern struct riscv_opcode *riscv_opcodes;
extern int bfd_riscv_num_opcodes;
#define NUMOPCODES bfd_riscv_num_opcodes

#endif /* _MIPS_H_ */
