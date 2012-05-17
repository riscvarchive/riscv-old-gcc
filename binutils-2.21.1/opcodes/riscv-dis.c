/* Print mips instructions for GDB, the GNU debugger, or for objdump.
   Copyright 1989, 1991, 1992, 1993, 1994, 1995, 1996, 1997, 1998, 1999,
   2000, 2001, 2002, 2003, 2005, 2007, 2008
   Free Software Foundation, Inc.
   Contributed by Nobuyuki Hikichi(hikichi@sra.co.jp).

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
   along with this program; if not, write to the Free Software
   Foundation, Inc., 51 Franklin Street - Fifth Floor, Boston,
   MA 02110-1301, USA.  */

#include "sysdep.h"
#include "dis-asm.h"
#include "libiberty.h"
#include "opcode/riscv.h"
#include "opintl.h"

/* FIXME: These are needed to figure out if the code is mips16 or
   not. The low bit of the address is often a good indicator.  No
   symbol table is available when this code runs out in an embedded
   system as when it is used for disassembler support in a monitor.  */

#if !defined(EMBEDDED_ENV)
#define SYMTAB_AVAILABLE 1
#include "elf-bfd.h"
#include "elf/riscv.h"
#endif

#include <assert.h>

/* Mips instructions are at maximum this many bytes long.  */
#define INSNLEN 4


/* FIXME: These should be shared with gdb somehow.  */

static const char * const mips_gpr_names_numeric[32] =
{
  "x0",   "x1",   "x2",   "x3",   "x4",   "x5",   "x6",   "x7",
  "x8",   "x9",   "x10",  "x11",  "x12",  "x13",  "x14",  "x15",
  "x16",  "x17",  "x18",  "x19",  "x20",  "x21",  "x22",  "x23",
  "x24",  "x25",  "x26",  "x27",  "x28",  "x29",  "x30",  "x31"
};

static const char * const mips_gpr_names_abi[32] =
{
  "zero", "ra",   "v0",   "v1",   "a0",   "a1",   "a2",   "a3",
  "a4",   "a5",   "a6",   "a7",   "t0",   "t1",   "t2",   "t3",
  "t4",   "t5",   "t6",   "t7",   "s0",   "s1",   "s2",   "s3",
  "s4",   "s5",   "s6",   "s7",   "s8",   "s9",   "sp",   "tp"
};

static const char * const mips_fpr_names_numeric[32] =
{
  "f0",   "f1",   "f2",   "f3",   "f4",   "f5",   "f6",   "f7",
  "f8",   "f9",   "f10",  "f11",  "f12",  "f13",  "f14",  "f15",
  "f16",  "f17",  "f18",  "f19",  "f20",  "f21",  "f22",  "f23",
  "f24",  "f25",  "f26",  "f27",  "f28",  "f29",  "f30",  "f31"
};

static const char * const mips_fpr_names_abi[32] =
{
  "ft0",  "ft1",  "fv0",  "fv1",  "fa0",  "fa1",  "fa2",  "fa3",
  "fa4",  "fa5",  "fa6",  "fa7",  "ft2",  "ft3",  "ft4",  "ft5",
  "ft6",  "ft7",  "ft8",  "ft9",  "fs0",  "fs1",  "fs2",  "fs3",
  "fs4",  "fs5",  "fs6",  "fs7",  "fs8",  "fs9",  "ft10", "ft11"
};

static const char * const mips_cp0_names_numeric[32] =
{
  "$0",   "$1",   "$2",   "$3",   "$4",   "$5",   "$6",   "$7",
  "$8",   "$9",   "$10",  "$11",  "$12",  "$13",  "$14",  "$15",
  "$16",  "$17",  "$18",  "$19",  "$20",  "$21",  "$22",  "$23",
  "$24",  "$25",  "$26",  "$27",  "$28",  "$29",  "$30",  "$31"
};

static const char * const mips_vgr_reg_names_riscv[32] =
{
  "vzero","vra",  "vv0",  "vv1",  "va0",  "va1",  "va2",  "va3",
  "va4",  "va5",  "va6",  "va7",  "vt0",  "vt1",  "vt2",  "vt3",
  "vt4",  "vt5",  "vt6",  "vt7",  "vs0",  "vs1",  "vs2",  "vs3",
  "vs4",  "vs5",  "vs6",  "vs7",  "vs8",  "vs9",  "vsp",  "vtp"
};

static const char * const mips_vfp_reg_names_riscv[32] =
{
  "vf0",  "vf1",  "vf2",  "vf3",  "vf4",  "vf5",  "vf6",  "vf7",
  "vf8",  "vf9",  "vf10", "vf11", "vf12", "vf13", "vf14", "vf15",
  "vf16", "vf17", "vf18", "vf19", "vf20", "vf21", "vf22", "vf23",
  "vf24", "vf25", "vf26", "vf27", "vf28", "vf29", "vf30", "vf31"
};

struct mips_abi_choice
{
  const char * name;
  const char * const *gpr_names;
  const char * const *fpr_names;
};

struct mips_abi_choice mips_abi_choices[] =
{
  { "numeric", mips_gpr_names_numeric, mips_fpr_names_numeric },
  { "32", mips_gpr_names_abi, mips_fpr_names_abi },
  { "64", mips_gpr_names_abi, mips_fpr_names_abi },
};

struct mips_arch_choice
{
  const char *name;
  int bfd_mach_valid;
  unsigned long bfd_mach;
  int processor;
  int isa;
  const char * const *cp0_names;
};

const struct mips_arch_choice mips_arch_choices[] =
{
  { "numeric",	0, 0, 0, 0,
    mips_cp0_names_numeric },

  { "rv32",	1, bfd_mach_riscv_rocket32, CPU_ROCKET32, ISA_RV32,
    mips_cp0_names_numeric },

  { "rv64",	1, bfd_mach_riscv_rocket64, CPU_ROCKET64, ISA_RV64,
    mips_cp0_names_numeric },
};

/* ISA and processor type to disassemble for, and register names to use.
   set_default_mips_dis_options and parse_mips_dis_options fill in these
   values.  */
static int mips_processor;
static int mips_isa;
static const char * const *mips_gpr_names;
static const char * const *mips_fpr_names;
static const char * const *mips_cp0_names;

/* Other options */
static int no_aliases;	/* If set disassemble as most general inst.  */

static const struct mips_abi_choice *
choose_abi_by_name (const char *name, unsigned int namelen)
{
  const struct mips_abi_choice *c;
  unsigned int i;

  for (i = 0, c = NULL; i < ARRAY_SIZE (mips_abi_choices) && c == NULL; i++)
    if (strncmp (mips_abi_choices[i].name, name, namelen) == 0
	&& strlen (mips_abi_choices[i].name) == namelen)
      c = &mips_abi_choices[i];

  return c;
}

static const struct mips_arch_choice *
choose_arch_by_name (const char *name, unsigned int namelen)
{
  const struct mips_arch_choice *c = NULL;
  unsigned int i;

  for (i = 0, c = NULL; i < ARRAY_SIZE (mips_arch_choices) && c == NULL; i++)
    if (strncmp (mips_arch_choices[i].name, name, namelen) == 0
	&& strlen (mips_arch_choices[i].name) == namelen)
      c = &mips_arch_choices[i];

  return c;
}

static const struct mips_arch_choice *
choose_arch_by_number (unsigned long mach)
{
  static unsigned long hint_bfd_mach;
  static const struct mips_arch_choice *hint_arch_choice;
  const struct mips_arch_choice *c;
  unsigned int i;

  /* We optimize this because even if the user specifies no
     flags, this will be done for every instruction!  */
  if (hint_bfd_mach == mach
      && hint_arch_choice != NULL
      && hint_arch_choice->bfd_mach == hint_bfd_mach)
    return hint_arch_choice;

  for (i = 0, c = NULL; i < ARRAY_SIZE (mips_arch_choices) && c == NULL; i++)
    {
      if (mips_arch_choices[i].bfd_mach_valid
	  && mips_arch_choices[i].bfd_mach == mach)
	{
	  c = &mips_arch_choices[i];
	  hint_bfd_mach = mach;
	  hint_arch_choice = c;
	}
    }
  return c;
}

static void
set_default_mips_dis_options (struct disassemble_info *info)
{
  const struct mips_arch_choice *chosen_arch;

  /* Defaults: mipsIII/r3000 (?!), (o)32-style ("oldabi") GPR names,
     and numeric FPR, CP0 register, and HWR names.  */
  mips_isa = ISA_RV64;
  mips_processor =  CPU_ROCKET64;
  mips_gpr_names = mips_gpr_names_abi;
  mips_fpr_names = mips_fpr_names_abi;
  mips_cp0_names = mips_cp0_names_numeric;
  no_aliases = 0;

  /* Set ISA, architecture, and cp0 register names as best we can.  */
#if ! SYMTAB_AVAILABLE
  /* This is running out on a target machine, not in a host tool.
     FIXME: Where does mips_target_info come from?  */
  target_processor = mips_target_info.processor;
  mips_isa = mips_target_info.isa;
#else
  chosen_arch = choose_arch_by_number (info->mach);
  if (chosen_arch != NULL)
    {
      mips_processor = chosen_arch->processor;
      mips_isa = chosen_arch->isa;
      mips_cp0_names = chosen_arch->cp0_names;
    }
#endif
}

static void
parse_mips_dis_option (const char *option, unsigned int len)
{
  unsigned int i, optionlen, vallen;
  const char *val;
  const struct mips_abi_choice *chosen_abi;
  const struct mips_arch_choice *chosen_arch;

  /* Try to match options that are simple flags */
  if (CONST_STRNEQ (option, "no-aliases"))
    {
      no_aliases = 1;
      return;
    }
  
  /* Look for the = that delimits the end of the option name.  */
  for (i = 0; i < len; i++)
    if (option[i] == '=')
      break;

  if (i == 0)		/* Invalid option: no name before '='.  */
    return;
  if (i == len)		/* Invalid option: no '='.  */
    return;
  if (i == (len - 1))	/* Invalid option: no value after '='.  */
    return;

  optionlen = i;
  val = option + (optionlen + 1);
  vallen = len - (optionlen + 1);

  if (strncmp ("gpr-names", option, optionlen) == 0
      && strlen ("gpr-names") == optionlen)
    {
      chosen_abi = choose_abi_by_name (val, vallen);
      if (chosen_abi != NULL)
	mips_gpr_names = chosen_abi->gpr_names;
      return;
    }

  if (strncmp ("fpr-names", option, optionlen) == 0
      && strlen ("fpr-names") == optionlen)
    {
      chosen_abi = choose_abi_by_name (val, vallen);
      if (chosen_abi != NULL)
	mips_fpr_names = chosen_abi->fpr_names;
      return;
    }

  if (strncmp ("cp0-names", option, optionlen) == 0
      && strlen ("cp0-names") == optionlen)
    {
      chosen_arch = choose_arch_by_name (val, vallen);
      if (chosen_arch != NULL)
	{
	  mips_cp0_names = chosen_arch->cp0_names;
	}
      return;
    }

  if (strncmp ("hwr-names", option, optionlen) == 0
      && strlen ("hwr-names") == optionlen)
    {
      chosen_arch = choose_arch_by_name (val, vallen);
      return;
    }

  if (strncmp ("reg-names", option, optionlen) == 0
      && strlen ("reg-names") == optionlen)
    {
      /* We check both ABI and ARCH here unconditionally, so
	 that "numeric" will do the desirable thing: select
	 numeric register names for all registers.  Other than
	 that, a given name probably won't match both.  */
      chosen_abi = choose_abi_by_name (val, vallen);
      if (chosen_abi != NULL)
	{
	  mips_gpr_names = chosen_abi->gpr_names;
	  mips_fpr_names = chosen_abi->fpr_names;
	}
      chosen_arch = choose_arch_by_name (val, vallen);
      if (chosen_arch != NULL)
	mips_cp0_names = chosen_arch->cp0_names;
      return;
    }

  /* Invalid option.  */
}

static void
parse_mips_dis_options (const char *options)
{
  const char *option_end;

  if (options == NULL)
    return;

  while (*options != '\0')
    {
      /* Skip empty options.  */
      if (*options == ',')
	{
	  options++;
	  continue;
	}

      /* We know that *options is neither NUL or a comma.  */
      option_end = options + 1;
      while (*option_end != ',' && *option_end != '\0')
	option_end++;

      parse_mips_dis_option (options, option_end - options);

      /* Go on to the next one.  If option_end points to a comma, it
	 will be skipped above.  */
      options = option_end;
    }
}

/* Print insn arguments for 32/64-bit code.  */

static void
print_insn_args (const char *d,
		 register unsigned long int l,
		 bfd_vma pc,
		 struct disassemble_info *info)
{
  int delta;

  for (; *d != '\0'; d++)
    {
      switch (*d)
	{
        case '#':
          switch ( *++d ) {
            case 'g':
              (*info->fprintf_func)
                ( info->stream, "%d",
                  ((l >> OP_SH_IMMNGPR) & OP_MASK_IMMNGPR));
              break;
            case 'f':
              (*info->fprintf_func)
                ( info->stream, "%d",
                  ((l >> OP_SH_IMMNFPR) & OP_MASK_IMMNFPR));
              break;
            case 'n':
              (*info->fprintf_func)
                ( info->stream, "%d",
                  (((l >> OP_SH_IMMSEGNELM) & OP_MASK_IMMSEGNELM) + 1));
              break;
            case 'm':
              (*info->fprintf_func)
                ( info->stream, "%d",
                  (((l >> OP_SH_IMMSEGSTNELM) & OP_MASK_IMMSEGSTNELM) + 1));
              break;
            case 'd':
              (*info->fprintf_func)
                ( info->stream, "%s",
                  mips_vgr_reg_names_riscv[(l >> OP_SH_VRD) & OP_MASK_VRD]);
              break;
            case 's':
              (*info->fprintf_func)
                ( info->stream, "%s",
                  mips_vgr_reg_names_riscv[(l >> OP_SH_VRS) & OP_MASK_VRS]);
              break;
            case 't':
              (*info->fprintf_func)
                ( info->stream, "%s",
                  mips_vgr_reg_names_riscv[(l >> OP_SH_VRT) & OP_MASK_VRT]);
              break;
            case 'r':
              (*info->fprintf_func)
                ( info->stream, "%s",
                  mips_vgr_reg_names_riscv[(l >> OP_SH_VRR) & OP_MASK_VRR]);
              break;
            case 'D':
              (*info->fprintf_func)
                ( info->stream, "%s",
                  mips_vfp_reg_names_riscv[(l >> OP_SH_VFD) & OP_MASK_VFD]);
              break;
            case 'S':
              (*info->fprintf_func)
                ( info->stream, "%s",
                  mips_vfp_reg_names_riscv[(l >> OP_SH_VFS) & OP_MASK_VFS]);
              break;
            case 'T':
              (*info->fprintf_func)
                ( info->stream, "%s",
                  mips_vfp_reg_names_riscv[(l >> OP_SH_VFT) & OP_MASK_VFT]);
              break;
            case 'R':
              (*info->fprintf_func)
                ( info->stream, "%s",
                  mips_vfp_reg_names_riscv[(l >> OP_SH_VFR) & OP_MASK_VFR]);
              break;
          }
          break;

	case ',':
	case '(':
	case ')':
	case '[':
	case ']':
	  (*info->fprintf_func) (info->stream, "%c", *d);
	  break;

	case '0':
	  (*info->fprintf_func) (info->stream, "0");
	  break;

	case 'b':
	case 's':
	  (*info->fprintf_func) (info->stream, "%s",
				 mips_gpr_names[(l >> OP_SH_RS) & OP_MASK_RS]);
	  break;

	case 't':
	  (*info->fprintf_func) (info->stream, "%s",
				 mips_gpr_names[(l >> OP_SH_RT) & OP_MASK_RT]);
	  break;

	case 'u':
	  (*info->fprintf_func) (info->stream, "0x%lx",
				 (l >> OP_SH_BIGIMMEDIATE) & OP_MASK_BIGIMMEDIATE);
	  break;

	case 'm':
        {
	  assert(OP_MASK_RM < ARRAY_SIZE(riscv_rm));
          const char* rm = riscv_rm[(l >> OP_SH_RM) & OP_MASK_RM];
          if(rm == NULL)
            rm = "unknown";

	  (*info->fprintf_func) (info->stream, "%s", rm);
	  break;
	}
	case 'j': /* Same as i, but sign-extended.  */
	case 'o':
	  delta = (l >> OP_SH_IMMEDIATE) & OP_MASK_IMMEDIATE;
	  if (delta & (RISCV_IMM_REACH/2))
	    delta |= ~(RISCV_IMM_REACH-1);
	  (*info->fprintf_func) (info->stream, "%d",
				 delta);
	  break;

	case 'q':
	  delta = ((l >> OP_SH_IMMLO) & OP_MASK_IMMLO) | (((l >> OP_SH_IMMHI) & OP_MASK_IMMHI) << RISCV_IMMLO_BITS);
	  if (delta & (RISCV_IMM_REACH/2))
	    delta |= ~(RISCV_IMM_REACH-1);
	  (*info->fprintf_func) (info->stream, "%d",
				 delta);
	  break;

	case 'a':
	  delta = (l >> OP_SH_TARGET) & OP_MASK_TARGET;
	  if (delta & ((1<<RISCV_JUMP_BITS)/2))
	    delta |= ~((1<<RISCV_JUMP_BITS)-1);
	  info->target = (delta << RISCV_JUMP_ALIGN_BITS) + pc;
	  (*info->print_address_func) (info->target, info);
	  break;

	case 'p':
	  /* Sign extend the displacement.  */
	  delta = ((l >> OP_SH_IMMLO) & OP_MASK_IMMLO) | (((l >> OP_SH_IMMHI) & OP_MASK_IMMHI) << RISCV_IMMLO_BITS);
	  if (delta & (RISCV_IMM_REACH/2))
	    delta |= ~(RISCV_IMM_REACH-1);
	  info->target = (delta << RISCV_BRANCH_ALIGN_BITS) + pc;
	  (*info->print_address_func) (info->target, info);
	  break;

	case 'd':
	  (*info->fprintf_func) (info->stream, "%s",
				 mips_gpr_names[(l >> OP_SH_RD) & OP_MASK_RD]);
	  break;

	case 'z':
	  (*info->fprintf_func) (info->stream, "%s", mips_gpr_names[0]);
	  break;

	case '>':
	  (*info->fprintf_func) (info->stream, "0x%lx",
				 (l >> OP_SH_SHAMT) & OP_MASK_SHAMT);
	  break;

	case '<':
	  (*info->fprintf_func) (info->stream, "0x%lx",
				 (l >> OP_SH_SHAMTW) & OP_MASK_SHAMTW);
	  break;

	case 'S':
	  (*info->fprintf_func) (info->stream, "%s",
				 mips_fpr_names[(l >> OP_SH_FS) & OP_MASK_FS]);
	  break;

	case 'T':
	  (*info->fprintf_func) (info->stream, "%s",
				 mips_fpr_names[(l >> OP_SH_FT) & OP_MASK_FT]);
	  break;

	case 'D':
	  (*info->fprintf_func) (info->stream, "%s",
				 mips_fpr_names[(l >> OP_SH_FD) & OP_MASK_FD]);
	  break;

	case 'R':
	  (*info->fprintf_func) (info->stream, "%s",
				 mips_fpr_names[(l >> OP_SH_FR) & OP_MASK_FR]);
	  break;

	case 'E':
	  /* Coprocessor register for lwcN instructions, et al.

	     Note that there is no load/store cp0 instructions, and
	     that FPU (cp1) instructions disassemble this field using
	     'T' format.  Therefore, until we gain understanding of
	     cp2 register names, we can simply print the register
	     numbers.  */
	  (*info->fprintf_func) (info->stream, "cr%ld",
				 (l >> OP_SH_RS) & OP_MASK_RS);
	  break;

	case 'G':
	  /* Control registers */
	  (*info->fprintf_func) (info->stream, "%s",
				 mips_cp0_names[(l >> OP_SH_RS) & OP_MASK_RS]);
	  break;

	default:
	  /* xgettext:c-format */
	  (*info->fprintf_func) (info->stream,
				 _("# internal error, undefined modifier (%c)"),
				 *d);
	  return;
	}
    }
}

/* Print the mips instruction at address MEMADDR in debugged memory,
   on using INFO.  Returns length of the instruction, in bytes.
   BIGENDIAN must be 1 if this is big-endian code, 0 if
   this is little-endian code.  */

static unsigned long
riscv_rvc_uncompress(unsigned long rvc_insn)
{
  #define IS_INSN(x, op) (((x) & MASK_##op) == MATCH_##op)
  #define EXTRACT_OPERAND(x, op) (((x) >> OP_SH_##op) & OP_MASK_##op)

  int crd = EXTRACT_OPERAND(rvc_insn, CRD);
  int crs1 = EXTRACT_OPERAND(rvc_insn, CRS1);
  int crs2 = EXTRACT_OPERAND(rvc_insn, CRS2);
  int crds = EXTRACT_OPERAND(rvc_insn, CRDS);
  int crs1s = EXTRACT_OPERAND(rvc_insn, CRS1S);
  int crs2s = EXTRACT_OPERAND(rvc_insn, CRS2S);
  int crs2bs = EXTRACT_OPERAND(rvc_insn, CRS2BS);

  int cimm6 = EXTRACT_OPERAND(rvc_insn, CIMM6);
  int imm6 = ((int32_t)cimm6 << 26 >> 26) & (RISCV_IMM_REACH-1);
  //int imm6lo = imm6 & ((1<<RISCV_IMMLO_BITS)-1);
  //int imm6hi = (imm6 >> RISCV_IMMLO_BITS) & ((1<<RISCV_IMMHI_BITS)-1);
  int imm6x4 = (((int32_t)cimm6 << 26 >> 26)*4) & (RISCV_IMM_REACH-1);
  int imm6x4lo = imm6x4 & ((1<<RISCV_IMMLO_BITS)-1);
  int imm6x4hi = (imm6x4 >> RISCV_IMMLO_BITS) & ((1<<RISCV_IMMHI_BITS)-1);
  int imm6x8 = (((int32_t)cimm6 << 26 >> 26)*8) & (RISCV_IMM_REACH-1);
  int imm6x8lo = imm6x8 & ((1<<RISCV_IMMLO_BITS)-1);
  int imm6x8hi = (imm6x8 >> RISCV_IMMLO_BITS) & ((1<<RISCV_IMMHI_BITS)-1);

  int cimm5 = EXTRACT_OPERAND(rvc_insn, CIMM5);
  int imm5 = ((int32_t)cimm5 << 27 >> 27) & (RISCV_IMM_REACH-1);
  int imm5lo = imm5 & ((1<<RISCV_IMMLO_BITS)-1);
  int imm5hi = (imm5 >> RISCV_IMMLO_BITS) & ((1<<RISCV_IMMHI_BITS)-1);
  int imm5x4 = (((int32_t)cimm5 << 27 >> 27)*4) & (RISCV_IMM_REACH-1);
  int imm5x4lo = imm5x4 & ((1<<RISCV_IMMLO_BITS)-1);
  int imm5x4hi = (imm5x4 >> RISCV_IMMLO_BITS) & ((1<<RISCV_IMMHI_BITS)-1);
  int imm5x8 = (((int32_t)cimm5 << 27 >> 27)*8) & (RISCV_IMM_REACH-1);
  int imm5x8lo = imm5x8 & ((1<<RISCV_IMMLO_BITS)-1);
  int imm5x8hi = (imm5x8 >> RISCV_IMMLO_BITS) & ((1<<RISCV_IMMHI_BITS)-1);

  int cimm10 = EXTRACT_OPERAND(rvc_insn, CIMM10);
  int jt10 = ((int32_t)cimm10 << 22 >> 22) & ((1<<RISCV_JUMP_BITS)-1);

  if(IS_INSN(rvc_insn, C_ADDI))
  {
    if(crd == 0)
    {
      if(imm6 & 0x20)
        return MATCH_JALR_C | (LINK_REG << OP_SH_RD) | (crs1 << OP_SH_RS);
      else if(crs1 == 1)
        return MATCH_JALR_R | (crs1 << OP_SH_RS);
      else
        return MATCH_JALR_J | (crs1 << OP_SH_RS);
    }
    return MATCH_ADDI | (crd << OP_SH_RD) | (crd << OP_SH_RS) |
           (imm6 << OP_SH_IMMEDIATE);
  }
  if(IS_INSN(rvc_insn, C_ADDIW))
    return MATCH_ADDIW | (crd << OP_SH_RD) | (crd << OP_SH_RS) | (imm6 << OP_SH_IMMEDIATE);
  if(IS_INSN(rvc_insn, C_LI))
    return MATCH_ADDI | (crd << OP_SH_RD) | (imm6 << OP_SH_IMMEDIATE);
  if(IS_INSN(rvc_insn, C_MOVE))
    return MATCH_ADDI | (crd << OP_SH_RD) | (crs1 << OP_SH_RS);
  if(IS_INSN(rvc_insn, C_SLLI))
    return MATCH_SLLI | (cimm5 << OP_SH_SHAMT) | (rvc_rd_regmap[crds] << OP_SH_RD) | (rvc_rd_regmap[crds] << OP_SH_RS);
  if(IS_INSN(rvc_insn, C_SLLI32))
    return MATCH_SLLI | ((cimm5+32) << OP_SH_SHAMT) | (rvc_rd_regmap[crds] << OP_SH_RD) | (rvc_rd_regmap[crds] << OP_SH_RS);
  if(IS_INSN(rvc_insn, C_SRLI))
    return MATCH_SRLI | (cimm5 << OP_SH_SHAMT) | (rvc_rd_regmap[crds] << OP_SH_RD) | (rvc_rd_regmap[crds] << OP_SH_RS);
  if(IS_INSN(rvc_insn, C_SRLI32))
    return MATCH_SRLI | ((cimm5+32) << OP_SH_SHAMT) | (rvc_rd_regmap[crds] << OP_SH_RD) | (rvc_rd_regmap[crds] << OP_SH_RS);
  if(IS_INSN(rvc_insn, C_SRAI))
    return MATCH_SRAI | (cimm5 << OP_SH_SHAMT) | (rvc_rd_regmap[crds] << OP_SH_RD) | (rvc_rd_regmap[crds] << OP_SH_RS);
  if(IS_INSN(rvc_insn, C_SRAI32))
    return MATCH_SRAI | ((cimm5+32) << OP_SH_SHAMT) | (rvc_rd_regmap[crds] << OP_SH_RD) | (rvc_rd_regmap[crds] << OP_SH_RS);
  if(IS_INSN(rvc_insn, C_SLLIW))
    return MATCH_SLLIW | (cimm5 << OP_SH_SHAMT) | (rvc_rd_regmap[crds] << OP_SH_RD) | (rvc_rd_regmap[crds] << OP_SH_RS);
  if(IS_INSN(rvc_insn, C_ADD))
    return MATCH_ADD | (crd << OP_SH_RD) | (crs1 << OP_SH_RS) | (crd << OP_SH_RT);
  if(IS_INSN(rvc_insn, C_SUB))
    return MATCH_SUB | (crd << OP_SH_RD) | (crs1 << OP_SH_RS) | (crd << OP_SH_RT);
  if(IS_INSN(rvc_insn, C_ADD3))
    return MATCH_ADD | (rvc_rd_regmap[crds] << OP_SH_RD) | (rvc_rs1_regmap[crs1s] << OP_SH_RS) | (rvc_rs2b_regmap[crs2bs] << OP_SH_RT);
  if(IS_INSN(rvc_insn, C_SUB3))
    return MATCH_SUB | (rvc_rd_regmap[crds] << OP_SH_RD) | (rvc_rs1_regmap[crs1s] << OP_SH_RS) | (rvc_rs2b_regmap[crs2bs] << OP_SH_RT);
  if(IS_INSN(rvc_insn, C_AND3))
    return MATCH_AND | (rvc_rd_regmap[crds] << OP_SH_RD) | (rvc_rs1_regmap[crs1s] << OP_SH_RS) | (rvc_rs2b_regmap[crs2bs] << OP_SH_RT);
  if(IS_INSN(rvc_insn, C_OR3))
    return MATCH_OR | (rvc_rd_regmap[crds] << OP_SH_RD) | (rvc_rs1_regmap[crs1s] << OP_SH_RS) | (rvc_rs2b_regmap[crs2bs] << OP_SH_RT);
  if(IS_INSN(rvc_insn, C_J))
    return MATCH_J | (jt10 << OP_SH_TARGET);
  if(IS_INSN(rvc_insn, C_BEQ))
    return MATCH_BEQ | (rvc_rs1_regmap[crs1s] << OP_SH_RS) | (rvc_rs2_regmap[crs2s] << OP_SH_RT) | (imm5lo << OP_SH_IMMLO) | (imm5hi << OP_SH_IMMHI);
  if(IS_INSN(rvc_insn, C_BNE))
    return MATCH_BNE | (rvc_rs1_regmap[crs1s] << OP_SH_RS) | (rvc_rs2_regmap[crs2s] << OP_SH_RT) | (imm5lo << OP_SH_IMMLO) | (imm5hi << OP_SH_IMMHI);
  if(IS_INSN(rvc_insn, C_LDSP))
    return MATCH_LD | (30 << OP_SH_RS) | (crd << OP_SH_RD) | (imm6x8 << OP_SH_IMMEDIATE);
  if(IS_INSN(rvc_insn, C_LWSP))
    return MATCH_LW | (30 << OP_SH_RS) | (crd << OP_SH_RD) | (imm6x4 << OP_SH_IMMEDIATE);
  if(IS_INSN(rvc_insn, C_SDSP))
    return MATCH_SD | (30 << OP_SH_RS) | (crs2 << OP_SH_RT) | (imm6x8lo << OP_SH_IMMLO) | (imm6x8hi << OP_SH_IMMHI);
  if(IS_INSN(rvc_insn, C_SWSP))
    return MATCH_SW | (30 << OP_SH_RS) | (crs2 << OP_SH_RT) | (imm6x4lo << OP_SH_IMMLO) | (imm6x4hi << OP_SH_IMMHI);
  if(IS_INSN(rvc_insn, C_LD))
    return MATCH_LD | (rvc_rs1_regmap[crs1s] << OP_SH_RS) | (rvc_rd_regmap[crds] << OP_SH_RD) | (imm5x8 << OP_SH_IMMEDIATE);
  if(IS_INSN(rvc_insn, C_LW))
    return MATCH_LW | (rvc_rs1_regmap[crs1s] << OP_SH_RS) | (rvc_rd_regmap[crds] << OP_SH_RD) | (imm5x4 << OP_SH_IMMEDIATE);
  if(IS_INSN(rvc_insn, C_SD))
    return MATCH_SD | (rvc_rs1_regmap[crs1s] << OP_SH_RS) | (rvc_rs2_regmap[crs2s] << OP_SH_RT) | (imm5x8lo << OP_SH_IMMLO) | (imm5x8hi << OP_SH_IMMHI);
  if(IS_INSN(rvc_insn, C_SW))
    return MATCH_SW | (rvc_rs1_regmap[crs1s] << OP_SH_RS) | (rvc_rs2_regmap[crs2s] << OP_SH_RT) | (imm5x4lo << OP_SH_IMMLO) | (imm5x4hi << OP_SH_IMMHI);
  if(IS_INSN(rvc_insn, C_LD0))
    return MATCH_LD | (crs1 << OP_SH_RS) | (crd << OP_SH_RD);
  if(IS_INSN(rvc_insn, C_LW0))
    return MATCH_LW | (crs1 << OP_SH_RS) | (crd << OP_SH_RD);
  if(IS_INSN(rvc_insn, C_FLD))
    return MATCH_FLD | (rvc_rs1_regmap[crs1s] << OP_SH_RS) | (rvc_rd_regmap[crds] << OP_SH_RD) | (imm5x8 << OP_SH_IMMEDIATE);
  if(IS_INSN(rvc_insn, C_FLW))
    return MATCH_FLW | (rvc_rs1_regmap[crs1s] << OP_SH_RS) | (rvc_rd_regmap[crds] << OP_SH_RD) | (imm5x4 << OP_SH_IMMEDIATE);
  if(IS_INSN(rvc_insn, C_FSD))
    return MATCH_FSD | (rvc_rs1_regmap[crs1s] << OP_SH_RS) | (rvc_rs2_regmap[crs2s] << OP_SH_RT) | (imm5x8lo << OP_SH_IMMLO) | (imm5x8hi << OP_SH_IMMHI);
  if(IS_INSN(rvc_insn, C_FSW))
    return MATCH_FSW | (rvc_rs1_regmap[crs1s] << OP_SH_RS) | (rvc_rs2_regmap[crs2s] << OP_SH_RT) | (imm5x4lo << OP_SH_IMMLO) | (imm5x4hi << OP_SH_IMMHI);

  return rvc_insn;
}

static int
print_insn_mips (bfd_vma memaddr,
		 unsigned long int word,
		 struct disassemble_info *info)
{
  const struct riscv_opcode *op;
  static bfd_boolean init = 0;
  static const struct riscv_opcode *mips_hash[OP_MASK_OP + 1];
  int insnlen;

  /* Build a hash table to shorten the search time.  */
  if (! init)
    {
      unsigned int i;

      for (i = 0; i <= OP_MASK_OP; i++)
	{
	  for (op = riscv_opcodes; op < &riscv_opcodes[NUMOPCODES]; op++)
	    {
	      if (op->pinfo == INSN_MACRO
		  || (no_aliases && (op->pinfo & INSN_ALIAS)))
		continue;
	      if (i == ((op->match >> OP_SH_OP) & OP_MASK_OP))
		{
		  mips_hash[i] = op;
		  break;
		}
	    }
	}

      init = 1;
    }

  insnlen = 4;
#if 0
  /* this enables rvc disassembly */
  if ((word & 0x3) < 3)
    insnlen = 2;
#endif

  if (insnlen == 2)
    word = riscv_rvc_uncompress(word);

  info->bytes_per_chunk = insnlen;
  info->display_endian = info->endian;
  info->insn_info_valid = 1;
  info->branch_delay_insns = 0;
  info->data_size = 0;
  info->insn_type = dis_nonbranch;
  info->target = 0;
  info->target2 = 0;

  op = mips_hash[(word >> OP_SH_OP) & OP_MASK_OP];
  if (op != NULL)
    {
      for (; op < &riscv_opcodes[NUMOPCODES]; op++)
	{
	  if (op->pinfo != INSN_MACRO 
	      && !(no_aliases && (op->pinfo & INSN_ALIAS))
	      && (word & op->mask) == op->match)
	    {
	      const char *d;

	      (*info->fprintf_func) (info->stream, "%s", op->name);

	      d = op->args;
	      if (d != NULL && *d != '\0')
		{
		  (*info->fprintf_func) (info->stream, "\t");
		  print_insn_args (d, word, memaddr, info);
		}

	      return insnlen;
	    }
	}
    }

  /* Handle undefined instructions.  */
  info->insn_type = dis_noninsn;
  (*info->fprintf_func) (info->stream, "0x%lx", word);
  return insnlen;
}


/* In an environment where we do not know the symbol type of the
   instruction we are forced to assume that the low order bit of the
   instructions' address may mark it as a mips16 instruction.  If we
   are single stepping, or the pc is within the disassembled function,
   this works.  Otherwise, we need a clue.  Sometimes.  */

static int
_print_insn_mips (bfd_vma memaddr,
		  struct disassemble_info *info,
		  enum bfd_endian endianness)
{
  bfd_byte buffer[INSNLEN];
  int status;

  set_default_mips_dis_options (info);
  parse_mips_dis_options (info->disassembler_options);

  status = (*info->read_memory_func) (memaddr, buffer, 2, info);
  if(status == 0)
    {
      unsigned long insn;

      if (endianness == BFD_ENDIAN_BIG)
	insn = (unsigned long) bfd_getb16 (buffer);
      else
	insn = (unsigned long) bfd_getl16 (buffer);

      if ((insn & 0x3) < 3)
	return print_insn_mips (memaddr, insn, info);
    }

  status = (*info->read_memory_func) (memaddr, buffer, INSNLEN, info);
  if (status == 0)
    {
      unsigned long insn;

      if (endianness == BFD_ENDIAN_BIG)
	insn = (unsigned long) bfd_getb32 (buffer);
      else
	insn = (unsigned long) bfd_getl32 (buffer);

      return print_insn_mips (memaddr, insn, info);
    }
  else
    {
      (*info->memory_error_func) (status, memaddr, info);
      return -1;
    }
}

int
print_insn_big_riscv (bfd_vma memaddr ATTRIBUTE_UNUSED,
                      struct disassemble_info *info ATTRIBUTE_UNUSED)
{
  assert(0);
}

int
print_insn_little_riscv (bfd_vma memaddr, struct disassemble_info *info)
{
  return _print_insn_mips (memaddr, info, BFD_ENDIAN_LITTLE);
}

void
print_mips_disassembler_options (FILE *stream)
{
  unsigned int i;

  fprintf (stream, _("\n\
The following MIPS specific disassembler options are supported for use\n\
with the -M switch (multiple options should be separated by commas):\n"));

  fprintf (stream, _("\n\
  gpr-names=ABI            Print GPR names according to  specified ABI.\n\
                           Default: based on binary being disassembled.\n"));

  fprintf (stream, _("\n\
  fpr-names=ABI            Print FPR names according to specified ABI.\n\
                           Default: numeric.\n"));

  fprintf (stream, _("\n\
  cp0-names=ARCH           Print CP0 register names according to\n\
                           specified architecture.\n\
                           Default: based on binary being disassembled.\n"));

  fprintf (stream, _("\n\
  hwr-names=ARCH           Print HWR names according to specified \n\
			   architecture.\n\
                           Default: based on binary being disassembled.\n"));

  fprintf (stream, _("\n\
  reg-names=ABI            Print GPR and FPR names according to\n\
                           specified ABI.\n"));

  fprintf (stream, _("\n\
  reg-names=ARCH           Print CP0 register and HWR names according to\n\
                           specified architecture.\n"));

  fprintf (stream, _("\n\
  For the options above, the following values are supported for \"ABI\":\n\
   "));
  for (i = 0; i < ARRAY_SIZE (mips_abi_choices); i++)
    fprintf (stream, " %s", mips_abi_choices[i].name);
  fprintf (stream, _("\n"));

  fprintf (stream, _("\n\
  For the options above, The following values are supported for \"ARCH\":\n\
   "));
  for (i = 0; i < ARRAY_SIZE (mips_arch_choices); i++)
    if (*mips_arch_choices[i].name != '\0')
      fprintf (stream, " %s", mips_arch_choices[i].name);
  fprintf (stream, _("\n"));

  fprintf (stream, _("\n"));
}
