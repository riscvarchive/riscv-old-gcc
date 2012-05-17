/* tc-mips.c -- assemble code for a MIPS chip.
   Copyright 1993, 1994, 1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002,
   2003, 2004, 2005, 2006, 2007, 2008, 2009  Free Software Foundation, Inc.
   Contributed by the OSF and Ralph Campbell.
   Written by Keith Knowles and Ralph Campbell, working independently.
   Modified for ECOFF and R4000 support by Ian Lance Taylor of Cygnus
   Support.

   This file is part of GAS.

   GAS is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 3, or (at your option)
   any later version.

   GAS is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with GAS; see the file COPYING.  If not, write to the Free
   Software Foundation, 51 Franklin Street - Fifth Floor, Boston, MA
   02110-1301, USA.  */

#include "as.h"
#include "config.h"
#include "subsegs.h"
#include "safe-ctype.h"

#include "itbl-ops.h"
#include "dwarf2dbg.h"
#include "dw2gencfi.h"

#include <execinfo.h>
#include <stdint.h>

#ifdef DEBUG
#define DBG(x) printf x
#else
#define DBG(x)
#endif

#ifdef OBJ_MAYBE_ELF
/* Clean up namespace so we can include obj-elf.h too.  */
static int mips_output_flavor (void);
static int mips_output_flavor (void) { return OUTPUT_FLAVOR; }
#undef OBJ_PROCESS_STAB
#undef OUTPUT_FLAVOR
#undef S_GET_ALIGN
#undef S_GET_SIZE
#undef S_SET_ALIGN
#undef S_SET_SIZE
#undef obj_frob_file
#undef obj_frob_file_after_relocs
#undef obj_frob_symbol
#undef obj_pop_insert
#undef obj_sec_sym_ok_for_reloc
#undef OBJ_COPY_SYMBOL_ATTRIBUTES

#include "obj-elf.h"
/* Fix any of them that we actually care about.  */
#undef OUTPUT_FLAVOR
#define OUTPUT_FLAVOR mips_output_flavor()
#endif

#if defined (OBJ_ELF)
#include "elf/riscv.h"
#endif

int mips_flag_pdr = TRUE;

#include "ecoff.h"

#if defined (OBJ_ELF) || defined (OBJ_MAYBE_ELF)
static char *mips_regmask_frag;
#endif

#define ILLEGAL_REG (32)

#define ZERO 0
#define TREG 18
#define PIC_CALL_REG 19
#define KT0 ILLEGAL_REG
#define KT1 ILLEGAL_REG
#define GP  ILLEGAL_REG
#define FP  29
#define SP  30
#define ATREG ILLEGAL_REG
#define RA  LINK_REG

/* Allow override of standard little-endian ECOFF format.  */

#ifndef ECOFF_LITTLE_FORMAT
#define ECOFF_LITTLE_FORMAT "ecoff-littlemips"
#endif

extern int target_big_endian;

/* Information about an instruction, including its format, operands
   and fixups.  */
struct mips_cl_insn
{
  /* The opcode's entry in riscv_opcodes or mips16_opcodes.  */
  const struct riscv_opcode *insn_mo;

  /* The 16-bit or 32-bit bitstring of the instruction itself.  This is
     a copy of INSN_MO->match with the operands filled in.  */
  unsigned long insn_opcode;

  /* The frag that contains the instruction.  */
  struct frag *frag;

  /* The offset into FRAG of the first instruction byte.  */
  long where;

  /* The relocs associated with the instruction, if any.  */
  fixS *fixp[3];
};

/* The ABI to use.  */
enum mips_abi_level
{
  NO_ABI = 0,
  ABI_32,
  ABI_64,
};

/* MIPS ABI we are using for this output file.  */
static enum mips_abi_level mips_abi = NO_ABI;

/* Whether or not we have code that can call pic code.  */
int mips_abicalls = FALSE;

/* This is the set of options which may be modified by the .set
   pseudo-op.  We use a struct so that .set push and .set pop are more
   reliable.  */

struct mips_set_options
{
  /* MIPS ISA (Instruction Set Architecture) level.  This is set to -1
     if it has not been initialized.  Changed by `.set mipsN', and the
     -mipsN command line option, and the default CPU.  */
  int isa;
  /* Enable RVC instruction compression */
  int rvc;
  /* Restrict general purpose registers and floating point registers
     to 32 bit.  This is initially determined when -mgp32 or -mfp32
     is passed but can changed if the assembler code uses .set mipsN.  */
  int gp32;
  /* True if ".set sym32" is in effect.  */
  bfd_boolean sym32;
};

/* This is the struct we use to hold the current set of options.  Note
   that we must set the isa field to ISA_UNKNOWN and the ASE fields to
   -1 to indicate that they have not been initialized.  */

/* True if -mrvc was passed.  */
static int file_mips_rvc = 0;

/* True if -mgp32 was passed.  */
static int file_mips_gp32 = -1;

static struct mips_set_options mips_opts =
{
  /* isa */ ISA_UNKNOWN, /* rvc */ 0,
  /* gp32 */ 0, /* sym32 */ TRUE
};

/* These variables are filled in with the masks of registers used.
   The object format code reads them and puts them in the appropriate
   place.  */
unsigned long mips_gprmask;
unsigned long mips_fprmask;

/* The argument of the -march= flag.  The architecture we are assembling.  */
static int file_mips_arch = CPU_UNKNOWN;
static const char *mips_arch_string;

/* True if the given ABI requires 32-bit registers.  */
#define ABI_NEEDS_32BIT_REGS(ABI) ((ABI) == ABI_32)

/* Likewise 64-bit registers.  */
#define ABI_NEEDS_64BIT_REGS(ABI) ((ABI) == ABI_64)

/*  Return true if ISA supports 64 bit wide gp registers.  */
#define ISA_HAS_64BIT_REGS(ISA) ((ISA) == ISA_RV64)

#define HAVE_32BIT_GPRS		                   \
    (mips_opts.gp32 || !ISA_HAS_64BIT_REGS (mips_opts.isa))

#define HAVE_64BIT_GPRS (!HAVE_32BIT_GPRS)

#define HAVE_64BIT_OBJECTS (mips_abi == ABI_64)

/* The ABI-derived address size.  */
#define HAVE_64BIT_ADDRESSES HAVE_64BIT_GPRS
#define HAVE_32BIT_ADDRESSES (!HAVE_64BIT_ADDRESSES)

/* The size of symbolic constants (i.e., expressions of the form
   "SYMBOL" or "SYMBOL + OFFSET").  */
#define HAVE_32BIT_SYMBOLS \
  (HAVE_32BIT_ADDRESSES || !HAVE_64BIT_OBJECTS || mips_opts.sym32)
#define HAVE_64BIT_SYMBOLS (!HAVE_32BIT_SYMBOLS)

/* MIPS PIC level.  */

enum mips_pic_level mips_pic;

/* handle of the OPCODE hash table */
static struct hash_control *op_hash = NULL;

/* This array holds the chars that always start a comment.  If the
    pre-processor is disabled, these aren't very useful */
const char comment_chars[] = "#";

/* This array holds the chars that only start a comment at the beginning of
   a line.  If the line seems to have the form '# 123 filename'
   .line and .file directives will appear in the pre-processed output */
/* Note that input_file.c hand checks for '#' at the beginning of the
   first line of the input file.  This is because the compiler outputs
   #NO_APP at the beginning of its output.  */
/* Also note that C style comments are always supported.  */
const char line_comment_chars[] = "#";

/* This array holds machine specific line separator characters.  */
const char line_separator_chars[] = ";";

/* Chars that can be used to separate mant from exp in floating point nums */
const char EXP_CHARS[] = "eE";

/* Chars that mean this number is a floating point constant */
/* As in 0f12.456 */
/* or    0d1.2345e12 */
const char FLT_CHARS[] = "rRsSfFdDxXpP";

/* Also be aware that MAXIMUM_NUMBER_OF_CHARS_FOR_FLOAT may have to be
   changed in read.c .  Ideally it shouldn't have to know about it at all,
   but nothing is ideal around here.
 */

static char *insn_error;

static int auto_align = 1;

/* To output NOP instructions correctly, we need to keep information
   about the previous two instructions.  */

/* Debugging level.  -g sets this to 2.  -gN sets this to N.  -g0 is
   equivalent to seeing no -g option at all.  */
static int mips_debug = 0;

/* Nop instructions used by emit_nop.  */
static struct mips_cl_insn nop_insn;

/* For ECOFF and ELF, relocations against symbols are done in two
   parts, with a HI relocation and a LO relocation.  Each relocation
   has only 16 bits of space to store an addend.  This means that in
   order for the linker to handle carries correctly, it must be able
   to locate both the HI and the LO relocation.  This means that the
   relocations must appear in order in the relocation table.

   In order to implement this, we keep track of each unmatched HI
   relocation.  We then sort them so that they immediately precede the
   corresponding LO relocation.  */

struct mips_hi_fixup
{
  /* Next HI fixup.  */
  struct mips_hi_fixup *next;
  /* This fixup.  */
  fixS *fixp;
  /* The section this fixup is in.  */
  segT seg;
};

/* The frag containing the last explicit relocation operator.
   Null if explicit relocations have not been used.  */

static fragS *prev_reloc_op_frag;

#define RELAX_BRANCH_ENCODE(uncond, toofar) \
  ((relax_substateT) \
   (0xc0000000 \
    | ((toofar) ? 1 : 0) \
    | ((uncond) ? 8 : 0)))
#define RELAX_BRANCH_P(i) (((i) & 0xf0000000) == 0xc0000000)
#define RELAX_BRANCH_UNCOND(i) (((i) & 8) != 0)
#define RELAX_BRANCH_TOOFAR(i) (((i) & 1) != 0)

/* Is the given value a sign-extended 32-bit value?  */
#define IS_SEXT_32BIT_NUM(x)						\
  (((x) &~ (offsetT) 0x7fffffff) == 0					\
   || (((x) &~ (offsetT) 0x7fffffff) == ~ (offsetT) 0x7fffffff))

#define IS_SEXT_NBIT_NUM(x,n) \
  ({ int64_t __tmp = (x); \
     __tmp = (__tmp << (64-(n))) >> (64-(n)); \
     __tmp == (x); })

/* Is the given value a zero-extended 32-bit value?  Or a negated one?  */
#define IS_ZEXT_32BIT_NUM(x)						\
  (((x) &~ (offsetT) 0xffffffff) == 0					\
   || (((x) &~ (offsetT) 0xffffffff) == ~ (offsetT) 0xffffffff))

/* Replace bits MASK << SHIFT of STRUCT with the equivalent bits in
   VALUE << SHIFT.  VALUE is evaluated exactly once.  */
#define INSERT_BITS(STRUCT, VALUE, MASK, SHIFT) \
  (STRUCT) = (((STRUCT) & ~((MASK) << (SHIFT))) \
	      | (((VALUE) & (MASK)) << (SHIFT)))

/* Extract bits MASK << SHIFT from STRUCT and shift them right
   SHIFT places.  */
#define EXTRACT_BITS(STRUCT, MASK, SHIFT) \
  (((STRUCT) >> (SHIFT)) & (MASK))

/* Change INSN's opcode so that the operand given by FIELD has value VALUE.
   INSN is a mips_cl_insn structure and VALUE is evaluated exactly once.

   include/opcode/mips.h specifies operand fields using the macros
   OP_MASK_<FIELD> and OP_SH_<FIELD>.  The MIPS16 equivalents start
   with "MIPS16OP" instead of "OP".  */
#define INSERT_OPERAND(FIELD, INSN, VALUE) \
  INSERT_BITS ((INSN).insn_opcode, VALUE, OP_MASK_##FIELD, OP_SH_##FIELD)

/* Extract the operand given by FIELD from mips_cl_insn INSN.  */
#define EXTRACT_OPERAND(FIELD, INSN) \
  EXTRACT_BITS ((INSN).insn_opcode, OP_MASK_##FIELD, OP_SH_##FIELD)

/* Determine if an instruction matches an opcode. */
#define OPCODE_MATCHES(OPCODE, OP) \
  (((OPCODE) & MASK_##OP) == MATCH_##OP)

#define INSN_MATCHES(INSN, OP) \
  (((INSN).insn_opcode & MASK_##OP) == MATCH_##OP)

#define OPCODE_IS_STORE(OPCODE) \
  (OPCODE_MATCHES(OPCODE, SD)  || OPCODE_MATCHES(OPCODE, SW) || \
   OPCODE_MATCHES(OPCODE, SH)  || OPCODE_MATCHES(OPCODE, SB) || \
   OPCODE_MATCHES(OPCODE, FSW) || OPCODE_MATCHES(OPCODE, FSD))

/* Prototypes for static functions.  */

#define internalError()							\
    as_fatal (_("internal Error, line %d, %s"), __LINE__, __FILE__)

enum mips_regclass { MIPS_GR_REG, MIPS_FP_REG };

static void append_insn
  (struct mips_cl_insn *ip, expressionS *p, bfd_reloc_code_real_type *r);
static void macro (struct mips_cl_insn * ip);
static void mips_ip (char *str, struct mips_cl_insn * ip);
static size_t my_getSmallExpression
  (expressionS *, bfd_reloc_code_real_type *, char *);
static void my_getExpression (expressionS *, char *);
static void s_align (int);
static void s_change_sec (int);
static void s_change_section (int);
static void s_cons (int);
static void s_float_cons (int);
static void s_mips_globl (int);
static void s_option (int);
static void s_mipsset (int);
static void s_abicalls (int);
static void s_dtprelword (int);
static void s_dtpreldword (int);
static void s_gpword (int);
static void s_gpdword (int);
static void s_insn (int);
static void s_mips_ent (int);
static void s_mips_end (int);
static void s_mips_frame (int);
static void s_mips_mask (int reg_type);
static void s_mips_stab (int);
static void s_mips_weakext (int);
static void s_mips_file (int);
static void s_mips_loc (int);
static int validate_mips_insn (const struct riscv_opcode *);
static int relaxed_branch_length (fragS *fragp, asection *sec, int update);

/* Table and functions used to map between CPU/ISA names, and
   ISA levels, and CPU numbers.  */

struct mips_cpu_info
{
  const char *name;           /* CPU or ISA name.  */
  int flags;                  /* ASEs available, or ISA flag.  */
  int isa;                    /* ISA level.  */
  int cpu;                    /* CPU number (default CPU if ISA).  */
};

#define MIPS_CPU_IS_ISA		0x0001	/* Is this an ISA?  (If 0, a CPU.) */

static const struct mips_cpu_info *mips_parse_cpu (const char *, const char *);
static const struct mips_cpu_info *mips_cpu_info_from_isa (int);

/* Pseudo-op table.

   The following pseudo-ops from the Kane and Heinrich MIPS book
   should be defined here, but are currently unsupported: .alias,
   .galive, .gjaldef, .gjrlive, .livereg, .noalias.

   The following pseudo-ops from the Kane and Heinrich MIPS book are
   specific to the type of debugging information being generated, and
   should be defined by the object format: .aent, .begin, .bend,
   .bgnb, .end, .endb, .ent, .fmask, .frame, .loc, .mask, .verstamp,
   .vreg.

   The following pseudo-ops from the Kane and Heinrich MIPS book are
   not MIPS CPU specific, but are also not specific to the object file
   format.  This file is probably the best place to define them, but
   they are not currently supported: .asm0, .endr, .lab, .struct.  */

static const pseudo_typeS mips_pseudo_table[] =
{
  /* MIPS specific pseudo-ops.  */
  {"option", s_option, 0},
  {"set", s_mipsset, 0},
  {"rdata", s_change_sec, 'r'},
  {"sdata", s_change_sec, 's'},
  {"livereg", s_ignore, 0},
  {"abicalls", s_abicalls, 0},
  {"dtprelword", s_dtprelword, 0},
  {"dtpreldword", s_dtpreldword, 0},
  {"gpword", s_gpword, 0},
  {"gpdword", s_gpdword, 0},
  {"insn", s_insn, 0},

  /* Relatively generic pseudo-ops that happen to be used on MIPS
     chips.  */
  {"asciiz", stringer, 8 + 1},
  {"bss", s_change_sec, 'b'},
  {"err", s_err, 0},
  {"half", s_cons, 1},
  {"dword", s_cons, 3},
  {"weakext", s_mips_weakext, 0},
  {"origin", s_org, 0},
  {"repeat", s_rept, 0},

  /* These pseudo-ops are defined in read.c, but must be overridden
     here for one reason or another.  */
  {"align", s_align, 0},
  {"byte", s_cons, 0},
  {"data", s_change_sec, 'd'},
  {"double", s_float_cons, 'd'},
  {"float", s_float_cons, 'f'},
  {"globl", s_mips_globl, 0},
  {"global", s_mips_globl, 0},
  {"hword", s_cons, 1},
  {"int", s_cons, 2},
  {"long", s_cons, 2},
  {"octa", s_cons, 4},
  {"quad", s_cons, 3},
  {"section", s_change_section, 0},
  {"short", s_cons, 1},
  {"single", s_float_cons, 'f'},
  {"stabn", s_mips_stab, 'n'},
  {"text", s_change_sec, 't'},
  {"word", s_cons, 2},

  { NULL, NULL, 0 },
};

static const pseudo_typeS mips_nonecoff_pseudo_table[] =
{
  /* These pseudo-ops should be defined by the object file format.
     However, a.out doesn't support them, so we have versions here.  */
  {"aent", s_mips_ent, 1},
  {"bgnb", s_ignore, 0},
  {"end", s_mips_end, 0},
  {"endb", s_ignore, 0},
  {"ent", s_mips_ent, 0},
  {"file", s_mips_file, 0},
  {"fmask", s_mips_mask, 'F'},
  {"frame", s_mips_frame, 0},
  {"loc", s_mips_loc, 0},
  {"mask", s_mips_mask, 'R'},
  {"verstamp", s_ignore, 0},
  { NULL, NULL, 0 },
};

extern void pop_insert (const pseudo_typeS *);

void
mips_pop_insert (void)
{
  pop_insert (mips_pseudo_table);
  pop_insert (mips_nonecoff_pseudo_table);
}

/* Symbols labelling the current insn.  */

struct insn_label_list
{
  struct insn_label_list *next;
  symbolS *label;
};

static struct insn_label_list *free_insn_labels;
#define label_list tc_segment_info_data.labels

void
mips_clear_insn_labels (void)
{
  register struct insn_label_list **pl;
  segment_info_type *si;

  if (now_seg)
    {
      for (pl = &free_insn_labels; *pl != NULL; pl = &(*pl)->next)
	;
      
      si = seg_info (now_seg);
      *pl = si->label_list;
      si->label_list = NULL;
    }
}


static char *expr_end;

/* Expressions which appear in instructions.  These are set by
   mips_ip.  */

static expressionS imm_expr;
static expressionS imm2_expr;
static expressionS offset_expr;

/* Relocs associated with imm_expr and offset_expr.  */

static bfd_reloc_code_real_type imm_reloc[3]
  = {BFD_RELOC_UNUSED, BFD_RELOC_UNUSED, BFD_RELOC_UNUSED};
static bfd_reloc_code_real_type offset_reloc[3]
  = {BFD_RELOC_UNUSED, BFD_RELOC_UNUSED, BFD_RELOC_UNUSED};

#ifdef OBJ_ELF
/* The pdr segment for per procedure frame/regmask info.  Not used for
   ECOFF debugging.  */

static segT pdr_seg;
#endif

/* The default target format to use.  */

const char *
mips_target_format (void)
{
  return HAVE_64BIT_OBJECTS ? "elf64-littleriscv" : "elf32-littleriscv";
}

/* Return the length of instruction INSN.  */

static inline unsigned int
insn_length (const struct mips_cl_insn *insn)
{
  /* RVC instructions have insn[1:0] != 3 */
  return (insn->insn_opcode & 0x3) != 0x3 ? 2 : 4;
}

static int
imm_bits_needed(int32_t imm)
{
  int imm_bits = 32;
  while(imm_bits > 1 && (imm << (32-(imm_bits-1)) >> (32-(imm_bits-1))) == imm)
    imm_bits--;
  return imm_bits;
}

/* return the rvc small register id, if it exists; else, return -1. */
#define ARRAY_FIND(array, x) ({ \
  size_t _pos = ARRAY_SIZE(array), _i; \
  for(_i = 0; _i < ARRAY_SIZE(array); _i++) \
    if((x) == (array)[_i]) \
      { _pos = _i; break; } \
  _pos; })
#define IN_ARRAY(array, x) (ARRAY_FIND(array, x) != ARRAY_SIZE(array))

#define is_rvc_reg(type, x) IN_ARRAY(rvc_##type##_regmap, x)
#define rvc_reg(type, x) ARRAY_FIND(rvc_##type##_regmap, x)

/* If insn can be compressed, compress it and return 1; else return 0. */
static int
riscv_rvc_compress(struct mips_cl_insn* insn)
{
  int rd = EXTRACT_OPERAND(RD, *insn);
  int rs1 = EXTRACT_OPERAND(RS, *insn);
  int rs2 ATTRIBUTE_UNUSED = EXTRACT_OPERAND(RT, *insn);
  int32_t imm = EXTRACT_OPERAND(IMMEDIATE, *insn);
  imm = imm << (32-RISCV_IMM_BITS) >> (32-RISCV_IMM_BITS);
  int32_t shamt = imm & 0x3f;
  int32_t bimm = EXTRACT_OPERAND(IMMLO, *insn) |
                 (EXTRACT_OPERAND(IMMHI, *insn) << RISCV_IMMLO_BITS);
  bimm = bimm << (32-RISCV_IMM_BITS) >> (32-RISCV_IMM_BITS);
  int32_t jt = EXTRACT_OPERAND(TARGET, *insn);
  jt = jt << (32-RISCV_JUMP_BITS) >> (32-RISCV_JUMP_BITS);

  gas_assert(insn_length(insn) == 4);

  int imm_bits = imm_bits_needed(imm);
  int bimm_bits = imm_bits_needed(bimm);
  int jt_bits = imm_bits_needed(jt);

  if(INSN_MATCHES(*insn, ADDI) && rd != 0 && rd == rs1 && imm_bits <= 6)
  {
    insn->insn_opcode = MATCH_C_ADDI;
    INSERT_OPERAND(CRD, *insn, rd);
    INSERT_OPERAND(CIMM6, *insn, imm);
  }
  else if(INSN_MATCHES(*insn, ADDIW) && rd != 0 && rd == rs1 && imm_bits <= 6)
  {
    insn->insn_opcode = MATCH_C_ADDIW;
    INSERT_OPERAND(CRD, *insn, rd);
    INSERT_OPERAND(CIMM6, *insn, imm);
  }
  else if(INSN_MATCHES(*insn, JALR_J) && rd == 0 && rs1 != 1 && imm == 0)
  {
    // jalr.j rd=0, rs1 != 1, imm=0 is encoded as c.addi rd=0, imm={1'b0,rs1}
    insn->insn_opcode = MATCH_C_ADDI;
    INSERT_OPERAND(CIMM6, *insn, rs1);
  }
  else if(INSN_MATCHES(*insn, JALR_R) && rd == 0 && rs1 == 1 && imm == 0)
  {
    // jalr.r rd=0, rs1=1, imm=0 is encoded as c.addi rd=0, imm={1'b0,rs1}
    insn->insn_opcode = MATCH_C_ADDI;
    INSERT_OPERAND(CIMM6, *insn, rs1);
  }
  else if(INSN_MATCHES(*insn, JALR_C) && rd == 1 && imm == 0)
  {
    // jalr.c rd=1, rs1, imm=0 is encoded as c.addi rd=0, imm={1'b1,rs1}
    insn->insn_opcode = MATCH_C_ADDI;
    INSERT_OPERAND(CIMM6, *insn, 0x20 | rs1);
  }
  else if((INSN_MATCHES(*insn, ADDI) || INSN_MATCHES(*insn, ORI) ||
          INSN_MATCHES(*insn, XORI)) && rs1 == 0 && imm_bits <= 6)
  {
    insn->insn_opcode = MATCH_C_LI;
    INSERT_OPERAND(CRD, *insn, rd);
    INSERT_OPERAND(CIMM6, *insn, imm);
  }
  else if((INSN_MATCHES(*insn, ADDI) || INSN_MATCHES(*insn, ORI) ||
          INSN_MATCHES(*insn, XORI)) && rs1 == 0 && imm_bits <= 6)
  {
    insn->insn_opcode = MATCH_C_LI;
    INSERT_OPERAND(CRD, *insn, rd);
    INSERT_OPERAND(CIMM6, *insn, imm);
  }
  else if((INSN_MATCHES(*insn, ADDI) || INSN_MATCHES(*insn, ORI) ||
           INSN_MATCHES(*insn, XORI)) && imm == 0)
  {
    insn->insn_opcode = MATCH_C_MOVE;
    INSERT_OPERAND(CRD, *insn, rd);
    INSERT_OPERAND(CRS1, *insn, rs1);
  }
  else if((INSN_MATCHES(*insn, ADD) || INSN_MATCHES(*insn, OR) ||
           INSN_MATCHES(*insn, XOR)) && 
          (rs1 == 0 || rs2 == 0))
  {
    insn->insn_opcode = MATCH_C_MOVE;
    INSERT_OPERAND(CRD, *insn, rd);
    INSERT_OPERAND(CRS1, *insn, rs1 == 0 ? rs2 : rs1);
  }
  else if(INSN_MATCHES(*insn, ADD) && (rd == rs1 || rd == rs2))
  {
    insn->insn_opcode = MATCH_C_ADD;
    INSERT_OPERAND(CRD, *insn, rd);
    INSERT_OPERAND(CRS1, *insn, rd == rs1 ? rs2 : rs1);
  }
  else if(INSN_MATCHES(*insn, SUB) && rd == rs2)
  {
    insn->insn_opcode = MATCH_C_SUB;
    INSERT_OPERAND(CRD, *insn, rd);
    INSERT_OPERAND(CRS1, *insn, rs1);
  }
  else if(INSN_MATCHES(*insn, ADD) && is_rvc_reg(rd, rd) && is_rvc_reg(rs1, rs1) && is_rvc_reg(rs2b, rs2))
  {
    insn->insn_opcode = MATCH_C_ADD3;
    INSERT_OPERAND(CRDS, *insn, rvc_reg(rd, rd));
    INSERT_OPERAND(CRS1S, *insn, rvc_reg(rs1, rs1));
    INSERT_OPERAND(CRS2BS, *insn, rvc_reg(rs2b, rs2));
  }
  else if(INSN_MATCHES(*insn, SUB) && is_rvc_reg(rd, rd) && is_rvc_reg(rs1, rs1) && is_rvc_reg(rs2b, rs2))
  {
    insn->insn_opcode = MATCH_C_SUB3;
    INSERT_OPERAND(CRDS, *insn, rvc_reg(rd, rd));
    INSERT_OPERAND(CRS1S, *insn, rvc_reg(rs1, rs1));
    INSERT_OPERAND(CRS2BS, *insn, rvc_reg(rs2b, rs2));
  }
  else if(INSN_MATCHES(*insn, OR) && is_rvc_reg(rd, rd) && is_rvc_reg(rs1, rs1) && is_rvc_reg(rs2b, rs2))
  {
    insn->insn_opcode = MATCH_C_OR3;
    INSERT_OPERAND(CRDS, *insn, rvc_reg(rd, rd));
    INSERT_OPERAND(CRS1S, *insn, rvc_reg(rs1, rs1));
    INSERT_OPERAND(CRS2BS, *insn, rvc_reg(rs2b, rs2));
  }
  else if(INSN_MATCHES(*insn, AND) && is_rvc_reg(rd, rd) && is_rvc_reg(rs1, rs1) && is_rvc_reg(rs2b, rs2))
  {
    insn->insn_opcode = MATCH_C_AND3;
    INSERT_OPERAND(CRDS, *insn, rvc_reg(rd, rd));
    INSERT_OPERAND(CRS1S, *insn, rvc_reg(rs1, rs1));
    INSERT_OPERAND(CRS2BS, *insn, rvc_reg(rs2b, rs2));
  }
  else if(INSN_MATCHES(*insn, SLLI) && rd == rs1 && is_rvc_reg(rd, rd))
  {
    insn->insn_opcode = shamt >= 32 ? MATCH_C_SLLI32 : MATCH_C_SLLI;
    INSERT_OPERAND(CRDS, *insn, rvc_reg(rd, rd));
    INSERT_OPERAND(CIMM5, *insn, shamt);
  }
  else if(INSN_MATCHES(*insn, SRLI) && rd == rs1 && is_rvc_reg(rd, rd))
  {
    insn->insn_opcode = shamt >= 32 ? MATCH_C_SRLI32 : MATCH_C_SRLI;
    INSERT_OPERAND(CRDS, *insn, rvc_reg(rd, rd));
    INSERT_OPERAND(CIMM5, *insn, shamt);
  }
  else if(INSN_MATCHES(*insn, SRAI) && rd == rs1 && is_rvc_reg(rd, rd))
  {
    insn->insn_opcode = shamt >= 32 ? MATCH_C_SRAI32 : MATCH_C_SRAI;
    INSERT_OPERAND(CRDS, *insn, rvc_reg(rd, rd));
    INSERT_OPERAND(CIMM5, *insn, shamt);
  }
  else if(INSN_MATCHES(*insn, SLLIW) && rd == rs1 && is_rvc_reg(rd, rd))
  {
    insn->insn_opcode = MATCH_C_SLLIW;
    INSERT_OPERAND(CRDS, *insn, rvc_reg(rd, rd));
    INSERT_OPERAND(CIMM5, *insn, shamt);
  }
  else if(INSN_MATCHES(*insn, J) && jt_bits <= 10)
  {
    insn->insn_opcode = MATCH_C_J;
    INSERT_OPERAND(CIMM10, *insn, jt);
  }
  else if(INSN_MATCHES(*insn, BEQ) && rs1 == rs2 && bimm_bits <= 10)
  {
    insn->insn_opcode = MATCH_C_J;
    INSERT_OPERAND(CIMM10, *insn, bimm);
  }
  else if(INSN_MATCHES(*insn, BEQ) && is_rvc_reg(rs1, rs1) && is_rvc_reg(rs2, rs2) && bimm_bits <= 5)
  {
    insn->insn_opcode = MATCH_C_BEQ;
    INSERT_OPERAND(CRS1S, *insn, rvc_reg(rs1, rs1));
    INSERT_OPERAND(CRS2S, *insn, rvc_reg(rs2, rs2));
    INSERT_OPERAND(CIMM5, *insn, bimm);
  }
  else if(INSN_MATCHES(*insn, BNE) && is_rvc_reg(rs1, rs1) && is_rvc_reg(rs2, rs2) && bimm_bits <= 5)
  {
    insn->insn_opcode = MATCH_C_BNE;
    INSERT_OPERAND(CRS1S, *insn, rvc_reg(rs1, rs1));
    INSERT_OPERAND(CRS2S, *insn, rvc_reg(rs2, rs2));
    INSERT_OPERAND(CIMM5, *insn, bimm);
  }
  else if(INSN_MATCHES(*insn, LD) && rs1 == 30 && imm%8 == 0 && imm_bits <= 9)
  {
    insn->insn_opcode = MATCH_C_LDSP;
    INSERT_OPERAND(CRD, *insn, rd);
    INSERT_OPERAND(CIMM6, *insn, imm/8);
  }
  else if(INSN_MATCHES(*insn, LW) && rs1 == 30 && imm%4 == 0 && imm_bits <= 8)
  {
    insn->insn_opcode = MATCH_C_LWSP;
    INSERT_OPERAND(CRD, *insn, rd);
    INSERT_OPERAND(CIMM6, *insn, imm/4);
  }
  else if(INSN_MATCHES(*insn, SD) && rs1 == 30 && bimm%8 == 0 && bimm_bits <= 9)
  {
    insn->insn_opcode = MATCH_C_SDSP;
    INSERT_OPERAND(CRS2, *insn, rs2);
    INSERT_OPERAND(CIMM6, *insn, bimm/8);
  }
  else if(INSN_MATCHES(*insn, SW) && rs1 == 30 && bimm%4 == 0 && bimm_bits <= 8)
  {
    insn->insn_opcode = MATCH_C_SWSP;
    INSERT_OPERAND(CRS2, *insn, rs2);
    INSERT_OPERAND(CIMM6, *insn, bimm/4);
  }
  else if(INSN_MATCHES(*insn, LD) && is_rvc_reg(rs1, rs1) && is_rvc_reg(rd, rd) && imm%8 == 0 && imm_bits <= 8)
  {
    insn->insn_opcode = MATCH_C_LD;
    INSERT_OPERAND(CRS1S, *insn, rvc_reg(rs1, rs1));
    INSERT_OPERAND(CRDS, *insn, rvc_reg(rd, rd));
    INSERT_OPERAND(CIMM5, *insn, imm/8);
  }
  else if(INSN_MATCHES(*insn, LW) && is_rvc_reg(rs1, rs1) && is_rvc_reg(rd, rd) && imm%4 == 0 && imm_bits <= 7)
  {
    insn->insn_opcode = MATCH_C_LW;
    INSERT_OPERAND(CRS1S, *insn, rvc_reg(rs1, rs1));
    INSERT_OPERAND(CRDS, *insn, rvc_reg(rd, rd));
    INSERT_OPERAND(CIMM5, *insn, imm/4);
  }
  else if(INSN_MATCHES(*insn, SD) && is_rvc_reg(rs1, rs1) && is_rvc_reg(rs2, rs2) && bimm%8 == 0 && bimm_bits <= 8)
  {
    insn->insn_opcode = MATCH_C_SD;
    INSERT_OPERAND(CRS1S, *insn, rvc_reg(rs1, rs1));
    INSERT_OPERAND(CRS2S, *insn, rvc_reg(rs2, rs2));
    INSERT_OPERAND(CIMM5, *insn, bimm/8);
  }
  else if(INSN_MATCHES(*insn, SW) && is_rvc_reg(rs1, rs1) && is_rvc_reg(rs2, rs2) && bimm%4 == 0 && bimm_bits <= 7)
  {
    insn->insn_opcode = MATCH_C_SW;
    INSERT_OPERAND(CRS1S, *insn, rvc_reg(rs1, rs1));
    INSERT_OPERAND(CRS2S, *insn, rvc_reg(rs2, rs2));
    INSERT_OPERAND(CIMM5, *insn, bimm/4);
  }
  else if(INSN_MATCHES(*insn, LD) && imm == 0)
  {
    insn->insn_opcode = MATCH_C_LD0;
    INSERT_OPERAND(CRS1, *insn, rs1);
    INSERT_OPERAND(CRD, *insn, rd);
  }
  else if(INSN_MATCHES(*insn, LW) && imm == 0)
  {
    insn->insn_opcode = MATCH_C_LW0;
    INSERT_OPERAND(CRS1, *insn, rs1);
    INSERT_OPERAND(CRD, *insn, rd);
  }
  else if(INSN_MATCHES(*insn, FLD) && is_rvc_reg(rs1, rs1) && is_rvc_reg(rd, rd) && imm%8 == 0 && imm_bits <= 8)
  {
    insn->insn_opcode = MATCH_C_FLD;
    INSERT_OPERAND(CRS1S, *insn, rvc_reg(rs1, rs1));
    INSERT_OPERAND(CRDS, *insn, rvc_reg(rd, rd));
    INSERT_OPERAND(CIMM5, *insn, imm/8);
  }
  else if(INSN_MATCHES(*insn, FLW) && is_rvc_reg(rs1, rs1) && is_rvc_reg(rd, rd) && imm%4 == 0 && imm_bits <= 7)
  {
    insn->insn_opcode = MATCH_C_FLW;
    INSERT_OPERAND(CRS1S, *insn, rvc_reg(rs1, rs1));
    INSERT_OPERAND(CRDS, *insn, rvc_reg(rd, rd));
    INSERT_OPERAND(CIMM5, *insn, imm/4);
  }
  else if(INSN_MATCHES(*insn, FSD) && is_rvc_reg(rs1, rs1) && is_rvc_reg(rs2, rs2) && bimm%8 == 0 && bimm_bits <= 8)
  {
    insn->insn_opcode = MATCH_C_FSD;
    INSERT_OPERAND(CRS1S, *insn, rvc_reg(rs1, rs1));
    INSERT_OPERAND(CRS2S, *insn, rvc_reg(rs2, rs2));
    INSERT_OPERAND(CIMM5, *insn, bimm/8);
  }
  else if(INSN_MATCHES(*insn, FSW) && is_rvc_reg(rs1, rs1) && is_rvc_reg(rs2, rs2) && bimm%4 == 0 && bimm_bits <= 7)
  {
    insn->insn_opcode = MATCH_C_FSW;
    INSERT_OPERAND(CRS1S, *insn, rvc_reg(rs1, rs1));
    INSERT_OPERAND(CRS2S, *insn, rvc_reg(rs2, rs2));
    INSERT_OPERAND(CIMM5, *insn, bimm/4);
  }
  else
    return 0;

  gas_assert(insn_length(insn) == 2);

  return 1;
}

/* Initialise INSN from opcode entry MO.  Leave its position unspecified.  */

static void
create_insn (struct mips_cl_insn *insn, const struct riscv_opcode *mo)
{
  size_t i;

  insn->insn_mo = mo;
  insn->insn_opcode = mo->match;
  insn->frag = NULL;
  insn->where = 0;
  for (i = 0; i < ARRAY_SIZE (insn->fixp); i++)
    insn->fixp[i] = NULL;
}

/* Install INSN at the location specified by its "frag" and "where" fields.  */

static void
install_insn (const struct mips_cl_insn *insn)
{
  char *f = insn->frag->fr_literal + insn->where;
  md_number_to_chars (f, insn->insn_opcode, insn_length(insn));
}

/* Move INSN to offset WHERE in FRAG.  Adjust the fixups accordingly
   and install the opcode in the new location.  */

static void
move_insn (struct mips_cl_insn *insn, fragS *frag, long where)
{
  size_t i;

  insn->frag = frag;
  insn->where = where;
  for (i = 0; i < ARRAY_SIZE (insn->fixp); i++)
    if (insn->fixp[i] != NULL)
      {
	insn->fixp[i]->fx_frag = frag;
	insn->fixp[i]->fx_where = where;
      }
  install_insn (insn);
}

/* Add INSN to the end of the output.  */

static void
add_fixed_insn (struct mips_cl_insn *insn)
{
  char *f = frag_more (insn_length (insn));
  move_insn (insn, frag_now, f - frag_now->fr_literal);
}

struct regname {
  const char *name;
  unsigned int num;
};

#define RTYPE_MASK	0x1ff00
#define RTYPE_NUM	0x00100
#define RTYPE_FPU	0x00200
#define RTYPE_VEC	0x00800
#define RTYPE_GP	0x01000
#define RTYPE_CP0	0x02000
#define RTYPE_VGR_REG	0x20000
#define RTYPE_VFP_REG	0x40000
#define RNUM_MASK	0x000ff
#define RWARN		0x80000

#define GENERIC_REGISTER_NUMBERS \
    {"x0",	RTYPE_NUM | 0},  \
    {"x1",	RTYPE_NUM | 1},  \
    {"x2",	RTYPE_NUM | 2},  \
    {"x3",	RTYPE_NUM | 3},  \
    {"x4",	RTYPE_NUM | 4},  \
    {"x5",	RTYPE_NUM | 5},  \
    {"x6",	RTYPE_NUM | 6},  \
    {"x7",	RTYPE_NUM | 7},  \
    {"x8",	RTYPE_NUM | 8},  \
    {"x9",	RTYPE_NUM | 9},  \
    {"x10",	RTYPE_NUM | 10}, \
    {"x11",	RTYPE_NUM | 11}, \
    {"x12",	RTYPE_NUM | 12}, \
    {"x13",	RTYPE_NUM | 13}, \
    {"x14",	RTYPE_NUM | 14}, \
    {"x15",	RTYPE_NUM | 15}, \
    {"x16",	RTYPE_NUM | 16}, \
    {"x17",	RTYPE_NUM | 17}, \
    {"x18",	RTYPE_NUM | 18}, \
    {"x19",	RTYPE_NUM | 19}, \
    {"x20",	RTYPE_NUM | 20}, \
    {"x21",	RTYPE_NUM | 21}, \
    {"x22",	RTYPE_NUM | 22}, \
    {"x23",	RTYPE_NUM | 23}, \
    {"x24",	RTYPE_NUM | 24}, \
    {"x25",	RTYPE_NUM | 25}, \
    {"x26",	RTYPE_NUM | 26}, \
    {"x27",	RTYPE_NUM | 27}, \
    {"x28",	RTYPE_NUM | 28}, \
    {"x29",	RTYPE_NUM | 29}, \
    {"x30",	RTYPE_NUM | 30}, \
    {"x31",	RTYPE_NUM | 31} 

#define FP_REGISTER_NAMES       \
    {"f0",	RTYPE_FPU | 0},  \
    {"f1",	RTYPE_FPU | 1},  \
    {"f2",	RTYPE_FPU | 2},  \
    {"f3",	RTYPE_FPU | 3},  \
    {"f4",	RTYPE_FPU | 4},  \
    {"f5",	RTYPE_FPU | 5},  \
    {"f6",	RTYPE_FPU | 6},  \
    {"f7",	RTYPE_FPU | 7},  \
    {"f8",	RTYPE_FPU | 8},  \
    {"f9",	RTYPE_FPU | 9},  \
    {"f10",	RTYPE_FPU | 10}, \
    {"f11",	RTYPE_FPU | 11}, \
    {"f12",	RTYPE_FPU | 12}, \
    {"f13",	RTYPE_FPU | 13}, \
    {"f14",	RTYPE_FPU | 14}, \
    {"f15",	RTYPE_FPU | 15}, \
    {"f16",	RTYPE_FPU | 16}, \
    {"f17",	RTYPE_FPU | 17}, \
    {"f18",	RTYPE_FPU | 18}, \
    {"f19",	RTYPE_FPU | 19}, \
    {"f20",	RTYPE_FPU | 20}, \
    {"f21",	RTYPE_FPU | 21}, \
    {"f22",	RTYPE_FPU | 22}, \
    {"f23",	RTYPE_FPU | 23}, \
    {"f24",	RTYPE_FPU | 24}, \
    {"f25",	RTYPE_FPU | 25}, \
    {"f26",	RTYPE_FPU | 26}, \
    {"f27",	RTYPE_FPU | 27}, \
    {"f28",	RTYPE_FPU | 28}, \
    {"f29",	RTYPE_FPU | 29}, \
    {"f30",	RTYPE_FPU | 30}, \
    {"f31",	RTYPE_FPU | 31}

#define CR_REGISTER_NUMBERS \
    {"cr0",	RTYPE_CP0 | 0},  \
    {"cr1",	RTYPE_CP0 | 1},  \
    {"cr2",	RTYPE_CP0 | 2},  \
    {"cr3",	RTYPE_CP0 | 3},  \
    {"cr4",	RTYPE_CP0 | 4},  \
    {"cr5",	RTYPE_CP0 | 5},  \
    {"cr6",	RTYPE_CP0 | 6},  \
    {"cr7",	RTYPE_CP0 | 7},  \
    {"cr8",	RTYPE_CP0 | 8},  \
    {"cr9",	RTYPE_CP0 | 9},  \
    {"cr10",	RTYPE_CP0 | 10}, \
    {"cr11",	RTYPE_CP0 | 11}, \
    {"cr12",	RTYPE_CP0 | 12}, \
    {"cr13",	RTYPE_CP0 | 13}, \
    {"cr14",	RTYPE_CP0 | 14}, \
    {"cr15",	RTYPE_CP0 | 15}, \
    {"cr16",	RTYPE_CP0 | 16}, \
    {"cr17",	RTYPE_CP0 | 17}, \
    {"cr18",	RTYPE_CP0 | 18}, \
    {"cr19",	RTYPE_CP0 | 19}, \
    {"cr20",	RTYPE_CP0 | 20}, \
    {"cr21",	RTYPE_CP0 | 21}, \
    {"cr22",	RTYPE_CP0 | 22}, \
    {"cr23",	RTYPE_CP0 | 23}, \
    {"cr24",	RTYPE_CP0 | 24}, \
    {"cr25",	RTYPE_CP0 | 25}, \
    {"cr26",	RTYPE_CP0 | 26}, \
    {"cr27",	RTYPE_CP0 | 27}, \
    {"cr28",	RTYPE_CP0 | 28}, \
    {"cr29",	RTYPE_CP0 | 29}, \
    {"cr30",	RTYPE_CP0 | 30}, \
    {"cr31",	RTYPE_CP0 | 31} 

/* Remaining symbolic register names */
#define SYMBOLIC_REGISTER_NAMES \
    {"zero",	RTYPE_GP | 0},  \
    {"ra",	RTYPE_GP | 1},  \
    {"v0",	RTYPE_GP | 2},  \
    {"v1",	RTYPE_GP | 3},  \
    {"a0",	RTYPE_GP | 4},  \
    {"a1",	RTYPE_GP | 5},  \
    {"a2",	RTYPE_GP | 6},  \
    {"a3",	RTYPE_GP | 7},  \
    {"a4",	RTYPE_GP | 8},  \
    {"a5",	RTYPE_GP | 9},  \
    {"a6",	RTYPE_GP | 10},  \
    {"a7",	RTYPE_GP | 11}, \
    {"t0",	RTYPE_GP | 12}, \
    {"t1",	RTYPE_GP | 13}, \
    {"t2",	RTYPE_GP | 14}, \
    {"t3",	RTYPE_GP | 15}, \
    {"t4",	RTYPE_GP | 16}, \
    {"t5",	RTYPE_GP | 17}, \
    {"t6",	RTYPE_GP | 18}, \
    {"t7",	RTYPE_GP | 19}, \
    {"s0",	RTYPE_GP | 20}, \
    {"s1",	RTYPE_GP | 21}, \
    {"s2",	RTYPE_GP | 22}, \
    {"s3",	RTYPE_GP | 23}, \
    {"s4",	RTYPE_GP | 24}, \
    {"s5",	RTYPE_GP | 25}, \
    {"s6",	RTYPE_GP | 26}, \
    {"s7",	RTYPE_GP | 27}, \
    {"s8",	RTYPE_GP | 28}, \
    {"gp",	RTYPE_GP | 28}, \
    {"s9",	RTYPE_GP | 29}, \
    {"sp",	RTYPE_GP | 30}, \
    {"tp",	RTYPE_GP | 31}

#define FP_SYMBOLIC_REGISTER_NAMES \
    {"ft0",	RTYPE_FPU | 0},  \
    {"ft1",	RTYPE_FPU | 1},  \
    {"fv0",	RTYPE_FPU | 2},  \
    {"fv1",	RTYPE_FPU | 3},  \
    {"fa0",	RTYPE_FPU | 4},  \
    {"fa1",	RTYPE_FPU | 5},  \
    {"fa2",	RTYPE_FPU | 6},  \
    {"fa3",	RTYPE_FPU | 7},  \
    {"fa4",	RTYPE_FPU | 8},  \
    {"fa5",	RTYPE_FPU | 9},  \
    {"fa6",	RTYPE_FPU | 10},  \
    {"fa7",	RTYPE_FPU | 11}, \
    {"ft2",	RTYPE_FPU | 12}, \
    {"ft3",	RTYPE_FPU | 13}, \
    {"ft4",	RTYPE_FPU | 14}, \
    {"ft5",	RTYPE_FPU | 15}, \
    {"ft6",	RTYPE_FPU | 16}, \
    {"ft7",	RTYPE_FPU | 17}, \
    {"ft8",	RTYPE_FPU | 18}, \
    {"ft9",	RTYPE_FPU | 19}, \
    {"fs0",	RTYPE_FPU | 20}, \
    {"fs1",	RTYPE_FPU | 21}, \
    {"fs2",	RTYPE_FPU | 22}, \
    {"fs3",	RTYPE_FPU | 23}, \
    {"fs4",	RTYPE_FPU | 24}, \
    {"fs5",	RTYPE_FPU | 25}, \
    {"fs6",	RTYPE_FPU | 26}, \
    {"fs7",	RTYPE_FPU | 27}, \
    {"fs8",	RTYPE_FPU | 28}, \
    {"fs9",	RTYPE_FPU | 29}, \
    {"ft10",	RTYPE_FPU | 30}, \
    {"ft11",	RTYPE_FPU | 31}

#define RISCV_VEC_GR_REGISTER_NAMES \
    {"vx0",	RTYPE_VGR_REG | 0}, \
    {"vx1",	RTYPE_VGR_REG | 1}, \
    {"vx2",	RTYPE_VGR_REG | 2}, \
    {"vx3",	RTYPE_VGR_REG | 3}, \
    {"vx4",	RTYPE_VGR_REG | 4}, \
    {"vx5",	RTYPE_VGR_REG | 5}, \
    {"vx6",	RTYPE_VGR_REG | 6}, \
    {"vx7",	RTYPE_VGR_REG | 7}, \
    {"vx8",	RTYPE_VGR_REG | 8}, \
    {"vx9",	RTYPE_VGR_REG | 9}, \
    {"vx10",	RTYPE_VGR_REG | 10}, \
    {"vx11",	RTYPE_VGR_REG | 11}, \
    {"vx12",	RTYPE_VGR_REG | 12}, \
    {"vx13",	RTYPE_VGR_REG | 13}, \
    {"vx14",	RTYPE_VGR_REG | 14}, \
    {"vx15",	RTYPE_VGR_REG | 15}, \
    {"vx16",	RTYPE_VGR_REG | 16}, \
    {"vx17",	RTYPE_VGR_REG | 17}, \
    {"vx18",	RTYPE_VGR_REG | 18}, \
    {"vx19",	RTYPE_VGR_REG | 19}, \
    {"vx20",	RTYPE_VGR_REG | 20}, \
    {"vx21",	RTYPE_VGR_REG | 21}, \
    {"vx22",	RTYPE_VGR_REG | 22}, \
    {"vx23",	RTYPE_VGR_REG | 23}, \
    {"vx24",	RTYPE_VGR_REG | 24}, \
    {"vx25",	RTYPE_VGR_REG | 25}, \
    {"vx26",	RTYPE_VGR_REG | 26}, \
    {"vx27",	RTYPE_VGR_REG | 27}, \
    {"vx28",	RTYPE_VGR_REG | 28}, \
    {"vx29",	RTYPE_VGR_REG | 29}, \
    {"vx30",	RTYPE_VGR_REG | 30}, \
    {"vx31",	RTYPE_VGR_REG | 31}

#define RISCV_VEC_FP_REGISTER_NAMES \
    {"vf0",	RTYPE_VFP_REG | 0}, \
    {"vf1",	RTYPE_VFP_REG | 1}, \
    {"vf2",	RTYPE_VFP_REG | 2}, \
    {"vf3",	RTYPE_VFP_REG | 3}, \
    {"vf4",	RTYPE_VFP_REG | 4}, \
    {"vf5",	RTYPE_VFP_REG | 5}, \
    {"vf6",	RTYPE_VFP_REG | 6}, \
    {"vf7",	RTYPE_VFP_REG | 7}, \
    {"vf8",	RTYPE_VFP_REG | 8}, \
    {"vf9",	RTYPE_VFP_REG | 9}, \
    {"vf10",	RTYPE_VFP_REG | 10}, \
    {"vf11",	RTYPE_VFP_REG | 11}, \
    {"vf12",	RTYPE_VFP_REG | 12}, \
    {"vf13",	RTYPE_VFP_REG | 13}, \
    {"vf14",	RTYPE_VFP_REG | 14}, \
    {"vf15",	RTYPE_VFP_REG | 15}, \
    {"vf16",	RTYPE_VFP_REG | 16}, \
    {"vf17",	RTYPE_VFP_REG | 17}, \
    {"vf18",	RTYPE_VFP_REG | 18}, \
    {"vf19",	RTYPE_VFP_REG | 19}, \
    {"vf20",	RTYPE_VFP_REG | 20}, \
    {"vf21",	RTYPE_VFP_REG | 21}, \
    {"vf22",	RTYPE_VFP_REG | 22}, \
    {"vf23",	RTYPE_VFP_REG | 23}, \
    {"vf24",	RTYPE_VFP_REG | 24}, \
    {"vf25",	RTYPE_VFP_REG | 25}, \
    {"vf26",	RTYPE_VFP_REG | 26}, \
    {"vf27",	RTYPE_VFP_REG | 27}, \
    {"vf28",	RTYPE_VFP_REG | 28}, \
    {"vf29",	RTYPE_VFP_REG | 29}, \
    {"vf30",	RTYPE_VFP_REG | 30}, \
    {"vf31",	RTYPE_VFP_REG | 31}

static const struct regname reg_names[] = {
  GENERIC_REGISTER_NUMBERS,
  FP_REGISTER_NAMES,
  CR_REGISTER_NUMBERS,

  /* The $txx registers depends on the abi,
     these will be added later into the symbol table from
     one of the tables below once mips_abi is set after 
     parsing of arguments from the command line. */
  SYMBOLIC_REGISTER_NAMES,
  FP_SYMBOLIC_REGISTER_NAMES,

  RISCV_VEC_GR_REGISTER_NAMES,
  RISCV_VEC_FP_REGISTER_NAMES,

  {0, 0}
};

static struct hash_control *reg_names_hash = NULL;

static int
reg_lookup (char **s, unsigned int types, unsigned int *regnop)
{
  struct regname *r;
  char *e;
  char save_c;
  int reg = -1;

  /* Find end of name.  */
  e = *s;
  if (is_name_beginner (*e))
    ++e;
  while (is_part_of_name (*e))
    ++e;

  /* Terminate name.  */
  save_c = *e;
  *e = '\0';

  /* Look for the register.  */
  r = (struct regname *) hash_find (reg_names_hash, *s);
  if (r != NULL && (r->num & types))
    reg = r->num & RNUM_MASK;

  /* Advance to next token if a register was recognised.  */
  if (reg >= 0)
    *s = e;
  else if (types & RWARN)
    as_warn ("Unrecognized register name `%s'", *s);

  *e = save_c;
  if (regnop)
    *regnop = reg;
  return reg >= 0;
}

/* This function is called once, at assembler startup time.  It should set up
   all the tables, etc. that the MD part of the assembler will need.  */

void
md_begin (void)
{
  const char *retval = NULL;
  int i = 0;
  int broken = 0;

  if (! bfd_set_arch_mach (stdoutput, bfd_arch_riscv, file_mips_arch))
    as_warn (_("Could not set architecture and machine"));

  op_hash = hash_new ();

  for (i = 0; i < NUMOPCODES;)
    {
      const char *name = riscv_opcodes[i].name;

      retval = hash_insert (op_hash, name, (void *) &riscv_opcodes[i]);
      if (retval != NULL)
	{
	  fprintf (stderr, _("internal error: can't hash `%s': %s\n"),
		   riscv_opcodes[i].name, retval);
	  /* Probably a memory allocation problem?  Give up now.  */
	  as_fatal (_("Broken assembler.  No assembly attempted."));
	}
      do
	{
	  if (riscv_opcodes[i].pinfo != INSN_MACRO)
	    {
	      if (!validate_mips_insn (&riscv_opcodes[i]))
		broken = 1;
	      if (nop_insn.insn_mo == NULL && strcmp (name, "nop") == 0)
		{
		  create_insn (&nop_insn, riscv_opcodes + i);
		}
	    }
	  ++i;
	}
      while ((i < NUMOPCODES) && !strcmp (riscv_opcodes[i].name, name));
    }

  if (broken)
    as_fatal (_("Broken assembler.  No assembly attempted."));

  reg_names_hash = hash_new ();
  for (i = 0; reg_names[i].name; i++)
    {
      retval = hash_insert (reg_names_hash, reg_names[i].name,
			    (void*) &reg_names[i]);
      if (retval != NULL)
	{
	  fprintf (stderr, _("internal error: can't hash `%s': %s\n"),
		   reg_names[i].name, retval);
	  /* Probably a memory allocation problem?  Give up now.  */
	  as_fatal (_("Broken assembler.  No assembly attempted."));
	}
    }

  mips_clear_insn_labels ();

  mips_gprmask = 0;
  mips_fprmask = 0;

  /* set the default alignment for the text section (2**2) */
  record_alignment (text_section, 2);

#ifdef OBJ_ELF
  if (IS_ELF)
    {
      /* Sections must be aligned to 16 byte boundaries.  When configured
         for an embedded ELF target, we don't bother.  */
      if (strncmp (TARGET_OS, "elf", 3) != 0)
	{
	  (void) bfd_set_section_alignment (stdoutput, text_section, 4);
	  (void) bfd_set_section_alignment (stdoutput, data_section, 4);
	  (void) bfd_set_section_alignment (stdoutput, bss_section, 4);
	}

      /* Create a .reginfo section for register masks and a .mdebug
	 section for debugging information.  */
      {
	segT seg;
	subsegT subseg;
	flagword flags;
	segT sec;

	seg = now_seg;
	subseg = now_subseg;

	/* The ABI says this section should be loaded so that the
	   running program can access it.  However, we don't load it
	   if we are configured for an embedded target */
	flags = SEC_READONLY | SEC_DATA;
	if (strncmp (TARGET_OS, "elf", 3) != 0)
	  flags |= SEC_ALLOC | SEC_LOAD;

	if (mips_abi != ABI_64)
	  {
	    sec = subseg_new (".reginfo", (subsegT) 0);

	    bfd_set_section_flags (stdoutput, sec, flags);
	    bfd_set_section_alignment (stdoutput, sec, 3);

	    mips_regmask_frag = frag_more (sizeof (Elf32_External_RegInfo));
	  }
	else
	  {
	    /* The 64-bit ABI uses a .MIPS.options section rather than
               .reginfo section.  */
	    sec = subseg_new (".MIPS.options", (subsegT) 0);
	    bfd_set_section_flags (stdoutput, sec, flags);
	    bfd_set_section_alignment (stdoutput, sec, 3);

	    /* Set up the option header.  */
	    {
	      Elf_Internal_Options opthdr;
	      char *f;

	      opthdr.kind = ODK_REGINFO;
	      opthdr.size = (sizeof (Elf_External_Options)
			     + sizeof (Elf64_External_RegInfo));
	      opthdr.section = 0;
	      opthdr.info = 0;
	      f = frag_more (sizeof (Elf_External_Options));
	      bfd_riscv_elf_swap_options_out (stdoutput, &opthdr,
					     (Elf_External_Options *) f);

	      mips_regmask_frag = frag_more (sizeof (Elf64_External_RegInfo));
	    }
	  }

	if (mips_flag_pdr)
	  {
	    pdr_seg = subseg_new (".pdr", (subsegT) 0);
	    (void) bfd_set_section_flags (stdoutput, pdr_seg,
					  SEC_READONLY | SEC_RELOC
					  | SEC_DEBUGGING);
	    (void) bfd_set_section_alignment (stdoutput, pdr_seg, 2);
	  }

	subseg_set (seg, subseg);
      }
    }
#endif /* OBJ_ELF */
}

void
md_assemble (char *str)
{
  struct mips_cl_insn insn;
  bfd_reloc_code_real_type unused_reloc[3]
    = {BFD_RELOC_UNUSED, BFD_RELOC_UNUSED, BFD_RELOC_UNUSED};

  imm_expr.X_op = O_absent;
  imm2_expr.X_op = O_absent;
  offset_expr.X_op = O_absent;
  imm_reloc[0] = BFD_RELOC_UNUSED;
  imm_reloc[1] = BFD_RELOC_UNUSED;
  imm_reloc[2] = BFD_RELOC_UNUSED;
  offset_reloc[0] = BFD_RELOC_UNUSED;
  offset_reloc[1] = BFD_RELOC_UNUSED;
  offset_reloc[2] = BFD_RELOC_UNUSED;

  mips_ip (str, &insn);
  DBG ((_("returned from mips_ip(%s) insn_opcode = 0x%x\n"),
    str, insn.insn_opcode));
  

  if (insn_error)
    {
      as_bad ("%s `%s'", insn_error, str);
      return;
    }

  if (insn.insn_mo->pinfo == INSN_MACRO)
    macro (&insn);
  else
    {
      if (imm_expr.X_op != O_absent)
	append_insn (&insn, &imm_expr, imm_reloc);
      else if (offset_expr.X_op != O_absent)
	append_insn (&insn, &offset_expr, offset_reloc);
      else
	append_insn (&insn, NULL, unused_reloc);
    }
}

static inline bfd_boolean
got16_reloc_p (bfd_reloc_code_real_type reloc)
{
  return reloc == BFD_RELOC_MIPS_GOT16 || reloc == BFD_RELOC_MIPS16_GOT16;
}

static inline bfd_boolean
hi16_reloc_p (bfd_reloc_code_real_type reloc)
{
  return reloc == BFD_RELOC_HI16_S || reloc == BFD_RELOC_MIPS16_HI16_S;
}

static inline bfd_boolean
lo16_reloc_p (bfd_reloc_code_real_type reloc)
{
  return reloc == BFD_RELOC_LO16 || reloc == BFD_RELOC_MIPS16_LO16;
}

/* Return true if the given fixup is followed by a matching R_MIPS_LO16
   relocation.  */

static inline bfd_boolean
fixup_has_matching_lo_p (fixS *fixp)
{
  return (fixp->fx_next != NULL
	  && fixp->fx_next->fx_r_type == BFD_RELOC_LO16
	  && fixp->fx_addsy == fixp->fx_next->fx_addsy
	  && fixp->fx_offset == fixp->fx_next->fx_offset);
}

static void
add_relaxed_insn (struct mips_cl_insn *insn, int max_chars, int var,
      relax_substateT subtype, symbolS *symbol, offsetT offset)
{
  frag_grow (max_chars);
  move_insn (insn, frag_now, frag_more (0) - frag_now->fr_literal);
  frag_var (rs_machine_dependent, max_chars, var,
      subtype, symbol, offset, NULL);
}

/* Output an instruction.  IP is the instruction information.
   ADDRESS_EXPR is an operand of the instruction to be used with
   RELOC_TYPE.  */

static void
append_insn (struct mips_cl_insn *ip, expressionS *address_expr,
	     bfd_reloc_code_real_type *reloc_type)
{
  unsigned long pinfo;

  pinfo = ip->insn_mo->pinfo;

#ifdef OBJ_ELF
  /* The value passed to dwarf2_emit_insn is the distance between
     the beginning of the current instruction and the address that
     should be recorded in the debug tables.  For MIPS16 debug info
     we want to use ISA-encoded addresses, so we pass -1 for an
     address higher by one than the current.  */
  dwarf2_emit_insn (0);
#endif

  gas_assert(*reloc_type <= BFD_RELOC_UNUSED);

  /* don't compress instructions with relocs */
  int compressible = (*reloc_type == BFD_RELOC_UNUSED ||
    address_expr == NULL || address_expr->X_op == O_constant) && mips_opts.rvc;

  /* speculate that branches/jumps can be compressed.  if not, we'll relax. */
  if (address_expr != NULL && mips_opts.rvc)
  {
    int compressible_branch = *reloc_type == BFD_RELOC_16_PCREL_S2 &&
      (INSN_MATCHES(*ip, BEQ) || INSN_MATCHES(*ip, BNE));
    int compressible_jump = *reloc_type == BFD_RELOC_MIPS_JMP &&
      INSN_MATCHES(*ip, J);
    if(compressible_branch || compressible_jump)
    {
      if(riscv_rvc_compress(ip))
      {
        add_relaxed_insn(ip, 4 /* worst case length */, 0,
                         RELAX_BRANCH_ENCODE(compressible_jump, 0),
                         address_expr->X_add_symbol,
                         address_expr->X_add_number);
        *reloc_type = BFD_RELOC_UNUSED;
        return;
      }
    }
  }

  if(!compressible)
    add_fixed_insn(ip);

  if (address_expr != NULL)
    {
      if (address_expr->X_op == O_constant)
	{
	  unsigned int tmp;

	  switch (*reloc_type)
	    {
	    case BFD_RELOC_32:
	      ip->insn_opcode |= address_expr->X_add_number;
	      break;

	    case BFD_RELOC_HI16_S:
	      tmp = (address_expr->X_add_number + RISCV_IMM_REACH/2) >> RISCV_IMM_BITS;
	      ip->insn_opcode |= (tmp & ((1<<(32-RISCV_IMM_BITS))-1)) << OP_SH_BIGIMMEDIATE; // assumes lui bits == 32 - imm bits
	      break;

	    case BFD_RELOC_HI16:
	      ip->insn_opcode |= ((address_expr->X_add_number >> RISCV_IMM_BITS) & (RISCV_BIGIMM_REACH-1)) << OP_SH_BIGIMMEDIATE;
	      break;

	    case BFD_RELOC_UNUSED:
	    case BFD_RELOC_LO16:
	    case BFD_RELOC_MIPS_GOT_DISP:
		  /* Stores have a split immediate field. */
	      if (OPCODE_IS_STORE(ip->insn_opcode))
		{
		  int value = address_expr->X_add_number & (RISCV_IMM_REACH-1);
		  value = ((value >> RISCV_IMMLO_BITS) << OP_SH_IMMHI) |
		          ((value & ((1<<RISCV_IMMLO_BITS)-1)) << OP_SH_IMMLO);
		  ip->insn_opcode |= value;
		}
	      else
	        ip->insn_opcode |= (address_expr->X_add_number & (RISCV_IMM_REACH-1)) << OP_SH_IMMEDIATE;
	      break;

	    case BFD_RELOC_MIPS_JMP:
	      if ((address_expr->X_add_number & 1) != 0)
		as_bad (_("jump to misaligned address (0x%lx)"),
			(unsigned long) address_expr->X_add_number);
	      if ((address_expr->X_add_number + RISCV_JUMP_REACH/2) & (RISCV_JUMP_REACH-1))
		as_bad (_("jump address range overflow (0x%lx)"),
			(unsigned long) address_expr->X_add_number);
	      ip->insn_opcode |= ((unsigned long long)(address_expr->X_add_number & (RISCV_JUMP_REACH-1))/RISCV_JUMP_ALIGN) << OP_SH_TARGET;
	      break;

	    case BFD_RELOC_16_PCREL_S2:
	      if ((address_expr->X_add_number & 1) != 0)
		as_bad (_("branch to misaligned address (0x%lx)"),
			(unsigned long) address_expr->X_add_number);
	      if ((address_expr->X_add_number + RISCV_BRANCH_REACH/2) & (RISCV_BRANCH_REACH-1))
		as_bad (_("branch address range overflow (0x%lx)"),
			(unsigned long) address_expr->X_add_number);
	      unsigned delta = (((unsigned)address_expr->X_add_number & (RISCV_BRANCH_REACH-1)) >> RISCV_BRANCH_ALIGN_BITS) & ((1<<RISCV_BRANCH_BITS)-1);
	      ip->insn_opcode |= ((delta & ((1<<RISCV_IMMLO_BITS)-1)) << OP_SH_IMMLO) | (((delta >> RISCV_IMMLO_BITS) & ((1<<RISCV_IMMHI_BITS)-1)) << OP_SH_IMMHI);
	      break;

	    default:
	      internalError ();
	    }
	}
      else if (*reloc_type < BFD_RELOC_UNUSED)
	{
	  reloc_howto_type *howto;
	  int i;

	  /* In a compound relocation, it is the final (outermost)
	     operator that determines the relocated field.  */
	  for (i = 1; i < 3; i++)
	    if (reloc_type[i] == BFD_RELOC_UNUSED)
	      break;

	  howto = bfd_reloc_type_lookup (stdoutput, reloc_type[i - 1]);
	  if (howto == NULL)
	    as_bad (_("Unsupported MIPS relocation number %d"), reloc_type[i - 1]);
	  
	  ip->fixp[0] = fix_new_exp (ip->frag, ip->where,
				     bfd_get_reloc_size (howto),
				     address_expr,
				     reloc_type[0] == BFD_RELOC_16_PCREL_S2 ||
				     reloc_type[0] == BFD_RELOC_MIPS_JMP,
				     reloc_type[0]);

	  /* These relocations can have an addend that won't fit in
	     4 octets for 64bit assembly.  */
	  if (HAVE_64BIT_GPRS
	      && ! howto->partial_inplace
	      && (reloc_type[0] == BFD_RELOC_32
		  || reloc_type[0] == BFD_RELOC_GPREL16
		  || reloc_type[0] == BFD_RELOC_MIPS_LITERAL
		  || reloc_type[0] == BFD_RELOC_GPREL32
		  || reloc_type[0] == BFD_RELOC_64
		  || reloc_type[0] == BFD_RELOC_CTOR
		  || reloc_type[0] == BFD_RELOC_MIPS_SUB
		  || reloc_type[0] == BFD_RELOC_MIPS_SCN_DISP
		  || reloc_type[0] == BFD_RELOC_MIPS_REL16
		  || reloc_type[0] == BFD_RELOC_MIPS_RELGOT
		  || reloc_type[0] == BFD_RELOC_MIPS16_GPREL
		  || hi16_reloc_p (reloc_type[0])
		  || lo16_reloc_p (reloc_type[0])))
	    ip->fixp[0]->fx_no_overflow = 1;

	  /* Add fixups for the second and third relocations, if given.
	     Note that the ABI allows the second relocation to be
	     against RSS_UNDEF, RSS_GP, RSS_GP0 or RSS_LOC.  At the
	     moment we only use RSS_UNDEF, but we could add support
	     for the others if it ever becomes necessary.  */
	  for (i = 1; i < 3; i++)
	    if (reloc_type[i] != BFD_RELOC_UNUSED)
	      {
		ip->fixp[i] = fix_new (ip->frag, ip->where,
				       ip->fixp[0]->fx_size, NULL, 0,
				       FALSE, reloc_type[i]);

		/* Use fx_tcbit to mark compound relocs.  */
		ip->fixp[0]->fx_tcbit = 1;
		ip->fixp[i]->fx_tcbit = 1;
	      }
	}
    }

  if(compressible)
  {
    riscv_rvc_compress(ip);
    add_fixed_insn (ip);
  }

  install_insn (ip);

  /* Update the register mask information.  */
      if (pinfo & INSN_WRITE_GPR_D)
	mips_gprmask |= 1 << EXTRACT_OPERAND (RD, *ip);
      if (pinfo & INSN_WRITE_GPR_RA)
	mips_gprmask |= 1 << RA;
      if (pinfo & INSN_WRITE_FPR_D)
	mips_fprmask |= 1 << EXTRACT_OPERAND (FD, *ip);

      if (pinfo & INSN_READ_GPR_S)
	mips_gprmask |= 1 << EXTRACT_OPERAND (RS, *ip);
      if (pinfo & INSN_READ_GPR_T)
	mips_gprmask |= 1 << EXTRACT_OPERAND (RT, *ip);
      if (pinfo & INSN_READ_FPR_S)
	mips_fprmask |= 1 << EXTRACT_OPERAND (FS, *ip);
      if (pinfo & INSN_READ_FPR_T)
	mips_fprmask |= 1 << EXTRACT_OPERAND (FT, *ip);
      if (pinfo & INSN_READ_FPR_R)
	mips_fprmask |= 1 << EXTRACT_OPERAND (FR, *ip);
      /* Never set the bit for $0, which is always zero.  */
      mips_gprmask &= ~1 << 0;

  /* We just output an insn, so the next one doesn't have a label.  */
  mips_clear_insn_labels ();
}

/* Read a macro's relocation codes from *ARGS and store them in *R.
   The first argument in *ARGS will be either the code for a single
   relocation or -1 followed by the three codes that make up a
   composite relocation.  */

static void
macro_read_relocs (va_list *args, bfd_reloc_code_real_type *r)
{
  int i, next;

  next = va_arg (*args, int);
  if (next >= 0)
    r[0] = (bfd_reloc_code_real_type) next;
  else
    for (i = 0; i < 3; i++)
      r[i] = (bfd_reloc_code_real_type) va_arg (*args, int);
}

/* Build an instruction created by a macro expansion.  This is passed
   a pointer to the count of instructions created so far, an
   expression, the name of the instruction to build, an operand format
   string, and corresponding arguments.  */

static void
macro_build (expressionS *ep, const char *name, const char *fmt, ...)
{
  const struct riscv_opcode *mo;
  struct mips_cl_insn insn;
  bfd_reloc_code_real_type r[3];
  va_list args;

  va_start (args, fmt);

  r[0] = BFD_RELOC_UNUSED;
  r[1] = BFD_RELOC_UNUSED;
  r[2] = BFD_RELOC_UNUSED;
  mo = (struct riscv_opcode *) hash_find (op_hash, name);
  gas_assert (mo);
  gas_assert (strcmp (name, mo->name) == 0);

  while (1)
    {
      /* Search until we get a match for NAME.  It is assumed here that
	 macros will never generate MDMX, MIPS-3D, or MT instructions.  */
      if (strcmp (fmt, mo->args) == 0
	  && mo->pinfo != INSN_MACRO)
	break;

      ++mo;
      gas_assert (mo->name);
      gas_assert (strcmp (name, mo->name) == 0);
    }

  create_insn (&insn, mo);
  for (;;)
    {
      switch (*fmt++)
	{
	case '\0':
	  break;

        case '#':
          switch ( *fmt++ ) {
            case 'g':
              INSERT_OPERAND( IMMNGPR, insn, va_arg( args, int ) );
              continue;
            case 'f':
              INSERT_OPERAND( IMMNFPR, insn, va_arg( args, int ) );
              continue;
            case 'n':
              INSERT_OPERAND( IMMSEGNELM, insn, va_arg( args, int ) - 1 );
              continue;
            case 'm':
              INSERT_OPERAND( IMMSEGSTNELM, insn, va_arg( args, int ) - 1 );
              continue;
            case 'd':
              INSERT_OPERAND( VRD, insn, va_arg( args, int ) );
              continue;
            case 's':
              INSERT_OPERAND( VRS, insn, va_arg( args, int ) );
              continue;
            case 't':
              INSERT_OPERAND( VRT, insn, va_arg( args, int ) );
              continue;
            case 'r':
              INSERT_OPERAND( VRR, insn, va_arg( args, int ) );
              continue;
            case 'D':
              INSERT_OPERAND( VFD, insn, va_arg( args, int ) );
              continue;
            case 'S':
              INSERT_OPERAND( VFS, insn, va_arg( args, int ) );
              continue;
            case 'T':
              INSERT_OPERAND( VFT, insn, va_arg( args, int ) );
              continue;
            case 'R':
              INSERT_OPERAND( VFR, insn, va_arg( args, int ) );
              continue;
            default:
              internalError();
          }
          continue;

	case ',':
	case '(':
	case ')':
	  continue;

	case 't':
	  INSERT_OPERAND (RT, insn, va_arg (args, int));
	  continue;

	case 'T':
	case 'W':
	  INSERT_OPERAND (FT, insn, va_arg (args, int));
	  continue;

	case 'd':
	  INSERT_OPERAND (RD, insn, va_arg (args, int));
	  continue;

	case 'U':
	  {
	    int tmp = va_arg (args, int);

	    INSERT_OPERAND (RT, insn, tmp);
	    INSERT_OPERAND (RD, insn, tmp);
	    continue;
	  }

	case 'S':
	  INSERT_OPERAND (FS, insn, va_arg (args, int));
	  continue;

	case 'z':
	  continue;

	case '<':
	  INSERT_OPERAND (SHAMTW, insn, va_arg (args, int));
	  continue;

	case '>':
	  INSERT_OPERAND (SHAMT, insn, va_arg (args, int));
	  continue;

	case 'D':
	  INSERT_OPERAND (FD, insn, va_arg (args, int));
	  continue;

	case 'b':
	case 's':
	case 'E':
	  INSERT_OPERAND (RS, insn, va_arg (args, int));
	  continue;

	case 'm':
	  INSERT_OPERAND (RM, insn, va_arg (args, int));
	  continue;

	case 'j':
	case 'o':
	  macro_read_relocs (&args, r);
	  gas_assert (*r == BFD_RELOC_GPREL16
		  || *r == BFD_RELOC_MIPS_LITERAL
		  || *r == BFD_RELOC_LO16
		  || *r == BFD_RELOC_MIPS_GOT16
		  || *r == BFD_RELOC_MIPS_CALL16
		  || *r == BFD_RELOC_MIPS_GOT_DISP
		  || *r == BFD_RELOC_MIPS_GOT_LO16
		  || *r == BFD_RELOC_MIPS_CALL_LO16);
	  continue;

	case 'u':
	  macro_read_relocs (&args, r);
	  gas_assert (ep != NULL
		  && (ep->X_op == O_constant
		      || (ep->X_op == O_symbol
			  && (*r == BFD_RELOC_HI16_S
			      || *r == BFD_RELOC_HI16
			      /*|| *r == BFD_RELOC_GPREL16*/
			      || *r == BFD_RELOC_MIPS_GOT_HI16
			      || *r == BFD_RELOC_MIPS_CALL_HI16))));
	  continue;

	case 'p':
	  gas_assert (ep != NULL);

	  /*
	   * This allows macro() to pass an immediate expression for
	   * creating short branches without creating a symbol.
	   *
	   * We don't allow branch relaxation for these branches, as
	   * they should only appear in ".set nomacro" anyway.
	   */
	  if (ep->X_op == O_constant)
	    {
	      unsigned long long delta;
	      if ((ep->X_add_number & (RISCV_BRANCH_ALIGN-1)) != 0)
		as_bad (_("branch to misaligned address (0x%lx)"),
			(unsigned long) ep->X_add_number);
	      if ((ep->X_add_number + RISCV_BRANCH_REACH/2) & ~(RISCV_BRANCH_REACH-1))
		as_bad (_("branch address range overflow (0x%lx)"),
			(unsigned long) ep->X_add_number);
	      delta = (unsigned long long)(ep->X_add_number & (RISCV_BRANCH_REACH-1))/RISCV_BRANCH_ALIGN;
	      insn.insn_opcode |= ((delta & ((1<<RISCV_IMMLO_BITS)-1)) << OP_SH_IMMLO) | (((delta >> RISCV_IMMLO_BITS) & ((1<<RISCV_IMMHI_BITS)-1)) << OP_SH_IMMHI);
	      ep = NULL;
	    }
	  else
	    *r = BFD_RELOC_16_PCREL_S2;
	  continue;

	case 'a':
	  gas_assert (ep != NULL);
	  if (ep->X_op == O_constant)
	    {
	      if ((ep->X_add_number & (RISCV_JUMP_ALIGN-1)) != 0)
		as_bad (_("jump to misaligned address (0x%lx)"),
			(unsigned long) ep->X_add_number);
	      if ((ep->X_add_number + RISCV_JUMP_REACH/2) & ~(RISCV_JUMP_REACH-1))
		as_bad (_("jump address range overflow (0x%lx)"),
			(unsigned long) ep->X_add_number);
	      insn.insn_opcode |= ((unsigned long long)(ep->X_add_number & (RISCV_JUMP_REACH-1))/RISCV_JUMP_ALIGN) << OP_SH_TARGET;
	      ep = NULL;
	    }
	  else
	    *r = BFD_RELOC_MIPS_JMP;
	  continue;

	default:
	  internalError ();
	}
      break;
    }
  va_end (args);
  gas_assert (*r == BFD_RELOC_UNUSED ? ep == NULL : ep != NULL);

  append_insn (&insn, ep, r);
}

/*
 * Sign-extend 32-bit mode constants that have bit 31 set and all
 * higher bits unset.
 */
static void
normalize_constant_expr (expressionS *ex)
{
  if (ex->X_op == O_constant
      && IS_ZEXT_32BIT_NUM (ex->X_add_number))
    ex->X_add_number = (((ex->X_add_number & 0xffffffff) ^ 0x80000000)
			- 0x80000000);
}

/*
 * Sign-extend 32-bit mode address offsets that have bit 31 set and
 * all higher bits unset.
 */
static void
normalize_address_expr (expressionS *ex)
{
  if (((ex->X_op == O_constant && HAVE_32BIT_ADDRESSES)
	|| (ex->X_op == O_symbol && HAVE_32BIT_SYMBOLS))
      && IS_ZEXT_32BIT_NUM (ex->X_add_number))
    ex->X_add_number = (((ex->X_add_number & 0xffffffff) ^ 0x80000000)
			- 0x80000000);
}

/*
 * Generate a "lui" instruction.
 */
static void
macro_build_lui (expressionS *ep, int regnum)
{
  expressionS high_expr;
  const struct riscv_opcode *mo;
  struct mips_cl_insn insn;
  bfd_reloc_code_real_type r[3]
    = {BFD_RELOC_UNUSED, BFD_RELOC_UNUSED, BFD_RELOC_UNUSED};
  const char *name = "lui";
  const char *fmt = "d,u";

  high_expr = *ep;

  if (high_expr.X_op == O_constant)
    {
      /* We can compute the instruction now without a relocation entry.  */
      high_expr.X_add_number = ((high_expr.X_add_number + RISCV_IMM_REACH/2)
				>> RISCV_IMM_BITS) & (RISCV_IMM_REACH-1);
      *r = BFD_RELOC_UNUSED;
    }
  else
    {
      gas_assert (ep->X_op == O_symbol);
      *r = BFD_RELOC_HI16_S;
    }

  mo = hash_find (op_hash, name);
  gas_assert (strcmp (name, mo->name) == 0);
  gas_assert (strcmp (fmt, mo->args) == 0);
  create_insn (&insn, mo);

  insn.insn_opcode = insn.insn_mo->match;
  INSERT_OPERAND (RD, insn, regnum);
  if (*r == BFD_RELOC_UNUSED)
    {
      insn.insn_opcode |= high_expr.X_add_number;
      append_insn (&insn, NULL, r);
    }
  else
    append_insn (&insn, &high_expr, r);
}

/* Warn if an expression is not a constant.  */

static void
check_absolute_expr (struct mips_cl_insn *ip, expressionS *ex)
{
  if (ex->X_op == O_big)
    as_bad (_("unsupported large constant"));
  else if (ex->X_op != O_constant)
    as_bad (_("Instruction %s requires absolute expression"),
	    ip->insn_mo->name);

  if (HAVE_32BIT_GPRS)
    normalize_constant_expr (ex);
}

/* load_register generates an unoptimized instruction sequence to load
 * an absolute expression into a register. */
static void
load_register (int reg, expressionS *ep)
{
  gas_assert (ep->X_op == O_constant);
  gas_assert (reg != ZERO);

  // this is an awful way to generate arbitrary 64-bit constants.
  // fortunately, this is just used for hand-coded assembly programs.
  if(HAVE_64BIT_GPRS && !IS_SEXT_32BIT_NUM(ep->X_add_number))
  {
    expressionS upper = *ep, lower = *ep;
    upper.X_add_number = (int64_t)ep->X_add_number >> (RISCV_IMM_BITS-1);
    load_register(reg, &upper);

    macro_build (NULL, "sll", "d,s,>", reg, reg, RISCV_IMM_BITS-1);

    lower.X_add_number = ep->X_add_number & (RISCV_IMM_REACH/2-1);
    if (lower.X_add_number != 0)
      macro_build (&lower, "addi", "d,s,j", reg, reg, BFD_RELOC_LO16);
  }
  else // load a sign-extended 32-bit constant
  {
    int hi_reg = ZERO;

    int32_t hi = ep->X_add_number & (RISCV_IMM_REACH-1);
    hi = hi << (32-RISCV_IMM_BITS) >> (32-RISCV_IMM_BITS);
    hi = (int32_t)ep->X_add_number - hi;
    if(hi)
    {
      macro_build (ep, "lui", "d,u", reg, BFD_RELOC_HI16_S);
      hi_reg = reg;
    }

    if((ep->X_add_number & (RISCV_IMM_REACH-1)) || hi_reg == ZERO)
    {
      macro_build (ep, (HAVE_64BIT_GPRS ? "addiw" : "addi"), "d,s,j",
                   reg, hi_reg, BFD_RELOC_LO16);
    }
  }
}

/*
 *			Build macros
 *   This routine implements the seemingly endless macro or synthesized
 * instructions and addressing modes in the mips assembly language. Many
 * of these macros are simple and are similar to each other. These could
 * probably be handled by some kind of table or grammar approach instead of
 * this verbose method. Others are not simple macros but are more like
 * optimizing code generation.
 *   One interesting optimization is when several store macros appear
 * consecutively that would load AT with the upper half of the same address.
 * The ensuing load upper instructions are ommited. This implies some kind
 * of global optimization. We currently only optimize within a single macro.
 *   For many of the load and store macros if the address is specified as a
 * constant expression in the first 64k of memory (ie ld $2,0x4000c) we
 * first load register 'at' with zero and use it as the base register. The
 * mips assembler simply uses register $zero. Just one tiny optimization
 * we're missing.
 */
static void
macro (struct mips_cl_insn *ip)
{
  unsigned int sreg, dreg, breg;
  int mask;

  dreg = (ip->insn_opcode >> OP_SH_RD) & OP_MASK_RD;
  breg = sreg = (ip->insn_opcode >> OP_SH_RS) & OP_MASK_RS;
  mask = ip->insn_mo->mask;

  switch (mask)
    {
    case M_LA_AB:
      /* Load the address of a symbol into a register. */

      if(offset_expr.X_op == O_constant
         && offset_expr.X_add_number >= -(signed)RISCV_IMM_REACH/2
         && offset_expr.X_add_number < (signed)RISCV_IMM_REACH/2)
      {
        macro_build (&offset_expr, "addi",
                     "d,s,j", dreg, breg, BFD_RELOC_LO16);
        break;
      }

      if(!IS_SEXT_32BIT_NUM (offset_expr.X_add_number))
        as_bad(_("offset too large"));

      if(breg == dreg && breg != ZERO)
        as_bad(_("expression too complex"));

      if(offset_expr.X_op == O_constant)
      {
        load_register(dreg, &offset_expr);
        if(breg != ZERO)
           macro_build (NULL, "add", "d,s,t", dreg, dreg, breg);

        break;
      }

      /* We're loading a symbol, not an absolute address. */
      if(mips_pic != NO_PIC)
        as_bad("can't use la with PIC");

      if(HAVE_64BIT_SYMBOLS)
        as_bad("la is unimplemented for 64-bit symbols");

      macro_build_lui (&offset_expr, dreg);
      macro_build (&offset_expr, "addi", "d,s,j",
                   dreg, dreg, BFD_RELOC_LO16);

      if (breg != ZERO)
        macro_build (NULL, "add", "d,s,t", dreg, dreg, breg);
      break;

    case M_J: /* replace "j $rs" with "ret" if rs=ra, else with "jr $rs" */
      if (sreg == LINK_REG)
        macro_build (NULL, "ret", "");
      else
        macro_build (NULL, "jr", "s", sreg);
      break;

    case M_LI:
      load_register (dreg, &imm_expr);
      break;

    default:
      as_bad (_("Macro %s not implemented"), ip->insn_mo->name);
      break;
    }
}

/* For consistency checking, verify that all bits are specified either
   by the match/mask part of the instruction definition, or by the
   operand list.  */
static int
validate_mips_insn (const struct riscv_opcode *opc)
{
  const char *p = opc->args;
  char c;
  unsigned long used_bits = opc->mask;

  if ((used_bits & opc->match) != opc->match)
    {
      as_bad (_("internal: bad mips opcode (mask error): %s %s"),
	      opc->name, opc->args);
      return 0;
    }
#define USE_BITS(mask,shift)	(used_bits |= ((mask) << (shift)))
  while (*p)
    switch (c = *p++)
      {
      case '#':
    	switch (c = *p++)
	  {
	  case 'g': USE_BITS (OP_MASK_IMMNGPR, OP_SH_IMMNGPR); break;
	  case 'f': USE_BITS (OP_MASK_IMMNFPR, OP_SH_IMMNFPR); break;
	  case 'n': USE_BITS (OP_MASK_IMMSEGNELM, OP_SH_IMMSEGNELM); break;
	  case 'm': USE_BITS (OP_MASK_IMMSEGSTNELM, OP_SH_IMMSEGSTNELM); break;
	  case 'd': USE_BITS (OP_MASK_VRD, OP_SH_VRD); break;
	  case 's': USE_BITS (OP_MASK_VRS, OP_SH_VRS); break;
	  case 't': USE_BITS (OP_MASK_VRT, OP_SH_VRT); break;
	  case 'r': USE_BITS (OP_MASK_VRR, OP_SH_VRR); break;
	  case 'D': USE_BITS (OP_MASK_VFD, OP_SH_VFD); break;
	  case 'S': USE_BITS (OP_MASK_VFS, OP_SH_VFS); break;
	  case 'T': USE_BITS (OP_MASK_VFT, OP_SH_VFT); break;
	  case 'R': USE_BITS (OP_MASK_VFR, OP_SH_VFR); break;

	  default:
	    as_bad (_("internal: bad mips opcode (unknown extension operand type `#%c'): %s %s"),
		    c, opc->name, opc->args);
	    return 0;
	  }
	break;
      case ',': break;
      case '(': break;
      case ')': break;
      case '<': USE_BITS (OP_MASK_SHAMTW,	OP_SH_SHAMTW);	break;
      case '>':	USE_BITS (OP_MASK_SHAMT,	OP_SH_SHAMT);	break;
      case 'A': break;
      case 'D':	USE_BITS (OP_MASK_FD,		OP_SH_FD);	break;
      case 'E':	USE_BITS (OP_MASK_RS,		OP_SH_RS);	break;
      case 'I': break;
      case 'R':	USE_BITS (OP_MASK_FR,		OP_SH_FR);	break;
      case 'S':	USE_BITS (OP_MASK_FS,		OP_SH_FS);	break;
      case 'T':	USE_BITS (OP_MASK_FT,		OP_SH_FT);	break;
      case 'a':	USE_BITS (OP_MASK_TARGET,	OP_SH_TARGET);	break;
      case 'b':	USE_BITS (OP_MASK_RS,		OP_SH_RS);	break;
      case 'd':	USE_BITS (OP_MASK_RD,		OP_SH_RD);	break;
      case 'j':	USE_BITS (OP_MASK_IMMEDIATE,	OP_SH_IMMEDIATE);	break;
      case 'm':	USE_BITS (OP_MASK_RM,		OP_SH_RM);	break;
      case 'o': USE_BITS (OP_MASK_IMMEDIATE,	OP_SH_IMMEDIATE);	break;
      case 'p':	USE_BITS (OP_MASK_IMMLO,	OP_SH_IMMLO);
              	USE_BITS (OP_MASK_IMMHI,	OP_SH_IMMHI);	break;
      case 'q':	USE_BITS (OP_MASK_IMMLO,	OP_SH_IMMLO);
              	USE_BITS (OP_MASK_IMMHI,	OP_SH_IMMHI);	break;
      case 's':	USE_BITS (OP_MASK_RS,		OP_SH_RS);	break;
      case 't':	USE_BITS (OP_MASK_RT,		OP_SH_RT);	break;
      case 'u':	USE_BITS (OP_MASK_BIGIMMEDIATE,	OP_SH_BIGIMMEDIATE); break;
      case '[': break;
      case ']': break;
      case '0': break;
      default:
	as_bad (_("internal: bad mips opcode (unknown operand type `%c'): %s %s"),
		c, opc->name, opc->args);
	return 0;
      }
#undef USE_BITS
  if ((used_bits&0xffffffff) != 0xffffffff)
    {
      as_bad (_("internal: bad mips opcode (bits 0x%lx undefined): %s %s"),
	      ~used_bits & 0xffffffff, opc->name, opc->args);
      return 0;
    }
  return 1;
}

/* This routine assembles an instruction into its binary format.  As a
   side effect, it sets one of the global variables imm_reloc or
   offset_reloc to the type of relocation to do if one of the operands
   is an address expression.  */

static void
mips_ip (char *str, struct mips_cl_insn *ip)
{
  char *s;
  const char *args;
  char c = 0;
  struct riscv_opcode *insn;
  char *argsStart;
  unsigned int regno;
  char save_c = 0;
  int argnum;
  unsigned int rtype;

  insn_error = NULL;

  /* If the instruction contains a '.', we first try to match an instruction
     including the '.'.  Then we try again without the '.'.  */
  insn = NULL;
  for (s = str; *s != '\0' && !ISSPACE (*s); ++s)
    continue;

  /* If we stopped on whitespace, then replace the whitespace with null for
     the call to hash_find.  Save the character we replaced just in case we
     have to re-parse the instruction.  */
  if (ISSPACE (*s))
    {
      save_c = *s;
      *s++ = '\0';
    }

  insn = (struct riscv_opcode *) hash_find (op_hash, str);

  /* If we didn't find the instruction in the opcode table, try again, but
     this time with just the instruction up to, but not including the
     first '.'.  */
  if (insn == NULL)
    {
      /* Restore the character we overwrite above (if any).  */
      if (save_c)
	*(--s) = save_c;

      /* Scan up to the first '.' or whitespace.  */
      for (s = str;
	   *s != '\0' && *s != '.' && !ISSPACE (*s);
	   ++s)
	continue;

      /* If we did not find a '.', then we can quit now.  */
      if (*s != '.')
	{
	  insn_error = "unrecognized opcode";
	  return;
	}

      /* Lookup the instruction in the hash table.  */
      *s++ = '\0';
      if ((insn = (struct riscv_opcode *) hash_find (op_hash, str)) == NULL)
	{
	  insn_error = "unrecognized opcode";
	  return;
	}
    }

  argsStart = s;
  for (;;)
    {
      bfd_boolean ok = TRUE;
      gas_assert (strcmp (insn->name, str) == 0);

      create_insn (ip, insn);
      insn_error = NULL;
      argnum = 1;
      for (args = insn->args;; ++args)
	{
	  s += strspn (s, " \t");
	  switch (*args)
	    {
	    case '\0':		/* end of args */
	      if (*s == '\0')
		return;
	      break;

            case '#':
              switch ( *++args )
                {
                case 'g':
                  my_getExpression( &imm_expr, s );
                  check_absolute_expr( ip, &imm_expr );
                  if ((unsigned long) imm_expr.X_add_number > 32 )
                    as_warn( _( "Improper ngpr amount (%lu)" ),
                             (unsigned long) imm_expr.X_add_number );
                  INSERT_OPERAND( IMMNGPR, *ip, imm_expr.X_add_number );
                  imm_expr.X_op = O_absent;
                  s = expr_end;
                  continue;
                case 'f':
                  my_getExpression( &imm_expr, s );
                  check_absolute_expr( ip, &imm_expr );
                  if ((unsigned long) imm_expr.X_add_number > 32 )
                    as_warn( _( "Improper nfpr amount (%lu)" ),
                             (unsigned long) imm_expr.X_add_number );
                  INSERT_OPERAND( IMMNFPR, *ip, imm_expr.X_add_number );
                  imm_expr.X_op = O_absent;
                  s = expr_end;
                  continue;
                case 'n':
                  my_getExpression( &imm_expr, s );
                  check_absolute_expr( ip, &imm_expr );
                  if ((unsigned long) imm_expr.X_add_number > 32 )
                    as_warn( _( "Improper nelm amount (%lu)" ),
                             (unsigned long) imm_expr.X_add_number );
                  INSERT_OPERAND( IMMSEGNELM, *ip, imm_expr.X_add_number - 1 );
                  imm_expr.X_op = O_absent;
                  s = expr_end;
                  continue;
                case 'm':
                  my_getExpression( &imm_expr, s );
                  check_absolute_expr( ip, &imm_expr );
                  if ((unsigned long) imm_expr.X_add_number > 32 )
                    as_warn( _( "Improper nelm amount (%lu)" ),
                             (unsigned long) imm_expr.X_add_number );
                  INSERT_OPERAND( IMMSEGSTNELM, *ip, imm_expr.X_add_number - 1 );
                  imm_expr.X_op = O_absent;
                  s = expr_end;
                  continue;
                case 'd':
                  ok = reg_lookup( &s, RTYPE_NUM|RTYPE_VGR_REG, &regno );
                  if ( !ok )
                    as_bad( _( "Invalid vector register" ) );
                  INSERT_OPERAND( VRD, *ip, regno );
                  continue;
                case 's':
                  ok = reg_lookup( &s, RTYPE_NUM|RTYPE_VGR_REG, &regno );
                  if ( !ok )
                    as_bad( _( "Invalid vector register" ) );
                  INSERT_OPERAND( VRS, *ip, regno );
                  continue;
                case 't':
                  ok = reg_lookup( &s, RTYPE_NUM|RTYPE_VGR_REG, &regno );
                  if ( !ok )
                    as_bad( _( "Invalid vector register" ) );
                  INSERT_OPERAND( VRT, *ip, regno );
                  continue;
                case 'r':
                  ok = reg_lookup( &s, RTYPE_NUM|RTYPE_VGR_REG, &regno );
                  if ( !ok )
                    as_bad( _( "Invalid vector register" ) );
                  INSERT_OPERAND( VRR, *ip, regno );
                  continue;
                case 'D':
                  ok = reg_lookup( &s, RTYPE_NUM|RTYPE_VFP_REG, &regno );
                  if ( !ok )
                    as_bad( _( "Invalid vector register" ) );
                  INSERT_OPERAND( VFD, *ip, regno );
                  continue;
                case 'S':
                  ok = reg_lookup( &s, RTYPE_NUM|RTYPE_VFP_REG, &regno );
                  if ( !ok )
                    as_bad( _( "Invalid vector register" ) );
                  INSERT_OPERAND( VFS, *ip, regno );
                  continue;
                case 'T':
                  ok = reg_lookup( &s, RTYPE_NUM|RTYPE_VFP_REG, &regno );
                  if ( !ok )
                    as_bad( _( "Invalid vector register" ) );
                  INSERT_OPERAND( VFT, *ip, regno );
                  continue;
                case 'R':
                  ok = reg_lookup( &s, RTYPE_NUM|RTYPE_VFP_REG, &regno );
                  if ( !ok )
                    as_bad( _( "Invalid vector register" ) );
                  INSERT_OPERAND( VFR, *ip, regno );
                  continue;
                }
              break;

	    case '0': /* memory instruction with 0-offset (namely, AMOs) */
	      if (my_getSmallExpression (&offset_expr, offset_reloc, s) == 0
		  && (offset_expr.X_op != O_constant
		      || offset_expr.X_add_number != 0))
		break;

	      s = expr_end;
	      continue;

	    case ',':
	      ++argnum;
	      if (*s++ == *args)
		continue;
	      s--;
	      break;

	    case '(':
	      /* Handle optional base register.
		 Either the base register is omitted or
		 we must have a left paren.  */
	      /* This is dependent on the next operand specifier
		 is a base register specification.  */
	      gas_assert (args[1] == 'b' || args[1] == '5'
		      || args[1] == '-' || args[1] == '4');
	      if (*s == '\0')
		return;

	    case ')':		/* these must match exactly */
	    case '[':
	    case ']':
	      if (*s++ == *args)
		continue;
	      break;

	    case '<':		/* must be at least one digit */
	      /*
	       * According to the manual, if the shift amount is greater
	       * than 31 or less than 0, then the shift amount should be
	       * mod 32.  In reality the mips assembler issues an error.
	       * We issue a warning and mask out all but the low 5 bits.
	       */
	      my_getExpression (&imm_expr, s);
	      check_absolute_expr (ip, &imm_expr);
	      if ((unsigned long) imm_expr.X_add_number > 31)
		as_warn (_("Improper shift amount (%lu)"),
			 (unsigned long) imm_expr.X_add_number);
	      INSERT_OPERAND (SHAMTW, *ip, imm_expr.X_add_number);
	      imm_expr.X_op = O_absent;
	      s = expr_end;
	      continue;

	    case '>':		/* shift amount, 0-63 */
	      my_getExpression (&imm_expr, s);
	      check_absolute_expr (ip, &imm_expr);
	      INSERT_OPERAND (SHAMT, *ip, imm_expr.X_add_number);
	      imm_expr.X_op = O_absent;
	      s = expr_end;
	      continue;

	    case 'E':		/* Control register.  */
	      ok = reg_lookup (&s, RTYPE_NUM | RTYPE_CP0, &regno);
	      INSERT_OPERAND (RS, *ip, regno);
	      if (ok) 
		continue;
	      else
		break;

            case 'm':		/* rounding mode */
            {
              size_t i, found = ARRAY_SIZE(riscv_rm);
              for(i = 0; i < found; i++)
                if(riscv_rm[i] && !strncmp(s,riscv_rm[i],strlen(riscv_rm[i])))
                    found = i;

              if(found == ARRAY_SIZE(riscv_rm))
                as_bad("bad rounding mode: `%s'",s);

              INSERT_OPERAND(RM, *ip, found);
              s += strlen(riscv_rm[found]);
              continue;
            }

	    case 'b':		/* base register */
	    case 'd':		/* destination register */
	    case 's':		/* source register */
	    case 't':		/* target register */
	    case 'z':		/* must be zero register */
	    case 'U':           /* destination register (clo/clz).  */
	    case 'g':		/* coprocessor destination register */
	      ok = reg_lookup (&s, RTYPE_NUM | RTYPE_GP, &regno);
	      if (ok)
		{
		  c = *args;
		  if (*s == ' ')
		    ++s;
		  /* 'z' only matches $0.  */
		  if (c == 'z' && regno != 0)
		    break;

	/* Now that we have assembled one operand, we use the args string
	 * to figure out where it goes in the instruction.  */
		  switch (c)
		    {
		    case 's':
		    case 'b':
		    case 'E':
		      INSERT_OPERAND (RS, *ip, regno);
		      break;
		    case 'd':
		      INSERT_OPERAND (RD, *ip, regno);
		      break;
		    case 'g':
		      INSERT_OPERAND (FS, *ip, regno);
		      break;
		    case 'U':
		      INSERT_OPERAND (RD, *ip, regno);
		      INSERT_OPERAND (RT, *ip, regno);
		      break;
		    case 't':
		      INSERT_OPERAND (RT, *ip, regno);
		      break;
		    case 'x':
		      /* This case exists because on the r3000 trunc
			 expands into a macro which requires a gp
			 register.  On the r6000 or r4000 it is
			 assembled into a single instruction which
			 ignores the register.  Thus the insn version
			 is MIPS_ISA2 and uses 'x', and the macro
			 version is MIPS_ISA1 and uses 't'.  */
		      break;
		    case 'z':
		      /* This case is for the div instruction, which
			 acts differently if the destination argument
			 is $0.  This only matches $0, and is checked
			 outside the switch.  */
		      break;
		    case 'D':
		      /* Itbl operand; not yet implemented. FIXME ?? */
		      break;
		      /* What about all other operands like 'i', which
			 can be specified in the opcode table? */
		    }
		  continue;
		}
	      break;

	    case 'D':		/* floating point destination register */
	    case 'S':		/* floating point source register */
	    case 'T':		/* floating point target register */
	    case 'R':		/* floating point source register */
	      rtype = RTYPE_FPU;
	      if (reg_lookup (&s, rtype, &regno))
		{
		  c = *args;
		  if (*s == ' ')
		    ++s;
		  switch (c)
		    {
		    case 'D':
		      INSERT_OPERAND (FD, *ip, regno);
		      break;
		    case 'S':
		      INSERT_OPERAND (FS, *ip, regno);
		      break;
		    case 'T':
		      INSERT_OPERAND (FT, *ip, regno);
		      break;
		    case 'R':
		      INSERT_OPERAND (FR, *ip, regno);
		      break;
		    }
		  continue;
		}

	      break;

	    case 'I':
	      my_getExpression (&imm_expr, s);
	      if (imm_expr.X_op != O_big
		  && imm_expr.X_op != O_constant)
		insn_error = _("absolute expression required");
	      if (HAVE_32BIT_GPRS)
		normalize_constant_expr (&imm_expr);
	      s = expr_end;
	      continue;

	    case 'A':
	      my_getExpression (&offset_expr, s);
	      normalize_address_expr (&offset_expr);
	      *imm_reloc = BFD_RELOC_32;
	      s = expr_end;
	      continue;

	    case 'j':		/* sign-extended RISCV_IMM_BITS immediate */
	      *imm_reloc = BFD_RELOC_LO16;
	      if (my_getSmallExpression (&imm_expr, imm_reloc, s) == 0)
		{
		  int more;
		  offsetT minval, maxval;

		  more = (insn + 1 < &riscv_opcodes[NUMOPCODES]
			  && strcmp (insn->name, insn[1].name) == 0);

		  /* If the expression was written as an unsigned number,
		     only treat it as signed if there are no more
		     alternatives.  */
		  if (more
		      && *args == 'j'
		      && sizeof (imm_expr.X_add_number) <= 4
		      && imm_expr.X_op == O_constant
		      && imm_expr.X_add_number < 0
		      && imm_expr.X_unsigned
		      && HAVE_64BIT_GPRS)
		    break;

		  /* For compatibility with older assemblers, we accept
		     0x8000-0xffff as signed 16-bit numbers when only
		     signed numbers are allowed.  */
		  if (more)
		    minval = -(signed)RISCV_IMM_REACH/2, maxval = RISCV_IMM_REACH/2-1;
		  else
		    minval = -(signed)RISCV_IMM_REACH/2, maxval = RISCV_IMM_REACH-1;

		  if (imm_expr.X_op != O_constant
		      || imm_expr.X_add_number < minval
		      || imm_expr.X_add_number > maxval)
		    {
		      if (more)
			break;
		      if (imm_expr.X_op == O_constant
			  || imm_expr.X_op == O_big)
			as_bad (_("expression out of range"));
		    }
		}
	      s = expr_end;
	      continue;

	    case 'q':		/* 16 bit offset */
	    case 'o':		/* 16 bit offset */
	      /* Check whether there is only a single bracketed expression
		 left.  If so, it must be the base register and the
		 constant must be zero.  */
	      if (*s == '(' && strchr (s + 1, '(') == 0)
		{
		  offset_expr.X_op = O_constant;
		  offset_expr.X_add_number = 0;
		  continue;
		}

	      /* If this value won't fit into a 16 bit offset, then go
		 find a macro that will generate the 32 bit offset
		 code pattern.  */
	      if (my_getSmallExpression (&offset_expr, offset_reloc, s) == 0
		  && (offset_expr.X_op != O_constant
		      || offset_expr.X_add_number >= (signed)RISCV_IMM_REACH/2
		      || offset_expr.X_add_number < -(signed)RISCV_IMM_REACH/2))
		break;

	      s = expr_end;
	      continue;

	    case 'p':		/* pc relative offset */
	      *offset_reloc = BFD_RELOC_16_PCREL_S2;
	      my_getExpression (&offset_expr, s);
	      s = expr_end;
	      continue;

	    case 'u':		/* upper 20 bits */
	      if (my_getSmallExpression (&imm_expr, imm_reloc, s) == 0
		  && imm_expr.X_op == O_constant)
		{
		  if (imm_expr.X_add_number < 0
		      || imm_expr.X_add_number >= (signed)RISCV_BIGIMM_REACH)
		    as_bad (_("lui expression not in range 0..1048575"));
	      
		  *imm_reloc = BFD_RELOC_HI16;
		  imm_expr.X_add_number <<= RISCV_IMM_BITS;
		}
	      s = expr_end;
	      continue;

	    case 'a':		/* 26 bit address */
	      my_getExpression (&offset_expr, s);
	      s = expr_end;
	      *offset_reloc = BFD_RELOC_MIPS_JMP;
	      continue;

	    default:
	      as_bad (_("bad char = '%c'\n"), *args);
	      internalError ();
	    }
	  break;
	}
      /* Args don't match.  */
      if (insn + 1 < &riscv_opcodes[NUMOPCODES] &&
	  !strcmp (insn->name, insn[1].name))
	{
	  ++insn;
	  s = argsStart;
	  insn_error = _("illegal operands");
	  continue;
	}
      if (save_c)
	*(--argsStart) = save_c;
      insn_error = _("illegal operands");
      return;
    }
}

struct percent_op_match
{
  const char *str;
  bfd_reloc_code_real_type reloc;
};

static const struct percent_op_match mips_percent_op[] =
{
  {"%lo", BFD_RELOC_LO16},
#ifdef OBJ_ELF
  {"%call_hi", BFD_RELOC_MIPS_CALL_HI16},
  {"%call_lo", BFD_RELOC_MIPS_CALL_LO16},
  {"%call16", BFD_RELOC_MIPS_CALL16},
  {"%got_disp", BFD_RELOC_MIPS_GOT_DISP},
  {"%got_hi", BFD_RELOC_MIPS_GOT_HI16},
  {"%got_lo", BFD_RELOC_MIPS_GOT_LO16},
  {"%got", BFD_RELOC_MIPS_GOT16},
  {"%gp_rel", BFD_RELOC_GPREL16},
  {"%neg", BFD_RELOC_MIPS_SUB},
  {"%tlsgd", BFD_RELOC_MIPS_TLS_GD},
  {"%tlsgd_hi", BFD_RELOC_RISCV_TLS_GD_HI16},
  {"%tlsgd_lo", BFD_RELOC_RISCV_TLS_GD_LO16},
  {"%tlsldm", BFD_RELOC_MIPS_TLS_LDM},
  {"%tlsldm_hi", BFD_RELOC_RISCV_TLS_LDM_HI16},
  {"%tlsldm_lo", BFD_RELOC_RISCV_TLS_LDM_LO16},
  {"%dtprel_hi", BFD_RELOC_MIPS_TLS_DTPREL_HI16},
  {"%dtprel_lo", BFD_RELOC_MIPS_TLS_DTPREL_LO16},
  {"%tprel_hi", BFD_RELOC_MIPS_TLS_TPREL_HI16},
  {"%tprel_lo", BFD_RELOC_MIPS_TLS_TPREL_LO16},
  {"%gottprel", BFD_RELOC_MIPS_TLS_GOTTPREL},
  {"%gottp_hi", BFD_RELOC_RISCV_TLS_GOT_HI16},
  {"%gottp_lo", BFD_RELOC_RISCV_TLS_GOT_LO16},
#endif
  {"%hi", BFD_RELOC_HI16_S}
};

/* Return true if *STR points to a relocation operator.  When returning true,
   move *STR over the operator and store its relocation code in *RELOC.
   Leave both *STR and *RELOC alone when returning false.  */

static bfd_boolean
parse_relocation (char **str, bfd_reloc_code_real_type *reloc)
{
  const struct percent_op_match *percent_op;
  size_t limit, i;

  percent_op = mips_percent_op;
  limit = ARRAY_SIZE (mips_percent_op);

  for (i = 0; i < limit; i++)
    if (strncasecmp (*str, percent_op[i].str, strlen (percent_op[i].str)) == 0)
      {
	int len = strlen (percent_op[i].str);

	if (!ISSPACE ((*str)[len]) && (*str)[len] != '(')
	  continue;

	*str += strlen (percent_op[i].str);
	*reloc = percent_op[i].reloc;

	/* Check whether the output BFD supports this relocation.
	   If not, issue an error and fall back on something safe.  */
	if (!bfd_reloc_type_lookup (stdoutput, percent_op[i].reloc))
	  {
	    as_bad ("relocation %s isn't supported by the current ABI",
		    percent_op[i].str);
	    *reloc = BFD_RELOC_UNUSED;
	  }
	return TRUE;
      }
  return FALSE;
}


/* Parse string STR as a 16-bit relocatable operand.  Store the
   expression in *EP and the relocations in the array starting
   at RELOC.  Return the number of relocation operators used.

   On exit, EXPR_END points to the first character after the expression.  */

static size_t
my_getSmallExpression (expressionS *ep, bfd_reloc_code_real_type *reloc,
		       char *str)
{
  bfd_reloc_code_real_type reversed_reloc[3];
  size_t reloc_index, i;
  int crux_depth, str_depth;
  char *crux;

  /* Search for the start of the main expression, recoding relocations
     in REVERSED_RELOC.  End the loop with CRUX pointing to the start
     of the main expression and with CRUX_DEPTH containing the number
     of open brackets at that point.  */
  reloc_index = -1;
  str_depth = 0;
  do
    {
      reloc_index++;
      crux = str;
      crux_depth = str_depth;

      /* Skip over whitespace and brackets, keeping count of the number
	 of brackets.  */
      while (*str == ' ' || *str == '\t' || *str == '(')
	if (*str++ == '(')
	  str_depth++;
    }
  while (*str == '%'
	 && reloc_index < 3
	 && parse_relocation (&str, &reversed_reloc[reloc_index]));

  my_getExpression (ep, crux);
  str = expr_end;

  /* Match every open bracket.  */
  while (crux_depth > 0 && (*str == ')' || *str == ' ' || *str == '\t'))
    if (*str++ == ')')
      crux_depth--;

  if (crux_depth > 0)
    as_bad ("unclosed '('");

  expr_end = str;

  if (reloc_index != 0)
    {
      prev_reloc_op_frag = frag_now;
      for (i = 0; i < reloc_index; i++)
	reloc[i] = reversed_reloc[reloc_index - 1 - i];
    }

  return reloc_index;
}

static void
my_getExpression (expressionS *ep, char *str)
{
  char *save_in;

  save_in = input_line_pointer;
  input_line_pointer = str;
  expression (ep);
  expr_end = input_line_pointer;
  input_line_pointer = save_in;
}

char *
md_atof (int type, char *litP, int *sizeP)
{
  return ieee_md_atof (type, litP, sizeP, target_big_endian);
}

void
md_number_to_chars (char *buf, valueT val, int n)
{
  if (target_big_endian)
    number_to_chars_bigendian (buf, val, n);
  else
    number_to_chars_littleendian (buf, val, n);
}

const char *md_shortopts = "O::g::G:";

enum options
  {
    OPTION_MARCH = OPTION_MD_BASE,
    OPTION_MTUNE,
    OPTION_EB,
    OPTION_EL,
    OPTION_MRVC,
    OPTION_MNO_RVC,
    OPTION_GP32,
    OPTION_GP64,
    OPTION_MSYM32,
    OPTION_MNO_SYM32,
#ifdef OBJ_ELF
    OPTION_CALL_SHARED,
    OPTION_CALL_NONPIC,
    OPTION_NON_SHARED,
    OPTION_XGOT,
    OPTION_MABI,
    OPTION_PDR,
    OPTION_NO_PDR,
#endif /* OBJ_ELF */
    OPTION_END_OF_ENUM    
  };
  
struct option md_longopts[] =
{
  /* Options which specify architecture.  */
  {"march", required_argument, NULL, OPTION_MARCH},
  {"mtune", required_argument, NULL, OPTION_MTUNE},

  /* Miscellaneous options.  */
  {"EB", no_argument, NULL, OPTION_EB},
  {"EL", no_argument, NULL, OPTION_EL},
  {"mrvc", no_argument, NULL, OPTION_MRVC},
  {"mno-rvc", no_argument, NULL, OPTION_MNO_RVC},
  {"mgp32", no_argument, NULL, OPTION_GP32},
  {"mgp64", no_argument, NULL, OPTION_GP64},
  {"msym32", no_argument, NULL, OPTION_MSYM32},
  {"mno-sym32", no_argument, NULL, OPTION_MNO_SYM32},
  
  /* ELF-specific options.  */
#ifdef OBJ_ELF
  {"KPIC",        no_argument, NULL, OPTION_CALL_SHARED},
  {"call_shared", no_argument, NULL, OPTION_CALL_SHARED},
  {"call_nonpic", no_argument, NULL, OPTION_CALL_NONPIC},
  {"non_shared",  no_argument, NULL, OPTION_NON_SHARED},
  {"xgot",        no_argument, NULL, OPTION_XGOT},
  {"mabi", required_argument, NULL, OPTION_MABI},
  {"mpdr", no_argument, NULL, OPTION_PDR},
  {"mno-pdr", no_argument, NULL, OPTION_NO_PDR},
#endif /* OBJ_ELF */

  {NULL, no_argument, NULL, 0}
};
size_t md_longopts_size = sizeof (md_longopts);

/* Set STRING_PTR (either &mips_arch_string or &mips_tune_string) to
   NEW_VALUE.  Warn if another value was already specified.  Note:
   we have to defer parsing the -march and -mtune arguments in order
   to handle 'from-abi' correctly, since the ABI might be specified
   in a later argument.  */

static void
mips_set_option_string (const char **string_ptr, const char *new_value)
{
  if (*string_ptr != 0 && strcasecmp (*string_ptr, new_value) != 0)
    as_warn (_("A different %s was already specified, is now %s"),
	     string_ptr == &mips_arch_string ? "-march" : "-mtune",
	     new_value);

  *string_ptr = new_value;
}

int
md_parse_option (int c, char *arg)
{
  switch (c)
    {
    case OPTION_EB:
      target_big_endian = 1;
      break;

    case OPTION_EL:
      target_big_endian = 0;
      break;

    case 'g':
      if (arg == NULL)
	mips_debug = 2;
      else
	mips_debug = atoi (arg);
      break;

    case OPTION_MARCH:
      mips_set_option_string (&mips_arch_string, arg);
      break;

    case OPTION_MSYM32:
      mips_opts.sym32 = TRUE;
      break;

    case OPTION_MNO_SYM32:
      mips_opts.sym32 = FALSE;
      break;

#ifdef OBJ_ELF
      /* When generating ELF code, we permit -KPIC and -call_shared to
	 select SVR4_PIC, and -non_shared to select no PIC.  This is
	 intended to be compatible with Irix 5.  */
    case OPTION_CALL_SHARED:
      if (!IS_ELF)
	{
	  as_bad (_("-call_shared is supported only for ELF format"));
	  return 0;
	}
      mips_pic = SVR4_PIC;
      mips_abicalls = TRUE;
      break;

    case OPTION_CALL_NONPIC:
      if (!IS_ELF)
	{
	  as_bad (_("-call_nonpic is supported only for ELF format"));
	  return 0;
	}
      mips_pic = NO_PIC;
      mips_abicalls = TRUE;
      break;

    case OPTION_NON_SHARED:
      if (!IS_ELF)
	{
	  as_bad (_("-non_shared is supported only for ELF format"));
	  return 0;
	}
      mips_pic = NO_PIC;
      mips_abicalls = FALSE;
      break;

#endif /* OBJ_ELF */

    case OPTION_GP32:
      file_mips_gp32 = 1;
      break;

    case OPTION_GP64:
      file_mips_gp32 = 0;
      break;

    case OPTION_MRVC:
      file_mips_rvc = 1;
      break;

    case OPTION_MNO_RVC:
      file_mips_rvc = 0;
      break;

#ifdef OBJ_ELF
    case OPTION_MABI:
      if (!IS_ELF)
	{
	  as_bad (_("-mabi is supported for ELF format only"));
	  return 0;
	}
      if (strcmp (arg, "32") == 0)
	mips_abi = ABI_32;
      else if (strcmp (arg, "64") == 0)
	  mips_abi = ABI_64;
      else
	{
	  as_fatal (_("invalid abi -mabi=%s"), arg);
	  return 0;
	}
      break;
#endif /* OBJ_ELF */

#ifdef OBJ_ELF
    case OPTION_PDR:
      mips_flag_pdr = TRUE;
      break;

    case OPTION_NO_PDR:
      mips_flag_pdr = FALSE;
      break;
#endif /* OBJ_ELF */

    default:
      return 0;
    }

  return 1;
}

/* Set up globals to generate code for the ISA or processor
   described by INFO.  */

static void
mips_set_architecture (const struct mips_cpu_info *info)
{
  if (info != 0)
    {
      file_mips_arch = info->cpu;
      mips_opts.isa = info->isa;
    }
}


void
mips_after_parse_args (void)
{
  const struct mips_cpu_info *arch_info = 0;

  if (mips_abi == NO_ABI)
    mips_abi = ABI_64;

  /* The following code determines the architecture and register size.
     Similar code was added to GCC 3.3 (see override_options() in
     config/mips/mips.c).  The GAS and GCC code should be kept in sync
     as much as possible.  */

  if (mips_arch_string != 0)
    arch_info = mips_parse_cpu ("-march", mips_arch_string);

  if (arch_info == 0)
    arch_info = mips_parse_cpu ("default CPU", "from-abi");

  if (ABI_NEEDS_64BIT_REGS (mips_abi) && !ISA_HAS_64BIT_REGS (arch_info->isa))
    as_bad ("-march=%s is not compatible with the selected ABI",
	    arch_info->name);

  mips_set_architecture (arch_info);

  if (file_mips_gp32 >= 0)
    {
      /* The user specified the size of the integer registers.  Make sure
	 it agrees with the ABI and ISA.  */
      if (file_mips_gp32 == 0 && !ISA_HAS_64BIT_REGS (mips_opts.isa))
	as_bad (_("-mgp64 used with a 32-bit processor"));
      else if (file_mips_gp32 == 1 && ABI_NEEDS_64BIT_REGS (mips_abi))
	as_bad (_("-mgp32 used with a 64-bit ABI"));
      else if (file_mips_gp32 == 0 && ABI_NEEDS_32BIT_REGS (mips_abi))
	as_bad (_("-mgp64 used with a 32-bit ABI"));
    }
  else
    {
      /* Infer the integer register size from the ABI and processor.
	 Restrict ourselves to 32-bit registers if that's all the
	 processor has, or if the ABI cannot handle 64-bit registers.  */
      file_mips_gp32 = (ABI_NEEDS_32BIT_REGS (mips_abi)
			|| !ISA_HAS_64BIT_REGS (mips_opts.isa));
    }

  /* End of GCC-shared inference code.  */

  mips_opts.rvc = file_mips_rvc;
  mips_opts.gp32 = file_mips_gp32;
}

void
mips_init_after_args (void)
{
  /* initialize opcodes */
  bfd_riscv_num_opcodes = bfd_riscv_num_builtin_opcodes;
  riscv_opcodes = (struct riscv_opcode *) riscv_builtin_opcodes;
}

long
md_pcrel_from (fixS *fixP)
{
  return fixP->fx_where + fixP->fx_frag->fr_address;
}

/* We may have combined relocations without symbols in the N32/N64 ABI.
   We have to prevent gas from dropping them.  */

int
mips_force_relocation (fixS *fixp)
{
  if (generic_force_reloc (fixp))
    return 1;

  if (S_GET_SEGMENT (fixp->fx_addsy) == bfd_abs_section_ptr
      && (fixp->fx_r_type == BFD_RELOC_MIPS_SUB
	  || hi16_reloc_p (fixp->fx_r_type)
	  || lo16_reloc_p (fixp->fx_r_type)))
    return 1;

  return 0;
}

/* Apply a fixup to the object file.  */

void
md_apply_fix (fixS *fixP, valueT *valP, segT seg ATTRIBUTE_UNUSED)
{
  bfd_byte *buf;
  long insn;
  reloc_howto_type *howto;


  /* We ignore generic BFD relocations we don't know about.  */
  howto = bfd_reloc_type_lookup (stdoutput, fixP->fx_r_type);
  if (! howto)
    return;

  gas_assert (fixP->fx_size == 4
	  || fixP->fx_r_type == BFD_RELOC_64
	  || fixP->fx_r_type == BFD_RELOC_CTOR
	  || fixP->fx_r_type == BFD_RELOC_MIPS_SUB
	  || fixP->fx_r_type == BFD_RELOC_VTABLE_INHERIT
	  || fixP->fx_r_type == BFD_RELOC_VTABLE_ENTRY
	  || fixP->fx_r_type == BFD_RELOC_MIPS_TLS_DTPREL64);

  buf = (bfd_byte *) (fixP->fx_frag->fr_literal + fixP->fx_where);

  gas_assert (!fixP->fx_pcrel || (fixP->fx_r_type == BFD_RELOC_16_PCREL_S2 ||
                                 fixP->fx_r_type == BFD_RELOC_MIPS_JMP));

  /* Don't treat parts of a composite relocation as done.  There are two
     reasons for this:

     (1) The second and third parts will be against 0 (RSS_UNDEF) but
	 should nevertheless be emitted if the first part is.

     (2) In normal usage, composite relocations are never assembly-time
	 constants.  The easiest way of dealing with the pathological
	 exceptions is to generate a relocation against STN_UNDEF and
	 leave everything up to the linker.  */
  if (fixP->fx_addsy == NULL && !fixP->fx_pcrel && fixP->fx_tcbit == 0)
    fixP->fx_done = 1;

  if (target_big_endian)
    insn = (buf[0] << 24) | (buf[1] << 16) | (buf[2] << 8) | buf[3];
  else
    insn = (buf[3] << 24) | (buf[2] << 16) | (buf[1] << 8) | buf[0];

  switch (fixP->fx_r_type)
    {
    case BFD_RELOC_MIPS_TLS_GD:
    case BFD_RELOC_RISCV_TLS_GD_HI16:
    case BFD_RELOC_RISCV_TLS_GD_LO16:
    case BFD_RELOC_MIPS_TLS_LDM:
    case BFD_RELOC_RISCV_TLS_LDM_HI16:
    case BFD_RELOC_RISCV_TLS_LDM_LO16:
    case BFD_RELOC_MIPS_TLS_DTPREL32:
    case BFD_RELOC_MIPS_TLS_DTPREL64:
    case BFD_RELOC_MIPS_TLS_DTPREL_HI16:
    case BFD_RELOC_MIPS_TLS_DTPREL_LO16:
    case BFD_RELOC_MIPS_TLS_GOTTPREL:
    case BFD_RELOC_RISCV_TLS_GOT_HI16:
    case BFD_RELOC_RISCV_TLS_GOT_LO16:
    case BFD_RELOC_MIPS_TLS_TPREL_HI16:
    case BFD_RELOC_MIPS_TLS_TPREL_LO16:
      S_SET_THREAD_LOCAL (fixP->fx_addsy);
      /* fall through */

    case BFD_RELOC_MIPS_GOT_DISP:
    case BFD_RELOC_MIPS_SUB:
    case BFD_RELOC_MIPS_INSERT_A:
    case BFD_RELOC_MIPS_INSERT_B:
    case BFD_RELOC_MIPS_DELETE:
    case BFD_RELOC_MIPS_SCN_DISP:
    case BFD_RELOC_MIPS_REL16:
    case BFD_RELOC_MIPS_RELGOT:
    case BFD_RELOC_MIPS_JALR:
    case BFD_RELOC_HI16:
    case BFD_RELOC_HI16_S:
    case BFD_RELOC_GPREL16:
    case BFD_RELOC_MIPS_LITERAL:
    case BFD_RELOC_MIPS_CALL16:
    case BFD_RELOC_MIPS_GOT16:
    case BFD_RELOC_GPREL32:
    case BFD_RELOC_MIPS_GOT_HI16:
    case BFD_RELOC_MIPS_GOT_LO16:
    case BFD_RELOC_MIPS_CALL_HI16:
    case BFD_RELOC_MIPS_CALL_LO16:
    case BFD_RELOC_MIPS16_GPREL:
    case BFD_RELOC_MIPS16_GOT16:
    case BFD_RELOC_MIPS16_CALL16:
    case BFD_RELOC_MIPS16_HI16:
    case BFD_RELOC_MIPS16_HI16_S:
    case BFD_RELOC_MIPS16_JMP:
      /* Nothing needed to do.  The value comes from the reloc entry.  */
      break;

    case BFD_RELOC_64:
      /* This is handled like BFD_RELOC_32, but we output a sign
         extended value if we are only 32 bits.  */
      if (fixP->fx_done)
	{
	  if (8 <= sizeof (valueT))
	    md_number_to_chars ((char *) buf, *valP, 8);
	  else
	    {
	      valueT hiv;

	      if ((*valP & 0x80000000) != 0)
		hiv = 0xffffffff;
	      else
		hiv = 0;
	      md_number_to_chars ((char *)(buf + (target_big_endian ? 4 : 0)),
				  *valP, 4);
	      md_number_to_chars ((char *)(buf + (target_big_endian ? 0 : 4)),
				  hiv, 4);
	    }
	}
      break;

    case BFD_RELOC_RVA:
    case BFD_RELOC_32:
      /* If we are deleting this reloc entry, we must fill in the
	 value now.  This can happen if we have a .word which is not
	 resolved when it appears but is later defined.  */
      if (fixP->fx_done)
	md_number_to_chars ((char *) buf, *valP, fixP->fx_size);
      break;

    case BFD_RELOC_LO16:
    case BFD_RELOC_MIPS16_LO16:
      if (!fixP->fx_done)
	break;

      if (*valP + RISCV_IMM_REACH/2 > RISCV_IMM_REACH-1)
        as_bad_where (fixP->fx_file, fixP->fx_line,
		      _("relocation overflow"));

      if (OPCODE_IS_STORE(insn)) /* Stores have a split immediate field. */
	{
	  valueT value = *valP & (RISCV_IMM_REACH-1);
	  value = ((value >> RISCV_IMMLO_BITS) << OP_SH_IMMHI) |
	          ((value & ((1<<RISCV_IMMLO_BITS)-1)) << OP_SH_IMMLO);
	  insn |= value;
	}
      else
	insn |= (*valP & ((1<<RISCV_IMM_BITS)-1)) << OP_SH_IMMEDIATE;

      md_number_to_chars ((char *) buf, insn, 4);
      break;

    case BFD_RELOC_MIPS_JMP:
      if ((*valP & (RISCV_JUMP_ALIGN-1)) != 0)
	as_bad_where (fixP->fx_file, fixP->fx_line,
		      _("Branch to misaligned address (%lx)"), (long) *valP);

      /* We need to save the bits in the instruction since fixup_segment()
	 might be deleting the relocation entry (i.e., a branch within
	 the current segment).  */
      if (! fixP->fx_done)
	break;

      /* Update old instruction data.  */

      if (*valP + RISCV_JUMP_REACH/2 <= RISCV_JUMP_REACH-1)
	{
	  insn |= ((*valP >> RISCV_JUMP_ALIGN_BITS) & ((1<<RISCV_JUMP_BITS)-1)) << OP_SH_TARGET;
	  md_number_to_chars ((char *) buf, insn, 4);
	}
      else
	{
	  /* If we got here, we have branch-relaxation disabled,
	     and there's nothing we can do to fix this instruction
	     without turning it into a longer sequence.  */
	  as_bad_where (fixP->fx_file, fixP->fx_line,
			_("Jump out of range"));
	}
      break;

    case BFD_RELOC_16_PCREL_S2:
      if ((*valP & (RISCV_BRANCH_ALIGN-1)) != 0)
	as_bad_where (fixP->fx_file, fixP->fx_line,
		      _("Branch to misaligned address (%lx)"), (long) *valP);

      /* We need to save the bits in the instruction since fixup_segment()
	 might be deleting the relocation entry (i.e., a branch within
	 the current segment).  */
      if (! fixP->fx_done)
	break;

      /* Update old instruction data.  */
      if (*valP + RISCV_BRANCH_REACH/2 <= RISCV_BRANCH_REACH-1)
	{
	  unsigned delta = ((unsigned)*valP >> RISCV_BRANCH_ALIGN_BITS) & ((1<<RISCV_BRANCH_BITS)-1);;
	  insn |= ((delta & ((1<<RISCV_IMMLO_BITS)-1)) << OP_SH_IMMLO) | (((delta >> RISCV_IMMLO_BITS) & ((1<<RISCV_IMMHI_BITS)-1)) << OP_SH_IMMHI);
	  md_number_to_chars ((char *) buf, insn, 4);
	}
      else
	{
	  /* If we got here, we have branch-relaxation disabled,
	     and there's nothing we can do to fix this instruction
	     without turning it into a longer sequence.  */
	  as_bad_where (fixP->fx_file, fixP->fx_line,
			_("Branch out of range"));
	}
      break;

    case BFD_RELOC_VTABLE_INHERIT:
      fixP->fx_done = 0;
      if (fixP->fx_addsy
          && !S_IS_DEFINED (fixP->fx_addsy)
          && !S_IS_WEAK (fixP->fx_addsy))
        S_SET_WEAK (fixP->fx_addsy);
      break;

    case BFD_RELOC_VTABLE_ENTRY:
      fixP->fx_done = 0;
      break;

    default:
      internalError ();
    }

  /* Remember value for tc_gen_reloc.  */
  fixP->fx_addnumber = *valP;
}

static symbolS *
get_symbol (void)
{
  int c;
  char *name;
  symbolS *p;

  name = input_line_pointer;
  c = get_symbol_end ();
  p = (symbolS *) symbol_find_or_make (name);
  *input_line_pointer = c;
  return p;
}

/* Align the current frag to a given power of two.  If a particular
   fill byte should be used, FILL points to an integer that contains
   that byte, otherwise FILL is null.

   The MIPS assembler also automatically adjusts any preceding
   label.  */

static void
mips_align (int to, int *fill, symbolS *label)
{
  mips_clear_insn_labels ();
  if (fill == NULL && subseg_text_p (now_seg))
    frag_align_code (to, 0);
  else
    frag_align (to, fill ? *fill : 0, 0);
  record_alignment (now_seg, to);
  if (label != NULL)
    {
      gas_assert (S_GET_SEGMENT (label) == now_seg);
      symbol_set_frag (label, frag_now);
      S_SET_VALUE (label, (valueT) frag_now_fix ());
    }
}

/* Align to a given power of two.  .align 0 turns off the automatic
   alignment used by the data creating pseudo-ops.  */

static void
s_align (int x ATTRIBUTE_UNUSED)
{
  int temp, fill_value, *fill_ptr;
  long max_alignment = 28;

  /* o Note that the assembler pulls down any immediately preceding label
       to the aligned address.
     o It's not documented but auto alignment is reinstated by
       a .align pseudo instruction.
     o Note also that after auto alignment is turned off the mips assembler
       issues an error on attempt to assemble an improperly aligned data item.
       We don't.  */

  temp = get_absolute_expression ();
  if (temp > max_alignment)
    as_bad (_("Alignment too large: %d. assumed."), temp = max_alignment);
  else if (temp < 0)
    {
      as_warn (_("Alignment negative: 0 assumed."));
      temp = 0;
    }
  if (*input_line_pointer == ',')
    {
      ++input_line_pointer;
      fill_value = get_absolute_expression ();
      fill_ptr = &fill_value;
    }
  else
    fill_ptr = 0;
  if (temp)
    {
      segment_info_type *si = seg_info (now_seg);
      struct insn_label_list *l = si->label_list;
      /* Auto alignment should be switched on by next section change.  */
      auto_align = 1;
      mips_align (temp, fill_ptr, l != NULL ? l->label : NULL);
    }
  else
    {
      auto_align = 0;
    }

  demand_empty_rest_of_line ();
}

static void
s_change_sec (int sec)
{
  segT seg;

#ifdef OBJ_ELF
  /* The ELF backend needs to know that we are changing sections, so
     that .previous works correctly.  We could do something like check
     for an obj_section_change_hook macro, but that might be confusing
     as it would not be appropriate to use it in the section changing
     functions in read.c, since obj-elf.c intercepts those.  FIXME:
     This should be cleaner, somehow.  */
  if (IS_ELF)
    obj_elf_section_change_hook ();
#endif

  mips_clear_insn_labels ();

  switch (sec)
    {
    case 't':
      s_text (0);
      break;
    case 'd':
      s_data (0);
      break;
    case 'b':
      subseg_set (bss_section, (subsegT) get_absolute_expression ());
      demand_empty_rest_of_line ();
      break;

    case 'r':
      seg = subseg_new (".rodata", (subsegT) get_absolute_expression ());
      if (IS_ELF)
	{
	  bfd_set_section_flags (stdoutput, seg, (SEC_ALLOC | SEC_LOAD
						  | SEC_READONLY | SEC_RELOC
						  | SEC_DATA));
	  if (strncmp (TARGET_OS, "elf", 3) != 0)
	    record_alignment (seg, 4);
	}
      demand_empty_rest_of_line ();
      break;

    case 's':
      seg = subseg_new (".sdata", (subsegT) get_absolute_expression ());
      if (IS_ELF)
	{
	  bfd_set_section_flags (stdoutput, seg,
				 SEC_ALLOC | SEC_LOAD | SEC_RELOC | SEC_DATA);
	  if (strncmp (TARGET_OS, "elf", 3) != 0)
	    record_alignment (seg, 4);
	}
      demand_empty_rest_of_line ();
      break;
    }

  auto_align = 1;
}

void
s_change_section (int ignore ATTRIBUTE_UNUSED)
{
#ifdef OBJ_ELF
  char *section_name;
  char c;
  char next_c = 0;
  int section_type;
  int section_flag;
  int section_entry_size;

  if (!IS_ELF)
    return;

  section_name = input_line_pointer;
  c = get_symbol_end ();
  if (c)
    next_c = *(input_line_pointer + 1);

  /* Do we have .section Name<,"flags">?  */
  if (c != ',' || (c == ',' && next_c == '"'))
    {
      /* just after name is now '\0'.  */
      *input_line_pointer = c;
      input_line_pointer = section_name;
      obj_elf_section (ignore);
      return;
    }
  input_line_pointer++;

  /* Do we have .section Name<,type><,flag><,entry_size><,alignment>  */
  if (c == ',')
    section_type = get_absolute_expression ();
  else
    section_type = 0;
  if (*input_line_pointer++ == ',')
    section_flag = get_absolute_expression ();
  else
    section_flag = 0;
  if (*input_line_pointer++ == ',')
    section_entry_size = get_absolute_expression ();
  else
    section_entry_size = 0;

  section_name = xstrdup (section_name);

  /* When using the generic form of .section (as implemented by obj-elf.c),
     there's no way to set the section type to SHT_MIPS_DWARF.  Users have
     traditionally had to fall back on the more common @progbits instead.

     There's nothing really harmful in this, since bfd will correct
     SHT_PROGBITS to SHT_MIPS_DWARF before writing out the file.  But it
     means that, for backwards compatibility, the special_section entries
     for dwarf sections must use SHT_PROGBITS rather than SHT_MIPS_DWARF.

     Even so, we shouldn't force users of the MIPS .section syntax to
     incorrectly label the sections as SHT_PROGBITS.  The best compromise
     seems to be to map SHT_MIPS_DWARF to SHT_PROGBITS before calling the
     generic type-checking code.  */
  if (section_type == SHT_MIPS_DWARF)
    section_type = SHT_PROGBITS;

  obj_elf_change_section (section_name, section_type, section_flag,
			  section_entry_size, 0, 0, 0);

  if (now_seg->name != section_name)
    free (section_name);
#endif /* OBJ_ELF */
}

void
mips_enable_auto_align (void)
{
  auto_align = 1;
}

static void
s_cons (int log_size)
{
  segment_info_type *si = seg_info (now_seg);
  struct insn_label_list *l = si->label_list;
  symbolS *label;

  label = l != NULL ? l->label : NULL;
  mips_clear_insn_labels ();
  if (log_size > 0 && auto_align)
    mips_align (log_size, 0, label);
  mips_clear_insn_labels ();
  cons (1 << log_size);
}

static void
s_float_cons (int type)
{
  segment_info_type *si = seg_info (now_seg);
  struct insn_label_list *l = si->label_list;
  symbolS *label;

  label = l != NULL ? l->label : NULL;

  mips_clear_insn_labels ();

  if (auto_align)
    {
      if (type == 'd')
	mips_align (3, 0, label);
      else
	mips_align (2, 0, label);
    }

  mips_clear_insn_labels ();

  float_cons (type);
}

/* Handle .globl.  We need to override it because on Irix 5 you are
   permitted to say
       .globl foo .text
   where foo is an undefined symbol, to mean that foo should be
   considered to be the address of a function.  */

static void
s_mips_globl (int x ATTRIBUTE_UNUSED)
{
  char *name;
  int c;
  symbolS *symbolP;
  flagword flag;

  do
    {
      name = input_line_pointer;
      c = get_symbol_end ();
      symbolP = symbol_find_or_make (name);
      S_SET_EXTERNAL (symbolP);

      *input_line_pointer = c;
      SKIP_WHITESPACE ();

      /* On Irix 5, every global symbol that is not explicitly labelled as
         being a function is apparently labelled as being an object.  */
      flag = BSF_OBJECT;

      if (!is_end_of_line[(unsigned char) *input_line_pointer]
	  && (*input_line_pointer != ','))
	{
	  char *secname;
	  asection *sec;

	  secname = input_line_pointer;
	  c = get_symbol_end ();
	  sec = bfd_get_section_by_name (stdoutput, secname);
	  if (sec == NULL)
	    as_bad (_("%s: no such section"), secname);
	  *input_line_pointer = c;

	  if (sec != NULL && (sec->flags & SEC_CODE) != 0)
	    flag = BSF_FUNCTION;
	}

      symbol_get_bfdsym (symbolP)->flags |= flag;

      c = *input_line_pointer;
      if (c == ',')
	{
	  input_line_pointer++;
	  SKIP_WHITESPACE ();
	  if (is_end_of_line[(unsigned char) *input_line_pointer])
	    c = '\n';
	}
    }
  while (c == ',');

  demand_empty_rest_of_line ();
}

static void
s_option (int x ATTRIBUTE_UNUSED)
{
  char *opt;
  char c;

  opt = input_line_pointer;
  c = get_symbol_end ();

  if (strncmp (opt, "pic", 3) == 0)
    {
      int i;

      i = atoi (opt + 3);
      if (i == 0)
	mips_pic = NO_PIC;
      else if (i == 2)
	{
	mips_pic = SVR4_PIC;
	  mips_abicalls = TRUE;
	}
      else
	as_bad (_(".option pic%d not supported"), i);
    }
  else
    as_warn (_("Unrecognized option \"%s\""), opt);

  *input_line_pointer = c;
  demand_empty_rest_of_line ();
}

/* This structure is used to hold a stack of .set values.  */

struct mips_option_stack
{
  struct mips_option_stack *next;
  struct mips_set_options options;
};

static struct mips_option_stack *mips_opts_stack;

/* Handle the .set pseudo-op.  */

static void
s_mipsset (int x ATTRIBUTE_UNUSED)
{
  char *name = input_line_pointer, ch;

  while (!is_end_of_line[(unsigned char) *input_line_pointer])
    ++input_line_pointer;
  ch = *input_line_pointer;
  *input_line_pointer = '\0';

  if (strcmp (name, "gp=default") == 0)
    mips_opts.gp32 = file_mips_gp32;
  else if (strcmp (name, "gp=32") == 0)
    mips_opts.gp32 = 1;
  else if (strcmp (name, "gp=64") == 0)
    {
      if (!ISA_HAS_64BIT_REGS (mips_opts.isa))
	as_warn ("%s isa does not support 64-bit registers",
		 mips_cpu_info_from_isa (mips_opts.isa)->name);
      mips_opts.gp32 = 0;
    }
  else if (strcmp (name, "rvc") == 0)
    mips_opts.rvc = 1;
  else if (strcmp (name, "norvc") == 0)
    mips_opts.rvc = 0;
  else if (strcmp (name, "push") == 0)
    {
      struct mips_option_stack *s;

      s = (struct mips_option_stack *) xmalloc (sizeof *s);
      s->next = mips_opts_stack;
      s->options = mips_opts;
      mips_opts_stack = s;
    }
  else if (strcmp (name, "pop") == 0)
    {
      struct mips_option_stack *s;

      s = mips_opts_stack;
      if (s == NULL)
	as_bad (_(".set pop with no .set push"));
      else
	{
	  mips_opts = s->options;
	  mips_opts_stack = s->next;
	  free (s);
	}
    }
  else if (strcmp (name, "sym32") == 0)
    mips_opts.sym32 = TRUE;
  else if (strcmp (name, "nosym32") == 0)
    mips_opts.sym32 = FALSE;
  else if (strchr (name, ','))
    {
      /* Generic ".set" directive; use the generic handler.  */
      *input_line_pointer = ch;
      input_line_pointer = name;
      s_set (0);
      return;
    }
  else
    {
      as_warn (_("Tried to set unrecognized symbol: %s\n"), name);
    }
  *input_line_pointer = ch;
  demand_empty_rest_of_line ();
}

/* Handle the .abicalls pseudo-op.  I believe this is equivalent to
   .option pic2.  It means to generate SVR4 PIC calls.  */

static void
s_abicalls (int ignore ATTRIBUTE_UNUSED)
{
  mips_pic = SVR4_PIC;
  mips_abicalls = TRUE;

  demand_empty_rest_of_line ();
}

/* Handle the .dtprelword and .dtpreldword pseudo-ops.  They generate
   a 32-bit or 64-bit DTP-relative relocation (BYTES says which) for
   use in DWARF debug information.  */

static void
s_dtprel_internal (size_t bytes)
{
  expressionS ex;
  char *p;

  expression (&ex);

  if (ex.X_op != O_symbol)
    {
      as_bad (_("Unsupported use of %s"), (bytes == 8
					   ? ".dtpreldword"
					   : ".dtprelword"));
      ignore_rest_of_line ();
    }

  p = frag_more (bytes);
  md_number_to_chars (p, 0, bytes);
  fix_new_exp (frag_now, p - frag_now->fr_literal, bytes, &ex, FALSE,
	       (bytes == 8
		? BFD_RELOC_MIPS_TLS_DTPREL64
		: BFD_RELOC_MIPS_TLS_DTPREL32));

  demand_empty_rest_of_line ();
}

/* Handle .dtprelword.  */

static void
s_dtprelword (int ignore ATTRIBUTE_UNUSED)
{
  s_dtprel_internal (4);
}

/* Handle .dtpreldword.  */

static void
s_dtpreldword (int ignore ATTRIBUTE_UNUSED)
{
  s_dtprel_internal (8);
}

/* Handle the .gpword pseudo-op.  This is used when generating PIC
   code.  It generates a 32 bit GP relative reloc.  */

static void
s_gpword (int ignore ATTRIBUTE_UNUSED)
{
  segment_info_type *si;
  struct insn_label_list *l;
  symbolS *label;
  expressionS ex;
  char *p;

  /* When not generating PIC code, this is treated as .word.  */
  if (mips_pic != SVR4_PIC)
    {
      s_cons (2);
      return;
    }

  si = seg_info (now_seg);
  l = si->label_list;
  label = l != NULL ? l->label : NULL;
  mips_clear_insn_labels ();
  if (auto_align)
    mips_align (2, 0, label);
  mips_clear_insn_labels ();

  expression (&ex);

  if (ex.X_op != O_symbol || ex.X_add_number != 0)
    {
      as_bad (_("Unsupported use of .gpword"));
      ignore_rest_of_line ();
    }

  p = frag_more (4);
  md_number_to_chars (p, 0, 4);
  fix_new_exp (frag_now, p - frag_now->fr_literal, 4, &ex, FALSE,
	       BFD_RELOC_GPREL32);

  demand_empty_rest_of_line ();
}

static void
s_gpdword (int ignore ATTRIBUTE_UNUSED)
{
  segment_info_type *si;
  struct insn_label_list *l;
  symbolS *label;
  expressionS ex;
  char *p;

  /* When not generating PIC code, this is treated as .dword.  */
  if (mips_pic != SVR4_PIC)
    {
      s_cons (3);
      return;
    }

  si = seg_info (now_seg);
  l = si->label_list;
  label = l != NULL ? l->label : NULL;
  mips_clear_insn_labels ();
  if (auto_align)
    mips_align (3, 0, label);
  mips_clear_insn_labels ();

  expression (&ex);

  if (ex.X_op != O_symbol || ex.X_add_number != 0)
    {
      as_bad (_("Unsupported use of .gpdword"));
      ignore_rest_of_line ();
    }

  p = frag_more (8);
  md_number_to_chars (p, 0, 8);
  fix_new_exp (frag_now, p - frag_now->fr_literal, 4, &ex, FALSE,
	       BFD_RELOC_GPREL32)->fx_tcbit = 1;

  /* GPREL32 composed with 64 gives a 64-bit GP offset.  */
  fix_new (frag_now, p - frag_now->fr_literal, 8, NULL, 0,
	   FALSE, BFD_RELOC_64)->fx_tcbit = 1;

  demand_empty_rest_of_line ();
}

/* Handle the .insn pseudo-op.  This marks instruction labels in
   mips16 mode.  This permits the linker to handle them specially,
   such as generating jalx instructions when needed.  We also make
   them odd for the duration of the assembly, in order to generate the
   right sort of code.  We will make them even in the adjust_symtab
   routine, while leaving them marked.  This is convenient for the
   debugger and the disassembler.  The linker knows to make them odd
   again.  */

static void
s_insn (int ignore ATTRIBUTE_UNUSED)
{
  demand_empty_rest_of_line ();
}

/* Handle a .stabn directive.  We need these in order to mark a label
   as being a mips16 text label correctly.  Sometimes the compiler
   will emit a label, followed by a .stabn, and then switch sections.
   If the label and .stabn are in mips16 mode, then the label is
   really a mips16 text label.  */

static void
s_mips_stab (int type)
{
  s_stab (type);
}

/* Handle the .weakext pseudo-op as defined in Kane and Heinrich.  */

static void
s_mips_weakext (int ignore ATTRIBUTE_UNUSED)
{
  char *name;
  int c;
  symbolS *symbolP;
  expressionS exp;

  name = input_line_pointer;
  c = get_symbol_end ();
  symbolP = symbol_find_or_make (name);
  S_SET_WEAK (symbolP);
  *input_line_pointer = c;

  SKIP_WHITESPACE ();

  if (! is_end_of_line[(unsigned char) *input_line_pointer])
    {
      if (S_IS_DEFINED (symbolP))
	{
	  as_bad ("ignoring attempt to redefine symbol %s",
		  S_GET_NAME (symbolP));
	  ignore_rest_of_line ();
	  return;
	}

      if (*input_line_pointer == ',')
	{
	  ++input_line_pointer;
	  SKIP_WHITESPACE ();
	}

      expression (&exp);
      if (exp.X_op != O_symbol)
	{
	  as_bad ("bad .weakext directive");
	  ignore_rest_of_line ();
	  return;
	}
      symbol_set_value_expression (symbolP, &exp);
    }

  demand_empty_rest_of_line ();
}

/* Parse a register string into a number. */

static int
tc_get_register (void)
{
  unsigned int reg;

  SKIP_WHITESPACE ();
  if (! reg_lookup (&input_line_pointer, RWARN | RTYPE_NUM | RTYPE_GP, &reg))
    reg = 0;
  return reg;
}

valueT
md_section_align (asection *seg, valueT addr)
{
  int align = bfd_get_section_alignment (stdoutput, seg);

  if (IS_ELF)
    {
      /* We don't need to align ELF sections to the full alignment.
	 However, Irix 5 may prefer that we align them at least to a 16
	 byte boundary.  We don't bother to align the sections if we
	 are targeted for an embedded system.  */
      if (strncmp (TARGET_OS, "elf", 3) == 0)
        return addr;
      if (align > 4)
        align = 4;
    }

  return ((addr + (1 << align) - 1) & (-1 << align));
}

/* Compute the length of a branch sequence, and adjust the
   RELAX_BRANCH_TOOFAR bit accordingly.  If FRAGP is NULL, the
   worst-case length is computed, with UPDATE being used to indicate
   whether an unconditional (-1), branch-likely (+1) or regular (0)
   branch is to be computed.  */
static int
relaxed_branch_length (fragS *fragp, asection *sec, int update)
{
  bfd_boolean toofar;

  if (fragp
      && S_IS_DEFINED (fragp->fr_symbol)
      && sec == S_GET_SEGMENT (fragp->fr_symbol))
    {
      offsetT val = S_GET_VALUE (fragp->fr_symbol) + fragp->fr_offset;
      val -= fragp->fr_address + fragp->fr_fix;

      if(RELAX_BRANCH_UNCOND (fragp->fr_subtype))
        toofar = (bfd_vma)(val + RVC_JUMP_REACH/2) >= RVC_JUMP_REACH;
      else
        toofar = (bfd_vma)(val + RVC_BRANCH_REACH/2) >= RVC_BRANCH_REACH;
    }
  else
    /* If the symbol is not defined or it's in a different segment,
       assume it's too far. */
    toofar = TRUE;

  if (fragp && update && toofar != RELAX_BRANCH_TOOFAR (fragp->fr_subtype))
    fragp->fr_subtype
      = RELAX_BRANCH_ENCODE (RELAX_BRANCH_UNCOND (fragp->fr_subtype), toofar);

  return toofar ? 4 : 2;
}

int
md_estimate_size_before_relax (fragS *fragp, asection *segtype)
{
  return (fragp->fr_var = relaxed_branch_length (fragp, segtype, FALSE));
}

/* This is called to see whether a reloc against a defined symbol
   should be converted into a reloc against a section.  */

int
mips_fix_adjustable (fixS *fixp)
{
  if (fixp->fx_r_type == BFD_RELOC_VTABLE_INHERIT
      || fixp->fx_r_type == BFD_RELOC_VTABLE_ENTRY)
    return 0;

  return 1;
}

/* Translate internal representation of relocation info to BFD target
   format.  */

arelent **
tc_gen_reloc (asection *section ATTRIBUTE_UNUSED, fixS *fixp)
{
  static arelent *retval[4];
  arelent *reloc;
  bfd_reloc_code_real_type code;

  memset (retval, 0, sizeof(retval));
  reloc = retval[0] = (arelent *) xcalloc (1, sizeof (arelent));
  reloc->sym_ptr_ptr = (asymbol **) xmalloc (sizeof (asymbol *));
  *reloc->sym_ptr_ptr = symbol_get_bfdsym (fixp->fx_addsy);
  reloc->address = fixp->fx_frag->fr_address + fixp->fx_where;

  if (fixp->fx_pcrel)
    {
      gas_assert (fixp->fx_r_type == BFD_RELOC_16_PCREL_S2 ||
                  fixp->fx_r_type == BFD_RELOC_MIPS_JMP);

      /* At this point, fx_addnumber is "symbol offset - pcrel address".
	 Relocations want only the symbol offset.  */
      reloc->addend = fixp->fx_addnumber + reloc->address;
      if (!IS_ELF)
	{
	  /* A gruesome hack which is a result of the gruesome gas
	     reloc handling.  What's worse, for COFF (as opposed to
	     ECOFF), we might need yet another copy of reloc->address.
	     See bfd_install_relocation.  */
	  reloc->addend += reloc->address;
	}
    }
  else
    reloc->addend = fixp->fx_addnumber;

  code = fixp->fx_r_type;

  reloc->howto = bfd_reloc_type_lookup (stdoutput, code);
  if (reloc->howto == NULL)
    {
      as_bad_where (fixp->fx_file, fixp->fx_line,
		    _("Can not represent %s relocation in this object file format"),
		    bfd_get_reloc_code_name (code));
      retval[0] = NULL;
    }

  return retval;
}

int
mips_relax_frag (asection *sec, fragS *fragp, long stretch ATTRIBUTE_UNUSED)
{
  if (RELAX_BRANCH_P (fragp->fr_subtype))
    {
      offsetT old_var = fragp->fr_var;
      fragp->fr_var = relaxed_branch_length (fragp, sec, TRUE);
      return fragp->fr_var - old_var;
    }

  return 0;
}

/* Convert a machine dependent frag.  */

static void
md_convert_frag_branch (bfd *abfd ATTRIBUTE_UNUSED, segT asec ATTRIBUTE_UNUSED,
                 fragS *fragp)
{
  bfd_byte *buf;
  unsigned long insn;
  expressionS exp;
  fixS *fixp;
  bfd_reloc_code_real_type reloc_type = BFD_RELOC_16_PCREL_S2;

  buf = (bfd_byte *)fragp->fr_literal + fragp->fr_fix;

  if (target_big_endian)
    insn = bfd_getb16 (buf);
  else
    insn = bfd_getl16 (buf);

  if (!RELAX_BRANCH_TOOFAR (fragp->fr_subtype))
    {
      gas_assert(S_IS_DEFINED(fragp->fr_symbol));
      gas_assert(fragp->fr_var == 2);

      offsetT target = S_GET_VALUE (fragp->fr_symbol) + fragp->fr_offset;
      target -= fragp->fr_address + fragp->fr_fix;
      target >>= RVC_JUMP_ALIGN_BITS;
      gas_assert(RVC_JUMP_ALIGN_BITS == RVC_BRANCH_ALIGN_BITS);
     
      if((insn & MASK_C_J) == MATCH_C_J)
        insn |= ((target & OP_MASK_CIMM10) << OP_SH_CIMM10);
      else if((insn & MASK_C_BEQ) == MATCH_C_BEQ ||
              (insn & MASK_C_BNE) == MATCH_C_BNE)
        insn |= ((target & OP_MASK_CIMM5) << OP_SH_CIMM5);
      else
        gas_assert(0);

      md_number_to_chars ((char *) buf, insn, 2);
      buf += 2;
    }
  else
    {
      gas_assert(fragp->fr_var == 4);

      int rs1 = rvc_rs1_regmap[(insn >> OP_SH_CRS1S) & OP_MASK_CRS1S];
      int rs2 = rvc_rs2_regmap[(insn >> OP_SH_CRS2S) & OP_MASK_CRS2S];

      if((insn & MASK_C_J) == MATCH_C_J)
      {
        insn = MATCH_J;
        reloc_type = BFD_RELOC_MIPS_JMP;
      }
      else if((insn & MASK_C_BEQ) == MATCH_C_BEQ)
        insn = MATCH_BEQ | (rs1 << OP_SH_RS) | (rs2 << OP_SH_RT);
      else if((insn & MASK_C_BNE) == MATCH_C_BNE)
        insn = MATCH_BNE | (rs1 << OP_SH_RS) | (rs2 << OP_SH_RT);
      else
        gas_assert(0);

      exp.X_op = O_symbol;
      exp.X_add_symbol = fragp->fr_symbol;
      exp.X_add_number = fragp->fr_offset;

      fixp = fix_new_exp (fragp, buf - (bfd_byte *)fragp->fr_literal,
			  4, &exp, FALSE, reloc_type);
      fixp->fx_file = fragp->fr_file;
      fixp->fx_line = fragp->fr_line;
      fixp->fx_pcrel = 1;

      md_number_to_chars ((char *) buf, insn, 4);
      buf += 4;
    }

  gas_assert (buf == (bfd_byte *)fragp->fr_literal
          + fragp->fr_fix + fragp->fr_var);

  fragp->fr_fix += fragp->fr_var;
}

/* Relax a machine dependent frag.  This returns the amount by which
   the current size of the frag should change.  */

void
md_convert_frag(bfd *abfd, segT asec, fragS *fragp)
{
  if(RELAX_BRANCH_P(fragp->fr_subtype))
    md_convert_frag_branch(abfd, asec, fragp);
  else
    gas_assert(0);
}

/* This function is called whenever a label is defined.  It is used
   when handling branch delays; if a branch has a label, we assume we
   can not move it.  */

void
mips_define_label (symbolS *sym)
{
  segment_info_type *si = seg_info (now_seg);
  struct insn_label_list *l;

  if (free_insn_labels == NULL)
    l = (struct insn_label_list *) xmalloc (sizeof *l);
  else
    {
      l = free_insn_labels;
      free_insn_labels = l->next;
    }

  l->label = sym;
  l->next = si->label_list;
  si->label_list = l;

#ifdef OBJ_ELF
  dwarf2_emit_label (sym);
#endif
}

#if defined (OBJ_ELF) || defined (OBJ_MAYBE_ELF)

/* Some special processing for a MIPS ELF file.  */

void
mips_elf_final_processing (void)
{
  /* Write out the register information.  */
  if (mips_abi != ABI_64)
    {
      Elf32_RegInfo s;

      s.ri_gprmask = mips_gprmask;
      s.ri_cprmask[0] = 0;
      s.ri_cprmask[1] = mips_fprmask;
      s.ri_cprmask[2] = 0;
      s.ri_cprmask[3] = 0;
      /* The gp_value field is set by the MIPS ELF backend.  */

      bfd_riscv_elf32_swap_reginfo_out (stdoutput, &s,
				       ((Elf32_External_RegInfo *)
					mips_regmask_frag));
    }
  else
    {
      Elf64_Internal_RegInfo s;

      s.ri_gprmask = mips_gprmask;
      s.ri_pad = 0;
      s.ri_cprmask[0] = 0;
      s.ri_cprmask[1] = mips_fprmask;
      s.ri_cprmask[2] = 0;
      s.ri_cprmask[3] = 0;
      /* The gp_value field is set by the MIPS ELF backend.  */

      bfd_riscv_elf64_swap_reginfo_out (stdoutput, &s,
				       ((Elf64_External_RegInfo *)
					mips_regmask_frag));
    }

  /* Set the MIPS ELF flag bits.  FIXME: There should probably be some
     sort of BFD interface for this.  */
  if (mips_pic != NO_PIC)
    {
      elf_elfheader (stdoutput)->e_flags |= EF_MIPS_PIC;
      elf_elfheader (stdoutput)->e_flags |= EF_MIPS_CPIC;
    }
  if (mips_abicalls)
    elf_elfheader (stdoutput)->e_flags |= EF_MIPS_CPIC;

  /* Set the MIPS ELF ABI flags.  */
  if (mips_abi == ABI_64)
    elf_elfheader (stdoutput)->e_flags |= E_RISCV_ABI_64;
  else if (mips_abi == ABI_32)
    elf_elfheader (stdoutput)->e_flags |= E_RISCV_ABI_32;
  else
    gas_assert(0);
}

#endif /* OBJ_ELF || OBJ_MAYBE_ELF */

typedef struct proc {
  symbolS *func_sym;
  symbolS *func_end_sym;
  unsigned long reg_mask;
  unsigned long reg_offset;
  unsigned long fpreg_mask;
  unsigned long fpreg_offset;
  unsigned long frame_offset;
  unsigned long frame_reg;
  unsigned long pc_reg;
} procS;

static procS cur_proc;
static procS *cur_proc_ptr;
static int numprocs;

void
mips_handle_align (fragS *fragp)
{
  char *p;

  if (fragp->fr_type != rs_align_code)
    return;

  p = fragp->fr_literal + fragp->fr_fix;
  md_number_to_chars (p, RISCV_NOP, 4);
  fragp->fr_var = 4;
}

void
md_mips_end (void)
{
  if (cur_proc_ptr)
    as_warn (_("missing .end at end of assembly"));
}

static long
get_number (void)
{
  int negative = 0;
  long val = 0;

  if (*input_line_pointer == '-')
    {
      ++input_line_pointer;
      negative = 1;
    }
  if (!ISDIGIT (*input_line_pointer))
    as_bad (_("expected simple number"));
  if (input_line_pointer[0] == '0')
    {
      if (input_line_pointer[1] == 'x')
	{
	  input_line_pointer += 2;
	  while (ISXDIGIT (*input_line_pointer))
	    {
	      val <<= 4;
	      val |= hex_value (*input_line_pointer++);
	    }
	  return negative ? -val : val;
	}
      else
	{
	  ++input_line_pointer;
	  while (ISDIGIT (*input_line_pointer))
	    {
	      val <<= 3;
	      val |= *input_line_pointer++ - '0';
	    }
	  return negative ? -val : val;
	}
    }
  if (!ISDIGIT (*input_line_pointer))
    {
      printf (_(" *input_line_pointer == '%c' 0x%02x\n"),
	      *input_line_pointer, *input_line_pointer);
      as_warn (_("invalid number"));
      return -1;
    }
  while (ISDIGIT (*input_line_pointer))
    {
      val *= 10;
      val += *input_line_pointer++ - '0';
    }
  return negative ? -val : val;
}

/* The .file directive; just like the usual .file directive, but there
   is an initial number which is the ECOFF file index.  In the non-ECOFF
   case .file implies DWARF-2.  */

static void
s_mips_file (int x ATTRIBUTE_UNUSED)
{
  static int first_file_directive = 0;

  char *filename;

  filename = dwarf2_directive_file (0);

  /* Versions of GCC up to 3.1 start files with a ".file"
     directive even for stabs output.  Make sure that this
     ".file" is handled.  Note that you need a version of GCC
     after 3.1 in order to support DWARF-2 on MIPS.  */
  if (filename != NULL && ! first_file_directive)
    {
      (void) new_logical_line (filename, -1);
      s_app_file_string (filename, 0);
    }
  first_file_directive = 1;
}

/* The .loc directive, implying DWARF-2.  */

static void
s_mips_loc (int x ATTRIBUTE_UNUSED)
{
  dwarf2_directive_loc (0);
}

/* The .end directive.  */

static void
s_mips_end (int x ATTRIBUTE_UNUSED)
{
  symbolS *p;

  if (!is_end_of_line[(unsigned char) *input_line_pointer])
    {
      p = get_symbol ();
      demand_empty_rest_of_line ();
    }
  else
    p = NULL;

  if ((bfd_get_section_flags (stdoutput, now_seg) & SEC_CODE) == 0)
    as_warn (_(".end not in text section"));

  if (!cur_proc_ptr)
    {
      as_warn (_(".end directive without a preceding .ent directive."));
      demand_empty_rest_of_line ();
      return;
    }

  if (p != NULL)
    {
      gas_assert (S_GET_NAME (p));
      if (strcmp (S_GET_NAME (p), S_GET_NAME (cur_proc_ptr->func_sym)))
	as_warn (_(".end symbol does not match .ent symbol."));

      if (debug_type == DEBUG_STABS)
	stabs_generate_asm_endfunc (S_GET_NAME (p),
				    S_GET_NAME (p));
    }
  else
    as_warn (_(".end directive missing or unknown symbol"));

#ifdef OBJ_ELF
  /* Create an expression to calculate the size of the function.  */
  if (p && cur_proc_ptr)
    {
      OBJ_SYMFIELD_TYPE *obj = symbol_get_obj (p);
      expressionS *exp = xmalloc (sizeof (expressionS));

      obj->size = exp;
      exp->X_op = O_subtract;
      exp->X_add_symbol = symbol_temp_new_now ();
      exp->X_op_symbol = p;
      exp->X_add_number = 0;

      cur_proc_ptr->func_end_sym = exp->X_add_symbol;
    }

  /* Generate a .pdr section.  */
  if (IS_ELF && mips_flag_pdr)
    {
      segT saved_seg = now_seg;
      subsegT saved_subseg = now_subseg;
      expressionS exp;
      char *fragp;

#ifdef md_flush_pending_output
      md_flush_pending_output ();
#endif

      gas_assert (pdr_seg);
      subseg_set (pdr_seg, 0);

      /* Write the symbol.  */
      exp.X_op = O_symbol;
      exp.X_add_symbol = p;
      exp.X_add_number = 0;
      emit_expr (&exp, 4);

      fragp = frag_more (7 * 4);

      md_number_to_chars (fragp, cur_proc_ptr->reg_mask, 4);
      md_number_to_chars (fragp + 4, cur_proc_ptr->reg_offset, 4);
      md_number_to_chars (fragp + 8, cur_proc_ptr->fpreg_mask, 4);
      md_number_to_chars (fragp + 12, cur_proc_ptr->fpreg_offset, 4);
      md_number_to_chars (fragp + 16, cur_proc_ptr->frame_offset, 4);
      md_number_to_chars (fragp + 20, cur_proc_ptr->frame_reg, 4);
      md_number_to_chars (fragp + 24, cur_proc_ptr->pc_reg, 4);

      subseg_set (saved_seg, saved_subseg);
    }
#endif /* OBJ_ELF */

  cur_proc_ptr = NULL;
}

/* The .aent and .ent directives.  */

static void
s_mips_ent (int aent)
{
  symbolS *symbolP;

  symbolP = get_symbol ();
  if (*input_line_pointer == ',')
    ++input_line_pointer;
  SKIP_WHITESPACE ();
  if (ISDIGIT (*input_line_pointer)
      || *input_line_pointer == '-')
    get_number ();

  if ((bfd_get_section_flags (stdoutput, now_seg) & SEC_CODE) == 0)
    as_warn (_(".ent or .aent not in text section."));

  if (!aent && cur_proc_ptr)
    as_warn (_("missing .end"));

  if (!aent)
    {
      cur_proc_ptr = &cur_proc;
      memset (cur_proc_ptr, '\0', sizeof (procS));

      cur_proc_ptr->func_sym = symbolP;

      symbol_get_bfdsym (symbolP)->flags |= BSF_FUNCTION;

      ++numprocs;

      if (debug_type == DEBUG_STABS)
        stabs_generate_asm_func (S_GET_NAME (symbolP),
				 S_GET_NAME (symbolP));
    }

  demand_empty_rest_of_line ();
}

/* The .frame directive. If the mdebug section is present (IRIX 5 native)
   then ecoff.c (ecoff_directive_frame) is used. For embedded targets,
   s_mips_frame is used so that we can set the PDR information correctly.
   We can't use the ecoff routines because they make reference to the ecoff
   symbol table (in the mdebug section).  */

static void
s_mips_frame (int ignore ATTRIBUTE_UNUSED)
{
#ifdef OBJ_ELF
  if (IS_ELF)
    {
      long val;

      if (cur_proc_ptr == (procS *) NULL)
	{
	  as_warn (_(".frame outside of .ent"));
	  demand_empty_rest_of_line ();
	  return;
	}

      cur_proc_ptr->frame_reg = tc_get_register ();

      SKIP_WHITESPACE ();
      if (*input_line_pointer++ != ','
	  || get_absolute_expression_and_terminator (&val) != ',')
	{
	  as_warn (_("Bad .frame directive"));
	  --input_line_pointer;
	  demand_empty_rest_of_line ();
	  return;
	}

      cur_proc_ptr->frame_offset = val;
      cur_proc_ptr->pc_reg = tc_get_register ();

      demand_empty_rest_of_line ();
    }
  else
#endif /* OBJ_ELF */
    s_ignore (ignore);
}

/* The .fmask and .mask directives. If the mdebug section is present
   (IRIX 5 native) then ecoff.c (ecoff_directive_mask) is used. For
   embedded targets, s_mips_mask is used so that we can set the PDR
   information correctly. We can't use the ecoff routines because they
   make reference to the ecoff symbol table (in the mdebug section).  */

static void
s_mips_mask (int reg_type)
{
#ifdef OBJ_ELF
  if (IS_ELF)
    {
      long mask, off;

      if (cur_proc_ptr == (procS *) NULL)
	{
	  as_warn (_(".mask/.fmask outside of .ent"));
	  demand_empty_rest_of_line ();
	  return;
	}

      if (get_absolute_expression_and_terminator (&mask) != ',')
	{
	  as_warn (_("Bad .mask/.fmask directive"));
	  --input_line_pointer;
	  demand_empty_rest_of_line ();
	  return;
	}

      off = get_absolute_expression ();

      if (reg_type == 'F')
	{
	  cur_proc_ptr->fpreg_mask = mask;
	  cur_proc_ptr->fpreg_offset = off;
	}
      else
	{
	  cur_proc_ptr->reg_mask = mask;
	  cur_proc_ptr->reg_offset = off;
	}

      demand_empty_rest_of_line ();
    }
  else
#endif /* OBJ_ELF */
    s_ignore (reg_type);
}

/* A table describing all the processors gas knows about.  Names are
   matched in the order listed.

   To ease comparison, please keep this table in the same order as
   gcc's mips_cpu_info_table[].  */
static const struct mips_cpu_info mips_cpu_info_table[] =
{
  /* Entries for generic ISAs */
  { "rv32",           MIPS_CPU_IS_ISA,		ISA_RV32,       CPU_ROCKET32 },
  { "rv64",           MIPS_CPU_IS_ISA,		ISA_RV64,       CPU_ROCKET64 },
  { "riscv",          MIPS_CPU_IS_ISA,		ISA_RV64,       CPU_ROCKET64 },

  /* End marker */
  { NULL, 0, 0, 0 }
};


/* Return true if GIVEN is the same as CANONICAL, or if it is CANONICAL
   with a final "000" replaced by "k".  Ignore case.

   Note: this function is shared between GCC and GAS.  */

static bfd_boolean
mips_strict_matching_cpu_name_p (const char *canonical, const char *given)
{
  while (*given != 0 && TOLOWER (*given) == TOLOWER (*canonical))
    given++, canonical++;

  return ((*given == 0 && *canonical == 0)
	  || (strcmp (canonical, "000") == 0 && strcasecmp (given, "k") == 0));
}

/* Parse an option that takes the name of a processor as its argument.
   OPTION is the name of the option and CPU_STRING is the argument.
   Return the corresponding processor enumeration if the CPU_STRING is
   recognized, otherwise report an error and return null.

   A similar function exists in GCC.  */

static const struct mips_cpu_info *
mips_parse_cpu (const char *option, const char *cpu_string)
{
  const struct mips_cpu_info *p;

  /* 'from-abi' selects the most compatible architecture for the given
     ABI: MIPS I for 32-bit ABIs and MIPS III for 64-bit ABIs.  For the
     EABIs, we have to decide whether we're using the 32-bit or 64-bit
     version.  Look first at the -mgp options, if given, otherwise base
     the choice on MIPS_DEFAULT_64BIT.

     Treat NO_ABI like the EABIs.  One reason to do this is that the
     plain 'mips' and 'mips64' configs have 'from-abi' as their default
     architecture.  This code picks MIPS I for 'mips' and MIPS III for
     'mips64', just as we did in the days before 'from-abi'.  */
  if (strcasecmp (cpu_string, "from-abi") == 0)
    {
      if (ABI_NEEDS_32BIT_REGS (mips_abi))
	return mips_cpu_info_from_isa (ISA_RV32);

      if (ABI_NEEDS_64BIT_REGS (mips_abi))
	return mips_cpu_info_from_isa (ISA_RV64);

      if (file_mips_gp32 >= 0)
	return mips_cpu_info_from_isa (file_mips_gp32 ? ISA_RV32 : ISA_RV64);

      return mips_cpu_info_from_isa (ISA_RV64);
    }

  /* 'default' has traditionally been a no-op.  Probably not very useful.  */
  if (strcasecmp (cpu_string, "default") == 0)
    return 0;

  for (p = mips_cpu_info_table; p->name != 0; p++)
    if (mips_strict_matching_cpu_name_p (p->name, cpu_string))
      return p;

  as_bad ("Bad value (%s) for %s", cpu_string, option);
  return 0;
}

/* Return the canonical processor information for ISA (a member of the
   ISA_MIPS* enumeration).  */

static const struct mips_cpu_info *
mips_cpu_info_from_isa (int isa)
{
  int i;

  for (i = 0; mips_cpu_info_table[i].name != NULL; i++)
    if ((mips_cpu_info_table[i].flags & MIPS_CPU_IS_ISA)
	&& isa == mips_cpu_info_table[i].isa)
      return (&mips_cpu_info_table[i]);

  return NULL;
}

static void
show (FILE *stream, const char *string, int *col_p, int *first_p)
{
  if (*first_p)
    {
      fprintf (stream, "%24s", "");
      *col_p = 24;
    }
  else
    {
      fprintf (stream, ", ");
      *col_p += 2;
    }

  if (*col_p + strlen (string) > 72)
    {
      fprintf (stream, "\n%24s", "");
      *col_p = 24;
    }

  fprintf (stream, "%s", string);
  *col_p += strlen (string);

  *first_p = 0;
}

void
md_show_usage (FILE *stream)
{
  int column, first;
  size_t i;

  fprintf (stream, _("\
MIPS options:\n\
-EB			generate big endian output\n\
-EL			generate little endian output\n"));
  fprintf (stream, _("\
-march=CPU/-mtune=CPU	generate code/schedule for CPU, where CPU is one of:\n"));

  first = 1;

  for (i = 0; mips_cpu_info_table[i].name != NULL; i++)
    show (stream, mips_cpu_info_table[i].name, &column, &first);
  show (stream, "from-abi", &column, &first);
  fputc ('\n', stream);

  fprintf (stream, _("\
-mgp32			use 32-bit GPRs, regardless of the chosen ISA\n\
-msym32			assume all symbols have 32-bit values\n"));
#ifdef OBJ_ELF
  fprintf (stream, _("\
-KPIC, -call_shared	generate SVR4 position independent code\n\
-call_nonpic		generate non-PIC code that can operate with DSOs\n\
-non_shared		do not generate code that can operate with DSOs\n\
-xgot			assume a 32 bit GOT\n\
-mpdr, -mno-pdr		enable/disable creation of .pdr sections\n\
-mshared, -mno-shared   disable/enable .cpload optimization for\n\
                        position dependent (non shared) code\n\
-mabi=ABI		create ABI conformant object file for:\n"));

  first = 1;

  show (stream, "32", &column, &first);
  show (stream, "o64", &column, &first);
  show (stream, "n32", &column, &first);
  show (stream, "64", &column, &first);
  show (stream, "eabi", &column, &first);

  fputc ('\n', stream);

  fprintf (stream, _("\
-32			create o32 ABI object file (default)\n\
-n32			create n32 ABI object file\n\
-64			create 64 ABI object file\n"));
#endif
}

enum dwarf2_format
mips_dwarf2_format (asection *sec ATTRIBUTE_UNUSED)
{
  if (HAVE_64BIT_SYMBOLS)
    return dwarf2_format_64bit;
  else
    return dwarf2_format_32bit;
}

int
mips_dwarf2_addr_size (void)
{
  if (HAVE_64BIT_OBJECTS)
    return 8;
  else
    return 4;
}

/* Standard calling conventions leave the CFA at SP on entry.  */
void
mips_cfi_frame_initial_instructions (void)
{
  cfi_add_CFA_def_cfa_register (SP);
}

int
tc_mips_regname_to_dw2regnum (char *regname)
{
  unsigned int regnum = -1;
  unsigned int reg;

  if (reg_lookup (&regname, RTYPE_GP | RTYPE_NUM, &reg))
    regnum = reg;

  return regnum;
}
