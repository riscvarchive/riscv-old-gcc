/* RISC-V ELF support for BFD.
   Copyright 1993, 1994, 1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002,
   2003, 2004, 2005, 2008, 2009, 2010
   Free Software Foundation, Inc.

   By Andrew Waterman, University of California,
   <waterman@eecs.berkeley.edu>.
   Based on MIPS ELF support for BFD, by Ian Lance Taylor.

   This file is part of BFD, the Binary File Descriptor library.

   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 3 of the License, or
   (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software
   Foundation, Inc., 51 Franklin Street - Fifth Floor, Boston,
   MA 02110-1301, USA.  */

/* This file holds definitions specific to the RISCV ELF ABI.  Note
   that most of this is not actually implemented by BFD.  */

#ifndef _ELF_RISCV_H
#define _ELF_RISCV_H

#include "elf/reloc-macros.h"

/* Relocation types.  */
START_RELOC_NUMBERS (elf_riscv_reloc_type)
  RELOC_NUMBER (R_RISCV_NONE, 0)
  RELOC_NUMBER (R_RISCV_32, 2)		/* In Elf 64: alias R_RISCV_ADD */
  RELOC_NUMBER (R_RISCV_REL32, 3)	/* In Elf 64: alias R_RISCV_REL */
  RELOC_NUMBER (R_RISCV_26, 4)
  RELOC_NUMBER (R_RISCV_HI16, 5)
  RELOC_NUMBER (R_RISCV_LO16, 6)
  RELOC_NUMBER (R_RISCV_GPREL16, 7)	/* In Elf 64: alias R_RISCV_GPREL */
  RELOC_NUMBER (R_RISCV_LITERAL, 8)
  RELOC_NUMBER (R_RISCV_GOT16, 9)	/* In Elf 64: alias R_RISCV_GOT */
  RELOC_NUMBER (R_RISCV_PC16, 10)
  RELOC_NUMBER (R_RISCV_CALL16, 11)	/* In Elf 64: alias R_RISCV_CALL */
  RELOC_NUMBER (R_RISCV_GPREL32, 12)
  /* The remaining relocs are defined on Irix, although they are not
     in the RISC-V ELF ABI.  */
  RELOC_NUMBER (R_RISCV_UNUSED1, 13)
  RELOC_NUMBER (R_RISCV_UNUSED2, 14)
  RELOC_NUMBER (R_RISCV_UNUSED3, 15)

  RELOC_NUMBER (R_RISCV_64, 18)
  RELOC_NUMBER (R_RISCV_GOT_DISP, 19)
  RELOC_NUMBER (R_RISCV_GOT_HI16, 22)
  RELOC_NUMBER (R_RISCV_GOT_LO16, 23)
  RELOC_NUMBER (R_RISCV_SUB, 24)
  RELOC_NUMBER (R_RISCV_INSERT_A, 25)
  RELOC_NUMBER (R_RISCV_INSERT_B, 26)
  RELOC_NUMBER (R_RISCV_DELETE, 27)
  RELOC_NUMBER (R_RISCV_CALL_HI16, 30)
  RELOC_NUMBER (R_RISCV_CALL_LO16, 31)
  RELOC_NUMBER (R_RISCV_SCN_DISP, 32)
  RELOC_NUMBER (R_RISCV_REL16, 33)
  RELOC_NUMBER (R_RISCV_ADD_IMMEDIATE, 34)
  RELOC_NUMBER (R_RISCV_PJUMP, 35)
  RELOC_NUMBER (R_RISCV_RELGOT, 36)
  RELOC_NUMBER (R_RISCV_JALR, 37)
  /* TLS relocations.  */
  RELOC_NUMBER (R_RISCV_TLS_DTPMOD32, 38)
  RELOC_NUMBER (R_RISCV_TLS_DTPREL32, 39)
  RELOC_NUMBER (R_RISCV_TLS_DTPMOD64, 40)
  RELOC_NUMBER (R_RISCV_TLS_DTPREL64, 41)
  RELOC_NUMBER (R_RISCV_TLS_GD, 42)
  RELOC_NUMBER (R_RISCV_TLS_LDM, 43)
  RELOC_NUMBER (R_RISCV_TLS_DTPREL_HI16, 44)
  RELOC_NUMBER (R_RISCV_TLS_DTPREL_LO16, 45)
  RELOC_NUMBER (R_RISCV_TLS_GOTTPREL, 46)
  RELOC_NUMBER (R_RISCV_TLS_TPREL32, 47)
  RELOC_NUMBER (R_RISCV_TLS_TPREL64, 48)
  RELOC_NUMBER (R_RISCV_TLS_TPREL_HI16, 49)
  RELOC_NUMBER (R_RISCV_TLS_TPREL_LO16, 50)
  RELOC_NUMBER (R_RISCV_TLS_GOT_HI16, 51)
  RELOC_NUMBER (R_RISCV_TLS_GOT_LO16, 52)
  RELOC_NUMBER (R_RISCV_TLS_GD_HI16, 53)
  RELOC_NUMBER (R_RISCV_TLS_GD_LO16, 54)
  RELOC_NUMBER (R_RISCV_TLS_LDM_HI16, 55)
  RELOC_NUMBER (R_RISCV_TLS_LDM_LO16, 56)
  RELOC_NUMBER (R_RISCV_GLOB_DAT, 57)
  FAKE_RELOC (R_RISCV_max, 58)
  /* These relocations are specific to VxWorks.  */
  RELOC_NUMBER (R_RISCV_COPY, 126)
  RELOC_NUMBER (R_RISCV_JUMP_SLOT, 127)
  /* This was a GNU extension used by embedded-PIC.  It was co-opted by
     riscv-linux for exception-handling data.  It is no longer used, but
     should continue to be supported by the linker for backward
     compatibility.  (GCC stopped using it in May, 2004.)  */
  RELOC_NUMBER (R_RISCV_PC32, 248)
  /* FIXME: this relocation is used internally by gas.  */
  RELOC_NUMBER (R_RISCV_GNU_REL16_S2, 250)
  /* These are GNU extensions to enable C++ vtable garbage collection.  */
  RELOC_NUMBER (R_RISCV_GNU_VTINHERIT, 253)
  RELOC_NUMBER (R_RISCV_GNU_VTENTRY, 254)
END_RELOC_NUMBERS (R_RISCV_maxext)

/* Processor specific flags for the ELF header e_flags field.  */

/* File contains position independent code.  */
#define EF_RISCV_PIC		0x00000002

/* Process the .RISCV.options section first by ld */
#define EF_RISCV_OPTIONS_FIRST	0x00000080

/* Architectural Extensions used by this file */
#define EF_RISCV_ARCH_ASE	0x0f000000

/* Four bit RISCV architecture field.  */
#define EF_RISCV_ARCH		0xf0000000

/* RV32 code.  */
#define E_RISCV_ARCH_RV32 0x10000000

/* RV64 code.  */
#define E_RISCV_ARCH_RV64 0x20000000

/* The ABI of the file.  Also see EF_RISCV_ABI2 above. */
#define EF_RISCV_ABI		0x0000f000

/* The 32-bit abi. */
#define E_RISCV_ABI_32          0x00001000

/* The 64-bit abi. */
#define E_RISCV_ABI_64          0x00002000

/* Machine variant if we know it.  This field was invented at Cygnus,
   but it is hoped that other vendors will adopt it.  If some standard
   is developed, this code should be changed to follow it. */

#define EF_RISCV_MACH		0x00ff0000

/* Cygnus is choosing values between 80 and 9F;
   00 - 7F should be left for a future standard;
   the rest are open. */

#define E_RISCV_MACH_ROCKET32 0x00810000
#define E_RISCV_MACH_ROCKET64 0x00820000

/* Processor specific section indices.  These sections do not actually
   exist.  Symbols with a st_shndx field corresponding to one of these
   values have a special meaning.  */

/* Defined and allocated common symbol.  Value is virtual address.  If
   relocated, alignment must be preserved.  */
#define SHN_RISCV_ACOMMON	SHN_LORESERVE

/* Defined and allocated text symbol.  Value is virtual address.
   Occur in the dynamic symbol table of Alpha OSF/1 and Irix 5 executables.  */
#define SHN_RISCV_TEXT		(SHN_LORESERVE + 1)

/* Defined and allocated data symbol.  Value is virtual address.
   Occur in the dynamic symbol table of Alpha OSF/1 and Irix 5 executables.  */
#define SHN_RISCV_DATA		(SHN_LORESERVE + 2)

/* Small common symbol.  */
#define SHN_RISCV_SCOMMON	(SHN_LORESERVE + 3)

/* Small undefined symbol.  */
#define SHN_RISCV_SUNDEFINED	(SHN_LORESERVE + 4)

/* Processor specific section types.  */

/* Section contains the set of dynamic shared objects used when
   statically linking.  */
#define SHT_RISCV_LIBLIST	0x70000000

/* Section contains list of symbols whose definitions conflict with
   symbols defined in shared objects.  */
#define SHT_RISCV_CONFLICT	0x70000002

/* Section contains the global pointer table.  */
#define SHT_RISCV_GPTAB		0x70000003

/* Section contains some sort of debugging information.  The exact
   format is unspecified.  It's probably ECOFF symbols.  */
#define SHT_RISCV_DEBUG		0x70000005

/* Section contains register usage information.  */
#define SHT_RISCV_REGINFO	0x70000006

/* Section contains interface information.  */
#define SHT_RISCV_IFACE		0x7000000b

/* Section contains description of contents of another section.  */
#define SHT_RISCV_CONTENT	0x7000000c

/* Section contains miscellaneous options.  */
#define SHT_RISCV_OPTIONS	0x7000000d

/* List of libraries the binary depends on.  Includes a time stamp, version
   number.  */
#define SHT_RISCV_SYMBOL_LIB	0x70000020

/* Events section.  */
#define SHT_RISCV_EVENTS		0x70000021

/* Get ELf32_xxx struct definitions */
#include "mips.h"

/* RISC-V ELF .reginfo swapping routines.  */
extern void bfd_riscv_elf32_swap_reginfo_in
  (bfd *, const Elf32_External_RegInfo *, Elf32_RegInfo *);
extern void bfd_riscv_elf32_swap_reginfo_out
  (bfd *, const Elf32_RegInfo *, Elf32_External_RegInfo *);

/* Processor specific section flags.  */

/* This section must be in the global data area.  */
#define SHF_RISCV_GPREL		0x10000000

/* This section may not be stripped.  */
#define SHF_RISCV_NOSTRIP	0x08000000

/* Processor specific program header types.  */

/* Register usage information.  Identifies one .reginfo section.  */
#define PT_RISCV_REGINFO		0x70000000

/* Runtime procedure table.  */
#define PT_RISCV_RTPROC		0x70000001

/* .RISCV.options section.  */
#define PT_RISCV_OPTIONS		0x70000002

/* Processor specific dynamic array tags.  */

/* 32 bit version number for runtime linker interface.  */
#define DT_RISCV_RLD_VERSION	0x70000001

/* Time stamp.  */
#define DT_RISCV_TIME_STAMP	0x70000002

/* Checksum of external strings and common sizes.  */
#define DT_RISCV_ICHECKSUM	0x70000003

/* Index of version string in string table.  */
#define DT_RISCV_IVERSION	0x70000004

/* 32 bits of flags.  */
#define DT_RISCV_FLAGS		0x70000005

/* Base address of the segment.  */
#define DT_RISCV_BASE_ADDRESS	0x70000006

/* ??? */
#define DT_RISCV_MSYM		0x70000007

/* Address of .conflict section.  */
#define DT_RISCV_CONFLICT	0x70000008

/* Address of .liblist section.  */
#define DT_RISCV_LIBLIST		0x70000009

/* Number of local global offset table entries.  */
#define DT_RISCV_LOCAL_GOTNO	0x7000000a

/* Number of entries in the .conflict section.  */
#define DT_RISCV_CONFLICTNO	0x7000000b

/* Number of entries in the .liblist section.  */
#define DT_RISCV_LIBLISTNO	0x70000010

/* Number of entries in the .dynsym section.  */
#define DT_RISCV_SYMTABNO	0x70000011

/* Index of first external dynamic symbol not referenced locally.  */
#define DT_RISCV_UNREFEXTNO	0x70000012

/* Index of first dynamic symbol in global offset table.  */
#define DT_RISCV_GOTSYM		0x70000013

/* Number of page table entries in global offset table.  */
#define DT_RISCV_HIPAGENO	0x70000014

/* Address of run time loader map, used for debugging.  */
#define DT_RISCV_RLD_MAP		0x70000016

/* Delta C++ class definition.  */
#define DT_RISCV_DELTA_CLASS	0x70000017

/* Number of entries in DT_RISCV_DELTA_CLASS.  */
#define DT_RISCV_DELTA_CLASS_NO	0x70000018

/* Delta C++ class instances.  */
#define DT_RISCV_DELTA_INSTANCE	0x70000019

/* Number of entries in DT_RISCV_DELTA_INSTANCE.  */
#define DT_RISCV_DELTA_INSTANCE_NO	0x7000001a

/* Delta relocations.  */
#define DT_RISCV_DELTA_RELOC	0x7000001b

/* Number of entries in DT_RISCV_DELTA_RELOC.  */
#define DT_RISCV_DELTA_RELOC_NO	0x7000001c

/* Delta symbols that Delta relocations refer to.  */
#define DT_RISCV_DELTA_SYM	0x7000001d

/* Number of entries in DT_RISCV_DELTA_SYM.  */
#define DT_RISCV_DELTA_SYM_NO	0x7000001e

/* Delta symbols that hold class declarations.  */
#define DT_RISCV_DELTA_CLASSSYM	0x70000020

/* Number of entries in DT_RISCV_DELTA_CLASSSYM.  */
#define DT_RISCV_DELTA_CLASSSYM_NO	0x70000021

/* Flags indicating information about C++ flavor.  */
#define DT_RISCV_CXX_FLAGS	0x70000022

/* Pixie information (???).  */
#define DT_RISCV_PIXIE_INIT	0x70000023

/* Address of .RISCV.symlib */
#define DT_RISCV_SYMBOL_LIB	0x70000024

/* The GOT index of the first PTE for a segment */
#define DT_RISCV_LOCALPAGE_GOTIDX	0x70000025

/* The GOT index of the first PTE for a local symbol */
#define DT_RISCV_LOCAL_GOTIDX	0x70000026

/* The GOT index of the first PTE for a hidden symbol */
#define DT_RISCV_HIDDEN_GOTIDX	0x70000027

/* The GOT index of the first PTE for a protected symbol */
#define DT_RISCV_PROTECTED_GOTIDX	0x70000028

/* Address of `.RISCV.options'.  */
#define DT_RISCV_OPTIONS		0x70000029

/* Address of `.interface'.  */
#define DT_RISCV_INTERFACE	0x7000002a

/* ??? */
#define DT_RISCV_DYNSTR_ALIGN	0x7000002b

/* Size of the .interface section.  */
#define DT_RISCV_INTERFACE_SIZE	0x7000002c

/* Size of rld_text_resolve function stored in the GOT.  */
#define DT_RISCV_RLD_TEXT_RESOLVE_ADDR	0x7000002d

/* Default suffix of DSO to be added by rld on dlopen() calls.  */
#define DT_RISCV_PERF_SUFFIX	0x7000002e

/* Size of compact relocation section (O32).  */
#define DT_RISCV_COMPACT_SIZE	0x7000002f

/* GP value for auxiliary GOTs.  */
#define DT_RISCV_GP_VALUE	0x70000030

/* Address of auxiliary .dynamic.  */
#define DT_RISCV_AUX_DYNAMIC	0x70000031

/* Address of the base of the PLTGOT.  */
#define DT_RISCV_PLTGOT         0x70000032

/* Points to the base of a writable PLT.  */
#define DT_RISCV_RWPLT          0x70000034

/* The RISC-V psABI was updated in 2008 with support for PLTs and copy
   relocs.  There are therefore two types of nonzero SHN_UNDEF functions:
   PLT entries and traditional RISC-V lazy binding stubs.  We mark the former
   with STO_RISCV_PLT to distinguish them from the latter.  */
#define STO_RISCV_PLT		0x8

/* This value is used to mark PIC functions in an object that mixes
   PIC and non-PIC.  */
#define STO_RISCV_PIC		0x20
#define ELF_ST_IS_RISCV_PIC(OTHER) \
  (((OTHER) & ~ELF_ST_VISIBILITY (-1)) == STO_RISCV_PIC)
#define ELF_ST_SET_RISCV_PIC(OTHER) \
  (STO_RISCV_PIC | ELF_ST_VISIBILITY (OTHER))

/* The 64-bit RISC-V ELF ABI uses an unusual reloc format.  Each
   relocation entry specifies up to three actual relocations, all at
   the same address.  The first relocation which required a symbol
   uses the symbol in the r_sym field.  The second relocation which
   requires a symbol uses the symbol in the r_ssym field.  If all
   three relocations require a symbol, the third one uses a zero
   value.  */

/* An entry in a 64 bit SHT_REL section.  */

typedef struct
{
  /* Address of relocation.  */
  unsigned char r_offset[8];
  /* Symbol index.  */
  unsigned char r_sym[4];
  /* Special symbol.  */
  unsigned char r_ssym[1];
  /* Third relocation.  */
  unsigned char r_type3[1];
  /* Second relocation.  */
  unsigned char r_type2[1];
  /* First relocation.  */
  unsigned char r_type[1];
} Elf64_RISCV_External_Rel;

typedef struct
{
  /* Address of relocation.  */
  bfd_vma r_offset;
  /* Symbol index.  */
  unsigned long r_sym;
  /* Special symbol.  */
  unsigned char r_ssym;
  /* Third relocation.  */
  unsigned char r_type3;
  /* Second relocation.  */
  unsigned char r_type2;
  /* First relocation.  */
  unsigned char r_type;
} Elf64_RISCV_Internal_Rel;

/* An entry in a 64 bit SHT_RELA section.  */

typedef struct
{
  /* Address of relocation.  */
  unsigned char r_offset[8];
  /* Symbol index.  */
  unsigned char r_sym[4];
  /* Special symbol.  */
  unsigned char r_ssym[1];
  /* Third relocation.  */
  unsigned char r_type3[1];
  /* Second relocation.  */
  unsigned char r_type2[1];
  /* First relocation.  */
  unsigned char r_type[1];
  /* Addend.  */
  unsigned char r_addend[8];
} Elf64_RISCV_External_Rela;

typedef struct
{
  /* Address of relocation.  */
  bfd_vma r_offset;
  /* Symbol index.  */
  unsigned long r_sym;
  /* Special symbol.  */
  unsigned char r_ssym;
  /* Third relocation.  */
  unsigned char r_type3;
  /* Second relocation.  */
  unsigned char r_type2;
  /* First relocation.  */
  unsigned char r_type;
  /* Addend.  */
  bfd_signed_vma r_addend;
} Elf64_RISCV_Internal_Rela;

/* RISC-V ELF 64 relocation info access macros.  */
#define ELF64_RISCV_R_SSYM(i) (((i) >> 24) & 0xff)
#define ELF64_RISCV_R_TYPE3(i) (((i) >> 16) & 0xff)
#define ELF64_RISCV_R_TYPE2(i) (((i) >> 8) & 0xff)
#define ELF64_RISCV_R_TYPE(i) ((i) & 0xff)

/* RISC-V ELF option header swapping routines.  */
extern void bfd_riscv_elf_swap_options_in
  (bfd *, const Elf_External_Options *, Elf_Internal_Options *);
extern void bfd_riscv_elf_swap_options_out
  (bfd *, const Elf_Internal_Options *, Elf_External_Options *);

/* RISC-V ELF reginfo swapping routines.  */
extern void bfd_riscv_elf64_swap_reginfo_in
  (bfd *, const Elf64_External_RegInfo *, Elf64_Internal_RegInfo *);
extern void bfd_riscv_elf64_swap_reginfo_out
  (bfd *, const Elf64_Internal_RegInfo *, Elf64_External_RegInfo *);

#endif /* _ELF_RISCV_H */
