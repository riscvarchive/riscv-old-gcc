/* Definitions for MIPS running Linux-based GNU systems with ELF format.
   Copyright (C) 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005, 2006,
   2007, 2008, 2010, 2011 Free Software Foundation, Inc.

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 3, or (at your option)
any later version.

GCC is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING3.  If not see
<http://www.gnu.org/licenses/>.  */

#undef WCHAR_TYPE
#define WCHAR_TYPE "int"

#undef WCHAR_TYPE_SIZE
#define WCHAR_TYPE_SIZE 32

#undef ASM_DECLARE_OBJECT_NAME
#define ASM_DECLARE_OBJECT_NAME mips_declare_object_name

#undef TARGET_VERSION
#if TARGET_ENDIAN_DEFAULT == 0
#define TARGET_VERSION fprintf (stderr, " (RISC-V LE Linux/ELF)");
#else
#define TARGET_VERSION fprintf (stderr, " (RISC-V BE Linux/ELF)");
#endif

/* If we don't set MASK_ABICALLS, we can't default to PIC.  */
#undef TARGET_DEFAULT
#define TARGET_DEFAULT MASK_ABICALLS

#define TARGET_OS_CPP_BUILTINS()				\
  do {								\
    LINUX_TARGET_OS_CPP_BUILTINS();				\
    /* The GNU C++ standard library requires this.  */		\
    if (c_dialect_cxx ())					\
      builtin_define ("_GNU_SOURCE");				\
  } while (0)

#undef SUBTARGET_CPP_SPEC
#define SUBTARGET_CPP_SPEC "%{posix:-D_POSIX_SOURCE} %{pthread:-D_REENTRANT}"

/* A standard GNU/Linux mapping.  On most targets, it is included in
   CC1_SPEC itself by config/linux.h, but mips.h overrides CC1_SPEC
   and provides this hook instead.  */
#undef SUBTARGET_CC1_SPEC
#define SUBTARGET_CC1_SPEC "%{profile:-p}"

#define GLIBC_DYNAMIC_LINKER "/lib/ld.so.1"

/* Borrowed from sparc/linux.h */
#undef LINK_SPEC
#define LINK_SPEC \
 "%(endian_spec) \
  %{shared:-shared} \
  %{!shared: \
    %{!static: \
      %{rdynamic:-export-dynamic} \
      -dynamic-linker " LINUX_DYNAMIC_LINKER "} \
      %{static:-static}}"

#undef SUBTARGET_ASM_SPEC
#define SUBTARGET_ASM_SPEC \
  "%{fPIC:-KPIC}"

/* The MIPS assembler has different syntax for .set. We set it to
   .dummy to trap any errors.  */
#undef SET_ASM_OP
#define SET_ASM_OP "\t.dummy\t"

#undef ASM_OUTPUT_DEF
#define ASM_OUTPUT_DEF(FILE,LABEL1,LABEL2)				\
 do {									\
	fputc ( '\t', FILE);						\
	assemble_name (FILE, LABEL1);					\
	fputs ( " = ", FILE);						\
	assemble_name (FILE, LABEL2);					\
	fputc ( '\n', FILE);						\
 } while (0)

/* The glibc _mcount stub will save $v0 for us.  Don't mess with saving
   it, since ASM_OUTPUT_REG_PUSH/ASM_OUTPUT_REG_POP do not work in the
   presence of $gp-relative calls.  */
#undef ASM_OUTPUT_REG_PUSH
#undef ASM_OUTPUT_REG_POP

#undef LIB_SPEC
#define LIB_SPEC "\
%{pthread:-lpthread} \
%{shared:-lc} \
%{!shared: \
  %{profile:-lc_p} %{!profile:-lc}}"

#define MD_UNWIND_SUPPORT "config/riscv/linux-unwind.h"

#ifdef HAVE_AS_NO_SHARED
/* Default to -mno-shared for non-PIC.  */
# define NO_SHARED_SPECS \
  "%{mshared|mno-shared|fpic|fPIC|fpie|fPIE:;:-mno-shared}"
#else
# define NO_SHARED_SPECS ""
#endif

/* -march=native handling only makes sense with compiler running on
   a RISC-V machine.  */
#define MARCH_MTUNE_NATIVE_SPECS ""

#define LINUX_DRIVER_SELF_SPECS \
  NO_SHARED_SPECS		\
  MARCH_MTUNE_NATIVE_SPECS

#undef DRIVER_SELF_SPECS
#define DRIVER_SELF_SPECS \
  LINUX_DRIVER_SELF_SPECS

/* Similar to standard Linux, but adding -ffast-math support.  */
#undef  ENDFILE_SPEC
#define ENDFILE_SPEC \
   "%{shared|pie:crtendS.o%s;:crtend.o%s} crtn.o%s"
