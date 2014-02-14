/* PLT trampoline.  MIPS version.
   Copyright (C) 1996-2001, 2002, 2003, 2004, 2005
   Free Software Foundation, Inc.
   This file is part of the GNU C Library.
   Contributed by Kazumoto Kojima <kkojima@info.kanagawa-u.ac.jp>.

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

/*  FIXME: Profiling of shared libraries is not implemented yet.  */

#include <sysdep.h>
#include <link.h>
#include <elf.h>
#include <ldsodefs.h>
#include <dl-machine.h>

static inline bool
stub_pc_in_map (struct link_map *l, ElfW(Addr) stub_pc)
{
  const ElfW(Phdr) *p = l->l_phdr;
  ElfW(Half) e, nent = l->l_phnum;

  for (e = 0; e < nent; e++)
    {
      if (p[e].p_type == PT_LOAD)
	{
	  ElfW(Addr) base = p[e].p_vaddr + l->l_addr;
	  ElfW(Addr) limit = base + p[e].p_memsz;
	  if (stub_pc >= base && stub_pc < limit)
	    return true;
	}
    }

  return false;
}

/* Get link map for callers object containing STUB_PC.  */
static inline struct link_map *
elf_machine_runtime_link_map (ElfW(Addr) gpreg, ElfW(Addr) stub_pc)
{
    struct link_map *l;
    Lmid_t nsid;

    /* For the common case of a stub being called from the containing
       object, STUB_PC will point to somewhere within the object that
       is described by the link map fetched via got[1]. */
    l = (struct link_map *) (elf_mips_got_from_gpreg (gpreg) [1]);
    if (stub_pc_in_map (l, stub_pc))
      return l;

    /* Otherwise we have to scan all maps.  */
    for (nsid = 0; nsid < DL_NNS; ++nsid)
      for (l = GL(dl_ns)[nsid]._ns_loaded; l != NULL; l = l->l_next)
	if (stub_pc_in_map (l, stub_pc))
	  return l;

  _dl_signal_error (0, NULL, NULL, "cannot find runtime link map");
  return NULL;
}

/* Assembler veneer called from the PLT header code when using PLTs.
   The PLT header places the 2 args to _dl_fixup into t0 and t1. */
asm ("\n\
	.text\n\
	.align	2\n\
	.globl	_dl_runtime_pltresolve\n\
	.type	_dl_runtime_pltresolve,@function\n\
_dl_runtime_pltresolve:\n\
	# Save arguments and sp value in stack.\n\
1:	addi sp, sp, " STRINGXP(-10*SZREG) "\n\
	" STRINGXP(REG_S) " ra, 9*" STRINGXP(SZREG) "(sp)\n\
	" STRINGXP(REG_S) " a0, 1*" STRINGXP(SZREG) "(sp)\n\
	" STRINGXP(REG_S) " a1, 2*" STRINGXP(SZREG) "(sp)\n\
	" STRINGXP(REG_S) " a2, 3*" STRINGXP(SZREG) "(sp)\n\
	" STRINGXP(REG_S) " a3, 4*" STRINGXP(SZREG) "(sp)\n\
	" STRINGXP(REG_S) " a4, 5*" STRINGXP(SZREG) "(sp)\n\
	" STRINGXP(REG_S) " a5, 6*" STRINGXP(SZREG) "(sp)\n\
	" STRINGXP(REG_S) " a6, 7*" STRINGXP(SZREG) "(sp)\n\
	" STRINGXP(REG_S) " a7, 8*" STRINGXP(SZREG) "(sp)\n\
	mv a0, t0\n\
	mv a1, t1\n\
	jal _dl_fixup\n\
	" STRINGXP(REG_L) " ra, 9*" STRINGXP(SZREG) "(sp)\n\
	" STRINGXP(REG_L) " a0, 1*" STRINGXP(SZREG) "(sp)\n\
	" STRINGXP(REG_L) " a1, 2*" STRINGXP(SZREG) "(sp)\n\
	" STRINGXP(REG_L) " a2, 3*" STRINGXP(SZREG) "(sp)\n\
	" STRINGXP(REG_L) " a3, 4*" STRINGXP(SZREG) "(sp)\n\
	" STRINGXP(REG_L) " a4, 5*" STRINGXP(SZREG) "(sp)\n\
	" STRINGXP(REG_L) " a5, 6*" STRINGXP(SZREG) "(sp)\n\
	" STRINGXP(REG_L) " a6, 7*" STRINGXP(SZREG) "(sp)\n\
	" STRINGXP(REG_L) " a7, 8*" STRINGXP(SZREG) "(sp)\n\
	addi sp, sp, " STRINGXP(10*SZREG) "\n\
	jr	v0\n\
	.size	_dl_runtime_pltresolve, .-_dl_runtime_pltresolve\n\
");
