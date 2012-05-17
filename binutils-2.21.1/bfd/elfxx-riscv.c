/* MIPS-specific support for ELF
   Copyright 1993, 1994, 1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002,
   2003, 2004, 2005, 2006, 2007, 2008, 2009, 2010 Free Software Foundation, Inc.

   Most of the information added by Ian Lance Taylor, Cygnus Support,
   <ian@cygnus.com>.
   N32/64 ABI support added by Mark Mitchell, CodeSourcery, LLC.
   <mark@codesourcery.com>
   Traditional MIPS targets support added by Koundinya.K, Dansk Data
   Elektronik & Operations Research Group. <kk@ddeorg.soft.net>

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


/* This file handles functionality common to the different MIPS ABI's.  */

#include "sysdep.h"
#include "bfd.h"
#include "libbfd.h"
#include "libiberty.h"
#include "elf-bfd.h"
#include "elfxx-riscv.h"
#include "elf/riscv.h"
#include "opcode/riscv.h"

/* Get the ECOFF swapping routines.  */
#include "coff/sym.h"
#include "coff/symconst.h"
#include "coff/ecoff.h"
#include "coff/mips.h"

#include "hashtab.h"

/* This structure is used to hold information about one GOT entry.
   There are three types of entry:

      (1) absolute addresses
	    (abfd == NULL)
      (2) SYMBOL + OFFSET addresses, where SYMBOL is local to an input bfd
	    (abfd != NULL, symndx >= 0)
      (3) SYMBOL addresses, where SYMBOL is not local to an input bfd
	    (abfd != NULL, symndx == -1)

   Type (3) entries are treated differently for different types of GOT.
   In the "master" GOT -- i.e.  the one that describes every GOT
   reference needed in the link -- the mips_got_entry is keyed on both
   the symbol and the input bfd that references it.  If it turns out
   that we need multiple GOTs, we can then use this information to
   create separate GOTs for each input bfd.

   However, we want each of these separate GOTs to have at most one
   entry for a given symbol, so their type (3) entries are keyed only
   on the symbol.  The input bfd given by the "abfd" field is somewhat
   arbitrary in this case.

   This means that when there are multiple GOTs, each GOT has a unique
   mips_got_entry for every symbol within it.  We can therefore use the
   mips_got_entry fields (tls_type and gotidx) to track the symbol's
   GOT index.

   However, if it turns out that we need only a single GOT, we continue
   to use the master GOT to describe it.  There may therefore be several
   mips_got_entries for the same symbol, each with a different input bfd.
   We want to make sure that each symbol gets a unique GOT entry, so when
   there's a single GOT, we use the symbol's hash entry, not the
   mips_got_entry fields, to track a symbol's GOT index.  */
struct mips_got_entry
{
  /* The input bfd in which the symbol is defined.  */
  bfd *abfd;
  /* The index of the symbol, as stored in the relocation r_info, if
     we have a local symbol; -1 otherwise.  */
  long symndx;
  union
  {
    /* If abfd == NULL, an address that must be stored in the got.  */
    bfd_vma address;
    /* If abfd != NULL && symndx != -1, the addend of the relocation
       that should be added to the symbol value.  */
    bfd_vma addend;
    /* If abfd != NULL && symndx == -1, the hash table entry
       corresponding to symbol in the GOT.  The symbol's entry
       is in the local area if h->global_got_area is GGA_NONE,
       otherwise it is in the global area.  */
    struct mips_elf_link_hash_entry *h;
  } d;

  /* The TLS types included in this GOT entry (specifically, GD and
     IE).  The GD and IE flags can be added as we encounter new
     relocations.  LDM can also be set; it will always be alone, not
     combined with any GD or IE flags.  An LDM GOT entry will be
     a local symbol entry with r_symndx == 0.  */
  unsigned char tls_type;

  /* The offset from the beginning of the .got section to the entry
     corresponding to this symbol+addend.  If it's a global symbol
     whose offset is yet to be decided, it's going to be -1.  */
  long gotidx;
};

/* This structure describes a range of addends: [MIN_ADDEND, MAX_ADDEND].
   The structures form a non-overlapping list that is sorted by increasing
   MIN_ADDEND.  */
struct mips_got_page_range
{
  struct mips_got_page_range *next;
  bfd_signed_vma min_addend;
  bfd_signed_vma max_addend;
};

/* This structure describes the range of addends that are applied to page
   relocations against a given symbol.  */
struct mips_got_page_entry
{
  /* The input bfd in which the symbol is defined.  */
  bfd *abfd;
  /* The index of the symbol, as stored in the relocation r_info.  */
  long symndx;
  /* The ranges for this page entry.  */
  struct mips_got_page_range *ranges;
  /* The maximum number of page entries needed for RANGES.  */
  bfd_vma num_pages;
};

/* This structure is used to hold .got information when linking.  */

struct mips_got_info
{
  /* The global symbol in the GOT with the lowest index in the dynamic
     symbol table.  */
  struct elf_link_hash_entry *global_gotsym;
  /* The number of global .got entries.  */
  unsigned int global_gotno;
  /* The number of global .got entries that are in the GGA_RELOC_ONLY area.  */
  unsigned int reloc_only_gotno;
  /* The number of .got slots used for TLS.  */
  unsigned int tls_gotno;
  /* The first unused TLS .got entry.  Used only during
     mips_elf_initialize_tls_index.  */
  unsigned int tls_assigned_gotno;
  /* The number of local .got entries, eventually including page entries.  */
  unsigned int local_gotno;
  /* The maximum number of page entries needed.  */
  unsigned int page_gotno;
  /* The number of local .got entries we have used.  */
  unsigned int assigned_gotno;
  /* A hash table holding members of the got.  */
  struct htab *got_entries;
  /* A hash table of mips_got_page_entry structures.  */
  struct htab *got_page_entries;
  /* This is the GOT index of the TLS LDM entry for the GOT, MINUS_ONE
     for none, or MINUS_TWO for not yet assigned.  This is needed
     because a single-GOT link may have multiple hash table entries
     for the LDM.  It does not get initialized in multi-GOT mode.  */
  bfd_vma tls_ldm_offset;
};

/* Another structure used to pass arguments for got entries traversal.  */

struct mips_elf_set_global_got_offset_arg
{
  struct mips_got_info *g;
  int value;
  unsigned int needed_relocs;
  struct bfd_link_info *info;
};

/* A structure used to count TLS relocations or GOT entries, for GOT
   entry or ELF symbol table traversal.  */

struct mips_elf_count_tls_arg
{
  struct bfd_link_info *info;
  unsigned int needed;
};

struct _mips_elf_section_data
{
  struct bfd_elf_section_data elf;
  union
  {
    bfd_byte *tdata;
  } u;
};

#define mips_elf_section_data(sec) \
  ((struct _mips_elf_section_data *) elf_section_data (sec))

#define is_mips_elf(bfd)				\
  (bfd_get_flavour (bfd) == bfd_target_elf_flavour	\
   && elf_tdata (bfd) != NULL				\
   && elf_object_id (bfd) == MIPS_ELF_DATA)

/* The ABI says that every symbol used by dynamic relocations must have
   a global GOT entry.  Among other things, this provides the dynamic
   linker with a free, directly-indexed cache.  The GOT can therefore
   contain symbols that are not referenced by GOT relocations themselves
   (in other words, it may have symbols that are not referenced by things
   like R_RISCV_GOT16).

   GOT relocations are less likely to overflow if we put the associated
   GOT entries towards the beginning.  We therefore divide the global
   GOT entries into two areas: "normal" and "reloc-only".  Entries in
   the first area can be used for both dynamic relocations and GP-relative
   accesses, while those in the "reloc-only" area are for dynamic
   relocations only.

   These GGA_* ("Global GOT Area") values are organised so that lower
   values are more general than higher values.  Also, non-GGA_NONE
   values are ordered by the position of the area in the GOT.  */
#define GGA_NORMAL 0
#define GGA_RELOC_ONLY 1
#define GGA_NONE 2

/* Information about a non-PIC interface to a PIC function.  There are
   two ways of creating these interfaces.  The first is to add:

	lui	t7,%hi(func)
	addi	t7,t7,%lo(func)

   immediately before a PIC function "func".  The second is to add:

	lui	t7,%hi(func)
	addi	t7,t7,%lo(func)
	j	func

   to a separate trampoline section.

   Stubs of the first kind go in a new section immediately before the
   target function.  Stubs of the second kind go in a single section
   pointed to by the hash table's "strampoline" field.  */
struct mips_elf_la25_stub {
  /* The generated section that contains this stub.  */
  asection *stub_section;

  /* The offset of the stub from the start of STUB_SECTION.  */
  bfd_vma offset;

  /* One symbol for the original function.  Its location is available
     in H->root.root.u.def.  */
  struct mips_elf_link_hash_entry *h;
};

/* This structure is passed to mips_elf_sort_hash_table_f when sorting
   the dynamic symbols.  */

struct mips_elf_hash_sort_data
{
  /* The symbol in the global GOT with the lowest dynamic symbol table
     index.  */
  struct elf_link_hash_entry *low;
  /* The least dynamic symbol table index corresponding to a non-TLS
     symbol with a GOT entry.  */
  long min_got_dynindx;
  /* The greatest dynamic symbol table index corresponding to a symbol
     with a GOT entry that is not referenced (e.g., a dynamic symbol
     with dynamic relocations pointing to it from non-primary GOTs).  */
  long max_unref_got_dynindx;
  /* The greatest dynamic symbol table index not corresponding to a
     symbol without a GOT entry.  */
  long max_non_got_dynindx;
};

/* The MIPS ELF linker needs additional information for each symbol in
   the global hash table.  */

struct mips_elf_link_hash_entry
{
  struct elf_link_hash_entry root;

  /* External symbol information.  */
  EXTR esym;

  /* The la25 stub we have created for ths symbol, if any.  */
  struct mips_elf_la25_stub *la25_stub;

  /* Number of R_RISCV_32, R_RISCV_REL32, or R_RISCV_64 relocs against
     this symbol.  */
  unsigned int possibly_dynamic_relocs;

#define GOT_NORMAL	0
#define GOT_TLS_GD	1
#define GOT_TLS_LDM	2
#define GOT_TLS_IE	4
#define GOT_TLS_OFFSET_DONE    0x40
#define GOT_TLS_DONE    0x80
  unsigned char tls_type;

  /* This is only used in single-GOT mode; in multi-GOT mode there
     is one mips_got_entry per GOT entry, so the offset is stored
     there.  In single-GOT mode there may be many mips_got_entry
     structures all referring to the same GOT slot.  It might be
     possible to use root.got.offset instead, but that field is
     overloaded already.  */
  bfd_vma tls_got_offset;

  /* The highest GGA_* value that satisfies all references to this symbol.  */
  unsigned int global_got_area : 2;

  /* True if all GOT relocations against this symbol are for calls.  This is
     a looser condition than no_fn_stub below, because there may be other
     non-call non-GOT relocations against the symbol.  */
  unsigned int got_only_for_calls : 1;

  /* True if one of the relocations described by possibly_dynamic_relocs
     is against a readonly section.  */
  unsigned int readonly_reloc : 1;

  /* True if there is a relocation against this symbol that must be
     resolved by the static linker (in other words, if the relocation
     cannot possibly be made dynamic).  */
  unsigned int has_static_relocs : 1;

  /* True if we must not create a .MIPS.stubs entry for this symbol.
     This is set, for example, if there are relocations related to
     taking the function's address, i.e. any but R_RISCV_CALL*16 ones.
     See "MIPS ABI Supplement, 3rd Edition", p. 4-20.  */
  unsigned int no_fn_stub : 1;

  /* True if this symbol is referenced by branch relocations from
     any non-PIC input file.  This is used to determine whether an
     la25 stub is required.  */
  unsigned int has_nonpic_branches : 1;

  /* Does this symbol need a traditional MIPS lazy-binding stub
     (as opposed to a PLT entry)?  */
  unsigned int needs_lazy_stub : 1;
};

/* MIPS ELF linker hash table.  */

struct mips_elf_link_hash_table
{
  struct elf_link_hash_table root;
#if 0
  /* We no longer use this.  */
  /* String section indices for the dynamic section symbols.  */
  bfd_size_type dynsym_sec_strindex[SIZEOF_MIPS_DYNSYM_SECNAMES];
#endif

  /* The number of .rtproc entries.  */
  bfd_size_type procedure_count;

  /* This flag indicates that the value of DT_MIPS_RLD_MAP dynamic
     entry is set to the address of __rld_obj_head as in IRIX5.  */
  bfd_boolean use_rld_obj_head;

  /* This is the value of the __rld_map or __rld_obj_head symbol.  */
  bfd_vma rld_value;

  /* True if we can generate copy relocs and PLTs.  */
  bfd_boolean use_plts_and_copy_relocs;

  /* Shortcuts to some dynamic sections, or NULL if they are not
     being used.  */
  asection *srelbss;
  asection *sdynbss;
  asection *srelplt;
  asection *srelplt2;
  asection *sgotplt;
  asection *splt;
  asection *sstubs;
  asection *sgot;

  /* The master GOT information.  */
  struct mips_got_info *got_info;

  /* The size of the PLT header in bytes.  */
  bfd_vma plt_header_size;

  /* The size of a PLT entry in bytes.  */
  bfd_vma plt_entry_size;

  /* The number of functions that need a lazy-binding stub.  */
  bfd_vma lazy_stub_count;

  /* The size of a function stub entry in bytes.  */
  bfd_vma function_stub_size;

  /* The number of reserved entries at the beginning of the GOT.  */
  unsigned int reserved_gotno;

  /* The section used for mips_elf_la25_stub trampolines.
     See the comment above that structure for details.  */
  asection *strampoline;

  /* A table of mips_elf_la25_stubs, indexed by (input_section, offset)
     pairs.  */
  htab_t la25_stubs;

  /* A function FN (NAME, IS, OS) that creates a new input section
     called NAME and links it to output section OS.  If IS is nonnull,
     the new section should go immediately before it, otherwise it
     should go at the (current) beginning of OS.

     The function returns the new section on success, otherwise it
     returns null.  */
  asection *(*add_stub_section) (const char *, asection *, asection *);
};

/* Get the MIPS ELF linker hash table from a link_info structure.  */

#define mips_elf_hash_table(p) \
  (elf_hash_table_id ((struct elf_link_hash_table *) ((p)->hash)) \
  == MIPS_ELF_DATA ? ((struct mips_elf_link_hash_table *) ((p)->hash)) : NULL)

/* A structure used to communicate with htab_traverse callbacks.  */
struct mips_htab_traverse_info
{
  /* The usual link-wide information.  */
  struct bfd_link_info *info;
  bfd *output_bfd;

  /* Starts off FALSE and is set to TRUE if the link should be aborted.  */
  bfd_boolean error;
};

#define TLS_RELOC_P(r_type) \
  (r_type == R_RISCV_TLS_DTPMOD32		\
   || r_type == R_RISCV_TLS_DTPMOD64		\
   || r_type == R_RISCV_TLS_DTPREL32		\
   || r_type == R_RISCV_TLS_DTPREL64		\
   || TLS_GD_RELOC_P(r_type)			\
   || TLS_LDM_RELOC_P(r_type)			\
   || r_type == R_RISCV_TLS_LDM			\
   || r_type == R_RISCV_TLS_DTPREL_HI16		\
   || r_type == R_RISCV_TLS_DTPREL_LO16		\
   || TLS_GOTTPREL_RELOC_P(r_type)		\
   || r_type == R_RISCV_TLS_TPREL32		\
   || r_type == R_RISCV_TLS_TPREL64		\
   || r_type == R_RISCV_TLS_TPREL_HI16		\
   || r_type == R_RISCV_TLS_TPREL_LO16)

#define TLS_GOTTPREL_RELOC_P(r_type) \
  ((r_type) == R_RISCV_TLS_GOTTPREL		\
   || (r_type) == R_RISCV_TLS_GOT_HI16		\
   || (r_type) == R_RISCV_TLS_GOT_LO16)

#define TLS_GD_RELOC_P(r_type) \
  ((r_type) == R_RISCV_TLS_GD			\
   || (r_type) == R_RISCV_TLS_GD_HI16		\
   || (r_type) == R_RISCV_TLS_GD_LO16)

#define TLS_LDM_RELOC_P(r_type) \
  ((r_type) == R_RISCV_TLS_LDM			\
   || (r_type) == R_RISCV_TLS_LDM_HI16		\
   || (r_type) == R_RISCV_TLS_LDM_LO16)

/* Structure used to pass information to mips_elf_output_extsym.  */

struct extsym_info
{
  bfd *abfd;
  struct bfd_link_info *info;
  struct ecoff_debug_info *debug;
  const struct ecoff_debug_swap *swap;
  bfd_boolean failed;
};

/* The structure of the runtime procedure descriptor created by the
   loader for use by the static exception system.  */

typedef struct runtime_pdr {
	bfd_vma	adr;		/* Memory address of start of procedure.  */
	long	regmask;	/* Save register mask.  */
	long	regoffset;	/* Save register offset.  */
	long	fregmask;	/* Save floating point register mask.  */
	long	fregoffset;	/* Save floating point register offset.  */
	long	frameoffset;	/* Frame size.  */
	short	framereg;	/* Frame pointer register.  */
	short	pcreg;		/* Offset or reg of return pc.  */
	long	irpss;		/* Index into the runtime string table.  */
	long	reserved;
	struct exception_info *exception_info;/* Pointer to exception array.  */
} RPDR, *pRPDR;
#define cbRPDR sizeof (RPDR)
#define rpdNil ((pRPDR) 0)

static struct mips_got_entry *mips_elf_create_local_got_entry
  (bfd *, struct bfd_link_info *, bfd *, bfd_vma, unsigned long,
   struct mips_elf_link_hash_entry *, int);
static bfd_boolean mips_elf_sort_hash_table_f
  (struct mips_elf_link_hash_entry *, void *);
static bfd_boolean mips_elf_create_dynamic_relocation
  (bfd *, struct bfd_link_info *, const Elf_Internal_Rela *,
   struct mips_elf_link_hash_entry *, asection *, bfd_vma,
   bfd_vma *, asection *);
static hashval_t mips_elf_got_entry_hash
  (const void *);

/* This will be used when we sort the dynamic relocation records.  */
static bfd *reldyn_sorting_bfd;

/* True if ABFD is a PIC object.  */
#define PIC_OBJECT_P(abfd) \
  ((elf_elfheader (abfd)->e_flags & EF_MIPS_PIC) != 0)

/* Nonzero if ABFD is using the RV64 ABI.  */
#define ABI_64_P(abfd) \
  (get_elf_backend_data (abfd)->s->elfclass == ELFCLASS64)

/* Nonzero if ABFD is using the RV32 ABI.  */
#define ABI_32_P(abfd) (!ABI_64_P(abfd))

/* The name of the options section.  */
#define MIPS_ELF_OPTIONS_SECTION_NAME(abfd) ".MIPS.options"

/* True if NAME is the recognized name of any SHT_MIPS_OPTIONS section.
   Some IRIX system files do not use MIPS_ELF_OPTIONS_SECTION_NAME.  */
#define MIPS_ELF_OPTIONS_SECTION_NAME_P(NAME) \
  (strcmp (NAME, ".MIPS.options") == 0)

/* Whether the section is readonly.  */
#define MIPS_ELF_READONLY_SECTION(sec) \
  ((sec->flags & (SEC_ALLOC | SEC_LOAD | SEC_READONLY))		\
   == (SEC_ALLOC | SEC_LOAD | SEC_READONLY))

/* The name of the stub section.  */
#define MIPS_ELF_STUB_SECTION_NAME(abfd) ".MIPS.stubs"

/* The size of an external REL relocation.  */
#define MIPS_ELF_REL_SIZE(abfd) \
  (get_elf_backend_data (abfd)->s->sizeof_rel)

/* The size of an external dynamic table entry.  */
#define MIPS_ELF_DYN_SIZE(abfd) \
  (get_elf_backend_data (abfd)->s->sizeof_dyn)

/* The size of a GOT entry.  */
#define MIPS_ELF_GOT_SIZE(abfd) \
  (get_elf_backend_data (abfd)->s->arch_size / 8)

/* The size of a symbol-table entry.  */
#define MIPS_ELF_SYM_SIZE(abfd) \
  (get_elf_backend_data (abfd)->s->sizeof_sym)

/* The default alignment for sections, as a power of two.  */
#define MIPS_ELF_LOG_FILE_ALIGN(abfd)				\
  (get_elf_backend_data (abfd)->s->log_file_align)

/* Get word-sized data.  */
#define MIPS_ELF_GET_WORD(abfd, ptr) \
  (ABI_64_P (abfd) ? bfd_get_64 (abfd, ptr) : bfd_get_32 (abfd, ptr))

/* Put out word-sized data.  */
#define MIPS_ELF_PUT_WORD(abfd, val, ptr)	\
  (ABI_64_P (abfd) 				\
   ? bfd_put_64 (abfd, val, ptr) 		\
   : bfd_put_32 (abfd, val, ptr))

/* Add a dynamic symbol table-entry.  */
#define MIPS_ELF_ADD_DYNAMIC_ENTRY(info, tag, val)	\
  _bfd_elf_add_dynamic_entry (info, tag, val)

#define MIPS_ELF_RTYPE_TO_HOWTO(abfd, rtype, rela)			\
  (get_elf_backend_data (abfd)->elf_backend_mips_rtype_to_howto (rtype, rela))

/* The name of the dynamic relocation section.  */
#define MIPS_ELF_REL_DYN_NAME(INFO) ".rel.dyn"

/* In case we're on a 32-bit machine, construct a 64-bit "-1" value
   from smaller values.  Start with zero, widen, *then* decrement.  */
#define MINUS_ONE	(((bfd_vma)0) - 1)
#define MINUS_TWO	(((bfd_vma)0) - 2)

/* The value to write into got[1] for SVR4 targets, to identify it is
   a GNU object.  The dynamic linker can then use got[1] to store the
   module pointer.  */
#define MIPS_ELF_GNU_GOT1_MASK(abfd) \
  ((bfd_vma) 1 << (ABI_64_P (abfd) ? 63 : 31))

#define MIPS_FUNCTION_STUB_NORMAL_SIZE 16
#define MIPS_FUNCTION_STUB_BIG_SIZE 20

/* The name of the dynamic interpreter.  This is put in the .interp
   section.  */

#define ELF_DYNAMIC_INTERPRETER(abfd) 		\
   (ABI_64_P (abfd) ? "/lib/ld.so.1" 	\
    : "/lib32/ld.so.1")

#ifdef BFD64
#define MNAME(bfd,pre,pos) \
  (ABI_64_P (bfd) ? CONCAT4 (pre,64,_,pos) : CONCAT4 (pre,32,_,pos))
#define ELF_R_SYM(bfd, i)					\
  (ABI_64_P (bfd) ? ELF64_R_SYM (i) : ELF32_R_SYM (i))
#define ELF_R_TYPE(bfd, i)					\
  (ABI_64_P (bfd) ? ELF64_MIPS_R_TYPE (i) : ELF32_R_TYPE (i))
#define ELF_R_INFO(bfd, s, t)					\
  (ABI_64_P (bfd) ? ELF64_R_INFO (s, t) : ELF32_R_INFO (s, t))
#else
#define MNAME(bfd,pre,pos) CONCAT4 (pre,32,_,pos)
#define ELF_R_SYM(bfd, i)					\
  (ELF32_R_SYM (i))
#define ELF_R_TYPE(bfd, i)					\
  (ELF32_R_TYPE (i))
#define ELF_R_INFO(bfd, s, t)					\
  (ELF32_R_INFO (s, t))
#endif

#define IS_STORE_RELOC(bfd, reloc, opcode) \
  ((ELF_R_TYPE (bfd, reloc) == R_RISCV_LO16		\
    || ELF_R_TYPE (bfd, reloc) == R_RISCV_TLS_TPREL_LO16	\
    || ELF_R_TYPE (bfd, reloc) == R_RISCV_TLS_DTPREL_LO16)	\
   && OPCODE_IS_STORE(opcode))

#define MATCH_LREG(abfd) (ABI_64_P(abfd) ? MATCH_LD : MATCH_LW)

#define OPCODE_MATCHES(OPCODE, OP) \
  (((OPCODE) & MASK_##OP) == MATCH_##OP)

#define OPCODE_IS_STORE(OPCODE) \
  (OPCODE_MATCHES(OPCODE, SD)  || OPCODE_MATCHES(OPCODE, SW) || \
   OPCODE_MATCHES(OPCODE, SH)  || OPCODE_MATCHES(OPCODE, SB) || \
   OPCODE_MATCHES(OPCODE, FSW) || OPCODE_MATCHES(OPCODE, FSD))

/* The format of the first PLT entry.  */

#define RISCV_PLT0_ENTRY_INSNS 8
static void
riscv_make_plt0_entry(bfd* abfd, bfd_vma gotplt_value,
                      bfd_vma entry[RISCV_PLT0_ENTRY_INSNS])
{
  /* lui    t2, %hi(GOTPLT)
     l[w|d] t7, %lo(GOTPLT)(t2)
     addi   t2, t2, %lo(GOTPLT)
     sub    t6, t6, t2
     move   t5, ra
     srli   t6, t6, (RV32 ? 2 : 3)
     addi   t6, t6, -2
     jalr   t7
  */

  entry[0] = RISCV_LTYPE(LUI, 14, RISCV_LUI_HIGH_PART(gotplt_value));
  entry[1] = RISCV_ITYPE(LREG(abfd),  19, 14, RISCV_CONST_LOW_PART(gotplt_value));
  entry[2] = RISCV_ITYPE(ADDI, 14, 14, RISCV_CONST_LOW_PART(gotplt_value));
  entry[3] = RISCV_RTYPE(SUB, 18, 18, 14);
  entry[4] = RISCV_ITYPE(ADDI, 17, LINK_REG, 0);
  entry[5] = RISCV_ITYPE(SRLI, 18, 18, ABI_64_P(abfd) ? 3 : 2);
  entry[6] = RISCV_ITYPE(ADDI, 18, 18, -2);
  entry[7] = RISCV_ITYPE(JALR_C, LINK_REG, 19, 0);
}

/* The format of subsequent PLT entries.  */

#define RISCV_PLT_ENTRY_INSNS 4
static void
riscv_make_plt_entry(bfd* abfd, bfd_vma got_address,
                     bfd_vma entry[RISCV_PLT_ENTRY_INSNS])
{
  /* lui    t6, %hi(.got.plt entry)
     l[w|d] t7, %lo(.got.plt entry)(t6)
     addi   t6, t6, %lo(.got.plt entry)
     jr     t7
  */

  entry[0] = RISCV_LTYPE(LUI, 18, RISCV_LUI_HIGH_PART(got_address));
  entry[1] = RISCV_ITYPE(LREG(abfd),  19, 18, RISCV_CONST_LOW_PART(got_address));
  entry[2] = RISCV_ITYPE(ADDI, 18, 18, RISCV_CONST_LOW_PART(got_address));
  entry[3] = RISCV_ITYPE(JALR_J, 0, 19, 0);
}

/* Look up an entry in a MIPS ELF linker hash table.  */

#define mips_elf_link_hash_lookup(table, string, create, copy, follow)	\
  ((struct mips_elf_link_hash_entry *)					\
   elf_link_hash_lookup (&(table)->root, (string), (create),		\
			 (copy), (follow)))

/* Traverse a MIPS ELF linker hash table.  */

#define mips_elf_link_hash_traverse(table, func, info)			\
  (elf_link_hash_traverse						\
   (&(table)->root,							\
    (bfd_boolean (*) (struct elf_link_hash_entry *, void *)) (func),	\
    (info)))

/* Find the base offsets for thread-local storage in this object,
   for GD/LD and IE/LE respectively.  */

#define TP_OFFSET 0x7000
#define DTP_OFFSET 0x8000

static bfd_vma
dtprel_base (struct bfd_link_info *info)
{
  /* If tls_sec is NULL, we should have signalled an error already.  */
  if (elf_hash_table (info)->tls_sec == NULL)
    return 0;
  return elf_hash_table (info)->tls_sec->vma + DTP_OFFSET;
}

static bfd_vma
tprel_base (struct bfd_link_info *info)
{
  /* If tls_sec is NULL, we should have signalled an error already.  */
  if (elf_hash_table (info)->tls_sec == NULL)
    return 0;
  return elf_hash_table (info)->tls_sec->vma + TP_OFFSET;
}

/* Create an entry in a MIPS ELF linker hash table.  */

static struct bfd_hash_entry *
mips_elf_link_hash_newfunc (struct bfd_hash_entry *entry,
			    struct bfd_hash_table *table, const char *string)
{
  struct mips_elf_link_hash_entry *ret =
    (struct mips_elf_link_hash_entry *) entry;

  /* Allocate the structure if it has not already been allocated by a
     subclass.  */
  if (ret == NULL)
    ret = bfd_hash_allocate (table, sizeof (struct mips_elf_link_hash_entry));
  if (ret == NULL)
    return (struct bfd_hash_entry *) ret;

  /* Call the allocation method of the superclass.  */
  ret = ((struct mips_elf_link_hash_entry *)
	 _bfd_elf_link_hash_newfunc ((struct bfd_hash_entry *) ret,
				     table, string));
  if (ret != NULL)
    {
      /* Set local fields.  */
      memset (&ret->esym, 0, sizeof (EXTR));
      /* We use -2 as a marker to indicate that the information has
	 not been set.  -1 means there is no associated ifd.  */
      ret->esym.ifd = -2;
      ret->la25_stub = 0;
      ret->possibly_dynamic_relocs = 0;
      ret->tls_type = GOT_NORMAL;
      ret->global_got_area = GGA_NONE;
      ret->got_only_for_calls = TRUE;
      ret->readonly_reloc = FALSE;
      ret->has_static_relocs = FALSE;
      ret->no_fn_stub = FALSE;
      ret->has_nonpic_branches = FALSE;
      ret->needs_lazy_stub = FALSE;
    }

  return (struct bfd_hash_entry *) ret;
}

bfd_boolean
_bfd_riscv_elf_new_section_hook (bfd *abfd, asection *sec)
{
  if (!sec->used_by_bfd)
    {
      struct _mips_elf_section_data *sdata;
      bfd_size_type amt = sizeof (*sdata);

      sdata = bfd_zalloc (abfd, amt);
      if (sdata == NULL)
	return FALSE;
      sec->used_by_bfd = sdata;
    }

  return _bfd_elf_new_section_hook (abfd, sec);
}

/* Read ECOFF debugging information from a .mdebug section into a
   ecoff_debug_info structure.  */

bfd_boolean
_bfd_riscv_elf_read_ecoff_info (bfd *abfd, asection *section,
			       struct ecoff_debug_info *debug)
{
  HDRR *symhdr;
  const struct ecoff_debug_swap *swap;
  char *ext_hdr;

  swap = get_elf_backend_data (abfd)->elf_backend_ecoff_debug_swap;
  memset (debug, 0, sizeof (*debug));

  ext_hdr = bfd_malloc (swap->external_hdr_size);
  if (ext_hdr == NULL && swap->external_hdr_size != 0)
    goto error_return;

  if (! bfd_get_section_contents (abfd, section, ext_hdr, 0,
				  swap->external_hdr_size))
    goto error_return;

  symhdr = &debug->symbolic_header;
  (*swap->swap_hdr_in) (abfd, ext_hdr, symhdr);

  /* The symbolic header contains absolute file offsets and sizes to
     read.  */
#define READ(ptr, offset, count, size, type)				\
  if (symhdr->count == 0)						\
    debug->ptr = NULL;							\
  else									\
    {									\
      bfd_size_type amt = (bfd_size_type) size * symhdr->count;		\
      debug->ptr = bfd_malloc (amt);					\
      if (debug->ptr == NULL)						\
	goto error_return;						\
      if (bfd_seek (abfd, symhdr->offset, SEEK_SET) != 0		\
	  || bfd_bread (debug->ptr, amt, abfd) != amt)			\
	goto error_return;						\
    }

  READ (line, cbLineOffset, cbLine, sizeof (unsigned char), unsigned char *);
  READ (external_dnr, cbDnOffset, idnMax, swap->external_dnr_size, void *);
  READ (external_pdr, cbPdOffset, ipdMax, swap->external_pdr_size, void *);
  READ (external_sym, cbSymOffset, isymMax, swap->external_sym_size, void *);
  READ (external_opt, cbOptOffset, ioptMax, swap->external_opt_size, void *);
  READ (external_aux, cbAuxOffset, iauxMax, sizeof (union aux_ext),
	union aux_ext *);
  READ (ss, cbSsOffset, issMax, sizeof (char), char *);
  READ (ssext, cbSsExtOffset, issExtMax, sizeof (char), char *);
  READ (external_fdr, cbFdOffset, ifdMax, swap->external_fdr_size, void *);
  READ (external_rfd, cbRfdOffset, crfd, swap->external_rfd_size, void *);
  READ (external_ext, cbExtOffset, iextMax, swap->external_ext_size, void *);
#undef READ

  debug->fdr = NULL;

  return TRUE;

 error_return:
  if (ext_hdr != NULL)
    free (ext_hdr);
  if (debug->line != NULL)
    free (debug->line);
  if (debug->external_dnr != NULL)
    free (debug->external_dnr);
  if (debug->external_pdr != NULL)
    free (debug->external_pdr);
  if (debug->external_sym != NULL)
    free (debug->external_sym);
  if (debug->external_opt != NULL)
    free (debug->external_opt);
  if (debug->external_aux != NULL)
    free (debug->external_aux);
  if (debug->ss != NULL)
    free (debug->ss);
  if (debug->ssext != NULL)
    free (debug->ssext);
  if (debug->external_fdr != NULL)
    free (debug->external_fdr);
  if (debug->external_rfd != NULL)
    free (debug->external_rfd);
  if (debug->external_ext != NULL)
    free (debug->external_ext);
  return FALSE;
}

/* We're going to create a stub for H.  Create a symbol for the stub's
   value and size, to help make the disassembly easier to read.  */

static bfd_boolean
mips_elf_create_stub_symbol (struct bfd_link_info *info,
			     struct mips_elf_link_hash_entry *h,
			     const char *prefix, asection *s, bfd_vma value,
			     bfd_vma size)
{
  struct bfd_link_hash_entry *bh;
  struct elf_link_hash_entry *elfh;
  const char *name;

  /* Create a new symbol.  */
  name = ACONCAT ((prefix, h->root.root.root.string, NULL));
  bh = NULL;
  if (!_bfd_generic_link_add_one_symbol (info, s->owner, name,
					 BSF_LOCAL, s, value, NULL,
					 TRUE, FALSE, &bh))
    return FALSE;

  /* Make it a local function.  */
  elfh = (struct elf_link_hash_entry *) bh;
  elfh->type = ELF_ST_INFO (STB_LOCAL, STT_FUNC);
  elfh->size = size;
  elfh->forced_local = 1;
  return TRUE;
}

/* Hashtable callbacks for mips_elf_la25_stubs.  */

static hashval_t
mips_elf_la25_stub_hash (const void *entry_)
{
  const struct mips_elf_la25_stub *entry;

  entry = (struct mips_elf_la25_stub *) entry_;
  return entry->h->root.root.u.def.section->id
    + entry->h->root.root.u.def.value;
}

static int
mips_elf_la25_stub_eq (const void *entry1_, const void *entry2_)
{
  const struct mips_elf_la25_stub *entry1, *entry2;

  entry1 = (struct mips_elf_la25_stub *) entry1_;
  entry2 = (struct mips_elf_la25_stub *) entry2_;
  return ((entry1->h->root.root.u.def.section
	   == entry2->h->root.root.u.def.section)
	  && (entry1->h->root.root.u.def.value
	      == entry2->h->root.root.u.def.value));
}

/* Called by the linker to set up the la25 stub-creation code.  FN is
   the linker's implementation of add_stub_function.  Return true on
   success.  */

bfd_boolean
_bfd_riscv_elf_init_stubs (struct bfd_link_info *info,
			  asection *(*fn) (const char *, asection *,
					   asection *))
{
  struct mips_elf_link_hash_table *htab;

  htab = mips_elf_hash_table (info);
  if (htab == NULL)
    return FALSE;

  htab->add_stub_section = fn;
  htab->la25_stubs = htab_try_create (1, mips_elf_la25_stub_hash,
				      mips_elf_la25_stub_eq, NULL);
  if (htab->la25_stubs == NULL)
    return FALSE;

  return TRUE;
}

/* Return true if H is a locally-defined PIC function, in the sense
   that it might need $25 to be valid on entry.  Note that MIPS16
   functions never need $25 to be valid on entry; they set up $gp
   using PC-relative instructions instead.  */

static bfd_boolean
mips_elf_local_pic_function_p (struct mips_elf_link_hash_entry *h)
{
  return ((h->root.root.type == bfd_link_hash_defined
	   || h->root.root.type == bfd_link_hash_defweak)
	  && h->root.def_regular
	  && !bfd_is_abs_section (h->root.root.u.def.section)
	  && !ELF_ST_IS_MIPS16 (h->root.other)
	  && (PIC_OBJECT_P (h->root.root.u.def.section->owner)
	      || ELF_ST_IS_MIPS_PIC (h->root.other)));
}

/* STUB describes an la25 stub that we have decided to implement
   by inserting RDNPC before the target function.
   Create the section and redirect the function symbol to it.  */

static bfd_boolean
mips_elf_add_la25_intro (struct mips_elf_la25_stub *stub,
			 struct bfd_link_info *info)
{
  struct mips_elf_link_hash_table *htab;
  char *name;
  asection *s, *input_section;
  unsigned int align;

  htab = mips_elf_hash_table (info);
  if (htab == NULL)
    return FALSE;

  /* Create a unique name for the new section.  */
  name = bfd_malloc (11 + sizeof (".text.stub."));
  if (name == NULL)
    return FALSE;
  sprintf (name, ".text.stub.%d", (int) htab_elements (htab->la25_stubs));

  /* Create the section.  */
  input_section = stub->h->root.root.u.def.section;
  s = htab->add_stub_section (name, input_section,
			      input_section->output_section);
  if (s == NULL)
    return FALSE;

  /* Make sure that any padding goes before the stub.  */
  align = input_section->alignment_power;
  if (!bfd_set_section_alignment (s->owner, s, align))
    return FALSE;
  if (align > 2)
    s->size = (1 << align) - 4;

  /* Create a symbol for the stub.  */
  mips_elf_create_stub_symbol (info, stub->h, ".pic.", s, s->size, 8);
  stub->stub_section = s;
  stub->offset = s->size;

  /* Allocate room for it.  */
  s->size += 4;
  return TRUE;
}

/* STUB describes an la25 stub that we have decided to implement
   with a separate trampoline.  Allocate room for it and redirect
   the function symbol to it.  */

static bfd_boolean
mips_elf_add_la25_trampoline (struct mips_elf_la25_stub *stub,
			      struct bfd_link_info *info)
{
  struct mips_elf_link_hash_table *htab;
  asection *s;

  htab = mips_elf_hash_table (info);
  if (htab == NULL)
    return FALSE;

  /* Create a trampoline section, if we haven't already.  */
  s = htab->strampoline;
  if (s == NULL)
    {
      asection *input_section = stub->h->root.root.u.def.section;
      s = htab->add_stub_section (".text", NULL,
				  input_section->output_section);
      if (s == NULL || !bfd_set_section_alignment (s->owner, s, 4))
	return FALSE;
      htab->strampoline = s;
    }

  /* Create a symbol for the stub.  */
  mips_elf_create_stub_symbol (info, stub->h, ".pic.", s, s->size, 16);
  stub->stub_section = s;
  stub->offset = s->size;

  /* Allocate room for it.  */
  s->size += 16;
  return TRUE;
}

/* H describes a symbol that needs an la25 stub.  Make sure that an
   appropriate stub exists and point H at it.  */

static bfd_boolean
mips_elf_add_la25_stub (struct bfd_link_info *info,
			struct mips_elf_link_hash_entry *h)
{
  struct mips_elf_link_hash_table *htab;
  struct mips_elf_la25_stub search, *stub;
  bfd_boolean use_trampoline_p;
  asection *s;
  bfd_vma value;
  void **slot;

  /* Prefer to use LUI/ADDIU stubs if the function is at the beginning
     of the section and if we would need no more than 2 nops.  */
  s = h->root.root.u.def.section;
  value = h->root.root.u.def.value;
  use_trampoline_p = (value != 0 || s->alignment_power > 4);

  /* Describe the stub we want.  */
  search.stub_section = NULL;
  search.offset = 0;
  search.h = h;

  /* See if we've already created an equivalent stub.  */
  htab = mips_elf_hash_table (info);
  if (htab == NULL)
    return FALSE;

  slot = htab_find_slot (htab->la25_stubs, &search, INSERT);
  if (slot == NULL)
    return FALSE;

  stub = (struct mips_elf_la25_stub *) *slot;
  if (stub != NULL)
    {
      /* We can reuse the existing stub.  */
      h->la25_stub = stub;
      return TRUE;
    }

  /* Create a permanent copy of ENTRY and add it to the hash table.  */
  stub = bfd_malloc (sizeof (search));
  if (stub == NULL)
    return FALSE;
  *stub = search;
  *slot = stub;

  h->la25_stub = stub;
  return (use_trampoline_p
	  ? mips_elf_add_la25_trampoline (stub, info)
	  : mips_elf_add_la25_intro (stub, info));
}

/* A mips_elf_link_hash_traverse callback that is called before sizing
   sections.  DATA points to a mips_htab_traverse_info structure.  */

static bfd_boolean
mips_elf_check_symbols (struct mips_elf_link_hash_entry *h, void *data)
{
  struct mips_htab_traverse_info *hti;

  hti = (struct mips_htab_traverse_info *) data;
  if (h->root.root.type == bfd_link_hash_warning)
    h = (struct mips_elf_link_hash_entry *) h->root.root.u.i.link;

  if (mips_elf_local_pic_function_p (h))
    {
      /* H is a function that might need $25 to be valid on entry.
	 If we're creating a non-PIC relocatable object, mark H as
	 being PIC.  If we're creating a non-relocatable object with
	 non-PIC branches and jumps to H, make sure that H has an la25
	 stub.  */
      if (hti->info->relocatable)
	{
	  if (!PIC_OBJECT_P (hti->output_bfd))
	    h->root.other = ELF_ST_SET_MIPS_PIC (h->root.other);
	}
      else if (h->has_nonpic_branches && !mips_elf_add_la25_stub (hti->info, h))
	{
	  hti->error = TRUE;
	  return FALSE;
	}
    }
  return TRUE;
}


static inline bfd_boolean
got16_reloc_p (int r_type)
{
  return r_type == R_RISCV_GOT16;
}

static inline bfd_boolean
call16_reloc_p (int r_type)
{
  return r_type == R_RISCV_CALL16;
}

static inline bfd_boolean
hi16_reloc_p (int r_type)
{
  return r_type == R_RISCV_HI16;
}

static inline bfd_boolean
lo16_reloc_p (int r_type)
{
  return r_type == R_RISCV_LO16;
}

static bfd_vma
mips_elf_high (bfd_vma value)
{
  return RISCV_LUI_HIGH_PART (value) & ((1<<RISCV_BIGIMM_BITS)-1);
}

bfd_reloc_status_type
_bfd_riscv_elf_gprel16_with_gp (bfd *abfd, asymbol *symbol,
			       arelent *reloc_entry, asection *input_section,
			       bfd_boolean relocatable, void *data, bfd_vma gp)
{
  bfd_vma relocation;
  bfd_signed_vma val;
  bfd_reloc_status_type status;

  if (bfd_is_com_section (symbol->section))
    relocation = 0;
  else
    relocation = symbol->value;

  relocation += symbol->section->output_section->vma;
  relocation += symbol->section->output_offset;

  if (reloc_entry->address > bfd_get_section_limit (abfd, input_section))
    return bfd_reloc_outofrange;

  /* Set val to the offset into the section or symbol.  */
  val = reloc_entry->addend;

  _bfd_riscv_elf_sign_extend (val, RISCV_IMM_BITS);

  /* Adjust val for the final section location and GP value.  If we
     are producing relocatable output, we don't want to do this for
     an external symbol.  */
  if (! relocatable
      || (symbol->flags & BSF_SECTION_SYM) != 0)
    val += relocation - gp;

  if (reloc_entry->howto->partial_inplace)
    {
      status = _bfd_relocate_contents (reloc_entry->howto, abfd, val,
				       (bfd_byte *) data
				       + reloc_entry->address);
      if (status != bfd_reloc_ok)
	return status;
    }
  else
    reloc_entry->addend = val;

  if (relocatable)
    reloc_entry->address += input_section->output_offset;

  return bfd_reloc_ok;
}

/* Used to store a REL high-part relocation such as R_RISCV_HI16 or
   R_RISCV_GOT16.  REL is the relocation, INPUT_SECTION is the section
   that contains the relocation field and DATA points to the start of
   INPUT_SECTION.  */

struct mips_hi16
{
  struct mips_hi16 *next;
  bfd_byte *data;
  asection *input_section;
  arelent rel;
};

/* FIXME: This should not be a static variable.  */

static struct mips_hi16 *mips_hi16_list;

/* A howto special_function for REL *HI16 relocations.  We can only
   calculate the correct value once we've seen the partnering
   *LO16 relocation, so just save the information for later.

   The ABI requires that the *LO16 immediately follow the *HI16.
   However, as a GNU extension, we permit an arbitrary number of
   *HI16s to be associated with a single *LO16.  This significantly
   simplies the relocation handling in gcc.  */

bfd_reloc_status_type
_bfd_riscv_elf_hi16_reloc (bfd *abfd ATTRIBUTE_UNUSED, arelent *reloc_entry,
			  asymbol *symbol ATTRIBUTE_UNUSED, void *data,
			  asection *input_section, bfd *output_bfd,
			  char **error_message ATTRIBUTE_UNUSED)
{
  struct mips_hi16 *n;

  if (reloc_entry->address > bfd_get_section_limit (abfd, input_section))
    return bfd_reloc_outofrange;

  n = bfd_malloc (sizeof *n);
  if (n == NULL)
    return bfd_reloc_outofrange;

  n->next = mips_hi16_list;
  n->data = data;
  n->input_section = input_section;
  n->rel = *reloc_entry;
  mips_hi16_list = n;

  if (output_bfd != NULL)
    reloc_entry->address += input_section->output_offset;

  return bfd_reloc_ok;
}

/* A howto special_function for REL R_MIPS*_GOT16 relocations.  This is just
   like any other 16-bit relocation when applied to global symbols, but is
   treated in the same as R_RISCV_HI16 when applied to local symbols.  */

bfd_reloc_status_type
_bfd_riscv_elf_got16_reloc (bfd *abfd, arelent *reloc_entry, asymbol *symbol,
			   void *data, asection *input_section,
			   bfd *output_bfd, char **error_message)
{
  if ((symbol->flags & (BSF_GLOBAL | BSF_WEAK)) != 0
      || bfd_is_und_section (bfd_get_section (symbol))
      || bfd_is_com_section (bfd_get_section (symbol)))
    /* The relocation is against a global symbol.  */
    return _bfd_riscv_elf_generic_reloc (abfd, reloc_entry, symbol, data,
					input_section, output_bfd,
					error_message);

  return _bfd_riscv_elf_hi16_reloc (abfd, reloc_entry, symbol, data,
				   input_section, output_bfd, error_message);
}

/* A howto special_function for REL *LO16 relocations.  The *LO16 itself
   is a straightforward 16 bit inplace relocation, but we must deal with
   any partnering high-part relocations as well.  */

bfd_reloc_status_type
_bfd_riscv_elf_lo16_reloc (bfd *abfd, arelent *reloc_entry, asymbol *symbol,
			  void *data, asection *input_section,
			  bfd *output_bfd, char **error_message)
{
  bfd_vma vallo;
  bfd_byte *location = (bfd_byte *) data + reloc_entry->address;

  if (reloc_entry->address > bfd_get_section_limit (abfd, input_section))
    return bfd_reloc_outofrange;

  vallo = bfd_get_32 (abfd, location);

  while (mips_hi16_list != NULL)
    {
      bfd_reloc_status_type ret;
      struct mips_hi16 *hi;

      hi = mips_hi16_list;

      /* R_MIPS*_GOT16 relocations are something of a special case.  We
	 want to install the addend in the same way as for a R_MIPS*_HI16
	 relocation (with a rightshift of 16).  However, since GOT16
	 relocations can also be used with global symbols, their howto
	 has a rightshift of 0.  */
      if (hi->rel.howto->type == R_RISCV_GOT16)
	hi->rel.howto = MIPS_ELF_RTYPE_TO_HOWTO (abfd, R_RISCV_HI16, FALSE);

      /* VALLO is a signed 16-bit number.  Bias it by 0x8000 so that any
	 carry or borrow will induce a change of +1 or -1 in the high part.  */
      hi->rel.addend += (vallo + RISCV_IMM_REACH/2) & (RISCV_IMM_REACH-1);

      ret = _bfd_riscv_elf_generic_reloc (abfd, &hi->rel, symbol, hi->data,
					 hi->input_section, output_bfd,
					 error_message);
      if (ret != bfd_reloc_ok)
	return ret;

      mips_hi16_list = hi->next;
      free (hi);
    }

  return _bfd_riscv_elf_generic_reloc (abfd, reloc_entry, symbol, data,
				      input_section, output_bfd,
				      error_message);
}

/* A generic howto special_function.  This calculates and installs the
   relocation itself, thus avoiding the oft-discussed problems in
   bfd_perform_relocation and bfd_install_relocation.  */

bfd_reloc_status_type
_bfd_riscv_elf_generic_reloc (bfd *abfd ATTRIBUTE_UNUSED, arelent *reloc_entry,
			     asymbol *symbol, void *data ATTRIBUTE_UNUSED,
			     asection *input_section, bfd *output_bfd,
			     char **error_message ATTRIBUTE_UNUSED)
{
  bfd_signed_vma val;
  bfd_reloc_status_type status;
  bfd_boolean relocatable;

  relocatable = (output_bfd != NULL);

  if (reloc_entry->address > bfd_get_section_limit (abfd, input_section))
    return bfd_reloc_outofrange;

  /* Build up the field adjustment in VAL.  */
  val = 0;
  if (!relocatable || (symbol->flags & BSF_SECTION_SYM) != 0)
    {
      /* Either we're calculating the final field value or we have a
	 relocation against a section symbol.  Add in the section's
	 offset or address.  */
      val += symbol->section->output_section->vma;
      val += symbol->section->output_offset;
    }

  if (!relocatable)
    {
      /* We're calculating the final field value.  Add in the symbol's value
	 and, if pc-relative, subtract the address of the field itself.  */
      val += symbol->value;
      if (reloc_entry->howto->pc_relative)
	{
	  val -= input_section->output_section->vma;
	  val -= input_section->output_offset;
	  val -= reloc_entry->address;
	}
    }

  /* VAL is now the final adjustment.  If we're keeping this relocation
     in the output file, and if the relocation uses a separate addend,
     we just need to add VAL to that addend.  Otherwise we need to add
     VAL to the relocation field itself.  */
  if (relocatable && !reloc_entry->howto->partial_inplace)
    reloc_entry->addend += val;
  else
    {
      bfd_byte *loc = (bfd_byte *) data + reloc_entry->address;
      struct reloc_howto_struct howto = *reloc_entry->howto;

      /* Add in the separate addend, if any.  */
      val += reloc_entry->addend;

      /* Fix up dst_mask and value for R_RISCV_LO16 relocs on stores. */
      if (IS_STORE_RELOC (abfd, howto.type,
		bfd_big_endian(abfd) ? bfd_getb32(loc) : bfd_getl32(loc)))
	{
	  val >>= OP_SH_IMMEDIATE;
	  val = ((val >> RISCV_IMMLO_BITS) << OP_SH_IMMHI) |
		((val & ((1<<RISCV_IMMLO_BITS)-1)) << OP_SH_IMMLO);
	  howto.dst_mask = (OP_MASK_IMMHI << OP_SH_IMMHI) | 
			   (OP_MASK_IMMLO << OP_SH_IMMLO);
	}

      /* Add VAL to the reloc field.  */
      status = _bfd_relocate_contents (&howto, abfd, val, loc);

      if (status != bfd_reloc_ok)
	return status;
    }

  if (relocatable)
    reloc_entry->address += input_section->output_offset;

  return bfd_reloc_ok;
}

/* A .reginfo section holds a single Elf32_RegInfo structure.  These
   routines swap this structure in and out.  They are used outside of
   BFD, so they are globally visible.  */

void
bfd_riscv_elf32_swap_reginfo_in (bfd *abfd, const Elf32_External_RegInfo *ex,
				Elf32_RegInfo *in)
{
  in->ri_gprmask = H_GET_32 (abfd, ex->ri_gprmask);
  in->ri_cprmask[0] = H_GET_32 (abfd, ex->ri_cprmask[0]);
  in->ri_cprmask[1] = H_GET_32 (abfd, ex->ri_cprmask[1]);
  in->ri_cprmask[2] = H_GET_32 (abfd, ex->ri_cprmask[2]);
  in->ri_cprmask[3] = H_GET_32 (abfd, ex->ri_cprmask[3]);
  in->ri_gp_value = H_GET_32 (abfd, ex->ri_gp_value);
}

void
bfd_riscv_elf32_swap_reginfo_out (bfd *abfd, const Elf32_RegInfo *in,
				 Elf32_External_RegInfo *ex)
{
  H_PUT_32 (abfd, in->ri_gprmask, ex->ri_gprmask);
  H_PUT_32 (abfd, in->ri_cprmask[0], ex->ri_cprmask[0]);
  H_PUT_32 (abfd, in->ri_cprmask[1], ex->ri_cprmask[1]);
  H_PUT_32 (abfd, in->ri_cprmask[2], ex->ri_cprmask[2]);
  H_PUT_32 (abfd, in->ri_cprmask[3], ex->ri_cprmask[3]);
  H_PUT_32 (abfd, in->ri_gp_value, ex->ri_gp_value);
}

/* In the 64 bit ABI, the .MIPS.options section holds register
   information in an Elf64_Reginfo structure.  These routines swap
   them in and out.  They are globally visible because they are used
   outside of BFD.  These routines are here so that gas can call them
   without worrying about whether the 64 bit ABI has been included.  */

void
bfd_riscv_elf64_swap_reginfo_in (bfd *abfd, const Elf64_External_RegInfo *ex,
				Elf64_Internal_RegInfo *in)
{
  in->ri_gprmask = H_GET_32 (abfd, ex->ri_gprmask);
  in->ri_pad = H_GET_32 (abfd, ex->ri_pad);
  in->ri_cprmask[0] = H_GET_32 (abfd, ex->ri_cprmask[0]);
  in->ri_cprmask[1] = H_GET_32 (abfd, ex->ri_cprmask[1]);
  in->ri_cprmask[2] = H_GET_32 (abfd, ex->ri_cprmask[2]);
  in->ri_cprmask[3] = H_GET_32 (abfd, ex->ri_cprmask[3]);
  in->ri_gp_value = H_GET_64 (abfd, ex->ri_gp_value);
}

void
bfd_riscv_elf64_swap_reginfo_out (bfd *abfd, const Elf64_Internal_RegInfo *in,
				 Elf64_External_RegInfo *ex)
{
  H_PUT_32 (abfd, in->ri_gprmask, ex->ri_gprmask);
  H_PUT_32 (abfd, in->ri_pad, ex->ri_pad);
  H_PUT_32 (abfd, in->ri_cprmask[0], ex->ri_cprmask[0]);
  H_PUT_32 (abfd, in->ri_cprmask[1], ex->ri_cprmask[1]);
  H_PUT_32 (abfd, in->ri_cprmask[2], ex->ri_cprmask[2]);
  H_PUT_32 (abfd, in->ri_cprmask[3], ex->ri_cprmask[3]);
  H_PUT_64 (abfd, in->ri_gp_value, ex->ri_gp_value);
}

/* Swap in an options header.  */

void
bfd_riscv_elf_swap_options_in (bfd *abfd, const Elf_External_Options *ex,
			      Elf_Internal_Options *in)
{
  in->kind = H_GET_8 (abfd, ex->kind);
  in->size = H_GET_8 (abfd, ex->size);
  in->section = H_GET_16 (abfd, ex->section);
  in->info = H_GET_32 (abfd, ex->info);
}

/* Swap out an options header.  */

void
bfd_riscv_elf_swap_options_out (bfd *abfd, const Elf_Internal_Options *in,
			       Elf_External_Options *ex)
{
  H_PUT_8 (abfd, in->kind, ex->kind);
  H_PUT_8 (abfd, in->size, ex->size);
  H_PUT_16 (abfd, in->section, ex->section);
  H_PUT_32 (abfd, in->info, ex->info);
}

/* This function is called via qsort() to sort the dynamic relocation
   entries by increasing r_symndx value.  */

static int
sort_dynamic_relocs (const void *arg1, const void *arg2)
{
  Elf_Internal_Rela int_reloc1;
  Elf_Internal_Rela int_reloc2;
  int diff;

  bfd_elf32_swap_reloc_in (reldyn_sorting_bfd, arg1, &int_reloc1);
  bfd_elf32_swap_reloc_in (reldyn_sorting_bfd, arg2, &int_reloc2);

  diff = ELF32_R_SYM (int_reloc1.r_info) - ELF32_R_SYM (int_reloc2.r_info);
  if (diff != 0)
    return diff;

  if (int_reloc1.r_offset < int_reloc2.r_offset)
    return -1;
  if (int_reloc1.r_offset > int_reloc2.r_offset)
    return 1;
  return 0;
}

/* Like sort_dynamic_relocs, but used for elf64 relocations.  */

static int
sort_dynamic_relocs_64 (const void *arg1 ATTRIBUTE_UNUSED,
			const void *arg2 ATTRIBUTE_UNUSED)
{
#ifdef BFD64
  Elf_Internal_Rela int_reloc1[3];
  Elf_Internal_Rela int_reloc2[3];

  (*get_elf_backend_data (reldyn_sorting_bfd)->s->swap_reloc_in)
    (reldyn_sorting_bfd, arg1, int_reloc1);
  (*get_elf_backend_data (reldyn_sorting_bfd)->s->swap_reloc_in)
    (reldyn_sorting_bfd, arg2, int_reloc2);

  if (ELF64_R_SYM (int_reloc1[0].r_info) < ELF64_R_SYM (int_reloc2[0].r_info))
    return -1;
  if (ELF64_R_SYM (int_reloc1[0].r_info) > ELF64_R_SYM (int_reloc2[0].r_info))
    return 1;

  if (int_reloc1[0].r_offset < int_reloc2[0].r_offset)
    return -1;
  if (int_reloc1[0].r_offset > int_reloc2[0].r_offset)
    return 1;
  return 0;
#else
  abort ();
#endif
}


/* This routine is used to write out ECOFF debugging external symbol
   information.  It is called via mips_elf_link_hash_traverse.  The
   ECOFF external symbol information must match the ELF external
   symbol information.  Unfortunately, at this point we don't know
   whether a symbol is required by reloc information, so the two
   tables may wind up being different.  We must sort out the external
   symbol information before we can set the final size of the .mdebug
   section, and we must set the size of the .mdebug section before we
   can relocate any sections, and we can't know which symbols are
   required by relocation until we relocate the sections.
   Fortunately, it is relatively unlikely that any symbol will be
   stripped but required by a reloc.  In particular, it can not happen
   when generating a final executable.  */

static bfd_boolean
mips_elf_output_extsym (struct mips_elf_link_hash_entry *h, void *data)
{
  struct extsym_info *einfo = data;
  bfd_boolean strip;
  asection *sec, *output_section;

  if (h->root.root.type == bfd_link_hash_warning)
    h = (struct mips_elf_link_hash_entry *) h->root.root.u.i.link;

  if (h->root.indx == -2)
    strip = FALSE;
  else if ((h->root.def_dynamic
	    || h->root.ref_dynamic
	    || h->root.type == bfd_link_hash_new)
	   && !h->root.def_regular
	   && !h->root.ref_regular)
    strip = TRUE;
  else if (einfo->info->strip == strip_all
	   || (einfo->info->strip == strip_some
	       && bfd_hash_lookup (einfo->info->keep_hash,
				   h->root.root.root.string,
				   FALSE, FALSE) == NULL))
    strip = TRUE;
  else
    strip = FALSE;

  if (strip)
    return TRUE;

  if (h->esym.ifd == -2)
    {
      h->esym.jmptbl = 0;
      h->esym.cobol_main = 0;
      h->esym.weakext = 0;
      h->esym.reserved = 0;
      h->esym.ifd = ifdNil;
      h->esym.asym.value = 0;
      h->esym.asym.st = stGlobal;

      if (h->root.root.type == bfd_link_hash_undefined
	  || h->root.root.type == bfd_link_hash_undefweak)
	h->esym.asym.sc = scUndefined;
      else if (h->root.root.type != bfd_link_hash_defined
	  && h->root.root.type != bfd_link_hash_defweak)
	h->esym.asym.sc = scAbs;
      else
	{
	  const char *name;

	  sec = h->root.root.u.def.section;
	  output_section = sec->output_section;

	  /* When making a shared library and symbol h is the one from
	     the another shared library, OUTPUT_SECTION may be null.  */
	  if (output_section == NULL)
	    h->esym.asym.sc = scUndefined;
	  else
	    {
	      name = bfd_section_name (output_section->owner, output_section);

	      if (strcmp (name, ".text") == 0)
		h->esym.asym.sc = scText;
	      else if (strcmp (name, ".data") == 0)
		h->esym.asym.sc = scData;
	      else if (strcmp (name, ".sdata") == 0)
		h->esym.asym.sc = scSData;
	      else if (strcmp (name, ".rodata") == 0
		       || strcmp (name, ".rdata") == 0)
		h->esym.asym.sc = scRData;
	      else if (strcmp (name, ".bss") == 0)
		h->esym.asym.sc = scBss;
	      else if (strcmp (name, ".sbss") == 0)
		h->esym.asym.sc = scSBss;
	      else if (strcmp (name, ".init") == 0)
		h->esym.asym.sc = scInit;
	      else if (strcmp (name, ".fini") == 0)
		h->esym.asym.sc = scFini;
	      else
		h->esym.asym.sc = scAbs;
	    }
	}

      h->esym.asym.reserved = 0;
      h->esym.asym.index = indexNil;
    }

  if (h->root.root.type == bfd_link_hash_common)
    h->esym.asym.value = h->root.root.u.c.size;
  else if (h->root.root.type == bfd_link_hash_defined
	   || h->root.root.type == bfd_link_hash_defweak)
    {
      if (h->esym.asym.sc == scCommon)
	h->esym.asym.sc = scBss;
      else if (h->esym.asym.sc == scSCommon)
	h->esym.asym.sc = scSBss;

      sec = h->root.root.u.def.section;
      output_section = sec->output_section;
      if (output_section != NULL)
	h->esym.asym.value = (h->root.root.u.def.value
			      + sec->output_offset
			      + output_section->vma);
      else
	h->esym.asym.value = 0;
    }
  else
    {
      struct mips_elf_link_hash_entry *hd = h;

      while (hd->root.root.type == bfd_link_hash_indirect)
	hd = (struct mips_elf_link_hash_entry *)h->root.root.u.i.link;

      if (hd->needs_lazy_stub)
	{
	  /* Set type and value for a symbol with a function stub.  */
	  h->esym.asym.st = stProc;
	  sec = hd->root.root.u.def.section;
	  if (sec == NULL)
	    h->esym.asym.value = 0;
	  else
	    {
	      output_section = sec->output_section;
	      if (output_section != NULL)
		h->esym.asym.value = (hd->root.plt.offset
				      + sec->output_offset
				      + output_section->vma);
	      else
		h->esym.asym.value = 0;
	    }
	}
    }

  if (! bfd_ecoff_debug_one_external (einfo->abfd, einfo->debug, einfo->swap,
				      h->root.root.root.string,
				      &h->esym))
    {
      einfo->failed = TRUE;
      return FALSE;
    }

  return TRUE;
}

/* Functions to manage the got entry hash table.  */

/* Use all 64 bits of a bfd_vma for the computation of a 32-bit
   hash number.  */

static INLINE hashval_t
mips_elf_hash_bfd_vma (bfd_vma addr)
{
#ifdef BFD64
  return addr + (addr >> 32);
#else
  return addr;
#endif
}

/* got_entries only match if they're identical, except for gotidx, so
   use all fields to compute the hash, and compare the appropriate
   union members.  */

static hashval_t
mips_elf_got_entry_hash (const void *entry_)
{
  const struct mips_got_entry *entry = (struct mips_got_entry *)entry_;

  return entry->symndx
    + ((entry->tls_type & GOT_TLS_LDM) << 17)
    + (! entry->abfd ? mips_elf_hash_bfd_vma (entry->d.address)
       : entry->abfd->id
         + (entry->symndx >= 0 ? mips_elf_hash_bfd_vma (entry->d.addend)
	    : entry->d.h->root.root.root.hash));
}

static int
mips_elf_got_entry_eq (const void *entry1, const void *entry2)
{
  const struct mips_got_entry *e1 = (struct mips_got_entry *)entry1;
  const struct mips_got_entry *e2 = (struct mips_got_entry *)entry2;

  /* An LDM entry can only match another LDM entry.  */
  if ((e1->tls_type ^ e2->tls_type) & GOT_TLS_LDM)
    return 0;

  return e1->abfd == e2->abfd && e1->symndx == e2->symndx
    && (! e1->abfd ? e1->d.address == e2->d.address
	: e1->symndx >= 0 ? e1->d.addend == e2->d.addend
	: e1->d.h == e2->d.h);
}

static hashval_t
mips_got_page_entry_hash (const void *entry_)
{
  const struct mips_got_page_entry *entry;

  entry = (const struct mips_got_page_entry *) entry_;
  return entry->abfd->id + entry->symndx;
}

static int
mips_got_page_entry_eq (const void *entry1_, const void *entry2_)
{
  const struct mips_got_page_entry *entry1, *entry2;

  entry1 = (const struct mips_got_page_entry *) entry1_;
  entry2 = (const struct mips_got_page_entry *) entry2_;
  return entry1->abfd == entry2->abfd && entry1->symndx == entry2->symndx;
}

/* Return the dynamic relocation section.  If it doesn't exist, try to
   create a new it if CREATE_P, otherwise return NULL.  Also return NULL
   if creation fails.  */

static asection *
mips_elf_rel_dyn_section (struct bfd_link_info *info, bfd_boolean create_p)
{
  const char *dname;
  asection *sreloc;
  bfd *dynobj;

  dname = MIPS_ELF_REL_DYN_NAME (info);
  dynobj = elf_hash_table (info)->dynobj;
  sreloc = bfd_get_section_by_name (dynobj, dname);
  if (sreloc == NULL && create_p)
    {
      sreloc = bfd_make_section_with_flags (dynobj, dname,
					    (SEC_ALLOC
					     | SEC_LOAD
					     | SEC_HAS_CONTENTS
					     | SEC_IN_MEMORY
					     | SEC_LINKER_CREATED
					     | SEC_READONLY));
      if (sreloc == NULL
	  || ! bfd_set_section_alignment (dynobj, sreloc,
					  MIPS_ELF_LOG_FILE_ALIGN (dynobj)))
	return NULL;
    }
  return sreloc;
}

/* Count the number of relocations needed for a TLS GOT entry, with
   access types from TLS_TYPE, and symbol H (or a local symbol if H
   is NULL).  */

static int
mips_tls_got_relocs (struct bfd_link_info *info, unsigned char tls_type,
		     struct elf_link_hash_entry *h)
{
  int indx = 0;
  int ret = 0;
  bfd_boolean need_relocs = FALSE;
  bfd_boolean dyn = elf_hash_table (info)->dynamic_sections_created;

  if (h && WILL_CALL_FINISH_DYNAMIC_SYMBOL (dyn, info->shared, h)
      && (!info->shared || !SYMBOL_REFERENCES_LOCAL (info, h)))
    indx = h->dynindx;

  if ((info->shared || indx != 0)
      && (h == NULL
	  || ELF_ST_VISIBILITY (h->other) == STV_DEFAULT
	  || h->root.type != bfd_link_hash_undefweak))
    need_relocs = TRUE;

  if (!need_relocs)
    return FALSE;

  if (tls_type & GOT_TLS_GD)
    {
      ret++;
      if (indx != 0)
	ret++;
    }

  if (tls_type & GOT_TLS_IE)
    ret++;

  if ((tls_type & GOT_TLS_LDM) && info->shared)
    ret++;

  return ret;
}

/* Count the number of TLS relocations required for the GOT entry in
   ARG1, if it describes a local symbol.  */

static int
mips_elf_count_local_tls_relocs (void **arg1, void *arg2)
{
  struct mips_got_entry *entry = * (struct mips_got_entry **) arg1;
  struct mips_elf_count_tls_arg *arg = arg2;

  if (entry->abfd != NULL && entry->symndx != -1)
    arg->needed += mips_tls_got_relocs (arg->info, entry->tls_type, NULL);

  return 1;
}

/* Count the number of TLS GOT entries required for the global (or
   forced-local) symbol in ARG1.  */

static int
mips_elf_count_global_tls_entries (void *arg1, void *arg2)
{
  struct mips_elf_link_hash_entry *hm
    = (struct mips_elf_link_hash_entry *) arg1;
  struct mips_elf_count_tls_arg *arg = arg2;

  if (hm->tls_type & GOT_TLS_GD)
    arg->needed += 2;
  if (hm->tls_type & GOT_TLS_IE)
    arg->needed += 1;

  return 1;
}

/* Count the number of TLS relocations required for the global (or
   forced-local) symbol in ARG1.  */

static int
mips_elf_count_global_tls_relocs (void *arg1, void *arg2)
{
  struct mips_elf_link_hash_entry *hm
    = (struct mips_elf_link_hash_entry *) arg1;
  struct mips_elf_count_tls_arg *arg = arg2;

  arg->needed += mips_tls_got_relocs (arg->info, hm->tls_type, &hm->root);

  return 1;
}

/* Output a simple dynamic relocation into SRELOC.  */

static void
mips_elf_output_dynamic_relocation (bfd *output_bfd,
				    asection *sreloc,
				    unsigned long reloc_index,
				    unsigned long indx,
				    int r_type,
				    bfd_vma offset)
{
  Elf_Internal_Rela rel[3];

  memset (rel, 0, sizeof (rel));

  rel[0].r_info = ELF_R_INFO (output_bfd, indx, r_type);
  rel[0].r_offset = rel[1].r_offset = rel[2].r_offset = offset;

  if (ABI_64_P (output_bfd))
    {
      (*get_elf_backend_data (output_bfd)->s->swap_reloc_out)
	(output_bfd, &rel[0],
	 (sreloc->contents
	  + reloc_index * sizeof (Elf64_RISCV_External_Rel)));
    }
  else
    bfd_elf32_swap_reloc_out
      (output_bfd, &rel[0],
       (sreloc->contents
	+ reloc_index * sizeof (Elf32_External_Rel)));
}

/* Initialize a set of TLS GOT entries for one symbol.  */

static void
mips_elf_initialize_tls_slots (bfd *abfd, bfd_vma got_offset,
			       unsigned char *tls_type_p,
			       struct bfd_link_info *info,
			       struct mips_elf_link_hash_entry *h,
			       bfd_vma value)
{
  struct mips_elf_link_hash_table *htab;
  int indx;
  asection *sreloc, *sgot;
  bfd_vma offset, offset2;
  bfd_boolean need_relocs = FALSE;

  htab = mips_elf_hash_table (info);
  if (htab == NULL)
    return;

  sgot = htab->sgot;

  indx = 0;
  if (h != NULL)
    {
      bfd_boolean dyn = elf_hash_table (info)->dynamic_sections_created;

      if (WILL_CALL_FINISH_DYNAMIC_SYMBOL (dyn, info->shared, &h->root)
	  && (!info->shared || !SYMBOL_REFERENCES_LOCAL (info, &h->root)))
	indx = h->root.dynindx;
    }

  if (*tls_type_p & GOT_TLS_DONE)
    return;

  if ((info->shared || indx != 0)
      && (h == NULL
	  || ELF_ST_VISIBILITY (h->root.other) == STV_DEFAULT
	  || h->root.type != bfd_link_hash_undefweak))
    need_relocs = TRUE;

  /* MINUS_ONE means the symbol is not defined in this object.  It may not
     be defined at all; assume that the value doesn't matter in that
     case.  Otherwise complain if we would use the value.  */
  BFD_ASSERT (value != MINUS_ONE || (indx != 0 && need_relocs)
	      || h->root.root.type == bfd_link_hash_undefweak);

  /* Emit necessary relocations.  */
  sreloc = mips_elf_rel_dyn_section (info, FALSE);

  /* General Dynamic.  */
  if (*tls_type_p & GOT_TLS_GD)
    {
      offset = got_offset;
      offset2 = offset + MIPS_ELF_GOT_SIZE (abfd);

      if (need_relocs)
	{
	  mips_elf_output_dynamic_relocation
	    (abfd, sreloc, sreloc->reloc_count++, indx,
	     ABI_64_P (abfd) ? R_RISCV_TLS_DTPMOD64 : R_RISCV_TLS_DTPMOD32,
	     sgot->output_offset + sgot->output_section->vma + offset);

	  if (indx)
	    mips_elf_output_dynamic_relocation
	      (abfd, sreloc, sreloc->reloc_count++, indx,
	       ABI_64_P (abfd) ? R_RISCV_TLS_DTPREL64 : R_RISCV_TLS_DTPREL32,
	       sgot->output_offset + sgot->output_section->vma + offset2);
	  else
	    MIPS_ELF_PUT_WORD (abfd, value - dtprel_base (info),
			       sgot->contents + offset2);
	}
      else
	{
	  MIPS_ELF_PUT_WORD (abfd, 1,
			     sgot->contents + offset);
	  MIPS_ELF_PUT_WORD (abfd, value - dtprel_base (info),
			     sgot->contents + offset2);
	}

      got_offset += 2 * MIPS_ELF_GOT_SIZE (abfd);
    }

  /* Initial Exec model.  */
  if (*tls_type_p & GOT_TLS_IE)
    {
      offset = got_offset;

      if (need_relocs)
	{
	  if (indx == 0)
	    MIPS_ELF_PUT_WORD (abfd, value - elf_hash_table (info)->tls_sec->vma,
			       sgot->contents + offset);
	  else
	    MIPS_ELF_PUT_WORD (abfd, 0,
			       sgot->contents + offset);

	  mips_elf_output_dynamic_relocation
	    (abfd, sreloc, sreloc->reloc_count++, indx,
	     ABI_64_P (abfd) ? R_RISCV_TLS_TPREL64 : R_RISCV_TLS_TPREL32,
	     sgot->output_offset + sgot->output_section->vma + offset);
	}
      else
	MIPS_ELF_PUT_WORD (abfd, value - tprel_base (info),
			   sgot->contents + offset);
    }

  if (*tls_type_p & GOT_TLS_LDM)
    {
      /* The initial offset is zero, and the LD offsets will include the
	 bias by DTP_OFFSET.  */
      MIPS_ELF_PUT_WORD (abfd, 0,
			 sgot->contents + got_offset
			 + MIPS_ELF_GOT_SIZE (abfd));

      if (!info->shared)
	MIPS_ELF_PUT_WORD (abfd, 1,
			   sgot->contents + got_offset);
      else
	mips_elf_output_dynamic_relocation
	  (abfd, sreloc, sreloc->reloc_count++, indx,
	   ABI_64_P (abfd) ? R_RISCV_TLS_DTPMOD64 : R_RISCV_TLS_DTPMOD32,
	   sgot->output_offset + sgot->output_section->vma + got_offset);
    }

  *tls_type_p |= GOT_TLS_DONE;
}

/* Return the GOT index to use for a relocation of type R_TYPE against
   a symbol accessed using TLS_TYPE models.  The GOT entries for this
   symbol in this GOT start at GOT_INDEX.  This function initializes the
   GOT entries and corresponding relocations.  */

static bfd_vma
mips_tls_got_index (bfd *abfd, bfd_vma got_index, unsigned char *tls_type,
		    int r_type, struct bfd_link_info *info,
		    struct mips_elf_link_hash_entry *h, bfd_vma symbol)
{
  BFD_ASSERT (TLS_GOTTPREL_RELOC_P(r_type) || TLS_GD_RELOC_P(r_type)
	      || TLS_LDM_RELOC_P(r_type));

  mips_elf_initialize_tls_slots (abfd, got_index, tls_type, info, h, symbol);

  if (TLS_GOTTPREL_RELOC_P(r_type))
    {
      BFD_ASSERT (*tls_type & GOT_TLS_IE);
      if (*tls_type & GOT_TLS_GD)
	return got_index + 2 * MIPS_ELF_GOT_SIZE (abfd);
      else
	return got_index;
    }

  if (TLS_GD_RELOC_P(r_type))
    {
      BFD_ASSERT (*tls_type & GOT_TLS_GD);
      return got_index;
    }

  if (TLS_LDM_RELOC_P(r_type))
    {
      BFD_ASSERT (*tls_type & GOT_TLS_LDM);
      return got_index;
    }

  return got_index;
}

/* Return the GOT offset for address VALUE.   If there is not yet a GOT
   entry for this value, create one.  If R_SYMNDX refers to a TLS symbol,
   create a TLS GOT entry instead.  Return -1 if no satisfactory GOT
   offset can be found.  */

static bfd_vma
mips_elf_local_got_index (bfd *abfd, bfd *ibfd, struct bfd_link_info *info,
			  bfd_vma value, unsigned long r_symndx,
			  struct mips_elf_link_hash_entry *h, int r_type)
{
  struct mips_elf_link_hash_table *htab;
  struct mips_got_entry *entry;

  htab = mips_elf_hash_table (info);
  BFD_ASSERT (htab != NULL);

  entry = mips_elf_create_local_got_entry (abfd, info, ibfd, value,
					   r_symndx, h, r_type);
  if (!entry)
    return MINUS_ONE;

  if (TLS_RELOC_P (r_type))
    {
      if (entry->symndx == -1)
	/* A type (3) entry in the single-GOT case.  We use the symbol's
	   hash table entry to track the index.  */
	return mips_tls_got_index (abfd, h->tls_got_offset, &h->tls_type,
				   r_type, info, h, value);
      else
	return mips_tls_got_index (abfd, entry->gotidx, &entry->tls_type,
				   r_type, info, h, value);
    }
  else
    return entry->gotidx;
}

/* Returns the GOT index for the global symbol indicated by H.  */

static bfd_vma
mips_elf_global_got_index (bfd *abfd, struct elf_link_hash_entry *h,
			   int r_type, struct bfd_link_info *info)
{
  struct mips_elf_link_hash_table *htab;
  bfd_vma got_index;
  struct mips_got_info *g;
  long global_got_dynindx = 0;

  htab = mips_elf_hash_table (info);
  BFD_ASSERT (htab != NULL);

  g = htab->got_info;

  if (g->global_gotsym != NULL)
    global_got_dynindx = g->global_gotsym->dynindx;

  if (TLS_RELOC_P (r_type))
    {
      struct mips_elf_link_hash_entry *hm
	= (struct mips_elf_link_hash_entry *) h;
      bfd_vma value = MINUS_ONE;

      if ((h->root.type == bfd_link_hash_defined
	   || h->root.type == bfd_link_hash_defweak)
	  && h->root.u.def.section->output_section)
	value = (h->root.u.def.value
		 + h->root.u.def.section->output_offset
		 + h->root.u.def.section->output_section->vma);

      got_index = mips_tls_got_index (abfd, hm->tls_got_offset, &hm->tls_type,
				      r_type, info, hm, value);
    }
  else
    {
      /* Once we determine the global GOT entry with the lowest dynamic
	 symbol table index, we must put all dynamic symbols with greater
	 indices into the GOT.  That makes it easy to calculate the GOT
	 offset.  */
      BFD_ASSERT (h->dynindx >= global_got_dynindx);
      got_index = ((h->dynindx - global_got_dynindx + g->local_gotno)
		   * MIPS_ELF_GOT_SIZE (abfd));
    }
  BFD_ASSERT (got_index < htab->sgot->size);

  return got_index;
}

/* Find a local GOT entry for an R_MIPS*_GOT16 relocation against VALUE.
   EXTERNAL is true if the relocation was originally against a global
   symbol that binds locally.  */

static bfd_vma
mips_elf_got16_entry (bfd *abfd, bfd *ibfd, struct bfd_link_info *info,
		      bfd_vma value, bfd_boolean external)
{
  struct mips_got_entry *entry;

  /* GOT16 relocations against local symbols are followed by a LO16
     relocation; those against global symbols are not.  Thus if the
     symbol was originally local, the GOT16 relocation should load the
     equivalent of %hi(VALUE), otherwise it should load VALUE itself.  */
  if (! external)
    value = mips_elf_high (value) << RISCV_IMM_BITS;

  /* It doesn't matter whether the original relocation was R_RISCV_GOT16,
     R_MIPS16_GOT16, R_RISCV_CALL16, etc.  The format of the entry is the
     same in all cases.  */
  entry = mips_elf_create_local_got_entry (abfd, info, ibfd, value, 0,
					   NULL, R_RISCV_GOT16);
  if (entry)
    return entry->gotidx;
  else
    return MINUS_ONE;
}

/* Returns the offset for the entry at the INDEXth position
   in the GOT.  */

static bfd_vma
mips_elf_got_offset_from_index (struct bfd_link_info *info, bfd *output_bfd,
				bfd_vma got_index)
{
  struct mips_elf_link_hash_table *htab;
  asection *sgot;
  bfd_vma gp;

  htab = mips_elf_hash_table (info);
  BFD_ASSERT (htab != NULL);

  sgot = htab->sgot;
  gp = _bfd_get_gp_value (output_bfd);

  return sgot->output_section->vma + sgot->output_offset + got_index - gp;
}

/* Create and return a local GOT entry for VALUE, which was calculated
   from a symbol belonging to INPUT_SECTON.  Return NULL if it could not
   be created.  If R_SYMNDX refers to a TLS symbol, create a TLS entry
   instead.  */

static struct mips_got_entry *
mips_elf_create_local_got_entry (bfd *abfd, struct bfd_link_info *info,
				 bfd *ibfd, bfd_vma value,
				 unsigned long r_symndx,
				 struct mips_elf_link_hash_entry *h,
				 int r_type)
{
  struct mips_got_entry entry, **loc;
  struct mips_got_info *g;
  struct mips_elf_link_hash_table *htab;

  htab = mips_elf_hash_table (info);
  BFD_ASSERT (htab != NULL);

  entry.abfd = NULL;
  entry.symndx = -1;
  entry.d.address = value;
  entry.tls_type = 0;

  g = htab->got_info;

  /* This function shouldn't be called for symbols that live in the global
     area of the GOT.  */
  BFD_ASSERT (h == NULL || h->global_got_area == GGA_NONE);
  if (TLS_RELOC_P (r_type))
    {
      struct mips_got_entry *p;

      entry.abfd = ibfd;
      if (TLS_LDM_RELOC_P(r_type))
	{
	  entry.tls_type = GOT_TLS_LDM;
	  entry.symndx = 0;
	  entry.d.addend = 0;
	}
      else if (h == NULL)
	{
	  entry.symndx = r_symndx;
	  entry.d.addend = 0;
	}
      else
	entry.d.h = h;

      p = (struct mips_got_entry *)
	htab_find (g->got_entries, &entry);

      BFD_ASSERT (p);
      return p;
    }

  loc = (struct mips_got_entry **) htab_find_slot (g->got_entries, &entry,
						   INSERT);
  if (*loc)
    return *loc;

  entry.gotidx = MIPS_ELF_GOT_SIZE (abfd) * g->assigned_gotno++;
  entry.tls_type = 0;

  *loc = (struct mips_got_entry *)bfd_alloc (abfd, sizeof entry);

  if (! *loc)
    return NULL;

  memcpy (*loc, &entry, sizeof entry);

  if (g->assigned_gotno > g->local_gotno)
    {
      (*loc)->gotidx = -1;
      /* We didn't allocate enough space in the GOT.  */
      (*_bfd_error_handler)
	(_("not enough GOT space for local GOT entries"));
      bfd_set_error (bfd_error_bad_value);
      return NULL;
    }

  MIPS_ELF_PUT_WORD (abfd, value,
		     (htab->sgot->contents + entry.gotidx));

  return *loc;
}

/* Return the number of dynamic section symbols required by OUTPUT_BFD.
   The number might be exact or a worst-case estimate, depending on how
   much information is available to elf_backend_omit_section_dynsym at
   the current linking stage.  */

static bfd_size_type
count_section_dynsyms (bfd *output_bfd, struct bfd_link_info *info)
{
  bfd_size_type count;

  count = 0;
  if (info->shared || elf_hash_table (info)->is_relocatable_executable)
    {
      asection *p;
      const struct elf_backend_data *bed;

      bed = get_elf_backend_data (output_bfd);
      for (p = output_bfd->sections; p ; p = p->next)
	if ((p->flags & SEC_EXCLUDE) == 0
	    && (p->flags & SEC_ALLOC) != 0
	    && !(*bed->elf_backend_omit_section_dynsym) (output_bfd, info, p))
	  ++count;
    }
  return count;
}

/* Sort the dynamic symbol table so that symbols that need GOT entries
   appear towards the end.  */

static bfd_boolean
mips_elf_sort_hash_table (bfd *abfd, struct bfd_link_info *info)
{
  struct mips_elf_link_hash_table *htab;
  struct mips_elf_hash_sort_data hsd;
  struct mips_got_info *g;

  if (elf_hash_table (info)->dynsymcount == 0)
    return TRUE;

  htab = mips_elf_hash_table (info);
  BFD_ASSERT (htab != NULL);

  g = htab->got_info;
  if (g == NULL)
    return TRUE;

  hsd.low = NULL;
  hsd.max_unref_got_dynindx
    = hsd.min_got_dynindx
    = (elf_hash_table (info)->dynsymcount - g->reloc_only_gotno);
  hsd.max_non_got_dynindx = count_section_dynsyms (abfd, info) + 1;
  mips_elf_link_hash_traverse (((struct mips_elf_link_hash_table *)
				elf_hash_table (info)),
			       mips_elf_sort_hash_table_f,
			       &hsd);

  /* There should have been enough room in the symbol table to
     accommodate both the GOT and non-GOT symbols.  */
  BFD_ASSERT (hsd.max_non_got_dynindx <= hsd.min_got_dynindx);
  BFD_ASSERT ((unsigned long) hsd.max_unref_got_dynindx
	      == elf_hash_table (info)->dynsymcount);
  BFD_ASSERT (elf_hash_table (info)->dynsymcount - hsd.min_got_dynindx
	      == g->global_gotno);

  /* Now we know which dynamic symbol has the lowest dynamic symbol
     table index in the GOT.  */
  g->global_gotsym = hsd.low;

  return TRUE;
}

/* If H needs a GOT entry, assign it the highest available dynamic
   index.  Otherwise, assign it the lowest available dynamic
   index.  */

static bfd_boolean
mips_elf_sort_hash_table_f (struct mips_elf_link_hash_entry *h, void *data)
{
  struct mips_elf_hash_sort_data *hsd = data;

  if (h->root.root.type == bfd_link_hash_warning)
    h = (struct mips_elf_link_hash_entry *) h->root.root.u.i.link;

  /* Symbols without dynamic symbol table entries aren't interesting
     at all.  */
  if (h->root.dynindx == -1)
    return TRUE;

  switch (h->global_got_area)
    {
    case GGA_NONE:
      h->root.dynindx = hsd->max_non_got_dynindx++;
      break;

    case GGA_NORMAL:
      BFD_ASSERT (h->tls_type == GOT_NORMAL);

      h->root.dynindx = --hsd->min_got_dynindx;
      hsd->low = (struct elf_link_hash_entry *) h;
      break;

    case GGA_RELOC_ONLY:
      BFD_ASSERT (h->tls_type == GOT_NORMAL);

      if (hsd->max_unref_got_dynindx == hsd->min_got_dynindx)
	hsd->low = (struct elf_link_hash_entry *) h;
      h->root.dynindx = hsd->max_unref_got_dynindx++;
      break;
    }

  return TRUE;
}

/* If H is a symbol that needs a global GOT entry, but has a dynamic
   symbol table index lower than any we've seen to date, record it for
   posterity.  FOR_CALL is true if the caller is only interested in
   using the GOT entry for calls.  */

static bfd_boolean
mips_elf_record_global_got_symbol (struct elf_link_hash_entry *h,
				   bfd *abfd, struct bfd_link_info *info,
				   bfd_boolean for_call,
				   unsigned char tls_flag)
{
  struct mips_elf_link_hash_table *htab;
  struct mips_elf_link_hash_entry *hmips;
  struct mips_got_entry entry, **loc;
  struct mips_got_info *g;

  htab = mips_elf_hash_table (info);
  BFD_ASSERT (htab != NULL);

  hmips = (struct mips_elf_link_hash_entry *) h;
  if (!for_call)
    hmips->got_only_for_calls = FALSE;

  /* A global symbol in the GOT must also be in the dynamic symbol
     table.  */
  if (h->dynindx == -1)
    {
      switch (ELF_ST_VISIBILITY (h->other))
	{
	case STV_INTERNAL:
	case STV_HIDDEN:
	  _bfd_elf_link_hash_hide_symbol (info, h, TRUE);
	  break;
	}
      if (!bfd_elf_link_record_dynamic_symbol (info, h))
	return FALSE;
    }

  /* Make sure we have a GOT to put this entry into.  */
  g = htab->got_info;
  BFD_ASSERT (g != NULL);

  entry.abfd = abfd;
  entry.symndx = -1;
  entry.d.h = (struct mips_elf_link_hash_entry *) h;
  entry.tls_type = 0;

  loc = (struct mips_got_entry **) htab_find_slot (g->got_entries, &entry,
						   INSERT);

  /* If we've already marked this entry as needing GOT space, we don't
     need to do it again.  */
  if (*loc)
    {
      (*loc)->tls_type |= tls_flag;
      return TRUE;
    }

  *loc = (struct mips_got_entry *)bfd_alloc (abfd, sizeof entry);

  if (! *loc)
    return FALSE;

  entry.gotidx = -1;
  entry.tls_type = tls_flag;

  memcpy (*loc, &entry, sizeof entry);

  if (tls_flag == 0)
    hmips->global_got_area = GGA_NORMAL;

  return TRUE;
}

/* Reserve space in G for a GOT entry containing the value of symbol
   SYMNDX in input bfd ABDF, plus ADDEND.  */

static bfd_boolean
mips_elf_record_local_got_symbol (bfd *abfd, long symndx, bfd_vma addend,
				  struct bfd_link_info *info,
				  unsigned char tls_flag)
{
  struct mips_elf_link_hash_table *htab;
  struct mips_got_info *g;
  struct mips_got_entry entry, **loc;

  htab = mips_elf_hash_table (info);
  BFD_ASSERT (htab != NULL);

  g = htab->got_info;
  BFD_ASSERT (g != NULL);

  entry.abfd = abfd;
  entry.symndx = symndx;
  entry.d.addend = addend;
  entry.tls_type = tls_flag;
  loc = (struct mips_got_entry **)
    htab_find_slot (g->got_entries, &entry, INSERT);

  if (*loc)
    {
      if (tls_flag == GOT_TLS_GD && !((*loc)->tls_type & GOT_TLS_GD))
	{
	  g->tls_gotno += 2;
	  (*loc)->tls_type |= tls_flag;
	}
      else if (tls_flag == GOT_TLS_IE && !((*loc)->tls_type & GOT_TLS_IE))
	{
	  g->tls_gotno += 1;
	  (*loc)->tls_type |= tls_flag;
	}
      return TRUE;
    }

  if (tls_flag != 0)
    {
      entry.gotidx = -1;
      entry.tls_type = tls_flag;
      if (tls_flag == GOT_TLS_IE)
	g->tls_gotno += 1;
      else if (tls_flag == GOT_TLS_GD)
	g->tls_gotno += 2;
      else if (g->tls_ldm_offset == MINUS_ONE)
	{
	  g->tls_ldm_offset = MINUS_TWO;
	  g->tls_gotno += 2;
	}
    }
  else
    {
      entry.gotidx = g->local_gotno++;
      entry.tls_type = 0;
    }

  *loc = (struct mips_got_entry *)bfd_alloc (abfd, sizeof entry);

  if (! *loc)
    return FALSE;

  memcpy (*loc, &entry, sizeof entry);

  return TRUE;
}

/* Return the maximum number of GOT page entries required for RANGE.  */

static bfd_vma
mips_elf_pages_for_range (const struct mips_got_page_range *range)
{
  return (range->max_addend - range->min_addend + RISCV_IMM_REACH-1) >> RISCV_IMM_BITS;
}

/* Record that ABFD has a page relocation against symbol SYMNDX and
   that ADDEND is the addend for that relocation.

   This function creates an upper bound on the number of GOT slots
   required; no attempt is made to combine references to non-overridable
   global symbols across multiple input files.  */

static bfd_boolean
mips_elf_record_got_page_entry (struct bfd_link_info *info, bfd *abfd,
				long symndx, bfd_signed_vma addend)
{
  struct mips_elf_link_hash_table *htab;
  struct mips_got_info *g;
  struct mips_got_page_entry lookup, *entry;
  struct mips_got_page_range **range_ptr, *range;
  bfd_vma old_pages, new_pages;
  void **loc;

  htab = mips_elf_hash_table (info);
  BFD_ASSERT (htab != NULL);

  g = htab->got_info;
  BFD_ASSERT (g != NULL);

  /* Find the mips_got_page_entry hash table entry for this symbol.  */
  lookup.abfd = abfd;
  lookup.symndx = symndx;
  loc = htab_find_slot (g->got_page_entries, &lookup, INSERT);
  if (loc == NULL)
    return FALSE;

  /* Create a mips_got_page_entry if this is the first time we've
     seen the symbol.  */
  entry = (struct mips_got_page_entry *) *loc;
  if (!entry)
    {
      entry = bfd_alloc (abfd, sizeof (*entry));
      if (!entry)
	return FALSE;

      entry->abfd = abfd;
      entry->symndx = symndx;
      entry->ranges = NULL;
      entry->num_pages = 0;
      *loc = entry;
    }

  /* Skip over ranges whose maximum extent cannot share a page entry
     with ADDEND.  */
  range_ptr = &entry->ranges;
  while (*range_ptr && addend > (*range_ptr)->max_addend + RISCV_IMM_REACH/2-1)
    range_ptr = &(*range_ptr)->next;

  /* If we scanned to the end of the list, or found a range whose
     minimum extent cannot share a page entry with ADDEND, create
     a new singleton range.  */
  range = *range_ptr;
  if (!range || addend < range->min_addend - (RISCV_IMM_REACH/2-1))
    {
      range = bfd_alloc (abfd, sizeof (*range));
      if (!range)
	return FALSE;

      range->next = *range_ptr;
      range->min_addend = addend;
      range->max_addend = addend;

      *range_ptr = range;
      entry->num_pages++;
      g->page_gotno++;
      return TRUE;
    }

  /* Remember how many pages the old range contributed.  */
  old_pages = mips_elf_pages_for_range (range);

  /* Update the ranges.  */
  if (addend < range->min_addend)
    range->min_addend = addend;
  else if (addend > range->max_addend)
    {
      if (range->next && addend >= range->next->min_addend - (RISCV_IMM_REACH/2-1))
	{
	  old_pages += mips_elf_pages_for_range (range->next);
	  range->max_addend = range->next->max_addend;
	  range->next = range->next->next;
	}
      else
	range->max_addend = addend;
    }

  /* Record any change in the total estimate.  */
  new_pages = mips_elf_pages_for_range (range);
  if (old_pages != new_pages)
    {
      entry->num_pages += new_pages - old_pages;
      g->page_gotno += new_pages - old_pages;
    }

  return TRUE;
}

/* Add room for N relocations to the .rel(a).dyn section in ABFD.  */

static void
mips_elf_allocate_dynamic_relocations (bfd *abfd, struct bfd_link_info *info,
				       unsigned int n)
{
  asection *s;
  struct mips_elf_link_hash_table *htab;

  htab = mips_elf_hash_table (info);
  BFD_ASSERT (htab != NULL);

  s = mips_elf_rel_dyn_section (info, FALSE);
  BFD_ASSERT (s != NULL);

  if (s->size == 0)
    {
      /* Make room for a null element.  */
      s->size += MIPS_ELF_REL_SIZE (abfd);
      ++s->reloc_count;
    }
  s->size += n * MIPS_ELF_REL_SIZE (abfd);
}

/* A htab_traverse callback for GOT entries.  Set boolean *DATA to true
   if the GOT entry is for an indirect or warning symbol.  */

static int
mips_elf_check_recreate_got (void **entryp, void *data)
{
  struct mips_got_entry *entry;
  bfd_boolean *must_recreate;

  entry = (struct mips_got_entry *) *entryp;
  must_recreate = (bfd_boolean *) data;
  if (entry->abfd != NULL && entry->symndx == -1)
    {
      struct mips_elf_link_hash_entry *h;

      h = entry->d.h;
      if (h->root.root.type == bfd_link_hash_indirect
	  || h->root.root.type == bfd_link_hash_warning)
	{
	  *must_recreate = TRUE;
	  return 0;
	}
    }
  return 1;
}

/* A htab_traverse callback for GOT entries.  Add all entries to
   hash table *DATA, converting entries for indirect and warning
   symbols into entries for the target symbol.  Set *DATA to null
   on error.  */

static int
mips_elf_recreate_got (void **entryp, void *data)
{
  htab_t *new_got;
  struct mips_got_entry *entry;
  void **slot;

  new_got = (htab_t *) data;
  entry = (struct mips_got_entry *) *entryp;
  if (entry->abfd != NULL && entry->symndx == -1)
    {
      struct mips_elf_link_hash_entry *h;

      h = entry->d.h;
      while (h->root.root.type == bfd_link_hash_indirect
	     || h->root.root.type == bfd_link_hash_warning)
	{
	  BFD_ASSERT (h->global_got_area == GGA_NONE);
	  h = (struct mips_elf_link_hash_entry *) h->root.root.u.i.link;
	}
      entry->d.h = h;
    }
  slot = htab_find_slot (*new_got, entry, INSERT);
  if (slot == NULL)
    {
      *new_got = NULL;
      return 0;
    }
  if (*slot == NULL)
    *slot = entry;
  else
    free (entry);
  return 1;
}

/* If any entries in G->got_entries are for indirect or warning symbols,
   replace them with entries for the target symbol.  */

static bfd_boolean
mips_elf_resolve_final_got_entries (struct mips_got_info *g)
{
  bfd_boolean must_recreate;
  htab_t new_got;

  must_recreate = FALSE;
  htab_traverse (g->got_entries, mips_elf_check_recreate_got, &must_recreate);
  if (must_recreate)
    {
      new_got = htab_create (htab_size (g->got_entries),
			     mips_elf_got_entry_hash,
			     mips_elf_got_entry_eq, NULL);
      htab_traverse (g->got_entries, mips_elf_recreate_got, &new_got);
      if (new_got == NULL)
	return FALSE;

      /* Each entry in g->got_entries has either been copied to new_got
	 or freed.  Now delete the hash table itself.  */
      htab_delete (g->got_entries);
      g->got_entries = new_got;
    }
  return TRUE;
}

/* A mips_elf_link_hash_traverse callback for which DATA points
   to the link_info structure.  Count the number of type (3) entries
   in the master GOT.  */

static int
mips_elf_count_got_symbols (struct mips_elf_link_hash_entry *h, void *data)
{
  struct bfd_link_info *info;
  struct mips_elf_link_hash_table *htab;
  struct mips_got_info *g;

  info = (struct bfd_link_info *) data;
  htab = mips_elf_hash_table (info);
  g = htab->got_info;
  if (h->global_got_area != GGA_NONE)
    {
      /* Make a final decision about whether the symbol belongs in the
	 local or global GOT.  Symbols that bind locally can (and in the
	 case of forced-local symbols, must) live in the local GOT.
	 Those that are aren't in the dynamic symbol table must also
	 live in the local GOT.

	 Note that the former condition does not always imply the
	 latter: symbols do not bind locally if they are completely
	 undefined.  We'll report undefined symbols later if appropriate.  */
      if (h->root.dynindx == -1
	  || (h->got_only_for_calls
	      ? SYMBOL_CALLS_LOCAL (info, &h->root)
	      : SYMBOL_REFERENCES_LOCAL (info, &h->root)))
	{
	  /* The symbol belongs in the local GOT.  We no longer need this
	     entry if it was only used for relocations; those relocations
	     will be against the null or section symbol instead of H.  */
	  if (h->global_got_area != GGA_RELOC_ONLY)
	    g->local_gotno++;
	  h->global_got_area = GGA_NONE;
	}
      else
	{
	  g->global_gotno++;
	  if (h->global_got_area == GGA_RELOC_ONLY)
	    g->reloc_only_gotno++;
	}
    }
  return 1;
}

/* Set the TLS GOT index for the GOT entry in ENTRYP.  ENTRYP's NEXT field
   is null iff there is just a single GOT.  */

static int
mips_elf_initialize_tls_index (void **entryp, void *p)
{
  struct mips_got_entry *entry = (struct mips_got_entry *)*entryp;
  struct mips_got_info *g = p;
  bfd_vma next_index;
  unsigned char tls_type;

  /* We're only interested in TLS symbols.  */
  if (entry->tls_type == 0)
    return 1;

  next_index = MIPS_ELF_GOT_SIZE (entry->abfd) * (long) g->tls_assigned_gotno;

  if (entry->symndx == -1)
    {
      /* A type (3) got entry in the single-GOT case.  We use the symbol's
	 hash table entry to track its index.  */
      if (entry->d.h->tls_type & GOT_TLS_OFFSET_DONE)
	return 1;
      entry->d.h->tls_type |= GOT_TLS_OFFSET_DONE;
      entry->d.h->tls_got_offset = next_index;
      tls_type = entry->d.h->tls_type;
    }
  else
    {
      if (entry->tls_type & GOT_TLS_LDM)
	{
	  /* There are separate mips_got_entry objects for each input bfd
	     that requires an LDM entry.  Make sure that all LDM entries in
	     a GOT resolve to the same index.  */
	  if (g->tls_ldm_offset != MINUS_TWO && g->tls_ldm_offset != MINUS_ONE)
	    {
	      entry->gotidx = g->tls_ldm_offset;
	      return 1;
	    }
	  g->tls_ldm_offset = next_index;
	}
      entry->gotidx = next_index;
      tls_type = entry->tls_type;
    }

  /* Account for the entries we've just allocated.  */
  if (tls_type & (GOT_TLS_GD | GOT_TLS_LDM))
    g->tls_assigned_gotno += 2;
  if (tls_type & GOT_TLS_IE)
    g->tls_assigned_gotno += 1;

  return 1;
}

/* Returns the first relocation of type r_type found, beginning with
   RELOCATION.  RELEND is one-past-the-end of the relocation table.  */

static const Elf_Internal_Rela *
mips_elf_next_relocation (bfd *abfd ATTRIBUTE_UNUSED, unsigned int r_type,
			  const Elf_Internal_Rela *relocation,
			  const Elf_Internal_Rela *relend)
{
  unsigned long r_symndx = ELF_R_SYM (abfd, relocation->r_info);

  while (relocation < relend)
    {
      if (ELF_R_TYPE (abfd, relocation->r_info) == r_type
	  && ELF_R_SYM (abfd, relocation->r_info) == r_symndx)
	return relocation;

      ++relocation;
    }

  /* We didn't find it.  */
  return NULL;
}

/* Return whether an input relocation is against a local symbol.  */

static bfd_boolean
mips_elf_local_relocation_p (bfd *input_bfd,
			     const Elf_Internal_Rela *relocation,
			     asection **local_sections)
{
  unsigned long r_symndx;
  Elf_Internal_Shdr *symtab_hdr;
  size_t extsymoff;

  r_symndx = ELF_R_SYM (input_bfd, relocation->r_info);
  symtab_hdr = &elf_tdata (input_bfd)->symtab_hdr;
  extsymoff = (elf_bad_symtab (input_bfd)) ? 0 : symtab_hdr->sh_info;

  if (r_symndx < extsymoff)
    return TRUE;
  if (elf_bad_symtab (input_bfd) && local_sections[r_symndx] != NULL)
    return TRUE;

  return FALSE;
}

/* Sign-extend VALUE, which has the indicated number of BITS.  */

bfd_vma
_bfd_riscv_elf_sign_extend (bfd_vma value, int bits)
{
  if (value & ((bfd_vma) 1 << (bits - 1)))
    /* VALUE is negative.  */
    value |= ((bfd_vma) - 1) << bits;

  return value;
}

/* Return non-zero if the indicated VALUE has overflowed the maximum
   range expressible by a signed number with the indicated number of
   BITS.  */

static bfd_boolean
mips_elf_overflow_p (bfd_vma value, int bits)
{
  bfd_signed_vma svalue = (bfd_signed_vma) value;

  if (svalue > (1 << (bits - 1)) - 1)
    /* The value is too big.  */
    return TRUE;
  else if (svalue < -(1 << (bits - 1)))
    /* The value is too small.  */
    return TRUE;

  /* All is well.  */
  return FALSE;
}

/* Create the .got section to hold the global offset table.  */

static bfd_boolean
mips_elf_create_got_section (bfd *abfd, struct bfd_link_info *info)
{
  flagword flags;
  register asection *s;
  struct elf_link_hash_entry *h;
  struct bfd_link_hash_entry *bh;
  struct mips_got_info *g;
  bfd_size_type amt;
  struct mips_elf_link_hash_table *htab;

  htab = mips_elf_hash_table (info);
  BFD_ASSERT (htab != NULL);

  /* This function may be called more than once.  */
  if (htab->sgot)
    return TRUE;

  flags = (SEC_ALLOC | SEC_LOAD | SEC_HAS_CONTENTS | SEC_IN_MEMORY
	   | SEC_LINKER_CREATED);

  /* We have to use an alignment of 2**4 here because this is hardcoded
     in the function stub generation and in the linker script.  */
  s = bfd_make_section_with_flags (abfd, ".got", flags);
  if (s == NULL
      || ! bfd_set_section_alignment (abfd, s, 4))
    return FALSE;
  htab->sgot = s;

  /* Define the symbol _GLOBAL_OFFSET_TABLE_.  We don't do this in the
     linker script because we don't want to define the symbol if we
     are not creating a global offset table.  */
  bh = NULL;
  if (! (_bfd_generic_link_add_one_symbol
	 (info, abfd, "_GLOBAL_OFFSET_TABLE_", BSF_GLOBAL, s,
	  0, NULL, FALSE, get_elf_backend_data (abfd)->collect, &bh)))
    return FALSE;

  h = (struct elf_link_hash_entry *) bh;
  h->non_elf = 0;
  h->def_regular = 1;
  h->type = STT_OBJECT;
  elf_hash_table (info)->hgot = h;

  if (info->shared
      && ! bfd_elf_link_record_dynamic_symbol (info, h))
    return FALSE;

  amt = sizeof (struct mips_got_info);
  g = bfd_alloc (abfd, amt);
  if (g == NULL)
    return FALSE;
  g->global_gotsym = NULL;
  g->global_gotno = 0;
  g->reloc_only_gotno = 0;
  g->tls_gotno = 0;
  g->local_gotno = 0;
  g->page_gotno = 0;
  g->assigned_gotno = 0;
  g->tls_ldm_offset = MINUS_ONE;
  g->got_entries = htab_try_create (1, mips_elf_got_entry_hash,
				    mips_elf_got_entry_eq, NULL);
  if (g->got_entries == NULL)
    return FALSE;
  g->got_page_entries = htab_try_create (1, mips_got_page_entry_hash,
					 mips_got_page_entry_eq, NULL);
  if (g->got_page_entries == NULL)
    return FALSE;
  htab->got_info = g;
  mips_elf_section_data (s)->elf.this_hdr.sh_flags
    |= SHF_ALLOC | SHF_WRITE | SHF_MIPS_GPREL;

  /* We also need a .got.plt section when generating PLTs.  */
  s = bfd_make_section_with_flags (abfd, ".got.plt",
				   SEC_ALLOC | SEC_LOAD | SEC_HAS_CONTENTS
				   | SEC_IN_MEMORY | SEC_LINKER_CREATED);
  if (s == NULL)
    return FALSE;
  htab->sgotplt = s;

  return TRUE;
}

/* Return TRUE if a relocation of type R_TYPE from INPUT_BFD might
   require an la25 stub.  See also mips_elf_local_pic_function_p,
   which determines whether the destination function ever requires a
   stub.  */

static bfd_boolean
mips_elf_relocation_needs_la25_stub (bfd *input_bfd, int r_type)
{
  /* We specifically ignore branches and jumps from EF_PIC objects,
     where the onus is on the compiler or programmer to perform any
     necessary initialization of $25.  Sometimes such initialization
     is unnecessary; for example, -mno-shared functions do not use
     the incoming value of $25, and may therefore be called directly.  */
  if (PIC_OBJECT_P (input_bfd))
    return FALSE;

  switch (r_type)
    {
    case R_RISCV_26:
    case R_RISCV_PC16:
    case R_MIPS16_26:
      return TRUE;

    default:
      return FALSE;
    }
}

/* Calculate the value produced by the RELOCATION (which comes from
   the INPUT_BFD).  The ADDEND is the addend to use for this
   RELOCATION; RELOCATION->R_ADDEND is ignored.

   The result of the relocation calculation is stored in VALUEP.
   On exit, set *CROSS_MODE_JUMP_P to true if the relocation field
   is a MIPS16 jump to non-MIPS16 code, or vice versa.

   This function returns bfd_reloc_continue if the caller need take no
   further action regarding this relocation, bfd_reloc_notsupported if
   something goes dramatically wrong, bfd_reloc_overflow if an
   overflow occurs, and bfd_reloc_ok to indicate success.  */

static bfd_reloc_status_type
mips_elf_calculate_relocation (bfd *abfd, bfd *input_bfd,
			       asection *input_section,
			       struct bfd_link_info *info,
			       const Elf_Internal_Rela *relocation,
			       bfd_vma addend, reloc_howto_type *howto,
			       Elf_Internal_Sym *local_syms,
			       asection **local_sections, bfd_vma *valuep,
			       const char **namep,
			       bfd_boolean save_addend)
{
  /* The eventual value we will return.  */
  bfd_vma value;
  /* The address of the symbol against which the relocation is
     occurring.  */
  bfd_vma symbol = 0;
  /* The final GP value to be used for the relocatable, executable, or
     shared object file being produced.  */
  bfd_vma gp;
  /* The place (section offset or address) of the storage unit being
     relocated.  */
  bfd_vma p;
  /* The value of GP used to create the relocatable object.  */
  bfd_vma gp0;
  /* The offset into the global offset table at which the address of
     the relocation entry symbol, adjusted by the addend, resides
     during execution.  */
  bfd_vma g = MINUS_ONE;
  /* The section in which the symbol referenced by the relocation is
     located.  */
  asection *sec = NULL;
  struct mips_elf_link_hash_entry *h = NULL;
  /* TRUE if the symbol referred to by this relocation is a local
     symbol.  */
  bfd_boolean local_p, was_local_p;
  /* TRUE if the symbol referred to by this relocation is
     "__gnu_local_gp".  */
  bfd_boolean gnu_local_gp_p = FALSE;
  Elf_Internal_Shdr *symtab_hdr;
  size_t extsymoff;
  unsigned long r_symndx;
  int r_type;
  /* TRUE if overflow occurred during the calculation of the
     relocation value.  */
  bfd_boolean overflowed_p;
  /* TRUE if this relocation refers to a MIPS16 function.  */
  struct mips_elf_link_hash_table *htab;
  bfd *dynobj;

  dynobj = elf_hash_table (info)->dynobj;
  htab = mips_elf_hash_table (info);
  BFD_ASSERT (htab != NULL);

  /* Parse the relocation.  */
  r_symndx = ELF_R_SYM (input_bfd, relocation->r_info);
  r_type = ELF_R_TYPE (input_bfd, relocation->r_info);
  p = (input_section->output_section->vma
       + input_section->output_offset
       + relocation->r_offset);

  /* Assume that there will be no overflow.  */
  overflowed_p = FALSE;

  /* Figure out whether or not the symbol is local, and get the offset
     used in the array of hash table entries.  */
  symtab_hdr = &elf_tdata (input_bfd)->symtab_hdr;
  local_p = mips_elf_local_relocation_p (input_bfd, relocation,
					 local_sections);
  was_local_p = local_p;
  if (! elf_bad_symtab (input_bfd))
    extsymoff = symtab_hdr->sh_info;
  else
    {
      /* The symbol table does not follow the rule that local symbols
	 must come before globals.  */
      extsymoff = 0;
    }

  /* Figure out the value of the symbol.  */
  if (local_p)
    {
      Elf_Internal_Sym *sym;

      sym = local_syms + r_symndx;
      sec = local_sections[r_symndx];

      symbol = sec->output_section->vma + sec->output_offset;
      if (ELF_ST_TYPE (sym->st_info) != STT_SECTION
	  || (sec->flags & SEC_MERGE))
	symbol += sym->st_value;
      if ((sec->flags & SEC_MERGE)
	  && ELF_ST_TYPE (sym->st_info) == STT_SECTION)
	{
	  addend = _bfd_elf_rel_local_sym (abfd, sym, &sec, addend);
	  addend -= symbol;
	  addend += sec->output_section->vma + sec->output_offset;
	}

      /* MIPS16 text labels should be treated as odd.  */
      if (ELF_ST_IS_MIPS16 (sym->st_other))
	++symbol;

      /* Record the name of this symbol, for our caller.  */
      *namep = bfd_elf_string_from_elf_section (input_bfd,
						symtab_hdr->sh_link,
						sym->st_name);
      if (*namep == '\0')
	*namep = bfd_section_name (input_bfd, sec);
    }
  else
    {
      /* ??? Could we use RELOC_FOR_GLOBAL_SYMBOL here ?  */

      /* For global symbols we look up the symbol in the hash-table.  */
      h = ((struct mips_elf_link_hash_entry *)
	   elf_sym_hashes (input_bfd) [r_symndx - extsymoff]);
      /* Find the real hash-table entry for this symbol.  */
      while (h->root.root.type == bfd_link_hash_indirect
	     || h->root.root.type == bfd_link_hash_warning)
	h = (struct mips_elf_link_hash_entry *) h->root.root.u.i.link;

      /* Record the name of this symbol, for our caller.  */
      *namep = h->root.root.root.string;

      /* See if this is the special _gp symbol.  Note that such a
	 symbol must always be a global symbol.  */
      if (strcmp (*namep, "__gnu_local_gp") == 0)
	gnu_local_gp_p = TRUE;


      /* If this symbol is defined, calculate its address.  Note that
	 _gp_disp is a magic symbol, always implicitly defined by the
	 linker, so it's inappropriate to check to see whether or not
	 its defined.  */
      else if ((h->root.root.type == bfd_link_hash_defined
		|| h->root.root.type == bfd_link_hash_defweak)
	       && h->root.root.u.def.section)
	{
	  sec = h->root.root.u.def.section;
	  if (sec->output_section)
	    symbol = (h->root.root.u.def.value
		      + sec->output_section->vma
		      + sec->output_offset);
	  else
	    symbol = h->root.root.u.def.value;
	}
      else if (h->root.root.type == bfd_link_hash_undefweak)
	/* We allow relocations against undefined weak symbols, giving
	   it the value zero, so that you can undefined weak functions
	   and check to see if they exist by looking at their
	   addresses.  */
	symbol = 0;
      else if (info->unresolved_syms_in_objects == RM_IGNORE
	       && ELF_ST_VISIBILITY (h->root.other) == STV_DEFAULT)
	symbol = 0;
      else if (strcmp (*namep, "_DYNAMIC_LINKING") == 0)
	{
	  /* If this is a dynamic link, we should have created a
	     _DYNAMIC_LINKING symbol
	     in in _bfd_riscv_elf_create_dynamic_sections.
	     Otherwise, we should define the symbol with a value of 0.
	     FIXME: It should probably get into the symbol table
	     somehow as well.  */
	  BFD_ASSERT (! info->shared);
	  BFD_ASSERT (bfd_get_section_by_name (abfd, ".dynamic") == NULL);
	  symbol = 0;
	}
      else if ((*info->callbacks->undefined_symbol)
	       (info, h->root.root.root.string, input_bfd,
		input_section, relocation->r_offset,
		(info->unresolved_syms_in_objects == RM_GENERATE_ERROR)
		 || ELF_ST_VISIBILITY (h->root.other)))
	{
	  return bfd_reloc_undefined;
	}
      else
	{
	  return bfd_reloc_notsupported;
	}
    }

  /* If this is a direct call to a PIC function, redirect to the
     non-PIC stub.  */
  if (h != NULL && h->la25_stub
	   && mips_elf_relocation_needs_la25_stub (input_bfd, r_type))
    symbol = (h->la25_stub->stub_section->output_section->vma
	      + h->la25_stub->stub_section->output_offset
	      + h->la25_stub->offset);

  local_p = h == NULL || SYMBOL_REFERENCES_LOCAL (info, &h->root);

  gp0 = _bfd_get_gp_value (input_bfd);
  gp = _bfd_get_gp_value (abfd);

  if (gnu_local_gp_p)
    symbol = gp;

  /* If we haven't already determined the GOT offset, and we're going
     to need it, get it now.  */
  switch (r_type)
    {
    case R_MIPS16_CALL16:
    case R_MIPS16_GOT16:
    case R_RISCV_CALL16:
    case R_RISCV_GOT16:
    case R_RISCV_GOT_DISP:
    case R_RISCV_GOT_HI16:
    case R_RISCV_CALL_HI16:
    case R_RISCV_GOT_LO16:
    case R_RISCV_CALL_LO16:
    case R_RISCV_TLS_GD:
    case R_RISCV_TLS_GD_HI16:
    case R_RISCV_TLS_GD_LO16:
    case R_RISCV_TLS_GOTTPREL:
    case R_RISCV_TLS_GOT_HI16:
    case R_RISCV_TLS_GOT_LO16:
    case R_RISCV_TLS_LDM:
    case R_RISCV_TLS_LDM_HI16:
    case R_RISCV_TLS_LDM_LO16:
      /* Find the index into the GOT where this value is located.  */
      if (TLS_LDM_RELOC_P(r_type))
	{
	  g = mips_elf_local_got_index (abfd, input_bfd, info,
					0, 0, NULL, r_type);
	  if (g == MINUS_ONE)
	    return bfd_reloc_outofrange;
	}
      else if (!local_p)
	{
	      BFD_ASSERT (addend == 0);
	      g = mips_elf_global_got_index (dynobj, &h->root, r_type, info);
	      if (h->tls_type == GOT_NORMAL
		  && !elf_hash_table (info)->dynamic_sections_created)
		/* This is a static link.  We must initialize the GOT entry.  */
		MIPS_ELF_PUT_WORD (dynobj, symbol, htab->sgot->contents + g);
	}
      else if (call16_reloc_p (r_type) || got16_reloc_p (r_type))
	/* The calculation below does not involve "g".  */
	break;
      else
	{
	  g = mips_elf_local_got_index (abfd, input_bfd, info,
					symbol + addend, r_symndx, h, r_type);
	  if (g == MINUS_ONE)
	    return bfd_reloc_outofrange;
	}

      /* Convert GOT indices to actual offsets.  */
      g = mips_elf_got_offset_from_index (info, abfd, g);
      break;
    }

  /* Figure out what kind of relocation is being performed.  */
  switch (r_type)
    {
    case R_RISCV_NONE:
      return bfd_reloc_continue;

    case R_RISCV_32:
    case R_RISCV_REL32:
    case R_RISCV_64:
      if ((info->shared
	   || (htab->root.dynamic_sections_created
	       && h != NULL
	       && h->root.def_dynamic
	       && !h->root.def_regular
	       && !h->has_static_relocs))
	  && r_symndx != STN_UNDEF
	  && (h == NULL
	      || h->root.root.type != bfd_link_hash_undefweak
	      || ELF_ST_VISIBILITY (h->root.other) == STV_DEFAULT)
	  && (input_section->flags & SEC_ALLOC) != 0)
	{
	  /* If we're creating a shared library, then we can't know
	     where the symbol will end up.  So, we create a relocation
	     record in the output, and leave the job up to the dynamic
	     linker.  We must do the same for executable references to
	     shared library symbols, unless we've decided to use copy
	     relocs or PLTs instead.  */
	  value = addend;
	  if (!mips_elf_create_dynamic_relocation (abfd,
						   info,
						   relocation,
						   h,
						   sec,
						   symbol,
						   &value,
						   input_section))
	    return bfd_reloc_undefined;
	}
      else
	{
	  if (r_type != R_RISCV_REL32)
	    value = symbol + addend;
	  else
	    value = addend;
	}
      value &= howto->dst_mask;
      break;

    case R_RISCV_PC32:
      value = symbol + addend - p;
      value &= howto->dst_mask;
      break;

    case R_RISCV_26:
      if (symbol == 0)
	{
	  /* Need to support JAL to address 0 to statically link against libc.
	     For now, implement as an infinite loop, "1: JAL 1b", but this is
	     incorrect and we should relax it to JALR ra, x0. */
	  value = 0;
	}
      else
	{
	  value = symbol + _bfd_riscv_elf_sign_extend (addend, RISCV_JUMP_BITS+RISCV_JUMP_ALIGN_BITS) - p;
	  overflowed_p = mips_elf_overflow_p (value, RISCV_JUMP_BITS+RISCV_JUMP_ALIGN_BITS);
	}
      value >>= howto->rightshift;
      value <<= OP_SH_TARGET;
      value &= howto->dst_mask;
      break;

    case R_RISCV_TLS_DTPREL_HI16:
      value = ((mips_elf_high (addend + symbol - dtprel_base (info))
		<< OP_SH_BIGIMMEDIATE) & howto->dst_mask);
      break;

    case R_RISCV_TLS_DTPREL_LO16:
    case R_RISCV_TLS_DTPREL32:
    case R_RISCV_TLS_DTPREL64:
      value = ((addend + symbol - dtprel_base (info))
		<< OP_SH_IMMEDIATE) & howto->dst_mask;
      break;

    case R_RISCV_TLS_TPREL_HI16:
      value = (mips_elf_high (addend + symbol - tprel_base (info))
	       << OP_SH_BIGIMMEDIATE) & howto->dst_mask;
      break;

    case R_RISCV_TLS_TPREL_LO16:
      value = ((symbol + addend - tprel_base (info))
	        << OP_SH_IMMEDIATE) & howto->dst_mask;
      break;

    case R_RISCV_HI16:
    case R_MIPS16_HI16:
      value = mips_elf_high (addend + symbol) << OP_SH_BIGIMMEDIATE;
      value &= howto->dst_mask;
      break;

    case R_RISCV_LO16:
    case R_MIPS16_LO16:
      value = ((symbol + addend) << OP_SH_IMMEDIATE) & howto->dst_mask;
      break;

    case R_RISCV_LITERAL:
      /* Because we don't merge literal sections, we can handle this
	 just like R_RISCV_GPREL16.  In the long run, we should merge
	 shared literals, and then we will need to additional work
	 here.  */

      /* Fall through.  */

    case R_MIPS16_GPREL:
      /* The R_MIPS16_GPREL performs the same calculation as
	 R_RISCV_GPREL16, but stores the relocated bits in a different
	 order.  We don't need to do anything special here; the
	 differences are handled in mips_elf_perform_relocation.  */
    case R_RISCV_GPREL16:
      /* Only sign-extend the addend if it was extracted from the
	 instruction.  If the addend was separate, leave it alone,
	 otherwise we may lose significant bits.  */
      if (howto->partial_inplace)
	addend = _bfd_riscv_elf_sign_extend (addend, RISCV_IMM_BITS);
      value = symbol + addend - gp;
      /* If the symbol was local, any earlier relocatable links will
	 have adjusted its addend with the gp offset, so compensate
	 for that now.  Don't do it for symbols forced local in this
	 link, though, since they won't have had the gp offset applied
	 to them before.  */
      if (was_local_p)
	value += gp0;
      overflowed_p = mips_elf_overflow_p (value, RISCV_IMM_BITS);
      break;

    case R_MIPS16_GOT16:
    case R_MIPS16_CALL16:
    case R_RISCV_GOT16:
    case R_RISCV_CALL16:
      if (local_p)
	{
	  value = mips_elf_got16_entry (abfd, input_bfd, info,
					symbol + addend, !was_local_p);
	  if (value == MINUS_ONE)
	    return bfd_reloc_outofrange;
	  value
	    = mips_elf_got_offset_from_index (info, abfd, value);
	  overflowed_p = mips_elf_overflow_p (value, RISCV_IMM_BITS);
	  value = (value << OP_SH_IMMEDIATE) & howto->dst_mask;
	  break;
	}

      /* Fall through.  */

    case R_RISCV_TLS_GD:
    case R_RISCV_TLS_GOTTPREL:
    case R_RISCV_TLS_LDM:
    case R_RISCV_GOT_DISP:
      value = g;
      overflowed_p = mips_elf_overflow_p (value, RISCV_IMM_BITS);
      value = (value << OP_SH_IMMEDIATE) & howto->dst_mask;
      break;

    case R_RISCV_GPREL32:
      value = (addend + symbol + gp0 - gp);
      if (!save_addend)
	value &= howto->dst_mask;
      break;

    case R_RISCV_TLS_GOT_HI16:
    case R_RISCV_TLS_GD_HI16:
    case R_RISCV_TLS_LDM_HI16:
    case R_RISCV_GOT_HI16:
    case R_RISCV_CALL_HI16:
      /* We're allowed to handle these two relocations identically.
	 The dynamic linker is allowed to handle the CALL relocations
	 differently by creating a lazy evaluation stub.  */
      value = mips_elf_high (g) << OP_SH_BIGIMMEDIATE;
      break;

    case R_RISCV_TLS_GOT_LO16:
    case R_RISCV_TLS_GD_LO16:
    case R_RISCV_TLS_LDM_LO16:
    case R_RISCV_GOT_LO16:
    case R_RISCV_CALL_LO16:
      value = (g << OP_SH_IMMEDIATE) & howto->dst_mask;
      break;

    case R_RISCV_SUB:
      value = symbol - addend;
      value &= howto->dst_mask;
      break;

    case R_RISCV_SCN_DISP:
      value = symbol + addend - sec->output_offset;
      value &= howto->dst_mask;
      break;

    case R_RISCV_JALR:
      /* This relocation is only a hint.  In some cases, we optimize
	 it into a bal instruction.  But we don't try to optimize
	 when the symbol does not resolve locally.  */
      if (h != NULL && !SYMBOL_CALLS_LOCAL (info, &h->root))
	return bfd_reloc_continue;
      value = symbol + addend;
      break;

    case R_RISCV_PJUMP:
    case R_RISCV_GNU_VTINHERIT:
    case R_RISCV_GNU_VTENTRY:
      /* We don't do anything with these at present.  */
      return bfd_reloc_continue;

    default:
      /* An unrecognized relocation type.  */
      return bfd_reloc_notsupported;
    }

  /* Store the VALUE for our caller.  */
  *valuep = value;
  return overflowed_p ? bfd_reloc_overflow : bfd_reloc_ok;
}

/* Obtain the field relocated by RELOCATION.  */

static bfd_vma
mips_elf_obtain_contents (reloc_howto_type *howto,
			  const Elf_Internal_Rela *relocation,
			  bfd *input_bfd, bfd_byte *contents)
{
  bfd_vma x;
  bfd_byte *location = contents + relocation->r_offset;

  /* Obtain the bytes.  */
  x = bfd_get ((8 * bfd_get_reloc_size (howto)), input_bfd, location);

  return x;
}

/* It has been determined that the result of the RELOCATION is the
   VALUE.  Use HOWTO to place VALUE into the output file at the
   appropriate position.  The SECTION is the section to which the
   relocation applies.  
   CROSS_MODE_JUMP_P is true if the relocation field
   is a MIPS16 jump to non-MIPS16 code, or vice versa.

   Returns FALSE if anything goes wrong.  */

static bfd_boolean
mips_elf_perform_relocation (struct bfd_link_info *info ATTRIBUTE_UNUSED,
			     reloc_howto_type *howto,
			     const Elf_Internal_Rela *relocation,
			     bfd_vma value, bfd *input_bfd,
			     asection *input_section ATTRIBUTE_UNUSED, bfd_byte *contents)
{
  bfd_vma x;
  bfd_byte *location;
  bfd_vma dst_mask = howto->dst_mask;

  /* Figure out where the relocation is occurring.  */
  location = contents + relocation->r_offset;

  /* Obtain the current value.  */
  x = mips_elf_obtain_contents (howto, relocation, input_bfd, contents);

  /* Fix up dst_mask and value for R_RISCV_LO16 relocs on stores. */
  if (IS_STORE_RELOC (input_bfd, relocation->r_info, x))
    {
      value >>= OP_SH_IMMEDIATE;
      value = ((value >> RISCV_IMMLO_BITS) << OP_SH_IMMHI) |
	      ((value & ((1<<RISCV_IMMLO_BITS)-1)) << OP_SH_IMMLO);
      dst_mask = (OP_MASK_IMMHI << OP_SH_IMMHI) | (OP_MASK_IMMLO << OP_SH_IMMLO);
    }

  /* Clear the field we are setting.  */
  x &= ~dst_mask;

  /* Set the field.  */
  x |= (value & dst_mask);

  /* Put the value into the output.  */
  bfd_put (8 * bfd_get_reloc_size (howto), input_bfd, x, location);

  return TRUE;
}

/* Create a rel.dyn relocation for the dynamic linker to resolve.  REL
   is the original relocation, which is now being transformed into a
   dynamic relocation.  The ADDENDP is adjusted if necessary; the
   caller should store the result in place of the original addend.  */

static bfd_boolean
mips_elf_create_dynamic_relocation (bfd *output_bfd,
				    struct bfd_link_info *info,
				    const Elf_Internal_Rela *rel,
				    struct mips_elf_link_hash_entry *h,
				    asection *sec, bfd_vma symbol,
				    bfd_vma *addendp, asection *input_section)
{
  Elf_Internal_Rela outrel[3];
  asection *sreloc;
  int r_type;
  long indx;
  bfd_boolean defined_p;
  struct mips_elf_link_hash_table *htab;

  htab = mips_elf_hash_table (info);
  BFD_ASSERT (htab != NULL);

  r_type = ELF_R_TYPE (output_bfd, rel->r_info);
  sreloc = mips_elf_rel_dyn_section (info, FALSE);
  BFD_ASSERT (sreloc != NULL);
  BFD_ASSERT (sreloc->contents != NULL);
  BFD_ASSERT (sreloc->reloc_count * MIPS_ELF_REL_SIZE (output_bfd)
	      < sreloc->size);

  outrel[0].r_offset =
    _bfd_elf_section_offset (output_bfd, info, input_section, rel[0].r_offset);
  if (ABI_64_P (output_bfd))
    {
      outrel[1].r_offset =
	_bfd_elf_section_offset (output_bfd, info, input_section, rel[1].r_offset);
      outrel[2].r_offset =
	_bfd_elf_section_offset (output_bfd, info, input_section, rel[2].r_offset);
    }

  if (outrel[0].r_offset == MINUS_ONE)
    /* The relocation field has been deleted.  */
    return TRUE;

  if (outrel[0].r_offset == MINUS_TWO)
    {
      /* The relocation field has been converted into a relative value of
	 some sort.  Functions like _bfd_elf_write_section_eh_frame expect
	 the field to be fully relocated, so add in the symbol's value.  */
      *addendp += symbol;
      return TRUE;
    }

  /* We must now calculate the dynamic symbol table index to use
     in the relocation.  */
  if (h != NULL && ! SYMBOL_REFERENCES_LOCAL (info, &h->root))
    {
      BFD_ASSERT (h->global_got_area != GGA_NONE);
      indx = h->root.dynindx;
      /* ??? glibc's ld.so just adds the final GOT entry to the
         relocation field.  It therefore treats relocs against
         defined symbols in the same way as relocs against
         undefined symbols.  */
      defined_p = FALSE;
    }
  else
    {
      if (sec != NULL && bfd_is_abs_section (sec))
	indx = 0;
      else if (sec == NULL || sec->owner == NULL)
	{
	  bfd_set_error (bfd_error_bad_value);
	  return FALSE;
	}
      else
	{
	  indx = elf_section_data (sec->output_section)->dynindx;
	  if (indx == 0)
	    {
	      asection *osec = htab->root.text_index_section;
	      indx = elf_section_data (osec)->dynindx;
	    }
	  if (indx == 0)
	    abort ();
	}

      /* Instead of generating a relocation using the section
	 symbol, we may as well make it a fully relative
	 relocation.  We want to avoid generating relocations to
	 local symbols because we used to generate them
	 incorrectly, without adding the original symbol value,
	 which is mandated by the ABI for section symbols.  In
	 order to give dynamic loaders and applications time to
	 phase out the incorrect use, we refrain from emitting
	 section-relative relocations.  It's not like they're
	 useful, after all.  This should be a bit more efficient
	 as well.  */
      /* ??? Although this behavior is compatible with glibc's ld.so,
	 the ABI says that relocations against STN_UNDEF should have
	 a symbol value of 0.  Irix rld honors this, so relocations
	 against STN_UNDEF have no effect.  */
      indx = 0;
      defined_p = TRUE;
    }

  /* If the relocation was previously an absolute relocation and
     this symbol will not be referred to by the relocation, we must
     adjust it by the value we give it in the dynamic symbol table.
     Otherwise leave the job up to the dynamic linker.  */
  if (defined_p && r_type != R_RISCV_REL32)
    *addendp += symbol;

  /* The relocation is always an REL32 relocation because we don't
     know where the shared library will wind up at load-time.  */
  outrel[0].r_info = ELF_R_INFO (output_bfd, (unsigned long) indx,
				 R_RISCV_REL32);

  /* For strict adherence to the ABI specification, we should
     generate a R_RISCV_64 relocation record by itself before the
     _REL32/_64 record as well, such that the addend is read in as
     a 64-bit value (REL32 is a 32-bit relocation, after all).
     However, since none of the existing ELF64 MIPS dynamic
     loaders seems to care, we don't waste space with these
     artificial relocations.  If this turns out to not be true,
     mips_elf_allocate_dynamic_relocation() should be tweaked so
     as to make room for a pair of dynamic relocations per
     invocation if ABI_64_P, and here we should generate an
     additional relocation record with R_RISCV_64 by itself for a
     NULL symbol before this relocation record.  */
  outrel[1].r_info = ELF_R_INFO (output_bfd, 0,
				 ABI_64_P (output_bfd)
				 ? R_RISCV_64
				 : R_RISCV_NONE);
  outrel[2].r_info = ELF_R_INFO (output_bfd, 0, R_RISCV_NONE);

  /* Adjust the output offset of the relocation to reference the
     correct location in the output file.  */
  outrel[0].r_offset += (input_section->output_section->vma
			 + input_section->output_offset);
  outrel[1].r_offset += (input_section->output_section->vma
			 + input_section->output_offset);
  outrel[2].r_offset += (input_section->output_section->vma
			 + input_section->output_offset);

  /* Put the relocation back out.  We have to use the special
     relocation outputter in the 64-bit case since the 64-bit
     relocation format is non-standard.  */
  if (ABI_64_P (output_bfd))
    {
      (*get_elf_backend_data (output_bfd)->s->swap_reloc_out)
	(output_bfd, &outrel[0],
	 (sreloc->contents
	  + sreloc->reloc_count * sizeof (Elf64_RISCV_External_Rel)));
    }
  else
    bfd_elf32_swap_reloc_out
      (output_bfd, &outrel[0],
       (sreloc->contents + sreloc->reloc_count * sizeof (Elf32_External_Rel)));

  /* We've now added another relocation.  */
  ++sreloc->reloc_count;

  /* Make sure the output section is writable.  The dynamic linker
     will be writing to it.  */
  elf_section_data (input_section->output_section)->this_hdr.sh_flags
    |= SHF_WRITE;

  /* If we've written this relocation for a readonly section,
     we need to set DF_TEXTREL again, so that we do not delete the
     DT_TEXTREL tag.  */
  if (MIPS_ELF_READONLY_SECTION (input_section))
    info->flags |= DF_TEXTREL;

  return TRUE;
}

/* Return the MACH for a MIPS e_flags value.  */

unsigned long
_bfd_elf_riscv_mach (flagword flags)
{
  switch (flags & EF_MIPS_MACH)
    {
    case E_RISCV_MACH_ROCKET32:
      return bfd_mach_riscv_rocket32;

    case E_RISCV_MACH_ROCKET64:
      return bfd_mach_riscv_rocket64;

    default:
      switch (flags & EF_MIPS_ARCH)
	{
	case E_RISCV_ARCH_RV32:
	  return bfd_mach_riscv_rocket32;

	default:
	case E_RISCV_ARCH_RV64:
	  return bfd_mach_riscv_rocket64;
	}
    }

  return 0;
}

/* Return printable name for ABI.  */

static INLINE char *
elf_mips_abi_name (bfd *abfd)
{
  flagword flags;

  flags = elf_elfheader (abfd)->e_flags;
  switch (flags & EF_MIPS_ABI)
    {
    case 0:
      if (ABI_32_P (abfd))
	return "32";
      else if (ABI_64_P (abfd))
	return "64";
      else
	return "none";
    default:
      return "unknown abi";
    }
}

/* MIPS ELF uses two common sections.  One is the usual one, and the
   other is for small objects.  All the small objects are kept
   together, and then referenced via the gp pointer, which yields
   faster assembler code.  This is what we use for the small common
   section.  This approach is copied from ecoff.c.  */
static asection mips_elf_scom_section;
static asymbol mips_elf_scom_symbol;
static asymbol *mips_elf_scom_symbol_ptr;

/* MIPS ELF also uses an acommon section, which represents an
   allocated common symbol which may be overridden by a
   definition in a shared library.  */
static asection mips_elf_acom_section;
static asymbol mips_elf_acom_symbol;
static asymbol *mips_elf_acom_symbol_ptr;

/* This is used for both the 32-bit and the 64-bit ABI.  */

void
_bfd_riscv_elf_symbol_processing (bfd *abfd ATTRIBUTE_UNUSED, asymbol *asym)
{
  elf_symbol_type *elfsym;

  /* Handle the special MIPS section numbers that a symbol may use.  */
  elfsym = (elf_symbol_type *) asym;
  switch (elfsym->internal_elf_sym.st_shndx)
    {
    case SHN_MIPS_ACOMMON:
      /* This section is used in a dynamically linked executable file.
	 It is an allocated common section.  The dynamic linker can
	 either resolve these symbols to something in a shared
	 library, or it can just leave them here.  For our purposes,
	 we can consider these symbols to be in a new section.  */
      if (mips_elf_acom_section.name == NULL)
	{
	  /* Initialize the acommon section.  */
	  mips_elf_acom_section.name = ".acommon";
	  mips_elf_acom_section.flags = SEC_ALLOC;
	  mips_elf_acom_section.output_section = &mips_elf_acom_section;
	  mips_elf_acom_section.symbol = &mips_elf_acom_symbol;
	  mips_elf_acom_section.symbol_ptr_ptr = &mips_elf_acom_symbol_ptr;
	  mips_elf_acom_symbol.name = ".acommon";
	  mips_elf_acom_symbol.flags = BSF_SECTION_SYM;
	  mips_elf_acom_symbol.section = &mips_elf_acom_section;
	  mips_elf_acom_symbol_ptr = &mips_elf_acom_symbol;
	}
      asym->section = &mips_elf_acom_section;
      break;

    case SHN_COMMON:
      break;

    case SHN_MIPS_SCOMMON:
      if (mips_elf_scom_section.name == NULL)
	{
	  /* Initialize the small common section.  */
	  mips_elf_scom_section.name = ".scommon";
	  mips_elf_scom_section.flags = SEC_IS_COMMON;
	  mips_elf_scom_section.output_section = &mips_elf_scom_section;
	  mips_elf_scom_section.symbol = &mips_elf_scom_symbol;
	  mips_elf_scom_section.symbol_ptr_ptr = &mips_elf_scom_symbol_ptr;
	  mips_elf_scom_symbol.name = ".scommon";
	  mips_elf_scom_symbol.flags = BSF_SECTION_SYM;
	  mips_elf_scom_symbol.section = &mips_elf_scom_section;
	  mips_elf_scom_symbol_ptr = &mips_elf_scom_symbol;
	}
      asym->section = &mips_elf_scom_section;
      asym->value = elfsym->internal_elf_sym.st_size;
      break;

    case SHN_MIPS_SUNDEFINED:
      asym->section = bfd_und_section_ptr;
      break;
  }

  /* If this is an odd-valued function symbol, assume it's a MIPS16 one.  */
  if (ELF_ST_TYPE (elfsym->internal_elf_sym.st_info) == STT_FUNC
      && (asym->value & 1) != 0)
    {
      asym->value--;
      elfsym->internal_elf_sym.st_other
	= ELF_ST_SET_MIPS16 (elfsym->internal_elf_sym.st_other);
    }
}

/* Implement elf_backend_eh_frame_address_size. */

unsigned int
_bfd_riscv_elf_eh_frame_address_size (bfd *abfd, asection *sec ATTRIBUTE_UNUSED)
{
  if (elf_elfheader (abfd)->e_ident[EI_CLASS] == ELFCLASS64)
    return 8;
  return 4;
}

/* There appears to be a bug in the MIPSpro linker that causes GOT_DISP
   relocations against two unnamed section symbols to resolve to the
   same address.  For example, if we have code like:

	lw	$4,%got_disp(.data)($gp)
	lw	$25,%got_disp(.text)($gp)
	jalr	$25

   then the linker will resolve both relocations to .data and the program
   will jump there rather than to .text.

   We can work around this problem by giving names to local section symbols.
   This is also what the MIPSpro tools do.  */

bfd_boolean
_bfd_riscv_elf_name_local_section_symbols (bfd *abfd ATTRIBUTE_UNUSED)
{
  return FALSE;
}

/* Work over a section just before writing it out.  This routine is
   used by both the 32-bit and the 64-bit ABI.  FIXME: We recognize
   sections that need the SHF_MIPS_GPREL flag by name; there has to be
   a better way.  */

bfd_boolean
_bfd_riscv_elf_section_processing (bfd *abfd, Elf_Internal_Shdr *hdr)
{
  if (hdr->sh_type == SHT_MIPS_REGINFO
      && hdr->sh_size > 0)
    {
      bfd_byte buf[4];

      BFD_ASSERT (hdr->sh_size == sizeof (Elf32_External_RegInfo));
      BFD_ASSERT (hdr->contents == NULL);

      if (bfd_seek (abfd,
		    hdr->sh_offset + sizeof (Elf32_External_RegInfo) - 4,
		    SEEK_SET) != 0)
	return FALSE;
      H_PUT_32 (abfd, elf_gp (abfd), buf);
      if (bfd_bwrite (buf, 4, abfd) != 4)
	return FALSE;
    }

  if (hdr->sh_type == SHT_MIPS_OPTIONS
      && hdr->bfd_section != NULL
      && mips_elf_section_data (hdr->bfd_section) != NULL
      && mips_elf_section_data (hdr->bfd_section)->u.tdata != NULL)
    {
      bfd_byte *contents, *l, *lend;

      /* We stored the section contents in the tdata field in the
	 set_section_contents routine.  We save the section contents
	 so that we don't have to read them again.
	 At this point we know that elf_gp is set, so we can look
	 through the section contents to see if there is an
	 ODK_REGINFO structure.  */

      contents = mips_elf_section_data (hdr->bfd_section)->u.tdata;
      l = contents;
      lend = contents + hdr->sh_size;
      while (l + sizeof (Elf_External_Options) <= lend)
	{
	  Elf_Internal_Options intopt;

	  bfd_riscv_elf_swap_options_in (abfd, (Elf_External_Options *) l,
					&intopt);
	  if (intopt.size < sizeof (Elf_External_Options))
	    {
	      (*_bfd_error_handler)
		(_("%B: Warning: bad `%s' option size %u smaller than its header"),
		abfd, MIPS_ELF_OPTIONS_SECTION_NAME (abfd), intopt.size);
	      break;
	    }
	  if (ABI_64_P (abfd) && intopt.kind == ODK_REGINFO)
	    {
	      bfd_byte buf[8];

	      if (bfd_seek (abfd,
			    (hdr->sh_offset
			     + (l - contents)
			     + sizeof (Elf_External_Options)
			     + (sizeof (Elf64_External_RegInfo) - 8)),
			     SEEK_SET) != 0)
		return FALSE;
	      H_PUT_64 (abfd, elf_gp (abfd), buf);
	      if (bfd_bwrite (buf, 8, abfd) != 8)
		return FALSE;
	    }
	  else if (intopt.kind == ODK_REGINFO)
	    {
	      bfd_byte buf[4];

	      if (bfd_seek (abfd,
			    (hdr->sh_offset
			     + (l - contents)
			     + sizeof (Elf_External_Options)
			     + (sizeof (Elf32_External_RegInfo) - 4)),
			    SEEK_SET) != 0)
		return FALSE;
	      H_PUT_32 (abfd, elf_gp (abfd), buf);
	      if (bfd_bwrite (buf, 4, abfd) != 4)
		return FALSE;
	    }
	  l += intopt.size;
	}
    }

  if (hdr->bfd_section != NULL)
    {
      const char *name = bfd_get_section_name (abfd, hdr->bfd_section);

      /* .sbss is not handled specially here because the GNU/Linux
	 prelinker can convert .sbss from NOBITS to PROGBITS and
	 changing it back to NOBITS breaks the binary.  The entry in
	 _bfd_riscv_elf_special_sections will ensure the correct flags
	 are set on .sbss if BFD creates it without reading it from an
	 input file, and without special handling here the flags set
	 on it in an input file will be followed.  */
      if (strcmp (name, ".sdata") == 0
	  || strcmp (name, ".lit8") == 0
	  || strcmp (name, ".lit4") == 0)
	{
	  hdr->sh_flags |= SHF_ALLOC | SHF_WRITE | SHF_MIPS_GPREL;
	  hdr->sh_type = SHT_PROGBITS;
	}
      else if (strcmp (name, ".srdata") == 0)
	{
	  hdr->sh_flags |= SHF_ALLOC | SHF_MIPS_GPREL;
	  hdr->sh_type = SHT_PROGBITS;
	}
      else if (strcmp (name, ".compact_rel") == 0)
	{
	  hdr->sh_flags = 0;
	  hdr->sh_type = SHT_PROGBITS;
	}
      else if (strcmp (name, ".rtproc") == 0)
	{
	  if (hdr->sh_addralign != 0 && hdr->sh_entsize == 0)
	    {
	      unsigned int adjust;

	      adjust = hdr->sh_size % hdr->sh_addralign;
	      if (adjust != 0)
		hdr->sh_size += hdr->sh_addralign - adjust;
	    }
	}
    }

  return TRUE;
}

/* Handle a MIPS specific section when reading an object file.  This
   is called when elfcode.h finds a section with an unknown type.
   This routine supports both the 32-bit and 64-bit ELF ABI.

   FIXME: We need to handle the SHF_MIPS_GPREL flag, but I'm not sure
   how to.  */

bfd_boolean
_bfd_riscv_elf_section_from_shdr (bfd *abfd,
				 Elf_Internal_Shdr *hdr,
				 const char *name,
				 int shindex)
{
  flagword flags = 0;

  /* There ought to be a place to keep ELF backend specific flags, but
     at the moment there isn't one.  We just keep track of the
     sections by their name, instead.  Fortunately, the ABI gives
     suggested names for all the MIPS specific sections, so we will
     probably get away with this.  */
  switch (hdr->sh_type)
    {
    case SHT_MIPS_LIBLIST:
      if (strcmp (name, ".liblist") != 0)
	return FALSE;
      break;
    case SHT_MIPS_MSYM:
      if (strcmp (name, ".msym") != 0)
	return FALSE;
      break;
    case SHT_MIPS_CONFLICT:
      if (strcmp (name, ".conflict") != 0)
	return FALSE;
      break;
    case SHT_MIPS_GPTAB:
      BFD_ASSERT (FALSE);
    case SHT_MIPS_UCODE:
      if (strcmp (name, ".ucode") != 0)
	return FALSE;
      break;
    case SHT_MIPS_DEBUG:
      if (strcmp (name, ".mdebug") != 0)
	return FALSE;
      flags = SEC_DEBUGGING;
      break;
    case SHT_MIPS_REGINFO:
      if (strcmp (name, ".reginfo") != 0
	  || hdr->sh_size != sizeof (Elf32_External_RegInfo))
	return FALSE;
      flags = (SEC_LINK_ONCE | SEC_LINK_DUPLICATES_SAME_SIZE);
      break;
    case SHT_MIPS_IFACE:
      if (strcmp (name, ".MIPS.interfaces") != 0)
	return FALSE;
      break;
    case SHT_MIPS_CONTENT:
      if (! CONST_STRNEQ (name, ".MIPS.content"))
	return FALSE;
      break;
    case SHT_MIPS_OPTIONS:
      if (!MIPS_ELF_OPTIONS_SECTION_NAME_P (name))
	return FALSE;
      break;
    case SHT_MIPS_DWARF:
      if (! CONST_STRNEQ (name, ".debug_")
          && ! CONST_STRNEQ (name, ".zdebug_"))
	return FALSE;
      break;
    case SHT_MIPS_SYMBOL_LIB:
      if (strcmp (name, ".MIPS.symlib") != 0)
	return FALSE;
      break;
    case SHT_MIPS_EVENTS:
      if (! CONST_STRNEQ (name, ".MIPS.events")
	  && ! CONST_STRNEQ (name, ".MIPS.post_rel"))
	return FALSE;
      break;
    default:
      break;
    }

  if (! _bfd_elf_make_section_from_shdr (abfd, hdr, name, shindex))
    return FALSE;

  if (flags)
    {
      if (! bfd_set_section_flags (abfd, hdr->bfd_section,
				   (bfd_get_section_flags (abfd,
							   hdr->bfd_section)
				    | flags)))
	return FALSE;
    }

  /* FIXME: We should record sh_info for a .gptab section.  */

  /* For a .reginfo section, set the gp value in the tdata information
     from the contents of this section.  We need the gp value while
     processing relocs, so we just get it now.  The .reginfo section
     is not used in the 64-bit MIPS ELF ABI.  */
  if (hdr->sh_type == SHT_MIPS_REGINFO)
    {
      Elf32_External_RegInfo ext;
      Elf32_RegInfo s;

      if (! bfd_get_section_contents (abfd, hdr->bfd_section,
				      &ext, 0, sizeof ext))
	return FALSE;
      bfd_riscv_elf32_swap_reginfo_in (abfd, &ext, &s);
      elf_gp (abfd) = s.ri_gp_value;
    }

  /* For a SHT_MIPS_OPTIONS section, look for a ODK_REGINFO entry, and
     set the gp value based on what we find.  We may see both
     SHT_MIPS_REGINFO and SHT_MIPS_OPTIONS/ODK_REGINFO; in that case,
     they should agree.  */
  if (hdr->sh_type == SHT_MIPS_OPTIONS)
    {
      bfd_byte *contents, *l, *lend;

      contents = bfd_malloc (hdr->sh_size);
      if (contents == NULL)
	return FALSE;
      if (! bfd_get_section_contents (abfd, hdr->bfd_section, contents,
				      0, hdr->sh_size))
	{
	  free (contents);
	  return FALSE;
	}
      l = contents;
      lend = contents + hdr->sh_size;
      while (l + sizeof (Elf_External_Options) <= lend)
	{
	  Elf_Internal_Options intopt;

	  bfd_riscv_elf_swap_options_in (abfd, (Elf_External_Options *) l,
					&intopt);
	  if (intopt.size < sizeof (Elf_External_Options))
	    {
	      (*_bfd_error_handler)
		(_("%B: Warning: bad `%s' option size %u smaller than its header"),
		abfd, MIPS_ELF_OPTIONS_SECTION_NAME (abfd), intopt.size);
	      break;
	    }
	  if (ABI_64_P (abfd) && intopt.kind == ODK_REGINFO)
	    {
	      Elf64_Internal_RegInfo intreg;

	      bfd_riscv_elf64_swap_reginfo_in
		(abfd,
		 ((Elf64_External_RegInfo *)
		  (l + sizeof (Elf_External_Options))),
		 &intreg);
	      elf_gp (abfd) = intreg.ri_gp_value;
	    }
	  else if (intopt.kind == ODK_REGINFO)
	    {
	      Elf32_RegInfo intreg;

	      bfd_riscv_elf32_swap_reginfo_in
		(abfd,
		 ((Elf32_External_RegInfo *)
		  (l + sizeof (Elf_External_Options))),
		 &intreg);
	      elf_gp (abfd) = intreg.ri_gp_value;
	    }
	  l += intopt.size;
	}
      free (contents);
    }

  return TRUE;
}

/* Set the correct type for a MIPS ELF section.  We do this by the
   section name, which is a hack, but ought to work.  This routine is
   used by both the 32-bit and the 64-bit ABI.  */

bfd_boolean
_bfd_riscv_elf_fake_sections (bfd *abfd ATTRIBUTE_UNUSED,
                              Elf_Internal_Shdr *hdr, asection *sec)
{
  const char *name = bfd_get_section_name (abfd, sec);

  if (strcmp (name, ".liblist") == 0)
    {
      hdr->sh_type = SHT_MIPS_LIBLIST;
      hdr->sh_info = sec->size / sizeof (Elf32_Lib);
      /* The sh_link field is set in final_write_processing.  */
    }
  else if (strcmp (name, ".conflict") == 0)
    hdr->sh_type = SHT_MIPS_CONFLICT;
  else if (CONST_STRNEQ (name, ".gptab."))
    BFD_ASSERT (FALSE);
  else if (strcmp (name, ".ucode") == 0)
    hdr->sh_type = SHT_MIPS_UCODE;
  else if (strcmp (name, ".mdebug") == 0)
    {
      hdr->sh_type = SHT_MIPS_DEBUG;
      hdr->sh_entsize = 1;
    }
  else if (strcmp (name, ".reginfo") == 0)
    {
      hdr->sh_type = SHT_MIPS_REGINFO;
      /* In a shared object on IRIX 5.3, the .reginfo section has an
         entsize of 0x18.  FIXME: Does this matter?  */
      hdr->sh_entsize = sizeof (Elf32_External_RegInfo);
    }
  else if (strcmp (name, ".got") == 0
	   || strcmp (name, ".srdata") == 0
	   || strcmp (name, ".sdata") == 0
	   || strcmp (name, ".sbss") == 0
	   || strcmp (name, ".lit4") == 0
	   || strcmp (name, ".lit8") == 0)
    hdr->sh_flags |= SHF_MIPS_GPREL;
  else if (strcmp (name, ".MIPS.interfaces") == 0)
    {
      hdr->sh_type = SHT_MIPS_IFACE;
      hdr->sh_flags |= SHF_MIPS_NOSTRIP;
    }
  else if (CONST_STRNEQ (name, ".MIPS.content"))
    {
      hdr->sh_type = SHT_MIPS_CONTENT;
      hdr->sh_flags |= SHF_MIPS_NOSTRIP;
      /* The sh_info field is set in final_write_processing.  */
    }
  else if (MIPS_ELF_OPTIONS_SECTION_NAME_P (name))
    {
      hdr->sh_type = SHT_MIPS_OPTIONS;
      hdr->sh_entsize = 1;
      hdr->sh_flags |= SHF_MIPS_NOSTRIP;
    }
  else if (CONST_STRNEQ (name, ".debug_")
           || CONST_STRNEQ (name, ".zdebug_"))
    {
      hdr->sh_type = SHT_MIPS_DWARF;
    }
  else if (strcmp (name, ".MIPS.symlib") == 0)
    {
      hdr->sh_type = SHT_MIPS_SYMBOL_LIB;
      /* The sh_link and sh_info fields are set in
         final_write_processing.  */
    }
  else if (CONST_STRNEQ (name, ".MIPS.events")
	   || CONST_STRNEQ (name, ".MIPS.post_rel"))
    {
      hdr->sh_type = SHT_MIPS_EVENTS;
      hdr->sh_flags |= SHF_MIPS_NOSTRIP;
      /* The sh_link field is set in final_write_processing.  */
    }
  else if (strcmp (name, ".msym") == 0)
    {
      hdr->sh_type = SHT_MIPS_MSYM;
      hdr->sh_flags |= SHF_ALLOC;
      hdr->sh_entsize = 8;
    }

  /* The generic elf_fake_sections will set up REL_HDR using the default
   kind of relocations.  We used to set up a second header for the
   non-default kind of relocations here, but only NewABI would use
   these, and the IRIX ld doesn't like resulting empty RELA sections.
   Thus we create those header only on demand now.  */

  return TRUE;
}

/* Given a BFD section, try to locate the corresponding ELF section
   index.  This is used by both the 32-bit and the 64-bit ABI.
   Actually, it's not clear to me that the 64-bit ABI supports these,
   but for non-PIC objects we will certainly want support for at least
   the .scommon section.  */

bfd_boolean
_bfd_riscv_elf_section_from_bfd_section (bfd *abfd ATTRIBUTE_UNUSED,
					asection *sec, int *retval)
{
  if (strcmp (bfd_get_section_name (abfd, sec), ".scommon") == 0)
    {
      *retval = SHN_MIPS_SCOMMON;
      return TRUE;
    }
  if (strcmp (bfd_get_section_name (abfd, sec), ".acommon") == 0)
    {
      *retval = SHN_MIPS_ACOMMON;
      return TRUE;
    }
  return FALSE;
}

/* Hook called by the linker routine which adds symbols from an object
   file.  We must handle the special MIPS section numbers here.  */

bfd_boolean
_bfd_riscv_elf_add_symbol_hook (bfd *abfd ATTRIBUTE_UNUSED,
             struct bfd_link_info *info ATTRIBUTE_UNUSED,
			       Elf_Internal_Sym *sym, const char **namep ATTRIBUTE_UNUSED,
			       flagword *flagsp ATTRIBUTE_UNUSED,
			       asection **secp, bfd_vma *valp)
{
  switch (sym->st_shndx)
    {
    case SHN_COMMON:
      break;

    case SHN_MIPS_SCOMMON:
      *secp = bfd_make_section_old_way (abfd, ".scommon");
      (*secp)->flags |= SEC_IS_COMMON;
      *valp = sym->st_size;
      break;

    case SHN_MIPS_TEXT:
      /* This section is used in a shared object.  */
      if (elf_tdata (abfd)->elf_text_section == NULL)
	{
	  asymbol *elf_text_symbol;
	  asection *elf_text_section;
	  bfd_size_type amt = sizeof (asection);

	  elf_text_section = bfd_zalloc (abfd, amt);
	  if (elf_text_section == NULL)
	    return FALSE;

	  amt = sizeof (asymbol);
	  elf_text_symbol = bfd_zalloc (abfd, amt);
	  if (elf_text_symbol == NULL)
	    return FALSE;

	  /* Initialize the section.  */

	  elf_tdata (abfd)->elf_text_section = elf_text_section;
	  elf_tdata (abfd)->elf_text_symbol = elf_text_symbol;

	  elf_text_section->symbol = elf_text_symbol;
	  elf_text_section->symbol_ptr_ptr = &elf_tdata (abfd)->elf_text_symbol;

	  elf_text_section->name = ".text";
	  elf_text_section->flags = SEC_NO_FLAGS;
	  elf_text_section->output_section = NULL;
	  elf_text_section->owner = abfd;
	  elf_text_symbol->name = ".text";
	  elf_text_symbol->flags = BSF_SECTION_SYM | BSF_DYNAMIC;
	  elf_text_symbol->section = elf_text_section;
	}
      /* This code used to do *secp = bfd_und_section_ptr if
         info->shared.  I don't know why, and that doesn't make sense,
         so I took it out.  */
      *secp = elf_tdata (abfd)->elf_text_section;
      break;

    case SHN_MIPS_ACOMMON:
      /* Fall through. XXX Can we treat this as allocated data?  */
    case SHN_MIPS_DATA:
      /* This section is used in a shared object.  */
      if (elf_tdata (abfd)->elf_data_section == NULL)
	{
	  asymbol *elf_data_symbol;
	  asection *elf_data_section;
	  bfd_size_type amt = sizeof (asection);

	  elf_data_section = bfd_zalloc (abfd, amt);
	  if (elf_data_section == NULL)
	    return FALSE;

	  amt = sizeof (asymbol);
	  elf_data_symbol = bfd_zalloc (abfd, amt);
	  if (elf_data_symbol == NULL)
	    return FALSE;

	  /* Initialize the section.  */

	  elf_tdata (abfd)->elf_data_section = elf_data_section;
	  elf_tdata (abfd)->elf_data_symbol = elf_data_symbol;

	  elf_data_section->symbol = elf_data_symbol;
	  elf_data_section->symbol_ptr_ptr = &elf_tdata (abfd)->elf_data_symbol;

	  elf_data_section->name = ".data";
	  elf_data_section->flags = SEC_NO_FLAGS;
	  elf_data_section->output_section = NULL;
	  elf_data_section->owner = abfd;
	  elf_data_symbol->name = ".data";
	  elf_data_symbol->flags = BSF_SECTION_SYM | BSF_DYNAMIC;
	  elf_data_symbol->section = elf_data_section;
	}
      /* This code used to do *secp = bfd_und_section_ptr if
         info->shared.  I don't know why, and that doesn't make sense,
         so I took it out.  */
      *secp = elf_tdata (abfd)->elf_data_section;
      break;

    case SHN_MIPS_SUNDEFINED:
      *secp = bfd_und_section_ptr;
      break;
    }

  return TRUE;
}

/* This hook function is called before the linker writes out a global
   symbol.  We mark symbols as small common if appropriate.  This is
   also where we undo the increment of the value for a mips16 symbol.  */

int
_bfd_riscv_elf_link_output_symbol_hook
  (struct bfd_link_info *info ATTRIBUTE_UNUSED,
   const char *name ATTRIBUTE_UNUSED, Elf_Internal_Sym *sym,
   asection *input_sec, struct elf_link_hash_entry *h ATTRIBUTE_UNUSED)
{
  /* If we see a common symbol, which implies a relocatable link, then
     if a symbol was small common in an input file, mark it as small
     common in the output file.  */
  if (sym->st_shndx == SHN_COMMON
      && strcmp (input_sec->name, ".scommon") == 0)
    sym->st_shndx = SHN_MIPS_SCOMMON;

  if (ELF_ST_IS_MIPS16 (sym->st_other))
    sym->st_value &= ~1;

  return 1;
}

/* Functions for the dynamic linker.  */

/* Create dynamic sections when linking against a dynamic object.  */

bfd_boolean
_bfd_riscv_elf_create_dynamic_sections (bfd *abfd, struct bfd_link_info *info)
{
  struct elf_link_hash_entry *h;
  struct bfd_link_hash_entry *bh;
  flagword flags;
  register asection *s;
  struct mips_elf_link_hash_table *htab;

  htab = mips_elf_hash_table (info);
  BFD_ASSERT (htab != NULL);

  flags = (SEC_ALLOC | SEC_LOAD | SEC_HAS_CONTENTS | SEC_IN_MEMORY
	   | SEC_LINKER_CREATED | SEC_READONLY);

  /* The psABI requires a read-only .dynamic section. */
  s = bfd_get_section_by_name (abfd, ".dynamic");
  if (s != NULL)
    {
      if (! bfd_set_section_flags (abfd, s, flags))
        return FALSE;
    }

  /* We need to create .got section.  */
  if (!mips_elf_create_got_section (abfd, info))
    return FALSE;

  if (! mips_elf_rel_dyn_section (info, TRUE))
    return FALSE;

  /* Create .stub section.  */
  s = bfd_make_section_with_flags (abfd,
				   MIPS_ELF_STUB_SECTION_NAME (abfd),
				   flags | SEC_CODE);
  if (s == NULL
      || ! bfd_set_section_alignment (abfd, s,
				      MIPS_ELF_LOG_FILE_ALIGN (abfd)))
    return FALSE;
  htab->sstubs = s;

  if (!info->shared && bfd_get_section_by_name (abfd, ".rld_map") == NULL)
    {
      s = bfd_make_section_with_flags (abfd, ".rld_map",
				       flags &~ (flagword) SEC_READONLY);
      if (s == NULL
	  || ! bfd_set_section_alignment (abfd, s,
					  MIPS_ELF_LOG_FILE_ALIGN (abfd)))
	return FALSE;
    }

  if (!info->shared)
    {
      const char *name;

      name = "_DYNAMIC_LINKING";
      bh = NULL;
      if (!(_bfd_generic_link_add_one_symbol
	    (info, abfd, name, BSF_GLOBAL, bfd_abs_section_ptr, 0,
	     NULL, FALSE, get_elf_backend_data (abfd)->collect, &bh)))
	return FALSE;

      h = (struct elf_link_hash_entry *) bh;
      h->non_elf = 0;
      h->def_regular = 1;
      h->type = STT_SECTION;

      if (! bfd_elf_link_record_dynamic_symbol (info, h))
	return FALSE;

      if (! mips_elf_hash_table (info)->use_rld_obj_head)
	{
	  /* __rld_map is a four byte word located in the .data section
	     and is filled in by the rtld to contain a pointer to
	     the _r_debug structure. Its symbol value will be set in
	     _bfd_riscv_elf_finish_dynamic_symbol.  */
	  s = bfd_get_section_by_name (abfd, ".rld_map");
	  BFD_ASSERT (s != NULL);

	  name = "__RLD_MAP";
	  bh = NULL;
	  if (!(_bfd_generic_link_add_one_symbol
		(info, abfd, name, BSF_GLOBAL, s, 0, NULL, FALSE,
		 get_elf_backend_data (abfd)->collect, &bh)))
	    return FALSE;

	  h = (struct elf_link_hash_entry *) bh;
	  h->non_elf = 0;
	  h->def_regular = 1;
	  h->type = STT_OBJECT;

	  if (! bfd_elf_link_record_dynamic_symbol (info, h))
	    return FALSE;
	}
    }

  /* Create the .plt, .rel(a).plt, .dynbss and .rel(a).bss sections.
     Also create the _PROCEDURE_LINKAGE_TABLE symbol.  */
  if (!_bfd_elf_create_dynamic_sections (abfd, info))
    return FALSE;

  /* Cache the sections created above.  */
  htab->splt = bfd_get_section_by_name (abfd, ".plt");
  htab->sdynbss = bfd_get_section_by_name (abfd, ".dynbss");
  htab->srelplt = bfd_get_section_by_name (abfd, ".rel.plt");
  if (!htab->sdynbss
      || !htab->srelplt
      || !htab->splt)
    abort ();

  if (!info->shared)
    {
      /* All variants of the plt0 entry are the same size.  */
      htab->plt_header_size = RISCV_PLT0_ENTRY_INSNS * 4;
      htab->plt_entry_size = RISCV_PLT_ENTRY_INSNS * 4;
    }

  return TRUE;
}

/* Return true if relocation REL against section SEC is a REL rather than
   RELA relocation.  RELOCS is the first relocation in the section and
   ABFD is the bfd that contains SEC.  */

static bfd_boolean
mips_elf_rel_relocation_p (bfd *abfd, asection *sec,
			   const Elf_Internal_Rela *relocs,
			   const Elf_Internal_Rela *rel)
{
  Elf_Internal_Shdr *rel_hdr;
  const struct elf_backend_data *bed;

  /* To determine which flavor of relocation this is, we depend on the
     fact that the INPUT_SECTION's REL_HDR is read before RELA_HDR.  */
  rel_hdr = elf_section_data (sec)->rel.hdr;
  if (rel_hdr == NULL)
    return FALSE;
  bed = get_elf_backend_data (abfd);
  return ((size_t) (rel - relocs)
	  < NUM_SHDR_ENTRIES (rel_hdr) * bed->s->int_rels_per_ext_rel);
}

/* Read the addend for REL relocation REL, which belongs to bfd ABFD.
   HOWTO is the relocation's howto and CONTENTS points to the contents
   of the section that REL is against.  */

static bfd_vma
mips_elf_read_rel_addend (bfd *abfd, const Elf_Internal_Rela *rel,
			  reloc_howto_type *howto, bfd_byte *contents)
{
  bfd_vma addend;

  /* Get the addend, which is stored in the input file.  */
  addend = mips_elf_obtain_contents (howto, rel, abfd, contents);

  return addend & howto->src_mask;
}

/* REL is a relocation in ABFD that needs a partnering LO16 relocation
   and *ADDEND is the addend for REL itself.  Look for the LO16 relocation
   and update *ADDEND with the final addend.  Return true on success
   or false if the LO16 could not be found.  RELEND is the exclusive
   upper bound on the relocations for REL's section.  */

static bfd_boolean
mips_elf_add_lo16_rel_addend (bfd *abfd,
			      const Elf_Internal_Rela *rel,
			      const Elf_Internal_Rela *relend,
			      bfd_byte *contents, bfd_vma *addend)
{
  unsigned int lo16_type;
  const Elf_Internal_Rela *lo16_relocation;
  reloc_howto_type *lo16_howto;
  bfd_vma l;

  lo16_type = R_RISCV_LO16;

  /* The combined value is the sum of the HI16 addend, left-shifted by
     sixteen bits, and the LO16 addend, sign extended.  (Usually, the
     code does a `lui' of the HI16 value, and then an `addiu' of the
     LO16 value.)

     Scan ahead to find a matching LO16 relocation.

     According to the MIPS ELF ABI, the R_RISCV_LO16 relocation must
     be immediately following.  However, for the IRIX6 ABI, the next
     relocation may be a composed relocation consisting of several
     relocations for the same address.  In that case, the R_RISCV_LO16
     relocation may occur as one of these.  We permit a similar
     extension in general, as that is useful for GCC.

     In some cases GCC dead code elimination removes the LO16 but keeps
     the corresponding HI16.  This is strictly speaking a violation of
     the ABI but not immediately harmful.  */
  lo16_relocation = mips_elf_next_relocation (abfd, lo16_type, rel, relend);
  if (lo16_relocation == NULL)
    return FALSE;

  /* Obtain the addend kept there.  */
  lo16_howto = MIPS_ELF_RTYPE_TO_HOWTO (abfd, lo16_type, FALSE);
  l = mips_elf_read_rel_addend (abfd, lo16_relocation, lo16_howto, contents);

  l <<= lo16_howto->rightshift;
  l = _bfd_riscv_elf_sign_extend (l, RISCV_IMM_BITS);

  *addend <<= RISCV_IMM_BITS;
  *addend += l;
  return TRUE;
}

/* Try to read the contents of section SEC in bfd ABFD.  Return true and
   store the contents in *CONTENTS on success.  Assume that *CONTENTS
   already holds the contents if it is nonull on entry.  */

static bfd_boolean
mips_elf_get_section_contents (bfd *abfd, asection *sec, bfd_byte **contents)
{
  if (*contents)
    return TRUE;

  /* Get cached copy if it exists.  */
  if (elf_section_data (sec)->this_hdr.contents != NULL)
    {
      *contents = elf_section_data (sec)->this_hdr.contents;
      return TRUE;
    }

  return bfd_malloc_and_get_section (abfd, sec, contents);
}

/* Look through the relocs for a section during the first phase, and
   allocate space in the global offset table.  */

bfd_boolean
_bfd_riscv_elf_check_relocs (bfd *abfd, struct bfd_link_info *info,
			    asection *sec, const Elf_Internal_Rela *relocs)
{
  const char *name;
  bfd *dynobj;
  Elf_Internal_Shdr *symtab_hdr;
  struct elf_link_hash_entry **sym_hashes;
  size_t extsymoff;
  const Elf_Internal_Rela *rel;
  const Elf_Internal_Rela *rel_end;
  asection *sreloc;
  const struct elf_backend_data *bed;
  struct mips_elf_link_hash_table *htab;
  bfd_byte *contents;
  bfd_vma addend;
  reloc_howto_type *howto;

  if (info->relocatable)
    return TRUE;

  htab = mips_elf_hash_table (info);
  BFD_ASSERT (htab != NULL);

  dynobj = elf_hash_table (info)->dynobj;
  symtab_hdr = &elf_tdata (abfd)->symtab_hdr;
  sym_hashes = elf_sym_hashes (abfd);
  extsymoff = (elf_bad_symtab (abfd)) ? 0 : symtab_hdr->sh_info;

  bed = get_elf_backend_data (abfd);
  rel_end = relocs + sec->reloc_count * bed->s->int_rels_per_ext_rel;

  name = bfd_get_section_name (abfd, sec);

  sreloc = NULL;
  contents = NULL;
  for (rel = relocs; rel < rel_end; ++rel)
    {
      unsigned long r_symndx;
      unsigned int r_type;
      struct elf_link_hash_entry *h;
      bfd_boolean can_make_dynamic_p;

      r_symndx = ELF_R_SYM (abfd, rel->r_info);
      r_type = ELF_R_TYPE (abfd, rel->r_info);

      if (r_symndx < extsymoff)
	h = NULL;
      else if (r_symndx >= extsymoff + NUM_SHDR_ENTRIES (symtab_hdr))
	{
	  (*_bfd_error_handler)
	    (_("%B: Malformed reloc detected for section %s"),
	     abfd, name);
	  bfd_set_error (bfd_error_bad_value);
	  return FALSE;
	}
      else
	{
	  h = sym_hashes[r_symndx - extsymoff];
	  while (h != NULL
		 && (h->root.type == bfd_link_hash_indirect
		     || h->root.type == bfd_link_hash_warning))
	    h = (struct elf_link_hash_entry *) h->root.u.i.link;
	}

      /* Set CAN_MAKE_DYNAMIC_P to true if we can convert this
	 relocation into a dynamic one.  */
      can_make_dynamic_p = FALSE;
      switch (r_type)
	{
	case R_MIPS16_GOT16:
	case R_MIPS16_CALL16:
	case R_RISCV_GOT16:
	case R_RISCV_CALL16:
	case R_RISCV_CALL_HI16:
	case R_RISCV_CALL_LO16:
	case R_RISCV_GOT_HI16:
	case R_RISCV_GOT_LO16:
	case R_RISCV_GOT_DISP:
	case R_RISCV_TLS_GOTTPREL:
	case R_RISCV_TLS_GOT_HI16:
	case R_RISCV_TLS_GOT_LO16:
	case R_RISCV_TLS_GD:
	case R_RISCV_TLS_GD_HI16:
	case R_RISCV_TLS_GD_LO16:
	case R_RISCV_TLS_LDM:
	case R_RISCV_TLS_LDM_HI16:
	case R_RISCV_TLS_LDM_LO16:
	  if (dynobj == NULL)
	    elf_hash_table (info)->dynobj = dynobj = abfd;
	  if (!mips_elf_create_got_section (dynobj, info))
	    return FALSE;
	  break;

	  /* This is just a hint; it can safely be ignored.  Don't set
	     has_static_relocs for the corresponding symbol.  */
	case R_RISCV_JALR:
	  break;

	case R_RISCV_32:
	case R_RISCV_REL32:
	case R_RISCV_64:
	  /* For executables that use PLTs and copy-relocs, we have a
	     choice between converting the relocation into a dynamic
	     one or using copy relocations or PLT entries.  It is
	     usually better to do the former, unless the relocation is
	     against a read-only section.  */
	  if ((info->shared
	       || (h != NULL
		   && strcmp (h->root.root.string, "__gnu_local_gp") != 0
		   && !(!info->nocopyreloc
			&& !PIC_OBJECT_P (abfd)
			&& MIPS_ELF_READONLY_SECTION (sec))))
	      && (sec->flags & SEC_ALLOC) != 0)
	    {
	      can_make_dynamic_p = TRUE;
	      if (dynobj == NULL)
		elf_hash_table (info)->dynobj = dynobj = abfd;
	      break;
	    }
	  /* For sections that are not SEC_ALLOC a copy reloc would be
	     output if possible (implying questionable semantics for
	     read-only data objects) or otherwise the final link would
	     fail as ld.so will not process them and could not therefore
	     handle any outstanding dynamic relocations.

	     For such sections that are also SEC_DEBUGGING, we can avoid
	     these problems by simply ignoring any relocs as these
	     sections have a predefined use and we know it is safe to do
	     so.

	     This is needed in cases such as a global symbol definition
	     in a shared library causing a common symbol from an object
	     file to be converted to an undefined reference.  If that
	     happens, then all the relocations against this symbol from
	     SEC_DEBUGGING sections in the object file will resolve to
	     nil.  */
	  if ((sec->flags & SEC_DEBUGGING) != 0)
	    break;
	  /* Fall through.  */

	default:
	  /* Most static relocations require pointer equality, except
	     for branches.  */
	  if (h)
	    h->pointer_equality_needed = TRUE;
	  /* Fall through.  */

	case R_RISCV_26:
	case R_RISCV_PC16:
	case R_MIPS16_26:
	  if (h)
	    ((struct mips_elf_link_hash_entry *) h)->has_static_relocs = TRUE;
	  break;
	}

      if ((h == NULL && r_type == R_RISCV_CALL_LO16)
	       || r_type == R_RISCV_GOT_LO16
	       || r_type == R_RISCV_GOT_DISP)
	{
	  /* We may need a local GOT entry for this relocation.  We
	     don't count R_RISCV_GOT_PAGE because we can estimate the
	     maximum number of pages needed by looking at the size of
	     the segment.  Similar comments apply to R_MIPS*_GOT16 and
	     R_MIPS*_CALL16, except on VxWorks, where GOT relocations
	     always evaluate to "G".  We don't count R_RISCV_GOT_HI16, or
	     R_RISCV_CALL_HI16 because these are always followed by an
	     R_RISCV_GOT_LO16 or R_RISCV_CALL_LO16.  */
	  if (!mips_elf_record_local_got_symbol (abfd, r_symndx,
						 rel->r_addend, info, 0))
	    return FALSE;
	}

      if (h != NULL && mips_elf_relocation_needs_la25_stub (abfd, r_type))
	((struct mips_elf_link_hash_entry *) h)->has_nonpic_branches = TRUE;

      switch (r_type)
	{
	case R_RISCV_CALL16:
	case R_MIPS16_CALL16:
	  if (h == NULL)
	    {
	      (*_bfd_error_handler)
		(_("%B: CALL16 reloc at 0x%lx not against global symbol"),
		 abfd, (unsigned long) rel->r_offset);
	      bfd_set_error (bfd_error_bad_value);
	      return FALSE;
	    }
	  /* Fall through.  */

	case R_RISCV_CALL_HI16:
	case R_RISCV_CALL_LO16:
	  if (h != NULL)
	    {
	      /* Make sure there is room in the regular GOT to hold the
		 function's address.  We may eliminate it in favour of
		 a .got.plt entry later; see mips_elf_count_got_symbols.  */
	      if (!mips_elf_record_global_got_symbol (h, abfd, info, TRUE, 0))
		return FALSE;

	      /* We need a stub, not a plt entry for the undefined
		 function.  But we record it as if it needs plt.  See
		 _bfd_elf_adjust_dynamic_symbol.  */
	      h->needs_plt = 1;
	      h->type = STT_FUNC;
	    }
	  break;

	case R_MIPS16_GOT16:
	case R_RISCV_GOT16:
	case R_RISCV_GOT_HI16:
	case R_RISCV_GOT_LO16:
	  if (!h)
	    {
	      /* This relocation needs a page entry in the GOT. */
	      if (mips_elf_rel_relocation_p (abfd, sec, relocs, rel))
		{
		  if (!mips_elf_get_section_contents (abfd, sec, &contents))
		    return FALSE;
		  howto = MIPS_ELF_RTYPE_TO_HOWTO (abfd, r_type, FALSE);
		  addend = mips_elf_read_rel_addend (abfd, rel,
						     howto, contents);
		  if (got16_reloc_p (r_type))
		    mips_elf_add_lo16_rel_addend (abfd, rel, rel_end,
						  contents, &addend);
		  else
		    addend <<= howto->rightshift;
		}
	      else
		addend = rel->r_addend;
	      if (!mips_elf_record_got_page_entry (info, abfd, r_symndx,
						   addend))
		return FALSE;
	    }
	  /* Fall through.  */

	case R_RISCV_GOT_DISP:
	  if (h && !mips_elf_record_global_got_symbol (h, abfd, info,
						       FALSE, 0))
	    return FALSE;
	  break;

	case R_RISCV_TLS_GOTTPREL:
	case R_RISCV_TLS_GOT_HI16:
	case R_RISCV_TLS_GOT_LO16:
	  if (info->shared)
	    info->flags |= DF_STATIC_TLS;
	  /* Fall through */

	case R_RISCV_TLS_LDM:
	case R_RISCV_TLS_LDM_HI16:
	case R_RISCV_TLS_LDM_LO16:
	  if (TLS_LDM_RELOC_P(r_type))
	    {
	      r_symndx = STN_UNDEF;
	      h = NULL;
	    }
	  /* Fall through */

	case R_RISCV_TLS_GD:
	case R_RISCV_TLS_GD_HI16:
	case R_RISCV_TLS_GD_LO16:
	  /* This symbol requires a global offset table entry, or two
	     for TLS GD relocations.  */
	  {
	    unsigned char flag = (TLS_GD_RELOC_P(r_type)
				  ? GOT_TLS_GD
				  : TLS_LDM_RELOC_P(r_type)
				  ? GOT_TLS_LDM
				  : GOT_TLS_IE);
	    if (h != NULL)
	      {
		struct mips_elf_link_hash_entry *hmips =
		  (struct mips_elf_link_hash_entry *) h;
		hmips->tls_type |= flag;

		if (h && !mips_elf_record_global_got_symbol (h, abfd, info,
							     FALSE, flag))
		  return FALSE;
	      }
	    else
	      {
		BFD_ASSERT (flag == GOT_TLS_LDM || r_symndx != STN_UNDEF);

		if (!mips_elf_record_local_got_symbol (abfd, r_symndx,
						       rel->r_addend,
						       info, flag))
		  return FALSE;
	      }
	  }
	  break;

	case R_RISCV_32:
	case R_RISCV_REL32:
	case R_RISCV_64:
	  /* In VxWorks executables, references to external symbols
	     are handled using copy relocs or PLT stubs, so there's
	     no need to add a .rela.dyn entry for this relocation.  */
	  if (can_make_dynamic_p)
	    {
	      if (sreloc == NULL)
		{
		  sreloc = mips_elf_rel_dyn_section (info, TRUE);
		  if (sreloc == NULL)
		    return FALSE;
		}
	      if (info->shared && h == NULL)
		{
		  /* When creating a shared object, we must copy these
		     reloc types into the output file as R_RISCV_REL32
		     relocs.  Make room for this reloc in .rel(a).dyn.  */
		  mips_elf_allocate_dynamic_relocations (dynobj, info, 1);
		  if (MIPS_ELF_READONLY_SECTION (sec))
		    /* We tell the dynamic linker that there are
		       relocations against the text segment.  */
		    info->flags |= DF_TEXTREL;
		}
	      else
		{
		  struct mips_elf_link_hash_entry *hmips;

		  /* For a shared object, we must copy this relocation
		     unless the symbol turns out to be undefined and
		     weak with non-default visibility, in which case
		     it will be left as zero.

		     We could elide R_RISCV_REL32 for locally binding symbols
		     in shared libraries, but do not yet do so.

		     For an executable, we only need to copy this
		     reloc if the symbol is defined in a dynamic
		     object.  */
		  hmips = (struct mips_elf_link_hash_entry *) h;
		  ++hmips->possibly_dynamic_relocs;
		  if (MIPS_ELF_READONLY_SECTION (sec))
		    /* We need it to tell the dynamic linker if there
		       are relocations against the text segment.  */
		    hmips->readonly_reloc = TRUE;
		}
	    }

	  break;

	case R_RISCV_26:
	case R_RISCV_GPREL16:
	case R_RISCV_LITERAL:
	case R_RISCV_GPREL32:
	  break;

	  /* This relocation describes the C++ object vtable hierarchy.
	     Reconstruct it for later use during GC.  */
	case R_RISCV_GNU_VTINHERIT:
	  if (!bfd_elf_gc_record_vtinherit (abfd, sec, h, rel->r_offset))
	    return FALSE;
	  break;

	  /* This relocation describes which C++ vtable entries are actually
	     used.  Record for later use during GC.  */
	case R_RISCV_GNU_VTENTRY:
	  BFD_ASSERT (h != NULL);
	  if (h != NULL
	      && !bfd_elf_gc_record_vtentry (abfd, sec, h, rel->r_offset))
	    return FALSE;
	  break;

	default:
	  break;
	}

      /* We must not create a stub for a symbol that has relocations
	 related to taking the function's address. */
      if (h != NULL)
	switch (r_type)
	  {
	  default:
	    ((struct mips_elf_link_hash_entry *) h)->no_fn_stub = TRUE;
	    break;
	  case R_MIPS16_CALL16:
	  case R_RISCV_CALL16:
	  case R_RISCV_CALL_HI16:
	  case R_RISCV_CALL_LO16:
	  case R_RISCV_JALR:
	    break;
	  }

      /* Refuse some position-dependent relocations when creating a
	 shared library.  Do not refuse R_RISCV_32 / R_RISCV_64; they're
	 not PIC, but we can create dynamic relocations and the result
	 will be fine.  Also do not refuse R_RISCV_LO16, which can be
	 combined with R_RISCV_GOT16.  */
      if (info->shared)
	{
	  switch (r_type)
	    {
	    case R_MIPS16_HI16:
	    case R_RISCV_HI16:
	      /* Don't refuse a high part relocation if it's against
		 no symbol (e.g. part of a compound relocation).  */
	      if (r_symndx == STN_UNDEF)
		break;

	      howto = MIPS_ELF_RTYPE_TO_HOWTO (abfd, r_type, FALSE);
	      (*_bfd_error_handler)
		(_("%B: relocation %s against `%s' can not be used when making a shared object; recompile with -fPIC"),
		 abfd, howto->name,
		 (h) ? h->root.root.string : "a local symbol");
	      bfd_set_error (bfd_error_bad_value);
	      return FALSE;
	    default:
	      break;
	    }
	}
    }

  return TRUE;
}

bfd_boolean
_bfd_riscv_relax_section (bfd *abfd ATTRIBUTE_UNUSED,
			 asection *sec ATTRIBUTE_UNUSED,
			 struct bfd_link_info *link_info ATTRIBUTE_UNUSED,
			 bfd_boolean *again)
{
  *again = FALSE;
  return TRUE;
}

/* Allocate space for global sym dynamic relocs.  */

static bfd_boolean
allocate_dynrelocs (struct elf_link_hash_entry *h, void *inf)
{
  struct bfd_link_info *info = inf;
  bfd *dynobj;
  struct mips_elf_link_hash_entry *hmips;
  struct mips_elf_link_hash_table *htab;

  htab = mips_elf_hash_table (info);
  BFD_ASSERT (htab != NULL);

  dynobj = elf_hash_table (info)->dynobj;
  hmips = (struct mips_elf_link_hash_entry *) h;

  /* Ignore indirect and warning symbols.  All relocations against
     such symbols will be redirected to the target symbol.  */
  if (h->root.type == bfd_link_hash_indirect
      || h->root.type == bfd_link_hash_warning)
    return TRUE;

  /* If this symbol is defined in a dynamic object, or we are creating
     a shared library, we will need to copy any R_RISCV_32 or
     R_RISCV_REL32 relocs against it into the output file.  */
  if (! info->relocatable
      && hmips->possibly_dynamic_relocs != 0
      && (h->root.type == bfd_link_hash_defweak
	  || !h->def_regular
	  || info->shared))
    {
      bfd_boolean do_copy = TRUE;

      if (h->root.type == bfd_link_hash_undefweak)
	{
	  /* Do not copy relocations for undefined weak symbols with
	     non-default visibility.  */
	  if (ELF_ST_VISIBILITY (h->other) != STV_DEFAULT)
	    do_copy = FALSE;

	  /* Make sure undefined weak symbols are output as a dynamic
	     symbol in PIEs.  */
	  else if (h->dynindx == -1 && !h->forced_local)
	    {
	      if (! bfd_elf_link_record_dynamic_symbol (info, h))
		return FALSE;
	    }
	}

      if (do_copy)
	{
	  /* Even though we don't directly need a GOT entry for this symbol,
	     the SVR4 psABI requires it to have a dynamic symbol table
	     index greater that DT_MIPS_GOTSYM if there are dynamic
	     relocations against it. */
	  if (hmips->global_got_area > GGA_RELOC_ONLY)
	    hmips->global_got_area = GGA_RELOC_ONLY;
	  hmips->got_only_for_calls = FALSE;

	  mips_elf_allocate_dynamic_relocations
	    (dynobj, info, hmips->possibly_dynamic_relocs);
	  if (hmips->readonly_reloc)
	    /* We tell the dynamic linker that there are relocations
	       against the text segment.  */
	    info->flags |= DF_TEXTREL;
	}
    }

  return TRUE;
}

/* Adjust a symbol defined by a dynamic object and referenced by a
   regular object.  The current definition is in some section of the
   dynamic object, but we're not including those sections.  We have to
   change the definition to something the rest of the link can
   understand.  */

bfd_boolean
_bfd_riscv_elf_adjust_dynamic_symbol (struct bfd_link_info *info,
				     struct elf_link_hash_entry *h)
{
  bfd *dynobj;
  struct mips_elf_link_hash_entry *hmips;
  struct mips_elf_link_hash_table *htab;

  htab = mips_elf_hash_table (info);
  BFD_ASSERT (htab != NULL);

  dynobj = elf_hash_table (info)->dynobj;
  hmips = (struct mips_elf_link_hash_entry *) h;

  /* Make sure we know what is going on here.  */
  BFD_ASSERT (dynobj != NULL
	      && (h->needs_plt
		  || h->u.weakdef != NULL
		  || (h->def_dynamic
		      && h->ref_regular
		      && !h->def_regular)));

  hmips = (struct mips_elf_link_hash_entry *) h;

  /* If there are call relocations against an externally-defined symbol,
     see whether we can create a MIPS lazy-binding stub for it.  We can
     only do this if all references to the function are through call
     relocations, and in that case, the traditional lazy-binding stubs
     are much more efficient than PLT entries. */
  if (h->needs_plt && !hmips->no_fn_stub)
    {
      if (! elf_hash_table (info)->dynamic_sections_created)
	return TRUE;

      /* If this symbol is not defined in a regular file, then set
	 the symbol to the stub location.  This is required to make
	 function pointers compare as equal between the normal
	 executable and the shared library.  */
      if (!h->def_regular)
	{
	  hmips->needs_lazy_stub = TRUE;
	  htab->lazy_stub_count++;
	  return TRUE;
	}
    }
  /* As above, VxWorks requires PLT entries for externally-defined
     functions that are only accessed through call relocations.

     Both VxWorks and non-VxWorks targets also need PLT entries if there
     are static-only relocations against an externally-defined function.
     This can technically occur for shared libraries if there are
     branches to the symbol, although it is unlikely that this will be
     used in practice due to the short ranges involved.  It can occur
     for any relative or absolute relocation in executables; in that
     case, the PLT entry becomes the function's canonical address.  */
  else if (((h->needs_plt && !hmips->no_fn_stub)
	    || (h->type == STT_FUNC && hmips->has_static_relocs))
	   && htab->use_plts_and_copy_relocs
	   && !SYMBOL_CALLS_LOCAL (info, h)
	   && !(ELF_ST_VISIBILITY (h->other) != STV_DEFAULT
		&& h->root.type == bfd_link_hash_undefweak))
    {
      /* If this is the first symbol to need a PLT entry, allocate room
	 for the header.  */
      if (htab->splt->size == 0)
	{
	  BFD_ASSERT (htab->sgotplt->size == 0);

	  /* If we're using the PLT additions to the psABI, each PLT
	     entry is 16 bytes and the PLT0 entry is 32 bytes.
	     Encourage better cache usage by aligning.  We do this
	     lazily to avoid pessimizing traditional objects.  */
	  if (!bfd_set_section_alignment (dynobj, htab->splt, 5))
	    return FALSE;

	  /* Make sure that .got.plt is word-aligned.  We do this lazily
	     for the same reason as above.  */
	  if (!bfd_set_section_alignment (dynobj, htab->sgotplt,
					  MIPS_ELF_LOG_FILE_ALIGN (dynobj)))
	    return FALSE;

	  htab->splt->size += htab->plt_header_size;

	  /* The first two entries in .got.plt are reserved.  */
	  htab->sgotplt->size += 2 * MIPS_ELF_GOT_SIZE (dynobj);
	}

      /* Assign the next .plt entry to this symbol.  */
      h->plt.offset = htab->splt->size;
      htab->splt->size += htab->plt_entry_size;

      /* If the output file has no definition of the symbol, set the
	 symbol's value to the address of the stub.  */
      if (!info->shared && !h->def_regular)
	{
	  h->root.u.def.section = htab->splt;
	  h->root.u.def.value = h->plt.offset;
	}

      /* Make room for the .got.plt entry and the R_RISCV_JUMP_SLOT
	 relocation.  */
      htab->sgotplt->size += MIPS_ELF_GOT_SIZE (dynobj);
      htab->srelplt->size += MIPS_ELF_REL_SIZE (dynobj);

      /* All relocations against this symbol that could have been made
	 dynamic will now refer to the PLT entry instead.  */
      hmips->possibly_dynamic_relocs = 0;

      return TRUE;
    }

  /* If this is a weak symbol, and there is a real definition, the
     processor independent code will have arranged for us to see the
     real definition first, and we can just use the same value.  */
  if (h->u.weakdef != NULL)
    {
      BFD_ASSERT (h->u.weakdef->root.type == bfd_link_hash_defined
		  || h->u.weakdef->root.type == bfd_link_hash_defweak);
      h->root.u.def.section = h->u.weakdef->root.u.def.section;
      h->root.u.def.value = h->u.weakdef->root.u.def.value;
      return TRUE;
    }

  /* Otherwise, there is nothing further to do for symbols defined
     in regular objects.  */
  if (h->def_regular)
    return TRUE;

  /* There's also nothing more to do if we'll convert all relocations
     against this symbol into dynamic relocations.  */
  if (!hmips->has_static_relocs)
    return TRUE;

  /* We're now relying on copy relocations.  Complain if we have
     some that we can't convert.  */
  if (!htab->use_plts_and_copy_relocs || info->shared)
    {
      (*_bfd_error_handler) (_("non-dynamic relocations refer to "
			       "dynamic symbol %s"),
			     h->root.root.string);
      bfd_set_error (bfd_error_bad_value);
      return FALSE;
    }

  /* We must allocate the symbol in our .dynbss section, which will
     become part of the .bss section of the executable.  There will be
     an entry for this symbol in the .dynsym section.  The dynamic
     object will contain position independent code, so all references
     from the dynamic object to this symbol will go through the global
     offset table.  The dynamic linker will use the .dynsym entry to
     determine the address it must put in the global offset table, so
     both the dynamic object and the regular object will refer to the
     same memory location for the variable.  */

  if ((h->root.u.def.section->flags & SEC_ALLOC) != 0)
    {
      mips_elf_allocate_dynamic_relocations (dynobj, info, 1);
      h->needs_copy = 1;
    }

  /* All relocations against this symbol that could have been made
     dynamic will now refer to the local copy instead.  */
  hmips->possibly_dynamic_relocs = 0;

  return _bfd_elf_adjust_dynamic_copy (h, htab->sdynbss);
}

/* This function is called after all the input files have been read,
   and the input sections have been assigned to output sections.  We
   check for any mips16 stub sections that we can discard.  */

bfd_boolean
_bfd_riscv_elf_always_size_sections (bfd *output_bfd,
				    struct bfd_link_info *info)
{
  asection *ri;
  struct mips_elf_link_hash_table *htab;
  struct mips_htab_traverse_info hti;

  htab = mips_elf_hash_table (info);
  BFD_ASSERT (htab != NULL);

  /* The .reginfo section has a fixed size.  */
  ri = bfd_get_section_by_name (output_bfd, ".reginfo");
  if (ri != NULL)
    bfd_set_section_size (output_bfd, ri, sizeof (Elf32_External_RegInfo));

  hti.info = info;
  hti.output_bfd = output_bfd;
  hti.error = FALSE;
  mips_elf_link_hash_traverse (mips_elf_hash_table (info),
			       mips_elf_check_symbols, &hti);
  if (hti.error)
    return FALSE;

  return TRUE;
}

/* If the link uses a GOT, lay it out and work out its size.  */

static bfd_boolean
mips_elf_lay_out_got (bfd *output_bfd, struct bfd_link_info *info)
{
  bfd *dynobj;
  asection *s;
  struct mips_got_info *g;
  bfd_size_type loadable_size = 0;
  bfd_size_type page_gotno;
  bfd *sub;
  struct mips_elf_count_tls_arg count_tls_arg;
  struct mips_elf_link_hash_table *htab;
  struct mips_elf_count_tls_arg arg;

  htab = mips_elf_hash_table (info);
  BFD_ASSERT (htab != NULL);

  s = htab->sgot;
  if (s == NULL)
    return TRUE;

  dynobj = elf_hash_table (info)->dynobj;
  g = htab->got_info;

  /* Allocate room for the reserved entries. */
  BFD_ASSERT (g->assigned_gotno == 0);
  htab->reserved_gotno = 2;
  g->local_gotno += htab->reserved_gotno;
  g->assigned_gotno = htab->reserved_gotno;

  /* Replace entries for indirect and warning symbols with entries for
     the target symbol.  */
  if (!mips_elf_resolve_final_got_entries (g))
    return FALSE;

  /* Count the number of GOT symbols.  */
  mips_elf_link_hash_traverse (htab, mips_elf_count_got_symbols, info);

  /* Calculate the total loadable size of the output.  That
     will give us the maximum number of GOT_PAGE entries
     required.  */
  for (sub = info->input_bfds; sub; sub = sub->link_next)
    {
      asection *subsection;

      for (subsection = sub->sections;
	   subsection;
	   subsection = subsection->next)
	{
	  if ((subsection->flags & SEC_ALLOC) == 0)
	    continue;
	  loadable_size += ((subsection->size + 0xf)
			    &~ (bfd_size_type) 0xf);
	}
    }

  /* Assume there are two loadable segments consisting of contiguous
     sections.  Is 5 enough?  */
  page_gotno = (loadable_size >> 16) + 5;

  /* Choose the smaller of the two estimates; both are intended to be
     conservative.  */
  if (page_gotno > g->page_gotno)
    page_gotno = g->page_gotno;

  g->local_gotno += page_gotno;
  s->size += g->local_gotno * MIPS_ELF_GOT_SIZE (output_bfd);
  s->size += g->global_gotno * MIPS_ELF_GOT_SIZE (output_bfd);

  /* We need to calculate tls_gotno for global symbols at this point
     instead of building it up earlier, to avoid doublecounting
     entries for one global symbol from multiple input files.  */
  count_tls_arg.info = info;
  count_tls_arg.needed = 0;
  elf_link_hash_traverse (elf_hash_table (info),
			  mips_elf_count_global_tls_entries,
			  &count_tls_arg);
  g->tls_gotno += count_tls_arg.needed;
  s->size += g->tls_gotno * MIPS_ELF_GOT_SIZE (output_bfd);

  /* Set up TLS entries.  */
  g->tls_assigned_gotno = g->global_gotno + g->local_gotno;
  htab_traverse (g->got_entries, mips_elf_initialize_tls_index, g);

  /* Allocate room for the TLS relocations.  */
  arg.info = info;
  arg.needed = 0;
  htab_traverse (g->got_entries, mips_elf_count_local_tls_relocs, &arg);
  elf_link_hash_traverse (elf_hash_table (info),
			mips_elf_count_global_tls_relocs, &arg);
  if (arg.needed)
    mips_elf_allocate_dynamic_relocations (dynobj, info, arg.needed);

  return TRUE;
}

/* Estimate the size of the .MIPS.stubs section.  */

static void
mips_elf_estimate_stub_size (bfd *output_bfd, struct bfd_link_info *info)
{
  struct mips_elf_link_hash_table *htab;
  bfd_size_type dynsymcount;

  htab = mips_elf_hash_table (info);
  BFD_ASSERT (htab != NULL);

  if (htab->lazy_stub_count == 0)
    return;

  /* IRIX rld assumes that a function stub isn't at the end of the .text
     section, so add a dummy entry to the end.  */
  htab->lazy_stub_count++;

  /* Get a worst-case estimate of the number of dynamic symbols needed.
     At this point, dynsymcount does not account for section symbols
     and count_section_dynsyms may overestimate the number that will
     be needed.  */
  dynsymcount = (elf_hash_table (info)->dynsymcount
		 + count_section_dynsyms (output_bfd, info));

  /* Determine the size of one stub entry.  */
  htab->function_stub_size = (dynsymcount >= RISCV_IMM_REACH/2
			      ? MIPS_FUNCTION_STUB_BIG_SIZE
			      : MIPS_FUNCTION_STUB_NORMAL_SIZE);

  htab->sstubs->size = htab->lazy_stub_count * htab->function_stub_size;
}

/* A mips_elf_link_hash_traverse callback for which DATA points to the
   MIPS hash table.  If H needs a traditional MIPS lazy-binding stub,
   allocate an entry in the stubs section.  */

static bfd_boolean
mips_elf_allocate_lazy_stub (struct mips_elf_link_hash_entry *h, void **data)
{
  struct mips_elf_link_hash_table *htab;

  htab = (struct mips_elf_link_hash_table *) data;
  if (h->needs_lazy_stub)
    {
      h->root.root.u.def.section = htab->sstubs;
      h->root.root.u.def.value = htab->sstubs->size;
      h->root.plt.offset = htab->sstubs->size;
      htab->sstubs->size += htab->function_stub_size;
    }
  return TRUE;
}

/* Allocate offsets in the stubs section to each symbol that needs one.
   Set the final size of the .MIPS.stub section.  */

static void
mips_elf_lay_out_lazy_stubs (struct bfd_link_info *info)
{
  struct mips_elf_link_hash_table *htab;

  htab = mips_elf_hash_table (info);
  BFD_ASSERT (htab != NULL);

  if (htab->lazy_stub_count == 0)
    return;

  htab->sstubs->size = 0;
  mips_elf_link_hash_traverse (htab, mips_elf_allocate_lazy_stub, htab);
  htab->sstubs->size += htab->function_stub_size;
  BFD_ASSERT (htab->sstubs->size
	      == htab->lazy_stub_count * htab->function_stub_size);
}

/* Set the sizes of the dynamic sections.  */

bfd_boolean
_bfd_riscv_elf_size_dynamic_sections (bfd *output_bfd,
				     struct bfd_link_info *info)
{
  bfd *dynobj;
  asection *s, *sreldyn;
  struct mips_elf_link_hash_table *htab;

  htab = mips_elf_hash_table (info);
  BFD_ASSERT (htab != NULL);
  dynobj = elf_hash_table (info)->dynobj;
  BFD_ASSERT (dynobj != NULL);

  if (elf_hash_table (info)->dynamic_sections_created)
    {
      /* Set the contents of the .interp section to the interpreter.  */
      if (info->executable)
	{
	  s = bfd_get_section_by_name (dynobj, ".interp");
	  BFD_ASSERT (s != NULL);
	  s->size
	    = strlen (ELF_DYNAMIC_INTERPRETER (output_bfd)) + 1;
	  s->contents
	    = (bfd_byte *) ELF_DYNAMIC_INTERPRETER (output_bfd);
	}

      /* Create a symbol for the PLT, if we know that we are using it.  */
      if (htab->splt && htab->splt->size > 0 && htab->root.hplt == NULL)
	{
	  struct elf_link_hash_entry *h;

	  BFD_ASSERT (htab->use_plts_and_copy_relocs);

	  h = _bfd_elf_define_linkage_sym (dynobj, info, htab->splt,
					   "_PROCEDURE_LINKAGE_TABLE_");
	  htab->root.hplt = h;
	  if (h == NULL)
	    return FALSE;
	  h->type = STT_FUNC;
	}
    }

  /* Allocate space for global sym dynamic relocs.  */
  elf_link_hash_traverse (&htab->root, allocate_dynrelocs, (PTR) info);

  mips_elf_estimate_stub_size (output_bfd, info);

  if (!mips_elf_lay_out_got (output_bfd, info))
    return FALSE;

  mips_elf_lay_out_lazy_stubs (info);

  /* The check_relocs and adjust_dynamic_symbol entry points have
     determined the sizes of the various dynamic sections.  Allocate
     memory for them.  */
  for (s = dynobj->sections; s != NULL; s = s->next)
    {
      const char *name;

      /* It's OK to base decisions on the section name, because none
	 of the dynobj section names depend upon the input files.  */
      name = bfd_get_section_name (dynobj, s);

      if ((s->flags & SEC_LINKER_CREATED) == 0)
	continue;

      if (CONST_STRNEQ (name, ".rel"))
	{
	  if (s->size != 0)
	    {
	      /* We use the reloc_count field as a counter if we need
		 to copy relocs into the output file.  */
	      if (strcmp (name, MIPS_ELF_REL_DYN_NAME (info)) != 0)
		s->reloc_count = 0;

	      /* If combreloc is enabled, elf_link_sort_relocs() will
		 sort relocations, but in a different way than we do,
		 and before we're done creating relocations.  Also, it
		 will move them around between input sections'
		 relocation's contents, so our sorting would be
		 broken, so don't let it run.  */
	      info->combreloc = 0;
	    }
	}
      else if (! info->shared
	       && ! mips_elf_hash_table (info)->use_rld_obj_head
	       && CONST_STRNEQ (name, ".rld_map"))
	{
	  /* We add a room for __rld_map.  It will be filled in by the
	     rtld to contain a pointer to the _r_debug structure.  */
	  s->size += 4;
	}
      else if (s == htab->splt)
	{
	}
      else if (! CONST_STRNEQ (name, ".init")
	       && s != htab->sgot
	       && s != htab->sgotplt
	       && s != htab->sstubs
	       && s != htab->sdynbss)
	{
	  /* It's not one of our sections, so don't allocate space.  */
	  continue;
	}

      if (s->size == 0)
	{
	  s->flags |= SEC_EXCLUDE;
	  continue;
	}

      if ((s->flags & SEC_HAS_CONTENTS) == 0)
	continue;

      /* Allocate memory for the section contents.  */
      s->contents = bfd_zalloc (dynobj, s->size);
      if (s->contents == NULL)
	{
	  bfd_set_error (bfd_error_no_memory);
	  return FALSE;
	}
    }

  if (elf_hash_table (info)->dynamic_sections_created)
    {
      /* Add some entries to the .dynamic section.  We fill in the
	 values later, in _bfd_riscv_elf_finish_dynamic_sections, but we
	 must add the entries now so that we get the correct size for
	 the .dynamic section.  */

      /* SGI object has the equivalence of DT_DEBUG in the
	 DT_MIPS_RLD_MAP entry.  This must come first because glibc
	 only fills in DT_MIPS_RLD_MAP (not DT_DEBUG) and GDB only
	 looks at the first one it sees.  */
      if (!info->shared
	  && !MIPS_ELF_ADD_DYNAMIC_ENTRY (info, DT_MIPS_RLD_MAP, 0))
	return FALSE;

      /* The DT_DEBUG entry may be filled in by the dynamic linker and
	 used by the debugger.  */
      if (info->executable
	  && !MIPS_ELF_ADD_DYNAMIC_ENTRY (info, DT_DEBUG, 0))
	return FALSE;

      if ((info->flags & DF_TEXTREL) != 0)
	{
	  if (! MIPS_ELF_ADD_DYNAMIC_ENTRY (info, DT_TEXTREL, 0))
	    return FALSE;

	  /* Clear the DF_TEXTREL flag.  It will be set again if we
	     write out an actual text relocation; we may not, because
	     at this point we do not know whether e.g. any .eh_frame
	     absolute relocations have been converted to PC-relative.  */
	  info->flags &= ~DF_TEXTREL;
	}

      if (! MIPS_ELF_ADD_DYNAMIC_ENTRY (info, DT_PLTGOT, 0))
	return FALSE;

      sreldyn = mips_elf_rel_dyn_section (info, FALSE);
	{
	  if (sreldyn && sreldyn->size > 0)
	    {
	      if (! MIPS_ELF_ADD_DYNAMIC_ENTRY (info, DT_REL, 0))
		return FALSE;

	      if (! MIPS_ELF_ADD_DYNAMIC_ENTRY (info, DT_RELSZ, 0))
		return FALSE;

	      if (! MIPS_ELF_ADD_DYNAMIC_ENTRY (info, DT_RELENT, 0))
		return FALSE;
	    }

	  if (! MIPS_ELF_ADD_DYNAMIC_ENTRY (info, DT_MIPS_RLD_VERSION, 0))
	    return FALSE;

	  if (! MIPS_ELF_ADD_DYNAMIC_ENTRY (info, DT_MIPS_FLAGS, 0))
	    return FALSE;

	  if (! MIPS_ELF_ADD_DYNAMIC_ENTRY (info, DT_MIPS_BASE_ADDRESS, 0))
	    return FALSE;

	  if (! MIPS_ELF_ADD_DYNAMIC_ENTRY (info, DT_MIPS_LOCAL_GOTNO, 0))
	    return FALSE;

	  if (! MIPS_ELF_ADD_DYNAMIC_ENTRY (info, DT_MIPS_SYMTABNO, 0))
	    return FALSE;

	  if (! MIPS_ELF_ADD_DYNAMIC_ENTRY (info, DT_MIPS_UNREFEXTNO, 0))
	    return FALSE;

	  if (! MIPS_ELF_ADD_DYNAMIC_ENTRY (info, DT_MIPS_GOTSYM, 0))
	    return FALSE;
	}
      if (htab->splt->size > 0)
	{
	  if (! MIPS_ELF_ADD_DYNAMIC_ENTRY (info, DT_PLTREL, 0))
	    return FALSE;

	  if (! MIPS_ELF_ADD_DYNAMIC_ENTRY (info, DT_JMPREL, 0))
	    return FALSE;

	  if (! MIPS_ELF_ADD_DYNAMIC_ENTRY (info, DT_PLTRELSZ, 0))
	    return FALSE;

	  if (! MIPS_ELF_ADD_DYNAMIC_ENTRY (info, DT_MIPS_PLTGOT, 0))
	    return FALSE;
	}
    }

  return TRUE;
}

/* REL is a relocation in INPUT_BFD that is being copied to OUTPUT_BFD.
   Adjust its R_ADDEND field so that it is correct for the output file.
   LOCAL_SYMS and LOCAL_SECTIONS are arrays of INPUT_BFD's local symbols
   and sections respectively; both use symbol indexes.  */

static void
mips_elf_adjust_addend (bfd *output_bfd, struct bfd_link_info *info,
			bfd *input_bfd, Elf_Internal_Sym *local_syms,
			asection **local_sections, Elf_Internal_Rela *rel)
{
  unsigned int r_type, r_symndx;
  Elf_Internal_Sym *sym;
  asection *sec;

  if (mips_elf_local_relocation_p (input_bfd, rel, local_sections))
    {
      r_type = ELF_R_TYPE (output_bfd, rel->r_info);
      if (r_type == R_MIPS16_GPREL
	  || r_type == R_RISCV_GPREL16
	  || r_type == R_RISCV_GPREL32
	  || r_type == R_RISCV_LITERAL)
	{
	  rel->r_addend += _bfd_get_gp_value (input_bfd);
	  rel->r_addend -= _bfd_get_gp_value (output_bfd);
	}

      r_symndx = ELF_R_SYM (output_bfd, rel->r_info);
      sym = local_syms + r_symndx;

      /* Adjust REL's addend to account for section merging.  */
      if (!info->relocatable)
	{
	  sec = local_sections[r_symndx];
	  _bfd_elf_rela_local_sym (output_bfd, sym, &sec, rel);
	}

      /* This would normally be done by the rela_normal code in elflink.c.  */
      if (ELF_ST_TYPE (sym->st_info) == STT_SECTION)
	rel->r_addend += local_sections[r_symndx]->output_offset;
    }
}

/* Relocate a MIPS ELF section.  */

bfd_boolean
_bfd_riscv_elf_relocate_section (bfd *output_bfd, struct bfd_link_info *info,
				bfd *input_bfd, asection *input_section,
				bfd_byte *contents, Elf_Internal_Rela *relocs,
				Elf_Internal_Sym *local_syms,
				asection **local_sections)
{
  Elf_Internal_Rela *rel;
  const Elf_Internal_Rela *relend;
  bfd_vma addend = 0;
  bfd_boolean use_saved_addend_p = FALSE;
  const struct elf_backend_data *bed;

  bed = get_elf_backend_data (output_bfd);
  relend = relocs + input_section->reloc_count * bed->s->int_rels_per_ext_rel;
  for (rel = relocs; rel < relend; ++rel)
    {
      const char *name;
      bfd_vma value = 0;
      reloc_howto_type *howto;
      /* TRUE if the relocation is a RELA relocation, rather than a
         REL relocation.  */
      bfd_boolean rela_relocation_p = TRUE;
      unsigned int r_type = ELF_R_TYPE (output_bfd, rel->r_info);
      const char *msg;
      unsigned long r_symndx;
      asection *sec;
      Elf_Internal_Shdr *symtab_hdr;
      struct elf_link_hash_entry *h;
      bfd_boolean rel_reloc;

      rel_reloc = mips_elf_rel_relocation_p (input_bfd, input_section,
					     relocs, rel);
      /* Find the relocation howto for this relocation.  */
      howto = MIPS_ELF_RTYPE_TO_HOWTO (input_bfd, r_type, !rel_reloc);

      r_symndx = ELF_R_SYM (input_bfd, rel->r_info);
      symtab_hdr = &elf_tdata (input_bfd)->symtab_hdr;
      if (mips_elf_local_relocation_p (input_bfd, rel, local_sections))
	{
	  sec = local_sections[r_symndx];
	  h = NULL;
	}
      else
	{
	  unsigned long extsymoff;

	  extsymoff = 0;
	  if (!elf_bad_symtab (input_bfd))
	    extsymoff = symtab_hdr->sh_info;
	  h = elf_sym_hashes (input_bfd) [r_symndx - extsymoff];
	  while (h->root.type == bfd_link_hash_indirect
		 || h->root.type == bfd_link_hash_warning)
	    h = (struct elf_link_hash_entry *) h->root.u.i.link;

	  sec = NULL;
	  if (h->root.type == bfd_link_hash_defined
	      || h->root.type == bfd_link_hash_defweak)
	    sec = h->root.u.def.section;
	}

      if (sec != NULL && elf_discarded_section (sec))
	RELOC_AGAINST_DISCARDED_SECTION (info, input_bfd, input_section,
					 rel, relend, howto, contents);

      if (!use_saved_addend_p)
	{
	  /* If these relocations were originally of the REL variety,
	     we must pull the addend out of the field that will be
	     relocated.  Otherwise, we simply use the contents of the
	     RELA relocation.  */
	  if (mips_elf_rel_relocation_p (input_bfd, input_section,
					 relocs, rel))
	    {
	      rela_relocation_p = FALSE;
	      addend = mips_elf_read_rel_addend (input_bfd, rel,
						 howto, contents);
	      if (hi16_reloc_p (r_type)
		  || (got16_reloc_p (r_type)
		      && mips_elf_local_relocation_p (input_bfd, rel,
						      local_sections)))
		{
		  if (!mips_elf_add_lo16_rel_addend (input_bfd, rel, relend,
						     contents, &addend))
		    {
		      if (h)
			name = h->root.root.string;
		      else
			name = bfd_elf_sym_name (input_bfd, symtab_hdr,
						 local_syms + r_symndx,
						 sec);
		      (*_bfd_error_handler)
			(_("%B: Can't find matching LO16 reloc against `%s' for %s at 0x%lx in section `%A'"),
			 input_bfd, input_section, name, howto->name,
			 rel->r_offset);
		    }
		}
	      else
		addend <<= howto->rightshift;
	    }
	  else
	    addend = rel->r_addend;
	  mips_elf_adjust_addend (output_bfd, info, input_bfd,
				  local_syms, local_sections, rel);
	}

      if (info->relocatable)
	{
	  if (!rela_relocation_p && rel->r_addend)
	    {
	      addend += rel->r_addend;
	      if (hi16_reloc_p (r_type) || got16_reloc_p (r_type))
		addend = mips_elf_high (addend);
	      else
		addend >>= howto->rightshift;

	      /* We use the source mask, rather than the destination
		 mask because the place to which we are writing will be
		 source of the addend in the final link.  */
	      addend &= howto->src_mask;

	      if (! mips_elf_perform_relocation (info, howto, rel, addend,
						 input_bfd, input_section,
						 contents))
		return FALSE;
	    }

	  /* Go on to the next relocation.  */
	  continue;
	}

      /* In the N32 and 64-bit ABIs there may be multiple consecutive
	 relocations for the same offset.  In that case we are
	 supposed to treat the output of each relocation as the addend
	 for the next.  */
      if (rel + 1 < relend
	  && rel->r_offset == rel[1].r_offset
	  && ELF_R_TYPE (input_bfd, rel[1].r_info) != R_RISCV_NONE)
	use_saved_addend_p = TRUE;
      else
	use_saved_addend_p = FALSE;

      /* Figure out what value we are supposed to relocate.  */
      switch (mips_elf_calculate_relocation (output_bfd, input_bfd,
					     input_section, info, rel,
					     addend, howto, local_syms,
					     local_sections, &value,
					     &name, use_saved_addend_p))
	{
	case bfd_reloc_continue:
	  /* There's nothing to do.  */
	  continue;

	case bfd_reloc_undefined:
	  /* mips_elf_calculate_relocation already called the
	     undefined_symbol callback.  There's no real point in
	     trying to perform the relocation at this point, so we
	     just skip ahead to the next relocation.  */
	  continue;

	case bfd_reloc_notsupported:
	  msg = _("internal error: unsupported relocation error");
	  info->callbacks->warning
	    (info, msg, name, input_bfd, input_section, rel->r_offset);
	  return FALSE;

	case bfd_reloc_overflow:
	  if (use_saved_addend_p)
	    /* Ignore overflow until we reach the last relocation for
	       a given location.  */
	    ;
	  else
	    {
	      struct mips_elf_link_hash_table *htab;

	      htab = mips_elf_hash_table (info);
	      BFD_ASSERT (htab != NULL);
	      BFD_ASSERT (name != NULL);
	      if (! ((*info->callbacks->reloc_overflow)
		     (info, NULL, name, howto->name, (bfd_vma) 0,
		      input_bfd, input_section, rel->r_offset)))
		return FALSE;
	    }
	  break;

	case bfd_reloc_ok:
	  break;

	default:
	  abort ();
	  break;
	}

      /* If we've got another relocation for the address, keep going
	 until we reach the last one.  */
      if (use_saved_addend_p)
	{
	  addend = value;
	  continue;
	}

      /* Actually perform the relocation.  */
      if (! mips_elf_perform_relocation (info, howto, rel, value,
					 input_bfd, input_section,
					 contents))
	return FALSE;
    }

  return TRUE;
}

/* A function that iterates over each entry in la25_stubs and fills
   in the code for each one.  DATA points to a mips_htab_traverse_info.  */

static int
mips_elf_create_la25_stub (void **slot, void *data)
{
  struct mips_htab_traverse_info *hti;
  struct mips_elf_link_hash_table *htab;
  struct mips_elf_la25_stub *stub;
  asection *s;
  bfd_byte *loc;
  bfd_vma offset, target;

  stub = (struct mips_elf_la25_stub *) *slot;
  hti = (struct mips_htab_traverse_info *) data;
  htab = mips_elf_hash_table (hti->info);
  BFD_ASSERT (htab != NULL);

  /* Create the section contents, if we haven't already.  */
  s = stub->stub_section;
  loc = s->contents;
  if (loc == NULL)
    {
      loc = bfd_malloc (s->size);
      if (loc == NULL)
	{
	  hti->error = TRUE;
	  return FALSE;
	}
      s->contents = loc;
    }

  /* Work out where in the section this stub should go.  */
  offset = stub->offset;

  /* Work out the target address.  */
  target = (stub->h->root.root.u.def.section->output_section->vma
	    + stub->h->root.root.u.def.section->output_offset
	    + stub->h->root.root.u.def.value);

  if (stub->stub_section != htab->strampoline)
    {
      /* This is a simple RDNPC stub.  Zero out the beginning
	 of the section and write the instruction at the end. */
      memset (loc, 0, offset);
      loc += offset;
      bfd_put_32 (hti->output_bfd, RISCV_ITYPE (RDNPC, 19, 0, 0), loc);
    }
  else
    {
      /* This is trampoline.  */
      loc += offset;
      bfd_put_32 (hti->output_bfd, RISCV_LTYPE (LUI, 18, RISCV_LUI_HIGH_PART(target)), loc);
      bfd_put_32 (hti->output_bfd, RISCV_ITYPE (ADDI, 19, 18, RISCV_CONST_LOW_PART(target)), loc + 4);
      bfd_put_32 (hti->output_bfd, RISCV_ITYPE (JALR_J, 0, 18, RISCV_CONST_LOW_PART(target)), loc + 8);
      bfd_put_32 (hti->output_bfd, 0, loc + 12);
    }
  return TRUE;
}

/* Finish up dynamic symbol handling.  We set the contents of various
   dynamic sections here.  */

bfd_boolean
_bfd_riscv_elf_finish_dynamic_symbol (bfd *output_bfd,
				     struct bfd_link_info *info,
				     struct elf_link_hash_entry *h,
				     Elf_Internal_Sym *sym)
{
  bfd *dynobj;
  asection *sgot;
  struct mips_got_info *g;
  const char *name;
  int idx;
  struct mips_elf_link_hash_table *htab;
  struct mips_elf_link_hash_entry *hmips;

  htab = mips_elf_hash_table (info);
  BFD_ASSERT (htab != NULL);
  dynobj = elf_hash_table (info)->dynobj;
  hmips = (struct mips_elf_link_hash_entry *) h;

  if (h->plt.offset != MINUS_ONE && hmips->no_fn_stub)
    {
      /* We've decided to create a PLT entry for this symbol.  */
      bfd_byte *loc;
      bfd_vma header_address, plt_index, got_address;
      bfd_vma plt_entry[RISCV_PLT_ENTRY_INSNS];
      int i;

      BFD_ASSERT (htab->use_plts_and_copy_relocs);
      BFD_ASSERT (h->dynindx != -1);
      BFD_ASSERT (htab->splt != NULL);
      BFD_ASSERT (h->plt.offset <= htab->splt->size);
      BFD_ASSERT (!h->def_regular);

      /* Calculate the address of the PLT header.  */
      header_address = (htab->splt->output_section->vma
			+ htab->splt->output_offset);

      /* Calculate the index of the entry.  */
      plt_index = ((h->plt.offset - htab->plt_header_size)
		   / htab->plt_entry_size);

      /* Calculate the address of the .got.plt entry.  */
      got_address = (htab->sgotplt->output_section->vma
		     + htab->sgotplt->output_offset
		     + (2 + plt_index) * MIPS_ELF_GOT_SIZE (dynobj));

      /* Initially point the .got.plt entry at the PLT header.  */
      loc = (htab->sgotplt->contents
	     + (2 + plt_index) * MIPS_ELF_GOT_SIZE (dynobj));
      if (ABI_64_P (output_bfd))
	bfd_put_64 (output_bfd, header_address, loc);
      else
	bfd_put_32 (output_bfd, header_address, loc);

      /* Find out where the .plt entry should go.  */
      loc = htab->splt->contents + h->plt.offset;

      /* Fill in the PLT entry itself.  */
      riscv_make_plt_entry (output_bfd, got_address, plt_entry);
      for (i = 0; i < RISCV_PLT_ENTRY_INSNS; i++)
        bfd_put_32 (output_bfd, plt_entry[i], loc + 4*i);

      /* Emit an R_RISCV_JUMP_SLOT relocation against the .got.plt entry.  */
      mips_elf_output_dynamic_relocation (output_bfd, htab->srelplt,
					  plt_index, h->dynindx,
					  R_RISCV_JUMP_SLOT, got_address);

      /* We distinguish between PLT entries and lazy-binding stubs by
	 giving the former an st_other value of STO_MIPS_PLT.  Set the
	 flag and leave the value if there are any relocations in the
	 binary where pointer equality matters.  */
      sym->st_shndx = SHN_UNDEF;
      if (h->pointer_equality_needed)
	sym->st_other = STO_MIPS_PLT;
      else
	sym->st_value = 0;
    }
  else if (h->plt.offset != MINUS_ONE)
    {
      /* We've decided to create a lazy-binding stub.  */
      bfd_byte stub[MIPS_FUNCTION_STUB_BIG_SIZE];

      /* This symbol has a stub.  Set it up.  */

      BFD_ASSERT (h->dynindx != -1);

      BFD_ASSERT ((htab->function_stub_size == MIPS_FUNCTION_STUB_BIG_SIZE)
                  || (h->dynindx < RISCV_IMM_REACH/2));

      /* Values up to 2^31 - 1 are allowed.  Larger values would cause
	 sign extension at runtime in the stub, resulting in a negative
	 index value.  */
      if (h->dynindx & ~0x7fffffff)
	return FALSE;

      /* Fill the stub.  */
      /* l[w|d] t7, -GP_OFFSET(gp)
         move   t5, ra
         lui    t6, %hi(idx)
         addi   t6, t6, %lo(idx)
         jalr   t7
       */
      idx = 0;
      bfd_put_32 (output_bfd, RISCV_ITYPE (LREG(output_bfd), 19, 28, 0), stub + idx);
      idx += 4;
      bfd_put_32 (output_bfd, RISCV_ITYPE (ADDI, 17, LINK_REG, 0), stub + idx);
      idx += 4;
      if (htab->function_stub_size == MIPS_FUNCTION_STUB_BIG_SIZE)
        {
          bfd_put_32 (output_bfd, RISCV_LTYPE (LUI, 18, RISCV_LUI_HIGH_PART(h->dynindx)),
                      stub + idx);
          idx += 4;

	  bfd_put_32 (output_bfd, RISCV_ITYPE (ADDI, 18, 18, RISCV_CONST_LOW_PART(h->dynindx)), stub + idx);
          idx += 4;
        }
      else
        {
	  bfd_put_32 (output_bfd, RISCV_ITYPE (ADDI, 18, 0, RISCV_CONST_LOW_PART(h->dynindx)), stub + idx);
          idx += 4;
        }
      bfd_put_32 (output_bfd, RISCV_ITYPE (JALR_J, LINK_REG, 19, 0), stub + idx);
      idx += 4;

      BFD_ASSERT (h->plt.offset <= htab->sstubs->size);
      memcpy (htab->sstubs->contents + h->plt.offset,
	      stub, htab->function_stub_size);

      /* Mark the symbol as undefined.  plt.offset != -1 occurs
	 only for the referenced symbol.  */
      sym->st_shndx = SHN_UNDEF;

      /* The run-time linker uses the st_value field of the symbol
	 to reset the global offset table entry for this external
	 to its stub address when unlinking a shared object.  */
      sym->st_value = (htab->sstubs->output_section->vma
		       + htab->sstubs->output_offset
		       + h->plt.offset);
    }

  BFD_ASSERT (h->dynindx != -1
	      || h->forced_local);

  sgot = htab->sgot;
  g = htab->got_info;
  BFD_ASSERT (g != NULL);

  /* Run through the global symbol table, creating GOT entries for all
     the symbols that need them.  */
  if (hmips->global_got_area != GGA_NONE)
    {
      bfd_vma offset;
      bfd_vma value;

      value = sym->st_value;
      offset = mips_elf_global_got_index (dynobj, h, R_RISCV_GOT16, info);
      MIPS_ELF_PUT_WORD (output_bfd, value, sgot->contents + offset);
    }

  /* Mark _DYNAMIC and _GLOBAL_OFFSET_TABLE_ as absolute.  */
  name = h->root.root.string;
  if (strcmp (name, "_DYNAMIC") == 0
      || h == elf_hash_table (info)->hgot)
    sym->st_shndx = SHN_ABS;
  else if (strcmp (name, "_DYNAMIC_LINKING") == 0)
    {
      sym->st_shndx = SHN_ABS;
      sym->st_info = ELF_ST_INFO (STB_GLOBAL, STT_SECTION);
      sym->st_value = 1;
    }

  /* Emit a copy reloc, if needed.  */
  if (h->needs_copy)
    {
      asection *s;
      bfd_vma symval;

      BFD_ASSERT (h->dynindx != -1);
      BFD_ASSERT (htab->use_plts_and_copy_relocs);

      s = mips_elf_rel_dyn_section (info, FALSE);
      symval = (h->root.u.def.section->output_section->vma
		+ h->root.u.def.section->output_offset
		+ h->root.u.def.value);
      mips_elf_output_dynamic_relocation (output_bfd, s, s->reloc_count++,
					  h->dynindx, R_RISCV_COPY, symval);
    }

  if (! info->shared)
    {
      if (! mips_elf_hash_table (info)->use_rld_obj_head
	  && (strcmp (name, "__rld_map") == 0
	      || strcmp (name, "__RLD_MAP") == 0))
	{
	  asection *s = bfd_get_section_by_name (dynobj, ".rld_map");
	  BFD_ASSERT (s != NULL);
	  sym->st_value = s->output_section->vma + s->output_offset;
	  bfd_put_32 (output_bfd, 0, s->contents);
	  if (mips_elf_hash_table (info)->rld_value == 0)
	    mips_elf_hash_table (info)->rld_value = sym->st_value;
	}
      else if (mips_elf_hash_table (info)->use_rld_obj_head
	       && strcmp (name, "__rld_obj_head") == 0)
	{
	  BFD_ASSERT (bfd_get_section_by_name (dynobj, ".rld_map") != NULL);
	  mips_elf_hash_table (info)->rld_value = sym->st_value;
	}
    }

  /* Keep dynamic MIPS16 symbols odd.  This allows the dynamic linker to
     treat MIPS16 symbols like any other.  */
  if (ELF_ST_IS_MIPS16 (sym->st_other))
    {
      BFD_ASSERT (sym->st_value & 1);
      sym->st_other -= STO_MIPS16;
    }

  return TRUE;
}

/* Write out a plt0 entry to the beginning of .plt.  */

static void
mips_finish_exec_plt (bfd *output_bfd, struct bfd_link_info *info)
{
  bfd_byte *loc;
  bfd_vma gotplt_value;
  bfd_vma plt_entry[RISCV_PLT0_ENTRY_INSNS];
  struct mips_elf_link_hash_table *htab;
  int i;

  htab = mips_elf_hash_table (info);
  BFD_ASSERT (htab != NULL);

  /* Calculate the value of .got.plt.  */
  gotplt_value = (htab->sgotplt->output_section->vma
		  + htab->sgotplt->output_offset);

  /* Install the PLT header.  */
  loc = htab->splt->contents;
  riscv_make_plt0_entry (output_bfd, gotplt_value, plt_entry);
  for (i = 0; i < RISCV_PLT0_ENTRY_INSNS; i++)
    bfd_put_32 (output_bfd, plt_entry[i], loc + 4*i);
}

/* Finish up the dynamic sections.  */

bfd_boolean
_bfd_riscv_elf_finish_dynamic_sections (bfd *output_bfd,
				       struct bfd_link_info *info)
{
  bfd *dynobj;
  asection *sdyn;
  asection *sgot;
  struct mips_got_info *gg, *g;
  struct mips_elf_link_hash_table *htab;

  htab = mips_elf_hash_table (info);
  BFD_ASSERT (htab != NULL);

  dynobj = elf_hash_table (info)->dynobj;

  sdyn = bfd_get_section_by_name (dynobj, ".dynamic");

  sgot = htab->sgot;
  g = gg = htab->got_info;

  if (elf_hash_table (info)->dynamic_sections_created)
    {
      bfd_byte *b;
      int dyn_to_skip = 0, dyn_skipped = 0;

      BFD_ASSERT (sdyn != NULL);
      BFD_ASSERT (gg != NULL);

      for (b = sdyn->contents;
	   b < sdyn->contents + sdyn->size;
	   b += MIPS_ELF_DYN_SIZE (dynobj))
	{
	  Elf_Internal_Dyn dyn;
	  const char *name;
	  size_t elemsize;
	  asection *s;
	  bfd_boolean swap_out_p;

	  /* Read in the current dynamic entry.  */
	  (*get_elf_backend_data (dynobj)->s->swap_dyn_in) (dynobj, b, &dyn);

	  /* Assume that we're going to modify it and write it out.  */
	  swap_out_p = TRUE;

	  switch (dyn.d_tag)
	    {
	    case DT_RELENT:
	      dyn.d_un.d_val = MIPS_ELF_REL_SIZE (dynobj);
	      break;

	    case DT_STRSZ:
	      /* Rewrite DT_STRSZ.  */
	      dyn.d_un.d_val =
		_bfd_elf_strtab_size (elf_hash_table (info)->dynstr);
	      break;

	    case DT_PLTGOT:
	      s = htab->sgot;
	      dyn.d_un.d_ptr = s->output_section->vma + s->output_offset;
	      break;

	    case DT_MIPS_PLTGOT:
	      s = htab->sgotplt;
	      dyn.d_un.d_ptr = s->output_section->vma + s->output_offset;
	      break;

	    case DT_MIPS_RLD_VERSION:
	      dyn.d_un.d_val = 1; /* XXX */
	      break;

	    case DT_MIPS_FLAGS:
	      dyn.d_un.d_val = RHF_NOTPOT; /* XXX */
	      break;

	    case DT_MIPS_TIME_STAMP:
	      {
		time_t t;
		time (&t);
		dyn.d_un.d_val = t;
	      }
	      break;

	    case DT_MIPS_ICHECKSUM:
	      /* XXX FIXME: */
	      swap_out_p = FALSE;
	      break;

	    case DT_MIPS_IVERSION:
	      /* XXX FIXME: */
	      swap_out_p = FALSE;
	      break;

	    case DT_MIPS_BASE_ADDRESS:
	      s = output_bfd->sections;
	      BFD_ASSERT (s != NULL);
	      dyn.d_un.d_ptr = s->vma & ~(bfd_vma) (RISCV_IMM_REACH-1);
	      break;

	    case DT_MIPS_LOCAL_GOTNO:
	      dyn.d_un.d_val = g->local_gotno;
	      break;

	    case DT_MIPS_UNREFEXTNO:
	      /* The index into the dynamic symbol table which is the
		 entry of the first external symbol that is not
		 referenced within the same object.  */
	      dyn.d_un.d_val = bfd_count_sections (output_bfd) + 1;
	      break;

	    case DT_MIPS_GOTSYM:
	      if (gg->global_gotsym)
		{
		  dyn.d_un.d_val = gg->global_gotsym->dynindx;
		  break;
		}
	      /* In case if we don't have global got symbols we default
		 to setting DT_MIPS_GOTSYM to the same value as
		 DT_MIPS_SYMTABNO, so we just fall through.  */

	    case DT_MIPS_SYMTABNO:
	      name = ".dynsym";
	      elemsize = MIPS_ELF_SYM_SIZE (output_bfd);
	      s = bfd_get_section_by_name (output_bfd, name);
	      BFD_ASSERT (s != NULL);

	      dyn.d_un.d_val = s->size / elemsize;
	      break;

	    case DT_MIPS_HIPAGENO:
	      dyn.d_un.d_val = g->local_gotno - htab->reserved_gotno;
	      break;

	    case DT_MIPS_RLD_MAP:
	      dyn.d_un.d_ptr = mips_elf_hash_table (info)->rld_value;
	      break;

	    case DT_MIPS_OPTIONS:
	      s = (bfd_get_section_by_name
		   (output_bfd, MIPS_ELF_OPTIONS_SECTION_NAME (output_bfd)));
	      dyn.d_un.d_ptr = s->vma;
	      break;

	    case DT_PLTREL:
	      BFD_ASSERT (htab->use_plts_and_copy_relocs);
	      dyn.d_un.d_val = DT_REL;
	      break;

	    case DT_PLTRELSZ:
	      BFD_ASSERT (htab->use_plts_and_copy_relocs);
	      dyn.d_un.d_val = htab->srelplt->size;
	      break;

	    case DT_JMPREL:
	      BFD_ASSERT (htab->use_plts_and_copy_relocs);
	      dyn.d_un.d_ptr = (htab->srelplt->output_section->vma
				+ htab->srelplt->output_offset);
	      break;

	    case DT_TEXTREL:
	      /* If we didn't need any text relocations after all, delete
		 the dynamic tag.  */
	      if (!(info->flags & DF_TEXTREL))
		{
		  dyn_to_skip = MIPS_ELF_DYN_SIZE (dynobj);
		  swap_out_p = FALSE;
		}
	      break;

	    case DT_FLAGS:
	      /* If we didn't need any text relocations after all, clear
		 DF_TEXTREL from DT_FLAGS.  */
	      if (!(info->flags & DF_TEXTREL))
		dyn.d_un.d_val &= ~DF_TEXTREL;
	      else
		swap_out_p = FALSE;
	      break;

	    default:
	      swap_out_p = FALSE;
	      break;
	    }

	  if (swap_out_p || dyn_skipped)
	    (*get_elf_backend_data (dynobj)->s->swap_dyn_out)
	      (dynobj, &dyn, b - dyn_skipped);

	  if (dyn_to_skip)
	    {
	      dyn_skipped += dyn_to_skip;
	      dyn_to_skip = 0;
	    }
	}

      /* Wipe out any trailing entries if we shifted down a dynamic tag.  */
      if (dyn_skipped > 0)
	memset (b - dyn_skipped, 0, dyn_skipped);
    }

  if (sgot != NULL && sgot->size > 0
      && !bfd_is_abs_section (sgot->output_section))
    {
      /* The first entry of the global offset table will be filled at
         runtime. The second entry will be used by some runtime loaders.
         This isn't the case of IRIX rld.  */
      MIPS_ELF_PUT_WORD (output_bfd, (bfd_vma) 0, sgot->contents);
      MIPS_ELF_PUT_WORD (output_bfd, MIPS_ELF_GNU_GOT1_MASK (output_bfd),
	                 sgot->contents + MIPS_ELF_GOT_SIZE (output_bfd));

      elf_section_data (sgot->output_section)->this_hdr.sh_entsize
	 = MIPS_ELF_GOT_SIZE (output_bfd);
    }

  /* The generation of dynamic relocations for the non-primary gots
     adds more dynamic relocations.  We cannot count them until
     here.  */

  if (elf_hash_table (info)->dynamic_sections_created)
    {
      bfd_byte *b;
      bfd_boolean swap_out_p;

      BFD_ASSERT (sdyn != NULL);

      for (b = sdyn->contents;
	   b < sdyn->contents + sdyn->size;
	   b += MIPS_ELF_DYN_SIZE (dynobj))
	{
	  Elf_Internal_Dyn dyn;
	  asection *s;

	  /* Read in the current dynamic entry.  */
	  (*get_elf_backend_data (dynobj)->s->swap_dyn_in) (dynobj, b, &dyn);

	  /* Assume that we're going to modify it and write it out.  */
	  swap_out_p = TRUE;

	  switch (dyn.d_tag)
	    {
	    case DT_RELSZ:
	      /* Reduce DT_RELSZ to account for any relocations we
		 decided not to make.  This is for the n64 irix rld,
		 which doesn't seem to apply any relocations if there
		 are trailing null entries.  */
	      s = mips_elf_rel_dyn_section (info, FALSE);
	      dyn.d_un.d_val = (s->reloc_count
				* (ABI_64_P (output_bfd)
				   ? sizeof (Elf64_RISCV_External_Rel)
				   : sizeof (Elf32_External_Rel)));
	      /* Adjust the section size too.  Tools like the prelinker
		 can reasonably expect the values to the same.  */
	      elf_section_data (s->output_section)->this_hdr.sh_size
		= dyn.d_un.d_val;
	      break;

	    default:
	      swap_out_p = FALSE;
	      break;
	    }

	  if (swap_out_p)
	    (*get_elf_backend_data (dynobj)->s->swap_dyn_out)
	      (dynobj, &dyn, b);
	}
    }

  {
    asection *s;

    /* The psABI says that the dynamic relocations must be sorted in
       increasing order of r_symndx. */
    s = mips_elf_rel_dyn_section (info, FALSE);
    if (s != NULL && s->size > (bfd_vma)2 * MIPS_ELF_REL_SIZE (output_bfd))
      {
        reldyn_sorting_bfd = output_bfd;

        if (ABI_64_P (output_bfd))
          qsort ((Elf64_External_Rel *) s->contents + 1,
	         s->reloc_count - 1, sizeof (Elf64_RISCV_External_Rel),
	         sort_dynamic_relocs_64);
        else
          qsort ((Elf32_External_Rel *) s->contents + 1,
	         s->reloc_count - 1, sizeof (Elf32_External_Rel),
	         sort_dynamic_relocs);
      }
  }

  if (htab->splt && htab->splt->size > 0)
    {
      BFD_ASSERT (!info->shared);
      mips_finish_exec_plt (output_bfd, info);
    }
  return TRUE;
}


/* Set ABFD's EF_MIPS_ARCH and EF_MIPS_MACH flags.  */

static void
mips_set_isa_flags (bfd *abfd)
{
  flagword val;

  switch (bfd_get_mach (abfd))
    {
    default:
    case bfd_mach_riscv_rocket64:
      val = E_RISCV_ARCH_RV64 | E_RISCV_MACH_ROCKET64;
      break;

    case bfd_mach_riscv_rocket32:
      val = E_RISCV_ARCH_RV32 | E_RISCV_MACH_ROCKET32;
      break;
    }

  elf_elfheader (abfd)->e_flags &= ~(EF_MIPS_ARCH | EF_MIPS_MACH);
  elf_elfheader (abfd)->e_flags |= val;
}


/* The final processing done just before writing out a MIPS ELF object
   file.  This gets the MIPS architecture right based on the machine
   number.  This is used by both the 32-bit and the 64-bit ABI.  */

void
_bfd_riscv_elf_final_write_processing (bfd *abfd,
				      bfd_boolean linker ATTRIBUTE_UNUSED)
{
  unsigned int i;
  Elf_Internal_Shdr **hdrpp;
  const char *name;
  asection *sec;

  /* Keep the existing EF_MIPS_MACH and EF_MIPS_ARCH flags if the former
     is nonzero.  This is for compatibility with old objects, which used
     a combination of a 32-bit EF_MIPS_ARCH and a 64-bit EF_MIPS_MACH.  */
  if ((elf_elfheader (abfd)->e_flags & EF_MIPS_MACH) == 0)
    mips_set_isa_flags (abfd);

  /* Set the sh_info field for .gptab sections and other appropriate
     info for each special section.  */
  for (i = 1, hdrpp = elf_elfsections (abfd) + 1;
       i < elf_numsections (abfd);
       i++, hdrpp++)
    {
      switch ((*hdrpp)->sh_type)
	{
	case SHT_MIPS_MSYM:
	case SHT_MIPS_LIBLIST:
	  sec = bfd_get_section_by_name (abfd, ".dynstr");
	  if (sec != NULL)
	    (*hdrpp)->sh_link = elf_section_data (sec)->this_idx;
	  break;

	case SHT_MIPS_GPTAB:
          BFD_ASSERT (FALSE);

	case SHT_MIPS_CONTENT:
	  BFD_ASSERT ((*hdrpp)->bfd_section != NULL);
	  name = bfd_get_section_name (abfd, (*hdrpp)->bfd_section);
	  BFD_ASSERT (name != NULL
		      && CONST_STRNEQ (name, ".MIPS.content"));
	  sec = bfd_get_section_by_name (abfd,
					 name + sizeof ".MIPS.content" - 1);
	  BFD_ASSERT (sec != NULL);
	  (*hdrpp)->sh_link = elf_section_data (sec)->this_idx;
	  break;

	case SHT_MIPS_SYMBOL_LIB:
	  sec = bfd_get_section_by_name (abfd, ".dynsym");
	  if (sec != NULL)
	    (*hdrpp)->sh_link = elf_section_data (sec)->this_idx;
	  sec = bfd_get_section_by_name (abfd, ".liblist");
	  if (sec != NULL)
	    (*hdrpp)->sh_info = elf_section_data (sec)->this_idx;
	  break;

	case SHT_MIPS_EVENTS:
	  BFD_ASSERT ((*hdrpp)->bfd_section != NULL);
	  name = bfd_get_section_name (abfd, (*hdrpp)->bfd_section);
	  BFD_ASSERT (name != NULL);
	  if (CONST_STRNEQ (name, ".MIPS.events"))
	    sec = bfd_get_section_by_name (abfd,
					   name + sizeof ".MIPS.events" - 1);
	  else
	    {
	      BFD_ASSERT (CONST_STRNEQ (name, ".MIPS.post_rel"));
	      sec = bfd_get_section_by_name (abfd,
					     (name
					      + sizeof ".MIPS.post_rel" - 1));
	    }
	  BFD_ASSERT (sec != NULL);
	  (*hdrpp)->sh_link = elf_section_data (sec)->this_idx;
	  break;

	}
    }
}

/* When creating an IRIX5 executable, we need REGINFO and RTPROC
   segments.  */

int
_bfd_riscv_elf_additional_program_headers (bfd *abfd,
					  struct bfd_link_info *info ATTRIBUTE_UNUSED)
{
  asection *s;
  int ret = 0;

  /* See if we need a PT_MIPS_REGINFO segment.  */
  s = bfd_get_section_by_name (abfd, ".reginfo");
  if (s && (s->flags & SEC_LOAD))
    ++ret;

  /* Allocate a PT_NULL header in dynamic objects.  See
     _bfd_riscv_elf_modify_segment_map for details.  */
  if (bfd_get_section_by_name (abfd, ".dynamic"))
    ++ret;

  return ret;
}

/* Modify the segment map for an IRIX5 executable.  */

bfd_boolean
_bfd_riscv_elf_modify_segment_map (bfd *abfd,
				  struct bfd_link_info *info)
{
  asection *s;
  struct elf_segment_map *m, **pm;
  bfd_size_type amt;

  /* If there is a .reginfo section, we need a PT_MIPS_REGINFO
     segment.  */
  s = bfd_get_section_by_name (abfd, ".reginfo");
  if (s != NULL && (s->flags & SEC_LOAD) != 0)
    {
      for (m = elf_tdata (abfd)->segment_map; m != NULL; m = m->next)
	if (m->p_type == PT_MIPS_REGINFO)
	  break;
      if (m == NULL)
	{
	  amt = sizeof *m;
	  m = bfd_zalloc (abfd, amt);
	  if (m == NULL)
	    return FALSE;

	  m->p_type = PT_MIPS_REGINFO;
	  m->count = 1;
	  m->sections[0] = s;

	  /* We want to put it after the PHDR and INTERP segments.  */
	  pm = &elf_tdata (abfd)->segment_map;
	  while (*pm != NULL
		 && ((*pm)->p_type == PT_PHDR
		     || (*pm)->p_type == PT_INTERP))
	    pm = &(*pm)->next;

	  m->next = *pm;
	  *pm = m;
	}
    }

    {
      /* On IRIX5, the PT_DYNAMIC segment includes the .dynamic,
	 .dynstr, .dynsym, and .hash sections, and everything in
	 between.  */
      for (pm = &elf_tdata (abfd)->segment_map; *pm != NULL;
	   pm = &(*pm)->next)
	if ((*pm)->p_type == PT_DYNAMIC)
	  break;
      m = *pm;
      if (m != NULL)
	{
	  /* For a normal mips executable the permissions for the PT_DYNAMIC
	     segment are read, write and execute. We do that here since
	     the code in elf.c sets only the read permission. This matters
	     sometimes for the dynamic linker.  */
	  if (bfd_get_section_by_name (abfd, ".dynamic") != NULL)
	    {
	      m->p_flags = PF_R | PF_W | PF_X;
	      m->p_flags_valid = 1;
	    }
	}
    }

  /* Allocate a spare program header in dynamic objects so that tools
     like the prelinker can add an extra PT_LOAD entry.

     If the prelinker needs to make room for a new PT_LOAD entry, its
     standard procedure is to move the first (read-only) sections into
     the new (writable) segment.  However, the MIPS ABI requires
     .dynamic to be in a read-only segment, and the section will often
     start within sizeof (ElfNN_Phdr) bytes of the last program header.

     Although the prelinker could in principle move .dynamic to a
     writable segment, it seems better to allocate a spare program
     header instead, and avoid the need to move any sections.
     There is a long tradition of allocating spare dynamic tags,
     so allocating a spare program header seems like a natural
     extension.

     If INFO is NULL, we may be copying an already prelinked binary
     with objcopy or strip, so do not add this header.  */
  if (info != NULL
      && bfd_get_section_by_name (abfd, ".dynamic"))
    {
      for (pm = &elf_tdata (abfd)->segment_map; *pm != NULL; pm = &(*pm)->next)
	if ((*pm)->p_type == PT_NULL)
	  break;
      if (*pm == NULL)
	{
	  m = bfd_zalloc (abfd, sizeof (*m));
	  if (m == NULL)
	    return FALSE;

	  m->p_type = PT_NULL;
	  *pm = m;
	}
    }

  return TRUE;
}

/* Return the section that should be marked against GC for a given
   relocation.  */

asection *
_bfd_riscv_elf_gc_mark_hook (asection *sec,
			    struct bfd_link_info *info,
			    Elf_Internal_Rela *rel,
			    struct elf_link_hash_entry *h,
			    Elf_Internal_Sym *sym)
{
  /* ??? Do mips16 stub sections need to be handled special?  */

  if (h != NULL)
    switch (ELF_R_TYPE (sec->owner, rel->r_info))
      {
      case R_RISCV_GNU_VTINHERIT:
      case R_RISCV_GNU_VTENTRY:
	return NULL;
      }

  return _bfd_elf_gc_mark_hook (sec, info, rel, h, sym);
}

/* Update the got entry reference counts for the section being removed.  */

bfd_boolean
_bfd_riscv_elf_gc_sweep_hook (bfd *abfd ATTRIBUTE_UNUSED,
			     struct bfd_link_info *info ATTRIBUTE_UNUSED,
			     asection *sec ATTRIBUTE_UNUSED,
			     const Elf_Internal_Rela *relocs ATTRIBUTE_UNUSED)
{
  return TRUE;
}

/* Copy data from a MIPS ELF indirect symbol to its direct symbol,
   hiding the old indirect symbol.  Process additional relocation
   information.  Also called for weakdefs, in which case we just let
   _bfd_elf_link_hash_copy_indirect copy the flags for us.  */

void
_bfd_riscv_elf_copy_indirect_symbol (struct bfd_link_info *info,
				    struct elf_link_hash_entry *dir,
				    struct elf_link_hash_entry *ind)
{
  struct mips_elf_link_hash_entry *dirmips, *indmips;

  _bfd_elf_link_hash_copy_indirect (info, dir, ind);

  dirmips = (struct mips_elf_link_hash_entry *) dir;
  indmips = (struct mips_elf_link_hash_entry *) ind;
  /* Any absolute non-dynamic relocations against an indirect or weak
     definition will be against the target symbol.  */
  if (indmips->has_static_relocs)
    dirmips->has_static_relocs = TRUE;

  if (ind->root.type != bfd_link_hash_indirect)
    return;

  dirmips->possibly_dynamic_relocs += indmips->possibly_dynamic_relocs;
  if (indmips->readonly_reloc)
    dirmips->readonly_reloc = TRUE;
  if (indmips->no_fn_stub)
    dirmips->no_fn_stub = TRUE;
  if (indmips->global_got_area < dirmips->global_got_area)
    dirmips->global_got_area = indmips->global_got_area;
  if (indmips->global_got_area < GGA_NONE)
    indmips->global_got_area = GGA_NONE;
  if (indmips->has_nonpic_branches)
    dirmips->has_nonpic_branches = TRUE;

  if (dirmips->tls_type == 0)
    dirmips->tls_type = indmips->tls_type;
}

#define PDR_SIZE 32

bfd_boolean
_bfd_riscv_elf_discard_info (bfd *abfd, struct elf_reloc_cookie *cookie,
			    struct bfd_link_info *info)
{
  asection *o;
  bfd_boolean ret = FALSE;
  unsigned char *tdata;
  size_t i, skip;

  o = bfd_get_section_by_name (abfd, ".pdr");
  if (! o)
    return FALSE;
  if (o->size == 0)
    return FALSE;
  if (o->size % PDR_SIZE != 0)
    return FALSE;
  if (o->output_section != NULL
      && bfd_is_abs_section (o->output_section))
    return FALSE;

  tdata = bfd_zmalloc (o->size / PDR_SIZE);
  if (! tdata)
    return FALSE;

  cookie->rels = _bfd_elf_link_read_relocs (abfd, o, NULL, NULL,
					    info->keep_memory);
  if (!cookie->rels)
    {
      free (tdata);
      return FALSE;
    }

  cookie->rel = cookie->rels;
  cookie->relend = cookie->rels + o->reloc_count;

  for (i = 0, skip = 0; i < o->size / PDR_SIZE; i ++)
    {
      if (bfd_elf_reloc_symbol_deleted_p (i * PDR_SIZE, cookie))
	{
	  tdata[i] = 1;
	  skip ++;
	}
    }

  if (skip != 0)
    {
      mips_elf_section_data (o)->u.tdata = tdata;
      o->size -= skip * PDR_SIZE;
      ret = TRUE;
    }
  else
    free (tdata);

  if (! info->keep_memory)
    free (cookie->rels);

  return ret;
}

bfd_boolean
_bfd_riscv_elf_ignore_discarded_relocs (asection *sec)
{
  if (strcmp (sec->name, ".pdr") == 0)
    return TRUE;
  return FALSE;
}

bfd_boolean
_bfd_riscv_elf_write_section (bfd *output_bfd,
			     struct bfd_link_info *link_info ATTRIBUTE_UNUSED,
                             asection *sec, bfd_byte *contents)
{
  bfd_byte *to, *from, *end;
  int i;

  if (strcmp (sec->name, ".pdr") != 0)
    return FALSE;

  if (mips_elf_section_data (sec)->u.tdata == NULL)
    return FALSE;

  to = contents;
  end = contents + sec->size;
  for (from = contents, i = 0;
       from < end;
       from += PDR_SIZE, i++)
    {
      if ((mips_elf_section_data (sec)->u.tdata)[i] == 1)
	continue;
      if (to != from)
	memcpy (to, from, PDR_SIZE);
      to += PDR_SIZE;
    }
  bfd_set_section_contents (output_bfd, sec->output_section, contents,
			    sec->output_offset, sec->size);
  return TRUE;
}

/* MIPS ELF uses a special find_nearest_line routine in order the
   handle the ECOFF debugging information.  */

struct mips_elf_find_line
{
  struct ecoff_debug_info d;
  struct ecoff_find_line i;
};

bfd_boolean
_bfd_riscv_elf_find_nearest_line (bfd *abfd, asection *section,
				 asymbol **symbols, bfd_vma offset,
				 const char **filename_ptr,
				 const char **functionname_ptr,
				 unsigned int *line_ptr)
{
  asection *msec;

  if (_bfd_dwarf1_find_nearest_line (abfd, section, symbols, offset,
				     filename_ptr, functionname_ptr,
				     line_ptr))
    return TRUE;

  if (_bfd_dwarf2_find_nearest_line (abfd, section, symbols, offset,
				     filename_ptr, functionname_ptr,
				     line_ptr, ABI_64_P (abfd) ? 8 : 0,
				     &elf_tdata (abfd)->dwarf2_find_line_info))
    return TRUE;

  msec = bfd_get_section_by_name (abfd, ".mdebug");
  if (msec != NULL)
    {
      flagword origflags;
      struct mips_elf_find_line *fi;
      const struct ecoff_debug_swap * const swap =
	get_elf_backend_data (abfd)->elf_backend_ecoff_debug_swap;

      /* If we are called during a link, mips_elf_final_link may have
	 cleared the SEC_HAS_CONTENTS field.  We force it back on here
	 if appropriate (which it normally will be).  */
      origflags = msec->flags;
      if (elf_section_data (msec)->this_hdr.sh_type != SHT_NOBITS)
	msec->flags |= SEC_HAS_CONTENTS;

      fi = elf_tdata (abfd)->find_line_info;
      if (fi == NULL)
	{
	  bfd_size_type external_fdr_size;
	  char *fraw_src;
	  char *fraw_end;
	  struct fdr *fdr_ptr;
	  bfd_size_type amt = sizeof (struct mips_elf_find_line);

	  fi = bfd_zalloc (abfd, amt);
	  if (fi == NULL)
	    {
	      msec->flags = origflags;
	      return FALSE;
	    }

	  if (! _bfd_riscv_elf_read_ecoff_info (abfd, msec, &fi->d))
	    {
	      msec->flags = origflags;
	      return FALSE;
	    }

	  /* Swap in the FDR information.  */
	  amt = fi->d.symbolic_header.ifdMax * sizeof (struct fdr);
	  fi->d.fdr = bfd_alloc (abfd, amt);
	  if (fi->d.fdr == NULL)
	    {
	      msec->flags = origflags;
	      return FALSE;
	    }
	  external_fdr_size = swap->external_fdr_size;
	  fdr_ptr = fi->d.fdr;
	  fraw_src = (char *) fi->d.external_fdr;
	  fraw_end = (fraw_src
		      + fi->d.symbolic_header.ifdMax * external_fdr_size);
	  for (; fraw_src < fraw_end; fraw_src += external_fdr_size, fdr_ptr++)
	    (*swap->swap_fdr_in) (abfd, fraw_src, fdr_ptr);

	  elf_tdata (abfd)->find_line_info = fi;

	  /* Note that we don't bother to ever free this information.
             find_nearest_line is either called all the time, as in
             objdump -l, so the information should be saved, or it is
             rarely called, as in ld error messages, so the memory
             wasted is unimportant.  Still, it would probably be a
             good idea for free_cached_info to throw it away.  */
	}

      if (_bfd_ecoff_locate_line (abfd, section, offset, &fi->d, swap,
				  &fi->i, filename_ptr, functionname_ptr,
				  line_ptr))
	{
	  msec->flags = origflags;
	  return TRUE;
	}

      msec->flags = origflags;
    }

  /* Fall back on the generic ELF find_nearest_line routine.  */

  return _bfd_elf_find_nearest_line (abfd, section, symbols, offset,
				     filename_ptr, functionname_ptr,
				     line_ptr);
}

bfd_boolean
_bfd_riscv_elf_find_inliner_info (bfd *abfd,
				 const char **filename_ptr,
				 const char **functionname_ptr,
				 unsigned int *line_ptr)
{
  bfd_boolean found;
  found = _bfd_dwarf2_find_inliner_info (abfd, filename_ptr,
					 functionname_ptr, line_ptr,
					 & elf_tdata (abfd)->dwarf2_find_line_info);
  return found;
}


/* When are writing out the .options or .MIPS.options section,
   remember the bytes we are writing out, so that we can install the
   GP value in the section_processing routine.  */

bfd_boolean
_bfd_riscv_elf_set_section_contents (bfd *abfd, sec_ptr section,
				    const void *location,
				    file_ptr offset, bfd_size_type count)
{
  if (MIPS_ELF_OPTIONS_SECTION_NAME_P (section->name))
    {
      bfd_byte *c;

      if (elf_section_data (section) == NULL)
	{
	  bfd_size_type amt = sizeof (struct bfd_elf_section_data);
	  section->used_by_bfd = bfd_zalloc (abfd, amt);
	  if (elf_section_data (section) == NULL)
	    return FALSE;
	}
      c = mips_elf_section_data (section)->u.tdata;
      if (c == NULL)
	{
	  c = bfd_zalloc (abfd, section->size);
	  if (c == NULL)
	    return FALSE;
	  mips_elf_section_data (section)->u.tdata = c;
	}

      memcpy (c + offset, location, count);
    }

  return _bfd_elf_set_section_contents (abfd, section, location, offset,
					count);
}

/* This is almost identical to bfd_generic_get_... except that some
   MIPS relocations need to be handled specially.  Sigh.  */

bfd_byte *
_bfd_elf_riscv_get_relocated_section_contents
  (bfd *abfd,
   struct bfd_link_info *link_info,
   struct bfd_link_order *link_order,
   bfd_byte *data,
   bfd_boolean relocatable,
   asymbol **symbols)
{
  /* Get enough memory to hold the stuff */
  bfd *input_bfd = link_order->u.indirect.section->owner;
  asection *input_section = link_order->u.indirect.section;
  bfd_size_type sz;

  long reloc_size = bfd_get_reloc_upper_bound (input_bfd, input_section);
  arelent **reloc_vector = NULL;
  long reloc_count;

  if (reloc_size < 0)
    goto error_return;

  reloc_vector = bfd_malloc (reloc_size);
  if (reloc_vector == NULL && reloc_size != 0)
    goto error_return;

  /* read in the section */
  sz = input_section->rawsize ? input_section->rawsize : input_section->size;
  if (!bfd_get_section_contents (input_bfd, input_section, data, 0, sz))
    goto error_return;

  reloc_count = bfd_canonicalize_reloc (input_bfd,
					input_section,
					reloc_vector,
					symbols);
  if (reloc_count < 0)
    goto error_return;

  if (reloc_count > 0)
    {
      arelent **parent;

      {
	struct bfd_hash_entry *h;
	struct bfd_link_hash_entry *lh;
	/* Skip all this stuff if we aren't mixing formats.  */
	if (abfd && input_bfd
	    && abfd->xvec == input_bfd->xvec)
	  lh = 0;
	else
	  {
	    h = bfd_hash_lookup (&link_info->hash->table, "_gp", FALSE, FALSE);
	    lh = (struct bfd_link_hash_entry *) h;
	  }
      lookup:
	if (lh)
	  {
	    switch (lh->type)
	      {
	      case bfd_link_hash_undefined:
	      case bfd_link_hash_undefweak:
	      case bfd_link_hash_common:
	      case bfd_link_hash_defined:
	      case bfd_link_hash_defweak:
		break;
	      case bfd_link_hash_indirect:
	      case bfd_link_hash_warning:
		lh = lh->u.i.link;
		/* @@FIXME  ignoring warning for now */
		goto lookup;
	      case bfd_link_hash_new:
	      default:
		abort ();
	      }
	  }
      }
      /* end mips */
      for (parent = reloc_vector; *parent != NULL; parent++)
	{
	  char *error_message = NULL;
	  bfd_reloc_status_type r;

	  r = bfd_perform_relocation (input_bfd, *parent, data,
				      input_section,
				      relocatable ? abfd : NULL,
				      &error_message);

	  if (relocatable)
	    {
	      asection *os = input_section->output_section;

	      /* A partial link, so keep the relocs */
	      os->orelocation[os->reloc_count] = *parent;
	      os->reloc_count++;
	    }

	  if (r != bfd_reloc_ok)
	    {
	      switch (r)
		{
		case bfd_reloc_undefined:
		  if (!((*link_info->callbacks->undefined_symbol)
			(link_info, bfd_asymbol_name (*(*parent)->sym_ptr_ptr),
			 input_bfd, input_section, (*parent)->address, TRUE)))
		    goto error_return;
		  break;
		case bfd_reloc_dangerous:
		  BFD_ASSERT (error_message != NULL);
		  if (!((*link_info->callbacks->reloc_dangerous)
			(link_info, error_message, input_bfd, input_section,
			 (*parent)->address)))
		    goto error_return;
		  break;
		case bfd_reloc_overflow:
		  if (!((*link_info->callbacks->reloc_overflow)
			(link_info, NULL,
			 bfd_asymbol_name (*(*parent)->sym_ptr_ptr),
			 (*parent)->howto->name, (*parent)->addend,
			 input_bfd, input_section, (*parent)->address)))
		    goto error_return;
		  break;
		case bfd_reloc_outofrange:
		default:
		  abort ();
		  break;
		}

	    }
	}
    }
  if (reloc_vector != NULL)
    free (reloc_vector);
  return data;

error_return:
  if (reloc_vector != NULL)
    free (reloc_vector);
  return NULL;
}

/* Create a MIPS ELF linker hash table.  */

struct bfd_link_hash_table *
_bfd_riscv_elf_link_hash_table_create (bfd *abfd)
{
  struct mips_elf_link_hash_table *ret;
  bfd_size_type amt = sizeof (struct mips_elf_link_hash_table);

  ret = bfd_malloc (amt);
  if (ret == NULL)
    return NULL;

  if (!_bfd_elf_link_hash_table_init (&ret->root, abfd,
				      mips_elf_link_hash_newfunc,
				      sizeof (struct mips_elf_link_hash_entry),
				      MIPS_ELF_DATA))
    {
      free (ret);
      return NULL;
    }

#if 0
  /* We no longer use this.  */
  for (i = 0; i < SIZEOF_MIPS_DYNSYM_SECNAMES; i++)
    ret->dynsym_sec_strindex[i] = (bfd_size_type) -1;
#endif
  ret->procedure_count = 0;
  ret->use_rld_obj_head = FALSE;
  ret->rld_value = 0;
  ret->use_plts_and_copy_relocs = FALSE;
  ret->srelbss = NULL;
  ret->sdynbss = NULL;
  ret->srelplt = NULL;
  ret->srelplt2 = NULL;
  ret->sgotplt = NULL;
  ret->splt = NULL;
  ret->sstubs = NULL;
  ret->sgot = NULL;
  ret->got_info = NULL;
  ret->plt_header_size = 0;
  ret->plt_entry_size = 0;
  ret->lazy_stub_count = 0;
  ret->function_stub_size = 0;
  ret->strampoline = NULL;
  ret->la25_stubs = NULL;
  ret->add_stub_section = NULL;

  return &ret->root.root;
}

/* A function that the linker calls if we are allowed to use PLTs
   and copy relocs.  */

void
_bfd_riscv_elf_use_plts_and_copy_relocs (struct bfd_link_info *info)
{
  mips_elf_hash_table (info)->use_plts_and_copy_relocs = TRUE;
}

/* We need to use a special link routine to handle the .reginfo and
   the .mdebug sections.  We need to merge all instances of these
   sections together, not write them all out sequentially.  */

bfd_boolean
_bfd_riscv_elf_final_link (bfd *abfd, struct bfd_link_info *info)
{
  asection *o;
  struct bfd_link_order *p;
  asection *reginfo_sec, *mdebug_sec;
  Elf32_RegInfo reginfo;
  struct ecoff_debug_info debug;
  struct mips_htab_traverse_info hti;
  const struct elf_backend_data *bed = get_elf_backend_data (abfd);
  const struct ecoff_debug_swap *swap = bed->elf_backend_ecoff_debug_swap;
  HDRR *symhdr = &debug.symbolic_header;
  void *mdebug_handle = NULL;
  asection *s;
  EXTR esym;
  unsigned int i;
  struct mips_elf_link_hash_table *htab;

  static const char * const secname[] =
  {
    ".text", ".init", ".fini", ".data",
    ".rodata", ".sdata", ".sbss", ".bss"
  };
  static const int sc[] =
  {
    scText, scInit, scFini, scData,
    scRData, scSData, scSBss, scBss
  };

  /* Sort the dynamic symbols so that those with GOT entries come after
     those without.  */
  htab = mips_elf_hash_table (info);
  BFD_ASSERT (htab != NULL);

  if (!mips_elf_sort_hash_table (abfd, info))
    return FALSE;

  /* Create any scheduled LA25 stubs.  */
  hti.info = info;
  hti.output_bfd = abfd;
  hti.error = FALSE;
  htab_traverse (htab->la25_stubs, mips_elf_create_la25_stub, &hti);
  if (hti.error)
    return FALSE;

  /* Get a value for the GP register.  */
  if (elf_gp (abfd) == 0)
    {
      struct bfd_link_hash_entry *h;

      h = bfd_link_hash_lookup (info->hash, "_gp", FALSE, FALSE, TRUE);
      if (h != NULL && h->type == bfd_link_hash_defined)
	elf_gp (abfd) = (h->u.def.value
			 + h->u.def.section->output_section->vma
			 + h->u.def.section->output_offset);
      else if (info->relocatable)
	{
	  bfd_vma lo = MINUS_ONE;

	  /* Find the GP-relative section with the lowest offset.  */
	  for (o = abfd->sections; o != NULL; o = o->next)
	    if (o->vma < lo
		&& (elf_section_data (o)->this_hdr.sh_flags & SHF_MIPS_GPREL))
	      lo = o->vma;

	  /* And calculate GP relative to that.  */
	  elf_gp (abfd) = lo;
	}
      else
	{
	  /* If the relocate_section function needs to do a reloc
	     involving the GP value, it should make a reloc_dangerous
	     callback to warn that GP is not defined.  */
	}
    }

  /* Go through the sections and collect the .reginfo and .mdebug
     information.  */
  reginfo_sec = NULL;
  mdebug_sec = NULL;
  for (o = abfd->sections; o != NULL; o = o->next)
    {
      if (strcmp (o->name, ".reginfo") == 0)
	{
	  memset (&reginfo, 0, sizeof reginfo);

	  /* We have found the .reginfo section in the output file.
	     Look through all the link_orders comprising it and merge
	     the information together.  */
	  for (p = o->map_head.link_order; p != NULL; p = p->next)
	    {
	      asection *input_section;
	      bfd *input_bfd;
	      Elf32_External_RegInfo ext;
	      Elf32_RegInfo sub;

	      if (p->type != bfd_indirect_link_order)
		{
		  if (p->type == bfd_data_link_order)
		    continue;
		  abort ();
		}

	      input_section = p->u.indirect.section;
	      input_bfd = input_section->owner;

	      if (! bfd_get_section_contents (input_bfd, input_section,
					      &ext, 0, sizeof ext))
		return FALSE;

	      bfd_riscv_elf32_swap_reginfo_in (input_bfd, &ext, &sub);

	      reginfo.ri_gprmask |= sub.ri_gprmask;
	      reginfo.ri_cprmask[0] |= sub.ri_cprmask[0];
	      reginfo.ri_cprmask[1] |= sub.ri_cprmask[1];
	      reginfo.ri_cprmask[2] |= sub.ri_cprmask[2];
	      reginfo.ri_cprmask[3] |= sub.ri_cprmask[3];

	      /* ri_gp_value is set by the function
		 mips_elf32_section_processing when the section is
		 finally written out.  */

	      /* Hack: reset the SEC_HAS_CONTENTS flag so that
		 elf_link_input_bfd ignores this section.  */
	      input_section->flags &= ~SEC_HAS_CONTENTS;
	    }

	  /* Size has been set in _bfd_riscv_elf_always_size_sections.  */
	  BFD_ASSERT(o->size == sizeof (Elf32_External_RegInfo));

	  /* Skip this section later on (I don't think this currently
	     matters, but someday it might).  */
	  o->map_head.link_order = NULL;

	  reginfo_sec = o;
	}

      if (strcmp (o->name, ".mdebug") == 0)
	{
	  struct extsym_info einfo;
	  bfd_vma last;

	  /* We have found the .mdebug section in the output file.
	     Look through all the link_orders comprising it and merge
	     the information together.  */
	  symhdr->magic = swap->sym_magic;
	  /* FIXME: What should the version stamp be?  */
	  symhdr->vstamp = 0;
	  symhdr->ilineMax = 0;
	  symhdr->cbLine = 0;
	  symhdr->idnMax = 0;
	  symhdr->ipdMax = 0;
	  symhdr->isymMax = 0;
	  symhdr->ioptMax = 0;
	  symhdr->iauxMax = 0;
	  symhdr->issMax = 0;
	  symhdr->issExtMax = 0;
	  symhdr->ifdMax = 0;
	  symhdr->crfd = 0;
	  symhdr->iextMax = 0;

	  /* We accumulate the debugging information itself in the
	     debug_info structure.  */
	  debug.line = NULL;
	  debug.external_dnr = NULL;
	  debug.external_pdr = NULL;
	  debug.external_sym = NULL;
	  debug.external_opt = NULL;
	  debug.external_aux = NULL;
	  debug.ss = NULL;
	  debug.ssext = debug.ssext_end = NULL;
	  debug.external_fdr = NULL;
	  debug.external_rfd = NULL;
	  debug.external_ext = debug.external_ext_end = NULL;

	  mdebug_handle = bfd_ecoff_debug_init (abfd, &debug, swap, info);
	  if (mdebug_handle == NULL)
	    return FALSE;

	  esym.jmptbl = 0;
	  esym.cobol_main = 0;
	  esym.weakext = 0;
	  esym.reserved = 0;
	  esym.ifd = ifdNil;
	  esym.asym.iss = issNil;
	  esym.asym.st = stLocal;
	  esym.asym.reserved = 0;
	  esym.asym.index = indexNil;
	  last = 0;
	  for (i = 0; i < sizeof (secname) / sizeof (secname[0]); i++)
	    {
	      esym.asym.sc = sc[i];
	      s = bfd_get_section_by_name (abfd, secname[i]);
	      if (s != NULL)
		{
		  esym.asym.value = s->vma;
		  last = s->vma + s->size;
		}
	      else
		esym.asym.value = last;
	      if (!bfd_ecoff_debug_one_external (abfd, &debug, swap,
						 secname[i], &esym))
		return FALSE;
	    }

	  for (p = o->map_head.link_order; p != NULL; p = p->next)
	    {
	      asection *input_section;
	      bfd *input_bfd;
	      const struct ecoff_debug_swap *input_swap;
	      struct ecoff_debug_info input_debug;
	      char *eraw_src;
	      char *eraw_end;

	      if (p->type != bfd_indirect_link_order)
		{
		  if (p->type == bfd_data_link_order)
		    continue;
		  abort ();
		}

	      input_section = p->u.indirect.section;
	      input_bfd = input_section->owner;

	      if (!is_mips_elf (input_bfd))
		{
		  /* I don't know what a non MIPS ELF bfd would be
		     doing with a .mdebug section, but I don't really
		     want to deal with it.  */
		  continue;
		}

	      input_swap = (get_elf_backend_data (input_bfd)
			    ->elf_backend_ecoff_debug_swap);

	      BFD_ASSERT (p->size == input_section->size);

	      /* The ECOFF linking code expects that we have already
		 read in the debugging information and set up an
		 ecoff_debug_info structure, so we do that now.  */
	      if (! _bfd_riscv_elf_read_ecoff_info (input_bfd, input_section,
						   &input_debug))
		return FALSE;

	      if (! (bfd_ecoff_debug_accumulate
		     (mdebug_handle, abfd, &debug, swap, input_bfd,
		      &input_debug, input_swap, info)))
		return FALSE;

	      /* Loop through the external symbols.  For each one with
		 interesting information, try to find the symbol in
		 the linker global hash table and save the information
		 for the output external symbols.  */
	      eraw_src = input_debug.external_ext;
	      eraw_end = (eraw_src
			  + (input_debug.symbolic_header.iextMax
			     * input_swap->external_ext_size));
	      for (;
		   eraw_src < eraw_end;
		   eraw_src += input_swap->external_ext_size)
		{
		  EXTR ext;
		  const char *name;
		  struct mips_elf_link_hash_entry *h;

		  (*input_swap->swap_ext_in) (input_bfd, eraw_src, &ext);
		  if (ext.asym.sc == scNil
		      || ext.asym.sc == scUndefined
		      || ext.asym.sc == scSUndefined)
		    continue;

		  name = input_debug.ssext + ext.asym.iss;
		  h = mips_elf_link_hash_lookup (mips_elf_hash_table (info),
						 name, FALSE, FALSE, TRUE);
		  if (h == NULL || h->esym.ifd != -2)
		    continue;

		  if (ext.ifd != -1)
		    {
		      BFD_ASSERT (ext.ifd
				  < input_debug.symbolic_header.ifdMax);
		      ext.ifd = input_debug.ifdmap[ext.ifd];
		    }

		  h->esym = ext;
		}

	      /* Free up the information we just read.  */
	      free (input_debug.line);
	      free (input_debug.external_dnr);
	      free (input_debug.external_pdr);
	      free (input_debug.external_sym);
	      free (input_debug.external_opt);
	      free (input_debug.external_aux);
	      free (input_debug.ss);
	      free (input_debug.ssext);
	      free (input_debug.external_fdr);
	      free (input_debug.external_rfd);
	      free (input_debug.external_ext);

	      /* Hack: reset the SEC_HAS_CONTENTS flag so that
		 elf_link_input_bfd ignores this section.  */
	      input_section->flags &= ~SEC_HAS_CONTENTS;
	    }

	  /* Build the external symbol information.  */
	  einfo.abfd = abfd;
	  einfo.info = info;
	  einfo.debug = &debug;
	  einfo.swap = swap;
	  einfo.failed = FALSE;
	  mips_elf_link_hash_traverse (mips_elf_hash_table (info),
				       mips_elf_output_extsym, &einfo);
	  if (einfo.failed)
	    return FALSE;

	  /* Set the size of the .mdebug section.  */
	  o->size = bfd_ecoff_debug_size (abfd, &debug, swap);

	  /* Skip this section later on (I don't think this currently
	     matters, but someday it might).  */
	  o->map_head.link_order = NULL;

	  mdebug_sec = o;
	}
    }

  /* Invoke the regular ELF backend linker to do all the work.  */
  if (!bfd_elf_final_link (abfd, info))
    return FALSE;

  /* Now write out the computed sections.  */

  if (reginfo_sec != NULL)
    {
      Elf32_External_RegInfo ext;

      bfd_riscv_elf32_swap_reginfo_out (abfd, &reginfo, &ext);
      if (! bfd_set_section_contents (abfd, reginfo_sec, &ext, 0, sizeof ext))
	return FALSE;
    }

  if (mdebug_sec != NULL)
    {
      BFD_ASSERT (abfd->output_has_begun);
      if (! bfd_ecoff_write_accumulated_debug (mdebug_handle, abfd, &debug,
					       swap, info,
					       mdebug_sec->filepos))
	return FALSE;

      bfd_ecoff_debug_free (mdebug_handle, abfd, &debug, swap, info);
    }

  return TRUE;
}

/* Return true if bfd machine EXTENSION is an extension of machine BASE.  */

static bfd_boolean
mips_mach_extends_p (unsigned long base, unsigned long extension)
{
  return extension == base;
}


/* Return true if the given ELF header flags describe a 32-bit binary.  */

static bfd_boolean
mips_32bit_flags_p (flagword flags)
{
  return ((flags & EF_MIPS_32BITMODE) != 0
	  || (flags & EF_MIPS_ABI) == E_RISCV_ABI_32
	  || (flags & EF_MIPS_ARCH) == E_RISCV_ARCH_RV32);
}


/* Merge object attributes from IBFD into OBFD.  Raise an error if
   there are conflicting attributes.  */
static bfd_boolean
mips_elf_merge_obj_attributes (bfd *ibfd, bfd *obfd)
{
  if (!elf_known_obj_attributes_proc (obfd)[0].i)
    {
      /* This is the first object.  Copy the attributes.  */
      _bfd_elf_copy_obj_attributes (ibfd, obfd);

      /* Use the Tag_null value to indicate the attributes have been
	 initialized.  */
      elf_known_obj_attributes_proc (obfd)[0].i = 1;

      return TRUE;
    }

  /* Merge Tag_compatibility attributes and any common GNU ones.  */
  _bfd_elf_merge_object_attributes (ibfd, obfd);

  return TRUE;
}

/* Merge backend specific data from an object file to the output
   object file when linking.  */

bfd_boolean
_bfd_riscv_elf_merge_private_bfd_data (bfd *ibfd, bfd *obfd)
{
  flagword old_flags;
  flagword new_flags;
  bfd_boolean ok;
  bfd_boolean null_input_bfd = TRUE;
  asection *sec;

  /* Check if we have the same endianess */
  if (! _bfd_generic_verify_endian_match (ibfd, obfd))
    {
      (*_bfd_error_handler)
	(_("%B: endianness incompatible with that of the selected emulation"),
	 ibfd);
      return FALSE;
    }

  if (!is_mips_elf (ibfd) || !is_mips_elf (obfd))
    return TRUE;

  if (strcmp (bfd_get_target (ibfd), bfd_get_target (obfd)) != 0)
    {
      (*_bfd_error_handler)
	(_("%B: ABI is incompatible with that of the selected emulation"),
	 ibfd);
      return FALSE;
    }

  if (!mips_elf_merge_obj_attributes (ibfd, obfd))
    return FALSE;

  new_flags = elf_elfheader (ibfd)->e_flags;
  elf_elfheader (obfd)->e_flags |= new_flags & EF_MIPS_NOREORDER;
  old_flags = elf_elfheader (obfd)->e_flags;

  if (! elf_flags_init (obfd))
    {
      elf_flags_init (obfd) = TRUE;
      elf_elfheader (obfd)->e_flags = new_flags;
      elf_elfheader (obfd)->e_ident[EI_CLASS]
	= elf_elfheader (ibfd)->e_ident[EI_CLASS];

      if (bfd_get_arch (obfd) == bfd_get_arch (ibfd)
	  && (bfd_get_arch_info (obfd)->the_default
	      || mips_mach_extends_p (bfd_get_mach (obfd), 
				      bfd_get_mach (ibfd))))
	{
	  if (! bfd_set_arch_mach (obfd, bfd_get_arch (ibfd),
				   bfd_get_mach (ibfd)))
	    return FALSE;
	}

      return TRUE;
    }

  /* Check flag compatibility.  */

  new_flags &= ~EF_MIPS_NOREORDER;
  old_flags &= ~EF_MIPS_NOREORDER;

  /* Some IRIX 6 BSD-compatibility objects have this bit set.  It
     doesn't seem to matter.  */
  new_flags &= ~EF_MIPS_XGOT;
  old_flags &= ~EF_MIPS_XGOT;

  /* MIPSpro generates ucode info in n64 objects.  Again, we should
     just be able to ignore this.  */
  new_flags &= ~EF_MIPS_UCODE;
  old_flags &= ~EF_MIPS_UCODE;

  /* DSOs should only be linked with CPIC code.  */
  if ((ibfd->flags & DYNAMIC) != 0)
    new_flags |= EF_MIPS_PIC | EF_MIPS_CPIC;

  if (new_flags == old_flags)
    return TRUE;

  /* Check to see if the input BFD actually contains any sections.
     If not, its flags may not have been initialised either, but it cannot
     actually cause any incompatibility.  */
  for (sec = ibfd->sections; sec != NULL; sec = sec->next)
    {
      /* Ignore synthetic sections and empty .text, .data and .bss sections
	 which are automatically generated by gas.  Also ignore fake
	 (s)common sections, since merely defining a common symbol does
	 not affect compatibility.  */
      if ((sec->flags & SEC_IS_COMMON) == 0
	  && strcmp (sec->name, ".reginfo")
	  && strcmp (sec->name, ".mdebug")
	  && (sec->size != 0
	      || (strcmp (sec->name, ".text")
		  && strcmp (sec->name, ".data")
		  && strcmp (sec->name, ".bss"))))
	{
	  null_input_bfd = FALSE;
	  break;
	}
    }
  if (null_input_bfd)
    return TRUE;

  ok = TRUE;

  if (((new_flags & (EF_MIPS_PIC | EF_MIPS_CPIC)) != 0)
      != ((old_flags & (EF_MIPS_PIC | EF_MIPS_CPIC)) != 0))
    {
      (*_bfd_error_handler)
	(_("%B: warning: linking abicalls files with non-abicalls files"),
	 ibfd);
      ok = TRUE;
    }

  if (new_flags & (EF_MIPS_PIC | EF_MIPS_CPIC))
    elf_elfheader (obfd)->e_flags |= EF_MIPS_CPIC;
  if (! (new_flags & EF_MIPS_PIC))
    elf_elfheader (obfd)->e_flags &= ~EF_MIPS_PIC;

  new_flags &= ~ (EF_MIPS_PIC | EF_MIPS_CPIC);
  old_flags &= ~ (EF_MIPS_PIC | EF_MIPS_CPIC);

  /* Compare the ISAs.  */
  if (mips_32bit_flags_p (old_flags) != mips_32bit_flags_p (new_flags))
    {
      (*_bfd_error_handler)
	(_("%B: linking 32-bit code with 64-bit code"),
	 ibfd);
      ok = FALSE;
    }
  else if (!mips_mach_extends_p (bfd_get_mach (ibfd), bfd_get_mach (obfd)))
    {
      /* OBFD's ISA isn't the same as, or an extension of, IBFD's.  */
      if (mips_mach_extends_p (bfd_get_mach (obfd), bfd_get_mach (ibfd)))
	{
	  /* Copy the architecture info from IBFD to OBFD.  Also copy
	     the 32-bit flag (if set) so that we continue to recognise
	     OBFD as a 32-bit binary.  */
	  bfd_set_arch_info (obfd, bfd_get_arch_info (ibfd));
	  elf_elfheader (obfd)->e_flags &= ~(EF_MIPS_ARCH | EF_MIPS_MACH);
	  elf_elfheader (obfd)->e_flags
	    |= new_flags & (EF_MIPS_ARCH | EF_MIPS_MACH | EF_MIPS_32BITMODE);

	  /* Copy across the ABI flags if OBFD doesn't use them
	     and if that was what caused us to treat IBFD as 32-bit.  */
	  if ((old_flags & EF_MIPS_ABI) == 0
	      && mips_32bit_flags_p (new_flags)
	      && !mips_32bit_flags_p (new_flags & ~EF_MIPS_ABI))
	    elf_elfheader (obfd)->e_flags |= new_flags & EF_MIPS_ABI;
	}
      else
	{
	  /* The ISAs aren't compatible.  */
	  (*_bfd_error_handler)
	    (_("%B: linking %s module with previous %s modules"),
	     ibfd,
	     bfd_printable_name (ibfd),
	     bfd_printable_name (obfd));
	  ok = FALSE;
	}
    }

  new_flags &= ~(EF_MIPS_ARCH | EF_MIPS_MACH | EF_MIPS_32BITMODE);
  old_flags &= ~(EF_MIPS_ARCH | EF_MIPS_MACH | EF_MIPS_32BITMODE);

  /* Compare ABIs.  The 64-bit ABI does not use EF_MIPS_ABI.  But, it
     does set EI_CLASS differently from any 32-bit ABI.  */
  if ((new_flags & EF_MIPS_ABI) != (old_flags & EF_MIPS_ABI)
      || (elf_elfheader (ibfd)->e_ident[EI_CLASS]
	  != elf_elfheader (obfd)->e_ident[EI_CLASS]))
    {
      /* Only error if both are set (to different values).  */
      if (((new_flags & EF_MIPS_ABI) && (old_flags & EF_MIPS_ABI))
	  || (elf_elfheader (ibfd)->e_ident[EI_CLASS]
	      != elf_elfheader (obfd)->e_ident[EI_CLASS]))
	{
	  (*_bfd_error_handler)
	    (_("%B: ABI mismatch: linking %s module with previous %s modules"),
	     ibfd,
	     elf_mips_abi_name (ibfd),
	     elf_mips_abi_name (obfd));
	  ok = FALSE;
	}
      new_flags &= ~EF_MIPS_ABI;
      old_flags &= ~EF_MIPS_ABI;
    }

  /* For now, allow arbitrary mixing of ASEs (retain the union).  */
  if ((new_flags & EF_MIPS_ARCH_ASE) != (old_flags & EF_MIPS_ARCH_ASE))
    {
      elf_elfheader (obfd)->e_flags |= new_flags & EF_MIPS_ARCH_ASE;

      new_flags &= ~ EF_MIPS_ARCH_ASE;
      old_flags &= ~ EF_MIPS_ARCH_ASE;
    }

  /* Warn about any other mismatches */
  if (new_flags != old_flags)
    {
      (*_bfd_error_handler)
	(_("%B: uses different e_flags (0x%lx) fields than previous modules (0x%lx)"),
	 ibfd, (unsigned long) new_flags,
	 (unsigned long) old_flags);
      ok = FALSE;
    }

  if (! ok)
    {
      bfd_set_error (bfd_error_bad_value);
      return FALSE;
    }

  return TRUE;
}

/* Function to keep MIPS specific file flags like as EF_MIPS_PIC.  */

bfd_boolean
_bfd_riscv_elf_set_private_flags (bfd *abfd, flagword flags)
{
  BFD_ASSERT (!elf_flags_init (abfd)
	      || elf_elfheader (abfd)->e_flags == flags);

  elf_elfheader (abfd)->e_flags = flags;
  elf_flags_init (abfd) = TRUE;
  return TRUE;
}

char *
_bfd_riscv_elf_get_target_dtag (bfd_vma dtag)
{
  switch (dtag)
    {
    default: return "";
    case DT_MIPS_RLD_VERSION:
      return "MIPS_RLD_VERSION";
    case DT_MIPS_TIME_STAMP:
      return "MIPS_TIME_STAMP";
    case DT_MIPS_ICHECKSUM:
      return "MIPS_ICHECKSUM";
    case DT_MIPS_IVERSION:
      return "MIPS_IVERSION";
    case DT_MIPS_FLAGS:
      return "MIPS_FLAGS";
    case DT_MIPS_BASE_ADDRESS:
      return "MIPS_BASE_ADDRESS";
    case DT_MIPS_MSYM:
      return "MIPS_MSYM";
    case DT_MIPS_CONFLICT:
      return "MIPS_CONFLICT";
    case DT_MIPS_LIBLIST:
      return "MIPS_LIBLIST";
    case DT_MIPS_LOCAL_GOTNO:
      return "MIPS_LOCAL_GOTNO";
    case DT_MIPS_CONFLICTNO:
      return "MIPS_CONFLICTNO";
    case DT_MIPS_LIBLISTNO:
      return "MIPS_LIBLISTNO";
    case DT_MIPS_SYMTABNO:
      return "MIPS_SYMTABNO";
    case DT_MIPS_UNREFEXTNO:
      return "MIPS_UNREFEXTNO";
    case DT_MIPS_GOTSYM:
      return "MIPS_GOTSYM";
    case DT_MIPS_HIPAGENO:
      return "MIPS_HIPAGENO";
    case DT_MIPS_RLD_MAP:
      return "MIPS_RLD_MAP";
    case DT_MIPS_DELTA_CLASS:
      return "MIPS_DELTA_CLASS";
    case DT_MIPS_DELTA_CLASS_NO:
      return "MIPS_DELTA_CLASS_NO";
    case DT_MIPS_DELTA_INSTANCE:
      return "MIPS_DELTA_INSTANCE";
    case DT_MIPS_DELTA_INSTANCE_NO:
      return "MIPS_DELTA_INSTANCE_NO";
    case DT_MIPS_DELTA_RELOC:
      return "MIPS_DELTA_RELOC";
    case DT_MIPS_DELTA_RELOC_NO:
      return "MIPS_DELTA_RELOC_NO";
    case DT_MIPS_DELTA_SYM:
      return "MIPS_DELTA_SYM";
    case DT_MIPS_DELTA_SYM_NO:
      return "MIPS_DELTA_SYM_NO";
    case DT_MIPS_DELTA_CLASSSYM:
      return "MIPS_DELTA_CLASSSYM";
    case DT_MIPS_DELTA_CLASSSYM_NO:
      return "MIPS_DELTA_CLASSSYM_NO";
    case DT_MIPS_CXX_FLAGS:
      return "MIPS_CXX_FLAGS";
    case DT_MIPS_PIXIE_INIT:
      return "MIPS_PIXIE_INIT";
    case DT_MIPS_SYMBOL_LIB:
      return "MIPS_SYMBOL_LIB";
    case DT_MIPS_LOCALPAGE_GOTIDX:
      return "MIPS_LOCALPAGE_GOTIDX";
    case DT_MIPS_LOCAL_GOTIDX:
      return "MIPS_LOCAL_GOTIDX";
    case DT_MIPS_HIDDEN_GOTIDX:
      return "MIPS_HIDDEN_GOTIDX";
    case DT_MIPS_PROTECTED_GOTIDX:
      return "MIPS_PROTECTED_GOT_IDX";
    case DT_MIPS_OPTIONS:
      return "MIPS_OPTIONS";
    case DT_MIPS_INTERFACE:
      return "MIPS_INTERFACE";
    case DT_MIPS_DYNSTR_ALIGN:
      return "DT_MIPS_DYNSTR_ALIGN";
    case DT_MIPS_INTERFACE_SIZE:
      return "DT_MIPS_INTERFACE_SIZE";
    case DT_MIPS_RLD_TEXT_RESOLVE_ADDR:
      return "DT_MIPS_RLD_TEXT_RESOLVE_ADDR";
    case DT_MIPS_PERF_SUFFIX:
      return "DT_MIPS_PERF_SUFFIX";
    case DT_MIPS_COMPACT_SIZE:
      return "DT_MIPS_COMPACT_SIZE";
    case DT_MIPS_GP_VALUE:
      return "DT_MIPS_GP_VALUE";
    case DT_MIPS_AUX_DYNAMIC:
      return "DT_MIPS_AUX_DYNAMIC";
    case DT_MIPS_PLTGOT:
      return "DT_MIPS_PLTGOT";
    case DT_MIPS_RWPLT:
      return "DT_MIPS_RWPLT";
    }
}

bfd_boolean
_bfd_riscv_elf_print_private_bfd_data (bfd *abfd, void *ptr)
{
  FILE *file = ptr;

  BFD_ASSERT (abfd != NULL && ptr != NULL);

  /* Print normal ELF private data.  */
  _bfd_elf_print_private_bfd_data (abfd, ptr);

  /* xgettext:c-format */
  fprintf (file, _("private flags = %lx:"), elf_elfheader (abfd)->e_flags);

  if (ABI_32_P (abfd))
    fprintf (file, _(" [abi=32]"));
  else if (ABI_64_P (abfd))
    fprintf (file, _(" [abi=64]"));
  else
    fprintf (file, _(" [no abi set]"));

  if ((elf_elfheader (abfd)->e_flags & EF_MIPS_ARCH) == E_MIPS_ARCH_32)
    fprintf (file, " [rv32]");
  else if ((elf_elfheader (abfd)->e_flags & EF_MIPS_ARCH) == E_MIPS_ARCH_64)
    fprintf (file, " [rv64]");
  else
    fprintf (file, _(" [unknown ISA]"));

  if (elf_elfheader (abfd)->e_flags & EF_MIPS_PIC)
    fprintf (file, " [PIC]");

  if (elf_elfheader (abfd)->e_flags & EF_MIPS_CPIC)
    fprintf (file, " [CPIC]");

  if (elf_elfheader (abfd)->e_flags & EF_MIPS_XGOT)
    fprintf (file, " [XGOT]");

  fputc ('\n', file);

  return TRUE;
}

const struct bfd_elf_special_section _bfd_riscv_elf_special_sections[] =
{
  { STRING_COMMA_LEN (".lit4"),   0, SHT_PROGBITS,   SHF_ALLOC + SHF_WRITE + SHF_MIPS_GPREL },
  { STRING_COMMA_LEN (".lit8"),   0, SHT_PROGBITS,   SHF_ALLOC + SHF_WRITE + SHF_MIPS_GPREL },
  { STRING_COMMA_LEN (".mdebug"), 0, SHT_MIPS_DEBUG, 0 },
  { STRING_COMMA_LEN (".sbss"),  -2, SHT_NOBITS,     SHF_ALLOC + SHF_WRITE + SHF_MIPS_GPREL },
  { STRING_COMMA_LEN (".sdata"), -2, SHT_PROGBITS,   SHF_ALLOC + SHF_WRITE + SHF_MIPS_GPREL },
  { STRING_COMMA_LEN (".ucode"),  0, SHT_MIPS_UCODE, 0 },
  { NULL,                     0,  0, 0,              0 }
};

/* Merge non visibility st_other attributes.  Ensure that the
   STO_OPTIONAL flag is copied into h->other, even if this is not a
   definiton of the symbol.  */
void
_bfd_riscv_elf_merge_symbol_attribute (struct elf_link_hash_entry *h,
				      const Elf_Internal_Sym *isym,
				      bfd_boolean definition,
				      bfd_boolean dynamic ATTRIBUTE_UNUSED)
{
  if ((isym->st_other & ~ELF_ST_VISIBILITY (-1)) != 0)
    {
      unsigned char other;

      other = (definition ? isym->st_other : h->other);
      other &= ~ELF_ST_VISIBILITY (-1);
      h->other = other | ELF_ST_VISIBILITY (h->other);
    }
}

bfd_boolean
_bfd_riscv_elf_common_definition (Elf_Internal_Sym *sym)
{
  return (sym->st_shndx == SHN_COMMON
	  || sym->st_shndx == SHN_MIPS_ACOMMON
	  || sym->st_shndx == SHN_MIPS_SCOMMON);
}

/* Return address for Ith PLT stub in section PLT, for relocation REL
   or (bfd_vma) -1 if it should not be included.  */

bfd_vma
_bfd_riscv_elf_plt_sym_val (bfd_vma i, const asection *plt,
			   const arelent *rel ATTRIBUTE_UNUSED)
{
  return (plt->vma
	  + RISCV_PLT0_ENTRY_INSNS * 4
	  + i * RISCV_PLT_ENTRY_INSNS * 4);
}

void
_bfd_riscv_post_process_headers (bfd *abfd, struct bfd_link_info *link_info)
{
  struct mips_elf_link_hash_table *htab;
  Elf_Internal_Ehdr *i_ehdrp;

  i_ehdrp = elf_elfheader (abfd);
  if (link_info)
    {
      htab = mips_elf_hash_table (link_info);
      BFD_ASSERT (htab != NULL);

      if (htab->use_plts_and_copy_relocs)
	i_ehdrp->e_ident[EI_ABIVERSION] = 1;
    }
}
