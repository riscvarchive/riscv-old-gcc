/* Prototypes of target machine for GNU compiler.  MIPS version.
   Copyright (C) 1989, 1990, 1991, 1992, 1993, 1994, 1995, 1996, 1997, 1998,
   1999, 2001, 2002, 2003, 2004, 2005, 2007, 2008, 2009, 2010, 2011
   Free Software Foundation, Inc.
   Contributed by A. Lichnewsky (lich@inria.inria.fr).
   Changed by Michael Meissner	(meissner@osf.org).
   64-bit r4000 support by Ian Lance Taylor (ian@cygnus.com) and
   Brendan Eich (brendan@microunity.com).

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

#ifndef GCC_MIPS_PROTOS_H
#define GCC_MIPS_PROTOS_H

enum mips_symbol_type {
  SYMBOL_ABSOLUTE,
  SYMBOL_GOT_DISP,
  SYMBOL_TLS,
  SYMBOL_TLS_LE,
  SYMBOL_TLS_IE
};
#define NUM_SYMBOL_TYPES (SYMBOL_TLS_IE + 1)

extern bool mips_symbolic_constant_p (rtx, enum mips_symbol_type *);
extern int riscv_regno_mode_ok_for_base_p (int, enum machine_mode, bool);
extern int riscv_address_insns (rtx, enum machine_mode, bool);
extern int mips_const_insns (rtx);
extern int mips_split_const_insns (rtx);
extern int mips_load_store_insns (rtx, rtx);
extern rtx mips_emit_move (rtx, rtx);
extern bool mips_split_symbol (rtx, rtx, enum machine_mode, rtx *);
extern rtx mips_unspec_address (rtx, enum mips_symbol_type);
extern void mips_move_integer (rtx, rtx, HOST_WIDE_INT);
extern bool mips_legitimize_move (enum machine_mode, rtx, rtx);
extern bool mips_legitimize_vector_move (enum machine_mode, rtx, rtx);

extern rtx mips_subword (rtx, bool);
extern bool mips_split_64bit_move_p (rtx, rtx);
extern void mips_split_doubleword_move (rtx, rtx);
extern const char *mips_output_move (rtx, rtx);
extern const char *mips_riscv_output_vector_move (enum machine_mode, rtx, rtx);
#ifdef RTX_CODE
extern void riscv_expand_scc (rtx *);
extern void riscv_expand_conditional_branch (rtx *);
#endif
extern rtx riscv_expand_call (bool, rtx, rtx, rtx);
extern void riscv_expand_fcc_reload (rtx, rtx, rtx);
extern void mips_set_return_address (rtx, rtx);
extern bool riscv_expand_block_move (rtx, rtx, rtx);
extern void riscv_expand_synci_loop (rtx, rtx);

extern void mips_init_cumulative_args (CUMULATIVE_ARGS *, tree);

extern bool riscv_expand_ext_as_unaligned_load (rtx, rtx, HOST_WIDE_INT,
					       HOST_WIDE_INT);
extern bool riscv_expand_ins_as_unaligned_store (rtx, rtx, HOST_WIDE_INT,
						HOST_WIDE_INT);
extern void mips_order_regs_for_local_alloc (void);

extern HOST_WIDE_INT mips_initial_elimination_offset (int, int);
extern rtx mips_return_addr (int, rtx);
extern void mips_emit_save_slot_move (rtx, rtx, rtx);
extern void riscv_expand_prologue (void);
extern void riscv_expand_epilogue (bool);
extern bool mips_can_use_return_insn (void);
extern rtx mips_function_value (const_tree, const_tree, enum machine_mode);

extern bool mips_cannot_change_mode_class (enum machine_mode,
					   enum machine_mode, enum reg_class);
extern enum reg_class mips_secondary_reload_class (enum reg_class,
						   enum machine_mode,
						   rtx, bool);
extern int mips_class_max_nregs (enum reg_class, enum machine_mode);

extern const char *mips_output_conditional_branch (rtx, rtx *, const char *,
						   const char *);
extern unsigned int riscv_hard_regno_nregs (int, enum machine_mode);

extern void irix_asm_output_align (FILE *, unsigned);
extern const char *current_section_name (void);
extern unsigned int current_section_flags (void);

extern void riscv_expand_vector_init (rtx, rtx);

extern bool riscv_size_ok_for_small_data_p (int size);

#endif /* ! GCC_MIPS_PROTOS_H */
