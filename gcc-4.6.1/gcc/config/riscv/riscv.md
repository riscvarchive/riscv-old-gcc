;;  Mips.md	     Machine Description for MIPS based processors
;;  Copyright (C) 1989, 1990, 1991, 1992, 1993, 1994, 1995, 1996, 1997, 1998,
;;  1999, 2000, 2001, 2002, 2003, 2004, 2005, 2006, 2007, 2008, 2009, 2010
;;  Free Software Foundation, Inc.
;;  Contributed by   A. Lichnewsky, lich@inria.inria.fr
;;  Changes by       Michael Meissner, meissner@osf.org
;;  64-bit r4000 support by Ian Lance Taylor, ian@cygnus.com, and
;;  Brendan Eich, brendan@microunity.com.

;; This file is part of GCC.

;; GCC is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; GCC is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GCC; see the file COPYING3.  If not see
;; <http://www.gnu.org/licenses/>.

(define_enum "processor" [
  rocket32
  rocket64
])

(define_c_enum "unspec" [
  ;; Unaligned accesses.
  UNSPEC_LOAD_LEFT
  UNSPEC_LOAD_RIGHT
  UNSPEC_STORE_LEFT
  UNSPEC_STORE_RIGHT

  ;; Floating-point moves.
  UNSPEC_LOAD_LOW
  UNSPEC_LOAD_HIGH
  UNSPEC_STORE_WORD
  UNSPEC_MFHC1
  UNSPEC_MTHC1

  ;; HI/LO moves.
  UNSPEC_MFHI
  UNSPEC_MTHI
  UNSPEC_SET_HILO

  ;; GP manipulation.
  UNSPEC_LOADGP
  UNSPEC_COPYGP
  UNSPEC_MOVE_GP
  UNSPEC_RESTORE_GP
  UNSPEC_EH_RETURN
  UNSPEC_SET_GOT_VERSION
  UNSPEC_UPDATE_GOT_VERSION

  ;; Symbolic accesses.
  UNSPEC_LOAD_CALL
  UNSPEC_LOAD_GOT
  UNSPEC_TLS_LDM

  ;; MIPS16 constant pools.
  UNSPEC_ALIGN
  UNSPEC_CONSTTABLE_INT
  UNSPEC_CONSTTABLE_FLOAT

  ;; Blockage and synchronisation.
  UNSPEC_BLOCKAGE
  UNSPEC_CLEAR_HAZARD
  UNSPEC_RDHWR
  UNSPEC_SYNC

  ;; Interrupt handling.
  UNSPEC_MIPS_CACHE
  UNSPEC_ERET
  UNSPEC_DERET
  UNSPEC_DI
  UNSPEC_EHB
  UNSPEC_RDPGPR
  UNSPEC_COP0

  ;; Used in a call expression in place of args_size.  It's present for PIC
  ;; indirect calls where it contains args_size and the function symbol.
  UNSPEC_CALL_ATTR

  ;; Fences
  UNSPEC_FENCE
  UNSPEC_FENCE_I
])

(define_constants
  [(RETURN_ADDR_REGNUM		1)
   (GOT_VERSION_REGNUM		79)
  ]
)

(include "predicates.md")
(include "constraints.md")

;; ....................
;;
;;	Attributes
;;
;; ....................

(define_attr "got" "unset,xgot_high,load"
  (const_string "unset"))

;; For jal instructions, this attribute is DIRECT when the target address
;; is symbolic and INDIRECT when it is a register.
(define_attr "jal" "unset,direct,indirect"
  (const_string "unset"))

;; Classification of moves, extensions and truncations.  Most values
;; are as for "type" (see below) but there are also the following
;; move-specific values:
;;
;; constN	move an N-constraint integer into a MIPS16 register
;; sll0		"sll DEST,SRC,0", which on 64-bit targets is guaranteed
;;		to produce a sign-extended DEST, even if SRC is not
;;		properly sign-extended
;; ext_ins	EXT, DEXT, INS or DINS instruction
;; andi		a single ANDI instruction
;; shift_shift	a shift left followed by a shift right
;;
;; This attribute is used to determine the instruction's length and
;; scheduling type.  For doubleword moves, the attribute always describes
;; the split instructions; in some cases, it is more appropriate for the
;; scheduling type to be "multi" instead.
(define_attr "move_type"
  "unknown,load,fpload,store,fpstore,mtc,mfc,mthilo,mfhilo,move,fmove,
   const,constN,signext,ext_ins,logical,arith,sll0,andi,shift_shift"
  (const_string "unknown"))

(define_attr "alu_type" "unknown,add,sub,not,nor,and,or,xor"
  (const_string "unknown"))

;; Main data type used by the insn
(define_attr "mode" "unknown,none,QI,HI,SI,DI,TI,SF,DF,TF,FPSW"
  (const_string "unknown"))

;; True if the main data type is twice the size of a word.
(define_attr "dword_mode" "no,yes"
  (cond [(and (eq_attr "mode" "DI,DF")
	      (eq (symbol_ref "TARGET_64BIT") (const_int 0)))
	 (const_string "yes")

	 (and (eq_attr "mode" "TI,TF")
	      (ne (symbol_ref "TARGET_64BIT") (const_int 0)))
	 (const_string "yes")]
	(const_string "no")))

;; Classification of each insn.
;; branch	conditional branch
;; jump		unconditional jump
;; call		unconditional call
;; load		load instruction(s)
;; fpload	floating point load
;; fpidxload    floating point indexed load
;; store	store instruction(s)
;; fpstore	floating point store
;; fpidxstore	floating point indexed store
;; prefetch	memory prefetch (register + offset)
;; prefetchx	memory indexed prefetch (register + register)
;; condmove	conditional moves
;; mtc		transfer to coprocessor
;; mfc		transfer from coprocessor
;; mthilo	transfer to hi/lo registers
;; mfhilo	transfer from hi/lo registers
;; const	load constant
;; arith	integer arithmetic instructions
;; logical      integer logical instructions
;; shift	integer shift instructions
;; slt		set less than instructions
;; signext      sign extend instructions
;; clz		the clz and clo instructions
;; pop		the pop instruction
;; trap		trap if instructions
;; imul		integer multiply 2 operands
;; imul3	integer multiply 3 operands
;; imul3nc	integer multiply 3 operands without clobbering HI/LO
;; imadd	integer multiply-add
;; idiv		integer divide 2 operands
;; idiv3	integer divide 3 operands
;; move		integer register move ({,D}ADD{,U} with rt = 0)
;; fmove	floating point register move
;; fadd		floating point add/subtract
;; fmul		floating point multiply
;; fmadd	floating point multiply-add
;; fdiv		floating point divide
;; frdiv	floating point reciprocal divide
;; frdiv1	floating point reciprocal divide step 1
;; frdiv2	floating point reciprocal divide step 2
;; fabs		floating point absolute value
;; fneg		floating point negation
;; fcmp		floating point compare
;; fcvt		floating point convert
;; fsqrt	floating point square root
;; frsqrt       floating point reciprocal square root
;; frsqrt1      floating point reciprocal square root step1
;; frsqrt2      floating point reciprocal square root step2
;; multi	multiword sequence (or user asm statements)
;; nop		no operation
;; ghost	an instruction that produces no real code
(define_attr "type"
  "unknown,branch,jump,call,load,fpload,fpidxload,store,fpstore,fpidxstore,
   prefetch,prefetchx,condmove,mtc,mfc,mthilo,mfhilo,const,arith,logical,
   shift,slt,signext,clz,pop,trap,imul,imul3,imul3nc,imadd,idiv,idiv3,move,
   fmove,fadd,fmul,fmadd,fdiv,frdiv,frdiv1,frdiv2,fabs,fneg,fcmp,fcvt,fsqrt,
   frsqrt,frsqrt1,frsqrt2,multi,nop,ghost"
  (cond [(eq_attr "jal" "!unset") (const_string "call")
	 (eq_attr "got" "load") (const_string "load")

	 (eq_attr "alu_type" "add,sub") (const_string "arith")

	 (eq_attr "alu_type" "not,nor,and,or,xor") (const_string "logical")

	 ;; If a doubleword move uses these expensive instructions,
	 ;; it is usually better to schedule them in the same way
	 ;; as the singleword form, rather than as "multi".
	 (eq_attr "move_type" "load") (const_string "load")
	 (eq_attr "move_type" "fpload") (const_string "fpload")
	 (eq_attr "move_type" "store") (const_string "store")
	 (eq_attr "move_type" "fpstore") (const_string "fpstore")
	 (eq_attr "move_type" "mtc") (const_string "mtc")
	 (eq_attr "move_type" "mfc") (const_string "mfc")
	 (eq_attr "move_type" "mthilo") (const_string "mthilo")
	 (eq_attr "move_type" "mfhilo") (const_string "mfhilo")

	 ;; These types of move are always single insns.
	 (eq_attr "move_type" "fmove") (const_string "fmove")
	 (eq_attr "move_type" "signext") (const_string "signext")
	 (eq_attr "move_type" "ext_ins") (const_string "arith")
	 (eq_attr "move_type" "arith") (const_string "arith")
	 (eq_attr "move_type" "logical") (const_string "logical")
	 (eq_attr "move_type" "sll0") (const_string "shift")
	 (eq_attr "move_type" "andi") (const_string "logical")

	 ;; These types of move are always split.
	 (eq_attr "move_type" "constN,shift_shift")
	   (const_string "multi")

	 ;; These types of move are split for doubleword modes only.
	 (and (eq_attr "move_type" "move,const")
	      (eq_attr "dword_mode" "yes"))
	   (const_string "multi")
	 (eq_attr "move_type" "move") (const_string "move")
	 (eq_attr "move_type" "const") (const_string "const")]
	(const_string "unknown")))

;; Mode for conversion types (fcvt)
;; I2S          integer to float single (SI/DI to SF)
;; I2D          integer to float double (SI/DI to DF)
;; S2I          float to integer (SF to SI/DI)
;; D2I          float to integer (DF to SI/DI)
;; D2S          double to float single
;; S2D          float single to double

(define_attr "cnv_mode" "unknown,I2S,I2D,S2I,D2I,D2S,S2D" 
  (const_string "unknown"))

;; Attributes describing a sync loop.  These loops have the form:
;;
;;       if (RELEASE_BARRIER == YES) sync
;;    1: OLDVAL = *MEM
;;       if ((OLDVAL & INCLUSIVE_MASK) != REQUIRED_OLDVAL) goto 2
;;       $TMP1 = OLDVAL & EXCLUSIVE_MASK
;;       $TMP2 = INSN1 (OLDVAL, INSN1_OP2)
;;       $TMP3 = INSN2 ($TMP2, INCLUSIVE_MASK)
;;       $AT |= $TMP1 | $TMP3
;;       if (!commit (*MEM = $AT)) goto 1.
;;         if (INSN1 != MOVE && INSN1 != LI) NEWVAL = $TMP3 [delay slot]
;;       sync
;;    2:
;;
;; where "$" values are temporaries and where the other values are
;; specified by the attributes below.  Values are specified as operand
;; numbers and insns are specified as enums.  If no operand number is
;; specified, the following values are used instead:
;;
;;    - OLDVAL: $AT
;;    - NEWVAL: $AT
;;    - INCLUSIVE_MASK: -1
;;    - REQUIRED_OLDVAL: OLDVAL & INCLUSIVE_MASK
;;    - EXCLUSIVE_MASK: 0
;;
;; MEM and INSN1_OP2 are required.
;;
;; Ideally, the operand attributes would be integers, with -1 meaning "none",
;; but the gen* programs don't yet support that.
(define_attr "sync_mem" "none,0,1,2,3,4,5" (const_string "none"))
(define_attr "sync_oldval" "none,0,1,2,3,4,5" (const_string "none"))
(define_attr "sync_newval" "none,0,1,2,3,4,5" (const_string "none"))
(define_attr "sync_inclusive_mask" "none,0,1,2,3,4,5" (const_string "none"))
(define_attr "sync_exclusive_mask" "none,0,1,2,3,4,5" (const_string "none"))
(define_attr "sync_required_oldval" "none,0,1,2,3,4,5" (const_string "none"))
(define_attr "sync_insn1_op2" "none,0,1,2,3,4,5" (const_string "none"))
(define_attr "sync_insn1" "move,li,add,addi,sub,and,andi,or,ori,xor,xori"
  (const_string "move"))
(define_attr "sync_insn2" "nop,and,xor,not"
  (const_string "nop"))
(define_attr "sync_release_barrier" "yes,no"
  (const_string "yes"))

;; Length of instruction in bytes.
(define_attr "length" ""
   (cond [
	  ;; Direct branch instructions have a range of [-0x1000,0xffc],
	  ;; relative to the address of the delay slot.  If a branch is
	  ;; outside this range, convert a branch like:
	  ;;
	  ;;	bne	r1,r2,target
	  ;;
	  ;; to:
	  ;;
	  ;;	beq	r1,r2,1f
	  ;;  j target
	  ;; 1:
	  ;;
	  (eq_attr "type" "branch")
	  (if_then_else (and (le (minus (match_dup 0) (pc)) (const_int 4088))
				  (le (minus (pc) (match_dup 0)) (const_int 4092)))
	  (const_int 4)
	  (const_int 8))

	  ;; "Ghost" instructions occupy no space.
	  (eq_attr "type" "ghost")
	  (const_int 0)

	  (eq_attr "got" "load")
		(const_int 4)
	  (eq_attr "got" "xgot_high")
	  (const_int 8)

	  ;; SHIFT_SHIFTs are decomposed into two separate instructions.
	  ;; They are extended instructions on MIPS16 targets.
	  (eq_attr "move_type" "shift_shift")
		(const_int 8)

	  ;; Check for doubleword moves that are decomposed into two
	  ;; instructions.
	  (and (eq_attr "move_type" "mtc,mfc,mthilo,mfhilo,move")
	       (eq_attr "dword_mode" "yes"))
	  (const_int 8)

	  ;; Doubleword CONST{,N} moves are split into two word
	  ;; CONST{,N} moves.
	  (and (eq_attr "move_type" "const,constN")
	       (eq_attr "dword_mode" "yes"))
	  (symbol_ref "mips_split_const_insns (operands[1]) * 4")

	  ;; Otherwise, constants, loads and stores are handled by external
	  ;; routines.
	  (eq_attr "move_type" "const,constN")
	  (symbol_ref "mips_const_insns (operands[1]) * 4")
	  (eq_attr "move_type" "load,fpload")
	  (symbol_ref "mips_load_store_insns (operands[1], insn) * 4")
	  (eq_attr "move_type" "store,fpstore")
	  (symbol_ref "mips_load_store_insns (operands[0], insn) * 4")
	  ] (const_int 4)))

;; Attribute describing the processor.
(define_enum_attr "cpu" "processor"
  (const (symbol_ref "mips_tune")))

;; Describe a user's asm statement.
(define_asm_attributes
  [(set_attr "type" "multi")])

;; This mode iterator allows 32-bit and 64-bit GPR patterns to be generated
;; from the same template.
(define_mode_iterator GPR [SI (DI "TARGET_64BIT")])

;; A copy of GPR that can be used when a pattern has two independent
;; modes.
(define_mode_iterator GPR2 [SI (DI "TARGET_64BIT")])

;; This mode iterator allows :P to be used for patterns that operate on
;; pointer-sized quantities.  Exactly one of the two alternatives will match.
(define_mode_iterator P [(SI "Pmode == SImode") (DI "Pmode == DImode")])

;; 32-bit integer moves for which we provide move patterns.
(define_mode_iterator IMOVE32 [SI])

;; 64-bit modes for which we provide move patterns.
(define_mode_iterator MOVE64 [DI DF])

;; 128-bit modes for which we provide move patterns on 64-bit targets.
(define_mode_iterator MOVE128 [TI TF])

;; This mode iterator allows the QI and HI extension patterns to be
;; defined from the same template.
(define_mode_iterator SHORT [QI HI])

;; Likewise the 64-bit truncate-and-shift patterns.
(define_mode_iterator SUBDI [QI HI SI])

;; This mode iterator allows :ANYF to be used wherever a scalar or vector
;; floating-point mode is allowed.
(define_mode_iterator ANYF [(SF "TARGET_HARD_FLOAT")
			    (DF "TARGET_HARD_FLOAT")])

;; Like ANYF, but only applies to scalar modes.
(define_mode_iterator SCALARF [(SF "TARGET_HARD_FLOAT")
			       (DF "TARGET_HARD_FLOAT")])

;; A floating-point mode for which moves involving FPRs may need to be split.
(define_mode_iterator SPLITF
  [(DF "!TARGET_64BIT")
   (DI "!TARGET_64BIT")
   (TF "TARGET_64BIT")])

;; This attribute gives the length suffix for a sign- or zero-extension
;; instruction.
(define_mode_attr size [(QI "b") (HI "h")])

;; This attributes gives the mode mask of a SHORT.
(define_mode_attr mask [(QI "0x00ff") (HI "0xffff")])

;; Mode attributes for GPR loads.
(define_mode_attr load [(SI "lw") (DI "ld")])
;; Instruction names for stores.
(define_mode_attr store [(QI "sb") (HI "sh") (SI "sw") (DI "sd")])

;; This attribute gives the best constraint to use for registers of
;; a given mode.
(define_mode_attr reg [(SI "d") (DI "d") (CC "z")])

;; This attribute gives the format suffix for floating-point operations.
(define_mode_attr fmt [(SF "s") (DF "d") (V2SF "ps")])

;; This attribute gives the upper-case mode name for one unit of a
;; floating-point mode.
(define_mode_attr UNITMODE [(SF "SF") (DF "DF") (V2SF "SF")])

;; This attribute gives the integer mode that has the same size as a
;; fixed-point mode.
(define_mode_attr IMODE [(QQ "QI") (HQ "HI") (SQ "SI") (DQ "DI")
			 (UQQ "QI") (UHQ "HI") (USQ "SI") (UDQ "DI")
			 (HA "HI") (SA "SI") (DA "DI")
			 (UHA "HI") (USA "SI") (UDA "DI")
			 (V4UQQ "SI") (V2UHQ "SI") (V2UHA "SI")
			 (V2HQ "SI") (V2HA "SI")])

;; This attribute gives the integer mode that has half the size of
;; the controlling mode.
(define_mode_attr HALFMODE [(DF "SI") (DI "SI") (V2SF "SI")
			    (V2SI "SI") (V4HI "SI") (V8QI "SI")
			    (TF "DI")])

;; This code iterator allows signed and unsigned widening multiplications
;; to use the same template.
(define_code_iterator any_extend [sign_extend zero_extend])

;; This code iterator allows the two right shift instructions to be
;; generated from the same template.
(define_code_iterator any_shiftrt [ashiftrt lshiftrt])

;; This code iterator allows the three shift instructions to be generated
;; from the same template.
(define_code_iterator any_shift [ashift ashiftrt lshiftrt])

;; This code iterator allows unsigned and signed division to be generated
;; from the same template.
(define_code_iterator any_div [div udiv])

;; This code iterator allows unsigned and signed modulus to be generated
;; from the same template.
(define_code_iterator any_mod [mod umod])

;; Equality operators.
(define_code_iterator equality_op [eq ne])

;; These code iterators allow the signed and unsigned scc operations to use
;; the same template.
(define_code_iterator any_gt [gt gtu])
(define_code_iterator any_ge [ge geu])
(define_code_iterator any_lt [lt ltu])
(define_code_iterator any_le [le leu])

;; <u> expands to an empty string when doing a signed operation and
;; "u" when doing an unsigned operation.
(define_code_attr u [(sign_extend "") (zero_extend "u")
		     (div "") (udiv "u")
		     (mod "") (umod "u")
		     (gt "") (gtu "u")
		     (ge "") (geu "u")
		     (lt "") (ltu "u")
		     (le "") (leu "u")])

;; <su> is like <u>, but the signed form expands to "s" rather than "".
(define_code_attr su [(sign_extend "s") (zero_extend "u")])

;; <optab> expands to the name of the optab for a particular code.
(define_code_attr optab [(ashift "ashl")
			 (ashiftrt "ashr")
			 (lshiftrt "lshr")
			 (ior "ior")
			 (xor "xor")
			 (and "and")
			 (plus "add")
			 (minus "sub")])

;; <insn> expands to the name of the insn that implements a particular code.
(define_code_attr insn [(ashift "sll")
			(ashiftrt "sra")
			(lshiftrt "srl")
			(ior "or")
			(xor "xor")
			(and "and")
			(plus "add")
			(minus "sub")])

;; The value of the bit when the branch is taken for branch_bit patterns.
;; Comparison is always against zero so this depends on the operator.
(define_code_attr bbv [(eq "0") (ne "1")])

;; This is the inverse value of bbv.
(define_code_attr bbinv [(eq "1") (ne "0")])

;; Pipeline descriptions.
;;
;; generic.md provides a fallback for processors without a specific
;; pipeline description.  It is derived from the old define_function_unit
;; version and uses the "alu" and "imuldiv" units declared below.
;;
;; Some of the processor-specific files are also derived from old
;; define_function_unit descriptions and simply override the parts of
;; generic.md that don't apply.  The other processor-specific files
;; are self-contained.
(define_automaton "alu,imuldiv")

(define_cpu_unit "alu" "alu")
(define_cpu_unit "imuldiv" "imuldiv")

;; Ghost instructions produce no real code and introduce no hazards.
;; They exist purely to express an effect on dataflow.
(define_insn_reservation "ghost" 0
  (eq_attr "type" "ghost")
  "nothing")

(include "generic.md")

;;
;;  ....................
;;
;;	ADDITION
;;
;;  ....................
;;

(define_insn "add<mode>3"
  [(set (match_operand:ANYF 0 "register_operand" "=f")
	(plus:ANYF (match_operand:ANYF 1 "register_operand" "f")
		   (match_operand:ANYF 2 "register_operand" "f")))]
  ""
  "fadd.<fmt>\t%0,%1,%2"
  [(set_attr "type" "fadd")
   (set_attr "mode" "<UNITMODE>")])

(define_expand "add<mode>3"
  [(set (match_operand:GPR 0 "register_operand")
	(plus:GPR (match_operand:GPR 1 "register_operand")
		  (match_operand:GPR 2 "arith_operand")))]
  "")

(define_insn "*addsi3"
  [(set (match_operand:SI 0 "register_operand" "=d,d")
	(plus:SI (match_operand:GPR 1 "register_operand" "d,d")
		  (match_operand:GPR2 2 "arith_operand" "d,Q")))]
  ""
  { return TARGET_64BIT ? "addw\t%0,%1,%2" : "add\t%0,%1,%2"; }
  [(set_attr "type" "arith")
   (set_attr "mode" "SI")])

(define_insn "*adddi3"
  [(set (match_operand:DI 0 "register_operand" "=d,d")
	(plus:DI (match_operand:DI 1 "register_operand" "d,d")
		  (match_operand:DI 2 "arith_operand" "d,Q")))]
  "TARGET_64BIT"
  "add\t%0,%1,%2"
  [(set_attr "type" "arith")
   (set_attr "mode" "DI")])

(define_insn "*addsi3_extended"
  [(set (match_operand:DI 0 "register_operand" "=d,d")
	(sign_extend:DI
	     (plus:SI (match_operand:SI 1 "register_operand" "d,d")
		      (match_operand:SI 2 "arith_operand" "d,Q"))))]
  "TARGET_64BIT"
  "addw\t%0,%1,%2"
  [(set_attr "type" "arith")
   (set_attr "mode" "SI")])

(define_insn "*adddisi3"
  [(set (match_operand:SI 0 "register_operand" "=d,d")
	     (plus:SI (truncate:SI (match_operand:DI 1 "register_operand" "d,d"))
		      (truncate:SI (match_operand:DI 2 "arith_operand" "d,Q"))))]
  "TARGET_64BIT"
  "addw\t%0,%1,%2"
  [(set_attr "type" "arith")
   (set_attr "mode" "SI")])

(define_insn "*adddi3_truncsi"
  [(set (match_operand:SI 0 "register_operand" "=d,d")
          (truncate:SI
	     (plus:DI (match_operand:DI 1 "register_operand" "d,d")
		      (match_operand:DI 2 "arith_operand" "d,Q"))))]
  "TARGET_64BIT"
  "addw\t%0,%1,%2"
  [(set_attr "type" "arith")
   (set_attr "mode" "SI")])

;; this is used for generating HImode constants.
;; see comments in mips_move_integer().
(define_insn "bullshit_addsihi"
  [(set (match_operand:HI 0 "register_operand" "=d")
	(plus:HI (match_operand:SI 1 "register_operand" "d")
		       (match_operand:SI 2 "const_arith_operand" "I")))]
  "0"
{
  return TARGET_64BIT ? "addw\t%0,%1,%2" : "add\t%0,%1,%2";
}
  [(set_attr "type" "arith")
   (set_attr "mode" "SI")])

;;
;;  ....................
;;
;;	SUBTRACTION
;;
;;  ....................
;;

(define_insn "sub<mode>3"
  [(set (match_operand:ANYF 0 "register_operand" "=f")
	(minus:ANYF (match_operand:ANYF 1 "register_operand" "f")
		    (match_operand:ANYF 2 "register_operand" "f")))]
  ""
  "fsub.<fmt>\t%0,%1,%2"
  [(set_attr "type" "fadd")
   (set_attr "mode" "<UNITMODE>")])

(define_insn "subdi3"
  [(set (match_operand:DI 0 "register_operand" "=d")
	(minus:DI (match_operand:DI 1 "register_operand" "d")
		   (match_operand:DI 2 "register_operand" "d")))]
  "TARGET_64BIT"
  "sub\t%0,%1,%2"
  [(set_attr "type" "arith")
   (set_attr "mode" "DI")])

(define_insn "subsi3"
  [(set (match_operand:SI 0 "register_operand" "=d")
	(minus:SI (match_operand:SI 1 "register_operand" "d")
		   (match_operand:SI 2 "register_operand" "d")))]
  ""
  { return TARGET_64BIT ? "subw\t%0,%1,%2" : "sub\t%0,%1,%2"; }
  [(set_attr "type" "arith")
   (set_attr "mode" "SI")])

(define_insn "*subsi3_extended"
  [(set (match_operand:DI 0 "register_operand" "=d")
	(sign_extend:DI
	    (minus:SI (match_operand:SI 1 "register_operand" "d")
		      (match_operand:SI 2 "register_operand" "d"))))]
  "TARGET_64BIT"
  "subw\t%0,%1,%2"
  [(set_attr "type" "arith")
   (set_attr "mode" "DI")])

(define_insn "*subdisi3"
  [(set (match_operand:SI 0 "register_operand" "=d,d")
	     (minus:SI (truncate:SI (match_operand:DI 1 "register_operand" "d,d"))
		      (truncate:SI (match_operand:DI 2 "arith_operand" "d,Q"))))]
  "TARGET_64BIT"
  "subw\t%0,%1,%2"
  [(set_attr "type" "arith")
   (set_attr "mode" "SI")])

(define_insn "*subdi3_truncsi"
  [(set (match_operand:SI 0 "register_operand" "=d,d")
          (truncate:SI
	     (minus:DI (match_operand:DI 1 "register_operand" "d,d")
		      (match_operand:DI 2 "arith_operand" "d,Q"))))]
  "TARGET_64BIT"
  "subw\t%0,%1,%2"
  [(set_attr "type" "arith")
   (set_attr "mode" "SI")])

;;
;;  ....................
;;
;;	MULTIPLICATION
;;
;;  ....................
;;

(define_insn "mul<mode>3"
  [(set (match_operand:SCALARF 0 "register_operand" "=f")
	(mult:SCALARF (match_operand:SCALARF 1 "register_operand" "f")
		      (match_operand:SCALARF 2 "register_operand" "f")))]
  ""
  "fmul.<fmt>\t%0,%1,%2"
  [(set_attr "type" "fmul")
   (set_attr "mode" "<UNITMODE>")])

(define_insn "mulsi3"
  [(set (match_operand:SI 0 "register_operand" "=d")
	(mult:SI (match_operand:SI 1 "register_operand" "d")
		  (match_operand:SI 2 "register_operand" "d")))]
  ""
  { return TARGET_64BIT ? "mulw\t%0,%1,%2" : "mul\t%0,%1,%2"; }
  [(set_attr "type" "imul3")
   (set_attr "mode" "SI")])

(define_insn "*muldisi3"
  [(set (match_operand:SI 0 "register_operand" "=d")
	     (mult:SI (truncate:SI (match_operand:DI 1 "register_operand" "d"))
		      (truncate:SI (match_operand:DI 2 "register_operand" "d"))))]
  "TARGET_64BIT"
  "mulw\t%0,%1,%2"
  [(set_attr "type" "imul3")
   (set_attr "mode" "SI")])

(define_insn "*muldi3_truncsi"
  [(set (match_operand:SI 0 "register_operand" "=d")
          (truncate:SI
	     (mult:DI (match_operand:DI 1 "register_operand" "d")
		      (match_operand:DI 2 "register_operand" "d"))))]
  "TARGET_64BIT"
  "mulw\t%0,%1,%2"
  [(set_attr "type" "imul3")
   (set_attr "mode" "SI")])

(define_insn "muldi3"
  [(set (match_operand:DI 0 "register_operand" "=d")
	(mult:DI (match_operand:DI 1 "register_operand" "d")
		  (match_operand:DI 2 "register_operand" "d")))]
  "TARGET_64BIT"
  "mul\t%0,%1,%2"
  [(set_attr "type" "imul3")
   (set_attr "mode" "DI")])

;;
;;  ........................
;;
;;	MULTIPLICATION HIGH-PART
;;
;;  ........................
;;


;; Using a clobber here is ghetto, but I'm not smart enough to do better. '
(define_insn_and_split "<u>mulditi3"
  [(set (match_operand:TI 0 "register_operand" "=d")
	(mult:TI (any_extend:TI
		   (match_operand:DI 1 "register_operand" "d"))
		 (any_extend:TI
		   (match_operand:DI 2 "register_operand" "d"))))
  (clobber (match_scratch:DI 3 "=d"))]
  "TARGET_64BIT"
  "#"
  "reload_completed"
  [
   (set (match_dup 3) (mult:DI (match_dup 1) (match_dup 2)))
   (set (match_dup 4) (truncate:DI
			(lshiftrt:TI
			  (mult:TI (any_extend:TI (match_dup 1))
				   (any_extend:TI (match_dup 2)))
			  (const_int 64))))
   (set (match_dup 5) (match_dup 3))
  ]
{
  operands[4] = mips_subword (operands[0], true);
  operands[5] = mips_subword (operands[0], false);
}
  )

(define_insn "<u>muldi3_highpart"
  [(set (match_operand:DI 0 "register_operand" "=d")
	(truncate:DI
	  (lshiftrt:TI
	    (mult:TI (any_extend:TI
		       (match_operand:DI 1 "register_operand" "d"))
		     (any_extend:TI
		       (match_operand:DI 2 "register_operand" "d")))
	    (const_int 64))))]
  "TARGET_64BIT"
  "mulh<u>\t%0,%1,%2"
  [(set_attr "type" "imul3")
   (set_attr "mode" "DI")])


(define_insn_and_split "usmulditi3"
  [(set (match_operand:TI 0 "register_operand" "=d")
	(mult:TI (zero_extend:TI
		   (match_operand:DI 1 "register_operand" "d"))
		 (sign_extend:TI
		   (match_operand:DI 2 "register_operand" "d"))))
  (clobber (match_scratch:DI 3 "=d"))]
  "TARGET_64BIT"
  "#"
  "reload_completed"
  [
   (set (match_dup 3) (mult:DI (match_dup 1) (match_dup 2)))
   (set (match_dup 4) (truncate:DI
			(lshiftrt:TI
			  (mult:TI (zero_extend:TI (match_dup 1))
				   (sign_extend:TI (match_dup 2)))
			  (const_int 64))))
   (set (match_dup 5) (match_dup 3))
  ]
{
  operands[4] = mips_subword (operands[0], true);
  operands[5] = mips_subword (operands[0], false);
}
  )

(define_insn "usmuldi3_highpart"
  [(set (match_operand:DI 0 "register_operand" "=d")
	(truncate:DI
	  (lshiftrt:TI
	    (mult:TI (zero_extend:TI
		       (match_operand:DI 1 "register_operand" "d"))
		     (sign_extend:TI
		       (match_operand:DI 2 "register_operand" "d")))
	    (const_int 64))))]
  "TARGET_64BIT"
  "mulhsu\t%0,%2,%1"
  [(set_attr "type" "imul3")
   (set_attr "mode" "DI")])

(define_insn_and_split "<u>mulsidi3"
  [(set (match_operand:DI 0 "register_operand" "=d")
	(mult:DI (any_extend:DI
		   (match_operand:SI 1 "register_operand" "d"))
		 (any_extend:DI
		   (match_operand:SI 2 "register_operand" "d"))))
  (clobber (match_scratch:SI 3 "=d"))]
  "!TARGET_64BIT"
  "#"
  "reload_completed"
  [
   (set (match_dup 3) (mult:SI (match_dup 1) (match_dup 2)))
   (set (match_dup 4) (truncate:SI
			(lshiftrt:DI
			  (mult:DI (any_extend:DI (match_dup 1))
				   (any_extend:DI (match_dup 2)))
			  (const_int 32))))
   (set (match_dup 5) (match_dup 3))
  ]
{
  operands[4] = mips_subword (operands[0], true);
  operands[5] = mips_subword (operands[0], false);
}
  )

(define_insn "<u>mulsi3_highpart"
  [(set (match_operand:SI 0 "register_operand" "=d")
	(truncate:SI
	  (lshiftrt:DI
	    (mult:DI (any_extend:DI
		       (match_operand:SI 1 "register_operand" "d"))
		     (any_extend:DI
		       (match_operand:SI 2 "register_operand" "d")))
	    (const_int 32))))]
  "!TARGET_64BIT"
  "mulh<u>\t%0,%1,%2"
  [(set_attr "type" "imul3")
   (set_attr "mode" "SI")])


(define_insn_and_split "usmulsidi3"
  [(set (match_operand:DI 0 "register_operand" "=d")
	(mult:DI (zero_extend:DI
		   (match_operand:SI 1 "register_operand" "d"))
		 (sign_extend:DI
		   (match_operand:SI 2 "register_operand" "d"))))
  (clobber (match_scratch:SI 3 "=d"))]
  "!TARGET_64BIT"
  "#"
  "reload_completed"
  [
   (set (match_dup 3) (mult:SI (match_dup 1) (match_dup 2)))
   (set (match_dup 4) (truncate:SI
			(lshiftrt:DI
			  (mult:DI (zero_extend:DI (match_dup 1))
				   (sign_extend:DI (match_dup 2)))
			  (const_int 32))))
   (set (match_dup 5) (match_dup 3))
  ]
{
  operands[4] = mips_subword (operands[0], true);
  operands[5] = mips_subword (operands[0], false);
}
  )

(define_insn "usmulsi3_highpart"
  [(set (match_operand:SI 0 "register_operand" "=d")
	(truncate:SI
	  (lshiftrt:DI
	    (mult:DI (zero_extend:DI
		       (match_operand:SI 1 "register_operand" "d"))
		     (sign_extend:DI
		       (match_operand:SI 2 "register_operand" "d")))
	    (const_int 32))))]
  "!TARGET_64BIT"
  "mulhsu\t%0,%2,%1"
  [(set_attr "type" "imul3")
   (set_attr "mode" "SI")])

;;
;;  ....................
;;
;;	DIVISION and REMAINDER
;;
;;  ....................
;;

(define_insn "<u>divsi3"
  [(set (match_operand:SI 0 "register_operand" "=d")
	(any_div:SI (match_operand:SI 1 "register_operand" "d")
		  (match_operand:SI 2 "register_operand" "d")))]
  ""
  { return TARGET_64BIT ? "div<u>w\t%0,%1,%2" : "div<u>\t%0,%1,%2"; }
  [(set_attr "type" "idiv3")
   (set_attr "mode" "SI")])

(define_insn "<u>divdi3"
  [(set (match_operand:DI 0 "register_operand" "=d")
	(any_div:DI (match_operand:DI 1 "register_operand" "d")
		  (match_operand:DI 2 "register_operand" "d")))]
  "TARGET_64BIT"
  "div<u>\t%0,%1,%2"
  [(set_attr "type" "idiv3")
   (set_attr "mode" "DI")])

(define_insn "<u>modsi3"
  [(set (match_operand:SI 0 "register_operand" "=d")
	(any_mod:SI (match_operand:SI 1 "register_operand" "d")
		  (match_operand:SI 2 "register_operand" "d")))]
  ""
  { return TARGET_64BIT ? "rem<u>w\t%0,%1,%2" : "rem<u>\t%0,%1,%2"; }
  [(set_attr "type" "idiv3")
   (set_attr "mode" "SI")])

(define_insn "<u>moddi3"
  [(set (match_operand:DI 0 "register_operand" "=d")
	(any_mod:DI (match_operand:DI 1 "register_operand" "d")
		  (match_operand:DI 2 "register_operand" "d")))]
  "TARGET_64BIT"
  "rem<u>\t%0,%1,%2"
  [(set_attr "type" "idiv3")
   (set_attr "mode" "DI")])

(define_insn "div<mode>3"
  [(set (match_operand:ANYF 0 "register_operand" "=f")
	(div:ANYF (match_operand:ANYF 1 "register_operand" "f")
		  (match_operand:ANYF 2 "register_operand" "f")))]
  ""
  "fdiv.<fmt>\t%0,%1,%2"
  [(set_attr "type" "fdiv")
   (set_attr "mode" "<UNITMODE>")])

;;
;;  ....................
;;
;;	SQUARE ROOT
;;
;;  ....................

(define_insn "sqrt<mode>2"
  [(set (match_operand:ANYF 0 "register_operand" "=f")
	(sqrt:ANYF (match_operand:ANYF 1 "register_operand" "f")))]
  ""
{
    return "fsqrt.<fmt>\t%0,%1";
}
  [(set_attr "type" "fsqrt")
   (set_attr "mode" "<UNITMODE>")
   (set (attr "length") (const_int 4))])

;; Floating point multiply accumulate instructions.

(define_insn "*madd4<mode>"
  [(set (match_operand:ANYF 0 "register_operand" "=f")
	(plus:ANYF (mult:ANYF (match_operand:ANYF 1 "register_operand" "f")
			      (match_operand:ANYF 2 "register_operand" "f"))
		   (match_operand:ANYF 3 "register_operand" "f")))]
  "TARGET_HARD_FLOAT"
  "fmadd.<fmt>\t%0,%1,%2,%3"
  [(set_attr "type" "fmadd")
   (set_attr "mode" "<UNITMODE>")])

(define_insn "*msub4<mode>"
  [(set (match_operand:ANYF 0 "register_operand" "=f")
	(minus:ANYF (mult:ANYF (match_operand:ANYF 1 "register_operand" "f")
			       (match_operand:ANYF 2 "register_operand" "f"))
		    (match_operand:ANYF 3 "register_operand" "f")))]
  "TARGET_HARD_FLOAT"
  "fmsub.<fmt>\t%0,%1,%2,%3"
  [(set_attr "type" "fmadd")
   (set_attr "mode" "<UNITMODE>")])

(define_insn "*nmadd4<mode>"
  [(set (match_operand:ANYF 0 "register_operand" "=f")
	(neg:ANYF (plus:ANYF
		   (mult:ANYF (match_operand:ANYF 1 "register_operand" "f")
			      (match_operand:ANYF 2 "register_operand" "f"))
		   (match_operand:ANYF 3 "register_operand" "f"))))]
  "TARGET_HARD_FLOAT"
  "fnmadd.<fmt>\t%0,%1,%2,%3"
  [(set_attr "type" "fmadd")
   (set_attr "mode" "<UNITMODE>")])

(define_insn "*nmadd4<mode>_fastmath"
  [(set (match_operand:ANYF 0 "register_operand" "=f")
	(minus:ANYF
	 (mult:ANYF (neg:ANYF (match_operand:ANYF 1 "register_operand" "f"))
		    (match_operand:ANYF 2 "register_operand" "f"))
	 (match_operand:ANYF 3 "register_operand" "f")))]
  "TARGET_HARD_FLOAT"
  "fnmadd.<fmt>\t%0,%1,%2,%3"
  [(set_attr "type" "fmadd")
   (set_attr "mode" "<UNITMODE>")])

(define_insn "*nmsub4<mode>"
  [(set (match_operand:ANYF 0 "register_operand" "=f")
	(neg:ANYF (minus:ANYF
		   (mult:ANYF (match_operand:ANYF 2 "register_operand" "f")
			      (match_operand:ANYF 3 "register_operand" "f"))
		   (match_operand:ANYF 1 "register_operand" "f"))))]
  "TARGET_HARD_FLOAT"
  "fnmsub.<fmt>\t%0,%1,%2,%3"
  [(set_attr "type" "fmadd")
   (set_attr "mode" "<UNITMODE>")])

(define_insn "*nmsub4<mode>_fastmath"
  [(set (match_operand:ANYF 0 "register_operand" "=f")
	(minus:ANYF
	 (match_operand:ANYF 1 "register_operand" "f")
	 (mult:ANYF (match_operand:ANYF 2 "register_operand" "f")
		    (match_operand:ANYF 3 "register_operand" "f"))))]
  "TARGET_HARD_FLOAT"
  "fnmsub.<fmt>\t%0,%1,%2,%3"
  [(set_attr "type" "fmadd")
   (set_attr "mode" "<UNITMODE>")])

;;
;;  ....................
;;
;;	ABSOLUTE VALUE
;;
;;  ....................

(define_insn "abs<mode>2"
  [(set (match_operand:ANYF 0 "register_operand" "=f")
	(abs:ANYF (match_operand:ANYF 1 "register_operand" "f")))]
  ""
  "fsgnjx.<fmt>\t%0,%1,%1"
  [(set_attr "type" "fabs")
   (set_attr "mode" "<UNITMODE>")])


;;
;;  ....................
;;
;;	MIN/MAX
;;
;;  ....................

(define_insn "smin<mode>3"
  [(set (match_operand:ANYF 0 "register_operand" "=f")
		   (smin:ANYF (match_operand:ANYF 1 "register_operand" "f")
			    (match_operand:ANYF 2 "register_operand" "f")))]
  ""
  "fmin.<fmt>\t%0,%1,%2"
  [(set_attr "type" "fabs")
   (set_attr "mode" "<UNITMODE>")])

(define_insn "smax<mode>3"
  [(set (match_operand:ANYF 0 "register_operand" "=f")
		   (smax:ANYF (match_operand:ANYF 1 "register_operand" "f")
			    (match_operand:ANYF 2 "register_operand" "f")))]
  ""
  "fmax.<fmt>\t%0,%1,%2"
  [(set_attr "type" "fabs")
   (set_attr "mode" "<UNITMODE>")])


;;
;;  ....................
;;
;;	NEGATION and ONE'S COMPLEMENT '
;;
;;  ....................

(define_insn "negsi2"
  [(set (match_operand:SI 0 "register_operand" "=d")
	(neg:SI (match_operand:SI 1 "register_operand" "d")))]
  ""
  { return TARGET_64BIT ? "subw\t%0,zero,%1" : "sub\t%0,zero,%1"; }
  [(set_attr "type"	"arith")
   (set_attr "mode"	"SI")])

(define_insn "negdi2"
  [(set (match_operand:DI 0 "register_operand" "=d")
	(neg:DI (match_operand:DI 1 "register_operand" "d")))]
  "TARGET_64BIT"
  "sub\t%0,zero,%1"
  [(set_attr "type"	"arith")
   (set_attr "mode"	"DI")])

(define_insn "neg<mode>2"
  [(set (match_operand:ANYF 0 "register_operand" "=f")
	(neg:ANYF (match_operand:ANYF 1 "register_operand" "f")))]
  ""
  "fsgnjn.<fmt>\t%0,%1,%1"
  [(set_attr "type" "fneg")
   (set_attr "mode" "<UNITMODE>")])

(define_insn "one_cmpl<mode>2"
  [(set (match_operand:GPR 0 "register_operand" "=d")
	(not:GPR (match_operand:GPR 1 "register_operand" "d")))]
  ""
  "xor\t%0,%1,-1"
  [(set_attr "type" "logical")
   (set_attr "mode" "<MODE>")])

;;
;;  ....................
;;
;;	LOGICAL
;;
;;  ....................
;;

(define_insn "and<mode>3"
  [(set (match_operand:GPR 0 "register_operand" "=d,d")
	(and:GPR (match_operand:GPR 1 "register_operand" "%d,d")
		 (match_operand:GPR 2 "arith_operand" "d,Q")))]
  ""
  "and\t%0,%1,%2"
  [(set_attr "type" "logical")
   (set_attr "mode" "<MODE>")])

(define_insn "ior<mode>3"
  [(set (match_operand:GPR 0 "register_operand" "=d,d")
	(ior:GPR (match_operand:GPR 1 "register_operand" "%d,d")
		 (match_operand:GPR 2 "arith_operand" "d,Q")))]
  ""
  "or\t%0,%1,%2"
  [(set_attr "type" "logical")
   (set_attr "mode" "<MODE>")])

;; this is used for generating HImode constants.
(define_insn "iorhi3"
  [(set (match_operand:HI 0 "register_operand" "=d,d")
	(ior:HI (match_operand:HI 1 "register_operand" "%d,d")
		 (match_operand:HI 2 "arith_operand" "d,Q")))]
  ""
  "or\t%0,%1,%2"
  [(set_attr "type" "logical")
   (set_attr "mode" "HI")])

(define_insn "xor<mode>3"
  [(set (match_operand:GPR 0 "register_operand" "=d,d")
	(xor:GPR (match_operand:GPR 1 "register_operand" "%d,d")
		 (match_operand:GPR 2 "arith_operand" "d,Q")))]
  ""
  "xor\t%0,%1,%2"
  [(set_attr "type" "logical")
   (set_attr "mode" "<MODE>")])

;;
;;  ....................
;;
;;	TRUNCATION
;;
;;  ....................



(define_insn "truncdfsf2"
  [(set (match_operand:SF 0 "register_operand" "=f")
	(float_truncate:SF (match_operand:DF 1 "register_operand" "f")))]
  "TARGET_HARD_FLOAT"
  "fcvt.s.d\t%0,%1"
  [(set_attr "type"	"fcvt")
   (set_attr "cnv_mode"	"D2S")   
   (set_attr "mode"	"SF")])

;; Integer truncation patterns.  Truncating SImode values to smaller
;; modes is a no-op, as it is for most other GCC ports.  Truncating
;; DImode values to SImode is not a no-op for TARGET_64BIT since we
;; need to make sure that the lower 32 bits are properly sign-extended
;; (see TRULY_NOOP_TRUNCATION).  Truncating DImode values into modes
;; smaller than SImode is equivalent to two separate truncations:
;;
;;                        A       B
;;    DI ---> HI  ==  DI ---> SI ---> HI
;;    DI ---> QI  ==  DI ---> SI ---> QI
;;
;; Step A needs a real instruction but step B does not.

(define_insn "truncdisi2"
  [(set (match_operand:SI 0 "nonimmediate_operand" "=d,m")
        (truncate:SI (match_operand:DI 1 "register_operand" "d,d")))]
  "TARGET_64BIT"
  "@
    sllw\t%0,%1,0
    sw\t%1,%0"
  [(set_attr "move_type" "sll0,store")
   (set_attr "mode" "SI")])

(define_insn "truncdihi2"
  [(set (match_operand:HI 0 "nonimmediate_operand" "=d,m")
        (truncate:HI (match_operand:DI 1 "register_operand" "d,d")))]
  "TARGET_64BIT"
  "@
    sllw\t%0,%1,0
    sh\t%1,%0"
  [(set_attr "move_type" "sll0,store")
   (set_attr "mode" "SI")])

(define_insn "truncdiqi2"
  [(set (match_operand:QI 0 "nonimmediate_operand" "=d,m")
        (truncate:QI (match_operand:DI 1 "register_operand" "d,d")))]
  "TARGET_64BIT"
  "@
    sllw\t%0,%1,0
    sb\t%1,%0"
  [(set_attr "move_type" "sll0,store")
   (set_attr "mode" "SI")])

;; Combiner patterns to optimize shift/truncate combinations.

(define_insn "*ashr_trunc<mode>"
  [(set (match_operand:SUBDI 0 "register_operand" "=d")
        (truncate:SUBDI
	  (ashiftrt:DI (match_operand:DI 1 "register_operand" "d")
		       (match_operand:DI 2 "const_arith_operand" ""))))]
  "TARGET_64BIT && IN_RANGE (INTVAL (operands[2]), 32, 63)"
  "sra\t%0,%1,%2"
  [(set_attr "type" "shift")
   (set_attr "mode" "<MODE>")])

(define_insn "*lshr32_trunc<mode>"
  [(set (match_operand:SUBDI 0 "register_operand" "=d")
        (truncate:SUBDI
	  (lshiftrt:DI (match_operand:DI 1 "register_operand" "d")
		       (const_int 32))))]
  "TARGET_64BIT"
  "sra\t%0,%1,32"
  [(set_attr "type" "shift")
   (set_attr "mode" "<MODE>")])

;; Combiner patterns for truncate/sign_extend combinations.  The SI versions
;; use the shift/truncate patterns above.

(define_insn_and_split "*extenddi_truncate<mode>"
  [(set (match_operand:DI 0 "register_operand" "=d")
	(sign_extend:DI
	    (truncate:SHORT (match_operand:DI 1 "register_operand" "d"))))]
  "TARGET_64BIT"
  "#"
  "&& reload_completed"
  [(set (match_dup 2)
	(ashift:DI (match_dup 1)
		   (match_dup 3)))
   (set (match_dup 0)
	(ashiftrt:DI (match_dup 2)
		     (match_dup 3)))]
{
  operands[2] = gen_lowpart (DImode, operands[0]);
  operands[3] = GEN_INT (BITS_PER_WORD - GET_MODE_BITSIZE (<MODE>mode));
})

(define_insn_and_split "*extendsi_truncate<mode>"
  [(set (match_operand:SI 0 "register_operand" "=d")
	(sign_extend:SI
	    (truncate:SHORT (match_operand:DI 1 "register_operand" "d"))))]
  "TARGET_64BIT"
  "#"
  "&& reload_completed"
  [(set (match_dup 2)
	(ashift:DI (match_dup 1)
		   (match_dup 3)))
   (set (match_dup 0)
	(truncate:SI (ashiftrt:DI (match_dup 2)
				  (match_dup 3))))]
{
  operands[2] = gen_lowpart (DImode, operands[0]);
  operands[3] = GEN_INT (BITS_PER_WORD - GET_MODE_BITSIZE (<MODE>mode));
})

;; Combiner patterns to optimize truncate/zero_extend combinations.

(define_insn "*zero_extend<mode>_truncqi"
  [(set (match_operand:GPR 0 "register_operand" "=d")
        (zero_extend:GPR
	    (truncate:QI (match_operand:DI 1 "register_operand" "d"))))]
  "TARGET_64BIT"
  "andi\t%0,%1,0xff"
  [(set_attr "type" "logical")
   (set_attr "mode" "<MODE>")])

(define_insn ""
  [(set (match_operand:HI 0 "register_operand" "=d")
        (zero_extend:HI
	    (truncate:QI (match_operand:DI 1 "register_operand" "d"))))]
  "TARGET_64BIT"
  "andi\t%0,%1,0xff"
  [(set_attr "type" "logical")
   (set_attr "mode" "HI")])

;;
;;  ....................
;;
;;	ZERO EXTENSION
;;
;;  ....................

;; Extension insns.

(define_insn_and_split "zero_extendsidi2"
  [(set (match_operand:DI 0 "register_operand" "=d,d")
        (zero_extend:DI (match_operand:SI 1 "nonimmediate_operand" "d,W")))]
  "TARGET_64BIT"
  "@
   #
   lwu\t%0,%1"
  "&& reload_completed && REG_P (operands[1])"
  [(set (match_dup 0)
        (ashift:DI (match_dup 1) (const_int 32)))
   (set (match_dup 0)
        (lshiftrt:DI (match_dup 0) (const_int 32)))]
  { operands[1] = gen_lowpart (DImode, operands[1]); }
  [(set_attr "move_type" "shift_shift,load")
   (set_attr "mode" "DI")])

;; Combine is not allowed to convert this insn into a zero_extendsidi2
;; because of TRULY_NOOP_TRUNCATION.

(define_insn_and_split "*clear_upper32"
  [(set (match_operand:DI 0 "register_operand" "=d,d")
        (and:DI (match_operand:DI 1 "nonimmediate_operand" "d,W")
		(const_int 4294967295)))]
  "TARGET_64BIT"
{
  if (which_alternative == 0)
    return "#";

  operands[1] = gen_lowpart (SImode, operands[1]);
  return "lwu\t%0,%1";
}
  "&& reload_completed && REG_P (operands[1])"
  [(set (match_dup 0)
        (ashift:DI (match_dup 1) (const_int 32)))
   (set (match_dup 0)
        (lshiftrt:DI (match_dup 0) (const_int 32)))]
  ""
  [(set_attr "move_type" "shift_shift,load")
   (set_attr "mode" "DI")])

(define_insn_and_split "zero_extendhi<GPR:mode>2"
  [(set (match_operand:GPR 0 "register_operand" "=d,d")
        (zero_extend:GPR (match_operand:HI 1 "nonimmediate_operand" "d,m")))]
  ""
  "@
   #
   lhu\t%0,%1"
  "&& reload_completed && REG_P (operands[1])"
  [(set (match_dup 0)
        (ashift:GPR (match_dup 1) (match_dup 2)))
   (set (match_dup 0)
        (lshiftrt:GPR (match_dup 0) (match_dup 2)))]
  {
    operands[1] = gen_lowpart (<GPR:MODE>mode, operands[1]);
    operands[2] = GEN_INT(GET_MODE_BITSIZE(<GPR:MODE>mode) - 16);
  }
  [(set_attr "move_type" "shift_shift,load")
   (set_attr "mode" "<GPR:MODE>")])

(define_insn "zero_extendqi<GPR:mode>2"
  [(set (match_operand:GPR 0 "register_operand" "=d,d")
        (zero_extend:GPR
	     (match_operand:QI 1 "nonimmediate_operand" "d,m")))]
  ""
  "@
   and\t%0,%1,0xff
   lbu\t%0,%1"
  [(set_attr "move_type" "andi,load")
   (set_attr "mode" "<GPR:MODE>")])

(define_insn "zero_extendqihi2"
  [(set (match_operand:HI 0 "register_operand" "=d,d")
        (zero_extend:HI (match_operand:QI 1 "nonimmediate_operand" "d,m")))]
  ""
  "@
   andi\t%0,%1,0x00ff
   lbu\t%0,%1"
  [(set_attr "move_type" "andi,load")
   (set_attr "mode" "HI")])

;;
;;  ....................
;;
;;	SIGN EXTENSION
;;
;;  ....................

;; Extension insns.
;; Those for integer source operand are ordered widest source type first.

;; When TARGET_64BIT, all SImode integer registers should already be in
;; sign-extended form (see TRULY_NOOP_TRUNCATION and truncdisi2).  We can
;; therefore get rid of register->register instructions if we constrain
;; the source to be in the same register as the destination.
;;
;; The register alternative has type "arith" so that the pre-reload
;; scheduler will treat it as a move.  This reflects what happens if
;; the register alternative needs a reload.
(define_insn_and_split "extendsidi2"
  [(set (match_operand:DI 0 "register_operand" "=d,d")
        (sign_extend:DI (match_operand:SI 1 "nonimmediate_operand" "0,m")))]
  "TARGET_64BIT"
  "@
   #
   lw\t%0,%1"
  "&& reload_completed && register_operand (operands[1], VOIDmode)"
  [(const_int 0)]
{
  emit_note (NOTE_INSN_DELETED);
  DONE;
}
  [(set_attr "move_type" "move,load")
   (set_attr "mode" "DI")])

(define_insn_and_split "extend<SHORT:mode><GPR:mode>2"
  [(set (match_operand:GPR 0 "register_operand" "=d,d")
        (sign_extend:GPR
	     (match_operand:SHORT 1 "nonimmediate_operand" "d,m")))]
  ""
  "@
   #
   l<SHORT:size>\t%0,%1"
  "&& reload_completed && REG_P (operands[1])"
  [(set (match_dup 0) (ashift:GPR (match_dup 1) (match_dup 2)))
   (set (match_dup 0) (ashiftrt:GPR (match_dup 0) (match_dup 2)))]
{
  operands[1] = gen_lowpart (<GPR:MODE>mode, operands[1]);
  operands[2] = GEN_INT (GET_MODE_BITSIZE (<GPR:MODE>mode)
			 - GET_MODE_BITSIZE (<SHORT:MODE>mode));
}
  [(set_attr "move_type" "shift_shift,load")
   (set_attr "mode" "<GPR:MODE>")])

(define_insn_and_split "extendqihi2"
  [(set (match_operand:HI 0 "register_operand" "=d,d")
        (sign_extend:HI
	     (match_operand:QI 1 "nonimmediate_operand" "d,m")))]
  ""
  "@
   #
   lb\t%0,%1"
  "&& reload_completed && REG_P (operands[1])"
  [(set (match_dup 0) (ashift:SI (match_dup 1) (match_dup 2)))
   (set (match_dup 0) (ashiftrt:SI (match_dup 0) (match_dup 2)))]
{
  operands[0] = gen_lowpart (SImode, operands[0]);
  operands[1] = gen_lowpart (SImode, operands[1]);
  operands[2] = GEN_INT (GET_MODE_BITSIZE (SImode)
			 - GET_MODE_BITSIZE (QImode));
}
  [(set_attr "move_type" "shift_shift,load")
   (set_attr "mode" "SI")])

(define_insn "extendsfdf2"
  [(set (match_operand:DF 0 "register_operand" "=f")
	(float_extend:DF (match_operand:SF 1 "register_operand" "f")))]
  "TARGET_HARD_FLOAT"
  "fcvt.d.s\t%0,%1"
  [(set_attr "type"	"fcvt")
   (set_attr "cnv_mode"	"S2D")   
   (set_attr "mode"	"DF")])

;;
;;  ....................
;;
;;	CONVERSIONS
;;
;;  ....................

(define_insn "fix_truncdfsi2"
  [(set (match_operand:SI 0 "register_operand" "=d")
	(fix:SI (match_operand:DF 1 "register_operand" "f")))]
  "TARGET_HARD_FLOAT"
  "fcvt.w.d %0,%1,rtz"
  [(set_attr "type"	"fcvt")
   (set_attr "mode"	"DF")
   (set_attr "cnv_mode"	"D2I")])


(define_insn "fix_truncsfsi2"
  [(set (match_operand:SI 0 "register_operand" "=d")
	(fix:SI (match_operand:SF 1 "register_operand" "f")))]
  "TARGET_HARD_FLOAT"
  "fcvt.w.s %0,%1,rtz"
  [(set_attr "type"	"fcvt")
   (set_attr "mode"	"SF")
   (set_attr "cnv_mode"	"S2I")])


(define_insn "fix_truncdfdi2"
  [(set (match_operand:DI 0 "register_operand" "=d")
	(fix:DI (match_operand:DF 1 "register_operand" "f")))]
  "TARGET_HARD_FLOAT"
  "fcvt.l.d %0,%1,rtz"
  [(set_attr "type"	"fcvt")
   (set_attr "mode"	"DF")
   (set_attr "cnv_mode"	"D2I")])


(define_insn "fix_truncsfdi2"
  [(set (match_operand:DI 0 "register_operand" "=d")
	(fix:DI (match_operand:SF 1 "register_operand" "f")))]
  "TARGET_HARD_FLOAT"
  "fcvt.l.s %0,%1,rtz"
  [(set_attr "type"	"fcvt")
   (set_attr "mode"	"SF")
   (set_attr "cnv_mode"	"S2I")])


(define_insn "floatsidf2"
  [(set (match_operand:DF 0 "register_operand" "=f")
	(float:DF (match_operand:SI 1 "register_operand" "d")))]
  "TARGET_HARD_FLOAT"
  "fcvt.d.w\t%0,%1"
  [(set_attr "type"	"fcvt")
   (set_attr "mode"	"DF")
   (set_attr "cnv_mode"	"I2D")])


(define_insn "floatdidf2"
  [(set (match_operand:DF 0 "register_operand" "=f")
	(float:DF (match_operand:DI 1 "register_operand" "d")))]
  "TARGET_HARD_FLOAT"
  "fcvt.d.l\t%0,%1"
  [(set_attr "type"	"fcvt")
   (set_attr "mode"	"DF")
   (set_attr "cnv_mode"	"I2D")])


(define_insn "floatsisf2"
  [(set (match_operand:SF 0 "register_operand" "=f")
	(float:SF (match_operand:SI 1 "register_operand" "d")))]
  "TARGET_HARD_FLOAT"
  "fcvt.s.w\t%0,%1"
  [(set_attr "type"	"fcvt")
   (set_attr "mode"	"SF")
   (set_attr "cnv_mode"	"I2S")])


(define_insn "floatdisf2"
  [(set (match_operand:SF 0 "register_operand" "=f")
	(float:SF (match_operand:DI 1 "register_operand" "d")))]
  "TARGET_HARD_FLOAT"
  "fcvt.s.l\t%0,%1"
  [(set_attr "type"	"fcvt")
   (set_attr "mode"	"SF")
   (set_attr "cnv_mode"	"I2S")])


(define_insn "floatunssidf2"
  [(set (match_operand:DF 0 "register_operand" "=f")
	(unsigned_float:DF (match_operand:SI 1 "register_operand" "d")))]
  "TARGET_HARD_FLOAT"
  "fcvt.d.wu\t%0,%1"
  [(set_attr "type"	"fcvt")
   (set_attr "mode"	"DF")
   (set_attr "cnv_mode"	"I2D")])


(define_insn "floatunsdidf2"
  [(set (match_operand:DF 0 "register_operand" "=f")
	(unsigned_float:DF (match_operand:DI 1 "register_operand" "d")))]
  "TARGET_HARD_FLOAT"
  "fcvt.d.lu\t%0,%1"
  [(set_attr "type"	"fcvt")
   (set_attr "mode"	"DF")
   (set_attr "cnv_mode"	"I2D")])


(define_insn "floatunssisf2"
  [(set (match_operand:SF 0 "register_operand" "=f")
	(unsigned_float:SF (match_operand:SI 1 "register_operand" "d")))]
  "TARGET_HARD_FLOAT"
  "fcvt.s.wu\t%0,%1"
  [(set_attr "type"	"fcvt")
   (set_attr "mode"	"SF")
   (set_attr "cnv_mode"	"I2S")])


(define_insn "floatunsdisf2"
  [(set (match_operand:SF 0 "register_operand" "=f")
	(unsigned_float:SF (match_operand:DI 1 "register_operand" "d")))]
  "TARGET_HARD_FLOAT"
  "fcvt.s.lu\t%0,%1"
  [(set_attr "type"	"fcvt")
   (set_attr "mode"	"SF")
   (set_attr "cnv_mode"	"I2S")])


(define_insn "fixuns_truncdfsi2"
  [(set (match_operand:SI 0 "register_operand" "=d")
	(unsigned_fix:SI (match_operand:DF 1 "register_operand" "f")))]
  "TARGET_HARD_FLOAT"
  "fcvt.wu.d %0,%1,rtz"
  [(set_attr "type"	"fcvt")
   (set_attr "mode"	"DF")
   (set_attr "cnv_mode"	"D2I")])


(define_insn "fixuns_truncsfsi2"
  [(set (match_operand:SI 0 "register_operand" "=d")
	(unsigned_fix:SI (match_operand:SF 1 "register_operand" "f")))]
  "TARGET_HARD_FLOAT"
  "fcvt.wu.s %0,%1,rtz"
  [(set_attr "type"	"fcvt")
   (set_attr "mode"	"SF")
   (set_attr "cnv_mode"	"S2I")])


(define_insn "fixuns_truncdfdi2"
  [(set (match_operand:DI 0 "register_operand" "=d")
	(unsigned_fix:DI (match_operand:DF 1 "register_operand" "f")))]
  "TARGET_HARD_FLOAT"
  "fcvt.lu.d %0,%1,rtz"
  [(set_attr "type"	"fcvt")
   (set_attr "mode"	"DF")
   (set_attr "cnv_mode"	"D2I")])


(define_insn "fixuns_truncsfdi2"
  [(set (match_operand:DI 0 "register_operand" "=d")
	(unsigned_fix:DI (match_operand:SF 1 "register_operand" "f")))]
  "TARGET_HARD_FLOAT"
  "fcvt.lu.s %0,%1,rtz"
  [(set_attr "type"	"fcvt")
   (set_attr "mode"	"SF")
   (set_attr "cnv_mode"	"S2I")])

;;
;;  ....................
;;
;;	DATA MOVEMENT
;;
;;  ....................

;; Insns to fetch a symbol from a big GOT.

(define_insn_and_split "*xgot_hi<mode>"
  [(set (match_operand:P 0 "register_operand" "=d")
	(high:P (match_operand:P 1 "got_disp_operand" "")))]
  "TARGET_XGOT"
  "#"
  "&& reload_completed"
  [(set (match_dup 0) (high:P (match_dup 2)))
   (set (match_dup 0) (plus:P (match_dup 0) (match_dup 3)))]
{
  operands[2] = mips_unspec_address (operands[1], SYMBOL_GOTOFF_DISP);
  operands[3] = pic_offset_table_rtx;
}
  [(set_attr "got" "xgot_high")
   (set_attr "mode" "<MODE>")])

(define_insn_and_split "*xgot_lo<mode>"
  [(set (match_operand:P 0 "register_operand" "=d")
	(lo_sum:P (match_operand:P 1 "register_operand" "d")
		  (match_operand:P 2 "got_disp_operand" "")))]
  "TARGET_XGOT"
  "#"
  "&& reload_completed"
  [(set (match_dup 0)
	(unspec:P [(match_dup 1) (match_dup 3)] UNSPEC_LOAD_GOT))]
  { operands[3] = mips_unspec_address (operands[2], SYMBOL_GOTOFF_DISP); }
  [(set_attr "got" "load")
   (set_attr "mode" "<MODE>")])

;; Insns to fetch a symbol from a normal GOT.

(define_insn_and_split "*got_disp<mode>"
  [(set (match_operand:P 0 "register_operand" "=d")
	(match_operand:P 1 "got_disp_operand" ""))]
  "!mips_split_p[SYMBOL_GOT_DISP]"
  "#"
  "&& reload_completed"
  [(set (match_dup 0) (match_dup 2))]
  { operands[2] = mips_got_load (NULL, operands[1], SYMBOL_GOTOFF_DISP); }
  [(set_attr "got" "load")
   (set_attr "mode" "<MODE>")])

;; Convenience expander that generates the rhs of a load_got<mode> insn.
(define_expand "unspec_got<mode>"
  [(unspec:P [(match_operand:P 0)
	      (match_operand:P 1)] UNSPEC_LOAD_GOT)])

;; Lower-level instructions for loading an address from the GOT.
;; We could use MEMs, but an unspec gives more optimization
;; opportunities.

(define_insn "load_got<mode>"
  [(set (match_operand:P 0 "register_operand" "=d")
	(unspec:P [(match_operand:P 1 "register_operand" "d")
		   (match_operand:P 2 "immediate_operand" "")]
		  UNSPEC_LOAD_GOT))]
  ""
  "<load>\t%0,%R2(%1)"
  [(set_attr "got" "load")
   (set_attr "mode" "<MODE>")])

;; Instructions for adding the low 16 bits of an address to a register.
;; Operand 2 is the address: mips_print_operand works out which relocation
;; should be applied.

(define_insn "*low<mode>"
  [(set (match_operand:P 0 "register_operand" "=d")
	(lo_sum:P (match_operand:P 1 "register_operand" "d")
		  (match_operand:P 2 "immediate_operand" "")))]
  ""
  { return Pmode == SImode && TARGET_64BIT ? "addw\t%0,%1,%R2" : "add\t%0,%1,%R2"; }
  [(set_attr "alu_type" "add")
   (set_attr "mode" "<MODE>")])

;; Allow combine to split complex const_int load sequences, using operand 2
;; to store the intermediate results.  See move_operand for details.
(define_split
  [(set (match_operand:GPR 0 "register_operand")
	(match_operand:GPR 1 "splittable_const_int_operand"))
   (clobber (match_operand:GPR 2 "register_operand"))]
  ""
  [(const_int 0)]
{
  mips_move_integer (operands[2], operands[0], INTVAL (operands[1]));
  DONE;
})

;; Likewise, for symbolic operands.
(define_split
  [(set (match_operand:P 0 "register_operand")
	(match_operand:P 1))
   (clobber (match_operand:P 2 "register_operand"))]
  "mips_split_symbol (operands[2], operands[1], MAX_MACHINE_MODE, NULL)"
  [(set (match_dup 0) (match_dup 3))]
{
  mips_split_symbol (operands[2], operands[1],
		     MAX_MACHINE_MODE, &operands[3]);
})

;; 64-bit integer moves

;; Unlike most other insns, the move insns can't be split with '
;; different predicates, because register spilling and other parts of
;; the compiler, have memoized the insn number already.

(define_expand "movdi"
  [(set (match_operand:DI 0 "")
	(match_operand:DI 1 ""))]
  ""
{
  if (mips_legitimize_move (DImode, operands[0], operands[1]))
    DONE;
})

(define_insn "*movdi_32bit"
  [(set (match_operand:DI 0 "nonimmediate_operand" "=d,d,d,m,*f,*f,*d,*m")
	(match_operand:DI 1 "move_operand" "d,i,m,d,*J*d,*m,*f,*f"))]
  "!TARGET_64BIT
   && (register_operand (operands[0], DImode)
       || reg_or_0_operand (operands[1], DImode))"
  { return mips_output_move (operands[0], operands[1]); }
  [(set_attr "move_type" "move,const,load,store,mtc,fpload,mfc,fpstore")
   (set_attr "mode" "DI")])

(define_insn "*movdi_64bit"
  [(set (match_operand:DI 0 "nonimmediate_operand" "=d,d,e,d,m,*f,*f,*d,*m")
	(match_operand:DI 1 "move_operand" "d,U,T,m,dJ,*d*J,*m,*f,*f"))]
  "TARGET_64BIT
   && (register_operand (operands[0], DImode)
       || reg_or_0_operand (operands[1], DImode))"
  { return mips_output_move (operands[0], operands[1]); }
  [(set_attr "move_type" "move,const,const,load,store,mtc,fpload,mfc,fpstore")
   (set_attr "mode" "DI")])

;; 32-bit Integer moves

;; Unlike most other insns, the move insns can't be split with
;; different predicates, because register spilling and other parts of
;; the compiler, have memoized the insn number already.

(define_expand "mov<mode>"
  [(set (match_operand:IMOVE32 0 "")
	(match_operand:IMOVE32 1 ""))]
  ""
{
  if (mips_legitimize_move (<MODE>mode, operands[0], operands[1]))
    DONE;
})

;; The difference between these two is whether or not ints are allowed
;; in FP registers (off by default, use -mdebugh to enable).

(define_insn "*mov<mode>_internal"
  [(set (match_operand:IMOVE32 0 "nonimmediate_operand" "=d,d,e,d,m,*f,*f,*d,*m,*d,*z")
	(match_operand:IMOVE32 1 "move_operand" "d,U,T,m,dJ,*d*J,*m,*f,*f,*z,*d"))]
  "(register_operand (operands[0], <MODE>mode)
    || reg_or_0_operand (operands[1], <MODE>mode))"
  { return mips_output_move (operands[0], operands[1]); }
  [(set_attr "move_type" "move,const,const,load,store,mtc,fpload,mfc,fpstore,mfc,mtc")
   (set_attr "mode" "SI")])

;; 16-bit Integer moves

;; Unlike most other insns, the move insns can't be split with
;; different predicates, because register spilling and other parts of
;; the compiler, have memoized the insn number already.
;; Unsigned loads are used because LOAD_EXTEND_OP returns ZERO_EXTEND.

(define_expand "movhi"
  [(set (match_operand:HI 0 "")
	(match_operand:HI 1 ""))]
  ""
{
  if (mips_legitimize_move (HImode, operands[0], operands[1]))
    DONE;
})

(define_insn "*movhi_internal"
  [(set (match_operand:HI 0 "nonimmediate_operand" "=d,d,d,m")
	(match_operand:HI 1 "move_operand"         "d,Z,m,dJ"))]
  "(register_operand (operands[0], HImode)
    || reg_or_0_operand (operands[1], HImode))"
  { return mips_output_move (operands[0], operands[1]); }
  [(set_attr "move_type" "move,const,load,store")
   (set_attr "mode" "HI")])

;; 8-bit Integer moves

;; Unlike most other insns, the move insns can't be split with '
;; different predicates, because register spilling and other parts of
;; the compiler, have memoized the insn number already.
;; Unsigned loads are used because LOAD_EXTEND_OP returns ZERO_EXTEND.

(define_expand "movqi"
  [(set (match_operand:QI 0 "")
	(match_operand:QI 1 ""))]
  ""
{
  if (mips_legitimize_move (QImode, operands[0], operands[1]))
    DONE;
})

(define_insn "*movqi_internal"
  [(set (match_operand:QI 0 "nonimmediate_operand" "=d,d,d,m")
	(match_operand:QI 1 "move_operand"         "d,I,m,dJ"))]
  "(register_operand (operands[0], QImode)
    || reg_or_0_operand (operands[1], QImode))"
  { return mips_output_move (operands[0], operands[1]); }
  [(set_attr "move_type" "move,const,load,store")
   (set_attr "mode" "QI")])

;; 32-bit floating point moves

(define_expand "movsf"
  [(set (match_operand:SF 0 "")
	(match_operand:SF 1 ""))]
  ""
{
  if (mips_legitimize_move (SFmode, operands[0], operands[1]))
    DONE;
})

(define_insn "*movsf_hardfloat"
  [(set (match_operand:SF 0 "nonimmediate_operand" "=f,f,f,m,m,*f,*d,*d,*d,*m")
	(match_operand:SF 1 "move_operand" "f,G,m,f,G,*d,*f,*G*d,*m,*d"))]
  "TARGET_HARD_FLOAT
   && (register_operand (operands[0], SFmode)
       || reg_or_0_operand (operands[1], SFmode))"
  { return mips_output_move (operands[0], operands[1]); }
  [(set_attr "move_type" "fmove,mtc,fpload,fpstore,store,mtc,mfc,move,load,store")
   (set_attr "mode" "SF")])

(define_insn "*movsf_softfloat"
  [(set (match_operand:SF 0 "nonimmediate_operand" "=d,d,m")
	(match_operand:SF 1 "move_operand" "Gd,m,d"))]
  "TARGET_SOFT_FLOAT
   && (register_operand (operands[0], SFmode)
       || reg_or_0_operand (operands[1], SFmode))"
  { return mips_output_move (operands[0], operands[1]); }
  [(set_attr "move_type" "move,load,store")
   (set_attr "mode" "SF")])

;; 64-bit floating point moves

(define_expand "movdf"
  [(set (match_operand:DF 0 "")
	(match_operand:DF 1 ""))]
  ""
{
  if (mips_legitimize_move (DFmode, operands[0], operands[1]))
    DONE;
})

;; In RV32, we lack mtf.d/mff.d.  Go through memory instead.
;; (except for moving a constant 0 to an FPR.  for that we use fcvt.d.w.)
(define_insn "*movdf_hardfloat_rv32"
  [(set (match_operand:DF 0 "nonimmediate_operand" "=f,f,f,m,m,*d,*d,*m")
	(match_operand:DF 1 "move_operand" "f,G,m,f,G,*d*G,*m,*d"))]
  "!TARGET_64BIT && TARGET_HARD_FLOAT
   && (register_operand (operands[0], DFmode)
       || reg_or_0_operand (operands[1], DFmode))"
  { return mips_output_move (operands[0], operands[1]); }
  [(set_attr "move_type" "fmove,mtc,fpload,fpstore,store,move,load,store")
   (set_attr "mode" "DF")])

(define_insn "*movdf_hardfloat_rv64"
  [(set (match_operand:DF 0 "nonimmediate_operand" "=f,f,f,m,m,*f,*d,*d,*d,*m")
	(match_operand:DF 1 "move_operand" "f,G,m,f,G,*d,*f,*d*G,*m,*d"))]
  "TARGET_64BIT && TARGET_HARD_FLOAT
   && (register_operand (operands[0], DFmode)
       || reg_or_0_operand (operands[1], DFmode))"
  { return mips_output_move (operands[0], operands[1]); }
  [(set_attr "move_type" "fmove,mtc,fpload,fpstore,store,mtc,mfc,move,load,store")
   (set_attr "mode" "DF")])

(define_insn "*movdf_softfloat"
  [(set (match_operand:DF 0 "nonimmediate_operand" "=d,d,m")
	(match_operand:DF 1 "move_operand" "dG,m,dG"))]
  "TARGET_SOFT_FLOAT
   && (register_operand (operands[0], DFmode)
       || reg_or_0_operand (operands[1], DFmode))"
  { return mips_output_move (operands[0], operands[1]); }
  [(set_attr "move_type" "move,load,store")
   (set_attr "mode" "DF")])

;; 128-bit integer moves

(define_expand "movti"
  [(set (match_operand:TI 0)
	(match_operand:TI 1))]
  "TARGET_64BIT"
{
  if (mips_legitimize_move (TImode, operands[0], operands[1]))
    DONE;
})

(define_insn "*movti"
  [(set (match_operand:TI 0 "nonimmediate_operand" "=d,d,d,m")
	(match_operand:TI 1 "move_operand" "d,i,m,dJ"))]
  "TARGET_64BIT
   && (register_operand (operands[0], TImode)
       || reg_or_0_operand (operands[1], TImode))"
  "#"
  [(set_attr "move_type" "move,const,load,store")
   (set_attr "mode" "TI")])

(define_split
  [(set (match_operand:MOVE64 0 "nonimmediate_operand")
	(match_operand:MOVE64 1 "move_operand"))]
  "reload_completed && !TARGET_64BIT
   && mips_split_64bit_move_p (operands[0], operands[1])"
  [(const_int 0)]
{
  mips_split_doubleword_move (operands[0], operands[1]);
  DONE;
})

(define_split
  [(set (match_operand:MOVE128 0 "nonimmediate_operand")
	(match_operand:MOVE128 1 "move_operand"))]
  "TARGET_64BIT && reload_completed"
  [(const_int 0)]
{
  mips_split_doubleword_move (operands[0], operands[1]);
  DONE;
})

;; 64-bit paired-single floating point moves

;; Load the low word of operand 0 with operand 1.
(define_insn "load_low<mode>"
  [(set (match_operand:SPLITF 0 "register_operand" "=f,f")
	(unspec:SPLITF [(match_operand:<HALFMODE> 1 "general_operand" "dJ,m")]
		       UNSPEC_LOAD_LOW))]
  "TARGET_HARD_FLOAT"
{
  operands[0] = mips_subword (operands[0], 0);
  return mips_output_move (operands[0], operands[1]);
}
  [(set_attr "move_type" "mtc,fpload")
   (set_attr "mode" "<HALFMODE>")])

;; Load the high word of operand 0 from operand 1, preserving the value
;; in the low word.
(define_insn "load_high<mode>"
  [(set (match_operand:SPLITF 0 "register_operand" "=f,f")
	(unspec:SPLITF [(match_operand:<HALFMODE> 1 "general_operand" "dJ,m")
			(match_operand:SPLITF 2 "register_operand" "0,0")]
		       UNSPEC_LOAD_HIGH))]
  "TARGET_HARD_FLOAT"
{
  operands[0] = mips_subword (operands[0], 1);
  return mips_output_move (operands[0], operands[1]);
}
  [(set_attr "move_type" "mtc,fpload")
   (set_attr "mode" "<HALFMODE>")])

;; Store one word of operand 1 in operand 0.  Operand 2 is 1 to store the
;; high word and 0 to store the low word.
(define_insn "store_word<mode>"
  [(set (match_operand:<HALFMODE> 0 "nonimmediate_operand" "=d,m")
	(unspec:<HALFMODE> [(match_operand:SPLITF 1 "register_operand" "f,f")
			    (match_operand 2 "const_int_operand")]
			   UNSPEC_STORE_WORD))]
  "TARGET_HARD_FLOAT"
{
  operands[1] = mips_subword (operands[1], INTVAL (operands[2]));
  return mips_output_move (operands[0], operands[1]);
}
  [(set_attr "move_type" "mfc,fpstore")
   (set_attr "mode" "<HALFMODE>")])

;; Insn to initialize $gp for n32/n64 abicalls.  Operand 0 is the offset
;; of _gp from the start of this function.  Operand 1 is the incoming
;; function address.
(define_insn_and_split "loadgp_newabi_<mode>"
  [(set (match_operand:P 0 "register_operand" "=d")
	(unspec:P [(match_operand:P 1)
		   (match_operand:P 2 "register_operand" "d")]
		  UNSPEC_LOADGP))]
  "mips_current_loadgp_style () == LOADGP_NEWABI"
  { return mips_must_initialize_gp_p () ? "#" : ""; }
  "&& mips_must_initialize_gp_p ()"
  [(set (match_dup 0) (match_dup 3))
   (set (match_dup 0) (match_dup 4))
   (set (match_dup 0) (match_dup 5))]
{
  operands[3] = gen_rtx_HIGH (Pmode, operands[1]);
  operands[4] = gen_rtx_PLUS (Pmode, operands[0], operands[2]);
  operands[5] = gen_rtx_LO_SUM (Pmode, operands[0], operands[1]);
}
  [(set_attr "type" "ghost")])

;; Likewise, for -mno-shared code.  Operand 0 is the __gnu_local_gp symbol.
(define_insn_and_split "loadgp_absolute_<mode>"
  [(set (match_operand:P 0 "register_operand" "=d")
	(unspec:P [(match_operand:P 1)] UNSPEC_LOADGP))]
  "mips_current_loadgp_style () == LOADGP_ABSOLUTE"
  { return mips_must_initialize_gp_p () ? "#" : ""; }
  "&& mips_must_initialize_gp_p ()"
  [(const_int 0)]
{
  mips_emit_move (operands[0], operands[1]);
  DONE;
}
  [(set_attr "type" "ghost")])

;; This blockage instruction prevents the gp load from being
;; scheduled after an implicit use of gp.  It also prevents
;; the load from being deleted as dead.
(define_insn "loadgp_blockage"
  [(unspec_volatile [(reg:SI 28)] UNSPEC_BLOCKAGE)]
  ""
  ""
  [(set_attr "type" "ghost")])

;; Expand in-line code to clear the instruction cache between operand[0] and
;; operand[1].
(define_expand "clear_cache"
  [(match_operand 0 "pmode_register_operand")
   (match_operand 1 "pmode_register_operand")]
  ""
  "
{
  emit_insn(gen_fence_i());
  DONE;
}")

(define_insn "fence"
  [(unspec_volatile [(const_int 0)] UNSPEC_FENCE)]
  ""
  "%|fence%-")

(define_insn "fence_i"
  [(unspec_volatile [(const_int 0)] UNSPEC_FENCE_I)]
  ""
  "fence.i")

;; Block moves, see mips.c for more details.
;; Argument 0 is the destination
;; Argument 1 is the source
;; Argument 2 is the length
;; Argument 3 is the alignment

(define_expand "movmemsi"
  [(parallel [(set (match_operand:BLK 0 "general_operand")
		   (match_operand:BLK 1 "general_operand"))
	      (use (match_operand:SI 2 ""))
	      (use (match_operand:SI 3 "const_int_operand"))])]
  "!TARGET_MEMCPY"
{
  if (mips_expand_block_move (operands[0], operands[1], operands[2]))
    DONE;
  else
    FAIL;
})

;;
;;  ....................
;;
;;	SHIFTS
;;
;;  ....................

(define_insn "<optab>si3"
  [(set (match_operand:SI 0 "register_operand" "=d")
	(any_shift:SI (match_operand:SI 1 "register_operand" "d")
		       (match_operand:SI 2 "arith_operand" "dI")))]
  ""
{
  if (GET_CODE (operands[2]) == CONST_INT)
    operands[2] = GEN_INT (INTVAL (operands[2])
			   & (GET_MODE_BITSIZE (SImode) - 1));

  return TARGET_64BIT ? "<insn>w\t%0,%1,%2" : "<insn>\t%0,%1,%2";
}
  [(set_attr "type" "shift")
   (set_attr "mode" "SI")])

(define_insn "*<optab>disi3"
  [(set (match_operand:SI 0 "register_operand" "=d")
	     (any_shift:SI (truncate:SI (match_operand:DI 1 "register_operand" "d"))
		      (truncate:SI (match_operand:DI 2 "arith_operand" "dI"))))]
  "TARGET_64BIT"
  "<insn>w\t%0,%1,%2"
  [(set_attr "type" "shift")
   (set_attr "mode" "SI")])

(define_insn "*ashldi3_truncsi"
  [(set (match_operand:SI 0 "register_operand" "=d")
          (truncate:SI
	     (ashift:DI (match_operand:DI 1 "register_operand" "d")
		      (match_operand:DI 2 "arith_operand" "dI"))))]
  "TARGET_64BIT && (GET_CODE (operands[2]) == CONST_INT ? INTVAL (operands[2]) < 32 : 1)"
  "sllw\t%0,%1,%2"
  [(set_attr "type" "shift")
   (set_attr "mode" "SI")])

(define_insn "<optab>di3"
  [(set (match_operand:DI 0 "register_operand" "=d")
	(any_shift:DI (match_operand:DI 1 "register_operand" "d")
		       (match_operand:DI 2 "arith_operand" "dI")))]
  "TARGET_64BIT"
{
  if (GET_CODE (operands[2]) == CONST_INT)
    operands[2] = GEN_INT (INTVAL (operands[2])
			   & (GET_MODE_BITSIZE (DImode) - 1));

  return "<insn>\t%0,%1,%2";
}
  [(set_attr "type" "shift")
   (set_attr "mode" "DI")])

(define_insn "<optab>si3_extend"
  [(set (match_operand:DI 0 "register_operand" "=d")
	(sign_extend:DI
	   (any_shift:SI (match_operand:SI 1 "register_operand" "d")
			 (match_operand:SI 2 "arith_operand" "dI"))))]
  "TARGET_64BIT"
{
  if (GET_CODE (operands[2]) == CONST_INT)
    operands[2] = GEN_INT (INTVAL (operands[2]) & 0x1f);

  return "<insn>w\t%0,%1,%2";
}
  [(set_attr "type" "shift")
   (set_attr "mode" "SI")])

;;
;;  ....................
;;
;;	CONDITIONAL BRANCHES
;;
;;  ....................

;; Conditional branches

(define_insn "*branch_order<mode>"
  [(set (pc)
	(if_then_else
	 (match_operator 1 "order_operator"
			 [(match_operand:GPR 2 "register_operand" "d")
			  (match_operand:GPR 3 "reg_or_0_operand" "dJ")])
	 (label_ref (match_operand 0 "" ""))
	 (pc)))]
  ""
{
  return mips_output_conditional_branch (insn, operands,
					 "b%C1\t%2,%z3,%0",
					 "b%N1\t%2,%z3,%0");
}
  [(set_attr "type" "branch")
   (set_attr "mode" "none")])

(define_insn "*branch_order<mode>_inverted"
  [(set (pc)
	(if_then_else
	 (match_operator 1 "order_operator"
			 [(match_operand:GPR 2 "register_operand" "d")
			  (match_operand:GPR 3 "reg_or_0_operand" "dJ")])
	 (pc)
	 (label_ref (match_operand 0 "" ""))))]
  ""
{
  return mips_output_conditional_branch (insn, operands,
					 "b%N1\t%2,%z3,%0",
					 "b%C1\t%2,%z3,%0");
}
  [(set_attr "type" "branch")
   (set_attr "mode" "none")])

;; Used to implement built-in functions.
(define_expand "condjump"
  [(set (pc)
	(if_then_else (match_operand 0)
		      (label_ref (match_operand 1))
		      (pc)))])

(define_expand "cbranch<mode>4"
  [(set (pc)
	(if_then_else (match_operator 0 "comparison_operator"
		       [(match_operand:GPR 1 "register_operand")
		        (match_operand:GPR 2 "nonmemory_operand")])
		      (label_ref (match_operand 3 ""))
		      (pc)))]
  ""
{
  mips_expand_conditional_branch (operands);
  DONE;
})

(define_expand "cbranch<mode>4"
  [(set (pc)
	(if_then_else (match_operator 0 "comparison_operator"
		       [(match_operand:SCALARF 1 "register_operand")
		        (match_operand:SCALARF 2 "register_operand")])
		      (label_ref (match_operand 3 ""))
		      (pc)))]
  ""
{
  mips_expand_conditional_branch (operands);
  DONE;
})

;;
;;  ....................
;;
;;	SETTING A REGISTER FROM A COMPARISON
;;
;;  ....................

;; Destination is always set in SI mode.

(define_expand "cstore<mode>4"
  [(set (match_operand:SI 0 "register_operand")
	(match_operator:SI 1 "order_operator"
	 [(match_operand:GPR 2 "register_operand")
	  (match_operand:GPR 3 "nonmemory_operand")]))]
  ""
{
  mips_expand_scc (operands);
  DONE;
})

(define_insn "cstore<mode>4"
   [(set (match_operand:SI 0 "register_operand" "=d")
        (match_operator:SI 1 "fp_order_operator"
	      [(match_operand:SCALARF 2 "register_operand" "f")
	       (match_operand:SCALARF 3 "register_operand" "f")]))]
  "TARGET_HARD_FLOAT"
{
  if (GET_CODE (operands[1]) == GT || GET_CODE (operands[1]) == GE)
    return "f%S1.<fmt>\t%0,%3,%2";
  else
    return "f%C1.<fmt>\t%0,%2,%3";
}
  [(set_attr "type" "fcmp")
   (set_attr "mode" "<UNITMODE>")])

(define_insn "*seq_zero_<GPR:mode><GPR2:mode>"
  [(set (match_operand:GPR2 0 "register_operand" "=d")
	(eq:GPR2 (match_operand:GPR 1 "register_operand" "d")
		 (const_int 0)))]
  ""
  "sltu\t%0,%1,1"
  [(set_attr "type" "slt")
   (set_attr "mode" "<GPR:MODE>")])

(define_insn "*sne_zero_<GPR:mode><GPR2:mode>"
  [(set (match_operand:GPR2 0 "register_operand" "=d")
	(ne:GPR2 (match_operand:GPR 1 "register_operand" "d")
		 (const_int 0)))]
  ""
  "sltu\t%0,zero,%1"
  [(set_attr "type" "slt")
   (set_attr "mode" "<GPR:MODE>")])

(define_insn "*sgt<u>_<GPR:mode><GPR2:mode>"
  [(set (match_operand:GPR2 0 "register_operand" "=d")
	(any_gt:GPR2 (match_operand:GPR 1 "register_operand" "d")
		     (match_operand:GPR 2 "reg_or_0_operand" "dJ")))]
  ""
  "slt<u>\t%0,%z2,%1"
  [(set_attr "type" "slt")
   (set_attr "mode" "<GPR:MODE>")])

(define_insn "*sge<u>_<GPR:mode><GPR2:mode>"
  [(set (match_operand:GPR2 0 "register_operand" "=d")
	(any_ge:GPR2 (match_operand:GPR 1 "register_operand" "d")
		     (const_int 1)))]
  ""
  "slt<u>\t%0,zero,%1"
  [(set_attr "type" "slt")
   (set_attr "mode" "<GPR:MODE>")])

(define_insn "*slt<u>_<GPR:mode><GPR2:mode>"
  [(set (match_operand:GPR2 0 "register_operand" "=d")
	(any_lt:GPR2 (match_operand:GPR 1 "register_operand" "d")
		     (match_operand:GPR 2 "arith_operand" "dI")))]
  ""
  "slt<u>\t%0,%1,%2"
  [(set_attr "type" "slt")
   (set_attr "mode" "<GPR:MODE>")])

(define_insn "*sle<u>_<GPR:mode><GPR2:mode>"
  [(set (match_operand:GPR2 0 "register_operand" "=d")
	(any_le:GPR2 (match_operand:GPR 1 "register_operand" "d")
		     (match_operand:GPR 2 "sle_operand" "")))]
  ""
{
  operands[2] = GEN_INT (INTVAL (operands[2]) + 1);
  return "slt<u>\t%0,%1,%2";
}
  [(set_attr "type" "slt")
   (set_attr "mode" "<GPR:MODE>")])

;;
;;  ....................
;;
;;	UNCONDITIONAL BRANCHES
;;
;;  ....................

;; Unconditional branches.

(define_insn "jump"
  [(set (pc)
	(label_ref (match_operand 0 "" "")))]
  ""
  "j\t%l0"
  [(set_attr "type"	"jump")
   (set_attr "mode"	"none")])

(define_expand "indirect_jump"
  [(set (pc) (match_operand 0 "register_operand"))]
  ""
{
  operands[0] = force_reg (Pmode, operands[0]);
  if (Pmode == SImode)
    emit_jump_insn (gen_indirect_jumpsi (operands[0]));
  else
    emit_jump_insn (gen_indirect_jumpdi (operands[0]));
  DONE;
})

(define_insn "indirect_jump<mode>"
  [(set (pc) (match_operand:P 0 "register_operand" "d"))]
  ""
  "j\t%0"
  [(set_attr "type" "jump")
   (set_attr "mode" "none")])

(define_expand "tablejump"
  [(set (pc)
	(match_operand 0 "register_operand"))
   (use (label_ref (match_operand 1 "")))]
  ""
{
  if (TARGET_GPWORD)
    operands[0] = expand_binop (Pmode, add_optab, operands[0],
				pic_offset_table_rtx, 0, 0, OPTAB_WIDEN);

  if (Pmode == SImode)
    emit_jump_insn (gen_tablejumpsi (operands[0], operands[1]));
  else
    emit_jump_insn (gen_tablejumpdi (operands[0], operands[1]));
  DONE;
})

(define_insn "tablejump<mode>"
  [(set (pc)
	(match_operand:P 0 "register_operand" "d"))
   (use (label_ref (match_operand 1 "" "")))]
  ""
  "j\t%0"
  [(set_attr "type" "jump")
   (set_attr "mode" "none")])

;; For TARGET_USE_GOT, we save the gp in the jmp_buf as well.
;; While it is possible to either pull it off the stack (in the
;; o32 case) or recalculate it given t9 and our target label,
;; it takes 3 or 4 insns to do so.

(define_expand "builtin_setjmp_setup"
  [(use (match_operand 0 "register_operand"))]
  "TARGET_USE_GOT"
{
  rtx addr;

  addr = plus_constant (operands[0], GET_MODE_SIZE (Pmode) * 3);
  mips_emit_move (gen_rtx_MEM (Pmode, addr), pic_offset_table_rtx);
  DONE;
})

;; Restore the gp that we saved above.  Despite the earlier comment, it seems
;; that older code did recalculate the gp from $25.  Continue to jump through
;; $25 for compatibility (we lose nothing by doing so).

(define_expand "builtin_longjmp"
  [(use (match_operand 0 "register_operand"))]
  "TARGET_USE_GOT"
{
  /* The elements of the buffer are, in order:  */
  int W = GET_MODE_SIZE (Pmode);
  rtx fp = gen_rtx_MEM (Pmode, operands[0]);
  rtx lab = gen_rtx_MEM (Pmode, plus_constant (operands[0], 1*W));
  rtx stack = gen_rtx_MEM (Pmode, plus_constant (operands[0], 2*W));
  rtx gpv = gen_rtx_MEM (Pmode, plus_constant (operands[0], 3*W));
  rtx pv = gen_rtx_REG (Pmode, PIC_FUNCTION_ADDR_REGNUM);
  /* Use gen_raw_REG to avoid being given pic_offset_table_rtx.
     The target is bound to be using $28 as the global pointer
     but the current function might not be.  */
  rtx gp = gen_raw_REG (Pmode, GLOBAL_POINTER_REGNUM);

  /* This bit is similar to expand_builtin_longjmp except that it
     restores $gp as well.  */
  mips_emit_move (hard_frame_pointer_rtx, fp);
  mips_emit_move (pv, lab);
  emit_stack_restore (SAVE_NONLOCAL, stack);
  mips_emit_move (gp, gpv);
  emit_use (hard_frame_pointer_rtx);
  emit_use (stack_pointer_rtx);
  emit_use (gp);
  emit_indirect_jump (pv);
  DONE;
})

;;
;;  ....................
;;
;;	Function prologue/epilogue
;;
;;  ....................
;;

(define_expand "prologue"
  [(const_int 1)]
  ""
{
  mips_expand_prologue ();
  DONE;
})

;; Block any insns from being moved before this point, since the
;; profiling call to mcount can use various registers that aren't
;; saved or used to pass arguments.

(define_insn "blockage"
  [(unspec_volatile [(const_int 0)] UNSPEC_BLOCKAGE)]
  ""
  ""
  [(set_attr "type" "ghost")
   (set_attr "mode" "none")])

(define_expand "epilogue"
  [(const_int 2)]
  ""
{
  mips_expand_epilogue (false);
  DONE;
})

(define_expand "sibcall_epilogue"
  [(const_int 2)]
  ""
{
  mips_expand_epilogue (true);
  DONE;
})

;; Trivial return.  Make it look like a normal return insn as that
;; allows jump optimizations to work better.

(define_insn "return"
  [(return)]
  "mips_can_use_return_insn ()"
  "ret"
  [(set_attr "type"	"jump")
   (set_attr "mode"	"none")])

;; Normal return.

(define_insn "return_internal"
  [(return)
   (use (match_operand 0 "pmode_register_operand" ""))]
  ""
  "j\t%0"
  [(set_attr "type"	"jump")
   (set_attr "mode"	"none")])

;; This is used in compiling the unwind routines.
(define_expand "eh_return"
  [(use (match_operand 0 "general_operand"))]
  ""
{
  if (GET_MODE (operands[0]) != word_mode)
    operands[0] = convert_to_mode (word_mode, operands[0], 0);
  if (TARGET_64BIT)
    emit_insn (gen_eh_set_lr_di (operands[0]));
  else
    emit_insn (gen_eh_set_lr_si (operands[0]));
  DONE;
})

;; Clobber the return address on the stack.  We can't expand this
;; until we know where it will be put in the stack frame.

(define_insn "eh_set_lr_si"
  [(unspec [(match_operand:SI 0 "register_operand" "d")] UNSPEC_EH_RETURN)
   (clobber (match_scratch:SI 1 "=&d"))]
  "! TARGET_64BIT"
  "#")

(define_insn "eh_set_lr_di"
  [(unspec [(match_operand:DI 0 "register_operand" "d")] UNSPEC_EH_RETURN)
   (clobber (match_scratch:DI 1 "=&d"))]
  "TARGET_64BIT"
  "#")

(define_split
  [(unspec [(match_operand 0 "register_operand")] UNSPEC_EH_RETURN)
   (clobber (match_scratch 1))]
  "reload_completed"
  [(const_int 0)]
{
  mips_set_return_address (operands[0], operands[1]);
  DONE;
})

(define_expand "exception_receiver"
  [(const_int 0)]
  "TARGET_USE_GOT"
{
  /* See the comment above load_call<mode> for details.  */
  emit_insn (gen_set_got_version ());
  DONE;
})

(define_expand "nonlocal_goto_receiver"
  [(const_int 0)]
  "TARGET_USE_GOT"
{
  /* See the comment above load_call<mode> for details.  */
  emit_insn (gen_set_got_version ());
  DONE;
})

;; Move between $gp and its register save slot.
(define_insn_and_split "move_gp<mode>"
  [(set (match_operand:GPR 0 "nonimmediate_operand" "=d,m")
  	(unspec:GPR [(match_operand:GPR 1 "move_operand" "m,d")]
		    UNSPEC_MOVE_GP))]
  ""
  { return mips_must_initialize_gp_p () ? "#" : ""; }
  "mips_must_initialize_gp_p ()"
  [(const_int 0)]
{
  mips_emit_move (operands[0], operands[1]);
  DONE;
}
  [(set_attr "type" "ghost")])

;;
;;  ....................
;;
;;	FUNCTION CALLS
;;
;;  ....................

;; Instructions to load a call address from the GOT.  The address might
;; point to a function or to a lazy binding stub.  In the latter case,
;; the stub will use the dynamic linker to resolve the function, which
;; in turn will change the GOT entry to point to the function's real
;; address.
;;
;; This means that every call, even pure and constant ones, can
;; potentially modify the GOT entry.  And once a stub has been called,
;; we must not call it again.
;;
;; We represent this restriction using an imaginary, fixed, call-saved
;; register called GOT_VERSION_REGNUM.  The idea is to make the register
;; live throughout the function and to change its value after every
;; potential call site.  This stops any rtx value that uses the register
;; from being computed before an earlier call.  To do this, we:
;;
;;    - Ensure that the register is live on entry to the function,
;;	so that it is never thought to be used uninitalized.
;;
;;    - Ensure that the register is live on exit from the function,
;;	so that it is live throughout.
;;
;;    - Make each call (lazily-bound or not) use the current value
;;	of GOT_VERSION_REGNUM, so that updates of the register are
;;	not moved across call boundaries.
;;
;;    - Add "ghost" definitions of the register to the beginning of
;;	blocks reached by EH and ABNORMAL_CALL edges, because those
;;	edges may involve calls that normal paths don't.  (E.g. the
;;	unwinding code that handles a non-call exception may change
;;	lazily-bound GOT entries.)  We do this by making the
;;	exception_receiver and nonlocal_goto_receiver expanders emit
;;	a set_got_version instruction.
;;
;;    - After each call (lazily-bound or not), use a "ghost"
;;	update_got_version instruction to change the register's value.
;;	This instruction mimics the _possible_ effect of the dynamic
;;	resolver during the call and it remains live even if the call
;;	itself becomes dead.
;;
;;    - Leave GOT_VERSION_REGNUM out of all register classes.
;;	The register is therefore not a valid register_operand
;;	and cannot be moved to or from other registers.

(define_insn "load_call<mode>"
  [(set (match_operand:P 0 "register_operand" "=d")
	(unspec:P [(match_operand:P 1 "register_operand" "d")
		   (match_operand:P 2 "immediate_operand" "")
		   (reg:SI GOT_VERSION_REGNUM)] UNSPEC_LOAD_CALL))]
  "TARGET_USE_GOT"
  "<load>\t%0,%R2(%1)"
  [(set_attr "got" "load")
   (set_attr "mode" "<MODE>")])

(define_insn "set_got_version"
  [(set (reg:SI GOT_VERSION_REGNUM)
	(unspec_volatile:SI [(const_int 0)] UNSPEC_SET_GOT_VERSION))]
  "TARGET_USE_GOT"
  ""
  [(set_attr "type" "ghost")])

(define_insn "update_got_version"
  [(set (reg:SI GOT_VERSION_REGNUM)
	(unspec:SI [(reg:SI GOT_VERSION_REGNUM)] UNSPEC_UPDATE_GOT_VERSION))]
  "TARGET_USE_GOT"
  ""
  [(set_attr "type" "ghost")])

;; Sibling calls.  All these patterns use jump instructions.

;; call_insn_operand will only accept constant
;; addresses if a direct jump is acceptable.  Since the 'S' constraint
;; is defined in terms of call_insn_operand, the same is true of the
;; constraints.

;; When we use an indirect jump, we need a register that will be
;; preserved by the epilogue.  Since TARGET_ABICALLS forces us to use
;; t7 for this purpose, which the epilogue never clobbers, we may
;; as well use it in general.

(define_expand "sibcall"
  [(parallel [(call (match_operand 0 "")
		    (match_operand 1 ""))
	      (use (match_operand 2 ""))	;; next_arg_reg
	      (use (match_operand 3 ""))])]	;; struct_value_size_rtx
  ""
{
  mips_expand_call (MIPS_CALL_SIBCALL, NULL_RTX, XEXP (operands[0], 0),
		    operands[1], operands[2], false);
  DONE;
})

(define_insn "sibcall_internal"
  [(call (mem:SI (match_operand 0 "call_insn_operand" "j,S"))
	 (match_operand 1 "" ""))]
  "SIBLING_CALL_P (insn)"
  { return REG_P (operands[0]) ? "jr\t%0" : "j\t%0"; }
  [(set_attr "type" "call")])

(define_expand "sibcall_value"
  [(parallel [(set (match_operand 0 "")
		   (call (match_operand 1 "")
			 (match_operand 2 "")))
	      (use (match_operand 3 ""))])]		;; next_arg_reg
  ""
{
  mips_expand_call (MIPS_CALL_SIBCALL, operands[0], XEXP (operands[1], 0),
		    operands[2], operands[3], false);
  DONE;
})

(define_insn "sibcall_value_internal"
  [(set (match_operand 0 "register_operand" "")
        (call (mem:SI (match_operand 1 "call_insn_operand" "j,S"))
              (match_operand 2 "" "")))]
  "SIBLING_CALL_P (insn)"
  { return REG_P (operands[1]) ? "jr\t%1" : "j\t%1"; }
  [(set_attr "type" "call")])

(define_insn "sibcall_value_multiple_internal"
  [(set (match_operand 0 "register_operand" "")
        (call (mem:SI (match_operand 1 "call_insn_operand" "j,S"))
              (match_operand 2 "" "")))
   (set (match_operand 3 "register_operand" "")
	(call (mem:SI (match_dup 1))
	      (match_dup 2)))]
  "SIBLING_CALL_P (insn)"
  { return REG_P (operands[1]) ? "jr\t%1" : "j\t%1"; }
  [(set_attr "type" "call")])

(define_expand "call"
  [(parallel [(call (match_operand 0 "")
		    (match_operand 1 ""))
	      (use (match_operand 2 ""))	;; next_arg_reg
	      (use (match_operand 3 ""))])]	;; struct_value_size_rtx
  ""
{
  mips_expand_call (MIPS_CALL_NORMAL, NULL_RTX, XEXP (operands[0], 0),
		    operands[1], operands[2], false);
  DONE;
})

(define_insn "call_internal"
  [(call (mem:SI (match_operand 0 "call_insn_operand" "c,S"))
	 (match_operand 1 "" ""))
   (clobber (reg:SI RETURN_ADDR_REGNUM))]
  ""
  { return REG_P (operands[0]) ? "jalr\t%0" : "jal\t%0"; }
  [(set_attr "jal" "indirect,direct")])

;; A pattern for calls that must be made directly.  It is used for
;; MIPS16 calls that the linker may need to redirect to a hard-float
;; stub; the linker relies on the call relocation type to detect when
;; such redirection is needed.
(define_insn "call_internal_direct"
  [(call (mem:SI (match_operand 0 "const_call_insn_operand"))
	 (match_operand 1))
   (const_int 1)
   (clobber (reg:SI RETURN_ADDR_REGNUM))]
  ""
  "jal\t%0"
  [(set_attr "type" "call")])

(define_expand "call_value"
  [(parallel [(set (match_operand 0 "")
		   (call (match_operand 1 "")
			 (match_operand 2 "")))
	      (use (match_operand 3 ""))])]		;; next_arg_reg
  ""
{
  mips_expand_call (MIPS_CALL_NORMAL, operands[0], XEXP (operands[1], 0),
		    operands[2], operands[3], false);
  DONE;
})

;; See comment for call_internal.
(define_insn "call_value_internal"
  [(set (match_operand 0 "register_operand" "")
        (call (mem:SI (match_operand 1 "call_insn_operand" "c,S"))
              (match_operand 2 "" "")))
   (clobber (reg:SI RETURN_ADDR_REGNUM))]
  ""
  { return REG_P (operands[1]) ? "jalr\t%1" : "jal\t%1"; }
  [(set_attr "jal" "indirect,direct")])

;; See call_internal_direct.
(define_insn "call_value_internal_direct"
  [(set (match_operand 0 "register_operand")
        (call (mem:SI (match_operand 1 "const_call_insn_operand"))
              (match_operand 2)))
   (const_int 1)
   (clobber (reg:SI RETURN_ADDR_REGNUM))]
  ""
  "jal\t%1"
  [(set_attr "type" "call")])

;; See comment for call_internal.
(define_insn "call_value_multiple_internal"
  [(set (match_operand 0 "register_operand" "")
        (call (mem:SI (match_operand 1 "call_insn_operand" "c,S"))
              (match_operand 2 "" "")))
   (set (match_operand 3 "register_operand" "")
	(call (mem:SI (match_dup 1))
	      (match_dup 2)))
   (clobber (reg:SI RETURN_ADDR_REGNUM))]
  ""
  { return REG_P (operands[1]) ? "jalr\t%1" : "jal\t%1"; }
  [(set_attr "jal" "indirect,direct")])

;; Call subroutine returning any type.

(define_expand "untyped_call"
  [(parallel [(call (match_operand 0 "")
		    (const_int 0))
	      (match_operand 1 "")
	      (match_operand 2 "")])]
  ""
{
  int i;

  emit_call_insn (GEN_CALL (operands[0], const0_rtx, NULL, const0_rtx));

  for (i = 0; i < XVECLEN (operands[2], 0); i++)
    {
      rtx set = XVECEXP (operands[2], 0, i);
      mips_emit_move (SET_DEST (set), SET_SRC (set));
    }

  emit_insn (gen_blockage ());
  DONE;
})

(define_insn "mips_cache"
  [(set (mem:BLK (scratch))
	(unspec:BLK [(match_operand:SI 0 "const_int_operand")
		     (match_operand:QI 1 "address_operand" "p")]
		    UNSPEC_MIPS_CACHE))]
  "0"
  "cache\t%X0,%a1")

(define_insn "nop"
  [(const_int 0)]
  ""
  "nop"
  [(set_attr "type"	"nop")
   (set_attr "mode"	"none")])


(define_insn "align"
  [(unspec_volatile [(match_operand 0 "const_int_operand" "")] UNSPEC_ALIGN)]
  ""
  ".align\t%0"
  [(set (attr "length") (symbol_ref "(1 << INTVAL (operands[0])) - 1"))])

;; Synchronization instructions.

(include "sync.md")

(define_c_enum "unspec" [
  UNSPEC_ADDRESS_FIRST
])
