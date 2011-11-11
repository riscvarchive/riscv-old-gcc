;;  Machine Description for MIPS based processor synchronization
;;  instructions.
;;  Copyright (C) 2007, 2008, 2009, 2010
;;  Free Software Foundation, Inc.

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

(define_c_enum "unspec" [
  UNSPEC_COMPARE_AND_SWAP
  UNSPEC_COMPARE_AND_SWAP_12
  UNSPEC_SYNC_OLD_OP
  UNSPEC_SYNC_NEW_OP
  UNSPEC_SYNC_NEW_OP_12
  UNSPEC_SYNC_OLD_OP_12
  UNSPEC_SYNC_EXCHANGE
  UNSPEC_SYNC_EXCHANGE_12
  UNSPEC_MEMORY_BARRIER
])

(define_code_iterator any_atomic [plus ior xor and])

;; Atomic memory operations.

(define_expand "memory_barrier"
  [(set (match_dup 0)
	(unspec:BLK [(match_dup 0)] UNSPEC_MEMORY_BARRIER))]
  ""
{
  operands[0] = gen_rtx_MEM (BLKmode, gen_rtx_SCRATCH (Pmode));
  MEM_VOLATILE_P (operands[0]) = 1;
})

(define_insn "*memory_barrier"
  [(set (match_operand:BLK 0 "" "")
	(unspec:BLK [(match_dup 0)] UNSPEC_MEMORY_BARRIER))]
  ""
  "fence")

(define_insn "sync_<optab>di"
  [(set (match_operand:DI 0 "memory_operand" "+YR")
	(unspec_volatile:DI
          [(any_atomic:DI (match_dup 0)
		     (match_operand:DI 1 "register_operand" "d"))]
	 UNSPEC_SYNC_OLD_OP))]
  "TARGET_64BIT"
  "amo<insn>.d zero,%1,%0")

(define_insn "sync_old_<optab>di"
  [(set (match_operand:DI 0 "register_operand" "=&d")
	(match_operand:DI 1 "memory_operand" "+YR"))
   (set (match_dup 1)
	(unspec_volatile:DI
          [(any_atomic:DI (match_dup 1)
		     (match_operand:DI 2 "register_operand" "d"))]
	 UNSPEC_SYNC_OLD_OP))]
  "TARGET_64BIT"
  "amo<insn>.d %0,%2,%1")

(define_insn "sync_lock_test_and_setdi"
  [(set (match_operand:DI 0 "register_operand" "=&d")
	(match_operand:DI 1 "memory_operand" "+YR"))
   (set (match_dup 1)
	(unspec_volatile:DI [(match_operand:DI 2 "register_operand" "d")]
	 UNSPEC_SYNC_EXCHANGE))]
  "TARGET_64BIT"
  "amoswap.d %0,%2,%1")

(define_insn "sync_<optab>si"
  [(set (match_operand:SI 0 "memory_operand" "+YR")
	(unspec_volatile:SI
          [(any_atomic:SI (match_dup 0)
		     (match_operand:SI 1 "register_operand" "d"))]
	 UNSPEC_SYNC_OLD_OP))]
  ""
  "amo<insn>.w zero,%1,%0")

(define_insn "sync_old_<optab>si"
  [(set (match_operand:SI 0 "register_operand" "=&d")
	(match_operand:SI 1 "memory_operand" "+YR"))
   (set (match_dup 1)
	(unspec_volatile:SI
          [(any_atomic:SI (match_dup 1)
		     (match_operand:SI 2 "register_operand" "d"))]
	 UNSPEC_SYNC_OLD_OP))]
  ""
  "amo<insn>.w %0,%2,%1")

(define_insn "sync_lock_test_and_setsi"
  [(set (match_operand:SI 0 "register_operand" "=&d")
	(match_operand:SI 1 "memory_operand" "+YR"))
   (set (match_dup 1)
	(unspec_volatile:SI [(match_operand:SI 2 "register_operand" "d")]
	 UNSPEC_SYNC_EXCHANGE))]
  ""
  "amoswap.w %0,%2,%1")
