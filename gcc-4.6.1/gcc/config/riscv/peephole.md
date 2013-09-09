;;........................
;; DI -> SI optimizations
;;........................

;; Simplify (int)(a + 1), etc.
(define_peephole2
  [(set (match_operand:DI 0 "register_operand")
	(match_operator:DI 4 "modular_operator"
	  [(match_operand:DI 1 "register_operand")
	   (match_operand:DI 2 "arith_operand")]))
   (set (match_operand:SI 3 "register_operand")
        (truncate:SI (match_dup 0)))]
  "TARGET_64BIT && (REGNO (operands[0]) == REGNO (operands[3]) || peep2_reg_dead_p (2, operands[0]))"
  [(set (match_dup 3)
          (truncate:SI
	     (match_op_dup:DI 4 
	       [(match_operand:DI 1 "register_operand")
		(match_operand:DI 2 "arith_operand")])))])

;; Simplify (int)a + 1, etc.
(define_peephole2
  [(set (match_operand:SI 0 "register_operand")
        (truncate:SI (match_operand:DI 1 "register_operand")))
   (set (match_operand:SI 3 "register_operand")
	(match_operator:SI 4 "modular_operator"
	  [(match_dup 0)
	   (match_operand:SI 2 "arith_operand")]))]
  "TARGET_64BIT && (REGNO (operands[0]) == REGNO (operands[3]) || peep2_reg_dead_p (2, operands[0]))"
  [(set (match_dup 3)
	(match_op_dup:SI 4 [(match_dup 1) (match_dup 2)]))])

;; Simplify -(int)a, etc.
(define_peephole2
  [(set (match_operand:SI 0 "register_operand")
        (truncate:SI (match_operand:DI 2 "register_operand")))
   (set (match_operand:SI 3 "register_operand")
	(match_operator:SI 4 "modular_operator"
	  [(match_operand:SI 1 "reg_or_0_operand")
	   (match_dup 0)]))]
  "TARGET_64BIT && (REGNO (operands[0]) == REGNO (operands[3]) || peep2_reg_dead_p (2, operands[0]))"
  [(set (match_dup 3)
	(match_op_dup:SI 4 [(match_dup 1) (match_dup 2)]))])
