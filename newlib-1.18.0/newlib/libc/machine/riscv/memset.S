# memset, optimized for RISC-V
# Andrew Waterman
# 2012/10/15

.text
.global memset
memset:
  li a6, 7
  move v0, a0
  bleu a2, a6, small
  and a5, a0, 7
  bnez a5, misaligned

aligned:
  bnez a1, wordify

aligned_and_wordified:
  and a4, a2, 8
  and a5, a2, -8
  add a3, a0, a5
  bnez a4, aligned8

#ifdef __riscv64
1:sd a1, 0(a0)
  sd a1, 8(a0)
#else
1:sw a1, 0(a0)
  sw a1, 4(a0)
  sw a1, 8(a0)
  sw a1, 12(a0)
#endif
  add a0, a0, 16
  bltu a0, a3, 1b

  bne a5, a2, tail
  ret

aligned8:
#ifdef __riscv64
  sd a1, 0(a0)
#else
  sw a1, 0(a0)
  sw a1, 4(a0)
#endif
  add a0, a0, 8
  bltu a0, a3, 1b

  bne a5, a2, tail
  ret

wordify:
  and a1, a1, 0xFF
  sll a3, a1, 8
  or  a1, a1, a3
  sll a3, a1, 16
  or  a1, a1, a3
#ifdef __riscv64
  sll a3, a1, 32
  or  a1, a1, a3
#endif
  j aligned_and_wordified

tail:
  sub a2, a2, a5
small:
  sub a3, a6, a2
  sll a3, a3, 2
.set push
.set norvc
  rdpc a4
  add a3, a3, a4
  jalr.j x0, a3, 12

  sb a1, 6(a0)
  sb a1, 5(a0)
  sb a1, 4(a0)
  sb a1, 3(a0)
  sb a1, 2(a0)
  sb a1, 1(a0)
  sb a1, 0(a0)
  ret
.set pop

misaligned:
  sll a3, a5, 2
.set push
.set norvc
  rdpc a4
  add a3, a3, a4
  jalr.j x0, a3, 8

  sb a1, 6(a0)
  sb a1, 5(a0)
  sb a1, 4(a0)
  sb a1, 3(a0)
  sb a1, 2(a0)
  sb a1, 1(a0)
  sb a1, 0(a0)
  add a5, a5, -8
  sub a0, a0, a5
  add a2, a2, a5
  bleu a2, a6, small
  j aligned
.set pop
