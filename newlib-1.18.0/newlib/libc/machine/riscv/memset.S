# memset, optimized for RISC-V
# Andrew Waterman
# 2012/10/15

.text
.ent memset
.global memset
memset:
  li t3, 7
  move v0, a0
  bleu a2, t3, small
  and t2, a0, 7
  bnez t2, misaligned

aligned:
  bnez a1, wordify

aligned_and_wordified:
  and t1, a2, 8
  and t2, a2, -8
  add t0, a0, t2
  bnez t1, aligned8

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
  bltu a0, t0, 1b

  bne t2, a2, tail
  ret

aligned8:
#ifdef __riscv64
  sd a1, 0(a0)
#else
  sw a1, 0(a0)
  sw a1, 4(a0)
#endif
  sd a1, 0(a0)
  add a0, a0, 8
  bltu a0, t0, 1b

  bne t2, a2, tail
  ret

wordify:
  and a1, a1, 0xFF
  sll t0, a1, 8
  or  a1, a1, t0
  sll t0, a1, 16
  or  a1, a1, t0
#ifdef __riscv64
  sll t0, a1, 32
  or  a1, a1, t0
#endif
  j aligned_and_wordified

tail:
  sub a2, a2, t2
small:
  sub t0, t3, a2
  sll t0, t0, 2
  rdnpc t1
.set push
.set norvc
  add t0, t0, t1
  jalr.j x0, t0, 8

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
  sll t0, t2, 2
  rdnpc t1
.set push
.set norvc
  add t0, t0, t1
  jalr.j x0, t0, 4

  sb a1, 6(a0)
  sb a1, 5(a0)
  sb a1, 4(a0)
  sb a1, 3(a0)
  sb a1, 2(a0)
  sb a1, 1(a0)
  sb a1, 0(a0)
  add t2, t2, -8
  sub a0, a0, t2
  add a2, a2, t2
  bleu a2, t3, small
  j aligned
.set pop
.end memset
