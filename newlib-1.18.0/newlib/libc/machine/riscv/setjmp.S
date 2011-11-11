#ifdef __riscv64
# define STORE    sd
# define LOAD     ld
# define REGBYTES 8
#else
# define STORE    sw
# define LOAD     lw
# define REGBYTES 4
#endif

/* int setjmp (jmp_buf);  */
  .globl  setjmp
  .ent  setjmp
setjmp:

  fsd   fs0, 0*8(a0)
  fsd   fs1, 1*8(a0)
  fsd   fs2, 2*8(a0)
  fsd   fs3, 3*8(a0)
  fsd   fs4, 4*8(a0)
  fsd   fs5, 5*8(a0)
  fsd   fs6, 6*8(a0)
  fsd   fs7, 7*8(a0)
  fsd   fs8, 8*8(a0)
  fsd   fs9, 9*8(a0)

  mffsr t0

  STORE s0,  10*8+ 0*REGBYTES(a0)
  STORE s1,  10*8+ 1*REGBYTES(a0)
  STORE s2,  10*8+ 2*REGBYTES(a0)
  STORE s3,  10*8+ 3*REGBYTES(a0)
  STORE s4,  10*8+ 4*REGBYTES(a0)
  STORE s5,  10*8+ 5*REGBYTES(a0)
  STORE s6,  10*8+ 6*REGBYTES(a0)
  STORE s7,  10*8+ 7*REGBYTES(a0)
  STORE s8,  10*8+ 8*REGBYTES(a0)
  STORE s9,  10*8+ 9*REGBYTES(a0)
  STORE sp,  10*8+10*REGBYTES(a0)
  STORE tp,  10*8+11*REGBYTES(a0)
  STORE ra,  10*8+12*REGBYTES(a0)
  STORE t0,  10*8+13*REGBYTES(a0)

  li v0, 0
  ret

  .end  setjmp

/* volatile void longjmp (jmp_buf, int);  */
  .globl  longjmp
  .ent  longjmp
longjmp:

  fld   fs0, 0*8(a0)
  fld   fs1, 1*8(a0)
  fld   fs2, 2*8(a0)
  fld   fs3, 3*8(a0)
  fld   fs4, 4*8(a0)
  fld   fs5, 5*8(a0)
  fld   fs6, 6*8(a0)
  fld   fs7, 7*8(a0)
  fld   fs8, 8*8(a0)
  fld   fs9, 9*8(a0)

  LOAD  t0,  10*8+13*REGBYTES(a0)
  LOAD  s0,  10*8+ 0*REGBYTES(a0)
  LOAD  s1,  10*8+ 1*REGBYTES(a0)
  LOAD  s2,  10*8+ 2*REGBYTES(a0)
  LOAD  s3,  10*8+ 3*REGBYTES(a0)
  LOAD  s4,  10*8+ 4*REGBYTES(a0)
  LOAD  s5,  10*8+ 5*REGBYTES(a0)
  LOAD  s6,  10*8+ 6*REGBYTES(a0)
  LOAD  s7,  10*8+ 7*REGBYTES(a0)
  LOAD  s8,  10*8+ 8*REGBYTES(a0)
  LOAD  s9,  10*8+ 9*REGBYTES(a0)
  LOAD  sp,  10*8+10*REGBYTES(a0)
  LOAD  tp,  10*8+11*REGBYTES(a0)
  LOAD  ra,  10*8+12*REGBYTES(a0)

  mtfsr t0

  sltiu v0, a1, 1    # v0 = (a1 == 0)
  add   v0, v0, a1   # v0 = (a1 == 0) ? 1 : a1
  ret

  .end longjmp
