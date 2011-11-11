#ifndef _MACHINE_SYSCALL_H
#define _MACHINE_SYSCALL_H

#define	SYS_exit	1
#define	SYS_getpid	20
#define	SYS_kill	37
#define	SYS_read	3
#define	SYS_write	4
#define	SYS_open	5
#define	SYS_close	6
#define	SYS_lseek	19
#define	SYS_brk		17
#define	SYS_link	9
#define	SYS_unlink	10
#define	SYS_chdir	12
#define SYS_stat	18
#define SYS_fstat	28
#define	SYS_lstat	84
#define	SYS_pread 180
#define	SYS_pwrite 181
#define SYS_getmainvars 201

typedef struct
{
  long result;
  long err;
} sysret_t;

static inline sysret_t
__internal_syscall(long n, long _a0, long _a1, long _a2, long _a3)
{
  register long result asm("v0");
  register long err asm("v1");
  register long a0 asm("a0") = _a0;
  register long a1 asm("a1") = _a1;
  register long a2 asm("a2") = _a2;
  register long a3 asm("a3") = _a3;

  result = n;
  asm volatile ("syscall"
                    : "=r"(result),"=r"(err)
                    : "0"(result),"r"(a0),"r"(a1),"r"(a2),"r"(a3));

  sysret_t s;
  s.result = result;
  s.err = err;
  return s;
}

#endif
