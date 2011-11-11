#include <sysdep.h>
#include <sys/asm.h>
#include <sys/ucontext.h>
#include <stdarg.h>

void __makecontext (ucontext_t *ucp, void (*func) (), int argc, ...)
{
  extern void __start_context(void);
  int i, reg_args;
  long sp;
  va_list vl;
  va_start(vl, argc);

  /* Store a magic flag in x0's save slot */
  ucp->uc_mcontext.gregs[0] = 1;

  /* Set up the stack. */
  sp = ((long)ucp->uc_stack.ss_sp + ucp->uc_stack.ss_size) & ALMASK;
  reg_args = argc < 8 ? argc : 8;
  sp = (sp - (argc - reg_args) * sizeof(long)) & ALMASK;

  /* Put args in a0-a7, then put any remaining args on the stack. */
  for (i = 0; i < reg_args; i++)
    ucp->uc_mcontext.gregs[4 + i] = va_arg(vl, long);
  for (i = 0; i < argc - reg_args; i++)
    ((long*)sp)[i] = va_arg(vl, long);

  ucp->uc_mcontext.gregs[1] = (long)&__start_context;
  ucp->uc_mcontext.gregs[20] = (long)ucp->uc_link;
  ucp->uc_mcontext.gregs[30] = sp;
  ucp->uc_mcontext.pc = (long)func;
}

weak_alias (__makecontext, makecontext)
