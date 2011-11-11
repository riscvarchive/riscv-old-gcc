/* Macros to support TLS testing in times of missing compiler support.  */

#include <sys/cdefs.h>
#include <sys/asm.h>

#define __STRING2(X) __STRING(X)
#define LW __STRING2(REG_L)

/* Load the GOT pointer, which may not be in $28 in a non-PIC
   (abicalls pic0) function.  */
#ifndef __PIC__
# define LOAD_GP "move %[tmp], gp\n\tla gp, __gnu_local_gp\n\t"
# define UNLOAD_GP "\n\tmove gp, %[tmp]"
#else
# define LOAD_GP
# define UNLOAD_GP
#endif

# define TLS_GD(x)					\
  ({ void *__result, *__tmp;				\
     extern void *__tls_get_addr (void *);		\
     asm ("addi %0, gp, %%tlsgd(" #x ")"	        \
	  UNLOAD_GP					\
	  : "=r" (__result), [tmp] "=&r" (__tmp));	\
     (int *)__tls_get_addr (__result); })
# define TLS_LD(x)					\
  ({ void *__result, *__tmp;				\
     extern void *__tls_get_addr (void *);		\
     asm (LOAD_GP "addi %0, gp, %%tlsldm(" #x ")"	\
	  UNLOAD_GP					\
	  : "=r" (__result), [tmp] "=&r" (__tmp));	\
     __result = __tls_get_addr (__result);		\
     asm ("lui %1,%%dtprel_hi(" #x ")\n\t"		\
	  "addi %1,%1,%%dtprel_lo(" #x ")\n\t"		\
	  "add %0,%0,%1"				\
	  : "+r" (__result), "=&r"(__tmp));		\
     __result; })
# define TLS_IE(x)					\
  ({ void *__result, *__tmp, *__tmp2;			\
     asm (LOAD_GP LW " %2,%%gottprel(" #x ")(gp)\n\t"	\
	  "add %0,tp,%2"				\
	  UNLOAD_GP					\
	  : "=r" (__result), [tmp] "=&r" (__tmp), "=&r" (__tmp2)); \
     __result; })
# define TLS_LE(x)					\
  ({ void *__result, *__tmp;				\
     asm ("lui %1,%%tprel_hi(" #x ")\n\t"		\
	  "addi %1,%1,%%tprel_lo(" #x ")\n\t"		\
	  "add %0,tp,%1"				\
	  : "=r" (__result), "=&r" (__tmp)		\
     __result; })
