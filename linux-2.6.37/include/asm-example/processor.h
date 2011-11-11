#ifndef _ASM_X86_PROCESSOR_H
#define _ASM_X86_PROCESSOR_H

//#include <asm/processor-flags.h>

/* Forward declaration, a strange C thing */
struct task_struct;
struct mm_struct;

//#include <asm/vm86.h>
//#include <asm/math_emu.h>
#include <asm/segment.h>
#include <asm/types.h>
#include <asm/sigcontext.h>
//#include <asm/current.h>
//#include <asm/cpufeature.h>
#include <asm/system.h>
//#include <asm/page.h>
#include <asm/percpu.h>
//#include <asm/msr.h>
//#include <asm/desc_defs.h>
//#include <asm/nops.h>
//#include <asm/ds.h>

#include <linux/personality.h>
#include <linux/cpumask.h>
#include <linux/cache.h>
#include <linux/threads.h>
#include <linux/init.h>

/*
 * Default implementation of macro that returns current
 * instruction pointer ("program counter").
 */
static inline void *current_text_addr(void)
{
	void *pc;

	asm volatile("mov $1f, %0; 1:":"=r" (pc));

	return pc;
}

# define ARCH_MIN_TASKALIGN		16
# define ARCH_MIN_MMSTRUCT_ALIGN	0

#ifdef CONFIG_SMP
DECLARE_PER_CPU(struct cpuinfo_x86, cpu_info);
#define cpu_data(cpu)		per_cpu(cpu_info, cpu)
#define current_cpu_data	__get_cpu_var(cpu_info)
#else
#define cpu_data(cpu)		boot_cpu_data
#define current_cpu_data	boot_cpu_data
#endif

struct thread_struct {
	struct pt_regs		*uregs;		/* userspace register frame */
	unsigned long		pc;		/* kernel PC */
	unsigned long		sp;		/* kernel SP */
	unsigned long		sp0;		/* kernel SP */
	unsigned long		a3;		/* kernel FP */
	unsigned long		wchan;
	unsigned long		usp;
	struct pt_regs		*__frame;

	/* Cached TLS descriptors: */
#if 0
	struct desc_struct	tls_array[GDT_ENTRY_TLS_ENTRIES];
	unsigned long		sp0;
	unsigned long		sp;
#ifdef CONFIG_X86_32
	unsigned long		sysenter_cs;
#else
	unsigned long		usersp;	/* Copy from PDA */
	unsigned short		es;
	unsigned short		ds;
	unsigned short		fsindex;
	unsigned short		gsindex;
#endif
	unsigned long		ip;
	unsigned long		fs;
	unsigned long		gs;
	/* Hardware debugging registers: */
	unsigned long		debugreg0;
	unsigned long		debugreg1;
	unsigned long		debugreg2;
	unsigned long		debugreg3;
	unsigned long		debugreg6;
	unsigned long		debugreg7;
	/* Fault info: */
	unsigned long		cr2;
	unsigned long		trap_no;
	unsigned long		error_code;
	/* floating point and extended processor state */
	union thread_xstate	*xstate;
#ifdef CONFIG_X86_32
	/* Virtual 86 mode info */
	struct vm86_struct __user *vm86_info;
	unsigned long		screen_bitmap;
	unsigned long		v86flags;
	unsigned long		v86mask;
	unsigned long		saved_sp0;
	unsigned int		saved_fs;
	unsigned int		saved_gs;
#endif
	/* IO permissions: */
	unsigned long		*io_bitmap_ptr;
	unsigned long		iopl;
	/* Max allowed port in the bitmap, in bytes: */
	unsigned		io_bitmap_max;
/* MSR_IA32_DEBUGCTLMSR value to switch in if TIF_DEBUGCTLMSR is set.  */
	unsigned long	debugctlmsr;
#ifdef CONFIG_X86_DS
/* Debug Store context; see include/asm-x86/ds.h; goes into MSR_IA32_DS_AREA */
	struct ds_context	*ds_ctx;
#endif /* CONFIG_X86_DS */
#ifdef CONFIG_X86_PTRACE_BTS
/* the signal to send on a bts buffer overflow */
	unsigned int	bts_ovfl_signal;
#endif /* CONFIG_X86_PTRACE_BTS */
#endif
};

typedef struct {
	unsigned long		seg;
} mm_segment_t;


/*
 * create a kernel thread without removing it from tasklists
 */
extern int kernel_thread(int (*fn)(void *), void *arg, unsigned long flags);

/* Free all resources held by a thread. */
static inline void release_thread(struct task_struct *task)
{
}

static inline void exit_thread(void)
{
}

/* Prepare to copy thread state - unlazy all lazy state */
#define prepare_to_copy(tsk) do { } while (0)

unsigned long get_wchan(struct task_struct *p);

extern void cpu_relax(void);

extern unsigned long thread_saved_pc(struct task_struct *tsk);


/*
 * User space process size. 47bits minus one guard page.
 */
#define TASK_SIZE64	((1UL << 47) - PAGE_SIZE)

/* This decides where the kernel will search for a free chunk of vm
 * space during mmap's.
 */
#define TASK_SIZE		PAGE_OFFSET
#define TASK_SIZE_OF(child)	PAGE_OFFSET

#define STACK_TOP		TASK_SIZE
#define STACK_TOP_MAX		TASK_SIZE64

#define INIT_THREAD  { \
}

#define INIT_TSS  { \
}

/*
 * Return saved PC of a blocked thread.
 * What is this good for? it will be always the scheduler or ret_from_fork.
 */

#define task_pt_regs(tsk)	((struct pt_regs *)(tsk)->thread.sp0 - 1)
#define KSTK_ESP(tsk)		-1 /* sorry. doesn't work for syscall. */

extern void start_thread(struct pt_regs *regs, unsigned long new_ip,
					       unsigned long new_sp);

/*
 * This decides where the kernel will search for a free chunk of vm
 * space during mmap's.
 */
#define TASK_UNMAPPED_BASE	(PAGE_ALIGN(TASK_SIZE / 3))

#define KSTK_EIP(task)		(task_pt_regs(task)->ip)

#endif /* _ASM_X86_PROCESSOR_H */
