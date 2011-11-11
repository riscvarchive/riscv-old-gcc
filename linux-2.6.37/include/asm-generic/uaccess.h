#ifndef __ASM_GENERIC_UACCESS_H
#define __ASM_GENERIC_UACCESS_H

/*
 * User space memory access functions, these should work
 * on a ny machine that has kernel and user data in the same
 * address space, e.g. all NOMMU machines.
 */
#include <linux/sched.h>
#include <linux/mm.h>
#include <linux/string.h>

#include <asm/segment.h>

#ifndef get_fs
#define MAKE_MM_SEG(s)	((mm_segment_t) { (s) })
#define KERNEL_DS	MAKE_MM_SEG(~0UL)
#define USER_DS		MAKE_MM_SEG(TASK_SIZE - 1)

#define get_ds() (KERNEL_DS)
#define get_fs() (current_thread_info()->addr_limit)

static inline void set_fs(mm_segment_t fs)
{
	current_thread_info()->addr_limit = fs;
}
#endif

#define segment_eq(a, b) ((a).seg == (b).seg)

#define VERIFY_READ	0
#define VERIFY_WRITE	1

#define access_ok(type, addr, size) _access_ok((unsigned long)(addr),(size))

/*
 * The architecture should really override this if possible, at least
 * doing a check on the get_fs()
 */
#ifndef _access_ok
static inline int _access_ok(unsigned long addr, unsigned long size)
{
	return 1;
}
#endif

/*
 * The exception table consists of pairs of addresses: the first is the
 * address of an instruction that is allowed to fault, and the second is
 * the address at which the program should continue.  No registers are
 * modified, so it is entirely up to the continuation code to figure out
 * what to do.
 *
 * All the routines below use bits of fixup code that are out of line
 * with the main instruction path.  This means when everything is well,
 * we don't even have to jump over them.  Further, they do not intrude
 * on our cache or tlb entries.
 */

struct exception_table_entry
{
	unsigned long insn, fixup;
};

/* Returns 0 if exception not found and fixup otherwise.  */
extern unsigned long search_exception_table(unsigned long);

/*
 * These are the main single-value transfer routines.  They automatically
 * use the right size if we just have the right pointer type.
 */
#ifndef __put_user
#define __put_user(x, ptr)				\
({							\
	int __pu_err = 0;				\
	typeof(*(ptr)) __pu_val = (x);			\
	switch (sizeof (*(ptr))) {			\
	case 1:						\
	case 2:						\
	case 4:						\
		*(ptr) = (__pu_val);			\
		break;					\
	case 8:						\
		memcpy(ptr, &__pu_val, sizeof (*(ptr)));\
		break;					\
	default:					\
		__pu_err = __put_user_bad();		\
		break;					\
	}						\
	__pu_err;					\
})
extern int __put_user_bad(void);
#endif

#define put_user(x, ptr) (				\
	access_ok(VERIFY_WRITE, ptr, sizeof (*ptr)) ?	\
		__put_user(x, ptr) :			\
		-EFAULT)

#ifndef __get_user
#define __get_user(x, ptr)				\
({							\
	int __gu_err = 0;				\
	unsigned long __gu_val = (unsigned long)*ptr;	\
	switch (sizeof(*(ptr))) {			\
	case 1:						\
	case 2:						\
	case 4:						\
	case 8:						\
		break;					\
	default:					\
		__gu_err = __get_user_bad();		\
		__gu_val = 0;				\
		break;					\
	}						\
	(x) = (typeof(*ptr))__gu_val;			\
	__gu_err;					\
})
extern int __get_user_bad(void);
#endif

#define get_user(x, ptr) (				\
	access_ok(VERIFY_READ, ptr, sizeof (*ptr)) ?	\
		__get_user(x, ptr) :			\
		-EFAULT)

#define __copy_from_user(to, from, n) (memcpy(to, from, n), 0)
#define __copy_to_user(to, from, n) (memcpy(to, from, n), 0)
#define __copy_to_user_inatomic __copy_to_user
#define __copy_from_user_inatomic __copy_from_user

#define copy_to_user_ret(to,from,n,retval) ({ if (copy_to_user(to,from,n)) return retval; })

#define copy_from_user_ret(to,from,n,retval) ({ if (copy_from_user(to,from,n)) return retval; })

static inline long copy_from_user(void *to,
				  const void __user * from, unsigned long n)
{
	if (access_ok(VERIFY_READ, from, n))
		__copy_from_user(to, from, n);
	else
		return n;
	return 0;
}

static inline long copy_to_user(void *to,
				const void __user * from, unsigned long n)
{
	if (access_ok(VERIFY_WRITE, to, n))
		__copy_to_user(to, from, n);
	else
		return n;
	return 0;
}

/*
 * Copy a null terminated string from userspace.
 */

static inline long
strncpy_from_user(char *dst, const char *src, long count)
{
	char *tmp;
	if (!access_ok(VERIFY_READ, src, 1))
		return -EFAULT;
	strncpy(dst, src, count);
	for (tmp = dst; *tmp && count > 0; tmp++, count--)
		;
	return tmp - dst;
}

/*
 * Return the size of a string (including the ending 0)
 *
 * Return 0 on exception, a value greater than N if too long
 */
static inline long strnlen_user(const char *src, long n)
{
	return strlen(src) + 1;
}

#define strlen_user(str) strnlen_user(str, 32767)

/*
 * Zero Userspace
 */

static inline unsigned long
__clear_user(void *to, unsigned long n)
{
	memset(to, 0, n);
	return 0;
}

#define clear_user(to, n) __clear_user(to, n)

#endif /* __ASM_GENERIC_UACCESS_H */
