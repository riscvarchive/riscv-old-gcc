#ifndef __ASM_GENERIC_TIMEX_H
#define __ASM_GENERIC_TIMEX_H

/*
 * CLOCK_TICK_RATE is highly PC-specific and should not
 * be used in portable code. 1193182 is the value for the
 * original i8253 PIC.
 */
#ifndef CLOCK_TICK_RATE
#define CLOCK_TICK_RATE		1193182
#endif

/*
 * If you have a cycle counter, return the value here.
 */
typedef unsigned long cycles_t;
static inline cycles_t get_cycles(void)
{
	return 0;
}

/*
 * Architectures are encouraged to implement read_current_timer
 * and define this in order to avoid the expensive delay loop
 * calibration during boot.
 */
#undef ARCH_HAS_READ_CURRENT_TIMER

#endif /* __ASM_GENERIC_TIMEX_H */
