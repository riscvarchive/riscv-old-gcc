#ifndef _RISCV_MACHINE_TYPES_H
#define _RISCV_MACHINE_TYPES_H

#include <machine/_default_types.h>

// these types keep struct stat in sync with x86-64 linux

#define __ino_t_defined
typedef unsigned long long __ino_t;

#define __dev_t_defined
typedef unsigned long long __dev_t;

#define __nlink_t_defined
typedef unsigned long long __nlink_t;

#define __mode_t_defined
typedef unsigned int __mode_t;

#define __uid_t_defined
typedef unsigned int __uid_t;

#define __gid_t_defined
typedef unsigned int __gid_t;

#define __off_t_defined
typedef unsigned long long _off_t;

#define __fpos_t_defined
typedef unsigned long long _fpos_t;

#define __time_t_defined
typedef long long time_t;

#endif
