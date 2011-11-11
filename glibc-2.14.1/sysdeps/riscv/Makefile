ifeq ($(subdir),misc)
sysdep_headers += sys/asm.h sgidefs.h
endif

ifeq ($(subdir),rt)
librt-sysdep_routines += rt-sysdep
endif

ASFLAGS-.os += $(pic-ccflag)
