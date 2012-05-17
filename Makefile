#!/bin/bash

BINUTILS_VERSION := 2.21.1
GCC_VERSION := 4.6.1
GLIBC_VERSION := 2.14.1
NEWLIB_VERSION := 1.18.0
LINUX_VERSION := 2.6.37

LINUX_DIR := $(CURDIR)/linux-$(LINUX_VERSION)

MAKE_JOBS := 16

# Check that INSTALL_DIR was specified on the command line or in Makelocal
-include Makelocal
ifndef INSTALL_DIR
busted: 
	@echo -e "\
You need to set INSTALL_DIR to point to where you want the tools \n\
installed. The best way to do this is to create a 'Makelocal' file\n\
in the current directory and put it in there.  Alternatively, specify\n\
it on the command line, like so:\n\
\n\
    make INSTALL_DIR=/your/install/path\n\
"; false
newlib: busted
riscv: busted
endif

# Check that we have gawk installed, rather than mawk etc.
AWK := $(shell awk -W version 2>&1 | head -n 1 | awk '{print $$1}')
ifneq ($(AWK),GNU)
$(error You need to have 'gawk' installed on your system!)
endif

# Check that we have the GMP, MPFR, and MPC libraries installed
GMP_OK := $(shell echo "int main() {return 0;}" | gcc -x c - -lgmp -lmpfr -lmpc -o /dev/null > /dev/null && echo OK)
ifneq ($(GMP_OK),OK)
$(error You need to have the libraries GMP, MPFR, and MPC installed on your system!)
endif

newlib: build-gcc-newlib

linux: build-gcc-linux-stage2

build-binutils-newlib:
	mkdir $@
	cd $@ && ../binutils-$(BINUTILS_VERSION)/configure \
		--target=riscv-elf \
		--program-prefix=riscv- \
		--prefix=$(INSTALL_DIR) \
		--enable-shared \
		--enable-tls \
		--enable-languages=c \
		--with-newlib \
		--disable-multilib
	$(MAKE) -C $@ -j $(MAKE_JOBS)
	$(MAKE) -C $@ -j $(MAKE_JOBS) install

build-gcc-newlib-src: build-binutils-newlib
	cp -r gcc-$(GCC_VERSION) $@
	cp -r newlib-$(NEWLIB_VERSION)/newlib $@
	cp -r newlib-$(NEWLIB_VERSION)/libgloss $@

build-gcc-newlib: build-gcc-newlib-src
	mkdir $@
	cd $@ && ../$</configure \
		--target=riscv-elf \
		--program-prefix=riscv- \
		--prefix=$(INSTALL_DIR) \
		--disable-shared \
		--disable-threads \
		--enable-tls \
		--enable-languages=c,c++ \
		--with-newlib \
		--disable-libmudflap \
		--disable-libssp \
		--disable-libquadmath \
		--disable-libgomp \
		--disable-nls \
		--disable-multilib
	$(MAKE) -C $@ -j $(MAKE_JOBS) inhibit-libc=true
	$(MAKE) -C $@ -j $(MAKE_JOBS) install

build-binutils-linux:
	mkdir $@
	cd $@ && ../binutils-$(BINUTILS_VERSION)/configure \
		--target=riscv-linux \
		--prefix=$(INSTALL_DIR) \
		--enable-shared \
		--enable-tls \
		--enable-languages=c \
		--with-newlib \
		--disable-multilib
	$(MAKE) -C $@ -j $(MAKE_JOBS)
	$(MAKE) -C $@ -j $(MAKE_JOBS) install

build-gcc-linux-stage1: build-binutils-linux
	mkdir $@
	cd $@ && ../gcc-$(GCC_VERSION)/configure \
		--target=riscv-linux \
		--prefix=$(INSTALL_DIR) \
		--enable-shared \
		--disable-threads \
		--enable-tls \
		--enable-languages=c \
		--with-newlib \
		--disable-libmudflap \
		--disable-libssp \
		--disable-libquadmath \
		--disable-libgomp \
		--disable-nls \
		--disable-multilib \
		--with-headers=$(LINUX_DIR)/include
	-$(MAKE) -C $@ -j $(MAKE_JOBS) inhibit-libc=true
	$(MAKE) -C $@ -j $(MAKE_JOBS) install

build-glibc-linux: build-gcc-linux-stage1
	mkdir $@
	cd $@ && $(CURDIR)/glibc-$(GLIBC_VERSION)/configure \
		riscv-linux \
		--target=riscv-linux \
		--prefix=$(INSTALL_DIR)/riscv-linux \
		libc_cv_forced_unwind=yes \
		libc_cv_c_cleanup=yes \
		--enable-shared \
		--enable-__thread \
		--disable-multilib \
		--with-headers=$(LINUX_DIR)/include
	$(MAKE) -C $@ -j $(MAKE_JOBS) cross-compiling=yes
	$(MAKE) -C $@ -j $(MAKE_JOBS) install cross-compiling=yes

build-gcc-linux-stage2: build-glibc-linux
	mkdir $@
	cd $@ && ../gcc-$(GCC_VERSION)/configure \
		--target=riscv-linux \
		--prefix=$(INSTALL_DIR) \
		--enable-shared \
		--disable-threads \
		--enable-tls \
		--enable-languages=c,c++ \
		--disable-libmudflap \
		--disable-libssp \
		--disable-libquadmath \
		--disable-libgomp \
		--disable-nls \
		--disable-multilib \
		--with-headers=$(LINUX_DIR)/include
	$(MAKE) -C $@ -j $(MAKE_JOBS)
	$(MAKE) -C $@ -j $(MAKE_JOBS) install

clean:
	rm -rf build-* *-*-riscv.patch

patches:
	git diff 68ad6027997289e55963bee1c60fcf6aeec2dafd -- binutils-$(BINUTILS_VERSION) | sed 's/^+++ b\/binutils-/+++ binutils-/' > binutils-$(BINUTILS_VERSION)-riscv.patch
	git diff 68ad6027997289e55963bee1c60fcf6aeec2dafd -- gcc-$(GCC_VERSION) | sed 's/^+++ b\/gcc-/+++ gcc-/' > gcc-$(GCC_VERSION)-riscv.patch
	git diff 68ad6027997289e55963bee1c60fcf6aeec2dafd -- glibc-$(GLIBC_VERSION) | sed 's/^+++ b\/glibc-/+++ glibc-/' > glibc-$(GLIBC_VERSION)-riscv.patch
