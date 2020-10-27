.PHONY: all clean help run

ifdef release
    OBJ_DIR_SUFF := _release
else
    OBJ_DIR_SUFF := _debug
endif

ifdef intel
    ODIR := obj_intel$(OBJ_DIR_SUFF)
    F90 := ifort
    F90_OPTS := -fPIC -fpp -r8 -module $(ODIR)
    ifdef release
        F90_OPTS_EXTRA := #-fp-model precise -fprotect-parens -xHost -prec-sqrt -qopenmp-simd -qopenmp -stand f08
        F90_OPTS += -O3 $(F90_OPTS_EXTRA) -warn all
        ARCH_NAME := build-intel-release.tgz
    else
        F90_OPTS += -D_DEBUG -g -check bounds -warn all -debug-parameters used -traceback
        ARCH_NAME := build-intel-debug.tgz
    endif
    LINK_OPTS := -static-intel
else
    ODIR := obj_gfortran$(OBJ_DIR_SUFF)
    F90 := gfortran
    F90_OPTS := -fPIC -cpp -std=f2018 -fimplicit-none -fdefault-real-8 -ffree-line-length-200 -Wall -Wextra -J$(ODIR)
    ifdef release
        F90_OPTS += -O3
        ARCH_NAME := build-gfortran-release.tgz
    else
        F90_OPTS += -D_DEBUG -W -ggdb -fbounds-check -ffpe-trap=denormal,invalid
        ARCH_NAME := build-gfortran-debug.tgz
    endif
    LINK_OPTS :=
endif

all: $(ODIR) avd

debug: avd
	gdb avd -x gdb_comm

run: avd
	avd

avd: $(ODIR) avd.f90 avd_sm.f90 av_utest.f90 Makefile
	$(F90) $(F90_OPTS) avd.f90 avd_sm.f90 av_utest.f90 -o $@

$(ODIR):
	mkdir -p $(ODIR)

clean:
	@rm -vrf $(ODIR) *~ $(ARCH_NAME) *.{mod,smod,o} avd
	
help:
	@echo "SRC = $(SRC)"
	@echo "OBJ = $(OBJ)"
