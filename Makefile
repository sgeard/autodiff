.PHONY: all clean veryclean help test

# Compiler selection (default: gfortran)
F ?= gfortran

# Build type: use 'make debug=1' for debug build
ifdef debug
  BUILD := debug
  EXT   := _d
else
  BUILD := release
  EXT   :=
endif

# --- Compiler-specific settings ---

ifeq ($(F),ifx)
  ODIR     := obj_intel_$(BUILD)
  MOD_OPTS := -module $(ODIR) -I$(ODIR)
  F_BASE   := -stand f23 -fPIC -fpp -r8
  ifdef debug
    F_BUILD := -g -debug-parameters -O0 -check all -warn all
  else
    F_BUILD := -O3 -fp-model precise -fprotect-parens -xHost -warn all
  endif
  F_LOPTS  :=
else
  ODIR     := obj_$(F)_$(BUILD)
  MOD_OPTS := -J$(ODIR) -I$(ODIR)
  F_BASE   := -fPIC -cpp -fimplicit-none -fdefault-real-8 -ffree-line-length-200 -Wall -Wextra
  ifdef debug
    F_BUILD := -ggdb -fbounds-check -ffpe-trap=denormal,invalid
  else
    F_BUILD := -O3
  endif
  F_LOPTS  :=
endif

F_OPTS  := $(F_BASE) $(F_BUILD) $(MOD_OPTS)
LIB     := $(ODIR)/libavd.a

SRC     := src/avd.f90 src/avd_sm.f90 src/avd_functions.f90
OBJ     := $(SRC:src/%.f90=$(ODIR)/%.o)

all: $(LIB)

test: $(LIB) test/av_utest.f90 | $(ODIR)
	$(F) $(F_OPTS) -o av_utest$(EXT) test/av_utest.f90 $(LIB) $(F_LOPTS)
	./av_utest$(EXT)

$(LIB): $(OBJ) | $(ODIR)
	ar crv $@ $^

$(ODIR)/%.o: src/%.f90 | $(ODIR)
	$(F) -c $(F_OPTS) -o $@ $<

$(ODIR):
	mkdir -p $@

clean:
	@rm -vf $(ODIR)/*.o $(ODIR)/*.mod $(ODIR)/*.smod *~

veryclean: clean
	@rm -vf $(LIB) av_utest$(EXT)
	@rm -vfr $(ODIR)

help:
	@echo "Targets : all, test, clean, veryclean"
	@echo "Options : F=gfortran|ifx  debug=1"
	@echo "ODIR    = $(ODIR)"
	@echo "F_OPTS  = $(F_OPTS)"
