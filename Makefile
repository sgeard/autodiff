.PHONY: all clean veryclean help test
.DEFAULT_GOAL := all

F_EXTRA_GF  := -fPIC -fimplicit-none -ffree-line-length-200 -Wextra
F_EXTRA_IFX := -fPIC

# --- Compiler selection: default ifx (release), validated ------------------
F ?= ifx
VALID_F := gfortran ifx lfortran flang
ifeq ($(filter $(F),$(VALID_F)),)
  $(error Unknown Fortran compiler 'F=$(F)' -- choose one of: $(VALID_F))
endif

# Canonical compiler options, generated into foptions_$(F).mk by generate_fopts.tcl
OPTIONS_FNAME := foptions_$(F).mk
$(OPTIONS_FNAME): generate_fopts.tcl
	tclsh generate_fopts.tcl $(F) $(OPTIONS_FNAME)

-include $(OPTIONS_FNAME)

LIB     := $(ODIR)/libavd.a

all: $(OPTIONS_FNAME) $(LIB)

SRC     := src/avd.f90 src/avd_sm.f90 src/avd_functions.f90
OBJ     := $(SRC:src/%.f90=$(ODIR)/%.o)

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
	@rm -vf $(ODIR)/*.o $(ODIR)/*.mod $(ODIR)/*.smod *~ foptions_*.mk

veryclean: clean
	@rm -vf $(LIB) av_utest$(EXT)
	@rm -vfr $(ODIR)

help:
	@echo "Targets : all, test, clean, veryclean"
	@echo "Options : F=gfortran|ifx|lfortran|flang (default ifx)  debug=1"
	@echo "ODIR    = $(ODIR)"
	@echo "F_OPTS  = $(F_OPTS)"
