.PHONY: all clean veryclean help test

F_EXTRA_GF  := -fPIC -fimplicit-none -fdefault-real-8 -ffree-line-length-200 -Wextra
F_EXTRA_IFX := -fPIC -r8

foptions.mk: generate_fopts.tcl
	tclsh9.1 $<

include foptions.mk

LIB     := $(ODIR)/libavd.a

all: foptions.mk $(LIB)

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
	@rm -vf $(ODIR)/*.o $(ODIR)/*.mod $(ODIR)/*.smod *~

veryclean: clean
	@rm -vf $(LIB) av_utest$(EXT)
	@rm -vfr $(ODIR)

help:
	@echo "Targets : all, test, clean, veryclean"
	@echo "Options : F=gfortran|ifx  debug=1"
	@echo "ODIR    = $(ODIR)"
	@echo "F_OPTS  = $(F_OPTS)"
