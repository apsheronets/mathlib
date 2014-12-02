SOURCES=genops.mli genops.ml numref.mli numref.ml math.mli math.ml constant.mli \
	constant.ml float.mli float.ml complexer.mli complexer.ml \
        mersenne.mli mersenne.ml rand.mli rand.ml vector.mli vector.ml stubs.c
RESULT=mathlib


# If on a system without dynamic linking (Such as Mac OS X), comment this out
export NO_CUSTOM=1
# and uncomment this
# STATIC=1


ML_SOURCES=$(filter %.ml, $(SOURCES))
MLI_SOURCES=$(filter %.mli, $(SOURCES))

LIBINSTALL_FILES=$(RESULT).cma $(RESULT).cmxa $(MLI_SOURCES) \
	$(ML_SOURCES:.ml=.cmx) $(MLI_SOURCES:.mli=.cmi) lib$(RESULT)_stubs.a \
	$(RESULT).a dll$(RESULT)_stubs.so

CC=gcc
RANLIB=ranlib
CFLAGS=-g -O2 -fPIC -DPIC

OCAMLDOCFLAGS=-sort

all: byte-code-library 
opt: native-code-library
install: libinstall
uninstall: libuninstall

-include OCamlMakefile
