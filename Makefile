#######################################################################
#                                                                     #
#                            OCamlSpotter                             #
#                                                                     #
#                             Jun FURUSE                              #
#                                                                     #
#   Copyright 2008-2012 Jun Furuse. All rights reserved.              #
#   This file is distributed under the terms of the GNU Library       #
#   General Public License, with the special exception on linking     #
#   described in file LICENSE.                                        #
#                                                                     #
#######################################################################


OCAMLDIR=$(shell ocamlc -where)
include $(OCAMLDIR)/Makefile.config

# Various commands and dir
##########################
CAMLRUN= ocamlrun
OCAMLC   = ocamlc -annot -bin-annot -w A-4-9-40-42-44 -warn-error A-4-9-32-33-34-40-42-44
OCAMLOPT = ocamlopt -annot -bin-annot -w A-4-9-40-42-44 -warn-error A-4-9-32-33-34-40-42-44
OCAMLDEP = ocamldep
OCAMLLEX = ocamllex
OCAMLYACC= ocamlyacc
OCAMLLIB = $(LIBDIR)
OCAMLBIN = $(BINDIR)

# Compilation
#############
OCAMLSRCDIR=..
INCLUDES_DEP=-I +compiler-libs

# Requires unix!
COMPFLAGS= -g $(INCLUDES_DEP) -I +unix

MODULES= utils checksum fileident filepath dotfile compdir xset treeset command typeexpand \
	xlongident name xident xpath locident typeFix xprinttyp ext cmt spot spoteval spotconfig_intf spotconfig spotfile ocamlspot # pathreparse 

OBJS=		$(addsuffix .cmo, $(MODULES))

XOBJS=		$(addsuffix .cmx, $(MODULES))

WITH_OCAMLOPT=$(shell which ocamlopt)

ifeq ($(WITH_OCAMLOPT),)
all: byt
else
all: byt opt
endif

byt: ocamlspot.byt ocamlspot

opt: ocamlspot.opt ocamlspot


.PHONY: test byt opt

tests:
	(cd tests; $(MAKE))

# At install, we must do it differently.
ifeq ($(WITH_OCAMLOPT),)
ocamlspot: ocamlspot.byt
	ln -s ocamlspot.byt ocamlspot
else   
ocamlspot: ocamlspot.opt
	ln -s ocamlspot.opt ocamlspot
endif

ocamlspot.byt: $(COMPOBJS) $(OBJS)
	$(OCAMLC) -o $@ $(COMPFLAGS) $(COMPOBJS) unix.cma ocamlcommon.cma $(OBJS)

ocamlspot.opt: $(COMPXOBJS) $(XOBJS)
	$(OCAMLOPT) -o $@ $(COMPFLAGS) $(COMPXOBJS) unix.cmxa ocamlcommon.cmxa $(XOBJS)


clean:
	rm -f ocamlspot ocamlspot.byt ocamlspot.opt *.cm* *.o *.annot *.sp*t 
	(cd tests; $(MAKE) clean)

# generic rules :
#################

.SUFFIXES: .mll .mly .ml .mli .cmo .cmi .cmx

typedtreefold.cmo: typedtreefold.ml
	$(OCAMLC) -I +compiler-libs -pp 'camlp4o Camlp4FoldGenerator.cmo' typedtreefold.ml

.ml.cmo:
	$(OCAMLC) $(OCAMLPP) $(COMPFLAGS) -c $<

.mli.cmi:
	$(OCAMLC) $(OCAMLPP) $(COMPFLAGS) -c $<

.ml.cmx:
	$(OCAMLOPT) $(OCAMLPP) $(COMPFLAGS) -c $<

.mll.ml:
	$(OCAMLLEX) $<

.mly.ml:
	$(OCAMLYACC) -v $<

.mly.mli:
	$(OCAMLYACC) -v $<

beforedepend::

depend: beforedepend
	ocamldep $(INCLUDES) *.mli *.ml > .depend

.PHONY: clean install installopt beforedepend depend test

# elisp

EMACS=emacs

ELISPS=ocamlspot.el

COMPILECMD=(progn \
		(setq load-path (cons "." load-path)) \
		(byte-compile-file "ocamlspot.el"))

install-elisp:
	@if test "$(EMACSDIR)" = ""; then \
          set xxx `($(EMACS) --batch --eval "(mapcar 'print load-path)") \
                   2>/dev/null | \
                   sed -n -e '/\/site-lisp/s/"//gp'`; \
          if test "$$2" = ""; then \
            echo "Cannot determine Emacs site-lisp directory"; \
            exit 2; \
          else \
            $(MAKE) EMACSDIR="$$2" simple-install; \
	  fi; \
        else \
          $(MAKE) simple-install; \
        fi

# install the .el files, but do not compile them.
install-el:
	$(MAKE) NOCOMPILE=true install-elisp

simple-install:
	@echo "Installing in $(EMACSDIR)..."
	if test -d $(EMACSDIR); then : ; else mkdir -p $(EMACSDIR); fi
	cp $(ELISPS) $(EMACSDIR)
	if [ -z "$(NOCOMPILE)" ]; then \
	  cd $(EMACSDIR); $(EMACS) --batch --eval '$(COMPILECMD)'; \
	fi

install:: ocamlspot.byt
	cp ocamlspot.byt $(BINDIR)/ocamlspot.byt$(EXE)
	# The following is optional
	# $(MAKE) install-elisp

ifneq ($(WITH_OCAMLOPT),)
install:: ocamlspot.opt
	cp ocamlspot.opt $(BINDIR)/ocamlspot.opt$(EXE)
endif

ifeq ($(WITH_OCAMLOPT),)
install:: 
	rm -f $(BINDIR)/ocamlspot$(EXE)
	ln -s $(BINDIR)/ocamlspot.byt$(EXE) $(BINDIR)/ocamlspot$(EXE)
else
install:: 
	rm -f $(BINDIR)/ocamlspot$(EXE)
	ln -s $(BINDIR)/ocamlspot.opt$(EXE) $(BINDIR)/ocamlspot$(EXE)
endif

uninstall::
	rm -f $(BINDIR)/ocamlspot.byt$(EXE)
	rm -f $(BINDIR)/ocamlspot.opt$(EXE)
	rm -f $(BINDIR)/ocamlspot$(EXE)

test: ocamlspot ocamlspot.cmo
	tests/auto-test.pl ocamlspot.ml treeset.ml xset.ml 

include .depend
