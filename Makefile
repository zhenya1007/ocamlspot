#########################################################################
#                                                                       #
#                             OCamlSpotter                              #
#                                                                       #
#                              Jun FURUSE                               #
#                                                                       #
#     Copyright 2008 Jun Furuse. All rights reserved.                   #
#     This file is distributed under the terms of the GNU Library       #
#     General Public License, with the special exception on linking     #
#     described in file LICENSE.                                        #
#                                                                       #
#########################################################################

OCAMLDIR=$(PREFIX)/lib/ocaml/
include $(OCAMLDIR)/Makefile.config

# Various commands and dir
##########################
CAMLRUN= ocamlrun
OCAMLC   = ocamlc -annot -bin-annot -w Ae-9 -warn-error Ae-9
OCAMLOPT = ocamlopt -annot -bin-annot -w Ae-9 -warn-error Ae-9
OCAMLDEP = ocamldep
OCAMLLEX = ocamllex
OCAMLYACC= ocamlyacc
OCAMLLIB = $(LIBDIR)
OCAMLBIN = $(BINDIR)

# Compilation
#############
OCAMLSRCDIR=..
INCLUDES_DEP=-I $(OCAMLDIR)/compiler-libs

# Requires unix!
COMPFLAGS= $(INCLUDES_DEP) -I $(OTHERS)/unix

MODULES= utils dotfile xset treeset command typeexpand \
	xlongident name xident xpath locident typeFix xprinttyp ext spot spoteval spotconfig_intf spotconfig pathreparse ocamlspot

OBJS=		$(addsuffix .cmo, $(MODULES))

XOBJS=		$(addsuffix .cmx, $(MODULES))

all: ocamlspot 

.PHONY: test

tests:
	(cd tests; $(MAKE))

ocamlspot: $(COMPOBJS) $(OBJS)
	$(OCAMLC) -o $@ $(COMPFLAGS) $(COMPOBJS) unix.cma ocamlcommon.cma $(OBJS)

opt.opt: ocamlspot.opt

ocamlspot.opt: $(COMPXOBJS) $(XOBJS)
	(cd ../asmrun; $(MAKE) meta.o dynlink.o)
	$(OCAMLOPT) -o $@ $(COMPFLAGS) $(COMPXOBJS) unix.cmxa $(XOBJS) \
	  ../asmrun/meta.o ../asmrun/dynlink.o -cclib "$(BYTECCLIBS)"

#	$(CAMLOPT) $(LINKFLAGS) -ccopt "$(BYTECCLINKOPTS)" -o ocamlc.opt \
#	  $(COMPOBJS:.cmo=.cmx) \
#	  asmrun/meta.o asmrun/dynlink.o -cclib "$(BYTECCLIBS)"
#	@sed -e 's|@compiler@|$$topdir/ocamlc.opt|' \
#	  driver/ocamlcomp.sh.in > ocamlcomp.sh
#	@chmod +x ocamlcomp.sh


opt: ocamlspot.opt

.PHONY: opt opt.opt

clean:
	rm -f ocamlspot ocamlspot.opt *.cm* *.o *.annot *.sp*t 
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
	$(MAKE) NOCOMPILE=true install

simple-install:
	@echo "Installing in $(EMACSDIR)..."
	if test -d $(EMACSDIR); then : ; else mkdir -p $(EMACSDIR); fi
	cp $(ELISPS) $(EMACSDIR)
	if [ -z "$(NOCOMPILE)" ]; then \
	  cd $(EMACSDIR); $(EMACS) --batch --eval '$(COMPILECMD)'; \
	fi

install installopt::
	cp ocamlspot $(BINDIR)/ocamlspot$(EXE)
	if test -f ocamlspot.opt; \
	  then cp ocamlspot.opt $(BINDIR)/ocamlspot.opt$(EXE); else :; fi
	# The following is optional
	# $(MAKE) install-emacs-lisp

test: ocamlspot ocamlspot.cmo
	tests/auto-test.pl ocamlspot.ml treeset.ml xset.ml 

include .depend
