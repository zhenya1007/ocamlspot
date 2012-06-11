OCAMLC   = ocamlc -bin-annot -annot
OCAMLOPT = ocamlopt -bin-annot -annot
OCAMLDEP = ocamldep
OCAMLLEX = ocamllex
OCAMLYACC= ocamlyacc

# Requires unix!
COMPFLAGS= $(INCLUDES_DEP) -I $(OTHERS)/unix

include Makefile.targets

all: test2.cmo $(TARGETS) 

pack.cmo: packed.cmo
	$(OCAMLC) -pack -o $@ $<

packed_alias.cmo: pack.cmo

test2.cmo: test.cmo
	$(OCAMLC) -pack -o $@ $<

dir1/test4.cmo: dir1/test4.ml
	$(OCAMLC) -c -I dir1 -o $@ $<

test5.cmo: test5.ml
	$(OCAMLC) -c -I dir1 -o $@ $<

dir2/test6.cmo: dir2/test6.ml
	$(OCAMLC) -c -I dir2 -o $@ $<

clean:
	rm -f *.cm* *.o *.annot *.sp*t  */*.cm* */*.annot */*.sp*t

# generic rules :
#################

.SUFFIXES: .mll .mly .ml .mli .cmo .cmi .cmx

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
	$(OCAMLDEP) $(INCLUDES) -I dir *.mli *.ml */*.mli */*.ml > .depend

#	$(OCAMLDEP) $(INCLUDES) -I dir1 -I dir2 *.mli *.ml */*.mli */*.ml > .depend

Makefile.targets: *.ml *.mli */*.ml
	echo TARGETS= \\ > $@
	ls *.ml *.mli | sed -e 's/mli$$/cmi/' -e 's/ml$$/cmo/' -e 's/$$/ \\/'	 >> $@

.PHONY: clean install installopt beforedepend depend

ocamlspot:
	ln -s ../ocamlspot ocamlspot

include .depend