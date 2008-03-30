PROGNAME=checker

GMCDIR=../../gmc

OCAMLC=ocamlc
OCAMLOPT=ocamlopt
OCAMLDEP=ocamldep
OCAMLYACC=ocamlyacc
OCAMLLEX=ocamllex
INCLUDES=-I ../nat -I ../ML1 -I $(GMCDIR)   # all relevant -I options here
OCAMLFLAGS=$(INCLUDES)    # add other options for ocamlc here
OCAMLOPTFLAGS=$(INCLUDES) # add other options for ocamlopt here

OBJS=$(GMCDIR)/mySupport.cmx derivation.cmx lexer.cmx \
		../nat/nat.cmx \
		../ML1/ml1.cmx \
		main.cmx


DEPEND += lexer.ml

all: nat ML1 $(OBJS) $(DEPEND) 
	ocamlopt -o $(PROGNAME) $(OBJS)

nat: 
	make -C ../nat depend
	make -C ../nat

ML1:
	make -C ../ML1 depend
	make -C ../ML1

%.cmx: %.ml
	ocamlopt $(INCLUDES) -c $<

lexer.ml: lexer.mll
	@rm -f $@
	$(OCAMLLEX) $<
	@chmod -w $@

# Clean up
clean:
	rm -f $(PROGNAME)
	rm -f *.cm[iox] *.o *~ parser.output lexer.ml .depend

# Dependencies
depend:: $(DEPEND)
	$(OCAMLDEP) -native $(INCLUDES) *.mli *.ml > .depend

-include .depend
