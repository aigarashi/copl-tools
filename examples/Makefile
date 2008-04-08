PROGNAME=checker

GMCDIR=../gmc

OCAMLC=ocamlc
OCAMLOPT=ocamlopt
OCAMLDEP=ocamldep
OCAMLYACC=ocamlyacc
OCAMLLEX=ocamllex
INCLUDES=-I nat -I ML1 -I ML2 -I $(GMCDIR)   # all relevant -I options here
OCAMLFLAGS=$(INCLUDES)    # add other options for ocamlc here
OCAMLOPTFLAGS=$(INCLUDES) # add other options for ocamlopt here

OBJS=$(GMCDIR)/mySupport.cmx derivation.cmx lexer.cmx \
		nat/nat.cmx \
		ML1/ml1.cmx \
		ML2/ml2.cmx \
		main.cmx


DEPEND += lexer.ml

all: nat.pack ML1.pack ML2.pack $(OBJS) $(DEPEND) 
	ocamlopt -o $(PROGNAME) $(OBJS)

%.pack: 
	make -C `basename $@ .pack` depend
	make -C `basename $@ .pack`

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

main.cmx: nat/nat.cmx ML1/ml1.cmx ML2/ml2.cmx

-include .depend
