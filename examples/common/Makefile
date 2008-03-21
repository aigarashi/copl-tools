GMCDIR=../../gmc
GMC=$(GMCDIR)/gmc

OCAMLC=ocamlc
OCAMLOPT=ocamlopt
OCAMLDEP=ocamldep
OCAMLYACC=ocamlyacc
OCAMLLEX=ocamllex
INCLUDES= -I . -I $(GMCDIR)    # all relevant -I options here
OCAMLFLAGS=$(INCLUDES)    # add other options for ocamlc here
OCAMLOPTFLAGS=$(INCLUDES) # add other options for ocamlopt here


# The list of object files for prog1
OBJS=$(GMCDIR)/mySupport.cmx syntax.cmx core.cmx pp.cmx \
	parser.cmx keywords.cmx lexer.cmx main.cmx

DEPEND += lexer.ml parser.ml core.ml

all: $(DEPEND) $(OBJS)
	$(OCAMLOPT) -o $(PROGNAME) $(OCAMLFLAGS) $(OBJS)

# Common rules
.SUFFIXES: .ml .mli .cmo .cmi .cmx

.ml.cmo:
	$(OCAMLC) $(OCAMLFLAGS) -c $<

.mli.cmi:
	$(OCAMLC) $(OCAMLFLAGS) -c $<

.ml.cmx:
	$(OCAMLOPT) $(OCAMLOPTFLAGS) -c $<

parser.ml parser.mli: parser.mly	
	@rm -f $@
	$(OCAMLYACC) -v $<
	@chmod -w $@

lexer.ml: lexer.mll
	@rm -f $@
	$(OCAMLLEX) $<
	@chmod -w $@

core.ml: ../$(PROGNAME).gm
	@rm -f $@
	$(GMC) $< > $@
	@chmod -w $@

# Clean up
clean:
	rm -f $(PROGNAME)
	rm -f *.cm[iox] *.o *~ parser.ml parser.mli parser.output lexer.ml core.ml .depend

# Dependencies
depend:: $(DEPEND)
	$(OCAMLDEP) -native $(INCLUDES) *.mli *.ml > .depend

-include .depend
