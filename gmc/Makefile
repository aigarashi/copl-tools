OCAMLC=ocamlc
OCAMLOPT=ocamlopt
OCAMLDEP=ocamldep
OCAMLYACC=ocamlyacc
OCAMLLEX=ocamllex
INCLUDES=                 # all relevant -I options here
OCAMLFLAGS=$(INCLUDES)    # add other options for ocamlc here
OCAMLOPTFLAGS=$(INCLUDES) # add other options for ocamlopt here

PROGNAME=gmc

# The list of object files for prog1
# OBJS=syntax.cmx parser.cmx lexer.cmx emit.cmx main.cmx
OBJS=mySupport.cmo syntax.cmo parser.cmo lexer.cmo emit.cmo mode.cmo main.cmo

DEPEND += lexer.ml parser.ml

all: $(DEPEND) $(OBJS)
	$(OCAMLC) -o $(PROGNAME) $(OCAMLFLAGS) str.cma $(OBJS)
#	$(OCAMLOPT) -o $(PROGNAME) $(OCAMLFLAGS) str.cmxa $(OBJS)

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

# Clean up
clean:
	rm -f $(PROGNAME)
	rm -f *.cm[iox] *~ *.o parser.ml parser.mli parser.output lexer.ml .depend

# Dependencies
depend:: $(DEPEND)
	$(OCAMLDEP) $(INCLUDES) -native *.mli *.ml > .depend

-include .depend
