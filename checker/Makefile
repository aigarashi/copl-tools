PROGNAME=checker

GMCDIR=../gmc

OCAMLC=ocamlc
OCAMLOPT=ocamlopt
OCAMLDEP=ocamldep
OCAMLYACC=ocamlyacc
OCAMLLEX=ocamllex

GAMES=Nat NatLt1 NatLt2 NatLt3 NatExp ML1 ML2 ML3 ML4 ML4Cont ML5 ML6 TypingML2 TypingML4 TypingML5 PolyML4 PolyML5

INCLUDES=-I Nat -I NatLt1 -I NatLt2 -I NatLt3 -I NatExp \
	-I ML1 -I ML2 -I ML3 -I ML4 -I ML4Cont -I ML5 -I ML6 \
	-I TypingML2 -I TypingML4 -I TypingML5 -I PolyML4 -I PolyML5 -I $(GMCDIR)   # all relevant -I options here
OCAMLFLAGS=$(INCLUDES)    # add other options for ocamlc here
OCAMLOPTFLAGS=$(INCLUDES) # add other options for ocamlopt here

OBJS=$(GMCDIR)/mySupport.cmx derivation.cmx lexer.cmx \
		Nat/nat.cmx \
		NatLt1/natLt1.cmx \
		NatLt2/natLt2.cmx \
		NatLt3/natLt3.cmx \
		NatExp/natExp.cmx \
		ML1/ml1.cmx \
		ML2/ml2.cmx \
		ML3/ml3.cmx \
		ML4/ml4.cmx \
		ML4Cont/ml4Cont.cmx \
		ML5/ml5.cmx \
		ML6/ml6.cmx \
		TypingML2/typingML2.cmx \
		TypingML4/typingML4.cmx \
		TypingML5/typingML5.cmx \
		PolyML4/polyML4.cmx \
		PolyML5/polyML5.cmx \
		main.cmx


DEPEND += lexer.ml

all: $(GMCDIR)/gmc $(addsuffix .pack,$(GAMES)) $(OBJS) $(DEPEND) 
	ocamlopt -o $(PROGNAME) $(OBJS)

%.pack: 
	$(MAKE) -C `basename $@ .pack` depend
	$(MAKE) -C `basename $@ .pack`

%.cmx: %.ml
	ocamlopt $(INCLUDES) -c $<

lexer.ml: lexer.mll
	@rm -f $@
	$(OCAMLLEX) $<
	@chmod -w $@

# Clean up
clean:
	$(MAKE) -C Nat clean
	$(MAKE) -C NatLt1 clean
	$(MAKE) -C NatLt2 clean
	$(MAKE) -C NatLt3 clean
	$(MAKE) -C NatExp clean
	$(MAKE) -C ML1 clean
	$(MAKE) -C ML2 clean
	$(MAKE) -C ML3 clean
	$(MAKE) -C ML4 clean
	$(MAKE) -C ML4Cont clean
	$(MAKE) -C ML5 clean
	$(MAKE) -C ML6 clean
	$(MAKE) -C TypingML2 clean
	$(MAKE) -C TypingML4 clean
	$(MAKE) -C TypingML5 clean
	$(MAKE) -C PolyML4 clean	
	$(MAKE) -C PolyML5 clean	
	rm -f $(PROGNAME)
	rm -f *.cm[iox] *.o *~ parser.output lexer.ml .depend

# Dependencies
depend:: $(DEPEND)
	$(OCAMLDEP) -native $(INCLUDES) *.mli *.ml > .depend

main.cmx: Nat/nat.cmx NatLt1/natLt1.cmx NatLt2/natLt2.cmx NatLt3/natLt3.cmx NatExp/natExp.cmx \
	ML1/ml1.cmx ML2/ml2.cmx ML3/ml3.cmx ML4/ml4.cmx ML4Cont/ml4Cont.cmx ML5/ml5.cmx ML6/ml6.cmx \
	TypingML2/typingML2.cmx TypingML4/typingML4.cmx TypingML5/typingML5.cmx \
	PolyML4/polyML4.cmx PolyML5/polyML5.cmx

-include .depend
