PROGNAME=checker

GMCDIR=../gmc

OCAMLC=ocamlc
OCAMLOPT=ocamlopt
OCAMLDEP=ocamldep
OCAMLYACC=ocamlyacc
OCAMLLEX=ocamllex

GAMES=Nat CompareNat1 CompareNat2 CompareNat3 EvalNatExp ReduceNatExp \
	ML1 ML2 ML3 ML4 ContML1 ContML4 RefML4 ML5 ML6 TypingML2 TypingML4 TypingML5 PolyML4 PolyML5

INCLUDES=$(foreach game,$(GAMES),-I $(game)) # -I $(GMCDIR)

OCAMLFLAGS=$(INCLUDES)    # add other options for ocamlc here
OCAMLOPTFLAGS=$(INCLUDES) # add other options for ocamlopt here

OBJS=mySupport.cmx derivation.cmx lexer.cmx \
		Nat/nat.cmx \
		CompareNat1/compareNat1.cmx \
		CompareNat2/compareNat2.cmx \
		CompareNat3/compareNat3.cmx \
		EvalNatExp/evalNatExp.cmx \
		ReduceNatExp/reduceNatExp.cmx \
		ML1/ml1.cmx \
		ML2/ml2.cmx \
		ML3/ml3.cmx \
		ML4/ml4.cmx \
		ContML1/contML1.cmx \
		ContML4/contML4.cmx \
		RefML4/refML4.cmx \
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
	ocamlopt -o $(PROGNAME) str.cmxa $(OBJS)

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
#	$(foreach game,$(GAMES), $(MAKE) -C $(game) clean)
	$(MAKE) -C Nat clean
	$(MAKE) -C CompareNat1 clean
	$(MAKE) -C CompareNat2 clean
	$(MAKE) -C CompareNat3 clean
	$(MAKE) -C EvalNatExp clean
	$(MAKE) -C ReduceNatExp clean
	$(MAKE) -C ML1 clean
	$(MAKE) -C ML2 clean
	$(MAKE) -C ML3 clean
	$(MAKE) -C ML4 clean
	$(MAKE) -C ContML1 clean
	$(MAKE) -C ContML4 clean
	$(MAKE) -C RefML4 clean
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

main.cmx: Nat/nat.cmx CompareNat1/compareNat1.cmx CompareNat2/compareNat2.cmx CompareNat3/compareNat3.cmx EvalNatExp/evalNatExp.cmx ReduceNatExp/reduceNatExp.cmx \
	ML1/ml1.cmx ML2/ml2.cmx ML3/ml3.cmx ML4/ml4.cmx ContML1/contML1.cmx ContML4/contML4.cmx RefML4/refML4.cmx ML5/ml5.cmx ML6/ml6.cmx \
	TypingML2/typingML2.cmx TypingML4/typingML4.cmx TypingML5/typingML5.cmx \
	PolyML4/polyML4.cmx PolyML5/polyML5.cmx

-include .depend
