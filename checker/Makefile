PROGNAME=checker

GMCDIR=../gmc

OCAMLC=ocamlc
OCAMLOPT=ocamlopt
OCAMLDEP=ocamldep
OCAMLYACC=ocamlyacc
OCAMLLEX=ocamllex

GAMES=Nat CompareNat1 CompareNat2 CompareNat3 EvalNatExp ReduceNatExp \
	EvalML1 EvalML2 EvalML3 EvalML4 EvalML5  \
	EvalML1Err \
	ContML1 ContML4 RefML4 \
	TypingML2 TypingML4 TypingML5 PolyML4 PolyML5

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
		EvalML1/evalML1.cmx \
		EvalML2/evalML2.cmx \
		EvalML3/evalML3.cmx \
		EvalML4/evalML4.cmx \
		EvalML5/evalML5.cmx \
		EvalML1Err/evalML1Err.cmx \
		ContML1/contML1.cmx \
		ContML4/contML4.cmx \
		RefML4/refML4.cmx \
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
	$(MAKE) -C EvalML1 clean
	$(MAKE) -C EvalML2 clean
	$(MAKE) -C EvalML3 clean
	$(MAKE) -C EvalML4 clean
	$(MAKE) -C EvalML5 clean
	$(MAKE) -C EvalML1Err clean
	$(MAKE) -C ContML1 clean
	$(MAKE) -C ContML4 clean
	$(MAKE) -C RefML4 clean
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

main.cmx: Nat/nat.cmx \
	CompareNat1/compareNat1.cmx \
	CompareNat2/compareNat2.cmx \
	CompareNat3/compareNat3.cmx \
	EvalNatExp/evalNatExp.cmx \
	ReduceNatExp/reduceNatExp.cmx \
	EvalML1/evalML1.cmx \
	EvalML2/evalML2.cmx \
	EvalML3/evalML3.cmx \
	EvalML4/evalML4.cmx \
	EvalML5/evalML5.cmx \
	EvalML1Err/evalML1Err.cmx \
	ContML1/contML1.cmx \
	ContML4/contML4.cmx \
	RefML4/refML4.cmx \
	TypingML2/typingML2.cmx \
	TypingML4/typingML4.cmx \
	TypingML5/typingML5.cmx \
	PolyML4/polyML4.cmx \
	PolyML5/polyML5.cmx

-include .depend
