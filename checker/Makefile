PROGNAME=checker

GMCDIR=../gmc

OCAMLC=ocamlc
OCAMLOPT=ocamlopt
OCAMLDEP=ocamldep
OCAMLYACC=ocamlyacc
OCAMLLEX=ocamllex

GAMES=Nat CompareNat1 CompareNat2 CompareNat3 EvalNatExp ReduceNatExp \
	EvalML1 EvalML2 EvalML3 EvalML4 EvalML5 EvalML6 \
	EvalML1Err \
	EvalContML1 EvalContML3 EvalContML4 \
	EvalDContML4 \
	EvalRefML3 EvalRefML4 \
	NamelessML3 EvalNamelessML3 \
	TypingML2 TypingML3 TypingML4 TypingML6 \
	PolyTypingML3 PolyTypingML4 \
	While

INCLUDES=$(foreach game,$(GAMES),-I $(game)) -I +str # -I $(GMCDIR)

OCAMLFLAGS=$(INCLUDES)    # add other options for ocamlc here
OCAMLOPTFLAGS=$(INCLUDES) # add other options for ocamlopt here

GAMEOBJS=	Nat/nat.cmx \
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
		EvalML6/evalML6.cmx \
		EvalML1Err/evalML1Err.cmx \
		EvalContML1/evalContML1.cmx \
		EvalContML3/evalContML3.cmx \
		EvalContML4/evalContML4.cmx \
		EvalDContML4/evalDContML4.cmx \
		EvalRefML3/evalRefML3.cmx \
		EvalRefML4/evalRefML4.cmx \
		NamelessML3/namelessML3.cmx \
		EvalNamelessML3/evalNamelessML3.cmx \
		TypingML2/typingML2.cmx \
		TypingML3/typingML3.cmx \
		TypingML4/typingML4.cmx \
		TypingML6/typingML6.cmx \
		PolyTypingML3/polyTypingML3.cmx \
		PolyTypingML4/polyTypingML4.cmx \
		While/while.cmx

FIRSTOBJS=mySupport.cmx derivation.cmx lexer.cmx
OBJS=$(GAMEOBJS) main.cmx

DEPEND += lexer.ml

all: $(GMCDIR)/gmc $(FIRSTOBJS) $(addsuffix .pack,$(GAMES)) $(OBJS) $(DEPEND)
	$(OCAMLOPT) -I +str -o $(PROGNAME) str.cmxa $(FIRSTOBJS) $(OBJS)

%.pack:
	$(MAKE) -C `basename $@ .pack` depend
	$(MAKE) -C `basename $@ .pack`

%.cmo: %.ml
	$(OCAMLC) $(INCLUDES) -c $<

%.cmx: %.ml
	$(OCAMLOPT) $(INCLUDES) -c $<

lexer.ml: lexer.mll
	@rm -f $@
	$(OCAMLLEX) $<
	@chmod -w $@

# Clean up
clean:
	for game in $(GAMES); do $(MAKE) -C $$game clean; done
	rm -f $(PROGNAME)
	rm -f *.cm[iox] *.o *~ parser.output lexer.ml .depend

# Dependencies
depend:: $(DEPEND)
	$(OCAMLDEP) $(INCLUDES) *.mli *.ml > .depend

main.cmo: $(GAMEOBJS)

-include .depend
