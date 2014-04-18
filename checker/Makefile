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

INCLUDES=$(foreach game,$(GAMES),-I $(game)) # -I $(GMCDIR)

OCAMLFLAGS=$(INCLUDES)    # add other options for ocamlc here
OCAMLOPTFLAGS=$(INCLUDES) # add other options for ocamlopt here

GAMEOBJS=	Nat/nat.cmo \
		CompareNat1/compareNat1.cmo \
		CompareNat2/compareNat2.cmo \
		CompareNat3/compareNat3.cmo \
		EvalNatExp/evalNatExp.cmo \
		ReduceNatExp/reduceNatExp.cmo \
		EvalML1/evalML1.cmo \
		EvalML2/evalML2.cmo \
		EvalML3/evalML3.cmo \
		EvalML4/evalML4.cmo \
		EvalML5/evalML5.cmo \
		EvalML6/evalML6.cmo \
		EvalML1Err/evalML1Err.cmo \
		EvalContML1/evalContML1.cmo \
		EvalContML3/evalContML3.cmo \
		EvalContML4/evalContML4.cmo \
		EvalDContML4/evalDContML4.cmo \
		EvalRefML3/evalRefML3.cmo \
		EvalRefML4/evalRefML4.cmo \
		NamelessML3/namelessML3.cmo \
		EvalNamelessML3/evalNamelessML3.cmo \
		TypingML2/typingML2.cmo \
		TypingML3/typingML3.cmo \
		TypingML4/typingML4.cmo \
		TypingML6/typingML6.cmo \
		PolyTypingML3/polyTypingML3.cmo \
		PolyTypingML4/polyTypingML4.cmo \
		While/while.cmo

OBJS=mySupport.cmo derivation.cmo lexer.cmo $(GAMEOBJS) main.cmo

DEPEND += lexer.ml

all: $(GMCDIR)/gmc $(addsuffix .pack,$(GAMES)) $(OBJS) $(DEPEND) 
	$(OCAMLC) -o $(PROGNAME) str.cma $(OBJS)

%.pack: 
	$(MAKE) -C `basename $@ .pack` depend
	$(MAKE) -C `basename $@ .pack`

%.cmo: %.ml
	ocamlc $(INCLUDES) -c $<

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
