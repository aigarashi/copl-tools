GMCDIR=../gmc
GMC=$(GMCDIR)/gmc
RULE2TEX=./rules2TeX.scm

GAMES=Nat CompareNat1 CompareNat2 CompareNat3 EvalNatExp ReduceNatExp \
	EvalML1 EvalML2 EvalML3 EvalML4 EvalML5 \
	EvalML1Err \
	NamelessML3 EvalNamelessML3 \
	TypingML2 TypingML3 TypingML4 \
	PolyTypingML3 PolyTypingML4	
#	ContML1 ContML4 RefML4 

html: $(addsuffix .html,$(GAMES))
	-mkdir games
	mv $^ games

%.scm: $(GMC)
	$(GMC) -sexp $(basename $@) ../checker/$(basename $@)/$(patsubst %.scm,%.gm,$@) > $@

%.html: %.scm
	$(RULE2TEX) $(basename $@) > $@

$(GMC):
	make -C $(GMCDIR)

clean:
	-rm $(addsuffix .html, $(GAMES)) $(addsuffix .scm,$(GAMES))
	-rm -rf games

