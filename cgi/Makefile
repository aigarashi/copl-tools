GMCDIR=../gmc
GMC=$(GMCDIR)/gmc
RULE2TEX=./rules2TeX.scm

GAMES=Nat CompareNat1 CompareNat2 CompareNat3 EvalNatExp ReduceNatExp \
	EvalML1 EvalML2 EvalML3 EvalML4 EvalML5 EvalML6 \
	EvalML1Err \
	NamelessML3 EvalNamelessML3 \
	TypingML2 TypingML3 TypingML4 TypingML6 \
	PolyTypingML3 PolyTypingML4 \
	EvalContML1 EvalContML4 \
	EvalRefML3 \
	While

html: $(addsuffix .html,$(GAMES))
	-mkdir games
	mv $^ games

%.scm: $(GMC)
	$(GMC) -sexp $* ../checker/$*/$(patsubst %.scm,%.gm,$@) > $@

%.html: %.scm
	$(RULE2TEX) $* > $@

$(GMC):
	make -C $(GMCDIR)

mo: po/ja.po
	msgfmt --output-file ja/LC_MESSAGES/copl.mo $^

clean:
	-rm *~
	-rm $(addsuffix .html, $(GAMES)) $(addsuffix .scm,$(GAMES))
	-rm -rf games

