GMCDIR=../gmc
GMC=$(GMCDIR)/gmc
RULE2TEX=./rules2TeX.scm

GAMES=Nat NatLt1 NatLt2 NatLt3 NatExp \
	ML1 ML2 ML3 ML4 ContML4 RefML4 ML5 ML6 \
	TypingML2 TypingML4 TypingML5 PolyML4 PolyML5

html: $(addsuffix .html,$(GAMES))
	-mkdir games
	mv $^ games

%.scm: $(GMC)
	$(GMC) -sexp $(basename $@) ../checker/$(basename $@)/$(patsubst %.scm,%.gm,$@) > $@

rules2TeX.scm: 

%.html: %.scm rules2TeX.scm
	$(RULE2TEX) $(basename $@) > $@

$(GMC):
	make -C $(GMCDIR)

clean:
	-rm $(addsuffix .html, $(GAMES)) $(addsuffix .scm,$(GAMES))
	-rm -rf games

