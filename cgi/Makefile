GMCDIR=../gmc
GMC=$(GMCDIR)/gmc
RULE2TEX=./rules2TeX.scm

GAMES=nat ML1 ML2 ML3 ML4 ML5 ML6 TypingML2 TypingML4 TypingML5 PolyML4

html: $(addsuffix .html,$(GAMES))

%.scm:
	$(GMC) -sexp $(basename $@) ../checker/$(basename $@)/$(patsubst %.scm,%.gm,$@) > $@

%.html: %.scm
	$(RULE2TEX) $(basename $@) > $@

clean:
	-rm $(addsuffix .html,$(GAMES)) $(addsuffix .scm,$(GAMES))

