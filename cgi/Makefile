GMCDIR=../gmc
GMC=$(GMCDIR)/gmc
RULE2TEX=./rules2TeX.scm

GAMES=nat natLt1 natLt2 natLt3 NatExp \
	ML1 ML2 ML3 ML4 ML5 ML6 \
	TypingML2 TypingML4 TypingML5 PolyML4

html: $(addsuffix .html,$(GAMES))

%.scm: $(GMC)
	$(GMC) -sexp $(basename $@) ../checker/$(basename $@)/$(patsubst %.scm,%.gm,$@) > $@

%.html: %.scm rules2TeX.scm
	$(RULE2TEX) $(basename $@) > $@

$(GMC):
	make -C $(GMCDIR)

clean:
	-rm $(addsuffix .html,$(GAMES)) $(addsuffix .scm,$(GAMES))
