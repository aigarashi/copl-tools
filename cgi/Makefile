GMCDIR=../gmc
GMC=$(GMCDIR)/gmc

GAMES=nat ML1 ML2 ML3 ML4 ML5 ML6 TypingML2 TypingML4 TypingML5 PolyML4

scm: $(addsuffix .scm,$(GAMES))

%.scm:
	$(GMC) -sexp $(basename $@) ../checker/$(basename $@)/$(patsubst %.scm,%.gm,$@) > $@

clean:
	-rm *.scm

