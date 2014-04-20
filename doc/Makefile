TOPDIR=..
GMCDIR=$(TOPDIR)/gmc
GMC=$(GMCDIR)/gmc
GAMESDIR=$(TOPDIR)/checker
SRC2DRV=tools/src2tex/src2drv
SRC2TEX=tools/src2tex/src2tex

GAMES=Nat CompareNat1 CompareNat2 CompareNat3 EvalNatExp ReduceNatExp \
	EvalML1 EvalML1Err EvalML2 EvalML3 EvalML4 EvalML5 EvalML6 \
	NamelessML3 EvalNamelessML3 \
	TypingML2 TypingML3 TypingML4 TypingML6 PolyTypingML4 \
	EvalML4Err \
	EvalContML1 EvalContML3 EvalContML4 \
	EvalDContML4 \
	EvalRefML3 \
	While

all: rulebook.pdf

rulebook.pdf: rules rulebook.tex
	platex rulebook.tex
	dvipdfmx rulebook.dvi

%.pdf: %.dvi
	dvipdfmx $<

%.dvi: %.tex
	platex $<

rules: formatted-rules $(addprefix formatted-rules/,$(addsuffix .tex,$(GAMES)))

formatted-rules:
	-mkdir $@

formatted-rules/%.tex: gmc formatted-rules 
	$(GMC) -TeX $(notdir $(basename $@)) $(GAMESDIR)/$(notdir $(basename $@))/$(notdir $(patsubst %.tex,%.gm,$@)) > $@

gmc:
	make -C $(GMCDIR)

$(SRC2DRV): $(SRC2DRV).mll
	$(MAKE) -C tools/src2tex src2drv

$(SRC2TEX): $(SRC2TEX).mll
	$(MAKE) -C tools/src2tex src2tex

clean:
	-rm *.aux *.log *.dvi
	-rm *~
	-rm -rf formatted-rules