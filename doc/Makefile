all: main.pdf

%.pdf: %.dvi
	dvipdfmx $<

%.dvi: %.tex
	platex $<

games.dvi:: natrules.tex ML1rules.tex natderivs.tex ML1derivs.tex

natrules.tex:
	../gmc/gmc -TeX ../examples/nat.gm > $@

natderivs.tex:
	../examples/common/checker -TeX -full -game nat ../examples/nat.drv > $@

ML1rules.tex:
	../gmc/gmc -TeX ../examples/ML1.gm > $@

ML1derivs.tex:
	../examples/common/checker -TeX -full -game ML1 ../examples/ML1.drv > $@
