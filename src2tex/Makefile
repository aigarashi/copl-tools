OCAMLLEX=ocamllex
OCAMLOPT=ocamlopt
SRC2DRV=src2drv
SRC2TEX=src2tex

all: $(SRC2DRV) $(SRC2TEX)

$(SRC2DRV): $(SRC2DRV).mll
	$(OCAMLLEX) $<
	$(OCAMLOPT) -o $@ $(SRC2DRV).ml

$(SRC2TEX): $(SRC2TEX).mll
	$(OCAMLLEX) $<
	$(OCAMLOPT) -o $@ $(SRC2TEX).ml

clean:
	-rm *~ $(SRC2DRV).ml $(SRC2TEX).ml *.cmi *.cmo $(SRC2DRV) $(SRC2TEX)
