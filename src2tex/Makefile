SRC2DRV=src2drv
SRC2TEX=src2tex

all: $(SRC2DRV) $(SRC2TEX)

$(SRC2DRV): $(SRC2DRV).mll
	ocamllex $(SRC2DRV).mll
	ocamlc -o $(SRC2DRv) $(SRC2DRV).ml

$(SRC2TEX): $(SRC2TEX).mll
	ocamllex $(SRC2TEX).mll
	ocamlc -o $(SRC2TEX) $(SRC2TEX).ml

clean:
	-rm *~ $(SRC2DRV).ml $(SRC2TEX).ml *.cmi *.cmo $(SRC2DRV) $(SRC2TEX)
