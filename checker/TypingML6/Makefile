PROGNAME = TypingML6
MODULEPATH = TypingML6
PACKED=typingML6

-include ../common/Makefile.game

parser.ml parser.mli: parser-local.mly
	@rm -f $@
	ln -s $< parser.mly
	$(OCAMLYACC) -v parser.mly
	@chmod -w $@

