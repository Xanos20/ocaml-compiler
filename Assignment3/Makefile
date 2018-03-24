OCBFLAGS :=
OCB := ocamlbuild $(OCBFLAGS)

.PHONY: all debug clean top

all: Compiler.native
debug: all Compiler.cma

%.cma: .FORCE
	$(OCB) $@

%.cmxa: .FORCE
	$(OCB) $@

%.native: .FORCE
	$(OCB) $@

%.p.native: .FORCE
	$(OCB) $@

%.byte: .FORCE
	$(OCB) $@

.FORCE:

clean:
	$(OCB) -clean

top: Compiler.cma
	utop
