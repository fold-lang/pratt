
default: run

run: build
	./main.byte

build:
	ocamlbuild main.byte

clean:
	rm -rf _build *.byte *.native
