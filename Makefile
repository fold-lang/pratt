
default: run

run: build
	./main.byte

build:
	ocamlbuild -j 4 main.byte

clean:
	rm -rf _build *.byte *.native
