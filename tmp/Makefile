
default: run

run: build
	./main.byte

build:
	ocamlbuild -j 4 main.byte -pkg sedlex

clean:
	rm -rf _build *.byte *.native
