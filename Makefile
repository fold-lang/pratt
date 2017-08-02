
build:
	jbuilder build -j4

test:
	jbuilder runtest

clean:
	jbuilder clean

shell: build
	/usr/bin/env bash -c 'utop -init <(cat ~/.ocamlinit Init.ml)'

watch:
	ls src/*.ml* tests/*.ml* | entr -cr make test

.PHONY: test all clean

