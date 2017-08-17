
build:
	jbuilder build -j4 @install

install: build
	jbuilder install

uninstall:
	jbuilder uninstall

reinstall: uninstall install

test:
	jbuilder runtest

clean:
	jbuilder clean

shell: build
	/usr/bin/env bash -c 'utop -init <(cat ~/.ocamlinit Init.ml)'

watch:
	ls src/*.ml* tests/*.ml* | entr -cr make test

watch-js:
	ls src/*.ml* examples/*.ml* | entr -cr sh -c 'jbuilder build examples/Javascript.exe; ./_build/default/examples/Javascript.exe'

.PHONY: test all clean

