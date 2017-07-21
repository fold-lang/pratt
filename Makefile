
build:
	jbuilder build @install @DEFAULT -j4

test:
	jbuilder runtest

clean:
	jbuilder clean

.PHONY: test all clean

