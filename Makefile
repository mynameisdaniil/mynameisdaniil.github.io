ELM = /usr/bin/env elm-make

default: build

build:
	$(ELM) src/*.elm
