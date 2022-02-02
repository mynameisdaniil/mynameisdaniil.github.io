# just checking commit auto-sign
ELM = /usr/bin/env elm
PYTHON3 = /usr/bin/env python3

default: build

build:
	$(ELM) make --debug src/*.elm

release:
	$(ELM) make --optimize src/*.elm

server:
	$(PYTHON3) -m http.server
