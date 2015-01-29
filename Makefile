LISP_FILES=$(shell find . -name '*.lisp')
ASDF_TREE=~/quicklisp/
BIN=dist/lxc-wrapper
DESTDIR=/usr/bin/

$(BIN): $(LISP_FILES)
	mkdir -p dist
	buildapp --load-system lxc-wrapper --entry lxc-wrapper:main --output $(BIN) --asdf-tree $(ASDF_TREE) --compress-core

clean:
	rm -rf dist/

install: $(BIN)
	cp $(BIN) $(DESTDIR)

.PHONY: clean install
