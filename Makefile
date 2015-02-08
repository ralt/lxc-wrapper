APP_NAME=lxc-wrapper
VERSION=
LISP_FILES=$(shell find . -name '*.lisp')
ASDF_TREE ?= ~/quicklisp/
DIST_FOLDER=dist/root/usr/bin
APP_OUT=dist/root/usr/bin/lxc-wrapper
QL_LOCAL=$(PWD)/.quicklocal/quicklisp
QUICKLISP_SCRIPT=http://beta.quicklisp.org/quicklisp.lisp
LOCAL_OPTS=--noinform --noprint --disable-debugger --no-sysinit --no-userinit
QL_OPTS=--load $(QL_LOCAL)/setup.lisp
LISP ?= sbcl
SOURCES := $(wildcard src/*.lisp) $(wildcard *.asd)
BUILDAPP = ./bin/buildapp
TEST_SOURCES=$(shell find test/ -name '*.lisp')

.PHONY: clean install release deb rpm test man

all: $(APP_OUT)

test: $(TEST_SOURCES) $(QL_LOCAL)/setup.lisp install-deps
	@sbcl $(QL_OPTS) \
		--eval '(ql:quickload :fiveam)' \
		--eval '(ql:quickload :lxc-wrapper)' \
		--eval '(asdf:test-system :lxc-wrapper)' \
		--quit

release:
	make clean
	make
	make man
	make deb
	make rpm

man:
	mkdir -p dist/root/usr/share/man/man1/
	pandoc -s -t man manpage.md > dist/root/usr/share/man/man1/lxc-wrapper.1
	gzip dist/root/usr/share/man/man1/lxc-wrapper.1

deb: $(APP_OUT)
	@fpm -p dist/ \
		-d "lxc (>= 1.0)" \
		-s dir -t deb -n $(APP_NAME) -v $(VERSION) -C dist/root usr
	@gpg --output dist/$(APP_NAME)_$(VERSION)-deb.sig \
		--detach-sig dist/$(APP_NAME)_$(VERSION)_amd64.deb

rpm: $(APP_OUT)
	@fpm -p dist/ \
		-d "lxc" \
		-s dir -t rpm -n $(APP_NAME) -v $(VERSION) -C dist/root usr
	@gpg --output dist/$(APP_NAME)_$(VERSION)-rpm.sig \
		--detach-sig dist/$(APP_NAME)-$(VERSION)-1.x86_64.rpm

install: $(APP_OUT)
	install $(APP_OUT) $(DESTDIR)/usr/bin
	install -g 0 -o 0 -m 0644 dist/root/usr/share/man/man1/lxc-wrapper.1.gz /usr/share/man/man1/

bin:
	@mkdir bin

clean:
	@-yes | rm -rf $(QL_LOCAL)
	@-rm -f $(APP_OUT) deps install-deps
	@-rm -f dist/lxc-wrapper*

$(QL_LOCAL)/setup.lisp:
	@curl -O $(QUICKLISP_SCRIPT)
	@sbcl $(LOCAL_OPTS) \
		--load quicklisp.lisp \
		--eval '(quicklisp-quickstart:install :path "$(QL_LOCAL)")' \
		--eval '(quit)'

deps:
	@sbcl $(LOCAL_OPTS) $(QL_OPTS) \
             --eval '(push "$(PWD)/" asdf:*central-registry*)' \
             --eval '(ql:quickload :lxc-wrapper)' \
             --eval '(quit)'
	@touch $@

install-deps: $(QL_LOCAL)/setup.lisp deps
	@touch $@

bin/buildapp: bin $(QL_LOCAL)/setup.lisp
	@cd $(shell sbcl $(LOCAL_OPTS) $(QL_OPTS) \
				--eval '(ql:quickload :buildapp :silent t)' \
				--eval '(format t "~A~%" (asdf:system-source-directory :buildapp))' \
				--eval '(quit)') && \
	$(MAKE) DESTDIR=$(PWD) install

$(APP_OUT): $(SOURCES) bin/buildapp $(QL_LOCAL)/setup.lisp install-deps
	@mkdir -p $(DIST_FOLDER)
	@$(BUILDAPP) --logfile /tmp/build.log \
			--sbcl sbcl \
			--asdf-path . \
			--asdf-tree $(QL_LOCAL)/local-projects \
			--asdf-tree $(QL_LOCAL)/dists \
			--asdf-path . \
			--load-system $(APP_NAME) \
			--entry $(APP_NAME):main \
			--compress-core \
			--output $(APP_OUT)
