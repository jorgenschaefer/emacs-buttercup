EMACS := emacs
VERSION := $(shell sed -ne 's/^;; Version: \(.*\)/\1/p' buttercup.el)
ELISP_FILES := $(shell ls *.el | grep -v -- '-pkg\.el$$')
DISTFILES := $(ELISP_FILES) buttercup-pkg.el README.md

.PHONY: test compile clean

all: test

test: compile
	./bin/buttercup -L .
	$(EMACS) -batch -L . -l buttercup.el -f buttercup-run-markdown docs/writing-tests.md

compile: $(patsubst %.el,%.elc,$(ELISP_FILES))

%.elc: %.el
	$(EMACS) -batch -L . -f batch-byte-compile $<

release: clean test
	mkdir -p dist
	tar -c $(DISTFILES) --transform "s,^,buttercup-$(VERSION)/," --transform 's/README.md/README.txt/' > "dist/buttercup-$(VERSION).tar"

clean:
	rm -f *.elc
