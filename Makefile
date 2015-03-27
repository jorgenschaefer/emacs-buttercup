EMACS := emacs
VERSION = $(shell sed -ne 's/^;; Version: \(.*\)/\1/p' buttercup.el)
DISTFILES = buttercup.el buttercup-compat.el buttercup-pkg.el README.md


.PHONY: test

all: test

test:
	$(EMACS) -batch -L . -l buttercup.el -f buttercup-run-markdown README.md
	$(EMACS) -batch -L . -l buttercup.el -f buttercup-run-discover

tar:
	mkdir -p dist
	tar -c $(DISTFILES) --transform "s,^,buttercup-$(VERSION)/," --transform 's/README.md/README.txt/' > "dist/buttercup-$(VERSION).tar"
