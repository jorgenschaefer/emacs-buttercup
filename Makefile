EMACS := emacs

.PHONY: test

all: test

test:
	$(EMACS) -batch -L . -l buttercup.el -f buttercup-run-markdown README.md
	$(EMACS) -batch -L . -l buttercup.el -f buttercup-run-discover
