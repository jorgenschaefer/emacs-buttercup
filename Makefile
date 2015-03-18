EMACS := emacs

.PHONY: test

all: test

test:
	$(EMACS) -batch -L . -l buttercup.el -f buttercup-markdown-runner README.md
	$(EMACS) -batch -L . -l buttercup-test.el -f buttercup-run
