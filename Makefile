.PHONY: test

all: test

test:
	emacs -batch -L . -l buttercup.el -f buttercup-markdown-runner README.md
	emacs -batch -L . -l buttercup-test.el -f buttercup-run
