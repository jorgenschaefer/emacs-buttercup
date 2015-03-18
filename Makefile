.PHONY: test

all: test

test:
	emacs -batch -L . -l buttercup.el -f buttercup-markdown-runner README.md
