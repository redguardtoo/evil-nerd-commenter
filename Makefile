SHELL = /bin/sh
EMACS ?= emacs
FILES = $(filter-out evil-test-helpers.el evil-tests.el evil-pkg.el,$(wildcard evil*.el))
PROFILER =

.PHONY: test

# Delete byte-compiled files etc.
clean:
	rm -f *~
	rm -f \#*\#
	rm -f *.elc

# Run tests.
test:
	$(EMACS) -batch -Q -l evil-nerd-commenter-sdk.el -l evil-nerd-commenter.el -l evil-nerd-commenter-tests.el
	rm -f *.elc .depend