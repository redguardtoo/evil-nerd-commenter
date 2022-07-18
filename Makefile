SHELL = /bin/sh
EMACS ?= emacs
PROFILER =
RM= @rm -rf
EMACS_BATCH_OPTS=--batch -Q \
-L . \
-L deps/ \
-L deps/evil-1.14.2 \
-l deps/web-mode.el \
-l evil-nerd-commenter-sdk.el \
-l evil-nerd-commenter.el

.PHONY: test deps clean compile lint

# Delete byte-compiled files etc.
clean:
	$(RM) *~
	$(RM) \#*\#
	$(RM) deps/*
	$(RM) *.elc

deps:
	@mkdir -p deps;
	@if [ ! -f deps/evil-1.14.2/evil.el ]; then curl -L https://stable.melpa.org/packages/evil-1.14.2.tar | tar x -C deps/; fi;
	@if [ ! -f deps/web-mode.el ]; then curl -L https://raw.githubusercontent.com/fxbois/web-mode/master/web-mode.el > deps/web-mode.el; fi;

compile: deps
	$(RM) *.elc
	@$(EMACS) $(EMACS_BATCH_OPTS) -l tests/my-byte-compile.el 2>&1 | grep -E "([Ee]rror|[Ww]arning):" && exit 1 || exit 0

# Run tests.
test: compile deps
	@$(EMACS) $(EMACS_BATCH_OPTS) -l tests/evil-nerd-commenter-tests.el
