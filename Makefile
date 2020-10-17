SHELL = /bin/sh
EMACS ?= emacs
PROFILER =

.PHONY: test deps

# Delete byte-compiled files etc.
clean:
	@rm -f *~
	@rm -f \#*\#
	@rm -rf deps/
	@rm -f *.elc

deps:
	@mkdir -p deps;
	@if [ ! -f deps/web-mode.el ]; then curl -L https://raw.githubusercontent.com/fxbois/web-mode/master/web-mode.el > deps/web-mode.el; fi;

# Run tests.
test: deps
	$(EMACS) -batch -Q -L deps/ -l deps/web-mode.el -l evil-nerd-commenter-sdk.el -l evil-nerd-commenter.el -l evil-nerd-commenter-tests.el
