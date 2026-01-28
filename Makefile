EMACS ?= emacs
LOAD_PATH = -L .

.PHONY: test
test:
	$(EMACS) $(LOAD_PATH) --batch -l ert -l test/lazy-test.el -f ert-run-tests-batch-and-exit

.PHONY: compile
compile:
	$(EMACS) $(LOAD_PATH) --batch \
		--eval "(setq byte-compile-warnings t)" \
		--eval "(require 'checkdoc)" \
		--eval "(checkdoc-file \"lazy.el\")" \
		-f batch-byte-compile lazy.el

.PHONY: clean
clean:
	rm -f *.elc test/*.elc
