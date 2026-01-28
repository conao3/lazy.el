EMACS ?= emacs
LOAD_PATH = -L .

.PHONY: test
test:
	$(EMACS) $(LOAD_PATH) --batch -l ert -l test/lazy-test.el -f ert-run-tests-batch-and-exit

.PHONY: compile
compile:
	$(EMACS) $(LOAD_PATH) --batch -f batch-byte-compile lazy.el

.PHONY: clean
clean:
	rm -f *.elc test/*.elc
