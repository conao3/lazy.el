EMACS ?= emacs
LOAD_PATH = -L .

.PHONY: test
test:
	$(EMACS) $(LOAD_PATH) --batch -l ert -l test/lazy-test.el -f ert-run-tests-batch-and-exit
	$(EMACS) $(LOAD_PATH) --batch -l ert -l test/lazy-seq-test.el -f ert-run-tests-batch-and-exit

.PHONY: compile
compile:
	$(EMACS) $(LOAD_PATH) --batch \
		--eval "(setq byte-compile-warnings t)" \
		--eval "(require 'checkdoc)" \
		--eval "(checkdoc-file \"lazy.el\")" \
		--eval "(checkdoc-file \"lazy-seq.el\")"
	$(EMACS) $(LOAD_PATH) --batch \
		--eval "(require 'package-lint)" \
		-f package-lint-batch-and-exit lazy.el
	$(EMACS) $(LOAD_PATH) --batch -f batch-byte-compile lazy.el lazy-seq.el

.PHONY: clean
clean:
	rm -f *.elc test/*.elc
