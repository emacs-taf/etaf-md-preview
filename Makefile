.DEFAULT_GOAL := check
.NOTPARALLEL:
EMACS ?= emacs
LOAD_PATH = -L . -L lisp -L tests -L ../../etaf -L ../../etaf-ui -L ../../ebox -L ../../ecss
SOURCES = lisp/etaf-md-preview-parser.el lisp/etaf-md-preview-theme.el
SOURCES += lisp/etaf-md-preview-model.el lisp/etaf-md-preview-views.el
SOURCES += lisp/etaf-md-preview-app.el etaf-md-preview.el
SOURCES += $(wildcard examples/*.el)
SOURCES += scripts/md-preview-gui-probe.el

.PHONY: compile load check clean structure-check setup-hooks acceptance api-check test benchmark

compile:
	rm -f *.elc lisp/*.elc tests/*.elc examples/*.elc scripts/*.elc
	$(EMACS) -Q --batch $(LOAD_PATH) \
		--eval '(setq load-prefer-newer t byte-compile-error-on-warn t)' \
		--eval '(require (quote cl-lib))' \
		--eval '(unless (cl-every (function byte-compile-file) (quote ($(foreach file,$(SOURCES),"$(file)")))) (kill-emacs 1))'

load: compile
	$(EMACS) -Q --batch $(LOAD_PATH) --eval '(require (quote etaf-md-preview))' --eval '(princ "etaf-md-preview load OK\n")'

check: structure-check compile acceptance api-check

structure-check:
	python3 scripts/check-repository.py

setup-hooks:
	git config --local core.hooksPath .githooks

acceptance:
	python3 scripts/run-acceptance.py --emacs "$(EMACS)"

api-check:
	$(EMACS) -Q --batch -L . $(LOAD_PATH) --eval '(setq load-prefer-newer t)' -l scripts/check-api.el

benchmark:
	$(EMACS) -Q --batch $(LOAD_PATH) --eval '(setq load-prefer-newer t)' -l benchmarks/render-large-document.el

test: acceptance

clean:
	rm -f *.elc lisp/*.elc tests/*.elc examples/*.elc scripts/*.elc benchmarks/*.elc
