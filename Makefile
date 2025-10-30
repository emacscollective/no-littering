-include .config.mk
include default.mk

.PHONY: lisp docs

all: lisp docs

help:
	$(info make all          - generate byte-code and autoloads)
	$(info make lisp         - generate byte-code and autoloads)
	$(info make docs         - generate html readme file)
	$(info make html         - generate html readme file)
	$(info make publish      - publish html readme file)
	$(info make redo         - re-generate byte-code and autoloads)
	$(info make clean        - remove generated files)
	@printf "\n"

redo: clean lisp

lisp: $(ELCS) loaddefs check-declare

docs:
	@$(MAKE) -C docs docs
html:
	@$(MAKE) -C docs html
publish:
	@$(MAKE) -C docs publish

CLEAN  = $(ELCS) $(PKG)-autoloads.el

clean:
	@printf " Cleaning...\n"
	@rm -rf $(CLEAN)
	@$(MAKE) -C docs clean

loaddefs: $(PKG)-autoloads.el

%.elc: %.el
	@printf "Compiling $<\n"
	@$(EMACS) -Q --batch $(EMACS_ARGS) $(LOAD_PATH) -f batch-byte-compile $<

check-declare:
	@printf " Checking function declarations\n"
	@$(EMACS) -Q --batch $(EMACS_ARGS) $(LOAD_PATH) \
	--eval "(check-declare-directory default-directory)"

$(PKG)-autoloads.el: $(ELS)
	@printf " Creating $@\n"
	@$(EMACS) -Q --batch -l autoload -l cl-lib --eval "\
(let* ((file (expand-file-name \"$@\"))\
       (generated-autoload-file file)\
       (coding-system-for-write 'utf-8-emacs-unix)\
       (backup-inhibited t)\
       (version-control 'never)\
       (inhibit-message t))\
  (write-region (autoload-rubric file \"package\" t) nil file)\
  (update-directory-autoloads default-directory))" \
	2>&1 | sed "/^Package autoload is deprecated$$/d"
