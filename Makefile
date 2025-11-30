-include .config.mk
include default.mk

.PHONY: lisp docs

all: lisp docs

help:
	$(info make all        -- Build lisp)
	$(info make lisp       -- Build lisp)
	$(info make redo       -- Build lisp from scratch)
	$(info make docs       -- Build html documentation)
	$(info make html       -- Build html documentation)
	$(info make publish    -- Publish html documentation)
	$(info make clean      -- Remove built files)
	@printf "\n"

redo: clean lisp

lisp: $(ELCS) autoloads check-declare

docs:
	@$(MAKE) -C docs docs
html:
	@$(MAKE) -C docs html
publish:
	@$(MAKE) -C docs publish

CLEAN = $(ELCS) $(PKG)-autoloads.el

clean:
	@printf " Cleaning...\n"
	@rm -rf $(CLEAN)
	@$(MAKE) -C docs clean

autoloads: $(PKG)-autoloads.el

%.elc: %.el
	@printf "Compiling $<\n"
	@$(EMACS_BATCH) --funcall batch-byte-compile $<

check-declare:
	@printf " Checking function declarations\n"
	@$(EMACS_BATCH) --eval "(check-declare-directory default-directory)"

$(PKG)-autoloads.el: $(ELS)
	@printf " Creating $@\n"
	@$(EMACS_BATCH) --load autoload --eval "\
(let* ((file (expand-file-name \"$@\"))\
       (generated-autoload-file file)\
       (coding-system-for-write 'utf-8-emacs-unix)\
       (backup-inhibited t)\
       (version-control 'never)\
       (inhibit-message t))\
  (write-region (autoload-rubric file \"package\" t) nil file)\
  (update-directory-autoloads default-directory))" \
	2>&1 | sed "/^Package autoload is deprecated$$/d"
