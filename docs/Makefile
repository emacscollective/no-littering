-include ../config.mk
include ../default.mk

docs: html

HTML  = index.html
HTML += migrate.html

html: $(HTML)

ORG_EVAL += --eval "(setq indent-tabs-mode nil)"
ORG_EVAL += --eval "(setq org-src-preserve-indentation nil)"
ORG_EVAL += --eval "(setq org-html-checkbox-type 'html)"
ORG_EVAL += --eval "(require 'ox-html)"
ORG_EVAL += --funcall org-html-export-to-html

%.html: ../%.org
	@printf "Generating $@\n"
	@$(EMACS_ORG) $< $(ORG_EVAL)

index.html: ../README.org
	@printf "Generating $@\n"
	@$(EMACS_ORG) $< $(ORG_EVAL)

DOCS_DOMAIN = docs.$(DOMAIN)
DOCS_TARGET = $(subst .,_,$(DOCS_DOMAIN)):$(PKG)/

publish: html
	@printf "Publishing readme...\n"
	@$(RCLONE) copy $(RCLONE_ARGS) index.html   $(DOCS_TARGET)
	@$(RCLONE) copy $(RCLONE_ARGS) migrate.html $(DOCS_TARGET)

CLEAN = $(HTML)

clean:
	@printf " Cleaning docs/*...\n"
	@rm -rf $(CLEAN)
