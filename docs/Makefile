-include ../config.mk
include ../default.mk

docs: html

HTML  = $(PKG).html
HTML += migrate.html

html: $(HTML)

ORG_ARGS  = --batch -Q $(ORG_LOAD_PATH)
ORG_EVAL += --eval "(setq indent-tabs-mode nil)"
ORG_EVAL += --eval "(setq org-src-preserve-indentation nil)"
ORG_EVAL += --eval "(setq org-html-checkbox-type 'html)"
ORG_EVAL += --eval "(require 'ox-html)"
ORG_EVAL += --funcall org-html-export-to-html

%.html: ../%.org
	@printf "Generating $@\n"
	@$(EMACS) $(ORG_ARGS) $< $(ORG_EVAL)

no-littering.html: ../README.org
	@printf "Generating $@\n"
	@$(EMACS) $(ORG_ARGS) $< $(ORG_EVAL)

PUBLISH_PATH   ?= /manual/
S3_BUCKET      ?= s3://$(DOMAIN)
PUBLISH_TARGET  = $(S3_BUCKET)$(PUBLISH_PATH)
CFRONT_PATHS    = $(PKG).html

comma := ,
empty :=
space := $(empty) $(empty)

publish: html
	@for f in $(HTML); do aws s3 cp $$f $(PUBLISH_TARGET); done
	@printf "Generating CDN invalidation\n"
	@aws cloudfront create-invalidation --distribution-id $(CFRONT_DIST) --paths \
	"$(subst $(space),$(comma),$(addprefix $(PUBLISH_PATH),$(CFRONT_PATHS)))" > /dev/null

CLEAN = $(HTML)

clean:
	@printf " Cleaning docs/*...\n"
	@rm -rf $(CLEAN)
