TOP := $(dir $(lastword $(MAKEFILE_LIST)))

PKG = no-littering

ELS   = $(PKG).el
ELCS  = $(ELS:.el=.elc)

DEPS  = compat

DOMAIN      ?= emacsmirror.net
CFRONT_DIST ?= E1IXJGPIOM4EUW

EMACS      ?= emacs
EMACS_ARGS ?=

LOAD_PATH  ?= $(addprefix -L ../,$(DEPS))
LOAD_PATH  += -L .

ORG_LOAD_PATH ?= -L ../../htmlize
