TOP := $(dir $(lastword $(MAKEFILE_LIST)))

PKG = no-littering

ELS   = $(PKG).el
ELCS  = $(ELS:.el=.elc)

DEPS  = compat

EMACS      ?= emacs
EMACS_ARGS ?=

LOAD_PATH  ?= $(addprefix -L ../,$(DEPS))
LOAD_PATH  += -L .
