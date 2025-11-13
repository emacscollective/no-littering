TOP := $(dir $(lastword $(MAKEFILE_LIST)))

DOMAIN ?= emacsmirror.org

PKG = no-littering

ELS   = $(PKG).el
ELCS  = $(ELS:.el=.elc)

DEPS  = compat

EMACS      ?= emacs
EMACS_ARGS ?=

LOAD_PATH  ?= $(addprefix -L ../,$(DEPS))
LOAD_PATH  += -L .

ORG_LOAD_PATH ?= -L ../../htmlize

RCLONE      ?= rclone
RCLONE_ARGS ?= -v
