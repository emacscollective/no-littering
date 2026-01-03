TOP := $(dir $(lastword $(MAKEFILE_LIST)))

DOMAIN ?= emacsmirror.org

PKG = no-littering

ELS   = $(PKG).el
ELCS  = $(ELS:.el=.elc)

DEPS  = compat

LOAD_PATH     ?= $(addprefix -L ../,$(DEPS))
LOAD_PATH     += -L .
ORG_LOAD_PATH ?= -L ../org/lisp

EMACS       ?= emacs
EMACS_ARGS  ?=
EMACS_Q_ARG ?= -Q
EMACS_BATCH ?= $(EMACS) $(EMACS_Q_ARG) --batch $(EMACS_ARGS) $(LOAD_PATH)
EMACS_ORG   ?= $(EMACS) $(EMACS_Q_ARG) --batch $(EMACS_ARGS) $(ORG_LOAD_PATH)

RCLONE      ?= rclone
RCLONE_ARGS ?= -v
