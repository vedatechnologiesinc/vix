#———————————————————————————————————————————————————————————————————————————————
# base head

.ONESHELL:
.SHELLFLAGS := -eu -o pipefail -c
.DELETE_ON_ERROR:

SHELL := bash
MAKEFLAGS += --warn-undefined-variables
MAKEFLAGS += --no-builtin-rules

#———————————————————————————————————————————————————————————————————————————————
# program head

PROG=vix
BIN=$(HOME)/bin
BINARY=$(BIN)/$(PROG)
SCRIPT=$(PWD)/$(PROG)

LISP ?= sbcl
SBCL_FLAGS = --load
LISPWORKS_FLAGS = -build
LISP_FLAGS =

ifeq ($(LISP),lispworks)
  LISP_FLAGS=$(LISPWORKS_FLAGS)
else
  LISP_FLAGS=$(SBCL_FLAGS)
endif

#———————————————————————————————————————————————————————————————————————————————
# targets

.PHONY: all build install clean

all: build install

build:
	@$(LISP) $(LISP_FLAGS) src/build.lisp

install: build
	@mkdir -p $(BIN)
	@rm -f $(BINARY)
	@ln -s $(SCRIPT) $(BINARY)

clean:
	@rm -f $(NAME)
