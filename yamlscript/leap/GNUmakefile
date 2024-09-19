SHELL := bash

BASE := $(shell pwd)

include .yamlscript/exercise.mk

YS_LOCAL_PREFIX := ../../../.local/v$(YS_VERSION)
ifeq (,$(shell [[ -d "$(YS_LOCAL_PREFIX)" ]] && echo ok))
YS_LOCAL_PREFIX := $(shell cd .. && pwd -P)/.local/v$(YS_VERSION)
endif

YS_LOCAL_BIN := $(YS_LOCAL_PREFIX)/bin
YS_BIN := $(YS_LOCAL_BIN)/ys-$(YS_VERSION)

YS_INSTALLER := .yamlscript/exercism-ys-installer
YS_INSTALLER_CMD := \
  bash $(YS_INSTALLER) $(YS_VERSION) $(YS_LOCAL_PREFIX) $(MAKE)

TEST_FILE ?= $(wildcard *-test.ys)

export PATH := $(YS_LOCAL_BIN):$(PATH)
export YSPATH := $(BASE)


#-------------------------------------------------------------------------------
default:
	@echo " No default make rule. Try 'make test'."

test: $(YS_BIN)
	prove -v $(TEST_FILE)

install-ys:
	@$(YS_INSTALLER_CMD)

uninstall-ys:
	rm -fr $(YS_LOCAL_PREFIX)


#-------------------------------------------------------------------------------
ifdef EXERCISM_YAMLSCRIPT_GHA
$(YS_BIN):

else ifeq (/mnt/,$(dir $(BASE)))
$(YS_BIN):

else
$(YS_BIN):
	@$(YS_INSTALLER_CMD) auto
endif
