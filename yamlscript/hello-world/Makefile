SHELL := bash

test ?= $(wildcard *-test.ys)

export YSPATH=$(shell pwd -P)


default:

test:
	prove -v $(test)
