SHELL = bash

FILES = $(wildcard *.r)

test: $(FILES)
	$(foreach file, $(FILES), Rscript $(file);)

default: test
