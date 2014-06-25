SHELL = bash

EX = $(wildcard ex*.r)

test: $(EX)
	$(foreach file, $(EX), Rscript $(file);)

default: test
