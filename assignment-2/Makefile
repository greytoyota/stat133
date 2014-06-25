SHELL = bash

EX = $(wildcard ex*.r)

default: $(EX)
	$(foreach file, $(EX), Rscript $(file);)

test: default
