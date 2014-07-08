SHELL = bash

FILES = $(wildcard *.r)

test: $(FILES)
	$(foreach file, $(FILES), echo ">>> Checking file $(file)" && Rscript $(file);)

default: test
