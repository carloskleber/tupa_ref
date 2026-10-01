# Builds the TAGS driver of the cross-code validation (docs/validation/tags-xval.md).
# Needs: gcc, LAPACK, OpenBLAS, FFTW3 and the TAGS + Cubature submodules:
#   git submodule update --init --recursive benchmarks/tags
TAGS = ../tags
CC = gcc
CFLAGS = -O2 -std=c11 -fopenmp -Wall -I$(TAGS)/src -I$(TAGS)/cubature
LINK = -llapack -lopenblas -lgfortran -lpthread -lfftw3 -lm -ldl

tags_hem: tags_hem.c $(TAGS)/src/electrode.c $(TAGS)/src/auxiliary.c $(TAGS)/src/grid.c $(TAGS)/src/linalg.c $(TAGS)/cubature/hcubature.c
	$(CC) $(CFLAGS) -o $@ $^ $(LINK)

clean:
	rm -f tags_hem
.PHONY: clean
