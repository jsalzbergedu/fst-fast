CC ?= gcc
CFLAGS += -Wall
CFLAGS += -g
CFLAGS += -std=c99

.PHONY: all
all: fst_fast

fst_fast: fst_fast.c
	${CC} ${CFLAGS} fst_fast.c -o fst_fast

.PHONY: clean
clean:
	rm -rf fst_fast

.PHONY: clang-format
clang-format:
	clang-format -i fst_fast.c
