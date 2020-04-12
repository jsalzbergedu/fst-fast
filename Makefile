CC ?= gcc
CFLAGS += -Wall
CFLAGS += -g
CFLAGS += -std=c99
CFLAGS += -I.
LUALIBS = $(shell pkg-config --cflags --libs lua-5.2)
LUAINCLUDES = $(shell pkg-config --cflags lua-5.2)

.PHONY: all
all: fst_fast.o fst_fast.so fst_fast_runner

fst_fast.o: fst_fast.c fst_fast.h
	${CC} ${CFLAGS} ${LUAINCLUDES} -c fst_fast.c  -o fst_fast.o

fst_fast.so: fst_fast.o
	${CC} -shared -fPIC -o fst_fast.so fst_fast.o

fst_fast_runner.o: fst_fast_runner.c fst_fast.h
	${CC} ${CFLAGS} ${LUAINCLUDES} -c fst_fast_runner.c -o fst_fast_runner.o

fst_fast_runner: fst_fast_runner.o fst_fast.o fst_fast.h
	${CC} ${CFLAGS} ${LUALIBS} -o fst_fast_runner fst_fast.o fst_fast_runner.o


.PHONY: clean
clean:
	rm -rf fst_fast_runner fst_fast.so fst_fast.o fst_fast_runner.o

.PHONY: clang-format
clang-format:
	clang-format -i fst_fast.c
