# Temporary makefile for testing, to be revised

SRCS=                       \
    platform/memory.js      \
    ir/instructions.js      \
    ir/inline.js            \
    ir/cfg.js

all: run

doc: $(SRCS)
	jsdoc -d=doc $(SRCS)

run: 
	d8 --allow_natives_syntax $(SRCS)
