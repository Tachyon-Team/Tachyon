# Temporary makefile for testing, to be revised

SRCS=                       \
    utility/debug.js        \
    utility/modules.js      \
    utility/arrays.js       \
    platform/memory.js      \
    ir/instructions.js      \
    ir/inline.js            \
    ir/cfg.js

all: run

doc: $(SRCS)
	jsdoc -d=doc $(SRCS)

run: 
	d8 --allow_natives_syntax $(SRCS)
