# Temporary makefile for testing, to be revised

SRCS=                       \
    utility/modules.js      \
    platform/memory.js      \
    ir/basicblocks.js       \
    ir/instructions.js      \
    ir/inline.js

all: run

doc: $(SRCS)
	jsdoc -d=doc $(SRCS)

run: 
	d8 --allow_natives_syntax $(SRCS)
