# JavaScript VM command
JSVM = d8 --allow_natives_syntax

# Base program source files
BASE_SRCS =                 \
    utility/debug.js        \
    utility/modules.js      \
    utility/arrays.js       \
    utility/hashmap.js      \
    utility/strings.js      \
    platform/memory.js      \
    ir/instructions.js      \
    ir/inline.js            \
    ir/cfg.js               \
    ir/functions.js

# Main-specific source files
MAIN_SPEC_SRCS =            \
    main.js

# Test-specific source files
# Insert unit test files between testing.js and testmain.js
TEST_SPEC_SRCS =            \
    tests/testing.js        \
    tests/testmain.js

# Main target source files
MAIN_SRCS =                 \
    $(BASE_SRCS)            \
    $(MAIN_SPEC_SRCS)

# Test target source files
TEST_SRCS =                 \
    $(BASE_SRCS)            \
    $(TEST_SPEC_SRCS)

# All source files
ALL_SRCS =                  \
    $(BASE_SRCS)            \
    $(MAIN_SPEC_SRCS)       \
    $(TEST_SPEC_SRCS)

all: run

run: 
	$(JSVM) $(MAIN_SRCS)

test: 
	$(JSVM) $(TEST_SRCS)

doc: $(SRCS)
	jsdoc -d=../doc/source $(ALL_SRCS)

