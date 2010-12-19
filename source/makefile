# JavaScript VM command
JSVM = d8 --allow_natives_syntax

# For bootstrapping with Tachyon JavaScript to Scheme compiler
JS2SCM = parser/js2scm

# Base program source files
BASE_SRCS =                         \
    utility/constants.js            \
    utility/debug.js                \
    utility/system.js               \
    utility/iterators.js            \
    utility/graph.js                \
    utility/arrays.js               \
    utility/hashmap.js              \
    utility/linkedlist.js           \
    utility/strings.js              \
    utility/modules.js              \
    utility/misc.js                 \
    utility/xml.js                  \
    utility/html.js                 \
    parser/misc.js                  \
    parser/scanner.js               \
    parser/parser.js                \
    parser/pp.js                    \
    parser/ast-passes.js            \
    platform/ffi.js                 \
    platform/memory.js              \
    ir/static.js                    \
    ir/instructions.js              \
    ir/iir.js                       \
    ir/cfg.js                       \
    ir/functions.js                 \
    ir/ir.js                        \
    ir/optpatterns.js               \
    ir/constprop.js                 \
    ir/commelim.js                  \
    ir/inlining.js                  \
    ir/lowering.js                  \
    frontend.js                     \
    runtime/objlayout.js            \
    runtime/context.js              \
    runtime/objects.js              \
    codegen/asm.js                  \
    codegen/asm-x86.js              \
    codegen/linearscan.js           \
    codegen/backend.js              \
    codegen/ir-to-asm-x86.js        \
    codegen/regalloc-config-x86.js  \
    jslib/errors.js                 \
    compiler.js                     \
    init.js

# Main-specific source files
MAIN_SPEC_SRCS =                    \
    main.js

# Test-specific source files
# Insert unit test files between testing.js and testmain.js
TEST_SPEC_SRCS =                    \
    tests/testing.js                \
    utility/tests/*.js              \
    ir/tests/*.js                   \
    runtime/tests/*.js              \
    codegen/tests/*.js              \
    programs/tests/*.js             \
    tests/testmain.js

# Main target source files
MAIN_SRCS =                         \
    $(BASE_SRCS)                    \
    $(MAIN_SPEC_SRCS)

# Test target source files
TEST_SRCS =                         \
    $(BASE_SRCS)                    \
    $(TEST_SPEC_SRCS)

# All source files
ALL_SRCS =                          \
    $(BASE_SRCS)                    \
    $(MAIN_SPEC_SRCS)               \
    $(TEST_SPEC_SRCS)

all: run

run: 
	$(JSVM) $(MAIN_SRCS)

bootstrap-js2scm: 
	$(JS2SCM) -scm $(MAIN_SRCS)

test: 
	$(JSVM) $(TEST_SRCS)

perf: 
	./bench/d8.sh                                   \
        -cfgFile "bench/configs/time_benchs.json"   \
        -genReport "benchreport.html"

doc: $(SRCS)
	jsdoc -d=../doc/source $(MAIN_SRCS)

