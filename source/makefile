# makefile for Tachyon JavaScript compiler

#JavaScript VM program
JSVMPROG = d8

#JavaScript VM command options
JSVMOPTS = --allow_natives_syntax --nocollect_maps

#JavaScript VM command options when debugging
JSVMOPTSDEBUG = $(JSVMOPTS) --noopt

#JavaScript VM command
JSVM = $(JSVMPROG) $(JSVMOPTS)

# JavaScript VM command with rlwrap
RLWJSVM = rlwrap $(JSVM)

# For bootstrapping with Tachyon JavaScript to Scheme compiler
JS2SCM = parser/js2scm

# Base program source files
BASE_SRCS =                             \
    utility/debug.js                    \
    utility/system.js                   \
    utility/iterators.js                \
    utility/graph.js                    \
    utility/arrays.js                   \
    utility/heap.js                     \
    utility/hashmap.js                  \
    utility/set.js                      \
    utility/linkedlist.js               \
    utility/strings.js                  \
    utility/modules.js                  \
    utility/misc.js                     \
    utility/num.js                      \
    utility/xml.js                      \
    utility/html.js                     \
    compiler/targets.js                 \
    compiler/params.js                  \
    compiler/config.js                  \
    compiler/compiler.js                \
    compiler/init.js                    \
    compiler/bootstrap.js               \
    parser/misc.js                      \
    parser/scanner.js                   \
    parser/parser.js                    \
    parser/pp.js                        \
    parser/ast-passes.js                \
    ir/types.js                         \
    ir/static.js                        \
    ir/instructions.js                  \
    ir/constants.js                     \
    ir/iir.js                           \
    ir/cfg.js                           \
    ir/functions.js                     \
    ir/ast-to-ir.js                     \
    ir/optpatterns.js                   \
    ir/constprop.js                     \
    ir/commelim.js                      \
    ir/inlining.js                      \
    ir/lowering.js                      \
    platform/ffi.js                     \
    platform/mcb.js                     \
    runtime/layout.js                   \
    runtime/context.js                  \
    runtime/objects.js                  \
    backend/asm.js                      \
    backend/regalloc.js                 \
    backend/linearscan.js               \
    backend/backend.js                  \
    backend/x86/asm.js                  \
    backend/x86/config.js               \
    backend/x86/ir-to-asm.js            

# Runtime library source files
RUNTIME_SRCS =                          \
    runtime/utility.js                  \
    runtime/primitives.js               \
    runtime/strings.js                  \
    runtime/ffi.js                      \
    runtime/rtinit.js

# Standard library source files
STDLIB_SRCS =                           \
    stdlib/arrays.js                    \
    stdlib/errors.js                    \
    stdlib/extensions.js                \
    stdlib/functions.js                 \
    stdlib/math.js                      \
    stdlib/numbers.js                   \
    stdlib/objects.js                   \
    stdlib/strings.js

# Main-specific source files
MAIN_SPEC_SRCS =                        \
    main.js

# Test-specific source files
# Insert unit test files between testing.js and testmain.js
TEST_SPEC_SRCS =                        \
    tests/testing.js                    \
    utility/tests/*.js                  \
    stdlib/tests/*.js                   \
    ir/tests/*.js                       \
    runtime/tests/*.js                  \
    backend/tests/*.js                  \
    programs/tests/programs.js          \
    tests/testmain.js                   \

# Main target source files
MAIN_SRCS =                             \
    $(BASE_SRCS)                        \
    $(MAIN_SPEC_SRCS)

# Test target source files
TEST_SRCS =                             \
    $(BASE_SRCS)                        \
    $(TEST_SPEC_SRCS)

# All source files
ALL_SRCS =                              \
    $(RUNTIME_SRCS)                     \
    $(STDLIB_SRCS)                      \
    $(BASE_SRCS)                        \
    $(MAIN_SPEC_SRCS)                   \
    $(TEST_SPEC_SRCS)

all: run

load:
	$(JSVM) $(BASE_SRCS) --shell

run: 
	$(JSVM) $(MAIN_SRCS)

shell:
	$(RLWJSVM) $(MAIN_SRCS)

bootstrap:
	time $(JSVM) $(MAIN_SRCS) -- -bootstrap true

bootstrap-debug:
	echo "run $(JSVMOPTSDEBUG) $(MAIN_SRCS) -- -bootstrap true" > gdb-command
	echo "bt"                                                   >> gdb-command
	time gdb --command=gdb-command $(JSVMPROG)

test: 
	$(JSVM) $(TEST_SRCS)

perf: 
	./bench/d8.sh                                   \
        -cfgFile "bench/configs/time_benchs.json"   \
        -genReport "benchreport.html"

bootstrap-js2scm:
	$(JS2SCM) -scm $(MAIN_SRCS)

bootstrap-js2scm-debug:
	$(JS2SCM) -scm -debug $(MAIN_SRCS)

doc: $(SRCS)
	jsdoc -d=../doc/source $(MAIN_SRCS)

snapshot: makefile d8/d8-tachyon-exts.cc $(ALL_SRCS)
	rm -rf tachyon-snapshot tachyon-snapshot.tar.gz
	mkdir tachyon-snapshot
	(tar cf - $^ | (cd tachyon-snapshot ; tar xf -))
	tar cf tachyon-snapshot.tar tachyon-snapshot
	gzip -9 tachyon-snapshot.tar
