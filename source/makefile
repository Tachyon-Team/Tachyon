# _________________________________________________________________________
#
#             Tachyon : A Self-Hosted JavaScript Virtual Machine
#
#
#  This file is part of the Tachyon JavaScript project. Tachyon is
#  distributed at:
#  http://github.com/Tachyon-Team/Tachyon
#
#
#  Copyright (c) 2011, Universite de Montreal
#  All rights reserved.
#
#  This software is licensed under the following license (Modified BSD
#  License):
#
#  Redistribution and use in source and binary forms, with or without
#  modification, are permitted provided that the following conditions are
#  met:
#    * Redistributions of source code must retain the above copyright
#      notice, this list of conditions and the following disclaimer.
#    * Redistributions in binary form must reproduce the above copyright
#      notice, this list of conditions and the following disclaimer in the
#      documentation and/or other materials provided with the distribution.
#    * Neither the name of the Universite de Montreal nor the names of its
#      contributors may be used to endorse or promote products derived
#      from this software without specific prior written permission.
#
#  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS
#  IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED
#  TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A
#  PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL UNIVERSITE DE
#  MONTREAL BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
#  EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
#  PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
#  PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
#  LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
#  NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
#  SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
# _________________________________________________________________________

# makefile for Tachyon JavaScript compiler

ROOT_DIR = `(cd ..;pwd)`

#JavaScript VM program
JSVMPROG = d8
JSVMPROG64 = d8_64

#JavaScript VM command options
JSVMOPTS = --allow_natives_syntax --nocollect_maps

#JavaScript VM command options when debugging
JSVMOPTSDEBUG = $(JSVMOPTS) --noopt

#JavaScript VM command
JSVM = $(JSVMPROG) $(JSVMOPTS)
JSVM64 = $(JSVMPROG64) $(JSVMOPTS)

# JavaScript VM command with rlwrap
RLWJSVM = rlwrap $(JSVM)
RLWJSVM64 = rlwrap $(JSVM64)

# For bootstrapping with Tachyon JavaScript to Scheme compiler
JS2SCM = parser/js2scm

# For debugging Tachyon
JS2JS = parser/js2js

# Base program source files
BASE_SRCS =                             \
    utility/log.js                      \
    utility/debug.js                    \
    utility/system.js                   \
    utility/iterators.js                \
    utility/graph.js                    \
    utility/arrays.js                   \
    utility/heap.js                     \
    utility/hashmap.js                  \
    utility/hashset.js                  \
    utility/linkedlist.js               \
    utility/strings.js                  \
    utility/modules.js                  \
    utility/misc.js                     \
    utility/num.js                      \
    utility/xml.js                      \
    utility/html.js                     \
    compiler/sources.js                 \
    compiler/targets.js                 \
    compiler/params.js                  \
    compiler/config.js                  \
    compiler/compiler.js                \
    compiler/bootstrap.js               \
    compiler/imgwriter.js               \
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
    ir/cfgvis.js                        \
    analysis/usedist.js                 \
    runtime/layout.js                   \
    runtime/context.js                  \
    runtime/objects.js                  \
    runtime/misc.js                     \
    platform/ffi.js                     \
    platform/mcb.js                     \
    backend/asm.js                      \
    backend/regalloc.js                 \
    backend/onthefly.js                 \
    backend/linearscan.js               \
    backend/backend.js                  \
    backend/x86/asm.js                  \
    backend/x86/config.js               \
    backend/x86/ir-to-asm.js            \
    profiler/event_recording_profiler.js

# Runtime library source files
RUNTIME_SRCS =                          \
    runtime/utility.js                  \
    runtime/primitives.js               \
    runtime/strings.js                  \
    runtime/ffi.js                      \
    runtime/rtinit.js

# Standard library source files
STDLIB_SRCS =                           \
    stdlib/object.js                    \
    stdlib/function.js                  \
    stdlib/array.js                     \
    stdlib/error.js                     \
    stdlib/boolean.js                   \
    stdlib/number.js                    \
    stdlib/string.js                    \
    stdlib/math.js                      \
    stdlib/date.js                      \
    stdlib/json.js                      \
    stdlib/extensions.js                \
    stdlib/regexp/*.js                  \
    stdlib/regexp.js

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

all: tachyon compiler/sources.js

tachyon: makefile tachyon.in
	sed -e "s:@JSVMPROG@:$(JSVMPROG):g" -e "s:@ROOT_DIR@:$(ROOT_DIR):g" -e "s:@SOURCE_DIR@:$(ROOT_DIR)/source:g" -e "s:@BASE_SRCS@:$(BASE_SRCS):g" -e "s:@RUNTIME_SRCS@:$(RUNTIME_SRCS):g" -e "s:@STDLIB_SRCS@:$(STDLIB_SRCS):g" -e "s:@MAIN_SPEC_SRCS@:$(MAIN_SPEC_SRCS):g" -e "s:@TEST_SPEC_SRCS@:$(TEST_SPEC_SRCS):g" tachyon.in > tachyon
	chmod +x tachyon

compiler/sources.js: makefile compiler/sources.js.in
	cp compiler/sources.js.in compiler/sources.js
	@echo "var TACHYON_ROOT_DIR = \"$(ROOT_DIR)\";"  >> compiler/sources.js
	@echo ""                                         >> compiler/sources.js
	@echo "var TACHYON_SOURCE_DIR = \"$(ROOT_DIR)/source\";" >> compiler/sources.js
	@echo ""                                         >> compiler/sources.js
	@echo "var TACHYON_BASE_SRCS = ["                >> compiler/sources.js
	@sep=""
	@for f in $(BASE_SRCS); do \
	  echo "$$sep\"$$f\"" >> compiler/sources.js; \
	  sep=","; \
	done
	@echo "];"                                       >> compiler/sources.js
	@echo ""                                         >> compiler/sources.js
	@echo "var TACHYON_RUNTIME_SRCS = ["             >> compiler/sources.js
	@sep=""
	@for f in $(RUNTIME_SRCS); do \
	  echo "$$sep\"$$f\"" >> compiler/sources.js; \
	  sep=","; \
	done
	@echo "];"                                       >> compiler/sources.js
	@echo ""                                         >> compiler/sources.js
	@echo "var TACHYON_STDLIB_SRCS = ["              >> compiler/sources.js
	@sep=""
	@for f in $(STDLIB_SRCS); do \
	  echo "$$sep\"$$f\"" >> compiler/sources.js; \
	  sep=","; \
	done
	@echo "];"                                       >> compiler/sources.js
	@echo ""                                         >> compiler/sources.js
	@echo "var TACHYON_MAIN_SPEC_SRCS = ["           >> compiler/sources.js
	@sep=""
	@for f in $(MAIN_SPEC_SRCS); do \
	  echo "$$sep\"$$f\"" >> compiler/sources.js; \
	  sep=","; \
	done
	@echo "];"                                       >> compiler/sources.js
	@echo ""                                         >> compiler/sources.js
	@echo "var TACHYON_TEST_SPEC_SRCS = [         "  >> compiler/sources.js
	@sep=""
	@for f in $(TEST_SPEC_SRCS); do \
	  echo "$$sep\"$$f\"" >> compiler/sources.js; \
	  sep=","; \
	done
	@echo "];"                                       >> compiler/sources.js

load:
	$(JSVM) $(BASE_SRCS) --shell

run:
	$(JSVM) $(MAIN_SRCS)

shell:
	$(RLWJSVM) $(MAIN_SRCS)

shell64:
	$(RLWJSVM64) $(MAIN_SRCS) -- -x86_64

bt-parser:
	$(JSVM) $(BASE_SRCS) bt-parser.js bt-parser32.js

bt-parser64:
	$(JSVM64) $(BASE_SRCS) bt-parser.js bt-parser64.js

mem-fib:
	$(JSVM) --no-opt --trace_gc --trace_gc_nvp $(BASE_SRCS) test-fib.js > gc_trace.txt && python mem.py

host-vm:
	(cd host ; ./install-host-vm)

host-vm-v8:
	(cd host ; ./install-host-vm v8)

gc:
	$(JSVM) $(MAIN_SRCS) -- -gc
	gcc -std=c99 -c -o host/gc-generated.o host/gc-generated.c

bootstrap:
	time $(JSVM) $(MAIN_SRCS) -- -bootstrap -v=all

bootstrap64:
	time $(JSVM64) $(MAIN_SRCS) -- -bootstrap -x86_64 -v=all

bootstrap-debug:
	echo "run $(JSVMOPTSDEBUG) $(MAIN_SRCS) -- -bootstrap" > gdb-command
	echo "bt"                                              >> gdb-command
	time gdb --command=gdb-command $(JSVMPROG)

image:
	$(JSVM) $(MAIN_SRCS) -- -image -v=all
	gcc -m32 -o host/tachyon host/loader.c host/tachyon-exts.c host/tachyon.S

test:
	$(JSVM) $(TEST_SRCS) -- -v=all

test64:
	$(JSVM64) $(TEST_SRCS) -- -x86_64 -v=all

perf:
	./bench/d8.sh                                   \
        -cfgFile "bench/configs/time_benchs.json"   \
        -genReport "benchreport.html"

bootstrap-js2scm:
	$(JS2SCM) -scm $(MAIN_SRCS)

bootstrap-js2scm-debug:
	$(JS2SCM) -scm -debug $(MAIN_SRCS)

tachyon-debug.js: $(MAIN_SRCS)
	$(JS2JS) -debug $(MAIN_SRCS) > tachyon-debug.js

doc: $(SRCS)
	jsdoc -d=../doc/source $(MAIN_SRCS)

snapshot: makefile d8/d8-tachyon-exts.cc $(ALL_SRCS)
	rm -rf tachyon-snapshot tachyon-snapshot.tar.gz
	mkdir tachyon-snapshot
	(tar cf - $^ | (cd tachyon-snapshot ; tar xf -))
	tar cf tachyon-snapshot.tar tachyon-snapshot
	gzip -9 tachyon-snapshot.tar

clean:
	rm -f compiler/sources.js tachyon
