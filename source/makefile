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
#  License):#
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

#
# Makefile for Tachyon JavaScript compiler
#

# Include the common makefile
include makefile.common

# Base program source files
BASE_SRCS =                             \
    $(COMMON_SRCS)                      \
    compiler/sources.js                 \
    compiler/params.js                  \
    compiler/config.js                  \
    compiler/compiler.js                \
    compiler/init.js                    \
    compiler/bootstrap.js               \
    compiler/imgwriter.js               \
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
    analysis/typeprop.js                \
    runtime/layout.js                   \
    runtime/context.js                  \
    runtime/objects.js                  \
    runtime/misc.js                     \
    platform/ffi.js                     \
    platform/mcb.js                     \
    backend/codeblock.js                \
    backend/blockorder.js               \
    backend/liveness.js                 \
    backend/linker.js                   \
    backend/backend.js                  \
    backend/x86/operands.js             \
    backend/x86/instr-table.js          \
    backend/x86/instructions.js         \
    backend/x86/assembler.js            \
    backend/x86/peephole.js             \
    backend/x86/callconvs.js            \
    backend/x86/regalloc.js             \
    backend/x86/codegen.js              \
    backend/x86/ir-to-asm.js            \
    backend/x86/backend.js

# Runtime library source files
RUNTIME_SRCS =                          \
    runtime/utility.js                  \
    runtime/primitives.js               \
    runtime/strings.js                  \
    runtime/ffi.js                      \
    runtime/rtinit.js

# Main-specific source files
MAIN_SPEC_SRCS =                        \
    repl.js                             \
    main.js

# Test-specific source files
TEST_SPEC_SRCS =                        \
    tests/testing.js                    \
    utility/tests/*.js                  \
    stdlib/tests/*.js                   \
    ir/tests/cfg.js                     \
    ir/tests/ir.js                      \
    ir/tests/constprop.js               \
    runtime/tests/*.js                  \
    backend/x86/tests/assembler.js      \
    backend/x86/tests/ir-to-asm.js      \
    programs/tests/programs.js

# Source files used to run the Tachyon VM
RUN_SRCS =                              \
    $(BASE_SRCS)                        \
    $(TEST_SPEC_SRCS)                   \
    $(MAIN_SPEC_SRCS)                   \

# All source files
ALL_SRCS =                              \
    $(BASE_SRCS)                        \
    $(RUNTIME_SRCS)                     \
    $(STDLIB_SRCS)                      \
    $(TEST_SPEC_SRCS)                   \
    $(MAIN_SPEC_SRCS)                   \

all: tachyon compiler/sources.js

tachyon: makefile tachyon.in compiler/sources.js
	sed -e "s:@JSVMPROG@:$(JSVMPROG):g" -e "s:@ROOT_DIR@:$(ROOT_DIR):g" -e "s:@SOURCE_DIR@:$(ROOT_DIR)/source:g" -e "s:@RUN_SRCS@:$(RUN_SRCS):g" tachyon.in > tachyon
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

test:
	$(JSVM) $(RUN_SRCS) -- -test -v=trace

test64:
	$(JSVM64) $(RUN_SRCS) -- -test -v=trace

shell:
	$(RLWJSVM) $(RUN_SRCS)

shell64:
	$(RLWJSVM64) $(RUN_SRCS)

bootstrap:
	time $(JSVM) $(RUN_SRCS) -- -bootstrap -v=all

bootstrap64:
	time $(JSVM64) $(RUN_SRCS) -- -bootstrap -v=all

prof:
	$(JSVM) --prof $(RUN_SRCS) -- -test -v=trace
	deps/v8/tools/linux-tick-processor v8.log > prof_log.txt
	rm v8.log

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

