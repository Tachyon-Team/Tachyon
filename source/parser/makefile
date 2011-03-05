# File: "makefile", Time-stamp: <2011-03-01 10:59:46 feeley>

# Copyright (c) 2010 by Marc Feeley, All Rights Reserved.

# makefile for building Javascript parser from the WebKit grammar.

# This makefile assumes that the following are installed:
#
#   - V8 Javascript interpreter (the "d8" executable in particular)
#   - Gambit Scheme compiler (the "gsi" and "gsc" executables in particular)

GRAMMAR=Grammar

all: test-parser

test-compiler: misc.js scanner.js parser.js scheme.js js.js
	./js2scm -exe t.js
	time ./t.js.exe

test-parser: misc.js scanner.js parser.js js.js
	./js tests/test0.js
	./js tests/test1.js
	./js tests/test2.js
	time ./js tests/test3.js > /dev/null
	./js tests/all.js

$(GRAMMAR).y.js: $(GRAMMAR).y keywords.scm
	./yacc2js.scm $(GRAMMAR).y

tokens.js: $(GRAMMAR).y.js
	sed -e '/^$$/q' $(GRAMMAR).y.js > tokens.js

keywords.js: keywords.scm
	gsc -prelude '(include "keywords.scm")' build-keyword-ht.scm
	gsi build-keyword-ht > keywords.js
	rm -f build-keyword-ht.o*

scanner-tables.js: tokens.js keywords.js
	cat tokens.js keywords.js > scanner-tables.js

scanner.js: scanner-tables.js
	sed -e '/START-OF-SCANNER-TABLES/q' scanner.js > part1.js
	sed -n '/END-OF-SCANNER-TABLES/,$$p' scanner.js > part2.js
	cat part1.js scanner-tables.js part2.js > scanner.js.new
	if ! diff scanner.js.new scanner.js > /dev/null; then \
	  for f in @*scanner.js; do \
	    echo mv $$f @$$f; \
	    mv $$f @$$f; \
	  done; \
	  mv scanner.js @scanner.js; \
	fi
	mv scanner.js.new scanner.js
	rm -f part1.js part2.js

parser-tables.js: $(GRAMMAR).y.js
	sed -n '/var action_table =/,$$p' $(GRAMMAR).y.js > parser-tables.js

parser.js: parser-tables.js
	sed -e '/START-OF-PARSER-TABLES/q' parser.js > part1.js
	sed -n '/END-OF-PARSER-TABLES/,$$p' parser.js > part2.js
	cat part1.js parser-tables.js part2.js > parser.js.new
	if ! diff parser.js.new parser.js > /dev/null; then \
	  for f in @*parser.js; do \
	    echo mv $$f @$$f; \
	    mv $$f @$$f; \
	  done; \
	  mv parser.js @parser.js; \
	fi
	mv parser.js.new parser.js
	rm -f part1.js part2.js

bootstrap:
	time ./js2scm -exe ../utility/debug.js ../utility/system.js ../utility/hashmap.js ../utility/num.js misc.js scanner.js parser.js pp.js ast-passes.js js.js
	time ./js2scm -exe ../utility/debug.js ../utility/system.js ../utility/hashmap.js ../utility/num.js misc.js scanner.js parser.js pp.js ast-passes.js js2js.js
	time ./js2scm -exe ../utility/debug.js ../utility/system.js ../utility/hashmap.js ../utility/num.js misc.js scanner.js parser.js pp.js ast-passes.js scheme.js js2scm.js

clean realclean:
	rm -f $(GRAMMAR).y.js tokens.js keywords.js scanner-tables.js parser-tables.js build-keyword-ht.o*
