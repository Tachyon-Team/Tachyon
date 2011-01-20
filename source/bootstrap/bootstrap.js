/**
@fileOverview
Implementation of the compilation of Tachyon using Tachyon.

@author
Maxime Chevalier-Boisvert

@copyright
Copyright (c) 2010-2011 Maxime Chevalier-Boisvert, All Rights Reserved
*/

// Source files, in the order they should be compiled and executed
// TODO: this should probably be populated using a script which also
//       populates source file names in the makefile
var tachyonSrcs = [
    'utility/debug.js',
    'utility/system.js',
    'utility/iterators.js',
    'utility/graph.js',
    'utility/arrays.js',
    'utility/hashmap.js',
    'utility/linkedlist.js',
    'utility/strings.js',
    'utility/modules.js',
    'utility/misc.js',
    'utility/xml.js',
    'utility/html.js',
    'compiler/targets.js',
    'compiler/params.js',
    'compiler/config.js',
    'compiler/compiler.js',
    'compiler/init.js',
    'parser/misc.js',
    'parser/scanner.js',
    'parser/parser.js',
    'parser/pp.js',
    'parser/ast-passes.js',
    'ir/types.js',
    'ir/static.js',
    'ir/instructions.js',
    'ir/constants.js',
    'ir/iir.js',
    'ir/cfg.js',
    'ir/functions.js',
    'ir/ast-to-ir.js',
    'ir/optpatterns.js',
    'ir/constprop.js',
    'ir/commelim.js',
    'ir/inlining.js',
    'ir/lowering.js',

    /*
    platform/ffi.js                 \
    platform/memory.js              \
    runtime/layout.js               \
    runtime/context.js              \
    runtime/objects.js              \
    codegen/asm.js                  \
    codegen/asm-x86.js              \
    codegen/linearscan.js           \
    codegen/backend.js              \
    codegen/ir-to-asm-x86.js        \
    codegen/regalloc-config-x86.js  \
    bootstrap/bootstrap.js

    stdlib/errors.js                \
    stdlib/math.js                  \
    stdlib/arrays.js                \
    stdlib/strings.js

    */

    'main.js'
];

/**
Compile the Tachyon compiler using Tachyon
*/
function bootstrap()
{
    // TODO: compile primitives

    // For each source file    
    for (var i = 0; i < tachyonSrcs.length; ++i)
    {
        var fileName = tachyonSrcs[i];

        // Compile this source file using the bootstrap parameters
        compTachyonSrc(fileName, config.bootParams);
    }

    // TODO: initialize heap

    // TODO: execute compiled units
}

/**
Compile a Tachyon source file
*/
function compTachyonSrc(fileName, params)
{
    print('Compiling Tachyon source: "' + fileName + '"');

    var ast = parse_src_file(fileName, params);

    // TODO: need to compile primitives first
    //var ir = unitToIR(ast, params);



    /*
    lowerIRFunc(ir, params);
    compileIR(ir, params);
    linkIR(ir, params);
    */
}

