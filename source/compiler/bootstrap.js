/**
@fileOverview
Implementation of the compilation of Tachyon using Tachyon.

@author
Maxime Chevalier-Boisvert

@copyright
Copyright (c) 2010-2011 Maxime Chevalier-Boisvert, All Rights Reserved
*/

/**
Compile and initialize the Tachyon compiler using Tachyon
*/
function bootstrap(allCode, params)
{
    // Create the context and object layouts
    makeContextLayout(params);
    makeObjectLayouts(params);

    // Initialize the FFI functions
    initFFI(params);

    // Get the source code for the primitives
    var primSrcs = getPrimSrcs(params);

    // Compile the primitives
    var primIRs = compSources(primSrcs, params);

    // Initialize the runtime
    initRuntime(params);

    print('Re-linking primitives');

    // Re-link the primitives with each other to link strings
    for (var i = 0; i < primIRs.length; ++i)
    {
        linkIR(primIRs[i], params);
    }

    // Get the source code for the standard library
    var libSrcs = getLibSrcs(params);

    // Compile the standard library
    var libIRs = compSources(libSrcs, params);

    print('Initializing standard library');

    // Execute the standard library code units
    for (var i = 0; i < libIRs.length; ++i)
    {
        execUnit(libIRs[i], params);
    }

    // Initialize the standard library bindings in the run-time
    initLibRuntime(params);

    // If all code should be compiled
    if (allCode === true)
    {
        // Get the Tachyon compiler source code
        var tachyonSrcs = getTachyonSrcs(params);

        // Compile the Tachyon sources
        var tachyonIRs = compSources(tachyonSrcs, params);

        // TODO: execute compiled Tachyon units
    }

    print('Tachyon initialization complete');
}

/**
Get the source code for the Tachyon primitives
*/
function getPrimSrcs(params)
{
    // Declare a variable for the layout source
    var layoutSrc = '';

    // Generate methods for the instantiable layouts
    for (var l in params.memLayouts)
    {
        var layout = params.memLayouts[l];

        if (layout.isInstantiable() === false)
            continue;
 
        layoutSrc += layout.genMethods();
    }

    // Declare a variable for the FFI wrapper source
    var wrapperSrc = '';

    // Generate wrapper code for the FFI functions
    for (var f in params.ffiFuncs)
    {
        var func = params.ffiFuncs[f];

        wrapperSrc += func.genWrapper();
    }

    // Build a list of the primitive source code units
    var primSrcs = [
        // Generated code for the object layouts
        { str: layoutSrc, desc: 'object layout source' },
        // Generated code for the FFI functions
        { str: wrapperSrc, desc: 'FFI wrapper source' },
        // Utility code for the runtime primitives
        'runtime/utility.js',
        // Source code for the primitives
        'runtime/primitives.js',
        // Source code for string operations
        'runtime/strings.js',
        // Source code for the runtime initialization
        'runtime/rtinit.js'
    ];

    return primSrcs;
}

/**
Get a source code listing for the standard library files
*/
function getLibSrcs(params)
{
    // For now, compile only the string code
    var stdlibSrcs = [
        'stdlib/objects.js',
        'stdlib/strings.js'
    ];

    // TODO: add missing libraries once working
    // 'stdlib/errors.js',
    // 'stdlib/math.js',
    // 'stdlib/arrays.js',

    return stdlibSrcs;
}

/**
Get a source code listing for the Tachyon compiler, excluding the primitives
*/
function getTachyonSrcs(params)
{
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
        'compiler/bootstrap.js',
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
        'platform/ffi.js',
        'runtime/layout.js',
        'runtime/context.js',
        'runtime/objects.js',    
        'codegen/asm.js',
        'codegen/asm-x86.js',
        'codegen/linearscan.js',
        'codegen/backend.js',
        'codegen/ir-to-asm-x86.js',
        'codegen/regalloc-config-x86.js',
        'main.js'
    ];

    //var tachyonSrcs = ['parser/parser.js'];

    return tachyonSrcs;
}

/**
Parse Tachyon source code units, link them together and compile them
down to machine code, either for bootstrapping or to allow compiling
client code.
*/
function compSources(srcList, params)
{
    assert (
        params instanceof CompParams,
        'expected compilation parameters'
    );

    // Function to get the name string for a code unit
    function getSrcName(srcIdx)
    {
        var src = srcList[srcIdx];
        if (typeof src === 'object')
            return src.desc;
        else
            return src;
    }

    // List for parsed ASTs
    var astList = [];

    // For each source unit
    for (var i = 0; i < srcList.length; ++i)
    {
        var src = srcList[i];

        print('Parsing Tachyon source: "' + getSrcName(i) + '"');

        // Parse the source unit
        if (typeof src === 'object')
            var ast = parse_src_str(src.str, params);
        else
            var ast = parse_src_file(src, params);

        // Parse static bindings in the unit
        params.staticEnv.parseUnit(ast);

        // Add the parsed AST to the list
        astList.push(ast);
    }

    // List for parsed IR function objects
    var irList = [];

    // For each AST
    for (var i = 0; i < astList.length; ++i)
    {
        var ast = astList[i];

        print('Generating IR for: "' + getSrcName(i) + '"');

        // Generate IR from the AST
        var ir = unitToIR(ast, params);

        // Add the IR function to the list
        irList.push(ir);
    }

    // For each IR
    for (var i = 0; i < irList.length; ++i)
    {
        var ir = irList[i];

        print('Performing IR lowering for: "' + getSrcName(i) + '"');

        // Perform IR lowering on the primitives
        lowerIRFunc(ir, params);

        //print('Done lowering for: "' + getSrcName(i) + '"');

        // Validate the resulting code
        ir.validate();

        //print('Done validation for: "' + getSrcName(i) + '"');
    }

    //params.print = print;

    // Compile the IR functions to machine code
    for (var i = 0; i < irList.length; ++i)
    {
        var ir = irList[i];

        print('Generating machine code for: "' + getSrcName(i) + '"');

        compileIR(ir, params);
    }

    //params.print = undefined;

    // Link the primitives with each other
    for (var i = 0; i < irList.length; ++i)
    {
        var ir = irList[i];

        if (ir.linking.linked)
            continue;

        print('Linking machine code for: "' + getSrcName(i) + '"');

        linkIR(ir, params);
    }

    // Return the list of IR functions
    return irList;
}

/**
Initialize the run-time components, including the context 
and the global object.
*/
function initRuntime(params)
{
    print('Initializing run-time');

    // Allocate a 64MB heap
    var heapBlock = allocMachineCodeBlock(Math.pow(2, 26));
    var heapAddr = getBlockAddr(heapBlock, 0);

    // Get the heap initialization function
    var initHeap = params.staticEnv.getBinding('initHeap');

    // Create a bridge to call the heap init function
    var initHeapBridge = makeBridge(
        initHeap,
        params,
        ['void*'],
        'void*'
    );

    // Initialize the heap
    var ctxPtr = initHeapBridge(asm.address([0,0,0,0]).getBytes(), heapAddr);

    // Store the context pointer in the compilation parameters
    params.ctxPtr = ctxPtr;

    // Get the string allocation function
    var getStrObj = params.staticEnv.getBinding('getStrObj');

    // Create a bridge to call the string allocation function
    var getStrObjBridge = makeBridge(
        getStrObj,
        params,
        ['void*', 'int'],
        'void*'
    );

    /**
    Function to allocate a string object on the heap
    */
    function getStrObjFunc(jsStr)
    {
        assert (
            typeof jsStr === 'string',
            'expected string value in getStrObjFunc'
        );

        var numBytes = 2 * jsStr.length;

        if (numBytes > 0)
        {
            var memBlock = allocMachineCodeBlock(numBytes);
            var blockAddr = getBlockAddr(memBlock, 0);
        }
        else
        {
            var blockAddr = [0,0,0,0];
        }

        for (var i = 0; i < jsStr.length; ++i)
        {
            var ch = jsStr.charCodeAt(i);

            memBlock[2 * i] = ch & 0xFF;
            memBlock[2 * i + 1] = ch >> 8;
        }

        var strObj = getStrObjBridge(ctxPtr, blockAddr, jsStr.length);

        //print(strObj);

        freeMachineCodeBlock(memBlock);

        return strObj;
    }

    // Store the string allocatiom function in the compilation parameters
    params.getStrObj = getStrObjFunc;
}

/**
Execute a compiled code unit
*/
function execUnit(unitFunc, params)
{
    assert (
        unitFunc instanceof IRFunction,
        'invalid IR function'
    );

    assert (
        params.ctxPtr !== null,
        'cannot execute unit without context pointer'
    );

    // Create a bridge to call the compiled unit
    var unitBridge = makeBridge(
        unitFunc,
        params,
        [],
        'int'
    );

    // Call the compiled unit with the context pointer
    unitBridge(params.ctxPtr);
}

/**
Initialize the standard library bindings in the run-time
*/
function initLibRuntime(params)
{
    assert (
        params.ctxPtr !== null,
        'cannot execute unit without context pointer'
    );

    // Get the stdlib initialization function
    var initLib = params.staticEnv.getBinding('initStdlib');

    // Create a bridge to call the function
    var initLibBridge = makeBridge(
        initLib,
        params,
        [],
        'int'
    );

    // Initialize the bindings
    initLibBridge(params.ctxPtr);
}

