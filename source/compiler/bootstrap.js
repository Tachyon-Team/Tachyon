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

    // Compile the Tachyon source code
    var irList = compTachyon(allCode, params);

    // Initialize the runtime
    initRuntime(params);

    print('re-linking');

    // Re-link the primitives with each other to link strings
    for (var i = 0; i < irList.length; ++i)
    {
        var ir = irList[i];

        //print('Linking machine code for: "' + getSrcName(i) + '"');

        linkIR(ir, params);
    }

    print('Tachyon initialization complete');

    // TODO: execute compiled units
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
        'stdlib/errors.js',
        'stdlib/math.js',
        'stdlib/arrays.js',
        'stdlib/strings.js',
        'main.js'
    ];

    return tachyonSrcs;
}

/**
Compile either only the primitive code, or all of the Tachyon source code
*/
function compTachyon(allCode, params)
{
    // Get the source code for the primitives
    var primSrcs = getPrimSrcs(params);

    // If all code should be compiled
    if (allCode === true)
    {
        // Get the Tachyon compiler source code
        var tachyonSrcs = getTachyonSrcs(params);

        // Compile all souce code
        var srcs = primSrcs.concat(tachyonSrcs);
    }
    else
    {
        // Compile only the primitives
        var srcs = primSrcs;
    }

    var irList = compSources(srcs, params);

    return irList;
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

        // Validate the resulting code
        ir.validate();
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

    print('Linking run-time init functions');

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
    var initHeap = config.hostParams.staticEnv.getBinding('initHeap');

    //print('creating bridge');

    // Create a bridge to call the heap init function
    var initHeapBridge = makeBridge(
        initHeap,
        ['void*'],
        'void*'
    );

    //print('calling initHeap');

    // Initialize the heap
    var ctxPtr = initHeapBridge(asm.address([0,0,0,0]).getBytes(), heapAddr);

    // Store the context pointer in the compilation parameters
    config.hostParams.ctxPtr = ctxPtr;

    // Get the string allocation function
    var getStrObj = config.hostParams.staticEnv.getBinding('getStrObj');

    // Create a bridge to call the string allocation function
    var getStrObjBridge = makeBridge(
        getStrObj,
        ['void*'],
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

        var memBlock = allocMachineCodeBlock(2 * (jsStr.length + 1));
        var blockAddr = getBlockAddr(memBlock, 0);

        for (var i = 0; i < jsStr.length; ++i)
        {
            var ch = jsStr.charCodeAt(i);

            memBlock[2 * i] = ch & 0xFF;
            memBlock[2 * i + 1] = ch >> 8;
        }

        memBlock[2 * i] = 0;
        memBlock[2 * i + 1] = 0;

        var strObj = getStrObjBridge(ctxPtr, blockAddr);

        //print(strObj);

        freeMachineCodeBlock(memBlock);

        return strObj;
    }

    // Store the string allocatiom function in the compilation parameters
    config.hostParams.getStrObj = getStrObjFunc;
}

