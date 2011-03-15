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
    print('bootstrap');

    print('Creating backend context layout');
    // Create the context and object layouts
    params.target.backendCfg.makeContextLayout(params);
    print('Creating fronted context layout');
    makeContextLayout(params);
    print('Creating object layouts');
    makeObjectLayouts(params);

    // Validate the backend configuration
    params.target.backendCfg.validate(params);
    
    // Initialize the FFI functions
    print('Initialize FFI functions');
    initFFI(params);

    // Get the source code for the primitives
    print('Get primitives source code');
    var primSrcs = getPrimSrcs(params);
    var primIRs;

    measurePerformance(
        "Compiling runtime",
        function ()
        {
            // Compile the primitives
            print('Compile primitives source code');
            primIRs = compSources(primSrcs, params);
        }
    );

    // Initialize the runtime
    initRuntime(params);

    print('Re-linking primitives');

    // Re-link the primitives with each other to link strings
    for (var i = 0; i < primIRs.length; ++i)
    {
        linkIR(primIRs[i], params);
    }
    print("After linking");

    // Get the source code for the standard library
    var libSrcs = getLibSrcs(params);
    var libIRs;

    print("Compiling stdlib");
    measurePerformance(
        "Compiling stdlib",
        function ()
        {
            // Compile the standard library
            libIRs = compSources(libSrcs, params);
        }
    );

    print('Initializing standard library');

    // Execute the standard library code units
    for (var i = 0; i < libIRs.length; ++i)
    {
        print('Executing unit for: "' + libSrcs[i] + '"');

        execUnit(libIRs[i], params);
    }

    // If all code should be compiled
    if (allCode === true)
    {
        // Get the Tachyon compiler source code
        var tachyonSrcs = getTachyonSrcs(params);
        var tachyonIRs;

        measurePerformance(
            "Compiling Tachyon",
            function ()
            {
                // Compile the Tachyon sources
                tachyonIRs = compSources(tachyonSrcs, params);
            });

        reportPerformance();

        // Execute the Tachyon code units
        for (var i = 0; i < tachyonIRs.length; ++i)
        {
            print('Executing unit for: "' + tachyonSrcs[i] + '"'); 
            execUnit(tachyonIRs[i], params);
        }
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

    print("Generate methods for the instantiable layouts");
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

    print("Generate wrapper code for the FFI functions");
    // Generate wrapper code for the FFI functions
    for (var f in params.ffiFuncs)
    {
        var func = params.ffiFuncs[f];

        wrapperSrc += func.genWrapper();
    }

    print("Building list of the primitives");
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
        // Source code for FFI interfacing
        'runtime/ffi.js',
        // Source code for the runtime initialization
        'runtime/rtinit.js'
    ];

    print("Returning the primitives");
    return primSrcs;
}

/**
Get a source code listing for the standard library files
*/
function getLibSrcs(params)
{
    var stdlibSrcs = [
        'stdlib/objects.js',
        'stdlib/functions.js',
        'stdlib/arrays.js',
        'stdlib/numbers.js',
        'stdlib/strings.js',
        'stdlib/math.js',
        'stdlib/errors.js',
        'stdlib/extensions.js',
    ];

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
        'utility/heap.js',
        'utility/hashmap.js',
        'utility/hashset.js',
        'utility/linkedlist.js',
        'utility/strings.js',
        'utility/modules.js',
        'utility/misc.js',
        'utility/num.js',
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
        'platform/mcb.js',
        'runtime/layout.js',
        'runtime/context.js',
        'runtime/objects.js',
        'backend/asm.js',
        'backend/regalloc.js',
        'backend/linearscan.js',
        'backend/backend.js',
        'backend/x86/asm.js',
        'backend/x86/config.js',
        'backend/x86/ir-to-asm.js',
        //'main.js'
        'bt-fib.js',
        ((params.target === Target.x86_32) ? 'bt-fib32.js' : 'bt-fib64.js')
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

    measurePerformance(
        "Parsing",
        function ()
        {
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
        }
    );

    // List for parsed IR function objects
    var irList = [];

    measurePerformance(
        "IR generation",
        function ()
        {
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
        }
    );

    measurePerformance(
        "IR lowering",
        function ()
        {
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
        }
    );

    measurePerformance(
        "Machine code generation",
        function ()
        {
            // Compile the IR functions to machine code
            for (var i = 0; i < irList.length; ++i)
            {
                var ir = irList[i];

                print('Generating machine code for: "' + getSrcName(i) + '"');

                compileIR(ir, params);
            }
        }
    );

    measurePerformance(
        "Machine code linking",
        function ()
        {
            // Link the primitives with each other
            for (var i = 0; i < irList.length; ++i)
            {
                var ir = irList[i];

                if (ir.linking.linked)
                    continue;

                print('Linking machine code for: "' + getSrcName(i) + '"');

                linkIR(ir, params);
            }
        }
    );

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

    // Allocate a 512MB heap
    var heapSize = Math.pow(2, 29);
    var heapBlock = allocMemoryBlock(heapSize, false);
    var heapAddr = getBlockAddr(heapBlock, 0);

    // Get the heap initialization function
    var initHeap = params.staticEnv.getBinding('initHeap');

    // Create a bridge to call the heap init function
    var initHeapBridge = makeBridge(
        initHeap,
        params,
        [new CPtrAsPtr(), new CIntAsInt()],
        new CPtrAsRef()
    );

    // Initialize the heap
    var ctxPtr = initHeapBridge(
        asm.address.nullAddr(params.target.ptrSizeBits).getBytes(),
        heapAddr,
        heapSize
    );
    
    // Store the context pointer in the compilation parameters
    params.ctxPtr = ctxPtr;

    // Get the string allocation function
    var getStrObj = params.staticEnv.getBinding('getStrObj');

    // Create a bridge to call the string allocation function
    var getStrObjBridge = makeBridge(
        getStrObj,
        params,
        [new CPtrAsPtr(), new CIntAsInt()],
        new CPtrAsBox()
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
            var memBlock = allocMemoryBlock(numBytes, false);
            var blockAddr = getBlockAddr(memBlock, 0);
        }
        else
        {
            var blockAddr = asm.address.
                            nullAddr(params.target.ptrSizeBits).getBytes();
        }

        for (var i = 0; i < jsStr.length; ++i)
        {
            var ch = jsStr.charCodeAt(i);

            memBlock[2 * i] = ch & 0xFF;
            memBlock[2 * i + 1] = ch >> 8;
        }

        var strObj = getStrObjBridge(ctxPtr, blockAddr, jsStr.length);

        //print(strObj);

        if (numBytes > 0)
        {
            freeMemoryBlock(memBlock);
        }

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
        new CIntAsBox()
    );

    // Call the compiled unit with the context pointer
    unitBridge(params.ctxPtr);
}

