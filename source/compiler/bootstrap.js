/* _________________________________________________________________________
 *
 *             Tachyon : A Self-Hosted JavaScript Virtual Machine
 *
 *
 *  This file is part of the Tachyon JavaScript project. Tachyon is
 *  distributed at:
 *  http://github.com/Tachyon-Team/Tachyon
 *
 *
 *  Copyright (c) 2011, Universite de Montreal
 *  All rights reserved.
 *
 *  This software is licensed under the following license (Modified BSD
 *  License):
 *
 *  Redistribution and use in source and binary forms, with or without
 *  modification, are permitted provided that the following conditions are
 *  met:
 *    * Redistributions of source code must retain the above copyright
 *      notice, this list of conditions and the following disclaimer.
 *    * Redistributions in binary form must reproduce the above copyright
 *      notice, this list of conditions and the following disclaimer in the
 *      documentation and/or other materials provided with the distribution.
 *    * Neither the name of the Universite de Montreal nor the names of its
 *      contributors may be used to endorse or promote products derived
 *      from this software without specific prior written permission.
 *
 *  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS
 *  IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED
 *  TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A
 *  PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL UNIVERSITE DE
 *  MONTREAL BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
 *  EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
 *  PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
 *  PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
 *  LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
 *  NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
 *  SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 * _________________________________________________________________________
 */

/**
@fileOverview
Implementation of the compilation of Tachyon using Tachyon.

@author
Maxime Chevalier-Boisvert
*/

/**
Compile and initialize the Tachyon compiler using Tachyon
*/
function bootstrap(params, allCode, writeImg)
{
    assert (
        params instanceof CompParams,
        'expected compilation parameters in bootstrap'
    );
    
    /* FIXME: disabled for testing purposes
    assert (
        !(writeImg === true && allCode !== true),
        'must compile all code to write image'
    );
    */

    log.trace('Beginning bootstrap (gen #' + TACHYON_GEN_NUMBER + ')');

    log.trace('Creating backend context layout');
    // Create the context and object layouts
    params.target.backendCfg.makeContextLayout(params);
    log.trace('Creating fronted context layout');
    makeContextLayout(params);
    log.trace('Creating object layouts');
    makeObjectLayouts(params);
    log.trace('Creating Tachyon constants');
    makeTachyonConsts(params);

    // Validate the backend configuration
    params.target.backendCfg.validate(params);
    
    // Initialize the FFI functions
    log.trace('Initialize FFI functions');
    initFFI(params);

    // Get the source code for the primitives
    log.trace('Get primitives source code');
    var primSrcs = getPrimSrcs(params);
    var primIRs;

    measurePerformance(
        "Compiling runtime",
        function ()
        {
            // Compile the primitives
            log.trace('Compile primitives source code');
            primIRs = compSources(primSrcs, params, writeImg);
        }
    );

    // If we are not writing an image
    if (writeImg !== true)
    {
        // Initialize the runtime
        initRuntime(params);
    }

    log.trace('Re-linking primitives');

    // Re-link the primitives with each other to link strings
    for (var i = 0; i < primIRs.length; ++i)
    {
        linkIR(primIRs[i], params);
    }
    log.trace("After linking");

    // Get the source code for the standard library
    var libSrcs = getLibSrcs(params);
    var libIRs;

    if (params.target === Target.x86_64 || !RUNNING_IN_TACHYON)
    {
        log.trace("Compiling stdlib");
        measurePerformance(
            "Compiling stdlib",
            function ()
            {
                // Compile the standard library
                libIRs = compSources(libSrcs, params, writeImg);
            }
        );

        // If we are not writing an image
        if (writeImg !== true)
        {
            log.trace('Initializing standard library');

            // Execute the standard library code units
            for (var i = 0; i < libIRs.length; ++i)
            {
                log.trace('Executing unit for: "' + libSrcs[i] + '"');
                execUnit(libIRs[i], params);
            }
        }
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
                tachyonIRs = compSources(tachyonSrcs, params, writeImg);
            });

        reportPerformance();

        log.trace("Code bytes allocated: " + codeBytesAllocated);

        // If we are not writing an image
        if (writeImg !== true)
        {
            // Execute the Tachyon code units
            for (var i = 0; i < tachyonIRs.length; ++i)
            {
                log.trace('Executing unit for: "' + tachyonSrcs[i] + '"'); 
                execUnit(tachyonIRs[i], params);
            }
        }
    }

    // If we are writing an image
    if (writeImg === true)
    {
        writeImage(
            params,
            primIRs,
            libIRs,
            tachyonIRs
        );
    }
    else
    {
        log.trace('Tachyon initialization complete');
    }
}

/**
Get the source code for the Tachyon primitives
*/
function getPrimSrcs(params)
{
    // Declare a variable for the layout source
    var layoutSrc = '';

    log.trace("Generate methods for the instantiable layouts");
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

    log.trace("Generate wrapper code for the FFI functions");
    // Generate wrapper code for the FFI functions
    for (var f in params.ffiFuncs)
    {
        var func = params.ffiFuncs[f];

        wrapperSrc += func.genWrapper();
    }

    log.trace("Building list of the primitives");

    // Add auto-generated code to the primitive source list
    var primSrcs = [
        // Generated code for the object layouts
        { str: layoutSrc, desc: 'object layout source' },
        // Generated code for the FFI functions
        { str: wrapperSrc, desc: 'FFI wrapper source' },
    ];

    // Add the runtime primitive source files to the list
    var primSrcs = primSrcs.concat(TACHYON_RUNTIME_SRCS);

    log.trace("Returning the primitives");

    return primSrcs;
}

/**
Get a source code listing for the standard library files
*/
function getLibSrcs(params)
{
    return TACHYON_STDLIB_SRCS;
}

/**
Get a source code listing for the Tachyon compiler, excluding the primitives
*/
function getTachyonSrcs(params)
{
    return TACHYON_BASE_SRCS.concat(TACHYON_MAIN_SPEC_SRCS);
}

/**
Parse Tachyon source code units, link them together and compile them
down to machine code, either for bootstrapping or to allow compiling
client code.
*/
function compSources(srcList, params, writeImg)
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

                log.trace('Parsing Tachyon source: "' + getSrcName(i) + '"');

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

                log.trace('Generating IR for: "' + getSrcName(i) + '"');

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

                log.trace('Performing IR lowering for: "' + getSrcName(i) + '"');

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

                log.trace('Generating machine code for: "' + getSrcName(i) + '"');

                compileIR(ir, params, writeImg);
            }
        }
    );

    // If we are not writing an image
    if (writeImg !== true)
    {
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

                    var addr = getBlockAddr(ir.runtime.mcb, 0);
                    log.trace('Linking machine code for: "' + getSrcName(i) + 
                          '" at address ' + addr);

                    linkIR(ir, params);
                }
            }
        );
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
    log.trace('Initializing run-time');

    // Allocate the heap
    var heapSize = params.heapSize;
    log.trace('Allocating heap (' + heapSize + ' bytes)');
    var heapBlock = allocMemoryBlock(heapSize, false);
    log.trace('Retrieving block address');
    var heapAddr = getBlockAddr(heapBlock, 0);

    log.trace('Heap address: ' + heapAddr);

    // Get the heap initialization function
    log.trace('Get heap initialization function');
    var initHeap = params.staticEnv.getBinding('initHeap');

    // Create a bridge to call the heap init function
    log.trace('Creating bridge to call the heap init function');
    //params.printRegAlloc = true;
    //params.printASM = true;
    //params.printMCB = true;

    var initHeapBridge = makeBridge(
        initHeap,
        params,
        [new CPtrAsPtr(), new CIntAsInt()],
        new CPtrAsRef()
    );

    // Initialize the heap
    log.trace('Calling ' + initHeap.funcName);
    var ctxPtr = initHeapBridge(
        asm.address.nullAddr(params.target.ptrSizeBits).getBytes(),
        heapAddr,
        heapSize
    );
    //params.printRegAlloc = false;
    //params.printASM = false;
    //params.printMCB = false;

    log.trace('Context pointer: ' + ctxPtr);
    
    // Store the context pointer in the compilation parameters
    params.ctxPtr = ctxPtr;

    // Get the string allocation function
    log.trace('Get the string allocation function');
    var getStrObj = params.staticEnv.getBinding('getStrObj');

    // Create a bridge to call the string allocation function
    log.trace('Creating bridge to the getStrObj');
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

            //print('ch #' + i + ' = ' + ch);

            writeToMemoryBlock(memBlock, 2 * i, ch & 0xFF);
            writeToMemoryBlock(memBlock, 2 * i + 1, ch >> 8);
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

