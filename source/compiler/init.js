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
Tachyon compiler initialization.

@author
Maxime Chevalier-Boisvert
*/

/**
Compile and initialize the Tachyon primitives
*/
function initPrimitives(params, partialInit)
{
    assert (
        params instanceof CompParams,
        'expected compilation parameters in primitive initialization'
    );

    if (partialInit === undefined)
        partialInit = false;

    log.trace(
        'Performing ' + (partialInit? 'partial ':'') + 
        'primitive initialization'
    );

    // Create the context and object layouts
    log.trace('Creating context layout');
    makeContextLayout(params);
    log.trace('Creating object layouts');
    makeObjectLayouts(params);
    log.trace('Creating Tachyon constants');
    makeTachyonConsts(params);

    // Initialize the FFI functions
    log.trace('Initializing FFI functions');
    initFFI(params);

    // Get the source code for the primitives
    var primSrcs = getPrimSrcs(params);
    var primIRs;

    // Compile the runtime code
    log.trace('Compile primitive source code');
    measurePerformance(
        "Compiling primitives",
        function ()
        {
            primIRs = compileSrcs(primSrcs, params, !partialInit);
        }
    );

    // If this is a partial initialization, stop here
    if (partialInit === true)
        return;

    // Initialize the runtime
    initRuntime(params);

    log.trace('Re-linking primitives');

    // Re-link the primitives with each other to link strings
    for (var i = 0; i < primIRs.length; ++i)
    {
        linkIR(primIRs[i], params);
    }

    log.trace('Primitive initialization complete');

    //reportPerformance();
}

/**
Compile and initialize the Tachyon standard library
*/
function initStdlib(params)
{
    assert (
        params instanceof CompParams,
        'expected compilation parameters in stdlib initialization'
    );

    log.trace('Performing standard library initialization');

    // Get the source code for the standard library
    var libSrcs = getLibSrcs(params);
 
    // Library unit IRs
    var libIRs;

    // Compile the standard library
    log.trace("Compiling stdlib");
    measurePerformance(
        "Compiling stdlib",
        function ()
        {
            libIRs = compileSrcs(libSrcs, params);
        }
    );

    log.trace('Initializing standard library');

    // Execute the standard library code units
    for (var i = 0; i < libIRs.length; ++i)
    {
        log.trace('Executing unit for: "' + libSrcs[i] + '"');
        execUnit(libIRs[i], params);
    }

    log.trace('Standard library initialization complete');

    //reportPerformance();
}

/**
Get the source code for the Tachyon primitives
*/
function getPrimSrcs(params)
{
    // Declare a variable for the layout source
    var layoutSrc = '';

    // Generate methods for the instantiable layouts
    log.trace("Generating layout methods");
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
    log.trace("Generating FFI wrapper code");
    for (var f in params.ffiFuncs)
    {
        var func = params.ffiFuncs[f];

        wrapperSrc += func.genWrapper();
    }

    // Add auto-generated code to the primitive source list
    var primSrcs = [
        // Generated code for the object layouts
        { str: layoutSrc, desc: 'object layout source' },
        // Generated code for the FFI functions
        { str: wrapperSrc, desc: 'FFI wrapper source' },
    ];

    // Add the runtime primitive source files to the list
    var primSrcs = primSrcs.concat(TACHYON_RUNTIME_SRCS);

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
Initialize the runtime components, including the context 
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

    var initHeapBridge = makeBridge(
        initHeap,
        params,
        [new CPtrAsPtr(), new CIntAsInt()],
        new CPtrAsRef()
    );

    // Initialize the heap
    log.trace('Calling ' + initHeap.funcName);
    var ctxPtr = initHeapBridge(
        (params.backend.regSizeBytes === 8)? [0,0,0,0,0,0,0,0]:[0,0,0,0],
        heapAddr,
        heapSize
    );

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
            var blockAddr = 
                (params.backend.regSizeBytes === 8)?
                [0,0,0,0,0,0,0,0]:[0,0,0,0];
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

