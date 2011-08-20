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
High-level compiler interface.

@author
Erick Lavoie
Maxime Chevalier-Boisvert
*/

/**
Compiles an IRFunction and assigns linking and runtime
information to it.
*/
function compileIR(ir, params) 
{
    assert (
        params instanceof CompParams,
        'expected compilation parameters'
    );
    
    const backend = params.backend;

    backend.genCode(ir, params);
};

/** 
Link the IRFunction.
*/
function linkIR(ir, params) 
{
    assert (
        params instanceof CompParams,
        'expected compilation parameters'
    );
    
    const backend = params.backend;

    backend.linkCode(ir, params);
};

/**
Compile an ast to compiled IR
*/
function compileAST(ast, params)
{
    assert (
        params instanceof CompParams,
        'compilation parameters expected'
    );
    
    var ir = unitToIR(ast, params);

    lowerIRFunc(ir, params);

    compileIR(ir, params);

    linkIR(ir, params);

    if (params.printMCB)
    {
        /*
        print("printing MCB information");
        var blockObj = ir.runtime.mcb;
        var size = getMemoryBlockSize(blockObj);
        var addr = getBlockAddr(blockObj, 0);
        print("MCB " + size + " bytes starting at " + addr);

        //for (var i = 0; i < size; ++i)
        //{
        //    print(readFromMemoryBlock(blockObj, i));
        //}
        */
    }

    // Return the compiled IR function
    return ir;
};

/**
Compile a source string to compiled IR
*/
function compileSrcString(str, params)
{
    var ast = parse_src_str(str, params);

    return compileAST(ast, params);
}

/**
Compile a source file to compiled IR
*/
function compileSrcFile(fileName, params)
{
    var ast = parse_src_file(fileName, params);

    return compileAST(ast, params);
}

/**
Compile a list of source code units, link them together and compile
them down to machine code.
*/
function compileSrcs(srcList, params, genCode)
{
    assert (
        params instanceof CompParams,
        'expected compilation parameters'
    );

    if (genCode === undefined)
        genCode = true;

    // Function to get the name string for a code unit
    function getSrcName(srcIdx)
    {
        var src = srcList[srcIdx];
        if (typeof src === 'object')
            return src.desc;
        else
            return src;
    }

    // List of parsed ASTs
    var astList = [];

    measurePerformance(
        "Parsing",
        function ()
        {
            // For each source unit
            for (var i = 0; i < srcList.length; ++i)
            {
                var src = srcList[i];

                log.trace('Parsing source: "' + getSrcName(i) + '"');

                // Parse the source unit
                if (typeof src === 'object')
                    var ast = parse_src_str(src.str, params);
                else
                    var ast = parse_src_file(src, params);

                // If we are compiling Tachyon source code,
                // parse static bindings in the unit
                if (params.tachyonSrc === true)
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

                log.trace('Lowering IR for: "' + getSrcName(i) + '"');

                // Perform IR lowering on the primitives
                lowerIRFunc(ir, params);

                // Validate the resulting IR
                ir.validate();
            }
        }
    );

    // If machine code should not be generated, stop here
    if (genCode === false)
        return irList;

    measurePerformance(
        "Machine code generation",
        function ()
        {
            // Compile the IR functions to machine code
            for (var i = 0; i < irList.length; ++i)
            {
                var ir = irList[i];

                log.trace('Generating machine code for: "' + getSrcName(i) + '"');

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

                log.trace('Linking code for: "' + getSrcName(i) + '"');

                linkIR(ir, params);
            }
        }
    );

    // Return the list of IR functions
    return irList;
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

