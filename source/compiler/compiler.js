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

/** @namespace Glue code to tie the frontend and the backend together. */
var compiler = {};

/** 
Function meant to be assigned to the 'execute' field 
of a runtime property of an IRFunction. It executes
the machine code block associated to the IRFunction
with the arguments and the runtime context supplied.
*/
compiler.execute = function (args, runtime) 
{
    return execMachineCodeBlock(this.mcb);    
};

/** 
Function meant to be assigned to the 'free' field 
of a runtime property of an IRFunction. 
It frees the machine code block associated to the IRFunction
and clear the runtime.mcb field
*/
compiler.free = function () 
{
    freeMachineCodeBlock(this.mcb);    
    this.mcb = null;
};

/** 
Function meant to be assigned to the 'link' field 
of a linking property of an IRFunction. 
It links the current machine code block in the runtime
property to its dependencies.
*/
compiler.link = function (params) 
{
    this.rt.mcb.link(params);
    this.linked = true;
};

/**
Compiles an IRFunction and assigns linking and runtime
information to it. The runtime.mcb properties on the 
IRFunction should be freed once it is no longer used.
*/
function compileIR(ir, params, keepCB) 
{
    assert (
        params instanceof CompParams,
        'expected compilation parameters'
    );
    
    ir.linking.linked = false;
    ir.linking.link = compiler.link;

    var cb = backend.compileIRToCB(ir, params);

    if (params.printASM === true)
        params.print(backend.listing(cb));

    var mcb = cb.assembleToMachineCodeBlock();

    if (keepCB === true)
        ir.runtime.cb = cb;

    ir.runtime.mcb = mcb;
    ir.runtime.execute = compiler.execute;
    ir.runtime.free = compiler.free;

    ir.linking.rt = ir.runtime;
};

/** 
    Link the IRFunction.
*/
function linkIR(ir, params) 
{
    ir.linking.link(params);
};

/**
Compile an ast to compiled IR
*/
function compileAst(ast, params)
{
    assert (
        params instanceof CompParams,
        'compilation parameters expected'
    );
    
    //Used for compile time profiling (under the "compiletime" option)
    var startCompileAstTimeMs = (new Date()).getTime();

    var ir = unitToIR(ast, params);

    //Used for compile time profiling (under the "compiletime" option)
    var endUnitToIRTimeMs = (new Date()).getTime();

    lowerIRFunc(ir, params);

    //Used for compile time profiling (under the "compiletime" option)
    var endLowerIRFuncTimeMs = (new Date()).getTime();

    compileIR(ir, params);

    //Used for compile time profiling (under the "compiletime" option)
    var endCompileIRTimeMs  = (new Date()).getTime();

    linkIR(ir, params);

    //Used for compile time profiling (under the "compiletime" option)
    var endLinkIRTimeMs = (new Date()).getTime();


    if (params.printMCB)
    {
        print("printing MCB information");
        var blockObj = ir.runtime.mcb;
        var size = getMemoryBlockSize(blockObj);
        var addr = getBlockAddr(blockObj, 0);
        print("MCB " + size + " bytes starting at " + addr);

        //for (var i = 0; i < size; ++i)
        //{
        //    print(readFromMemoryBlock(blockObj, i));
        //}
    }

    var endCompileAstTimeMs = (new Date()).getTime();

    if(params.compiletime){
        var compilationTimeMs = endCompileAstTimeMs - startCompileAstTimeMs;
        var unitToIRTimeMs = endUnitToIRTimeMs - startCompileAstTimeMs;
        var lowerIRFuncTimeMs = endLowerIRFuncTimeMs - endUnitToIRTimeMs;
        var compileIRTimeMs = endCompileIRTimeMs - endLowerIRFuncTimeMs;
        var linkIRTimeMs = endLinkIRTimeMs - endCompileIRTimeMs;

        print("    Compilation time: " + compilationTimeMs + " ms (100%)");
        print("        unitToIR time: " + unitToIRTimeMs + " ms (" + (100*unitToIRTimeMs/compilationTimeMs).toFixed(2) + "%)");
        print("        lowerIRFunc time: " + lowerIRFuncTimeMs + " ms (" + (100*lowerIRFuncTimeMs/compilationTimeMs).toFixed(2) + "%)");
        print("        compileIR time: " + compileIRTimeMs + " ms (" + (100*compileIRTimeMs/compilationTimeMs).toFixed(2) + "%)");
        print("        linkIR time: " + linkIRTimeMs + " ms (" + (100*linkIRTimeMs/compilationTimeMs).toFixed(2) + "%)\n");
        params.compiletime = false; //Because it seems the "compileAst" function is called twice...
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

    return compileAst(ast, params);
}

/**
Compile a source file to compiled IR
*/
function compileSrcFile(fileName, params)
{
    var startTimeMs = (new Date()).getTime();

    var ast = parse_src_file(fileName, params);

    var endTimeMs = (new Date()).getTime();

    if(params.compiletime){
        print("\n\n------- PROFILING: COMPILATION TIME BREAKDOWN REPORT -------\n");
        print("    Parsing time: " + (endTimeMs - startTimeMs) + " ms");
    }

    return compileAst(ast, params);
}

