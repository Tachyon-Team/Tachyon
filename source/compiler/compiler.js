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
    
    ir.linking.linked = false;
    ir.linking.link = function (params) 
    {
        this.rt.mcb.link(params);
        this.linked = true;
    };

    var cb = backend.compileIRToCB(ir, params);

    if (params.printASM === true)
        params.print(backend.listing(cb));

    var mcb = cb.assembleToMachineCodeBlock();

    ir.runtime.mcb = mcb;
    //ir.runtime.execute = compiler.execute;
    //ir.runtime.free = compiler.free;
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

